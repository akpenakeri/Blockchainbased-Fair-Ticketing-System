;; title: dynamic-event-pricing
;; version: 1.0
;; summary: Dynamic pricing system for artist events with supply/demand mechanics

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-EVENT-NOT-FOUND (err u200))
(define-constant ERR-EVENT-ENDED (err u201))
(define-constant ERR-EVENT-FULL (err u202))
(define-constant ERR-INVALID-PRICING-TIER (err u203))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u204))
(define-constant ERR-ALREADY-REGISTERED (err u205))
(define-constant ERR-REFUND-WINDOW-CLOSED (err u206))
(define-constant ERR-PRICE-UPDATE-TOO-FREQUENT (err u207))

;; Pricing constants
(define-constant BASE-PRICE-MULTIPLIER u100)
(define-constant DEMAND-SURGE-THRESHOLD u80) ;; 80% capacity triggers surge
(define-constant MAX-PRICE-MULTIPLIER u300) ;; Max 3x base price
(define-constant MIN-PRICE-MULTIPLIER u50)  ;; Min 0.5x base price
(define-constant EARLY-BIRD-DISCOUNT u20)   ;; 20% early discount
(define-constant LAST-MINUTE-SURGE u50)     ;; 50% last minute surge
(define-constant PRICE-UPDATE-COOLDOWN u10) ;; Blocks between price updates

;; Event structure
(define-map events
    uint
    {
        artist: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        base-price: uint,
        current-price: uint,
        max-capacity: uint,
        current-attendees: uint,
        start-block: uint,
        end-block: uint,
        registration-end: uint,
        location: (string-ascii 200),
        category: (string-ascii 50),
        refund-window: uint,
        last-price-update: uint,
        pricing-tier: uint,
        revenue-total: uint,
        active: bool
    }
)

;; Event registration tracking
(define-map event-registrations
    {event-id: uint, attendee: principal}
    {
        registration-block: uint,
        price-paid: uint,
        refunded: bool,
        check-in-status: bool
    }
)

;; Pricing tier configurations
(define-map pricing-tiers
    uint
    {
        name: (string-ascii 30),
        base-multiplier: uint,
        demand-sensitivity: uint,
        surge-threshold: uint,
        max-multiplier: uint
    }
)

;; Event analytics
(define-map event-analytics
    uint
    {
        total-revenue: uint,
        peak-attendance: uint,
        average-price: uint,
        refund-rate: uint,
        demand-score: uint
    }
)

;; Artist event statistics
(define-map artist-event-stats
    principal
    {
        total-events: uint,
        total-revenue: uint,
        average-attendance: uint,
        reputation-score: uint
    }
)

;; Data variables
(define-data-var next-event-id uint u1)
(define-data-var platform-fee-rate uint u5) ;; 5% platform fee

;; Initialize pricing tiers
(define-public (initialize-pricing-tiers)
    (begin
        (map-set pricing-tiers u1 {
            name: "Standard",
            base-multiplier: u100,
            demand-sensitivity: u20,
            surge-threshold: u75,
            max-multiplier: u200
        })
        (map-set pricing-tiers u2 {
            name: "Premium",
            base-multiplier: u150,
            demand-sensitivity: u30,
            surge-threshold: u60,
            max-multiplier: u300
        })
        (map-set pricing-tiers u3 {
            name: "Exclusive",
            base-multiplier: u200,
            demand-sensitivity: u50,
            surge-threshold: u50,
            max-multiplier: u500
        })
        (ok true)
    )
)

;; Create new event with dynamic pricing
(define-public (create-event 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (base-price uint)
    (max-capacity uint)
    (event-duration uint)
    (registration-duration uint)
    (location (string-ascii 200))
    (category (string-ascii 50))
    (pricing-tier uint))
    
    (let 
        ((event-id (var-get next-event-id))
         (current-block stacks-block-height)
         (tier-data (unwrap! (map-get? pricing-tiers pricing-tier) ERR-INVALID-PRICING-TIER))
         (adjusted-base-price (/ (* base-price (get base-multiplier tier-data)) u100)))
        
        (map-set events event-id {
            artist: tx-sender,
            title: title,
            description: description,
            base-price: base-price,
            current-price: adjusted-base-price,
            max-capacity: max-capacity,
            current-attendees: u0,
            start-block: (+ current-block registration-duration),
            end-block: (+ current-block registration-duration event-duration),
            registration-end: (+ current-block registration-duration),
            location: location,
            category: category,
            refund-window: (/ registration-duration u2),
            last-price-update: current-block,
            pricing-tier: pricing-tier,
            revenue-total: u0,
            active: true
        })
        
        (var-set next-event-id (+ event-id u1))
        (ok event-id)
    )
)

;; Calculate dynamic price based on demand
(define-read-only (calculate-dynamic-price (event-id uint))
    (let 
        ((event-data (unwrap! (map-get? events event-id) u0))
         (tier-data (unwrap! (map-get? pricing-tiers (get pricing-tier event-data)) u0))
         (capacity-percentage (/ (* (get current-attendees event-data) u100) (get max-capacity event-data)))
         (blocks-until-start (if (> (get start-block event-data) stacks-block-height)
                               (- (get start-block event-data) stacks-block-height)
                               u0))
         (base-price (get base-price event-data))
         (demand-multiplier (if (>= capacity-percentage (get surge-threshold tier-data))
                              (+ u100 (get demand-sensitivity tier-data))
                              u100))
         (time-multiplier (if (< blocks-until-start u50) ;; Last minute surge
                            (+ u100 LAST-MINUTE-SURGE)
                            (if (> blocks-until-start u500) ;; Early bird discount
                                (- u100 EARLY-BIRD-DISCOUNT)
                                u100))))
        
        (let ((calculated-price (/ (* (* base-price demand-multiplier) time-multiplier) u10000)))
            (if (> calculated-price (/ (* base-price (get max-multiplier tier-data)) u100))
                (/ (* base-price (get max-multiplier tier-data)) u100)
                (if (< calculated-price (/ (* base-price MIN-PRICE-MULTIPLIER) u100))
                    (/ (* base-price MIN-PRICE-MULTIPLIER) u100)
                    calculated-price
                )
            )
        )
    )
)

;; Update event pricing
(define-public (update-event-pricing (event-id uint))
    (let 
        ((event-data (unwrap! (map-get? events event-id) ERR-EVENT-NOT-FOUND))
         (current-block stacks-block-height)
         (new-price (calculate-dynamic-price event-id)))
        
        (asserts! (>= (- current-block (get last-price-update event-data)) PRICE-UPDATE-COOLDOWN) ERR-PRICE-UPDATE-TOO-FREQUENT)
        (asserts! (get active event-data) ERR-EVENT-ENDED)
        
        (map-set events event-id
            (merge event-data {
                current-price: new-price,
                last-price-update: current-block
            })
        )
        
        (ok new-price)
    )
)

;; Register for event with dynamic pricing
(define-public (register-for-event (event-id uint))
    (let 
        ((event-data (unwrap! (map-get? events event-id) ERR-EVENT-NOT-FOUND))
         (attendee tx-sender)
         (current-block stacks-block-height)
         (current-price (calculate-dynamic-price event-id))
         (platform-fee (/ (* current-price (var-get platform-fee-rate)) u100))
         (artist-payment (- current-price platform-fee)))
        
        (asserts! (get active event-data) ERR-EVENT-ENDED)
        (asserts! (< current-block (get registration-end event-data)) ERR-EVENT-ENDED)
        (asserts! (< (get current-attendees event-data) (get max-capacity event-data)) ERR-EVENT-FULL)
        (asserts! (is-none (map-get? event-registrations {event-id: event-id, attendee: attendee})) ERR-ALREADY-REGISTERED)
        
        ;; Process payment
        (try! (stx-transfer? artist-payment attendee (get artist event-data)))
        (try! (stx-transfer? platform-fee attendee (as-contract tx-sender)))
        
        ;; Record registration
        (map-set event-registrations 
            {event-id: event-id, attendee: attendee}
            {
                registration-block: current-block,
                price-paid: current-price,
                refunded: false,
                check-in-status: false
            }
        )
        
        ;; Update event data
        (map-set events event-id
            (merge event-data {
                current-attendees: (+ (get current-attendees event-data) u1),
                current-price: current-price,
                revenue-total: (+ (get revenue-total event-data) current-price),
                last-price-update: current-block
            })
        )
        
        (ok current-price)
    )
)

;; Cancel registration and process refund
(define-public (cancel-registration (event-id uint))
    (let 
        ((event-data (unwrap! (map-get? events event-id) ERR-EVENT-NOT-FOUND))
         (attendee tx-sender)
         (registration (unwrap! (map-get? event-registrations {event-id: event-id, attendee: attendee}) ERR-ALREADY-REGISTERED))
         (current-block stacks-block-height)
         (refund-deadline (+ (get registration-block registration) (get refund-window event-data))))
        
        (asserts! (not (get refunded registration)) ERR-ALREADY-REGISTERED)
        (asserts! (<= current-block refund-deadline) ERR-REFUND-WINDOW-CLOSED)
        
        ;; Calculate refund amount (with penalty)
        (let 
            ((refund-penalty (/ (get price-paid registration) u10)) ;; 10% penalty
             (refund-amount (- (get price-paid registration) refund-penalty)))
            
            ;; Process refund
            (try! (as-contract (stx-transfer? refund-amount tx-sender attendee)))
            
            ;; Update registration status
            (map-set event-registrations 
                {event-id: event-id, attendee: attendee}
                (merge registration {refunded: true})
            )
            
            ;; Update event data
            (map-set events event-id
                (merge event-data {
                    current-attendees: (- (get current-attendees event-data) u1),
                    revenue-total: (- (get revenue-total event-data) (get price-paid registration))
                })
            )
            
            (ok refund-amount)
        )
    )
)

;; Check-in attendee at event
(define-public (check-in-attendee (event-id uint) (attendee principal))
    (let 
        ((event-data (unwrap! (map-get? events event-id) ERR-EVENT-NOT-FOUND))
         (registration (unwrap! (map-get? event-registrations {event-id: event-id, attendee: attendee}) ERR-ALREADY-REGISTERED))
         (current-block stacks-block-height))
        
        (asserts! (is-eq tx-sender (get artist event-data)) ERR-NOT-AUTHORIZED)
        (asserts! (>= current-block (get start-block event-data)) ERR-EVENT-NOT-FOUND)
        (asserts! (<= current-block (get end-block event-data)) ERR-EVENT-ENDED)
        (asserts! (not (get refunded registration)) ERR-ALREADY-REGISTERED)
        
        (map-set event-registrations 
            {event-id: event-id, attendee: attendee}
            (merge registration {check-in-status: true})
        )
        
        (ok true)
    )
)

;; Get event details
(define-read-only (get-event (event-id uint))
    (map-get? events event-id)
)

;; Get current dynamic price for event
(define-read-only (get-current-price (event-id uint))
    (calculate-dynamic-price event-id)
)

;; Get registration details
(define-read-only (get-registration (event-id uint) (attendee principal))
    (map-get? event-registrations {event-id: event-id, attendee: attendee})
)

;; Get pricing tier details
(define-read-only (get-pricing-tier (tier-id uint))
    (map-get? pricing-tiers tier-id)
)

;; Calculate event demand score
(define-read-only (calculate-demand-score (event-id uint))
    (let 
        ((event-data (unwrap! (map-get? events event-id) u0))
         (capacity-percentage (/ (* (get current-attendees event-data) u100) (get max-capacity event-data)))
         (price-increase-percentage (if (> (get current-price event-data) (get base-price event-data))
                                     (/ (* (- (get current-price event-data) (get base-price event-data)) u100) (get base-price event-data))
                                     u0)))
        (+ capacity-percentage price-increase-percentage)
    )
)

;; Update artist statistics
(define-public (update-artist-stats (artist principal))
    (let 
        ((current-stats (default-to 
                         {total-events: u0, total-revenue: u0, average-attendance: u0, reputation-score: u0}
                         (map-get? artist-event-stats artist))))
        
        (map-set artist-event-stats artist
            (merge current-stats {
                reputation-score: (+ (get reputation-score current-stats) u10)
            })
        )
        
        (ok true)
    )
)

;; Get artist event statistics
(define-read-only (get-artist-stats (artist principal))
    (default-to 
        {total-events: u0, total-revenue: u0, average-attendance: u0, reputation-score: u0}
        (map-get? artist-event-stats artist)
    )
)

;; Get events by artist
(define-read-only (get-artist-events (artist principal))
    (ok artist) ;; Simplified - would need iteration in full implementation
)

;; Get platform statistics
(define-read-only (get-platform-stats)
    {
        total-events: (var-get next-event-id),
        platform-fee-rate: (var-get platform-fee-rate)
    }
)

;; Admin function to update platform fee
(define-public (update-platform-fee (new-fee uint))
    (begin
        (var-set platform-fee-rate new-fee)
        (ok true)
    )
)


