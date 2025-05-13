;; title: art-royalties
;; version: 1.0
;; summary: Subscription based Art Royalties

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ARTWORK-EXISTS (err u101))
(define-constant ERR-ARTWORK-NOT-FOUND (err u102))
(define-constant SUBSCRIPTION-PRICE u10000000) ;; 10 STX

;; Data Maps
(define-map artworks
    principal
    {
        title: (string-ascii 100),
        artist: principal,
        subscription-price: uint,
        active: bool
    }
)

(define-map subscriptions
    {subscriber: principal, artist: principal}
    {
        expires-at: uint,
        active: bool
    }
)

;; Public Functions
(define-public (register-artwork (title (string-ascii 100)))
    (let
        ((artist tx-sender))
        (if (is-none (map-get? artworks artist))
            (ok (map-set artworks artist {
                title: title,
                artist: artist,
                subscription-price: SUBSCRIPTION-PRICE,
                active: true
            }))
            ERR-ARTWORK-EXISTS
        )
    )
)

(define-public (subscribe-to-artist (artist principal))
    (let
        ((subscriber tx-sender)
         (artwork (unwrap! (map-get? artworks artist) ERR-ARTWORK-NOT-FOUND))
         (current-block stacks-block-height))
        (try! (stx-transfer? SUBSCRIPTION-PRICE subscriber artist))
        (ok (map-set subscriptions 
            {subscriber: subscriber, artist: artist}
            {
                expires-at: (+ current-block u144), ;; ~1 day in blocks
                active: true
            }
        ))
    )
)

;; Read Only Functions
(define-read-only (get-artwork (artist principal))
    (map-get? artworks artist)
)

(define-read-only (check-subscription (subscriber principal) (artist principal))
    (let
        ((sub (map-get? subscriptions {subscriber: subscriber, artist: artist})))
        (if (is-none sub)
            false
            (> (get expires-at (unwrap-panic sub)) stacks-block-height)
        )
    )
)



;; Add new map for artist profiles
(define-map artist-profiles
    principal
    {
        name: (string-ascii 50),
        bio: (string-ascii 500),
        social-links: (string-ascii 200),
        total-subscribers: uint
    }
)

(define-public (create-artist-profile (name (string-ascii 50)) (bio (string-ascii 500)) (social-links (string-ascii 200)))
    (ok (map-set artist-profiles tx-sender {
        name: name,
        bio: bio,
        social-links: social-links,
        total-subscribers: u0
    }))
)



(define-constant BASIC-TIER-PRICE u10000000)    ;; 10 STX
(define-constant PREMIUM-TIER-PRICE u20000000)  ;; 20 STX
(define-constant VIP-TIER-PRICE u50000000)      ;; 50 STX

(define-public (subscribe-with-tier (artist principal) (tier uint))
    (let 
        ((price (if (is-eq tier u1) 
            BASIC-TIER-PRICE
            (if (is-eq tier u2)
                PREMIUM-TIER-PRICE
                (if (is-eq tier u3)
                    VIP-TIER-PRICE
                    BASIC-TIER-PRICE)))))
        (try! (stx-transfer? price tx-sender artist))
        (ok true))
)



(define-map collections
    {artist: principal, collection-id: uint}
    {
        name: (string-ascii 100),
        description: (string-ascii 500),
        artwork-count: uint
    }
)

(define-data-var next-collection-id uint u1)

(define-public (create-collection (name (string-ascii 100)) (description (string-ascii 500)))
    (let ((collection-id (var-get next-collection-id)))
        (map-set collections 
            {artist: tx-sender, collection-id: collection-id}
            {name: name, description: description, artwork-count: u0}
        )
        (var-set next-collection-id (+ collection-id u1))
        (ok collection-id)
    )
)



(define-map revenue-sharing
    principal
    {
        collaborators: (list 5 principal),
        shares: (list 5 uint)
    }
)

(define-public (set-revenue-sharing (collaborators (list 5 principal)) (shares (list 5 uint)))
    (ok (map-set revenue-sharing tx-sender {
        collaborators: collaborators,
        shares: shares
    }))
)



(define-map special-offers
    principal
    {
        discount-price: uint,
        start-block: uint,
        end-block: uint,
        active: bool
    }
)

(define-public (create-special-offer (discount-price uint) (duration uint))
    (ok (map-set special-offers tx-sender {
        discount-price: discount-price,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration),
        active: true
    }))
)


(define-map subscriber-analytics
    principal
    {
        total-subscriptions: uint,
        subscription-history: (list 10 uint),
        last-active: uint
    }
)

(define-read-only (get-subscriber-stats (subscriber principal))
    (default-to 
        {total-subscriptions: u0, subscription-history: (list ), last-active: u0}
        (map-get? subscriber-analytics subscriber)
    )
)



(define-map gift-subscriptions
    {sender: principal, recipient: principal, artist: principal}
    {
        created-at: uint,
        duration: uint,
        redeemed: bool
    }
)

(define-public (gift-subscription (recipient principal) (artist principal))
    (let ((sender tx-sender))
        (try! (stx-transfer? SUBSCRIPTION-PRICE sender artist))
        (ok (map-set gift-subscriptions
            {sender: sender, recipient: recipient, artist: artist}
            {created-at: stacks-block-height, duration: u144, redeemed: false}
        ))
    )
)



;; Map to track subscription history for loyalty discounts
(define-map subscription-history
    principal
    {
        consecutive-subscriptions: uint,
        total-subscriptions: uint,
        last-subscription-block: uint
    }
)

;; Constants for loyalty discounts
(define-constant TIER1-DISCOUNT-THRESHOLD u3)  ;; 3 consecutive subscriptions
(define-constant TIER2-DISCOUNT-THRESHOLD u6)  ;; 6 consecutive subscriptions
(define-constant TIER1-DISCOUNT-PERCENT u10)   ;; 10% discount
(define-constant TIER2-DISCOUNT-PERCENT u20)   ;; 20% discount

;; Calculate discounted price based on loyalty
(define-read-only (get-discounted-price (subscriber principal) (base-price uint))
    (let 
        ((history (default-to 
                    {consecutive-subscriptions: u0, total-subscriptions: u0, last-subscription-block: u0} 
                    (map-get? subscription-history subscriber)))
         (consecutive (get consecutive-subscriptions history)))
        
        (if (>= consecutive TIER2-DISCOUNT-THRESHOLD)
            ;; Apply 20% discount
            (- base-price (/ (* base-price TIER2-DISCOUNT-PERCENT) u100))
            (if (>= consecutive TIER1-DISCOUNT-THRESHOLD)
                ;; Apply 10% discount
                (- base-price (/ (* base-price TIER1-DISCOUNT-PERCENT) u100))
                ;; No discount
                base-price
            )
        )
    )
)

;; Enhanced subscribe function with loyalty tracking
(define-public (subscribe-with-loyalty (artist principal))
    (let
        ((subscriber tx-sender)
         (artwork (unwrap! (map-get? artworks artist) ERR-ARTWORK-NOT-FOUND))
         (current-block stacks-block-height)
         (history (default-to 
                    {consecutive-subscriptions: u0, total-subscriptions: u0, last-subscription-block: u0} 
                    (map-get? subscription-history subscriber)))
         (is-renewal (and 
                        (> (get last-subscription-block history) u0)
                        (< (- current-block (get last-subscription-block history)) u300))) ;; Consider renewal if within ~2 days
         (consecutive (if is-renewal (+ (get consecutive-subscriptions history) u1) u1))
         (total (+ (get total-subscriptions history) u1))
         (discounted-price (get-discounted-price subscriber SUBSCRIPTION-PRICE)))
        
        ;; Transfer the discounted amount
        (try! (stx-transfer? discounted-price subscriber artist))
        
        ;; Update subscription
        (map-set subscriptions 
            {subscriber: subscriber, artist: artist}
            {
                expires-at: (+ current-block u144), ;; ~1 day in blocks
                active: true
            }
        )
        
        ;; Update loyalty history
        (map-set subscription-history subscriber {
            consecutive-subscriptions: consecutive,
            total-subscriptions: total,
            last-subscription-block: current-block
        })
        
        (ok discounted-price)
    )
)



;; Map to track tips
(define-map artist-tips
    principal
    {
        total-received: uint,
        tip-count: uint,
        last-tip-block: uint
    }
)

;; Map to track tipper stats
(define-map tipper-stats
    {tipper: principal, artist: principal}
    {
        total-tipped: uint,
        tip-count: uint,
        last-tip-block: uint
    }
)

;; Send a tip to an artist
(define-public (tip-artist (artist principal) (amount uint))
    (let 
        ((tipper tx-sender)
         (current-block stacks-block-height)
         (artist-tip-data (default-to 
                            {total-received: u0, tip-count: u0, last-tip-block: u0} 
                            (map-get? artist-tips artist)))
         (tipper-data (default-to 
                        {total-tipped: u0, tip-count: u0, last-tip-block: u0} 
                        (map-get? tipper-stats {tipper: tipper, artist: artist}))))
        
        ;; Transfer the tip amount
        (try! (stx-transfer? amount tipper artist))
        
        ;; Update artist tip stats
        (map-set artist-tips artist {
            total-received: (+ (get total-received artist-tip-data) amount),
            tip-count: (+ (get tip-count artist-tip-data) u1),
            last-tip-block: current-block
        })
        
        ;; Update tipper stats
        (map-set tipper-stats {tipper: tipper, artist: artist} {
            total-tipped: (+ (get total-tipped tipper-data) amount),
            tip-count: (+ (get tip-count tipper-data) u1),
            last-tip-block: current-block
        })
        
        (ok true)
    )
)

;; Get artist tip statistics
(define-read-only (get-artist-tip-stats (artist principal))
    (default-to 
        {total-received: u0, tip-count: u0, last-tip-block: u0}
        (map-get? artist-tips artist)
    )
)


;; Enhanced gift subscriptions with messages
(define-map enhanced-gift-subscriptions
    {sender: principal, recipient: principal, artist: principal}
    {
        created-at: uint,
        duration: uint,
        redeemed: bool,
        message: (string-ascii 200),
        gift-name: (string-ascii 50)
    }
)

;; Gift a subscription with a personalized message
(define-public (gift-subscription-with-message 
                (recipient principal) 
                (artist principal) 
                (message (string-ascii 200))
                (gift-name (string-ascii 50)))
    (let ((sender tx-sender))
        ;; Transfer the subscription fee to the artist
        (try! (stx-transfer? SUBSCRIPTION-PRICE sender artist))
        
        ;; Record the gift with message
        (ok (map-set enhanced-gift-subscriptions
            {sender: sender, recipient: recipient, artist: artist}
            {
                created-at: stacks-block-height, 
                duration: u144, 
                redeemed: false,
                message: message,
                gift-name: gift-name
            }
        ))
    )
)

;; Redeem a gifted subscription
(define-public (redeem-gift-subscription (sender principal) (artist principal))
    (let 
        ((recipient tx-sender)
         (gift-data (unwrap! 
                      (map-get? enhanced-gift-subscriptions 
                        {sender: sender, recipient: recipient, artist: artist}) 
                      (err u107)))
         (current-block stacks-block-height))
        
        ;; Check if already redeemed
        (asserts! (not (get redeemed gift-data)) (err u108))
        
        ;; Set up the subscription
        (map-set subscriptions 
            {subscriber: recipient, artist: artist}
            {
                expires-at: (+ current-block (get duration gift-data)),
                active: true
            }
        )
        
        ;; Mark as redeemed
        (map-set enhanced-gift-subscriptions
            {sender: sender, recipient: recipient, artist: artist}
            (merge gift-data {redeemed: true})
        )
        
        (ok true)
    )
)

;; Get gift details
(define-read-only (get-gift-details (sender principal) (recipient principal) (artist principal))
    (map-get? enhanced-gift-subscriptions {sender: sender, recipient: recipient, artist: artist})
)



;; Map to track collaborative artworks
(define-map collaborative-artworks
    uint
    {
        title: (string-ascii 100),
        primary-artist: principal,
        collaborators: (list 5 principal),
        shares: (list 5 uint),
        total-revenue: uint,
        subscription-price: uint,
        active: bool
    }
)

;; Track collaborative artwork IDs
(define-data-var next-collab-id uint u1)

(define-map exclusive-content
    uint
    {
        artist: principal,
        title: (string-ascii 100),
        start-block: uint,
        end-block: uint,
        active: bool
    }
)

(define-data-var next-exclusive-id uint u1)

(define-public (create-exclusive-content (title (string-ascii 100)) (duration uint))
    (let 
        ((content-id (var-get next-exclusive-id))
         (current-block stacks-block-height))
        (map-set exclusive-content content-id
            {
                artist: tx-sender,
                title: title,
                start-block: current-block,
                end-block: (+ current-block duration),
                active: true
            }
        )
        (var-set next-exclusive-id (+ content-id u1))
        (ok content-id)
    )
)

(define-read-only (can-access-exclusive (subscriber principal) (content-id uint))
    (let 
        ((content (unwrap! (map-get? exclusive-content content-id) false))
         (current-block stacks-block-height))
        (and 
            (get active content)
            (check-subscription subscriber (get artist content))
            (>= current-block (get start-block content))
            (<= current-block (get end-block content))
        )
    )
)


(define-map artist-bundles
    uint
    {
        name: (string-ascii 50),
        artists: (list 5 principal),
        price: uint,
        duration: uint,
        active: bool
    }
)

(define-data-var next-bundle-id uint u1)

(define-public (create-artist-bundle (name (string-ascii 50)) (artists (list 5 principal)) (price uint) (duration uint))
    (let ((bundle-id (var-get next-bundle-id)))
        (map-set artist-bundles bundle-id
            {
                name: name,
                artists: artists,
                price: price,
                duration: duration,
                active: true
            }
        )
        (var-set next-bundle-id (+ bundle-id u1))
        (ok bundle-id)
    )
)

(define-public (subscribe-to-bundle (bundle-id uint))
    (let 
        ((bundle (unwrap! (map-get? artist-bundles bundle-id) ERR-ARTWORK-NOT-FOUND))
         (subscriber tx-sender)
         (current-block stacks-block-height))
        (try! (stx-transfer? (get price bundle) subscriber (unwrap! (element-at (get artists bundle) u0) ERR-ARTWORK-NOT-FOUND)))
        (fold subscribe-artist-in-bundle (get artists bundle) (ok true))
    )
)

(define-private (subscribe-artist-in-bundle (artist principal) (previous-result (response bool uint)))
    (begin
        (map-set subscriptions 
            {subscriber: tx-sender, artist: artist}
            {
                expires-at: (+ stacks-block-height u144),
                active: true
            }
        )
        previous-result
    )
)


(define-map collaboration-events
    uint
    {
        title: (string-ascii 100),
        artists: (list 10 principal),
        start-block: uint,
        end-block: uint,
        price: uint,
        revenue-shares: (list 10 uint),
        active: bool
    }
)

(define-data-var next-collab-event-id uint u1)

(define-public (create-collaboration-event 
    (title (string-ascii 100))
    (collaborating-artists (list 10 principal))
    (duration uint)
    (event-price uint)
    (shares (list 10 uint)))
    
    (let ((event-id (var-get next-collab-event-id)))
        (map-set collaboration-events event-id
            {
                title: title,
                artists: collaborating-artists,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height duration),
                price: event-price,
                revenue-shares: shares,
                active: true
            }
        )
        (var-set next-collab-event-id (+ event-id u1))
        (ok event-id)
    )
)

(define-public (join-collaboration-event (event-id uint))
    (let 
        ((event (unwrap! (map-get? collaboration-events event-id) ERR-ARTWORK-NOT-FOUND))
         (subscriber tx-sender)
         (current-block stacks-block-height))
        
        (asserts! (get active event) (err u300))
        (asserts! (<= current-block (get end-block event)) (err u301))
        
        (try! (distribute-event-revenue (get price event) (get artists event) (get revenue-shares event)))
        (ok true)
    )
)

(define-private (map-artists-shares (artists (list 10 principal)) (shares (list 10 uint)))
    (map combine-artist-share artists shares)
)

(define-private (combine-artist-share (artist principal) (share uint))
    {artist: artist, share: share}
)

(define-private (distribute-event-revenue (total-amount uint) (artists (list 10 principal)) (shares (list 10 uint)))
    (fold distribute-to-artist (map combine-artist-share artists shares) (ok true))
)

(define-private (distribute-to-artist (artist-share {artist: principal, share: uint}) (previous-result (response bool uint)))
    (begin
        (try! (stx-transfer? (get share artist-share) tx-sender (get artist artist-share)))
        (ok true)
    )
)


(define-map nft-gated-content
    uint
    {
        artist: principal,
        title: (string-ascii 100),
        nft-contract: principal,
        required-token-id: uint,
        content-uri: (string-ascii 256),
        active: bool
    }
)

(define-data-var next-premium-content-id uint u1)

(define-trait nft-trait
    ((get-owner (uint) (response principal uint))
    (get-token-uri (uint) (response (optional (string-ascii 256)) uint)))
)

(define-public (create-nft-gated-content 
    (title (string-ascii 100))
    (nft-contract principal)
    (token-id uint)
    (content-uri (string-ascii 256)))
    
    (let ((content-id (var-get next-premium-content-id)))
        (map-set nft-gated-content content-id
            {
                artist: tx-sender,
                title: title,
                nft-contract: nft-contract,
                required-token-id: token-id,
                content-uri: content-uri,
                active: true
            }
        )
        (var-set next-premium-content-id (+ content-id u1))
        (ok content-id)
    )
)

;; (define-read-only (can-access-premium-content (user principal) (content-id uint))
;;     (match (map-get? nft-gated-content content-id)
;;         content (match (contract-call? 
;;                     (get nft-contract content)
;;                     get-owner
;;                     (get required-token-id content))
;;                 success (is-eq success user)
;;                 error false)
;;         none false
;;     )
;; )