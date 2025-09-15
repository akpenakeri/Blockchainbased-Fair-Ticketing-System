;; Event Waitlist & Notification System
;; Allows users to join waitlists for sold-out events and get automatically enrolled when spots open

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-EVENT-NOT-FOUND (err u301))
(define-constant ERR-WAITLIST-FULL (err u302))
(define-constant ERR-ALREADY-ON-WAITLIST (err u303))
(define-constant ERR-NOT-ON-WAITLIST (err u304))
(define-constant ERR-EVENT-NOT_FULL (err u305))
(define-constant ERR-NOTIFICATION-NOT-FOUND (err u306))
(define-constant ERR-NOTIFICATION_EXPIRED (err u307))
(define-constant ERR-INVALID-PRIORITY (err u308))

;; Waitlist constants
(define-constant MAX-WAITLIST-SIZE u200)
(define-constant NOTIFICATION-DURATION u144) ;; ~1 day to respond
(define-constant PRIORITY-SUBSCRIBER u3)
(define-constant PRIORITY_TIPPER u2)
(define-constant PRIORITY_STANDARD u1)

;; Data structures
(define-data-var next-waitlist-id uint u1)
(define-data-var next-notification-id uint u1)

;; Track waitlist entries for events
(define-map event-waitlists
  {event-id: uint, user: principal}
  {
    position: uint,
    joined-at: uint,
    priority-level: uint,
    notification-preference: uint,
    active: bool
  }
)

;; Track waitlist size per event
(define-map waitlist-stats
  uint
  {
    total-entries: uint,
    active-entries: uint,
    notifications-sent: uint,
    successful-conversions: uint
  }
)

;; Notification queue for available spots
(define-map spot-notifications
  uint
  {
    event-id: uint,
    user: principal,
    created-at: uint,
    expires-at: uint,
    spot-price: uint,
    claimed: bool,
    expired: bool
  }
)

;; User waitlist preferences
(define-map user-waitlist-preferences
  principal
  {
    max-price-increase: uint, ;; Max % price increase willing to pay
    auto-accept: bool,
    preferred-categories: (list 5 (string-ascii 50)),
    notification-cooldown: uint
  }
)

;; Public functions

;; Join waitlist for an event
(define-public (join-waitlist (event-id uint))
  (let
    (
      (user tx-sender)
      (current-block stacks-block-height)
      (waitlist-key {event-id: event-id, user: user})
      (current-stats (default-to 
        {total-entries: u0, active-entries: u0, notifications-sent: u0, successful-conversions: u0}
        (map-get? waitlist-stats event-id)
      ))
      (user-priority (calculate-user-priority user event-id))
      (new-position (+ (get active-entries current-stats) u1))
    )
    (asserts! (event-exists event-id) ERR-EVENT-NOT-FOUND)
    (asserts! (< (get active-entries current-stats) MAX-WAITLIST-SIZE) ERR-WAITLIST-FULL)
    (asserts! (is-none (map-get? event-waitlists waitlist-key)) ERR-ALREADY-ON-WAITLIST)
    
    ;; Add to waitlist
    (map-set event-waitlists waitlist-key {
      position: new-position,
      joined-at: current-block,
      priority-level: user-priority,
      notification-preference: u1, ;; Default notification enabled
      active: true
    })
    
    ;; Update stats
    (map-set waitlist-stats event-id {
      total-entries: (+ (get total-entries current-stats) u1),
      active-entries: (+ (get active-entries current-stats) u1),
      notifications-sent: (get notifications-sent current-stats),
      successful-conversions: (get successful-conversions current-stats)
    })
    
    (ok new-position)
  )
)

;; Leave waitlist
(define-public (leave-waitlist (event-id uint))
  (let
    (
      (user tx-sender)
      (waitlist-key {event-id: event-id, user: user})
      (waitlist-entry (unwrap! (map-get? event-waitlists waitlist-key) ERR-NOT-ON-WAITLIST))
      (current-stats (unwrap! (map-get? waitlist-stats event-id) ERR-EVENT-NOT-FOUND))
    )
    (asserts! (get active waitlist-entry) ERR-NOT-ON-WAITLIST)
    
    ;; Deactivate waitlist entry
    (map-set event-waitlists waitlist-key 
      (merge waitlist-entry {active: false})
    )
    
    ;; Update stats
    (map-set waitlist-stats event-id 
      (merge current-stats {
        active-entries: (- (get active-entries current-stats) u1)
      })
    )
    
    (ok true)
  )
)

;; Notify next waitlist user when spot becomes available
(define-public (notify-waitlist-user (event-id uint))
  (let
    (
      (notification-id (var-get next-notification-id))
      (current-block stacks-block-height)
      (current-price (contract-call? .dynamic-event-pricing get-current-price event-id))
      (next-user (find-next-waitlist-user event-id))
    )
    (asserts! (is-some next-user) ERR-NOT-ON-WAITLIST)
    
    (let
      (
        (user (unwrap-panic next-user))
        (notification-key {event-id: event-id, user: user})
      )
      
      ;; Create notification
      (map-set spot-notifications notification-id {
        event-id: event-id,
        user: user,
        created-at: current-block,
        expires-at: (+ current-block NOTIFICATION-DURATION),
        spot-price: current-price,
        claimed: false,
        expired: false
      })
      
      ;; Update stats
      (let
        (
          (current-stats (unwrap! (map-get? waitlist-stats event-id) ERR-EVENT-NOT-FOUND))
        )
        (map-set waitlist-stats event-id 
          (merge current-stats {
            notifications-sent: (+ (get notifications-sent current-stats) u1)
          })
        )
      )
      
      (var-set next-notification-id (+ notification-id u1))
      (ok notification-id)
    )
  )
)

;; Claim spot from notification
(define-public (claim-waitlist-spot (notification-id uint))
  (let
    (
      (notification (unwrap! (map-get? spot-notifications notification-id) ERR-NOTIFICATION-NOT-FOUND))
      (user tx-sender)
      (current-block stacks-block-height)
      (event-id (get event-id notification))
      (spot-price (get spot-price notification))
    )
    (asserts! (is-eq user (get user notification)) ERR-NOT-AUTHORIZED)
    (asserts! (< current-block (get expires-at notification)) ERR-NOTIFICATION_EXPIRED)
    (asserts! (not (get claimed notification)) ERR-NOTIFICATION_EXPIRED)
    (asserts! (not (get expired notification)) ERR-NOTIFICATION_EXPIRED)
    
    ;; Process payment and register for event
    (try! (contract-call? .dynamic-event-pricing register-for-event event-id))
    
    ;; Mark notification as claimed
    (map-set spot-notifications notification-id 
      (merge notification {claimed: true})
    )
    
    ;; Remove from waitlist
    (let
      (
        (waitlist-key {event-id: event-id, user: user})
        (waitlist-entry (unwrap! (map-get? event-waitlists waitlist-key) ERR-NOT-ON-WAITLIST))
      )
      (map-set event-waitlists waitlist-key 
        (merge waitlist-entry {active: false})
      )
    )
    
    ;; Update conversion stats
    (let
      (
        (current-stats (unwrap! (map-get? waitlist-stats event-id) ERR-EVENT-NOT-FOUND))
      )
      (map-set waitlist-stats event-id 
        (merge current-stats {
          successful-conversions: (+ (get successful-conversions current-stats) u1),
          active-entries: (- (get active-entries current-stats) u1)
        })
      )
    )
    
    (ok true)
  )
)

;; Set user waitlist preferences
(define-public (set-waitlist-preferences 
  (max-price-increase uint)
  (auto-accept bool)
  (categories (list 5 (string-ascii 50)))
  (cooldown uint))
  (begin
    (asserts! (<= max-price-increase u100) ERR-INVALID-PRIORITY) ;; Max 100% increase
    
    (map-set user-waitlist-preferences tx-sender {
      max-price-increase: max-price-increase,
      auto-accept: auto-accept,
      preferred-categories: categories,
      notification-cooldown: cooldown
    })
    
    (ok true)
  )
)

;; Expire old notifications
(define-public (expire-notification (notification-id uint))
  (let
    (
      (notification (unwrap! (map-get? spot-notifications notification-id) ERR-NOTIFICATION-NOT-FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (>= current-block (get expires-at notification)) ERR-NOTIFICATION_EXPIRED)
    (asserts! (not (get claimed notification)) ERR-NOTIFICATION_EXPIRED)
    
    ;; Mark as expired and notify next user
    (map-set spot-notifications notification-id 
      (merge notification {expired: true})
    )
    
    ;; Try to notify next user in waitlist
    (try! (notify-waitlist-user (get event-id notification)))
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-waitlist-entry (event-id uint) (user principal))
  (map-get? event-waitlists {event-id: event-id, user: user})
)

(define-read-only (get-waitlist-stats (event-id uint))
  (map-get? waitlist-stats event-id)
)

(define-read-only (get-notification (notification-id uint))
  (map-get? spot-notifications notification-id)
)

(define-read-only (get-user-preferences (user principal))
  (map-get? user-waitlist-preferences user)
)

(define-read-only (get-waitlist-position (event-id uint) (user principal))
  (match (map-get? event-waitlists {event-id: event-id, user: user})
    entry (get position entry)
    u0
  )
)

;; Helper functions

(define-private (event-exists (event-id uint))
  (is-some (contract-call? .dynamic-event-pricing get-event event-id))
)

(define-private (calculate-user-priority (user principal) (event-id uint))
  (let
    (
      (is-subscriber (contract-call? .art-royalties check-subscription user user)) ;; Simplified check
      (tip-stats (contract-call? .art-royalties get-artist-tip-stats user))
    )
    (if is-subscriber 
      PRIORITY-SUBSCRIBER
      (if (> (get total-received tip-stats) u1000000) ;; 1 STX in tips
        PRIORITY_TIPPER
        PRIORITY_STANDARD
      )
    )
  )
)

(define-private (find-next-waitlist-user (event-id uint))
  ;; Simplified - returns the user with highest priority
  ;; In full implementation would iterate through waitlist by priority and position
  (some tx-sender) ;; Placeholder - would need proper implementation
)
