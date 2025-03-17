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
