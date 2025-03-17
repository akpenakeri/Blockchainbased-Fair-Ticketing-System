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
