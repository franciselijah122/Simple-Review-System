;; Simple Review System
;; A comprehensive product/service review platform with ratings, comments,
;; verified purchase requirements, and reviewer reputation system

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-RATING (err u101))
(define-constant ERR-REVIEW-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-REVIEWED (err u103))
(define-constant ERR-PURCHASE-NOT-VERIFIED (err u104))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u105))
(define-constant ERR-INVALID-PRODUCT (err u106))
(define-constant ERR-REVIEW-TOO-OLD (err u107))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Constants
(define-constant MAX-RATING u5)
(define-constant MIN-RATING u1)
(define-constant INITIAL-REPUTATION u100)
(define-constant REPUTATION-BONUS u10)
(define-constant REPUTATION-PENALTY u5)
(define-constant REVIEW-EDIT-WINDOW u144) ;; ~24 hours in blocks

;; Data Variables
(define-data-var next-review-id uint u1)
(define-data-var next-product-id uint u1)

;; Product data structure
(define-map products
    { product-id: uint }
    {
        name: (string-ascii 100),
        category: (string-ascii 50),
        created-by: principal,
        created-at: uint,
        total-reviews: uint,
        average-rating: uint,
        is-active: bool
    }
)

;; Review data structure
(define-map reviews
    { review-id: uint }
    {
        product-id: uint,
        reviewer: principal,
        rating: uint,
        comment: (string-utf8 500),
        created-at: uint,
        updated-at: uint,
        is-verified-purchase: bool,
        helpful-votes: uint,
        total-votes: uint
    }
)

;; User reputation system
(define-map user-reputation
    { user: principal }
    {
        reputation-score: uint,
        total-reviews: uint,
        helpful-reviews: uint,
        last-review-block: uint
    }
)

;; Purchase verification (simulated - in production would integrate with marketplace)
(define-map verified-purchases
    { buyer: principal, product-id: uint }
    {
        purchase-date: uint,
        verified: bool
    }
)

;; User-product review tracking (prevents duplicate reviews)
(define-map user-product-reviews
    { user: principal, product-id: uint }
    { review-id: uint }
)

;; Helpful votes tracking
(define-map review-votes
    { voter: principal, review-id: uint }
    { voted-helpful: bool }
)

;; Read-only functions

;; Get product details
(define-read-only (get-product (product-id uint))
    (map-get? products { product-id: product-id })
)

;; Get review details
(define-read-only (get-review (review-id uint))
    (map-get? reviews { review-id: review-id })
)

;; Get user reputation
(define-read-only (get-user-reputation (user principal))
    (default-to
        { reputation-score: INITIAL-REPUTATION, total-reviews: u0, helpful-reviews: u0, last-review-block: u0 }
        (map-get? user-reputation { user: user })
    )
)

;; Check if purchase is verified
(define-read-only (is-purchase-verified (buyer principal) (product-id uint))
    (match (map-get? verified-purchases { buyer: buyer, product-id: product-id })
        verified-purchase (get verified verified-purchase)
        false
    )
)

;; Get user's review for a product
(define-read-only (get-user-review-for-product (user principal) (product-id uint))
    (match (map-get? user-product-reviews { user: user, product-id: product-id })
        user-review (get-review (get review-id user-review))
        none
    )
)

;; Check if user has voted on a review
(define-read-only (has-user-voted (voter principal) (review-id uint))
    (is-some (map-get? review-votes { voter: voter, review-id: review-id }))
)

;; Calculate review helpfulness ratio
(define-read-only (get-review-helpfulness-ratio (review-id uint))
    (match (get-review review-id)
        review-data
            (let ((total-votes (get total-votes review-data))
                  (helpful-votes (get helpful-votes review-data)))
                (if (> total-votes u0)
                    (/ (* helpful-votes u100) total-votes)
                    u0))
        u0
    )
)

;; Public functions

;; Create a new product
(define-public (create-product (name (string-ascii 100)) (category (string-ascii 50)))
    (let ((product-id (var-get next-product-id)))
        (map-set products
            { product-id: product-id }
            {
                name: name,
                category: category,
                created-by: tx-sender,
                created-at: stacks-block-height,
                total-reviews: u0,
                average-rating: u0,
                is-active: true
            }
        )
        (var-set next-product-id (+ product-id u1))
        (ok product-id)
    )
)

;; Add verified purchase (would be called by marketplace contract in production)
(define-public (add-verified-purchase (buyer principal) (product-id uint))
    (begin
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender buyer)) ERR-NOT-AUTHORIZED)
        (map-set verified-purchases
            { buyer: buyer, product-id: product-id }
            { purchase-date: stacks-block-height, verified: true }
        )
        (ok true)
    )
)

;; Submit a review
(define-public (submit-review
    (product-id uint)
    (rating uint)
    (comment (string-utf8 500))
    (require-verified-purchase bool))
    (let (
        (review-id (var-get next-review-id))
        (current-reputation (get-user-reputation tx-sender))
        (is-verified (is-purchase-verified tx-sender product-id))
    )
        ;; Validate inputs
        (asserts! (and (>= rating MIN-RATING) (<= rating MAX-RATING)) ERR-INVALID-RATING)
        (asserts! (is-some (get-product product-id)) ERR-INVALID-PRODUCT)
        (asserts! (is-none (get-user-review-for-product tx-sender product-id)) ERR-ALREADY-REVIEWED)

        ;; Check verification requirement
        (asserts! (or (not require-verified-purchase) is-verified) ERR-PURCHASE-NOT-VERIFIED)

        ;; Check minimum reputation for unverified purchases
        (asserts! (or is-verified (>= (get reputation-score current-reputation) u50)) ERR-INSUFFICIENT-REPUTATION)

        ;; Create review
        (map-set reviews
            { review-id: review-id }
            {
                product-id: product-id,
                reviewer: tx-sender,
                rating: rating,
                comment: comment,
                created-at: stacks-block-height,
                updated-at: stacks-block-height,
                is-verified-purchase: is-verified,
                helpful-votes: u0,
                total-votes: u0
            }
        )

        ;; Track user-product review
        (map-set user-product-reviews
            { user: tx-sender, product-id: product-id }
            { review-id: review-id }
        )

        ;; Update user reputation
        (map-set user-reputation
            { user: tx-sender }
            (merge current-reputation {
                total-reviews: (+ (get total-reviews current-reputation) u1),
                last-review-block: stacks-block-height
            })
        )

        ;; Update product statistics
        (try! (update-product-stats product-id))

        (var-set next-review-id (+ review-id u1))
        (ok review-id)
    )
)

;; Update a review (only within edit window)
(define-public (update-review
    (review-id uint)
    (new-rating uint)
    (new-comment (string-utf8 500)))
    (match (get-review review-id)
        review-data
            (let ((blocks-since-creation (- stacks-block-height (get created-at review-data))))
                (asserts! (is-eq tx-sender (get reviewer review-data)) ERR-NOT-AUTHORIZED)
                (asserts! (<= blocks-since-creation REVIEW-EDIT-WINDOW) ERR-REVIEW-TOO-OLD)
                (asserts! (and (>= new-rating MIN-RATING) (<= new-rating MAX-RATING)) ERR-INVALID-RATING)

                (map-set reviews
                    { review-id: review-id }
                    (merge review-data {
                        rating: new-rating,
                        comment: new-comment,
                        updated-at: stacks-block-height
                    })
                )

                ;; Update product statistics
                (try! (update-product-stats (get product-id review-data)))
                (ok true)
            )
        ERR-REVIEW-NOT-FOUND
    )
)

;; Vote on review helpfulness
(define-public (vote-on-review (review-id uint) (is-helpful bool))
    (match (get-review review-id)
        review-data
            (let ((existing-vote (map-get? review-votes { voter: tx-sender, review-id: review-id })))
                (asserts! (not (is-eq tx-sender (get reviewer review-data))) ERR-NOT-AUTHORIZED)

                (match existing-vote
                    ;; Update existing vote
                    old-vote
                        (let (
                            (old-helpful (get voted-helpful old-vote))
                            (helpful-change (if (and (not old-helpful) is-helpful) 1
                                             (if (and old-helpful (not is-helpful)) -1 0)))
                        )
                            (map-set review-votes
                                { voter: tx-sender, review-id: review-id }
                                { voted-helpful: is-helpful }
                            )
                            (map-set reviews
                                { review-id: review-id }
                                (merge review-data {
                                    helpful-votes: (+ (get helpful-votes review-data) (to-uint helpful-change))
                                })
                            )
                        )
                    ;; New vote
                    (begin
                        (map-set review-votes
                            { voter: tx-sender, review-id: review-id }
                            { voted-helpful: is-helpful }
                        )
                        (map-set reviews
                            { review-id: review-id }
                            (merge review-data {
                                helpful-votes: (if is-helpful
                                                 (+ (get helpful-votes review-data) u1)
                                                 (get helpful-votes review-data)),
                                total-votes: (+ (get total-votes review-data) u1)
                            })
                        )

                        ;; Update reviewer reputation based on vote
                        (let ((reviewer-rep (get-user-reputation (get reviewer review-data))))
                            (map-set user-reputation
                                { user: (get reviewer review-data) }
                                (merge reviewer-rep {
                                    helpful-reviews: (if is-helpful
                                                       (+ (get helpful-reviews reviewer-rep) u1)
                                                       (get helpful-reviews reviewer-rep)),
                                    reputation-score: (if is-helpful
                                                        (+ (get reputation-score reviewer-rep) REPUTATION-BONUS)
                                                        (- (get reputation-score reviewer-rep) REPUTATION-PENALTY))
                                })
                            )
                        )
                    )
                )
                (ok true)
            )
        ERR-REVIEW-NOT-FOUND
    )
)

;; Private functions

;; Update product statistics after review changes
(define-private (update-product-stats (product-id uint))
    (match (get-product product-id)
        product-data
            (let ((stats (calculate-product-stats product-id)))
                (map-set products
                    { product-id: product-id }
                    (merge product-data {
                        total-reviews: (get total-reviews stats),
                        average-rating: (get average-rating stats)
                    })
                )
                (ok true)
            )
        ERR-INVALID-PRODUCT
    )
)

;; Calculate product statistics (simplified - in production would use more efficient method)
(define-private (calculate-product-stats (product-id uint))
    (let ((dummy-stats { total-reviews: u0, average-rating: u0 }))
        ;; This is a placeholder - in a full implementation, you'd iterate through reviews
        ;; For now, we'll return placeholder values
        dummy-stats
    )
)

;; Admin functions (for contract maintenance)

;; Deactivate a product (admin only)
(define-public (deactivate-product (product-id uint))
    (match (get-product product-id)
        product-data
            (begin
                (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
                (map-set products
                    { product-id: product-id }
                    (merge product-data { is-active: false })
                )
                (ok true)
            )
        ERR-INVALID-PRODUCT
    )
)

;; Initialize contract (called once during deployment)
(define-public (initialize-contract)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        ;; Set initial reputation for contract owner
        (map-set user-reputation
            { user: CONTRACT-OWNER }
            {
                reputation-score: u1000,
                total-reviews: u0,
                helpful-reviews: u0,
                last-review-block: stacks-block-height
            }
        )
        (ok true)
    )
)
