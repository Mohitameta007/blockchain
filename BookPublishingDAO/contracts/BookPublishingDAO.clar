;; BookPublishing DAO Contract
;; A decentralized autonomous organization for book publishing with reader funding and author revenue sharing

;; Define the token for revenue sharing
(define-fungible-token book-token)

;; Constants
(define-constant contract-owner 'STB44HYPYAT2BB2QE513NSP81HTMYWBJP02HPGK6) ;; replace with your deployer principal
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-book-not-found (err u104))
(define-constant err-already-funded (err u105))

;; Data structures
(define-map books 
  uint 
  {
    author: principal,
    title: (string-ascii 100),
    funding-goal: uint,
    current-funding: uint,
    revenue-share-percentage: uint,
    is-published: bool,
    creation-block: uint
  })

(define-map book-funders 
  {book-id: uint, funder: principal}
  {amount-funded: uint, share-percentage: uint})

(define-map author-revenues principal uint)

;; Data variables
(define-data-var next-book-id uint u1)
(define-data-var total-books-published uint u0)

;; Function 1: Submit Book Proposal with Funding Goal
(define-public (submit-book-proposal 
                 (title (string-ascii 100)) 
                 (funding-goal uint) 
                 (revenue-share-percentage uint))
  (let ((book-id (var-get next-book-id)))  ;; Get current book ID
    (begin
      ;; Validation
      (asserts! (> funding-goal u0) err-invalid-amount)
      (asserts! (<= revenue-share-percentage u100) err-invalid-amount)
      (asserts! (> (len title) u0) err-invalid-amount)

      ;; Add book to map
      (map-set books
        book-id    ;; map key
        {
          author: tx-sender,
          title: title,
          funding-goal: funding-goal,
          current-funding: u0,
          revenue-share-percentage: revenue-share-percentage,
          is-published: false,
          creation-block: stacks-block-height
        })

      ;; Increment book ID
      (var-set next-book-id (+ book-id u1))

      ;; Emit event
      (print {
        event: "book-proposal-submitted",
        book-id: book-id,
        author: tx-sender,
        title: title,
        funding-goal: funding-goal,
        revenue-share-percentage: revenue-share-percentage
      })

      ;; Return book ID
      (ok book-id)
    )
  )
)


;; Function 2: Fund Book Project and Receive Revenue Sharing Rights
(define-public (fund-book-project (book-id uint) (funding-amount uint))
  (let ((book-data (unwrap! (map-get? books book-id) err-book-not-found)))
    (begin
      (asserts! (> funding-amount u0) err-invalid-amount)
      (asserts! (not (get is-published book-data)) err-already-funded)
      
      ;; Transfer STX from funder to contract owner
      (try! (stx-transfer? funding-amount tx-sender contract-owner))
      
      ;; Calculate funder's share percentage based on their contribution
      (let ((total-after-funding (+ (get current-funding book-data) funding-amount))
            (funder-share (/ (* funding-amount u100) (get funding-goal book-data))))
        
        ;; Update book funding status
        (map-set books book-id
          (merge book-data {current-funding: total-after-funding}))
        
        ;; Record funder's contribution and revenue share
        (map-set book-funders 
          {book-id: book-id, funder: tx-sender}
          {
            amount-funded: (+ funding-amount 
                            (get amount-funded 
                                 (default-to {amount-funded: u0, share-percentage: u0}
                                           (map-get? book-funders {book-id: book-id, funder: tx-sender})))) ,
            share-percentage: (+ funder-share
                               (get share-percentage
                                    (default-to {amount-funded: u0, share-percentage: u0}
                                              (map-get? book-funders {book-id: book-id, funder: tx-sender}))))
          })
        
        ;; Check if funding goal is reached and mark as published
        (if (>= total-after-funding (get funding-goal book-data))
          (begin
            (map-set books book-id (merge book-data 
              {current-funding: total-after-funding, is-published: true}))
            (var-set total-books-published (+ (var-get total-books-published) u1))
            (print {
              event: "book-fully-funded",
              book-id: book-id,
              total-funding: total-after-funding
            })
            none)
          (begin
            (print {
              event: "book-funding-progress",
              book-id: book-id,
              current-funding: total-after-funding,
              remaining: (- (get funding-goal book-data) total-after-funding)
            })
            none))
        
        ;; Print funding event
        (print {
          event: "book-funded",
          book-id: book-id,
          funder: tx-sender,
          amount: funding-amount,
          funder-share: funder-share
        })
        
        (ok true)))))


;; ===============================
;; Read-only functions for querying data
;; ===============================

(define-read-only (get-book-details (book-id uint))
  (map-get? books book-id))

(define-read-only (get-funder-info (book-id uint) (funder principal))
  (map-get? book-funders {book-id: book-id, funder: funder}))

(define-read-only (get-total-books-published)
  (var-get total-books-published))

(define-read-only (get-next-book-id)
  (var-get next-book-id))

(define-read-only (get-author-revenue (author principal))
  (default-to u0 (map-get? author-revenues author)))
