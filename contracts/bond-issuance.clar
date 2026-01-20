;; bond-issuance.clar
;; ------------------------------------------------------------
;; Bond Issuance Contract for STX Projects
;; - Issuer creates bonds with principal, interest, and maturity
;; - Investors purchase bonds by sending STX
;; - Investors redeem principal + interest after maturity
;; ------------------------------------------------------------

(define-constant ERR_NOT_ISSUER u100)
(define-constant ERR_BOND_NOT_FOUND u101)
(define-constant ERR_BEFORE_MATURITY u102)
(define-constant ERR_INSUFFICIENT_FUNDS u103)
(define-constant ERR_BOND_INACTIVE u104)
(define-constant ERR_INVALID_AMOUNT u105)
(define-constant ERR_NO_FUNDS u106)

;; Issuer of the bonds
(define-data-var issuer principal tx-sender)

;; Bond structure: bond-id -> { principal, interest-rate-bps, maturity-block, active }
;; interest-rate-bps: basis points (e.g., 500 = 5%)
(define-map bonds
  { bond-id: uint }
  {
    principal: uint,
    interest-rate-bps: uint,
    maturity-block: uint,
    active: bool
  })

;; Investor holdings: { bond-id, investor } -> amount of bonds held
(define-map holdings
  { bond-id: uint, investor: principal }
  { amount: uint })

;; Bond id counter
(define-data-var next-bond-id uint u1)

;; Events
(define-private (ev-bond-issued (bond-id uint) (principal uint) (rate uint) (maturity uint))
  (print { event: "bond-issued", bond_id: bond-id, principal: principal, interest_rate_bps: rate, maturity: maturity }))

(define-private (ev-bond-purchased (bond-id uint) (investor principal) (amount uint))
  (print { event: "bond-purchased", bond_id: bond-id, investor: investor, amount: amount }))

(define-private (ev-bond-redeemed (bond-id uint) (investor principal) (amount uint) (interest uint))
  (print { event: "bond-redeemed", bond_id: bond-id, investor: investor, amount: amount, interest: interest }))

(define-private (ev-bond-deactivated (bond-id uint))
  (print { event: "bond-deactivated", bond_id: bond-id }))

;; -------------------------
;; Issuer functions
;; -------------------------
(define-public (issue-bond (principal uint) (interest-rate-bps uint) (maturity-block uint))
  (if (not (is-eq tx-sender (var-get issuer)))
    (err ERR_NOT_ISSUER)
    (if (not (> principal u0))
      (err ERR_BOND_NOT_FOUND)
      (if (not (> maturity-block burn-block-height))
        (err ERR_INVALID_AMOUNT)
        (let ((bond-id (var-get next-bond-id)))
          (var-set next-bond-id (+ bond-id u1))
          (map-set bonds { bond-id: bond-id } { principal: principal, interest-rate-bps: interest-rate-bps, maturity-block: maturity-block, active: true })
          (ev-bond-issued bond-id principal interest-rate-bps maturity-block)
          (ok bond-id))))))

(define-public (deactivate-bond (bond-id uint))
  (let ((b? (map-get? bonds { bond-id: bond-id })))
    (asserts! (is-some b?) (err ERR_BOND_NOT_FOUND))
    (asserts! (is-eq tx-sender (var-get issuer)) (err ERR_NOT_ISSUER))
    (let ((b (unwrap-panic b?)))
      (map-set bonds { bond-id: bond-id } (merge b { active: false }))
      (ev-bond-deactivated bond-id)
      (ok true))))

;; -------------------------
;; Investor functions
;; -------------------------
;; Purchase bond by sending STX equal to principal * amount
(define-public (purchase-bond (bond-id uint) (amount uint))
  (let ((b? (map-get? bonds { bond-id: bond-id })))
    (asserts! (is-some b?) (err ERR_BOND_NOT_FOUND))
    (let ((b (unwrap-panic b?)))
      (asserts! (get active b) (err ERR_BOND_INACTIVE))
      (let ((total-cost (* (get principal b) amount)))
        (asserts! (is-ok (stx-transfer? total-cost tx-sender (as-contract tx-sender))) (err ERR_INSUFFICIENT_FUNDS))
        ;; update holdings
        (let ((h? (map-get? holdings { bond-id: bond-id, investor: tx-sender })))
          (if (is-some h?)
              (map-set holdings { bond-id: bond-id, investor: tx-sender } { amount: (+ (get amount (unwrap-panic h?)) amount) })
              (map-set holdings { bond-id: bond-id, investor: tx-sender } { amount: amount })))
        (ev-bond-purchased bond-id tx-sender amount)
        (ok true)))))

;; Redeem bond (principal + interest) after maturity
(define-public (redeem-bond (bond-id uint))
  (let ((b? (map-get? bonds { bond-id: bond-id })))
    (asserts! (is-some b?) (err ERR_BOND_NOT_FOUND))
    (let ((b (unwrap-panic b?)))
      (asserts! (>= burn-block-height (get maturity-block b)) (err ERR_BEFORE_MATURITY))
      (let ((h? (map-get? holdings { bond-id: bond-id, investor: tx-sender })))
        (asserts! (is-some h?) (err ERR_NO_FUNDS))
        (let ((amount (get amount (unwrap-panic h?)))
              (principal (get principal b))
              (rate (get interest-rate-bps b)))
          (map-delete holdings { bond-id: bond-id, investor: tx-sender })
          ;; calculate interest
          (let ((interest (/ (* principal amount rate) u10000))) ;; basis points
            (asserts! (is-ok (stx-transfer? (+ (* principal amount) interest) (as-contract tx-sender) tx-sender)) (err ERR_INSUFFICIENT_FUNDS))
            (ev-bond-redeemed bond-id tx-sender amount interest)
            (ok (+ (* principal amount) interest))))))))

;; -------------------------
;; Views
;; -------------------------
(define-read-only (get-bond (bond-id uint))
  (ok (map-get? bonds { bond-id: bond-id })))

(define-read-only (get-holding (bond-id uint) (investor principal))
  (ok (map-get? holdings { bond-id: bond-id, investor: investor })))

(define-read-only (get-next-bond-id) (ok (var-get next-bond-id)))
