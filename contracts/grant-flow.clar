;; ------------------------------------------------------------
;; Contract: EduFund Protocol
;; Type: Decentralized Education Sponsorship & Grant Platform
;; Network: Stacks Blockchain
;; Author: [Your Name]
;; Version: 1.0
;; ------------------------------------------------------------
;; Description:
;; A decentralized platform that connects sponsors directly to students.
;; Sponsors can fund verified students' education proposals, 
;; with funds released based on milestone verification.
;; ------------------------------------------------------------

;; ------------------------------
;; DATA VARIABLES
;; ------------------------------

(define-data-var proposal-counter uint u0)

;; ------------------------------
;; DATA MAPS
;; ------------------------------

;; Registered students
(define-map students
  principal
  {
    name: (string-ascii 50),
    institution: (string-ascii 100),
    verified: bool
  }
)

;; Funding proposals
(define-map proposals
  uint
  {
    student: principal,
    amount: uint,
    institution: (string-ascii 100),
    purpose: (string-ascii 200),
    funded: uint,
    active: bool
  }
)

;; Record of each sponsor's contribution per proposal
(define-map contributions
  { proposal-id: uint, sponsor: principal }
  uint
)

;; Milestone verification (tracks progress for fund release)
(define-map milestones
  uint
  {
    proposal-id: uint,
    verified: bool,
    released: bool
  }
)

;; ------------------------------
;; ERROR CODES
;; ------------------------------

(define-constant ERR-STUDENT-EXISTS (err u100))
(define-constant ERR-NOT-REGISTERED (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-NOT-AUTHORIZED (err u103))
(define-constant ERR-INACTIVE (err u104))
(define-constant ERR-INSUFFICIENT-FUND (err u105))
(define-constant ERR-ALREADY-VERIFIED (err u106))
(define-constant ERR-ALREADY-RELEASED (err u107))

;; ------------------------------
;; STUDENT FUNCTIONS
;; ------------------------------

(define-public (register-student (name (string-ascii 50)) (institution (string-ascii 100)))
  (if (is-some (map-get? students tx-sender))
      ERR-STUDENT-EXISTS
      (begin
        (map-set students tx-sender
          { name: name, institution: institution, verified: false })
        (ok "Student registered successfully")
      )
  )
)

(define-public (verify-student (student principal))
  ;; In real deployment, verification could be DAO/oracle-driven
  (let ((s (unwrap! (map-get? students student) ERR-NOT-REGISTERED)))
    (map-set students student (merge s { verified: true }))
    (ok "Student verified successfully")
  )
)

;; ------------------------------
;; PROPOSAL FUNCTIONS
;; ------------------------------

(define-public (create-proposal (amount uint) (institution (string-ascii 100)) (purpose (string-ascii 200)))
  (let ((student (unwrap! (map-get? students tx-sender) ERR-NOT-REGISTERED)))
    (if (not (get verified student))
        (err u108) ;; student not verified
        (let ((id (+ u1 (var-get proposal-counter))))
          (var-set proposal-counter id)
          (map-set proposals id
            {
              student: tx-sender,
              amount: amount,
              institution: institution,
              purpose: purpose,
              funded: u0,
              active: true
            })
          (ok id)
        )
    )
  )
)

(define-public (close-proposal (proposal-id uint))
  (let ((p (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    (if (is-eq tx-sender (get student p))
        (begin
          (map-set proposals proposal-id (merge p { active: false }))
          (ok "Proposal closed successfully")
        )
        ERR-NOT-AUTHORIZED
    )
  )
)

;; ------------------------------
;; FUNDING & CONTRIBUTION
;; ------------------------------

(define-public (fund-proposal (proposal-id uint) (amount uint))
  (let ((p (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    (if (not (get active p))
        ERR-INACTIVE
        (begin
          (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
          (map-set contributions { proposal-id: proposal-id, sponsor: tx-sender } amount)
          (map-set proposals proposal-id (merge p { funded: (+ (get funded p) amount) }))
          (ok "Proposal funded successfully")
        )
    )
  )
)

;; ------------------------------
;; MILESTONE & PAYOUT MANAGEMENT
;; ------------------------------

(define-public (verify-milestone (proposal-id uint))
  ;; Oracle/DAO verification simulation
  (let ((m (map-get? milestones proposal-id)))
    (if (is-some m)
        ERR-ALREADY-VERIFIED
        (begin
          (map-set milestones proposal-id { proposal-id: proposal-id, verified: true, released: false })
          (ok "Milestone verified successfully")
        )
    )
  )
)

(define-public (release-funds (proposal-id uint))
  (let (
        (p (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (m (unwrap! (map-get? milestones proposal-id) ERR-PROPOSAL-NOT-FOUND))
      )
    (if (not (get verified m))
        (err u109) ;; not verified yet
        (if (get released m)
            ERR-ALREADY-RELEASED
            (begin
              (try! (stx-transfer? (get funded p) (as-contract tx-sender) (get student p)))
              (map-set milestones proposal-id (merge m { released: true }))
              (map-set proposals proposal-id (merge p { active: false }))
              (ok "Funds released successfully to student")
            )
        )
    )
  )
)

;; ------------------------------
;; READ-ONLY FUNCTIONS
;; ------------------------------

(define-read-only (get-student (student principal))
  (map-get? students student)
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-funding (proposal-id uint))
  (let ((opt (map-get? proposals proposal-id)))
    (if (is-some opt)
        (get funded (unwrap-panic opt))
        u0))
)

(define-read-only (get-contribution (proposal-id uint) (sponsor principal))
  (map-get? contributions { proposal-id: proposal-id, sponsor: sponsor })
)

(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

;; ------------------------------------------------------------
;; END OF CONTRACT
;; ------------------------------------------------------------
