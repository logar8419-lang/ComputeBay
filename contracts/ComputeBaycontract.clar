;; title: ComputeBay - Decentralized Compute Auction Platform
;; version: 1.0.0
;; summary: A trustless marketplace for auctioning idle compute resources for AI training jobs
;; description: Enables compute providers to list resources and AI researchers to bid on distributed computing power with cryptographic execution verification and escrow payments

;; traits
(define-trait compute-provider-trait
  (
    (verify-execution ((buff 32)) (response bool uint))
    (get-resource-specs () (response {gpu: uint, cpu: uint, ram: uint} uint))
  )
)

;; token definitions
(define-fungible-token compute-token)

;; constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-RESOURCE-NOT-FOUND (err u101))
(define-constant ERR-AUCTION-NOT-FOUND (err u102))
(define-constant ERR-AUCTION-ENDED (err u103))
(define-constant ERR-AUCTION-ACTIVE (err u104))
(define-constant ERR-BID-TOO-LOW (err u105))
(define-constant ERR-INSUFFICIENT-BALANCE (err u106))
(define-constant ERR-JOB-NOT-FOUND (err u107))
(define-constant ERR-INVALID-PROOF (err u108))
(define-constant ERR-MILESTONE-NOT-READY (err u109))
(define-constant ERR-ALREADY-COMPLETED (err u110))

(define-constant CONTRACT-OWNER tx-sender)
(define-constant AUCTION-DURATION u144) ;; ~24 hours in blocks
(define-constant MIN-REPUTATION u0)
(define-constant MAX-REPUTATION u100)
(define-constant PLATFORM-FEE-RATE u25) ;; 2.5% (25/1000)

;; data vars
(define-data-var next-resource-id uint u1)
(define-data-var next-auction-id uint u1)
(define-data-var next-job-id uint u1)
(define-data-var platform-treasury uint u0)

;; data maps
(define-map compute-resources
  { resource-id: uint }
  {
    provider: principal,
    gpu-count: uint,
    cpu-cores: uint,
    ram-gb: uint,
    hourly-rate: uint,
    available: bool,
    created-at: uint
  }
)

(define-map auctions
  { auction-id: uint }
  {
    requester: principal,
    gpu-requirement: uint,
    cpu-requirement: uint,
    ram-requirement: uint,
    max-duration-hours: uint,
    starting-price: uint,
    current-bid: uint,
    current-bidder: (optional principal),
    end-block: uint,
    ended: bool,
    created-at: uint
  }
)

(define-map jobs
  { job-id: uint }
  {
    auction-id: uint,
    provider: principal,
    requester: principal,
    total-payment: uint,
    milestones: uint,
    completed-milestones: uint,
    execution-proof: (optional (buff 32)),
    status: (string-ascii 20), ;; "active", "completed", "disputed"
    created-at: uint
  }
)

(define-map provider-reputation
  { provider: principal }
  {
    score: uint,
    completed-jobs: uint,
    total-jobs: uint,
    total-earned: uint
  }
)

(define-map escrow-balances
  { job-id: uint, milestone: uint }
  {
    amount: uint,
    released: bool
  }
)

(define-map user-balances
  { user: principal }
  { balance: uint }
)

;; public functions
(define-public (list-compute-resource (gpu-count uint) (cpu-cores uint) (ram-gb uint) (hourly-rate uint))
  (let
    (
      (resource-id (var-get next-resource-id))
    )
    (map-set compute-resources
      { resource-id: resource-id }
      {
        provider: tx-sender,
        gpu-count: gpu-count,
        cpu-cores: cpu-cores,
        ram-gb: ram-gb,
        hourly-rate: hourly-rate,
        available: true,
        created-at: stacks-block-height
      }
    )
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

(define-public (create-auction (gpu-req uint) (cpu-req uint) (ram-req uint) (max-duration uint) (starting-price uint))
  (let
    (
      (auction-id (var-get next-auction-id))
      (end-block (+ stacks-block-height AUCTION-DURATION))
    )
    (map-set auctions
      { auction-id: auction-id }
      {
        requester: tx-sender,
        gpu-requirement: gpu-req,
        cpu-requirement: cpu-req,
        ram-requirement: ram-req,
        max-duration-hours: max-duration,
        starting-price: starting-price,
        current-bid: starting-price,
        current-bidder: none,
        end-block: end-block,
        ended: false,
        created-at: stacks-block-height
      }
    )
    (var-set next-auction-id (+ auction-id u1))
    (ok auction-id)
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) ERR-AUCTION-NOT-FOUND))
      (user-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender }))))
    )
    (asserts! (< stacks-block-height (get end-block auction)) ERR-AUCTION-ENDED)
    (asserts! (not (get ended auction)) ERR-AUCTION-ENDED)
    (asserts! (> bid-amount (get current-bid auction)) ERR-BID-TOO-LOW)
    (asserts! (>= user-balance bid-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Return previous bid to previous bidder if exists
    (match (get current-bidder auction)
      prev-bidder (is-ok (credit-user prev-bidder (get current-bid auction)))
      true
    )
    
    ;; Debit new bidder
    (is-ok (debit-user tx-sender bid-amount))
    
    ;; Update auction
    (map-set auctions
      { auction-id: auction-id }
      (merge auction {
        current-bid: bid-amount,
        current-bidder: (some tx-sender)
      })
    )
    (ok true)
  )
)

(define-public (end-auction (auction-id uint))
  (let
    (
      (auction (unwrap! (map-get? auctions { auction-id: auction-id }) ERR-AUCTION-NOT-FOUND))
      (job-id (var-get next-job-id))
    )
    (asserts! (>= stacks-block-height (get end-block auction)) ERR-AUCTION-ACTIVE)
    (asserts! (not (get ended auction)) ERR-ALREADY-COMPLETED)
    
    (map-set auctions
      { auction-id: auction-id }
      (merge auction { ended: true })
    )
    
    ;; Create job if there was a winning bidder
    (match (get current-bidder auction)
      winner (begin
        (map-set jobs
          { job-id: job-id }
          {
            auction-id: auction-id,
            provider: winner,
            requester: (get requester auction),
            total-payment: (get current-bid auction),
            milestones: u3, ;; Default 3 milestones
            completed-milestones: u0,
            execution-proof: none,
            status: "active",
            created-at: stacks-block-height
          }
        )
        (is-ok (setup-escrow job-id (get current-bid auction) u3))
        (var-set next-job-id (+ job-id u1))
        (ok job-id)
      )
      (ok u0) ;; No winner
    )
  )
)

(define-public (submit-execution-proof (job-id uint) (proof (buff 32)))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-JOB-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get provider job)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status job) "active") ERR-ALREADY-COMPLETED)
    
    (map-set jobs
      { job-id: job-id }
      (merge job {
        execution-proof: (some proof),
        status: "completed"
      })
    )
    (ok true)
  )
)

(define-public (release-milestone (job-id uint) (milestone uint))
  (let
    (
      (job (unwrap! (map-get? jobs { job-id: job-id }) ERR-JOB-NOT-FOUND))
      (escrow (unwrap! (map-get? escrow-balances { job-id: job-id, milestone: milestone }) ERR-MILESTONE-NOT-READY))
      (platform-fee (/ (* (get amount escrow) PLATFORM-FEE-RATE) u1000))
      (provider-payment (- (get amount escrow) platform-fee))
    )
    (asserts! (is-eq tx-sender (get requester job)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get released escrow)) ERR-ALREADY-COMPLETED)
    (asserts! (<= milestone (get milestones job)) ERR-MILESTONE-NOT-READY)
    
    ;; Release payment to provider
    (is-ok (credit-user (get provider job) provider-payment))
    
    ;; Add platform fee to treasury
    (var-set platform-treasury (+ (var-get platform-treasury) platform-fee))
    
    ;; Mark escrow as released
    (map-set escrow-balances
      { job-id: job-id, milestone: milestone }
      (merge escrow { released: true })
    )
    
    ;; Update job milestone count
    (map-set jobs
      { job-id: job-id }
      (merge job { completed-milestones: (+ (get completed-milestones job) u1) })
    )
    
    ;; Update provider reputation if job completed
    (if (is-eq (+ (get completed-milestones job) u1) (get milestones job))
      (is-ok (update-provider-reputation (get provider job) provider-payment))
      true
    )
    (ok true)
  )
)

(define-public (deposit-funds (amount uint))
  (begin
    (try! (ft-transfer? compute-token amount tx-sender (as-contract tx-sender)))
    (is-ok (credit-user tx-sender amount))
    (ok true)
  )
)

(define-public (withdraw-funds (amount uint))
  (let
    (
      (user-balance (default-to u0 (get balance (map-get? user-balances { user: tx-sender }))))
    )
    (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
    (is-ok (debit-user tx-sender amount))
    (try! (as-contract (ft-transfer? compute-token amount tx-sender tx-sender)))
    (ok true)
  )
)

;; read only functions
(define-read-only (get-resource (resource-id uint))
  (map-get? compute-resources { resource-id: resource-id })
)

(define-read-only (get-auction (auction-id uint))
  (map-get? auctions { auction-id: auction-id })
)

(define-read-only (get-job (job-id uint))
  (map-get? jobs { job-id: job-id })
)

(define-read-only (get-provider-reputation (provider principal))
  (default-to
    { score: u50, completed-jobs: u0, total-jobs: u0, total-earned: u0 }
    (map-get? provider-reputation { provider: provider })
  )
)

(define-read-only (get-user-balance (user principal))
  (default-to u0 (get balance (map-get? user-balances { user: user })))
)

(define-read-only (get-escrow-balance (job-id uint) (milestone uint))
  (map-get? escrow-balances { job-id: job-id, milestone: milestone })
)

(define-read-only (get-platform-treasury)
  (var-get platform-treasury)
)

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? auctions { auction-id: auction-id })
    auction (and (< stacks-block-height (get end-block auction)) (not (get ended auction)))
    false
  )
)

;; private functions
(define-private (credit-user (user principal) (amount uint))
  (let
    (
      (current-balance (default-to u0 (get balance (map-get? user-balances { user: user }))))
    )
    (map-set user-balances
      { user: user }
      { balance: (+ current-balance amount) }
    )
    (ok true)
  )
)

(define-private (debit-user (user principal) (amount uint))
  (let
    (
      (current-balance (default-to u0 (get balance (map-get? user-balances { user: user }))))
    )
    (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
    (map-set user-balances
      { user: user }
      { balance: (- current-balance amount) }
    )
    (ok true)
  )
)

(define-private (setup-escrow (job-id uint) (total-amount uint) (milestones uint))
  (let
    (
      (milestone-amount (/ total-amount milestones))
    )
    (is-ok (create-milestone-escrow job-id u1 milestone-amount))
    (is-ok (create-milestone-escrow job-id u2 milestone-amount))
    (is-ok (create-milestone-escrow job-id u3 (- total-amount (* milestone-amount u2))))
    (ok true)
  )
)

(define-private (create-milestone-escrow (job-id uint) (milestone uint) (amount uint))
  (begin
    (map-set escrow-balances
      { job-id: job-id, milestone: milestone }
      { amount: amount, released: false }
    )
    (ok true)
  )
)

(define-private (update-provider-reputation (provider principal) (earned-amount uint))
  (let
    (
      (current-rep (get-provider-reputation provider))
      (new-completed (+ (get completed-jobs current-rep) u1))
      (new-total (+ (get total-jobs current-rep) u1))
      (new-earned (+ (get total-earned current-rep) earned-amount))
      (completion-rate (/ (* new-completed u100) new-total))
      (bounded-completion (if (> completion-rate u100) u100 (if (< completion-rate u0) u0 completion-rate)))
    )
    (map-set provider-reputation
      { provider: provider }
      {
        score: bounded-completion,
        completed-jobs: new-completed,
        total-jobs: new-total,
        total-earned: new-earned
      }
    )
    (ok true)
  )
)
