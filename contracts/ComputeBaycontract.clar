;; title: ComputeBay - Decentralized Compute Auction Platform
;; version: 1.0.0
;; summary: A trustless marketplace for auctioning idle compute resources for AI training jobs
;; description: Enables compute providers to list resources and AI researchers to bid on distributed computing power with cryptographic execution verification and escrow payments

;; traits
(define-trait compute-provider-trait
  (
    (verify-execution (buff 32) (response bool uint))
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