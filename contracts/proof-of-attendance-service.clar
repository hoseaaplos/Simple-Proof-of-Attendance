;; ========================================
;; Proof of Attendance Protocol (POAP)
;; Main contract for event management and attendance verification
;; ========================================

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_EVENT_NOT_FOUND (err u101))
(define-constant ERR_EVENT_ENDED (err u102))
(define-constant ERR_ALREADY_CHECKED_IN (err u103))
(define-constant ERR_INVALID_LOCATION (err u104))
(define-constant ERR_EVENT_NOT_ACTIVE (err u105))
(define-constant ERR_INVALID_PARAMETERS (err u106))

;; Data Variables
(define-data-var next-event-id uint u1)
(define-data-var contract-paused bool false)

;; Event Structure
(define-map events
  { event-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 500),
    organizer: principal,
    start-block: uint,
    end-block: uint,
    location-lat: int,
    location-lng: int,
    location-radius: uint, ;; in meters
    reward-amount: uint,
    max-attendees: uint,
    current-attendees: uint,
    active: bool
  }
)

;; Attendance Records
(define-map attendance
  { event-id: uint, attendee: principal }
  {
    check-in-block: uint,
    location-lat: int,
    location-lng: int,
    reward-claimed: bool
  }
)

;; User Streak Data
(define-map user-streaks
  { user: principal }
  {
    current-streak: uint,
    longest-streak: uint,
    last-event-block: uint,
    total-events: uint
  }
)

;; Event organizer permissions
(define-map event-organizers
  { organizer: principal }
  { authorized: bool }
)

;; Read-only functions

(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id })
)

(define-read-only (get-attendance (event-id uint) (attendee principal))
  (map-get? attendance { event-id: event-id, attendee: attendee })
)

(define-read-only (get-user-streak (user principal))
  (default-to
    { current-streak: u0, longest-streak: u0, last-event-block: u0, total-events: u0 }
    (map-get? user-streaks { user: user })
  )
)

(define-read-only (is-event-active (event-id uint))
  (match (get-event event-id)
    event-data
      (and
        (get active event-data)
        (>= stacks-block-height (get start-block event-data))
        (<= stacks-block-height (get end-block event-data))
      )
    false
  )
)

(define-read-only (calculate-distance (lat1 int) (lng1 int) (lat2 int) (lng2 int))
  ;; Simplified distance calculation (Euclidean approximation)
  ;; In production, you'd want a more accurate haversine formula
  ;; Returns uint for consistent type handling
  (let (
    (lat-diff (if (> lat1 lat2) (- lat1 lat2) (- lat2 lat1)))
    (lng-diff (if (> lng1 lng2) (- lng1 lng2) (- lng2 lng1)))
  )
    (to-uint (+ (* lat-diff lat-diff) (* lng-diff lng-diff)))
  )
)

(define-read-only (is-within-location (event-id uint) (user-lat int) (user-lng int))
  (match (get-event event-id)
    event-data
      (let (
        (distance-squared (calculate-distance
          (get location-lat event-data)
          (get location-lng event-data)
          user-lat
          user-lng))
        (radius-squared (* (get location-radius event-data) (get location-radius event-data)))
      )
        (<= distance-squared radius-squared)
      )
    false
  )
)

(define-read-only (get-next-event-id)
  (var-get next-event-id)
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

;; Administrative functions

(define-public (authorize-organizer (organizer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (map-set event-organizers { organizer: organizer } { authorized: true }))
  )
)

(define-public (revoke-organizer (organizer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (map-set event-organizers { organizer: organizer } { authorized: false }))
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (var-set contract-paused true))
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (ok (var-set contract-paused false))
  )
)

;; Core functions

(define-public (create-event
  (name (string-ascii 100))
  (description (string-ascii 500))
  (start-block uint)
  (end-block uint)
  (location-lat int)
  (location-lng int)
  (location-radius uint)
  (reward-amount uint)
  (max-attendees uint)
)
  (let (
    (event-id (var-get next-event-id))
    (is-authorized (default-to false (get authorized (map-get? event-organizers { organizer: tx-sender }))))
  )
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) is-authorized) ERR_UNAUTHORIZED)
    (asserts! (> end-block start-block) ERR_INVALID_PARAMETERS)
    (asserts! (> max-attendees u0) ERR_INVALID_PARAMETERS)
    (asserts! (> location-radius u0) ERR_INVALID_PARAMETERS)

    (map-set events
      { event-id: event-id }
      {
        name: name,
        description: description,
        organizer: tx-sender,
        start-block: start-block,
        end-block: end-block,
        location-lat: location-lat,
        location-lng: location-lng,
        location-radius: location-radius,
        reward-amount: reward-amount,
        max-attendees: max-attendees,
        current-attendees: u0,
        active: true
      }
    )

    (var-set next-event-id (+ event-id u1))
    (print {
      event: "event-created",
      event-id: event-id,
      organizer: tx-sender,
      name: name
    })
    (ok event-id)
  )
)

(define-public (check-in (event-id uint) (user-lat int) (user-lng int))
  (let (
    (event-data (unwrap! (get-event event-id) ERR_EVENT_NOT_FOUND))
    (existing-attendance (get-attendance event-id tx-sender))
  )
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (is-none existing-attendance) ERR_ALREADY_CHECKED_IN)
    (asserts! (is-event-active event-id) ERR_EVENT_NOT_ACTIVE)
    (asserts! (< (get current-attendees event-data) (get max-attendees event-data)) ERR_EVENT_NOT_ACTIVE)
    (asserts! (is-within-location event-id user-lat user-lng) ERR_INVALID_LOCATION)

    ;; Record attendance
    (map-set attendance
      { event-id: event-id, attendee: tx-sender }
      {
        check-in-block: stacks-block-height,
        location-lat: user-lat,
        location-lng: user-lng,
        reward-claimed: false
      }
    )

    ;; Update event attendance count
    (map-set events
      { event-id: event-id }
      (merge event-data { current-attendees: (+ (get current-attendees event-data) u1) })
    )

    ;; Update user streak
    (update-user-streak tx-sender)

    (print {
      event: "checked-in",
      event-id: event-id,
      attendee: tx-sender,
      block: stacks-block-height
    })
    (ok true)
  )
)

(define-private (update-user-streak (user principal))
  (let (
    (current-streak-data (get-user-streak user))
    (current-block stacks-block-height)
    (last-block (get last-event-block current-streak-data))
    (blocks-since-last (if (> current-block last-block) (- current-block last-block) u0))
    (streak-broken (> blocks-since-last u1440)) ;; ~24 hours in blocks (assuming 10min blocks)
    (new-streak (if streak-broken u1 (+ (get current-streak current-streak-data) u1)))
    (new-longest (if (> new-streak (get longest-streak current-streak-data))
                    new-streak
                    (get longest-streak current-streak-data)))
  )
    (map-set user-streaks
      { user: user }
      {
        current-streak: new-streak,
        longest-streak: new-longest,
        last-event-block: current-block,
        total-events: (+ (get total-events current-streak-data) u1)
      }
    )
  )
)

(define-public (claim-reward (event-id uint))
  (let (
    (event-data (unwrap! (get-event event-id) ERR_EVENT_NOT_FOUND))
    (attendance-data (unwrap! (get-attendance event-id tx-sender) ERR_EVENT_NOT_FOUND))
  )
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (not (get reward-claimed attendance-data)) ERR_ALREADY_CHECKED_IN)
    (asserts! (> stacks-block-height (get end-block event-data)) ERR_EVENT_NOT_ACTIVE)

    ;; Mark reward as claimed
    (map-set attendance
      { event-id: event-id, attendee: tx-sender }
      (merge attendance-data { reward-claimed: true })
    )

    ;; In a full implementation, you would mint tokens or transfer rewards here
    ;; For now, we'll just emit an event
    (print {
      event: "reward-claimed",
      event-id: event-id,
      attendee: tx-sender,
      amount: (get reward-amount event-data)
    })
    (ok (get reward-amount event-data))
  )
)

(define-public (deactivate-event (event-id uint))
  (let (
    (event-data (unwrap! (get-event event-id) ERR_EVENT_NOT_FOUND))
  )
    (asserts! (not (var-get contract-paused)) ERR_UNAUTHORIZED)
    (asserts! (is-eq tx-sender (get organizer event-data)) ERR_UNAUTHORIZED)

    (map-set events
      { event-id: event-id }
      (merge event-data { active: false })
    )

    (print { event: "event-deactivated", event-id: event-id })
    (ok true)
  )
)

;; ========================================
;; POAP Token Contract
;; SIP-010 compliant token for attendance rewards
;; ========================================

;; Token Constants
(define-constant TOKEN_NAME "Proof of Attendance Protocol")
(define-constant TOKEN_SYMBOL "POAP")
(define-constant TOKEN_DECIMALS u6)

;; Token Errors
(define-constant ERR_NOT_TOKEN_OWNER (err u200))
(define-constant ERR_INSUFFICIENT_BALANCE (err u201))
(define-constant ERR_INVALID_RECIPIENT (err u202))

;; Token Data
(define-data-var token-total-supply uint u0)
(define-data-var token-contract-owner principal tx-sender)

;; Token Maps
(define-map token-balances principal uint)
(define-map token-allowances { owner: principal, spender: principal } uint)

;; Token Trait Implementation
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Token Read-only Functions
(define-read-only (get-name)
  (ok TOKEN_NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN_SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN_DECIMALS)
)

(define-read-only (get-balance (account principal))
  (ok (default-to u0 (map-get? token-balances account)))
)

(define-read-only (get-total-supply)
  (ok (var-get token-total-supply))
)

(define-read-only (get-token-uri)
  (ok (some u"https://poap.stacks.co/metadata"))
)

(define-read-only (get-allowance (owner principal) (spender principal))
  (default-to u0 (map-get? token-allowances { owner: owner, spender: spender }))
)

;; Token Public Functions
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (is-eq tx-sender sender) ERR_NOT_TOKEN_OWNER)
    (asserts! (not (is-eq sender recipient)) ERR_INVALID_RECIPIENT)

    (let (
      (sender-balance (unwrap! (get-balance sender) ERR_INSUFFICIENT_BALANCE))
    )
      (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)

      (try! (transfer-helper sender recipient amount))

      (match memo to-print (print to-print) 0x)
      (print {
        event: "token-transfer",
        sender: sender,
        recipient: recipient,
        amount: amount
      })
      (ok true)
    )
  )
)

(define-public (approve (spender principal) (amount uint))
  (begin
    (map-set token-allowances
      { owner: tx-sender, spender: spender }
      amount
    )
    (print {
      event: "token-approval",
      owner: tx-sender,
      spender: spender,
      amount: amount
    })
    (ok true)
  )
)

(define-public (transfer-from (amount uint) (owner principal) (recipient principal) (memo (optional (buff 34))))
  (let (
    (allowance (get-allowance owner tx-sender))
    (owner-balance (unwrap! (get-balance owner) ERR_INSUFFICIENT_BALANCE))
  )
    (asserts! (>= allowance amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (>= owner-balance amount) ERR_INSUFFICIENT_BALANCE)

    (try! (transfer-helper owner recipient amount))

    (map-set token-allowances
      { owner: owner, spender: tx-sender }
      (- allowance amount)
    )

    (match memo to-print (print to-print) 0x)
    (print {
      event: "token-transfer-from",
      owner: owner,
      recipient: recipient,
      amount: amount,
      spender: tx-sender
    })
    (ok true)
  )
)

;; Token Private Functions
(define-private (transfer-helper (sender principal) (recipient principal) (amount uint))
  (let (
    (sender-balance (unwrap! (get-balance sender) ERR_INSUFFICIENT_BALANCE))
    (recipient-balance (unwrap! (get-balance recipient) ERR_INSUFFICIENT_BALANCE))
  )
    (map-set token-balances sender (- sender-balance amount))
    (map-set token-balances recipient (+ recipient-balance amount))
    (ok true)
  )
)

;; Minting function (restricted to contract owner or authorized minters)
(define-public (mint (recipient principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get token-contract-owner)) ERR_NOT_TOKEN_OWNER)

    (let (
      (recipient-balance (unwrap! (get-balance recipient) ERR_INSUFFICIENT_BALANCE))
    )
      (map-set token-balances recipient (+ recipient-balance amount))
      (var-set token-total-supply (+ (var-get token-total-supply) amount))

      (print {
        event: "token-mint",
        recipient: recipient,
        amount: amount
      })
      (ok true)
    )
  )
)

;; Burn function
(define-public (burn (amount uint))
  (let (
    (sender-balance (unwrap! (get-balance tx-sender) ERR_INSUFFICIENT_BALANCE))
  )
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)

    (map-set token-balances tx-sender (- sender-balance amount))
    (var-set token-total-supply (- (var-get token-total-supply) amount))

    (print {
      event: "token-burn",
      burner: tx-sender,
      amount: amount
    })
    (ok true)
  )
)

;; Integration function to mint rewards for attendance
(define-public (mint-attendance-reward (event-id uint) (attendee principal))
  (let (
    (event-data (unwrap! (get-event event-id) ERR_EVENT_NOT_FOUND))
    (attendance-data (unwrap! (get-attendance event-id attendee) ERR_EVENT_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (var-get token-contract-owner)) ERR_NOT_TOKEN_OWNER)
    (asserts! (not (get reward-claimed attendance-data)) ERR_ALREADY_CHECKED_IN)

    ;; Mint tokens based on event reward amount
    (try! (mint attendee (get reward-amount event-data)))

    ;; Mark reward as claimed in the attendance record
    (map-set attendance
      { event-id: event-id, attendee: attendee }
      (merge attendance-data { reward-claimed: true })
    )

    (ok true)
  )
)
