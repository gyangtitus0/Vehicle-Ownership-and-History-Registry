(define-data-var contract-owner principal tx-sender)

(define-constant err-not-authorized (err u100))
(define-constant err-vehicle-exists (err u101))
(define-constant err-vehicle-not-found (err u102))
(define-constant err-not-owner (err u103))
(define-constant err-invalid-odometer (err u104))
(define-constant err-invalid-transfer (err u105))

(define-map vehicles
  { vin: (string-ascii 17) }
  {
    owner: principal,
    manufacturer: (string-ascii 50),
    model: (string-ascii 50),
    year: uint,
    initial-registration: uint,
    current-odometer: uint
  }
)

(define-map vehicle-history
  { vin: (string-ascii 17), timestamp: uint }
  {
    event-type: (string-ascii 20),
    previous-owner: (optional principal),
    new-owner: (optional principal),
    previous-odometer: (optional uint),
    new-odometer: (optional uint),
    service-description: (optional (string-ascii 200)),
    accident-description: (optional (string-ascii 200))
  }
)

(define-map vehicle-owners
  { vin: (string-ascii 17), owner: principal }
  { ownership-start: uint, ownership-end: (optional uint) }
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (get-vehicle-details (vin (string-ascii 17)))
  (match (map-get? vehicles { vin: vin })
    vehicle (ok vehicle)
    (err err-vehicle-not-found)
  )
)

(define-read-only (get-vehicle-history-events (vin (string-ascii 17)))
  (map-get? vehicle-history { vin: vin, timestamp: stacks-block-height })
)

(define-read-only (get-vehicle-owner (vin (string-ascii 17)))
  (match (map-get? vehicles { vin: vin })
    vehicle (ok (get owner vehicle))
    (err err-vehicle-not-found)
  )
)

(define-read-only (get-ownership-period (vin (string-ascii 17)) (owner principal))
  (map-get? vehicle-owners { vin: vin, owner: owner })
)

(define-read-only (verify-odometer (vin (string-ascii 17)) (claimed-odometer uint))
  (match (map-get? vehicles { vin: vin })
    vehicle (if (>= claimed-odometer (get current-odometer vehicle))
                (ok true)
                (err err-invalid-odometer))
    (err err-vehicle-not-found)
  )
)

(define-public (register-vehicle 
    (vin (string-ascii 17))
    (manufacturer (string-ascii 50))
    (model (string-ascii 50))
    (year uint)
    (initial-odometer uint))
  (let ((vehicle-exists (is-some (map-get? vehicles { vin: vin }))))
    (if vehicle-exists
        (err err-vehicle-exists)
        (begin
          (map-set vehicles
            { vin: vin }
            {
              owner: tx-sender,
              manufacturer: manufacturer,
              model: model,
              year: year,
              initial-registration: stacks-block-height,
              current-odometer: initial-odometer
            }
          )
          (map-set vehicle-owners
            { vin: vin, owner: tx-sender }
            { ownership-start: stacks-block-height, ownership-end: none }
          )
          (map-set vehicle-history
            { vin: vin, timestamp: stacks-block-height }
            {
              event-type: "registration",
              previous-owner: none,
              new-owner: (some tx-sender),
              previous-odometer: none,
              new-odometer: (some initial-odometer),
              service-description: none,
              accident-description: none
            }
          )
          (ok true)
        )
    )
  )
)

(define-public (transfer-ownership
    (vin (string-ascii 17))
    (new-owner principal)
    (current-odometer uint))
  (match (map-get? vehicles { vin: vin })
    vehicle 
      (if (is-eq tx-sender (get owner vehicle))
          (if (>= current-odometer (get current-odometer vehicle))
              (begin
                (map-set vehicle-owners
                  { vin: vin, owner: tx-sender }
                  { 
                    ownership-start: (get ownership-start (unwrap-panic (map-get? vehicle-owners { vin: vin, owner: tx-sender }))),
                    ownership-end: (some stacks-block-height)
                  }
                )
                (map-set vehicle-owners
                  { vin: vin, owner: new-owner }
                  { ownership-start: stacks-block-height, ownership-end: none }
                )
                (map-set vehicles
                  { vin: vin }
                  (merge vehicle { 
                    owner: new-owner,
                    current-odometer: current-odometer
                  })
                )
                (map-set vehicle-history
                  { vin: vin, timestamp: stacks-block-height }
                  {
                    event-type: "transfer",
                    previous-owner: (some tx-sender),
                    new-owner: (some new-owner),
                    previous-odometer: (some (get current-odometer vehicle)),
                    new-odometer: (some current-odometer),
                    service-description: none,
                    accident-description: none
                  }
                )
                (ok true)
              )
              (err err-invalid-odometer)
          )
          (err err-not-owner)
      )
    (err err-vehicle-not-found)
  )
)

(define-public (update-odometer
    (vin (string-ascii 17))
    (new-odometer uint))
  (match (map-get? vehicles { vin: vin })
    vehicle 
      (if (is-eq tx-sender (get owner vehicle))
          (if (>= new-odometer (get current-odometer vehicle))
              (begin
                (map-set vehicles
                  { vin: vin }
                  (merge vehicle { current-odometer: new-odometer })
                )
                (map-set vehicle-history
                  { vin: vin, timestamp: stacks-block-height }
                  {
                    event-type: "odometer-update",
                    previous-owner: none,
                    new-owner: none,
                    previous-odometer: (some (get current-odometer vehicle)),
                    new-odometer: (some new-odometer),
                    service-description: none,
                    accident-description: none
                  }
                )
                (ok true)
              )
              (err err-invalid-odometer)
          )
          (err err-not-owner)
      )
    (err err-vehicle-not-found)
  )
)

(define-public (record-service
    (vin (string-ascii 17))
    (odometer uint)
    (service-description (string-ascii 200)))
  (match (map-get? vehicles { vin: vin })
    vehicle 
      (if (is-eq tx-sender (get owner vehicle))
          (if (>= odometer (get current-odometer vehicle))
              (begin
                (map-set vehicles
                  { vin: vin }
                  (merge vehicle { current-odometer: odometer })
                )
                (map-set vehicle-history
                  { vin: vin, timestamp: stacks-block-height }
                  {
                    event-type: "service",
                    previous-owner: none,
                    new-owner: none,
                    previous-odometer: (some (get current-odometer vehicle)),
                    new-odometer: (some odometer),
                    service-description: (some service-description),
                    accident-description: none
                  }
                )
                (ok true)
              )
              (err err-invalid-odometer)
          )
          (err err-not-owner)
      )
    (err err-vehicle-not-found)
  )
)

(define-public (record-accident
    (vin (string-ascii 17))
    (odometer uint)
    (accident-description (string-ascii 200)))
  (match (map-get? vehicles { vin: vin })
    vehicle 
      (if (is-eq tx-sender (get owner vehicle))
          (if (>= odometer (get current-odometer vehicle))
              (begin
                (map-set vehicles
                  { vin: vin }
                  (merge vehicle { current-odometer: odometer })
                )
                (map-set vehicle-history
                  { vin: vin, timestamp: stacks-block-height }
                  {
                    event-type: "accident",
                    previous-owner: none,
                    new-owner: none,
                    previous-odometer: (some (get current-odometer vehicle)),
                    new-odometer: (some odometer),
                    service-description: none,
                    accident-description: (some accident-description)
                  }
                )
                (ok true)
              )
              (err err-invalid-odometer)
          )
          (err err-not-owner)
      )
    (err err-vehicle-not-found)
  )
)



(define-map maintenance-schedule
  { vin: (string-ascii 17), service-type: (string-ascii 50) }
  {
    interval-miles: uint,
    last-service: uint,
    next-due: uint
  }
)


(define-public (set-maintenance-schedule 
    (vin (string-ascii 17))
    (service-type (string-ascii 50))
    (interval-miles uint))
  (match (map-get? vehicles { vin: vin })
    vehicle 
      (if (is-eq tx-sender (get owner vehicle))
          (begin
            (map-set maintenance-schedule
              { vin: vin, service-type: service-type }
              {
                interval-miles: interval-miles,
                last-service: (get current-odometer vehicle),
                next-due: (+ (get current-odometer vehicle) interval-miles)
              }
            )
            (ok true))
          (err err-not-owner))
    (err err-vehicle-not-found)))


(define-map vehicle-valuations
  { vin: (string-ascii 17) }
  {
    initial-value: uint,
    annual-depreciation-rate: uint,
    accident-penalty: uint
  }
)

(define-public (set-vehicle-valuation
    (vin (string-ascii 17))
    (initial-value uint)
    (annual-depreciation-rate uint)
    (accident-penalty uint))
  (match (map-get? vehicles { vin: vin })
    vehicle
      (if (is-eq tx-sender (get owner vehicle))
          (begin
            (map-set vehicle-valuations
              { vin: vin }
              {
                initial-value: initial-value,
                annual-depreciation-rate: annual-depreciation-rate,
                accident-penalty: accident-penalty
              }
            )
            (ok true))
          (err err-not-owner))
    (err err-vehicle-not-found)))

(define-read-only (get-current-value (vin (string-ascii 17)))
  (match (map-get? vehicles { vin: vin })
    vehicle
      (match (map-get? vehicle-valuations { vin: vin })
        valuation
          (let ((age (- stacks-block-height (get initial-registration vehicle)))
                (accidents 0)) ;; TODO: Implement accident counting logic
            (ok (- (- (to-int (get initial-value valuation))
                     (to-int (* age (get annual-depreciation-rate valuation))))
                  (* accidents (to-int (get accident-penalty valuation))))))
        (err err-vehicle-not-found))
    (err err-vehicle-not-found)))