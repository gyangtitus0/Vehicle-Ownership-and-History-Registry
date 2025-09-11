;; Vehicle Recall Management System
;; Tracks manufacturer recalls, affected vehicles, and recall completion status

;; Error constants
(define-constant err-not-authorized (err u200))
(define-constant err-recall-not-found (err u201))
(define-constant err-recall-exists (err u202))
(define-constant err-vehicle-not-found (err u203))
(define-constant err-not-manufacturer (err u204))
(define-constant err-already-completed (err u205))
(define-constant err-recall-not-applicable (err u206))
(define-constant err-invalid-severity (err u207))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Recall counter
(define-data-var recall-counter uint u0)

;; Authorized manufacturers for issuing recalls
(define-map authorized-manufacturers
  { manufacturer: principal }
  {
    company-name: (string-ascii 100),
    authorized: bool,
    total-recalls-issued: uint
  }
)

;; Vehicle recall definitions
(define-map vehicle-recalls
  { recall-id: (string-ascii 30) }
  {
    manufacturer: principal,
    recall-number: (string-ascii 50),
    issue-date: uint,
    title: (string-ascii 100),
    description: (string-ascii 300),
    affected-models: (string-ascii 200),
    affected-years-start: uint,
    affected-years-end: uint,
    severity-level: (string-ascii 20),
    estimated-affected-vehicles: uint,
    remedy-description: (string-ascii 250),
    completion-deadline: uint,
    nhtsa-number: (optional (string-ascii 20))
  }
)

;; Vehicle recall status tracking
(define-map vehicle-recall-status
  { vin: (string-ascii 17), recall-id: (string-ascii 30) }
  {
    notification-sent: bool,
    notification-date: uint,
    owner-acknowledged: bool,
    acknowledgment-date: (optional uint),
    recall-completed: bool,
    completion-date: (optional uint),
    completion-facility: (optional principal),
    completion-odometer: (optional uint)
  }
)

;; Vehicle recall summary
(define-map vehicle-recall-summary
  { vin: (string-ascii 17) }
  {
    total-recalls: uint,
    completed-recalls: uint,
    pending-recalls: uint,
    last-recall-date: (optional uint)
  }
)

;; Owner notification preferences
(define-map owner-notification-prefs
  { owner: principal }
  {
    email-notifications: bool,
    urgent-only: bool,
    notification-method: (string-ascii 20)
  }
)

;; Authorize manufacturer to issue recalls
(define-public (authorize-manufacturer
    (manufacturer principal)
    (company-name (string-ascii 100)))
  (if (is-eq tx-sender (var-get contract-owner))
      (begin
        (map-set authorized-manufacturers
          { manufacturer: manufacturer }
          {
            company-name: company-name,
            authorized: true,
            total-recalls-issued: u0
          }
        )
        (ok true))
      (err err-not-authorized)))

;; Issue a new vehicle recall
(define-public (issue-recall
    (recall-id (string-ascii 30))
    (recall-number (string-ascii 50))
    (title (string-ascii 100))
    (description (string-ascii 300))
    (affected-models (string-ascii 200))
    (affected-years-start uint)
    (affected-years-end uint)
    (severity-level (string-ascii 20))
    (estimated-affected-vehicles uint)
    (remedy-description (string-ascii 250))
    (completion-deadline uint)
    (nhtsa-number (optional (string-ascii 20))))
  (let ((recall-exists (is-some (map-get? vehicle-recalls { recall-id: recall-id })))
        (manufacturer-authorized (default-to false (get authorized (map-get? authorized-manufacturers { manufacturer: tx-sender })))))
    (if (and (not recall-exists) manufacturer-authorized)
        (begin
          (map-set vehicle-recalls
            { recall-id: recall-id }
            {
              manufacturer: tx-sender,
              recall-number: recall-number,
              issue-date: stacks-block-height,
              title: title,
              description: description,
              affected-models: affected-models,
              affected-years-start: affected-years-start,
              affected-years-end: affected-years-end,
              severity-level: severity-level,
              estimated-affected-vehicles: estimated-affected-vehicles,
              remedy-description: remedy-description,
              completion-deadline: completion-deadline,
              nhtsa-number: nhtsa-number
            }
          )
          (var-set recall-counter (+ (var-get recall-counter) u1))
          (ok recall-id))
        (if recall-exists
            (err err-recall-exists)
            (err err-not-manufacturer)))))


;; Get recall details
(define-read-only (get-recall-details (recall-id (string-ascii 30)))
  (match (map-get? vehicle-recalls { recall-id: recall-id })
    recall (ok recall)
    (err err-recall-not-found)))

;; Get vehicle's recall status
(define-read-only (get-vehicle-recall-status
    (vin (string-ascii 17))
    (recall-id (string-ascii 30)))
  (match (map-get? vehicle-recall-status { vin: vin, recall-id: recall-id })
    status (ok status)
    (err err-recall-not-found)))

;; Get vehicle recall summary
(define-read-only (get-vehicle-recalls (vin (string-ascii 17)))
  (match (map-get? vehicle-recall-summary { vin: vin })
    summary (ok summary)
    (ok { total-recalls: u0, completed-recalls: u0, pending-recalls: u0, last-recall-date: none })))

;; Check if manufacturer is authorized
(define-read-only (is-authorized-manufacturer (manufacturer principal))
  (default-to false (get authorized (map-get? authorized-manufacturers { manufacturer: manufacturer }))))

;; Get manufacturer details
(define-read-only (get-manufacturer-info (manufacturer principal))
  (match (map-get? authorized-manufacturers { manufacturer: manufacturer })
    info (ok info)
    (err err-not-manufacturer)))

;; Get system statistics
(define-read-only (get-recall-system-stats)
  {
    total-recalls-issued: (var-get recall-counter),
    contract-owner: (var-get contract-owner)
  })
