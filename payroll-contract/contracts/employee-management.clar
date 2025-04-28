;; Employee Management Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-employee-exists (err u101))
(define-constant err-employee-not-found (err u102))
(define-constant err-invalid-salary (err u103))

;; Define data maps
(define-map employees
  { address: principal }
  {
    name: (string-ascii 64),
    role: (string-ascii 64),
    salary: uint,
    payment-address: principal,
    start-date: uint,
    active: bool
  }
)

(define-map employers
  { address: principal }
  {
    name: (string-ascii 64),
    authorized: bool
  }
)

;; Define variables
(define-data-var employee-count uint u0)
(define-data-var active-employee-count uint u0)

;; Initialize the contract owner as an employer
(begin
  (map-set employers
    { address: contract-owner }
    {
      name: "Contract Owner",
      authorized: true
    }
  )
)

;; Check if caller is authorized
(define-private (is-authorized)
  (default-to false (get authorized (map-get? employers { address: tx-sender })))
)

;; Add a new employer
(define-public (add-employer (employer-address principal) (employer-name (string-ascii 64)))
  (begin
    (asserts! (is-authorized) (err err-not-authorized))
    (ok (map-set employers
      { address: employer-address }
      {
        name: employer-name,
        authorized: true
      }
    ))
  )
)

;; Remove an employer
(define-public (remove-employer (employer-address principal))
  (begin
    (asserts! (is-authorized) (err err-not-authorized))
    (asserts! (not (is-eq employer-address contract-owner)) (err err-not-authorized))
    (ok (map-delete employers { address: employer-address }))
  )
)

;; Add a new employee
(define-public (add-employee (employee-address principal) (name (string-ascii 64)) (role (string-ascii 64)) (salary uint) (payment-address principal))
  (begin
    (asserts! (is-authorized) (err err-not-authorized))
    (asserts! (is-none (map-get? employees { address: employee-address })) (err err-employee-exists))
    (asserts! (> salary u0) (err err-invalid-salary))
    
    (map-set employees
      { address: employee-address }
      {
        name: name,
        role: role,
        salary: salary,
        payment-address: payment-address,
        start-date: stacks-block-height,
        active: true
      }
    )
    
    (var-set employee-count (+ (var-get employee-count) u1))
    (var-set active-employee-count (+ (var-get active-employee-count) u1))
    
    (ok true)
  )
)

;; Remove an employee (deactivate)
(define-public (remove-employee (employee-address principal))
  (let
    (
      (employee (unwrap! (map-get? employees { address: employee-address }) (err err-employee-not-found)))
    )
    (asserts! (is-authorized) (err err-not-authorized))
    (asserts! (get active employee) (err err-employee-not-found))
    
    (map-set employees
      { address: employee-address }
      (merge employee { active: false })
    )
    
    (var-set active-employee-count (- (var-get active-employee-count) u1))
    
    (ok true)
  )
)

;; Update employee name
(define-public (update-employee-name (employee-address principal) (new-name (string-ascii 64)))
  (let
    (
      (employee (unwrap! (map-get? employees { address: employee-address }) (err err-employee-not-found)))
    )
    (asserts! (is-authorized) (err err-not-authorized))
    (asserts! (get active employee) (err err-employee-not-found))
    
    (map-set employees
      { address: employee-address }
      (merge employee { name: new-name })
    )
    
    (ok true)
  )
)

;; Update employee role
(define-public (update-employee-role (employee-address principal) (new-role (string-ascii 64)))
  (let
    (
      (employee (unwrap! (map-get? employees { address: employee-address }) (err err-employee-not-found)))
    )
    (asserts! (is-authorized) (err err-not-authorized))
    (asserts! (get active employee) (err err-employee-not-found))
    
    (map-set employees
      { address: employee-address }
      (merge employee { role: new-role })
    )
    
    (ok true)
  )
)

;; Update employee salary
(define-public (update-employee-salary (employee-address principal) (new-salary uint))
  (let
    (
      (employee (unwrap! (map-get? employees { address: employee-address }) (err err-employee-not-found)))
    )
    (asserts! (is-authorized) (err err-not-authorized))
    (asserts! (get active employee) (err err-employee-not-found))
    (asserts! (> new-salary u0) (err err-invalid-salary))
    
    (map-set employees
      { address: employee-address }
      (merge employee { salary: new-salary })
    )
    
    (ok true)
  )
)

;; Update employee payment address
(define-public (update-employee-payment-address (employee-address principal) (new-payment-address principal))
  (let
    (
      (employee (unwrap! (map-get? employees { address: employee-address }) (err err-employee-not-found)))
    )
    (asserts! (is-authorized) (err err-not-authorized))
    (asserts! (get active employee) (err err-employee-not-found))
    
    (map-set employees
      { address: employee-address }
      (merge employee { payment-address: new-payment-address })
    )
    
    (ok true)
  )
)

;; Reactivate an employee
(define-public (reactivate-employee (employee-address principal))
  (let
    (
      (employee (unwrap! (map-get? employees { address: employee-address }) (err err-employee-not-found)))
    )
    (asserts! (is-authorized) (err err-not-authorized))
    (asserts! (not (get active employee)) (err err-employee-exists))
    
    (map-set employees
      { address: employee-address }
      (merge employee { active: true })
    )
    
    (var-set active-employee-count (+ (var-get active-employee-count) u1))
    
    (ok true)
  )
)

;; Get employee details
(define-read-only (get-employee (employee-address principal))
  (map-get? employees { address: employee-address })
)

;; Check if an address is an employee
(define-read-only (is-employee (address principal))
  (is-some (map-get? employees { address: address }))
)

;; Check if an employee is active
(define-read-only (is-active-employee (address principal))
  (default-to false (get active (map-get? employees { address: address })))
)

;; Get total number of employees (including inactive)
(define-read-only (get-employee-count)
  (var-get employee-count)
)

;; Get number of active employees
(define-read-only (get-active-employee-count)
  (var-get active-employee-count)
)

;; Check if an address is an employer
(define-read-only (is-employer (address principal))
  (is-some (map-get? employers { address: address }))
)

;; Get employer details
(define-read-only (get-employer (employer-address principal))
  (map-get? employers { address: employer-address })
)