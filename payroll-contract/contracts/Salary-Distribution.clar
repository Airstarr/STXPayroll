;; Salary Distribution Smart Contract
;; This contract manages regular payroll and emergency payments for employees

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_EMPLOYEE_NOT_FOUND (err u102))
(define-constant ERR_INVALID_PAYMENT (err u103))
(define-constant ERR_COOLDOWN_ACTIVE (err u104))

;; Define data maps

;; Employee information
(define-map employees 
  { employee-id: (string-ascii 64) }
  {
    stx-address: principal,
    salary-amount: uint,
    cycle-type: (string-ascii 20),   ;; "weekly", "bi-weekly", "monthly"
    last-paid-at-block: uint,
    emergency-payments-count: uint,
    last-emergency-payment-block: uint
  }
)

;; Company payroll information
(define-map company-info
  { company-id: (string-ascii 64) }
  {
    total-employees: uint,
    payroll-budget: uint, 
    emergency-payment-cooldown: uint,  ;; blocks between allowed emergency payments
    max-emergency-payment-ratio: uint  ;; percentage of salary allowed for emergency (e.g. 50 = 50%)
  }
)

;; Functions for administration

;; Initialize or update company information
(define-public (set-company-info 
                (company-id (string-ascii 64))
                (total-employees uint)
                (payroll-budget uint)
                (emergency-payment-cooldown uint)
                (max-emergency-payment-ratio uint))
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    ;; Input validation
    (asserts! (< u0 total-employees) (err u110))
    (asserts! (< u0 payroll-budget) (err u111))
    (asserts! (<= max-emergency-payment-ratio u100) (err u112))
    
    ;; Set company info after validation
    (map-set company-info
      { company-id: company-id }
      {
        total-employees: total-employees,
        payroll-budget: payroll-budget,
        emergency-payment-cooldown: emergency-payment-cooldown,
        max-emergency-payment-ratio: max-emergency-payment-ratio
      }
    )
    (ok true)
  )
)

;; Add or update an employee
(define-public (add-employee 
                (employee-id (string-ascii 64))
                (stx-address principal)
                (salary-amount uint)
                (cycle-type (string-ascii 20)))
  (begin
    ;; Authorization check
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    ;; Input validation
    (asserts! (> salary-amount u0) (err u120))
    (asserts! (not (is-eq stx-address CONTRACT_OWNER)) (err u121))  ;; Prevent setting owner as employee
    
    ;; Validate cycle-type is one of the allowed values
    (asserts! (or (is-eq cycle-type "weekly") 
                 (is-eq cycle-type "bi-weekly") 
                 (is-eq cycle-type "monthly")) 
              (err u122))
    
    ;; Check company payroll budget if applicable
    (match (map-get? company-info { company-id: "main" })
      company-data 
        (let ((current-employee (map-get? employees { employee-id: employee-id }))
              (company-budget (get payroll-budget company-data)))
          ;; If updating employee, check if new salary exceeds budget
          (if (is-some current-employee)
              (asserts! (<= salary-amount company-budget) (err u123))
              true)  ;; New employee validation handled elsewhere
        )
      ;; If no company data exists yet, continue without this check
      true
    )
    
    ;; Set employee data after validation
    (map-set employees
      { employee-id: employee-id }
      {
        stx-address: stx-address,
        salary-amount: salary-amount,
        cycle-type: cycle-type,
        last-paid-at-block: u0,
        emergency-payments-count: u0,
        last-emergency-payment-block: u0
      }
    )
    (ok true)
  )
)

;; Remove an employee
(define-public (remove-employee (employee-id (string-ascii 64)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-delete employees { employee-id: employee-id })
    (ok true)
  )
)

;; Fund the contract
(define-public (fund-contract (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (ok true)
  )
)

;; Check if it's time to pay an employee based on their cycle
(define-private (is-payment-due 
                  (last-paid-at-block uint)
                  (cycle-type (string-ascii 20)))
  (let ((blocks-passed (- stacks-block-height last-paid-at-block))
        (cycle-blocks (get-cycle-blocks cycle-type)))
    (>= blocks-passed cycle-blocks)
  )
)

;; Helper to convert cycle type to blocks
(define-private (get-cycle-blocks (cycle-type (string-ascii 20)))
  (if (is-eq cycle-type "weekly")
    u144 ;; ~1 day = 144 blocks, so 7 days = 1008 blocks
    (if (is-eq cycle-type "bi-weekly")
      u2016 ;; ~2 weeks = 2016 blocks
      u4320) ;; Monthly (30 days) = 4320 blocks
  )
)

;; Pay salary to a specific employee if due
(define-public (pay-salary (employee-id (string-ascii 64)))
  (let ((employee (unwrap! (map-get? employees { employee-id: employee-id }) ERR_EMPLOYEE_NOT_FOUND))
        (salary-amount (get salary-amount employee))
        (last-paid-at-block (get last-paid-at-block employee))
        (cycle-type (get cycle-type employee))
        (employee-address (get stx-address employee)))
    
    ;; Check if payment is due
    (asserts! (is-payment-due last-paid-at-block cycle-type) ERR_INVALID_PAYMENT)
    
    ;; Check if contract has enough funds
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) salary-amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; Update employee's last paid block
    (map-set employees
      { employee-id: employee-id }
      (merge employee { last-paid-at-block: stacks-block-height })
    )
    
    ;; Transfer the funds
    (as-contract (stx-transfer? salary-amount tx-sender employee-address))
  )
)

;; Batch process payroll for all employees due for payment
(define-public (process-payroll (company-id (string-ascii 64)) (employee-ids (list 100 (string-ascii 64))))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (fold process-employee-payment employee-ids (ok true))
  )
)

;; Helper function for batch processing
(define-private (process-employee-payment (employee-id (string-ascii 64)) (previous-result (response bool uint)))
  (match previous-result
    success (begin
              (try! (pay-salary employee-id))
              (ok true))
    error (err error)
  )
)

;; Make an emergency payment to an employee
(define-public (emergency-payment 
                (employee-id (string-ascii 64))
                (amount uint))
  (let ((employee (unwrap! (map-get? employees { employee-id: employee-id }) ERR_EMPLOYEE_NOT_FOUND))
        (company (unwrap! (map-get? company-info { company-id: "main" }) ERR_NOT_AUTHORIZED))
        (employee-address (get stx-address employee))
        (salary-amount (get salary-amount employee))
        (last-emergency-payment-block (get last-emergency-payment-block employee))
        (emergency-payment-cooldown (get emergency-payment-cooldown company))
        (max-emergency-payment-ratio (get max-emergency-payment-ratio company))
        (max-allowed-amount (/ (* salary-amount max-emergency-payment-ratio) u100)))
    
    ;; Check authorization
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    ;; Check cooldown period
    (asserts! (>= (- stacks-block-height last-emergency-payment-block) emergency-payment-cooldown) ERR_COOLDOWN_ACTIVE)
    
    ;; Check amount is within allowed limit
    (asserts! (<= amount max-allowed-amount) ERR_INVALID_PAYMENT)
    
    ;; Check if contract has enough funds
    (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; Update employee's emergency payment information
    (map-set employees
      { employee-id: employee-id }
      (merge employee { 
        emergency-payments-count: (+ (get emergency-payments-count employee) u1),
        last-emergency-payment-block: stacks-block-height
      })
    )
    
    ;; Transfer the funds
    (as-contract (stx-transfer? amount tx-sender employee-address))
  )
)

;; Getter functions

;; Get employee information
(define-read-only (get-employee (employee-id (string-ascii 64)))
  (map-get? employees { employee-id: employee-id })
)

;; Check if employee payment is due
(define-read-only (check-payment-due (employee-id (string-ascii 64)))
  (match (map-get? employees { employee-id: employee-id })
    employee (is-payment-due (get last-paid-at-block employee) (get cycle-type employee))
    false
  )
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

;; Get company information
(define-read-only (get-company-info (company-id (string-ascii 64)))
  (map-get? company-info { company-id: company-id })
)