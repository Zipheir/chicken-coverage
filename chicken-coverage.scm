;;;; This is free software.  See LICENSE for copyright info.

(import (chicken base)
        (chicken condition)
        (chicken io)
        (chicken irregex)
        (chicken process-context)
        (chicken type)
        (slib wt-tree))

(define-syntax assert-type
  (syntax-rules ()
    ((assert-type loc expr)
     (unless expr
       (abort
        (make-composite-condition
         (make-property-condition 'exn
          'location loc
          'message "type check failed"
          'arguments (list 'expr))
         (make-property-condition 'type)
         (make-property-condition 'assertion)))))))

;; Describes a test-group line.  May need refinement.
(define test-group-pattern
  (irregex
   '(: (* whitespace)
       "(test-group \""
       (submatch (: lower-case (+ (~ (or whitespace #\")))))  ; ident
       "\""
       (* whitespace))))

;; If 'form' is present in 'dict', then confirm it.  If it's not
;; present or has already been confirmed, show a warning.
(: confirm-form ((struct wt-tree) string -> (struct wt-tree)))
(define (confirm-form dict name)
  (assert-type 'confirm-form (wt-tree? dict))
  (assert-type 'confirm-form (string? name))
  (let ((v (wt-tree/lookup dict name 'not-found)))
    (case v
      ((#t)
       (warning "possible redundant test group" name)
       dict)
      ((not-found)
       (warning "test group for unknown form" name)
       dict)
      ((#f) (wt-tree/add dict name #t))
      (else
       (warning "can't happen: invalid value in tree" v name)
       dict))))

(: count-unconfirmed ((struct wt-tree) -> integer))
(define (count-unconfirmed dict)
  (assert-type 'count-unconfirmed (wt-tree? dict))
  (wt-tree/fold (lambda (junk b n)
                  (if b n (+ n 1)))
                0
                dict))

(: print-results ((struct wt-tree) integer -> undefined))
(define (print-results dict count)
  (assert-type 'print-results (wt-tree? dict))
  (assert-type 'print-results (integer? count))
  (display "Results:\n\n")
  (wt-tree/for-each
   (lambda (name tested)
     (print name ": " (if tested "yes" "NO")))
   dict)
  (newline)
  (case count
    ((0) (display "All forms confirmed tested.\n"))
    ((1) (display "1 form unconfirmed.\n"))  ; grammar!
    (else => (cut print <> " forms unconfirmed."))))

;; Read lines from 'port', searching for test-group headers.  Confirm
;; the names of those that look like form-specific groups.
;; Returns a boolean value indicating whether or not all forms were
;; confirmed.
(: check-tests ((list-of symbol) input-port -> boolean))
(define (check-tests forms port)
  (assert-type 'check-tests (or (pair? forms) (null? forms)))
  (assert-type 'check-tests (input-port? port))
  (let lp ((line (read-line port))
           (dict (make-dict forms)))
    (cond ((eof-object? line)
           (let ((c (count-unconfirmed dict)))
             (print-results dict c)
             (zero? c)))
          ((irregex-match test-group-pattern line) =>
           (lambda (m)
             (assert (= 1 (irregex-match-num-submatches m)))
             (lp (read-line port)
                 (confirm-form dict
                               (irregex-match-substring m 1)))))
          (else (lp (read-line port) dict)))))

(: make-dict ((list-of symbol) -> (struct wt-tree)))
(define (make-dict forms)
  (assert-type 'make-dict (or (pair? forms) (null? forms)))
  (foldl (lambda (dict sym)
           (wt-tree/add dict (symbol->string sym) #f))
         (make-wt-tree string-wt-type)
         forms))

;; Try to read a (module ...) S-exp from port.  If this is successful,
;; try to extract the module's list of exported identifiers.
(: read-exported-forms (input-port -> (list-of symbol)))
(define (read-exported-forms port)
  (assert-type 'read-exported-forms (input-port? port))
  (let ((sexp (read port)))
    (unless (and (pair? sexp) (eqv? (car sexp) 'module))
      (error "Failed to read module S-exp" sexp))
    (let ((forms (list-ref sexp 2)))
      (cond ((pair? forms) forms)
            ((null? forms) (error "Module has no exports"))
            (else (error "Exports must be a list" forms))))))

(let ((args (command-line-arguments)))
  (define (usage)
    (parameterize ((current-output-port (current-error-port)))
      (print "Usage: " (program-name) " <module-file> <test-file>"))
    (exit 1))
  (unless (= (length args) 2)
    (usage))
  (let* ((m-port (open-input-file (car args)))
         (t-port (open-input-file (cadr args)))
         (status (check-tests (read-exported-forms m-port) t-port)))
    (close-input-port m-port)
    (close-input-port t-port)
    (exit (if status 0 1))))
