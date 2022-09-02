;;;; This is free software.  See LICENSE for copyright info.

(import (chicken base)
        (chicken io)
        (chicken irregex)
        (chicken process-context)
        (slib wt-tree))

;; Describes a test-group line.  May need refinement.
(define test-group-pattern
  (irregex
   '(: (* whitespace)
       "(test-group \""
       (submatch (: lower-case (+ (~ (or whitespace #\")))))  ; ident
       "\""
       (* whitespace))))

(define (die msg . args)
  (parameterize ((current-output-port (current-error-port)))
    (display "Error: ")
    (display msg)
    (newline)
    (display "Irritants: ")
    (display args)
    (newline)
    (exit 1)))

;; If 'form' is present in 'dict', then confirm it.  If it's not
;; present or has already been confirmed, show a warning.
(define (confirm-form dict name)
  (assert (string? name))
  (let ((v (wt-tree/lookup dict name 'not-found)))
    (case v
      ((#t)
       (warning "possible redundant test group" name)
       dict)
      ((not-found)
       (warning "test group for unknown form" name)
       dict)
      ((#f) (wt-tree/add dict name #t))
      (else (warning "can't happen: invalid value in tree" v name)))))

(define (print-results dict)
  (define (count-unconfirmed)
    (wt-tree/fold (lambda (junk b n)
                    (if b n (+ n 1)))
                  0
                  dict))

  (display "Results:\n\n")
  (wt-tree/for-each
   (lambda (name tested)
     (print name ": " (if tested "yes" "NO")))
   dict)
  (newline)
  (case (count-unconfirmed)
    ((0) (display "All forms confirmed tested.\n"))
    ((1) (display "1 form unconfirmed.\n"))  ; grammar!
    (else => (cut print <> " forms unconfirmed."))))

;; Read lines from 'port', searching for test-group headers.  Confirm
;; the names of those that look like form-specific groups.
(define (check-tests forms port)
  (let lp ((line (read-line port))
           (dict (make-dict forms)))
    (cond ((eof-object? line)
           (print-results dict))
          ((irregex-match test-group-pattern line) =>
           (lambda (m)
             (lp (read-line port)
                 (confirm-form dict
                               (irregex-match-substring m 1)))))
          (else (lp (read-line port) dict)))))

(define (make-dict forms)
  (foldl (lambda (dict sym)
           (wt-tree/add dict (symbol->string sym) #f))
         (make-wt-tree string-wt-type)
         forms))

;; Try to read a (module ...) S-exp from port.  If this is successful,
;; try to extract the module's list of exported identifiers.
(define (read-exported-forms port)
  (let ((sexp (read port)))
    (unless (and (pair? sexp) (eqv? (car sexp) 'module))
      (die "Failed to read module"))
    (let ((forms (list-ref sexp 2)))
      (cond ((pair? forms) forms)
            ((null? forms) (die "Module has no exports"))
            (else (die "Exports must be a list" forms))))))

(let* ((args (command-line-arguments))
       (m-port (open-input-file (car args)))
       (t-port (open-input-file (cadr args))))
  (check-tests (read-exported-forms m-port) t-port)
  (close-input-port m-port)
  (close-input-port t-port)
  (exit 0))
