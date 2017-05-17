#lang typed/racket

(require typed/rackunit)

(define-type Atom (U Number String Boolean)) ; a basic atom type
(define-type S-Exp (U Atom (Listof S-Exp)))

(: atom? (-> Any Boolean))
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(: s-exp? (-> Any Boolean))
(define (s-exp? x)
  (or (atom? x)
      (and (list? x)
           (for/and ([i x]) (s-exp? i)))))

; Let's play

(check-equal? (atom? 'atom)
              #t)

(check-equal? (atom? 'turkey)
              #t)

(check-equal? (atom? 1492)
              #t)

(check-equal? (atom? "u")
              #t)

(check-equal? (atom? '*abc$)
              #t)

(check-equal? (list? '(atom turkey or))
              #t)

(check-equal? (list? '((atom turkey) or))
              #t)

(check-equal? (s-exp? 'xyz)
              #t)

(check-equal? (s-exp? '(x y z))
              #t)

(check-equal? (s-exp? '((x y) z))
              #t)

(check-equal? (list? '(how are you doing so far))
              #t)

(check-equal? (length '(how are you doing so far))
              6)

(check-equal? (s-exp? '(((how) are) ((you) (doing so)) far))
              #t)

(check-equal? (list? '(((how) are) ((you) (doing so)) far))
              #t)

(check-equal? (length '(((how) are) ((you) (doing so)) far))
              3)


(provide (all-defined-out))

