#lang racket/base
(require rackunit
         gmp
         "util.rkt")

(define (test-n n signed?)
  (define z (mpz n))
  ;; mpz->number
  (check-equal? (mpz->number z) n)
  (when (fixnum? n)
    (check-equal? (mpz_get_si z) n))
  (let ()
    (define z2 (mpz))
    (mpz-set! z2 n)
    (check mpz=? z2 z)
    (check-equal? (mpz->number z2) n)
    (when (fixnum? n)
      (check-equal? (mpz_get_si z2) n)))
  ;; mpz->string
  (check-equal? (mpz->string z 10) (number->string n 10))
  (check-equal? (mpz->string z 16) (number->string n 16))
  ;; zero/positive/negative
  (check-equal? (mpz-zero? z) (zero? n))
  (check-equal? (mpz-positive? z) (positive? n))
  (check-equal? (mpz-negative? z) (negative? n)))

(for* ([i #e1e3] [signed? '(#t #f)])
  (define n (rnd signed? 30))
  (test-n n signed?))

(for* ([i #e5e2] [signed? '(#t #f)])
  (define n (rnd signed? 320))
  (test-n n signed?))

(for* ([i #e1e2] [signed? '(#t #f)])
  (define n (rnd signed? 8000))
  (test-n n signed?))
