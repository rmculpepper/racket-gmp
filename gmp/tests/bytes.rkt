#lang racket/base
(require rackunit
         binaryio/integer
         gmp)

(define (rt n len signed?)
  (define z (mpz n))
  #;(printf "  ~s ~v, ~v\n" (if signed? "signed" "unsigned") n z)
  (for ([big-endian? '(#t #f)])
    (define zb (mpz->bytes z len signed? big-endian?))
    (when len
      (define nb (integer->bytes n len signed? big-endian?))
      (check-equal? zb nb))
    (check-equal? (mpz->number (bytes->mpz zb signed? big-endian?)) n)))

(define (rt2 n signed?) (rt n 2 signed?))

(for-each (lambda (n) (rt2 n #t))
          '(0 -1 1 127 128 -127 -128 -129 255 256))

(for-each (lambda (n) (rt2 n #f))
          '(0 1 127 128 255 256))

(define (rnd signed? nbits)
  (if signed?
      (- (random (expt 2 nbits)) (expt 2 (sub1 nbits)))
      (random (expt 2 nbits))))

;; random tests
(for ([signed? '(#t #f)])
  (for ([i #e1e3])
    (rt2 (rnd signed? 16) signed?)))
(for ([signed? '(#t #f)])
  (for ([i #e1e3])
    (rt (rnd signed? 24) 3 signed?)))
(for ([signed? '(#t #f)])
  (for ([nbits '(8 12 16 30)])
    (for ([i #e5e2])
      (rt (rnd signed? nbits) #f signed?))))

;; ----------------------------------------

(check-exn #rx"cannot convert negative mpz as unsigned"
           (lambda () (mpz->bytes (mpz -1)  1 #f)))

(check-exn #rx"mpz does not fit" (lambda () (mpz->bytes (mpz 256) 1 #f)))
(check-exn #rx"mpz does not fit" (lambda () (mpz->bytes (mpz 128)  1 #t)))
(check-exn #rx"mpz does not fit" (lambda () (mpz->bytes (mpz -129) 1 #t)))

(check-exn #rx"byte string length" (lambda () (mpz->bytes (mpz 1) 1 #t #t (bytes 0) 1)))
(check-exn #rx"byte string length" (lambda () (mpz->bytes (mpz 1) 2 #t #t (bytes 0))))
