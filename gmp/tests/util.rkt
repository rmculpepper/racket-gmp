#lang racket/base
(provide (all-defined-out))

(define (urandom nbits)
  (if (<= nbits 24)
      (random (expt 2 nbits))
      (+ (arithmetic-shift (urandom (- nbits 24)) 24)
         (random (expt 2 24)))))

(define (rnd signed? nbits)
  (if signed?
      (- (urandom nbits) (expt 2 (sub1 nbits)))
      (urandom nbits)))
