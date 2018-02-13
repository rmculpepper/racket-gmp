#lang info

;; ========================================
;; pkg info

(define version "1.1")
(define collection "gmp")
(define deps
  '("base"
    "rackunit-lib"
    "binaryio-lib"
    "gmp-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
(define implies
  '("gmp-lib"))
(define pkg-authors '(ryanc))

;; ========================================
;; collect info

(define name "gmp")
(define scribblings '(["gmp.scrbl" ()]))
