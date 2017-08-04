#lang info

;; ========================================
;; pkg info

(define collection "gmp")
(define deps
  '("base"
    "gmp-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
(define implies
  '("gmp-lib"))

;; ========================================
;; collect info

(define name "gmp")
(define scribblings '(["gmp.scrbl" ()]))
