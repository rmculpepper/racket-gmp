#lang info

;; ========================================
;; pkg info

(define collection "gmp")
(define deps
  '("base"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))

;; ========================================
;; collect info

(define name "gmp")
(define scribblings '(["gmp.scrbl" (#;multi-page)]))
