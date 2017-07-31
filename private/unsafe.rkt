#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/struct
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc)
(provide (protect-out (all-defined-out)))

(define-ffi-definer define-gmp0 (ffi-lib "libgmp" '(#f "10"))
  #:default-make-fail make-not-available)

(define-syntax (define-gmp stx)
  (syntax-case stx ()
    [(define-gmp name type option ...)
     (with-syntax ([cname (format-id #'name "__g~a" #'name)])
       #'(define-gmp0 name type #:c-id cname option ...))]))

(define-gmp mp_bits_per_limb _int #:fail (* 8 (ctype-sizeof _ulong)))

(define-cstruct _mpz_struct
  ([mp_alloc _int]
   [mp_size  _int]
   [mp_d     _pointer])
  #:malloc-mode 'atomic-interior
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (z) 'mpz)
   (lambda (z) (list (*mpz->number z)))))

(define-cstruct _mpq_struct
  ([mp_num   _mpz_struct]
   [mp_den   _mpz_struct])
  #:malloc-mode 'atomic-interior
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (q) 'mpq)
   (lambda (q) (list (/ (*mpz->number (mpq_struct-mp_num q)) (*mpz->number (mpq_struct-mp_den q)))))))

(define _mpz _mpz_struct-pointer)
(define _mpq _mpq_struct-pointer)

(define mpz? mpz_struct?)
(define mpq? mpq_struct?)

(define _mp_bitcnt _ulong)

;; ---- Unsafe mpz functions ----

(define-gmp mpz_clear
  (_fun _mpz -> _void)
  #:wrap (deallocator))

(define-gmp mpz_init
  (_fun (z : _mpz) -> _void -> z)
  #:wrap (allocator mpz_clear))

(define-gmp mpz_init2
  (_fun (z : _mpz) _mp_bitcnt -> _void -> z)
  #:wrap (allocator mpz_clear))

(define-gmp mpz_realloc2      (_fun _mpz _mp_bitcnt -> _void))

(define-gmp mpz_get_str (_fun _pointer _int _mpz -> _pointer))

(define-gmp mpz_import
  (_fun _mpz _size _int _size _int _size _pointer -> _void))

(define-gmp mpz_export
  (_fun _pointer (count : (_ptr o _size)) _int _size _int _size _mpz -> _pointer -> count))

(define-gmp mpz_sizeinbase    (_fun _mpz _int -> _size))
(define-gmp mpz_size          (_fun _mpz -> _size))

(define-gmp mpz_limbs_read    (_fun _mpz -> _pointer))
(define-gmp mpz_limbs_write   (_fun _mpz _size -> _pointer))
(define-gmp mpz_limbs_finish  (_fun _mpz _size -> _void))

;; ---- Unsafe mpq functions ----

(define-gmp mpq_clear
  (_fun _mpq -> _void)
  #:wrap (deallocator))

(define-gmp mpq_init
  (_fun (q : _mpq) -> _void -> q)
  #:wrap (allocator mpq_clear))

;; ============================================================
;; Other internals

(define (alloc-mpz) (make-mpz_struct 0 0 #f))
(define (alloc-mpq) (make-mpq_struct z0 z0))
(define z0 (alloc-mpz)) ;; FIXME?

(define (*mpz->number z)
  (define len (+ (max 1 (mpz_sizeinbase z 16))
                 (if (negative? (mpz_struct-mp_size z)) 1 0)))
  (define buf (make-bytes len))
  (mpz_get_str buf 16 z)
  (string->number (bytes->string/latin-1 buf) 16))
