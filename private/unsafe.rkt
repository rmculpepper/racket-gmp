#lang racket/base
(require (for-syntax racket/base racket/syntax)
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
   [mp_d     _pointer]))

(define-cstruct _mpq_struct
  ([mp_num   _mpz_struct]
   [mp_den   _mpz_struct]))

(define-cpointer-type _mpz _gcpointer)
(define-cpointer-type _mpq _gcpointer)

(define _mp_bitcnt _ulong)

;; ---- Unsafe mpz functions ----

(define-gmp mpz_clear
  (_fun _mpz -> _void)
  #:wrap (deallocator))

(define-gmp mpz_init
  (_fun (z : _mpz) -> _void -> z)
  #:wrap (allocator mpz_clear))

(define-gmp mpz_get_str (_fun _pointer _int _mpz -> _pointer))

(define-gmp mpz_import
  (_fun _mpz _size _int _size _int _size _pointer -> _void))

(define-gmp mpz_export
  (_fun _pointer (count : (_ptr o _size)) _int _size _int _size _mpz -> _pointer -> count))

(define-gmp mz_size         (_fun _mpz -> _size))
(define-gmp mpz_limbs_read  (_fun _mpz -> _pointer))
(define-gmp mpz_limbs_write (_fun _mpz _size -> _pointer))
(define-gmp mpz_limbs_finish (_fun _mpz _size -> _void))
(define-gmp mpz_realloc2    (_fun _mpz _mp_bitcnt -> _void))

;; ---- Unsafe mpq functions ----

(define-gmp mpq_clear
  (_fun _mpq -> _void)
  #:wrap (deallocator))

(define-gmp mpq_init
  (_fun (q : _mpq) -> _void -> q)
  #:wrap (allocator mpq_clear))

;; ============================================================
;; Other internals

(define mpz-size (ctype-sizeof _mpz_struct))
(define mpq-size (ctype-sizeof _mpq_struct))

(define (alloc-mpz) (cast (malloc mpz-size 'atomic-interior) _pointer _mpz))
(define (alloc-mpq) (cast (malloc mpq-size 'atomic-interior) _pointer _mpq))
