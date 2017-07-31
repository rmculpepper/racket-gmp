#lang racket/base
(require ffi/unsafe)
(provide (all-defined-out)
         mpz?
         mpq?)

;; ------------------------------------------------------------
(module unsafe racket/base
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

  (define-cstruct _mpz_struct
    ([mp_alloc _int]
     [mp_size  _int]
     [mp_d     _pointer]))

  (define-cstruct _mpq_struct
    ([mp_num   _mpz_struct]
     [mp_den   _mpz_struct]))

  (define-cpointer-type _mpz)
  (define-cpointer-type _mpq)

  ;; ---- Unsafe mpz functions ----

  (define-gmp mpz_clear
    (_fun _mpz -> _void)
    #:wrap (deallocator))

  (define-gmp mpz_init_set_ui
    (_fun (z value) :: (z : _mpz) (value : _ulong) -> _void -> z)
    #:wrap (allocator mpz_clear))

  (define-gmp mpz_get_str (_fun _pointer _int _mpz -> _pointer))

  (define-gmp mpz_import
    (_fun _mpz _size _int _size _int _size _pointer -> _void))

  (define-gmp mpz_export
    (_fun _pointer (count : (_ptr o _size)) _int _size _int _size _mpz -> _pointer -> count))

  )

;; ------------------------------------------------------------
(module internal racket/base
  (require (submod ".." unsafe)
           ffi/unsafe)
  (provide (all-defined-out))

  (define mpz-size (ctype-sizeof _mpz_struct))
  (define mpq-size (ctype-sizeof _mpq_struct))

  (define _mp_bitcnt _ulong)

  (define (alloc-mpz) (cast (malloc mpz-size 'atomic-interior) _pointer _mpz))
  (define (alloc-mpq) (cast (malloc mpq-size 'atomic-interior) _pointer _mpq)))

;; ----------------------------------------
(require 'unsafe 'internal)

;; ============================================================

(define (mpz [n 0])
  (unless (exact-integer? n)
    (raise-argument-error 'mpz "exact-integer?" 0 n))
  (define z (alloc-mpz))
  (mpz_init_set_ui z 0)
  (define absn (abs n))
  (for ([i (in-range (integer-length absn))])
    (when (bitwise-bit-set? absn i) (mpz_setbit z i)))
  (when (negative? n) (mpz_neg z z))
  z)

(define (mpz->string z [base 10])
  (unless (mpz? z)
    (raise-argument-error 'mpz->string "mpz?" 0 z base))
  (unless (and (fixnum? base) (<= 2 base 62))
    (raise-argument-error 'mpz->string "(integer-in 2 62)" 1 z base))
  (define buf (make-bytes (+ (mpz_sizeinbase z base) (if (= (mpz_sgn z) -1) 1 0))))
  (mpz_get_str buf base z)
  (bytes->string/latin-1 buf))

(define (mpz->number z)
  (unless (mpz? z)
    (raise-argument-error 'mpz->number "mpz?" 0 z))
  ;; FIXME!!!
  (string->number (mpz->string z 16) 16))

;; ============================================================
;; Safe mpz Functions

;; ----------------------------------------
;; Assignment

(define-gmp mpz_set     (_fun _mpz _mpz -> _void))
(define-gmp mpz_set_ui  (_fun _mpz _ulong -> _void))
(define-gmp mpz_set_si  (_fun _mpz _long -> _void))
(define-gmp mpz_set_d   (_fun _mpz _double* -> _void))
(define-gmp mpz_set_q   (_fun _mpz _mpq -> _void))
;; mpz_set_f
(define-gmp mpz_set_str (_fun _mpz _string _int -> _void))
(define-gmp mpz_swap    (_fun _mpz _mpz -> _void))

;; ----------------------------------------
;; Conversion

(define-gmp mpz_get_ui  (_fun _mpz -> _ulong))
(define-gmp mpz_get_si  (_fun _mpz -> _long))
(define-gmp mpz_get_d   (_fun _mpz -> _double))
(define-gmp mpz_get_d_2exp
  (_fun (exp : (_ptr o _long)) _mpz
        -> (d : _double)
        -> (values d exp)))
;; mpz_get_str (unsafe)

;; ----------------------------------------
;; Arithmetic

(define-gmp mpz_add     (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_add_ui  (_fun _mpz _mpz _ulong -> _void))

(define-gmp mpz_sub     (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_sub_ui  (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_ui_sub  (_fun _mpz _ulong _mpz -> _void))

(define-gmp mpz_mul     (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_mul_si  (_fun _mpz _mpz _long -> _void))
(define-gmp mpz_mul_ui  (_fun _mpz _mpz _ulong -> _void))

(define-gmp mpz_addmul    (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_addmul_ui (_fun _mpz _mpz _ulong -> _void))

(define-gmp mpz_submul    (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_submul_ui (_fun _mpz _mpz _ulong -> _void))

(define-gmp mpz_mul_2exp  (_fun _mpz _mpz _mp_bitcnt -> _void))
(define-gmp mpz_neg       (_fun _mpz _mpz -> _void))
(define-gmp mpz_abs       (_fun _mpz _mpz -> _void))

;; ----------------------------------------
;; Division

(define-gmp mpz_cdiv_q      (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_cdiv_r      (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_cdiv_qr     (_fun _mpz _mpz _mpz _mpz -> _void))
(define-gmp mpz_cdiv_q_ui   (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_cdiv_r_ui   (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_cdiv_qr_ui  (_fun _mpz _mpz _mpz _ulong -> _void))
(define-gmp mpz_cdiv_ui     (_fun _mpz _ulong -> _void))
(define-gmp mpz_cdiv_q_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))
(define-gmp mpz_cdiv_r_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))

(define-gmp mpz_fdiv_q      (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_fdiv_r      (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_fdiv_qr     (_fun _mpz _mpz _mpz _mpz -> _void))
(define-gmp mpz_fdiv_q_ui   (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_fdiv_r_ui   (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_fdiv_qr_ui  (_fun _mpz _mpz _mpz _ulong -> _void))
(define-gmp mpz_fdiv_ui     (_fun _mpz _ulong -> _void))
(define-gmp mpz_fdiv_q_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))
(define-gmp mpz_fdiv_r_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))

(define-gmp mpz_tdiv_q      (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_tdiv_r      (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_tdiv_qr     (_fun _mpz _mpz _mpz _mpz -> _void))
(define-gmp mpz_tdiv_q_ui   (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_tdiv_r_ui   (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_tdiv_qr_ui  (_fun _mpz _mpz _mpz _ulong -> _void))
(define-gmp mpz_tdiv_ui     (_fun _mpz _ulong -> _void))
(define-gmp mpz_tdiv_q_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))
(define-gmp mpz_tdiv_r_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))

(define-gmp mpz_mod         (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_mod_ui      (_fun _mpz _mpz _ulong -> _void))

(define-gmp mpz_divexact    (_fun _mpz _mpz _mpz -> _void))   ;; FIXME: check safety
(define-gmp mpz_divexact_ui (_fun _mpz _mpz _ulong -> _void)) ;; FIXME: check safety

(define-gmp mpz_divisible_p      (_fun _mpz _mpz -> _bool))
(define-gmp mpz_divisible_ui_p   (_fun _mpz _ulong -> _bool))
(define-gmp mpz_divisible_2exp_p (_fun _mpz _mp_bitcnt -> _bool))

(define-gmp mpz_congruent_p      (_fun _mpz _mpz _mpz -> _bool))
(define-gmp mpz_congruent_ui_p   (_fun _mpz _ulong _ulong -> _bool))
(define-gmp mpz_congruent_2exp_p (_fun _mpz _mpz _mp_bitcnt -> _bool))

;; ----------------------------------------
;; Exponentiation

(define-gmp mpz_powm        (_fun _mpz _mpz _mpz _mpz -> _void))
(define-gmp mpz_powm_ui     (_fun _mpz _mpz _mpz _ulong -> _void))
(define-gmp mpz_powm_sec    (_fun _mpz _mpz _mpz _mpz -> _void))
(define-gmp mpz_pow_ui      (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_ui_pow_ui   (_fun _mpz _ulong _ulong -> _void))

;; ----------------------------------------
;; Root Extraction

(define-gmp mpz_root        (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_rootrem     (_fun _mpz _mpz _mpz _ulong -> _void))
(define-gmp mpz_sqrt        (_fun _mpz _mpz -> _void))
(define-gmp mpz_sqrtrem     (_fun _mpz _mpz _mpz -> _void))

(define-gmp mpz_perfect_power_p (_fun _mpz -> _bool))
(define-gmp mpz_perfect_square_p (_fun _mpz -> _bool))

;; ----------------------------------------
;; Number Theoretic Functions

(define-gmp mpz_probab_prime_p  (_fun _mpz _int -> _int))
(define-gmp mpz_nextprime       (_fun _mpz _mpz -> _void))

(define-gmp mpz_gcd             (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_gcd_ui          (_fun _mpz _mpz _ulong -> _ulong))
(define-gmp mpz_gcdext          (_fun _mpz _mpz _mpz _mpz _mpz -> _void))

(define-gmp mpz_lcm             (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_lcm_ui          (_fun _mpz _mpz _ulong -> _void))

(define-gmp mpz_invert          (_fun _mpz _mpz _mpz -> _bool))

(define-gmp mpz_jacobi          (_fun _mpz _mpz -> _int))
(define-gmp mpz_legendre        (_fun _mpz _mpz -> _int))

(define-gmp mpz_kronecker       (_fun _mpz _mpz -> _int))
(define-gmp mpz_kronecker_si    (_fun _mpz _long  -> _int))
(define-gmp mpz_kronecker_ui    (_fun _mpz _ulong -> _int))
(define-gmp mpz_si_kronecker    (_fun _long  _mpz -> _int))
(define-gmp mpz_ui_kronecker    (_fun _ulong _mpz -> _int))

(define-gmp mpz_remove          (_fun _mpz _mpz _mpz -> _mp_bitcnt))

(define-gmp mpz_fac_ui          (_fun _mpz _ulong -> _void))
(define-gmp mpz_2fac_ui         (_fun _mpz _ulong -> _void))
(define-gmp mpz_mfac_uiui       (_fun _mpz _ulong _ulong -> _void))

(define-gmp mpz_primordial_ui   (_fun _mpz _ulong -> _void))

(define-gmp mpz_bin_ui          (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_bin_uiui        (_fun _mpz _ulong _ulong -> _void))

(define-gmp mpz_fib_ui          (_fun _mpz _ulong -> _void))
(define-gmp mpz_fib2_ui         (_fun _mpz _mpz _ulong -> _void))

(define-gmp mpz_lucnum_ui       (_fun _mpz _ulong -> _void))
(define-gmp mpz_lucnum2_ui      (_fun _mpz _mpz _ulong -> _void))

;; ----------------------------------------
;; Comparison

(define-gmp mpz_cmp         (_fun _mpz _mpz -> _int))
(define-gmp mpz_cmp_d       (_fun _mpz _double -> _int))
(define-gmp mpz_cmp_si      (_fun _mpz _long -> _int))
(define-gmp mpz_cmp_ui      (_fun _mpz _ulong -> _int))

(define-gmp mpz_cmpabs      (_fun _mpz _mpz -> _int))
(define-gmp mpz_cmpabs_d    (_fun _mpz _double -> _int))
(define-gmp mpz_cmpabs_ui   (_fun _mpz _ulong -> _int))

;; mpz_sgn (macro)
(define (mpz_sgn z)
  (define zsize (mpz_struct-mp_size (cast z _mpz _mpz_struct-pointer)))
  (cond [(= zsize 0) 0] [(> zsize 0) 1] [else -1]))

;; ----------------------------------------
;; Logical and Bit Manipulation Functions

(define-gmp mpz_and         (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_ior         (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_xor         (_fun _mpz _mpz _mpz -> _void))

(define-gmp mpz_com         (_fun _mpz _mpz -> _void))
(define-gmp mpz_popcount    (_fun _mpz -> _mp_bitcnt))
(define-gmp mpz_hamdist     (_fun _mpz _mpz -> _mp_bitcnt))

(define-gmp mpz_scan0       (_fun _mpz _mp_bitcnt -> _mp_bitcnt))
(define-gmp mpz_scan1       (_fun _mpz _mp_bitcnt -> _mp_bitcnt))

(define-gmp mpz_setbit      (_fun _mpz _mp_bitcnt -> _void))
(define-gmp mpz_clrbit      (_fun _mpz _mp_bitcnt -> _void))
(define-gmp mpz_combit      (_fun _mpz _mp_bitcnt -> _void))

(define-gmp mpz_tstbit      (_fun _mpz _mp_bitcnt -> _int))

;; ----------------------------------------
;; Input and Output

;; ----------------------------------------
;; Random Numbers

;; ----------------------------------------
;; Import and Export

;; mpz_import (unsafe)
;; mpz_export (unsafe)

;; ----------------------------------------
;; Miscellaneous

(define-gmp mpz_fits_ulong_p    (_fun _mpz -> _bool))
(define-gmp mpz_fits_slong_p    (_fun _mpz -> _bool))
(define-gmp mpz_fits_uint_p     (_fun _mpz -> _bool))
(define-gmp mpz_fits_sint_p     (_fun _mpz -> _bool))
(define-gmp mpz_fits_ushort_p   (_fun _mpz -> _bool))
(define-gmp mpz_fits_sshort_p   (_fun _mpz -> _bool))

;; mpz_odd_p
;; mpz_even_p

(define-gmp mpz_sizeinbase      (_fun _mpz _int -> _size))

;; ============================================================
;; Safe mpq Functions
