#lang racket/base
(require ffi/unsafe
         ffi/unsafe/atomic
         "private/unsafe.rkt")
(provide (all-defined-out)
         mpz?
         mpq?
         mpz_sizeinbase
         mpz_size)

;; ============================================================
;; mpz

(define (mpz [n 0])
  (unless (or (exact-integer? n) (mpz? n))
    (raise-argument-error 'mpz "exact-integer?" 0 n))
  (define z (alloc-mpz))
  (mpz_init z)
  (cond [(exact-integer? n)
         (mpz-set! z n)]
        [(mpz? n)
         (mpz_set z n)])
  z)

(define (mpz-set! z n)
  (unless (mpz? z)
    (raise-argument-error 'mpz-set! "mpz?" 0 z n))
  (unless (exact-integer? n)
    (raise-argument-error 'mpz-set! "exact-integer?" 1 z n))
  (define absn (abs n))
  (for ([i (in-range (integer-length absn))])
    (when (bitwise-bit-set? absn i) (mpz_setbit z i)))
  (when (negative? n) (mpz_neg z z)))

(define (mpz->string z [base 10])
  (unless (mpz? z)
    (raise-argument-error 'mpz->string "mpz?" 0 z base))
  (unless (memv base '(2 8 10 16))
    (raise-argument-error 'mpz->string "(or/c 2 8 10 16)" 1 z base))
  (define buf (make-bytes (+ (mpz_sizeinbase z base) (if (= (mpz_sgn z) -1) 1 0))))
  (mpz_get_str buf base z)
  (bytes->string/latin-1 buf))

(define (mpz->number z)
  (unless (mpz? z)
    (raise-argument-error 'mpz->number "mpz?" 0 z))
  ;; FIXME!!!
  (string->number (mpz->string z 16) 16))

(define (mpz-zero? z)
  (unless (mpz? z) (raise-argument-error 'mpz-zero? "mpz?" 0 z))
  (zero? (mpz_sgn z)))

;; ------------------------------------------------------------
;; Safe mpz Functions

;; ----------------------------------------
;; Assignment

(define-gmp mpz_set     (_fun _mpz _mpz -> _void))
(define-gmp mpz_set_ui  (_fun _mpz _ulong -> _void))
(define-gmp mpz_set_si  (_fun _mpz _long -> _void))
(define-gmp mpz_set_d   (_fun _mpz _double* -> _void))
(define-gmp mpz_set_q   (_fun _mpz _mpq -> _void))
;; mpz_set_f
(define-gmp mpz_set_str (_fun _mpz _string/latin-1 _int -> _int))
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

;; FIXME: catch division by zero :(
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

(define-gmp mpz_divisible_p      (_fun _mpz _mpz -> _int))
(define-gmp mpz_divisible_ui_p   (_fun _mpz _ulong -> _int))
(define-gmp mpz_divisible_2exp_p (_fun _mpz _mp_bitcnt -> _int))

(define-gmp mpz_congruent_p      (_fun _mpz _mpz _mpz -> _int))
(define-gmp mpz_congruent_ui_p   (_fun _mpz _ulong _ulong -> _int))
(define-gmp mpz_congruent_2exp_p (_fun _mpz _mpz _mp_bitcnt -> _int))

;; ----------------------------------------
;; Exponentiation

(define-gmp mpz_powm        (_fun _mpz _mpz _mpz _mpz -> _void))
(define-gmp mpz_powm_ui     (_fun _mpz _mpz _mpz _ulong -> _void))
(define-gmp mpz_powm_sec    (_fun _mpz _mpz _mpz _mpz -> _void))
(define-gmp mpz_pow_ui      (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_ui_pow_ui   (_fun _mpz _ulong _ulong -> _void))

;; ----------------------------------------
;; Root Extraction

(define-gmp mpz_root        (_fun _mpz _mpz _ulong -> _int))
(define-gmp mpz_rootrem     (_fun _mpz _mpz _mpz _ulong -> _void))
(define-gmp mpz_sqrt        (_fun _mpz _mpz -> _void))
(define-gmp mpz_sqrtrem     (_fun _mpz _mpz _mpz -> _void))

(define-gmp mpz_perfect_power_p (_fun _mpz -> _int))
(define-gmp mpz_perfect_square_p (_fun _mpz -> _int))

;; ----------------------------------------
;; Number Theoretic Functions

(define-gmp mpz_probab_prime_p  (_fun _mpz _int -> _int))
(define-gmp mpz_nextprime       (_fun _mpz _mpz -> _void))

(define-gmp mpz_gcd             (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_gcd_ui          (_fun _mpz _mpz _ulong -> _ulong))
(define-gmp mpz_gcdext          (_fun _mpz _mpz _mpz _mpz _mpz -> _void))

(define-gmp mpz_lcm             (_fun _mpz _mpz _mpz -> _void))
(define-gmp mpz_lcm_ui          (_fun _mpz _mpz _ulong -> _void))

(define-gmp mpz_invert          (_fun _mpz _mpz _mpz -> _int))

(define-gmp mpz_jacobi          (_fun _mpz _mpz -> _int))
(define-gmp mpz_legendre        (_fun _mpz _mpz -> _int))

(define-gmp mpz_kronecker       (_fun _mpz _mpz -> _int))
(define-gmp mpz_kronecker_si    (_fun _mpz _long  -> _int))
(define-gmp mpz_kronecker_ui    (_fun _mpz _ulong -> _int))
(define-gmp mpz_si_kronecker    (_fun _long  _mpz -> _int))
(define-gmp mpz_ui_kronecker    (_fun _ulong _mpz -> _int))

(define-gmp mpz_remove          (_fun _mpz _mpz _mpz -> _mp_bitcnt))

(define-gmp mpz_fac_ui          (_fun _mpz _ulong -> _void))
(define-gmp mpz_2fac_ui         (_fun _mpz _ulong -> _void))          ;; buggy??
(define-gmp mpz_mfac_uiui       (_fun _mpz _ulong _ulong -> _void))   ;; buggy??

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
  (unless (mpz? z) (raise-argument-error 'mpz_sgn "mpz?" 0 z))
  (define zsize (mpz_struct-mp_size z))
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
;; Random Numbers

;; ----------------------------------------
;; Import and Export

;; mpz_import (unsafe)
;; mpz_export (unsafe)

;; ----------------------------------------
;; Miscellaneous

(define-gmp mpz_fits_ulong_p    (_fun _mpz -> _int))
(define-gmp mpz_fits_slong_p    (_fun _mpz -> _int))
(define-gmp mpz_fits_uint_p     (_fun _mpz -> _int))
(define-gmp mpz_fits_sint_p     (_fun _mpz -> _int))
(define-gmp mpz_fits_ushort_p   (_fun _mpz -> _int))
(define-gmp mpz_fits_sshort_p   (_fun _mpz -> _int))

(define (mpz_odd_p z)
  (unless (mpz? z) (raise-argument-error 'mpz_odd_p "mpz?" 0 z))
  (odd? (mpz_get_si z)))

(define (mpz_even_p z)
  (unless (mpz? z) (raise-argument-error 'mpz_even_p "mpz?" 0 z))
  (even? (mpz_get_si z)))

;; mpz_sizeinbase (in private/unsafe, needed for printing)

;; ============================================================
;; mpq

(define (mpq [x 0])
  (unless (or (and (rational? x) (exact? x)) (mpz? q) (mpq? q))
    (raise-argument-error 'mpq "(or/c (and/c rational? exact?) mpz? mpq?)" 0 x))
  (define q (alloc-mpq))
  (mpq_init q)
  (cond [(rational? x)
         (define n (numerator x))
         (define d (denominator x))
         (cond [(and (fixnum? n) (fixnum? d))
                (mpq_set_si q n d)]
               [else
                (mpz-set! (mpq_numref q) n)
                (mpz-set! (mpq_denref q) d)])]
        [(mpz? x)
         (mpz_set (mpq_numref q) x)]
        [(mpq? x)
         (mpq_set q x)])
  q)

(define (mpq->string q)
  (unless (mpq? q) (raise-argument-error 'mpq->string "mpq?" 0 q))
  (format "~a/~a" (mpz->string (mpq_numref q)) (mpz->string (mpq_denref q))))

(define (mpq->number q)
  (unless (mpq? q) (raise-argument-error 'mpq->number "mpq?" 0 q))
  (/ (mpz->number (mpq_numref q)) (mpz->number (mpq_denref q))))

(define (mpq-zero? q)
  (unless (mpq? q) (raise-argument-error 'mpq-zero? "mpq?" 0 q))
  (zero? (mpq_sgn q)))

;; ------------------------------------------------------------
;; Safe mpq Functions

(define-gmp mpq_canonicalize (_fun _mpq -> _void))

;; ----------------------------------------
;; Assignment

(define-gmp mpq_set     (_fun _mpq _mpq -> _void))
(define-gmp mpq_set_z   (_fun _mpq _mpz -> _void))
(define-gmp mpq_set_ui  (_fun _mpq _ulong _ulong -> _void)) ;; FIXME: canonicalize
(define-gmp mpq_set_si  (_fun _mpq _long _ulong -> _void))  ;; FIXME: canonicalize
;; mpq_set_str
(define-gmp mpq_swap    (_fun _mpq _mpq -> _void))

;; ----------------------------------------
;; Conversion

(define-gmp mpq_get_d   (_fun _mpq -> _double))
(define-gmp mpq_set_d   (_fun _mpq _double* -> _void))
;; mpq_set_f
;; mpq_get_str

;; ----------------------------------------
;; Arithmetic

(define-gmp mpq_add     (_fun _mpq _mpq _mpq -> _void))
(define-gmp mpq_sub     (_fun _mpq _mpq _mpq -> _void))
(define-gmp mpq_mul     (_fun _mpq _mpq _mpq -> _void))
(define-gmp mpq_mul_2exp (_fun _mpq _mpq _mp_bitcnt -> _void))
(define-gmp mpq_div     (_fun _mpq _mpq _mpq -> _void)
  #:wrap (lambda (f) (lambda (dst a b)
                       (call-as-atomic
                        (lambda ()
                          (when (and (mpq? b) (mpq-zero? b))
                            (error 'mpq_div "division by zero"))
                          (f dst a b))))))
(define-gmp mpq_div_2exp (_fun _mpq _mpq _mp_bitcnt -> _void))
(define-gmp mpq_neg     (_fun _mpq _mpq -> _void))
(define-gmp mpq_abs     (_fun _mpq _mpq -> _void))
(define-gmp mpq_inv     (_fun _mpq _mpq -> _void)
  #:wrap (lambda (f) (lambda (dst src)
                       (call-as-atomic
                        (lambda ()
                          (when (and (mpq? src) (mpq-zero? src))
                            (error 'mpq_inv "division by zero"))
                          (f dst src))))))

;; ----------------------------------------
;; Comparison

(define-gmp mpq_cmp     (_fun _mpq _mpq -> _int))
(define-gmp mpq_cmp_z   (_fun _mpq _mpz -> _int))

(define-gmp mpq_cmp_ui  (_fun _mpq _ulong _ulong -> _int))
(define-gmp mpq_cmp_si  (_fun _mpq _long  _ulong -> _int))

(define (mpq_sgn q)
  (unless (mpq? q) (raise-argument-error 'mpq_sgn "mpq?" 0 q))
  (define zsize (mpz_struct-mp_size (mpq_struct-mp_num (cast q _mpq _mpq_struct-pointer))))
  (cond [(= zsize 0) 0] [(> zsize 0) 1] [else -1]))

(define-gmp mpq_equal   (_fun _mpq _mpq -> _int))

;; ----------------------------------------
;; Applying Integer Functions to Rationals

(define (mpq_numref q)
  (unless (mpq? q) (raise-argument-error 'mpq_numref "mpq?" 0 q))
  (cast (mpq_struct-mp_num (cast q _mpq _mpq_struct-pointer)) _mpz_struct-pointer _mpz))

(define (mpq_denref q)
  (unless (mpq? q) (raise-argument-error 'mpq_denref "mpq?" 0 q))
  (cast (mpq_struct-mp_den (cast q _mpq _mpq_struct-pointer)) _mpz_struct-pointer _mpz))

(define-gmp mpq_get_num (_fun _mpz _mpq -> _void))
(define-gmp mpq_get_den (_fun _mpz _mpq -> _void))
(define-gmp mpq_set_num (_fun _mpq _mpz -> _void)) ;; FIXME: canonicalize?
(define-gmp mpq_set_den (_fun _mpq _mpz -> _void)) ;; FIXME: canonicalize?
