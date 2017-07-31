#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/struct
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)

;; SAFETY ISSUES: We provide a safe API and an unsafe API. The unsafe
;; API must be provided with protect-out. A client of the safe API
;; should never be able to cause a crash (eg memory protection fault,
;; integer divide by zero trap, etc). To that end:
;; - A safe client should not be able to get an uninitialized mpz or mpq.
;; - Some operations cause low-level traps on eg divide by zero. When
;;   possible, we check for zero args and raise exn instead (see eg
;;   _mpz/NZ), and we consider that operation safe. For others, it is
;;   nontrivial to predict trap, and we mark the operation unsafe.
;; - Non-canonical mpqs are not considered unsafe, but mpqs with 0
;;   denominators are unsafe.

;; FIXME: may not be thread-safe; eg, (mpq_inv q) if q mutated after NZ check


;; ============================================================
;; libgmp, type definitions

(provide (protect-out define-gmp0 define-gmp))

(define-ffi-definer define-gmp0 (ffi-lib "libgmp" '(#f "10"))
  #:default-make-fail make-not-available)

(define-syntax (define-gmp stx)
  (syntax-case stx ()
    [(define-gmp name type)
     #'(define-gmp name type #:provide)]
    [(define-gmp name type mode option ...)
     (with-syntax ([cname (format-id #'name "__g~a" #'name)]
                   [provide-clause
                    (case (syntax-e #'mode)
                      [(#:provide) #'(provide name)]
                      [(#:unsafe)  #'(provide (protect-out name))]
                      [(#:local)   #'(begin)]
                      [else (raise-syntax-error #f "bad mode" stx #'mode)])])
       #'(begin (define-gmp0 name type #:c-id cname option ...)
                provide-clause))]))

;; ----------------------------------------

(provide (protect-out
          (struct-out mpz_struct)
          (struct-out mpq_struct)
          _mpz_struct
          _mpq_struct
          _mpz
          _mpq
          _mp_bitcnt
          _ulong/NZ
          _mpz/NZ
          _mpq/NZ))

(define-cstruct _mpz_struct
  ([mp_alloc _int]
   [mp_size  _int]
   [mp_d     _pointer])
  #:malloc-mode 'atomic-interior
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (z) 'mpz) (lambda (z) (list (mpz->number z)))))

(define-cstruct _mpq_struct
  ([mp_num   _mpz_struct]
   [mp_den   _mpz_struct])
  #:malloc-mode 'atomic-interior
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (q) 'mpq) (lambda (q) (list (mpq->number q)))))

(define _mpz _mpz_struct-pointer)
(define _mpq _mpq_struct-pointer)
(define _mp_bitcnt _ulong)

(define (_ulong/NZ who)
  (make-ctype _ulong (lambda (v) (if (zero? v) (error who "division by zero") v)) #f))
(define (_mpz/NZ who)
  (make-ctype _mpz (lambda (z) (if (mpz-zero? z) (error who "division by zero") z)) #f))
(define (_mpq/NZ who)
  (make-ctype _mpq (lambda (q) (if (mpq-zero? q) (error who "division by zero") q)) #f))


;; ============================================================
;; mpz

(provide mpz?
         mpz
         mpz-set!
         mpz->number
         mpz->string
         mpz-zero?
         mpz-positive?
         mpz-negative?)

(define mpz? mpz_struct?)

(define (mpz [n 0])
  (define z (make-mpz_struct 0 0 #f))
  (mpz_init z)
  (cond [(exact-integer? n)
         (mpz-set! z n)]
        [(mpz? n)
         (mpz_set z n)])
  z)

(define (mpz-set! z n)
  (define absn (abs n))
  (for ([i (in-range (integer-length absn))])
    (when (bitwise-bit-set? absn i) (mpz_setbit z i)))
  (when (negative? n) (mpz_neg z z)))

(define (mpz->number z)
  ;; FIXME!!!
  (string->number (mpz->string z 16) 16))

(define (mpz->string z [base 10])
  (define buf (make-bytes (+ (mpz_sizeinbase z base) (if (= (mpz_sgn z) -1) 1 0))))
  (mpz_get_str buf base z)
  (bytes->string/latin-1 buf))

(define (mpz-zero? z)     (zero? (mpz_struct-mp_size z)))
(define (mpz-positive? z) (positive? (mpz_sgn z)))
(define (mpz-negative? z) (negative? (mpz_sgn z)))

;; ----

(define (mpz_divisible? n d)        (not (zero? (mpz_divisible_p n d))))
(define (mpz_divisible_ui? n d)     (not (zero? (mpz_divisible_ui_p n d))))
(define (mpz_divisible_2exp? n b)   (not (zero? (mpz_divisible_2exp_p n b))))
(define (mpz_congruent? n c d)      (not (zero? (mpz_congruent_p n c d))))
(define (mpz_congruent_ui? n c d)   (not (zero? (mpz_congruent_ui_p n c d))))
(define (mpz_congruent_2exp? n c b) (not (zero? (mpz_congruent_2exp_p n c b))))
(define (mpz_perfect_power? z)      (not (zero? (mpz_perfect_power_p z))))
(define (mpz_perfect_square? z)     (not (zero? (mpz_perfect_square_p z))))
;; mpz_probab_prime_p is a little too weird to predicatify

(define (mpz_fits_ulong? z) (not (zero? (mpz_fits_ulong_p z))))
(define (mpz_fits_slong? z) (not (zero? (mpz_fits_slong_p z))))
(define (mpz_fits_uint? z) (not (zero? (mpz_fits_uint_p z))))
(define (mpz_fits_sint? z) (not (zero? (mpz_fits_sint_p z))))
(define (mpz_fits_ushort? z) (not (zero? (mpz_fits_ushort_p z))))
(define (mpz_fits_sshort? z) (not (zero? (mpz_fits_sshort_p z))))

(define (mpz_odd? z)  (odd?  (mpz_get_si z)))
(define (mpz_even? z) (even? (mpz_get_si z)))

(define (mpz-bit-set? z i) (not (zero? (mpz_tstbit z i))))

;; FIXME: mpz->bytes, bytes->mpz

;; ------------------------------------------------------------
;; FFI Functions and Constants

(define-gmp mp_bits_per_limb _int #:provide #:fail (* 8 (ctype-sizeof _ulong)))

;; ----------------------------------------
;; Initialization

(define-gmp mpz_clear     (_fun _mpz -> _void) #:unsafe #:wrap (deallocator))
(define-gmp mpz_init      (_fun (z : _mpz) -> _void -> z) #:unsafe #:wrap (allocator mpz_clear))
(define-gmp mpz_init2     (_fun (z : _mpz) _mp_bitcnt -> _void -> z) #:unsafe #:wrap (allocator mpz_clear))
(define-gmp mpz_realloc2  (_fun _mpz _mp_bitcnt -> _void) #:unsafe)

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
;; Combined Initialization and Assignment

(define-gmp mpz_init_set (_fun _mpz _mpz -> _void) #:unsafe #:wrap (allocator mpz_clear))
(define-gmp mpz_init_set_ui (_fun _mpz _ulong -> _void) #:unsafe #:wrap (allocator mpz_clear))
(define-gmp mpz_init_set_si (_fun _mpz _long -> _void)  #:unsafe #:wrap (allocator mpz_clear))
(define-gmp mpz_init_set_d (_fun _mpz _double -> _void) #:unsafe #:wrap (allocator mpz_clear))
(define-gmp mpz_init_set_str (_fun _mpz _string/latin-1 _int -> _void)
  #:unsafe #:wrap (allocator mpz_clear))

;; ----------------------------------------
;; Conversion

(define-gmp mpz_get_ui  (_fun _mpz -> _ulong))
(define-gmp mpz_get_si  (_fun _mpz -> _long))
(define-gmp mpz_get_d   (_fun _mpz -> _double))
(define-gmp mpz_get_d_2exp (_fun (exp : (_ptr o _long)) _mpz -> (d : _double) -> (values d exp)))
(define-gmp mpz_get_str (_fun _pointer _int _mpz -> _void) #:unsafe)

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
(define-gmp mpz_cdiv_q      (_fun _mpz _mpz (_mpz/NZ 'mpz_cdiv_q) -> _void))
(define-gmp mpz_cdiv_r      (_fun _mpz _mpz (_mpz/NZ 'mpz_cdiv_r) -> _void))
(define-gmp mpz_cdiv_qr     (_fun _mpz _mpz _mpz (_mpz/NZ 'mpz_cdiv_qr) -> _void))
(define-gmp mpz_cdiv_q_ui   (_fun _mpz _mpz (_ulong/NZ 'mpz_cdiv_q_ui) -> _void))
(define-gmp mpz_cdiv_r_ui   (_fun _mpz _mpz (_ulong/NZ 'mpz_cdiv_r_ui) -> _void))
(define-gmp mpz_cdiv_qr_ui  (_fun _mpz _mpz _mpz (_ulong/NZ 'mpz_cdiv_qr_ui) -> _void))
(define-gmp mpz_cdiv_ui     (_fun _mpz (_ulong/NZ 'mpz_cdiv_ui) -> _void))
(define-gmp mpz_cdiv_q_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))
(define-gmp mpz_cdiv_r_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))

(define-gmp mpz_fdiv_q      (_fun _mpz _mpz (_mpz/NZ 'mpz_fdiv_q) -> _void))
(define-gmp mpz_fdiv_r      (_fun _mpz _mpz (_mpz/NZ 'mpz_fdiv_r) -> _void))
(define-gmp mpz_fdiv_qr     (_fun _mpz _mpz _mpz (_mpz/NZ 'mpz_fdiv_qr) -> _void))
(define-gmp mpz_fdiv_q_ui   (_fun _mpz _mpz (_ulong/NZ 'mpz_fdiv_q_ui) -> _void))
(define-gmp mpz_fdiv_r_ui   (_fun _mpz _mpz (_ulong/NZ 'mpz_fdiv_r_ui) -> _void))
(define-gmp mpz_fdiv_qr_ui  (_fun _mpz _mpz _mpz (_ulong/NZ 'mpz_fdiv_qr_ui) -> _void))
(define-gmp mpz_fdiv_ui     (_fun _mpz (_ulong/NZ 'mpz_fdiv_ui) -> _void))
(define-gmp mpz_fdiv_q_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))
(define-gmp mpz_fdiv_r_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))

(define-gmp mpz_tdiv_q      (_fun _mpz _mpz (_mpz/NZ 'mpz_tdiv_q) -> _void))
(define-gmp mpz_tdiv_r      (_fun _mpz _mpz (_mpz/NZ 'mpz_tdiv_r) -> _void))
(define-gmp mpz_tdiv_qr     (_fun _mpz _mpz _mpz (_mpz/NZ 'mpz_tdiv_qr) -> _void))
(define-gmp mpz_tdiv_q_ui   (_fun _mpz _mpz (_ulong/NZ 'mpz_tdiv_q_ui) -> _void))
(define-gmp mpz_tdiv_r_ui   (_fun _mpz _mpz (_ulong/NZ 'mpz_tdiv_r_ui) -> _void))
(define-gmp mpz_tdiv_qr_ui  (_fun _mpz _mpz _mpz (_ulong/NZ 'mpz_tdiv_qr_ui) -> _void))
(define-gmp mpz_tdiv_ui     (_fun _mpz (_ulong/NZ 'mpz_tdiv_ui) -> _void))
(define-gmp mpz_tdiv_q_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))
(define-gmp mpz_tdiv_r_2exp (_fun _mpz _mpz _mp_bitcnt -> _void))

(define-gmp mpz_mod         (_fun _mpz _mpz (_mpz/NZ 'mpz_mod) -> _void))
(define-gmp mpz_mod_ui      (_fun _mpz _mpz (_ulong/NZ 'mpz_mod_ui) -> _void))

(define-gmp mpz_divexact    (_fun _mpz _mpz (_mpz/NZ 'mpz_divexact) -> _void))   ;; FIXME: safe?
(define-gmp mpz_divexact_ui (_fun _mpz _mpz (_ulong/NZ 'mpz_divexact_ui) -> _void)) ;; FIXME: safe?

(define-gmp mpz_divisible_p      (_fun _mpz _mpz -> _int))
(define-gmp mpz_divisible_ui_p   (_fun _mpz _ulong -> _int))
(define-gmp mpz_divisible_2exp_p (_fun _mpz _mp_bitcnt -> _int))

(define-gmp mpz_congruent_p      (_fun _mpz _mpz _mpz -> _int))
(define-gmp mpz_congruent_ui_p   (_fun _mpz _ulong _ulong -> _int))
(define-gmp mpz_congruent_2exp_p (_fun _mpz _mpz _mp_bitcnt -> _int))

;; ----------------------------------------
;; Exponentiation

(define-gmp mpz_powm      (_fun _mpz _mpz _mpz (_mpz/NZ 'mpz_powm) -> _void)      #:unsafe)
(define-gmp mpz_powm_ui   (_fun _mpz _mpz _mpz (_ulong/NZ 'mpz_powm_ui) -> _void) #:unsafe)
(define-gmp mpz_powm_sec  (_fun _mpz _mpz _mpz (_mpz/NZ 'mpz_powm_sec) -> _void)  #:unsafe)

(define-gmp mpz_pow_ui    (_fun _mpz _mpz _ulong -> _void))
(define-gmp mpz_ui_pow_ui (_fun _mpz _ulong _ulong -> _void))

;; ----------------------------------------
;; Root Extraction

(define-gmp mpz_root      (_fun _mpz _mpz _ulong -> _int))
(define-gmp mpz_rootrem   (_fun _mpz _mpz _mpz _ulong -> _void))
(define-gmp mpz_sqrt      (_fun _mpz _mpz -> _void))
(define-gmp mpz_sqrtrem   (_fun _mpz _mpz _mpz -> _void))

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

;; FIXME: safe?
(define-gmp mpz_jacobi          (_fun _mpz _mpz -> _int) #:unsafe) ;; ??
(define-gmp mpz_legendre        (_fun _mpz _mpz -> _int) #:unsafe) ;; ??
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

(define (mpz_sgn z)
  (define zsize (mpz_struct-mp_size z))
  (cond [(= zsize 0) 0] [(> zsize 0) 1] [else -1]))
(provide mpz_sgn)

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
;; Input and Output (skipped)

;; ----------------------------------------
;; Random Numbers

;; (define-gmp mpz_urandomb (_fun _mpz _gmp_randstate _mp_bitcnt -> _void))
;; (define-gmp mpz_urandomm (_fun _mpz _gmp_randstate _mpz -> _void) #:unsafe) ;; ??
;; (define-gmp mpz_rrandomb (_fun _mpz _gmp_randstate _mp_bitcnt -> _void))
;; mpz_random (deprecated)
;; mpz_random2 (deprecated)

;; ----------------------------------------
;; Import and Export

(define-gmp mpz_import
  (_fun _mpz _size _int _size _int _size _pointer -> _void)
  #:unsafe)
(define-gmp mpz_export
  (_fun _pointer (count : (_ptr o _size)) _int _size _int _size _mpz -> _pointer -> count)
  #:unsafe)

;; ----------------------------------------
;; Miscellaneous

(define-gmp mpz_fits_ulong_p  (_fun _mpz -> _int))
(define-gmp mpz_fits_slong_p  (_fun _mpz -> _int))
(define-gmp mpz_fits_uint_p   (_fun _mpz -> _int))
(define-gmp mpz_fits_sint_p   (_fun _mpz -> _int))
(define-gmp mpz_fits_ushort_p (_fun _mpz -> _int))
(define-gmp mpz_fits_sshort_p (_fun _mpz -> _int))

(define (mpz_odd_p z)  (if (odd?  (mpz_get_ui z)) 1 0))
(define (mpz_even_p z) (if (even? (mpz_get_ui z)) 1 0))
(provide mpz_odd_p mpz_even_p)

(define-gmp mpz_sizeinbase    (_fun _mpz _int -> _size))

;; ----------------------------------------
;; Special Functions

;; mpz_getlimbn
(define-gmp mpz_size          (_fun _mpz -> _size))
(define-gmp mpz_limbs_read    (_fun _mpz -> _pointer)        #:unsafe)
(define-gmp mpz_limbs_write   (_fun _mpz _size -> _pointer)  #:unsafe)
(define-gmp mpz_limbs_modify  (_fun _mpz _size -> _pointer)  #:unsafe)
(define-gmp mpz_limbs_finish  (_fun _mpz _size -> _void)     #:unsafe)


;; ============================================================
;; mpq

(provide mpq?
         mpq
         mpq->number
         mpq->string
         mpq-zero?
         mpq-negative?
         mpq-positive?)

(define mpq? mpq_struct?)

(define (mpq [x 0])
  (define q (make-mpq_struct z0 z0))
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

(define z0 (mpz 0)) ;; FIXME?

(define (mpq->number q)
  (/ (mpz->number (mpq_numref q)) (mpz->number (mpq_denref q))))

(define (mpq->string q) (number->string (mpq->number q)))

(define (mpq-zero? q)     (mpz-zero? (mpq_struct-mp_num q)))
(define (mpq-positive? q) (mpz-positive? (mpq_struct-mp_num q)))
(define (mpq-negative? q) (mpz-negative? (mpq_struct-mp_num q)))

;; ------------------------------------------------------------
;; FFI Functions and Constants

(define-gmp mpq_canonicalize (_fun _mpq -> _void))

;; ----------------------------------------
;; Initialization and Assigment

(define-gmp mpq_clear   (_fun _mpq -> _void)            #:unsafe #:wrap (deallocator))
(define-gmp mpq_init    (_fun (q : _mpq) -> _void -> q) #:unsafe #:wrap (allocator mpq_clear))

(define-gmp mpq_set     (_fun _mpq _mpq -> _void))
(define-gmp mpq_set_z   (_fun _mpq _mpz -> _void))
(define-gmp mpq_set_ui  (_fun _mpq _ulong (_ulong/NZ 'mpq_set_ui) -> _void)) ;; non-canonical
(define-gmp mpq_set_si  (_fun _mpq _long  (_ulong/NZ 'mpq_set_si) -> _void)) ;; non-canonical
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

(define-gmp mpq_add       (_fun _mpq _mpq _mpq -> _void))
(define-gmp mpq_sub       (_fun _mpq _mpq _mpq -> _void))
(define-gmp mpq_mul       (_fun _mpq _mpq _mpq -> _void))
(define-gmp mpq_mul_2exp  (_fun _mpq _mpq _mp_bitcnt -> _void))
(define-gmp mpq_div       (_fun _mpq _mpq (_mpq/NZ 'mpq_div) -> _void))
(define-gmp mpq_div_2exp  (_fun _mpq _mpq _mp_bitcnt -> _void))
(define-gmp mpq_neg       (_fun _mpq _mpq -> _void))
(define-gmp mpq_abs       (_fun _mpq _mpq -> _void))
(define-gmp mpq_inv       (_fun _mpq (_mpq/NZ 'mpq_inv) -> _void))

;; ----------------------------------------
;; Comparison

(define-gmp mpq_cmp     (_fun _mpq _mpq -> _int))
(define-gmp mpq_cmp_z   (_fun _mpq _mpz -> _int))
(define-gmp mpq_cmp_ui  (_fun _mpq _ulong (_ulong/NZ 'mpq_cmp_ui) -> _int))
(define-gmp mpq_cmp_si  (_fun _mpq _long  (_ulong/NZ 'mpq_cmp_si) -> _int))
(define-gmp mpq_equal   (_fun _mpq _mpq -> _int))

(define (mpq_sgn q) (mpz_sgn (mpq_struct-mp_num q)))
(provide mpq_sgn)

;; ----------------------------------------
;; Applying Integer Functions to Rationals

(define (mpq_numref q) (mpq_struct-mp_num q))
(define (mpq_denref q) (mpq_struct-mp_den q)) ;; UNSAFE

(define-gmp mpq_get_num (_fun _mpz _mpq -> _void))
(define-gmp mpq_get_den (_fun _mpz _mpq -> _void))
(define-gmp mpq_set_num (_fun _mpq _mpz -> _void)) ;; non-canonical
(define-gmp mpq_set_den (_fun _mpq (_mpz/NZ 'mpq_set_den) -> _void)) ;; non-canonical
