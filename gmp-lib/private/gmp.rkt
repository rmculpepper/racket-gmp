#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/struct
         racket/runtime-path
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

;; Cooperate with `raco distribute`:
(define-runtime-path libgmp-so '(so "libgmp" ("10" #f)))

(define-ffi-definer define-gmp0 (ffi-lib libgmp-so '("10" #f))
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

(define fixnum-fits-long?
  (<= (ctype-sizeof _fixnum) (ctype-sizeof _long)))

;; ----------------------------------------

(provide (protect-out
          (struct-out mpz_struct)
          (struct-out mpq_struct)
          (struct-out gmp_randstate_struct)
          mpz_struct-tag
          mpq_struct-tag
          gmp_randstate_struct-tag
          _mpz_struct
          _mpq_struct
          _gmp_randstate_struct
          _mpz
          _mpq
          _gmp_randstate
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

(define (_mpz/pos who)
  (make-ctype _mpz
              (lambda (z)
                (if (mpz-positive? z) z (error who "expected positive mpz\n  given: ~e" z)))
              #f))

(define (_ulong/pos who)
  (make-ctype _ulong
              (lambda (n)
                (if (positive? n) n (error who "expected non-zero _ulong value\n  given: ~e" n)))
              #f))

(define-cstruct _gmp_randstate_struct
  ([mp_seed _mpz_struct]
   [mp_alg  _int #|gmp_randalg_t is enum|#]
   [mp_algdata _pointer])
  #:malloc-mode 'atomic-interior)

(define _gmp_randstate _gmp_randstate_struct-pointer)

;; ============================================================
;; mpz

(provide mpz?
         mpz
         mpz-set!
         mpz->number
         mpz->string
         mpz-zero?
         mpz-positive?
         mpz-negative?
         mpz=?)

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
  (cond [(and (fixnum? n) fixnum-fits-long?)
         (mpz_set_si z n)]
        #|
        [else ;; This is slow.
         ;; For 800-bit and 8000-bit unsigned integers: 4-8x slower than string->number
         (mpz_set_ui z 0)
         ;; use 32-bit chunks
         (define NBITS 32)
         (define na (abs n))
         (define len (integer-length na))
         (mpz_realloc2 z len) ;; avoid repeated realloc
         (define nchunks (quotient (+ len NBITS -1) NBITS))
         (let loop ([i (sub1 nchunks)])
           (when (i . >= . 0)
             (mpz_mul_2exp z z NBITS)
             (mpz_add_ui z z (bitwise-bit-field na (* i NBITS) (* (add1 i) NBITS)))
             (loop (sub1 i))))
         (when (negative? n) (mpz_neg z z))]
        [else ;; This is really slow.
         ;; For 800-bit and 8000-bit unsigned integers: 20-30x slower than string->number
         (define na (abs n))
         (define len (integer-length na))
         (mpz_set_ui z 0)
         (mpz_realloc2 z len)
         (for ([i (in-range (add1 len))])
           (when (bitwise-bit-set? na i) (mpz_setbit z i)))
         (when (negative? n) (mpz_neg z z))]
        |#
        [else
         ;; It might seem silly to go through number->string, but it's actually
         ;; *significantly* faster than the alternatives.
         (or (zero? (mpz_set_str z (number->string n 16) 16))
             (error 'mpz-set! "internal error: mpz_set_str failed\n  n: ~e" n))])
  (void))

(define (mpz->number z)
  (cond [(mpz_fits_slong? z) (mpz_get_si z)]
        [else (string->number (mpz->string z 16) 16)]))

(define (mpz->string z [base 10])
  ;; Racket CS bytes don't include NUL terminator, so include it in buf itself.
  (define buf (make-bytes (+ 1 (mpz_sizeinbase z base) (if (= (mpz_sgn z) -1) 1 0))))
  (mpz_get_str buf base z)
  (cast buf _bytes _string/latin-1)) ;; handles NUL terminator

(define (mpz-zero? z)     (zero? (mpz_struct-mp_size z)))
(define (mpz-positive? z) (positive? (mpz_sgn z)))
(define (mpz-negative? z) (negative? (mpz_sgn z)))
(define (mpz=? z1 z2) (zero? (mpz_cmp z1 z2)))

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

;; ----

(provide (protect-out
          mpz_export_base256
          mpz_import_base256)
         mpz-bytes-length
         mpz->bytes
         bytes->mpz)

(define-gmp0 mpz_export_base256
  (_fun (buf src big-endian?) ::
        (buf : _pointer)      ;; pointer to chunk[]
        (len : (_ptr o _size))
        (order : _int = (if big-endian? 1 -1))  ;; (1 = most significant chunk first)
        (size : _size = 1)  ;; sizeof(chunk)
        (endian : _int = (if big-endian? 1 -1)) ;; (1 = most significant byte first in chunk)
        (nails : _size = 0) ;; no unused bits per chunk
        (src : _mpz)
        -> _void
        -> len)
  #:c-id __gmpz_export)

(define (mpz-bytes-length z signed?)
  (cond [(mpz-negative? z)
         (unless signed?
           (error 'mpz-bytes-length "cannot convert negative mpz as unsigned\n  mpz: ~e" z))
         (define pz (mpz z))
         (mpz_add_ui pz pz 1)
         (define size-in-bits (+ (mpz_sizeinbase pz 2) 1))
         (quotient (+ size-in-bits 7) 8)]
        [else
         (define size-in-bits (+ (mpz_sizeinbase z 2) (if signed? 1 0)))
         (quotient (+ size-in-bits 7) 8)]))

(define (mpz->bytes z len signed? [big-endian? #t]
                    [buf (and len (make-bytes len 0))] [start 0])
  ;; len,buf may be #f meaning calculate shortest; if so, check-avail sets them
  (define (check-avail needlen)
    (cond [len
           (unless (<= needlen len)
             (error 'mpz->bytes
                    "mpz does not fit into requested ~a bytes\n  mpz: ~e\n  requested bytes: ~e"
                    (if signed? "signed" "unsigned") z len))]
          [else
           (set! len needlen)
           (cond [buf (check-buf)]
                 [else (set! buf (make-bytes needlen 0))])]))
  (define (check-buf)
    (define buflen (bytes-length buf))
    (unless (>= buflen (+ start len))
      (error 'mpz->bytes
             (string-append "byte string length is shorter than starting position plus size"
                            "\n  byte string length: ~s\n  start: ~s\n  size: ~s")
             buflen start len)))
  (when buf (check-buf))
  ;; ----
  (cond [(mpz-negative? z)
         (unless signed?
           (error 'mpz->bytes "cannot convert negative mpz as unsigned\n  mpz: ~e" z))
         ;; need same number of bits as |z|-1, plus sign bit
         (define pz (mpz z))
         (mpz_add_ui pz pz 1)
         (define size-in-bits (add1 (mpz_sizeinbase pz 2))) ;; add sign bit
         (define size-in-bytes (quotient (+ size-in-bits 7) 8)) ;; = ceil(size-in-bits / 8)
         (check-avail size-in-bytes)
         (mpz_set_ui pz 1)
         (mpz_mul_2exp pz pz (* len 8))
         (mpz_add pz pz z)
         (mpz_export_base256 (ptr-add buf start) pz big-endian?)]
        [(mpz-zero? z)
         ;; mpz_export produces zero bytes for (mpz 0), so simpler to handle separately
         (check-avail 1)
         (memset buf start 0 len)]
        [else ;; positive
         (define size-in-bits (mpz_sizeinbase z 2))
         (define size-in-bytes (quotient (+ size-in-bits 7) 8)) ;; = ceil(size-in-bits / 8)
         (define tight? (zero? (remainder size-in-bits 8)))
         (check-avail (+ size-in-bytes (if (and signed? tight?) 1 0)))
         (memset buf start 0 len)
         ;; if big-endian, sign byte and extra space at front; if little-endian, at end
         (define offset
           (cond [big-endian? (- len size-in-bytes)]
                 [else 0]))
         (mpz_export_base256 (ptr-add buf (+ start offset)) z big-endian?)])
  buf)

(define-gmp0 mpz_import_base256
  (_fun (dst count src big-endian?) ::
        (dst : _mpz)
        (count : _size)
        (order : _int = (if big-endian? 1 -1))  ;; (1 = most significant chunk first)
        (size : _size = 1)  ;; sizeof(chunk)
        (endian : _int = (if big-endian? 1 -1)) ;; (1 = most significant byte first in chunk)
        (nails : _size = 0) ;; no unused bits per chunk
        (src : _pointer)    ;; pointer to chunk[count]
        -> _void)
  #:c-id __gmpz_import)

(define (bytes->mpz buf signed? [big-endian? #t] [start 0] [end (bytes-length buf)])
  (define buflen (bytes-length buf))
  (define len (- end start))
  (unless (<= 0 start buflen)
    (raise-range-error 'bytes->mpz "byte string" "starting " start buf 0 buflen))
  (unless (<= start end buflen)
    (raise-range-error 'bytes->mpz "byte string" "ending " end buf 0 buflen start))
  (when (zero? len)
    (error 'bytes->mpz "byte string range is empty\n  start: ~e\n  end: ~e\n  byte string: ~e"
           start end buf))
  (define z (mpz))
  (mpz_import_base256 z len (ptr-add buf start) big-endian?)
  (when (and signed? (mpz-bit-set? z (sub1 (* len 8))))
    (define pz (mpz 1))
    (mpz_mul_2exp pz pz (* len 8))
    (mpz_sub z z pz))
  z)

;; ------------------------------------------------------------
;; FFI Functions and Constants

(define-gmp mp_bits_per_limb _int #:provide #:fail (* 8 (ctype-sizeof _ulong)))

(define _mp_limb
  (or (for/or ([t (list _uint16 _uint32 _uint64 _uintptr)])
        (and (= mp_bits_per_limb (* 8 (ctype-sizeof t))) t))
      _ulong))

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

(define-gmp mpz_urandomb (_fun _mpz _gmp_randstate _mp_bitcnt -> _void))
(define-gmp mpz_urandomm (_fun _mpz _gmp_randstate (_mpz/pos 'mpz_urandomm) -> _void))
(define-gmp mpz_rrandomb (_fun _mpz _gmp_randstate _mp_bitcnt -> _void))

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

(define-gmp mpz_getlimbn      (_fun _mpz _mp_bitcnt -> _mp_limb))
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
(define (mpq_denref q) (mpq_struct-mp_den q)) ;; unsafe; can be set to 0
(provide (protect-out mpq_numref mpq_denref))

(define-gmp mpq_get_num (_fun _mpz _mpq -> _void))
(define-gmp mpq_get_den (_fun _mpz _mpq -> _void))
(define-gmp mpq_set_num (_fun _mpq _mpz -> _void)) ;; non-canonical
(define-gmp mpq_set_den (_fun _mpq (_mpz/NZ 'mpq_set_den) -> _void)) ;; non-canonical


;; ============================================================
;; random state

(provide gmp-randstate?
         gmp-randstate
         gmp-randstate-mt
         (protect-out
          gmp_randinit_default
          gmp_randinit_mt
          gmp_randinit_lc_2exp
          gmp_randinit_lc_2exp_size
          gmp_randinit_set
          gmp_randclear)
         gmp_randseed
         gmp_randseed_ui
         gmp_urandomb_ui
         gmp_urandomm_ui)

(define gmp-randstate? gmp_randstate_struct?)

(define (gmp-randstate)
  (gmp_randinit_default (make-gmp_randstate_struct (mpz) 0 #f)))
(define (gmp-randstate-mt)
  (gmp_randinit_mt (make-gmp_randstate_struct (mpz) 0 #f)))

(define-gmp0 gmp_randclear (_fun _gmp_randstate -> _void)
  #:wrap (deallocator)
  #:c-id __gmp_randclear)

(define-gmp0 gmp_randinit_default
  (_fun (rs : _gmp_randstate) -> _void -> rs)
  #:wrap (allocator gmp_randclear)
  #:c-id __gmp_randinit_default)
(define-gmp0 gmp_randinit_mt
  (_fun (rs : _gmp_randstate) -> _void -> rs)
  #:wrap (allocator gmp_randclear)
  #:c-id __gmp_randinit_mt)
(define-gmp0 gmp_randinit_lc_2exp
  (_fun (rs : _gmp_randstate) _mpz _ulong _mp_bitcnt -> _void -> rs)
  #:wrap (allocator gmp_randclear)
  #:c-id __gmp_randinit_lc_2exp)
(define-gmp0 gmp_randinit_lc_2exp_size
  (_fun (rs : _gmp_randstate) _mp_bitcnt -> _void -> rs)
  #:wrap (allocator gmp_randclear)
  #:c-id __gmp_randinit_lc_2exp_size)
(define-gmp0 gmp_randinit_set
  (_fun (rs : _gmp_randstate) _gmp_randstate -> _void -> rs)
  #:wrap (allocator gmp_randclear)
  #:c-id __gmp_randinit_set)

(define-gmp0 gmp_randseed (_fun _gmp_randstate _mpz -> _void)
  #:c-id __gmp_randseed)
(define-gmp0 gmp_randseed_ui (_fun _gmp_randstate _ulong -> _void)
  #:c-id __gmp_randseed_ui)

(define-gmp0 gmp_urandomb_ui (_fun _gmp_randstate _ulong -> _ulong)
  #:c-id __gmp_urandomb_ui)
(define-gmp0 gmp_urandomm_ui (_fun _gmp_randstate (_ulong/pos 'gmp_urandomm_ui) -> _ulong)
  #:c-id __gmp_urandomm_ui)
