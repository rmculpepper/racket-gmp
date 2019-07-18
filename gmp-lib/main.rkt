#lang racket/base
(require racket/contract/base
         "private/gmp.rkt")

(provide mpz?
         (contract-out
          [mpz          (->* [] [(or/c exact-integer? mpz?)] mpz?)]
          [mpz-set!     (-> mpz? exact-integer? void?)]
          [mpz->number  (-> mpz? exact-integer?)]
          [mpz->string  (->* [mpz?] [exact-positive-integer?] string?)]
          [mpz-zero?    (-> mpz? boolean?)]
          [mpz-positive? (-> mpz? boolean?)]
          [mpz-negative? (-> mpz? boolean?)]
          [mpz=?         (-> mpz? mpz? boolean?)]
          [mpz->bytes (->* [mpz? (or/c exact-positive-integer? #f) boolean?]
                           [boolean? bytes? exact-nonnegative-integer?]
                           bytes?)]
          [mpz-bytes-length (-> mpz? boolean? exact-positive-integer?)]
          [bytes->mpz (->* [bytes? boolean?]
                           [boolean? exact-nonnegative-integer? exact-nonnegative-integer?]
                           mpz?)])

         mpq?
         (contract-out
          [mpq          (->* [] [(or/c (and/c rational? exact?) mpz? mpq?)] mpq?)]
          [mpq->number  (-> mpq? (and/c rational? exact?))]
          [mpq-zero?    (-> mpq? boolean?)]
          [mpq-positive? (-> mpq? boolean?)]
          [mpq-negative? (-> mpq? boolean?)])

         gmp-randstate?
         (contract-out
          [gmp-randstate (-> gmp-randstate?)]
          [gmp-randstate-mt (-> gmp-randstate?)]))

;; ---- mpz ----
(provide mp_bits_per_limb
         ;; -- Initialization --
         ;; -- Assignment --
         mpz_set
         mpz_set_ui
         mpz_set_si
         mpz_set_d
         mpz_set_q
         ;; mpz_set_f
         mpz_set_str
         mpz_swap
         ;; -- Combined Initialization and Assignment --
         ;; -- Conversion --
         mpz_get_ui
         mpz_get_si
         mpz_get_d
         mpz_get_d_2exp
         ;; -- Arithmetic --
         mpz_add
         mpz_add_ui
         mpz_sub
         mpz_sub_ui
         mpz_ui_sub
         mpz_mul
         mpz_mul_si
         mpz_mul_ui
         mpz_addmul
         mpz_addmul_ui
         mpz_submul
         mpz_submul_ui
         mpz_mul_2exp
         mpz_neg
         mpz_abs
         ;; -- Division --
         mpz_cdiv_q
         mpz_cdiv_r
         mpz_cdiv_qr
         mpz_cdiv_q_ui
         mpz_cdiv_r_ui
         mpz_cdiv_qr_ui
         mpz_cdiv_ui
         mpz_cdiv_q_2exp
         mpz_cdiv_r_2exp
         mpz_fdiv_q
         mpz_fdiv_r
         mpz_fdiv_qr
         mpz_fdiv_q_ui
         mpz_fdiv_r_ui
         mpz_fdiv_qr_ui
         mpz_fdiv_ui
         mpz_fdiv_q_2exp
         mpz_fdiv_r_2exp
         mpz_tdiv_q
         mpz_tdiv_r
         mpz_tdiv_qr
         mpz_tdiv_q_ui
         mpz_tdiv_r_ui
         mpz_tdiv_qr_ui
         mpz_tdiv_ui
         mpz_tdiv_q_2exp
         mpz_tdiv_r_2exp
         mpz_mod
         mpz_mod_ui
         mpz_divexact   ;; FIXME: safe?
         mpz_divexact_ui ;; FIXME: safe?
         mpz_divisible_p
         mpz_divisible_ui_p
         mpz_divisible_2exp_p
         mpz_congruent_p
         mpz_congruent_ui_p
         mpz_congruent_2exp_p
         ;; -- Exponentiation --
         mpz_pow_ui
         mpz_ui_pow_ui
         ;; -- Root Extraction --
         mpz_root
         mpz_rootrem
         mpz_sqrt
         mpz_sqrtrem
         mpz_perfect_power_p
         mpz_perfect_square_p
         ;; -- Number Theoretic Functions --
         mpz_probab_prime_p
         mpz_nextprime
         mpz_gcd
         mpz_gcd_ui
         mpz_gcdext
         mpz_lcm
         mpz_lcm_ui
         mpz_invert
         mpz_kronecker
         mpz_kronecker_si
         mpz_kronecker_ui
         mpz_si_kronecker
         mpz_ui_kronecker
         mpz_remove
         mpz_fac_ui
         ;; mpz_2fac_ui     ;; buggy??
         ;; mpz_mfac_uiui   ;; buggy??
         mpz_primordial_ui
         mpz_bin_ui
         mpz_bin_uiui
         mpz_fib_ui
         mpz_fib2_ui
         mpz_lucnum_ui
         mpz_lucnum2_ui
         ;; -- Comparison --
         mpz_cmp
         mpz_cmp_d
         mpz_cmp_si
         mpz_cmp_ui
         mpz_cmpabs
         mpz_cmpabs_d
         mpz_cmpabs_ui
         (contract-out
          [mpz_sgn (-> mpz? fixnum?)])
         ;; -- Logical and Bit Manipulation Functions --
         mpz_and
         mpz_ior
         mpz_xor
         mpz_com
         mpz_popcount
         mpz_hamdist
         mpz_scan0
         mpz_scan1
         mpz_setbit
         mpz_clrbit
         mpz_combit
         mpz_tstbit
         ;; -- Input and Output --
         ;; -- Random Numbers --
         mpz_urandomb
         mpz_urandomm
         mpz_rrandomb
         ;; -- Import and Export --
         ;; -- Miscellaneous --
         mpz_fits_ulong_p
         mpz_fits_slong_p
         mpz_fits_uint_p
         mpz_fits_sint_p
         mpz_fits_ushort_p
         mpz_fits_sshort_p
         (contract-out
          [mpz_odd_p  (-> mpz? fixnum?)]
          [mpz_even_p (-> mpz? fixnum?)])
         mpz_sizeinbase
         ;; -- Special Functions --
         ;; mpz_getlimbn
         mpz_size)

;; ---- mpq ----
(provide mpq_canonicalize
         ;; -- Initialization and Assigment --
         mpq_set
         mpq_set_z
         mpq_set_ui
         mpq_set_si
         ;; mpq_set_str
         mpq_swap
         ;; -- Conversion --
         mpq_get_d
         mpq_set_d
         ;; mpq_set_f
         ;; mpq_get_str
         ;; -- Arithmetic --
         mpq_add
         mpq_sub
         mpq_mul
         mpq_mul_2exp
         mpq_div
         mpq_div_2exp
         mpq_neg
         mpq_abs
         mpq_inv
         ;; -- Comparison --
         mpq_cmp
         mpq_cmp_z
         mpq_cmp_ui
         mpq_cmp_si
         mpq_equal
         (contract-out
          [mpq_sgn (-> mpq? fixnum?)])
         ;; -- Applying Integer Functions to Rationals --
         mpq_get_num
         mpq_get_den
         mpq_set_num
         mpq_set_den)

(provide gmp_randseed
         gmp_randseed_ui
         gmp_urandomb_ui
         gmp_urandomm_ui)
