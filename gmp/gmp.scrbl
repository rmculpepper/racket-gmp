#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket racket/contract gmp
                     (only-in ffi/unsafe
                              [_int signed-integer]
                              [_long signed-long]
                              [_ulong unsigned-long])))

@title{GMP: Multi-precision Arithmetic}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(define(gmplink url-suffix . pre-flow)
   (apply hyperlink (format "https://gmplib.org/manual/~a" url-suffix) pre-flow))

This library provides safe low-level bindings to GMP, the GNU
Multi-Precision Arithmetic library. This manual assumes familiarity
with the relevant parts of @gmplink["index.html"]{the GMP manual}.

@defmodule[gmp]

This package is distributed under the
@hyperlink["https://www.gnu.org/licenses/lgpl.html"]{LGPL}.  Note
that the GMP native library is dual-licensed under the LGPLv3 and the
GPLv2 (see @gmplink["Copying.html"]{copying conditions} for details).

@; ------------------------------------------------------------
@section[#:tag "mpz"]{mpz: Multi-precision Integers}

An @deftech{mpz} (@tt{mpz_t}) is a @emph{mutable} value containing an
arbitrary-precision integer. In this library, such values are created
with @racket[mpz] and recognized with @racket[mpz?]. Their backing
storage is automatically initialized and garbage-collected.

@defproc[(mpz? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{mpz} value, @racket[#f]
otherwise.
}

@defproc[(mpz [n (or/c exact-integer? mpz?) 0]) mpz?]{

Creates a new @tech{mpz} initialized with the value @racket[n]. If
@racket[n] is an @tech{mpz}, then its value is copied; @racket[n] is
unchanged and does not share storage with the result.
}

@defproc[(mpz-set! [z mpz?] [n exact-integer?]) void?]{

Sets @racket[z] to contain the value @racket[n].
}

@defproc[(mpz->number [z mpz?]) exact-integer?]{

Returns the integer value stored in @racket[z].
}

@deftogether[[
@defproc[(mpz-zero?     [z mpz?]) boolean?]
@defproc[(mpz-positive? [z mpz?]) boolean?]
@defproc[(mpz-negative? [z mpz?]) boolean?]
]]{

Like @racket[zero?], @racket[positive?], and @racket[negative?], respectively.
}

@defproc[(mpz=? [z1 mpz?] [z2 mpz?]) boolean?]{

Returns @racket[#t] if @racket[z1] and @racket[z2] contain equal
integers, @racket[#f] otherwise.
}

@deftogether[[
@defproc[(mpz->bytes [z mpz?] [size (or/c exact-positive-integer? #f)]
                     [signed? boolean?] [big-endian? boolean? #t]
                     [dest-buf bytes? (make-bytes size)] [dest-start 0])
         bytes?]
@defproc[(bytes->mpz [buf bytes?]
                     [signed? boolean?] [big-endian? boolean? #t]
                     [start exact-nonnegative-integer? 0]
                     [end exact-nonnegative-integer? (bytes-length buf)])
         mpz?]
]]{

Like @racket[integer->integer-bytes] and @racket[integer-bytes->integer],
respectively. The @racket[size] argument to @racket[mpz->bytes] can be
any positive integer (as long as it is large enough to represent
@racket[z]), or it may be @racket[#f] to automatically use the
shortest suitable size.
}

@defproc[(mpz-bytes-length [z mpz?] [signed? boolean?])
         exact-positive-integer?]{

Computes the number of bytes needed by @racket[mpz->bytes] to
represent @racket[z].
}

@; ----------------------------------------
@subsection[#:tag "mpz-ops"]{Operations on mpz Values}

The following operations are supported. By GMP convention, destination
arguments come first, followed by source operands.

@deftogether[[
@defproc[(mpz_set    [rop mpz?] [op mpz?]) void?]
@defproc[(mpz_set_ui [rop mpz?] [op unsigned-long]) void?]
@defproc[(mpz_set_si [rop mpz?] [op signed-long]) void?]
@defproc[(mpz_set_d  [rop mpz?] [op rational?]) void?]
@defproc[(mpz_set_q  [rop mpz?] [op mpq?]) void?]
@defproc[(mpz_set_str [rop mpz] [op string?]) void?]
@defproc[(mpz_swap   [rop11 mpz?] [rop2 mpz?]) void?]
]]{

See @gmplink["Assigning-Integers.html"]{Assignment Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpz_get_ui [op mpz?]) exact-nonnegative-integer?]
@defproc[(mpz_get_si [op mpz?]) exact-integer?]
@defproc[(mpz_get_d  [op mpz?]) rational?]
@defproc[(mpz_get_d_2exp [op mpz?]) (values rational? exact-integer?)]
]]{

See @gmplink["Converting-Integers.html"]{Conversion Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpz_add    [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_add_ui [rop mpz?] [op1 mpz?] [op2 unsigned-long]) void?]
@defproc[(mpz_sub    [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_sub_ui [rop mpz?] [op1 mpz?] [op2 unsigned-long]) void?]
@defproc[(mpz_ui_sub [rop mpz?] [op1 unsigned-long] [op2 mpz?]) void?]
@defproc[(mpz_mul    [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_mul_si [rop mpz?] [op1 mpz?] [op2 signed-long]) void?]
@defproc[(mpz_mul_ui [rop mpz?] [op1 mpz?] [op2 unsigned-long]) void?]
@defproc[(mpz_addmul [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_addmul_ui [rop mpz?] [op1 mpz?] [op2 unsigned-long]) void?]
@defproc[(mpz_submul [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_submul_ui [rop mpz?] [op1 mpz?] [op2 unsigned-long]) void?]
@defproc[(mpz_mul_2exp [rop mpz?] [op1 mpz?] [op2 unsigned-long]) void?]
@defproc[(mpz_neg [rop mpz?] [op mpz?]) void?]
@defproc[(mpz_abs [rop mpz?] [op mpz?]) void?]
]]{

See @gmplink["Integer-Arithmetic.html"]{Arithmetic Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpz_cdiv_q     [q mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_cdiv_r     [r mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_cdiv_qr    [q mpz?] [r mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_cdiv_q_ui  [q mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_cdiv_r_ui  [r mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_cdiv_qr_ui [q mpz?] [r mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_cdiv_ui    [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_cdiv_q_2exp [q mpz?] [n mpz?] [b unsigned-long]) void?]
@defproc[(mpz_cdiv_r_2exp [r mpz?] [n mpz?] [b unsigned-long]) void?]
@defproc[(mpz_fdiv_q     [q mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_fdiv_r     [r mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_fdiv_qr    [q mpz?] [r mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_fdiv_q_ui  [q mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_fdiv_r_ui  [r mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_fdiv_qr_ui [q mpz?] [r mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_fdiv_ui    [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_fdiv_q_2exp [q mpz?] [n mpz?] [b unsigned-long]) void?]
@defproc[(mpz_fdiv_r_2exp [r mpz?] [n mpz?] [b unsigned-long]) void?]
@defproc[(mpz_tdiv_q     [q mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_tdiv_r     [r mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_tdiv_qr    [q mpz?] [r mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_tdiv_q_ui  [q mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_tdiv_r_ui  [r mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_tdiv_qr_ui [q mpz?] [r mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_tdiv_ui    [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_tdiv_q_2exp [q mpz?] [n mpz?] [b unsigned-long]) void?]
@defproc[(mpz_tdiv_r_2exp [r mpz?] [n mpz?] [b unsigned-long]) void?]
@defproc[(mpz_mod        [r mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_mod_ui     [r mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_divexact   [q mpz?] [n mpz?] [d mpz?]) void?]
@defproc[(mpz_divexact_ui [q mpz?] [n mpz?] [d unsigned-long]) void?]
@defproc[(mpz_divisible_p [n mpz?] [d mpz?]) exact-integer?]
@defproc[(mpz_divisible_ui_p [n mpz?] [d unsigned-long]) exact-integer?]
@defproc[(mpz_divisible_2exp_p [n mpz?] [b unsigned-long]) exact-integer?]
@defproc[(mpz_congruent [n mpz?] [c mpz?] [d mpz?]) exact-integer?]
@defproc[(mpz_congruent_ui_p [n mpz?] [c mpz?] [d unsigned-long]) exact-integer?]
@defproc[(mpz_congruent_2exp_p [n mpz?] [c mpz?] [b unsigned-long]) exact-integer?]
]]{

See @gmplink["Integer-Division.html"]{Division Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpz_pow_ui    [rop mpz?] [base mpz?] [exp unsigned-long]) void?]
@defproc[(mpz_ui_pow_ui [rop mpz?] [base unsigned-long] [exp unsigned-long]) void?]
]]{

See @gmplink["Integer-Exponentiation.html"]{Exponentiation Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpz_root      [root mpz?] [u mpz?] [n unsigned-long]) void?]
@defproc[(mpz_rootrem   [root mpz?] [rem mpz?] [u mpz?] [n unsigned-long]) void?]
@defproc[(mpz_sqrt      [root mpz?] [op mpz?]) void?]
@defproc[(mpz_sqrtrem   [root mpz?] [rem mpz?] [op mpz?]) void?]
@defproc[(mpz_perfect_power_p [op mpz?]) exact-integer?]
@defproc[(mpz_perfect_square_p [op mpz?]) exact-integer?]
]]{

See @gmplink["Integer-Roots.html"]{Root Extraction Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpz_probab_prime_p [n mpz?] [reps signed-integer]) exact-integer?]
@defproc[(mpz_nextprime   [rop mpz?] [op mpz?]) void?]
@defproc[(mpz_gcd         [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_gcd_ui      [rop mpz?] [op1 mpz?] [op2 unsigned-long]) unsigned-long]
@defproc[(mpz_gcdext      [g mpz?] [s mpz?] [t mpz?] [a mpz?] [b mpz?]) void?]
@defproc[(mpz_lcm         [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_lcm_ui      [rop mpz?] [op1 mpz?] [op2 unsigned-long]) void?]
@defproc[(mpz_invert      [rop mpz?] [op1 mpz?] [op2 mpz?]) exact-integer?]
@defproc[(mpz_kronecker   [a mpz?] [b mpz?]) exact-integer?]
@defproc[(mpz_kronecker_si [a mpz?] [b signed-long]) exact-integer?]
@defproc[(mpz_kronecker_ui [a mpz?] [b unsigned-long]) exact-integer?]
@defproc[(mpz_si_kronecker [a signed-long] [b mpz?]) exact-integer?]
@defproc[(mpz_ui_kronecker [a unsigned-long] [b mpz?]) exact-integer?]
@defproc[(mpz_remove      [rop mpz?] [op mpz?] [f mpz?]) exact-nonnegative-integer?]
@defproc[(mpz_fac_ui      [rop mpz?] [op mpz?]) void?]
@defproc[(mpz_primordial_ui [rop mpz?] [n unsigned-long]) void?]
@defproc[(mpz_bin_ui      [rop mpz?] [n mpz?] [k unsigned-long]) void?]
@defproc[(mpz_bin_uiui    [rop mpz?] [n unsigned-long] [k unsigned-long]) void?]
@defproc[(mpz_fib_ui      [fn mpz?] [n unsigned-long]) void?]
@defproc[(mpz_fib2_ui     [fn mpz?] [fnsub1 mpz?] [n unsigned-long]) void?]
@defproc[(mpz_lucnum_ui   [ln mpz?] [n unsigned-long]) void?]
@defproc[(mpz_lucnum2_ui  [ln mpz?] [lnsub1 mpz?] [n unsigned-long]) void?]
]]{

See @gmplink["Number-Theoretic-Functions.html"]{Number Theoretic Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpz_cmp    [op1 mpz?] [op2 mpz?]) exact-integer?]
@defproc[(mpz_cmp_d  [op1 mpz?] [op2 real?]) exact-integer?]
@defproc[(mpz_cmp_si [op1 mpz?] [op2 signed-long]) exact-integer?]
@defproc[(mpz_cmp_ui [op1 mpz?] [op2 unsigned-long]) exact-integer?]
@defproc[(mpz_cmpabs [op1 mpz?] [op2 mpz?]) exact-integer?]
@defproc[(mpz_cmpabs_d [op1 mpz?] [op2 real?]) exact-integer?]
@defproc[(mpz_cmpabs_ui [op1 mpz?] [op2 unsigned-long]) exact-integer?]
@defproc[(mpz_sgn    [op mpz?]) (or/c -1 0 1)]
]]{

See @gmplink["Integer-Comparisons.html"]{Comparison Functions} in the GMP manual.
}

@deftogether[[

@defproc[(mpz_and  [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_ior  [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_xor  [rop mpz?] [op1 mpz?] [op2 mpz?]) void?]
@defproc[(mpz_com  [rop mpz?] [op mpz?]) void?]
@defproc[(mpz_popcount [op mpz?]) exact-nonnegative-integer?]
@defproc[(mpz_hamdist  [op1 mpz?] [op2 mpz?]) exact-nonnegative-integer?]
@defproc[(mpz_scan0 [op mpz?] [start-bit exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(mpz_scan1 [op mpz?] [start-bit exact-nonnegative-integer?]) exact-nonnegative-integer?]
@defproc[(mpz_clrbit [rop mpz?] [bit-index exact-nonnegative-integer?]) void?]
@defproc[(mpz_combit [rop mpz?] [bit-index exact-nonnegative-integer?]) void?]
@defproc[(mpz_tstbit [op mpz?] [bit-index exact-nonnegative-integer?]) (or/c 0 1)]
]]{

See @gmplink["Integer-Logic-and-Bit-Fiddling.html"]{Logical and Bit
Manipulation Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpz_urandomb [rop mpz?] [state gmp-randstate?] [n exact-nonnegative-integer?]) void?]
@defproc[(mpz_urandomm [rop mpz?] [state gmp-randstate?] [n mpz?]) void?]
@defproc[(mpz_rrandomb [rop mpz?] [state gmp-randstate?] [n exact-nonnegative-integer?]) void?]
]]{

See @gmplink["Integer-Random-Numbers.html"]{Integer Random Numbers} in the GMP manual.

@history[#:added "1.2"]
}


@deftogether[[
@defproc[(mpz_fits_ulong_p [op mpz?]) exact-integer?]
@defproc[(mpz_fits_slong_p [op mpz?]) exact-integer?]
@defproc[(mpz_fits_uint_p [op mpz?]) exact-integer?]
@defproc[(mpz_fits_sint_p [op mpz?]) exact-integer?]
@defproc[(mpz_fits_ushort_p [op mpz?]) exact-integer?]
@defproc[(mpz_fits_sshort_p [op mpz?]) exact-integer?]
@defproc[(mpz_odd_p [op mpz?]) exact-integer?]
@defproc[(mpz_even_p [op mpz?]) exact-integer?]
@defproc[(mpz_sizeinbase [op mpz?] [base (integer-in 2 62)]) exact-nonnegative-integer?]
]]{

See @gmplink["Miscellaneous-Functions.html"]{Miscellaneous Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpz_size [op mpz?]) exact-nonnegative-integer?]
@defproc[(mpz_getlimbn [op mpz?] [n unsigned-long]) exact-nonnegative-integer?]
]]{

See @gmplink["Integer-Special-Functions.html"]{Special Functions} in the GMP manual.
}


@; ------------------------------------------------------------
@section[#:tag "mpq"]{mpq: Multi-precision Rational Numbers}

An @deftech{mpq} (@tt{mpq_t}) is a @emph{mutable} value containing a
rational number with arbitrary-precision numerator and denominator. In
this library, such values are created with @racket[mpq] and recognized
with @racket[mpq?]. Their backing storage is automatically initialized
and garbage-collected.

@defproc[(mpq? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{mpq} value, @racket[#f]
otherwise.
}

@defproc[(mpq [x (or/c (and/c rational? exact?) mpz? mpq?)]) mpq?]{

Creates a new @tech{mpq} initialized with the value @racket[x]. If
@racket[x] is an @tech{mpz} or @tech{mpq}, then its value is copied;
@racket[x] is unchanged does not share storage with the result.
}

@defproc[(mpq->number [q mpq?]) (and/c rational? exact?)]{

Returns the exact rational value stored in @racket[z].
}

@deftogether[[
@defproc[(mpq-zero?     [q mpq?]) boolean?]
@defproc[(mpq-positive? [q mpq?]) boolean?]
@defproc[(mpq-negative? [q mpq?]) boolean?]
]]{

Like @racket[zero?], @racket[positive?], and @racket[negative?], respectively.
}


@; ----------------------------------------
@subsection[#:tag "mpq-ops"]{Operations on mpq Values}

The following operations are supported. By GMP convention, destination
arguments come first, followed by source operands.

@deftogether[[
@defproc[(mpq_canonicalize [op mpq?]) void?]
]]{

See @gmplink["Rational-Number-Functions.html"]{Rational Number Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpq_set   [rop mpq?] [op mpq?]) void?]
@defproc[(mpq_set_z [rop mpq?] [op mpz?]) void?]
@defproc[(mpq_set_ui [rop mpq?] [n unsigned-long] [d unsigned-long]) void?]
@defproc[(mpq_set_si [rop mpq?] [n signed-long] [d unsigned-long]) void?]
@defproc[(mpq_swap [rop1 mpq?] [rop2 mpq?]) void?]
]]{

See @gmplink["Initializing-Rationals.html"]{Initializing Rationals} in the GMP manual.
}

@deftogether[[
@defproc[(mpq_get_d  [op mpq?]) real?]
@defproc[(mpq_set_d  [rop mpq?] [op real?]) void?]
]]{

See @gmplink["Rational-Conversions.html"]{Conversion Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpq_add [rop mpq?] [op1 mpq?] [op2 mpq?]) void?]
@defproc[(mpq_sub [rop mpq?] [op1 mpq?] [op2 mpq?]) void?]
@defproc[(mpq_mul [rop mpq?] [op1 mpq?] [op2 mpq?]) void?]
@defproc[(mpq_mul_2exp [rop mpq?] [op1 mpq?] [op2 unsigned-long]) void?]
@defproc[(mpq_div [rop mpq?] [op1 mpq?] [op2 mpq?]) void?]
@defproc[(mpq_div_2exp [rop mpq?] [op1 mpq?] [op2 unsigned-long?]) void?]
@defproc[(mpq_neg [rop mpq?] [op mpq?]) void?]
@defproc[(mpq_abs [rop mpq?] [op mpq?]) void?]
@defproc[(mpq_inv [rop mpq?] [op mpq?]) void?]
]]{

See @gmplink["Rational-Arithmetic.html"]{Arithmetic Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpq_cmp    [op1 mpq?] [op2 mpq?]) exact-integer?]
@defproc[(mpq_cmp_z  [op1 mpq?] [op2 mpz?]) exact-integer?]
@defproc[(mpq_cmp_ui [op1 mpq?] [n2 unsigned-long] [d2 unsigned-long]) exact-integer?]
@defproc[(mpq_cmp_si [op1 mpq?] [n2 signed-long] [d2 unsigned-long]) exact-integer?]
@defproc[(mpq_equal  [op1 mpq?] [op2 mpq?]) exact-integer?]
@defproc[(mpq_sgn    [op mpq?]) (or/c -1 0 1)]
]]{

See @gmplink["Comparing-Rationals.html"]{Comparision Functions} in the GMP manual.
}

@deftogether[[
@defproc[(mpq_get_num  [num mpz?] [rational mpq?]) void?]
@defproc[(mpq_get_den  [den mpz?] [rational mpq?]) void?]
@defproc[(mpq_set_num  [rational mpq?] [num mpz?]) void?]
@defproc[(mpq_set_den  [rational mpq?] [den mpq?]) void?]
]]{

See @gmplink["Applying-Integer-Functions.html"]{Applying Integer
Functions to Rationals} in the GMP manual.
}

@; ----------------------------------------
@section[#:tag "mpq-ops"]{Random Number Generation}

A @emph{randstate} (@tt{gmp_randstate_t}) represents a random number
generation algorithm and its state. In this library, such values are
recognized with @racket[gmp-randstate?] and created with
@racket[gmp-randstate] and @racket[gmp-randstate-mt]. They are
automatically initialized and garbage-collected.

@defproc[(gmp-randstate? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tt{gmp_randstate_t} value,
@racket[#f] otherwise.

@history[#:added "1.2"]
}

@deftogether[[
@defproc[(gmp-randstate) gmp-randstate?]
@defproc[(gmp-randstate-mt) gmp-randstate?]
]]{

Returns a new @tt{gmp_randstate_t} initialized with
@tt{gmp_randinit_default} or @tt{gmp_randinit_mt}, respectively.

@history[#:added "1.2"]
}

@deftogether[[
@defproc[(gmp_randseed [state gmp-randstate?] [seed mpz?]) void?]
@defproc[(gmp_randseed_ui [state gmp-randstate?] [seed exact-nonnegative-integer?]) void?]
]]{

See @gmplink["Random-State-Seeding.html"]{Random State Seeding} in the
GMP manual.

@history[#:added "1.2"]
}

@deftogether[[
@defproc[(gmp_urandomb_ui [state gmp-randstate?] [n exact-nonnegative-integer?])
         exact-nonnegative-integer?]
@defproc[(gmp_urandomm_ui [state gmp_randstate?] [n exact-positive-integer?])
         exact-nonnegative-integer?]
]]{

See @gmplink["Random-State-Miscellaneous.html"]{Random State
Miscellaneous} in the GMP manual.

@history[#:added "1.2"]
}
