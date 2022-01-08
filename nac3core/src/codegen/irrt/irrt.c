typedef _ExtInt(8) int8_t;
typedef unsigned _ExtInt(8) uint8_t;
typedef _ExtInt(32) int32_t;
typedef unsigned _ExtInt(32) uint32_t;
typedef _ExtInt(64) int64_t;
typedef unsigned _ExtInt(64) uint64_t;

// adapted from GNU Scientific Library: https://git.savannah.gnu.org/cgit/gsl.git/tree/sys/pow_int.c
// need to make sure `exp >= 0` before calling this function
#define  DEF_INT_EXP(T) T __nac3_irrt_int_exp_##T( \
    T base, \
    T exp \
) { \
    T res = (T)1; \
    /* repeated squaring method */ \
    do { \
       if (exp & 1) res *= base;  /* for n odd */ \
       exp >>= 1; \
       base *= base; \
    } while (exp); \
    return res; \
} \

DEF_INT_EXP(int32_t)
DEF_INT_EXP(int64_t)
