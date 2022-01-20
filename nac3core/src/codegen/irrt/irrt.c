typedef _ExtInt(8) int8_t;
typedef unsigned _ExtInt(8) uint8_t;
typedef _ExtInt(32) int32_t;
typedef unsigned _ExtInt(32) uint32_t;
typedef _ExtInt(64) int64_t;
typedef unsigned _ExtInt(64) uint64_t;

# define MAX(a, b) (a > b ? a : b)
# define MIN(a, b) (a > b ? b : a)

// adapted from GNU Scientific Library: https://git.savannah.gnu.org/cgit/gsl.git/tree/sys/pow_int.c
// need to make sure `exp >= 0` before calling this function
#define DEF_INT_EXP(T) T __nac3_int_exp_##T( \
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


int32_t __nac3_slice_index_bound(int32_t i, const int32_t len) {
    if (i < 0) {
        i = len + i;
    }
    if (i < 0) {
        return 0;
    } else if (i > len) {
        return len;
    }
    return i;
}

int32_t __nac3_range_slice_len(const int32_t start, const int32_t end, const int32_t step) {
    int32_t diff = end - start;
    if (diff > 0 && step > 0) {
        return ((diff - 1) / step) + 1;
    } else if (diff < 0 && step < 0) {
        return ((diff + 1) / step) + 1;
    } else {
        return 0;
    }
}

// Handle list assignment and dropping part of the list when
// both dest_step and src_step are +1.
// - All the index must *not* be out-of-bound or negative,
// - The end index is *inclusive*,
// - The length of src and dest slice size should already
// be checked: if dest.step == 1 then len(src) <= len(dest) else len(src) == len(dest)
# define DEF_SLICE_ASSIGN(T) int32_t __nac3_list_slice_assign_##T( \
    int32_t dest_start, \
    int32_t dest_end, \
    int32_t dest_step, \
    T *dest_arr, \
    int32_t dest_arr_len, \
    int32_t src_start, \
    int32_t src_end, \
    int32_t src_step, \
    T *src_arr, \
    int32_t src_arr_len \
) { \
    /* if dest_arr_len == 0, do nothing since we do not support extending list */ \
    if (dest_arr_len == 0) return dest_arr_len; \
    /* if both step is 1, memmove directly, handle the dropping of the list, and shrink size */ \
    if (src_step == dest_step && dest_step == 1) { \
        const int32_t src_len = (src_end >= src_start) ? (src_end - src_start + 1) : 0; \
        const int32_t dest_len = (dest_end >= dest_start) ? (dest_end - dest_start + 1) : 0; \
        if (src_len > 0) { \
            __builtin_memmove( \
                dest_arr + dest_start, \
                src_arr + src_start, \
                src_len * sizeof(T) \
            ); \
        } \
        if (dest_len > 0) { \
            /* dropping */ \
            __builtin_memmove( \
                dest_arr + dest_start + src_len, \
                dest_arr + dest_end + 1, \
                (dest_arr_len - dest_end - 1) * sizeof(T) \
            ); \
        } \
        /* shrink size */ \
        return dest_arr_len - (dest_len - src_len); \
    } \
    /* if two range overlaps, need alloca */ \
    uint8_t need_alloca = \
        (dest_arr == src_arr) \
        && !( \
            MAX(dest_start, dest_end) < MIN(src_start, src_end) \
            || MAX(src_start, src_end) < MIN(dest_start, dest_end) \
        ); \
    if (need_alloca) { \
        T *tmp = alloca(src_arr_len * sizeof(T)); \
        __builtin_memcpy(tmp, src_arr, src_arr_len * sizeof(T)); \
        src_arr = tmp; \
    } \
    int32_t src_ind = src_start; \
    int32_t dest_ind = dest_start; \
    for (; \
        (src_step > 0) ? (src_ind <= src_end) : (src_ind >= src_end); \
        src_ind += src_step, dest_ind += dest_step \
    ) { \
        dest_arr[dest_ind] = src_arr[src_ind]; \
    } \
    /* only dest_step == 1 can we shrink the dest list. */ \
    /* size should be ensured prior to calling this function */ \
    if (dest_step == 1 && dest_end >= dest_start) { \
        __builtin_memmove( \
            dest_arr + dest_ind, \
            dest_arr + dest_end + 1, \
            (dest_arr_len - dest_end - 1) * sizeof(T) \
        ); \
        return dest_arr_len - (dest_end - dest_ind) - 1; \
    } \
    return dest_arr_len; \
} \

DEF_SLICE_ASSIGN(uint8_t)
DEF_SLICE_ASSIGN(uint32_t)
DEF_SLICE_ASSIGN(uint64_t)

int32_t __nac3_list_slice_assign_var_size(
    int32_t dest_start,
    int32_t dest_end,
    int32_t dest_step,
    uint8_t *dest_arr,
    int32_t dest_arr_len,
    int32_t src_start,
    int32_t src_end,
    int32_t src_step,
    uint8_t *src_arr,
    int32_t src_arr_len,
    const int32_t size
) {
    /* if dest_arr_len == 0, do nothing since we do not support extending list */
    if (dest_arr_len == 0) return dest_arr_len;
    /* if both step is 1, memmove directly, handle the dropping of the list, and shrink size */
    if (src_step == dest_step && dest_step == 1) {
        const int32_t src_len = (src_end >= src_start) ? (src_end - src_start + 1) : 0;
        const int32_t dest_len = (dest_end >= dest_start) ? (dest_end - dest_start + 1) : 0;
        if (src_len > 0) {
            __builtin_memmove(
                dest_arr + dest_start * size,
                src_arr + src_start * size,
                src_len * size
            );
        }
        if (dest_len > 0) {
            /* dropping */
            __builtin_memmove(
                dest_arr + (dest_start + src_len) * size,
                dest_arr + (dest_end + 1) * size,
                (dest_arr_len - dest_end - 1) * size
            );
        }
        /* shrink size */
        return dest_arr_len - (dest_len - src_len);
    }
    /* if two range overlaps, need alloca */
    uint8_t need_alloca =
        (dest_arr == src_arr)
        && !(
            MAX(dest_start, dest_end) < MIN(src_start, src_end)
            || MAX(src_start, src_end) < MIN(dest_start, dest_end)
        );
    if (need_alloca) {
        uint8_t *tmp = alloca(src_arr_len * size);
        __builtin_memcpy(tmp, src_arr, src_arr_len * size);
        src_arr = tmp;
    }
    int32_t src_ind = src_start;
    int32_t dest_ind = dest_start;
    for (;
        (src_step > 0) ? (src_ind <= src_end) : (src_ind >= src_end);
        src_ind += src_step, dest_ind += dest_step
    ) {
        /* memcpy for var size, cannot overlap after previous alloca */
        __builtin_memcpy(dest_arr + dest_ind * size, src_arr + src_ind * size, size);
    }
    /* only dest_step == 1 can we shrink the dest list. */
    /* size should be ensured prior to calling this function */
    if (dest_step == 1 && dest_end >= dest_start) {
        __builtin_memmove(
            dest_arr + dest_ind * size,
            dest_arr + (dest_end + 1) * size,
            (dest_arr_len - dest_end - 1) * size
        );
        return dest_arr_len - (dest_end - dest_ind) - 1;
    }
    return dest_arr_len;
}