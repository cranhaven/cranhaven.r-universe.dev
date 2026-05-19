#ifndef DRESS_HISTOGRAM_H
#define DRESS_HISTOGRAM_H

#include <stddef.h>
#include <stdint.h>

#include "dress/dress.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ------------------------------------------------------------------ */
/*  Simple linear-probing hash map for histograms (double -> int64)   */
/* ------------------------------------------------------------------ */

typedef struct {
    double key;
    int64_t count;
    int used; // 0 = empty, 1 = occupied
} dress_hist_entry_t;

typedef struct {
    dress_hist_entry_t *entries;
    size_t capacity;
    size_t size;
} dress_histogram_t;

// Initialize a new histogram.
void dress_hist_init(dress_histogram_t *h, size_t initial_cap);

// Free histogram memory.
void dress_hist_free(dress_histogram_t *h);

// Add a value to the histogram.
void dress_hist_add(dress_histogram_t *h, double val);

// Add `count` copies of a value to the histogram.
void dress_hist_add_count(dress_histogram_t *h, double val, int64_t count);

// Flatten the histogram into a sorted array of int64_t.
// Format: [val_0_bits, count_0, val_1_bits, count_1, ...]
// where val_i_bits is the double value bit-cast to int64_t.
// Returns the array (caller must free) and sets *out_len to 2 * distinct_count.
int64_t *dress_hist_flatten(dress_histogram_t *h, int *out_len);

// Convert the histogram into a sorted array of public histogram pairs.
dress_hist_pair_t *dress_hist_pairs(dress_histogram_t *h, int *out_len);

// Convert a flattened [bits, count, ...] array into histogram pairs.
dress_hist_pair_t *dress_hist_flat_to_pairs(const int64_t *flat, int flat_len,
                                            int *out_len);

#ifdef __cplusplus
}
#endif

#endif // DRESS_HISTOGRAM_H
