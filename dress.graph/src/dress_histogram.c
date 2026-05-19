#include "dress_histogram.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>

#define MAX_LOAD 0.75

// splitmix64-style hash: bit-cast double to uint64, then mix.
static inline uint64_t hash_double(double d) {
    uint64_t u;
    memcpy(&u, &d, sizeof(double));
    u ^= u >> 30;
    u *= 0xbf58476d1ce4e5b9ULL;
    u ^= u >> 27;
    u *= 0x94d049bb133111ebULL;
    u ^= u >> 31;
    return u;
}

void dress_hist_init(dress_histogram_t *h, size_t initial_cap) {
    if (initial_cap < 16) initial_cap = 16;
    // Round up to next power of 2 (required for & mask indexing)
    size_t cap = 16;
    while (cap < initial_cap) cap <<= 1;
    h->entries = (dress_hist_entry_t *)calloc(cap, sizeof(dress_hist_entry_t));
    h->capacity = cap;
    h->size = 0;
}

void dress_hist_free(dress_histogram_t *h) {
    free(h->entries);
    h->entries = NULL;
    h->size = 0;
    h->capacity = 0;
}

static void resize(dress_histogram_t *h);

void dress_hist_add(dress_histogram_t *h, double val) {
    if (h->size + 1 > h->capacity * MAX_LOAD) {
        resize(h);
    }
    
    // Linear probing
    size_t idx = hash_double(val) & (h->capacity - 1);
    while (h->entries[idx].used) {
        if (h->entries[idx].key == val) { // exact double equality check
            h->entries[idx].count++;
            return;
        }
        idx = (idx + 1) & (h->capacity - 1);
    }
    
    // Insert new entry
    h->entries[idx].key = val;
    h->entries[idx].count = 1;
    h->entries[idx].used = 1;
    h->size++;
}

void dress_hist_add_count(dress_histogram_t *h, double val, int64_t count) {
    if (count <= 0) return;
    if (h->size + 1 > h->capacity * MAX_LOAD) {
        resize(h);
    }

    size_t idx = hash_double(val) & (h->capacity - 1);
    while (h->entries[idx].used) {
        if (h->entries[idx].key == val) {
            h->entries[idx].count += count;
            return;
        }
        idx = (idx + 1) & (h->capacity - 1);
    }

    h->entries[idx].key = val;
    h->entries[idx].count = count;
    h->entries[idx].used = 1;
    h->size++;
}

static void resize(dress_histogram_t *h) {
    size_t old_cap = h->capacity;
    dress_hist_entry_t *old_entries = h->entries;
    
    h->capacity *= 2;
    h->entries = (dress_hist_entry_t *)calloc(h->capacity, sizeof(dress_hist_entry_t));
    
    // Must reset size because add will re-increment it
    h->size = 0;
    
    // Re-insert
    for (size_t i = 0; i < old_cap; i++) {
        if (old_entries[i].used) {
            // We need to re-insert preserving the count!
            // dress_hist_add increments count to 1.
            // So we need a specialized internal add or just manually add
            
            double val = old_entries[i].key;
            size_t idx = hash_double(val) & (h->capacity - 1);
            while (h->entries[idx].used) {
                idx = (idx + 1) & (h->capacity - 1);
            }
            h->entries[idx].key = val;
            h->entries[idx].count = old_entries[i].count;
            h->entries[idx].used = 1;
            h->size++;
        }
    }
    
    free(old_entries);
}

// Comparison for flatten sort. Sort by value ascending.
static int cmp_hist_entries(const void *a, const void *b) {
    const dress_hist_entry_t *ea = (const dress_hist_entry_t *)a;
    const dress_hist_entry_t *eb = (const dress_hist_entry_t *)b;
    if (ea->key < eb->key) return -1;
    if (ea->key > eb->key) return 1;
    return 0;
}

int64_t *dress_hist_flatten(dress_histogram_t *h, int *out_len) {
    if (!h->size) {
        if (out_len) *out_len = 0;
        return NULL;
    }
    
    // Collect dense entries
    dress_hist_entry_t *dense = (dress_hist_entry_t *)malloc(h->size * sizeof(dress_hist_entry_t));
    size_t count = 0;
    for (size_t i = 0; i < h->capacity; i++) {
        if (h->entries[i].used) {
            dense[count++] = h->entries[i];
        }
    }
    
    // Sort by key value
    qsort(dense, count, sizeof(dress_hist_entry_t), cmp_hist_entries);
    
    // Flatten into int64 array [val_bits, count, val_bits, count...]
    int64_t *result = (int64_t *)malloc(count * 2 * sizeof(int64_t));
    for (size_t i = 0; i < count; i++) {
        int64_t bits;
        memcpy(&bits, &dense[i].key, sizeof(double));
        result[2 * i] = bits;
        result[2 * i + 1] = dense[i].count;
    }
    
    free(dense);
    
    if (out_len) *out_len = (int)(count * 2);
    return result;
}

dress_hist_pair_t *dress_hist_pairs(dress_histogram_t *h, int *out_len) {
    if (!h->size) {
        if (out_len) *out_len = 0;
        return NULL;
    }

    dress_hist_entry_t *dense = (dress_hist_entry_t *)malloc(h->size * sizeof(dress_hist_entry_t));
    size_t count = 0;
    for (size_t i = 0; i < h->capacity; i++) {
        if (h->entries[i].used) {
            dense[count++] = h->entries[i];
        }
    }

    qsort(dense, count, sizeof(dress_hist_entry_t), cmp_hist_entries);

    dress_hist_pair_t *result = (dress_hist_pair_t *)malloc(count * sizeof(dress_hist_pair_t));
    for (size_t i = 0; i < count; i++) {
        result[i].value = dense[i].key;
        result[i].count = dense[i].count;
    }

    free(dense);
    if (out_len) *out_len = (int)count;
    return result;
}

dress_hist_pair_t *dress_hist_flat_to_pairs(const int64_t *flat, int flat_len,
                                            int *out_len) {
    if (!flat || flat_len <= 0) {
        if (out_len) *out_len = 0;
        return NULL;
    }

    int num_pairs = flat_len / 2;
    dress_hist_pair_t *pairs = (dress_hist_pair_t *)malloc((size_t)num_pairs * sizeof(dress_hist_pair_t));
    for (int i = 0; i < num_pairs; i++) {
        memcpy(&pairs[i].value, &flat[2 * i], sizeof(double));
        pairs[i].count = flat[2 * i + 1];
    }

    if (out_len) *out_len = num_pairs;
    return pairs;
}
