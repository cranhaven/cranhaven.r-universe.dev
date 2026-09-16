#ifndef MDFS_REDUCE_COUNTERS_H
#define MDFS_REDUCE_COUNTERS_H

#include <cstddef>
#include <cstdint>

inline void reduce_counters(size_t n_classes, size_t n_cubes, const float *in, float *out, size_t rstride) {
    for (size_t c = 0, v = 0; c < n_cubes; c += rstride * n_classes) {
        for (size_t s = 0; s < rstride; ++s, ++v) {
            for (size_t d = 0; d < n_classes; ++d) {
                out[v] += in[c + s + (d * rstride)];
            }
        }
    }
}

#endif
