#ifndef MDFS_COUNT_COUNTERS_H
#define MDFS_COUNT_COUNTERS_H

#include <cstddef>
#include <cstdint>
#include <cstring>


// only 1 and 2 decision classes are supported
template <uint8_t n_decision_classes, uint8_t n_dimensions, bool with_contrast>
inline void count_counters(
    const uint8_t *data,
    const uint8_t *contrast_data,
    const uint8_t *decision,
    const size_t n_objects,
    const size_t n_classes,

    const size_t* tuple,
    const size_t contrast_idx,

    float* counters,
    const size_t n_cubes,

    const float p[n_decision_classes],
    const size_t* d
) {
    std::memset(counters, 0, sizeof(float) * n_cubes * n_decision_classes);

    for (size_t o = 0; o < n_objects; ++o) {
        size_t bucket = 0;
        if (n_dimensions >= 1) {
            bucket += data[tuple[0] * n_objects + o];
        }
        if (n_dimensions >= 2) {
            bucket += n_classes * data[tuple[1] * n_objects + o];
        }
        if (n_dimensions >= 3) {
            bucket += d[0] * data[tuple[2] * n_objects + o];
        }
        if (n_dimensions >= 4) {
            bucket += d[1] * data[tuple[3] * n_objects + o];
        }
        if (n_dimensions >= 5) {
            bucket += d[2] * data[tuple[4] * n_objects + o];
        }
        if (with_contrast) {
            if (n_dimensions == 0) {
                bucket += contrast_data[contrast_idx * n_objects + o];
            } else if (n_dimensions == 1) {
                bucket += n_classes * contrast_data[contrast_idx * n_objects + o];
            } else {
                bucket += d[n_dimensions-2] * contrast_data[contrast_idx * n_objects + o];
            }
        }

        if (n_decision_classes > 1) {
            size_t dec = decision[o];
            counters[dec * n_cubes + bucket] += 1.0f;
        } else {
            counters[bucket] += 1.0f;
        }
    }

    for (size_t c = 0; c < n_cubes; ++c) {
        counters[c] += p[0];
        if (n_decision_classes > 1) {
            counters[n_cubes + c] += p[1];
        }
    }
}

#endif
