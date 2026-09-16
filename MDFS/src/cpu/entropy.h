#ifndef ENTROPY_H
#define ENTROPY_H

#include <cstddef>
#include <cmath>


// only 2 decision classes are supported
// (note it does not make sense for 1 decision class)
template <uint8_t n_decision_classes>
inline float conditional_entropy(size_t n_cubes, const float *c) {
    float H = 0.0f;

    for (size_t i = 0; i < n_cubes; ++i) {
        float c_sum = c[i];
        if (n_decision_classes > 1) {
            c_sum += c[n_cubes + i];
        }
        H -= (c[i]) * std::log2(c[i]/c_sum);
        if (n_decision_classes > 1) {
            H -= (c[n_cubes + i]) * std::log2(c[n_cubes + i]/c_sum);
        }
    }

    return H;
}

inline float entropy(float total, size_t n_cubes, const float *c) {
    float H = 0.0f;

    for (size_t i = 0; i < n_cubes; ++i) {
        H -= (c[i]/total) * std::log2(c[i]/total);
    }

    return H;
}

#endif
