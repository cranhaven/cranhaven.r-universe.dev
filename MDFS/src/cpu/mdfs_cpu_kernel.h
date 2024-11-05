#ifndef MDFS_CPU_KERNEL_H
#define MDFS_CPU_KERNEL_H

#include <cstddef>
#include <cstdint>
#include <cstring>

#include "entropy.h"
#include "mdfs_count_counters.h"
#include "mdfs_reduce_counters.h"

enum StatMode { Entropy, MutualInformation, VariationOfInformation };

// only 1 and 2 decision classes are supported
template <uint8_t n_decision_classes, uint8_t n_dimensions, StatMode stat_mode>
inline void process_tuple(
    const uint8_t *data,
    const uint8_t *decision,
    const size_t n_objects,
    const size_t n_classes,

    const size_t* tuple,

    float* counters,
    float* counters_reduced,
    const size_t n_cubes,
    const size_t n_cubes_reduced,

    const float p[n_decision_classes],
    const float total,  // total of all counters; used only in no decision mode
    const size_t* d,

    const float H_Y,  // H(Y) (plain) entropy of decision

    const float* H,  // decisionless -> entropies of single variables
                     // decisionful  -> entropies of decision conditioned on single variables

    float igs[n_dimensions]
) {
    count_counters<n_decision_classes, n_dimensions, false>(data, nullptr, decision, n_objects, n_classes, tuple, 0, counters, n_cubes, p, d);

    // H(Y|{X_i}) conditional entropy of decision given all tuple vars
    float H_Y_given_all = 0.0f;
    if (n_decision_classes > 1) {
        H_Y_given_all = conditional_entropy<n_decision_classes>(n_cubes, counters);
    }
    // H({X_i}) (plain) entropy of all tuple vars
    float H_all = 0.0f;
    if (n_decision_classes == 1) {
        H_all = entropy(total, n_cubes, counters);
    }

    // optimised 1D version
    if (n_dimensions == 1) {
        if (n_decision_classes == 1) {
            // only one value type can be computed here - the entropy
            igs[0] = H_all;
        } else {
            switch (stat_mode) {
                case Entropy:
                    // H(Y|X_0) conditional entropy
                    igs[0] = H_Y_given_all;
                    break;
                case MutualInformation:
                    // I(Y;X_0) mutual information
                    igs[0] = H_Y - H_Y_given_all;
                    break;
                case VariationOfInformation:
                    break;  // impossible
            }
        }
        return;
    }

    // optimised entropy mode
    if (stat_mode == StatMode::Entropy) {
        for (size_t v = 0; v < n_dimensions; ++v) {
            if (n_decision_classes == 1) {
                // (plain) entropy
                igs[v] = H_all;
            } else {
                // conditional entropy
                igs[v] = H_Y_given_all;
            }
        }
        return;
    }

    // optimised 2D version
    if (n_dimensions == 2) {
        if (n_decision_classes == 1) {
            switch (stat_mode) {
                case Entropy:
                    break;  // already returned
                case MutualInformation:
                    // I(X_0;X_1) mutual information
                    igs[0] = igs[1] = H[tuple[0]] + H[tuple[1]] - H_all;
                    break;
                case VariationOfInformation:
                    // VI(X_0;X_1) variation of information
                    igs[0] = igs[1] = 2 * H_all - H[tuple[0]] - H[tuple[1]];
                    break;
            }
        } else {
            switch (stat_mode) {
                case Entropy:
                    break;  // already returned
                case MutualInformation:
                    // I(Y;X_0|X_1) conditional mutual information (information gain; IG)
                    igs[0] = H[tuple[1]] - H_Y_given_all;
                    // I(Y;X_1|X_0) conditional mutual information (information gain; IG)
                    igs[1] = H[tuple[0]] - H_Y_given_all;
                    break;
                case VariationOfInformation:
                    // ?(D|X_0,X_1)
                    igs[0] = igs[1] = H[tuple[0]] + H[tuple[1]] - 2 * H_Y_given_all;
                    break;
            }
        }
        return;
    }

    // only information gain (IG) is supported beyond this point

    for (size_t v = 0, stride = 1; v < n_dimensions; ++v, stride *= n_classes) {
        std::memset(counters_reduced, 0, sizeof(float) * n_cubes_reduced * n_decision_classes);
        reduce_counters(n_classes, n_cubes, counters, counters_reduced, stride);
        if (n_decision_classes > 1) {
            reduce_counters(n_classes, n_cubes, counters + n_cubes, counters_reduced + n_cubes_reduced, stride);
            // H(Y|{X_i!=X_k}) conditional entropy of decision given all tuple vars except the current one (X_k)
            float H_Y_given_all_except_current = conditional_entropy<n_decision_classes>(n_cubes_reduced, counters_reduced);
            // only one value type can be computed here
            // I(Y;X_k | {X_i!=X_k}) mutual information of decision and the current var given all the other tuple vars
            igs[v] = H_Y_given_all_except_current - H_Y_given_all;
        }
    }
}

// only 2 decision classes are supported
template <uint8_t n_decision_classes, uint8_t n_dimensions>
inline void process_subtuple(
    const uint8_t *data,
    const uint8_t *contrast_data,
    const uint8_t *decision,
    const size_t n_objects,
    const size_t n_classes,

    const size_t* subtuple,
    const size_t contrast_idx,

    float* counters,
    float* counters_reduced,
    const size_t n_cubes,
    const size_t n_cubes_reduced,

    const float p[n_decision_classes],
    const size_t* d,

    float *contrast_ig
) {
    count_counters<n_decision_classes, n_dimensions, true>(data, contrast_data, decision, n_objects, n_classes, subtuple, contrast_idx, counters, n_cubes, p, d);

    // H(Y|{X_i}) conditional entropy of decision given all tuple vars
    float H_Y_given_all = conditional_entropy<n_decision_classes>(n_cubes, counters);

    std::memset(counters_reduced, 0, sizeof(float) * n_cubes_reduced * n_decision_classes);
    reduce_counters(n_classes, n_cubes, counters, counters_reduced, n_cubes_reduced);
    reduce_counters(n_classes, n_cubes, counters + n_cubes, counters_reduced + n_cubes_reduced, n_cubes_reduced);
    // H(Y|{X_i!=X_k}) conditional entropy of decision given all tuple vars except the current one (X_k)
    float H_Y_given_all_except_contrast = conditional_entropy<n_decision_classes>(n_cubes_reduced, counters_reduced);
    // I(Y;X_k | {X_i!=X_k}) mutual information of decision and the current var given all the other tuple vars
    *contrast_ig = H_Y_given_all_except_contrast - H_Y_given_all;
}

#endif
