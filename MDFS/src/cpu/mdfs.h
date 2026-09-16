#ifndef MDFS
#define MDFS

#include "mdfs_cpu_kernel.h"

#include "common.h"
#include "dataset.h"
#include "discretize.h"

#include <algorithm>
#include <limits>
#include <memory>

#ifdef _OPENMP
#include <omp.h>
#endif


template <uint8_t n_decision_classes, uint8_t n_dimensions, StatMode stat_mode>
void scalarMDFS(
    const MDFSInfo& mdfs_info,
    RawData* raw_data,
    RawData* contrast_raw_data,
    std::unique_ptr<const DiscretizationInfo> dfi,
    MDFSOutput& out
) {
    size_t c[n_decision_classes];
    for (uint8_t i = 0; i < n_decision_classes; i++) {
        c[i] = 0;
    }
    uint8_t* decision = nullptr;
    if (n_decision_classes > 1) {
        decision = new uint8_t[raw_data->info.object_count];
        for (size_t i = 0; i < raw_data->info.object_count; i++) {
            decision[i] = raw_data->decision[i];
            c[decision[i]]++;
        }
    } else {
        c[0] = raw_data->info.object_count;
    }
    const float cmin = *std::min_element(c, c+n_decision_classes);

    // this is to treat ig_thr=0 and below as unset (ignored) and allow for the
    // numeric errors to pass through the relevance filter (IGs end up being
    // negative in some rare cases due to logarithm rounding)
    const float ig_thr = mdfs_info.ig_thr > 0.0f ? mdfs_info.ig_thr : -std::numeric_limits<float>::infinity();

    float p[n_decision_classes];
    for (uint8_t i = 0; i < n_decision_classes; i++) {
        p[i] = c[i] / cmin * mdfs_info.pseudo;
    }

    const size_t n_classes = mdfs_info.divisions + 1;
    const size_t num_of_cubes = std::pow(n_classes, n_dimensions);
    const size_t num_of_cubes_reduced = std::pow(n_classes, n_dimensions - 1);

    const auto d2 = n_classes*n_classes;
    const auto d3 = d2*n_classes;
    const auto d4 = d3*n_classes;
    const size_t d[3] = {d2, d3, d4};

    float H_Y_counters[n_decision_classes];
    for (uint8_t i = 0; i < n_decision_classes; i++) {
        H_Y_counters[i] = c[i] + p[i] * num_of_cubes;
    }
    // H(Y) (plain) entropy of decision (computed simply as conditional given an empty set of vars)
    const float H_Y = conditional_entropy<n_decision_classes>(1, H_Y_counters);

    // for the optimised 2D version
    float* H = nullptr;
    if (n_dimensions == 2) {
        H = new float[raw_data->info.variable_count];
        if (mdfs_info.I_lower != nullptr) {
            for (size_t i = 0; i < raw_data->info.variable_count; i++) {
                if (n_decision_classes == 1) {
                    H[i] = mdfs_info.I_lower[i];
                } else {
                    H[i] = H_Y - mdfs_info.I_lower[i];
                }
            }
        }
    }

    const size_t n_vars_to_discretize = mdfs_info.interesting_vars_count && mdfs_info.require_all_vars ?
                                        mdfs_info.interesting_vars_count :
                                        raw_data->info.variable_count;

    // total of all counters; used only in no decision mode
    const float total_counters = raw_data->info.object_count + p[0] * num_of_cubes;

    uint8_t* data = new uint8_t[raw_data->info.object_count * raw_data->info.variable_count];
    uint8_t* contrast_data = nullptr;
    if (contrast_raw_data != nullptr) {
        contrast_data = new uint8_t[contrast_raw_data->info.object_count * contrast_raw_data->info.variable_count];
    }

    #ifdef _OPENMP
    #pragma omp parallel
    #endif
    {
        #ifdef _OPENMP
        const int omp_numthr = omp_get_num_threads();
        const int omp_tidx = omp_get_thread_num();
        #else
        constexpr int omp_numthr = 1;
        constexpr int omp_tidx = 0;
        #endif

        size_t tuple[n_dimensions];
        size_t subtuple[n_dimensions]; // only n_dimensions-1 are used, not using -1 in here to avoid 0-size array
        float igs[n_dimensions];
        float* counters = new float[n_decision_classes * num_of_cubes];
        float* reduced = new float[n_decision_classes * num_of_cubes_reduced];

        TupleGenerator<n_dimensions> generator(
                mdfs_info.interesting_vars_count && mdfs_info.require_all_vars ?
                mdfs_info.interesting_vars_count : raw_data->info.variable_count);
        TupleGenerator<n_dimensions-1> subgenerator(  // against contrast vars
                mdfs_info.interesting_vars_count && mdfs_info.require_all_vars ?
                mdfs_info.interesting_vars_count : raw_data->info.variable_count);
        if (mdfs_info.interesting_vars_count && mdfs_info.require_all_vars) {
            generator.set_interesting_vars(std::vector<size_t>(mdfs_info.interesting_vars, mdfs_info.interesting_vars + mdfs_info.interesting_vars_count));
            subgenerator.set_interesting_vars(std::vector<size_t>(mdfs_info.interesting_vars, mdfs_info.interesting_vars + mdfs_info.interesting_vars_count));
        }

        #ifdef _OPENMP
        MDFSOutput* thread_out = nullptr;
        if (out.type == MDFSOutputType::MaxIGs) {
            if (contrast_raw_data != nullptr) {
                thread_out = new MDFSOutput(out.type, n_dimensions, raw_data->info.variable_count, contrast_raw_data->info.variable_count);
            } else {
                thread_out = new MDFSOutput(out.type, n_dimensions, raw_data->info.variable_count, 0);
            }
            if (out.max_igs_tuples != nullptr) {
                thread_out->setMaxIGsTuples(new int[n_dimensions*raw_data->info.variable_count], new int[raw_data->info.variable_count]);
            }
        }
        #endif

        for (size_t discretization_id = 0; discretization_id < mdfs_info.discretizations; discretization_id++) {
            #ifdef _OPENMP
            #pragma omp barrier
            #endif

            if (dfi) {
                for (size_t i = omp_tidx; i < n_vars_to_discretize; i += omp_numthr) {
                    const size_t v = mdfs_info.interesting_vars_count && mdfs_info.require_all_vars ?
                                     mdfs_info.interesting_vars[i] :
                                     i;
                    const double* in_data = raw_data->getVariable(v);

                    std::vector<double> sorted_in_data(in_data, in_data + raw_data->info.object_count);
                    std::sort(sorted_in_data.begin(), sorted_in_data.end());

                    discretize(
                        dfi->seed,
                        discretization_id,
                        v,
                        dfi->divisions,
                        raw_data->info.object_count,
                        in_data,
                        sorted_in_data,
                        data + v * raw_data->info.object_count,
                        dfi->range
                    );
                }
                if (contrast_raw_data != nullptr) {
                    for (size_t i = omp_tidx; i < contrast_raw_data->info.variable_count; i += omp_numthr) {
                        const size_t v = i;
                        const double* in_data = contrast_raw_data->getVariable(v);

                        std::vector<double> sorted_in_data(in_data, in_data + contrast_raw_data->info.object_count);
                        std::sort(sorted_in_data.begin(), sorted_in_data.end());

                        discretize(
                            dfi->seed,
                            discretization_id,
                            raw_data->info.variable_count + v, // offset to be backwards-compatible
                            dfi->divisions,
                            contrast_raw_data->info.object_count,
                            in_data,
                            sorted_in_data,
                            contrast_data + v * contrast_raw_data->info.object_count,
                            dfi->range
                        );
                    }
                }
            } else {
                // rewrite int to uint8_t
                for (size_t i = omp_tidx; i < n_vars_to_discretize; i += omp_numthr) {
                    const size_t v = mdfs_info.interesting_vars_count && mdfs_info.require_all_vars ?
                                     mdfs_info.interesting_vars[i] :
                                     i;
                    const int* in_data = raw_data->getVariableI(v);
                    uint8_t* data_current_var = data + v * raw_data->info.object_count;

                    for (size_t i = 0; i < raw_data->info.object_count; i++) {
                        data_current_var[i] = in_data[i];
                    }
                }
                if (contrast_raw_data != nullptr) {
                    for (size_t i = omp_tidx; i < contrast_raw_data->info.variable_count; i += omp_numthr) {
                        const size_t v = i;
                        const int* in_data = contrast_raw_data->getVariableI(v);
                        uint8_t* data_current_var = contrast_data + v * contrast_raw_data->info.object_count;

                        for (size_t i = 0; i < contrast_raw_data->info.object_count; i++) {
                            data_current_var[i] = in_data[i];
                        }
                    }
                }
            }

            #ifdef _OPENMP
            #pragma omp barrier
            #endif

            // optimised 2D version
            if (n_dimensions == 2 && mdfs_info.I_lower == nullptr) {
                float* mini_counters = new float[n_decision_classes * n_classes];
                // to match counting in higher dimensions
                float mini_p[n_decision_classes];
                for (uint8_t i = 0; i < n_decision_classes; i++) {
                    mini_p[i] = p[i] * num_of_cubes_reduced;
                }
                for (size_t i = omp_tidx; i < raw_data->info.variable_count; i += omp_numthr) {
                    count_counters<n_decision_classes, 1, false>(data, nullptr, decision, raw_data->info.object_count, 0, &i, 0, mini_counters, n_classes, mini_p, nullptr);
                    if (n_decision_classes == 1) {
                        // H(X_i) (plain) entropy of the current var
                        H[i] = entropy(total_counters, n_classes, mini_counters);
                    } else {
                        // H(Y|X_i) conditional entropy of decision given the current var
                        H[i] = conditional_entropy<n_decision_classes>(n_classes, mini_counters);
                    }
                }
                delete[] mini_counters;

                #ifdef _OPENMP
                #pragma omp barrier
                #endif
            }

            generator.reset();
            subgenerator.reset();

            do {
                #ifdef _OPENMP
                int consumed_i = 0;
                for (; consumed_i < omp_numthr; consumed_i++) {
                    if (!generator.hasNext()) {
                        break;
                    }
                    if (consumed_i == omp_tidx) {
                        generator.next(tuple);
                    } else {
                        generator.skip();
                    }
                }
                if (consumed_i <= omp_tidx) { // no tuple is available anymore
                    break;
                }
                #else
                if (!generator.hasNext()) {
                    break;
                }
                generator.next(tuple);
                #endif

                if (mdfs_info.interesting_vars_count && !mdfs_info.require_all_vars) {
                    std::list<int> current_interesting_vars;
                    std::set_intersection(
                        tuple, tuple+n_dimensions,
                        mdfs_info.interesting_vars, mdfs_info.interesting_vars + mdfs_info.interesting_vars_count,
                        std::back_inserter(current_interesting_vars));

                    if (current_interesting_vars.empty()) {
                        continue;
                    }
                }

                process_tuple<n_decision_classes, n_dimensions, stat_mode>(
                    data,
                    decision,
                    raw_data->info.object_count,
                    n_classes,
                    tuple,
                    counters, reduced,
                    num_of_cubes, num_of_cubes_reduced,
                    p,
                    total_counters,
                    d,
                    H_Y,
                    H,
                    igs);

                switch (out.type) {
                    case MDFSOutputType::MaxIGs:
                        #ifdef _OPENMP
                        thread_out->updateMaxIG(tuple, igs, discretization_id);
                        #else
                        out.updateMaxIG(tuple, igs, discretization_id);
                        #endif
                        break;

                    case MDFSOutputType::MatchingTuples:
                        #ifdef _OPENMP
                        #pragma omp critical (AddMatchingTuples)
                        #endif
                        for (size_t v = 0; v < n_dimensions; ++v) {
                            if (igs[v] > ig_thr) {
                                out.addTuple(tuple[v], igs[v], discretization_id, tuple);
                            }
                        }
                        break;

                    case MDFSOutputType::AllTuples:
                        if (mdfs_info.average) {
                            out.addAllTuplesIG(tuple, igs, discretization_id);
                        } else {
                            out.updateAllTuplesIG(tuple, igs, discretization_id);
                        }
                        break;
                }
            } while (true);

            if (contrast_raw_data != nullptr) {
                do {
                    if (!subgenerator.hasNext()) {
                        break;
                    }
                    subgenerator.next(subtuple);

                    if (mdfs_info.interesting_vars_count && !mdfs_info.require_all_vars) {
                        std::list<int> current_interesting_vars;
                        std::set_intersection(
                            subtuple, subtuple+(n_dimensions-1),
                            mdfs_info.interesting_vars, mdfs_info.interesting_vars + mdfs_info.interesting_vars_count,
                            std::back_inserter(current_interesting_vars));

                        if (current_interesting_vars.empty()) {
                            continue;
                        }
                    }

                    float contrast_ig;

                    for (size_t contrast_idx = omp_tidx; contrast_idx < contrast_raw_data->info.variable_count; contrast_idx += omp_numthr) {
                        // n_decision_classes == 2
                        // n_dimensions >= 2 && I_lower == nullptr
                        process_subtuple<n_decision_classes, n_dimensions-1>(
                            data,
                            contrast_data,
                            decision,
                            raw_data->info.object_count,
                            n_classes,
                            subtuple,
                            contrast_idx,
                            counters, reduced,
                            num_of_cubes, num_of_cubes_reduced,
                            p,
                            d,
                            &contrast_ig);

                        // out.type == MDFSOutputType::MaxIGs
                        #ifdef _OPENMP
                        thread_out->updateContrastMaxIG(contrast_idx, contrast_ig, discretization_id);
                        #else
                        out.updateContrastMaxIG(contrast_idx, contrast_ig, discretization_id);
                        #endif
                    }
                } while (true);
            }
        }

        #ifdef _OPENMP
        if (out.type == MDFSOutputType::MaxIGs) {
            if (out.max_igs_tuples != nullptr) {
                #pragma omp critical (SetOutput)
                for (size_t i = 0; i < raw_data->info.variable_count; i++) {
                    if ((*thread_out->max_igs)[i] > (*out.max_igs)[i]) {
                        (*out.max_igs)[i] = (*thread_out->max_igs)[i];
                        std::copy(thread_out->max_igs_tuples + n_dimensions * i,
                                  thread_out->max_igs_tuples + n_dimensions * (i+1),
                                  out.max_igs_tuples + n_dimensions * i);
                        out.dids[i] = thread_out->dids[i];
                    }
                }
                delete[] thread_out->max_igs_tuples;
                delete[] thread_out->dids;
            } else {
                #pragma omp critical (SetOutput)
                for (size_t i = 0; i < raw_data->info.variable_count; i++) {
                    if ((*thread_out->max_igs)[i] > (*out.max_igs)[i]) {
                        (*out.max_igs)[i] = (*thread_out->max_igs)[i];
                    }
                }
            }
            if (contrast_raw_data != nullptr) {
                #pragma omp critical (SetContrastOutput)
                for (size_t i = 0; i < contrast_raw_data->info.variable_count; i += 1) {
                    if ((*thread_out->contrast_max_igs)[i] > (*out.contrast_max_igs)[i]) {
                        (*out.contrast_max_igs)[i] = (*thread_out->contrast_max_igs)[i];
                    }
                }
            }
            delete thread_out;
        }
        #endif

        delete[] reduced;
        delete[] counters;
    }

    if (contrast_raw_data != nullptr) {
        delete[] contrast_data;
    }
    delete[] data;
    if (n_dimensions == 2) {
        delete[] H;
    }
    delete[] decision;

    // only 2D supported
    if (out.type == MDFSOutputType::AllTuples && mdfs_info.average) {
        for (size_t i = 0; i < raw_data->info.variable_count * raw_data->info.variable_count; i++) {
            (*out.all_tuples)[i] /= mdfs_info.discretizations;
        }
    }
}

typedef void (*MdfsImpl) (
    const MDFSInfo& mdfs_info,
    RawData* raw_data,
    RawData* contrast_raw_data,
    std::unique_ptr<const DiscretizationInfo> dfi,
    MDFSOutput& out
);

// mutual information with decision (conditional except 1D)
const MdfsImpl mdfs[5] = {
    scalarMDFS<2, 1, StatMode::MutualInformation>,
    scalarMDFS<2, 2, StatMode::MutualInformation>,
    scalarMDFS<2, 3, StatMode::MutualInformation>,
    scalarMDFS<2, 4, StatMode::MutualInformation>,
    scalarMDFS<2, 5, StatMode::MutualInformation>,
};

// entropy of decision (conditional on variables)
const MdfsImpl mdfsDecisionConditionalEntropy[5] = {
    scalarMDFS<2, 1, StatMode::Entropy>,
    scalarMDFS<2, 2, StatMode::Entropy>,
    scalarMDFS<2, 3, StatMode::Entropy>,
    scalarMDFS<2, 4, StatMode::Entropy>,
    scalarMDFS<2, 5, StatMode::Entropy>,
};

// variation of information in decision (conditional on variables)
const MdfsImpl mdfsDecisionConditionalVariationOfInformation[5] = {
    nullptr,
    scalarMDFS<2, 2, StatMode::VariationOfInformation>,
    nullptr,
    nullptr,
    nullptr,
};

const MdfsImpl mdfsMutualInformation[5] = {
    nullptr,
    scalarMDFS<1, 2, StatMode::MutualInformation>,
    nullptr,
    nullptr,
    nullptr,
};

const MdfsImpl mdfsEntropy[5] = {
    scalarMDFS<1, 1, StatMode::Entropy>,
    scalarMDFS<1, 2, StatMode::Entropy>,
    scalarMDFS<1, 3, StatMode::Entropy>,
    scalarMDFS<1, 4, StatMode::Entropy>,
    scalarMDFS<1, 5, StatMode::Entropy>,
};

const MdfsImpl mdfsVariationOfInformation[5] = {
    nullptr,
    scalarMDFS<1, 2, StatMode::VariationOfInformation>,
    nullptr,
    nullptr,
    nullptr,
};

#endif
