#ifndef COMMON_H
#define COMMON_H

#include <cstddef>
#include <cstdint>
#include <list>
#include <vector>
#include <map>
#include <tuple>


class MDFSInfo {
public:
    size_t dimensions;
    size_t divisions;
    size_t discretizations;
    float pseudo;
    float ig_thr;
    int* interesting_vars;  // has to be sorted
    size_t interesting_vars_count;
    bool require_all_vars;
    const double* I_lower;
    bool average;

    MDFSInfo(
        size_t dimensions,
        size_t divisions,
        size_t discretizations,
        float pseudo,
        float ig_thr,
        int* interesting_vars,
        size_t interesting_vars_count,
        bool require_all_vars,
        const double* I_lower,
        bool average
    );
};


template <uint8_t n_dimensions>
class TupleGenerator {
    size_t nextTuple[n_dimensions+1];
    const size_t n_variables;
    std::vector<size_t> interesting_vars;

public:
    TupleGenerator(size_t n_variables);

    void set_interesting_vars(const std::vector<size_t>& interesting_vars);
    void reset();
    bool hasNext() const;
    void next(size_t* out);
    void skip();
};


template<uint8_t n_dimensions> TupleGenerator<n_dimensions>::TupleGenerator(size_t n_variables)
        : n_variables(n_variables) {}

template<uint8_t n_dimensions> void TupleGenerator<n_dimensions>::set_interesting_vars(const std::vector<size_t>& interesting_vars) {
    this->interesting_vars = interesting_vars;
}

template<uint8_t n_dimensions> void TupleGenerator<n_dimensions>::reset() {
    this->nextTuple[0] = 0;  // the sentinel

    for (size_t d = 1; d <= n_dimensions; ++d) {
        this->nextTuple[d] = d - 1;
    }
}

template<uint8_t n_dimensions> bool TupleGenerator<n_dimensions>::hasNext() const {
    return this->nextTuple[0] == 0;
}

template<uint8_t n_dimensions> void TupleGenerator<n_dimensions>::next(size_t* out) {
    for (size_t i = 1; i <= n_dimensions; i++) {
        if (interesting_vars.empty()) {
            out[i-1] = this->nextTuple[i];
        } else {
            out[i-1] = interesting_vars[this->nextTuple[i]];
        }
    }

    this->skip();
}

template<uint8_t n_dimensions> void TupleGenerator<n_dimensions>::skip() {
    size_t d = n_dimensions;

    do {
        ++(this->nextTuple[d]);
        if (this->nextTuple[d] < n_variables - (n_dimensions - d) || d == 0) {
            break;
        }
        d--;
    } while (1);

    for (++d; d <= n_dimensions; ++d) {
        this->nextTuple[d] = this->nextTuple[d-1]+1;
    }
}


enum class MDFSOutputType { MaxIGs, MatchingTuples, AllTuples };

class MDFSOutput {
public:
    int *max_igs_tuples;
    int *dids;
    union {
        std::vector<float> *max_igs;
        std::map<std::tuple<std::vector<size_t>, size_t>, std::tuple<float, size_t>> *tuples;
        std::vector<float> *all_tuples;
    };
    std::vector<float> *contrast_max_igs;

    MDFSOutput(MDFSOutputType type, size_t n_dimensions, size_t variable_count, size_t n_contrast_variables);
    ~MDFSOutput();

    const MDFSOutputType type;
    const size_t n_dimensions;
    const size_t n_variables;
    const size_t n_contrast_variables;

    void setMaxIGsTuples(int *tuples, int *dids);
    void updateMaxIG(const size_t* tuple, float *igs, size_t discretization_id);
    void updateContrastMaxIG(const size_t contrast_idx, float contrast_ig, size_t discretization_id);
    void copyMaxIGsAsDouble(double *copy) const;
    void copyContrastMaxIGsAsDouble(double *copy) const;
    void addTuple(size_t i, float ig, size_t discretization_id, const size_t* vt);
    void updateAllTuplesIG(const size_t* tuple, float *igs, size_t discretization_id);
    void addAllTuplesIG(const size_t* tuple, float *igs, size_t discretization_id);
    size_t getMatchingTuplesCount() const;
    void copyMatchingTuples(int* matching_tuples_vars, double* IGs, int* matching_tuples) const;
    void copyAllTuples(int* matching_tuples_vars, double* IGs, int* matching_tuples) const;
    void copyAllTuplesMatrix(double* out_matrix) const;
};

#endif
