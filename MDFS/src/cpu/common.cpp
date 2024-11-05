#include "common.h"

#include <limits>


/* MDFS Info */

MDFSInfo::MDFSInfo(
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
) : dimensions(dimensions), divisions(divisions), discretizations(discretizations),
    pseudo(pseudo), ig_thr(ig_thr), interesting_vars(interesting_vars),
    interesting_vars_count(interesting_vars_count), require_all_vars(require_all_vars),
    I_lower(I_lower), average(average) {}


/* MDFSOutput */

MDFSOutput::MDFSOutput(MDFSOutputType type, size_t n_dimensions, size_t variable_count, size_t n_contrast_variables)
    : max_igs_tuples(nullptr), type(type), n_dimensions(n_dimensions), n_variables(variable_count), n_contrast_variables(n_contrast_variables) {
    switch(type) {
        case MDFSOutputType::MaxIGs:
            // init to -Inf to ensure we save negative values as well (they happen due to numerical errors with log)
            this->max_igs = new std::vector<float>(variable_count, -std::numeric_limits<float>::infinity());
            if (n_contrast_variables > 0) {
                this->contrast_max_igs = new std::vector<float>(n_contrast_variables, -std::numeric_limits<float>::infinity());
            }
            break;

        case MDFSOutputType::MatchingTuples:
            this->tuples = new std::map<std::tuple<std::vector<size_t>, size_t>, std::tuple<float, size_t>>();
            break;

        case MDFSOutputType::AllTuples:
            // 2D only now
            this->all_tuples = new std::vector<float>(variable_count * variable_count, 0);
            break;
   }
}

MDFSOutput::~MDFSOutput() {
    switch(type) {
        case MDFSOutputType::MaxIGs:
            delete this->max_igs;
            if (this->n_contrast_variables > 0) {
                delete this->contrast_max_igs;
            }
            break;

        case MDFSOutputType::MatchingTuples:
            delete this->tuples;
            break;

        case MDFSOutputType::AllTuples:
            delete this->all_tuples;
            break;
   }
}

void MDFSOutput::setMaxIGsTuples(int *tuples, int *dids) {
    this->max_igs_tuples = tuples;
    this->dids = dids;
}

void MDFSOutput::updateMaxIG(const size_t* tuple, float *igs, size_t discretization_id) {
    if (this->max_igs_tuples == nullptr) {
        for (size_t i = 0; i < n_dimensions; ++i) {
            size_t v = tuple[i];

            if (igs[i] > (*(this->max_igs))[v]) {
                (*(this->max_igs))[v] = igs[i];
            }
        }
    } else {
        for (size_t i = 0; i < n_dimensions; ++i) {
            size_t v = tuple[i];

            if (igs[i] > (*(this->max_igs))[v]) {
                (*(this->max_igs))[v] = igs[i];
                // std::copy cannot be memcpy because of the type difference
                std::copy(tuple, tuple+n_dimensions, this->max_igs_tuples + n_dimensions * v);
                this->dids[v] = discretization_id;
            }
        }
    }
}

void MDFSOutput::updateContrastMaxIG(const size_t contrast_idx, float contrast_ig, size_t discretization_id) {
    if (contrast_ig > (*this->contrast_max_igs)[contrast_idx]) {
        (*this->contrast_max_igs)[contrast_idx] = contrast_ig;
    }
}

void MDFSOutput::copyMaxIGsAsDouble(double *copy) const {
    std::copy(this->max_igs->begin(), this->max_igs->end(), copy);
}

void MDFSOutput::copyContrastMaxIGsAsDouble(double *copy) const {
    std::copy(this->contrast_max_igs->begin(), this->contrast_max_igs->end(), copy);
}

void MDFSOutput::addTuple(size_t i, float ig, size_t discretization_id, const size_t* vt) {
    const auto index = std::make_tuple(std::vector<size_t>(vt, vt+n_dimensions), i);
    auto existing = this->tuples->find(index);
    if (existing == this->tuples->end()) {
        this->tuples->insert({index, std::make_tuple(ig, discretization_id)});
    } else if (ig > std::get<0>(existing->second)) {
        existing->second = std::make_tuple(ig, discretization_id);
    }
}

// 2D only now
void MDFSOutput::updateAllTuplesIG(const size_t* tuple, float *igs, size_t discretization_id) {
    size_t index_0 = tuple[0] * this->n_variables + tuple[1];
    size_t index_1 = tuple[1] * this->n_variables + tuple[0];

    if ((*this->all_tuples)[index_0] < igs[0]) {
        (*this->all_tuples)[index_0] = igs[0];
    }

    if ((*this->all_tuples)[index_1] < igs[1]) {
        (*this->all_tuples)[index_1] = igs[1];
    }
}

// 2D only now
void MDFSOutput::addAllTuplesIG(const size_t* tuple, float *igs, size_t discretization_id) {
    size_t index_0 = tuple[0] * this->n_variables + tuple[1];
    size_t index_1 = tuple[1] * this->n_variables + tuple[0];

    (*this->all_tuples)[index_0] += igs[0];
    (*this->all_tuples)[index_1] += igs[1];
}

size_t MDFSOutput::getMatchingTuplesCount() const {
    return this->tuples->size();
}

void MDFSOutput::copyMatchingTuples(int* matching_tuples_vars, double* IGs, int* matching_tuples) const {
    size_t tuples_count = this->getMatchingTuplesCount();
    size_t i = 0;

    for (auto v = this->tuples->begin(); i < tuples_count; ++v, ++i) {
        matching_tuples_vars[i] = std::get<1>(v->first);
        IGs[i] = std::get<0>(v->second);

        for (size_t j = 0; j < std::get<0>(v->first).size(); ++j) {
            matching_tuples[j * tuples_count + i] = std::get<0>(v->first)[j]; // column-first
        }
    }
}

// 2D only now
void MDFSOutput::copyAllTuples(int* matching_tuples_vars, double* IGs, int* matching_tuples) const {
    size_t k = 0;
    const size_t n_tuples = this->n_variables * (this->n_variables - 1);

    for (size_t i = 0; i < this->n_variables; i++) {
        for (size_t j = i + 1; j < this->n_variables; j++) {
            matching_tuples_vars[k] = i;
            IGs[k] = (*this->all_tuples)[i*this->n_variables + j];
            matching_tuples[k] = i;
            matching_tuples[n_tuples + k] = j;
            k++;
            matching_tuples_vars[k] = j;
            IGs[k] = (*this->all_tuples)[j*this->n_variables + i];
            matching_tuples[k] = i;
            matching_tuples[n_tuples + k] = j;
            k++;
        }
    }
}

// 2D only now
void MDFSOutput::copyAllTuplesMatrix(double* out_matrix) const {
    for (size_t i = 0; i < this->n_variables; i++) {
        for (size_t j = 0; j < this->n_variables; j++) {
            // row-first to column-first conversion
            out_matrix[j*this->n_variables + i] = (*this->all_tuples)[i*this->n_variables + j];
        }
    }
}
