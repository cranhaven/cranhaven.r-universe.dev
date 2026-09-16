#ifndef DATASET_H
#define DATASET_H

#include <cstddef>
#include <cstdint>

class RawDataInfo {
public:
    RawDataInfo(size_t object_count, size_t variable_count)
        : object_count(object_count), variable_count(variable_count) {}

    size_t object_count;
    size_t variable_count;
};

class RawData {
public:
    RawData(RawDataInfo data_file_info, const void* data, const int* decision)
        : info(data_file_info), data(data), decision(decision) {}

    RawDataInfo info;
    const void* data; // either double or int
    const int* decision;

    // returns pointer to array (info.object_count length) with requested variable
    inline const double* getVariable(size_t var_index) const {
        return (const double*) this->data + var_index * this->info.object_count;
    }
    inline const int* getVariableI(size_t var_index) const {
        return (const int*) this->data + var_index * this->info.object_count;
    }
};

class DiscretizationInfo {
public:
    DiscretizationInfo(uint32_t seed, size_t disc, size_t div, double range)
        : seed(seed), discretizations(disc), divisions(div), range(range) {}

    uint32_t seed;
    size_t discretizations;
    size_t divisions;
    double range;
};

#endif
