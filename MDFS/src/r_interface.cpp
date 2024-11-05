// needs to go first to avoid omp.h conflicts with Rinternals.h macros
#include "cpu/mdfs.h"

#include "r_interface.h"

#ifdef WITH_CUDA
// <R.h> required to report errors - currently only CUDA reports any errors
#include <R.h>
#include "gpu/cucubes.h"
#endif

extern "C"
SEXP r_compute_max_ig(
        SEXP Rin_data,
        SEXP Rin_contrast_data,
        SEXP Rin_decision,
        SEXP Rin_dimensions,
        SEXP Rin_divisions,
        SEXP Rin_discretizations,
        SEXP Rin_seed,
        SEXP Rin_range,
        SEXP Rin_pseudocount,
        SEXP Rin_interesting_vars,
        SEXP Rin_require_all_vars,
        SEXP Rin_return_tuples,
        SEXP Rin_use_cuda)
{
    #ifndef WITH_CUDA
    if (asLogical(Rin_use_cuda)) {
        error("CUDA acceleration not compiled");
    }
    #endif

    const int* dataDims = INTEGER(getAttrib(Rin_data, R_DimSymbol));
    int* contrastDataDims = nullptr;
    if (!isNull(Rin_contrast_data)) {
        contrastDataDims = INTEGER(getAttrib(Rin_contrast_data, R_DimSymbol));
    }

    const int obj_count = dataDims[0];
    const int variable_count = dataDims[1];
    int contrast_variable_count = 0;
    if (!isNull(Rin_contrast_data)) {
        contrast_variable_count = contrastDataDims[1];
    }

    #ifdef WITH_CUDA
    if (asLogical(Rin_use_cuda)) {
        SEXP Rout_max_igs = PROTECT(allocVector(REALSXP, variable_count));

        try {
            run_cucubes(
                    obj_count,
                    variable_count,
                    asInteger(Rin_dimensions),
                    asInteger(Rin_divisions),
                    asInteger(Rin_discretizations),
                    asInteger(Rin_seed),
                    asReal(Rin_range),
                    asReal(Rin_pseudocount),
                    REAL(Rin_data),
                    INTEGER(Rin_decision),
                    REAL(Rout_max_igs));
        } catch (const cudaException& e) {
            // TODO: ensure cleanup inside library
            error("CUDA exception: %s (in %s:%d)", cudaGetErrorString(e.code), e.file, e.line);
        } catch (const NotImplementedException& e) {
            // TODO: is it possible to get this?
            error("Not-implemented exception: %s", e.msg.c_str());
        }

        const int result_members_count = 1;

        SEXP Rout_result = PROTECT(allocVector(VECSXP, result_members_count));
        SET_VECTOR_ELT(Rout_result, 0, Rout_max_igs);

        UNPROTECT(1 + result_members_count);

        return Rout_result;
    }
    #endif

    const int discretizations = asInteger(Rin_discretizations);
    const int divisions = asInteger(Rin_divisions);

    const int* decision = INTEGER(Rin_decision);

    RawData rawdata(RawDataInfo(obj_count, variable_count), REAL(Rin_data), decision);
    RawData* contrast_rawdata = nullptr;
    if (!isNull(Rin_contrast_data)) {
        contrast_rawdata = new RawData(RawDataInfo(obj_count, contrast_variable_count), REAL(Rin_contrast_data), nullptr);
    }

    std::unique_ptr<const DiscretizationInfo> dfi(new DiscretizationInfo(
        asInteger(Rin_seed),
        discretizations,
        divisions,
        asReal(Rin_range)
    ));

    MDFSInfo mdfs_info(
        asInteger(Rin_dimensions),
        divisions,
        discretizations,
        asReal(Rin_pseudocount),
        0.0f,
        INTEGER(Rin_interesting_vars),
        length(Rin_interesting_vars),
        asLogical(Rin_require_all_vars),
        nullptr,
        false
    );

    SEXP Rout_max_igs = PROTECT(allocVector(REALSXP, variable_count));
    SEXP Rout_contrast_max_igs = nullptr;
    SEXP Rout_tuples = nullptr;
    SEXP Rout_dids = nullptr;

    if (!isNull(Rin_contrast_data)) {
        Rout_contrast_max_igs = PROTECT(allocVector(REALSXP, contrast_variable_count));
    }

    const bool return_tuples = asLogical(Rin_return_tuples);
    MDFSOutput mdfs_output(MDFSOutputType::MaxIGs, mdfs_info.dimensions, variable_count, contrast_variable_count);
    if (return_tuples) {
        Rout_tuples = PROTECT(allocMatrix(INTSXP, mdfs_info.dimensions, variable_count));
        Rout_dids = PROTECT(allocVector(INTSXP, variable_count));
        mdfs_output.setMaxIGsTuples(INTEGER(Rout_tuples), INTEGER(Rout_dids)); // tuples are set row-first during computation, we transpose the result in R to speed up C code
    }

    mdfs[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, contrast_rawdata, std::move(dfi), mdfs_output);

    mdfs_output.copyMaxIGsAsDouble(REAL(Rout_max_igs));

    int result_members_count = 1;

    if (return_tuples) {
        result_members_count += 1; // for tuples
        result_members_count += 1; // for disc nr
    }

    if (!isNull(Rin_contrast_data)) {
        mdfs_output.copyContrastMaxIGsAsDouble(REAL(Rout_contrast_max_igs));
        result_members_count += 1;
    }

    SEXP Rout_result = PROTECT(allocVector(VECSXP, result_members_count));
    SET_VECTOR_ELT(Rout_result, 0, Rout_max_igs);

    if (return_tuples) {
        SET_VECTOR_ELT(Rout_result, 1, Rout_tuples);
        SET_VECTOR_ELT(Rout_result, 2, Rout_dids);
    }

    if (!isNull(Rin_contrast_data)) {
        if (return_tuples) {
            SET_VECTOR_ELT(Rout_result, 3, Rout_contrast_max_igs);
        } else {
            SET_VECTOR_ELT(Rout_result, 1, Rout_contrast_max_igs);
        }
    }

    UNPROTECT(1 + result_members_count);

    if (!isNull(Rin_contrast_data)) {
        delete contrast_rawdata;
    }

    return Rout_result;
}

extern "C"
SEXP r_compute_max_ig_discrete(
        SEXP Rin_data,
        SEXP Rin_contrast_data,
        SEXP Rin_decision,
        SEXP Rin_dimensions,
        SEXP Rin_divisions,
        SEXP Rin_pseudocount,
        SEXP Rin_interesting_vars,
        SEXP Rin_require_all_vars,
        SEXP Rin_return_tuples,
        SEXP Rin_use_cuda)
{
    #ifndef WITH_CUDA
    if (asLogical(Rin_use_cuda)) {
        error("CUDA acceleration not compiled");
    }
    #endif

    const int* dataDims = INTEGER(getAttrib(Rin_data, R_DimSymbol));
    int* contrastDataDims = nullptr;
    if (!isNull(Rin_contrast_data)) {
        contrastDataDims = INTEGER(getAttrib(Rin_contrast_data, R_DimSymbol));
    }

    const int obj_count = dataDims[0];
    const int variable_count = dataDims[1];
    int contrast_variable_count = 0;
    if (!isNull(Rin_contrast_data)) {
        contrast_variable_count = contrastDataDims[1];
    }

    #ifdef WITH_CUDA
    if (asLogical(Rin_use_cuda)) {
        error("CUDA not supported yet for the discrete variant");
    }
    #endif

    const int divisions = asInteger(Rin_divisions);

    const int* decision = INTEGER(Rin_decision);

    RawData rawdata(RawDataInfo(obj_count, variable_count), INTEGER(Rin_data), decision);
    RawData* contrast_rawdata = nullptr;
    if (!isNull(Rin_contrast_data)) {
        contrast_rawdata = new RawData(RawDataInfo(obj_count, contrast_variable_count), INTEGER(Rin_contrast_data), nullptr);
    }

    MDFSInfo mdfs_info(
        asInteger(Rin_dimensions),
        divisions,
        1, // only one discretization
        asReal(Rin_pseudocount),
        0.0f,
        INTEGER(Rin_interesting_vars),
        length(Rin_interesting_vars),
        asLogical(Rin_require_all_vars),
        nullptr,
        false
    );

    SEXP Rout_max_igs = PROTECT(allocVector(REALSXP, variable_count));
    SEXP Rout_contrast_max_igs = nullptr;
    SEXP Rout_tuples = nullptr;
    SEXP Rout_dids = nullptr;

    if (!isNull(Rin_contrast_data)) {
        Rout_contrast_max_igs = PROTECT(allocVector(REALSXP, contrast_variable_count));
    }

    const bool return_tuples = asLogical(Rin_return_tuples);
    MDFSOutput mdfs_output(MDFSOutputType::MaxIGs, mdfs_info.dimensions, variable_count, contrast_variable_count);
    if (return_tuples) {
        Rout_tuples = PROTECT(allocMatrix(INTSXP, mdfs_info.dimensions, variable_count));
        Rout_dids = PROTECT(allocVector(INTSXP, variable_count));
        mdfs_output.setMaxIGsTuples(INTEGER(Rout_tuples), INTEGER(Rout_dids)); // tuples are set row-first during computation, we transpose the result in R to speed up C code
    }

    mdfs[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, contrast_rawdata, nullptr, mdfs_output);

    mdfs_output.copyMaxIGsAsDouble(REAL(Rout_max_igs));

    int result_members_count = 1;

    if (return_tuples) {
        result_members_count += 1; // for tuples
        result_members_count += 1; // for disc nr
    }

    if (!isNull(Rin_contrast_data)) {
        mdfs_output.copyContrastMaxIGsAsDouble(REAL(Rout_contrast_max_igs));
        result_members_count += 1;
    }

    SEXP Rout_result = PROTECT(allocVector(VECSXP, result_members_count));
    SET_VECTOR_ELT(Rout_result, 0, Rout_max_igs);

    if (return_tuples) {
        SET_VECTOR_ELT(Rout_result, 1, Rout_tuples);
        SET_VECTOR_ELT(Rout_result, 2, Rout_dids);
    }

    if (!isNull(Rin_contrast_data)) {
        if (return_tuples) {
            SET_VECTOR_ELT(Rout_result, 3, Rout_contrast_max_igs);
        } else {
            SET_VECTOR_ELT(Rout_result, 1, Rout_contrast_max_igs);
        }
    }

    UNPROTECT(1 + result_members_count);

    if (!isNull(Rin_contrast_data)) {
        delete contrast_rawdata;
    }

    return Rout_result;
}

extern "C"
SEXP r_compute_all_matching_tuples(
        SEXP Rin_data,
        SEXP Rin_decision,
        SEXP Rin_dimensions,
        SEXP Rin_divisions,
        SEXP Rin_discretizations,
        SEXP Rin_seed,
        SEXP Rin_range,
        SEXP Rin_pseudocount,
        SEXP Rin_interesting_vars,
        SEXP Rin_require_all_vars,
        SEXP Rin_ig_thr,
        SEXP Rin_I_lower,
        SEXP Rin_return_matrix,
        SEXP Rin_stat_mode,
        SEXP Rin_average)
{
    const int* dataDims = INTEGER(getAttrib(Rin_data, R_DimSymbol));

    const int obj_count = dataDims[0];
    const int variable_count = dataDims[1];

    const int discretizations = asInteger(Rin_discretizations);
    const int divisions = asInteger(Rin_divisions);

    const int* decision = nullptr;
    if (!isNull(Rin_decision)) {
        decision = INTEGER(Rin_decision);
    }

    RawData rawdata(RawDataInfo(obj_count, variable_count), REAL(Rin_data), decision);

    std::unique_ptr<const DiscretizationInfo> dfi(new DiscretizationInfo(
        asInteger(Rin_seed),
        discretizations,
        divisions,
        asReal(Rin_range)
    ));

    const double* I_lower = nullptr;
    if (!isNull(Rin_I_lower)) {
        I_lower = REAL(Rin_I_lower);
    }

    MDFSInfo mdfs_info(
        asInteger(Rin_dimensions),
        divisions,
        discretizations,
        asReal(Rin_pseudocount),
        asReal(Rin_ig_thr),
        INTEGER(Rin_interesting_vars),
        length(Rin_interesting_vars),
        asLogical(Rin_require_all_vars),
        I_lower,
        asLogical(Rin_average)
    );

    MDFSOutputType out_type = mdfs_info.dimensions == 2 && asReal(Rin_ig_thr) <= 0.0 && length(Rin_interesting_vars) == 0 ? MDFSOutputType::AllTuples : MDFSOutputType::MatchingTuples;
    MDFSOutput mdfs_output(out_type, mdfs_info.dimensions, variable_count, 0);

    if (isNull(Rin_decision)) {
        switch (asInteger(Rin_stat_mode)) {
            case 1: mdfsEntropy[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, std::move(dfi), mdfs_output);
            break;
            case 2: mdfsMutualInformation[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, std::move(dfi), mdfs_output);
            break;
            case 3: mdfsVariationOfInformation[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, std::move(dfi), mdfs_output);
            break;
            default: error("Unknown statistic");
        }
    } else {
        switch (asInteger(Rin_stat_mode)) {
            case 1: mdfsDecisionConditionalEntropy[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, std::move(dfi), mdfs_output);
            break;
            case 2: mdfs[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, std::move(dfi), mdfs_output);
            break;
            case 3: mdfsDecisionConditionalVariationOfInformation[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, std::move(dfi), mdfs_output);
            break;
            default: error("Unknown statistic");
        }
    }

    if (out_type == MDFSOutputType::AllTuples && asLogical(Rin_return_matrix)) {
        SEXP Rout_result = PROTECT(allocMatrix(REALSXP, variable_count, variable_count));

        // TODO: perhaps we could avoid copying here at all and fill in this matrix already from the mdfs?
        mdfs_output.copyAllTuplesMatrix(REAL(Rout_result));

        UNPROTECT(1);

        return Rout_result;
    } else {
        const int result_members_count = 3;
        // 2D only now
        const int tuples_count = out_type == MDFSOutputType::AllTuples ? variable_count * (variable_count - 1) : mdfs_output.getMatchingTuplesCount();

        SEXP Rout_igs = PROTECT(allocVector(REALSXP, tuples_count));
        SEXP Rout_tuples = PROTECT(allocMatrix(INTSXP, tuples_count, mdfs_info.dimensions));
        SEXP Rout_vars = PROTECT(allocVector(INTSXP, tuples_count));

        if (out_type == MDFSOutputType::AllTuples) {
            mdfs_output.copyAllTuples(INTEGER(Rout_vars), REAL(Rout_igs), INTEGER(Rout_tuples));
        } else {
            mdfs_output.copyMatchingTuples(INTEGER(Rout_vars), REAL(Rout_igs), INTEGER(Rout_tuples));
        }

        SEXP Rout_result = PROTECT(allocVector(VECSXP, result_members_count));
        SET_VECTOR_ELT(Rout_result, 0, Rout_vars);
        SET_VECTOR_ELT(Rout_result, 1, Rout_tuples);
        SET_VECTOR_ELT(Rout_result, 2, Rout_igs);

        UNPROTECT(1 + result_members_count);

        return Rout_result;
    }
}

extern "C"
SEXP r_compute_all_matching_tuples_discrete(
        SEXP Rin_data,
        SEXP Rin_decision,
        SEXP Rin_dimensions,
        SEXP Rin_divisions,
        SEXP Rin_pseudocount,
        SEXP Rin_interesting_vars,
        SEXP Rin_require_all_vars,
        SEXP Rin_ig_thr,
        SEXP Rin_I_lower,
        SEXP Rin_return_matrix,
        SEXP Rin_stat_mode)
{
    const int* dataDims = INTEGER(getAttrib(Rin_data, R_DimSymbol));

    const int obj_count = dataDims[0];
    const int variable_count = dataDims[1];

    const int divisions = asInteger(Rin_divisions);

    const int* decision = nullptr;
    if (!isNull(Rin_decision)) {
        decision = INTEGER(Rin_decision);
    }

    RawData rawdata(RawDataInfo(obj_count, variable_count), INTEGER(Rin_data), decision);

    const double* I_lower = nullptr;
    if (!isNull(Rin_I_lower)) {
        I_lower = REAL(Rin_I_lower);
    }

    MDFSInfo mdfs_info(
        asInteger(Rin_dimensions),
        divisions,
        1, // only one discretization
        asReal(Rin_pseudocount),
        asReal(Rin_ig_thr),
        INTEGER(Rin_interesting_vars),
        length(Rin_interesting_vars),
        asLogical(Rin_require_all_vars),
        I_lower,
        false
    );

    MDFSOutputType out_type = mdfs_info.dimensions == 2 && asReal(Rin_ig_thr) <= 0.0 && length(Rin_interesting_vars) == 0 ? MDFSOutputType::AllTuples : MDFSOutputType::MatchingTuples;
    MDFSOutput mdfs_output(out_type, mdfs_info.dimensions, variable_count, 0);

    if (isNull(Rin_decision)) {
        switch (asInteger(Rin_stat_mode)) {
            case 1: mdfsEntropy[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, nullptr, mdfs_output);
            break;
            case 2: mdfsMutualInformation[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, nullptr, mdfs_output);
            break;
            case 3: mdfsVariationOfInformation[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, nullptr, mdfs_output);
            break;
            default: error("Unknown statistic");
        }
    } else {
        switch (asInteger(Rin_stat_mode)) {
            case 1: mdfsDecisionConditionalEntropy[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, nullptr, mdfs_output);
            break;
            case 2: mdfs[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, nullptr, mdfs_output);
            break;
            case 3: mdfsDecisionConditionalVariationOfInformation[asInteger(Rin_dimensions)-1](mdfs_info, &rawdata, nullptr, nullptr, mdfs_output);
            break;
            default: error("Unknown statistic");
        }
    }

    if (out_type == MDFSOutputType::AllTuples && asLogical(Rin_return_matrix)) {
        SEXP Rout_result = PROTECT(allocMatrix(REALSXP, variable_count, variable_count));

        // TODO: perhaps we could avoid copying here at all and fill in this matrix already from the mdfs?
        mdfs_output.copyAllTuplesMatrix(REAL(Rout_result));

        UNPROTECT(1);

        return Rout_result;
    } else {
        const int result_members_count = 3;
        // 2D only now
        const int tuples_count = out_type == MDFSOutputType::AllTuples ? variable_count * (variable_count - 1) : mdfs_output.getMatchingTuplesCount();

        SEXP Rout_igs = PROTECT(allocVector(REALSXP, tuples_count));
        SEXP Rout_tuples = PROTECT(allocMatrix(INTSXP, tuples_count, mdfs_info.dimensions));
        SEXP Rout_vars = PROTECT(allocVector(INTSXP, tuples_count));

        if (out_type == MDFSOutputType::AllTuples) {
            mdfs_output.copyAllTuples(INTEGER(Rout_vars), REAL(Rout_igs), INTEGER(Rout_tuples));
        } else {
            mdfs_output.copyMatchingTuples(INTEGER(Rout_vars), REAL(Rout_igs), INTEGER(Rout_tuples));
        }

        SEXP Rout_result = PROTECT(allocVector(VECSXP, result_members_count));
        SET_VECTOR_ELT(Rout_result, 0, Rout_vars);
        SET_VECTOR_ELT(Rout_result, 1, Rout_tuples);
        SET_VECTOR_ELT(Rout_result, 2, Rout_igs);

        UNPROTECT(1 + result_members_count);

        return Rout_result;
    }
}

extern "C"
SEXP r_discretize(
        SEXP Rin_variable,
        SEXP Rin_variable_idx,
        SEXP Rin_divisions,
        SEXP Rin_discretization_nr,
        SEXP Rin_seed,
        SEXP Rin_range)
{
    const R_len_t obj_count = length(Rin_variable);
    const int discretization_nr = asInteger(Rin_discretization_nr);
    const int variable_idx = asInteger(Rin_variable_idx);
    const int divisions = asInteger(Rin_divisions);
    const int seed = asInteger(Rin_seed);
    double range = asReal(Rin_range);
    double* variable = REAL(Rin_variable);

    std::vector<double> sorted_variable(variable, variable + obj_count);
    std::sort(sorted_variable.begin(), sorted_variable.end());

    uint8_t* discretized_variable = new uint8_t[obj_count];
    discretize(seed, discretization_nr, variable_idx, divisions, obj_count, variable, sorted_variable, discretized_variable, range);
    SEXP Rout_result = PROTECT(allocVector(INTSXP, obj_count));
    std::copy(discretized_variable, discretized_variable + obj_count, INTEGER(Rout_result));
    delete[] discretized_variable;
    UNPROTECT(1);

    return Rout_result;
}

extern "C"
SEXP r_omp_set_num_threads(
        SEXP Rin_num_threads)
{
    #ifdef _OPENMP
    const int num_threads = asInteger(Rin_num_threads);
    omp_set_num_threads(num_threads);
    #endif
    return R_NilValue;
}
