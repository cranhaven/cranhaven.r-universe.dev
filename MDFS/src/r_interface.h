// to make CRAN happy as it will be the default in R 4.5.0
#ifndef R_NO_REMAP
# define R_NO_REMAP
#endif
#include <Rinternals.h>

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
	SEXP Rin_use_cuda
);

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
	SEXP Rin_use_cuda
);

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
	SEXP Rin_average
);

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
	SEXP Rin_stat_mode
);

extern "C"
SEXP r_discretize(
	SEXP Rin_variable,
	SEXP Rin_variable_nr,
	SEXP Rin_divisions,
	SEXP Rin_discretization_nr,
	SEXP Rin_seed,
	SEXP Rin_range
);

extern "C"
SEXP r_omp_set_num_threads(
	SEXP Rin_num_threads
);
