#ifndef TIMESIFT_TS_PENALISED_H
#define TIMESIFT_TS_PENALISED_H

#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

#include "ts_core.h"

// The penalised fit, once, for both languages.
//
// An elastic net over a dense design, fitted by iteratively reweighted least squares with a
// cyclic coordinate descent inside it, along a path of penalties with a warm start at each step.
// The conventions are glmnet's, because the penalised arm is what the networks are measured
// against and a baseline that moves between the languages would make the tool the confound:
// weights normalised to sum to one, columns centred and scaled by their weighted mean and
// weighted standard deviation, the penalty path geometric from the smallest penalty that leaves
// every coefficient at zero, and the penalty chosen by cross-validated deviance.
//
// Everything here is plain arrays. The design is column-major, `x[i + j * n]`, which is what an
// R matrix already is and what a Fortran-ordered NumPy array already is.
namespace timesift {

enum class Family { gaussian, binomial };

Family family_from_name(const std::string& name);
const char* family_name(Family f);

struct PenaltySpec {
  double alpha = 1.0;                  // 1 lasso, 0 ridge
  int n_lambda = 100;                  // points of the derived path
  double lambda_min_ratio = 0.0;       // 0 -> 1e-4 where n > p, else 1e-2
  std::vector<double> lambda;          // a supplied path, descending; empty to derive one
  std::vector<double> penalty_factor;  // one per column, rescaled to sum to p; empty -> all one
  bool standardize = true;
  bool intercept = true;
  double thresh = 1e-8;                // coordinate descent stops below this. It is read on the
                                       // same scale glmnet reads its own, against the null
                                       // deviance, and one notch tighter than glmnet's default of
                                       // 1e-7: on the contract's twelve cases the fit then sits
                                       // between 1.8e-5 and 3.1e-4 above the optimum, closer than
                                       // glmnet's default leaves its own in every one of them
  int max_pass = 1000000;              // coordinate descent passes over the whole path
  int max_irls = 2000;                 // reweighted least squares steps at one penalty
  std::size_t max_active = 0;          // 0 -> every column may enter
  double fdev = 1e-5;                  // a path step explaining less than this of the deviance
                                       // ends the path; 0 to run every point
  double dev_max = 0.999;              // a fit explaining more than this ends the path
  int min_lambda = 5;                  // points fitted before either rule is read
  int threads = 1;                     // fits of a cross-validation run at once; 1 is serial
};

// The path, with the coefficients on the scale the columns were handed over in.
struct PenaltyPath {
  std::size_t n_column = 0;
  std::vector<double> lambda;          // descending, one per point fitted
  std::vector<double> a0;              // one per point
  std::vector<double> beta;            // [column, point], column fastest
  std::vector<std::int32_t> df;        // non-zero coefficients, one per point
  std::vector<double> dev_ratio;       // deviance explained, one per point
  double null_deviance = 0.0;
  std::int32_t passes = 0;            // coordinate descent passes the whole path took
  std::int32_t stalled = 0;           // the 1-based point of the path the fit did not settle at,
                                      // whose earlier points are the ones returned, as glmnet's
                                      // jerr = -m; 0 where the path ran to its end
  Family family = Family::gaussian;
};

// Throws Error for an empty design, for a response the family cannot read, and for a fit that
// does not settle at the first penalty of its path. A fit that does not settle at a later penalty,
// inside `max_irls` reweightings or inside what is left of `max_pass`, ends the path there and
// records it in `stalled`.
PenaltyPath penalised_path(const double* x, const double* y, const double* w, std::size_t n,
                           std::size_t p, Family family, const PenaltySpec& spec);

struct PenaltyCV {
  PenaltyPath path;               // the fit on every unit, which the folds are scored along
  std::vector<double> cv_mean;    // held-out deviance, one per point of the path
  std::vector<double> cv_sd;      // its standard error over the units
  std::size_t index_min = 0;      // the point of least held-out deviance
  std::size_t index_1se = 0;      // the largest penalty within one standard error of it
  std::vector<std::int32_t> fold_stalled;  // each fold's `stalled`, in fold order
};

// The path fitted on every unit, and the same penalties scored on units held out fold by fold.
// `fold` is one 0-based fold index per unit, which is what keeps a grouping whole: the caller
// deals the folds, not this.
PenaltyCV penalised_cv(const double* x, const double* y, const double* w, std::size_t n,
                       std::size_t p, Family family, const PenaltySpec& spec,
                       const std::int32_t* fold, std::int32_t n_fold);

// The coefficients at one penalty, interpolated between the two points of the path around it
// where the penalty is not one of them, as glmnet's `predict(s = )` interpolates.
void penalised_coef(const PenaltyPath& path, double lambda, double* a0, double* beta);

// The response at one penalty: the linear predictor for a Gaussian family, the probability for a
// binomial one.
void penalised_predict(const PenaltyPath& path, double lambda, const double* x, std::size_t n,
                       double* out);

}  // namespace timesift

#endif  // TIMESIFT_TS_PENALISED_H
