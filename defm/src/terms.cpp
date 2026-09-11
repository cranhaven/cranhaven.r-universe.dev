#include <Rcpp.h>
#include "barry/barry.hpp"
#include "barry/models/defm.hpp"
#include "defm-common.h"

using namespace Rcpp;

//' Model specification for DEFM
//'
//' @param m An object of class [DEFM].
//' @param covar String. Name of a covariate to use as an interaction
//' for the effect. If equal to `""`, then no interaction effect.
//' is used.
//' used to weight the term.
//' @export
//'
//' @returns Invisible 0.
//'
//' @name defm_terms
//' @aliases terms_defm
//' @examples
//' # Loading Valtente's SNS data
//' data(valentesnsList)
//' 
//' mymodel <- new_defm(
//'   id    = valentesnsList$id,
//'   Y     = valentesnsList$Y,
//'   X     = valentesnsList$X,
//'   order = 1
//' )
//' 
//' # Conventional regression intercept
//' td_logit_intercept(mymodel)
//'
//' # Interaction effect with Hispanic
//' td_logit_intercept(mymodel, covar = "Hispanic")
//' 
//' # Transition effect from only y1 to both equal to 1.
//' td_formula(mymodel, "{y1, 0y2} > {y1, y2}")
//' 
//' # Same but interaction with Female
//' td_formula(mymodel, "{y1, 0y2} > {y1, y2} x Female")
//'
//' # Inspecting the model
//' mymodel
//' 
//' # Initializing and fitting
//' init_defm(mymodel)
//' defm_mle(mymodel)
// [[Rcpp::export(invisible = true, rng = false)]]
SEXP td_ones(
  SEXP m,
  std::string covar = ""
) {

  int idx_ = -1;
  Rcpp::XPtr< defm::DEFM > ptr(m);

  // This will set the covar index, if needed
  check_covar(idx_, covar, ptr);

  defm::counter_ones(
    ptr->get_counters(), idx_,
    &ptr->get_X_names()
    );

  return m;
}

//' @param mat Integer matrix. The matrix specifies the type of motif to capture
//' (see details.)
//' @details
//' In `td_generic`, users can specify a particular motif to model. Motifs
//' are represented by cells with values equal to 1, for example, the matrix:
//'
//' ```  y0 y1 y2
//' t0:   1 NA NA
//' t1:   1  1 NA
//' ```
//'
//' represents a transition `y0 -> (y1, y2)`. If 0 is a motif of interest, then
//' the matrix should include 0 to mark zero values.
//' @export
//' @rdname defm_terms
// [[Rcpp::export(invisible = true, rng = false)]]
SEXP td_generic(
    SEXP m,
    IntegerMatrix & mat,
    std::string covar = ""
)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);
  int idx_ = -1;

  // This will set the covar index, if needed
  check_covar(idx_, covar, ptr);

  if (static_cast<size_t>(mat.nrow()) != (ptr->get_m_order() + 1u))
    stop("The number of rows in -mat- must be equal to the Markov order of the model + 1.");

  if (static_cast<size_t>(mat.ncol()) != ptr->get_n_y())
    stop("The number of columns in -mat- must be equal to the number of y-columns in the model.");

  // Converting coordinates
  std::vector< size_t > coords(0u);
  std::vector< bool > signs(0u);
  int loc = -1;
  for (int j = 0; j < static_cast<int>(mat.ncol()); ++j)
  {

    for (int i = 0; i < static_cast<int>(mat.nrow()); ++i)
    {

      loc++;

      // Only 1 or -1 make something
      if (mat(i,j) == R_NaInt)
        continue;

      if ((mat(i,j) != 1) && (mat(i,j) != 0))
        stop("Valid values for -mat- are NA, 0, or 1");

      coords.push_back(static_cast< size_t >(loc));
      signs.push_back(
        mat(i,j) == 1 ? true : false
      );

    }

  }

    defm::counter_generic(
      ptr->get_counters(), coords, signs,
      ptr->get_m_order(), ptr->get_n_y(),
      idx_,
      &ptr->get_X_names(),
      &ptr->get_Y_names()
    );

  return m;

}


//' @details The function `td_formula`,
//' will take the formula and generate the corresponding
//' input for defm::counter_transition(). Formulas can be specified in the
//' following ways:
//'
//' - Intercept effect: `{...}` No transition, only including the current state.
//' - Transition effect: `{...} > {...}` Includes current and previous states.
//'
//' The general notation is `[0]y[column id]_[row id]`. A preceeding zero
//' means that the value of the cell is considered to be zero. The column
//' id goes between 0 and the number of columns in the array - 1 (so it
//' is indexed from 0,) and the row id goes from 0 to m_order.
//'
//' Both Intercepts and Transition can interact with covariates. Using 
//' either the `covar` argument or, in the case of formulas, `x [Covar name]`,
//' for example:
//'
//' - Intercept effect: `{...} x Hispanic` interacts with the Hispanic covar.
//' - Transition effect: `{...} > {...} x Hispanic` Same.
//'
//' ## Intercept effects
//'
//' Intercept effects only involve a single set of curly brackets. Using the
//' 'greater-than' symbol (i.e., `<`) is only for transition effects. When
//' specifying intercept effects, users can skip the `row_id`, e.g.,
//' `y0_0` is equivalent to `y0`. If the passed `row id` is different from
//' the Markov order, i.e., `row_id != m_order`, then the function returns
//' with an error.
//'
//' Examples:
//'
//' - `"{y0, 0y1}"` is equivalent to set a motif with the first element equal
//' to one and the second to zero.
//'
//' ## Transition effects
//'
//' Transition effects can be specified using two sets of curly brackets and
//' an greater-than symbol, i.e., `{...} > {...}`. The first set of brackets,
//' which we call LHS, can only hold `row id` that are less than `m_order`.
//' @param formula Character scalar (see details).
//' @param new_name Character scalar. Name to be assigned for the new term.
//' if empty, then it builds a name based on the formula.
//' @export
//' @rdname defm_terms
// [[Rcpp::export(invisible = true, rng = false)]]
SEXP td_formula(
    SEXP m,
    std::string formula,
    std::string new_name = ""
)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);

  defm::counter_formula(
    ptr->get_counters(), formula,
    ptr->get_m_order(),
    ptr->get_n_y(),
    &ptr->get_X_names(),
    &ptr->get_Y_names()
  );

  // Renaming the counter on the fly (if the name is too long)
  if (new_name != "")
  {
    auto n_counter = ptr->nterms();

    if (n_counter == 0)
      stop("Something went wrong. No terms in the model.");

    (*ptr->get_counters())[n_counter - 1].set_name(
      new_name
    );
  }

  return m;

}

//' @export
//' @rdname defm_terms
//' @details The term `td_logit_intercept` will add what is equivalent to an
//' intercept in a logistic regression. When `y_indices` is specified, then the
//' function will add one intercept per outcome. These can be weighted by
//' a covariate.
//' @param y_indices Integer vector with the coordinates to include in the term.
// [[Rcpp::export(invisible = true, rng = false)]]
SEXP td_logit_intercept(
  SEXP m,
  IntegerVector y_indices = IntegerVector::create(),
  std::string covar = ""
) {

  Rcpp::XPtr< defm::DEFM > ptr(m);
  int idx_ = -1;

  // This will set the covar index, if needed
  check_covar(idx_, covar, ptr);

  std::vector< size_t > coords_;
  for (auto c : y_indices)
  {
    if (c < 0)
      stop("Element in `y_indices` is negative. Only zero or positive are allowed");
    coords_.push_back(c);
  }

  defm::counter_logit_intercept(
    ptr->get_counters(),
    ptr->get_n_y(),
    coords_,
    idx_,
    &ptr->get_X_names(),
    &ptr->get_Y_names()
  );

  return m;

}


//' @details The function `rule_not_one_to_zero` will avoid the transition one to zero in a Markov process.
//' @export
//' @rdname defm_terms
//' @param term_indices Non-negative vector of indices. Indicates which
//' outcomes this rule will apply.
// [[Rcpp::export(invisible = true, rng = false)]]
SEXP rule_not_one_to_zero(
  SEXP m,
  std::vector< size_t > term_indices
) {

  Rcpp::XPtr< defm::DEFM > ptr(m);

  defm::rules_dont_become_zero(
    ptr->get_support_fun(),
    term_indices
  );

  return m;
}

//' @details The function `rule_constrain_support` will constrain the support of the model
//' by specifying a lower and upper bound for a given statistic.
//' @param lb,ub Numeric scalars. Lower and upper bounds.
//' @param term_index Non-negative scalar. Which term this rule will apply.
//' @rdname defm_terms
//' @export
// [[Rcpp::export(invisible = true, rng = false)]]
SEXP rule_constrain_support(
  SEXP m,
  int term_index,
  double lb,
  double ub
) {

  if (term_index < 0)
    stop("`term_index` must be greater than or equal to zero.");

  Rcpp::XPtr< defm::DEFM > ptr(m);

  defm::rule_constrain_support(
    ptr->get_support_fun(),
    static_cast<size_t>(term_index),
    lb,
    ub
  );

  return m;



}
