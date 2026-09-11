#include <Rcpp.h>
#include <barry/barry.hpp>
#include <barry/models/defm.hpp>

using namespace Rcpp;

//' Discrete Exponential Family Model (DEFM)
//'
//' Discrete Exponential Family Models (DEFMs) are models from the exponential
//' family that deal with discrete data. Here, we deal with binary arrays which
//' can be used to represent, among other things, networks and multinomial binary
//' Markov processes.
//'
//' @param id Integer vector of length `n`. Observation ids, for example, person id.
//' @param Y 0/1 matrix of responses of `n_y` columns and `n` rows.
//' @param X Numeric matrix of covariates of size `n_x` by `n`.
//' @param order Integer. Order of the markov process, by default, 1.
//' @param copy_data Logical scalar. When `TRUE` (default) will copy the data
//' into the model, otherwise it will use the data as a pointer (see details).
//'
//' @details
//' The `id` vector is used to group the observations. For example, if you have
//' a dataset with multiple individuals, the `id` vector should contain the
//' individual ids. The `Y` matrix contains the binary responses, where each
//' column represents a different response variable. The `X` matrix contains
//' the covariates, which can be used to model the relationship between the
//' responses and the covariates. The `order` parameter specifies the order of
//' the Markov process, which determines how many previous observations are
//' used to predict the current observation.
//'
//' The `copy_data` parameter specifies
//' whether the data should be copied into the model or used as a pointer. If
//' `copy_data` is `TRUE`, the data will be copied into the model, which can
//' be useful if you want to avoid duplicating the data in memory. If 
//' `copy_data` is `FALSE`, the model will use the data as a pointer, which can 
//' be more efficient (but dangerous if the data is removed).
//' @return An external pointer of class `DEFM.`
//'
//' @name DEFM
//' @aliases new_defm defm
//' @examples
//' # Loading Valente's SNS data
//' data(valentesnsList)
//' 
//' mymodel <- new_defm(
//'   id = valentesnsList$id,
//'   Y = valentesnsList$Y,
//'   X = valentesnsList$X,
//'   order = 1
//' )
//' 
//' # Adding the intercept terms and a motif from tobacco to mj
//' td_logit_intercept(mymodel)
//' td_formula(mymodel, "{y1, 0y2} > {y1, y2}")
//' 
//' # Initialize the model
//' init_defm(mymodel)
//' 
//' # Fitting the MLE
//' defm_mle(mymodel)
//' 
// [[Rcpp::export(rng = false, name = 'new_defm_cpp')]]
SEXP new_defm(
    SEXP & id,
    SEXP & Y,
    SEXP & X,
    int order = 1,
    bool copy_data = true
  ) {

  // Adding typechecks to see if the SEXP objects have
  // the class attribute equal to the type
  if (!Rf_isInteger(id)) {
    stop("id must be an integer vector");
  }
  if (!Rf_isMatrix(Y) || !Rf_isInteger(Y)) {
    stop("Y must be an integer matrix");
  }
  if (!Rf_isMatrix(X) || !Rf_isNumeric(X)) {
    stop("X must be a numeric matrix");
  }

  int n_id = LENGTH(id);
  int n_y  = Rf_ncols(Y);
  int n_x  = Rf_ncols(X);

  if (n_id <= order)
    stop("The -order- cannot be greater than the number of observations.");

  if (n_id != Rf_nrows(Y))
    stop("The number of rows in Y does not match the length of id.");

  if (n_id != Rf_nrows(X))
    stop("The number of rows in X does not match the length of id.");

  Rcpp::XPtr< defm::DEFM > model(new defm::DEFM(
    &(INTEGER(id)[0u]),
    &(INTEGER(Y)[0u]),
    &(REAL(X)[0u]),
    static_cast< size_t >(n_id),
    static_cast< size_t >(n_y),
    static_cast< size_t >(n_x),
    order,
    copy_data
  ), true);

  model.attr("class") = "DEFM";

  return model;

}

// [[Rcpp::export(invisible = true, rng = false)]]
SEXP set_names(
  SEXP m,
  const std::vector< std::string > & ynames,
  const std::vector< std::string > & xnames
) {

  Rcpp::XPtr< defm::DEFM > ptr(m);
  ptr->set_names(
    ynames,
    xnames
  );

  return m;

}

//' Access to the names of a model's datasets
//'
//' Retrieve the column names of the dependent variable (`y`) and independent
//' variable (`x`) of an object of class [DEFM].
//'
//' @param m An object of class [DEFM].
//' @name defm-names
//' @returns A character vector.
//' @export
//' @return A character vector with the names of the dependent or independent
//' variables.
//' @examples
//' #' Using Valente's SNS data
//' data(valentesnsList)
//' 
//' # Creating the DEFM object
//' mymodel <- new_defm(
//'   id = valentesnsList$id,
//'   X = valentesnsList$X,
//'   Y = valentesnsList$Y,
//'   order = 0
//' )
//' 
//' # Getting the names
//' get_X_names(mymodel)
//' get_Y_names(mymodel)
// [[Rcpp::export(rng = false)]]
CharacterVector get_Y_names(
    SEXP m
) {
  Rcpp::XPtr< defm::DEFM > ptr(m);
  return wrap(ptr->get_Y_names());
}

//' @rdname defm-names
//' @export
// [[Rcpp::export(rng = false)]]
CharacterVector get_X_names(
    SEXP m
) {
  Rcpp::XPtr< defm::DEFM > ptr(m);
  return wrap(ptr->get_X_names());
}


//' @rdname DEFM
//' @param m An object of class `DEFM`.
//' @param force_new Logical scalar. When `TRUE` (default) no cache is used
//' to add new arrays (see details).
//' @details
//' The `init_defm` function initializes the model, which means it computes
//' the sufficient statistics and prepares the model for fitting. The 
//' `force_new` parameter specifies whether to force the model to be 
//' consider each array added as completely unique, even if it has the
//' same support set as an existing array. This is an experimental feature
//' and should be used with caution.  
//' @export
// [[Rcpp::export(invisible = true, rng = false)]]
SEXP init_defm(SEXP m, bool force_new = false)
{
  Rcpp::XPtr< defm::DEFM > ptr(m);
  ptr->init(force_new);
  return m;
}


// [[Rcpp::export(invisible = true, rng = false, name = "print_defm_cpp")]]
SEXP print_defm(SEXP x)
{

  Rcpp::XPtr< defm::DEFM > ptr(x);

  ptr->print();

  return x;

}


//' Log-Likelihood of DEFM
//'
//' @param m An object of class [DEFM]
//' @param par A vector of parameters of length `nterms_defm(m)`.
//' @param as_log Logical scalar. When `TRUE` (default) returns the log-likelihood,
//' otherwise it returns the likelihood.
//' @return
//' Numeric, the computed likelihood or log-likelihood of the model.
//' @export
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
//' # Adding the intercept terms and a motif from tobacco to mj
//' td_logit_intercept(mymodel)
//' td_formula(mymodel, "{y1, 0y2} > {y1, y2}")
//'
//' # Computing the log-likelihood
//' loglike_defm(mymodel, par = c(-1, -1, -1, 2), as_log = TRUE)
// [[Rcpp::export(rng = false)]]
double loglike_defm(SEXP m, std::vector< double > par, bool as_log = true)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);

  double res = ptr->likelihood_total(par, as_log);

  if (std::isfinite(res))
    return res;
  else
    return R_NegInf;

}

//' Simulate data using a DEFM
//'
//' @param m An object of class [DEFM]. The baseline model.
//' @param par Numeric vector of model parameters.
//' @param fill_t0 Logical scalar. When `TRUE` (default) will fill-in the baseline
//' value of each observation (i.e., the starting condition) (see details.)
//'
//' @details
//' Each observation in the simulation must have initial condition. In practice,
//' this means we start the markov process with a matrix of size
//' `morder_defm(m) x ncol_defm_y(m)`, i.e., order of the Markov process times
//' the number of output variables. when `fill_t0 = TRUE`, the function return
//' the rows corresponding to baseline states with the original value, otherwise
//' it replaces them with -1. This option is mostly for testing purposes.
//'
//' @returns An integer vector of size `nrows_defm(m) x ncol_defm_y(m)`.
//' @export
// [[Rcpp::export(rng = true)]]
IntegerMatrix sim_defm(
    SEXP m,
    std::vector< double > par,
    bool fill_t0 = true
  )
{

  size_t seed = static_cast<size_t>(
    R::unif_rand() * static_cast<double>(std::numeric_limits< size_t >::max())
  );


  Rcpp::XPtr< defm::DEFM > ptr(m);

  ptr->set_seed(seed);

  size_t nrows = ptr->get_n_rows();
  size_t ncols = ptr->get_n_y();

  std::vector< int > out(nrows * ncols, -1);
  ptr->simulate(par, &(out[0u]));

  IntegerMatrix res(nrows, ncols);

  size_t iter = 0u;
  const int * Y = ptr->get_Y();
  for (size_t i = 0u; i < nrows; ++i)
  {

    for (size_t j = 0u; j < ncols; ++j)
    {

      if (fill_t0 & (out[iter] == -1))
        res(i, j) = *(Y + j * nrows + i); // Column wise
      else
        res(i, j) = out[iter]; // But the simulation is row wise

      iter++;

    }

  }

  // Adding a name
  CharacterVector names = wrap(ptr->get_Y_names());
  Rcpp::colnames(res) = names;

  return res;
}


//' @export
//' @rdname DEFM
//' @param i An integer scalar indicating which set of statistics to print (see details.)
//' @details
//' The `print_stats` function prints the supportset of the ith type
//' of array in the model.
// [[Rcpp::export(rng = false, invisible = true)]]
int print_stats(SEXP m, int i = 0)
{
  Rcpp::XPtr< defm::DEFM > ptr(m);
  ptr->print_stats(static_cast< size_t >(i));

  return 0;
}

//' @export
//' @rdname DEFM
//' @returns - `nterms_defm` returns the number of terms in the model.
// [[Rcpp::export(rng = false)]]
int nterms_defm(SEXP m)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);
  return ptr->nterms();
}

//' @export
// [[Rcpp::export(rng = false, name = "names.DEFM")]]
CharacterVector names_defm(SEXP x)
{

  Rcpp::XPtr< defm::DEFM > ptr(x);
  return wrap(ptr->colnames());
}

//' @export
//' @rdname DEFM
//' @returns - `nrow_defm` returns the number of rows in the model.
// [[Rcpp::export(rng = false)]]
int nrow_defm(SEXP m)
{
  Rcpp::XPtr< defm::DEFM > ptr(m);
  return ptr->get_n_rows();
}

//' @export
//' @rdname DEFM
//' @returns - `ncol_defm_y` returns the number of output variables in 
//' the model.
// [[Rcpp::export(rng = false)]]
int ncol_defm_y(SEXP m)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);

  return ptr->get_n_y();

}

//' @export
//' @rdname DEFM
//' @returns - `ncol_defm_x` returns the number of covariates in the model.
// [[Rcpp::export(rng = false)]]
int ncol_defm_x(SEXP m)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);

  return ptr->get_n_covars();

}

//' @export
//' @rdname DEFM
//' @returns - `nobs_defm` returns the number of observations (events) in the
//' model.
// [[Rcpp::export(rng = false)]]
int nobs_defm(SEXP m)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);

  return ptr->get_n_obs();

}

//' @export
//' @rdname DEFM
//' @returns - `morder_defm` returns the order of the Markov process.
// [[Rcpp::export(rng = false)]]
int morder_defm(SEXP m)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);

  return ptr->get_m_order();

}

//' Get sufficient statistics counts
//' 
//' This function computes the individual counts of the sufficient statistics
//' included in the model. 
//' @param m An object of class [DEFM].
//' @export
//' @return A matrix with the counts of the sufficient statistics.
//' @examples
//' data(valentesnsList)
//' 
//' mymodel <- new_defm(
//'   id = valentesnsList$id,
//'   Y = valentesnsList$Y,
//'   X = valentesnsList$X,
//'   order = 1
//' )
//' 
//' # Adding the intercept terms and a motif from tobacco to mj
//' td_logit_intercept(mymodel)
//' td_formula(mymodel, "{y1, 0y2} > {y1, y2}")
//' 
//' # Initialize the model
//' init_defm(mymodel)
//' 
//' # Get the counts
//' head(get_stats(mymodel))
// [[Rcpp::export(rng = false)]]
NumericMatrix get_stats(SEXP m)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);

  auto model = ptr->get_model();

  // Getting sizes
  size_t nrows = ptr->get_n_rows();
  size_t ncols = model.nterms();
  size_t m_ord = ptr->get_m_order();

  const int * ID = ptr->get_ID();

  NumericMatrix res(nrows, ncols);
  auto target = model.get_stats_target();


  size_t i_effective = 0u;
  size_t n_obs_i = 0u;
  for (size_t i = 0u; i < nrows; ++i)
  {

    // Do we need to reset the counter?
    if ((i > 0) && (*(ID + i - 1u) != *(ID + i)))
      n_obs_i = 0u;

    // Did we passed the Markov order?
    if (n_obs_i++ < m_ord)
    {
      std::fill(res.row(i).begin(), res.row(i).end(), NA_REAL);
      continue;
    }

    for (size_t j = 0u; j < ncols; ++j)
      res(i, j) = (*target)[i_effective][j];

    i_effective++;

  }

  // Setting the names
  Rcpp::CharacterVector cnames(0);
  for (auto & n : model.colnames())
    cnames.push_back(n);
  Rcpp::colnames(res) = cnames;

  return res;

}

// [[Rcpp::export(rng = false)]]
NumericMatrix motif_census_cpp(SEXP m, std::vector<size_t> locs)
{

  Rcpp::XPtr< defm::DEFM > ptr(m);

  barry::FreqTable<int> res = ptr->motif_census(locs);
  auto dat = res.get_data();

  size_t nele = res.size();
  NumericMatrix m_res(nele, locs.size() * (ptr->get_m_order() + 1) + 1);

  size_t ele = 0u;
  for (size_t i = 0u; i < nele; ++i)
    for (size_t j = 0u; j < (locs.size() * (ptr->get_m_order() + 1) + 1); ++j)
      m_res(i, j) = dat[ele++];

  // Setting the names
  Rcpp::CharacterVector cnames = {"count"};
  for (size_t m = 0u; m < (ptr->get_m_order() + 1); ++m)
  {
    for (auto & n : locs)
      cnames.push_back(
        std::string("y") + std::to_string(m) + std::to_string(n)
      );
  }

  Rcpp::colnames(m_res) = cnames;

  return m_res;

}

//' @param i,j The row and column of the array to turn on for the log odds.
//' @param par The parameters of the model.
//' @param m An object of class [DEFM].
//' @return - `logodds` returns a numeric vector with the log-odds for each observation in the data.
//' @rdname defm_mle
//' @export
// [[Rcpp::export(rng = false)]]
NumericVector logodds(
    SEXP m,
    const std::vector<double> & par,
    int i,
    int j
) {

  // Error if i or j are negative
  if (i < 0 || j < 0)
    stop("i and j must be positive.");

  Rcpp::XPtr< defm::DEFM > ptr(m);

  return wrap(ptr->logodds(par, static_cast<size_t>(i), static_cast<size_t>(j)));

}

// [[Rcpp::export(rng = false)]]
LogicalVector is_motif(SEXP m) {
  Rcpp::XPtr< defm::DEFM > ptr(m);

  return wrap(ptr->is_motif());

}


