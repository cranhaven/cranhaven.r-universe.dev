


#' An internal function to set priors and initials values
#'
#'@description The \code{set_priors_initials}) sets priors and initials values
#'  which are passed from the [bsitar::bsitar()] function to
#'  \code{set_priors_initials}. For univariate-by- subgroup model (specified by
#'  using the \code{univariate_by}) and multivariate model (specified by using
#'  the \code{multivariate}), each argument is automatically matched with the
#'  sub-model(s).
#'
#'@param a_prior_beta Specify priors for the fixed effect parameter, \code{a}.
#'  See [bsitar::bsitar()] for details.
#'
#'@param b_prior_beta Specify priors for the fixed effect parameter, \code{b}.
#'  See [bsitar::bsitar()] for details.
#'
#'@param c_prior_beta Specify priors for the fixed effect parameter, \code{c}.
#'  See [bsitar::bsitar()] for details.
#'
#'@param d_prior_beta Specify priors for the fixed effect parameter, \code{d}.
#'  See [bsitar::bsitar()] for details.
#'
#'@param e_prior_beta Specify priors for the fixed effect parameter, \code{e}.
#'  See [bsitar::bsitar()] for details.
#'
#'@param f_prior_beta Specify priors for the fixed effect parameter, \code{f}.
#'  See [bsitar::bsitar()] for details.
#'
#'@param s_prior_beta Specify priors for the fixed effect parameter, \code{s}.
#'  See [bsitar::bsitar()] for details.
#'
#'@param a_cov_prior_beta Specify priors for the covariate(s) included for the
#'  fixed effect parameter, \code{a}. See [bsitar::bsitar()] for details.
#'
#'@param b_cov_prior_beta Specify priors for the covariate(s) included for the
#'  fixed effect parameter, \code{b}. See [bsitar::bsitar()] for details.
#'
#'@param c_cov_prior_beta Specify priors for the covariate(s) included for the
#'  fixed effect parameter, \code{c}. See [bsitar::bsitar()] for details.
#'
#'@param d_cov_prior_beta Specify priors for the covariate(s) included for the
#'  fixed effect parameter, \code{d}. See [bsitar::bsitar()] for details.
#'
#'@param e_cov_prior_beta Specify priors for the covariate(s) included for the
#'  fixed effect parameter, \code{e}. See [bsitar::bsitar()] for details.
#'
#'@param f_cov_prior_beta Specify priors for the covariate(s) included for the
#'  fixed effect parameter, \code{f}. See [bsitar::bsitar()] for details.
#'
#'@param s_cov_prior_beta Specify priors for the covariate(s) included for the
#'  fixed effect parameter, \code{s}. See [bsitar::bsitar()] for details.
#'
#'@param a_prior_sd Specify prior on the standard deviation (sd) of random
#'  effect parameter, \code{a}. See [bsitar::bsitar()] for details.
#'
#'@param b_prior_sd Specify prior on the standard deviation (sd) of random
#'  effect parameter, \code{b}. See [bsitar::bsitar()] for details.
#'
#'@param c_prior_sd Specify prior on the standard deviation (sd) of random
#'  effect parameter, \code{c}. See [bsitar::bsitar()] for details.
#'
#'@param d_prior_sd Specify prior on the standard deviation (sd) of random
#'  effect parameter, \code{d}. See [bsitar::bsitar()] for details.
#'
#'@param e_prior_sd Specify prior on the standard deviation (sd) of random
#'  effect parameter, \code{e}. See [bsitar::bsitar()] for details.
#'
#'@param f_prior_sd Specify prior on the standard deviation (sd) of random
#'  effect parameter, \code{f}. See [bsitar::bsitar()] for details.
#'
#'@param a_cov_prior_sd Specify prior on the standard deviation (sd) for the
#'  covariate(s) included in the random effect parameter, \code{a}. See
#'  [bsitar::bsitar()] for details.
#'
#'@param b_cov_prior_sd Specify prior on the standard deviation (sd) for the
#'  covariate(s) included in the random effect parameter, \code{b}. See
#'  [bsitar::bsitar()] for details.
#'
#'@param c_cov_prior_sd Specify prior on the standard deviation (sd) for the
#'  covariate(s) included in the random effect parameter, \code{c}. See
#'  [bsitar::bsitar()] for details.
#'
#'@param d_cov_prior_sd Specify prior on the standard deviation (sd) for the
#'  covariate(s) included in the random effect parameter, \code{d}. See
#'  [bsitar::bsitar()] for details.
#'
#'@param e_cov_prior_sd Specify prior on the standard deviation (sd) for the
#'  covariate(s) included in the random effect parameter, \code{w}. See
#'  [bsitar::bsitar()] for details.
#'
#'@param f_cov_prior_sd Specify prior on the standard deviation (sd) for the
#'  covariate(s) included in the random effect parameter, \code{f}. See
#'  [bsitar::bsitar()] for details.
#'
#'@param sigma_prior_beta Specify prior on the the distributional fixed effect
#'  parameter, \code{sigma}. See [bsitar::bsitar()] for details.
#'
#'@param sigma_cov_prior_beta Specify prior on the covariate(s) included in the
#'  distributional fixed effect parameter, \code{sigma}.  See [bsitar::bsitar()]
#'  for details.
#'
#'@param sigma_prior_sd Specify prior on the standard deviation (sd) of
#'  distributional random effect parameter, \code{sigma}. See [bsitar::bsitar()]
#'  for details.
#'
#'@param sigma_cov_prior_sd Specify prior on the standard deviation (sd) of
#'  covariate(s) included in the distributional random effect parameter,
#'  \code{sigma}. See [bsitar::bsitar()] for details.
#'
#'@param gr_prior_cor Specify prior on the correlation for group level random
#'  effect parameters. See [bsitar::bsitar()] for details.
#'
#'@param sigma_prior_cor Specify prior on the correlation for distribution level
#'  random effect parameters. See [bsitar::bsitar()] for details.
#'
#'@param rsd_prior_sigma Specify prior on the residual standared deviation
#'  parameter, \code{sigma}, See [bsitar::bsitar()] for details,
#'
#'@param dpar_prior_sigma Specify prior on the distributional parameter,
#'  \code{sigma} (which is same as residual standared deviation for Gaussian
#'  distribution). See [bsitar::bsitar()] for details,
#'
#'@param dpar_cov_prior_sigma Specify prior for the covariate(s) included in the
#'  distributional parameter, \code{sigma} (which is same as residual standard
#'  deviation for Gaussian distribution).See [bsitar::bsitar()] for details,
#'
#'@param autocor_prior_acor Specify priors on the the autocorrelation parameters
#'  \code{ar}, \code{ma} and \code{arma}. See [bsitar::bsitar()] for details,
#'
#'@param autocor_prior_unstr_acor Specify priors on the the unstructured
#'  autocorrelation parameter. See [bsitar::bsitar()] for details,
#'
#'@param mvr_prior_rescor Specify priors on the the residual correlation
#'  parameter for multivariate model. See [bsitar::bsitar()] for details,
#'
#'@param prior_data An optional argument (as named list) that can pass value for
#'  prior. See [bsitar::bsitar()] for details,
#'
#'@param prior_data_internal An internal data frame (named list) used to pass on
#'  the relevant information on priors from the [bsitar::bsitar()] function to
#'  the \code{set_priors_initials}.
#'
#'@param prior_args_internal An internal argument list that is passed from the
#'  [bsitar::bsitar()] function to the \code{set_priors_initials} and is used
#'  for setting the priors.
#'
#'@param init_arguments A list containing all the init arguments specified in
#'  the [bsitar::bsitar()] function and now passed on to the
#'  \code{set_priors_initials}.
#'
#'@param init_data An optional data argument (named list) used to pass initial
#'  values. See [bsitar::bsitar()] function, \code{prior_data} for details.
#'
#'@param init_data_internal An internal data frame (named list) to pass on the
#'  relevant information on initials from the [bsitar::bsitar()] function to the
#'  \code{set_priors_initials}.
#'
#'@param init_args_internal An internal argument list that is passed from the
#'  [bsitar::bsitar()] function to the \code{set_priors_initials} and is used
#'  for setting the initials.
#'
#'@param custom_order_prior_str An internal argument that is passed from the
#'  [bsitar::bsitar()] function to the \code{set_priors_initials} when setting
#'  the priors for the model with hierarchy level 3 and beyond. See
#'  [bsitar::bsitar()] for details,
#'
#'@return An object of class \code{brmsprior} (See \code{brmsprior}). In
#'  addition to the priors, the returned object contains a list of initial
#'  values.
#'  
#' @author Satpal Sandhu  \email{satpal.sandhu@bristol.ac.uk}
#'  
#' @keywords internal
#' @noRd
#'
set_priors_initials <- function(a_prior_beta,
                                b_prior_beta,
                                c_prior_beta,
                                d_prior_beta,
                                e_prior_beta,
                                f_prior_beta,
                                g_prior_beta,
                                h_prior_beta,
                                i_prior_beta,
                                s_prior_beta,
                                a_cov_prior_beta,
                                b_cov_prior_beta,
                                c_cov_prior_beta,
                                d_cov_prior_beta,
                                e_cov_prior_beta,
                                f_cov_prior_beta,
                                g_cov_prior_beta,
                                h_cov_prior_beta,
                                i_cov_prior_beta,
                                s_cov_prior_beta,
                                a_prior_sd,
                                b_prior_sd,
                                c_prior_sd,
                                d_prior_sd,
                                e_prior_sd,
                                f_prior_sd,
                                g_prior_sd,
                                h_prior_sd,
                                i_prior_sd,
                                s_prior_sd,
                                a_cov_prior_sd,
                                b_cov_prior_sd,
                                c_cov_prior_sd,
                                d_cov_prior_sd,
                                e_cov_prior_sd,
                                f_cov_prior_sd,
                                g_cov_prior_sd,
                                h_cov_prior_sd,
                                i_cov_prior_sd,
                                s_cov_prior_sd,
                                gr_prior_cor,
                                sigma_prior_cor,
                                sigma_prior_beta,
                                sigma_cov_prior_beta,
                                sigma_prior_sd,
                                sigma_cov_prior_sd,
                                rsd_prior_sigma,
                                dpar_prior_sigma,
                                dpar_cov_prior_sigma,
                                autocor_prior_acor,
                                autocor_prior_unstr_acor,
                                mvr_prior_rescor,
                                prior_data             = NULL,
                                prior_data_internal    = NULL,
                                prior_args_internal    = NULL,
                                init_arguments         = NULL,
                                init_data              = NULL,
                                init_data_internal     = NULL,
                                init_args_internal     = NULL,
                                custom_order_prior_str = NULL) {
  
  # Initiate non formalArgs()
  resp <- NULL;
  autocor_formi <- NULL;
  randomsi <- NULL;
  sigma_formula_grsi <- NULL;
  a_formulasi <- NULL;
  b_formulasi <- NULL;
  c_formulasi <- NULL;
  d_formulasi <- NULL;
  e_formulasi <- NULL;
  f_formulasi <- NULL;
  g_formulasi <- NULL;
  h_formulasi <- NULL;
  i_formulasi <- NULL;
  s_formulasi <- NULL;
  select_model <- NULL;
  fixedsi <- NULL;
  a_formula_grsi <- NULL;
  b_formula_grsi <- NULL;
  c_formula_grsi <- NULL;
  d_formula_grsi <- NULL;
  e_formula_grsi <- NULL;
  f_formula_grsi <- NULL;
  g_formula_grsi <- NULL;
  h_formula_grsi <- NULL;
  i_formula_grsi <- NULL;
  s_formula_grsi <- NULL;
  acovcoefnames <- NULL;
  bcovcoefnames <- NULL;
  ccovcoefnames <- NULL;
  dcovcoefnames <- NULL;
  ecovcoefnames <- NULL;
  fcovcoefnames <- NULL;
  gcovcoefnames <- NULL;
  hcovcoefnames <- NULL;
  icovcoefnames <- NULL;
  scovcoefnames <- NULL;
  acovcoefnames_gr <- NULL;
  bcovcoefnames_gr <- NULL;
  ccovcoefnames_gr <- NULL;
  dcovcoefnames_gr <- NULL;
  ecovcoefnames_gr <- NULL;
  fcovcoefnames_gr <- NULL;
  gcovcoefnames_gr <- NULL;
  hcovcoefnames_gr <- NULL;
  icovcoefnames_gr <- NULL;
  scovcoefnames_gr <- NULL;
  dpar_formulasi <- NULL;
  sigma_formulasi <- NULL;
  sigmacovcoefnames <- NULL;
  sigmacovcoefnames_gr <- NULL;
  nabcrei <- NULL;
  dparcovcoefnames <- NULL;
  sigma_arg_groupvar <- NULL;
  group_arg_groupvar <- NULL;
  autocor_formi <- NULL;
  acovcoefnames <- NULL;
  bcovcoefnames <- NULL;
  ccovcoefnames <- NULL;
  dcovcoefnames <- NULL;
  ecovcoefnames <- NULL;
  fcovcoefnames <- NULL;
  gcovcoefnames <- NULL;
  hcovcoefnames <- NULL;
  icovcoefnames <- NULL;
  scovcoefnames <- NULL;
  acovcoefnames_gr <- NULL;
  bcovcoefnames_gr <- NULL;
  ccovcoefnames_gr <- NULL;
  dcovcoefnames_gr <- NULL;
  ecovcoefnames_gr <- NULL;
  fcovcoefnames_gr <- NULL;
  gcovcoefnames_gr <- NULL;
  hcovcoefnames_gr <- NULL;
  icovcoefnames_gr <- NULL;
  scovcoefnames_gr <- NULL;
  sigmacovcoefnames <- NULL;
  sigmacovcoefnames_gr <- NULL;
  nabcrei <- NULL;
  resp <- NULL;
  nys <- NULL;
  fixedsi <- NULL;
  randomsi <- NULL;
  nabci <- NULL;
  nabcrei <- NULL;
  ii <- NULL;
  N_J_all <- NULL;
  dpar_formulasi <- NULL;
  initsi <- NULL;
  seed <- NULL;
  cortimeNlags_var <- NULL;
  cortimeNlags <- NULL;
  verbose <- NULL;
  sigma_formulasi <- NULL;
  s_formulasi <- NULL;
  sigma_formula_grsi <- NULL;
  nys <- NULL;
  cortimeNlags <- NULL;
  . <- NULL;
  d_adjustedsi <- NULL;
  
  
  
  eout <- list2env(prior_data_internal)
  for (eoutii in names(eout)) {
    assign(eoutii, eout[[eoutii]])
  }
  
  
  eout <- list2env(prior_args_internal)
  for (eoutii in names(eout)) {
    assign(eoutii, eout[[eoutii]])
  }
  
  
  # Depending on select_model, assign null values to all not part of the model
  for (set_randomsi_higher_levsli in c(letters[1:20])) {
    set_nlpar_what <- set_randomsi_higher_levsli
    if(!exists(paste0(set_randomsi_higher_levsli, 'form'))) {
      assign(paste0(set_nlpar_what, '_prior_beta'), NULL)
      assign(paste0(set_nlpar_what, '_cov_prior_beta'), NULL)
      assign(paste0(set_nlpar_what, '_prior_sd'), NULL)
      assign(paste0(set_nlpar_what, '_cov_prior_sd'), NULL)
    } else if(exists(paste0(set_randomsi_higher_levsli, 'form'))) {
      if(is.null(ept(paste0(set_randomsi_higher_levsli, 'form')))) {
        assign(paste0(set_nlpar_what, '_prior_beta'), NULL)
        assign(paste0(set_nlpar_what, '_cov_prior_beta'), NULL)
        assign(paste0(set_nlpar_what, '_prior_sd'), NULL)
        assign(paste0(set_nlpar_what, '_cov_prior_sd'), NULL)
      } # if(is.null(ept(paste0('f', 'form')))) {
    }
  } # for (set_randomsi_higher_levsli in c(letters[1:20])) {
  
  
  
  normalize <- ept(normalize)
  
  if (resp == "")
    resp_ <- ""
  if (resp != "")
    resp_ <- paste0("_", resp)
  
  
  if (is.null(autocor_formi)) {
    autocor_prior_acor <- NULL
    autocor_prior_unstr_acor <- NULL
  }
  
  if (!is.null(autocor_formi)) {
    if(grepl("unstr(", autocor_formi, fixed = T)) 
      autocor_prior_acor <- NULL
    if(!grepl("unstr(", autocor_formi, fixed = T)) 
      autocor_prior_unstr_acor <- NULL
  }
  
  
  getArgNames <-
    function(value)
      formalArgs(deparse(substitute(value)[[1]]))
  
  mvar <- multivariate$mvar
  
  if (!is.null(group_arg$cor)) {
    if (group_arg$cor == "un")
      abccorr <- TRUE
    if (group_arg$cor == "diagonal")
      abccorr <- FALSE
  } else {
    group_arg$cor <- "un"
    abccorr <- TRUE
  }
  
  if (group_arg$cor != "diagonal" & !group_arg$cor != "diagonal") {
    #  abccorr <- FALSE
  }
  
  if(group_arg$cor == "no") {
    abccorr <- FALSE
  }
  
  if (!is.null(group_arg$by)) {
    group_arg$by <- group_arg$by
  } else {
    group_arg$by <- NULL
  }
  
  if (!is.null(group_arg$cov)) {
    group_arg$cov <- group_arg$cov
  } else {
    group_arg$cov <- NULL
  }
  
  if (!is.null(group_arg$dist)) {
    group_arg$dist <- group_arg$dist
  } else {
    group_arg$dist <- 'gaussian'
  }
  
  if (!is.null(group_arg$verbose)) {
    group_arg$verbose <- group_arg$verbose
  } else {
    group_arg$verbose <- FALSE
  }
  
  # this on 9 5 23 to accomodate random = ''
  if(randomsi == "") gr_prior_cor <- NULL
  if(!abccorr)       gr_prior_cor <- NULL
  
  
  
  
  
  
  
  if (!is.null(sigma_group_arg$cor)) {
    if (sigma_group_arg$cor == "un")
      sigmacorr <- TRUE
    if (sigma_group_arg$cor == "diagonal")
      sigmacorr <- FALSE
  } else {
    sigma_group_arg$cor <- "un"
    sigmacorr <- TRUE
  }
  
  if (!is.null(sigma_group_arg$by)) {
    sigma_group_arg$by <- sigma_group_arg$by
  } else {
    sigma_group_arg$by <- NULL
  }
  
  if (!is.null(sigma_group_arg$cov)) {
    sigma_group_arg$cov <- sigma_group_arg$cov
  } else {
    sigma_group_arg$cov <- NULL
  }
  
  if (!is.null(sigma_group_arg$dist)) {
    sigma_group_arg$dist <- sigma_group_arg$dist
  } else {
    sigma_group_arg$dist <- 'gaussian'
  }
  
  if (!is.null(sigma_group_arg$verbose)) {
    sigma_group_arg$verbose <- sigma_group_arg$verbose
  } else {
    sigma_group_arg$verbose <- FALSE
  }
  
  
  # this on 9 5 23 to accomodate no random sigam 
  if(is.null(sigma_formula_grsi)) sigma_prior_cor <- NULL
  if(!sigmacorr)                  sigma_prior_cor <- NULL
  
  
  
  # If group and sigma ids are same, then sigma_prior_cor NULL otherwise 
  # duplicate priors error
  if(identical(sigma_group_arg$groupvar,
               group_arg$groupvar)) {
    sigma_prior_cor <- NULL
  }
  
  
  
  if (!(is.na(univariate_by$by) | univariate_by$by == "NA")) {
    if (!is.null(univariate_by$cor)) {
      if (univariate_by$cor == "un")
        uvarabccorr <- TRUE
      if (univariate_by$cor == "diagonal")
        uvarabccorr <- FALSE
    } else {
      univariate_by$cor <- "un"
      uvarabccorr <- TRUE
    }
    if (is.null(univariate_by$verbose))
      univariate_by$verbose <- FALSE
  }
  
  if ((is.na(univariate_by$by) | univariate_by$by == "NA")) {
    univariate_by$cor <- "un"
    uvarabccorr <- FALSE
    univariate_by$verbose <- FALSE
  }
  
  
  
  
  if (!(is.na(univariate_by$by) | univariate_by$by == "NA")) {
    if (!is.null(univariate_by$cor)) {
      if (univariate_by$cor == "un")
        uvarsigmacorr <- TRUE
      if (univariate_by$cor == "diagonal")
        uvarsigmacorr <- FALSE
    } else {
      univariate_by$cor <- "un"
      uvarsigmacorr <- TRUE
    }
    if (is.null(univariate_by$verbose))
      univariate_by$verbose <- FALSE
  }
  
  if ((is.na(univariate_by$by) | univariate_by$by == "NA")) {
    univariate_by$cor <- "un"
    uvarsigmacorr <- FALSE
    univariate_by$verbose <- FALSE
  }
  
  
  
  
  if (multivariate$mvar) {
    if (!is.null(multivariate$cor)) {
      if (multivariate$cor == "un")
        mvarccorr <- "WB"
      if (multivariate$cor == "un_s")
        mvarccorr <- "W"
      if (multivariate$cor == "diagonal")
        mvarccorr <- "none"
    } else {
      multivariate$cor <- "un"
      mvarccorr <- "WB"
    }
    if (!is.null(multivariate$rescor)) {
      multivariate$rescor <- multivariate$rescor
    } else {
      multivariate$rescor <- TRUE
    }
    if (is.null(multivariate$verbose))
      multivariate$verbose <- FALSE
  }
  
  if (!multivariate$mvar) {
    multivariate$cor <- "none"
    multivariate$rescor <- FALSE
    multivariate$verbose <- FALSE
  }
  
  
  if (!multivariate$rescor) {
    mvr_prior_rescor <- NULL
  }
  
  
  
  getcovlist <- function(x) {
    if (is.character(x))
      x <- x
    else
      x <- deparse(x)
    x <- gsub("~", "", gsub("\\s", "", x))
    x <- strsplit(x, "+", fixed = T)[[1]]
    if (length(x) == 1)
      x <- NULL
    else
      x <- x[-1]
    return(x)
  }
  
  if (is.null(getcovlist(a_formulasi)))
    a_cov_prior_beta <- NULL
  if (is.null(getcovlist(b_formulasi)))
    b_cov_prior_beta <- NULL
  if (is.null(getcovlist(c_formulasi)))
    c_cov_prior_beta <- NULL
  if (is.null(getcovlist(d_formulasi)))
    d_cov_prior_beta <- NULL
  if (is.null(getcovlist(e_formulasi)))
    e_cov_prior_beta <- NULL
  if (is.null(getcovlist(f_formulasi)))
    f_cov_prior_beta <- NULL
  if (is.null(getcovlist(g_formulasi)))
    g_cov_prior_beta <- NULL
  if (is.null(getcovlist(h_formulasi)))
    h_cov_prior_beta <- NULL
  if (is.null(getcovlist(i_formulasi)))
    i_cov_prior_beta <- NULL
  if (is.null(getcovlist(s_formulasi)))
    s_cov_prior_beta <- NULL
  
  
  if(select_model != 'sitar' & select_model != 'rcs') {
    s_prior_beta <- s_cov_prior_beta <- NULL
  }
  
  
  if (!grepl("a", fixedsi, fixed = T))
    a_prior_beta <- NULL
  if (!grepl("b", fixedsi, fixed = T))
    b_prior_beta <- NULL
  if (!grepl("c", fixedsi, fixed = T))
    c_prior_beta <- NULL
  if (!grepl("d", fixedsi, fixed = T))
    d_prior_beta <- NULL
  if (!grepl("e", fixedsi, fixed = T))
    e_prior_beta <- NULL
  if (!grepl("f", fixedsi, fixed = T))
    f_prior_beta <- NULL
  if (!grepl("g", fixedsi, fixed = T))
    g_prior_beta <- NULL
  if (!grepl("h", fixedsi, fixed = T))
    h_prior_beta <- NULL
  if (!grepl("i", fixedsi, fixed = T))
    i_prior_beta <- NULL
  if (!grepl("s", fixedsi, fixed = T))
    s_prior_beta <- NULL
  
  if (!grepl("a", randomsi, fixed = T))
    a_prior_sd <- NULL
  if (!grepl("b", randomsi, fixed = T))
    b_prior_sd <- NULL
  if (!grepl("c", randomsi, fixed = T))
    c_prior_sd <- NULL
  if (!grepl("d", randomsi, fixed = T))
    d_prior_sd <- NULL
  if (!grepl("e", randomsi, fixed = T))
    e_prior_sd <- NULL
  if (!grepl("f", randomsi, fixed = T))
    f_prior_sd <- NULL
  if (!grepl("g", randomsi, fixed = T))
    g_prior_sd <- NULL
  if (!grepl("h", randomsi, fixed = T))
    h_prior_sd <- NULL
  if (!grepl("i", randomsi, fixed = T))
    i_prior_sd <- NULL
  if (!grepl("s", randomsi, fixed = T))
    s_prior_sd <- NULL
  
  
  if (is.null(getcovlist(a_formula_grsi)))
    a_cov_prior_sd <- NULL
  if (is.null(getcovlist(b_formula_grsi)))
    b_cov_prior_sd <- NULL
  if (is.null(getcovlist(c_formula_grsi)))
    c_cov_prior_sd <- NULL
  if (is.null(getcovlist(d_formula_grsi)))
    d_cov_prior_sd <- NULL
  if (is.null(getcovlist(e_formula_grsi)))
    e_cov_prior_sd <- NULL
  if (is.null(getcovlist(f_formula_grsi)))
    f_cov_prior_sd <- NULL
  if (is.null(getcovlist(g_formula_grsi)))
    g_cov_prior_sd <- NULL
  if (is.null(getcovlist(h_formula_grsi)))
    h_cov_prior_sd <- NULL
  if (is.null(getcovlist(i_formula_grsi)))
    i_cov_prior_sd <- NULL
  if (is.null(getcovlist(s_formula_grsi)))
    s_cov_prior_sd <- NULL
  
  if (!is.null(a_cov_prior_beta))
    ancov <- length(acovcoefnames)
  else
    ancov <- NULL
  if (!is.null(b_cov_prior_beta))
    bncov <- length(bcovcoefnames)
  else
    bncov <- NULL
  if (!is.null(c_cov_prior_beta))
    cncov <- length(ccovcoefnames)
  else
    cncov <- NULL
  if (!is.null(d_cov_prior_beta))
    dncov <- length(dcovcoefnames)
  else
    dncov <- NULL
  if (!is.null(e_cov_prior_beta))
    encov <- length(ecovcoefnames)
  else
    encov <- NULL
  if (!is.null(f_cov_prior_beta))
    fncov <- length(fcovcoefnames)
  else
    fncov <- NULL
  if (!is.null(g_cov_prior_beta))
    gncov <- length(gcovcoefnames)
  else
    gncov <- NULL
  if (!is.null(h_cov_prior_beta))
    hncov <- length(hcovcoefnames)
  else
    hncov <- NULL
  if (!is.null(i_cov_prior_beta))
    incov <- length(icovcoefnames)
  else
    incov <- NULL
  if (!is.null(s_cov_prior_beta))
    sncov <- length(scovcoefnames)
  else
    sncov <- NULL
  
 
  
  if (!is.null(a_cov_prior_sd))
    ancov_gr <- length(acovcoefnames_gr)
  else
    ancov_gr <- NULL
  if (!is.null(b_cov_prior_sd))
    bncov_gr <- length(bcovcoefnames_gr)
  else
    bncov_gr <- NULL
  if (!is.null(c_cov_prior_sd))
    cncov_gr <- length(ccovcoefnames_gr)
  else
    cncov_gr <- NULL
  if (!is.null(d_cov_prior_sd))
    dncov_gr <- length(dcovcoefnames_gr)
  else
    dncov_gr <- NULL
  if (!is.null(e_cov_prior_sd))
    encov_gr <- length(ecovcoefnames_gr)
  else
    encov_gr <- NULL
  if (!is.null(f_cov_prior_sd))
    fncov_gr <- length(fcovcoefnames_gr)
  else
    fncov_gr <- NULL
  if (!is.null(g_cov_prior_sd))
    gncov_gr <- length(gcovcoefnames_gr)
  else
    gncov_gr <- NULL
  if (!is.null(h_cov_prior_sd))
    hncov_gr <- length(hcovcoefnames_gr)
  else
    hncov_gr <- NULL
  if (!is.null(i_cov_prior_sd))
    incov_gr <- length(icovcoefnames_gr)
  else
    incov_gr <- NULL
  if (!is.null(s_cov_prior_sd))
    sncov_gr <- length(scovcoefnames_gr)
  else
    sncov_gr <- NULL
  
  
  
  if (!is.null(dpar_formulasi)) {
    if (!grepl("lf\\(", dpar_formulasi) &
        !grepl("nlf\\(", dpar_formulasi)) {
      dpar_covi_mat_form <- dpar_formulasi
    } else {
      dpar_covi_mat_form <-
        gsub("\\(|)", "", strsplit(dpar_formulasi, "~")[[1]][2])
      dpar_covi_mat_form <- paste0("~", dpar_covi_mat_form)
    }
    dpar_covi_mat_form <- strsplit(dpar_covi_mat_form, ",")[[1]][1]
  }
  
  if (is.null(dpar_formulasi)) {
    dpar_covi_mat_form <- NULL
  }
  
  # rsd_prior_sigma and dpar_prior are mutually exclusive
  if (!is.null(dpar_formulasi)) {
    rsd_prior_sigma <- NULL
  } else {
    dpar_prior_sigma <- dpar_cov_prior_sigma <- NULL
  }
  
  
  
  if(!is.null(sigma_formulasi[1]) & sigma_formulasi != 'NULL') {
    rsd_prior_sigma <- dpar_prior_sigma <- dpar_cov_prior_sigma <- NULL
  }
  
  if(is.null(sigma_formulasi[1]) | sigma_formulasi == 'NULL') {
    sigma_prior_beta <- NULL
  }
  if (is.null(getcovlist(sigma_formulasi))) {
    sigma_cov_prior_beta <- NULL
  }
  
  
  
  if(is.null(sigma_formula_grsi)) {
    sigma_prior_sd <- NULL
  }
  if (is.null(getcovlist(sigma_formula_grsi))) {
    sigma_cov_prior_sd <- NULL
  }
  
  
  
  if (!is.null(sigma_cov_prior_beta))
    sigmancov <- length(sigmacovcoefnames)
  else
    sigmancov <- NULL
  
  if (!is.null(sigma_cov_prior_sd))
    sigmancov_gr <- length(sigmacovcoefnames_gr)
  else
    sigmancov_gr <- NULL
  
  
  
  
  if(!is.null(a_formulasi)) {
    if (grepl("~0", a_formulasi, fixed = T)) {
      a_form_0 <- TRUE
      a_cov_prior_beta <- NULL
    } else {
      a_form_0 <- FALSE
    }
  } else {
    a_form_0 <- FALSE
  }
  
  if(!is.null(b_formulasi)) {
    if (grepl("~0", b_formulasi, fixed = T)) {
      b_form_0 <- TRUE
      b_cov_prior_beta <- NULL
    } else {
      b_form_0 <- FALSE
    }
  } else {
    b_form_0 <- FALSE
  }
  
  
  if(!is.null(c_formulasi)) {
    if (grepl("~0", c_formulasi, fixed = T)) {
      c_form_0 <- TRUE
      c_cov_prior_beta <- NULL
    } else {
      c_form_0 <- FALSE
    }
  } else {
    c_form_0 <- FALSE
  }
  
  
  if(!is.null(d_formulasi)) {
    if (grepl("~0", d_formulasi, fixed = T)) {
      d_form_0 <- TRUE
      d_cov_prior_beta <- NULL
    } else {
      d_form_0 <- FALSE
    }
  } else {
    d_form_0 <- FALSE
  }
  
  
  if(!is.null(e_formulasi)) {
    if (grepl("~0", e_formulasi, fixed = T)) {
      e_form_0 <- TRUE
      e_cov_prior_beta <- NULL
    } else {
      e_form_0 <- FALSE
    }
  } else {
    e_prior_beta <- e_cov_prior_beta <- NULL
    e_prior_sd   <- e_cov_prior_sd <- NULL
    e_form_0 <- FALSE
  }
  
  
  if(!is.null(f_formulasi)) {
    if (grepl("~0", f_formulasi, fixed = T)) {
      f_form_0 <- TRUE
      f_cov_prior_beta <- NULL
    } else {
      f_form_0 <- FALSE
    }
  } else {
    f_form_0 <- FALSE
  }
  
  if(!is.null(g_formulasi)) {
    if (grepl("~0", g_formulasi, fixed = T)) {
      g_form_0 <- TRUE
      g_cov_prior_beta <- NULL
    } else {
      g_form_0 <- FALSE
    }
  } else {
    g_form_0 <- FALSE
  }
  
  
  if(!is.null(h_formulasi)) {
    if (grepl("~0", h_formulasi, fixed = T)) {
      h_form_0 <- TRUE
      h_cov_prior_beta <- NULL
    } else {
      h_form_0 <- FALSE
    }
  } else {
    h_form_0 <- FALSE
  }
  
  
  if(!is.null(i_formulasi)) {
    if (grepl("~0", i_formulasi, fixed = T)) {
      i_form_0 <- TRUE
      i_cov_prior_beta <- NULL
    } else {
      i_form_0 <- FALSE
    }
  } else {
    i_form_0 <- FALSE
  }
  
  
  if(!is.null(s_formulasi)) {
    if (grepl("~0", s_formulasi, fixed = T)) {
      s_form_0 <- TRUE
      s_cov_prior_beta <- NULL
    } else {
      s_form_0 <- FALSE
    }
  } else {
    s_form_0 <- FALSE
  }
  
  
  
  
  if(!is.null(a_formula_grsi)) {
    if (grepl("~0", a_formula_grsi, fixed = T)) {
      a_form_0_gr <- TRUE
      a_cov_prior_sd <- NULL
    } else {
      a_form_0_gr <- FALSE
    }
  } else {
    a_form_0_gr <- FALSE
  }
  
  
  if(!is.null(b_formula_grsi)) {
    if (grepl("~0", b_formula_grsi, fixed = T)) {
      b_form_0_gr <- TRUE
      b_cov_prior_sd <- NULL
    } else {
      b_form_0_gr <- FALSE
    }
  } else {
    b_form_0_gr <- FALSE
  }
  
  
  if(!is.null(c_formula_grsi)) {
    if (grepl("~0", c_formula_grsi, fixed = T)) {
      c_form_0_gr <- TRUE
      c_cov_prior_sd <- NULL
    } else {
      c_form_0_gr <- FALSE
    }
  } else {
    c_form_0_gr <- FALSE
  }
  
  
  
  if(!is.null(d_formula_grsi)) {
    if (grepl("~0", d_formula_grsi, fixed = T)) {
      d_form_0_gr <- TRUE
      d_cov_prior_sd <- NULL
    } else {
      d_form_0_gr <- FALSE
    }
  } else {
    d_form_0_gr <- FALSE
  }
  
  
  if(!is.null(e_formula_grsi)) {
    if (grepl("~0", e_formula_grsi, fixed = T)) {
      e_form_0_gr <- TRUE
      e_cov_prior_sd <- NULL
    } else {
      e_form_0_gr <- FALSE
    }
  } else {
    e_form_0_gr <- FALSE
  }
  
  
  if(!is.null(f_formula_grsi)) {
    if (grepl("~0", f_formula_grsi, fixed = T)) {
      f_form_0_gr <- TRUE
      f_cov_prior_sd <- NULL
    } else {
      f_form_0_gr <- FALSE
    }
  } else {
    f_form_0_gr <- FALSE
  }
  
  
  
  if(!is.null(g_formula_grsi)) {
    if (grepl("~0", g_formula_grsi, fixed = T)) {
      g_form_0_gr <- TRUE
      g_cov_prior_sd <- NULL
    } else {
      g_form_0_gr <- FALSE
    }
  } else {
    g_form_0_gr <- FALSE
  }
  
  
  if(!is.null(h_formula_grsi)) {
    if (grepl("~0", h_formula_grsi, fixed = T)) {
      h_form_0_gr <- TRUE
      h_cov_prior_sd <- NULL
    } else {
      h_form_0_gr <- FALSE
    }
  } else {
    h_form_0_gr <- FALSE
  }
  
  
  if(!is.null(i_formula_grsi)) {
    if (grepl("~0", i_formula_grsi, fixed = T)) {
      i_form_0_gr <- TRUE
      i_cov_prior_sd <- NULL
    } else {
      i_form_0_gr <- FALSE
    }
  } else {
    i_form_0_gr <- FALSE
  }
  
  
  if(!is.null(s_formula_grsi)) {
    if (grepl("~0", s_formula_grsi, fixed = T)) {
      s_form_0_gr <- TRUE
      s_cov_prior_sd <- NULL
    } else {
      s_form_0_gr <- FALSE
    }
  } else {
    s_form_0_gr <- FALSE
  }
  
  
  
  if (!is.null(dpar_formulasi)) {
    if (grepl("^~1$", dpar_covi_mat_form, fixed = F)) {
      dpar_intercept_only <- TRUE
    } else {
      dpar_intercept_only <- FALSE
    }
    if (!is.null(dpar_covi_mat_form) &
        grepl("~0", dpar_covi_mat_form, fixed = T)) {
      dpar_form_0 <- TRUE
      dpar_cov_prior_sigma <- NULL
    } else {
      dpar_form_0 <- FALSE
      if (grepl("^~1$", dpar_covi_mat_form, fixed = F)) {
        dpar_cov_prior_sigma <- NULL
      }
    }
  }
  
  if (is.null(dpar_formulasi)) {
    dpar_form_0 <- FALSE
    dpar_cov_prior_sigma <- NULL
  }
  
  
  if (grepl("~0", sigma_formulasi, fixed = T)) {
    sigma_form_0 <- TRUE
    sigma_cov_prior_beta <- NULL
  } else {
    sigma_form_0 <- FALSE
  }
  
  if(is.null(sigma_formulasi)) sigma_form_0 <- FALSE
  
  
  if(!is.null(sigma_formula_grsi)) {
    if (grepl("~0", sigma_formula_grsi, fixed = T)) {
      sigma_form_0_gr <- TRUE
      sigma_cov_prior_sd <- NULL
    } else {
      sigma_form_0_gr <- FALSE
    }
  }
  
  if(is.null(sigma_formula_grsi)) sigma_form_0_gr <- FALSE
  
  
  
  if ((is.na(univariate_by$by) |
       univariate_by$by == "NA") & !mvar & !abccorr) {
    gr_prior_cor <- NULL
  }
  
  if (is.null(randomsi[[1]]))
    gr_prior_cor <- NULL
  if (nabcrei == 1)
    gr_prior_cor <- NULL
  
  
  if (!(is.na(univariate_by$by) | univariate_by$by == "NA")) {
    if (uvarabccorr) {
      gr_prior_cor <- gr_prior_cor
    } else {
      gr_prior_cor <- NULL
    }
  }
  
  if (mvar && mvarccorr == "none") {
    gr_prior_cor <- NULL
  }
  
  
  
  if ((is.na(univariate_by$by) |
       univariate_by$by == "NA") & !mvar & !sigmacorr) {
    sigma_prior_cor <- NULL
  }
  
  if(is.null(sigmancov_gr)) {
    sigma_prior_cor <- NULL
  }
  
  
  
  
  # Note that currently brms does not allow setting separate cor prior for sigma
  # So, either set sigma_prior_cor <- NULL for all or else set group = '
  if (!(is.na(univariate_by$by) | univariate_by$by == "NA")) {
    if (uvarsigmacorr) {
      sigma_prior_cor <- sigma_prior_cor
    } else {
      sigma_prior_cor <- NULL
    }
  }
  
  if (mvar && mvarccorr == "none") {
    sigma_prior_cor <- NULL
  }
  
  
  # Evaluate prior arguments
  eval_prior_args <- function(x, ...) {
    x_org <- x
    if (grepl("_beta", x))
      class <- 'b'
    if (grepl("_sd", x))
      class <- 'sd'
    if (grepl("rsd_", x) & grepl("_sigma", x))
      class <- 'sigma'
    if (grepl("_cor$", x))
      class <- 'cor'
    if (grepl("dpar", x) &
        grepl("_sigma", x))
      class <- '' # no class sigma if sigma ~ .
    
    if (grepl("_rescor$", x))
      class <- 'rescor'
    
    nlpar <- ""
    if (grepl("a_", x))
      nlpar <- 'a'
    if (grepl("b_", x))
      nlpar <- 'b'
    if (grepl("c_", x))
      nlpar <- 'c'
    if (grepl("d_", x))
      nlpar <- 'd'
    if (grepl("e_", x))
      nlpar <- 'e'
    if (grepl("f_", x))
      nlpar <- 'f'
    if (grepl("g_", x))
      nlpar <- 'g'
    if (grepl("h_", x))
      nlpar <- 'h'
    if (grepl("i_", x))
      nlpar <- 'i'
    if (grepl("s_", x))
      nlpar <- 's'
    dpar <- ""
    if (grepl("dpar", x) &
        grepl("_sigma", x) & !grepl("rsd_", x))
      dpar <- "dpar"
    
    sigma_dpar <- ""
    if (grepl("sigma", x) &
        grepl("sigma_", x) & !grepl("rsd_", x) & !grepl("_sigma", x) & 
        !grepl("dpar", x)) {
      sigma_dpar <- "sigma"
      nlpar <- ""
      cov_nlpar <- ""
      # class <- 'b'
    }
    
    
    
    cov_dpar <- ""
    if (grepl("dpar_cov", x))
      dpar <- paste0(dpar, "_cov")
    
    dpar_cov <- dpar
    
    
    cov_sigma_dpar <- ""
    if (grepl("sigma_cov", x)) {
      sigma_dpar <- paste0('sigma', "")
      cov_sigma_dpar <- paste0('sigma', "_cov")
      nlpar <- ""
      cov_nlpar <- ""
      # class <- 'b'
    }
    
    
    
    cov_nlpar <- ""
    if (grepl("a_cov", x))
      cov_nlpar <- 'a'
    if (grepl("b_cov", x))
      cov_nlpar <- 'b'
    if (grepl("c_cov", x))
      cov_nlpar <- 'c'
    if (grepl("d_cov", x))
      cov_nlpar <- 'd'
    if (grepl("e_cov", x))
      cov_nlpar <- 'e'
    if (grepl("f_cov", x))
      cov_nlpar <- 'f'
    if (grepl("g_cov", x))
      cov_nlpar <- 'g'
    if (grepl("h_cov", x))
      cov_nlpar <- 'h'
    if (grepl("i_cov", x))
      cov_nlpar <- 'i'
    if (grepl("s_cov", x))
      cov_nlpar <- 's'
    
    if (!is.null(dpar_prior_sigma) |
        !is.null(dpar_cov_prior_sigma)) {
      dparncov <- length(dparcovcoefnames) - 1
    } else {
      dparncov <- NULL
    }
    
    
    
    if(sigma_dpar == 'sigma' | cov_sigma_dpar == 'sigma_cov') {
      group <- sigma_arg_groupvar
    } else {
      group <- group_arg_groupvar
    }
    
    
    get_acorclass <- function(autocor_formi2) {
      if(grepl("arma\\(", autocor_formi2, fixed = F)) {
        acorclass <- 'arma'
      } else if(grepl("ar\\(", autocor_formi2, fixed = F)) {
        acorclass <- 'ar'
      } else if(grepl("ma\\(", autocor_formi2, fixed = F)) {
        acorclass <- 'ma'
      } else if(grepl("cosy\\(", autocor_formi2, fixed = F)) {
        acorclass <- 'cosy'
      } else if(grepl("car\\(", autocor_formi2, fixed = F)) {
        acorclass <- 'car'
      } else if(grepl("lagsar\\(", autocor_formi2, fixed = F)) {
        acorclass <- 'lagsar'
      } else if(grepl("errorsar\\(", autocor_formi2, fixed = F)) {
        acorclass <- 'errorsar'
      } else if(grepl("unstr\\(", autocor_formi2, fixed = F)) {
        acorclass <- 'Lcortime'
      }
      acorclass
    }
    
    
    setautocorr <- FALSE
    if (grepl("autocor_", x) & grepl("_acor", x)) {
      setautocorr <- TRUE
      acorclass <- get_acorclass(autocor_formi)
      if (acorclass == "arma")
        class <- 'arma'
      if (acorclass == "ar")
        class <- 'ar'
      if (acorclass == "ma")
        class <- 'ma'
      if (acorclass == "cosy")
        class <- 'cosy'
      if (acorclass == "car")
        class <- 'car'
      if (acorclass == "lagsar")
        class <- 'lagsar'
      if (acorclass == "errorsar")
        class <- 'errorsar'
      if (acorclass == "Lcortime")
        class <- 'Lcortime'
    }
    
    
    if(setautocorr) {
      if(class == 'Lcortime') {
        acorclassname <- 'unstr' 
      } else {
        acorclassname <- acorclass
      }
      
      allowed_acor_classes <- c('ar', 'ma', 'arma', 'unstr')
      
      if(!acorclassname %in% allowed_acor_classes) {
        stop("Allowed autocorrelation classes are ", 
             paste(allowed_acor_classes, collapse = ", ")) 
      }
    }
    
    
    
    if (class == "b" & nlpar == 'a') {
      if (a_form_0) {
        nrep_of_parms <- length(acovcoefnames)
      } else {
        if (!grepl("a_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("a_cov", x)) {
          nrep_of_parms <- length(acovcoefnames) - 1
        }
      }
    } else if (class == "b" & nlpar == 'b') {
      if (b_form_0) {
        nrep_of_parms <- length(bcovcoefnames)
      } else {
        if (!grepl("b_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("b_cov", x)) {
          nrep_of_parms <- length(bcovcoefnames) - 1
        }
      }
    } else if (class == "b" & nlpar == 'c') {
      if (c_form_0) {
        nrep_of_parms <- length(ccovcoefnames)
      } else {
        if (!grepl("c_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("c_cov", x)) {
          nrep_of_parms <- length(ccovcoefnames) - 1
        }
      }
    } else if (class == "b" & nlpar == 'd') {
      if (d_form_0) {
        nrep_of_parms <- length(dcovcoefnames)
      } else {
        if (!grepl("d_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("d_cov", x)) {
          nrep_of_parms <- length(dcovcoefnames) - 1
        }
      }
    } else if (class == "b" & nlpar == 'e') {
      if (e_form_0) {
        nrep_of_parms <- length(ecovcoefnames)
      } else {
        if (!grepl("e_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("e_cov", x)) {
          nrep_of_parms <- length(ecovcoefnames) - 1
        }
      }
    } else if (class == "b" & nlpar == 'f') {
      if (f_form_0) {
        nrep_of_parms <- length(fcovcoefnames)
      } else {
        if (!grepl("f_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("f_cov", x)) {
          nrep_of_parms <- length(fcovcoefnames) - 1
        }
      }
    } else if (class == "b" & nlpar == 'g') {
      if (g_form_0) {
        nrep_of_parms <- length(gcovcoefnames)
      } else {
        if (!grepl("g_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("g_cov", x)) {
          nrep_of_parms <- length(gcovcoefnames) - 1
        }
      }
    } else if (class == "b" & nlpar == 'h') {
      if (h_form_0) {
        nrep_of_parms <- length(hcovcoefnames)
      } else {
        if (!grepl("h_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("h_cov", x)) {
          nrep_of_parms <- length(hcovcoefnames) - 1
        }
      }
    } else if (class == "b" & nlpar == 'i') {
      if (i_form_0) {
        nrep_of_parms <- length(icovcoefnames)
      } else {
        if (!grepl("i_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("i_cov", x)) {
          nrep_of_parms <- length(icovcoefnames) - 1
        }
      }
    } else if (class == "b" & nlpar == 's') {
      if (s_form_0) {
        nrep_of_parms <- df * length(scovcoefnames)
      } else {
        if (!grepl("s_cov", x)) {
          nrep_of_parms <- df
        } else if (grepl("s_cov", x)) {
          nrep_of_parms <- df * (length(scovcoefnames) - 1)
        }
      }
    } else if (class == "sd" & nlpar == 'a') {
      if (a_form_0_gr) {
        nrep_of_parms <- length(acovcoefnames_gr)
      } else {
        if (!grepl("a_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("a_cov", x)) {
          nrep_of_parms <- length(acovcoefnames_gr) - 1
        }
      }
    } else if (class == "sd" & nlpar == 'b') {
      if (b_form_0_gr) {
        nrep_of_parms <- length(bcovcoefnames_gr)
      } else {
        if (!grepl("b_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("b_cov", x)) {
          nrep_of_parms <- length(bcovcoefnames_gr) - 1
        }
      }
    } else if (class == "sd" & nlpar == 'c') {
      if (c_form_0_gr) {
        nrep_of_parms <- length(ccovcoefnames_gr)
      } else {
        if (!grepl("c_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("c_cov", x)) {
          nrep_of_parms <- length(ccovcoefnames_gr) - 1
        }
      }
    } else if (class == "sd" & nlpar == 'd') {
      if (d_form_0_gr) {
        nrep_of_parms <- length(dcovcoefnames_gr)
      } else {
        if (!grepl("d_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("d_cov", x)) {
          nrep_of_parms <- length(dcovcoefnames_gr) - 1
        }
      }
    } else if (class == "sd" & nlpar == 'e') {
      if (e_form_0_gr) {
        nrep_of_parms <- length(ecovcoefnames_gr)
      } else {
        if (!grepl("e_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("e_cov", x)) {
          nrep_of_parms <- length(ecovcoefnames_gr) - 1
        }
      }
    } else if (class == "sd" & nlpar == 'f') {
      if (f_form_0_gr) {
        nrep_of_parms <- length(fcovcoefnames_gr)
      } else {
        if (!grepl("f_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("f_cov", x)) {
          nrep_of_parms <- length(fcovcoefnames_gr) - 1
        }
      }
    } else if (class == "sd" & nlpar == 'g') {
      if (g_form_0_gr) {
        nrep_of_parms <- length(gcovcoefnames_gr)
      } else {
        if (!grepl("g_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("g_cov", x)) {
          nrep_of_parms <- length(gcovcoefnames_gr) - 1
        }
      }
    } else if (class == "sd" & nlpar == 'h') {
      if (h_form_0_gr) {
        nrep_of_parms <- length(hcovcoefnames_gr)
      } else {
        if (!grepl("h_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("h_cov", x)) {
          nrep_of_parms <- length(hcovcoefnames_gr) - 1
        }
      }
    } else if (class == "sd" & nlpar == 'i') {
      if (i_form_0_gr) {
        nrep_of_parms <- length(icovcoefnames_gr)
      } else {
        if (!grepl("i_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("i_cov", x)) {
          nrep_of_parms <- length(icovcoefnames_gr) - 1
        }
      }
    } else if (class == "sd" & nlpar == 's') {
      if (s_form_0_gr) {
        nrep_of_parms <- df * length(scovcoefnames_gr)
      } else {
        if (!grepl("s_cov", x)) {
          nrep_of_parms <- df
        } else if (grepl("s_cov", x)) {
          nrep_of_parms <- df * (length(scovcoefnames_gr) - 1)
        }
      }
    } else if (class == "sigma" &
               (class != 'b' |
                class != 'cor') & is.null(dparncov)) {
      nrep_of_parms <- 1
    } else if (class == "" &
               (class != 'b' |
                class != 'cor') & !is.null(dparncov)) {
      if (dpar_form_0) {
        nrep_of_parms <- length(dparcovcoefnames)
      } else {
        if (!grepl("dpar_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("dpar_cov", x)) {
          nrep_of_parms <- length(dparcovcoefnames) - 1
        }
      }
    } else if (setautocorr &
               class == "" &
               (class != 'b' |
                class != 'cor') & is.null(dparncov)) {
      nrep_of_parms <- 1
    } else if (class == "cor" &
               (class != 'b' |
                class != 'sigma') & is.null(dparncov)) {
      nrep_of_parms <- 1
    } else {
      nrep_of_parms <- 1
    }
    
    
    if(setautocorr) {
      tempzxxx <- autocor_formi 
      tempzxxx <- gsub("[[:space:]]", "", tempzxxx)
      if(grepl("p=", tempzxxx, fixed = F)) {
        acor_dim_p <- sub(",.*", "", sub(".*p=", "", tempzxxx) )  
        acor_dim_p <- gsub(")" , "", acor_dim_p, fixed = T)
      } else if(!grepl("p=", tempzxxx, fixed = F)) {
        acor_dim_p <- '1'
      }
      if(grepl("q=", tempzxxx, fixed = F)) {
        acor_dim_q <- sub(",.*", "", sub(".*q=", "", tempzxxx) )  
        acor_dim_q <- gsub(")" , "", acor_dim_q, fixed = T) 
      } else if(!grepl("q=", tempzxxx, fixed = F)) {
        acor_dim_q <- '1'
      }
      
      if(acorclass == 'ar') {
        if(!grepl("p=", tempzxxx, fixed = F)) 
          stop("Please specify arguments by names e.g., p=1")
      } else if(acorclass == 'ma') {
        if(!grepl("q=", tempzxxx, fixed = F))
          stop("Please specify arguments by names e.g., q=1")
      } else if(acorclass == 'arma') {
        if(!grepl("p=", tempzxxx, fixed = F)) 
          stop("Please specify arguments by names e.g., p=1")
        if(!grepl("q=", tempzxxx, fixed = F))
          stop("Please specify arguments by names e.g., q=1")
      }
      
      nrep_of_parms_p <- eval(parse(text = acor_dim_p))
      nrep_of_parms_q <- eval(parse(text = acor_dim_q))
    }
    
    if(!setautocorr) {
      nrep_of_parms_p <- nrep_of_parms_q <- 1
    }
    
    
    
    if (class == "b" & sigma_dpar == 'sigma' & 
        !grepl("rsd_", x) & !grepl("_sigma", x) & !grepl("dpar", x)) {
      if (sigma_form_0) {
        nrep_of_parms <- length(sigmacovcoefnames)
      } else {
        if (!grepl("sigma_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("sigma_cov", x)) {
          nrep_of_parms <- length(sigmacovcoefnames) - 1
        }
      }
    } 
    
    
    
    if (class == "b" & sigma_dpar == 'sigma') {
      if (sigma_form_0) {
        nrep_of_parms <- length(sigmacovcoefnames)
      } else {
        if (!grepl("sigma_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("sigma_cov", x)) {
          nrep_of_parms <- length(sigmacovcoefnames) - 1
        }
      }  
    }
    
    
    if (class == "sd" & sigma_dpar == 'sigma' & 
        !grepl("rsd_", x) & !grepl("_sigma", x) & !grepl("dpar", x)) {
      if (sigma_form_0_gr) {
        nrep_of_parms <- length(sigmacovcoefnames_gr)
      } else {
        if (!grepl("sigma_cov", x)) {
          nrep_of_parms <- 1
        } else if (grepl("sigma_cov", x)) {
          nrep_of_parms <- length(sigmacovcoefnames_gr) - 1
        }
      }
    }
    
    
  
    
    get_priors_parms <- function(x,
                                 prior_data,
                                 prior_data_internal,
                                 resp,
                                 nlpar,
                                 dpar,
                                 sigma_dpar,
                                 class,
                                 acorclass,
                                 get_priors_parms_args) {
      if (!is.null(get_priors_parms_args)) {
        eout <- list2env(get_priors_parms_args)
        for (eoutii in names(eout)) {
          assign(eoutii, eout[[eoutii]])
        }
      }
      
      x <- gsub("\"", "", gsub("\\s", "", x))
      
      if (resp == "") {
        resp_ <- ""
      } else if (resp != "") {
        resp_ <- paste0("_", resp)
      }
      
      
      prior_argument <- x
      zz <- prior_str_arg <- eval(parse(text = x))
      zz <- strsplit(zz, "\\(")[[1]]
      dist <- zz[1]
      
      
      #################
      
      list_names <-
        c(
          'prior_str_arg',
          'dist',
          'resp',
          'resp_',
          'nlpar',
          'dpar',
          'class',
          'cov_nlpar',
          'cov_dpar',
          'ancov',
          'bncov',
          'cncov',
          'dncov',
          'encov',
          'fncov',
          'gncov',
          'hncov',
          'incov',
          'sncov',
          'ancov_gr',
          'bncov_gr',
          'cncov_gr',
          'dncov_gr',
          'encov_gr',
          'fncov_gr',
          'gncov_gr',
          'hncov_gr',
          'incov_gr',
          'sncov_gr',
          'dparncov',
          'nabci',
          'nabcrei',
          'fixedsi',
          'randomsi',
          'df',
          'setautocorr',
          'nrep_of_parms',
          'nrep_of_parms_p',
          'nrep_of_parms_q',
          'N_J_all',
          'ii',
          'nys',
          'a_form_0',
          'b_form_0',
          'c_form_0',
          'd_form_0',
          'e_form_0',
          'f_form_0',
          'g_form_0',
          'h_form_0',
          'i_form_0',
          's_form_0',
          'a_form_0_gr',
          'b_form_0_gr',
          'c_form_0_gr',
          'd_form_0_gr',
          'e_form_0_gr',
          'f_form_0_gr',
          'g_form_0_gr',
          'h_form_0_gr',
          'i_form_0_gr',
          's_form_0_gr',
          'sigma_form_0',
          'sigma_form_0_gr',
          "sigma_dpar",
          "cov_sigma_dpar",
          'sigmancov',
          'sigmancov_gr',
          'sigma_dpar',
          'sigma_group_arg',
          'group_arg_groupvar',
          'group',
          'dpar_form_0',
          'dpar_covi_mat_form',
          'dpar_formulasi',
          'univariate_by',
          'multivariate',
          'group_arg',
          'setautocorr',
          'acorclass',
          'initsi',
          'normalize',
          'seed',
          'cortimeNlags_var',
          'cortimeNlags',
          'verbose'
        )
      
      
      prior_internal_args <- mget(list_names)
      
      # Set to NULL to appropriatly match priors
      for (ip in names(init_arguments)) {
        init_ip <- ip
        if (grepl("_init_", init_ip) &
            !grepl("r_init_z", init_ip)) {
          prior_ip <- gsub("_init_", "_prior_", init_ip)
          xx   <- deparse(prior_ip)
          if (is.null(ept(prior_ip))) init_arguments[[ip]] <- 'NULL'
        }
      }
      
      if (ept(nabcrei) == 0) init_arguments[['r_init_z']] <- 'NULL'
      
      out_p_str <- prepare_priors(
        prior_argument,
        prior_data,
        prior_data_internal,
        prior_internal_args,
        init_arguments,
        init_data,
        init_data_internal,
        init_args_internal
      )
      
      
      stanvars_data_in <- out_p_str$stanvars_data
      prior_str_arg    <- out_p_str$prior_str_arg
      lowerbound       <- out_p_str$lowerbound
      upperbound       <- out_p_str$upperbound
      initial_in       <- out_p_str$initial_out
      
      return(
        list(
          dist = dist,
          lowerbound = lowerbound,
          upperbound = upperbound,
          define_ = prior_str_arg,
          stanvars_data_in = stanvars_data_in,
          initial_in = initial_in
        )
      )
    }
    
    
    if (resp != "") {
      for (i in names(prior_data_internal)) {
        names(prior_data_internal)[names(prior_data_internal) == i] <-
          paste0(i, "_", resp)
      }
    }
    
    x_name <- deparse(substitute(x_org))
    
    
    get_priors_parms_args <- list(
      df = df,
      nys = nys,
      univariate_by = univariate_by,
      multivariate = multivariate,
      group_arg = group_arg,
      fixedsi = fixedsi,
      randomsi = randomsi,
      nabci = nabci,
      nabcrei = nabcrei,
      ii = ii,
      N_J_all = N_J_all,
      cov_nlpar = cov_nlpar,
      cov_dpar = cov_dpar,
      nrep_of_parms = nrep_of_parms,
      nrep_of_parms_p = nrep_of_parms_p,
      nrep_of_parms_q = nrep_of_parms_q,
      ancov = ancov,
      bncov = bncov,
      cncov = cncov,
      dncov = dncov,
      encov = encov,
      fncov = fncov,
      gncov = gncov,
      hncov = hncov,
      incov = incov,
      sncov = sncov,
      ancov_gr = ancov_gr,
      bncov_gr = bncov_gr,
      cncov_gr = cncov_gr,
      dncov_gr = dncov_gr,
      encov_gr = encov_gr,
      fncov_gr = fncov_gr,
      gncov_gr = gncov_gr,
      hncov_gr = hncov_gr,
      incov_gr = incov_gr,
      sncov_gr = sncov_gr,
      dparncov = dparncov,
      a_form_0 = a_form_0,
      b_form_0 = b_form_0,
      c_form_0 = c_form_0,
      d_form_0 = d_form_0,
      e_form_0 = e_form_0,
      f_form_0 = f_form_0,
      g_form_0 = g_form_0,
      h_form_0 = h_form_0,
      i_form_0 = i_form_0,
      s_form_0 = s_form_0,
      a_form_0_gr = a_form_0_gr,
      b_form_0_gr = b_form_0_gr,
      c_form_0_gr = c_form_0_gr,
      d_form_0_gr = d_form_0_gr,
      e_form_0_gr = e_form_0_gr,
      f_form_0_gr = f_form_0_gr,
      g_form_0_gr = g_form_0_gr,
      h_form_0_gr = h_form_0_gr,
      i_form_0_gr = i_form_0_gr,
      s_form_0_gr = s_form_0_gr,
      sigma_form_0 = sigma_form_0,
      sigma_form_0_gr = sigma_form_0_gr,
      cov_sigma_dpar = cov_sigma_dpar,
      sigmancov = sigmancov,
      sigmancov_gr = sigmancov_gr,
      sigma_group_arg = sigma_group_arg,
      group_arg_groupvar = group_arg_groupvar,
      group = group,
      
      dpar_form_0 = dpar_form_0,
      dpar_covi_mat_form = dpar_covi_mat_form,
      dpar_formulasi = dpar_formulasi,
      setautocorr = setautocorr,
      initsi = initsi,
      init_arguments = init_arguments,
      init_data = init_data,
      init_data_internal = init_data_internal,
      init_args_internal = init_args_internal,
      normalize = normalize,
      seed = seed,
      cortimeNlags_var = cortimeNlags_var,
      cortimeNlags = cortimeNlags,
      verbose = verbose
    )
    
    
    if (setautocorr) {
      if (acorclass == 'arma')
        acorclassclasses <- c("ar", "ma")
      if (acorclass == 'ar')
        acorclassclasses <- c("ar")
      if (acorclass == 'ma')
        acorclassclasses <- c("ma")
      
      
      if (acorclass == 'Lcortime')
        acorclassclasses <- c("Lcortime")
      
      priors_arma_c_define <- list()
      for (acorclassi in acorclassclasses) {
        priors_parms <- get_priors_parms(
          x_name,
          prior_data = prior_data,
          prior_data_internal = prior_data_internal,
          resp = resp,
          nlpar = nlpar,
          dpar = dpar,
          sigma_dpar = sigma_dpar,
          class = acorclassi,
          acorclass = acorclass,
          get_priors_parms_args = get_priors_parms_args
        )
        
        priors_arma_c_define[[acorclassi]] <- priors_parms
      }
    } else {
      priors_parms <- get_priors_parms(
        x_name,
        prior_data = prior_data,
        prior_data_internal = prior_data_internal,
        resp = resp,
        nlpar = nlpar,
        dpar = dpar,
        sigma_dpar = sigma_dpar,
        class = class,
        get_priors_parms_args = get_priors_parms_args
      )
    }
    
    
    define_ <- priors_parms$define_
    
    dist <- priors_parms$dist
    
    lowerbound <- priors_parms$lowerbound
    upperbound <- priors_parms$upperbound
    
    stanvars_data_in <- priors_parms$stanvars_data_in
    initial_in       <- priors_parms$initial_in
    
    
    if (class == 'b') {
      # Need to remove lb and ub if specifying coef, otherwise
      # Error: Argument 'coef' may not be specified when using boundaries.
      if(nlpar != '') {
        mnf <- paste0(nlpar, "_form_0")
        mnc <- paste0("cov_nlpar")
      }
      if(sigma_dpar == 'sigma' | cov_sigma_dpar == 'sigma_cov') {
        mnf <- paste0('sigma', "_form_0")
        mnc <- paste0("cov_sigma_dpar")
      }
      
      
      
      if (all(is.na(lowerbound)) |
          all(is.na(upperbound))) {
        if (nlpar == 'a')
          coef <- acovcoefnames
        if (nlpar == 'b')
          coef <- bcovcoefnames
        if (nlpar == 'c')
          coef <- ccovcoefnames
        if (nlpar == 'd')
          coef <- dcovcoefnames
        if (nlpar == 'e')
          coef <- ecovcoefnames
        if (nlpar == 'f')
          coef <- fcovcoefnames
        if (nlpar == 'g')
          coef <- gcovcoefnames
        if (nlpar == 'h')
          coef <- hcovcoefnames
        if (nlpar == 'i')
          coef <- icovcoefnames
        if (nlpar == 's') {
          coef <- scovcoefnames
          if (!s_form_0 & !is.null(sncov))
            coef <- coef[1]
        }
        
        
        # 'brms' does not allow Intercept as coef name for dpar sigma with ~1
        #  But this only when covaritae missing 
        if (sigma_dpar == 'sigma') {
          dpar <- sigma_dpar
          if(ept(mnf)) {
            coef <- sigmacovcoefnames
          }
          if(!ept(mnf)) {
            if (nlpar == '' & sigma_dpar != '' & 
                length(sigmacovcoefnames) == 1 &
                sigmacovcoefnames[1] == "Intercept" ) {
              coef <- ""
              class <- sigmacovcoefnames
            }
            if (nlpar == '' & sigma_dpar != '' & grepl("+", 
                                                       sigma_formulasi, 
                                                       fixed = T)
            ) {
              coef <- ""
              class <- 'Intercept'
            }
          }

          
          
        } else if (!all(is.na(lowerbound)) & !all(is.na(upperbound))) {
          if (nlpar == 'a')
            coef <- rep("", length(acovcoefnames))
          if (nlpar == 'b')
            coef <- rep("", length(bcovcoefnames))
          if (nlpar == 'c')
            coef <- rep("", length(ccovcoefnames))
          if (nlpar == 'd')
            coef <- rep("", length(dcovcoefnames))
          if (nlpar == 'e')
            coef <- rep("", length(ecovcoefnames))
          if (nlpar == 'f')
            coef <- rep("", length(fcovcoefnames))
          if (nlpar == 'g')
            coef <- rep("", length(gcovcoefnames))
          if (nlpar == 'h')
            coef <- rep("", length(hcovcoefnames))
          if (nlpar == 'i')
            coef <- rep("", length(icovcoefnames))
          if (nlpar == 's') {
            coef <- rep("", length(scovcoefnames))
            if (!s_form_0 & !is.null(sncov))
              coef <- coef[1]
          }
        }
      }
      
      
      
      
      # nlpar a b c d e f g h i - betas also sigma
      if (ept(mnf) & cov_nlpar == "" & cov_sigma_dpar == "") {
        if (!any(is.na(lowerbound)) | !any(is.na(upperbound))) {
          define_ <- unique(define_)
          lowerbound <- unique(lowerbound)
          upperbound <- unique(upperbound)
          setcoef <- ""
        } else {
          setcoef <- coef
        }
        
        
        
        priors_ <-
          brms::prior_string(
            define_,
            class = class,
            nlpar = nlpar,
            coef = setcoef,
            resp = resp,
            dpar = dpar,
            lb = lowerbound,
            ub = upperbound
          )
      }
      
      if (!ept(mnf) & cov_nlpar == "" & cov_sigma_dpar == "") {
        if (!any(is.na(lowerbound)) | !any(is.na(upperbound))) {
          define_ <- unique(define_)
          lowerbound <- unique(lowerbound)
          upperbound <- unique(upperbound)
          setcoef <- ""
        } else {
          if (ept(mnc) == "") {
            setcoef <- coef[1]
          } else {
            setcoef <- coef
          }
        }
        priors_ <-
          brms::prior_string(
            define_,
            class = class,
            nlpar = nlpar,
            coef = setcoef,
            resp = resp,
            dpar = dpar,
            lb = lowerbound,
            ub = upperbound
          )
      }
      
      
      
      
      # nlpar s - betas
      if (nlpar == 's' & cov_nlpar == "") {
        nlpar <- paste0(nlpar, 1:df)
        if (grepl("~1", s_formulasi, fixed = T)) {
          if (all(coef == "")) {
            priors_ <- brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              resp = resp,
              dpar = dpar,
              lb = lowerbound,
              ub = upperbound
            )
          } else {
            priors_ <-   brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
          }
        }
        if (grepl("~0", s_formulasi, fixed = T)) {
          nlpar <- rep(nlpar ,
                       times = 1,
                       each = length(scovcoefnames))
          if (all(coef == "")) {
            priors_ <- brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              resp = resp,
              dpar = dpar,
              lb = lowerbound,
              ub = upperbound
            )
          } else {
            priors_ <-   brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
          }
        }
      }
      
      
      
      # nlpar cov a - betas
      if (!a_form_0) {
        if (class == 'b' & grepl("a_cov", x) & !is.null(a_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- acovcoefnames
          } else {
            coef <- acovcoefnames[2:length(acovcoefnames)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
          
        }
      }
      
      # nlpar cov b - betas
      if (!b_form_0) {
        if (class == 'b' & grepl("b_cov", x) & !is.null(b_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- bcovcoefnames
          } else {
            coef <- bcovcoefnames[2:length(bcovcoefnames)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      # nlpar cov c - betas
      if (!c_form_0) {
        if (class == 'b' & grepl("c_cov", x) & !is.null(c_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- ccovcoefnames
          } else {
            coef <- ccovcoefnames[2:length(ccovcoefnames)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      # nlpar cov d - betas
      if (!d_form_0) {
        if (class == 'b' & grepl("d_cov", x) & !is.null(d_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- dcovcoefnames
          } else {
            coef <- dcovcoefnames[2:length(dcovcoefnames)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      # nlpar cov e - betas
      if (!e_form_0) {
        if (class == 'b' & grepl("e_cov", x) & !is.null(e_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- ecovcoefnames
          } else {
            coef <- ecovcoefnames[2:length(ecovcoefnames)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      
      # nlpar cov f - betas
      if (!f_form_0) {
        if (class == 'b' & grepl("f_cov", x) & !is.null(f_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- fcovcoefnames
          } else {
            coef <- fcovcoefnames[2:length(fcovcoefnames)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      # nlpar cov g - betas
      if (!g_form_0) {
        if (class == 'b' & grepl("g_cov", x) & !is.null(g_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- gcovcoefnames
          } else {
            coef <- gcovcoefnames[2:length(gcovcoefnames)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      
      # nlpar cov h - betas
      if (!h_form_0) {
        if (class == 'b' & grepl("h_cov", x) & !is.null(h_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- hcovcoefnames
          } else {
            coef <- hcovcoefnames[2:length(hcovcoefnames)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      # nlpar cov i - betas
      if (!i_form_0) {
        if (class == 'b' & grepl("i_cov", x) & !is.null(i_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- icovcoefnames
          } else {
            coef <- icovcoefnames[2:length(icovcoefnames)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      # sigma cov - betas
      if (!grepl("~0", sigma_formulasi, fixed = T) ) {
        class_org <- class
        if (grepl("sigma_cov", x) & !is.null(sigma_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- sigmacovcoefnames
          } else {
            coef <- sigmacovcoefnames[2:length(sigmacovcoefnames)]
            class <- c( rep('b', length(sigmacovcoefnames[-1])))
          }
          
          
          dpar <- sigma_dpar
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
          
        }
        class <- class_org
      }
      
      
      
      # nlpar cov s - betas
      if (!s_form_0) {
        if (class == 'b' & grepl("s_cov", x) & !is.null(s_cov_prior_beta)) {
          if (ept(mnf)) {
            coef <- scovcoefnames
          } else {
            coef <- scovcoefnames[2:length(scovcoefnames)]
          }
          nlpar <- paste0(nlpar, 1:df)
          nlpar <- rep(nlpar, times = length(coef), each = 1)
          coef <- rep(coef , times = 1, each = df)
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
          
        }
      }
      
    } # if(class == 'b')
    
    
    
    
    if (class == 'sd') {
      if(nlpar != '') {
        mnf <- paste0(nlpar, "_form_0_gr")
        mnc <- paste0("cov_nlpar")
      }
      if(sigma_dpar == 'sigma' | cov_sigma_dpar == 'sigma_cov') {
        mnf <- paste0('sigma', "_form_0_gr")
        mnc <- paste0("cov_sigma_dpar")
      }
      
      
      
      if (nlpar == 'a')
        coef <- acovcoefnames_gr
      if (nlpar == 'b')
        coef <- bcovcoefnames_gr
      if (nlpar == 'c')
        coef <- ccovcoefnames_gr
      if (nlpar == 'd')
        coef <- dcovcoefnames_gr
      if (nlpar == 'e')
        coef <- ecovcoefnames_gr
      if (nlpar == 'f')
        coef <- fcovcoefnames_gr
      if (nlpar == 'g')
        coef <- gcovcoefnames_gr
      if (nlpar == 'h')
        coef <- hcovcoefnames_gr
      if (nlpar == 'i')
        coef <- icovcoefnames_gr
      
      if (nlpar == 's') {
        coef <- rep("", length(scovcoefnames_gr))
        if (!s_form_0_gr & !is.null(sncov_gr))
          coef <- coef[1]
      }
      
      if (sigma_dpar == 'sigma') {
        dpar <- sigma_dpar
        coef <- sigmacovcoefnames_gr
      }
      
      
      # nlpar a b c d e f g h i - sd
      
      if (ept(mnf) & cov_nlpar == "") {
        if (!any(is.na(lowerbound)) | !any(is.na(upperbound))) {
          define_ <- unique(define_)
          lowerbound <- unique(lowerbound)
          upperbound <- unique(upperbound)
          setcoef <- ""
        } else {
          setcoef <- coef
        }
        priors_ <-
          brms::prior_string(
            define_,
            class = class,
            nlpar = nlpar,
            coef = setcoef,
            group = group,
            resp = resp,
            dpar = dpar,
            lb = lowerbound,
            ub = upperbound
          )
      }
      
      if (!ept(mnf) & cov_nlpar == "") {
        if (!any(is.na(lowerbound)) | !any(is.na(upperbound))) {
          define_ <- unique(define_)
          lowerbound <- unique(lowerbound)
          upperbound <- unique(upperbound)
          setcoef <- ""
        } else {
          if (ept(mnc) == "") {
            setcoef <- coef[1]
          } else {
            setcoef <- coef[-1]
          }
        }
        priors_ <-
          brms::prior_string(
            define_,
            class = class,
            nlpar = nlpar,
            coef = setcoef,
            group = group,
            resp = resp,
            dpar = dpar,
            lb = lowerbound,
            ub = upperbound
          )
      }
      
      
      
      if (nlpar == 's' & cov_nlpar == "") {
        nlpar <- paste0(nlpar, 1:df)
        if (grepl("~1", s_formula_grsi, fixed = T)) {
          if (all(coef == "")) {
            priors_ <- brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              resp = resp,
              dpar = dpar,
              lb = lowerbound,
              ub = upperbound
            )
          } else {
            priors_ <-   brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              group = group,
              resp = resp,
              dpar = dpar
            )
          }
        }
        if (grepl("~0", s_formula_grsi, fixed = T)) {
          nlpar <- rep(nlpar ,
                       times = 1,
                       each = length(scovcoefnames_gr))
          if (all(coef == "")) {
            priors_ <- brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              resp = resp,
              dpar = dpar,
              lb = lowerbound,
              ub = upperbound
            )
          } else {
            priors_ <-   brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              group = group,
              resp = resp,
              dpar = dpar
            )
          }
        }
      }
      
      
      # nlpar cov a - sd
      if (!a_form_0_gr) {
        if (class == 'sd' & grepl("a_cov", x) & !is.null(a_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- acovcoefnames_gr
          } else {
            coef <- acovcoefnames_gr[2:length(acovcoefnames_gr)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      # nlpar cov b - sd
      if (!b_form_0_gr) {
        if (class == 'sd' & grepl("b_cov", x) & !is.null(b_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- bcovcoefnames_gr
          } else {
            coef <- bcovcoefnames_gr[2:length(bcovcoefnames_gr)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      # nlpar cov c - sd
      if (!c_form_0_gr) {
        if (class == 'sd' & grepl("c_cov", x) & !is.null(c_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- ccovcoefnames_gr
          } else {
            coef <- ccovcoefnames_gr[2:length(ccovcoefnames_gr)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      
      # nlpar cov d - sd
      if (!d_form_0_gr) {
        if (class == 'sd' & grepl("d_cov", x) & !is.null(d_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- dcovcoefnames_gr
          } else {
            coef <- dcovcoefnames_gr[2:length(dcovcoefnames_gr)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      # nlpar cov e - sd
      if (!e_form_0_gr) {
        if (class == 'sd' & grepl("e_cov", x) & !is.null(e_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- ecovcoefnames_gr
          } else {
            coef <- ecovcoefnames_gr[2:length(ecovcoefnames_gr)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      
      # nlpar cov f - sd
      if (!f_form_0_gr) {
        if (class == 'sd' & grepl("f_cov", x) & !is.null(f_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- fcovcoefnames_gr
          } else {
            coef <- fcovcoefnames_gr[2:length(fcovcoefnames_gr)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      
      # nlpar cov g - sd
      if (!g_form_0_gr) {
        if (class == 'sd' & grepl("g_cov", x) & !is.null(g_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- gcovcoefnames_gr
          } else {
            coef <- gcovcoefnames_gr[2:length(gcovcoefnames_gr)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      # nlpar cov h - sd
      if (!h_form_0_gr) {
        if (class == 'sd' & grepl("h_cov", x) & !is.null(h_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- hcovcoefnames_gr
          } else {
            coef <- hcovcoefnames_gr[2:length(hcovcoefnames_gr)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      # nlpar cov i - sd
      if (!h_form_0_gr) {
        if (class == 'sd' & grepl("h_cov", x) & !is.null(h_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- hcovcoefnames_gr
          } else {
            coef <- hcovcoefnames_gr[2:length(hcovcoefnames_gr)]
          }
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              group = group,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      
      # nlpar cov s - betas
      if (!s_form_0_gr) {
        if (class == 'b' & grepl("s_cov", x) & !is.null(s_cov_prior_sd)) {
          if (ept(mnf)) {
            coef <- scovcoefnames_gr
          } else {
            coef <- scovcoefnames_gr[2:length(scovcoefnames_gr)]
          }
          nlpar <- paste0(nlpar, 1:df)
          nlpar <- rep(nlpar, times = length(coef), each = 1)
          coef <- rep(coef , times = 1, each = df)
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
          
        }
      }
      
      
      
      
      # sigma cov - sd
      if(!is.null(sigma_formula_grsi)) {
        if (!grepl("~0", sigma_formula_grsi, fixed = T)) {
          if (class == 'sd' & grepl("sigma_cov", x) & 
              !is.null(sigma_cov_prior_sd)) {
            if (ept(mnf)) {
              coef <- sigmacovcoefnames_gr
            } else {
              coef <- sigmacovcoefnames_gr[2:length(sigmacovcoefnames_gr)]
            }
            dpar <- sigma_dpar
            priors_ <-
              brms::prior_string(
                define_,
                class = class,
                nlpar = nlpar,
                group = group,
                coef = coef,
                resp = resp,
                dpar = dpar
              )
          }
        }
      }
      
      
    } # end if (class == 'sd') {
    
    
    
    # correlation priors (lkj)
    
    # Currently, 'brms' does not allow setting separate ljk prior for
    # subset and multivariate
    # Also, removing resp leads to duplicate priors, so need to set only once
    
    # Note, currently 'brms' not allowing setting separate cor prior for sigma
    # So, set sigma_prior_cor <- NULL (line 810) for all, or else set group = '
    # But then this won't let assign different prior - dup stanvars
    # So keeping group 
    
    if (class == 'cor') {
      # if(sigma_dpar == "sigma") group <- ""
      if (ii == 1) {
        priors_ <-  brms::prior_string(define_,
                                       class = class,
                                       group = group,
                                       dpar = dpar) # resp = resp,
      } else {
        priors_ <- ""
      }
    }
    
    
    if (class == 'rescor') {
      if (ii == 1) {
        priors_ <-  brms::prior_string(define_,
                                       class = class,
                                       group = "",
                                       dpar = "")
      } else {
        priors_ <- ""
      }
    }
    
    
    
    # residual standard deviation (sigma) prior
    if (class == 'sigma' & dpar == "") {
      priors_ <-  brms::prior_string(
        define_,
        class = class,
        lb = lowerbound,
        ub = upperbound,
        resp = resp,
        dpar = dpar
      )
      
    }
    
    # residual standard deviation (sigma) prior - dpar_formula formulation
    if (class == "" & !is.null(dpar_formulasi)) {
      # Need to remove lb and ub if specifying coef, otherwise
      # Error: Argument 'coef' may not be specified when using boundaries.
      dpar <- 'sigma'
      if (!is.null(dpar_covi_mat_form) &
          !grepl("~1$", dpar_covi_mat_form, fixed = F)) {
        class <- 'b'
        mnf <- paste0('dpar', "_form_0")
        mnc <- paste0("dpar_cov")
        
        if (all(is.na(lowerbound)) |
            all(is.na(upperbound))) {
          if (grepl("^lf\\(", dpar_formulasi)) {
            if (grepl("cmc=F", dpar_formulasi) |
                grepl("cmc=FALSE", dpar_formulasi)) {
              coef <- c("", dparcovcoefnames[2:length(dparcovcoefnames)])
              define_ <- c("", define_[2:length(define_)])
            } else {
              coef <- dparcovcoefnames
            }
          }
          if (!grepl("^lf\\(", dpar_formulasi) |
              !grepl("^nlf\\(", dpar_formulasi)) {
            coef <- dparcovcoefnames
          }
        } else if (!all(is.na(lowerbound)) & !all(is.na(upperbound))) {
          coef <- rep("", length(dparcovcoefnames))
        }
        
        if (ept(mnf)) {
          if (!any(is.na(lowerbound)) | !any(is.na(upperbound))) {
            define_ <- unique(define_)
            lowerbound <- unique(lowerbound)
            upperbound <- unique(upperbound)
            setcoef <- ""
          } else {
            setcoef <- coef
          }
          
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = setcoef,
              resp = resp,
              dpar = dpar,
              lb = lowerbound,
              ub = upperbound
            )
        }
        
        if (!ept(mnf)) {
          if (!any(is.na(lowerbound)) | !any(is.na(upperbound))) {
            define_ <- unique(define_)
            lowerbound <- unique(lowerbound)
            upperbound <- unique(upperbound)
            setcoef <- ""
          } else {
            if (ept(mnc) != "") {
              setcoef <- coef[1]
            } else {
              setcoef <- coef
            }
          }
          
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = setcoef,
              resp = resp,
              dpar = dpar,
              lb = lowerbound,
              ub = upperbound
            )
        }
        
        
        
        
        # residual standard deviation (sigma) covariate-dpar_formula formulation
        
        if (!is.null(dpar_covi_mat_form) &
            grepl("~1", dpar_covi_mat_form, fixed = T) &
            !grepl("~1$", dpar_covi_mat_form, fixed = T) &
            !is.null(dpar_cov_prior_sigma)) {
          if (grepl("dpar", x) & !grepl("dpar_cov", x)) {
            if (grepl("^lf\\(", dpar_formulasi)) {
              if (grepl("center=T", dpar_formulasi) |
                  grepl("center=TRUE", dpar_formulasi)) {
                class <- dparcovcoefnames[1]
                coef  <- ""
              } else {
                class <- 'b'
                coef  <- dparcovcoefnames[1]
              }
            } else if (!grepl("^lf\\(", dpar_formulasi) |
                       !grepl("^nlf\\(", dpar_formulasi)) {
              class <- dparcovcoefnames[1]
              coef  <- ""
            }
          }
          
          if (!grepl("dpar", x) & grepl("dpar_cov", x)) {
            class <- 'b'
            coef <- dparcovcoefnames
          }
          
          if (class == 'b') {
            if (grepl("center=T", dpar_formulasi) |
                grepl("center=TRUE", dpar_formulasi)) {
              coef <- coef[-1]
            } else {
              coef <- coef
            }
          }
          if (!grepl("center=T", dpar_formulasi) &
              !grepl("center=TRUE", dpar_formulasi)) {
            if (grepl("dpar_cov", x))
              coef <- coef[-1]
          }
          
          priors_ <-
            brms::prior_string(
              define_,
              class = class,
              nlpar = nlpar,
              coef = coef,
              resp = resp,
              dpar = dpar
            )
        }
      }
      
      
      
      if (!is.null(dpar_covi_mat_form) &
          grepl("~1$", dpar_covi_mat_form, fixed = F)) {
        if (grepl("center=T", dpar_formulasi) |
            grepl("center=TRUE", dpar_formulasi)) {
          class <- dparcovcoefnames[1]
          coef  <- ""
        } else {
          class <- 'b'
          coef  <- dparcovcoefnames[1]
        }
        
        priors_ <-
          brms::prior_string(
            define_,
            class = class,
            nlpar = nlpar,
            coef = coef,
            resp = resp,
            dpar = dpar
          )
      }
    }
    
    
    # autocorrelation priors
    
    if (setautocorr) {
      coef <- ""
      if (acorclass == 'arma') {
        acorclassclasses <- c("ar", "ma")
        priors_arma_c <- list()
        stanvars_data_in_c <- list()
        for (acorclassi in acorclassclasses) {
          define_ <- priors_arma_c_define[[acorclassi]]$define_
          lowerbound <-
            priors_arma_c_define[[acorclassi]]$lowerbound
          upperbound <-
            priors_arma_c_define[[acorclassi]]$upperbound
          priors_temp <-  brms::prior_string(
            define_,
            class = acorclassi,
            lb = lowerbound,
            ub = upperbound,
            coef = coef,
            resp = resp,
            dpar = dpar
          )
          priors_arma_c[[acorclassi]] <- priors_temp
          stanvars_data_in_c[[acorclassi]] <-
            priors_arma_c_define[[acorclassi]]$stanvars_data_in
        }
        priors_ <- priors_arma_c %>% do.call(rbind, .)
        stanvars_data_in <- stanvars_data_in_c %>% do.call(rbind, .)
      } else {
        priors_ <-  brms::prior_string(
          define_,
          class = class,
          lb = lowerbound,
          ub = upperbound,
          coef = coef,
          resp = resp,
          dpar = dpar
        )
      }
    }
    out_pr <-
      list(
        priors_ = priors_,
        stanvars_data_in = stanvars_data_in,
        initial_in = initial_in
      )
    return(out_pr)
  } 
  
  
  
  # use following custom order
  # This ensures that corresponding initial arguments are matched
  # with the sequence of prior argument evaluation
  
  custom_order_prior <- c(
    'a_prior_beta',
    'a_cov_prior_beta',
    'b_prior_beta',
    'b_cov_prior_beta',
    'c_prior_beta',
    'c_cov_prior_beta',
    'd_prior_beta',
    'd_cov_prior_beta',
    'e_prior_beta',
    'e_cov_prior_beta',
    'f_prior_beta',
    'f_cov_prior_beta',
    'g_prior_beta',
    'g_cov_prior_beta',
    'h_prior_beta',
    'h_cov_prior_beta',
    'i_prior_beta',
    'i_cov_prior_beta',
    's_prior_beta',
    's_cov_prior_beta',
    'a_prior_sd',
    'a_cov_prior_sd',
    'b_prior_sd',
    'b_cov_prior_sd',
    'c_prior_sd',
    'c_cov_prior_sd',
    'd_prior_sd',
    'd_cov_prior_sd',
    'e_prior_sd',
    'e_cov_prior_sd',
    'f_prior_sd',
    'f_cov_prior_sd',
    'g_prior_sd',
    'g_cov_prior_sd',
    'h_prior_sd',
    'h_cov_prior_sd',
    'i_prior_sd',
    'i_cov_prior_sd',
    's_prior_sd',
    's_cov_prior_sd',
    'sigma_prior_beta',
    'sigma_cov_prior_beta',
    'sigma_prior_sd',
    'sigma_cov_prior_sd',
    'gr_prior_cor',
    'sigma_prior_cor',
    'rsd_prior_sigma',
    'dpar_prior_sigma',
    'dpar_cov_prior_sigma',
    'autocor_prior_acor',
    'autocor_prior_unstr_acor',
    'mvr_prior_rescor'
  )
  
  
  custom_order_prior_str_evaluating <- FALSE
  if(any(custom_order_prior_str != "")) {
    custom_order_prior <- custom_order_prior_str
    custom_order_prior_str_evaluating <- TRUE
  }
  
  
  
  stanvars_data_5 <- list()
  initial_in_data <- list()
  c_priors <- list()
  for (ip in custom_order_prior) {
    if (grepl("_prior_", ip)) {
      if (!is.null(eval(parse(text = ip)))) {
        xx   <- deparse(ip)
        pget <- eval_prior_args(eval(parse(text = xx)))
        zz   <- pget$priors_
        stanvars_data_5[[ip]] <- pget$stanvars_data_in
        initial_in_data[[ip]] <- pget$initial_in
        c_priors[[ip]] <- zz
      }
    }
  }
  
  evaluated_priors <- c_priors %>% do.call(rbind, .)
  

  newlist <- c()
  for (i in 1:length(stanvars_data_5)) {
    ttt <- stanvars_data_5[[i]][1:length(stanvars_data_5[[i]])]
    newlist <- c(newlist, unname(ttt))
  }
  
  svardatalistlist <- c()
  for (istanvardata in 1:length(newlist)) {
    svardatalistlist[istanvardata] <-
      paste0("newlist[[", istanvardata, "]]")
  }
  
  stanvars <-
    eval(parse(text = paste(svardatalistlist, collapse = "+")))
  
  attr(evaluated_priors, 'stanvars') <- stanvars
  
  initial_in_datazz <- initial_in_data
  
  if (is.list(initial_in_datazz) & length(initial_in_datazz) == 0) {
    initial_in_datazz <- NULL
  }
  

  if(!is.null(initsi[[1]])) {
    if(initsi[[1]] == 'random') {
      initial_in_datazz <- NULL
      combined_inits <- NULL
    }
  }
  
  
  
  if (!is.null(initial_in_datazz)) {
    if (!is.null(gr_prior_cor) | !is.null(sigma_prior_cor) ) {
      list_ck <- list_ck_ <- list()
      list_ck_rescor <- list()
      ik_j <- ik_j_ <- 0
      what_not_to_flatten <- "^L_|^z_|Intercept_sigma|Lrescor"
      what_not_to_flatten2 <- "^L_|^z_"
      if(length(initial_in_datazz$a_prior_beta) == 0) {
        what_not_to_flatten <- paste0(what_not_to_flatten, "|b_a")
        what_not_to_flatten2 <- paste0(what_not_to_flatten2, "|b_s")
      }
      for (ik in 1:length(initial_in_datazz)) {
        ik_j <- ik_j + 1
        ik_names <- names(initial_in_datazz[[ik]])
        if (!any(grepl(what_not_to_flatten, ik_names))) {
          list_ck[[ik]] <- initial_in_datazz[[ik_j]]
          names(list_ck[[ik]]) <- ik_names
        } else if (any(grepl(what_not_to_flatten2, ik_names))) {
          mn <- 0
          for (ikl in 1:length(grepl(what_not_to_flatten2, ik_names))) {
            mn <- mn + 1
            ik_j_ <- ik_j_ + 1
            list_ck_[[ik_j_]] <- initial_in_datazz[[ik_j]][[mn]]
          }
          names(list_ck_) <- ik_names
        } else if (grepl("^Intercept_sigma", ik_names)) {
          list_ck[[ik]] <- initial_in_datazz[[ik_j]]
          names(list_ck[[ik]]) <- ik_names
        } else if (multivariate$mvar & multivariate$rescor &
                   grepl("^Lrescor", ik_names)) {
          list_ck_rescor[[ik]] <- initial_in_datazz[[ik_j]]
          names(list_ck_rescor[[ik]]) <- ik_names
        }
      }
      list_ck <- list_ck[lengths(list_ck) != 0]
      keys    <- unique(unlist(lapply(list_ck, names)))
      list_ck <-
        setNames(do.call(mapply, c(FUN = c, lapply(
          list_ck, `[`, keys
        ))), keys)
      combined_inits <- c(list_ck, list_ck_)
    }
    

    
    if (is.null(gr_prior_cor) & is.null(sigma_prior_cor) ) {
      list_ck <- list_ck_z <- list_ck_sd <- list()
      list_ck_rescor <- list()
      ik_j <- ik_j_ <- 0
      for (ik in 1:length(initial_in_datazz)) {
        ik_j <- ik_j + 1
        ik_names <- names(initial_in_datazz[[ik]])
        if (!any(grepl("^L_|^z_|Intercept_sigma|Lrescor", ik_names))) {
          list_ck[[ik]] <- initial_in_datazz[[ik_j]]
          names(list_ck[[ik]]) <- ik_names
        } else if (any(grepl("^L_|^z_", ik_names))) {
          mn <- 0
          for (ikl in 1:length(grepl("^L_|^z_", ik_names))) {
            mn <- mn + 1
            ik_j_ <- ik_j_ + 1
            if (is.matrix(initial_in_datazz[[ik_j]][[mn]]))
              list_ck_z[[ik_j_]] <- initial_in_datazz[[ik_j]][[mn]]
            if (!is.matrix(initial_in_datazz[[ik_j]][[mn]]))
              list_ck_sd[[ik_j_]] <- initial_in_datazz[[ik_j]][[mn]]
          }
          
          names(list_ck_z) <- ik_names[2]
          names(list_ck_sd) <- ik_names[1]
        } else if (grepl("^Intercept_sigma", ik_names)) {
          list_ck[[ik]] <- initial_in_datazz[[ik_j]]
          names(list_ck[[ik]]) <- ik_names
        } else if (multivariate$mvar & multivariate$rescor &
                   grepl("^Lrescor", ik_names)) {
          list_ck_rescor[[ik]] <- initial_in_datazz[[ik_j]]
          names(list_ck_rescor[[ik]]) <- ik_names
        }
      }
      list_ck <- list_ck[lengths(list_ck) != 0]
      keys    <- unique(unlist(lapply(list_ck, names)))
      list_ck <-
        setNames(do.call(mapply, c(FUN = c, lapply(
          list_ck, `[`, keys
        ))), keys)
      list_ck_sd <- list_ck_sd[lengths(list_ck_sd) != 0]
      # this on 9 5 23 to accomodate random = ''
      if(length(list_ck_sd) != 0) {
        for (list_ck_sd_i in 1:length(list_ck_sd)) {
          if (length(list_ck_sd[[list_ck_sd_i]]) > 1) {
            nami_ <-
              paste0(names(list_ck_sd[[list_ck_sd_i]][1]),
                     "cov",
                     2:length(list_ck_sd[[list_ck_sd_i]]) - 1)
            names(list_ck_sd[[list_ck_sd_i]]) <-
              c(names(list_ck_sd[[list_ck_sd_i]][1]), nami_)
          }
        }
      }
      names(list_ck_sd) <-
        rep(names(list_ck_sd[1]), length(list_ck_sd))
      list_ck_sd2 <- list_ck_sd
      list_ck_z <- list_ck_z[lengths(list_ck_z) != 0]
      list_ck_z2 <- list()
      if(length(list_ck_z) != 0) {
        for (list_ck_i in 1:length(list_ck_z)) {
          addelemnt <-
            strsplit(gsub("\\+", " ", randomsi), " ")[[1]][list_ck_i]
          list_ck_z2[[paste0("z", "_", addelemnt, resp_, list_ck_i)]] <-
            list_ck_z[[list_ck_i]]
          attr(list_ck_z2[[paste0("z", "_", addelemnt, list_ck_i)]], "names") <-
            NULL
        }
      }
      combined_inits <- c(list_ck, list_ck_sd2, list_ck_z2)
    }
    
    # Don't let it evaluate when custom_order_prior_str != "" 
    # i.e,  when evaluating when hierarchy priors 
    
    if (multivariate$mvar & 
        multivariate$rescor & 
        !custom_order_prior_str_evaluating) {
      list_ck_rescor <- list_ck_rescor[lengths(list_ck_rescor) != 0]
      list_ck_rescor <- list_ck_rescor[[1]]
      combined_inits <- c(combined_inits, list_ck_rescor)
    }
    
    
    # Convert vector of 's' initials to named individual (s1, s2)
    if(select_model == "sitar" | select_model == 'rcs') {
      # Don't let when evaluating _str higher custom order
      if("s_prior_beta" %in% custom_order_prior) {
        first_loop <- TRUE  
      } else {
        first_loop <- FALSE  
      }
      if(first_loop) {
        nlpar_s_init <- paste0('_s', 1:df)
        if (grepl("~0", s_formulasi, fixed = T)) {
          nlpar_s_init <-
            rep(nlpar_s_init ,
                times = 1,
                each = length(scovcoefnames))
        } else if (!grepl("~0", s_formulasi, fixed = T)) {
          nlpar_s_init <- rep(nlpar_s_init , times = length(scovcoefnames))
        }
        
        
        subset_sparms <-
          combined_inits[grepl(".*_s$", names(combined_inits))]
        
        subset_sparms_name <- names(subset_sparms)
        if(length(subset_sparms) != 0) subset_sparms_numeric <- 
          subset_sparms[[1]]
        if(length(subset_sparms) == 0) subset_sparms_numeric <- NULL
        if(!is.null(subset_sparms_numeric)) {
          subset_sparms2 <- list()
          subset_sparms2names <- c()
          
          for (subset_sparmsi in 1:length(subset_sparms_numeric)) {
            subset_sparms_namei <-
              gsub("_s", nlpar_s_init[subset_sparmsi], subset_sparms_name)
            subset_sparms2[[subset_sparms_namei]] <-
              subset_sparms_numeric[subset_sparmsi]
            subset_sparms2names <-
              c(subset_sparms2names, subset_sparms_namei)
          }
          names(subset_sparms_numeric) <- subset_sparms2names
          subset_sparms3 <- list()
          for (isi in 1:df) {
            subset_sparms3[[paste0("b", resp_, "_s", isi)]] <-
              subset_sparms_numeric[grep(paste0("b", resp_, "_s", isi),
                                         names(subset_sparms_numeric))]
          }
          subset_sparms <- subset_sparms3
          subset_sparms <-
            subset_sparms[!names(subset_sparms) %in% subset_sparms_name]
          combined_inits <-
            append(combined_inits, subset_sparms, after = grep(
              paste0("^", subset_sparms_name, "$"),
              names(combined_inits)
            ))
          combined_inits <-
            combined_inits[!names(combined_inits) %in% 
                             paste0("",
                                    subset_sparms_name, "")]
          initials <- combined_inits
        } # if(!is.null(subset_sparms_numeric)) {
        if(is.null(subset_sparms_numeric)) {
          initials <- combined_inits
        }
      } # if(first_loop) {
      
      if(!first_loop) initials <- combined_inits
      
    } # if(select_model == "sitar") {
    
    if(select_model != "sitar" & select_model != 'rcs') initials <- combined_inits
    
  } # if (!is.null(initial_in_datazz)) {
  
  
  
  # Mean all initals random
  if(length(combined_inits) == 0) initials <- NULL
  
  if (is.null(initial_in_datazz)) {
    initials <- NULL
  }
  
  ###################3
  
  stanvar_priors_names <- names(stanvars)
  getaux <- "tau"
  stanvar_priors_names_c <- c()
  for (stanvar_priors_namesi in stanvar_priors_names) {
    t <-
      stanvar_priors_namesi[grep(paste0(getaux, '_scale', resp_), 
                                 stanvar_priors_namesi)]
    t <- gsub(paste0('_scale', resp_), "", t, fixed = T)
    stanvar_priors_names_c <- c(stanvar_priors_names_c, t)
  }
  
  add_tau <- list()
  for (stanvar_priors_names_ci in stanvar_priors_names_c) {
    fstandat <-
      unlist(stanvars)[grep(paste0(
        stanvar_priors_names_ci,
        paste0('_scale', resp_, ".sdata")
      ), names(unlist(stanvars)))] %>% as.numeric()
    add_tau[[paste0(stanvar_priors_names_ci, resp_)]] <-
      rep(1, length(fstandat))
  }
  if (length(add_tau) == 0)
    add_tau <- NULL
  
  getaux <- "nu"
  stanvar_priors_names_c <- c()
  for (stanvar_priors_namesi in stanvar_priors_names) {
    t <-
      stanvar_priors_namesi[grep(paste0(getaux, '_scale', resp_), 
                                 stanvar_priors_namesi)]
    t <- gsub(paste0('_scale', resp_), "", t, fixed = T)
    stanvar_priors_names_c <- c(stanvar_priors_names_c, t)
  }
  add_nu <- list()
  for (stanvar_priors_names_ci in stanvar_priors_names_c) {
    add_nu[[paste0(stanvar_priors_names_ci, resp_)]] <-  5
  }
  if (length(add_nu) == 0)
    add_nu <- NULL
  
  initials <- c(initials, add_tau, add_nu)
  
  ################
  revSubstr <- function(x_) {
    x__ <- substr(x_, start = 1, stop = 3)
    x___ <- paste0(rev(strsplit(x__, "_")[[1]]), collapse = "_")
    x___ <- gsub(x__, x___, x_)
    x___
  }
  
  tau_nu_init_list <- c(add_tau, add_nu)
  
  if (length(tau_nu_init_list) != 0) {
    names_tau_nu_parms <- names(tau_nu_init_list)
    names_tau_nu_parmsi_c <- c()
    for (names_tau_nu_parmsi in names_tau_nu_parms) {
      plength <- length(tau_nu_init_list[[names_tau_nu_parmsi]])
      revstr <- revSubstr(names_tau_nu_parmsi)
      if (!grepl("^b_b", names_tau_nu_parmsi, fixed = F)) {
        o <-
          paste0("vector[",
                 plength,
                 "]",
                 " ",
                 revstr,
                 " = ",
                 names_tau_nu_parmsi,
                 ";")
        names_tau_nu_parmsi_c <- c(names_tau_nu_parmsi_c, o)
      }
    }
    names_tau_nu_parmsi_c <- names_tau_nu_parmsi_c
    names_tau_nu_parmsi_cc <-
      paste(names_tau_nu_parmsi_c, collapse = "\n")
    
    scode_auxillary <-
      brms::stanvar(scode = names_tau_nu_parmsi_cc,
                    block = "genquant",
                    position = 'end')
  } else if (length(tau_nu_init_list) == 0) {
    scode_auxillary <- NULL
  }
  
  ##################
  out_listx <- initials
  for (ili in names(initials)) {
    if(length(out_listx[[ili]]) == 1) {
      out_listx[[ili]] <- out_listx[[ili]]
      # here also need array for a b c d e etc.
      # but not for sigma when sigma ~ not used but default rsd formulation 
      if(nys == 1) sigma_par_name_rsd <- "sigma"
      if(nys > 1) sigma_par_name_rsd <- paste0('sigma', resp_)
      if(ili != sigma_par_name_rsd) {
        out_listx[[ili]] <- array(out_listx[[ili]], 
                                  dim = length(out_listx[[ili]]))
      }
    } else if(length(out_listx[[ili]]) > 1 & is.vector(out_listx[[ili]])) {
      out_listx[[ili]] <- array(out_listx[[ili]], 
                                dim = length(out_listx[[ili]]))
    }
    
    if(is.na(ili)) ili <- "xxxxxxxxxxxxxx"
    # for ar and ma, it is always vector , so array
    if(ili == 'ar' | ili == 'ma') {
      out_listx[[ili]] <- array(out_listx[[ili]], 
                                dim = length(out_listx[[ili]]))
    }
  }
  
  initials <- out_listx
  
  # When sigma  formula is ~1+.., then first element is Intercept_sigma and the 
  # remaining are b_sigma
  
  initialsx <- out_listx
  if(!sigma_form_0) {
    if(nys == 1) {
      sigma_par_name <- 'b_sigma'
      Intercept_sigma <- 'Intercept_sigma'
    } else if(nys > 1) {
      sigma_par_name <- paste0('b_sigma', resp_)
      Intercept_sigma <- paste0('Intercept_sigma', resp_)
    }
    if(!is.null(initialsx[[sigma_par_name]])) {
      g_sigma_i <- initialsx[[sigma_par_name]]
      initialsx[[Intercept_sigma]] <- g_sigma_i[1]
      if(length(g_sigma_i) > 1) {
        initialsx[[sigma_par_name]] <-  
          array(g_sigma_i[2:length(g_sigma_i)], 
                dim = length(g_sigma_i[2:length(g_sigma_i)]))
      } # if(length(g_sigma_i) > 1) {
    }
  }
  
  
  initials <- initialsx
  
  
  
  # Re create symmetric square Lcortime which is flattened to a vector
  # This could have been done above like L_|z_|Lrescor etc but this is same
  if(!is.null(initials[['Lcortime']])) {
    NC_dims         <- ept(cortimeNlags) %>% as.numeric()
    initials[['Lcortime']] <- matrix(initials[['Lcortime']], 
                                     nrow = NC_dims, 
                                     ncol = NC_dims)
  } # if(!is.null(initials[['Lcortime']])) {
  
  
  
  attr(evaluated_priors, 'initials') <- initials
  attr(evaluated_priors, 'scode_auxillary') <- scode_auxillary
  
  return(evaluated_priors)
}



