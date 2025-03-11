#' Approximate standard deviations for the models
#'
#' Approximate standard deviations are computed for the models in the given
#' model space. Two versions are computed.
#'
#' @param df Data frame with data for the SEM analysis.
#' @param dep_var_col Column with the dependent variable
#' @param timestamp_col The name of the column with timestamps
#' @param entity_col Column with entities (e.g. countries)
#' @param model_space A matrix (with named rows) with each column corresponding
#' to a model. Each column specifies model parameters. Compare with
#' \link[bdsm]{optimal_model_space}
#' @param model_prior Which model prior to use. For now there are two options:
#' \code{'uniform'} and \code{'binomial-beta'}. Default is \code{'uniform'}.
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[bdsm]{SEM_likelihood} for details.
#' @param run_parallel If \code{TRUE} the optimization is run in parallel using
#' the \link[parallel]{parApply} function. If \code{FALSE} (default value) the
#' base apply function is used. Note that using the parallel computing requires
#' setting the default cluster. See README.
#'
#' @return
#' Matrix with columns describing likelihood and standard deviations for each
#' model. The first row is the likelihood for the model (computed using the
#' parameters in the provided model space). The second row is almost 1/2 * BIC_k
#' as in Raftery's Bayesian Model Selection in Social Research eq. 19 (see TODO
#' in the code below). The third row is model posterior probability. Then there
#' are rows with standard deviations for each parameter. After that we have rows
#' with robust standard deviation (not sure yet what exactly "robust" means).
#'
#' @importFrom parallel parApply
#' @export
#'
#' @examples
#' \donttest{
#'   data_centered_scaled <-
#'     feature_standardization(df = bdsm::economic_growth[,1:7],
#'                             timestamp_col = year, entity_col = country)
#'   data_cross_sectional_standarized <-
#'     feature_standardization(df = data_centered_scaled, timestamp_col = year,
#'                             entity_col = country, cross_sectional = TRUE,
#'                             scale = FALSE)
#'
#'     likelihoods_summary(df = data_cross_sectional_standarized,
#'                         dep_var_col = gdp, timestamp_col = year,
#'                         entity_col = country, model_space = economic_growth_ms)
#' }
#'
likelihoods_summary <- function(df, dep_var_col, timestamp_col, entity_col,
                                model_space,
                                exact_value = TRUE, model_prior = 'uniform',
                                run_parallel = FALSE) {
  regressors <- df %>%
    regressor_names(timestamp_col = {{ timestamp_col }},
                    entity_col = {{ entity_col }},
                    dep_var_col = {{ dep_var_col }})
  regressors_n <- length(regressors)
  variables_n <- regressors_n + 1

  matrices_shared_across_models <- df %>%
    matrices_from_df(timestamp_col = {{ timestamp_col }},
                     entity_col = {{ entity_col }},
                     dep_var_col = {{ dep_var_col }},
                     which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix"))

  n_entities <- nrow(matrices_shared_across_models$Z)
  periods_n <- nrow(df) / n_entities - 1

  prior_exp_model_size <- regressors_n / 2
  prior_inc_prob <- prior_exp_model_size / regressors_n

  print(paste("Prior Mean Model Size:", prior_exp_model_size))
  print(paste("Prior Inclusion Probability:", prior_inc_prob))

  # parameter for beta (random) distribution of the prior inclusion probability
  b <- (regressors_n - prior_exp_model_size) / prior_exp_model_size

  std_dev_from_params <- function(params, data) {
    regressors_subset <-
      regressor_names_from_params_vector(params)

    lin_features_n <- length(regressors_subset) + 1
    features_n <- ncol(data$Z)

    model_specific_matrices <- df %>%
      matrices_from_df(timestamp_col = {{ timestamp_col }},
                       entity_col = {{ entity_col }},
                       dep_var_col = {{ dep_var_col }},
                       lin_related_regressors = regressors_subset,
                       which_matrices = c("cur_Y2", "cur_Z"))

    data$cur_Z <- model_specific_matrices$cur_Z
    data$cur_Y2 <- model_specific_matrices$cur_Y2

    params_no_na <- params %>% stats::na.omit()

    likelihood <-
      SEM_likelihood(params = params_no_na, data = data,
                     exact_value = exact_value)

    hess <- hessian(SEM_likelihood, theta = params_no_na, data = data)

    likelihood_per_entity <-
      SEM_likelihood(params_no_na, data = data, per_entity = TRUE)

    # TODO: how to interpret the Gmat and Imat
    Gmat <- rootSolve::gradient(SEM_likelihood, params_no_na, data = data,
                                per_entity = TRUE)
    Imat <- crossprod(Gmat)

    # Section 2.3.3 in Moral-Benito
    # GROWTH EMPIRICS IN PANEL DATA UNDER MODEL UNCERTAINTY AND WEAK EXOGENEITY:
    # "Finally, each model-specific posterior is given by a normal distribution
    # with mean at the MLE and dispersion matrix equal to the inverse of the
    # Fisher information."
    # This is most likely why hessian is used to compute standard errors.
    # TODO: Learn the Bernstein–von Mises theorem which explain in detail how
    # all this works
    stdr <- rep(0, features_n)
    stdh <- rep(0, features_n)

    . <- NULL
    linear_params <- t(params) %>% as.data.frame() %>%
      dplyr::select(tidyselect::matches('alpha'),
                    tidyselect::matches('beta')) %>%
      as.matrix() %>% t()

    betas_first_ind <- 4 + periods_n
    betas_last_ind <- betas_first_ind + lin_features_n - 2
    inds <- if (betas_first_ind > betas_last_ind) {
      c(1)
    } else {
      c(1, betas_first_ind:betas_last_ind)
    }

    stdr[!is.na(linear_params)] <- sqrt(diag(solve(hess) %*% Imat %*% solve(hess)))[inds]
    stdh[!is.na(linear_params)] <- sqrt(diag(solve(hess)))[inds]

    # Below we have almost 1/2 * BIC_k as in Raftery's Bayesian Model Selection
    # in Social Research eq. 19. The part with reference model M_1 is skipped,
    # because we use this formula to compute exp(logl) which is in turn used to
    # compute posterior probabilities using eqs. 34/35. Since the part connected
    # with M_1 model would be present in all posteriors it cancels out. Hence
    # the important part is the one computed below.
    #
    # TODO: Why everything is divided by n_entities?

    # Eq. 19
    loglikelihood <-
      (likelihood - (lin_features_n/2)*(log(n_entities*periods_n)))/n_entities

    # Eq. 35
    bic <- exp(loglikelihood)

    if (model_prior == 'binomial-beta') {
      prior_model_prob <-
        gamma(lin_features_n) * gamma(b + regressors_n - lin_features_n + 1)
    } else if (model_prior == 'uniform') {
      prior_model_prob <-
        prior_inc_prob^(lin_features_n - 1) *
        (1-prior_inc_prob)^(regressors_n - lin_features_n + 1)
    } else {
      stop("Please specify a correct model prior!")
    }

    posterior_model_prob <- prior_model_prob * bic

    c(likelihood, bic, posterior_model_prob, stdh, stdr)
  }

  likelihoods_info <- do.call(
    ifelse(run_parallel, "parApply", "apply"),
    list(
      X = model_space, MARGIN = 2,
      FUN = std_dev_from_params,
      data = matrices_shared_across_models
    )
  )

  likelihoods_info[3,] <- likelihoods_info[3,] / sum(likelihoods_info[3,])

  likelihoods_info
}

#' Summary of a model space
#'
#' A summary of a given model space is prepared. This include things such as
#' posterior inclusion probability (PIP), posterior mean and so on. This is the
#' core function of the package, because it allows to make assessments and
#' decisions about the parameters and models.
#'
#' @param df Data frame with data for the SEM analysis.
#' @param dep_var_col Column with the dependent variable
#' @param timestamp_col The name of the column with timestamps
#' @param entity_col Column with entities (e.g. countries)
#' @param model_space A matrix (with named rows) with each column corresponding
#' to a model. Each column specifies model parameters. Compare with
#' \link[bdsm]{optimal_model_space}
#' @param model_prior Which model prior to use. For now there are two options:
#' \code{'uniform'} and \code{'binomial-beta'}. Default is \code{'uniform'}.
#' @param exact_value Whether the exact value of the likelihood should be
#' computed (\code{TRUE}) or just the proportional part (\code{FALSE}). Check
#' \link[bdsm]{SEM_likelihood} for details.
#' @param run_parallel If \code{TRUE} the optimization is run in parallel using
#' the \link[parallel]{parApply} function. If \code{FALSE} (default value) the
#' base apply function is used. Note that using the parallel computing requires
#' setting the default cluster. See README.
#'
#' @return
#' List of parameters describing analyzed models
#'
#' @examples
#' \donttest{
#' library(magrittr)
#'
#' data_prepared <- economic_growth[,1:7] %>%
#'    feature_standardization(timestamp_col = year, entity_col = country) %>%
#'    feature_standardization(timestamp_col = year, entity_col = country,
#'                            cross_sectional = TRUE, scale = FALSE)
#'
#'
#' bma_result <- bma_summary(df = data_prepared, dep_var_col = gdp,
#'                           timestamp_col = year, entity_col = country,
#'                           model_space = economic_growth_ms)
#' }
#'
#' @export
bma_summary <- function(df, dep_var_col, timestamp_col, entity_col,
                        model_space,
                        exact_value = TRUE, model_prior = 'uniform',
                        run_parallel = FALSE) {
  regressors <- df %>%
    regressor_names(timestamp_col = {{ timestamp_col }},
                    entity_col = {{ entity_col }},
                    dep_var_col = {{ dep_var_col }})
  regressors_n <- length(regressors)
  variables_n <- regressors_n + 1

  matrices_shared_across_models <- df %>%
    matrices_from_df(timestamp_col = {{ timestamp_col }},
                     entity_col = {{ entity_col }},
                     dep_var_col = {{ dep_var_col }},
                     which_matrices = c("Y1", "Y2", "Z", "res_maker_matrix"))

  n_entities <- nrow(matrices_shared_across_models$Z)
  periods_n <- nrow(df) / n_entities - 1

  bet <- optimbase::zeros(variables_n,1)
  pvarh <- optimbase::zeros(variables_n,1)
  pvarr <- optimbase::zeros(variables_n,1)
  fy <- optimbase::zeros(variables_n,1)
  fyt <- 0
  ppmsize <- 0
  cout <- 0

  likelihoods_info <- likelihoods_summary(
    df, dep_var_col = {{ dep_var_col }}, timestamp_col = {{ timestamp_col }},
    entity_col = {{ entity_col }}, model_space = model_space,
    exact_value = exact_value, model_prior = model_prior,
    run_parallel = run_parallel
  )

  regressors_subsets <- rje::powerSet(regressors)
  regressors_subsets_matrix <-
    rje::powerSetMat(regressors_n) %>% as.data.frame()

  row_ind <- 0
  for (regressors_subset in regressors_subsets) {
    row_ind <- row_ind + 1
    mt <- as.matrix(t(regressors_subsets_matrix[row_ind, ]))
    out = (mt == 0)       # regressors out of the current model

    likelihood_max <- likelihoods_info[1, row_ind]

    postprob <- likelihoods_info[3, row_ind]

    # constructing the full vector of estimates #
    mty=rbind(1,mt)

    stdrt1 <- likelihoods_info[-(1:(regressors_n+4)), row_ind]
    stdht1 <- likelihoods_info[4:(regressors_n+4), row_ind]
    varrt1 <- stdrt1^2
    varht1 <- stdht1^2

    . <- NULL
    linear_params <- t(model_space[, row_ind]) %>% as.data.frame() %>%
      dplyr::select(tidyselect::matches('alpha'),
                    tidyselect::matches('beta')) %>% replace(is.na(.), 0) %>%
      as.matrix() %>% t()

    # calculating the percentage of significant regressions #
    ptr=linear_params/stdht1
    ntr=linear_params/stdht1
    if (row_ind==1) {
      pts=ptr; nts=ntr
    }
    else {
      pts=cbind(pts,ptr); nts=cbind(nts,ntr)
    }

    # accumulating estimates for posterior model probabilities #
    fy=fy+postprob*mty
    fyt=fyt+postprob
    ppmsize=ppmsize+postprob*(sum(mty))

    # storing estimates conditional on inclusion #
    bet=bet+postprob*linear_params
    pvarr=pvarr+(postprob*varrt1+postprob*(linear_params*linear_params))         # as in Leamer (1978) #
    pvarh=pvarh+(postprob*varht1+postprob*(linear_params*linear_params))         # as in Leamer (1978) #
  }

  list(variables_n = variables_n, models_posterior_prob = likelihoods_info[3,],
       bet = bet, pvarh = pvarh, pvarr = pvarr, fy = fy, fyt = fyt,
       ppmsize = ppmsize, cout = 0, nts = nts, pts = pts)
}

#' BMA summary for parameters of interest
#'
#' TODO This is just the code previously present in the morel-benito.R script
#' wrapped as a function (to get rid of the script). Well written code and docs
#' are still needed
#'
#' @param regressors TODO
#' @param bet TODO
#' @param pvarh TODO
#' @param pvarr TODO
#' @param fy TODO
#' @param fyt TODO
#' @param ppmsize TODO
#' @param cout TODO
#' @param nts TODO (negatives)
#' @param pts TODO (positives)
#' @param variables_n TODO
#'
#' @return
#' TODO dataframe with results
parameters_summary <- function(regressors, bet, pvarh, pvarr, fy, fyt, ppmsize, cout,
                               nts, pts, variables_n) {
  popmsize=ppmsize/fyt

  # computing posterior moments CONDITIONAL on inclusion
  postprobinc=fy/fyt
  postmean=bet/fy
  varrleamer=(pvarr/fy)-postmean^2
  varhleamer=(pvarh/fy)-postmean^2
  poststdr=sqrt(varrleamer)
  poststdh=sqrt(varhleamer)
  tr=postmean/poststdr
  th=postmean/poststdh

  # computing UNCONDITIONAL posterior moments
  upostmean = postmean * postprobinc
  uvarrleamer = (varrleamer + (postmean^2))*postprobinc - (upostmean^2)
  uvarhleamer = (varhleamer + (postmean^2))*postprobinc - (upostmean^2)
  upoststdr=sqrt(uvarrleamer)
  upoststdh=sqrt(uvarhleamer)

  # computing percentage of significant coeff estimates
  nts=t(nts)
  pts=t(pts)
  for (jt in 1:variables_n) {
    ntss=stats::na.omit(nts[,jt]); ptss=stats::na.omit(pts[,jt]); nsig=ntss<(-1.96); psig=ptss>1.96
    if (jt==1) {
      negper=mean(nsig); posper=mean(psig)
    }
    else {
      negper=rbind(negper,mean(nsig)); posper=rbind(posper,mean(psig))
    }
  }

  result=as.data.frame(
    cbind(
      c("alpha", regressors),
      postprobinc, postmean, poststdh, poststdr,
      upostmean, upoststdh, upoststdr
    )
  )
  names(result)<-c("varname","postprob","pmean","std","stdR","unc_pmean","unc_std","unc_stdR")

  print(paste("Posterior Mean Model Size: ", popmsize))
  result
}
