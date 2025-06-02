


mpt_mptinr <- function(
  dataset    # name of data file
  , data     # data.frame
  , model    # name of EQN file
  , method   # analysis approaches to be conducted
  , id
  , condition
  , core = NULL
){

  prepared <- prep_data_fitting(
    data = data
    , model_file = model
    , id = id
    , condition = condition
  )

  # Homogeneity tests
  homogeneity_tests <- dplyr::bind_rows(
    lapply(X = prepared$freq_list, FUN = function(x) {
      tmp <- TreeBUGS::testHetChi(freq = x, tree = prepared$trees)
      tibble::tibble(
        chisq = tmp$chisq
        , df = tmp$df
        , p = tmp$prob
      )
    })
    , .id = "condition"
  )

  res <- list()

  if(any(method %in% c("asymptotic_complete"))) {
    res[["complete_pooling"]] <- mpt_mptinr_complete(
      dataset = dataset
      , prepared = prepared
      , model = model
      , method = intersect(method, "asymptotic_complete")
      , id = id
      , condition = condition
      , core = core
    )
  }

  if(any(method %in% c("asymptotic_no", "pb_no", "npb_no"))) {
    res[["no_pooling"]] <- mpt_mptinr_no(
      dataset = dataset
      , prepared = prepared
      , model = model
      , method = intersect(method, c("asymptotic_no", "pb_no", "npb_no"))
      , id = id
      , condition = condition
      , core = core
    )
  }

  for(i in seq_len(length(res))) {
    if (nrow(res[[i]]) > 0)
      res[[i]]$test_homogeneity[[1]] <- homogeneity_tests
  }

  # return
  dplyr::bind_rows(res)
}


test_between_no <- function(test_between, indiv_pars, CI_SIZE, cols_ci) {
  for (i in seq_len(nrow(test_between))) {
    # all these should be character
    tmp_par <- test_between$parameter[i]
    tmp_c1 <- test_between$condition1[i]
    tmp_c2 <- test_between$condition2[i]

    tmp_df <- indiv_pars %>%
      dplyr::filter(
        .data$identifiable,
        .data$parameter == tmp_par,
        .data$condition %in%
          c(tmp_c1, tmp_c2)
      )

    ## skip calculation in case of only one observation per group
    try({

      tmp_t <- stats::t.test(tmp_df[ tmp_df$condition == tmp_c1,  ]$est,
                             tmp_df[ tmp_df$condition == tmp_c2,  ]$est)

      tmp_lm <- stats::lm(est ~ condition, tmp_df)

      tmp_se <- stats::coef(stats::summary.lm(tmp_lm))[2,"Std. Error"]

      test_between[ i , "est_diff"] <- diff(rev(tmp_t$estimate))
      test_between[ i , "se"] <- tmp_se
      test_between[ i , "p" ] <- tmp_t$p.value

      for (j in seq_along(cols_ci)) {
        test_between[i, cols_ci[j]] <- test_between[i, ]$est_diff +
          stats::qnorm(CI_SIZE[j])* test_between[i, ]$se
      }


    }, silent = TRUE)
  }
  return(test_between)
}

test_within_no <- function(test_within, est_indiv) {

  tmp_data <- est_indiv %>%
    dplyr::filter(.data$identifiable) %>%
    tidyr::pivot_wider(id_cols = c("id", "condition"), values_from = "est", names_from = "parameter")

  for (i in seq_len(nrow(test_within))) {

    c  <- test_within$condition[i]
    p1 <- test_within$parameter1[i]
    p2 <- test_within$parameter2[i]

    condition_data <-tmp_data[tmp_data$condition == c, , drop = FALSE]
    if(nrow(stats::na.omit(condition_data))>1) { # only calculate test if N > 1
      # two-sided *p* values
      t_out <- stats::t.test(x = condition_data[[p1]], y = condition_data[[p2]], paired = TRUE)
      test_within$est[i] <- t_out$estimate
      if(getRversion() >= "3.6.0") {
        test_within$se[i] <- t_out$stderr
      }
      test_within$statistic[i] <- t_out$statistic
      test_within$df[i] <- unname(t_out$parameter)
      test_within$p[i] <- t_out$p.value
    }
  }
  # confidence intervals from normal theory
  if(getRversion() >= "3.6.0") {
    CI <- getOption("MPTmultiverse")$ci_size
    for (i in seq_along(CI)) {
      test_within[[paste0("ci_", CI[i])]] <- stats::qt(p = CI[i], df = test_within$df) * test_within$se + test_within$est
    }
  }

  test_within
}

################
## no pooling ##
################

#' @importFrom tibble tibble
#' @importFrom rlang .data sym
#' @importFrom magrittr %>%
#' @importFrom stats qnorm
#' @keywords internal

mpt_mptinr_no <- function(
  dataset
  , prepared
  , model
  , method
  , id
  , condition
  , core = NULL
) {

  OPTIONS <- getOption("MPTmultiverse")
  MPTINR_OPTIONS <- OPTIONS$mptinr
  CI_SIZE <- OPTIONS$ci_size
  MAX_CI_INDIV <- OPTIONS$max_ci_indiv

  bootstrap <- gsub(intersect(method, c("pb_no", "npb_no")), pattern = "_no", replacement = "")

  t0 <- Sys.time()
  fit_mptinr <- MPTinR::fit.mpt(prepared$data[,prepared$col_freq],
                        model.filename = model,
                        n.optim = max(3L, MPTINR_OPTIONS$n.optim),
                        fit.aggregated = FALSE,
                        show.messages = FALSE, output = "full",
                        ci = (1 - stats::pnorm(1))*2*100)
  t1 <- Sys.time()
  additional_time <- difftime(t1, t0, units = "secs")

  convergence <- tibble::tibble(
    id = prepared$data[[id]]
    , condition = as.character(prepared$data[[condition]])
    , rank.fisher = fit_mptinr$model.info$individual$rank.fisher
    , n.parameters = fit_mptinr$model.info$individual$n.parameters
    , convergence = vapply(fit_mptinr$best.fits$individual,
                           FUN = function(x) x$convergence, FUN.VALUE = 0)
  )

  res <- vector("list", length(method))
  names(res) <- method

  if ("asymptotic_no" %in% method) {

    res[["asymptotic_no"]] <- make_results_row(model = model,
                                               dataset = dataset,
                                               pooling = "no",
                                               package = "MPTinR",
                                               method = "asymptotic",
                                               data = prepared$data,
                                               # parameters = prepared$parameters,
                                               id = id,
                                               condition = condition,
                                               core = core)



    res[["asymptotic_no"]]$gof_indiv[[1]]$type <- "G2"
    res[["asymptotic_no"]]$gof_indiv[[1]]$focus <- "mean"
    res[["asymptotic_no"]]$gof_indiv[[1]]$stat_obs <-
      fit_mptinr$goodness.of.fit$individual$G.Squared
    res[["asymptotic_no"]]$gof_indiv[[1]]$stat_df <-
      fit_mptinr$goodness.of.fit$individual$df
    res[["asymptotic_no"]]$gof_indiv[[1]]$p <-
      fit_mptinr$goodness.of.fit$individual$p.value


    ## make est_indiv and gof_indiv
    for (i in seq_len(nrow(prepared$data))) {

      ## get results from fitting runs to check if parameters are identified
      all_pars <- do.call(
        "rbind"
        , purrr::map(fit_mptinr$optim.runs$individual[[i]], "par")
      )
      colnames(all_pars) <- dimnames(fit_mptinr$parameters$individual)[[1]]
      all_pars <- as.data.frame(all_pars)
      all_pars$objective <- purrr::map_dbl(fit_mptinr$optim.runs$individual[[i]], "objective")
      all_pars$minimum <- abs(all_pars$objective - min(all_pars$objective)) < 0.01

      for (p in prepared$parameters) {

        res[["asymptotic_no"]]$est_indiv[[1]][
          res[["asymptotic_no"]]$est_indiv[[1]]$id == prepared$data[i,"id"] &
            res[["asymptotic_no"]]$est_indiv[[1]]$parameter == p, "est" ] <-
          fit_mptinr$parameters$individual[p,"estimates",i]

        res[["asymptotic_no"]]$est_indiv[[1]][
          res[["asymptotic_no"]]$est_indiv[[1]]$id == prepared$data[i,"id"] &
            res[["asymptotic_no"]]$est_indiv[[1]]$parameter == p, "se" ] <-
          fit_mptinr$parameters$individual[p, "upper.conf",i] -
          fit_mptinr$parameters$individual[p,"estimates",i]

        ## check if individual parameter is identifiable and flag otherwise
        pars_at_optimum <- all_pars[all_pars$minimum, p]
        if (length(pars_at_optimum) > 1) {
          if (all(abs(pars_at_optimum[1] - pars_at_optimum) < 0.01)) {
            res[["asymptotic_no"]]$est_indiv[[1]][
          res[["asymptotic_no"]]$est_indiv[[1]]$id == prepared$data[i,"id"] &
            res[["asymptotic_no"]]$est_indiv[[1]]$parameter == p, "identifiable" ] <-
              TRUE
          } else {
            res[["asymptotic_no"]]$est_indiv[[1]][
          res[["asymptotic_no"]]$est_indiv[[1]]$id == prepared$data[i,"id"] &
            res[["asymptotic_no"]]$est_indiv[[1]]$parameter == p, "identifiable" ] <-
              FALSE
          }
        }

      }
    }

    for (i in seq_along(CI_SIZE)) {
      res[["asymptotic_no"]]$est_indiv[[1]][, prepared$cols_ci[i]] <-
        res[["asymptotic_no"]]$est_indiv[[1]][,"est"] +
        stats::qnorm(CI_SIZE[i])*res[["asymptotic_no"]]$est_indiv[[1]][,"se"]
    }

    #### make est_group ####

    est_group2 <- res[["asymptotic_no"]]$est_indiv[[1]] %>%
      dplyr::filter(.data$identifiable | is.na(.data$identifiable)) %>%
      dplyr::group_by(.data$condition, .data$parameter, .data$core) %>%
      dplyr::summarise(estN = mean(.data$est),
                       se = stats::sd(.data$est) /
                         sqrt(sum(!is.na(.data$est)))) %>%
      dplyr::ungroup() %>%
      dplyr::rename(est = .data$estN)
    for (i in seq_along(CI_SIZE)) {
      est_group2[, prepared$cols_ci[i]] <- est_group2[,"est"] +
        stats::qnorm(CI_SIZE[i])*est_group2[,"se"]
    }

    res[["asymptotic_no"]]$est_group[[1]] <- dplyr::right_join(
      est_group2, res[["asymptotic_no"]]$est_group[[1]][,c("condition",
                                                           "parameter")],
      by = c("condition", "parameter"))


    # ----------------------------------------------------------------------------
    # make gof_group for asymptotic approach

    res[["asymptotic_no"]]$gof_group[[1]]$type <- "G2"
    res[["asymptotic_no"]]$gof_group[[1]]$focus <- "mean"

    tmp <- fit_mptinr$goodness.of.fit$individual
    tmp$condition <- factor(as.character(prepared$data$condition),
                            levels = prepared$conditions)
    ## creation of factor above (which is reverted below) ensures that output
    ## has same ordering as gof_group for other messages
    gof_group2 <- tmp %>%
      dplyr::group_by(.data$condition) %>%
      dplyr::summarise(stat_obs = sum(.data$G.Squared),
                       stat_pred = NA_real_,
                       stat_df = sum(.data$df))

    gof_group2$p <- stats::pchisq(
      q = gof_group2$stat_obs
      , df = gof_group2$stat_df
      , lower.tail = FALSE
    )
    gof_group2$condition <- as.character(gof_group2$condition)
    # ensure that factor levels fit: not necessary because we switched to character
    # gof_group2$condition <- factor(
    #   gof_group2$condition
    #   , levels = levels(res[["asymptotic_no"]]$gof_group[[1]]$condition)
    # )

    res[["asymptotic_no"]]$gof_group[[1]] <-
      dplyr::right_join(res[["asymptotic_no"]]$gof_group[[1]][, c("condition", "type", "focus")],
                        gof_group2,
                        by = c("condition"))


    # ----------------------------------------------------------------------------
    # make gof

    gof <- tibble::tibble(
      type = "G2"
      , focus = "mean"
      , stat_obs = fit_mptinr$goodness.of.fit$sum$G.Squared
      , stat_pred = NA_real_
      , stat_df = fit_mptinr$goodness.of.fit$sum$df
      , p = NA_real_
    )

    # Calculate *p* value from Chi-squared distribution
    res[["asymptotic_no"]]$gof[[1]] <- gof
    res[["asymptotic_no"]]$gof[[1]]$p <- stats::pchisq(
      q = res[["asymptotic_no"]]$gof[[1]]$stat_obs
      , df = res[["asymptotic_no"]]$gof[[1]]$stat_df
      , lower.tail = FALSE
    )

    # ----------------------------------------------------------------------------
    # make test_between

    res[["asymptotic_no"]]$test_between[[1]] <-
      test_between_no(test_between = res[["asymptotic_no"]]$test_between[[1]],
                      indiv_pars = res[["asymptotic_no"]]$est_indiv[[1]],
                      CI_SIZE = CI_SIZE, cols_ci = prepared$cols_ci)

    # test_within ---------------------------------------------------------
    res[["asymptotic_no"]]$test_within[[1]] <-
      test_within_no(
        test_within = res[["asymptotic_no"]]$test_within[[1]],
        est_indiv = res[["asymptotic_no"]]$est_indiv[[1]]
      )

    ### copy information that is same ----

    res[["asymptotic_no"]]$convergence <- list(convergence)
    # res[["asymptotic_no"]]$test_between <- res[["pb_no"]]$test_between # bugfix--MB--2018-10-14

    # write estimation time to results_row ----
    res[["asymptotic_no"]]$estimation[[1]] <- tibble::tibble(
      condition = "individual"
      , time_difference = additional_time
    )

  }


  if ("pb" %in% bootstrap) {
    try(res[["pb_no"]] <- get_pb_results(dataset = dataset
                                     , prepared = prepared
                                     , model = model
                                     , id = id
                                     , condition = condition
                                     , bootstrap = "pb"
                                     , fit_mptinr = fit_mptinr
                                     , additional_time = additional_time
                                     , convergence = convergence
                                     , core = core))
  }
  if ("npb" %in% bootstrap) {
    try(res[["npb_no"]] <- get_pb_results(dataset = dataset
                                     , prepared = prepared
                                     , model = model
                                     , id = id
                                     , condition = condition
                                     , bootstrap = "npb"
                                     , fit_mptinr = fit_mptinr
                                     , additional_time = additional_time
                                     , convergence = convergence
                                     , core = core))
  }
  # return
  dplyr::bind_rows(res)
}

get_pb_results <- function(dataset
  , prepared
  , model
  , id
  , condition
  , bootstrap
  , fit_mptinr
  , additional_time
  , convergence
  , core) {

  OPTIONS <- getOption("MPTmultiverse")
  MPTINR_OPTIONS <- OPTIONS$mptinr
  CI_SIZE <- OPTIONS$ci_size
  MAX_CI_INDIV <- OPTIONS$max_ci_indiv

  # only create a cluster if no default cluster is set
  if(is.null(parallel::getDefaultCluster())) {
    cl <- parallel::makeCluster(rep("localhost", OPTIONS$n.CPU))
  } else {
    cl <- parallel::getDefaultCluster()
  }

  parallel::clusterEvalQ(cl, library("MPTinR"))
  parallel::clusterSetRNGStream(cl, iseed = sample.int(.Machine$integer.max, 1))

  if (bootstrap == "pb") {
    res <- make_results_row(
      model = model
      , dataset = dataset
      , pooling = "no"
      , package = "MPTinR"
      , method = "PB/MLE"
      , data = prepared$data
      # , parameters = prepared$parameters
      , id = id
      , condition = condition
      , core = core
    )
    t1 <- Sys.time()
    fit_pb <- parallel::clusterApplyLB(
      cl
      , x = seq_len(nrow(prepared$data))
      , fun = get_pb_output
      , fit_mptinr = fit_mptinr
      , data = prepared$data
      , model_file = model
      , col_freq = prepared$col_freq
      , MPTINR_OPTIONS = MPTINR_OPTIONS
    )
    t2 <- Sys.time()
  }
  if (bootstrap == "npb") {
    res <- make_results_row(
      model = model
      , dataset = dataset
      , pooling = "no"
      , package = "MPTinR"
      , method = "NPB/MLE"
      , data = prepared$data
      # , parameters = prepared$parameters
      , id = id
      , condition = condition
      , core = core
    )
    t1 <- Sys.time()
    fit_pb <- parallel::clusterApplyLB(
      cl
      , seq_len(nrow(prepared$data))
      , get_npb_output
      , fit_mptinr = fit_mptinr
      , data = prepared$data
      , model_file = model
      , col_freq = prepared$col_freq
      , MPTINR_OPTIONS = MPTINR_OPTIONS
    )
    t2 <- Sys.time()
  }

  res$gof_indiv[[1]]$type <- "G2"
  res$gof_indiv[[1]]$focus <- "mean"
  res$gof_indiv[[1]]$stat_obs <- fit_mptinr$goodness.of.fit$individual$G.Squared
  res$gof_indiv[[1]]$stat_df <- fit_mptinr$goodness.of.fit$individual$df

  ## make est_indiv and gof_indiv
  for (i in seq_len(nrow(prepared$data))) {

    for (p in prepared$parameters) {
      res$est_indiv[[1]][
        res$est_indiv[[1]]$id == prepared$data[i,"id"] &
          res$est_indiv[[1]]$parameter == p, "est" ] <-
        fit_mptinr$parameters$individual[p,"estimates",i]

      tmp_ci <- stats::quantile(fit_pb[[i]]$parameters$individual[p,"estimates",], probs = CI_SIZE)
      for (cin in seq_along(prepared$cols_ci)) {
        res$est_indiv[[1]][
        res$est_indiv[[1]]$id == prepared$data[i,"id"] &
          res$est_indiv[[1]]$parameter == p,  prepared$cols_ci[cin]] <-
          tmp_ci[cin]
      }

      res$est_indiv[[1]][
        res$est_indiv[[1]]$id == prepared$data[i,"id"] &
          res$est_indiv[[1]]$parameter == p, "se" ] <-
        stats::sd(fit_pb[[i]]$parameters$individual[p,"estimates",])
    }
    # gof_indiv
    res$gof_indiv[[1]][
      res$gof_indiv[[1]]$id == prepared$data[i,"id"], "p" ] <-
      (sum(fit_pb[[i]]$goodness.of.fit$individual$G.Squared >
             fit_mptinr$goodness.of.fit$individual[i,"G.Squared"]) + 1) /
      (MPTINR_OPTIONS$bootstrap_samples + 1)

  }

  ## check for identifiability

  tmp_range_ci <- res$est_indiv[[1]][, prepared$cols_ci[length(prepared$cols_ci)]][[1]] -
    res$est_indiv[[1]][, prepared$cols_ci[1]][[1]]
  res$est_indiv[[1]]$identifiable <- tmp_range_ci < MAX_CI_INDIV

  #### make est_group ####

  non_identified_pars <-  res$est_indiv[[1]] %>%
    dplyr::filter(!.data$identifiable) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(parameter = paste0(.data$parameter, collapse = ", ")) %>%
    dplyr::ungroup()

  res$convergence <-
    list(tibble::as_tibble(dplyr::left_join(convergence, non_identified_pars, by = "id")))

  if (nrow(non_identified_pars) > 0) {
    warning("MPTinR-no: IDs and parameters with ", bootstrap, "-CIs > ",
            MAX_CI_INDIV, " (i.e., non-identified):\n",
            apply(non_identified_pars,
                  1, function(x) paste0(x[["id"]], ": ", x[["parameter"]], "\n") ),
            call. = FALSE)
  }

  est_group <- res$est_indiv[[1]] %>%
    dplyr::filter(.data$identifiable) %>%  ## exclude non-identified pars.
    dplyr::group_by(.data$condition, .data$parameter, .data$core) %>%
    dplyr::summarise(estN = mean(.data$est),
                     se = stats::sd(.data$est) /
                       sqrt(sum(!is.na(.data$est)))) %>%
    dplyr::ungroup() %>%
    dplyr::rename(est = .data$estN)
  for (i in seq_along(CI_SIZE)) {
    est_group[, prepared$cols_ci[i]] <- est_group[,"est"] +
      stats::qnorm(CI_SIZE[i])*est_group[,"se"]
  }

  res$est_group[[1]] <-
    dplyr::right_join(est_group,
                      res$est_group[[1]][, c("condition", "parameter")],
                      by = c("condition", "parameter"))

  # ----------------------------------------------------------------------------
  # make test_between

  res$test_between[[1]] <-
    test_between_no(test_between = res$test_between[[1]],
                    indiv_pars = res$est_indiv[[1]],
                      CI_SIZE = CI_SIZE, cols_ci = prepared$cols_ci)


  # test_within ----------------------------------------------------------------
  res$test_within[[1]] <- test_within_no(
    test_within = res$test_within[[1]]
    , est_indiv = res$est_indiv[[1]]
  )

  # ----------------------------------------------------------------------------
  # make gof_group for parametric-bootstrap approach

  res$gof_group[[1]]$type <- paste0(bootstrap, "-G2")
  res$gof_group[[1]]$focus <- "mean"

  tmp <- fit_mptinr$goodness.of.fit$individual
  tmp$condition <- as.character(prepared$data$condition)
  gof_group <- tmp %>%
    dplyr::group_by(.data$condition) %>%
    dplyr::summarise(stat_obs = sum(.data$G.Squared),
              stat_df = sum(.data$df))
  gof_group$p <- NA_real_

  g2_all <- vapply(fit_pb,
                   function(x) x$goodness.of.fit$individual$G.Squared,
                   rep(0, MPTINR_OPTIONS$bootstrap_samples))

  g2_cond <- vector("list", length(prepared$conditions))

  for (i in seq_along(prepared$conditions)) {
    g2_cond[[i]] <- apply(g2_all[ ,
                                 prepared$data$condition ==
                                   prepared$conditions[i]], 1, sum)
    res$gof_group[[1]][
      res$gof_group[[1]]$condition ==
        prepared$conditions[i], "stat_obs" ] <-
      gof_group[ gof_group$condition == prepared$conditions[i], "stat_obs"]
    res$gof_group[[1]][ res$gof_group[[1]]$condition ==
                                 prepared$conditions[i], "stat_df" ] <-
      gof_group[ gof_group$condition == prepared$conditions[i], "stat_df"]
    res$gof_group[[1]][ res$gof_group[[1]]$condition ==
                                 prepared$conditions[i], "p" ] <-
      (sum(gof_group[ gof_group$condition ==
                        prepared$conditions[i], "stat_obs"][[1]] <
             g2_cond[[i]]) + 1) / (MPTINR_OPTIONS$bootstrap_samples + 1)
  }

  gof <- tibble::tibble(
    type = "G2"
    , focus = "mean"
    , stat_obs = fit_mptinr$goodness.of.fit$sum$G.Squared
    , stat_pred = NA_real_
    , stat_df = fit_mptinr$goodness.of.fit$sum$df
    , p = NA_real_
  )

  # Calculate *p* value from distribution of bootstrapped G2 values
  res$gof[[1]] <- gof

  g2_all <- vapply(
    X = fit_pb
    , FUN = function(x) x$goodness.of.fit$individual$G.Squared
    , FUN.VALUE = rep(0, MPTINR_OPTIONS$bootstrap_samples)
  )
  g2_cond <- apply(
    X = g2_all
    , MARGIN = 1
    , FUN = sum
  )

  res$gof[[1]]$p <-
    (sum(res$gof[[1]]$stat_obs < g2_cond) + 1) /
    (MPTINR_OPTIONS$bootstrap_samples + 1)


   # write estimation time to results_row ----
  res$estimation[[1]] <- tibble::tibble(
    condition = "individual"
    , time_difference = (t2 - t1) + additional_time
  )

  parallel::stopCluster(cl)

  return(res)

}



#' Parametric Bootstrap for MPT
#'
#' Helper function for creating parametric-bootstrap confidence intervals.
#'
#' @keywords internal

get_pb_output <- function(
  i
  , fit_mptinr
  , data
  , model_file
  , col_freq
  , MPTINR_OPTIONS) {

  gen_data <- MPTinR::gen.data(
    fit_mptinr$parameters$individual[,"estimates", i]
    , samples = MPTINR_OPTIONS$bootstrap_samples
    , model.filename = model_file
    , data = unlist(data[i, col_freq])
  )
  MPTinR::fit.mpt(gen_data,
            model.filename = model_file,
            fit.aggregated = FALSE,
            n.optim = MPTINR_OPTIONS$n.optim,
            show.messages = FALSE)
}

get_npb_output <- function(
  i
  , fit_mptinr
  , data
  , model_file
  , col_freq
  , MPTINR_OPTIONS) {

  gen_data <- MPTinR::sample.data(
    data = unlist(data[i, col_freq])
    , samples = MPTINR_OPTIONS$bootstrap_samples
    , model.filename = model_file
  )
  MPTinR::fit.mpt(gen_data,
            model.filename = model_file,
            fit.aggregated = FALSE,
            n.optim = MPTINR_OPTIONS$n.optim,
            show.messages = FALSE)
}


# ------------------------------------------------------------------------------
# Complete-pooling (maximum-likelihood) approach


#' @importFrom magrittr %>%
#' @keywords internal

mpt_mptinr_complete <- function(dataset,
                                prepared,
                                model,
                                method,
                                id,
                                condition,
                                core = NULL) {
  OPTIONS <- getOption("MPTmultiverse")
  MPTINR_OPTIONS <- OPTIONS$mptinr
  CI_SIZE <- OPTIONS$ci_size
  MAX_CI_INDIV <- OPTIONS$max_ci_indiv


  res <- make_results_row(
    model = model
    , dataset = dataset
    , pooling = "complete"
    , package = "MPTinR"
    , method = "asymptotic"
    , data = prepared$data
    # , parameters = prepared$parameters
    , id = id
    , condition = condition
    , core = core
  )

  res$est_indiv <- list(tibble::tibble())
  res$gof_indiv <- list(tibble::tibble())

  #### fully aggregated:

  t0 <- Sys.time()
  fit_mptinr_agg <- MPTinR::fit.mpt(colSums(prepared$data[,prepared$col_freq]),
                            model.filename = model,
                            n.optim = MPTINR_OPTIONS$n.optim,
                            show.messages = FALSE, output = "full")

  res$estimation[[1]]$time_difference[
    res$estimation[[1]]$condition == "complete_data"
  ] <- difftime(Sys.time(), t0, units = "secs")

  ## gof

  res$gof[[1]][1,"type"] <- "G2"
  res$gof[[1]][1,"focus"] <- "mean"

  res$gof[[1]][1, c("stat_obs", "stat_df", "p")] <-
    fit_mptinr_agg$goodness.of.fit[, c("G.Squared", "df", "p.value")]


  #### aggregated by condition

  res$gof_group[[1]][, "type"] <- "G2"
  res$gof_group[[1]][, "focus"] <- "mean"

  convergence <- vector("list", 1 + length(prepared$conditions))
  names(convergence) <- c("aggregated", prepared$conditions)

  convergence$aggregated <- tibble::tibble(
    rank.fisher = fit_mptinr_agg$model.info$rank.fisher
    , n.parameters = fit_mptinr_agg$model.info$n.parameters
    , convergence = fit_mptinr_agg$best.fits[[1]]$convergence
  )

  variance_covariance_matrices <- vector(mode = "list", length = length(prepared$conditions))
  names(variance_covariance_matrices) <- prepared$conditions
  for (i in seq_along(prepared$conditions)) {
    t0 <- Sys.time()
    fit_mptinr_tmp <- MPTinR::fit.mpt(colSums(
      prepared$freq_list[[prepared$conditions[i]]][,prepared$col_freq]),
                              model.filename = model,
                              n.optim = MPTINR_OPTIONS$n.optim,
                              show.messages = FALSE,
                              output = "full")

    res$estimation[[1]]$time_difference[
      res$estimation[[1]]$condition == prepared$conditions[i]
      ] <- difftime(Sys.time(), t0, units = "secs")

    res$gof_group[[1]][
      res$gof_group[[1]]$condition ==
        prepared$conditions[i] , c("stat_obs", "stat_df", "p")] <-
      fit_mptinr_tmp$goodness.of.fit[, c("G.Squared", "df", "p.value")]

    res$est_group[[1]][
      res$est_group[[1]]$condition == prepared$conditions[i], "est"] <-
      fit_mptinr_tmp$parameters[
        res$est_group[[1]][res$est_group[[1]]$condition == prepared$conditions[i], ]$parameter, "estimates"]

    variance_covariance <- tryCatch(
      solve(fit_mptinr_tmp$hessian[[1]])
      , error = function(x){
        limSolve::Solve(fit_mptinr_tmp$hessian[[1]])
      })
    par_se <- rep(NA_real_, length(rownames(fit_mptinr_tmp$parameters)))
    par_se <- sqrt(diag(variance_covariance))
    rownames(variance_covariance) <-
      colnames(variance_covariance) <-
      names(par_se) <-
      rownames(fit_mptinr_tmp$parameters)
    variance_covariance_matrices[[prepared$conditions[i]]] <- variance_covariance

    res$est_group[[1]][
      res$est_group[[1]]$condition ==
        prepared$conditions[i], "se"
      ] <- par_se[
        res$est_group[[1]][
          res$est_group[[1]]$condition ==
            prepared$conditions[i],
          ]$parameter]

    convergence[[prepared$conditions[i]]] <- tibble::tibble(
      rank.fisher = fit_mptinr_tmp$model.info$rank.fisher
      , n.parameters = fit_mptinr_tmp$model.info$n.parameters
      , convergence = fit_mptinr_tmp$best.fits[[1]]$convergence
    )
  }

  for (i in seq_along(CI_SIZE)) {
    res$est_group[[1]][, prepared$cols_ci[i]] <-
      res$est_group[[1]][,"est"] +
      stats::qnorm(CI_SIZE[i])*res$est_group[[1]][,"se"]
  }

  # -----------------------------------------------------------------------
  # test_between
  est_group <- res$est_group[[1]]
  test_between <- res$test_between[[1]]

  for (i in seq_len(nrow(test_between))) {

    p <- test_between$parameter[i]
    c1 <- test_between$condition1[i]
    c2 <- test_between$condition2[i]

    test_between[i, "est_diff"] <-
      est_group[est_group$condition == c1 & est_group$parameter == p, ]$est -
      est_group[est_group$condition == c2 & est_group$parameter == p, ]$est

    # standard errors
    test_between[i, "se"] <- sqrt(
      (est_group[est_group$condition == c1 & est_group$parameter == p, ]$se)^2 +
      (est_group[est_group$condition == c2 & est_group$parameter == p, ]$se)^2
    )
  }

  # CIs from standard errors
  for(k in CI_SIZE) {
    test_between[[paste0("ci_", k)]] <- test_between$est_diff +
      test_between$se * qnorm(p = k)
  }

  res$test_between[[1]] <- test_between



  # test_within -----------------------------------------------------------
  test_within <- res$test_within[[1]]

  for(i in seq_len(nrow(test_within))) {
    c <- test_within$condition[i]
    p1 <- test_within$parameter1[i]
    p2 <- test_within$parameter2[i]

    test_within$est[i] <-
      est_group$est[est_group$condition == c & est_group$parameter == p1] -
      est_group$est[est_group$condition == c & est_group$parameter == p2]

    vec <- rep(0, nrow(variance_covariance_matrices[[c]]))
    names(vec) <- rownames(variance_covariance_matrices[[c]])
    vec[p1] <- +1
    vec[p2] <- -1
    # print(vec)

    test_within$se[i] <- sqrt(
      vec %*% variance_covariance_matrices[[c]] %*% vec
    )
  }
  # two-sided *p* values
  test_within$statistic <- test_within$est / test_within$se
  test_within$p <- 1 - abs(stats::pnorm(q = test_within$est, mean = 0, sd = test_within$se) - .5) * 2

  # confidence intervals from normal theory
  CI <- getOption("MPTmultiverse")$ci_size
  for (i in seq_along(CI)) {
    test_within[[paste0("ci_", CI[i])]] <- stats::qnorm(p = CI[i], mean = test_within$est, sd = test_within$se)
  }


  res$test_within[[1]] <- test_within


  # ----------------------------------------------------------------------------
  # convergence

  tmp <- names(convergence)
  convergence <- do.call("rbind", convergence)
  convergence <- dplyr::bind_cols(condition = tmp, convergence)

  res$convergence <- list(convergence)
  warn_conv <- convergence$convergence != 0
  if (any(warn_conv)) {
    warning("MPTinR-complete: Convergence code != 0 for: ",
            paste0(names(warn_conv)[warn_conv], collapse = ", "),
            call. = FALSE)
  }

  # res$estimation[[1]] <- tibble::tibble(
  #   condition = c("complete_data", names(t_cond))
  #   , time_difference = as.difftime(c(t_complete_data, unlist(t_cond)))
  # )

  return(res)
}
