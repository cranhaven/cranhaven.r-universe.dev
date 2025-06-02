
# overall goodness of fit across between-subject conditions
aggregate_ppp <- function(ppp_list, stat = "T1"){
  obs <- vapply(ppp_list, "[[", paste0(stat, ".obs"),
                FUN.VALUE = ppp_list[[1]]$T1.obs)
  pred <- vapply(ppp_list, "[[", paste0(stat, ".pred"),
                 FUN.VALUE = ppp_list[[1]]$T1.obs)
  s.obs <- rowSums(obs)
  s.pred <- rowSums(pred)
  c(stat_obs = mean(s.obs), stat_pred = mean(s.pred),
    stat_df = NA, p = mean(s.obs < s.pred))
}

#' @importFrom rlang .data
#' @importFrom magrittr %>%

mpt_treebugs <- function (
  method
  , dataset
  , data
  , model
  , id = "id"
  , condition = "condition"
  , core = NULL
){
  all_options <- getOption("MPTmultiverse")

  TREEBUGS_MCMC <- all_options$treebugs
  CI_SIZE <- all_options$ci_size

  # dlist <- prepare_data(model, data, id = "id", condition = "condition")
  conditions <- unique(data[[condition]])
  parameters <- as.character(MPTinR::check.mpt(model)$parameters)
  col_freq <- get_eqn_categories(model)

  data$id <- data[[id]]
  data$condition <- data[[condition]]

  freq_list <- split(data[, col_freq, drop = FALSE], f = data[[condition]])
  pooling <- switch(method,
                    "simple" = "no",
                    "simple_pooling" = "complete",
                    "partial")

  result_row <- make_results_row(model = model,
                                 dataset = dataset,
                                 pooling = pooling,
                                 package = "TreeBUGS",
                                 method = sub("_pooling","", method, fixed = TRUE),
                                 data = data,
                                 # parameters = parameters,
                                 id = id,
                                 condition = condition,
                                 core = core)

  # Homogeneity tests
  result_row$test_homogeneity[[1]] <- dplyr::bind_rows(
    lapply(X = freq_list, FUN = function(x) {
      tmp <- TreeBUGS::testHetChi(freq = x, tree = get_eqn_trees(model_file = model))
      tibble::tibble(
        chisq = tmp$chisq
        , df = tmp$df
        , p = tmp$prob
      )
    })
    , .id = "condition"
  )


  if (method == "simple_pooling"){
    method <- "simple"

    # pooling: aggregate across participants
    data <- stats::aggregate(data[, col_freq], list(condition = data$condition), sum)
    data[[condition]] <- data$condition
    data[[id]] <- data$id <- as.character(1:nrow(data))
    if(condition != "condition"){
      data$condition <- NULL
    }

    freq_list <- lapply(freq_list, function(x) as.matrix(colSums(x)))
  }

  # ----------------------------------------------------------------------------
  # Customize prior if necessary
  method_for_printing <- method    # save method for later printing
  if (method == "trait_uncorrelated"){
    method_for_printing <- method
    method <- "trait"
    prior_args <- list(df = 1, V = NA, xi = "dnorm(0,1)")
  } else {
    prior_args <- NULL
  }

  if (method == "beta") {
    prior_args <- list(
      alpha = TREEBUGS_MCMC$prior.beta,
      beta = TREEBUGS_MCMC$prior.beta
    )
  }

  # ----------------------------------------------------------------------------
  # Run MCMC function from TreeBUGS
  gof_group <- list()
  treebugs_fit <- list()

  for (i in seq_along(conditions)){
    cond <- conditions[i]
    sel_condition <- data[[condition]] == conditions[i]
    data_group <- data[sel_condition, col_freq]   #freq_list[[i]]
    rownames(data_group) <- data[[id]][sel_condition]

    fit_args <- list(eqnfile = model,
                     data = data_group,
                     n.chains = TREEBUGS_MCMC$n.chains,
                     n.iter = TREEBUGS_MCMC$n.iter,
                     n.adapt = TREEBUGS_MCMC$n.adapt,
                     n.burnin = TREEBUGS_MCMC$n.burnin,
                     n.thin = TREEBUGS_MCMC$n.thin)
    if (method == "betacpp" && gsub(TREEBUGS_MCMC$prior.beta, pattern = " |\t", replacement = "") != "dgamma(1,.1)")
      stop(
        'betacpp not compatible with custom priors as defined via mpt_options(prior.beta = "',
        TREEBUGS_MCMC$prior.beta,
        '")'
      )
    if (method %in% c("simple", "betacpp")){
      fit_args$n.adapt <- fit_args$alpha <- fit_args$beta <- NULL
      fit_args <- c(fit_args, cores = unname(all_options$n.CPU))
    }


    t0 <- Sys.time()
    treebugs_function <- ifelse(method == "betacpp",
                                "TreeBUGS::betaMPTcpp",
                                paste0("TreeBUGS::", method, "MPT"))
    treebugs_fit[[i]] <- do.call(eval(parse(text = treebugs_function)),
                                 args = c(fit_args, prior_args))
    summ <- treebugs_fit[[i]]$mcmc.summ

    # --------------------------------------------------------------------------
    # adaptively continue MCMC sampling (only available for betaMPT and traitMPT)
    if(method %in% c("beta", "trait")) {
      ext_cnt <- 0L
      try({
        repeat{
          min_neff <- min(summ[summ[, "n.eff"] > 0, "n.eff"], na.rm = TRUE)
          max_rhat <- max(summ[, "Rhat"], na.rm = TRUE)

          if(ext_cnt == TREEBUGS_MCMC$extend_max) break
          if(min_neff >= TREEBUGS_MCMC$Neff_min & max_rhat <= (TREEBUGS_MCMC$Rhat_max*2-1)) break

          cat(
            "Drawing additional samples for method \"", method_for_printing, "\".\n"
            , "max(Rhat) = ", round(max_rhat, digits = 3L), ", "
            , "min(n.eff) = ", round(min_neff, digits = 0L), "\n"
            , if(max_rhat <= TREEBUGS_MCMC$Rhat_max){"Combining with"}else{"Discarding"}
            , " previously drawn samples.\n"
            , sep = ""
          )
          treebugs_fit[[i]] <- TreeBUGS::extendMPT(
            treebugs_fit[[i]],
            n.iter = TREEBUGS_MCMC$n.iter,
            n.adapt = TREEBUGS_MCMC$n.adapt,
            # combine only if we already reached stationary distribution:
            combine = max_rhat <= TREEBUGS_MCMC$Rhat_max
          )
          summ <- treebugs_fit[[i]]$mcmc.summ
          ext_cnt <- ext_cnt + 1L
        }
      })
    }

    result_row$estimation[[1]]$time_difference[
      result_row$estimation[[1]]$condition == cond
    ] <- difftime(Sys.time(), t0, units = "secs")

    # test_within --------------------------------------------------------------
    test_within <- result_row$test_within[[1]]

    # create list with all transforms
    transform_pars <- as.list(apply(
      X = test_within[test_within$condition == conditions[i], c("parameter1", "parameter2")]
      , MARGIN = 1
      , FUN = function(x) {
      paste0(paste(c("diff", x), collapse = "__"), "=", x[1], "-", x[2])
    }))

    transformed_parameters <- TreeBUGS::transformedParameters(
      treebugs_fit[[i]]
      , transformedParameters = transform_pars
      , level = "group"
    )
    proportions <- apply(X = do.call("rbind", transformed_parameters), MARGIN = 2, FUN = function(x){mean(x <= 0)})
    tmp <- summary(
      transformed_parameters
      , quantiles = all_options$ci_size
    )

    pars <- strsplit(rownames(tmp$statistic), split = "__", fixed = TRUE)

    for(j in seq_len(nrow(tmp$statistics))) {
      diffname <- pars[[j]]
      p1 <- diffname[2]
      p2 <- diffname[3]

      idx <- which(
        test_within$condition == conditions[i] &
        test_within$parameter1 == p1 &
        test_within$parameter2 == p2)

      test_within$est[idx] <- tmp$statistics[j, "Mean"]
      test_within$se[idx] <- tmp$statistics[j, "SD"]
      test_within$p[idx] <- 1 - abs(proportions[rownames(tmp$statistic)[j]] - .5) * 2

      for (k in seq_along(all_options$ci_size)) { # comply with tibble 2.99
        test_within[idx, paste0("ci_", all_options$ci_size[k])] <-
          tmp$quantiles[rownames(tmp$statistic)[j], k, drop = TRUE]
      }
    }
    result_row$test_within[[1]] <- test_within


    # convergence summary (n.eff / Rhat / all estimates)
    tsum <- tibble::as_tibble(summ) %>%
      dplyr::mutate(parameter = rownames(summ),
             condition = as.character(cond)) %>%
      dplyr::select(.data$condition, .data$parameter, .data$Mean : .data$Rhat)

    result_row$convergence[[1]] <- dplyr::bind_rows(result_row$convergence[[1]], tsum)

    # parameter estimates
    summMPT <- TreeBUGS::summarizeMPT(treebugs_fit[[i]]$runjags$mcmc,
                                      mptInfo = treebugs_fit[[i]]$mptInfo,
                                      probs = CI_SIZE,
                                      summ = treebugs_fit[[i]]$mcmc.summ)

    sel_group <- result_row$est_group[[1]]$condition == conditions[i]

    for (k in 1:6) { # comply with tibble 2.99
      result_row$est_group[[1]][sel_group, 3 + k] <-
        summMPT$groupParameters$mean[paste0("mean_", parameters), k]
    }


    if (pooling != "complete"){
      # # old: array filled into data frame
      # result_row$est_indiv[[1]][sel_ind,-(1:4)] <-
      #   summMPT$individParameters[parameters,,1:(2+length(CI_SIZE))]
      sel_ind <- result_row$est_indiv[[1]]$condition == conditions[i]
      dimnames(summMPT$individParameters)$ID <- rownames(data_group)
      tmp <- summMPT$individParameters[parameters,,1:(2+length(CI_SIZE)), drop = FALSE] %>%
        reshape2::melt() %>%
        tidyr::spread("Statistic", "value")
      tmp$identifiable <- NA

      colnames(tmp) <- c("parameter", "id", colnames(result_row$est_indiv[[1]])[-(1:4)])
      tmp$parameter <- as.character(tmp$parameter)
      tmp$id <- as.character(tmp$id)

      tmp[[condition]] <- cond
      result_row$est_indiv[[1]][sel_ind,] <-
        dplyr::left_join(result_row$est_indiv[[1]][sel_ind,] %>%
                    dplyr::select("id", "condition", "parameter", "core"),
                  tmp, by = c("parameter", "id", condition = condition))
    }

    gof_group[[i]] <- TreeBUGS::PPP(treebugs_fit[[i]], M = TREEBUGS_MCMC$n.PPP, type = "G2",
                          T2 = pooling != "complete", nCPU = all_options$n.CPU)

    sel_gof <- result_row$gof_group[[1]]$condition == conditions[i]
    result_row$gof_group[[1]][sel_gof, ] <-
      result_row$gof_group[[1]] %>%
      dplyr::filter(.data$condition == conditions[i]) %>%
      dplyr::mutate(
        condition = conditions[i],
        type = "T1",
        focus = "mean",
        stat_obs = mean(gof_group[[i]]$T1.obs),
        stat_pred = mean(gof_group[[i]]$T1.pred),
        p = gof_group[[i]]$T1.p
      )

    if (pooling != "complete"){
      result_row$gof_group[[1]] <- tibble::add_row(result_row$gof_group[[1]],
                                           condition = cond,
                                           type = "T2", focus = "cov",
                                           stat_obs = mean(gof_group[[i]]$T2.obs),
                                           stat_pred = mean(gof_group[[i]]$T2.pred),
                                           p = gof_group[[i]]$T2.p)

      sel_fog_ind <- result_row$gof_indiv[[1]]$condition == conditions[i]
      result_row$gof_indiv[[1]][sel_fog_ind,] <-
        result_row$gof_indiv[[1]][sel_fog_ind,] %>%
        dplyr::mutate(
          condition = conditions[i],
          type = "T1_G2",
          focus = "mean",
          stat_obs = colMeans(gof_group[[i]]$ind.T1.obs),
          stat_pred = colMeans(gof_group[[i]]$ind.T1.pred),
          p = gof_group[[i]]$ind.T1.p)
    }
  }

  # between-subjects comparisons
  if (length(conditions) > 1){
    for (i in 1:(length(conditions) - 1)){
      for (j in 2:length(conditions)){
        for(p in parameters){
          test_between <- TreeBUGS::betweenSubjectMPT(treebugs_fit[[i]], treebugs_fit[[j]],
                                            par1 = p, stat = "x-y")
          test_summ <- TreeBUGS::summarizeMCMC(test_between$mcmc,
                                               probs = CI_SIZE,
                                               batchSize = 2)
          bayesp <- mean(do.call("rbind", test_between$mcmc) <= 0)

          sel_row <-
            result_row$test_between[[1]]$parameter == p &
            result_row$test_between[[1]]$condition1 == conditions[i] &
            result_row$test_between[[1]]$condition2 == conditions[j]

          # comply with tibble 2.99
          result_row$test_between[[1]]$est_diff[sel_row] <- test_summ[ , "Mean"]
          result_row$test_between[[1]]$se[sel_row] <- test_summ[, "SD"]
          result_row$test_between[[1]]$p[sel_row] <- ifelse(bayesp > .5, 1 - bayesp, bayesp) * 2 # two-sided Bayesian p values

          for (cl in CI_SIZE) {
            result_row$test_between[[1]][[paste0("ci_", cl)]][sel_row] <- test_summ[, paste0(cl * 100, "%")]
          }

          # result_row$test_between[[1]][sel_row,-(1:4)] <-
          #   c(test_summ[, c("Mean", "SD")],
          #     test_summ[, 2 + seq_along(CI_SIZE)])
        }
      }
    }
  }

  # don't save T2 if complete pooling was used ----
  # Why? I think it would be worthwhile
  # Daniel: T2 refers to the covariance matrix, which is not defined for aggregated frequencies.

  if (pooling != "complete"){
    result_row$gof[[1]] <- tibble::add_row(result_row$gof[[1]])   # T1 & T2

    T2_stats <- aggregate_ppp(gof_group, stat = "T2")
    result_row$gof[[1]]$stat_obs[2] <- T2_stats["stat_obs"]
    result_row$gof[[1]]$stat_pred[2] <- T2_stats["stat_pred"]
    result_row$gof[[1]]$p[2] <- T2_stats["p"]
  }
  result_row$gof[[1]]$type <- c("T1", if(pooling!="complete"){"T2"})
  result_row$gof[[1]]$focus <- c("mean", if(pooling!="complete"){"cov"})

  T1_stats <- aggregate_ppp(gof_group)
  result_row$gof[[1]]$stat_obs[1] <- T1_stats["stat_obs"]
  result_row$gof[[1]]$stat_pred[1] <- T1_stats["stat_pred"]
  result_row$gof[[1]]$p[1] <- T1_stats["p"]


  # estimation_time <- unlist(estimation_time)

  # result_row$estimation[[1]] <- tibble::tibble(
  #   condition = names(estimation_time)
  #   , time_difference = unname(estimation_time)
  # )

  if (method == "trait"){
    parnames <- coda::varnames(treebugs_fit[[1]]$runjags$mcmc)
    par_mat <- expand.grid("parameter1" = parameters, "parameter2" = parameters,
                            stringsAsFactors = FALSE)

    # Parameter correlations & fungibility
    sel_rho <- grep("rho[", parnames, fixed = TRUE, value = TRUE)
    sel_mean <- grep("mean[", parnames, fixed = TRUE, value = TRUE)

    for (i in seq_along(conditions)){
      mcmc <- treebugs_fit[[i]]$runjags$mcmc[,sel_rho]
      rho_summ <- TreeBUGS::summarizeMCMC(mcmc, probs = CI_SIZE,
                                          batchSize = 2)
      bayesp <- colMeans(do.call("rbind", mcmc) <= 0)
      res <- data.frame(par_mat,
                        rho_summ[,c("Mean", "SD")],
                        p = ifelse(bayesp > .5, 1 - bayesp, bayesp) * 2,  # two-sided Bayesian p values
                        rho_summ[,2 + seq_along(CI_SIZE)])
      colnames(res)[3:9] <- colnames(result_row$est_rho[[1]])[6:12]

      samples <- do.call("rbind", treebugs_fit[[i]]$runjags$mcmc[,sel_mean])
      colnames(samples) <- parameters
      rmat <- stats::cor(samples)

      for (j in 1:nrow(par_mat)){
        sel_row <-
          result_row$est_rho[[1]]$parameter1 == par_mat$parameter1[j] &
          result_row$est_rho[[1]]$parameter2 == par_mat$parameter2[j] &
          result_row$est_rho[[1]]$condition == conditions[i]
        if (sum(sel_row) > 0){
          result_row$est_rho[[1]][sel_row,-(1:5)] <- res[j,-(1:2)]
          result_row$fungibility[[1]][sel_row, "correlation"] <- rmat[par_mat$parameter1[j],
                                                                      par_mat$parameter2[j]]
        }
      }
    }
  }

  # save model objects to the working directory if requested by user ----
  if(all_options$save_models){
    dataset_name <- gsub("^.*[/\\]","", dataset)
    save(treebugs_fit, file = paste0(
      paste(
        c(
          gsub(model, pattern = ".eqn|.EQN", replacement = "")
          , gsub(dataset_name, pattern = ".csv|.CSV", replacement = "")
          , pooling
          , method
        )
        , collapse = "_"
      )
      , ".RData"
      )
    )
  }
  # return ----
  result_row
}


mpt_treebugs_safe <- purrr::possibly(
  .f = mpt_treebugs
  , otherwise = list()
  , quiet = FALSE
)
