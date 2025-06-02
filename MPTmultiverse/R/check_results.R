#' Check results from MPTmultiverse
#'
#' @description  Set of helper functions that allow checking if model estimation
#'   worked as intended. Depending on the method and function, these functions
#'   return slightly different information.
#'
#' @details   \code{check_results} prints relatively verbose output detailing
#'   diagnostic information for each method to the console. For the frequentist
#'   methods, this is based on either the rank of the observed Fischer (or
#'   Hessian) matrix of the MLE estimate or based on empirical identifiability
#'   (based either on repeated re-runs or the width of the bootstrapped
#'   parameter distribution). For the Bayesian methods, this is convergence
#'   statistics R-hat and number of effective samples.
#'   \code{write_check_results} writes the results of \code{check_results} to a
#'   specififed file (instead of printing it to the console).
#'
#'   \code{check_set} returns a \code{tibble} with one row, where each expected
#'   method corresponds to a column with a boolean (\code{TRUE}/\code{FALSE})
#'   value. Entries \code{TRUE} correspond to no problem and \code{FALSE}
#'   correspond to problems. \code{FALSE} means the method is either missing
#'   from the results file or (for the Bayesian methods) there are core
#'   parameters for which the convergence criteria defined in
#'   \code{getOption("MPTmultiverse")} are not met.
#'
#' @param results An object of class multiverseMPT.

#' @example examples/examples.check_results.R
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
check_results <- function(results) {
  #browser()
  expected <- structure(list(
    pooling = c("no", "no", "no", "complete", "no", "complete", "partial",
                "partial", "partial", "partial"),
    package = c("MPTinR", "MPTinR", "MPTinR", "MPTinR", "TreeBUGS", "TreeBUGS",
                "TreeBUGS", "TreeBUGS", "TreeBUGS", "TreeBUGS"),
    method = c("NPB/MLE", "PB/MLE", "asymptotic", "asymptotic", "simple",
               "simple", "trait", "trait_uncorrelated", "beta", "betacpp")),
    .Names = c("pooling", "package", "method"),
    class = c("tbl_df", "tbl", "data.frame"
    ), row.names = c(NA, -10L))
  missing <- dplyr::anti_join(expected, results[, 3:5], by = c("pooling", "package", "method"))
  if (nrow(missing) > 0) {
    cat("## Following analysis approaches missing from results:\n",
        paste(apply(missing, 1, paste, collapse = ", "), collapse = "\n"),
        "\n\n\n")
  }

  ### MPTinR: no pooling ###

  cat("## MPTinR: no pooling\n")

  mpt_no_pool <- c("asymptotic", "PB/MLE", "NPB/MLE")
  mpt_no_pool <- mpt_no_pool[mpt_no_pool %in% results$method]
  tryCatch({
    for(meth in mpt_no_pool){

      # conv_mptinr_no <- results %>%
      #   dplyr::filter(.data$package == "MPTinR" & .data$pooling == "no" & .data$method == meth) %>%
      #   dplyr::select("convergence") %>%
      #   tidyr::unnest()

      not_id <- results %>%
        dplyr::filter(.data$package == "MPTinR" & .data$pooling == "no" & .data$method == meth) %>%
        dplyr::select(.data$est_indiv) %>%
        tidyr::unnest(.data$est_indiv) %>%
        dplyr::group_by(.data$condition, .data$core) %>%
        dplyr::summarise(proportion = mean(!.data$identifiable |
                                             is.na(.data$identifiable))) %>%
        dplyr::ungroup()

      not_id2 <- results %>%
        dplyr::filter(.data$package == "MPTinR" & .data$pooling == "no" & .data$method == meth) %>%
        dplyr::select(.data$est_indiv) %>%
        tidyr::unnest(.data$est_indiv) %>%
        dplyr::filter(!.data$identifiable) %>%
        dplyr::group_by(.data$condition, .data$core, .data$parameter) %>%
        dplyr::count() %>%
        dplyr::ungroup()

      if (any(not_id$proportion > 0)) {
        cat("Based on", meth, "method, proportion of participants with non-identified parameters:\n")
        cat(format(not_id, n = Inf)[-c(1,3)], "", sep = "\n")

        cat("Based on", meth, "CIs, table of non-identified parameters:\n")
        cat(format(not_id2, n = Inf)[-c(1,3)], sep = "\n")

      } else {
        cat("Based on", meth, "CIs, all parameters of all participants seem to be identifiable.\n")
      }
      cat("\n")
    }
  }, error = function(e)
    cat("Convergence checks failed for unkown reason.\n"))

  cat("\n")


  ### MPTinR: complete pooling ###

  cat("## MPTinR: complete pooling\n")

  tryCatch({
    conv_mptinr_comp <- results %>%
      dplyr::filter(.data$package == "MPTinR" & .data$pooling == "complete") %>%
      dplyr::select(.data$convergence) %>%
      tidyr::unnest(.data$convergence)

    comp_prob <- (conv_mptinr_comp$convergence != 0) |
      (conv_mptinr_comp$rank.fisher != conv_mptinr_comp$n.parameters)

    if (any(comp_prob, na.rm = TRUE)) {
      cat("Convergence problems:\n")
      cat(format(conv_mptinr_comp[comp_prob,])[-c(1,3)], "", sep = "\n")
    } else if (any(is.na(comp_prob))) {
      cat("Convergence problems:\n")
      cat(format(conv_mptinr_comp[is.na(comp_prob),])[-c(1,3)], "", sep = "\n")
    } else {
      cat("No convergence problems.\n")
    }
  }, error = function(e)
    cat("Convergence checks failed for unkown reason.\n"))

  cat("\n\n")

  ### TreeBUGS
  res_tree <- results %>%
    dplyr::filter(.data$package == "TreeBUGS") %>%
    dplyr::select(.data$model, .data$dataset, .data$pooling, .data$package, .data$method, .data$convergence, .data$est_group)

  for (i in seq_len(nrow(res_tree))) {
    cat("## ", paste(res_tree[i, 1:5], collapse = " // "), ":\n", sep = "")

    params <- res_tree[i,] %>%
      tidyr::unnest(cols = .data$est_group) %>%
      dplyr::select(.data$parameter, .data$core)

    tmp_convergence <- res_tree[i, ]$convergence[[1]] %>%
      dplyr::filter(.data$Rhat > getOption("MPTmultiverse")$treebugs$Rhat_max) %>%
      dplyr::mutate(parameter = label_parameter(.data$parameter, params),
             core = grepl("COREPARAMETER", .data$parameter),
             parameter = gsub("COREPARAMETER", "", x = .data$parameter))

    if (nrow(tmp_convergence) > 0) {
      cat(" ", sum(tmp_convergence$core), " core parameters with Rhat >",
          getOption("MPTmultiverse")$treebugs$Rhat_max, ":\n")

      # cat(format(not_id, n = Inf)[-c(1,3)], "", sep = "\n")

      cat(paste(tmp_convergence$parameter[tmp_convergence$core], collapse = ", "), "\n")
      cat(" ", sum(!tmp_convergence$core), " auxiliary parameters with Rhat >",
          getOption("MPTmultiverse")$treebugs$Rhat_max, ":\n")
      cat(paste(tmp_convergence$parameter[!tmp_convergence$core], collapse = ", "), "\n")
    } else {
      cat("  All Rhat <", getOption("MPTmultiverse")$treebugs$Rhat_max, ".\n")
    }

    tmp_neff <- res_tree[i,]$convergence[[1]] %>%
      dplyr::filter(!is.na(.data$Rhat), .data$n.eff < getOption("MPTmultiverse")$treebugs$Neff_min) %>%
      dplyr::mutate(parameter = label_parameter(.data$parameter, params),
             core = grepl("COREPARAMETER", .data$parameter),
             parameter = gsub("COREPARAMETER", "", x = .data$parameter))

    if(nrow(tmp_neff) > 0) {
      cat(" ", sum(tmp_neff$core), " core parameters with effect sample size n.eff <",
          getOption("MPTmultiverse")$treebugs$Neff_min, ":\n")
      cat(paste(tmp_neff$parameter[tmp_neff$core], collapse = ", "), "\n")
      cat(" ", sum(!tmp_neff$core), " auxiliary parameters with effect sample size n.eff <",
          getOption("MPTmultiverse")$treebugs$Neff_min, ":\n")
      cat(paste(tmp_neff$parameter[!tmp_neff$core], collapse = ", "), "\n")
    } else {
      cat("  All effect sample sizes >", getOption("MPTmultiverse")$treebugs$Neff_min, ".\n")
    }

    cat("\n\n")
  }


}


#' @param DATA_FILE character string. File name to use.
#' @param append logical. If \code{TRUE}, output will be appended to
#'   \code{DATA_FILE}; otherwise, it will overwrite the contents of
#'   \code{DATA_FILE}.
#' @export
#' @rdname check_results
## @describeIn check_results writes check results to file.
write_check_results <- function(DATA_FILE, results, append = FALSE){
  sink(paste0(DATA_FILE, "_check_results.txt"), append = append)
  cat("################ OPTIONS ################\n\n")
  cat("TreeBUGS:\n") ; print(getOption("MPTmultiverse")$treebugs)
  cat("\nMPTinR:\n") ; print(getOption("MPTmultiverse")$mptinr)
  cat("\nCI_SIZE: ", getOption("MPTmultiverse")$ci_size, "\n")
  cat("MAX_CI_INDIV = ", getOption("MPTmultiverse")$max_ci_indiv, "\n\n")
  cat("################ CHECK RESULTS ################\n\n")
  print(check_results(results))
  sink()
}



#' @export
#' @rdname check_results
check_set <- function(results) {

  if (!all(results$model[1] == results$model))
    stop("All results need to use same model")
  if (!all(results$dataset[1] == results$dataset))
    stop("All results need to use same dataset")

  expected <- structure(list(
    pooling = c("no", "no", "no", "complete", "no", "complete", "partial",
                "partial", "partial", "partial"),
    package = c("MPTinR", "MPTinR", "MPTinR", "MPTinR", "TreeBUGS", "TreeBUGS",
                "TreeBUGS", "TreeBUGS", "TreeBUGS", "TreeBUGS"),
    method = c("NPB/MLE", "PB/MLE", "asymptotic", "asymptotic", "simple",
               "simple", "trait", "trait_uncorrelated", "beta", "betacpp")),
    .Names = c("pooling", "package", "method"),
    class = c("tbl_df", "tbl", "data.frame"
    ), row.names = c(NA, -10L))
  missing <- dplyr::anti_join(expected, results[, 3:5], by = c("pooling", "package", "method"))
  miss <- tidyr::unite(missing, "meth")

  out <- tidyr::unite(expected, "meth")
  out$problem <- TRUE

  for (i in seq_len(nrow(miss))) {
    out[ out$meth == miss$meth[i] , "problem"  ] <- FALSE
  }

  ### TreeBUGS
  res_tree <- results %>%
    dplyr::filter(.data$package == "TreeBUGS") %>%
    dplyr::select(!!c("model", "dataset", "pooling", "package", "method", "convergence", "est_group"))

  for (i in seq_len(nrow(res_tree))) {

    cur_meth <- tidyr::unite(res_tree[i,3:5], "meth")$meth

    params <- res_tree[i,] %>%
      tidyr::unnest(.data$est_group) %>%
      dplyr::select(.data$parameter, .data$core)

    tmp_convergence <- res_tree[i, ]$convergence[[1]] %>%
      dplyr::filter(.data$Rhat > getOption("MPTmultiverse")$treebugs$Rhat_max) %>%
      dplyr::mutate(parameter = label_parameter(.data$parameter, params),
                    core = grepl("COREPARAMETER", .data$parameter),
                    parameter = gsub("COREPARAMETER", "", x = .data$parameter))

    if (nrow(dplyr::filter(tmp_convergence, .data$core)) > 0) {
      out[ out$meth == cur_meth , "problem"  ] <- FALSE
    }

    tmp_neff <- res_tree[i,]$convergence[[1]] %>%
      dplyr::filter(!is.na(.data$Rhat), .data$n.eff < getOption("MPTmultiverse")$treebugs$Neff_min) %>%
      dplyr::mutate(parameter = label_parameter(.data$parameter, params),
                    core = grepl("COREPARAMETER", .data$parameter),
                    parameter = gsub("COREPARAMETER", "", x = .data$parameter))
    if (nrow(dplyr::filter(tmp_neff, .data$core)) > 0) {
      out[ out$meth == cur_meth , "problem"  ] <- FALSE
    }
  }
  dplyr::bind_cols(
    results[1, 1:2]
    ,
    tidyr::spread(out, "meth", "problem")
  )
}

label_parameter <- function(mcmc_output, params){
  for (j in seq_len(nrow(params))){
    type <- ifelse(params$core[j], "COREPARAMETER", "")
    mcmc_output <- gsub(paste0("([sigma,mean,sd,mu,alph,bet]+)\\[", j, "\\]"),
                        paste0("\\1[", params$parameter[j], "]", type),
                        mcmc_output)
    mcmc_output <- gsub(paste0("theta\\[",j,",([0-9]+)\\]"),
                        paste0(params$parameter[j], '[\\1]', type),
                        mcmc_output)
    mcmc_output <- gsub(paste0("rho\\[",j,",([0-9,a-z,A-Z,_]+)\\]"),
                        paste0("rho[", params$parameter[j], ',\\1]', type),
                        mcmc_output)
    mcmc_output <- gsub(paste0("rho\\[([^,]+),", j, "\\]"),
                        paste0("rho[\\1,", params$parameter[j], ']', type),
                        mcmc_output)
  }
  mcmc_output
}
