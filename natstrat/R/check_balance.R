#' Check covariate balance of the control and treated groups
#'
#' Reports standardized differences in means between the treated and
#' control group before and after choosing a subset of controls.
#' These differences are reported both across strata and within strata.
#' This function can also generate love plots of the same quantities.
#'
#' @inheritParams stand
#' @inheritParams optimize_controls
#' @param X a data frame containing the covariates in the columns over which balance is desired. The number
#' of rows should equal the length of \code{z}.
#' @param treated which treatment value should be considered the treated units. This
#' must be one of the values of \code{z}.
#' @param control which treatment value should be considered the control units. This
#' must be one of the values of \code{z}.
#' @param selected a boolean vector including whether each unit was selected as part of the treated and control
#' groups for analysis. Should be the same length as \code{z} and typically comes from the results of
#' \code{\link{optimize_controls}()}.
#' @param plot a boolean denoting whether to generate love plots for the standardized differences.
#' @param message a boolean denoting whether to print a message about the level of balance achieved
#' @return List containing:
#' \describe{
#'   \item{sd_across}{matrix with one row per covariate and two columns: one for the
#'   standardized difference before a subset of controls were selected and one for after.}
#'   \item{sd_strata}{matrix similar to \code{sd_across}, but with separate
#'   standardized differences for each stratum for each covariate.}
#'   \item{sd_strata_avg}{matrix similar to \code{sd_across}, but taking the
#'   average of the standardized differences within the strata, weighted by stratum size.}
#'   \item{plot_across}{ggplot object plotting \code{sd_across}, only exists if \code{plot = TRUE}.}
#'   \item{plot_strata}{a named list of ggplot objects plotting \code{sd_strata},
#'   one for each stratum, only exists if \code{plot = TRUE}.}
#'   \item{plot_strata_avg}{ggplot object plotting \code{sd_strata_avg}, only exists if \code{plot = TRUE}.}
#'   \item{plot_pair}{ggplot object with two facets displaying \code{sd_across} and
#'   \code{sd_strata_avg} with one y-axis, only exists if \code{plot = TRUE}.}
#' }
#' @export
#' @importFrom caret dummyVars
#' @importFrom stats predict median var
#'
#' @examples
#'
#' data('nh0506')
#'
#' # Create strata
#' age_cat <- cut(nh0506$age,
#'                breaks = c(19, 39, 50, 85),
#'                labels = c('< 40 years', '40 - 50 years', '> 50 years'))
#' strata <- age_cat : nh0506$sex
#'
#' # Balance age, race, education, poverty ratio, and bmi both across and within the levels of strata
#' constraints <- generate_constraints(
#'                  balance_formulas = list(age + race + education + povertyr + bmi ~ 1 + strata),
#'                  z = nh0506$z,
#'                  data = nh0506)
#'
#' # Choose one control for every treated unit in each stratum,
#' # balancing the covariates as described by the constraints
#' results <- optimize_controls(z = nh0506$z,
#'                              X = constraints$X,
#'                              st = strata,
#'                              importances = constraints$importances,
#'                              ratio = 1)
#'
#' cov_data <- nh0506[, c('sex', 'age', 'race', 'education', 'povertyr', 'bmi')]
#'
#' # Check balance
#' stand_diffs <- check_balance(z = nh0506$z,
#'                              X = cov_data,
#'                              st = strata,
#'                              selected = results$selected,
#'                              plot = TRUE)

check_balance <- function(z, X, st, selected, treated = 1, control = 0,
                          denom_variance = "treated", plot = FALSE, message = TRUE) {

  if (plot && !requireNamespace("ggplot2", quietly = TRUE) && !requireNamespace("rlang", quietly = TRUE)) {
    stop("Packages \"ggplot2\" and \"rlang\" needed if \"plot\" argument set to \"TRUE\". Please
         install these or switch the \"plot\" argument to \"FALSE\".",
         call. = FALSE)
  }

  st <- as.factor(st)
  X[, sapply(X, is.logical)] <- sapply(X[, sapply(X, is.logical)], as.numeric)
  dummies <- dummyVars( ~ ., data = X, levelsOnly = FALSE)
  full_X <- predict(dummies, newdata = X)

  sd_across <- get_stand_diffs(full_X, z, selected, treated = treated, control = control,
                               denom_variance = denom_variance)

  sd_strata <- NULL
  for (ist in levels(st)) {
    sd_strata <- rbind(sd_strata, cbind(get_stand_diffs(full_X, z, selected, st, ist,
                                                        treated = treated, control = control,
                                                        denom_variance = denom_variance), ist))
  }
  colnames(sd_strata)[4] <- "stratum"

  q_s <- sapply(levels(st), function(ist) {sum( z == control & selected & st == ist )})
  n_s <- sapply(levels(st), function(ist) {sum( z == control & st == ist )})

  sd_strata_avg <- sd_across
  sd_strata_avg[1:dim(sd_strata_avg)[1], 1:2] <- NA
  for (cov in row.names(sd_strata_avg)) {
    sd_strata_avg[cov, 1] <- sum(sapply(levels(st), function(ist) {
      sd_strata[sd_strata$covariate == cov & sd_strata$stratum == ist, 1] *
        (n_s[ist] - sum(is.na(X[z == 0 & st == ist, cov]))) })) /
      (sum(n_s) - sum(is.na(X[z == 0, cov])))
    sd_strata_avg[cov, 2] <- sum(sapply(levels(st), function(ist) {
      sd_strata[sd_strata$covariate == cov & sd_strata$stratum == ist, 2] *
        (q_s[ist] - sum(is.na(X[z == 0 & st == ist & selected, cov]))) })) /
      (sum(q_s) - sum(is.na(X[z == 0 & selected, cov])))
  }

  if (message) {
    summary_across <- paste0("The mean standardized difference across strata for the covariates is ",
                             round(mean(sd_across$abs_stand_diff_after),3), ",\n   the median is ",
                             round(median(sd_across$abs_stand_diff_after),3),
                             ",\n   and the maximum is ", round(max(sd_across$abs_stand_diff_after),3), ".")
    if (max(sd_across$abs_stand_diff_after, na.rm = TRUE) < .1) {
      cat(paste("Excellent balance achieved across strata (<.1 standardized difference in all covariates). \n",
                summary_across), "\n")
    } else if (max(sd_across$abs_stand_diff_after, na.rm = TRUE) < .2) {
      cat(paste("Adequate balance achieved across strata (<.2 standardized difference in all covariates). \n",
                summary_across), "\n")
    } else {
      cat(paste("Inadequate balance achieved across strata (>=.2 standardized difference in some covariates). \n",
                summary_across), "\n")
    }

    summary_strata_avg <- paste0("Taking a weighted average of standardized differences within strata, \n we have the mean covariate imbalance is ",
                                 round(mean(sd_strata_avg$abs_stand_diff_after, na.rm = TRUE),3), ",\n   the median is ",
                                 round(median(sd_strata_avg$abs_stand_diff_after, na.rm = TRUE),3), ",\n   and the maximum is ",
                                 round(max(sd_strata_avg$abs_stand_diff_after, na.rm = TRUE),3), ". \n")

    summary_strata <- paste0("The mean standardized difference within strata for the covariates is ",
                             round(mean(sd_strata$abs_stand_diff_after, na.rm = TRUE),3), ",\n   the median is ",
                             round(median(sd_strata$abs_stand_diff_after, na.rm = TRUE),3),
                             ",\n   and the maximum is ", round(max(sd_strata$abs_stand_diff_after, na.rm = TRUE),3), ". \n")
    if (max(sd_strata$abs_stand_diff_after, na.rm = TRUE) < .1) {
      cat(paste(summary_strata_avg,
                "Within strata, we see excellent balance (<.1 standardized difference in all covariates in all strata). \n",
                summary_strata))
    } else if (max(sd_strata$abs_stand_diff_after, na.rm = TRUE) < .2) {
      cat(paste(summary_strata_avg,
                "Within strata, we see adequate balance (<.2 standardized difference in all covariates in all strata). \n",
                summary_strata))
    } else {
      cat(paste(summary_strata_avg,
                "Within strata, we see imbalances (>=.2 standardized difference in some covariates in some strata). \n",
                summary_strata))
    }
  }

  sds <- list(sd_across = sd_across, sd_strata = sd_strata, sd_strata_avg = sd_strata_avg)
  output <- sds

  if (plot) {
    plot_across <- plot_stand_diffs(sds, "across")
    plot_strata_avg <- plot_stand_diffs(sds, "strata_avg")
    plot_strata <- plot_stand_diffs(sds, "strata")
    plot_pair <- plot_stand_diffs(sds, "pair")
    output <- c(output, list(plot_across = plot_across, plot_strata = plot_strata,
                             plot_strata_avg = plot_strata_avg, plot_pair = plot_pair))
  }

  return(output)
}

#' Calculate standardized differences
#'
#' Calculate standardized differences in means between treated and control groups,
#' before and after refining the control group. Used within the \code{\link{check_balance}} function.
#'
#' @inheritParams check_balance
#' @param data A data frame with columns for which the standardized differences should be calculated.
#' @param ist The specific stratum for which the standardized differences should be calculated.
#'
#' @return data frame containing two columns, one for standardized differences before
#' choosing a subset of controls, and one for after. The rows pertain to covariates.
#' @keywords internal

get_stand_diffs <- function(data, z, selected, st = NULL, ist = NULL,
                            treated = 1, control = 0, denom_variance = "treated") {
  if (is.vector(z)) {
    z <- as.factor(z)
  }
  if (!is.null(ist)) {
    ind <- st == ist
  } else {
    ind <- rep(TRUE, length(z))
  }
  # Standardized differences before matching
  treatedmat_before_full <- data[z == treated, , drop = FALSE]
  treatedmat_before <- data[z == treated & ind, , drop = FALSE]
  treatedmean_before <- apply(treatedmat_before, 2, mean, na.rm = TRUE)
  controlmat_before_full <- data[z == control, , drop = FALSE]
  controlmat_before <- data[z == control & ind, , drop = FALSE]
  controlmean_before <- apply(controlmat_before, 2, mean, na.rm = TRUE)
  variances <- sapply(levels(z), function(group) {
    return(apply(data[z == group, , drop = FALSE], 2, var, na.rm = TRUE))
  })
  if (is.vector(variances)) {
    variances <- matrix(variances, ncol = 1)
  }
  variances[is.na(variances)] <- 0
  if (denom_variance == "pooled") {
    denom <- sqrt(rowMeans(variances))
  } else {
    denom <- sqrt(variances[, levels(z) == treated])
    denom[denom == 0] <-
      sqrt(rowMeans(variances)[denom == 0])
  }
  stand_diff_before <- rep(NA, nrow(variances))
  names(stand_diff_before) <- dimnames(variances)[[1]]
  stand_diff_before <- (treatedmean_before - controlmean_before) / denom
  stand_diff_before[treatedmean_before == controlmean_before] <- 0.0
  # Standardized differences after matching
  controlmat_after <- data[selected & z == control & ind, , drop = FALSE]
  controlmean_after <- apply(controlmat_after, 2, mean, na.rm = TRUE)
  treatedmat_after <- data[selected & z == treated & ind, , drop = FALSE]
  treatedmean_after <- apply(treatedmat_after, 2, mean, na.rm = TRUE)
  stand_diff_after <- rep(NA, nrow(variances))
  names(stand_diff_after) <- dimnames(variances)[[1]]
  stand_diff_after <- (treatedmean_after - controlmean_after) / denom
  stand_diff_after[treatedmean_after == controlmean_after] <- 0.0
  sd_matrix <- data.frame(abs_stand_diff_before = abs(stand_diff_before),
                          abs_stand_diff_after = abs(stand_diff_after))
  if (!is.null(ist)) {
    sd_matrix$covariate <- row.names(sd_matrix)
    row.names(sd_matrix) <- paste(row.names(sd_matrix), paste0("stratum", ist), sep="_")

  }
  return(sd_matrix)
}

#' Plot standardized differences in means
#'
#' Used within the \code{\link{check_balance}} function to plot the standardized differences calculated
#' in the format of Love (2002).
#'
#' @param sds Standardized differences generated with \code{\link{get_stand_diffs}}.
#' @param type One of the following, stating which standardized differences to plot:
#'   \describe{
#'     \item{across}{standardized differences across the population}
#'     \item{strata_avg}{weighted average of the standardized differences within the strata
#'       (weighted by stratum size)}
#'     \item{pair}{two facets displaying both
#'        the previous plots together}
#'     \item{strata}{list of plots for the standardized differences
#'        within each stratum}}
#'
#' @return Either a ggplot object or a list of ggplot objects (if \code{type} is 'strata')
#'
#' @references Love, T. E. (2002), "Displaying covariate balance after adjustment for selection bias",
#' Joint Statistical Meetings, yumpu.com/en/document/read/41664623.
#'
#' @importFrom rlang .data
#' @import ggplot2
#' @keywords internal

plot_stand_diffs <- function(sds, type) {

  if (type %in% c("pair", "across")) {
    ordering <- order(sds$sd_across$abs_stand_diff_before)
  } else if (type == "strata_avg") {
    ordering <- order(sds$sd_strata_avg$abs_stand_diff_before)
  }

  if (type %in% c("pair", "across", "strata_avg")) {
    across_sds <- sds$sd_across[ordering, ]
    weighted_sds <- sds$sd_strata_avg[ordering, ]
  }

  if (type == "strata") {

    sds$sd_strata$covariate_name <- sapply(sds$sd_strata$covariate, function(cov) {
      x <- strsplit(cov, split = "`", fixed = TRUE)
      if (length(x[[1]]) == 3) {
        return(x[[1]][3])
      } else {
        return(cov)
      }
    })

    sds$sd_strata$covariate_name <-  sapply(sds$sd_strata$covariate_name, function(cov) {
      x <- strsplit(cov, split = ".", fixed = TRUE)[[1]]
      if (length(x) == 2) {
        returnx=(x[2])
      } else {
        return(cov)
      }
    })

    covariates_plot <- factor(sds$sd_strata$covariate_name,
                              levels = unique(sds$sd_strata$covariate_name))
  } else {

    covariates <- row.names(across_sds)
    covariates_plot <- sapply(covariates, function(cov) {
      x <- strsplit(cov, split = "`", fixed = TRUE)
      if (length(x[[1]]) == 3) {
        return(x[[1]][3])
      } else {
        return(cov)
      }
    })
    covariates_plot <- sapply(covariates_plot, function(cov) {
      x <- strsplit(cov, split = ".", fixed = TRUE)[[1]]
      if (length(x) == 2) {
        returnx=(x[2])
      } else {
        return(cov)
      }
    })

    covariates_plot <- stringr::str_remove_all(covariates_plot, "`")

    covariates_plot <- factor(covariates_plot, levels = covariates_plot)
  }

  plot_dataframe <- NULL

  if (type %in% c("pair", "across")) {
    plot_dataframe <- data.frame(abs_stand_diff = c(across_sds$abs_stand_diff_before,
                                                    across_sds$abs_stand_diff_after),
                                 covariates = rep(covariates_plot, 2),
                                 type = c(rep("Before", length(covariates_plot)),
                                          rep("After", length(covariates_plot))),
                                 pop = "Across strata")
  }

  if (type %in% c("pair", "strata_avg")) {
    plot_dataframe <- rbind(plot_dataframe,
                            data.frame(abs_stand_diff = c(weighted_sds$abs_stand_diff_before,
                                                          weighted_sds$abs_stand_diff_after),
                                       covariates = rep(covariates_plot, 2),
                                       type = c(rep("Before", length(covariates_plot)),
                                                rep("After", length(covariates_plot))),
                                       pop = "Within strata"))
  }

  if (type == "strata") {
    plot_dataframe <- data.frame(abs_stand_diff = c(sds$sd_strata$abs_stand_diff_before,
                                                    sds$sd_strata$abs_stand_diff_after),
                                 covariates = rep(covariates_plot, 2),
                                 type = factor(c(rep("Before", length(covariates_plot)),
                                                 rep("After", length(covariates_plot)))),
                                 stratum = sds$sd_strata$stratum)

    p <- apply(as.array(unique(sds$sd_strata$stratum)), 1, function(x) {
      ggplot(plot_dataframe[plot_dataframe$stratum == x,],
             aes(x = .data$abs_stand_diff, y = .data$covariates)) +
        geom_point(size = 5, aes(shape = .data$type)) +
        scale_shape_manual(values = c(4, 1)) +
        geom_vline(xintercept = c(.1,.2), lty = 2) +
        xlab("Absolute standardized difference") +
        ylab("Covariate") +
        labs(shape = "") +
        ggtitle(x) +
        theme(text = element_text(size=10), strip.text.x = element_text(size = 12))
    })
    names(p) <- unique(sds$sd_strata$stratum)
  } else {

    p <- ggplot(plot_dataframe, aes(x = .data$abs_stand_diff, y = .data$covariates)) +
      geom_point(size = 5, aes(shape = .data$type)) +
      scale_shape_manual(values = c(4, 1)) +
      geom_vline(xintercept = c(.1,.2), lty = 2) +
      xlab("Absolute standardized difference") +
      ylab("Covariate") +
      labs(shape = "") +
      theme(text = element_text(size=10), strip.text.x = element_text(size = 12))

    if (type == "pair") {
      p <- p + facet_grid(. ~ pop)
    }
  }

  return(p)
}
