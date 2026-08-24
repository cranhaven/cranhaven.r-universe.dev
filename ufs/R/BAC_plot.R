#' Bland-Altman Change plot
#'
#' @param data The data frame; if it only has two columns, the first of
#' which is the pre-change column, `cols` can be left empty.
#' @param cols The names of the columns with the data; the first is the
#' column with the pre-change data, the second the column after the change.
#' @param reliability The reliability estimate, for example as obtained with
#' the `ICC()` function in the `psych()` package; can be omitted, in which
#' case the intraclass correlation is computed.
#' @param pointSize The size of the points in the plot.
#' @param deterioratedColor,unchangedColor,improvedColor The colors to use for
#' cases who deteriorate, stay the same, and improve, respectively.
#' @param zeroLineColor,ciLineColor The colors for the line at 0 (no change)
#' and at the confidence interval bounds (i.e. the point at which a difference
#' becomes indicative of change given the reliability), respectively.
#' @param zeroLineType,ciLineType The line types for the line at 0 (no change)
#' and at the confidence interval bounds (i.e. the point at which a difference
#' becomes indicative of change given the reliability), respectively.
#' @param conf.level The confidence level of the confidence interval.
#' @param theme The ggplot2 theme to use.
#' @param iccFromPsych Whether to compute ICC using the [psych::ICC()] function
#' or not.
#' @param iccFromPsychArgs If using the [psych::ICC()] function, the arguments
#' to pass.
#' @param ignoreBias Whether to ignore bias (i.e. allow the measurements at
#' the second time to shift upwards or downwards). If `FALSE`, the variance
#' associated with such a shift is considered error variance (i.e.
#' 'unreliability').
#'
#' @return A ggplot2 plot.
#' @export
#'
#' @examples ### Create smaller dataset for example
#' dat <-
#'   ufs::testRetestSimData[
#'     1:25,
#'     c('t0_item1', 't1_item1')
#'   ];
#'
#' ufs::BAC_plot(dat, reliability = .5);
#' ufs::BAC_plot(dat, reliability = .8);
#' ufs::BAC_plot(dat, reliability = .9);
BAC_plot <- function(data,
                     cols = names(data),
                     reliability = NULL,
                     pointSize = 2,
                     #scales::show_col(viridis::viridis(3, begin=.1, end=.8, alpha=.9))
                     deterioratedColor = "#482576E6",
                     unchangedColor = "#25848E80",
                     #unchangedColor = "#25848EE6", #lower alpha of .5
                     improvedColor = "#7AD151E6",
                     zeroLineColor = "black",
                     zeroLineType = "dashed",
                     ciLineColor = "red",
                     ciLineType = "solid",
                     conf.level = .95,
                     theme = ggplot2::theme_minimal(),
                     ignoreBias = FALSE,
                     iccFromPsych = FALSE,
                     iccFromPsychArgs = NULL) {

  if (length(cols) == 1) {
    stop("Pass exactly two column names!");
  } else if (length(cols) > 2) {
    warning("More than two columns passed - only using the first two!");
    cols <- cols[1:2];
  }

  if (is.null(reliability)) {
    if (iccFromPsych) {
      iccFromPsychArgs <-
        c(list(x = data[, cols]),
          iccFromPsychArgs);
      iccObject <-
        do.call(
          psych::ICC,
          iccFromPsychArgs
        );
    } else {
      iccObject <- icc_from_psych(data[, cols]);
    }

    if (ignoreBias) {
      ### ICC c1
      ###
      ### relative, corrects for difference between means (conceptually
      ### can be considered to first 'standardize' within administration
      ### moment):
      ###
      ### participant_variance / (error)
      ###
      icc <- iccObject$results[3, 'ICC'];
    } else {
      ### ICC A1
      ###
      ### absolute, does not correct for difference between means, i.e.
      ### considers increase or decrease in all the scores part of the
      ### measurement error (i.e. if the average is higher or lower the
      ### second administration):
      ###
      ### participant_variance / (error + variance due to grand mean shift)
      ###
      icc <- iccObject$results[2, 'ICC'];
    }

    reliability <- icc;
    reliabilityWasEstimated <- TRUE;

 } else {
   reliabilityWasEstimated <- FALSE;
 }

  # longDat <-
  #   data.frame(
  #     id = c(1:nrow(data), 1:nrow(data)),
  #     variable = rep(cols, each=nrow(data)),
  #     value = c(data[, cols[1]], data[, cols[2]])
  #   );

  vector1 <- data[, cols[1]];
  vector2 <- data[, cols[2]];

  ### Compute vector with differences
  diffs <- vector2 - vector1;

  means <- (vector1 + vector2) / 2;

  diffMean <- mean(diffs, na.rm=TRUE);
  diffSD <- stats::sd(diffs, na.rm=TRUE);

  mean1 <- mean(vector1, na.rm = TRUE);
  mean2 <- mean(vector2, na.rm = TRUE);
  sd1 <- stats::sd(vector1, na.rm = TRUE);
  sd2 <- stats::sd(vector2, na.rm = TRUE);

  ### Compute SEM

  #SEM <- diffSD * sqrt(icc * (1 - icc));     ### Equation 11; seems wrong
  #SEM <- diffSD * sqrt(1 - icc);             ### Equation 8; consistent with
                                              ### Crocker & Algina
  #SEM <- diffSD * sqrt(1 - icc) * sqrt(2);    ###

  #ci_MoE <- abs(qnorm((1-conf.level)/2, lower.tail=TRUE)) * SEM * sqrt(2);
  ci_MoE <- abs(stats::qnorm((1-conf.level)/2, lower.tail=TRUE)) *
    sqrt(2 * (sd1 * sqrt(1 - reliability))^2);

  semCI <- c(0 - ci_MoE,
             0 + ci_MoE);

  secondaryAxisBreaks <-
    c(min(semCI),
      diffMean,
      max(semCI));

  dat <-
    data.frame(
      means = means,
      diffs = diffs,
      classification = ifelse(
        (diffs < min(semCI)),
        "Deteriorated",
        ifelse(
          (diffs > max(semCI)),
          "Improved",
          "Unchanged"
        )
      )
    );

  if (reliabilityWasEstimated) {
    dat$classification <-
      "Unchanged";
  }

  dat$classification <-
    factor(
      dat$classification,
      levels = c("Deteriorated", "Unchanged", "Improved"),
      labels = c("Deteriorated", "Unchanged", "Improved"),
      ordered = TRUE
    );

  plot <-
    ggplot2::ggplot(
      data = dat,
      mapping = ggplot2::aes_string(x = 'means',
                                    y = 'diffs',
                                    color = 'classification')
    ) +
    ggplot2::geom_point(
      size = pointSize
    ) +
    ggplot2::geom_hline(
      yintercept=0,
      color = zeroLineColor,
      linetype = zeroLineType
    ) +
    ggplot2::geom_hline(
      yintercept=semCI[1],
      color = ciLineColor,
      linetype = ciLineType
    ) +
    ggplot2::geom_hline(
      yintercept=semCI[2],
      color = ciLineColor,
      linetype = ciLineType
    ) +
    ggplot2::geom_hline(yintercept=diffMean) +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::dup_axis(
        breaks = round(secondaryAxisBreaks, 2),
        name = NULL
      )
    ) +
    ggplot2::labs(
      title = "Bland-Altman Change plot",
      subtitle =
        paste0(
          round(100*conf.level),
          "% confidence intervals for reliable change with ",
          ifelse(reliabilityWasEstimated, "(estimated)", "(specified)"),
          " reliability = ",
          round(reliability, 2)
        ),
      x = "Mean of two scores",
      y = "Difference between two scores"
    ) +
    theme;

  if (reliabilityWasEstimated) {
    plot <- plot +
      ggplot2::scale_color_manual(values = unchangedColor,
                                  guide = NULL);
  } else {
    plot <- plot +
      ggplot2::scale_color_manual(values = c(deterioratedColor,
                                             unchangedColor,
                                             improvedColor),
                                  name = 'Classification');
  }

  return(plot);

}



icc_from_psych <- function (x, alpha = 0.05) {

  cl <- match.call()
  if (is.matrix(x))
    x <- data.frame(x)
  n.obs.original <- dim(x)[1]
  x <- stats::na.omit(x);
  n.obs <- dim(x)[1]

  items <- colnames(x)
  n.items <- NCOL(x)

  nj <- dim(x)[2]
  x.s <- utils::stack(x)
  x.df <- data.frame(x.s, subs = rep(paste("S", 1:n.obs,
                                           sep = ""), nj))


  aov.x <- stats::aov(values ~ subs + ind, data = x.df)
  s.aov <- summary(aov.x)
  stats <- matrix(unlist(s.aov), ncol = 3, byrow = TRUE)
  MSB <- stats[3, 1]
  MSW <- (stats[2, 2] + stats[2, 3])/(stats[1, 2] + stats[1,
                                                          3])
  MSJ <- stats[3, 2]
  MSE <- stats[3, 3]
  MS.df <- NULL

  colnames(stats) <- c("subjects", "Judges", "Residual")
  rownames(stats) <- c("df", "SumSq", "MS",
                       "F", "p")
  ICC1 <- (MSB - MSW)/(MSB + (nj - 1) * MSW)
  ICC2 <- (MSB - MSE)/(MSB + (nj - 1) * MSE + nj * (MSJ - MSE)/n.obs)
  ICC3 <- (MSB - MSE)/(MSB + (nj - 1) * MSE)
  ICC12 <- (MSB - MSW)/(MSB)
  ICC22 <- (MSB - MSE)/(MSB + (MSJ - MSE)/n.obs)
  ICC32 <- (MSB - MSE)/MSB
  F11 <- MSB/MSW
  df11n <- n.obs - 1
  df11d <- n.obs * (nj - 1)
  p11 <- -expm1(stats::pf(F11, df11n, df11d, log.p = TRUE))
  F21 <- MSB/MSE
  df21n <- n.obs - 1
  df21d <- (n.obs - 1) * (nj - 1)
  p21 <- -expm1(stats::pf(F21, df21n, df21d, log.p = TRUE))
  F31 <- F21
  results <- data.frame(matrix(NA, ncol = 8, nrow = 6))
  colnames(results) <- c("type", "ICC", "F",
                         "df1", "df2", "p", "lower bound",
                         "upper bound")
  rownames(results) <- c("Single_raters_absolute", "Single_random_raters",
                         "Single_fixed_raters", "Average_raters_absolute",
                         "Average_random_raters", "Average_fixed_raters")
  results[1, 1] = "ICC1"
  results[2, 1] = "ICC2"
  results[3, 1] = "ICC3"
  results[4, 1] = "ICC1k"
  results[5, 1] = "ICC2k"
  results[6, 1] = "ICC3k"
  results[1, 2] = ICC1
  results[2, 2] = ICC2
  results[3, 2] = ICC3
  results[4, 2] = ICC12
  results[5, 2] = ICC22
  results[6, 2] = ICC32
  results[1, 3] <- results[4, 3] <- F11
  results[2, 3] <- F21
  results[3, 3] <- results[6, 3] <- results[5, 3] <- F31 <- F21
  results[5, 3] <- F21
  results[1, 4] <- results[4, 4] <- df11n
  results[1, 5] <- results[4, 5] <- df11d
  results[1, 6] <- results[4, 6] <- p11
  results[2, 4] <- results[3, 4] <- results[5, 4] <- results[6,
                                                             4] <- df21n
  results[2, 5] <- results[3, 5] <- results[5, 5] <- results[6,
                                                             5] <- df21d
  results[2, 6] <- results[5, 6] <- results[3, 6] <- results[6,
                                                             6] <- p21
  F1L <- F11/stats::qf(1 - alpha, df11n, df11d)
  F1U <- F11 * stats::qf(1 - alpha, df11d, df11n)
  L1 <- (F1L - 1)/(F1L + (nj - 1))
  U1 <- (F1U - 1)/(F1U + nj - 1)
  F3L <- F31/stats::qf(1 - alpha, df21n, df21d)
  F3U <- F31 * stats::qf(1 - alpha, df21d, df21n)
  results[1, 7] <- L1
  results[1, 8] <- U1
  results[3, 7] <- (F3L - 1)/(F3L + nj - 1)
  results[3, 8] <- (F3U - 1)/(F3U + nj - 1)
  results[4, 7] <- 1 - 1/F1L
  results[4, 8] <- 1 - 1/F1U
  results[6, 7] <- 1 - 1/F3L
  results[6, 8] <- 1 - 1/F3U
  Fj <- MSJ/MSE
  vn <- (nj - 1) * (n.obs - 1) * ((nj * ICC2 * Fj + n.obs *
                                     (1 + (nj - 1) * ICC2) - nj * ICC2))^2
  vd <- (n.obs - 1) * nj^2 * ICC2^2 * Fj^2 + (n.obs * (1 +
                                                         (nj - 1) * ICC2) - nj * ICC2)^2
  v <- vn/vd
  F3U <- stats::qf(1 - alpha, n.obs - 1, v)
  F3L <- stats::qf(1 - alpha, v, n.obs - 1)
  L3 <- n.obs * (MSB - F3U * MSE)/(F3U * (nj * MSJ + (nj *
                                                        n.obs - nj - n.obs) * MSE) + n.obs * MSB)
  results[2, 7] <- L3
  U3 <- n.obs * (F3L * MSB - MSE)/(nj * MSJ + (nj * n.obs -
                                                 nj - n.obs) * MSE + n.obs * F3L * MSB)
  results[2, 8] <- U3
  L3k <- L3 * nj/(1 + L3 * (nj - 1))
  U3k <- U3 * nj/(1 + U3 * (nj - 1))
  results[5, 7] <- L3k
  results[5, 8] <- U3k
  results[, 2:8] <- results[, 2:8]
  result <- list(results = results, summary = s.aov, stats = stats,
                 MSW = MSW, lme = MS.df, Call = cl, n.obs = n.obs, n.judge = nj)
  class(result) <- c("psych", "ICC")
  return(result)

}
