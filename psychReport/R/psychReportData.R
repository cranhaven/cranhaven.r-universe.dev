#' @title createDF
#'
#' @description Create dataframe (see also addDataDF)
#'
#' @param nVP Number of participants
#' @param nTrl Number of trials per factor/level for each participant
#' @param design Factors and levels
#'
#' @return dataframe
#'
#' @examples
#' # Example 1
#' dat <- createDF()
#'
#' # Example 2
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#'
#' # Example 3
#' dat <- createDF(nVP = 50, nTrl = 50, design = list(
#'   "Comp" = c("comp", "incomp"),
#'   "Side" = c("left", "right", "middle")
#' ))
#' @export
createDF <- function(nVP = 20,
                     nTrl = 50,
                     design = list("A" = c("A1", "A2"), "B" = c("B1", "B2"))) {
  dat <- data.frame(expand.grid(modifyList(design, list(VP = factor(c(1:nVP)), Trial = c(1:nTrl)))))
  return(dat[c("VP", names(design))])
}



#' @title addDataDF
#'
#' @description Add simulated ex-gaussian reaction-time (RT) data and
#' binary error (Error = 1, Correct = 0) data to an R DataFrame. This function
#' can be used to create simulated data sets.
#'
#' @param dat DataFrame (see createDF)
#' @param RT RT parameters (see rtDist)
#' @param Error Error parameters (see errDist)
#'
#' @return DataFrame with RT (ms) and Error (bool) columns
#'
#' @examples
#' # Example 1: default dataframe
#' dat <- createDF()
#' dat <- addDataDF(dat)
#' hist(dat$RT, 100)
#' table(dat$Error)
#'
#' # Example 2: defined overall RT parameters
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat, RT = c(500, 150, 100))
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#'
#' # Example 3: defined RT + Error parameters across conditions
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'   RT = list(
#'     "Comp comp" = c(500, 80, 100),
#'     "Comp incomp" = c(550, 80, 140)
#'   ),
#'   Error = list(
#'     "Comp comp" = 5,
#'     "Comp incomp" = 10
#'   )
#' )
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#'
#' # Example 4:
#' # create dataframe with defined RT + Error parameters across different conditions
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral")))
#' dat <- addDataDF(dat,
#'   RT = list(
#'     "Comp comp" = c(500, 150, 100),
#'     "Comp neutral" = c(550, 150, 100),
#'     "Comp incomp" = c(600, 150, 100)
#'   ),
#'   Error = list(
#'     "Comp comp" = 5,
#'     "Comp neutral" = 10,
#'     "Comp incomp" = 15
#'   )
#' )
#' boxplot(dat$RT ~ dat$Comp)
#' table(dat$Comp, dat$Error)
#'
#' # Example 5:
#' # create dataframe with defined RT + Error parameters across different conditions
#' dat <- createDF(
#'   nVP = 50, nTrl = 50,
#'   design = list(
#'     "Hand" = c("left_a", "right_a"),
#'     "Side" = c("left_a", "right_a")
#'   )
#' )
#' dat <- addDataDF(dat,
#'   RT = list(
#'     "Hand:Side left_a:left_a" = c(400, 150, 100),
#'     "Hand:Side left_a:right_a" = c(500, 150, 100),
#'     "Hand:Side right_a:left_a" = c(500, 150, 100),
#'     "Hand:Side right_a:right_a" = c(400, 150, 100)
#'   ),
#'   Error = list(
#'     "Hand:Side left_a:left_a" = c(5, 4, 2, 2, 1),
#'     "Hand:Side left_a:right_a" = c(15, 4, 2, 2, 1),
#'     "Hand:Side right_a:left_a" = c(15, 7, 4, 2, 1),
#'     "Hand:Side right_a:right_a" = c(5, 8, 5, 3, 1)
#'   )
#' )
#'
#' boxplot(dat$RT ~ dat$Hand + dat$Side)
#' table(dat$Error, dat$Hand, dat$Side)
#' @export
addDataDF <- function(dat, RT = NULL, Error = NULL) {

  # reaction time
  dat$RT <- 0
  if (is.null(RT)) {
    dat$RT <- rtDist(n = nrow(dat))
  } else if (!is.null(RT) & is.double(RT)) {
    dat$RT <- rtDist(n = nrow(dat), RT[1], RT[2], RT[3])
  } else if (!is.null(RT) & is.list(RT)) {
    for (i in c(1:length(RT))) {
      fcts_levls <- unlist(strsplit(gsub("\\s+", " ", names(RT[i])), split = " "))
      fcts <- unlist(strsplit(fcts_levls[1], split = ":"))
      levls <- unlist(strsplit(fcts_levls[2], split = ":"))

      idx <- NULL
      for (fct in c(1:length(fcts))) {
        idx <- cbind(idx, dat[fcts[fct]] == levls[fct])
      }
      idx <- apply(idx, 1, all)

      dat$RT[idx] <- rtDist(n = sum(idx), RT[[i]][1], RT[[i]][2], RT[[i]][3])
    }
  }

  # error rate
  dat$Error <- 0
  dat$bins <- 0
  if (is.null(Error)) {
    dat$Error <- errDist(n = nrow(dat))
  } else if (!is.null(Error) & is.double(Error)) {
    dat$Error <- errDist(n = nrow(dat), Error)
  } else if (!is.null(Error) & is.list(Error)) {
    for (i in c(1:length(Error))) {
      fcts_levls <- unlist(strsplit(gsub("\\s+", " ", names(Error[i])), split = " "))
      fcts <- unlist(strsplit(fcts_levls[1], split = ":"))
      levls <- unlist(strsplit(fcts_levls[2], split = ":"))

      idx <- NULL
      for (fct in c(1:length(fcts))) {
        idx <- cbind(idx, dat[fcts[fct]] == levls[fct])
      }
      idx <- apply(idx, 1, all)

      dat$bins[idx] <- dplyr::ntile(dat$RT[idx], length(Error[[1]]))
      for (bin in 1:length(Error[[1]])) {
        idx_bin <- dat$bins == bin & idx
        dat$Error[idx_bin] <- errDist(n = sum(idx_bin), Error[[i]][bin])
      }
    }
  }

  if ("bins" %in% names(dat)) {
    dat <- dat[, -which(names(dat) %in% c("bins"))]
  }

  return(dat)
}



#' @title rtDist
#'
#' @description Returns value(s) from a distribution appropriate to simulate reaction times.
#' The distribution is a combined exponential and gaussian distribution called
#' an exponentially modified Gaussian (EMG) distribution or ex-gaussian distribution.
#'
#' @param n Number of observations
#' @param gaussMean Mean of the gaussian distribution
#' @param gaussSD SD of the gaussian distribution
#' @param expRate Rate of the exponential function
#'
#' @return double
#'
#' @examples
#' # Example 1:
#' x <- rtDist()
#' hist(x, 100)
#'
#' # Example 2:
#' x <- rtDist(n = 20000, gaussMean = 800, gaussSD = 50, expRate = 100)
#' hist(x, 100)
#' @export
rtDist <- function(n = 10000, gaussMean = 600, gaussSD = 50, expRate = 200) {
  expDist <- stats::rexp(n, 1 / expRate)
  gaussDist <- stats::rnorm(n, gaussMean, gaussSD)
  return(round(expDist + gaussDist - mean(expDist)))
}



#' @title errDist
#'
#' @description Returns a random vector of 0's (correct) and 1's (incorrect) with
#' defined proportions (default = 10\% errors).
#'
#' @param n Number
#' @param proportion Approximate proportion of errors in percentage
#'
#' @return double
#'
#' @examples
#' # Example 1: approx 10% errors
#' x <- errDist(1000)
#' table(x)
#'
#' # Example 2: approx 20% errors
#' x <- errDist(1000, 20)
#' table(x)
#' @export
errDist <- function(n = 10000, proportion = 10) {
  return(ifelse(stats::runif(n) <= proportion / 100, 1, 0))
}


#' @title summaryMSDSE
#'
#' @description Aggregate data returning the mean, standard deviation, and standard error
#'
#' @param data A dataframe
#' @param factors List of factors over which to aggregate
#' @param dvs List of numeric data columns to aggregate
#' @param withinCorrection List of dvs which to apply within-subjects correction
#'  to the calculation of the standard deviation and standard error. Within-subject correction
#'  calculated according to Morey (2008). NB Data should be normed first (see normData).
#'
#' @return dataframe
#'
#' @examples
#' # Example 1:
#' library(dplyr)
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'   RT = list(
#'     "Comp comp" = c(500, 80, 100),
#'     "Comp incomp" = c(550, 80, 140)
#'   ),
#'   Error = list(
#'     "Comp comp" = 5,
#'     "Comp incomp" = 10
#'   )
#' )
#' datAggVP <- dat %>%
#'   group_by(VP, Comp) %>%
#'   summarize(
#'     N  = n(),
#'     RT = mean(RT[Error == 0]),
#'     ER = (sum(Error) / N) * 100
#'   )
#' datAgg <- summaryMSDSE(datAggVP, "Comp", c("RT", "ER"))
#'
#' # Example 2:
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'   RT = list(
#'     "Comp comp" = c(500, 80, 100),
#'     "Comp incomp" = c(550, 80, 140)
#'   ),
#'   Error = list(
#'     "Comp comp" = 5,
#'     "Comp incomp" = 10
#'   )
#' )
#' datAggVP <- dat %>%
#'   group_by(VP, Comp) %>%
#'   summarize(
#'     N = n(),
#'     RT = mean(RT[Error == 0]),
#'     ER = (sum(Error) / N) * 100
#'   )
#' datAggVP <- normData(datAggVP, "VP", c("RT", "ER"))
#' datAgg <- summaryMSDSE(
#'   datAggVP, "Comp", c("RT", "ER", "RT_norm", "ER_norm"),
#'   c("RT_norm", "ER_norm")
#' )
#' @export
summaryMSDSE <- function(data, factors, dvs, withinCorrection = NULL) {

  ncells <- prod(unlist(lapply(lapply(data[c(factors)], levels), length)))
  nvps <- nrow(data) / ncells

  # calculate N, mean, sd, and se for each group variable
  dat <- fn1 <- fn3 <- se <- NULL # avoid CRAN note!
  for (i in 1:length(dvs)) {
    tmp_dat <- data %>%
      dplyr::group_by_at(factors) %>%
      dplyr::summarize_at(dvs[i], c(length, mean, sd)) %>%
      dplyr::mutate(se = fn3 / sqrt(fn1),
                    se_ci = se * qt(0.975, nvps - 1)) %>%
      setNames(c(factors, "N", paste0(dvs[i], "_mean"), paste0(dvs[i], "_sd"), paste0(dvs[i], "_se"), paste0(dvs[i], "_se_ci")))
    if (i > 1) {
      tmp_dat <- tmp_dat[, (length(factors) + 2):ncol(tmp_dat)]
    }
    if (is.null(dat)) {
      dat <- tmp_dat
    } else {
      dat <- cbind(dat, tmp_dat)
    }
  }

  # apply within-participant correction (Morey, 2008) to sd/se for plotting errorbars
  # adapted from Cookbook for R: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
  if (!is.null(withinCorrection)) {
    cf <- sqrt(ncells / (ncells - 1))
    for (i in 1:length(withinCorrection)) {
      dat <- dat %>%
        dplyr::mutate(
          "{withinCorrection[i]}_sd" := !!as.name(paste0(withinCorrection[i], "_sd")) * cf,
          "{withinCorrection[i]}_se" := !!as.name(paste0(withinCorrection[i], "_se")) * cf,
          "{withinCorrection[i]}_se_ci" := !!as.name(paste0(withinCorrection[i], "_se_ci")) * cf
        ) %>%
        dplyr::select(-!!as.name(paste0(withinCorrection[i], "_mean")))
    }
  }

  return(dat)

}


#' @title normData
#'
#' @description Aggregate data returning the mean, standard deviation, and standard error
#'
#' @param data A dataframe
#' @param idvar Column indicating the individual participants
#' @param dvs List of numeric data columns to normalise
#'
#' @return dataframe
#'
#' @examples
#'
#' # Example 1:
#' library(dplyr)
#' dat <- createDF(nVP = 50, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
#' dat <- addDataDF(dat,
#'   RT = list(
#'     "Comp comp" = c(500, 80, 100),
#'     "Comp incomp" = c(550, 80, 140)
#'   ),
#'   Error = list(
#'     "Comp comp" = 5,
#'     "Comp incomp" = 10
#'   )
#' )
#' datAggVP <- dat %>%
#'   group_by(VP, Comp) %>%
#'   summarize(
#'     N = n(),
#'     RT = mean(RT[Error == 0]),
#'     ER = (sum(Error) / N) * 100
#'   )
#' datAggVP <- normData(datAggVP, "VP", c("RT", "ER"))
#' @export
normData <- function(data, idvar, dvs) {
  idmean <- NULL # avoid CRAN note!
  for (i in 1:length(dvs)) {
    grand_mean <- mean(data[[dvs[i]]])
    data <- data %>%
      dplyr::group_by_at(idvar) %>%
      dplyr::mutate(
        idmean = mean(!!as.name(dvs[i])),
        "{dvs[i]}_norm" := !!as.name(dvs[i]) - idmean + grand_mean
      ) %>%
      dplyr::select(-idmean)
  }
  return(data)
}
