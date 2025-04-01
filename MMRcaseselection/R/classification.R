#' Classification of cases as typical and deviant using a prediction
#' interval.
#'
#' Case are designated as typical (= well predicted) and deviant
#' (= badly predicted) based on the prediction interval. The x\% prediction
#' interval represents the range that we expect to include
#' x\% of outcome values in repeated samples. For example, a 95\%
#' prediction interval ranging from 0-5 conveys that 95\% of future outcome
#' values will be in the range of 0-5. If the observed outcome
#' is inside the prediction interval, the case is classified (or designated) as
#'  typical and as deviant otherwise.
#'
#' Proposed by Rohlfing, Ingo and Peter Starke (2013):
#' Building on Solid Ground: Robust Case Selection in Multi-Method Research.
#' *Swiss Political Science Review* 19 (4): 492-512.
#' (\url{https://doi.org/10.1111/spsr.12052})
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#' @param piwidth Width of the prediction interval (default is 0.95).
#'
#' @return A dataframe with the observed outcome, fitted outcome,
#' upper and lower bound of the \% prediction interval and classification
#' of cases as typical or deviant.
#'
#' @importFrom stats lm residuals predict.lm
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' predint(df, piwidth = 0.9)
#'
#' @export
predint <- function(lmobject, piwidth = 0.95) {
  if (class(lmobject) == "lm") {
    if (piwidth >= 0 & piwidth <= 1) {
      # calculating prediction interval
      temp <- as.data.frame(suppressWarnings(predict.lm(lmobject,
                                                        interval = "prediction",
                                                        level = piwidth)))
      # extracting outcome values
      outcome <- lmobject$model[, 1]
      # merging outcome values into dataframe
      comb <- cbind(temp, outcome)
      # classification of cases
      comb$status <- ifelse(comb$outcome < comb$lwr |
                              comb$outcome > comb$upr,
                            "deviant", "typical")
      return(comb)
    }
    else{
      stop("Prediction interval needs to be between 0 and 1")
    }
  }
  else{
    stop("Input into function is not of class lm")
  }
}

#' Plot of typical and deviant cases with prediction intervals
#'
#' Presented in Rohlfing, Ingo and Peter Starke (2013):
#' Building on Solid Ground: Robust Case Selection in Multi-Method Research.
#' \emph{Swiss Political Science Review} 19 (4): 492-512.
#' (\url{https://doi.org/10.1111/spsr.12052})
#'
#' @param pred_df A dataframe created with \code{\link{predint}}.
#'
#' @return A plot of the observed outcome against the fitted outcome with
#' prediction intervals and case classifications. Created with
#' \code{\link{ggplot2}}.
#'
#' @import ggplot2
#
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' predint_status <- predint(df, piwidth = 0.9)
#' predint_plot(predint_status)
#'
#' @export
predint_plot <- function(pred_df) {
  ggplot(data = pred_df) +
    geom_point(mapping = aes(x = fit, y = outcome, color = status)) +
    # bisecting line
    geom_abline(intercept = 0, slope = 1, linetype = 5) +
    # prediction intervals
    geom_errorbar(mapping = aes(x = fit, ymin = lwr, ymax = upr,
                                color = status)) +
    # colorblind scheme
    scale_color_viridis_d() +
    theme_classic() +
    theme(legend.title = element_blank())
}

#' Classification of cases as typical and deviant using the standard
#' deviation of the residuals.
#'
#' The share of the standard deviation of the residuals is used to
#' designate cases as typical or deviant.
#'
#' Proposed by Lieberman, Evan S. (2005): Nested Analysis as a Mixed-Method
#' Strategy for Comparative Research. \emph{American Political Science Review}
#' 99 (3): 435-452. \url{https://doi.org/10.1017/S0003055405051762}.
#'
#' @param lmobject Object generated with \code{\link[stats]{lm}}
#' @param stdshare Share of standard deviation of residuals distinguishing
#' between typical and deviant cases (default is 1).
#'
#' @return A dataframe with the observed outcome, fitted outcome,
#' residual standard deviation and classification of cases as typical
#' or deviant.
#'
#' @importFrom stats lm residuals
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' residstd(df, stdshare = 1)
#'
#' @export
residstd <- function(lmobject, stdshare = 1) {
  if (class(lmobject) == "lm") {
    if (stdshare >= 0) {
      # calculating standard deviation of residuals
      tempsd <- as.data.frame(suppressWarnings(predict.lm(df, se.fit = T)))
      # removing irrelevant columns
      tempsd <- tempsd[, c("fit", "residual.scale")]
      # extracing outcome values
      outcome <- df$model[, 1]
      # merging outcome values into dataframe
      comb <- cbind(tempsd, outcome)
      # classification of cases
      comb$status <- ifelse(comb$outcome <
                              comb$fit - stdshare * comb$residual.scale |
                              comb$outcome >
                              comb$fit + stdshare * comb$residual.scale,
                            "deviant", "typical")
      return(comb)
      }
    else{
      stop("Standard deviation should not be negative")
    }
  }
  else{
    stop("Input into function is not of class lm")
  }
}

#' Plot of typical and deviant cases based on residuals' standard deviation
#'
#' @param resid_df A dataframe created with \code{\link{residstd}}.
#'
#' @return A plot of the observed outcome against the fitted outcome with
#' interval and case classifications. Created with \code{\link{ggplot2}}.
#'
#' @import ggplot2
#'
#' @examples
#' df <- lm(mpg ~ disp + wt, data = mtcars)
#' residstd_status <- residstd(df, stdshare = 1)
#' residstd_plot(residstd_status)
#'
#' @export
residstd_plot <- function(resid_df) {
  # Calculation of upper and lower bounds
  resid_df$lwr <- resid_df$fit - resid_df$residual.scale
  resid_df$upr <- resid_df$fit + resid_df$residual.scale
  ggplot(data = resid_df) +
    # lower bound
    geom_line(mapping = aes(x = fit, y = lwr), color = "grey") +
    # upper bound
    geom_line(mapping = aes(x = fit, y = upr), color = "grey") +
    geom_point(mapping = aes(x = fit, y = outcome, color = status),
               size = 2) +
    # bisecting line
    geom_abline(intercept = 0, slope = 1, linetype = 5) +
    # colorblind scheme
    scale_color_viridis_d() +
    theme_classic() +
    theme(legend.title = element_blank())
}
