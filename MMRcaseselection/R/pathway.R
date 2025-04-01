

#' Pathway case
#'
#' Calculation of pathway values, defined as the difference between residuals of
#' full model and reduced model lacking the pathway variable. The larger the
#' difference, the more a case qualifies as a pathway case suitable for the
#' analysis of mechanisms.
#'
#' The difference between the absolute residuals of the full and reduced model
#' follows the approach developed by Weller and Barnes (2014): \emph{Finding
#' Pathways: Mixed-Method Research for Studying Causal Mechanisms.}
#' Cambridge: Cambridge University Press.
#' \url{https://doi.org/10.1017/CBO9781139644501}).
#'
#' The calculation of the absolute difference between the full-model and
#' reduced-model residuals, given a case's reduced-model residual is larger
#' than its full-model residual, follows the proposal by
#' Gerring (2007): Is There a (Viable) Crucial-Case Method?
#' \emph{Comparative Political Studies} 40 (3): 231-253.
#' \url{https://journals.sagepub.com/doi/10.1177/0010414006290784})
#'
#' @param full_model Full model including covariate of interest
#' (= pathway variable)
#' @param reduced_model Reduced model excluding covariate of interest
#'
#' @return A dataframe with
#'
#' - all full model variables,
#'
#' - full model residuals (\code{full_resid}),
#'
#' - reduced model residuals (\code{reduced_resid}),
#'
#' - pathway values following Weller/Barnes (\code{pathway_wb}),
#'
#' - pathway values following Gerring (\code{pathway_gvalue}),
#'
#' - variable showing whether Gerring's criterion for a pathway
#' case is met (\code{pathway_gstatus})
#'
#' @importFrom stats lm residuals
#'
#' @examples
#' df_full <- lm(mpg ~ disp + wt, data = mtcars)
#' df_reduced <- lm(mpg ~ wt, data = mtcars)
#' pathway(df_full, df_reduced)
#'
#' @export
pathway <- function(full_model, reduced_model) {
  if (class(full_model) == "lm") {
    if (class(reduced_model) == "lm") {
      # full model
      full_resid <- residuals(full_model)
      # reduced model
      reduced_resid <- residuals(reduced_model)
      # difference between absolute residuals
      pathway_wb <- abs(reduced_resid) - abs(full_resid)
      # absolute difference between residuals
      pathway_gvalue <- abs(reduced_resid - full_resid)
      # check for Gerring's criterion for pathway values
      pathway_gtype <- ifelse(abs(reduced_resid) > abs(full_resid), "yes", "no")
      comb <- cbind(full_model$model, full_resid, reduced_resid,
                    pathway_wb, pathway_gvalue, pathway_gtype)
      return(comb)
    }
    else{
      stop("Reduced model object is not of class lm")
    }
  }
  else{
    (stop("Full model object is not of class lm"))
  }
}

#' Plot of residuals against pathway variable
#'
#' @param full_model Full model including covariate of interest
#' (= pathway variable)
#' @param reduced_model Reduced model excluding covariate of interest
#' @param pathway_type Type of pathway values. \code{pathway_wb} are
#' pathway values proposed by Weller and Barnes. \code{pathway_gvalue}
#' are values as calculated by Gerring.
#'
#' @return A plot of the chosen type of pathway values against the pathway
#' variable created with \code{\link{ggplot2}}.
#'
#' @import ggplot2
#'
#' @examples
#' df_full <- lm(mpg ~ disp + wt, data = mtcars)
#' df_reduced <- lm(mpg ~ wt, data = mtcars)
#' pathway_xvr(df_full, df_reduced, pathway_type = "pathway_wb")
#'
#' @export
pathway_xvr <- function(full_model, reduced_model, pathway_type) {
  pwdf <- pathway(full_model, reduced_model)
  if (pathway_type == "pathway_wb") {
    pwplot <- ggplot2::ggplot() +
      geom_point(data = pwdf,
                 mapping = aes_string(x = setdiff(names(full_model$model),
                                                  names(reduced_model$model)),
                                      y = pathway_type)) +
      geom_hline(yintercept = 0, linetype = 5) +
      scale_y_continuous("Pathway values") +
      theme_classic() -> pwplot
  }
  else{
    pwplot <- ggplot2::ggplot() +
      geom_point(data = pwdf,
                 mapping = aes_string(x = setdiff(names(full_model$model),
                                                  names(reduced_model$model)),
                                      y = pathway_type,
                                      color = "pathway_gtype")) +
      geom_hline(yintercept = 0, linetype = 5) +
      scale_y_continuous("Pathway values") +
      scale_color_viridis_d("Reduced > full residuals") +
      theme_classic() +
      theme(legend.position = "bottom") -> pwplot
  }
  return(pwplot)
}
