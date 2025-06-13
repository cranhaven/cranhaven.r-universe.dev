# Functions related to protection estimates
# Arseniy Khvorov
# Created 2019/07/31
# Last edit 2019/10/15

#' Protection level calculations
#'
#' Calculates covariate values corresponding to a particular protection level.
#' Only accepts one covariate at a time, fixed values of all the others should
#' be provided. The search engine is \code{\link{find_prot_titre_val}}.
#'
#' @param fit Object returned by \code{\link{sclr}}.
#' @param var_name Name of the covariate for which to find values corresponding
#'   to a protection level. This name should appear in the formula in the call
#'   to \code{\link{sclr}} which was used to generate \code{fit}.
#' @param newdata A dataframe with all covariates except the one for which
#'   protection values should be calculated. If there is only one covariate, can
#'   be left as \code{NULL} (the default)
#' @param lvl Protection level to find covariate values for. Default is 0.5
#'   (50\%)
#' @param ci_level Confidence level for the calculated interval. Default is
#'   0.95.
#' @param tol Tolerance. The values will be found numerically, once the
#'   algorithm converges within \code{tol} of \code{lvl} it stops looking.
#'   Default is \eqn{10^(-7)}.
#'
#' @return A \code{\link[tibble]{tibble}}. Will have the same variables as
#'   \code{newdata} with the addition of the \code{var_name} variable.
#'   
#' @importFrom dplyr bind_rows
#'
#' @export
get_protection_level <- function(
  fit, var_name, newdata = NULL, 
  lvl = 0.5, ci_level = 0.95, tol = 10^(-7)
) {
  
  # Note: not checking newdata. Leaving it to the predict method.
  
  titre_low <- find_prot_titre_val(
    fit, var_name, newdata, "prot_u", lvl, ci_level
  )
  titre_point <- find_prot_titre_val(
    fit, var_name, newdata, "prot_point", lvl, ci_level
  )
  titre_high <- find_prot_titre_val(
    fit, var_name, newdata, "prot_l", lvl, ci_level
  )
  titre <- bind_rows(titre_low, titre_point, titre_high)
  titre$prot_prob <- lvl
  titre$est <- "point"
  titre$est[titre$protvar == "prot_u"] <- "low bound"
  titre$est[titre$protvar == "prot_l"] <- "upper bound"
  titre$protvar <- NULL
  titre
}

#' Search function for scaled logit protection covariate levels
#' 
#' The search engine behind \code{\link{get_protection_level}}. Should not
#' usually be necessary to call this directly.
#'
#' @param fit Object returned by \code{\link{sclr}}.
#' @param var_name Name of the covariate for which the protection values should
#' be calculated. This name should appear in the formula of the call to
#' \code{\link{sclr}} which was used to generate \code{fit}.
#' @param newdata A dataframe with all covariates except the one for which
#' protection values should be calculated.
#' @param prot_var_name A variable name among those returned by
#' \code{\link{predict.sclr}} which needs to equal \code{lvl} at the value of
#' \code{var_name} that is supposed to be found.
#' @param lvl Protection level to find titre values for. Default is 0.5 (50\%).
#' @param ci_level Confidence level for the calculated interval. 
#' Default is 0.95.
#' @param tol Tolerance. The values will be found numerically,
#' once the algorithm converges within \code{tol} of \code{lvl} 
#' it stops looking. Default is \eqn{10^(-7)}.
#'
#' @return A dataframe. Will have the same variables as \code{newdata} with
#' the addition of the \code{var_name} variable.
#' 
#' @importFrom rlang sym := .data
#' @importFrom dplyr mutate pull select if_else
#' @importFrom stats coef
#' @importFrom tibble tibble
#' 
#' @export
find_prot_titre_val <- function(
  fit, var_name, newdata = NULL, prot_var_name = "prot_point", lvl = 0.5, 
  ci_level = 0.95, tol = 10^(-7)
) {
  
  # Need to somehow initialise the return dataframe
  if (is.null(newdata)) newdata <- tibble(!!sym(var_name) := 0)
  
  # Initial guess interval
  newdata <- mutate(newdata, guess_low = -100, guess_high = 100)
  
  # Check if the variable is protective
  ests <- coef(fit)
  is_protective <- ests[grepl(var_name, names(ests))] > 0
  
  # Binary search
  while (TRUE) {
    
    newdata <- mutate(
      newdata, !!sym(var_name) := (.data$guess_low + .data$guess_high) / 2
    )
    prot_sample <- predict(fit, newdata, ci_level)
    
    curvals <- pull(prot_sample, !!sym(prot_var_name))
    notfound <- abs(curvals - lvl) > tol
    
    if (sum(notfound) == 0) {
      newdata <- select(newdata, -.data$guess_low, -.data$guess_high)
      newdata$protvar <- prot_var_name
      return(newdata)
    }
    
    newdata <- mutate(
      newdata,
      guess_low = if_else(
        xor(curvals < lvl, is_protective),
        .data$guess_low,
        !!sym(var_name)
      ),
      guess_high = if_else(
        xor(curvals > lvl, is_protective),
        .data$guess_high,
        !!sym(var_name)
      )
    )
  }
}
