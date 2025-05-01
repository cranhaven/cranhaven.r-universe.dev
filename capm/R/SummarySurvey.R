#' Summary statistics for sample surveys
#' @description Wraps functions for summary statistics from survey package.
#' @param design an output form \code{\link{DesignSurvey}} function.
#' @param variables \code{\link{character}} \code{\link{vector}} with the type of estimate for each variable contained in \code{design}. Possible types: total, mean, and prop (see details).
#' @param conf.level the confidence level required.
#' @param rnd the number of decimal places (round) or significant digits (signif) to be used. If \code{NA}, scientific notation is used.
#' @return Matrix with survey summaries. The error (column "Error (%)") for totals and means is equal to the coefficient of variation times \code{conf.level} times 100; for proportions it is equal to the upper confidence limit minus the point estimate times 100, that is, percent points.
#' @details The length of \code{variables} must be equal to the length of \code{names(design$variables)} (see examples).
#' @references Lumley, T. (2011). Complex surveys: A guide to analysis using R (Vol. 565). Wiley.
#' 
#' Baquero, O. S., Marconcin, S., Rocha, A., & Garcia, R. D. C. M. (2018). Companion animal demography and population management in Pinhais, Brazil. Preventive Veterinary Medicine.
#' 
#' \url{http://oswaldosantos.github.io/capm}
#' @export
#' @examples
#' data("cluster_sample")
#' data("psu_ssu")
#' 
#' ## Calibrated two-stage cluster design
#' cs <- cluster_sample[ , c("interview_id",
#'                           "census_tract_id",
#'                           "number_of_persons",
#'                           "number_of_dogs",
#'                           "number_of_cats")]
#' 
#' design <- DesignSurvey(na.omit(cs),
#'                        psu.ssu = psu_ssu,
#'                        psu.col = "census_tract_id",
#'                        ssu.col = "interview_id",
#'                        cal.col = "number_of_persons",
#'                        cal.N = 129445)
#' 
#' SummarySurvey(design, c("total", "total", "total"))
#' 
SummarySurvey <- function(design = NULL, variables = NULL, conf.level = 0.95, rnd = 3) {
  if (length(variables) != length(names(design$variables))) {
    stop('The length of variables argument must be equal to the length of names(design$variables)')
  }
  match1 <- names(design$variables)
  match2 <- c('psu.id', 'ssu.id', 'pop.size', 'psu.size')
  matches <- which(!is.na(match(match1, match2)))
  variables[matches] <- ''
  z <- abs(round(qnorm((1 - conf.level) / 2, 0, 1), 2))
  vrs <- design$variables
  out <- NULL
  for (i in 1:length(variables)) {
    if (variables[i] == 'total') {
      tmp <- svytotal(~ vrs[, i], design, na.rm = T, deff = T)
      tmp1 <- as.matrix(cbind(tmp, SE(tmp), confint(tmp), 
                              deff(tmp), cv(tmp) * z * 100), nr = 1)
      ci <- attributes(confint(tmp, level = conf.level))$dimnames[[2]]
      rownames(tmp1) <- paste0('Total_', names(vrs)[i])
      out <- rbind(out, tmp1)
    }
    if (variables[i] == 'mean') {
      tmp <- svymean(~ vrs[, i], design, na.rm = T, deff = T)
      tmp1 <- as.matrix(cbind(tmp, SE(tmp), confint(tmp), 
                              deff(tmp), cv(tmp) * z * 100), nr = 1)
      ci <- attributes(confint(tmp, level = conf.level))$dimnames[[2]]
      rownames(tmp1) <- paste0('Mean_', names(vrs)[i])
      out <- rbind(out, tmp1)
    }
    if (variables[i] == 'prop') {
      tmp <- svymean(~ vrs[, i], design, na.rm = T, deff = T)
      tmp1 <- as.matrix(cbind(tmp, SE(tmp), confint(tmp), 
                              deff(tmp), confint(tmp)[, 2] - tmp[1] * 100), nr = 1)
      ci <- attributes(confint(tmp, level = conf.level))$dimnames[[2]]
      rownames(tmp1) <- paste0('Prop_', rownames(tmp1))
      rownames(tmp1) <- gsub('vrs\\[, i\\]', paste0(names(vrs)[i]), rownames(tmp1))
      out <- rbind(out, tmp1)
    }
  }
  colnames(out) <- c('Estimate', 'SE', ci[1], ci[2], 'Deff', 'Error (%)')
  if ('simple' %in% names(design)) {
    out <- out[ , -5]
  }
  ifelse (is.na(rnd), return(out), return(round(out, rnd)))
}
