#' Survey design
#' @description A wraper for \code{\link{svydesign}} function from the survey package, to define one of the following survey designs: two-stage cluster, simple (systematic) or stratified. In the first case, weights are calculated considering a sample with probability proportional to size and with replacement for the first stage and a simple random sampling for the second stage. Finite population correction is specified as the population size for each level of sampling.
#' @param sample \code{\link{data.frame}} with sample observations. for two-stage cluster designs, one of the columns must contain unique identifiers for PSU and another column must contain unique identifiers for Secondary Sampling Units (SSU).
#' @param psu.ssu \code{\link{data.frame}} with all Primary Sampling Units (PSU). First column contains PSU unique identifiers. Second column contains \code{\link{numeric}} PSU sizes. It is used only for two-stage cluster designs.
#' @param psu.col the column of \code{sample} containing the psu identifiers (for two-stage cluster designs). It is used only for two-stage cluster designs.
#' @param ssu.col the column of \code{sample} containing the ssu identifiers (for two-stage cluster designs). It is used only for two-stage cluster designs.
#' @param cal.col the column of \code{sample} with the variable to calibrate estimates. It must be used together with \code{cal.N}.
#' @param N for simple designs, a \code{\link{numeric}} value representing the total of sampling units in the population. for a stratified design, it is a column of \code{sample} indicating, for each observation, the total of sampling units in its respective strata. \code{N} is ignored in two-stage cluster designs.
#' @param strata for stratified designs, a column of \code{sample} indicating the strata memebership of each observation.
#' @param cal.N population total for the variable to calibrate the estimates. It must be used togheter with \code{cal.col}.
#' @param ... further arguments passed to \code{\link{svydesign}} function. 
#' @return An object of class survey.design.
#' @details For two-stage cluster designs, a PSU appearing in both \code{psu.ssu} and in \code{sample} must have the same identifier. SSU identifiers must be unique but can appear more than once if there is more than one observation per SSU. \code{sample} argument must have just the varibles to be estimated plus the variables required to define the design (two-stage cluster or stratified). \code{cal.col} and \code{cal.N} are needed only if estimates will be calibrated. The calibration is based on a population total.
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
#' design <- DesignSurvey(na.omit(cluster_sample),
#'                        psu.ssu = psu_ssu,
#'                        psu.col = "census_tract_id",
#'                        ssu.col = "interview_id",
#'                        cal.col = "number_of_persons",
#'                        cal.N = 129445)
#'
#' ## Simple design
#' # If data in cluster_sample were from a simple design:
#' design <- DesignSurvey(na.omit(cluster_sample), 
#'                        N = sum(psu_ssu$hh),
#'                        cal.N = 129445)
#' 
#' ## Stratified design
#' # Simulate strata and assume that the data in cluster_design came
#' # from a stratified design
#' cluster_sample$strat <- sample(c("urban", "rural"),
#'                                nrow(cluster_sample),
#'                                prob = c(.95, .05),
#'                                replace = TRUE)
#' cluster_sample$strat_size <- round(sum(psu_ssu$hh) * .95)
#' cluster_sample$strat_size[cluster_sample$strat == "rural"] <-
#'   round(sum(psu_ssu$hh) * .05)
#' design <- DesignSurvey(cluster_sample,
#'                        N = "strat_size",
#'                        strata = "strat",
#'                        cal.N = 129445)
#' 
DesignSurvey <- function (sample = NULL, psu.ssu = NULL, psu.col = NULL,
                          ssu.col = NULL, cal.col = NULL, N = NULL,
                          strata = NULL, cal.N = NULL, ...) 
{
  if(any(class(sample) == "data.frame")) {
    sample <- as.data.frame(sample)
  }
  if (!is.null(psu.ssu)) { # Two-stage cluster design
    psu.ssu <- as.data.frame(psu.ssu)
    if (sum(psu.ssu[, 1] %in% sample[, psu.col]) == 0) {
      stop("There is no matches between PSU identifiers\nfrom psu.ssu and sample. See details section from the help page.")
    }
    if (is.numeric(psu.col)) {
      names(sample)[psu.col] <- "psu.id"
    } else {
      names(sample)[names(sample) == psu.col] <- "psu.id"
    }
    if (is.numeric(ssu.col)) {
      names(sample)[ssu.col] <- "ssu.id"
    } else {
      names(sample)[names(sample) == ssu.col] <- "ssu.id"
    }
    sample$pop.size <- nrow(psu.ssu) # PSUs in the population
    sample <- merge(sample, psu.ssu, by.x = "psu.id", by.y = 1)
    names(sample)[ncol(sample)] <- "psu.size"
    psu.sample.size <- tapply(sample$psu.size, sample$psu.id, length) # SSUs per PSU
    psu.sample.size <- rep(psu.sample.size, psu.sample.size) # sampled SSUs per PSU
    w1 <- sum(psu.ssu[, 2]) / sample$psu.size
    w2 <- sample$psu.size / psu.sample.size
    sample$weights <- w1 * w2 / length(unique(sample$psu.id))
    dsn <- svydesign(ids = ~psu.id + ssu.id, fpc = ~pop.size + 
                       psu.size, weights = ~weights, data = sample, ...)
    for (i in c('psu.id', 'ssu.id', 'pop.size', 'psu.size', 'weights')) {
      dsn$variables <- dsn$variables[-which(names(dsn$variables) == i)]
    }
    if (!is.null(cal.col) & !is.null(cal.N)) {
      dsn <- calibrate(dsn, formula = ~ sample[, cal.col]-1, population = cal.N)
    }
    return(dsn)
  }
  if (!is.null(N) & is.null(strata)) { # Simple design
    sample$N <- N
    dsn <- svydesign(ids = ~1, fpc = ~N, data = sample)
    if (!is.null(cal.col) & !is.null(cal.N)) {
      dsn <- calibrate(dsn, formula = ~ sample[, cal.col]-1, population = cal.N)
    }
    dsn$variables <- dsn$variables[-which(names(dsn$variables) == 'N')]
    dsn$simple <- 'yes'
    return(dsn)
  }
  if (!is.null(N) & !is.null(strata)) { # Stratified design
    dsn <- svydesign(ids = ~1, fpc = ~sample[, N],
                     strata = ~sample[, strata], data = sample)
    if (!is.null(cal.col) & !is.null(cal.N)) {
      dsn <- calibrate(dsn, formula = ~ sample[, cal.col]-1, population = cal.N)
    }
    if (is.numeric(N)) {
      N <- names(sample)[N]
    }
    if (is.numeric(strata)) {
      strata <- names(sample)[strata]
    }
    for (i in c(N, strata)) {
      dsn$variables <- dsn$variables[-which(names(dsn$variables) == i)]
    }
    return(dsn)
  }
}