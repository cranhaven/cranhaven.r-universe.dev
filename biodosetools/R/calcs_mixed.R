# Mixed fields function David ----

#' Calculate absorbed dose for mixed fields exposure
#'
#' @param num_cases number of cases to estimate.
#' @param dics dicentrics.
#' @param cells total cells.
#' @param coef_gamma Coefficients for gamma curve.
#' @param cov_gamma Covariance matrix for gamma curve.
#' @param coef_neutron Coefficients for neutron curve.
#' @param cov_neutron Covariance matrix for neutron curve.
#' @param ratio gamma-neutron ratio.
#' @param p photon contribution in mixed curve.
#'
#' @import msm openxlsx
#'
#' @return dose estimation
#' @example man/examples/fun.estimate.criticality_example.R
#' @export

fun.estimate.criticality <- function(num_cases, dics, cells, coef_gamma, cov_gamma, coef_neutron, cov_neutron, ratio, p){

  if(length(coef_neutron) == 2)
    coef_neutron <- c(coef_neutron, 0)

  if(nrow(cov_neutron) == 2){
    tmp_cov <- matrix(0, 3, 3)
    tmp_cov[1:2, 1:2] <- cov_neutron
    cov_neutron <- tmp_cov
  }
  output <- list()
  for(i in 1:num_cases){
    r <- 1/ratio
    mu_dics <- dics[[i]] / cells[[i]]
    sd.Y <- sqrt(dics[[i]]) / cells[[i]]
    cov <- matrix(0, nrow = 7, ncol =  7)
    cov[1:3, 1:3] <- cov_gamma
    cov[4:6, 4:6] <- cov_neutron
    cov[7, 7] <- sd.Y^2

    c <- (coef_neutron[1] + coef_gamma[1])/2 - mu_dics
    b <- coef_gamma[2] + ((coef_neutron[2] - p*coef_gamma[2])/(1-p))*r
    tmp <- coef_neutron[3] - p*p*coef_gamma[3]
    a <- coef_gamma[3] + ((ifelse(tmp < 0, 0, tmp))/((1-p)^2))*r^2

    #cat(a, "\n")
    #cat(coef_gamma, "\n")
    coef_neutron_cleaned <- c((coef_neutron[1] + coef_gamma[1])/2,
                              ((coef_neutron[2] - p*coef_gamma[2])/(1-p)),
                              (tmp/((1-p)^2)))
    #cat("Coefficients cleaned neutron curve: ", coef_neutron_cleaned, "\n")

    est_gamma <- (-b + sqrt(b^2-4*a*c))/(2*a)
    est_neutron <- r * est_gamma
    est_total <- est_gamma + est_neutron

    if(tmp >= 0){
      tmp_formula <- paste0("(-(x2 + ((x5 - ",p, "*x2)/(1-", p, "))*", r, ") +
                         sqrt(((x2 + ((x5 - ", p, "*x2)/(1-", p, "))*", r, "))^2 -
                                4*(x3 + ((x6 - ", p*p, "*x3)/((1-", p, ")^2))*", r, "^2)*((x4 + x1)/2 - x7)))/(2*(x3 + ((x6 - ", p*p, "*x3)/((1-", p, ")^2))*", r^2, "))")
      formula_neutron <- paste0("~", r, "*", tmp_formula)
      formula_gamma  <- paste0("~", tmp_formula)
      formula_total <- paste0("~", r+1, "*", tmp_formula)
    }else{
      tmp_formula <- paste0("(-(x2 + ((x5 - ",p, "*x2)/(1-", p, "))*", r, ") +
                         sqrt(((x2 + ((x5 - ", p, "*x2)/(1-", p, "))*", r, "))^2 - 4*x3*((x4 + x1)/2 - x7)))/(2*x3)")
      formula_neutron <- paste0("~", r, "*", tmp_formula)
      formula_gamma  <- paste0("~", tmp_formula)
      formula_total <- paste0("~", r+1, "*", tmp_formula)

    }


    sd_gamma <- deltamethod(stats::as.formula(formula_gamma), mean = c(coef_gamma, coef_neutron, mu_dics), cov = cov)
    sd_neutron <- deltamethod(stats::as.formula(formula_neutron), mean = c(coef_gamma, coef_neutron, mu_dics), cov = cov)
    sd_total <- deltamethod(stats::as.formula(formula_total), mean = c(coef_gamma, coef_neutron, mu_dics), cov = cov)
    est_neutron <- c(est = est_neutron, lwr = est_neutron - 1.96 * sd_neutron, upr = est_neutron + 1.96 * sd_neutron)
    est_gamma <- c(est = est_gamma, lwr = est_gamma - 1.96 * sd_gamma, upr = est_gamma + 1.96 * sd_gamma)
    est_total <- c(est = est_total, lwr = est_total - 1.96 * sd_total, upr = est_total + 1.96 * sd_total)
    names(est_neutron) <-names(est_gamma) <-names(est_total) <- c("est", "lwr", "upr")

    output[[i]] <- list(gamma = est_gamma, neutron = est_neutron, total = est_total)
    if(any(sapply(output[[i]], is.na))){
      showModal(modalDialog(
        title = "Error",
        "The values you've entered are causing a mathematical error. Please check your input values and try again.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      req(FALSE)
    }
  }
  return(output)

}
