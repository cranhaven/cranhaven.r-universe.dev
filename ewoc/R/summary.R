#'@export
summary.ewoc_d1classical <- function(object, ..., print = TRUE){

  p00 <- data.frame(min_dose = object$trial$min_dose,
                    max_dose = object$trial$max_dose,
                    theta = object$trial$theta,
                    alpha = object$trial$alpha,
                    n = length(object$trial$response))
  colnames(p00) <- c("Minimum Dose", "Maximum Dose", "Theta",
                       "Alpha", "Number of patients")

  hpd_dose <- coda::HPDinterval(coda::as.mcmc(object$mtd))
  hpd_dose <- round(as.numeric(hpd_dose), 2)
  next_dose <- round(as.numeric(object$next_dose), 2)
  tab01 <- data.frame(next_dose, hpd_dose[1], hpd_dose[2])

  hpd_pdlt <- coda::HPDinterval(coda::as.mcmc(object$pdlt))
  hpd_pdlt <- round(as.numeric(hpd_pdlt), 2)
  prob_dlt <- round(median(object$pdlt), 2)
  tab02 <- data.frame(prob_dlt, hpd_pdlt[1], hpd_pdlt[2])

  if (print){
    cat("Conditions\n")
    print(p00)
    cat("\n")

    cat("Next Dose\n")
    p01 <- data.frame(estimate = tab01[, 1],
                        hpd = paste0("(", tab01[, 2], " ; ", tab01[, 3], ")"))
    colnames(p01) <- c("Estimate", "95% HPD")
    print(p01)
    cat("\n")

    cat("P(DLT| next dose)\n")
    p02 <- data.frame(estimate = tab02[, 1],
                        hpd = paste0("(", tab02[, 2], " ; ", tab02[, 3], ")"))
    colnames(p02) <- c("Estimate", "95% HPD")
    print(p02)

  } else {

    out <- list(next_dose = next_dose, hpd_dose = hpd_dose,
                prob_dlt = prob_dlt, hpd_pdlt = hpd_pdlt)
    return(out)
  }
}

#'@export
summary.ewoc_d1extended <- function(object, ..., print = TRUE){

  p00 <- data.frame(min_dose = object$trial$min_dose,
                    max_dose = object$trial$max_dose,
                    theta = object$trial$theta,
                    alpha = object$trial$alpha,
                    n = length(object$trial$response))
  colnames(p00) <- c("Minimum Dose", "Maximum Dose", "Theta",
                       "Alpha", "Number of patients")

  hpd_dose <- coda::HPDinterval(coda::as.mcmc(object$mtd))
  hpd_dose <- round(as.numeric(hpd_dose), 2)
  next_dose <- round(as.numeric(object$next_dose), 2)
  tab01 <- data.frame(next_dose, hpd_dose[1], hpd_dose[2])

  hpd_pdlt <- coda::HPDinterval(coda::as.mcmc(object$pdlt))
  hpd_pdlt <- round(as.numeric(hpd_pdlt), 2)
  prob_dlt <- round(median(object$pdlt), 2)
  tab02 <- data.frame(prob_dlt, hpd_pdlt[1], hpd_pdlt[2])

  if (print){
    cat("Conditions\n")
    print(p00)
    cat("\n")

    cat("Next Dose\n")
    p01 <- data.frame(estimate = tab01[, 1],
                        hpd = paste0("(", tab01[, 2], " ; ", tab01[, 3], ")"))
    colnames(p01) <- c("Estimate", "95% HPD")
    print(p01)
    cat("\n")

    cat("P(DLT| next dose)\n")
    p02 <- data.frame(estimate = tab02[, 1],
                        hpd = paste0("(", tab02[, 2], " ; ", tab02[, 3], ")"))
    colnames(p02) <- c("Estimate", "95% HPD")
    print(p02)

  } else {

    out <- list(next_dose = next_dose, hpd_dose = hpd_dose,
                prob_dlt = prob_dlt, hpd_pdlt = hpd_pdlt)
    return(out)
  }
}

#'@export
summary.ewoc_d1ph <- function(object, ..., print = TRUE){

  p00 <- data.frame(min_dose = object$trial$min_dose,
                    max_dose = object$trial$max_dose,
                    theta = object$trial$theta,
                    alpha = object$trial$alpha,
                    n = nrow(object$trial$response))
  colnames(p00) <- c("Minimum Dose", "Maximum Dose", "Theta",
                       "Alpha", "Number of patients")

  hpd_dose <- coda::HPDinterval(coda::as.mcmc(object$mtd))
  hpd_dose <- round(as.numeric(hpd_dose), 2)
  next_dose <- round(as.numeric(object$next_dose), 2)
  tab01 <- data.frame(next_dose, hpd_dose[1], hpd_dose[2])

  hpd_pdlt <- coda::HPDinterval(coda::as.mcmc(object$pdlt))
  hpd_pdlt <- round(as.numeric(hpd_pdlt), 2)
  prob_dlt <- round(median(object$pdlt), 2)
  tab02 <- data.frame(prob_dlt, hpd_pdlt[1], hpd_pdlt[2])

  if (print){
    cat("Conditions\n")
    print(p00)
    cat("\n")

    cat("Next Dose\n")
    p01 <- data.frame(estimate = tab01[, 1],
                        hpd = paste0("(", tab01[, 2], " ; ", tab01[, 3], ")"))
    colnames(p01) <- c("Estimate", "95% HPD")
    print(p01)
    cat("\n")

    cat("P(DLT| next dose)\n")
    p02 <- data.frame(estimate = tab02[, 1],
                        hpd = paste0("(", tab02[, 2], " ; ", tab02[, 3], ")"))
    colnames(p02) <- c("Estimate", "95% HPD")
    print(p02)

  } else {

    out <- list(next_dose = next_dose, hpd_dose = hpd_dose,
                prob_dlt = prob_dlt, hpd_pdlt = hpd_pdlt)
    return(out)
  }

}

