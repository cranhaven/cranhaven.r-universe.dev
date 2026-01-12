#' Print results of cv_auc
#' @export
#' @param x An object of class "cvauc"
#' @param ci_level Level of confidence interval to print. Defaults to 0.95.
#' @param se_type The type of standard error (currently only "std")
#' @param ... Other options (not currently used) 
print.cvauc <- function(x, ci_level = 0.95, se_type = "std", ...){
	cvtmle_ci <- if(se_type == "std"){
		c(x$est_cvtmle - abs(stats::qnorm((1-ci_level)/2))*x$se_cvtmle, x$est_cvtmle + abs(stats::qnorm((1-ci_level)/2))*x$se_cvtmle)
	}else{
		stats::plogis(c(stats::qlogis(x$est_cvtmle) - abs(stats::qnorm((1-ci_level)/2))*x$se_cvtmle, stats::qlogis(x$est_cvtmle) + abs(stats::qnorm((1-ci_level)/2))*x$se_cvtmle))
	}
	cvtmle_rslt <- c(x$est_cvtmle, x$se_cvtmle, cvtmle_ci)

	esteq_ci <- if(se_type == "std"){
		c(x$est_esteq - abs(stats::qnorm((1-ci_level)/2))*x$se_esteq, x$est_esteq + abs(stats::qnorm((1-ci_level)/2))*x$se_esteq)
	}else{
		stats::plogis(c(stats::qlogis(x$est_esteq) - abs(stats::qnorm((1-ci_level)/2))*x$se_esteq, stats::qlogis(x$est_esteq) + abs(stats::qnorm((1-ci_level)/2))*x$se_esteq))
	}
	esteq_rslt <- c(x$est_esteq, x$se_esteq, esteq_ci)	

	onestep_ci <- if(se_type == "std"){
		c(x$est_onestep - abs(stats::qnorm((1-ci_level)/2))*x$se_onestep, x$est_onestep + abs(stats::qnorm((1-ci_level)/2))*x$se_onestep)
	}else{
		stats::plogis(c(stats::qlogis(x$est_onestep) - abs(stats::qnorm((1-ci_level)/2))*x$se_onestep, stats::qlogis(x$est_onestep) + abs(stats::qnorm((1-ci_level)/2))*x$se_onestep))
	}
	onestep_rslt <- c(x$est_onestep, x$se_onestep, onestep_ci)

	empirical_ci <- if(se_type == "std"){
		c(x$est_empirical - abs(stats::qnorm((1-ci_level)/2))*x$se_empirical, x$est_empirical + abs(stats::qnorm((1-ci_level)/2))*x$se_empirical)
	}else{
		stats::plogis(c(stats::qlogis(x$est_empirical) - abs(stats::qnorm((1-ci_level)/2))*x$se_empirical, stats::qlogis(x$est_empirical) + abs(stats::qnorm((1-ci_level)/2))*x$se_empirical))
	}
	empirical_rslt <- c(x$est_empirical, x$se_empirical, empirical_ci)

	out <- data.frame(rbind(cvtmle_rslt, onestep_rslt, esteq_rslt, empirical_rslt))

  	colnames(out) <- c("est","se","cil","ciu")
  	row.names(out) <- c("cvtmle","onestep","esteq","standard")
  	print(out)
}

#' Print results of cv_scrnp
#' @export
#' @param x An object of class "cvauc"
#' @param ci_level Level of confidence interval to print. Defaults to 0.95. 
#' @param se_type The type of standard error (currently only "std")
#' @param ... Other options (not currently used) 
print.scrnp <- function(x, se_type = "std", ci_level = 0.95, ...){
	cvtmle_ci <- if(se_type == "std"){
		c(x$est_cvtmle - abs(stats::qnorm((1-ci_level)/2))*x$se_cvtmle, x$est_cvtmle + abs(stats::qnorm((1-ci_level)/2))*x$se_cvtmle)
	}else{
		stats::plogis(c(stats::qlogis(x$est_cvtmle) - abs(stats::qnorm((1-ci_level)/2))*x$se_cvtmle, stats::qlogis(x$est_cvtmle) + abs(stats::qnorm((1-ci_level)/2))*x$se_cvtmle))
	}
	cvtmle_rslt <- c(x$est_cvtmle, x$se_cvtmle, cvtmle_ci)

	esteq_ci <- if(se_type == "std"){
		c(x$est_esteq - abs(stats::qnorm((1-ci_level)/2))*x$se_esteq, x$est_esteq + abs(stats::qnorm((1-ci_level)/2))*x$se_esteq)
	}else{
		stats::plogis(c(stats::qlogis(x$est_esteq) - abs(stats::qnorm((1-ci_level)/2))*x$se_esteq, stats::qlogis(x$est_esteq) + abs(stats::qnorm((1-ci_level)/2))*x$se_esteq))
	}
	esteq_rslt <- c(x$est_esteq, x$se_esteq, esteq_ci)	

	onestep_ci <- if(se_type == "std"){
		c(x$est_onestep - abs(stats::qnorm((1-ci_level)/2))*x$se_onestep, x$est_onestep + abs(stats::qnorm((1-ci_level)/2))*x$se_onestep)
	}else{
		stats::plogis(c(stats::qlogis(x$est_onestep) - abs(stats::qnorm((1-ci_level)/2))*x$se_onestep, stats::qlogis(x$est_onestep) + abs(stats::qnorm((1-ci_level)/2))*x$se_onestep))
	}
	onestep_rslt <- c(x$est_onestep, x$se_onestep, onestep_ci)

	empirical_ci <- if(se_type == "std"){
		c(x$est_empirical - abs(stats::qnorm((1-ci_level)/2))*x$se_empirical, x$est_empirical + abs(stats::qnorm((1-ci_level)/2))*x$se_empirical)
	}else{
		stats::plogis(c(stats::qlogis(x$est_empirical) - abs(stats::qnorm((1-ci_level)/2))*x$se_empirical, stats::qlogis(x$est_empirical) + abs(stats::qnorm((1-ci_level)/2))*x$se_empirical))
	}
	empirical_rslt <- c(x$est_empirical, x$se_empirical, empirical_ci)

	out <- data.frame(rbind(cvtmle_rslt, onestep_rslt, esteq_rslt, empirical_rslt))

  	colnames(out) <- c("est","se","cil","ciu")
  	row.names(out) <- c("cvtmle","onestep","esteq","standard")
  	print(out)
}