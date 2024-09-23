# bhpm.BB.summary
# Model bhpm.BB
# R. Carragher
# Date: 29/06/2018

Id <- "$Id: bhpm.cluster.BB.hier3.lev2.summary.stats.R,v 1.13 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.cluster.BB.dep.lev2.summary.stats <- function(raw, prob = 0.95)
{
	s_base = bhpm.cluster.1a.dep.lev2.summary.stats(raw, prob)

	if (is.null(s_base)) {
		return(NULL)
	}

	# Check which variables we are monitoring
	monitor = raw$monitor
	theta_mon = monitor[monitor$variable == "theta",]$monitor
	gamma_mon = monitor[monitor$variable == "gamma",]$monitor
	mu.theta_mon = monitor[monitor$variable == "mu.theta",]$monitor
	mu.gamma_mon = monitor[monitor$variable == "mu.gamma",]$monitor
	sigma2.theta_mon = monitor[monitor$variable == "sigma2.theta",]$monitor
	sigma2.gamma_mon = monitor[monitor$variable == "sigma2.gamma",]$monitor
	mu.theta.0_mon = monitor[monitor$variable == "mu.theta.0",]$monitor
	mu.gamma.0_mon = monitor[monitor$variable == "mu.gamma.0",]$monitor
	tau2.theta.0_mon = monitor[monitor$variable == "tau2.theta.0",]$monitor
	tau2.gamma.0_mon = monitor[monitor$variable == "tau2.gamma.0",]$monitor
	pi_mon = monitor[monitor$variable == "pi",]$monitor
	alpha_pi_mon = monitor[monitor$variable == "alpha.pi",]$monitor
	beta_pi_mon = monitor[monitor$variable == "beta.pi",]$monitor
	theta.trt.grps <- raw$Trt.Grps[ raw$Trt.Grps$param == "theta", ]$Trt.Grp

	if (pi_mon == 1 && !("pi" %in% names(raw))) {
		message("Missing pi data");
		return(NULL)
	}

	if (alpha_pi_mon == 1 && !("alpha.pi" %in% names(raw))) {
		message("Missing alpha.pi data");
		return(NULL)
	}

	if (beta_pi_mon == 1 && !("beta.pi" %in% names(raw))) {
		message("Missing beta.pi data");
		return(NULL)
	}

	nchains = raw$chains

	pi_summ = data.frame(Trt.Grp = integer(0), Cluster = character(0), Outcome.Grp = character(0), mean = numeric(0),
													median = numeric(0), hpi_lower = numeric(0), hpi_upper = numeric(0),
													SD = numeric(0), SE = numeric(0))
	alpha.pi_summ = data.frame(Trt.Grp = integer(0), mean = numeric(0), median = numeric(0),
													hpi_lower = numeric(0), hpi_upper = numeric(0),
													SD = numeric(0), SE = numeric(0))
	beta.pi_summ = data.frame(Trt.Grp = integer(0), mean = numeric(0), median = numeric(0),
													hpi_lower = numeric(0), hpi_upper = numeric(0),
													SD = numeric(0), SE = numeric(0))

	samples_combined <- rep(NA, (raw$iter - raw$burnin)*nchains)

	if (pi_mon == 1) {
		for (i in 1:raw$nClusters) {
			for (b in 1:raw$nOutcome.Grp[i]) {
				for (t in 1:(raw$nTreatments - 1)) {
					s = M_global$summaryStats(raw$pi[, t, i, b, ], nchains, prob)
					row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b], mean = s[1],
							median = s[2], hpi_lower = s[3], hpi_upper = s[4], SD = s[5], SE = s[6])
					pi_summ = rbind(pi_summ, row)
				}
			}
		}	
	}

	if (alpha_pi_mon == 1) {
		for (t in 1:(raw$nTreatments - 1)) {
			s = M_global$summaryStats(raw$alpha.pi[,t,], nchains, prob)
			row <- data.frame(Trt.Grp = theta.trt.grps[t], mean = s[1], median = s[2], hpi_lower = s[3], hpi_upper = s[4],
					SD = s[5], SE = s[6])
			alpha.pi_summ = rbind(alpha.pi_summ, row)
		}
	}

	if (beta_pi_mon == 1) {
		for (t in 1:(raw$nTreatments - 1)) {
			s = M_global$summaryStats(raw$beta.pi[,t,], nchains, prob)
			row <- data.frame(Trt.Grp = theta.trt.grps[t], mean = s[1], median = s[2], hpi_lower = s[3], hpi_upper = s[4],
					SD = s[5], SE = s[6])
			beta.pi_summ = rbind(beta.pi_summ, row)
		}
	}

	rownames(pi_summ) <- NULL
	rownames(alpha.pi_summ) <- NULL
	rownames(alpha.pi_summ) <- NULL

	s_BB = list(pi.summary = pi_summ, alpha.pi.summary = alpha.pi_summ, beta.pi.summary = beta.pi_summ)

	summary.stats= c(s_base, s_BB)

	attr(summary.stats, "model") = attr(raw, "model")

	return(summary.stats)
}

bhpm.cluster.BB.dep.lev2.print.summary.stats <- function(summ)
{
	if (is.null(summ)) {
		message("NULL summary data");
		return(NULL)
	}

	# Check which variables we are monitoring
	monitor = summ$monitor
	theta_mon = monitor[monitor$variable == "theta",]$monitor
	gamma_mon = monitor[monitor$variable == "gamma",]$monitor
	mu.theta_mon = monitor[monitor$variable == "mu.theta",]$monitor
	mu.gamma_mon = monitor[monitor$variable == "mu.gamma",]$monitor
	sigma2.theta_mon = monitor[monitor$variable == "sigma2.theta",]$monitor
	sigma2.gamma_mon = monitor[monitor$variable == "sigma2.gamma",]$monitor
	mu.theta.0_mon = monitor[monitor$variable == "mu.theta.0",]$monitor
	mu.gamma.0_mon = monitor[monitor$variable == "mu.gamma.0",]$monitor
	tau2.theta.0_mon = monitor[monitor$variable == "tau2.theta.0",]$monitor
	tau2.gamma.0_mon = monitor[monitor$variable == "tau2.gamma.0",]$monitor
	pi_mon = monitor[monitor$variable == "pi",]$monitor
	alpha_pi_mon = monitor[monitor$variable == "alpha.pi",]$monitor
	beta_pi_mon = monitor[monitor$variable == "beta.pi",]$monitor

	model = attr(summ, "model")
	if (is.null(model)) {
		message("Missing model attribute");
		return(NULL)
	}

	if (theta_mon == 1 && !("theta.summary" %in% names(summ))) {
		message("Missing theta.summary data");
		return(NULL)
	}
	if (gamma_mon == 1 && !("gamma.summary" %in% names(summ))) {
		message("Missing gamma.summary data");
		return(NULL)
	}
	if (mu.gamma_mon == 1 && !("mu.gamma.summary" %in% names(summ))) {
		message("Missing mu.gamma.summary data");
		return(NULL)
	}
	if (mu.theta_mon == 1 && !("mu.theta.summary" %in% names(summ))) {
		message("Missing mu.theta.summary data");
		return(NULL)
	}
	if (sigma2.gamma_mon == 1 && !("sigma2.gamma.summary" %in% names(summ))) {
		message("Missing sigma2.gamma.summary data");
		return(NULL)
	}
	if (sigma2.theta_mon == 1 && !("sigma2.theta.summary" %in% names(summ))) {
		message("Missing sigma2.theta.summary data");
		return(NULL)
	}
	if (mu.gamma.0_mon == 1 && !("mu.gamma.0.summary" %in% names(summ))) {
		message("Missing mu.gamma.0.summary data");
		return(NULL)
	}
	if (mu.theta.0_mon == 1 && !("mu.theta.0.summary" %in% names(summ))) {
		message("Missing mu.theta.0.summary data");
		return(NULL)
	}
	if (tau2.theta.0_mon == 1 && !("tau2.theta.0.summary" %in% names(summ))) {
		message("Missing tau2.theta.0.summary data");
		return(NULL)
	}
	if (tau2.gamma.0_mon == 1 && !("tau2.gamma.0.summary" %in% names(summ))) {
		message("Missing tau2.gamma.0.summary data");
		return(NULL)
	}

	if (pi_mon == 1 && !("pi.summary" %in% names(summ))) {
		message("Missing pi.summary data");
		return(NULL)
	}

	if (alpha_pi_mon == 1 && !("alpha.pi.summary" %in% names(summ))) {
		message("Missing alpha.pi.summary data");
		return(NULL)
	}

	if (beta_pi_mon == 1 && !("beta.pi.summary" %in% names(summ))) {
		message("Missing beta.pi.summary data");
		return(NULL)
	}

	cat(sprintf("Variable          Mean            (HPI)          SD       SE\n"))
	cat(sprintf("=============================================================\n"))
	if (gamma_mon == 1)
		for (i in 1:nrow(summ$gamma.summary)) {
			row = summ$gamma.summary[i, ]
			cat(sprintf("gamma[%s, %s, %s]: %0.6f (%0.6f %0.6f) %0.6f %0.6f\n", row$Cluster,
					row$Outcome.Grp, row$Outcome, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (theta_mon == 1)
		for (i in 1:nrow(summ$theta.summary)) {
			row = summ$theta.summary[i, ]
			cat(sprintf("theta[%d %s, %s, %s]: %0.6f (%0.6f %0.6f) %0.6f %0.6f\n", row$Trt.Grp, row$Cluster,
					row$Outcome.Grp, row$Outcome, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (mu.gamma_mon == 1)
		for (i in 1:nrow(summ$mu.gamma.summary)) {
			row = summ$mu.gamma.summary[i, ]
			cat(sprintf("mu.gamma[%s, %s]: %0.6f (%0.6f %0.6f) %0.6f %0.6f\n", row$Cluster,
					row$Outcome.Grp, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (mu.theta_mon == 1)
		for (i in 1:nrow(summ$mu.theta.summary)) {
			row = summ$mu.theta.summary[i, ]
			cat(sprintf("mu.theta[%d %s, %s]: %0.6f (%0.6f %0.6f) %0.6f %0.6f\n", row$Trt.Grp, row$Cluster,
					row$Outcome.Grp, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (sigma2.gamma_mon == 1)
		for (i in 1:nrow(summ$sigma2.gamma.summary)) {
			row = summ$sigma2.gamma.summary[i, ]
			cat(sprintf("sigma2.gamma[%s, %s]: %0.6f (%0.6f %0.6f) %0.6f %0.6f\n", row$Cluster,
					row$Outcome.Grp, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (sigma2.theta_mon == 1)
		for (i in 1:nrow(summ$sigma2.theta.summary)) {
			row = summ$sigma2.theta.summary[i, ]
			cat(sprintf("sigma2.theta[%d %s, %s]: %0.6f (%0.6f %0.6f) %0.6f %0.6f\n", row$Trt.Grp, row$Cluster,
					row$Outcome.Grp, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (pi_mon == 1)
		for (i in 1:nrow(summ$pi.summary)) {
			row = summ$pi.summary[i, ]
			cat(sprintf("pi[%d %s, %s]: %0.6f (%0.6f %0.6f) %0.6f %0.6f\n", row$Trt.Grp, row$Cluster,
					row$Outcome.Grp, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (mu.gamma.0_mon == 1)
		for (i in 1:nrow(summ$mu.gamma.0.summary)) {
			row = summ$mu.gamma.0.summary[i, ]
			cat(sprintf("mu.gamma.0: %0.6f (%0.6f %0.6f) %0.6f %0.6f\n",
					row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (mu.theta.0_mon == 1)
		for (i in 1:nrow(summ$mu.theta.0.summary)) {
			row = summ$mu.theta.0.summary[i, ]
			cat(sprintf("mu.theta.0: %d %0.6f (%0.6f %0.6f) %0.6f %0.6f\n",
					row$Trt.Grp, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (tau2.gamma.0_mon == 1)
		for (i in 1:nrow(summ$tau2.gamma.0.summary)) {
			row = summ$tau2.gamma.0.summary[i, ]
			cat(sprintf("tau2.gamma.0: %0.6f (%0.6f %0.6f) %0.6f %0.6f\n",
					row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (tau2.theta.0_mon == 1)
		for (i in 1:nrow(summ$tau2.theta.0.summary)) {
			row = summ$tau2.theta.0.summary[i, ]
			cat(sprintf("tau2.theta.0: %d %0.6f (%0.6f %0.6f) %0.6f %0.6f\n",
					row$Trt.Grp, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (alpha_pi_mon == 1)
		for (i in 1:nrow(summ$alpha.pi.summary)) {
			row = summ$alpha.pi.summary[i, ]
			cat(sprintf("alpha.pi: %d %0.6f (%0.6f %0.6f) %0.6f %0.6f\n",
					row$Trt.Grp, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
	if (beta_pi_mon == 1)
		for (i in 1:nrow(summ$beta.pi.summary)) {
			row = summ$beta.pi.summary[i, ]
			cat(sprintf("beta.pi: %d %0.6f (%0.6f %0.6f) %0.6f %0.6f\n",
					row$Trt.Grp, row$mean, row$hpi_lower, row$hpi_upper, row$SD, row$SE))
		}
}
