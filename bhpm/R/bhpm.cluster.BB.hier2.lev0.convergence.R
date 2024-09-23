# bhpm.cluster.BB.hier2.lev0.convergence.diag
# Model bhpm.BB
# R. Carragher
# Date: 29/06/2018
#
# If the MCMC simulation has been run for more than one chain report the Gelman-Rubin statistic.
# If the MCMC simulation has been run for only one chain report the Geweke diagnostic (Z-score)
#

Id <- "$Id: bhpm.cluster.BB.hier2.lev0.convergence.R,v 1.11 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.cluster.BB.hier2.lev0.convergence.diag <- function(raw, debug_diagnostic = FALSE)
{
	c_base = bhpm.cluster.1a.hier2.lev0.convergence.diag(raw, debug_diagnostic)

	if (is.null(c_base)) {
		return(NULL)
	}

	monitor = raw$monitor
	theta_mon = monitor[monitor$variable == "theta",]$monitor
	pi_mon = monitor[monitor$variable == "pi",]$monitor
	theta.trt.grps <- raw$Trt.Grps[ raw$Trt.Grps$param == "theta", ]$Trt.Grp

	nchains = raw$chains

	if (pi_mon == 1 && !("pi" %in% names(raw))) {
		message("Missing pi data")
		return(NULL)
	}

	if (theta_mon == 1 && !("theta_acc" %in% names(raw))) {
		message("Missing theta_acc data")
		return(NULL)
	}

	pi_conv = data.frame(Trt.Grp = integer(0), Cluster = character(0), Outcome.Grp = character(0), stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)

	type <- NA

	if (pi_mon == 1) {
		if (nchains > 1) {
			# Gelman-Rubin Statistics

			type = "Gelman-Rubin"

			for (i in 1:raw$nClusters) {
				for (b in 1:raw$nOutcome.Grp[i]) {

					# pi
					for (t in 1:(raw$nTreatments - 1)) {
						g = M_global$GelmanRubin(raw$pi[, t, i, b, ], nchains)
						row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
												stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
						pi_conv = rbind(pi_conv, row)
					}
				}
			}
		}
		else {
			# Geweke Diagnostic

			type = "Geweke"

			for (i in 1:raw$nClusters) {
				for (b in 1:raw$nOutcome.Grp[i]) {
					# pi
					for (t in 1:(raw$nTreatments - 1)) {
						g = M_global$Geweke(raw$pi[1, t, i, b, ])
						row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b], stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
						pi_conv = rbind(pi_conv, row)
					}
				}
			}
		}
	}

	theta_acc = data.frame(chain = numeric(0), Trt.Grp = integer(0), Cluster = character(0), Outcome.Grp = character(0),
											Outcome = character(0), rate = numeric(0), stringsAsFactors=FALSE)

	if (theta_mon == 1) {
		for (i in 1:raw$nClusters) {
			for (b in 1:raw$nOutcome.Grp[i]) {
				for (j in 1:raw$nOutcome[i, b]) {
					for (c in 1:nchains) {
						for (t in 1:(raw$nTreatments - 1)) {
							rate <- raw$theta_acc[c, t, i, b, j]/raw$iter
							row <- data.frame(chain = c, Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
								Outcome = raw$Outcome[i, b,j], rate = rate, stringsAsFactors=FALSE)
							theta_acc = rbind(theta_acc, row)
						}
					}
				}
			}
		}
	}

	rownames(theta_acc) <- NULL
	rownames(pi_conv) <- NULL

	c_base$theta_acc = theta_acc

	c_BB = list(pi.conv.diag = pi_conv)

	conv.diag = c(c_base, c_BB)

	attr(conv.diag, "model") = attr(raw, "model")
	return(conv.diag)
}

bhpm.cluster.BB.hier2.lev0.print.convergence.summary <- function(conv) {

	if (is.null(conv)) {
		message("NULL conv data")
		return(NULL)
	}

    # Check which variables we are monitoring
	monitor = conv$monitor
	theta_mon = monitor[monitor$variable == "theta",]$monitor
	gamma_mon = monitor[monitor$variable == "gamma",]$monitor
	mu.theta_mon = monitor[monitor$variable == "mu.theta",]$monitor
	mu.gamma_mon = monitor[monitor$variable == "mu.gamma",]$monitor
	sigma2.theta_mon = monitor[monitor$variable == "sigma2.theta",]$monitor
	sigma2.gamma_mon = monitor[monitor$variable == "sigma2.gamma",]$monitor
	pi_mon = monitor[monitor$variable == "pi",]$monitor

	model = attr(conv, "model")
	if (is.null(model)) {
		message("Convergence model attribute missing")
		return(NULL)
	}

	if (gamma_mon == 1 && !("gamma.conv.diag" %in% names(conv))) {
		message("Missing gamma.conv.diag data")
		return(NULL)
	}
	if (theta_mon == 1 && !("theta.conv.diag" %in% names(conv))) {
		message("Missing theta.conv.diag data")
		return(NULL)
	}
	if (mu.gamma_mon == 1 && !("mu.gamma.conv.diag" %in% names(conv))) {
		message("Missing mu.gamma.conv.diag data")
		return(NULL)
	}
	if (mu.theta_mon == 1 && !("mu.theta.conv.diag" %in% names(conv))) {
		message("Missing mu.theta.conv.diag data")
		return(NULL)
	}
	if (sigma2.gamma_mon == 1 && !("sigma2.gamma.conv.diag" %in% names(conv))) {
		message("Missing sigma2.gamma.conv.diag data")
		return(NULL)
	}

    if (sigma2.theta_mon == 1 && !("sigma2.theta.conv.diag" %in% names(conv))) {
		message("Missing sigma2.theta.conv.diag data")
		return(NULL)
	}
    if (gamma_mon == 1 && !("gamma_acc" %in% names(conv))) {
		message("Missing gamma_acc data")
		return(NULL)
	}
    if (theta_mon == 1 && !("theta_acc" %in% names(conv))) {
		message("Missing theta_acc data")
		return(NULL)
	}

	if (pi_mon == 1 && !("pi.conv.diag" %in% names(conv))) {
		message("Missing pi.conv.diag data")
		return(NULL)
	}

	cat(sprintf("Summary Convergence Diagnostics:\n"))
	cat(sprintf("================================\n"))

	if (conv$type == "Gelman-Rubin") {
		if (theta_mon == 1) {
			cat(sprintf("theta:\n"))
			cat(sprintf("------\n"))
	
			max_t = head(conv$theta.conv.diag[conv$theta.conv.diag$stat == max(conv$theta.conv.diag$stat),,
						drop = FALSE], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic (%d %s %s %s): %0.6f\n", max_t$Trt.Grp, max_t$Cluster, max_t$Outcome.Grp, max_t$Outcome, max_t$stat))
			min_t = head(conv$theta.conv.diag[conv$theta.conv.diag$stat == min(conv$theta.conv.diag$stat),,
						drop = FALSE], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic (%d %s %s %s): %0.6f\n", min_t$Trt.Grp, min_t$Cluster, min_t$Outcome.Grp, min_t$Outcome, min_t$stat))
		}
	
		if (gamma_mon == 1) {
			cat(sprintf("gamma:\n"))
			cat(sprintf("------\n"))
			max_t = head(conv$gamma.conv.diag[conv$gamma.conv.diag$stat == max(conv$gamma.conv.diag$stat),,
						drop = FALSE], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic (%s %s %s): %0.6f\n", max_t$Cluster, max_t$Outcome.Grp, max_t$Outcome, max_t$stat))
			min_t = head(conv$gamma.conv.diag[conv$gamma.conv.diag$stat == min(conv$gamma.conv.diag$stat),,
						drop = FALSE], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic (%s %s %s): %0.6f\n", min_t$Cluster, min_t$Outcome.Grp, min_t$Outcome, min_t$stat))
		}

		if (mu.gamma_mon == 1) {
			cat(sprintf("mu.gamma:\n"))
			cat(sprintf("---------\n"))
			max_t = head(conv$mu.gamma.conv.diag[conv$mu.gamma.conv.diag$stat
							== max(conv$mu.gamma.conv.diag$stat),, drop = FALSE], 1)
		cat(sprintf("Max Gelman-Rubin diagnostic (%s %s): %0.6f\n", max_t$Cluster, max_t$Outcome.Grp, max_t$stat))
		min_t = head(conv$mu.gamma.conv.diag[conv$mu.gamma.conv.diag$stat
							== min(conv$mu.gamma.conv.diag$stat),, drop = FALSE], 1)
		cat(sprintf("Min Gelman-Rubin diagnostic (%s %s): %0.6f\n", min_t$Cluster, min_t$Outcome.Grp, min_t$stat))
		}

		if (mu.theta_mon == 1) {
			cat(sprintf("mu.theta:\n"))
			cat(sprintf("---------\n"))
			max_t = head(conv$mu.theta.conv.diag[conv$mu.theta.conv.diag$stat
						== max(conv$mu.theta.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic (%d %s %s): %0.6f\n", max_t$Trt.Grp, max_t$Cluster, max_t$Outcome.Grp, max_t$stat))
			min_t = head(conv$mu.theta.conv.diag[conv$mu.theta.conv.diag$stat
						== min(conv$mu.theta.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic (%d %s %s): %0.6f\n", min_t$Trt.Grp, min_t$Cluster, min_t$Outcome.Grp, min_t$stat))
		}

		if (sigma2.gamma_mon == 1) {
			cat(sprintf("sigma2.gamma:\n"))
			cat(sprintf("-------------\n"))
			max_t = head(conv$sigma2.gamma.conv.diag[conv$sigma2.gamma.conv.diag$stat
					== max(conv$sigma2.gamma.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic (%s %s): %0.6f\n", max_t$Cluster, max_t$Outcome.Grp, max_t$stat))
			min_t = head(conv$sigma2.gamma.conv.diag[conv$sigma2.gamma.conv.diag$stat
					== min(conv$sigma2.gamma.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic (%s %s): %0.6f\n", min_t$Cluster, min_t$Outcome.Grp, min_t$stat))
		}

		if (sigma2.theta_mon == 1) {
			cat(sprintf("sigma2.theta:\n"))
			cat(sprintf("-------------\n"))
			max_t = head(conv$sigma2.theta.conv.diag[conv$sigma2.theta.conv.diag$stat
					== max(conv$sigma2.theta.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic (%d %s %s): %0.6f\n", max_t$Trt.Grp, max_t$Cluster, max_t$Outcome.Grp, max_t$stat))
			min_t = head(conv$sigma2.theta.conv.diag[conv$sigma2.theta.conv.diag$stat
					== min(conv$sigma2.theta.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic (%d %s %s): %0.6f\n", min_t$Trt.Grp, min_t$Cluster, min_t$Outcome.Grp, min_t$stat))
		}


		if (pi_mon == 1) {
			cat(sprintf("pi:\n"))
			cat(sprintf("---\n"))
			max_t = head(conv$pi.conv.diag[conv$pi.conv.diag$stat
					== max(conv$pi.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic (%d %s %s): %0.6f\n", max_t$Trt.Grp, max_t$Cluster, max_t$Outcome.Grp, max_t$stat))
			min_t = head(conv$pi.conv.diag[conv$pi.conv.diag$stat
					== min(conv$pi.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic (%d %s %s): %0.6f\n", min_t$Trt.Grp, min_t$Cluster, min_t$Outcome.Grp, min_t$stat))
		}

	}
	else {
		if (theta_mon == 1) {
			cat(sprintf("theta:\n"))
			cat(sprintf("------\n"))

			max_t = head(conv$theta.conv.diag[conv$theta.conv.diag$stat == max(conv$theta.conv.diag$stat),,
						drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic (%d %s %s %s): %0.6f (%s)\n", max_t$Trt.Grp, max_t$Cluster, max_t$Outcome.Grp, max_t$Outcome, max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$theta.conv.diag[conv$theta.conv.diag$stat == min(conv$theta.conv.diag$stat),,
						drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic (%d %s %s %s): %0.6f (%s)\n", min_t$Trt.Grp, min_t$Cluster, min_t$Outcome.Grp, min_t$Outcome, min_t$stat,
												chk_val(min_t$stat)))
		}
	
		if (gamma_mon == 1) {
			cat(sprintf("gamma:\n"))
			cat(sprintf("------\n"))
			max_t = head(conv$gamma.conv.diag[conv$gamma.conv.diag$stat == max(conv$gamma.conv.diag$stat),,
						drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic (%s %s %s): %0.6f (%s)\n", max_t$Cluster, max_t$Outcome.Grp, max_t$Outcome, max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$gamma.conv.diag[conv$gamma.conv.diag$stat == min(conv$gamma.conv.diag$stat),,
						drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic (%s %s %s): %0.6f (%s)\n", min_t$Cluster, min_t$Outcome.Grp, min_t$Outcome, min_t$stat,
												chk_val(min_t$stat)))
		}

		if (mu.gamma_mon == 1) {
			cat(sprintf("mu.gamma:\n"))
			cat(sprintf("---------\n"))
			max_t = head(conv$mu.gamma.conv.diag[conv$mu.gamma.conv.diag$stat
							== max(conv$mu.gamma.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic (%s %s): %0.6f (%s)\n", max_t$Cluster, max_t$Outcome.Grp, max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$mu.gamma.conv.diag[conv$mu.gamma.conv.diag$stat
							== min(conv$mu.gamma.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic (%s %s): %0.6f (%s)\n", min_t$Cluster, min_t$Outcome.Grp, min_t$stat,
												chk_val(min_t$stat)))
		}

		if (mu.theta_mon == 1) {
			cat(sprintf("mu.theta:\n"))
			cat(sprintf("---------\n"))
			max_t = head(conv$mu.theta.conv.diag[conv$mu.theta.conv.diag$stat
						== max(conv$mu.theta.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic (%d %s %s): %0.6f (%s)\n", max_t$Trt.Grp, max_t$Cluster, max_t$Outcome.Grp, max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$mu.theta.conv.diag[conv$mu.theta.conv.diag$stat
						== min(conv$mu.theta.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic (%d %s %s): %0.6f (%s)\n", min_t$Trt.Grp, min_t$Cluster, min_t$Outcome.Grp, min_t$stat,
												chk_val(min_t$stat)))
		}

		if (sigma2.gamma_mon == 1) {
			cat(sprintf("sigma2.gamma:\n"))
			cat(sprintf("-------------\n"))
			max_t = head(conv$sigma2.gamma.conv.diag[conv$sigma2.gamma.conv.diag$stat
					== max(conv$sigma2.gamma.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic (%s %s): %0.6f (%s)\n", max_t$Cluster, max_t$Outcome.Grp, max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$sigma2.gamma.conv.diag[conv$sigma2.gamma.conv.diag$stat
					== min(conv$sigma2.gamma.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic (%s %s): %0.6f (%s)\n", min_t$Cluster, min_t$Outcome.Grp, min_t$stat,
												chk_val(min_t$stat)))
		}

		if (sigma2.theta_mon == 1) {
			cat(sprintf("sigma2.theta:\n"))
			cat(sprintf("-------------\n"))
			max_t = head(conv$sigma2.theta.conv.diag[conv$sigma2.theta.conv.diag$stat
					== max(conv$sigma2.theta.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic (%d %s %s): %0.6f (%s)\n", max_t$Trt.Grp, max_t$Cluster, max_t$Outcome.Grp, max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$sigma2.theta.conv.diag[conv$sigma2.theta.conv.diag$stat
					== min(conv$sigma2.theta.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic (%d %s %s): %0.6f (%s)\n", min_t$Trt.Grp, min_t$Cluster, min_t$Outcome.Grp, min_t$stat,
												chk_val(min_t$stat)))
		}

		if (pi_mon == 1) {
			cat(sprintf("pi:\n"))
			cat(sprintf("---\n"))
			max_t = head(conv$pi.conv.diag[conv$pi.conv.diag$stat
					== max(conv$pi.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic (%d %s %s): %0.6f (%s)\n", max_t$Trt.Grp, max_t$Cluster, max_t$Outcome.Grp, max_t$stat,
											chk_val(max_t$stat)))
			min_t = head(conv$pi.conv.diag[conv$pi.conv.diag$stat
					== min(conv$pi.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic (%d %s %s): %0.6f (%s)\n", min_t$Trt.Grp, min_t$Cluster, min_t$Outcome.Grp, min_t$stat,
												chk_val(min_t$stat)))

		}
	}

	if (conv$sim_type == "MH") {
		cat("\nSampling Acceptance Rates:\n")
		cat("==========================\n")
		if (theta_mon == 1) {
			cat("theta:\n")
			cat("------\n")
			print(sprintf("Min: %0.6f, Max: %0.6f", min(conv$theta_acc$rate),
												max(conv$theta_acc$rate)))
		}

		if (gamma_mon == 1) {
			cat("gamma:\n")
			cat("------\n")
			print(sprintf("Min: %0.6f, Max: %0.6f", min(conv$gamma_acc$rate),
												max(conv$gamma_acc$rate)))
		}
	}
	else {
		cat("\nSampling Acceptance Rates:\n")
		cat("==========================\n")
		if (theta_mon == 1) {
			cat("theta:\n")
			cat("------\n")
			print(sprintf("Min: %0.6f, Max: %0.6f", min(conv$theta_acc$rate),
											max(conv$theta_acc$rate)))
		}
	}
}
