# bhpm.cluster.1a.dep.lev2.convergence.diag
# Model bhpm.BB
# R. Carragher
# Date: 29/06/2018
#
# If the MCMC simulation has been run for more than one chain report the Gelman-Rubin statistic.
# If the MCMC simulation has been run for only one chain report the Geweke diagnostic (Z-score)
#

Id <- "$Id: bhpm.cluster.1a.hier3.lev2.convergence.R,v 1.10 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.cluster.1a.dep.lev2.convergence.diag <- function(raw, debug_diagnostic = FALSE)
{
	if (is.null(raw)) {
		message("NULL raw data")
		return(NULL)
	}

	if (M_global$CLUSTER_check_conv_name_1a_3(raw)) {
		message("Missing names");
		return(NULL)
	}

	model = attr(raw, "model")
	if (is.null(model)) {
		message("Simulation model attribute missing")
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
	theta.trt.grps <- raw$Trt.Grps[ raw$Trt.Grps$param == "theta", ]$Trt.Grp

	nchains = raw$chains

	gamma_conv = data.frame(Cluster = character(0), Outcome.Grp = character(0),
					Outcome = character(0), stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)
	theta_conv = data.frame(Trt.Grp = integer(0), Cluster = character(0), Outcome.Grp = character(0), Outcome = character(0),
					stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)
	mu.gamma_conv = data.frame(Cluster = character(0), Outcome.Grp = character(0),
						stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)
	mu.theta_conv = data.frame(Trt.Grp = integer(0), Cluster = character(0), Outcome.Grp = character(0),
						stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)
	sigma2.gamma_conv = data.frame(Cluster = character(0), Outcome.Grp = character(0),
						stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)
	sigma2.theta_conv = data.frame(Trt.Grp = integer(0), Cluster = character(0), Outcome.Grp = character(0),
						stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)
	mu.gamma.0_conv = data.frame(stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)
	mu.theta.0_conv = data.frame(Trt.Grp = integer(0), stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)
	tau2.theta.0_conv = data.frame(Trt.Grp = integer(0), stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)
	tau2.gamma.0_conv = data.frame(stat = numeric(0), upper_ci = numeric(0), stringsAsFactors=FALSE)

	type <- NA

	if (nchains > 1) {
		# Gelman-Rubin Statistics
		type = "Gelman-Rubin"

		for (i in 1:raw$nClusters) {
			for (b in 1:raw$nOutcome.Grp[i]) {
				for (j in 1:raw$nOutcome[i, b]) {
					# theta
					if (theta_mon == 1) {
						for (t in 1:(raw$nTreatments - 1)) {
							g = M_global$GelmanRubin(raw$theta[, t, i, b, j, ], nchains)
							row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
								Outcome = raw$Outcome[i, b,j], stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
							theta_conv = rbind(theta_conv, row)
						}
					}

					# gamma
					if (gamma_mon == 1) {
						g = M_global$GelmanRubin(raw$gamma[, i, b, j, ], nchains)
						row <- data.frame(Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
								Outcome = raw$Outcome[i, b,j], stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
						gamma_conv = rbind(gamma_conv, row)
					}
				}

				# mu.gamma
				if (mu.gamma_mon == 1) {
					g = M_global$GelmanRubin(raw$mu.gamma[, i, b, ], nchains)
					row <- data.frame(Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
											stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
					mu.gamma_conv = rbind(mu.gamma_conv, row)
				}
	
				# mu.theta
				if (mu.theta_mon == 1) {
					for (t in 1:(raw$nTreatments - 1)) {
						g = M_global$GelmanRubin(raw$mu.theta[, t, i, b, ], nchains)
						row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
											stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
						mu.theta_conv = rbind(mu.theta_conv, row)
					}
				}

				# sigma2.theta
				if (sigma2.theta_mon == 1) {
					for (t in 1:(raw$nTreatments - 1)) {
						g = M_global$GelmanRubin(raw$sigma2.theta[, t, i, b, ], nchains)
						row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b], 
											stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
						sigma2.theta_conv = rbind(sigma2.theta_conv, row)
					}
				}

				# sigma2.gamma
				if (sigma2.gamma_mon == 1) {
					g = M_global$GelmanRubin(raw$sigma2.gamma[, i, b, ], nchains)
					row <- data.frame(Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
											stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
					sigma2.gamma_conv = rbind(sigma2.gamma_conv, row)
				}
			}
		}

		# mu.gamma.0
		if (mu.gamma.0_mon == 1) {
			g = M_global$GelmanRubin(raw$mu.gamma.0, nchains)
			row <- data.frame(stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
			mu.gamma.0_conv = rbind(mu.gamma.0_conv, row)
		}

		# mu.theta.0
		if (mu.theta.0_mon == 1) {
			for (t in 1:(raw$nTreatments - 1)) {
				g = M_global$GelmanRubin(raw$mu.theta.0[,t,], nchains)
				row <- data.frame(Trt.Grp = theta.trt.grps[t], stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
				mu.theta.0_conv = rbind(mu.theta.0_conv, row)
			}
		}

		# tau2.gamma.0
		if (tau2.gamma.0_mon == 1) {
			g = M_global$GelmanRubin(raw$tau2.gamma.0, nchains)
			row <- data.frame(stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
			tau2.gamma.0_conv = rbind(tau2.gamma.0_conv, row)
		}

		# tau2.theta.0
		if (tau2.theta.0_mon == 1) {
			for (t in 1:(raw$nTreatments - 1)) {
				g = M_global$GelmanRubin(raw$tau2.theta.0[,t,], nchains)
				row <- data.frame(Trt.Grp = theta.trt.grps[t], stat = g$psrf[1], upper_ci =  g$psrf[2], stringsAsFactors=FALSE)
				tau2.theta.0_conv = rbind(tau2.theta.0_conv, row)
			}
		}
	}
	else {
		# Geweke Diagnostic
		type = "Geweke"

		for (i in 1:raw$nClusters) {
			for (b in 1:raw$nOutcome.Grp[i]) {
				for (j in 1:raw$nOutcome[i, b]) {
					# theta
					if (theta_mon == 1) {
						for (t in 1:(raw$nTreatments - 1)) {
							g = M_global$Geweke(raw$theta[, t, i, b, j, ])
							row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
										Outcome = raw$Outcome[i, b, j], stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
							theta_conv = rbind(theta_conv, row)
						}
					}

					# gamma
					if (gamma_mon == 1) {
						g = M_global$Geweke(raw$gamma[, i, b, j, ])
						row <- data.frame(Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
										Outcome = raw$Outcome[i, b, j], stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
						gamma_conv = rbind(gamma_conv, row)
					}
				}

				# mu.gamma
				if (mu.gamma_mon == 1) {
					g = M_global$Geweke(raw$mu.gamma[1, i, b, ])
					row <- data.frame(Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b], stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
					mu.gamma_conv = rbind(mu.gamma_conv, row)
				}

				# mu.theta
				if (mu.theta_mon == 1) {
					for (t in 1:(raw$nTreatments - 1)) {
						g = M_global$Geweke(raw$mu.theta[1, t, i, b, ])
						row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b], stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
						mu.theta_conv = rbind(mu.theta_conv, row)
					}
				}

				# sigma2.theta
				if (sigma2.theta_mon == 1) {
					for (t in 1:(raw$nTreatments - 1)) {
						g = M_global$Geweke(raw$sigma2.theta[1, t, i, b, ])
						row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b], stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
						sigma2.theta_conv = rbind(sigma2.theta_conv, row)
					}
				}
			
				# sigma2.gamma
				if (sigma2.gamma_mon == 1) {
					g = M_global$Geweke(raw$sigma2.gamma[1, i, b, ])
					row <- data.frame(Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b], stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
					sigma2.gamma_conv = rbind(sigma2.gamma_conv, row)
				}
			}
		}

		if (mu.gamma.0_mon == 1) {
			g = M_global$Geweke(raw$mu.gamma.0[1,])
			row <- data.frame(stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
			mu.gamma.0_conv = rbind(mu.gamma.0_conv, row)
		}

		if (mu.theta.0_mon == 1) {
			for (t in 1:(raw$nTreatments - 1)) {
				g = M_global$Geweke(raw$mu.theta.0[1,t,])
				row <- data.frame(Trt.Grp = theta.trt.grps[t], stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
				mu.theta.0_conv = rbind(mu.theta.0_conv, row)
			}
		}

		if (tau2.gamma.0_mon == 1) {
			g = M_global$Geweke(raw$tau2.gamma.0[1,])
			row <- data.frame(stat = g$z, upper_ci = NA, stringsAsFactors=FALSE)
			tau2.gamma.0_conv = rbind(tau2.gamma.0_conv, row)
		}

		if (tau2.theta.0_mon == 1) {
			for (t in 1:(raw$nTreatments - 1)) {
				g = M_global$Geweke(raw$tau2.theta.0[1,t,])
				row <- data.frame(Trt.Grp = theta.trt.grps[t], stat = g$z, upper_ci = NA)
				tau2.theta.0_conv = rbind(tau2.theta.0_conv, row, stringsAsFactors=FALSE)
			}
		}
	}

	theta_acc = data.frame(chain = numeric(0), Trt.Grp = integer(0), Cluster = character(0), Outcome.Grp = character(0),
											Outcome = character(0), rate = numeric(0), stringsAsFactors=FALSE)
	gamma_acc = data.frame(chain = numeric(0), Cluster = character(0), Outcome.Grp = character(0),
											Outcome = character(0), rate = numeric(0), stringsAsFactors=FALSE)

	if (raw$sim_type == "MH") {
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

		if (gamma_mon == 1) {
			for (i in 1:raw$nClusters) {
				for (b in 1:raw$nOutcome.Grp[i]) {
					for (j in 1:raw$nOutcome[i, b]) {
						for (c in 1:nchains) {
							rate <- raw$gamma_acc[c, i, b, j]/raw$iter
							row <- data.frame(chain = c, Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b],
									Outcome = raw$Outcome[i, b,j], rate = rate, stringsAsFactors=FALSE)
							gamma_acc = rbind(gamma_acc, row)
						}
					}
				}
			}
		}
	}
	
	rownames(gamma_conv) <- NULL
	rownames(theta_conv) <- NULL
	rownames(mu.gamma_conv) <- NULL
	rownames(mu.theta_conv) <- NULL
	rownames(sigma2.gamma_conv) <- NULL
	rownames(sigma2.theta_conv) <- NULL

	rownames(mu.gamma.0_conv) <- NULL
	rownames(mu.theta.0_conv) <- NULL
	rownames(tau2.theta.0_conv) <- NULL
	rownames(tau2.gamma.0_conv) <- NULL

	rownames(gamma_acc) <- NULL
	rownames(theta_acc) <- NULL

	conv.diag = list(sim_type = raw$sim_type, type = type, monitor = monitor,
							gamma.conv.diag = gamma_conv,
							theta.conv.diag = theta_conv,
							mu.gamma.0.conv.diag = mu.gamma.0_conv,
							mu.theta.0.conv.diag = mu.theta.0_conv,
							tau2.gamma.0.conv.diag = tau2.gamma.0_conv,
							tau2.theta.0.conv.diag = tau2.theta.0_conv,
							mu.gamma.conv.diag = mu.gamma_conv,
							mu.theta.conv.diag = mu.theta_conv,
							sigma2.gamma.conv.diag = sigma2.gamma_conv,
							sigma2.theta.conv.diag = sigma2.theta_conv,
							gamma_acc = gamma_acc,
							theta_acc = theta_acc)

	attr(conv.diag, "model") = attr(raw, "model")
	return(conv.diag)
}

bhpm.cluster.1a.dep.lev2.print.convergence.summary <- function(conv) {

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
	mu.theta.0_mon = monitor[monitor$variable == "mu.theta.0",]$monitor
	mu.gamma.0_mon = monitor[monitor$variable == "mu.gamma.0",]$monitor
	tau2.theta.0_mon = monitor[monitor$variable == "tau2.theta.0",]$monitor
	tau2.gamma.0_mon = monitor[monitor$variable == "tau2.gamma.0",]$monitor

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

    if (sigma2.gamma_mon == 1 && !("sigma2.theta.conv.diag" %in% names(conv))) {
		message("Missing sigma2.theta.conv.diag data")
		return(NULL)
	}
    if (mu.gamma.0_mon == 1 && !("mu.gamma.0.conv.diag" %in% names(conv))) {
		message("Missing mu.gamma.0.conv.diag data")
		return(NULL)
	}
    if (mu.theta.0_mon == 1 && !("mu.theta.0.conv.diag" %in% names(conv))) {
		message("Missing mu.theta.0.conv.diag data")
		return(NULL)
	}
    if (tau2.gamma.0_mon == 1 && !("tau2.gamma.0.conv.diag" %in% names(conv))) {
		message("Missing tau2.gamma.0.conv.diag data")
		return(NULL)
	}
    if (tau2.theta.0_mon == 1 && !("tau2.theta.0.conv.diag" %in% names(conv))) {
		message("Missing tau2.theta.0.conv.diag data")
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
			cat(sprintf("Min Gelman-Rubin diagnostic (%d %s, %s %s): %0.6f\n", min_t$Trt.Grp, min_t$Cluster, min_t$Outcome.Grp, min_t$Outcome, min_t$stat))
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

		if (mu.gamma.0_mon == 1) {
			cat(sprintf("mu.gamma.0:\n"))
			cat(sprintf("-----------\n"))
			max_t = head(conv$mu.gamma.0.conv.diag[conv$mu.gamma.0.conv.diag$stat
									== max(conv$mu.gamma.0.conv.diag$stat), ], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic: %0.6f\n", max_t$stat))
			min_t = head(conv$mu.gamma.0.conv.diag[conv$mu.gamma.0.conv.diag$stat
									== min(conv$mu.gamma.0.conv.diag$stat), ], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic: %0.6f\n", min_t$stat))
		}

		if (mu.theta.0_mon == 1) {
			cat(sprintf("mu.theta.0:\n"))
			cat(sprintf("-----------\n"))
			max_t = head(conv$mu.theta.0.conv.diag[conv$mu.theta.0.conv.diag$stat
									== max(conv$mu.theta.0.conv.diag$stat), ], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic: %d %0.6f\n", max_t$Trt.Grp, max_t$stat))
			min_t = head(conv$mu.theta.0.conv.diag[conv$mu.theta.0.conv.diag$stat
									== min(conv$mu.theta.0.conv.diag$stat), ], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic: %d %0.6f\n", min_t$Trt.Grp, min_t$stat))
		}


		if (tau2.gamma.0_mon == 1) {
			cat(sprintf("tau2.gamma.0:\n"))
			cat(sprintf("-------------\n"))
			max_t = head(conv$tau2.gamma.0.conv.diag[conv$tau2.gamma.0.conv.diag$stat
									== max(conv$tau2.gamma.0.conv.diag$stat), ], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic: %0.6f\n", max_t$stat))
			min_t = head(conv$tau2.gamma.0.conv.diag[conv$tau2.gamma.0.conv.diag$stat
									== min(conv$tau2.gamma.0.conv.diag$stat), ], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic: %0.6f\n", min_t$stat))
		}


		if (tau2.theta.0_mon == 1) {
			cat(sprintf("tau2.theta.0:\n"))
			cat(sprintf("-------------\n"))
			max_t = head(conv$tau2.theta.0.conv.diag[conv$tau2.theta.0.conv.diag$stat
									== max(conv$tau2.theta.0.conv.diag$stat), ], 1)
			cat(sprintf("Max Gelman-Rubin diagnostic: %d %0.6f\n", max_t$Trt.Grp, max_t$stat))
			min_t = head(conv$tau2.theta.0.conv.diag[conv$tau2.theta.0.conv.diag$stat
									== min(conv$tau2.theta.0.conv.diag$stat), ], 1)
			cat(sprintf("Min Gelman-Rubin diagnostic: %d %0.6f\n", min_t$Trt.Grp, min_t$stat))
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

		if (mu.gamma.0_mon == 1) {
			cat(sprintf("mu.gamma.0:\n"))
			cat(sprintf("-----------\n"))
			max_t = head(conv$mu.gamma.0.conv.diag[conv$mu.gamma.0.conv.diag$stat
					== max(conv$mu.gamma.0.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic: %0.6f (%s)\n", max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$mu.gamma.0.conv.diag[conv$mu.gamma.0.conv.diag$stat
					== min(conv$mu.gamma.0.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic: %0.6f (%s)\n", min_t$stat,
												chk_val(min_t$stat)))
		}

		if (mu.theta.0_mon == 1) {
			cat(sprintf("mu.theta.0:\n"))
			cat(sprintf("-----------\n"))
			max_t = head(conv$mu.theta.0.conv.diag[conv$mu.theta.0.conv.diag$stat
					== max(conv$mu.theta.0.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic: %d %0.6f (%s)\n", max_t$Trt.Grp, max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$mu.theta.0.conv.diag[conv$mu.theta.0.conv.diag$stat
					== min(conv$mu.theta.0.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic: %d %0.6f (%s)\n", min_t$Trt.Grp, min_t$stat,
												chk_val(min_t$stat)))
		}

		if (tau2.gamma.0_mon == 1) {
			cat(sprintf("tau2.gamma.0:\n"))
			cat(sprintf("-------------\n"))
			max_t = head(conv$tau2.gamma.0.conv.diag[conv$tau2.gamma.0.conv.diag$stat
					== max(conv$tau2.gamma.0.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic: %0.6f (%s)\n", max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$tau2.gamma.0.conv.diag[conv$tau2.gamma.0.conv.diag$stat
					== min(conv$tau2.gamma.0.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic: %0.6f (%s)\n", min_t$stat,
												chk_val(min_t$stat)))
		}

		if (tau2.theta.0_mon == 1) {
			cat(sprintf("tau2.theta.0:\n"))
			cat(sprintf("-------------\n"))
			max_t = head(conv$tau2.theta.0.conv.diag[conv$tau2.theta.0.conv.diag$stat
					== max(conv$tau2.theta.0.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Max Geweke statistic: %d %0.6f (%s)\n", max_t$Trt.Grp, max_t$stat,
												chk_val(max_t$stat)))
			min_t = head(conv$tau2.theta.0.conv.diag[conv$tau2.theta.0.conv.diag$stat
					== min(conv$tau2.theta.0.conv.diag$stat),, drop = FALSE], 1)
			cat(sprintf("Min Geweke statistic: %d %0.6f (%s)\n", min_t$Trt.Grp, min_t$stat,
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
}
