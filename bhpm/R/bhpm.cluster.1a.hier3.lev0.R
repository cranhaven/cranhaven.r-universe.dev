# bhpm.cluster
# bhpm: Cluster Analysis wrapper
# R. Carragher
# Date: 29/06/2018


Mi <- new.env()

Mi$Id <- "$Id: bhpm.cluster.1a.hier3.lev0.R,v 1.13 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.cluster.1a.indep <- function(cluster.data, sim_type = "SLICE", burnin = 10000, iter = 40000, nchains = 3,
	global.sim.params = data.frame(type = c("MH", "SLICE"), param = c("sigma_MH", "w"), value = c(0.2,1),
	control = c(0,6)),
	sim.params = NULL,
	monitor = data.frame(variable = c("theta", "gamma", "mu.gamma", "mu.theta",
					"sigma2.theta", "sigma2.gamma",
		            "mu.theta.0", "mu.gamma.0", "tau2.theta.0", "tau2.gamma.0"),
					monitor = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
					stringsAsFactors = FALSE),
	initial_values = NULL,
	hyper_params = list(mu.gamma.0.0 = 0, tau2.gamma.0.0 = 10,
	mu.theta.0.0 = 0, tau2.theta.0.0 = 10, alpha.gamma.0.0 = 3, beta.gamma.0.0 = 1, alpha.theta.0.0 = 3,
	beta.theta.0.0 = 1, alpha.gamma = 3, beta.gamma = 1, alpha.theta = 3, beta.theta = 1), memory_model = "HIGH")
{

	cluster = M_global$CLUSTERdata(Mi, cluster.data, iter, nchains, burnin, initial_values)

	if (is.null(cluster)) {
		return(NULL)
	}

	cluster.data = cluster$cluster.data
	cntrl.data = cluster$cntrl.data

	Mi$sim_type <- sim_type

	if (nrow(global.sim.params[global.sim.params$type == sim_type,]) != 1) {
		message("Missing simulation parametetrs");
		return(NULL)
	}

	Mi$global.sim.param <- global.sim.params[global.sim.params$type == sim_type,]$value
	Mi$global.sim.param_ctrl <- global.sim.params[global.sim.params$type == sim_type,]$control

	if (Mi$global.sim.param <= 0) {
		message("Invalid simulation parametetr value");
		return(NULL)
	}

	Mi$level = 0

	sim.params = M_global$CLUSTER_sim_params1a(Mi, sim.params, sim_type, cluster.data, cntrl.data)

	monitor = M_global$CLUSTER_monitor_1a_3(monitor)

	# Initialise the hyper-parameters
	Mi$mu.gamma.0.0 <- hyper_params$mu.gamma.0.0
	Mi$tau2.gamma.0.0 <- hyper_params$tau2.gamma.0.0
	Mi$alpha.gamma <- hyper_params$alpha.gamma
	Mi$beta.gamma <- hyper_params$beta.gamma
	Mi$alpha.gamma.0.0 <- hyper_params$alpha.gamma.0.0
	Mi$beta.gamma.0.0 <- hyper_params$beta.gamma.0.0

	Mi$mu.theta.0.0 <- hyper_params$mu.theta.0.0
	Mi$tau2.theta.0.0 <- hyper_params$tau2.theta.0.0
	Mi$alpha.theta <- hyper_params$alpha.theta
	Mi$beta.theta <- hyper_params$beta.theta
	Mi$alpha.theta.0.0 <- hyper_params$alpha.theta.0.0
	Mi$beta.theta.0.0 <- hyper_params$beta.theta.0.0

	Ret2 = .Call("bhpm1a_poisson_mc_exec", as.integer(nchains), as.integer(burnin),
					as.integer(iter), Mi$sim_type,
					memory_model,
					as.numeric(Mi$global.sim.param),
					as.numeric(Mi$global.sim.param_ctrl),
					sim.params,
					monitor,
					as.integer(Mi$nTreatments),
					as.integer(Mi$numClusters), as.integer(Mi$level),
					Mi$maxOutcome.Grps, as.integer(Mi$numOutcome.Grp), as.integer(Mi$maxOutcomes),
					as.integer(t(Mi$nOutcome)), as.integer(aperm(Mi$x)),
					as.integer(aperm(Mi$y)),
					as.numeric(aperm(Mi$C)),
					as.numeric(aperm(Mi$T)),
					as.numeric(aperm(Mi$theta)),
					as.numeric(aperm(Mi$gamma)),
					as.numeric(Mi$mu.gamma.0.0),
					as.numeric(Mi$tau2.gamma.0.0),
					as.numeric(Mi$mu.theta.0.0),
					as.numeric(Mi$tau2.theta.0.0),
					as.numeric(Mi$alpha.gamma.0.0),
					as.numeric(Mi$beta.gamma.0.0),
					as.numeric(Mi$alpha.theta.0.0),
					as.numeric(Mi$beta.theta.0.0),
					as.numeric(Mi$alpha.gamma),
					as.numeric(Mi$beta.gamma),
					as.numeric(Mi$alpha.theta),
					as.numeric(Mi$beta.theta),
					as.numeric(aperm(Mi$mu.gamma.0)),
					as.numeric(aperm(Mi$tau2.gamma.0)),
					as.numeric(aperm(Mi$mu.theta.0)),
					as.numeric(aperm(Mi$tau2.theta.0)),
					as.numeric(aperm(Mi$mu.gamma)),
					as.numeric(aperm(Mi$mu.theta)),
					as.numeric(aperm(Mi$sigma2.gamma)),
					as.numeric(aperm(Mi$sigma2.theta)))

	mu.gamma.0_samples = NULL
	if (monitor[monitor$variable == "mu.gamma.0", ]$monitor == 1) {
		mu.gamma.0_samples <- .Call("getMuGamma0SamplesClusterAll")
		mu.gamma.0_samples = aperm(mu.gamma.0_samples)
	}

	mu.theta.0_samples = NULL
	if (monitor[monitor$variable == "mu.theta.0", ]$monitor == 1) {
		mu.theta.0_samples <- .Call("getMuTheta0SamplesClusterAll")
		mu.theta.0_samples = aperm(mu.theta.0_samples)
	}

	tau2.gamma.0_samples = NULL
	if (monitor[monitor$variable == "tau2.gamma.0", ]$monitor == 1) {
		tau2.gamma.0_samples <- .Call("getTau2Gamma0SamplesClusterAll")
		tau2.gamma.0_samples = aperm(tau2.gamma.0_samples)
	}

	tau2.theta.0_samples = NULL
	if (monitor[monitor$variable == "tau2.theta.0", ]$monitor == 1) {
		tau2.theta.0_samples <- .Call("getTau2Theta0SamplesClusterAll")
		tau2.theta.0_samples = aperm(tau2.theta.0_samples)
	}

	mu.theta_samples = NULL
	if (monitor[monitor$variable == "mu.theta", ]$monitor == 1) {
		mu.theta_samples <- .Call("getMuThetaSamplesClusterAll")
		mu.theta_samples <- aperm(mu.theta_samples)
	}

	mu.gamma_samples = NULL
	if (monitor[monitor$variable == "mu.gamma", ]$monitor == 1) {
		mu.gamma_samples <- .Call("getMuGammaSamplesClusterAll")
		mu.gamma_samples <- aperm(mu.gamma_samples)
	}

	sigma2.theta_samples = NULL
	if (monitor[monitor$variable == "sigma2.theta", ]$monitor == 1) {
		sigma2.theta_samples <- .Call("getSigma2ThetaSamplesClusterAll")
		sigma2.theta_samples <- aperm(sigma2.theta_samples)
	}

	sigma2.gamma_samples = NULL
	if (monitor[monitor$variable == "sigma2.gamma", ]$monitor == 1) {
		sigma2.gamma_samples <- .Call("getSigma2GammaSamplesClusterAll")
		sigma2.gamma_samples <- aperm(sigma2.gamma_samples)
	}

	gamma_samples = NULL
	gamma_acc = NULL
	if (monitor[monitor$variable == "gamma", ]$monitor == 1) {
		gamma_samples = .Call("getGammaSamplesClusterAll")
		gamma_samples = aperm(gamma_samples)

		gamma_acc = .Call("getGammaAcceptClusterAll")
		gamma_acc <- aperm(gamma_acc)
	}

	theta_samples = NULL
	theta_acc = NULL
	if (monitor[monitor$variable == "theta", ]$monitor == 1) {
		theta_samples = .Call("getThetaSamplesClusterAll")
		theta_samples = aperm(theta_samples)

		theta_acc = .Call("getThetaAcceptClusterAll")
		theta_acc <- aperm(theta_acc)
	}

	.C("Release_Cluster")

	model_fit = list(id = Mi$Id, sim_type = Mi$sim_type, chains = nchains, nClusters = Mi$numClusters,
			nTreatments = Mi$nTreatments,
			Clusters = Mi$Clusters, Trt.Grps = Mi$Trt.Grps, nOutcome.Grp = Mi$numOutcome.Grp, maxOutcome.Grps = Mi$maxOutcome.Grps,
			maxOutcomes = Mi$maxOutcomes, nOutcome = Mi$nOutcome, Outcome=Mi$Outcome, Outcome.Grp = Mi$Outcome.Grp,
			burnin = burnin, iter = iter,
			monitor = monitor,
			gamma = gamma_samples,
			theta = theta_samples,
			mu.gamma = mu.gamma_samples,
			mu.theta = mu.theta_samples,
			sigma2.gamma = sigma2.gamma_samples,
			sigma2.theta = sigma2.theta_samples,
			mu.gamma.0 = mu.gamma.0_samples,
			mu.theta.0 = mu.theta.0_samples,
			tau2.gamma.0 = tau2.gamma.0_samples,
			tau2.theta.0 = tau2.theta.0_samples,
			gamma_acc = gamma_acc,
			theta_acc = theta_acc)
			
	# Model is poisson with BB1a hierarchy and independent clusters
	attr(model_fit, "model") = "1a_pois_indep"

	return(model_fit)
}

Mi$initVars = function() {

    # Data Structure
    Mi$Outcome.Grp <- c()
    Mi$numOutcome.Grp <- NA
    Mi$numClusters <- NA
    Mi$nOutcome <- c()
    Mi$maxOutcomes <- NA

    # Cluster Event Data
    Mi$x <- array()
    Mi$C <- array()
    Mi$y <- array()
    Mi$T <- array()

    # Hyperparameters
    Mi$mu.gamma.0.0 <- NA
    Mi$tau2.gamma.0.0 <- NA
    Mi$mu.theta.0.0 <- NA
    Mi$tau2.theta.0.0 <- NA
    Mi$alpha.gamma.0.0 <- NA
    Mi$beta.gamma.0.0 <- NA
    Mi$alpha.theta.0.0 <- NA
    Mi$beta.theta.0.0 <- NA
    Mi$alpha.gamma <- NA
    Mi$beta.gamma <- NA
    Mi$alpha.theta <- NA
    Mi$beta.theta <- NA

    # Parameters/Simulated values
    # Stage 3
    Mi$mu.gamma.0 <- c()
    Mi$tau2.gamma.0 <- c()
    Mi$mu.theta.0 <- c()
    Mi$tau2.theta.0 <- c()

    # Stage 2
    Mi$mu.gamma <- array()
    Mi$mu.theta <- array()
    Mi$sigma2.gamma <- array()
    Mi$sigma2.theta <- array()

    # Stage 1
    Mi$theta <- array()
    Mi$gamma <- array()
}

Mi$initChains = function(c) {
	# Choose random values for gamma and theta
	for (i in 1:Mi$numClusters) {
		numOutcome.Grp = Mi$numOutcome.Grp[i]
		for (b in 1:numOutcome.Grp) {
			Mi$gamma[c, i, b, 1:Mi$nOutcome[i, b]] <- runif(Mi$nOutcome[i, b], -10, 10)

			Mi$gamma[c, i, b, ][is.infinite(Mi$gamma[c, i, b, ])] = -10

			Mi$gamma[c, i, b, ][is.nan(Mi$gamma[c, i, b, ])] = -10 # -1000

			for (t in 1:(Mi$nTreatments -1)) {
				Mi$theta[c, t, i, b, 1:Mi$nOutcome[i, b]] <- runif(Mi$nOutcome[i, b], -10, 10)
				Mi$theta[c, t, i, b, ][is.infinite(Mi$theta[c, t, i, b, ])] = -10
				Mi$theta[c, t, i, b, ][is.nan(Mi$theta[c, t, i, b, ])] = -10 # -1000
			}
		}

		Mi$mu.gamma[c, i, 1:numOutcome.Grp] = runif(numOutcome.Grp, -10, 10)
		Mi$mu.theta[c,, i, 1:numOutcome.Grp] = runif(numOutcome.Grp*(Mi$nTreatments -1), -10, 10)
		Mi$sigma2.gamma[c, i, 1:numOutcome.Grp] = runif(numOutcome.Grp, 5, 20)
		Mi$sigma2.theta[c,, i, 1:numOutcome.Grp] = runif(numOutcome.Grp*(Mi$nTreatments -1), 5, 20)


		Mi$mu.gamma.0[c, i] = runif(1, -10, 10)
		Mi$tau2.gamma.0[c, i] = runif(1, 5, 20)
		Mi$mu.theta.0[c,, i] = runif(1*(Mi$nTreatments -1), -10, 10)
		Mi$tau2.theta.0[c,, i] = runif(1*(Mi$nTreatments -1), 5, 20)
	}
}

Mi$initialiseChains = function(initial_values, nchains) {

	Mi$theta = array(0, dim=c(nchains, Mi$nTreatments - 1, Mi$numClusters, Mi$maxOutcome.Grps, Mi$maxOutcomes))
	Mi$gamma = array(0, dim=c(nchains, Mi$numClusters, Mi$maxOutcome.Grps, Mi$maxOutcomes))

	if (is.null(initial_values)) {

		# Initialise the first chain with the data
		for (i in 1:Mi$numClusters) {
			numOutcome.Grp = Mi$numOutcome.Grp[i]
			for (b in 1:numOutcome.Grp) {
				Mi$gamma[1, i, b, ] <- log(Mi$x[i, b,]/Mi$C[i, b, ])

				for (t in 1:(Mi$nTreatments - 1)) {
					Mi$theta[1, t, i, b, ] <- log(Mi$y[t, i, b,]/Mi$T[t, i, b, ]) - Mi$gamma[1, i, b, ]
					Mi$theta[1, t, i, b, ][is.infinite(Mi$theta[1, t, i, b, ])] = -10 # -1000
					Mi$theta[1, t, i, b, ][is.nan(Mi$theta[1, t, i, b, ])] = -10 # -1000
				}

				Mi$gamma[1, i, b, ][is.infinite(Mi$gamma[1, i, b, ])] = -10 # -1000
				Mi$gamma[1, i, b, ][is.nan(Mi$gamma[1, i, b, ])] = -10 # -1000
			}
		}

		Mi$mu.gamma <- array(0, dim = c(nchains, Mi$numClusters, Mi$maxOutcome.Grps))
		Mi$mu.theta <- array(0, dim = c(nchains, Mi$nTreatments - 1, Mi$numClusters, Mi$maxOutcome.Grps))
		Mi$sigma2.gamma <- array(10, dim = c(nchains, Mi$numClusters, Mi$maxOutcome.Grps))
		Mi$sigma2.theta <- array(10, dim = c(nchains, Mi$nTreatments - 1, Mi$numClusters, Mi$maxOutcome.Grps))

		Mi$mu.gamma.0 <- array(0, dim = c(nchains, Mi$numClusters))
		Mi$tau2.gamma.0 <- array(10, dim = c(nchains, Mi$numClusters))
		Mi$mu.theta.0 <- array(0, dim = c(nchains, Mi$nTreatments - 1, Mi$numClusters))
		Mi$tau2.theta.0 <- array(10, dim = c(nchains, Mi$nTreatments - 1, Mi$numClusters))

		if (nchains > 1) {
			for (c in 2:nchains) {
				Mi$initChains(c)
			}

		}
	}
	else {

		Mi$mu.gamma.0 <- array(0, dim = c(nchains, Mi$numClusters))
		Mi$tau2.gamma.0 <- array(10, dim = c(nchains, Mi$numClusters))
		Mi$mu.theta.0 <- array(0, dim = c(nchains, Mi$nTreatments - 1, Mi$numClusters))
		Mi$tau2.theta.0 <- array(10, dim = c(nchains, Mi$nTreatments - 1, Mi$numClusters))

		for (c in 1:nchains) {
			for (i in 1:Mi$numClusters) {
				cluster = Mi$Clusters[i]
				data = initial_values$mu.gamma.0[initial_values$mu.gamma.0$chain == c &
												initial_values$mu.gamma.0$Cluster == cluster, ]
				Mi$mu.gamma.0[c, i] = data$value

				data = initial_values$tau2.gamma.0[initial_values$tau2.gamma.0$chain == c &
												initial_values$tau2.gamma.0$Cluster == cluster, ]
				Mi$tau2.gamma.0[c, i] = data$value

				for (t in 1:(Mi$nTreatments - 1)) {
					data = initial_values$mu.theta.0[[t]][initial_values$mu.theta.0[[t]]$chain == c &
												initial_values$mu.theta.0[[t]]$Cluster == cluster, ]
					Mi$mu.theta.0[c, t, i] = data$value

					data = initial_values$tau2.theta.0[[t]][initial_values$tau2.theta.0[[t]]$chain == c &
												initial_values$tau2.theta.0[[t]]$Cluster == cluster, ]
					Mi$tau2.theta.0[c, t, i] = data$value

				}
			}
		}

		Mi$mu.gamma <- array(0, dim = c(nchains, Mi$numClusters, Mi$maxOutcome.Grps))
		Mi$sigma2.gamma <- array(0, dim = c(nchains, Mi$numClusters, Mi$maxOutcome.Grps))
		Mi$mu.theta <- array(0, dim = c(nchains, Mi$nTreatments - 1, Mi$numClusters, Mi$maxOutcome.Grps))
		Mi$sigma2.theta <- array(0, dim = c(nchains, Mi$nTreatments - 1, Mi$numClusters, Mi$maxOutcome.Grps))

		for (c in 1:nchains) {
			for (i in 1:Mi$numClusters) {
				cluster = Mi$Clusters[i]
				for (b in 1:Mi$numOutcome.Grp[i]) {
					data = initial_values$mu.gamma[initial_values$mu.gamma$chain == c &
									initial_values$mu.gamma$Cluster == cluster
												& initial_values$mu.gamma$Outcome.Grp == Mi$Outcome.Grp[i, b],]
					Mi$mu.gamma[c, i, b] = data$value

					data = initial_values$sigma2.gamma[initial_values$sigma2.gamma$chain == c &
									initial_values$sigma2.gamma$Cluster == cluster
												& initial_values$sigma2.gamma$Outcome.Grp == Mi$Outcome.Grp[i, b],]
					Mi$sigma2.gamma[c, i, b] = data$value

					for (t in 1:(Mi$nTreatments - 1)) {
						data = initial_values$mu.theta[[t]][initial_values$mu.theta[[t]]$chain == c &
									initial_values$mu.theta[[t]]$Cluster == cluster
												& initial_values$mu.theta[[t]]$Outcome.Grp == Mi$Outcome.Grp[i, b],]
						Mi$mu.theta[c, t, i, b] = data$value

						data = initial_values$sigma2.theta[[t]][initial_values$sigma2.theta[[t]]$chain == c &
									initial_values$sigma2.theta[[t]]$Cluster == cluster
												& initial_values$sigma2.theta[[t]]$Outcome.Grp == Mi$Outcome.Grp[i, b],]
						Mi$sigma2.theta[c, t, i, b] = data$value
					}
				}
			}
		}

		for (c in 1:nchains) {
			for (i in 1:Mi$numClusters) {
				cluster = Mi$Clusters[i]
				for (b in 1:Mi$numOutcome.Grp[i]) {
					for (j in 1:Mi$nOutcome[i, b]) {
						ae = Mi$Outcome[i, b, j]
						data = initial_values$gamma[initial_values$gamma$chain == c
										& initial_values$gamma$Cluster == cluster
										& initial_values$gamma$Outcome.Grp == Mi$Outcome.Grp[i, b]
										& initial_values$gamma$Outcome == ae,]
						Mi$gamma[c, i, b, j] = data$value

						for (t in 1:(Mi$nTreatments - 1)) {
							data = initial_values$theta[[t]][initial_values$theta[[t]]$chain == c
										& initial_values$theta[[t]]$Cluster == cluster
										& initial_values$theta[[t]]$Outcome.Grp == Mi$Outcome.Grp[i, b]
										& initial_values$theta[[t]]$Outcome == ae,]
							Mi$theta[c, t, i, b, j] = data$value
						}
					}
				}
			}
		}
	}
}
