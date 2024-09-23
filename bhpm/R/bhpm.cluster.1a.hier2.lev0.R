# bhpm.cluster
# bhpm: Cluster Analysis wrapper
# R. Carragher
# Date: 29/06/2018


Mi_1a_h2_l0 <- new.env()

Mi_1a_h2_l0$Id <- "$Id: bhpm.cluster.1a.hier2.lev0.R,v 1.13 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.cluster.1a.hier2.lev0 <- function(cluster.data, sim_type = "SLICE", burnin = 10000,
		iter = 40000, nchains = 3,
		global.sim.params = data.frame(type = c("MH", "SLICE"),
			param = c("sigma_MH", "w"),
			value = c(0.2,1),
			control = c(0,6)),
	sim.params = NULL,
	monitor = data.frame(variable = c("theta", "gamma", "mu.gamma", "mu.theta",
		"sigma2.theta", "sigma2.gamma"),
		monitor = c(1, 1, 1, 1, 1, 1), stringsAsFactors = FALSE),
	initial_values = NULL,
	hyper_params = list(mu.gamma.0 = 0, tau2.gamma.0 = 10,
	mu.theta.0 = 0, tau2.theta.0 = 10, alpha.gamma = 3, beta.gamma = 1, alpha.theta = 3,
	beta.theta = 1), memory_model = "HIGH")
{
	cluster = M_global$CLUSTERdata(Mi_1a_h2_l0, cluster.data, iter, nchains, burnin, initial_values)

	if (is.null(cluster)) {
		return(NULL)
	}

    cluster.data = cluster$cluster.data
    cntrl.data = cluster$cntrl.data
	
	Mi_1a_h2_l0$sim_type <- sim_type

	if (nrow(global.sim.params[global.sim.params$type == sim_type,]) != 1) {
		message("Missing simulation parametetrs");
		return(NULL)
	}

	Mi_1a_h2_l0$global.sim.param <- global.sim.params[global.sim.params$type == sim_type,]$value
	Mi_1a_h2_l0$global.sim.param_ctrl <- global.sim.params[global.sim.params$type == sim_type,]$control

	if (Mi_1a_h2_l0$global.sim.param <= 0) {
		message("Invalid simulation parametetr value");
		return(NULL)
	}

	Mi_1a_h2_l0$level = 0

	sim.params = M_global$CLUSTER_sim_params1a(Mi_1a_h2_l0, sim.params, sim_type, cluster.data, cntrl.data)

	monitor = M_global$CLUSTER_monitor_1a_2(monitor)

	# Initialise the hyper-parameters
	Mi_1a_h2_l0$mu.gamma.0 <- hyper_params$mu.gamma.0
	Mi_1a_h2_l0$tau2.gamma.0 <- hyper_params$tau2.gamma.0
	Mi_1a_h2_l0$alpha.gamma <- hyper_params$alpha.gamma
	Mi_1a_h2_l0$beta.gamma <- hyper_params$beta.gamma

	Mi_1a_h2_l0$mu.theta.0 <- hyper_params$mu.theta.0
	Mi_1a_h2_l0$tau2.theta.0 <- hyper_params$tau2.theta.0
	Mi_1a_h2_l0$alpha.theta <- hyper_params$alpha.theta
	Mi_1a_h2_l0$beta.theta <- hyper_params$beta.theta

	Ret2 = .Call("bhpm1a_cluster_hier2_exec", as.integer(nchains), as.integer(burnin),
					as.integer(iter), Mi_1a_h2_l0$sim_type,
					memory_model,
					as.numeric(Mi_1a_h2_l0$global.sim.param),
					as.numeric(Mi_1a_h2_l0$global.sim.param_ctrl),
					sim.params,
					monitor,
					as.integer(Mi_1a_h2_l0$nTreatments),
					as.integer(Mi_1a_h2_l0$numClusters), as.integer(Mi_1a_h2_l0$level),
					Mi_1a_h2_l0$maxOutcome.Grps, as.integer(Mi_1a_h2_l0$numOutcome.Grp),
					as.integer(Mi_1a_h2_l0$maxOutcomes),
					as.integer(t(Mi_1a_h2_l0$nOutcome)),
					as.integer(aperm(Mi_1a_h2_l0$x)),
					as.integer(aperm(Mi_1a_h2_l0$y)),
					as.numeric(aperm(Mi_1a_h2_l0$C)),
					as.numeric(aperm(Mi_1a_h2_l0$T)),
					as.numeric(aperm(Mi_1a_h2_l0$theta)),
					as.numeric(aperm(Mi_1a_h2_l0$gamma)),
					as.numeric(Mi_1a_h2_l0$mu.gamma.0),
					as.numeric(Mi_1a_h2_l0$tau2.gamma.0),
					as.numeric(Mi_1a_h2_l0$mu.theta.0),
					as.numeric(Mi_1a_h2_l0$tau2.theta.0),
					as.numeric(Mi_1a_h2_l0$alpha.gamma),
					as.numeric(Mi_1a_h2_l0$beta.gamma),
					as.numeric(Mi_1a_h2_l0$alpha.theta),
					as.numeric(Mi_1a_h2_l0$beta.theta),
					as.numeric(aperm(Mi_1a_h2_l0$mu.gamma)),
					as.numeric(aperm(Mi_1a_h2_l0$mu.theta)),
					as.numeric(aperm(Mi_1a_h2_l0$sigma2.gamma)),
					as.numeric(aperm(Mi_1a_h2_l0$sigma2.theta)))

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

	model_fit = list(id = Mi_1a_h2_l0$Id,
			sim_type = Mi_1a_h2_l0$sim_type,
			chains = nchains,
			nClusters = Mi_1a_h2_l0$numClusters,
			nTreatments = Mi_1a_h2_l0$nTreatments,
			Clusters = Mi_1a_h2_l0$Clusters,
			Trt.Grps = Mi_1a_h2_l0$Trt.Grps,
			nOutcome.Grp = Mi_1a_h2_l0$numOutcome.Grp,
			maxOutcome.Grps = Mi_1a_h2_l0$maxOutcome.Grps,
			maxOutcomes = Mi_1a_h2_l0$maxOutcomes,
			nOutcome = Mi_1a_h2_l0$nOutcome,
			Outcome=Mi_1a_h2_l0$Outcome,
			Outcome.Grp = Mi_1a_h2_l0$Outcome.Grp,
			burnin = burnin,
			iter = iter,
			monitor = monitor,
			gamma = gamma_samples,
			theta = theta_samples,
			mu.gamma = mu.gamma_samples,
			mu.theta = mu.theta_samples,
			sigma2.gamma = sigma2.gamma_samples,
			sigma2.theta = sigma2.theta_samples,
			gamma_acc = gamma_acc,
			theta_acc = theta_acc)
			
	# Model is poisson with BB1a hierarchy and independent clusters
	attr(model_fit, "model") = "1a_pois_h2_l0"

	return(model_fit)
}

Mi_1a_h2_l0$initVars = function() {

    # Data Structure
    Mi_1a_h2_l0$Outcome.Grp <- c()
    Mi_1a_h2_l0$numOutcome.Grp <- NA
    Mi_1a_h2_l0$numClusters <- NA
    Mi_1a_h2_l0$nOutcome <- c()
    Mi_1a_h2_l0$maxOutcomes <- NA

    # Cluster Event Data
    Mi_1a_h2_l0$x <- array()
    Mi_1a_h2_l0$C <- array()
    Mi_1a_h2_l0$y <- array()
    Mi_1a_h2_l0$T <- array()

    # Hyperparameters
    Mi_1a_h2_l0$mu.gamma.0 <- NA
    Mi_1a_h2_l0$tau2.gamma.0 <- NA
    Mi_1a_h2_l0$mu.theta.0 <- NA
    Mi_1a_h2_l0$tau2.theta.0 <- NA
    Mi_1a_h2_l0$alpha.gamma <- NA
    Mi_1a_h2_l0$beta.gamma <- NA
    Mi_1a_h2_l0$alpha.theta <- NA
    Mi_1a_h2_l0$beta.theta <- NA
    Mi_1a_h2_l0$alpha.pi <- NA
    Mi_1a_h2_l0$beta.pi <- NA

   # Parameters/Simulated values

    # Stage 2
    Mi_1a_h2_l0$mu.gamma <- array()
    Mi_1a_h2_l0$mu.theta <- array()
    Mi_1a_h2_l0$sigma2.gamma <- array()
    Mi_1a_h2_l0$sigma2.theta <- array()

    # Stage 1
    Mi_1a_h2_l0$theta <- array()
    Mi_1a_h2_l0$gamma <- array()

	Mi_1a_h2_l0$sim_type <- NA
}

Mi_1a_h2_l0$initChains = function(c) {
	# Choose random values for gamma and theta
	for (i in 1:Mi_1a_h2_l0$numClusters) {
		numOutcome.Grp = Mi_1a_h2_l0$numOutcome.Grp[i]
		for (b in 1:numOutcome.Grp) {
			Mi_1a_h2_l0$gamma[c, i, b, 1:Mi_1a_h2_l0$nOutcome[i, b]] <-
										runif(Mi_1a_h2_l0$nOutcome[i, b], -10, 10)

			Mi_1a_h2_l0$gamma[c, i, b, ][is.infinite(Mi_1a_h2_l0$gamma[c, i, b, ])] =
										-10

			Mi_1a_h2_l0$gamma[c, i, b, ][is.nan(Mi_1a_h2_l0$gamma[c, i, b, ])] =
										-10 # -1000

			for (t in 1:(Mi_1a_h2_l0$nTreatments -1)) {
				Mi_1a_h2_l0$theta[c, t, i, b, 1:Mi_1a_h2_l0$nOutcome[i, b]] <-
										runif(Mi_1a_h2_l0$nOutcome[i, b], -10, 10)

				Mi_1a_h2_l0$theta[c, t, i, b, ][is.infinite(Mi_1a_h2_l0$theta[c, t, i, b, ])] =
										-10

				Mi_1a_h2_l0$theta[c, t, i, b, ][is.nan(Mi_1a_h2_l0$theta[c, t, i, b, ])] =
										-10 # -1000
			}
		}

		Mi_1a_h2_l0$mu.gamma[c, i, 1:numOutcome.Grp] = runif(numOutcome.Grp, -10, 10)
		Mi_1a_h2_l0$mu.theta[c, , i, 1:numOutcome.Grp] = runif((Mi_1a_h2_l0$nTreatments - 1)*numOutcome.Grp, -10, 10)
		Mi_1a_h2_l0$sigma2.gamma[c, i, 1:numOutcome.Grp] = runif(numOutcome.Grp, 5, 20)
		Mi_1a_h2_l0$sigma2.theta[c, , i, 1:numOutcome.Grp] = runif((Mi_1a_h2_l0$nTreatments - 1)*numOutcome.Grp, 5, 20)
	}
}

Mi_1a_h2_l0$initialiseChains = function(initial_values, nchains) {

	Mi_1a_h2_l0$theta = array(0, dim=c(nchains, Mi_1a_h2_l0$nTreatments - 1, Mi_1a_h2_l0$numClusters,
											Mi_1a_h2_l0$maxOutcome.Grps, Mi_1a_h2_l0$maxOutcomes))
	Mi_1a_h2_l0$gamma = array(0, dim=c(nchains, Mi_1a_h2_l0$numClusters,
											Mi_1a_h2_l0$maxOutcome.Grps, Mi_1a_h2_l0$maxOutcomes))

	if (is.null(initial_values)) {

		# Initialise the first chain with the data
		for (i in 1:Mi_1a_h2_l0$numClusters) {
			numOutcome.Grp = Mi_1a_h2_l0$numOutcome.Grp[i]
			for (b in 1:numOutcome.Grp) {
				Mi_1a_h2_l0$gamma[1, i, b, ] <-
									log(Mi_1a_h2_l0$x[i, b,]/Mi_1a_h2_l0$C[i, b, ])

				for (t in 1:(Mi_1a_h2_l0$nTreatments - 1))  {
					Mi_1a_h2_l0$theta[1, t, i, b, ] <-
									log(Mi_1a_h2_l0$y[t, i, b,]/Mi_1a_h2_l0$T[t, i, b, ]) -
									Mi_1a_h2_l0$gamma[1, i, b, ]

					Mi_1a_h2_l0$theta[1, t, i, b, ][is.infinite(Mi_1a_h2_l0$theta[1, t, i, b, ])] = -10 # -1000

					Mi_1a_h2_l0$theta[1, t, i, b, ][is.nan(Mi_1a_h2_l0$theta[1, t, i, b, ])] = -10 # -1000
				}

				Mi_1a_h2_l0$gamma[1, i, b, ][is.infinite(Mi_1a_h2_l0$gamma[1, i, b, ])] = -10 # -1000
				Mi_1a_h2_l0$gamma[1, i, b, ][is.nan(Mi_1a_h2_l0$gamma[1, i, b, ])] = -10 # -1000
			}
		}

		Mi_1a_h2_l0$mu.gamma <- array(0, dim = c(nchains, Mi_1a_h2_l0$numClusters,
													Mi_1a_h2_l0$maxOutcome.Grps))
		Mi_1a_h2_l0$mu.theta <- array(0, dim = c(nchains, Mi_1a_h2_l0$nTreatments - 1, Mi_1a_h2_l0$numClusters,
													Mi_1a_h2_l0$maxOutcome.Grps))
		Mi_1a_h2_l0$sigma2.gamma <- array(10, dim = c(nchains, Mi_1a_h2_l0$numClusters,
													Mi_1a_h2_l0$maxOutcome.Grps))
		Mi_1a_h2_l0$sigma2.theta <- array(10, dim = c(nchains, Mi_1a_h2_l0$nTreatments - 1, Mi_1a_h2_l0$numClusters,
													Mi_1a_h2_l0$maxOutcome.Grps))

		if (nchains > 1) {
			for (c in 2:nchains) {
				Mi_1a_h2_l0$initChains(c)
			}

		}
	}
	else {

		Mi_1a_h2_l0$mu.gamma <- array(0, dim = c(nchains, Mi_1a_h2_l0$numClusters,
													Mi_1a_h2_l0$maxOutcome.Grps))
		Mi_1a_h2_l0$mu.theta <- array(0, dim = c(nchains, Mi_1a_h2_l0$nTreatments - 1, Mi_1a_h2_l0$numClusters,
													Mi_1a_h2_l0$maxOutcome.Grps))
		Mi_1a_h2_l0$sigma2.gamma <- array(0, dim = c(nchains, Mi_1a_h2_l0$numClusters,
													Mi_1a_h2_l0$maxOutcome.Grps))
		Mi_1a_h2_l0$sigma2.theta <- array(0, dim = c(nchains, Mi_1a_h2_l0$nTreatments - 1, Mi_1a_h2_l0$numClusters,
													Mi_1a_h2_l0$maxOutcome.Grps))

		for (c in 1:nchains) {
			for (i in 1:Mi_1a_h2_l0$numClusters) {
				cluster = Mi_1a_h2_l0$Clusters[i]
				for (b in 1:Mi_1a_h2_l0$numOutcome.Grp[i]) {
					data = initial_values$mu.gamma[initial_values$mu.gamma$chain ==
									c & initial_values$mu.gamma$Cluster == cluster
									& initial_values$mu.gamma$Outcome.Grp == Mi_1a_h2_l0$Outcome.Grp[i, b],]
					Mi_1a_h2_l0$mu.gamma[c, i, b] = data$value

					data = initial_values$sigma2.gamma[initial_values$sigma2.gamma$chain == c &
									initial_values$sigma2.gamma$Cluster == cluster
									& initial_values$sigma2.gamma$Outcome.Grp == Mi_1a_h2_l0$Outcome.Grp[i, b],]
					Mi_1a_h2_l0$sigma2.gamma[c, i, b] = data$value

					for (t in 1:(Mi_1a_h2_l0$nTreatments - 1))  {

						data = initial_values$mu.theta[[t]][initial_values$mu.theta[[t]]$chain ==
									c & initial_values$mu.theta[[t]]$Cluster == cluster
									& initial_values$mu.theta[[t]]$Outcome.Grp == Mi_1a_h2_l0$Outcome.Grp[i, b],]
						Mi_1a_h2_l0$mu.theta[c, t, i, b] = data$value

						data = initial_values$sigma2.theta[[t]][initial_values$sigma2.theta[[t]]$chain == c &
									initial_values$sigma2.theta[[t]]$Cluster == cluster
									& initial_values$sigma2.theta[[t]]$Outcome.Grp == Mi_1a_h2_l0$Outcome.Grp[i, b],]
						Mi_1a_h2_l0$sigma2.theta[c, t, i, b] = data$value
					}
				}
			}
		}

		for (c in 1:nchains) {
			for (i in 1:Mi_1a_h2_l0$numClusters) {
				cluster = Mi_1a_h2_l0$Clusters[i]
				for (b in 1:Mi_1a_h2_l0$numOutcome.Grp[i]) {
					for (j in 1:Mi_1a_h2_l0$nOutcome[i, b]) {
						ae = Mi_1a_h2_l0$Outcome[i, b, j]
						data = initial_values$gamma[initial_values$gamma$chain == c
										& initial_values$gamma$Cluster == cluster
										& initial_values$gamma$Outcome.Grp == Mi_1a_h2_l0$Outcome.Grp[i, b]
										& initial_values$gamma$Outcome == ae,]
						Mi_1a_h2_l0$gamma[c, i, b, j] = data$value

						for (t in 1:(Mi_1a_h2_l0$nTreatments - 1))  {
							data = initial_values$theta[[t]][initial_values$theta[[t]]$chain == c
										& initial_values$theta[[t]]$Cluster == cluster
										& initial_values$theta[[t]]$Outcome.Grp == Mi_1a_h2_l0$Outcome.Grp[i, b]
										& initial_values$theta[[t]]$Outcome == ae,]
							Mi_1a_h2_l0$theta[c, t, i, b, j] = data$value
						}
					}
				}
			}
		}
	}
}
