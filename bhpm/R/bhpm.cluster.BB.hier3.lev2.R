# bhpm.cluster
# bhpm: Cluster Analysis wrapper
# R. Carragher
# Date: 29/06/2018


Md1 <- new.env()

Md1$Id <- "$Id: bhpm.cluster.BB.hier3.lev2.R,v 1.13 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.cluster.BB.dep.lev2 <- function(cluster.data, sim_type = "SLICE", burnin = 10000, iter = 60000, nchains = 5,
	theta_algorithm = "MH",
	global.sim.params = data.frame(type = c("MH", "MH", "MH", "MH", "SLICE", "SLICE", "SLICE"),
                            param = c("sigma_MH_alpha", "sigma_MH_beta", "sigma_MH_gamma", "sigma_MH_theta",
                            "w_alpha", "w_beta", "w_gamma"),
                            value = c(3, 3, 0.2, 0.25, 1, 1, 1), control = c(0, 0, 0, 0, 6, 6, 6),
							stringsAsFactors = FALSE),
	sim.params = NULL,
	monitor = data.frame(variable = c("theta", "gamma", "mu.gamma", "mu.theta",
					"sigma2.theta", "sigma2.gamma",
		            "mu.theta.0", "mu.gamma.0", "tau2.theta.0", "tau2.gamma.0",
					"pi", "alpha.pi", "beta.pi"),
					monitor = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
					stringsAsFactors = FALSE),
	initial_values = NULL,
	hyper_params = list(mu.gamma.0.0 = 0, tau2.gamma.0.0 = 10,
	mu.theta.0.0 = 0, tau2.theta.0.0 = 10, alpha.gamma.0.0 = 3, beta.gamma.0.0 = 1, alpha.theta.0.0 = 3,
	beta.theta.0.0 = 1, alpha.gamma = 3, beta.gamma = 1, alpha.theta = 3, beta.theta = 1, lambda.alpha = 1.0,
	lambda.beta = 1.0),
	global.pm.weight = 0.5,
	pm.weights = NULL,
	adapt_phase=1, memory_model = "HIGH")
{
	cluster = M_global$CLUSTERdata(Md1, cluster.data, iter, nchains, burnin, initial_values)

	if (is.null(cluster)) {
		return(NULL)
	}

	cluster.data = cluster$cluster.data
	cntrl.data = cluster$cntrl.data

	Md1$Algo <- theta_algorithm

	Md1$sim_type <- sim_type

	if (nrow(global.sim.params[global.sim.params$type == sim_type,]) == 0) {
		message("Missing simulation parametetrs");
		return(NULL)
	}

	if (!all(global.sim.params$value > 0)) {
		message("Invalid simulation parameter value");
		return(NULL)
	}

	Md1$global.sim.params <- global.sim.params

	Md1$Level = 2

	sp = M_global$CLUSTER_sim_paramsBB_3(Md1, sim.params, pm.weights, sim_type, cluster.data, cntrl.data)

	sim.params = sp$sim.params
	pm.weights = sp$pm.weights

	monitor = M_global$CLUSTER_monitor_BB_3(monitor)

	# Initialise the hyper-parameters
	Md1$mu.gamma.0.0 <- hyper_params$mu.gamma.0.0
	Md1$tau2.gamma.0.0 <- hyper_params$tau2.gamma.0.0
	Md1$alpha.gamma <- hyper_params$alpha.gamma
	Md1$beta.gamma <- hyper_params$beta.gamma
	Md1$alpha.gamma.0.0 <- hyper_params$alpha.gamma.0.0
	Md1$beta.gamma.0.0 <- hyper_params$beta.gamma.0.0

	Md1$mu.theta.0.0 <- hyper_params$mu.theta.0.0
	Md1$tau2.theta.0.0 <- hyper_params$tau2.theta.0.0
	Md1$alpha.theta <- hyper_params$alpha.theta
	Md1$beta.theta <- hyper_params$beta.theta
	Md1$alpha.theta.0.0 <- hyper_params$alpha.theta.0.0
	Md1$beta.theta.0.0 <- hyper_params$beta.theta.0.0

	# BB2004 parameters
	Md1$lambda.alpha <- hyper_params$lambda.alpha
	Md1$lambda.beta <- hyper_params$lambda.beta

	algo = 1
	if (Md1$Algo == "BB2004") {
		algo <- 1;
	} else {
		if (Md1$Algo == "MH") {
			algo <- 2;
		} else {
			if (Md1$Algo == "Adapt") {
				algo <- 3;
			} else {
				if (Md1$Algo == "Indep") {
					algo <- 4;
				} else {
					algo <- 1;
				}
			}
		}
	}


	Ret2 = .Call("bhpmBB_poisson_mc_exec", as.integer(nchains), as.integer(burnin),
					as.integer(iter), Md1$sim_type,
					memory_model, Md1$global.sim.params,
					sim.params,
					as.numeric(global.pm.weight),
					pm.weights,
					monitor,
					as.integer(Md1$nTreatments),
					as.integer(Md1$numClusters), as.integer(Md1$Level),
					Md1$maxOutcome.Grps, as.integer(Md1$numOutcome.Grp), as.integer(Md1$maxOutcomes),
					as.integer(t(Md1$nOutcome)), as.integer(aperm(Md1$x)), as.integer(aperm(Md1$y)),
					as.numeric(aperm(Md1$C)),
					as.numeric(aperm(Md1$T)),
					as.numeric(aperm(Md1$theta)),
					as.numeric(aperm(Md1$gamma)),
					as.numeric(Md1$mu.gamma.0.0),
					as.numeric(Md1$tau2.gamma.0.0),
					as.numeric(Md1$mu.theta.0.0),
					as.numeric(Md1$tau2.theta.0.0),
					as.numeric(Md1$alpha.gamma.0.0),
					as.numeric(Md1$beta.gamma.0.0),
					as.numeric(Md1$alpha.theta.0.0),
					as.numeric(Md1$beta.theta.0.0),
					as.numeric(Md1$alpha.gamma),
					as.numeric(Md1$beta.gamma),
					as.numeric(Md1$alpha.theta),
					as.numeric(Md1$beta.theta),
					as.numeric(Md1$mu.gamma.0),
					as.numeric(Md1$tau2.gamma.0),
					as.numeric(Md1$mu.theta.0),
					as.numeric(Md1$tau2.theta.0),
					as.numeric(aperm(Md1$mu.gamma)),
					as.numeric(aperm(Md1$mu.theta)),
					as.numeric(aperm(Md1$sigma2.gamma)),
					as.numeric(aperm(Md1$sigma2.theta)),
					as.numeric(aperm(Md1$pi)),
					as.numeric(Md1$alpha.pi),
					as.numeric(Md1$beta.pi),
					as.numeric(Md1$lambda.alpha),
					as.numeric(Md1$lambda.beta),
					as.integer(algo),
					as.integer(adapt_phase))

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

	pi_samples = NULL
	if (monitor[monitor$variable == "pi", ]$monitor == 1) {
		pi_samples = .Call("getPiSamplesClusterAll")
		pi_samples <- aperm(pi_samples)
	}

	alpha.pi_samples = NULL
	alpha.pi_acc = NULL
	if (monitor[monitor$variable == "alpha.pi", ]$monitor == 1) {
		alpha.pi_samples = .Call("getAlphaPiSamplesClusterAll")
		alpha.pi_samples = aperm(alpha.pi_samples)

		alpha.pi_acc = .Call("getAlphaPiAcceptClusterAll")
		alpha.pi_acc = aperm(alpha.pi_acc)
	}

	beta.pi_samples = NULL
	beta.pi_acc = NULL
	if (monitor[monitor$variable == "beta.pi", ]$monitor == 1) {
		beta.pi_samples = .Call("getBetaPiSamplesClusterAll")
		beta.pi_samples = aperm(beta.pi_samples)

		beta.pi_acc = .Call("getBetaPiAcceptClusterAll")
		beta.pi_acc = aperm(beta.pi_acc)
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

	model_fit = list(id = Md1$Id, sim_type = Md1$sim_type, chains = nchains, nClusters = Md1$numClusters,
			nTreatments = Md1$nTreatments,
			Clusters = Md1$Clusters, Trt.Grps = Md1$Trt.Grps, nOutcome.Grp = Md1$numOutcome.Grp, maxOutcome.Grps = Md1$maxOutcome.Grps,
			maxOutcomes = Md1$maxOutcomes, nOutcome = Md1$nOutcome, Outcome=Md1$Outcome, Outcome.Grp = Md1$Outcome.Grp,
			burnin = burnin, iter = iter,
			monitor = monitor,
			gamma = gamma_samples,
			theta = theta_samples,
			mu.gamma = mu.gamma_samples,
			mu.theta = mu.theta_samples,
			sigma2.gamma = sigma2.gamma_samples,
			sigma2.theta = sigma2.theta_samples,
			pi = pi_samples,
			alpha.pi = alpha.pi_samples,
			beta.pi = beta.pi_samples,
			alpha.pi_acc = alpha.pi_acc,
			beta.pi_acc = beta.pi_acc,
			mu.gamma.0 = mu.gamma.0_samples,
			mu.theta.0 = mu.theta.0_samples,
			tau2.gamma.0 = tau2.gamma.0_samples,
			tau2.theta.0 = tau2.theta.0_samples,
			gamma_acc = gamma_acc,
			theta_acc = theta_acc)
			
	# Model is poisson with BB hierarchy and dependent clusters
	attr(model_fit, "model") = "BB_pois_dep_lev2"

	return(model_fit)
}

Md1$initVars = function() {

    # Data Structure
    Md1$Outcome.Grp <- c()
    Md1$numOutcome.Grp <- NA
    Md1$numClusters <- NA
    Md1$nOutcome <- c()
    Md1$maxOutcomes <- NA

    # Cluster Event Data
    Md1$x <- array()
    Md1$C <- array()
    Md1$y <- array()
    Md1$T <- array()

    # Hyperparameters
    Md1$mu.gamma.0.0 <- NA
    Md1$tau2.gamma.0.0 <- NA
    Md1$mu.theta.0.0 <- NA
    Md1$tau2.theta.0.0 <- NA
    Md1$alpha.gamma.0.0 <- NA
    Md1$beta.gamma.0.0 <- NA
    Md1$alpha.theta.0.0 <- NA
    Md1$beta.theta.0.0 <- NA
    Md1$alpha.gamma <- NA
    Md1$beta.gamma <- NA
    Md1$alpha.theta <- NA
    Md1$beta.theta <- NA

	# Parameters/Simulated values
    # Stage 3
    Md1$mu.gamma.0 <- c()
    Md1$tau2.gamma.0 <- c()
    Md1$mu.theta.0 <- c()
    Md1$tau2.theta.0 <- c()

    # Stage 2
    Md1$mu.gamma <- array()
    Md1$mu.theta <- array()
    Md1$sigma2.gamma <- array()
    Md1$sigma2.theta <- array()

    # Stage 1
    Md1$theta <- array()
    Md1$gamma <- array()

	# BB2004 parameters
	Md1$lambda.alpha <- NA
	Md1$lambda.beta <- NA
	Md1$alpha.pi <- NA
	Md1$beta.pi <- NA
	Md1$pi <- NA
}

Md1$initChains = function(c) {
	# Choose random values for gamma and theta
	for (i in 1:Md1$numClusters) {
		numOutcome.Grp = Md1$numOutcome.Grp[i]
		for (b in 1:numOutcome.Grp) {
			Md1$gamma[c, i, b, 1:Md1$nOutcome[i, b]] <- runif(Md1$nOutcome[i, b], -10, 10)

			Md1$gamma[c, i, b, ][is.infinite(Md1$gamma[c, i, b, ])] = -10

			Md1$gamma[c, i, b, ][is.nan(Md1$gamma[c, i, b, ])] = -10 # -1000

			for (t in 1:(Md1$nTreatments - 1)) {
				Md1$theta[c, t, i, b, 1:Md1$nOutcome[i, b]] <- runif(Md1$nOutcome[i, b], -10, 10)
				Md1$theta[c, t, i, b, ][is.infinite(Md1$theta[c, t, i, b, ])] = -10
				Md1$theta[c, t, i, b, ][is.nan(Md1$theta[c, t, i, b, ])] = -10 # -1000
			}
		}

		Md1$mu.gamma[c, i, 1:numOutcome.Grp] = runif(numOutcome.Grp, -10, 10)
		Md1$mu.theta[c,, i, 1:numOutcome.Grp] = runif(numOutcome.Grp*(Md1$nTreatments - 1), -10, 10)
		Md1$sigma2.gamma[c, i, 1:numOutcome.Grp] = runif(numOutcome.Grp, 5, 20)
		Md1$sigma2.theta[c,, i, 1:numOutcome.Grp] = runif(numOutcome.Grp*(Md1$nTreatments - 1), 5, 20)

		Md1$pi[c,, i, 1:numOutcome.Grp] = runif(numOutcome.Grp*(Md1$nTreatments - 1), 0, 1)

		Md1$mu.gamma.0[c] = runif(1, -10, 10)
		Md1$tau2.gamma.0[c] = runif(1, 5, 20)
		Md1$mu.theta.0[c,] = runif(1*(Md1$nTreatments - 1), -10, 10)
		Md1$tau2.theta.0[c,] = runif(1*(Md1$nTreatments - 1), 5, 20)

		Md1$alpha.pi[c,] = runif(1*(Md1$nTreatments - 1), 1.25, 100)
		Md1$beta.pi[c,] = runif(1*(Md1$nTreatments - 1), 1.25, 100)
	}
}

Md1$initialiseChains = function(initial_values, nchains) {

	Md1$theta = array(0, dim=c(nchains, Md1$nTreatments - 1, Md1$numClusters, Md1$maxOutcome.Grps, Md1$maxOutcomes))
	Md1$gamma = array(0, dim=c(nchains, Md1$numClusters, Md1$maxOutcome.Grps, Md1$maxOutcomes))

	if (is.null(initial_values)) {

		# Initialise the first chain with the data
		for (i in 1:Md1$numClusters) {
			numOutcome.Grp = Md1$numOutcome.Grp[i]
			for (b in 1:numOutcome.Grp) {
				Md1$gamma[1, i, b, ] <- log(Md1$x[i, b,]/Md1$C[i, b, ])

				for (t in 1:(Md1$nTreatments - 1)) {
					Md1$theta[1, t, i, b, ] <- log(Md1$y[t, i, b,]/Md1$T[t, i , b,]) - Md1$gamma[1, i, b, ]
					Md1$theta[1, t, i, b, ][is.infinite(Md1$theta[1, t, i, b, ])] = -10 # -1000
					Md1$theta[1, t, i, b, ][is.nan(Md1$theta[1, t, i, b, ])] = -10 # -1000
				}

				Md1$gamma[1, i, b, ][is.infinite(Md1$gamma[1, i, b, ])] = -10 # -1000
				Md1$gamma[1, i, b, ][is.nan(Md1$gamma[1, i, b, ])] = -10 # -1000
			}
		}

		Md1$mu.gamma <- array(0, dim = c(nchains, Md1$numClusters, Md1$maxOutcome.Grps))
		Md1$mu.theta <- array(0, dim = c(nchains, Md1$nTreatments - 1, Md1$numClusters, Md1$maxOutcome.Grps))
		Md1$sigma2.gamma <- array(10, dim = c(nchains, Md1$numClusters, Md1$maxOutcome.Grps))
		Md1$sigma2.theta <- array(10, dim = c(nchains, Md1$nTreatments - 1, Md1$numClusters, Md1$maxOutcome.Grps))

		Md1$pi <- array(0.5, dim = c(nchains, Md1$nTreatments - 1, Md1$numClusters, Md1$maxOutcome.Grps))

		Md1$mu.gamma.0 <- rep(0, nchains)
		Md1$tau2.gamma.0 <- rep(10, nchains)
		Md1$mu.theta.0 <- array(0, dim = c(nchains, Md1$nTreatments - 1))
		Md1$tau2.theta.0 <-  array(10, dim = c(nchains, Md1$nTreatments - 1))

		Md1$alpha.pi <- array(1.5, dim = c(nchains, Md1$nTreatments - 1))
		Md1$beta.pi <- array(1.5, dim = c(nchains, Md1$nTreatments - 1))

		if (nchains > 1) {
			for (c in 2:nchains) {
				Md1$initChains(c)
			}
		}
	}
	else {

		Md1$mu.gamma.0 <- rep(0, nchains)
		Md1$tau2.gamma.0 <- rep(10, nchains)

		Md1$mu.theta.0 <- array(0, dim = c(nchains, Md1$nTreatments - 1))
		Md1$tau2.theta.0 <-  array(10, dim = c(nchains, Md1$nTreatments - 1))

		Md1$alpha.pi <- array(1.5, dim = c(nchains, Md1$nTreatments - 1))
		Md1$beta.pi <- array(1.5, dim = c(nchains, Md1$nTreatments - 1))

		for (c in 1:nchains) {
			Md1$mu.gamma.0[c] = initial_values$mu.gamma.0[c]
			Md1$tau2.gamma.0[c] = initial_values$tau2.gamma.0[c]

			for (t in 1:(Md1$nTreatments - 1)) {
				Md1$mu.theta.0[c,t] = initial_values$mu.theta.0[[t]][c]
				Md1$tau2.theta.0[c,t] = initial_values$tau2.theta.0[[t]][c]

				Md1$alpha.pi[c,t] = initial_values$alpha.pi[[t]][c]
				Md1$beta.pi[c,t] = initial_values$beta.pi[[t]][c]
			}
		}

		Md1$mu.gamma <- array(0, dim = c(nchains, Md1$numClusters, Md1$maxOutcome.Grps))
		Md1$sigma2.gamma <- array(0, dim = c(nchains, Md1$numClusters, Md1$maxOutcome.Grps))

		Md1$mu.theta <- array(0, dim = c(nchains, Md1$nTreatments - 1, Md1$numClusters, Md1$maxOutcome.Grps))
		Md1$sigma2.theta <- array(0, dim = c(nchains, Md1$nTreatments - 1, Md1$numClusters, Md1$maxOutcome.Grps))

		Md1$pi <- array(0.5, dim = c(nchains, Md1$nTreatments - 1, Md1$numClusters, Md1$maxOutcome.Grps))

		for (c in 1:nchains) {
			for (i in 1:Md1$numClusters) {
				cluster = Md1$Clusters[i]
				for (b in 1:Md1$numOutcome.Grp[i]) {
					data = initial_values$mu.gamma[initial_values$mu.gamma$chain == c &
									initial_values$mu.gamma$Cluster == cluster
												& initial_values$mu.gamma$Outcome.Grp == Md1$Outcome.Grp[i, b],]
					Md1$mu.gamma[c, i, b] = data$value

					data = initial_values$sigma2.gamma[initial_values$sigma2.gamma$chain == c &
									initial_values$sigma2.gamma$Cluster == cluster
												& initial_values$sigma2.gamma$Outcome.Grp == Md1$Outcome.Grp[i, b],]
					Md1$sigma2.gamma[c, i, b] = data$value


					for (t in 1:(Md1$nTreatments - 1)) {
						data = initial_values$mu.theta[[t]][initial_values$mu.theta[[t]]$chain == c &
									initial_values$mu.theta[[t]]$Cluster == cluster
												& initial_values$mu.theta[[t]]$Outcome.Grp == Md1$Outcome.Grp[i, b],]
						Md1$mu.theta[c, t, i, b] = data$value

						data = initial_values$sigma2.theta[[t]][initial_values$sigma2.theta[[t]]$chain == c &
									initial_values$sigma2.theta[[t]]$Cluster == cluster
												& initial_values$sigma2.theta[[t]]$Outcome.Grp == Md1$Outcome.Grp[i, b],]
						Md1$sigma2.theta[c, t, i, b] = data$value


						data = initial_values$pi[[t]][initial_values$pi[[t]]$chain == c &
									initial_values$pi[[t]]$Cluster == cluster
												& initial_values$pi[[t]]$Outcome.Grp == Md1$Outcome.Grp[i, b],]
						Md1$pi[c, t, i, b] = data$value
					}
				}
			}
		}

		for (c in 1:nchains) {
			for (i in 1:Md1$numClusters) {
				cluster = Md1$Clusters[i]
				for (b in 1:Md1$numOutcome.Grp[i]) {
					for (j in 1:Md1$nOutcome[i, b]) {
						ae = Md1$Outcome[i, b, j]
						data = initial_values$gamma[initial_values$gamma$chain == c
										& initial_values$gamma$Cluster == cluster
										& initial_values$gamma$Outcome.Grp == Md1$Outcome.Grp[i, b]
										& initial_values$gamma$Outcome == ae,]
						Md1$gamma[c, i, b, j] = data$value

						for (t in 1:(Md1$nTreatments - 1)) {
							data = initial_values$theta[[t]][initial_values$theta[[t]]$chain == c
										& initial_values$theta[[t]]$Cluster == cluster
										& initial_values$theta[[t]]$Outcome.Grp == Md1$Outcome.Grp[i, b]
										& initial_values$theta[[t]]$Outcome == ae,]
							Md1$theta[c, t, i, b, j] = data$value
						}
					}
				}
			}
		}
	}
}
