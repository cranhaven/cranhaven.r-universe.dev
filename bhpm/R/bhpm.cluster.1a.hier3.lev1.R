# bhpm.cluster
# bhpm: Cluster Analysis wrapper
# R. Carragher
# Date: 29/06/2018


Md1a_lev1 <- new.env()

Md1a_lev1$Id <- "$Id: bhpm.cluster.1a.hier3.lev1.R,v 1.13 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.cluster.1a.dep.lev1 <- function(cluster.data, sim_type = "SLICE", burnin = 10000, iter = 40000, nchains = 3,
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
	cluster = M_global$CLUSTERdata(Md1a_lev1, cluster.data, iter, nchains, burnin, initial_values)

	if (is.null(cluster)) {
		return(NULL)
	}

	cluster.data = cluster$cluster.data
	cntrl.data = cluster$cntrl.data

	if (M_global$checkBS(Md1a_lev1, cntrl.data)) {
		return(NULL)
	}

	Md1a_lev1$sim_type <- sim_type

	if (nrow(global.sim.params[global.sim.params$type == sim_type,]) != 1) {
		message("Missing simulation parametetrs");
		return(NULL)
	}

	Md1a_lev1$global.sim.param <- global.sim.params[global.sim.params$type == sim_type,]$value
	Md1a_lev1$global.sim.param_ctrl <- global.sim.params[global.sim.params$type == sim_type,]$control

	if (Md1a_lev1$global.sim.param <= 0) {
		message("Invalid simulation parametetr value");
		return(NULL)
	}

	Md1a_lev1$level = 1

	sim.params = M_global$CLUSTER_sim_params1a(Md1a_lev1, sim.params, sim_type, cluster.data, cntrl.data)

	monitor = M_global$CLUSTER_monitor_1a_3(monitor)

	# Initialise the hyper-parameters
	Md1a_lev1$mu.gamma.0.0 <- hyper_params$mu.gamma.0.0
	Md1a_lev1$tau2.gamma.0.0 <- hyper_params$tau2.gamma.0.0
	Md1a_lev1$alpha.gamma <- hyper_params$alpha.gamma
	Md1a_lev1$beta.gamma <- hyper_params$beta.gamma
	Md1a_lev1$alpha.gamma.0.0 <- hyper_params$alpha.gamma.0.0
	Md1a_lev1$beta.gamma.0.0 <- hyper_params$beta.gamma.0.0

	Md1a_lev1$mu.theta.0.0 <- hyper_params$mu.theta.0.0
	Md1a_lev1$tau2.theta.0.0 <- hyper_params$tau2.theta.0.0
	Md1a_lev1$alpha.theta <- hyper_params$alpha.theta
	Md1a_lev1$beta.theta <- hyper_params$beta.theta
	Md1a_lev1$alpha.theta.0.0 <- hyper_params$alpha.theta.0.0
	Md1a_lev1$beta.theta.0.0 <- hyper_params$beta.theta.0.0

	Ret2 = .Call("bhpm1a_poisson_mc_exec", as.integer(nchains), as.integer(burnin),
					as.integer(iter), Md1a_lev1$sim_type,
					memory_model,
					as.numeric(Md1a_lev1$global.sim.param),
					as.numeric(Md1a_lev1$global.sim.param_ctrl),
					sim.params,
					monitor,
					as.integer(Md1a_lev1$nTreatments),
					as.integer(Md1a_lev1$numClusters), as.integer(Md1a_lev1$level),
					Md1a_lev1$maxOutcome.Grps, as.integer(Md1a_lev1$numOutcome.Grp), as.integer(Md1a_lev1$maxOutcomes),
					as.integer(t(Md1a_lev1$nOutcome)), as.integer(aperm(Md1a_lev1$x)), as.integer(aperm(Md1a_lev1$y)),
					as.numeric(aperm(Md1a_lev1$C)),
					as.numeric(aperm(Md1a_lev1$T)),
					as.numeric(aperm(Md1a_lev1$theta)),
					as.numeric(aperm(Md1a_lev1$gamma)),
					as.numeric(Md1a_lev1$mu.gamma.0.0),
					as.numeric(Md1a_lev1$tau2.gamma.0.0),
					as.numeric(Md1a_lev1$mu.theta.0.0),
					as.numeric(Md1a_lev1$tau2.theta.0.0),
					as.numeric(Md1a_lev1$alpha.gamma.0.0),
					as.numeric(Md1a_lev1$beta.gamma.0.0),
					as.numeric(Md1a_lev1$alpha.theta.0.0),
					as.numeric(Md1a_lev1$beta.theta.0.0),
					as.numeric(Md1a_lev1$alpha.gamma),
					as.numeric(Md1a_lev1$beta.gamma),
					as.numeric(Md1a_lev1$alpha.theta),
					as.numeric(Md1a_lev1$beta.theta),
					as.numeric(Md1a_lev1$mu.gamma.0),
					as.numeric(Md1a_lev1$tau2.gamma.0),
					as.numeric(Md1a_lev1$mu.theta.0),
					as.numeric(Md1a_lev1$tau2.theta.0),
					as.numeric(aperm(Md1a_lev1$mu.gamma)),
					as.numeric(aperm(Md1a_lev1$mu.theta)),
					as.numeric(aperm(Md1a_lev1$sigma2.gamma)),
					as.numeric(aperm(Md1a_lev1$sigma2.theta)))

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

	model_fit = list(id = Md1a_lev1$Id, sim_type = Md1a_lev1$sim_type, chains = nchains, nClusters = Md1a_lev1$numClusters,
			nTreatments = Md1a_lev1$nTreatments,
			Clusters = Md1a_lev1$Clusters, Trt.Grps = Md1a_lev1$Trt.Grps, nOutcome.Grp = Md1a_lev1$numOutcome.Grp,
			maxOutcome.Grps = Md1a_lev1$maxOutcome.Grps,
			maxOutcomes = Md1a_lev1$maxOutcomes, nOutcome = Md1a_lev1$nOutcome, Outcome=Md1a_lev1$Outcome,
			Outcome.Grp = Md1a_lev1$Outcome.Grp,
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
	attr(model_fit, "model") = "1a_pois_dep_lev1"

	return(model_fit)
}

Md1a_lev1$initVars = function() {

    # Data Structure
    Md1a_lev1$Outcome.Grp <- c()
    Md1a_lev1$numOutcome.Grp <- NA
    Md1a_lev1$numClusters <- NA
    Md1a_lev1$nOutcome <- c()
    Md1a_lev1$maxOutcomes <- NA

    # Cluster Event Data
    Md1a_lev1$x <- array()
    Md1a_lev1$C <- array()
    Md1a_lev1$y <- array()
    Md1a_lev1$T <- array()

    # Hyperparameters
    Md1a_lev1$mu.gamma.0.0 <- NA
    Md1a_lev1$tau2.gamma.0.0 <- NA
    Md1a_lev1$mu.theta.0.0 <- NA
    Md1a_lev1$tau2.theta.0.0 <- NA
    Md1a_lev1$alpha.gamma.0.0 <- NA
    Md1a_lev1$beta.gamma.0.0 <- NA
    Md1a_lev1$alpha.theta.0.0 <- NA
    Md1a_lev1$beta.theta.0.0 <- NA
    Md1a_lev1$alpha.gamma <- NA
    Md1a_lev1$beta.gamma <- NA
    Md1a_lev1$alpha.theta <- NA
    Md1a_lev1$beta.theta <- NA

	# Parameters/Simulated values
    # Stage 3
    Md1a_lev1$mu.gamma.0 <- c()
    Md1a_lev1$tau2.gamma.0 <- c()
    Md1a_lev1$mu.theta.0 <- c()
    Md1a_lev1$tau2.theta.0 <- c()

    # Stage 2
    Md1a_lev1$mu.gamma <- array()
    Md1a_lev1$mu.theta <- array()
    Md1a_lev1$sigma2.gamma <- array()
    Md1a_lev1$sigma2.theta <- array()

    # Stage 1
    Md1a_lev1$theta <- array()
    Md1a_lev1$gamma <- array()
}

Md1a_lev1$initChains = function(c) {
	# Choose random values for gamma and theta
	for (i in 1:Md1a_lev1$numClusters) {
		numOutcome.Grp = Md1a_lev1$numOutcome.Grp[i]
		for (b in 1:numOutcome.Grp) {
			Md1a_lev1$gamma[c, i, b, 1:Md1a_lev1$nOutcome[i, b]] <- runif(Md1a_lev1$nOutcome[i, b], -10, 10)

			Md1a_lev1$gamma[c, i, b, ][is.infinite(Md1a_lev1$gamma[c, i, b, ])] = -10

			Md1a_lev1$gamma[c, i, b, ][is.nan(Md1a_lev1$gamma[c, i, b, ])] = -10 # -1000

			for (t in 1:(Md1a_lev1$nTreatments - 1)) {
				Md1a_lev1$theta[c, t, i, b, 1:Md1a_lev1$nOutcome[i, b]] <- runif(Md1a_lev1$nOutcome[i, b], -10, 10)
				Md1a_lev1$theta[c, t, i, b, ][is.infinite(Md1a_lev1$theta[c, t, i, b, ])] = -10
				Md1a_lev1$theta[c, t, i, b, ][is.nan(Md1a_lev1$theta[c, t, i, b, ])] = -10 # -1000


			}
		}

	}


	Md1a_lev1$mu.gamma.0[c] = runif(1, -10, 10)
	Md1a_lev1$tau2.gamma.0[c] = runif(1, 5, 20)
	Md1a_lev1$mu.theta.0[c,] = runif(1*(Md1a_lev1$nTreatments - 1), -10, 10)
	Md1a_lev1$tau2.theta.0[c,] = runif(1*(Md1a_lev1$nTreatments - 1), 5, 20)

	Md1a_lev1$mu.theta[c,, 1:numOutcome.Grp] = runif(numOutcome.Grp*(Md1a_lev1$nTreatments - 1), -10, 10)
	Md1a_lev1$mu.gamma[c, 1:numOutcome.Grp] = runif(numOutcome.Grp, -10, 10)
	Md1a_lev1$sigma2.gamma[c, 1:numOutcome.Grp] = runif(numOutcome.Grp, 5, 20)
	Md1a_lev1$sigma2.theta[c,, 1:numOutcome.Grp] = runif(numOutcome.Grp*(Md1a_lev1$nTreatments - 1), 5, 20)
}

Md1a_lev1$initialiseChains = function(initial_values, nchains) {

	Md1a_lev1$theta = array(0, dim=c(nchains, Md1a_lev1$nTreatments - 1, Md1a_lev1$numClusters, Md1a_lev1$maxOutcome.Grps, Md1a_lev1$maxOutcomes))
	Md1a_lev1$gamma = array(0, dim=c(nchains, Md1a_lev1$numClusters, Md1a_lev1$maxOutcome.Grps, Md1a_lev1$maxOutcomes))

	if (is.null(initial_values)) {

		# Initialise the first chain with the data
		for (i in 1:Md1a_lev1$numClusters) {
			numOutcome.Grp = Md1a_lev1$numOutcome.Grp[i]
			for (b in 1:numOutcome.Grp) {
				Md1a_lev1$gamma[1, i, b, ] <- log(Md1a_lev1$x[i, b,]/Md1a_lev1$C[i, b, ])

				for (t in 1:(Md1a_lev1$nTreatments - 1)) {
					Md1a_lev1$theta[1, t, i, b, ] <- log(Md1a_lev1$y[t, i, b,]/Md1a_lev1$T[t, i, b,]) - Md1a_lev1$gamma[1, i, b, ]
					Md1a_lev1$theta[1, t, i, b, ][is.infinite(Md1a_lev1$theta[1, t, i, b, ])] = -10 # -1000
					Md1a_lev1$theta[1, t, i, b, ][is.nan(Md1a_lev1$theta[1, t, i, b, ])] = -10 # -1000
				}
				Md1a_lev1$gamma[1, i, b, ][is.infinite(Md1a_lev1$gamma[1, i, b, ])] = -10 # -1000
				Md1a_lev1$gamma[1, i, b, ][is.nan(Md1a_lev1$gamma[1, i, b, ])] = -10 # -1000
			}
		}

		Md1a_lev1$mu.theta <- array(0, dim = c(nchains, Md1a_lev1$nTreatments - 1, Md1a_lev1$maxOutcome.Grps))
		Md1a_lev1$mu.gamma <- array(0, dim = c(nchains, Md1a_lev1$maxOutcome.Grps))
		Md1a_lev1$sigma2.gamma <- array(10, dim = c(nchains, Md1a_lev1$maxOutcome.Grps))
		Md1a_lev1$sigma2.theta <- array(10, dim = c(nchains, Md1a_lev1$nTreatments - 1, Md1a_lev1$maxOutcome.Grps))

		Md1a_lev1$mu.gamma.0 <- rep(0, nchains)
		Md1a_lev1$tau2.gamma.0 <- rep(10, nchains)
		Md1a_lev1$mu.theta.0 <-  array(0, dim = c(nchains, Md1a_lev1$nTreatments - 1))
		Md1a_lev1$tau2.theta.0 <- array(10, dim = c(nchains, Md1a_lev1$nTreatments - 1))

		if (nchains > 1) {
			for (c in 2:nchains) {
				Md1a_lev1$initChains(c)
			}

		}
	}
	else {

		Md1a_lev1$mu.gamma.0 <- rep(0, nchains)
		Md1a_lev1$tau2.gamma.0 <- rep(10, nchains)

		Md1a_lev1$mu.theta.0 <-  array(0, dim = c(nchains, Md1a_lev1$nTreatments - 1))
		Md1a_lev1$tau2.theta.0 <- array(10, dim = c(nchains, Md1a_lev1$nTreatments - 1))

		for (c in 1:nchains) {
			Md1a_lev1$mu.gamma.0[c] = initial_values$mu.gamma.0[c]
			Md1a_lev1$tau2.gamma.0[c] = initial_values$tau2.gamma.0[c]

			for (t in 1:(Md1a_lev1$nTreatments - 1)) {
				Md1a_lev1$mu.theta.0[c,t] = initial_values$mu.theta.0[[t]][c]
				Md1a_lev1$tau2.theta.0[c,t] = initial_values$tau2.theta.0[[t]][c]
			}
		}

		Md1a_lev1$mu.theta <- array(0, dim = c(nchains, Md1a_lev1$nTreatments - 1, Md1a_lev1$maxOutcome.Grps))

		Md1a_lev1$mu.gamma <- array(0, dim = c(nchains, Md1a_lev1$maxOutcome.Grps))
		Md1a_lev1$sigma2.gamma <- array(0, dim = c(nchains, Md1a_lev1$maxOutcome.Grps))
		Md1a_lev1$sigma2.theta <- array(0, dim = c(nchains, Md1a_lev1$nTreatments - 1, Md1a_lev1$maxOutcome.Grps))

		for (c in 1:nchains) {
			# Have the same 
			for (b in 1:length(Md1a_lev1$Outcome.Grp[1,])) {
				data = initial_values$mu.gamma[initial_values$mu.gamma$chain == c
												& initial_values$mu.gamma$Outcome.Grp == Md1a_lev1$Outcome.Grp[1, b],]
				Md1a_lev1$mu.gamma[c, b] = data$value

				data = initial_values$sigma2.gamma[initial_values$sigma2.gamma$chain == c
												& initial_values$sigma2.gamma$Outcome.Grp == Md1a_lev1$Outcome.Grp[1, b],]
				Md1a_lev1$sigma2.gamma[c, b] = data$value

				for (t in 1:(Md1a_lev1$nTreatments - 1)) {
					data = initial_values$mu.theta[[t]][initial_values$mu.theta[[t]]$chain == c
												& initial_values$mu.theta[[t]]$Outcome.Grp == Md1a_lev1$Outcome.Grp[1, b],]
					Md1a_lev1$mu.theta[c, t, b] = data$value

					data = initial_values$sigma2.theta[[t]][initial_values$sigma2.theta[[t]]$chain == c
												& initial_values$sigma2.theta[[t]]$Outcome.Grp == Md1a_lev1$Outcome.Grp[1, b],]
					Md1a_lev1$sigma2.theta[c, t, b] = data$value

				}
			}
		}

		for (c in 1:nchains) {
			for (i in 1:Md1a_lev1$numClusters) {
				cluster = Md1a_lev1$Clusters[i]
				for (b in 1:Md1a_lev1$numOutcome.Grp[i]) {
					for (j in 1:Md1a_lev1$nOutcome[i, b]) {
						ae = Md1a_lev1$Outcome[i, b, j]
						data = initial_values$gamma[initial_values$gamma$chain == c
										& initial_values$gamma$Cluster == cluster
										& initial_values$gamma$Outcome.Grp == Md1a_lev1$Outcome.Grp[i, b]
										& initial_values$gamma$Outcome == ae,]
						Md1a_lev1$gamma[c, i, b, j] = data$value

						for (t in 1:(Md1a_lev1$nTreatments - 1)) {
							data = initial_values$theta[[t]][initial_values$theta[[t]]$chain == c
										& initial_values$theta[[t]]$Cluster == cluster
										& initial_values$theta[[t]]$Outcome.Grp == Md1a_lev1$Outcome.Grp[i, b]
										& initial_values$theta[[t]]$Outcome == ae,]
							Md1a_lev1$theta[c, t, i, b, j] = data$value
						}
					}
				}
			}
		}
	}
}
