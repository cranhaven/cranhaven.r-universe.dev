bhpm.gen.initial.values = function(cluster.data, nchains = 3, model = "1a", hier = 3, level = 1) {

	if (nchains < 1)
		return(NULL)

	if (is.character(cluster.data)) {
		file = cluster.data
		cluster.data <- read.table(file, header=TRUE, stringsAsFactors = FALSE)
	}

	facs <- sapply(cluster.data, is.factor)
	cluster.data[facs] <- sapply(cluster.data[facs], as.character)


	initial_values = bhpm.cluster.gen.initial.values(cluster.data, nchains, model, hier, level)

	initial_values
}

bhpm.cluster.gen.initial.values = function(cluster.data, nchains, model, hier, level) {

	initial_values = NULL

	# Check the correct columns are defined
	cols = c("Outcome.Grp", "Outcome", "Count", "Trt.Grp", "Cluster", "Exposure")
	if (M_global$checkCols(cols, cluster.data)) {
		message("Missing columns");
		return(NULL)
	}

   # Order by outcome group, outcome, cluster and treatment group
    ordered.data <- cluster.data[order(cluster.data$Cluster, cluster.data$Outcome.Grp, cluster.data$Outcome, cluster.data$Trt.Grp),, drop=FALSE]

	treatment.groups <- unique(ordered.data$Trt.Grp)
	cntrl.group = min(treatment.groups)
	comparator.groups = treatment.groups[treatment.groups > cntrl.group]
	td <- ordered.data[ordered.data$Trt.Grp > cntrl.group, ]

	cntrl.data <- ordered.data[ordered.data$Trt.Grp == cntrl.group, ]
	treat.data <- split(td, f = td$Trt.Grp)


    # The data is ordered so a straight comparison is possible
	for (i in 1:length(comparator.groups)) {
		if(!identical(cntrl.data$Outcome.Grp, treat.data[[i]]$Outcome.Grp)) {
			message("Mismatced outcome group data");
			return(NULL)
		}
		if(!identical(cntrl.data$Outcome, treat.data[[i]]$Outcome)) {
			message("Mismatced outcome data");
			return(NULL)
		}
		if(!identical(cntrl.data$Cluster, treat.data[[i]]$Cluster)) {
			message("Mismatced cluster data");
			return(NULL)
		}
	}

	Clusters = unique(cntrl.data$Cluster)
	numClusters = length(Clusters)
	numOutcome.Grp = c()

	for (i in 1:numClusters) {
		c = cntrl.data[cntrl.data$Cluster == Clusters[i],]
		b = unique(c$Outcome.Grp)
		numOutcome.Grp[i] = length(b)
	}

	maxOutcome.Grps = max(numOutcome.Grp)
	Outcome.Grp = array(NA, dim = c(numClusters, maxOutcome.Grps))
	for (i in 1:numClusters) {
		c = cntrl.data[cntrl.data$Cluster == Clusters[i],]
		b = unique(c$Outcome.Grp)
		Outcome.Grp[i, 1:length(b)] = b
	}

	nOutcome = array(0, dim = c(numClusters, maxOutcome.Grps))
	for (i in 1:numClusters) {
		n = numOutcome.Grp[i]
		for (b in 1:n) {
			nOutcome[i, b] = length(unique(cntrl.data[cntrl.data$Outcome.Grp == Outcome.Grp[i, b], ]$Outcome))
		}
	}

    maxOutcomes <- max(nOutcome)

	n = nrow(cntrl.data)

	data = cntrl.data[,colnames(cntrl.data) %in% c("Cluster", "Outcome.Grp", "Outcome")]
	chain = rep(1, n)

	t = do.call("rbind", rep(list(data), nchains))
	chain = rep(1:nchains, rep(n, nchains))
	t = cbind(t, chain)

	gamma = t

	value = rep(0, nrow(gamma))
	gamma = cbind(gamma, value)

	# First chain - derive the theta/gamma from the data
	x_chain = cntrl.data$Count/cntrl.data$Exposure
	ga = log(x_chain)
	ga[is.infinite(ga)] = -10 
	ga[is.nan(ga)] = -10
	gamma$value[1:n] = ga

	if (nchains > 1) {
		u = runif(n*(nchains -1), -10, 10)
		gamma$value[(n + 1):(n * nchains)] = u
	}

	theta = rep(list(t), length(comparator.groups))
	theta = lapply(1:length(comparator.groups), function (i) {
		theta[[i]]$value <- NA
        td = treat.data[[i]]
        y_chain = td$Count/td$Exposure
        th = log(y_chain) - ga
        th[is.infinite(th)] = -10
		th[is.nan(th)] = -10
        theta[[i]]$value[1:n] = th
        if (nchains > 2) {
			u = runif(n*(nchains - 1), -10, 10)
			theta[[i]]$value[(n + 1):(nchains*n)] = u
		}
		theta[[i]]
	})

	if (level == 1) {
		sz = length(Outcome.Grp[1,])
		mu.gamma <- data.frame(chain = numeric(nchains*sz), Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)
		sigma2.gamma <- data.frame(chain = numeric(nchains*sz), Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)

		mu.gamma[1:sz, ]$chain = 1
		mu.gamma[1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
		mu.gamma[1:sz, ]$value = 0
		sigma2.gamma[1:sz, ]$chain = 1
		sigma2.gamma[1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
		sigma2.gamma[1:sz, ]$value = 10

		if (nchains > 1) {
			offset = sz
			for (c in 2:nchains) {
				mu.gamma[offset + 1:sz, ]$chain = c
				mu.gamma[offset + 1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
				mu.gamma[offset + 1:sz, ]$value = runif(sz, -10, 10)
				sigma2.gamma[offset + 1:sz, ]$chain = c
				sigma2.gamma[offset + 1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
				sigma2.gamma[offset + 1:sz, ]$value = runif(sz, 5, 20)
				offset = offset + sz
			}
		}

		mu.theta <- list()
		sigma2.theta <- list()
		pi <- list()

		for (i in 1:length(comparator.groups)) {
			mu.theta[[i]] <- data.frame(chain = numeric(nchains*sz), Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)
			sigma2.theta[[i]] <- data.frame(chain = numeric(nchains*sz), Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)
			pi[[i]] <- data.frame(chain = numeric(nchains*sz), Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)
			mu.theta[[i]][1:sz, ]$chain = 1
			mu.theta[[i]][1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
			mu.theta[[i]][1:sz, ]$value = 0
			sigma2.theta[[i]][1:sz, ]$chain = 1
			sigma2.theta[[i]][1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
			sigma2.theta[[i]][1:sz, ]$value = 10
			pi[[i]][1:sz, ]$chain = 1
			pi[[i]][1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
			pi[[i]][1:sz, ]$value = 0.5

			if (nchains > 1) {
				offset = sz
				for (c in 2:nchains) {
					mu.theta[[i]][offset + 1:sz, ]$chain = c
					mu.theta[[i]][offset + 1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
					mu.theta[[i]][offset + 1:sz, ]$value = runif(sz, -10, 10)
					sigma2.theta[[i]][offset + 1:sz, ]$chain = c
					sigma2.theta[[i]][offset + 1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
					sigma2.theta[[i]][offset + 1:sz, ]$value = runif(sz, 5, 20)
	
					pi[[i]][offset + 1:sz, ]$chain = c
					pi[[i]][offset + 1:sz, ]$Outcome.Grp = Outcome.Grp[1,]
					pi[[i]][offset + 1:sz, ]$value = runif(sz, 0, 1)
					offset = offset + sz
				}
			}
		}
	}
	else {

		# 1a hier3 lev 0, 2
		sz = sum(numOutcome.Grp)
		mu.gamma <- data.frame(chain = numeric(nchains*sz), Cluster = character(nchains*sz),
											Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)
		sigma2.gamma <- data.frame(chain = numeric(nchains*sz), Cluster = character(nchains*sz),
											Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)

		# First chain used fixed values based on the original hyper-params

		offset = 0
		for (i in 1:numClusters) {
			mu.gamma[offset + 1:numOutcome.Grp[i],]$chain = 1
			mu.gamma[offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
			mu.gamma[offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
			mu.gamma[offset + 1:numOutcome.Grp[i],]$value = 0
			sigma2.gamma[offset + 1:numOutcome.Grp[i],]$chain = 1
			sigma2.gamma[offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
			sigma2.gamma[offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
			sigma2.gamma[offset + 1:numOutcome.Grp[i],]$value = 0

			offset = offset + numOutcome.Grp[i]
		}

		if (nchains > 1) {
			for (c in 2:nchains) {
				for (i in 1:numClusters) {
					mu.gamma[offset + 1:numOutcome.Grp[i],]$chain = c
					mu.gamma[offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
					mu.gamma[offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
					mu.gamma[offset + 1:numOutcome.Grp[i],]$value = runif(numOutcome.Grp[i], -10, 10)
					sigma2.gamma[offset + 1:numOutcome.Grp[i],]$chain = c
					sigma2.gamma[offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
					sigma2.gamma[offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
					sigma2.gamma[offset + 1:numOutcome.Grp[i],]$value = runif(numOutcome.Grp[i], 5, 20)
	
					offset = offset + numOutcome.Grp[i]
				}
			}
		}

		mu.theta <- list()
		sigma2.theta <- list()
		pi <- list()

		for (t in 1:length(comparator.groups)) {
			mu.theta[[t]] <- data.frame(chain = numeric(nchains*sz), Cluster = character(nchains*sz),
											Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)
			sigma2.theta[[t]] <- data.frame(chain = numeric(nchains*sz), Cluster = character(nchains*sz),
											Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)
			pi[[t]] <- data.frame(chain = numeric(nchains*sz), Cluster = character(nchains*sz),
											Outcome.Grp = character(nchains*sz),
											value = numeric(nchains*sz), stringsAsFactors = FALSE)
			offset = 0
			for (i in 1:numClusters) {
				mu.theta[[t]][offset + 1:numOutcome.Grp[i],]$chain = 1
				mu.theta[[t]][offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
				mu.theta[[t]][offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
				mu.theta[[t]][offset + 1:numOutcome.Grp[i],]$value = 0
				sigma2.theta[[t]][offset + 1:numOutcome.Grp[i],]$chain = 1
				sigma2.theta[[t]][offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
				sigma2.theta[[t]][offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
				sigma2.theta[[t]][offset + 1:numOutcome.Grp[i],]$value = 0

				pi[[t]][offset + 1:numOutcome.Grp[i],]$chain = 1
				pi[[t]][offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
				pi[[t]][offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
				pi[[t]][offset + 1:numOutcome.Grp[i],]$value = 0.5

				offset = offset + numOutcome.Grp[i]
			}

			if (nchains > 1) {
				for (c in 2:nchains) {
					for (i in 1:numClusters) {
						mu.theta[[t]][offset + 1:numOutcome.Grp[i],]$chain = c
						mu.theta[[t]][offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
						mu.theta[[t]][offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
						mu.theta[[t]][offset + 1:numOutcome.Grp[i],]$value = runif(numOutcome.Grp[i], -10, 10)
						sigma2.theta[[t]][offset + 1:numOutcome.Grp[i],]$chain = c
						sigma2.theta[[t]][offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
						sigma2.theta[[t]][offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
						sigma2.theta[[t]][offset + 1:numOutcome.Grp[i],]$value = runif(numOutcome.Grp[i], 5, 20)
	
						pi[[t]][offset + 1:numOutcome.Grp[i],]$chain = c
						pi[[t]][offset + 1:numOutcome.Grp[i],]$Cluster = Clusters[i]
						pi[[t]][offset + 1:numOutcome.Grp[i],]$Outcome.Grp = Outcome.Grp[i, 1:numOutcome.Grp[i]]
						pi[[t]][offset + 1:numOutcome.Grp[i],]$value = runif(numOutcome.Grp[i], 0, 1)
	
						offset = offset + numOutcome.Grp[i]
					}
				}
			}
		}
	}

	if (level == 0) {
		mu.gamma.0 <- data.frame(chain = numeric(nchains*numClusters), Cluster = character(nchains*numClusters),
                                            value = numeric(nchains*numClusters), stringsAsFactors = FALSE)
		tau2.gamma.0 <- data.frame(chain = numeric(nchains*numClusters), Cluster = character(nchains*numClusters),
                                            value = numeric(nchains*numClusters), stringsAsFactors = FALSE)

		for (i in 1:numClusters) {
			mu.gamma.0[i,]$chain = 1
			mu.gamma.0[i,]$Cluster = Clusters[i]
			mu.gamma.0[i,]$value = 0

			tau2.gamma.0[i,]$chain = 1
			tau2.gamma.0[i,]$Cluster = Clusters[i]
			tau2.gamma.0[i,]$value = 0
		}
	
		if (nchains > 1) {
			for (c in 2:nchains) {
				for (i in 1:numClusters) {
					mu.gamma.0[numClusters * (c - 1) + i,]$chain = c
					mu.gamma.0[numClusters * (c - 1) + i,]$Cluster = Clusters[i]
					mu.gamma.0[numClusters * (c - 1) + i,]$value = runif(1, -10, 10)
	
					tau2.gamma.0[numClusters * (c - 1) + i,]$chain = c
					tau2.gamma.0[numClusters * (c - 1) + i,]$Cluster = Clusters[i]
					tau2.gamma.0[numClusters * (c - 1) + i,]$value = runif(1, 5, 20)
				}
			}
		}

		mu.theta.0 <- list()
		tau2.theta.0 <- list()
		alpha.pi <- list()
		beta.pi <- list()

		for (t in 1:length(comparator.groups)) {

			mu.theta.0[[t]] <- data.frame(chain = numeric(nchains*numClusters), Cluster = character(nchains*numClusters),
                                            value = numeric(nchains*numClusters), stringsAsFactors = FALSE)
			tau2.theta.0[[t]] <- data.frame(chain = numeric(nchains*numClusters), Cluster = character(nchains*numClusters),
                                            value = numeric(nchains*numClusters), stringsAsFactors = FALSE)

			alpha.pi[[t]] <- data.frame(chain = numeric(nchains*numClusters), Cluster = character(nchains*numClusters),
                                            value = numeric(nchains*numClusters), stringsAsFactors = FALSE)
			beta.pi[[t]] <- data.frame(chain = numeric(nchains*numClusters), Cluster = character(nchains*numClusters),
                                            value = numeric(nchains*numClusters), stringsAsFactors = FALSE)

			for (i in 1:numClusters) {
				mu.theta.0[[t]][i,]$chain = 1
				mu.theta.0[[t]][i,]$Cluster = Clusters[i]
				mu.theta.0[[t]][i,]$value = 0

				tau2.theta.0[[t]][i,]$chain = 1
				tau2.theta.0[[t]][i,]$Cluster = Clusters[i]
				tau2.theta.0[[t]][i,]$value = 0

				alpha.pi[[t]][i,]$chain = 1
				alpha.pi[[t]][i,]$Cluster = Clusters[i]
				alpha.pi[[t]][i,]$value = 1.5
				beta.pi[[t]][i,]$chain = 1
				beta.pi[[t]][i,]$Cluster = Clusters[i]
				beta.pi[[t]][i,]$value = 1.5
			}
	
			if (nchains > 1) {
				for (c in 2:nchains) {
					for (i in 1:numClusters) {
						mu.theta.0[[t]][numClusters * (c - 1) + i,]$chain = c
						mu.theta.0[[t]][numClusters * (c - 1) + i,]$Cluster = Clusters[i]
						mu.theta.0[[t]][numClusters * (c - 1) + i,]$value = runif(1, -10, 10)
	
						tau2.theta.0[[t]][numClusters * (c - 1) + i,]$chain = c
						tau2.theta.0[[t]][numClusters * (c - 1) + i,]$Cluster = Clusters[i]
						tau2.theta.0[[t]][numClusters * (c - 1) + i,]$value = runif(1, 5, 20)
	
						alpha.pi[[t]][numClusters * (c - 1) + i,]$chain = c
						alpha.pi[[t]][numClusters * (c - 1) + i,]$Cluster = Clusters[i]
						alpha.pi[[t]][numClusters * (c - 1) + i,]$value = runif(1, 1.25, 100)
						beta.pi[[t]][numClusters * (c - 1) + i,]$chain = c
						beta.pi[[t]][numClusters * (c - 1) + i,]$Cluster = Clusters[i]
						beta.pi[[t]][numClusters * (c - 1) + i,]$value = runif(1, 1.25, 100)
					}
				}
			}
		}
	}
	else {
		# level 1, 2
		mu.gamma.0 = rep(0, nchains)
		tau2.gamma.0 = rep(10, nchains)

		if (nchains > 1) {
			for (c in 2:nchains) {
				mu.gamma.0[c] = runif(1, -10, 10)
				tau2.gamma.0[c] = runif(1, 5, 20)
			}
		}

		mu.theta.0 <- list()
		tau2.theta.0 <- list()
		alpha.pi = list()
		beta.pi = list()

		for (t in 1:length(comparator.groups)) {
			mu.theta.0[[t]] = rep(0, nchains)
			tau2.theta.0[[t]] = rep(10, nchains)


			alpha.pi[[t]] = rep(1.5, nchains)
			beta.pi[[t]] = rep(1.5, nchains)

			if (nchains > 1) {
				for (c in 2:nchains) {
					mu.theta.0[[t]][c] = runif(1, -10, 10)
					tau2.theta.0[[t]][c] = runif(1, 5, 20)
					alpha.pi[[t]][c] = runif(1, 1.25, 100)
					beta.pi[[t]][c] = runif(1, 1.25, 100)
				}
			}
		}
	}

	initial_values = list(gamma = gamma, theta = theta, mu.gamma = mu.gamma,
						mu.theta = mu.theta, sigma2.gamma = sigma2.gamma,
						sigma2.theta = sigma2.theta)

	if (model == "BB") {
		bb2_init = list(pi = pi)
		initial_values = c(initial_values, bb2_init)
	}

	if (hier == 3) {
		h3_init = list(mu.gamma.0 = mu.gamma.0, mu.theta.0 = mu.theta.0,
						tau2.gamma.0 = tau2.gamma.0,
                        tau2.theta.0 = tau2.theta.0)
		initial_values = c(initial_values, h3_init)
		if (model == "BB") {
			bb3_init = list(alpha.pi = alpha.pi, beta.pi = beta.pi)
			initial_values = c(initial_values, bb3_init)
		}
	}

	initial_values
}
