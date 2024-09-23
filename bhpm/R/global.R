# Global functions etc

M_global <- new.env()

M_global$logit <- function (p) {
	return (log(p/(1-p)))
}

M_global$checkCols <- function(cols, table) {

	for (i in 1:length(cols)) {

		if (!(cols[i] %in% colnames(table))) {
			message(sprintf("Missing %s column", cols[i]));
			return(1)
		}
	}
	0
}

M_global$checkNames <- function(n, table) {

	for (i in 1:length(n)) {

		if (!(n[i] %in% names(table))) {
			message(sprintf("Missing %s data", n[i]));
			return(1)
		}
	}
	0
}

M_global$CLUSTERdata <- function(M_env, cluster.data, iter, nchains, burnin, initial_values) {

	if (iter <= burnin || nchains < 1 || iter < 0 || burnin < 0) {
		message("Invalid simulation setup parametetrs");
		return(NULL)
	}

	M_env$initVars()

	if (is.character(cluster.data)) {
		file = cluster.data
		cluster.data <- read.table(file, header=TRUE, stringsAsFactors = FALSE)
	}

	# Perform some validation checks
	if (is.null(cluster.data)) {
		message("NULL cluster data")
		return(NULL)
	}

	if ((is.null(nrow(cluster.data))) || (nrow(cluster.data) == 0)) {
		message("Missing cluster data set");
		return(NULL)
	}

	# Check the correct columns are defined
	cols = c("Outcome.Grp", "Outcome", "Count", "Trt.Grp", "Cluster", "Exposure")
	if (M_global$checkCols(cols, cluster.data)) {
		message("Missing columns");
		return(NULL)
	}

	# Convert any factor columns to character (not needed if the data is in a file but if a data.frame
	# is passed to the function then we need to convert to strings)
	facs <- sapply(cluster.data, is.factor)
	cluster.data[facs] <- sapply(cluster.data[facs], as.character)

	# Order by cluster, outcome group, outcome, and treatment group
	ordered.data <- cluster.data[order(cluster.data$Cluster, cluster.data$Outcome.Grp, cluster.data$Outcome, cluster.data$Trt.Grp),, drop=FALSE]

	treatment.groups <- unique(ordered.data$Trt.Grp)
	cntrl.group = min(treatment.groups)
	comparator.groups = treatment.groups[treatment.groups > cntrl.group]

	cntrl.data <- ordered.data[ordered.data$Trt.Grp == cntrl.group, ]
	td <- ordered.data[ordered.data$Trt.Grp > cntrl.group, ]
	treat.data <- split(td, f = td$Trt.Grp)

	# Check that cntrl, treat data are matched 1 to 1
	# Check that we have matching outcome groups, outcomes and clusters in the control and
	# treatment groups
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

	# Size the data structures
	
	# At this stage the groups are matched exactly by Cluster, Outcome.Grp, and Outcome
	# However it is possible that not all Outcome.Grp's or Outcome's will appear
	# in all groups

	M_env$nTreatments = length(treatment.groups)
	M_env$Trt.Grps = data.frame(Trt.Grp = treatment.groups, param = c("gamma", rep("theta",  M_env$nTreatments - 1)))
	M_env$Clusters = unique(cntrl.data$Cluster)
	M_env$numClusters = length(M_env$Clusters)
	M_env$numOutcome.Grp = c()
	for (i in 1:M_env$numClusters) {
		c = cntrl.data[cntrl.data$Cluster == M_env$Clusters[i],]
		b = unique(c$Outcome.Grp)
		M_env$numOutcome.Grp[i] = length(b)
	}
	M_env$maxOutcome.Grps = max(M_env$numOutcome.Grp)
	M_env$Outcome.Grp = array(NA, dim = c(M_env$numClusters, M_env$maxOutcome.Grps))
	for (i in 1:M_env$numClusters) {
		c = cntrl.data[cntrl.data$Cluster == M_env$Clusters[i],]
		b = unique(c$Outcome.Grp)
		M_env$Outcome.Grp[i, 1:length(b)] = b
	}

	# Outcomes
	M_env$nOutcome = array(0, dim = c(M_env$numClusters, M_env$maxOutcome.Grps))
	for (i in 1:M_env$numClusters) {
		numOutcome.Grp = M_env$numOutcome.Grp[i]
		for (b in 1:numOutcome.Grp) {
			M_env$nOutcome[i, b] = length(unique(cntrl.data[cntrl.data$Outcome.Grp == M_env$Outcome.Grp[i, b], ]$Outcome))
		}
	}

	maxOutcomes <- max(M_env$nOutcome)
	M_env$maxOutcomes <- maxOutcomes
	M_env$Outcome = array(NA, dim = c(M_env$numClusters, M_env$maxOutcome.Grps, maxOutcomes))
	for (i in 1:M_env$numClusters) {
		numOutcome.Grp = M_env$numOutcome.Grp[i]
		for (b in 1:numOutcome.Grp) {
			c = cntrl.data[cntrl.data$Cluster == M_env$Clusters[i] & cntrl.data$Outcome.Grp == M_env$Outcome.Grp[i, b],]
			# Do we need a unique here - each outcome should only appear once in a outcome group
			# in an cluster - Let's add a check. Probably need to add it to bhpm.BB
			# etc. as well.
			if (length(unique(c$Outcome)) != length(c$Outcome)) {
				message("Multipe outcomes in outcome group");
				return(NULL)
			}

			M_env$Outcome[i, b,1:length(c$Outcome)] = c$Outcome
		}
	}

	# Cluster Data
	# Control: counts and exposure
	M_env$x = array(NA, dim = c(M_env$numClusters, M_env$maxOutcome.Grps, maxOutcomes))
	M_env$C <- array(NA, dim = c(M_env$numClusters, M_env$maxOutcome.Grps, maxOutcomes))

	# Treatment: counts and exposure
	M_env$y = array(NA, dim = c(M_env$nTreatments - 1, M_env$numClusters, M_env$maxOutcome.Grps, maxOutcomes))
	M_env$T <- array(NA, dim = c(M_env$nTreatments - 1, M_env$numClusters, M_env$maxOutcome.Grps, maxOutcomes))

	for (i in 1:M_env$numClusters) {
		numOutcome.Grp = M_env$numOutcome.Grp[i]
		for (b in 1:numOutcome.Grp) {
			control = cntrl.data[cntrl.data$Cluster == M_env$Clusters[i] & cntrl.data$Outcome.Grp == M_env$Outcome.Grp[i, b], ]
			M_env$x[i, b, 1:length(control$Count)] <- control$Count
			M_env$C[i, b, 1:length(control$Count)] <- control$Exposure

			for (t in 1:length(comparator.groups)) {
				t.d = treat.data[[t]]
				treatment = t.d[t.d$Cluster == M_env$Clusters[i] & t.d$Outcome.Grp == M_env$Outcome.Grp[i, b], ]
				M_env$y[t, i, b, 1:length(control$Count)] <- treatment$Count
				M_env$T[t, i, b, 1:length(control$Count)] <- treatment$Exposure
			}
		}
	}

	M_env$initialiseChains(initial_values, nchains)

	out = list(cluster.data = cluster.data, cntrl.data = cntrl.data, treat.data = treat.data)

	return(out)
}

# Check that all clusters contain the same outcome groups
# We only need to check the control data as, from above, the treatment data has the exact same structure
# This check is only used in level 1 models
M_global$checkBS <- function(M_env, cntrl.data) {

	c1 = cntrl.data[cntrl.data$Cluster == M_env$Clusters[1],]
	b1 = c1$Outcome.Grp
	if (M_env$numClusters > 1) {
		for (i in 2:M_env$numClusters) {
			c = cntrl.data[cntrl.data$Cluster == M_env$Clusters[i],]
			b = c$Outcome.Grp

			if (!identical(b1, b)) {
				message("Clusters contain different outcome groups");
				return(1)
			}
		}
	}
	0
}

M_global$CLUSTER_sim_params1a <- function(M_env, sim.params, sim_type, cluster.data, cntrl.data) {

	# Have any of the global simulation parameters been overridden
	if (!is.null(sim.params)) {
		# 1. Check if we have a full set for the simulation type
		sim.params = sim.params[sim.params$type == sim_type, ]
		sim.params <- sim.params[order(sim.params$Cluster, sim.params$Outcome.Grp, sim.params$Outcome, sim.params$Trt.Grp),,drop=FALSE]
		gamma.params = sim.params[sim.params$variable == "gamma",]
		theta.params = sim.params[sim.params$variable == "theta",]

		# Check full set of parameters
		full.set <- TRUE

		if (!identical(cntrl.data$Outcome.Grp, gamma.params$Outcome.Grp) ||
			!identical(cntrl.data$Outcome, gamma.params$Outcome) ||
			!identical(cntrl.data$Cluster, gamma.params$Cluster)) {
			full.set <- FALSE
		}

		comp.grps = unique(cluster.data$Trt.Grp)
		comp.grps <- comp.grps[comp.grps > 1]

		for (i in 1:length(comp.grps)) {
			t.p = theta.params[theta.params$Trt.Grp == comp.grps[i], ]

			if (!identical(cntrl.data$Outcome.Grp, t.p$Outcome.Grp) ||
			!identical(cntrl.data$Outcome, t.p$Outcome) || 
			!identical(cntrl.data$Cluster, t.p$Cluster)) {
				full.set <- FALSE
			}
		}

		if (full.set == FALSE) {
			# If we don't have a full set then we need to merge the values
			params = bhpm.sim.control.params(cluster.data, "1a")
			params = params[params$type == sim_type, ]

			params = merge(params, sim.params, by = c("type", "variable", "Cluster", "Outcome.Grp", "Outcome", "param", "Trt.Grp"), all.x = T)

			params = params[, !(names(params) %in% c("value.x", "control.x"))]
			names(params)[names(params) == "value.y"] = "value"
			names(params)[names(params) == "control.y"] = "control"
			params <- params[order(params$Cluster, params$Outcome.Grp, params$Outcome, params$Trt.Grp),, drop=FALSE]

			sim.params = params
		}

		# Add in the indices for clusters, outcome groups and outcomes
		C_index = c()
		B = c()
		j = c()
		Group = c()

		x = apply(M_env$nOutcome, 1, sum)
		C_index = rep(1:M_env$numClusters, x)
		d = cbind(M_env$nOutcome, M_env$numOutcome.Grp)
		B = as.vector(apply(d, 1, function(x) { n = length(x); rep(1:x[n], x[1:x[n]])})) # Should handle missing BS's - maybe
		j = as.vector(apply(M_env$nOutcome,1, sequence))

		C_index = as.integer(C_index)
		B = as.integer(B)
		j = as.integer(j)

		gamma.params = sim.params[sim.params$variable == "gamma",]
		theta.params = sim.params[sim.params$variable == "theta",]

		gamma.params = gamma.params[, !(names(gamma.params) %in% c("Cluster", "C_index","Outcome.Grp", "Outcome"))]
		gamma.params = cbind(gamma.params, C_index)
		gamma.params = cbind(gamma.params, B)
		gamma.params = cbind(gamma.params, j)

		gamma.params$Trt.Grp = as.integer(0)

		Group = rep(1:length(comp.grps), sum(x))
		Group = as.integer(Group)

		theta.params = theta.params[, !(names(theta.params) %in% c("Cluster", "C_index","Outcome.Grp", "Outcome"))]
		theta.params = cbind(theta.params, C_index = rep(C_index, each = length(comp.grps)))
		theta.params = cbind(theta.params, B = rep(B, each = length(comp.grps)))
		theta.params = cbind(theta.params, j = rep(j, each = length(comp.grps)))
		theta.params$Trt.Grp = as.integer(Group)

		sim.params = rbind(gamma.params, theta.params)
		sim.params = sim.params[!is.na(sim.params$value), ]

		sim.params$value = as.numeric(sim.params$value)
		sim.params$control = as.numeric(sim.params$control)
	}

	return(sim.params)
}

M_global$CLUSTER_sim_paramsBB_2 <- function(M_env, sim.params, pm.weights, sim_type, cluster.data, cntrl.data) {

	# Have any of the global parameters been overridden?
	# 1. Simulation parameters
	# 2. Point mass weights

	# Indices for changed parameters
	C_index = c()
	B = c()
	j = c()

	x = apply(M_env$nOutcome, 1, sum)
	C_index = rep(1:M_env$numClusters, x)
	d = cbind(M_env$nOutcome, M_env$numOutcome.Grp)
	B = as.vector(apply(d, 1, function(x) { n = length(x); rep(1:x[n], x[1:x[n]])})) # Should handle missing BS's - maybe
	j = as.vector(apply(M_env$nOutcome,1, sequence))

	C_index = as.integer(C_index)
	B = as.integer(B)
	j = as.integer(j)

	comp.grps = unique(cluster.data$Trt.Grp)
	comp.grps <- comp.grps[comp.grps > 1]
	Group = rep(1:length(comp.grps), sum(x))
	Group = as.integer(Group)

	if ((!is.null(sim.params)) && (nrow(sim.params) > 0)) {

		# 1. Check if we have a full set for the simulation type
		sim.params <- sim.params[order(sim.params$Cluster, sim.params$Outcome.Grp, sim.params$Outcome, sim.params$Trt.Grp),,drop=FALSE]
		sim.params = sim.params[sim.params$type == sim_type | sim.params$variable == "theta", ]

		if (nrow(sim.params) > 0) {

			gamma.params = sim.params[sim.params$variable == "gamma",]
			theta.params = sim.params[sim.params$variable == "theta",]

			# Check full set of parameters
			full.set <- TRUE

			if (!identical(cntrl.data$Outcome.Grp, gamma.params$Outcome.Grp) ||
				!identical(cntrl.data$Outcome, gamma.params$Outcome) ||
				!identical(cntrl.data$Cluster, gamma.params$Cluster)) {
				full.set <- FALSE
			}

			for (i in 1:length(comp.grps)) {
				t.p = theta.params[theta.params$Trt.Grp == comp.grps[i], ]

				if (!identical(cntrl.data$Outcome.Grp, t.p$Outcome.Grp) ||
					!identical(cntrl.data$Outcome, t.p$Outcome) || 
					!identical(cntrl.data$Cluster, t.p$Cluster)) {
					full.set <- FALSE
				}
			}

			if (full.set == FALSE) {
				# If we don't have a full set then we need to merge the values
				params = bhpm.sim.control.params(cluster.data, "BB")
				params = params[params$variable == "theta" | params$variable == "gamma",]

				params = params[params$type == sim_type | params$variable == "theta", ]

				params = merge(params, sim.params, by = c("type", "variable", "Cluster", "Outcome.Grp", "Outcome", "param", "Trt.Grp"), all.x = T)

				params = params[, !(names(params) %in% c("value.x", "control.x"))]
				names(params)[names(params) == "value.y"] = "value"
				names(params)[names(params) == "control.y"] = "control"
				params <- params[order(params$Cluster, params$Outcome.Grp, params$Outcome, params$Trt.Grp),, drop=FALSE]
	
				sim.params = params
			}

			# Add in the indices for clusters, outcome groups and outcomes

			gamma.params = sim.params[sim.params$variable == "gamma",]
			theta.params = sim.params[sim.params$variable == "theta",]

			gamma.params = gamma.params[, !(names(gamma.params) %in% c("Cluster", "C_index","Outcome.Grp", "Outcome"))]
			gamma.params = cbind(gamma.params, C_index)
			gamma.params = cbind(gamma.params, B)
			gamma.params = cbind(gamma.params, j)

			gamma.params$Trt.Grp <- as.integer(0)

			theta.params = theta.params[, !(names(theta.params) %in% c("Cluster", "C_index","Outcome.Grp", "Outcome"))]
			theta.params = cbind(theta.params, C_index = rep(C_index, each = length(comp.grps)))
			theta.params = cbind(theta.params, B = rep(B, each = length(comp.grps)))
			theta.params = cbind(theta.params, j =  rep(j, each = length(comp.grps)))

			theta.params$Trt.Grp <- as.integer(Group)

			sim.params = rbind(gamma.params, theta.params)
			sim.params = sim.params[!is.na(sim.params$value), ]

			sim.params$value = as.numeric(sim.params$value)
			sim.params$control = as.numeric(sim.params$control)
		}
	}

	pm.weights <- M_global$CLUSTER_pm_weights(M_env, pm.weights, cluster.data, cntrl.data, C_index, B, j, comp.grps, Group)

	out = list(sim.params = sim.params, pm.weights = pm.weights)

	return(out)
}

M_global$CLUSTER_sim_paramsBB_3 <- function(M_env, sim.params, pm.weights, sim_type, cluster.data, cntrl.data) {

	# Have any of the global parameters been overridden?
	# 1. Simulation parameters
	# 2. Point mass weights

	# Indices for changed parameters
	C_index = c()
	B = c()
	j = c()

	x = apply(M_env$nOutcome, 1, sum)
	C_index = rep(1:M_env$numClusters, x)
	d = cbind(M_env$nOutcome, M_env$numOutcome.Grp)
	B = as.vector(apply(d, 1, function(x) { n = length(x); rep(1:x[n], x[1:x[n]])})) # Should handle missing BS's - maybe
	j = as.vector(apply(M_env$nOutcome,1, sequence))

	C_index = as.integer(C_index)
	B = as.integer(B)
	j = as.integer(j)

	comp.grps = unique(cluster.data$Trt.Grp)
	comp.grps <- comp.grps[comp.grps > 1]
	Group = rep(1:length(comp.grps), sum(x))
	Group = as.integer(Group)

	if ((!is.null(sim.params)) && (nrow(sim.params) > 0)) {

		# 1. Check if we have a full set for the simulation type
		sim.params <- sim.params[order(sim.params$Cluster, sim.params$Outcome.Grp, sim.params$Outcome, sim.params$Trt.Grp), ,drop=FALSE]
		sim.params = sim.params[sim.params$type == sim_type | sim.params$variable == "theta", ]

		if (nrow(sim.params) > 0) {

			gamma.params = sim.params[sim.params$variable == "gamma",]
			theta.params = sim.params[sim.params$variable == "theta",]
			alpha.params = sim.params[sim.params$variable == "alpha",]
			beta.params = sim.params[sim.params$variable == "beta",]

			if (nrow(alpha.params) > 0) {
				alpha.params$Outcome.Grp <- ""
				alpha.params$Outcome <- ""
			}
			if (nrow(beta.params) > 0) {
				beta.params$Outcome.Grp <- ""
				beta.params$Outcome <- ""
			}

			# Check full set of parameters
			full.set <- TRUE

			if (!identical(cntrl.data$Outcome.Grp, gamma.params$Outcome.Grp) ||
				!identical(cntrl.data$Outcome, gamma.params$Outcome) ||
				!identical(cntrl.data$Cluster, gamma.params$Cluster)) {
				full.set <- FALSE
			}

			for (i in 1:length(comp.grps)) {
				t.p = theta.params[theta.params$Trt.Grp == comp.grps[i], ]
				a.p = alpha.params[alpha.params$Trt.Grp == comp.grps[i], ]
				b.p = beta.params[beta.params$Trt.Grp == comp.grps[i], ]

				if (!identical(cntrl.data$Outcome.Grp, t.p$Outcome.Grp) ||
					!identical(cntrl.data$Outcome, t.p$Outcome) || 
					!identical(cntrl.data$Cluster, t.p$Cluster) ||
					!identical(cntrl.data$Cluster, a.p$Cluster) ||
					!identical(cntrl.data$Cluster, b.p$Cluster)) {
					full.set <- FALSE
				}
			}

			if (full.set == FALSE) {
				# If we don't have a full set then we need to merge the values
				params = bhpm.sim.control.params(cluster.data, "BB")

				params = params[params$type == sim_type | params$variable == "theta", ]

				params = merge(params, sim.params, by = c("type", "variable", "Cluster", "Outcome.Grp", "Outcome", "param", "Trt.Grp"), all.x = T)

				params = params[, !(names(params) %in% c("value.x", "control.x"))]
				names(params)[names(params) == "value.y"] = "value"
				names(params)[names(params) == "control.y"] = "control"
				params <- params[order(params$Cluster, params$Outcome.Grp, params$Outcome, params$Trt.Grp),, drop=FALSE]
	
				sim.params = params
			}

			# Add in the indices for clusters, outcome groups and outcomes

			gamma.params = sim.params[sim.params$variable == "gamma",]
			theta.params = sim.params[sim.params$variable == "theta",]
			alpha.params = sim.params[sim.params$variable == "alpha",]
			beta.params = sim.params[sim.params$variable == "beta",]

			gamma.params = gamma.params[, !(names(gamma.params) %in% c("Cluster", "C_index","Outcome.Grp", "Outcome"))]
			gamma.params = cbind(gamma.params, C_index)
			gamma.params = cbind(gamma.params, B)
			gamma.params = cbind(gamma.params, j)

			gamma.params$Trt.Grp = as.integer(0)

			theta.params = theta.params[, !(names(theta.params) %in% c("Cluster", "C_index","Outcome.Grp", "Outcome"))]

			theta.params = cbind(theta.params, C_index = rep(C_index, each = length(comp.grps)))
			theta.params = cbind(theta.params, B = rep(B, each = length(comp.grps)))
			theta.params = cbind(theta.params, j = rep(j, each = length(comp.grps)))

			theta.params$Trt.Grp = as.integer(Group)


			C_index_lev3 = rep(1:M_env$numClusters,
								rep(length(comp.grps), M_env$numClusters))
			Group_lev3 = rep(1:length(comp.grps), M_env$numClusters)

			alpha.params = alpha.params[, !(names(alpha.params) %in% c("Cluster", "C_index","Outcome.Grp", "Outcome"))]
			alpha.params = cbind(alpha.params, C_index = as.integer(C_index_lev3))
			alpha.params = cbind(alpha.params, B = as.integer(0))
			alpha.params = cbind(alpha.params, j = as.integer(0))
			alpha.params$B <- NA
			alpha.params$j <- NA

			alpha.params$Trt.Grp = as.integer(Group_lev3)

			beta.params = beta.params[, !(names(beta.params) %in% c("Cluster", "C_index","Outcome.Grp", "Outcome"))]
			beta.params = cbind(beta.params, C_index = as.integer(C_index_lev3))
			beta.params = cbind(beta.params, B = as.integer(0))
			beta.params = cbind(beta.params, j = as.integer(0))
			beta.params$B <- NA
			beta.params$j <- NA

			beta.params$Trt.Grp = as.integer(Group_lev3)

			sim.params = rbind(gamma.params, theta.params, alpha.params, beta.params)
			sim.params = sim.params[!is.na(sim.params$value), ]

			sim.params$value = as.numeric(sim.params$value)
			sim.params$control = as.numeric(sim.params$control)
		}
	}

	pm.weights <- M_global$CLUSTER_pm_weights(M_env, pm.weights, cluster.data, cntrl.data, C_index, B, j, comp.grps, Group)

	out = list(sim.params = sim.params, pm.weights = pm.weights)

	return(out)
}

M_global$CLUSTER_pm_weights <- function(M_env, pm.weights, cluster.data, cntrl.data, C_index, B, j, comp.grps, Group) {

	# Have any of the default point-mass weights been overridden
	if ((!is.null(pm.weights)) && (nrow(pm.weights) > 0)) {

		pm.weights <- pm.weights[order(pm.weights$Cluster, pm.weights$Outcome.Grp, pm.weights$Outcome, pm.weights$Trt.Grp),,drop=FALSE]

		if (!identical(cntrl.data$Outcome.Grp, pm.weights$Outcome.Grp) ||
			!identical(cntrl.data$Outcome, pm.weights$Outcome)||
			!identical(cntrl.data$Trt.Grp, pm.weights$Trt.Grp)) {
			# If we don't have a full set then we need to merge the values

			w = bhpm.pointmass.weights(cluster.data)

			w = merge(w, pm.weights, by = c("Cluster", "Outcome.Grp", "Outcome", "Trt.Grp"), all.x = T)

			w = w[, !(names(w) %in% c("weight_pm.x"))]
			names(w)[names(w) == "weight_pm.y"] = "weight_pm"
			w <- w[order(w$Cluster, w$Outcome.Grp, w$Outcome, w$Trt.Grp),, drop=FALSE]

			pm.weights = w
		}

		pm.weights = pm.weights[, !(names(pm.weights) %in% c("Cluster", "C_index", "Outcome.Grp", "Outcome")), drop = FALSE]

		pm.weights = cbind(pm.weights, C_index = rep(C_index, each = length(comp.grps)))
		pm.weights = cbind(pm.weights, B = rep(B, each = length(comp.grps)))
		pm.weights = cbind(pm.weights, j = rep(j, each = length(comp.grps)))

		pm.weights$Trt.Grp = as.integer(Group)

		pm.weights = pm.weights[!is.na(pm.weights$weight_pm), ]
	}

	return(pm.weights)
}

M_global$CLUSTER_monitor_1a_2 <- function(monitor) {

	# monitor null means monitor all variables
	if (is.null(monitor)) {
		v = c("theta", "gamma", "mu.gamma", "mu.theta", "sigma2.theta", "sigma2.gamma")
		s = rep(1, length(v))
		monitor = data.frame(variable = v, monitor = s, stringsAsFactors = FALSE)
	}
	else {
		# Does monitor include values for all possible variables?
		if (nrow(monitor[monitor$variable == "theta", ]) == 0) {
			monitor = rbind(monitor, c("theta", 0))
		}
		if (nrow(monitor[monitor$variable == "gamma", ]) == 0) {
			monitor = rbind(monitor, c("gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.theta", ]) == 0) {
			monitor = rbind(monitor, c("mu.theta", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.gamma", ]) == 0) {
			monitor = rbind(monitor, c("mu.gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "sigma2.theta", ]) == 0) {
			monitor = rbind(monitor, c("sigma2.theta", 0))
		}
		if (nrow(monitor[monitor$variable == "sigma2.gamma", ]) == 0) {
			monitor = rbind(monitor, c("sigma2.gamma", 0))
		}
	}
	# Coerce any non-correct type
	monitor$monitor = as.integer(monitor$monitor)

	return(monitor)
}

M_global$CLUSTER_monitor_1a_3 <- function(monitor) {

	# monitor null means monitor all variables
	if (is.null(monitor)) {
		v = c("theta", "gamma", "mu.gamma", "mu.theta", "sigma2.theta", "sigma2.gamma",
				"mu.theta.0", "mu.gamma.0", "tau2.theta.0", "tau2.gamma.0")
		s = rep(1, length(v))
		monitor = data.frame(variable = v, monitor = s, stringsAsFactors = FALSE)
	}
	else {
		# Does monitor include values for all possible variables?
		if (nrow(monitor[monitor$variable == "theta", ]) == 0) {
			monitor = rbind(monitor, c("theta", 0))
		}
		if (nrow(monitor[monitor$variable == "gamma", ]) == 0) {
			monitor = rbind(monitor, c("gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.theta", ]) == 0) {
			monitor = rbind(monitor, c("mu.theta", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.gamma", ]) == 0) {
			monitor = rbind(monitor, c("mu.gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "sigma2.theta", ]) == 0) {
			monitor = rbind(monitor, c("sigma2.theta", 0))
		}
		if (nrow(monitor[monitor$variable == "sigma2.gamma", ]) == 0) {
			monitor = rbind(monitor, c("sigma2.gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.theta.0", ]) == 0) {
			monitor = rbind(monitor, c("mu.theta.0", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.gamma.0", ]) == 0) {
			monitor = rbind(monitor, c("mu.gamma.0", 0))
		}
		if (nrow(monitor[monitor$variable == "tau2.gamma.0", ]) == 0) {
			monitor = rbind(monitor, c("tau2.gamma.0", 0))
		}
		if (nrow(monitor[monitor$variable == "tau2.theta.0", ]) == 0) {
			monitor = rbind(monitor, c("tau2.theta.0", 0))
		}
	}
	# Coerce any non-correct type
	monitor$monitor = as.integer(monitor$monitor)

	return(monitor)
}

M_global$CLUSTER_monitor_BB_2 <- function(monitor) {

	# monitor null means monitor all variables
	if (is.null(monitor)) {
		v = c("theta", "gamma", "mu.gamma", "mu.theta", "sigma2.theta", "sigma2.gamma",
				"pi")
		s = rep(1, length(v))
		monitor = data.frame(variable = v, monitor = s, stringsAsFactors = FALSE)
	}
	else {
		# Does monitor include values for all possible variables?
		if (nrow(monitor[monitor$variable == "theta", ]) == 0) {
			monitor = rbind(monitor, c("theta", 0))
		}
		if (nrow(monitor[monitor$variable == "gamma", ]) == 0) {
			monitor = rbind(monitor, c("gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.theta", ]) == 0) {
			monitor = rbind(monitor, c("mu.theta", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.gamma", ]) == 0) {
			monitor = rbind(monitor, c("mu.gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "sigma2.theta", ]) == 0) {
			monitor = rbind(monitor, c("sigma2.theta", 0))
		}
		if (nrow(monitor[monitor$variable == "sigma2.gamma", ]) == 0) {
			monitor = rbind(monitor, c("sigma2.gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "pi", ]) == 0) {
			monitor = rbind(monitor, c("pi", 0))
		}
	}
	# Coerce any non-correct type
	monitor$monitor = as.integer(monitor$monitor)

	return(monitor)
}

M_global$CLUSTER_monitor_BB_3 <- function(monitor) {

	# monitor null means monitor all variables
	if (is.null(monitor)) {
		v = c("theta", "gamma", "mu.gamma", "mu.theta", "sigma2.theta", "sigma2.gamma",
				"mu.theta.0", "mu.gamma.0", "tau2.theta.0", "tau2.gamma.0",
				"pi", "alpha.pi", "beta.pi")
		s = rep(1, length(v))
		monitor = data.frame(variable = v, monitor = s, stringsAsFactors = FALSE)
	}
	else {
		# Does monitor include values for all possible variables?
		if (nrow(monitor[monitor$variable == "theta", ]) == 0) {
			monitor = rbind(monitor, c("theta", 0))
		}
		if (nrow(monitor[monitor$variable == "gamma", ]) == 0) {
			monitor = rbind(monitor, c("gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.theta", ]) == 0) {
			monitor = rbind(monitor, c("mu.theta", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.gamma", ]) == 0) {
			monitor = rbind(monitor, c("mu.gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "sigma2.theta", ]) == 0) {
			monitor = rbind(monitor, c("sigma2.theta", 0))
		}
		if (nrow(monitor[monitor$variable == "sigma2.gamma", ]) == 0) {
			monitor = rbind(monitor, c("sigma2.gamma", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.theta.0", ]) == 0) {
			monitor = rbind(monitor, c("mu.theta.0", 0))
		}
		if (nrow(monitor[monitor$variable == "mu.gamma.0", ]) == 0) {
			monitor = rbind(monitor, c("mu.gamma.0", 0))
		}
		if (nrow(monitor[monitor$variable == "tau2.gamma.0", ]) == 0) {
			monitor = rbind(monitor, c("tau2.gamma.0", 0))
		}
		if (nrow(monitor[monitor$variable == "tau2.theta.0", ]) == 0) {
			monitor = rbind(monitor, c("tau2.theta.0", 0))
		}
		if (nrow(monitor[monitor$variable == "pi", ]) == 0) {
			monitor = rbind(monitor, c("pi", 0))
		}
		if (nrow(monitor[monitor$variable == "alpha.pi", ]) == 0) {
			monitor = rbind(monitor, c("alpha.pi", 0))
		}
		if (nrow(monitor[monitor$variable == "beta.pi", ]) == 0) {
			monitor = rbind(monitor, c("beta.pi", 0))
		}
	}
	# Coerce any non-correct type
	monitor$monitor = as.integer(monitor$monitor)

	return(monitor)
}

M_global$CLUSTER_check_conv_name_1a_2 <- function(raw) {

	# Check which variables we are monitoring
	monitor = raw$monitor
	theta_mon = monitor[monitor$variable == "theta",]$monitor
	gamma_mon = monitor[monitor$variable == "gamma",]$monitor
	mu.theta_mon = monitor[monitor$variable == "mu.theta",]$monitor
	mu.gamma_mon = monitor[monitor$variable == "mu.gamma",]$monitor
	sigma2.theta_mon = monitor[monitor$variable == "sigma2.theta",]$monitor
	sigma2.gamma_mon = monitor[monitor$variable == "sigma2.gamma",]$monitor

	n = c("chains", "nClusters", "Clusters", "nOutcome.Grp", "maxOutcome.Grps", "maxOutcomes", "nOutcome", "Outcome.Grp", "Outcome", "iter")

	if (theta_mon == 1) {
		n = c(n, "theta")
		if (raw$sim_type == "MH") {
			n = c(n, "theta_acc")
		}
	}
	if (gamma_mon == 1 ) {
		n = c(n, "gamma")
		if (raw$sim_type == "MH") {
			n = c(n, "gamma_acc")
		}
	}
	if (mu.gamma_mon == 1) {
		n = c(n, "mu.gamma")
	}
	if (mu.theta_mon == 1) {
		n = c(n, "mu.theta")
	}
	if (sigma2.theta_mon) {
		n = c(n, "sigma2.theta")
	}
	if (sigma2.gamma_mon) {
		n = c(n, "sigma2.gamma")
	}

	if (M_global$checkNames(n, raw)) {
		message("Missing names");
		return(1)
	}
	0
}

M_global$CLUSTER_check_conv_name_1a_3 <- function(raw) {

	if (M_global$CLUSTER_check_conv_name_1a_2(raw)) {
		return(1)
	}

	monitor = raw$monitor
	mu.theta.0_mon = monitor[monitor$variable == "mu.theta.0",]$monitor
	mu.gamma.0_mon = monitor[monitor$variable == "mu.gamma.0",]$monitor
	tau2.theta.0_mon = monitor[monitor$variable == "tau2.theta.0",]$monitor
	tau2.gamma.0_mon = monitor[monitor$variable == "tau2.gamma.0",]$monitor

	n = c()

    if (mu.gamma.0_mon == 1) {
		n = c(n, "mu.gamma.0")
    }
    if (mu.theta.0_mon == 1) {
		n = c(n, "mu.theta.0")
    }
    if (tau2.gamma.0_mon == 1) {
		n = c(n, "tau2.gamma.0")
    }
    if (tau2.theta.0_mon == 1) {
		n = c(n, "tau2.theta.0")
    }

	if (length(n) > 0) {
		if (M_global$checkNames(n, raw)) {
			return(1)
		}
    }

	0
}

M_global$CLUSTER_check_summ_name_1a_2 <- function(raw) {

	# Check which variables we are monitoring
	monitor = raw$monitor
	theta_mon = monitor[monitor$variable == "theta",]$monitor
	gamma_mon = monitor[monitor$variable == "gamma",]$monitor
	mu.theta_mon = monitor[monitor$variable == "mu.theta",]$monitor
	mu.gamma_mon = monitor[monitor$variable == "mu.gamma",]$monitor
	sigma2.theta_mon = monitor[monitor$variable == "sigma2.theta",]$monitor
	sigma2.gamma_mon = monitor[monitor$variable == "sigma2.gamma",]$monitor

	n = c("chains", "nClusters", "nOutcome.Grp", "maxOutcome.Grps", "maxOutcomes", "nOutcome", "Outcome.Grp", "Outcome", "iter", "burnin")

	if (theta_mon == 1) {
		 n = c(n, "theta")
	}
	if (gamma_mon == 1) {
		 n = c(n, "gamma")
	}
	if (mu.gamma_mon == 1) {
		 n = c(n, "mu.gamma")
	}
	if (theta_mon == 1) {
		 n = c(n, "theta")
	}
	if (mu.theta_mon == 1) {
		 n = c(n, "mu.theta")
	}
	if (sigma2.theta_mon == 1) {
		 n = c(n, "sigma2.theta")
	}
	if (sigma2.gamma_mon == 1) {
		 n = c(n, "sigma2.gamma")
	}

	if (length(n) > 0) {
		if (M_global$checkNames(n, raw)) {
			return(1)
		}
	}

    0
}

M_global$CLUSTER_check_summ_name_1a_3 <- function(raw) {

	if (M_global$CLUSTER_check_summ_name_1a_2(raw)) {
		return(1)
	}

	monitor = raw$monitor
	mu.theta.0_mon = monitor[monitor$variable == "mu.theta.0",]$monitor
	mu.gamma.0_mon = monitor[monitor$variable == "mu.gamma.0",]$monitor
	tau2.theta.0_mon = monitor[monitor$variable == "tau2.theta.0",]$monitor
	tau2.gamma.0_mon = monitor[monitor$variable == "tau2.gamma.0",]$monitor

	n = c()

    if (mu.gamma.0_mon == 1) {
		n = c(n, "mu.gamma.0")
    }
    if (mu.theta.0_mon == 1) {
		n = c(n, "mu.theta.0")
    }
    if (tau2.gamma.0_mon == 1) {
		n = c(n, "tau2.gamma.0")
    }
    if (tau2.theta.0_mon == 1) {
		n = c(n, "tau2.theta.0")
    }

	if (length(n) > 0) {
		if (M_global$checkNames(n, raw)) {
			return(1)
		}
    }

	0
}

M_global$Geweke <- function(x) {
	mcmc_obj <- mcmc(x)
	g <- geweke.diag(mcmc_obj)

	return(g)
}

M_global$GelmanRubin <- function(x, nchains) {

	mcmc_obj <- list(NA)

	x1 = split(x, row(x))
	x2 = lapply(x1, mcmc)
	mlist <- mcmc.list(x2)

	g <- gelman.diag(mlist)

	return(g)
}	

M_global$GelmanRubin_new <- function(x, nchains) {

	mcmc_obj <- list(NA)

	x1 = split(x, row(x))
	x2 = lapply(x1, mcmc)
	mlist <- mcmc.list(x2)

	g <- gelman.diag(mlist)

	return(c(g$psrf[1], g$psrf[2]))
}	

M_global$summaryStats <- function(x, nchains, prob = .95) {

	if (nchains == 1) {
		x = as.matrix(t(x))
	}

	x1 = c(x[1:nchains,])
	m <- mcmc(x1)
	h <- HPDinterval(m, prob)
	m = c(mean(x1), median(x1))

	mcmc_obj <- list(NA)
	x1 = split(x, row(x))
	x2 = lapply(x1, mcmc)
	mlist <- mcmc.list(x2)
	stats = summary(mlist)

	return(c(m[1], m[2], h[1], h[2], stats$statistics["SD"], stats$statistics["Time-series SE"]))
}

chk_val <- function(val, q = 0.975) {
	if (abs(val) > qnorm(q)) {
		return("*")
	}
	else {
		return("-")
	}
}
