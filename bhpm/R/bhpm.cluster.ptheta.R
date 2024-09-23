# bhpm.cluster.BB.ptheta
# Model bhpm.BB
# R. Carragher
# Date: 29/06/2018

Id <- "$Id: bhpm.cluster.ptheta.R,v 1.11 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.cluster.ptheta <- function(raw)
{
	if (is.null(raw)) {
		message("NULL raw data");
		return(NULL)
	}

	model = attr(raw, "model")
	if (is.null(model)) {
		message("Missing model attribute");
		return(NULL)
	}

	if (!("chains" %in% names(raw))) {
		message("Missing chains data");
		return(NULL)
	}
	if (!("maxOutcome.Grps" %in% names(raw))) {
		message("Missing chains data");
		return(NULL)
	}
	if (!("nOutcome.Grp" %in% names(raw))) {
		message("Missing chains data");
		return(NULL)
	}
	if (!("maxOutcomes" %in% names(raw))) {
		message("Missing chains data");
		return(NULL)
	}
	if (!("nOutcome" %in% names(raw))) {
		message("Missing nOutcome data");
		return(NULL)
	}
	if (!("theta" %in% names(raw))) {
		message("Missing theta data");
		return(NULL)
	}
	if (!("Outcome.Grp" %in% names(raw))) {
		message("Missing Outcome.Grp data");
		return(NULL)
	}
	if (!("Outcome" %in% names(raw))) {
		message("Missing Outcome data");
		return(NULL)
	}

	nchains = raw$chains
	theta.trt.grps <- raw$Trt.Grps[ raw$Trt.Grps$param == "theta", ]$Trt.Grp

	summ <- data.frame(Trt.Grp = integer(0), Cluster = character(0), Outcome.Grp = character(0), Outcome = character(0), ptheta = numeric(0), ptheta.pos = numeric(0), ptheta.zero = numeric(0), ptheta.neg = numeric(0))

	# Inference - based on combined chains
	samples_combined = rep(NA, (raw$iter - raw$burnin)*nchains)

	for (t in 1:(raw$nTreatments - 1)) {
		for (i in 1:raw$nClusters) {
			for (b in 1:raw$nOutcome.Grp[i]) {
				for (j in 1:raw$nOutcome[i, b]) {
					mcmc_obj <- list(NA)
					for (c in 1:nchains) {
						mcmc_obj[[c]] <- mcmc(raw$theta[c, t, i, b, j, ])
					}
					mlist <- mcmc.list(mcmc_obj)
	
					samples_combined <- c(raw$theta[1:nchains, t, i, b, j, ])
					s <- ecdf(samples_combined)
					th <-  1 - s(0)
					th.pos <- length(samples_combined[samples_combined > 0])/length(samples_combined)
					th.0 <- length(samples_combined[samples_combined == 0])/length(samples_combined)
					th.neg <- length(samples_combined[samples_combined < 0])/length(samples_combined)
					row <- data.frame(Trt.Grp = theta.trt.grps[t], Cluster = raw$Clusters[i], Outcome.Grp = raw$Outcome.Grp[i, b], Outcome = raw$Outcome[i, b, j], ptheta = th, ptheta.pos = th.pos, ptheta.zero = th.0, ptheta.neg = th.neg)
					summ = rbind(summ, row)
				}
			}
		}
	}

	rownames(summ) <- NULL
	return(summ)
}
