# bhpm.convergence.diag
# Model bhpm.BB
# R. Carragher
# Date: 29/06/2018
#
# If the MCMC simulation has been run for more than one chain report the Gelman-Rubin statistic.
# If the MCMC simulation has been run for only one chain report the Geweke diagnostic (Z-score)
#

Id <- "$Id: bhpm.convergence.R,v 1.6 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.convergence.diag <- function(raw, debug_diagnostic = FALSE)
{
	if (is.null(raw)) {
		message("NULL raw data")
		return(NULL)
	}

	model = attr(raw, "model")
	if (is.null(model)) {
		message("Simulation model attribute missing")
		return(NULL)
	}

	conv.diag = list()

	if (model == "1a_pois_indep") {
		conv.diag = bhpm.cluster.1a.indep.convergence.diag(raw, debug_diagnostic);
	} else if (model == "1a_pois_dep_lev2") {
		conv.diag = bhpm.cluster.1a.dep.lev2.convergence.diag(raw, debug_diagnostic);
	} else if (model == "1a_pois_dep_lev1") {
		conv.diag = bhpm.cluster.1a.dep.lev1.convergence.diag(raw, debug_diagnostic);
	} else if (model == "BB_pois_indep") {
		conv.diag = bhpm.cluster.BB.indep.convergence.diag(raw, debug_diagnostic);
	} else if (model == "BB_pois_dep_lev2") {
		conv.diag = bhpm.cluster.BB.dep.lev2.convergence.diag(raw, debug_diagnostic);
	} else if (model == "BB_pois_dep_lev1") {
		conv.diag = bhpm.cluster.BB.dep.lev1.convergence.diag(raw, debug_diagnostic);
	} else if (model == "1a_pois_h2_l0") {
		conv.diag = bhpm.cluster.1a.hier2.lev0.convergence.diag(raw, debug_diagnostic);
	} else if (model == "1a_pois_h2_l1") {
		conv.diag = bhpm.cluster.1a.hier2.lev1.convergence.diag(raw, debug_diagnostic);
	} else if (model == "BB_pois_h2_l0") {
		conv.diag = bhpm.cluster.BB.hier2.lev0.convergence.diag(raw, debug_diagnostic);
	} else if (model == "BB_pois_h2_l1") {
		conv.diag = bhpm.cluster.BB.hier2.lev1.convergence.diag(raw, debug_diagnostic);
	}
	else {
		conv.diag = NULL
	}

	return(conv.diag)
}

bhpm.print.convergence.summary <- function(conv) {

	if (is.null(conv)) {
		message("NULL conv data")
		return(NULL)
	}

	model = attr(conv, "model")
	if (is.null(model)) {
		message("Convergence model attribute missing")
		return(NULL)
	}

	if (model == "1a_pois_indep") {
		bhpm.cluster.1a.indep.print.convergence.summary(conv)
	} else if (model == "1a_pois_dep_lev2") {
		bhpm.cluster.1a.dep.lev2.print.convergence.summary(conv)
	} else if (model == "1a_pois_dep_lev1") {
		bhpm.cluster.1a.dep.lev1.print.convergence.summary(conv)
	} else if (model == "BB_pois_indep") {
		bhpm.cluster.BB.indep.print.convergence.summary(conv)
	} else if (model == "BB_pois_dep_lev2") {
		bhpm.cluster.BB.dep.lev2.print.convergence.summary(conv)
	} else if (model == "BB_pois_dep_lev1") {
		bhpm.cluster.BB.dep.lev1.print.convergence.summary(conv)
	} else if (model == "1a_pois_h2_l0") {
		bhpm.cluster.1a.hier2.lev0.print.convergence.summary(conv)
	} else if (model == "1a_pois_h2_l1") {
		bhpm.cluster.1a.hier2.lev1.print.convergence.summary(conv)
	} else if (model == "BB_pois_h2_l0") {
		bhpm.cluster.BB.hier2.lev0.print.convergence.summary(conv)
	} else if (model == "BB_pois_h2_l1") {
		bhpm.cluster.BB.hier2.lev1.print.convergence.summary(conv)
	}
}
