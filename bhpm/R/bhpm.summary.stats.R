# bhpm.BB.summary
# Model bhpm.BB
# R. Carragher
# Date: 29/06/2018

Id <- "$Id: bhpm.summary.stats.R,v 1.7 2020/03/31 12:42:23 clb13102 Exp clb13102 $"

bhpm.summary.stats <- function(raw, prob = 0.95)
{
	if (is.null(raw)) {
		message("NULL raw data");
		return(NULL)
	}

	model = attr(raw, "model")
	if (is.null(model)) {
		message("Model attribute missing");
		return(NULL)
	}

	summary.stats = list()

	if (model == "1a_pois_indep") {
		summary.stats = bhpm.cluster.1a.indep.summary.stats(raw, prob)
	} else if (model == "1a_pois_dep_lev2") {
		summary.stats = bhpm.cluster.1a.dep.lev2.summary.stats(raw, prob)
	} else if (model == "1a_pois_dep_lev1") {
		summary.stats = bhpm.cluster.1a.dep.lev1.summary.stats(raw, prob)
	} else if (model == "BB_pois_indep") {
		summary.stats = bhpm.cluster.BB.indep.summary.stats(raw, prob)
	} else if (model == "BB_pois_dep_lev2") {
		summary.stats = bhpm.cluster.BB.dep.lev2.summary.stats(raw, prob)
	} else if (model == "BB_pois_dep_lev1") {
		summary.stats = bhpm.cluster.BB.dep.lev1.summary.stats(raw, prob)
	} else if (model == "1a_pois_h2_l0") {
		summary.stats = bhpm.cluster.1a.hier2.lev0.summary.stats(raw, prob)
	} else if (model == "1a_pois_h2_l1") {
		summary.stats = bhpm.cluster.1a.hier2.lev1.summary.stats(raw, prob)
	} else if (model == "BB_pois_h2_l0") {
		summary.stats = bhpm.cluster.BB.hier2.lev0.summary.stats(raw, prob)
	} else if (model == "BB_pois_h2_l1") {
		summary.stats = bhpm.cluster.BB.hier2.lev1.summary.stats(raw, prob)
	}
	return(summary.stats)
}

bhpm.print.summary.stats <- function(summ)
{
	if (is.null(summ)) {
		message("NULL summary data");
		return(NULL)
	}

	model = attr(summ, "model")
	if (is.null(model)) {
		message("Missing model attribute");
		return(NULL)
	}

	if (model == "1a_pois_indep") {
		bhpm.cluster.1a.indep.print.summary.stats(summ)
	} else if (model == "1a_pois_dep_lev2") {
		bhpm.cluster.1a.dep.lev2.print.summary.stats(summ)
	} else if (model == "1a_pois_dep_lev1") {
		bhpm.cluster.1a.dep.lev1.print.summary.stats(summ)
	} else if (model == "BB_pois_indep") {
		bhpm.cluster.BB.indep.print.summary.stats(summ)
	} else if (model == "BB_pois_dep_lev2") {
		bhpm.cluster.BB.dep.lev2.print.summary.stats(summ)
	} else if (model == "BB_pois_dep_lev1") {
		bhpm.cluster.BB.dep.lev1.print.summary.stats(summ)
	} else if (model == "1a_pois_h2_l0") {
		bhpm.cluster.1a.hier2.lev0.print.summary.stats(summ)
	} else if (model == "1a_pois_h2_l1") {
		bhpm.cluster.1a.hier2.lev1.print.summary.stats(summ)
	} else if (model == "BB_pois_h2_l0") {
		bhpm.cluster.BB.hier2.lev0.print.summary.stats(summ)
	} else if (model == "BB_pois_h2_l1") {
		bhpm.cluster.BB.hier2.lev1.print.summary.stats(summ)
	}
}
