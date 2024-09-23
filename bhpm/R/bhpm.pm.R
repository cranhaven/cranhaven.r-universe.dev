# bhpm.pm
# Cluster Analysis wrapper - point-mass
# R. Carragher
# Date: 10/06/2019

Id <- "$Id: bhpm.pm.R,v 1.2 2019/06/11 14:18:26 clb13102 Exp clb13102 $"

bhpm.pm <- function(cluster.data, hier = 3, sim_type = "SLICE", burnin = 20000, iter = 60000, nchains = 5, theta_algorithm = "MH",
					global.sim.params = NULL,
					sim.params = NULL,
					monitor = NULL,
					initial_values = NULL,
					level = 1,
					hyper_params = NULL,
					global.pm.weight = 0.5,
					pm.weights = NULL,
					adapt_phase=1,
					memory_model = "HIGH")
{
	g.s.p <-global.sim.params
	mon <- monitor
	h.p <- hyper_params
	model.fit <- NULL

	if (is.null(g.s.p)) {
		g.s.p  = bhpm.global.sim.param.defaults(model = "BB", hier = hier)
	}
	if (is.null(mon)) {
		mon <- bhpm.monitor.defaults(model = "BB", hier = hier)
	}
	if (is.null(h.p)) {
		h.p <- bhpm.hyper.param.defaults(model = "BB", hier = hier)
	}

	if (hier == 3) {
		model.fit <- bhpm.cluster.BB.hier3(cluster.data,
									sim_type,
									burnin,
									iter,
									nchains,
								    theta_algorithm,
								    global.sim.params = g.s.p,
									sim.params,
									monitor = mon,
									initial_values,
									level,
									hyper_params = h.p,
									global.pm.weight,
									pm.weights,
									adapt_phase,
									memory_model)
	}
	else if (hier == 2) {
		model.fit <- bhpm.cluster.BB.hier2(cluster.data,
									sim_type,
									burnin,
									iter,
									nchains,
									theta_algorithm,
									global.sim.params = g.s.p,
									sim.params,
									monitor = mon,
								    initial_values,
									level,
									hyper_params = h.p,
									global.pm.weight,
									pm.weights,
									adapt_phase,
									memory_model)
	}

	return(model.fit)
}
