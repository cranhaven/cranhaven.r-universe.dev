# bhpm.npm
# Cluster Analysis wrapper - no point-mass
# R. Carragher
# Date: 10/06/2019

Id <- "$Id: bhpm.npm.R,v 1.2 2019/06/11 14:18:26 clb13102 Exp clb13102 $"

bhpm.npm <- function(cluster.data, hier = 3, sim_type = "SLICE", burnin = 10000, iter = 40000, nchains = 3,
				global.sim.params = NULL,
				sim.params = NULL,
				monitor = NULL,
				initial_values = NULL,
				level = 1,
				hyper_params = NULL,
				memory_model = "HIGH")
{
	g.s.p <- global.sim.params
	mon <- monitor
	h.p <- hyper_params
	model.fit <- NULL

	if (is.null(g.s.p)) {
		g.s.p  = bhpm.global.sim.param.defaults(model = "1a", hier = hier)
	}
	if (is.null(mon)) {
		mon <- bhpm.monitor.defaults(model = "1a", hier = hier)
	}
	if (is.null(h.p)) {
		h.p <- bhpm.hyper.param.defaults(model = "1a", hier = hier)
	}

	if (hier == 3) {
		model.fit <- bhpm.cluster.1a.hier3(cluster.data,
									sim_type,
									burnin,
									iter,
									nchains,
									global.sim.params = g.s.p,
									sim.params,
									monitor = mon,
									initial_values,
									level,
									hyper_params = h.p,
									memory_model)
	}
	else if (hier == 2) {
		model.fit <- bhpm.cluster.1a.hier2(cluster.data,
									sim_type,
									burnin,
									iter,
									nchains,
									global.sim.params = g.s.p,
									sim.params,
									monitor = mon,
									initial_values,
									level,
									hyper_params = h.p,
									memory_model)
	}

	return(model.fit)
}
