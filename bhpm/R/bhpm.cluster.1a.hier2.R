# bhpm.cluster
# bhpm: Cluster Analysis wrapper
# R. Carragher
# Date: 29/06/2018


Id <- "$Id: bhpm.cluster.1a.hier2.R,v 1.4 2019/05/28 11:05:07 clb13102 Exp clb13102 $"

bhpm.cluster.1a.hier2 <- function(cluster.data, sim_type = "SLICE", burnin = 10000,
	iter = 40000, nchains = 3,
	global.sim.params = data.frame(type = c("MH", "SLICE"), param = c("sigma_MH", "w"),
							value = c(0.2,1), control = c(0,6), stringsAsFactors = FALSE),
	sim.params = NULL,
	monitor = data.frame(variable = c("theta", "gamma", "mu.gamma", "mu.theta",
					"sigma2.theta", "sigma2.gamma"),
					monitor = c(1, 1, 1, 1, 1, 1), stringsAsFactors = FALSE),
	initial_values = NULL, level = 1,
	hyper_params = list(mu.gamma.0 = 0, tau2.gamma.0 = 10, mu.theta.0 = 0,
		tau2.theta.0 = 10, alpha.gamma = 3, beta.gamma = 1,
		alpha.theta = 3, beta.theta = 1), memory_model = "HIGH")
{
	if (level == 0) {
		model_fit = bhpm.cluster.1a.hier2.lev0(cluster.data, sim_type, burnin,
						iter, nchains, global.sim.params, sim.params,
						monitor, initial_values,
						hyper_params, memory_model)
	}
	else if (level == 1) {
		model_fit = bhpm.cluster.1a.hier2.lev1(cluster.data, sim_type, burnin,
						iter, nchains, global.sim.params, sim.params,
						monitor, initial_values,
						hyper_params, memory_model)
	} else {
		return(NULL)
	}

	return(model_fit)
}
