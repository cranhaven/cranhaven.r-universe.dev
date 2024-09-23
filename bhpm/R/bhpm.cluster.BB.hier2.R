# bhpm.cluster
# bhpm: Cluster Analysis wrapper
# R. Carragher
# Date: 29/06/2018


Id <- "$Id: bhpm.cluster.BB.hier2.R,v 1.4 2019/05/28 11:05:07 clb13102 Exp clb13102 $"

bhpm.cluster.BB.hier2 <- function(cluster.data, sim_type = "SLICE", burnin = 20000, iter = 60000, nchains = 5,
	theta_algorithm = "MH",
	global.sim.params = data.frame(type = c("MH", "MH", "MH", "MH", "SLICE", "SLICE", "SLICE"),
				param = c("sigma_MH_alpha", "sigma_MH_beta", "sigma_MH_gamma",
				"sigma_MH_theta",
				"w_alpha", "w_beta", "w_gamma"),
				value = c(3, 3, 0.2, 0.5, 1, 1, 1), control = c(0, 0, 0, 0, 6, 6, 6),
				stringsAsFactors = FALSE),
	sim.params = NULL,
	monitor = data.frame(variable = c("theta", "gamma", "mu.gamma", "mu.theta",
					"sigma2.theta", "sigma2.gamma", "pi"),
					monitor = c(1, 1, 1, 1, 1, 1, 1), stringsAsFactors = FALSE),
	initial_values = NULL, level = 1,
	hyper_params = list(mu.gamma.0 = 0, tau2.gamma.0 = 10, mu.theta.0 = 0,
			tau2.theta.0 = 10, alpha.gamma = 3, beta.gamma = 1, alpha.theta = 3,
			beta.theta = 1, alpha.pi = 1.1, beta.pi = 1.1),
			global.pm.weight = 0.5,
			pm.weights = NULL,
			adapt_phase=1, memory_model = "HIGH")
{
	if (level == 0) {
		model_fit = bhpm.cluster.BB.hier2.lev0(cluster.data, sim_type, burnin, iter,
						nchains, theta_algorithm, global.sim.params, sim.params,
						monitor, initial_values,
						hyper_params, global.pm.weight, pm.weights, adapt_phase,
						memory_model)
	} else if (level == 1) {
		model_fit = bhpm.cluster.BB.hier2.lev1(cluster.data, sim_type, burnin, iter,
						nchains, theta_algorithm, global.sim.params, sim.params,
						monitor, initial_values,
						hyper_params, global.pm.weight, pm.weights, adapt_phase,
						memory_model)
	} else {
		return(NULL)
	}

	return(model_fit)
}
