# bhpm.1a
# Default model hyper-parameters
# R. Carragher
# Date: 10/06/2019

Id <- "$Id: bhpm.hyper.params.defaults.R,v 1.1 2019/06/10 15:33:53 clb13102 Exp clb13102 $"

bhpm.hyper.param.defaults <- function(model = "BB", hier = 3) {

	hyper_params <- NULL

	if (model == "1a") {
		if (hier == 3) {
			hyper_params = list(mu.gamma.0.0 = 0, tau2.gamma.0.0 = 10, mu.theta.0.0 = 0,
				tau2.theta.0.0 = 10, alpha.gamma.0.0 = 3, beta.gamma.0.0 = 1,
				alpha.theta.0.0 = 3, beta.theta.0.0 = 1, alpha.gamma = 3, beta.gamma = 1,
				alpha.theta = 3, beta.theta = 1)
		}
		else {
			hyper_params = list(mu.gamma.0 = 0, tau2.gamma.0 = 10, mu.theta.0 = 0,
				tau2.theta.0 = 10, alpha.gamma = 3, beta.gamma = 1,
				alpha.theta = 3, beta.theta = 1)
		}
	}
	else if (model == "BB") {
		if (hier == 3) {
			hyper_params = list(mu.gamma.0.0 = 0, tau2.gamma.0.0 = 10,
				mu.theta.0.0 = 0, tau2.theta.0.0 = 10, alpha.gamma.0.0 = 3, beta.gamma.0.0 = 1, alpha.theta.0.0 = 3,
				beta.theta.0.0 = 1, alpha.gamma = 3, beta.gamma = 1, alpha.theta = 3, beta.theta = 1,
				lambda.alpha = 1.0, lambda.beta = 1.0)
		}
		else {
			hyper_params = list(mu.gamma.0 = 0, tau2.gamma.0 = 10, mu.theta.0 = 0,
				tau2.theta.0 = 10, alpha.gamma = 3, beta.gamma = 1, alpha.theta = 3,
				beta.theta = 1, alpha.pi = 1.1, beta.pi = 1.1)
		}
	}

	return(hyper_params)
}
