# bhpm.1a
# Default variable monitoring
# R. Carragher
# Date: 10/06/2019

Id <- "$Id: bhpm.monitor.defaults.R,v 1.1 2019/06/10 15:33:53 clb13102 Exp clb13102 $"

bhpm.monitor.defaults <- function(model = "BB", hier = 3) {

	monitor <- NULL

	if (model == "1a") {
		if (hier == 3) {
			monitor = data.frame(variable = c("theta", "gamma", "mu.gamma", "mu.theta",
						"sigma2.theta", "sigma2.gamma",
						"mu.theta.0", "mu.gamma.0", "tau2.theta.0", "tau2.gamma.0"),
						monitor = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
						stringsAsFactors = FALSE)
		}
		else {
			monitor = data.frame(variable = c("theta", "gamma", "mu.gamma", "mu.theta",
						"sigma2.theta", "sigma2.gamma"),
						monitor = c(1, 1, 1, 1, 1, 1), stringsAsFactors = FALSE)

		}
	}
	else if (model == "BB") {
		if (hier == 3) {
			monitor = data.frame(variable = c("theta", "gamma", "mu.gamma", "mu.theta",
					"sigma2.theta", "sigma2.gamma",
					"mu.theta.0", "mu.gamma.0", "tau2.theta.0", "tau2.gamma.0",
					"pi", "alpha.pi", "beta.pi"), 
					monitor = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
					stringsAsFactors = FALSE)
		}
		else {
			monitor = data.frame(variable = c("theta", "gamma", "mu.gamma", "mu.theta",
					"sigma2.theta", "sigma2.gamma", "pi"),
					monitor = c(1, 1, 1, 1, 1, 1, 1), stringsAsFactors = FALSE)
		}
	}
	return(monitor)
}
