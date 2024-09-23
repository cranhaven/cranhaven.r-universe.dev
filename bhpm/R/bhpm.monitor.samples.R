bhpm.monitor.samples = function (model = "1a", hier = 3) {

	if (is.null(model)) {
		return(NULL)
	}

	# 1a - hier 2
	v = c("theta", "gamma", "mu.gamma", "mu.theta", "sigma2.theta", "sigma2.gamma")

	if (hier == 3) {
		v = c(v, c("mu.theta.0", "mu.gamma.0", "tau2.theta.0", "tau2.gamma.0"))
	}

	if (model == "BB") {
		v = c(v, "pi")
		if (hier == 3) {
			v = c(v, c("alpha.pi", "beta.pi"))
		}
	}

	s = c(1, rep(0, length(v) - 1))
	
	monitor = data.frame(variable = v, monitor = s, stringsAsFactors = FALSE)

	monitor
}
