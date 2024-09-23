# bhpm.1a
# Default global sim parameters
# R. Carragher
# Date: 10/06/2019

Id <- "$Id: bhpm.global.sim.param.defaults.R,v 1.1 2019/06/10 15:33:53 clb13102 Exp clb13102 $"

bhpm.global.sim.param.defaults <- function(model = "BB", hier = 3) {

	global.sim.params <- NULL

	if (model == "1a") {
			global.sim.params <- data.frame(type = c("MH", "SLICE"), param = c("sigma_MH", "w"),
					value = c(0.2,1), control = c(0,6), stringsAsFactors = FALSE)
	}
	else if (model == "BB") {
		if (hier == 3) {
			global.sim.params <- data.frame(type = c("MH", "MH", "MH", "MH", "SLICE", "SLICE", "SLICE"),
                    param = c("sigma_MH_alpha", "sigma_MH_beta", "sigma_MH_gamma", "sigma_MH_theta",
                    "w_alpha", "w_beta", "w_gamma"),
                    value = c(3, 3, 0.2, 0.25, 1, 1, 1), control = c(0, 0, 0, 0, 6, 6, 6),
                    stringsAsFactors = FALSE)

		}
		else {
			global.sim.params <- data.frame(type = c("MH", "MH", "MH", "MH", "SLICE", "SLICE", "SLICE"),
                param = c("sigma_MH_alpha", "sigma_MH_beta", "sigma_MH_gamma",
                "sigma_MH_theta",
                "w_alpha", "w_beta", "w_gamma"),
                value = c(3, 3, 0.2, 0.5, 1, 1, 1), control = c(0, 0, 0, 0, 6, 6, 6),
                stringsAsFactors = FALSE)
		}	

	}
	return(global.sim.params)
}
