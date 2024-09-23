Id <- "$Id: bhpm.params.R,v 1.14 2019/06/03 11:07:19 clb13102 Exp clb13102 $"

bhpm.sim.control.params = function(cluster.data, model = "1a") {

	if (model == "1a") {
		sim_param = bhpm.1a.sim.control.params(cluster.data)
	}
	else {
		sim_param = bhpm.BB.sim.control.params(cluster.data)
	}

	sim_param
}

bhpm.1a.sim.control.params = function(cluster.data) {

	if (is.character(cluster.data)) {
		file = cluster.data
		cluster.data <- read.table(file, header=TRUE, stringsAsFactors = FALSE)
	}

	facs <- sapply(cluster.data, is.factor)
	cluster.data[facs] <- sapply(cluster.data[facs], as.character)

	grps = unique(cluster.data$Trt.Grp)

	cntrl.group = min(grps)
	comp.grp = grps[grps > cntrl.group]

	cluster.data = cluster.data[cluster.data$Trt.Grp == cntrl.group,]
	n = nrow(cluster.data)

	sim_params <- NULL

	if ("Cluster" %in% names(cluster.data)) {

		p_SLICE = "w"
		v_SLICE = 1.0
		c_SLICE = 6

		p_MH = "sigma_MH"
		v_MH = 0.2
		c_MH = 0.0

		sp1 = data.frame(type = "SLICE", variable = "gamma",
				Cluster = cluster.data$Cluster,
				Outcome.Grp = cluster.data$Outcome.Grp,
				Outcome = cluster.data$Outcome, Trt.Grp = as.integer(1),
				param = p_SLICE, value = v_SLICE, control = c_SLICE,
				stringsAsFactors = FALSE)

		sp2 = data.frame(type = "MH", variable = "gamma",
				Cluster = cluster.data$Cluster,
				Outcome.Grp = cluster.data$Outcome.Grp,
				Outcome = cluster.data$Outcome, Trt.Grp = as.integer(1), 
				param = p_MH, value = v_MH, control = c_MH,
				stringsAsFactors = FALSE)

		sim_params <- rbind(sp1, sp2)

		for (i in 1:length(comp.grp)) {
			sp1 = data.frame(type = "SLICE", variable = "theta",
					Cluster = cluster.data$Cluster,
					Outcome.Grp = cluster.data$Outcome.Grp,
					Outcome = cluster.data$Outcome, Trt.Grp = as.integer(comp.grp[i]),
					param = p_SLICE, value = v_SLICE, control = c_SLICE,
					stringsAsFactors = FALSE)

			sp2 = data.frame(type = "MH", variable = "theta",
					Cluster = cluster.data$Cluster,
					Outcome.Grp = cluster.data$Outcome.Grp,
					Outcome = cluster.data$Outcome, Trt.Grp = as.integer(comp.grp[i]),
					param = p_MH, value = v_MH, control = c_MH,
					stringsAsFactors = FALSE)

			sim_params <- rbind(sim_params, sp1, sp2)
		}
	}

	sim_params
}

bhpm.BB.sim.control.params = function(cluster.data) {

	if (is.character(cluster.data)) {
		file = cluster.data
		cluster.data <- read.table(file, header=TRUE, stringsAsFactors = FALSE)
	}

	facs <- sapply(cluster.data, is.factor)
	cluster.data[facs] <- sapply(cluster.data[facs], as.character)

	grps = unique(cluster.data$Trt.Grp)

	cntrl.group = min(grps)
	comp.grp = grps[grps > cntrl.group]

	cluster.data = cluster.data[cluster.data$Trt.Grp == cntrl.group,]

	sim_params <- NULL

	if ("Cluster" %in% names(cluster.data)) {

		v_sigma_MH_gamma = 0.2
		v_sigma_MH_theta = 0.15

		v_w = 1
		v_w_control = 6
		v_sigma = 3

		sp1 = data.frame(type = "MH", variable = "gamma",
								Cluster = cluster.data$Cluster,
								Outcome.Grp = cluster.data$Outcome.Grp,
								Outcome = cluster.data$Outcome, Trt.Grp = 1,
								param = "sigma_MH_gamma", value = v_sigma_MH_gamma,
								control = 0,
								stringsAsFactors = FALSE)

		sp2 = data.frame(type = "SLICE", variable = "gamma",
								Cluster = cluster.data$Cluster,
								Outcome.Grp = cluster.data$Outcome.Grp,
								Outcome = cluster.data$Outcome, Trt.Grp = 1,
								param = "w_gamma", value = v_w,
								control = v_w_control,
								stringsAsFactors = FALSE)

		sim_params <- rbind(sp1, sp2)

		for (i in 1:length(comp.grp)) {
			sp1 = data.frame(type = "MH", variable = "theta",
								Cluster = cluster.data$Cluster,
								Outcome.Grp = cluster.data$Outcome.Grp,
								Outcome = cluster.data$Outcome, Trt.Grp = as.integer(comp.grp[i]),
								param = "sigma_MH_theta", value = v_sigma_MH_theta,
								control = 0,
								stringsAsFactors = FALSE)

			C_cl = unique(cluster.data$Cluster)

			sp2 = data.frame(type = "MH", variable = "alpha",
								Cluster = C_cl,
								Outcome.Grp = "",
								Outcome = "", Trt.Grp = as.integer(comp.grp[i]),
								param = "sigma_MH_alpha", value = v_sigma,
								control = 0,
								stringsAsFactors = FALSE)

			sp3 = data.frame(type = "SLICE", variable = "alpha",
								Cluster = C_cl,
								Outcome.Grp = "",
								Outcome = "", Trt.Grp = as.integer(comp.grp[i]),
								param = "w_alpha", value = v_w,
								control = v_w_control,
								stringsAsFactors = FALSE)

			sp4 = data.frame(type = "MH", variable = "beta",
								Cluster = C_cl,
								Outcome.Grp = "",
								Outcome = "", Trt.Grp = as.integer(comp.grp[i]),
								param = "sigma_MH_beta", value = v_sigma,
								control = 0,
								stringsAsFactors = FALSE)

			sp5 = data.frame(type = "SLICE", variable = "beta",
								Cluster = C_cl,
								Outcome.Grp = "",
								Outcome = "", Trt.Grp = as.integer(comp.grp[i]),
								param = "w_beta", value = v_w,
								control = v_w_control,
								stringsAsFactors = FALSE)

			sim_params = rbind(sim_params, sp1, sp2, sp3, sp4, sp5)
		}
	}

	sim_params
}
