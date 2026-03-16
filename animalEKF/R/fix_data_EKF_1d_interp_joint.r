fix_data_EKF_1d_interp_joint <- function(env_obj) {

	# verbosity
	if (! env_obj$verbose %in% 0:3) {
		stop("verbose must be one of (0,1,2,3)")
	}
	env_obj$show_prints <- env_obj$verbose %in% c(1,3)
	env_obj$show_plots <- env_obj$verbose %in% c(2,3) 
	

	# number of behavioral states
	env_obj$nstates <- as.integer(env_obj$nstates)
	if (! env_obj$nstates %in% c(1,2)) {
		stop("nstates must be one of (1,2)")
	}

	# confidence plot
	env_obj$loc_pred_plot_conf <- min(max(env_obj$loc_pred_plot_conf, 0.05), 0.95)
	# normal 
	env_obj$loc_pred_plot_conf_constant <- qnorm(p=0.5 + env_obj$loc_pred_plot_conf/2)


	env_obj$d <- env_obj$d[ order(env_obj$d$date_as_sec),]
	env_obj$first_time <- min(env_obj$d$date_as_sec)	
	env_obj$shark_names <- as.character(sort(unique(env_obj$d$tag)))

	# if NULL, maxStep should be the number of steps required to simulate entire submitted data
	observed_intervals <- lapply(env_obj$shark_names, function(s) unique(env_obj$d$t_intervals[ env_obj$d$tag==s ]))
	names(observed_intervals) <- env_obj$shark_names
	observed_intervals <- observed_intervals[sapply(observed_intervals, function(x) length(x) >= 3)]
	
	if (length(observed_intervals) == 0) {
		stop(paste("No observed animals have 3 or more intervals of length", env_obj$reg_dt, "with observations\nNeed to have more data or shorted reg_dt"))
	}
	
	third_steps <- sapply(observed_intervals, function(x) x[3])

	if (is.null(env_obj$max_int_wo_obs)) {
		env_obj$max_int_wo_obs <- Inf
	}
	else if (! any(third_steps <= env_obj$max_int_wo_obs)) {
		print(observed_intervals)
		stop(paste("No observed animals have consecutive observed intervals separated by less than", env_obj$max_int_wo_obs, "intervals.\nNeed to increase max_int_wo_obs to at least", min(third_steps))) 
	}

	min_intervals_needed <- min(third_steps[third_steps <= env_obj$max_int_wo_obs])

	
	# if NULL, maxStep should be the number of steps required to simulate entire submitted data
	max_required_steps <- ceiling(1+(env_obj$d$date_as_sec[ nrow(env_obj$d) ] - env_obj$first_time)/env_obj$reg_dt)

	env_obj$maxStep <- ifelse(is.null(env_obj$maxStep), max_required_steps, max(min_intervals_needed, min(env_obj$maxStep, max_required_steps)))
	

	env_obj$t_reg <- seq(from= env_obj$first_time, by=env_obj$reg_dt, length.out=env_obj$maxStep)
	#do this so the first interval captures the first observation at t=0
	env_obj$t_reg[ 1 ] <- env_obj$t_reg[ 1 ]-.Machine$double.eps
	
	env_obj$N <- length(env_obj$t_reg) 

	env_obj$max_int_wo_obs <- min(env_obj$N+1, env_obj$max_int_wo_obs)

	env_obj$d <- env_obj$d[ env_obj$d$date_as_sec <= env_obj$t_reg[ env_obj$N ],]

	env_obj$tags <- env_obj$d$tag

	#dt <- d$time_to_next

	env_obj$included_intervals <- 1:(env_obj$N-1)

	#calculate which regular step each observation falls into
	env_obj$d$t_intervals <- as.numeric(as.character(cut(x=env_obj$d$date_as_sec, breaks=env_obj$t_reg, labels=env_obj$included_intervals, right=TRUE)))

	print("t_intervals")
	print(env_obj$d$t_intervals)

	env_obj$shark_names <- as.character(sort(unique(env_obj$tags)))
	env_obj$nsharks <- length(env_obj$shark_names)
	print(paste("shark names are",paste(env_obj$shark_names, collapse=" ")))
	env_obj$shark_intervals <- list()
	env_obj$shark_valid_steps <- list()

	for (s in env_obj$shark_names) {

		env_obj$shark_intervals[[ s ]] <-  unique(env_obj$d$t_intervals[ env_obj$d$tag==s ])

		#keep steps where there are less than a certain gap between observations 
				 
		tmp <- c()
		tmp1 <- c()
		
		for (jj in 1:(length(env_obj$shark_intervals[[ s ]])-1)){
			if(diff(env_obj$shark_intervals[[ s ]][ jj:(jj+1) ]) <= env_obj$max_int_wo_obs) {
				tmp <- c(tmp, (env_obj$shark_intervals[[ s ]][ jj ]):(env_obj$shark_intervals[[ s ]][ jj+1 ]))
				tmp1 <- c(tmp1, env_obj$shark_intervals[[ s ]][ jj:(jj+1) ])  
			}
		}
		
		env_obj$shark_valid_steps[[ s ]] <- sort(unique(tmp))
		
		env_obj$shark_intervals[[ s ]] <- sort(unique(tmp1))

		 
	}
	
	print("shark intervals")
	print(env_obj$shark_intervals)
	print(env_obj$N)

	env_obj$included_intervals <- sort(unique(unlist(env_obj$shark_valid_steps))) 

	print(paste("sharks:", paste(env_obj$shark_names, collapse=" ")))



	env_obj$first_intervals <- lapply(env_obj$shark_valid_steps, function(x) x[ !((x-1) %in% x) ])

	names(env_obj$first_intervals) <- env_obj$shark_names
	print("starting observations per shark:")
	print(env_obj$first_intervals)

	env_obj$shark_symbols <- 1:env_obj$nsharks
	names(env_obj$shark_symbols) <- env_obj$shark_names


	print("intervals with observations per shark:")
	print(env_obj$shark_intervals)


	print("intervals to be simulated per shark:")
	print(env_obj$shark_valid_steps)
	
	
	#last interval with a valid observation
	env_obj$shark_final_obs <- sapply(env_obj$shark_intervals, max)
	names(env_obj$shark_final_obs) <- env_obj$shark_names


	if (env_obj$nsharks==1) {
		env_obj$interact <- FALSE
	}

	if (env_obj$nstates==1) {
	  env_obj$states <- rep(1, length(env_obj$states))
	  env_obj$next_states <- rep(1, length(env_obj$next_states))
	  env_obj$interact <- FALSE
	}


	print(paste("nstates:", env_obj$nstates))

	if (env_obj$update_params_for_obs_only) {
		env_obj$update_eachstep <- FALSE		
	}


	env_obj$d$shark_obs_index  <- NA
	for (s in env_obj$shark_names) {
	   ss <- which(env_obj$tags==s)
	   env_obj$d$shark_obs_index[ ss ]<- 1:length(ss)
	}

	#print(d$shark_obs_index)
	if (env_obj$nsharks > 1) { print(env_obj$tags) }  

	env_obj$d$rowid <- 1:nrow(env_obj$d)
	
	#if we want to model it as one state, so be it
	
	if (env_obj$nstates == 1) {
		env_obj$d$state.guess2[ ! is.na(env_obj$d$state.guess2)] <- 1		
		env_obj$d$next.guess2[ ! is.na(env_obj$d$next.guess2)] <- 1
		env_obj$d$lambda[ ! is.na(env_obj$d$lambda)] <- 1

	}
	
	
	env_obj$d$state.guess2 <- as.numeric(env_obj$d$state.guess2)
	env_obj$d$next.guess2 <- as.numeric(env_obj$d$next.guess2)
	env_obj$d$lambda <- as.numeric(env_obj$d$lambda)


	env_obj$states <- as.numeric(env_obj$d$state.guess2)
	env_obj$next_states <- as.numeric(env_obj$d$next.guess2)


	#nstates <- max(length(unique(states)), nstates)

	env_obj$d <- env_obj$d[,c("shark_obs_index","X","velocity","date_as_sec","time_to_next",
							"lambda","state.guess2","next.guess2","t_intervals")]


	env_obj$d <- as.matrix(env_obj$d)
	rownames(env_obj$d) <- 1:nrow(env_obj$d)
	#for 1D log velocity is just angle_velocity
	
	nus <- length(unique(env_obj$states, na.rm=TRUE))
	nust <- env_obj$nstates
	if (env_obj$compare_with_known) {
		nust <- length(unique(env_obj$known_regular_step_ds$state.guess2, na.rm=TRUE))
		
		true_diffs <- unique(diff(unique(env_obj$known_regular_step_ds$date_as_sec[ ! is.na(env_obj$known_regular_step_ds$date_as_sec)])))
		if (length(true_diffs) > 1) {
			stop(paste("known_regular_step_ds has multiple observed time gaps:", paste(true_diffs, collapse=", ")))	
		}
		else if (! (env_obj$reg_dt %in% true_diffs)) {
			stop(paste("known_regular_step_ds has observed time gap ", true_diffs, " but argument reg_dt is ", env_obj$reg_dt, "; they must be the same"))	
		
		}
	}
	
	
	if (nus != env_obj$nstates || nust != env_obj$nstates) {
		print(paste("Observed/true data has", nus, "and", nust, "behaviors, but choose to model with", env_obj$nstates, "behaviors"))
	}
		
	invisible(NULL)
}