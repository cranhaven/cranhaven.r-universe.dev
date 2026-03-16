fix_data_EKF_interp_joint <- function(env_obj) {

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

	if (length(env_obj$logvelocity_truncate) != 2) {
		print(env_obj$logvelocity_truncate)
		stop("logvelocity_truncate must be of length 2")
	}
	
	if (diff(env_obj$logvelocity_truncate[1:2]) <= 0) {
		print(env_obj$logvelocity_truncate)
		stop("The second value of logvelocity_truncate must be strictly > the first")
	}
		
	# truncation options
	if (is.null(env_obj$area_map)) {
		env_obj$truncate_to_map <- FALSE
	}

	if (env_obj$truncate_to_map == FALSE) {
		env_obj$do_trunc_adjust <- FALSE
		env_obj$enforce_full_line_in_map <- FALSE
	}
	
	
	# make sure if truncate, that all observed locations are inside the map
	if (env_obj$truncate_to_map) {
	
		# observed_points <- sp::SpatialPoints(env_obj$d[,c("X","Y"),drop=FALSE])
		# observed_points@proj4string <- env_obj$area_map@proj4string   
 		# inside <- rgeos::gContains(env_obj$area_map, observed_points)
		observed_points <- sf::st_as_sf(env_obj$d[,c("X","Y"),drop=FALSE], coords=c("X", "Y"))
		sf::st_crs(observed_points) <- sf::st_crs(env_obj$area_map)
		inside <- binary_A_within_B(observed_points, env_obj$area_map)
		
		if (any(inside == FALSE)) {
			print("Truncation to map will be performed, but the following observed points were outside the map:")
			print(env_obj$d[inside == FALSE,,drop=FALSE])
			
		}
		
		# now truncate
		env_obj$d <- env_obj$d[inside,,drop=FALSE]
		
		# now do the same with true locations, if provided
		if (env_obj$compare_with_known) {
			# true_points <- sp::SpatialPoints(env_obj$known_regular_step_ds[,c("X","Y"),drop=FALSE])
			# true_points@proj4string <- env_obj$area_map@proj4string   
 
			# inside <- rgeos::gContains(env_obj$area_map, true_points)
			true_points <- sf::st_as_sf(env_obj$known_regular_step_ds[,c("X","Y"),drop=FALSE], coords=c("X", "Y"))
			sf::st_crs(true_points) <- sf::st_crs(env_obj$area_map)
			inside <- binary_A_within_B(true_points, env_obj$area_map)

			
			if (any(inside == FALSE)) {
				print("Truncation to map will be performed, but the following true points were outside the map:")
				print(env_obj$known_regular_step_ds[inside == FALSE,,drop=FALSE])
			
			}
			env_obj$known_regular_step_ds <- env_obj$known_regular_step_ds[inside,,drop=FALSE]
		}
	}
		
	

	# now conduct filtering on observations		
	
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

	max_required_steps <- ceiling(1+(env_obj$d$date_as_sec[ nrow(env_obj$d) ] - env_obj$first_time)/env_obj$reg_dt)

	env_obj$maxStep <- ifelse(is.null(env_obj$maxStep), max_required_steps, max(min_intervals_needed, min(env_obj$maxStep, max_required_steps)))
	

	env_obj$t_reg <- seq(from= env_obj$first_time, by=env_obj$reg_dt, length.out=env_obj$maxStep)
	
	#do this so the first interval captures the first observation at t=0
	env_obj$t_reg[ 1 ] <- env_obj$t_reg[ 1 ] - env_obj$reg_dt * 0.0001  #.Machine$double.eps

	env_obj$N <- length(env_obj$t_reg)

	env_obj$max_int_wo_obs <- min(env_obj$N+1, env_obj$max_int_wo_obs)
	
	env_obj$d <- env_obj$d[ env_obj$d$date_as_sec <= env_obj$t_reg[ env_obj$N ] ,]

	env_obj$tags <- env_obj$d$tag

	env_obj$included_intervals <- 1:(env_obj$N - 1)

	#calculate which regular step each observation falls into
	#we do right=TRUE so that if this is regular intervals, each observation is the next step (j= 1 rather than 0).
	env_obj$d$t_intervals <- as.numeric(as.character(cut(x=env_obj$d$date_as_sec, breaks=env_obj$t_reg, labels=env_obj$included_intervals, right=TRUE)))

	print("t_intervals")
	print(env_obj$d$t_intervals)

	env_obj$shark_names <- as.character(sort(unique(env_obj$tags)))
	print(paste("shark names are",paste(env_obj$shark_names, collapse=" ")))
	env_obj$shark_intervals <- list()
	env_obj$shark_valid_steps <- list()



	for (s in env_obj$shark_names) {
		env_obj$shark_intervals[[ s ]] <- unique(env_obj$d$t_intervals[ env_obj$d$tag==s ])

		#keep steps where there are less than a certain gap between observations 
		#excluded_steps <- env_obj$shark_intervals[[ s ]][ -1 ][ diff(env_obj$shark_intervals[[ s ]]) > max_int_wo_obs ]
		
		#env_obj$shark_intervals[[ s ]] <- c(env_obj$shark_intervals[[ s ]][ 1 ], env_obj$shark_intervals[[ s ]][ -1 ][ diff(env_obj$shark_intervals[[ s ]]) <= max_int_wo_obs ])
		
		#env_obj$shark_valid_steps[[ s ]] <- min(env_obj$shark_intervals[[ s ]]):max(env_obj$shark_intervals[[ s ]])
		#env_obj$shark_valid_steps[[ s ]] <- env_obj$shark_valid_steps[[ s ]][ ! env_obj$shark_valid_steps[[ s ]] %in% excluded_steps ]
		#env_obj$shark_valid_steps[[ s ]] <- env_obj$shark_valid_steps[[ s ]][ ! env_obj$shark_valid_steps[[ s ]] %in% excluded_steps ]
	  
	  
		if (length(env_obj$shark_intervals[[ s ]]) > 1) {
	  
			
			tmp <- c()
			tmp1 <- c()
		
	  
			print(env_obj$shark_intervals[[ s ]])
			for (jj in 1:(length(env_obj$shark_intervals[[ s ]])-1)){
				if(diff(env_obj$shark_intervals[[ s ]][ jj:(jj+1) ]) <= env_obj$max_int_wo_obs) {
					tmp <- c(tmp, (env_obj$shark_intervals[[ s ]][ jj ]):(env_obj$shark_intervals[[ s ]][ jj+1 ]))
					tmp1 <- c(tmp1, env_obj$shark_intervals[[ s ]][ jj:(jj+1) ])  
				}
			}
			
			#valid intervals to simulate for
			env_obj$shark_valid_steps[[ s ]] <- sort(unique(tmp))
			#only the intervals with observations
			env_obj$shark_intervals[[ s ]] <- sort(unique(tmp1))
			
		}
		else {
			env_obj$shark_names[ env_obj$shark_names==s ] <- NA
		}
		
	}

	env_obj$shark_names <- env_obj$shark_names[ ! is.na(env_obj$shark_names) ]

	#env_obj$shark_intervals <- env_obj$shark_intervals[ sapply(env_obj$shark_intervals, function(x) length(x) >1) ]
	#env_obj$shark_names <- names(env_obj$shark_intervals)
	env_obj$nsharks <- length(env_obj$shark_names)
	#env_obj$shark_valid_steps <- env_obj$shark_valid_steps[ env_obj$shark_names ]

	env_obj$included_intervals <- sort(unique(unlist(env_obj$shark_valid_steps))) 

	print("valid observations per shark:")
	print(env_obj$shark_valid_steps)

	print(paste("sharks:", paste(env_obj$shark_names, collapse=" ")))


	env_obj$first_intervals <- lapply(env_obj$shark_valid_steps, function(x) x[ !((x-1) %in% x) ])


	names(env_obj$first_intervals) <- env_obj$shark_names
	print("starting observations per shark:")
	print(env_obj$first_intervals)

	#last interval with a valid observation
	env_obj$shark_final_obs <- sapply(env_obj$shark_intervals, max)
	names(env_obj$shark_final_obs) <- env_obj$shark_names


	if (env_obj$nsharks==1) {
		env_obj$interact <- FALSE
	}

	if (env_obj$nstates==1) {
		env_obj$next_states <- env_obj$states <- rep(1, nrow(env_obj$d))
		env_obj$interact <- FALSE
	}

	
	print(paste("nstates:", env_obj$nstates))
	print(paste("interactions", env_obj$interact))

	if (env_obj$update_params_for_obs_only) {
		env_obj$update_eachstep <- FALSE		
	}
	
	
	#keep the initial interval, plus any that come after more than max_int_wo_obs observations
	#env_obj$first_intervals <- lapply(env_obj$shark_intervals, function(x) c(x[1], x[-1][ diff(x) > max_int_wo_obs ]) )
	#second interval
	#second_intervals <- sapply(env_obj$shark_intervals, function(x) x[2])

	#names(env_obj$first_intervals) <- env_obj$shark_namesenv_obj$
	#print("starting observations per shark:")
	#print(env_obj$first_intervals)

	env_obj$shark_symbols <- 1:env_obj$nsharks
	names(env_obj$shark_symbols) <- env_obj$shark_names

	#regions
	env_obj$XY <- as.matrix(env_obj$d[,c("X","Y")])

	env_obj$centroids <- as.matrix(env_obj$centroids, ncol=2)
	rownames(env_obj$centroids) <- NULL
	colnames(env_obj$centroids) <- NULL
	env_obj$regions <- apply(env_obj$XY[,,drop=FALSE], 1, function(x) which_region(x, centroid=env_obj$centroids))
	env_obj$nregions <- nrow(env_obj$centroids)

	env_obj$d <- env_obj$d[, colnames(env_obj$d) != "region" ]

	if(env_obj$nregions > 1) { env_obj$d <- cbind(env_obj$d, region=env_obj$regions) }
	else { env_obj$d <- cbind(env_obj$d, region=1) }


	env_obj$pnames <- paste("p", 1:env_obj$npart, sep="")
	env_obj$state_names <- paste("state", 1:env_obj$nstates, sep="")
	env_obj$rnames <- paste("r", 1:env_obj$nregions, sep="")
	env_obj$Nnames <- paste("N", 1:env_obj$N, sep="")

	#lookup vector for 11 12 21 22
	#trans_names <- 1:(env_obj$nstates^2)
	#names(trans_names) <- do.call(paste, c(expand.grid(1:env_obj$nstates,1:env_obj$nstates)[,2:1], sep=""))
	env_obj$trans_names <- as.character(do.call(paste, c(expand.grid(1:env_obj$nstates, 1:env_obj$nstates)[,2:1], sep="")))

	env_obj$d <- cbind(env_obj$d, shark_obs_index= NA)
	for (s in env_obj$shark_names) {
		ss <- which(env_obj$tags==s)
		env_obj$d[ss,"shark_obs_index"] <- 1:length(ss)
	}

	#print(d$shark_obs_index)
	#if (env_obj$nsharks>1) { print(env_obj$tags) }  

	env_obj$d <- cbind(env_obj$d, rowid=1:nrow(env_obj$d))

	print("intervals with observations per shark:")
	print(env_obj$shark_intervals)


	print("intervals to be simulated per shark:")
	print(env_obj$shark_valid_steps)
	#drop env_obj$tags and convert to matrix for easier computation


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


	#env_obj$nstates <- max(length(unique(states)), env_obj$nstates)

	# print(sort(colnames(env_obj$d)))

	env_obj$d <- env_obj$d[,c("rowid","shark_obs_index","X","Y","log_speed","date_as_sec",
							   "speed","turn.angle.rad","bearing.to.east.tonext.rad","region","time_to_next","dx_to_next",
							   "dy_to_next","lambda","state.guess2","next.guess2","t_intervals")]


	# check logvelocity_truncate
	log_speed_extreme <- ! is.na(env_obj$d$log_speed) & ((env_obj$d$log_speed < env_obj$logvelocity_truncate[1]) | (env_obj$d$log_speed > env_obj$logvelocity_truncate[2]))
	if (any(log_speed_extreme)) {
		print("log-speed is restricted to:")
		print(env_obj$logvelocity_truncate)
		print("The following observations have log-speed outside of these ranges (large values are more of a concern)")
		print(env_obj$d[log_speed_extreme, c("X", "Y", "log_speed", "speed", "turn.angle.rad", "date_as_sec", "time_to_next", "dx_to_next", "dy_to_next", "region")])
	
	
	}
	

	rownames(env_obj$d) <- 1:nrow(env_obj$d)
	env_obj$d <- as.matrix(env_obj$d)
	
	env_obj$obs_XY_bbox <- apply(env_obj$d[,c("X","Y"), drop=FALSE], 2, function(x) range(x, na.rm=TRUE))
	env_obj$obs_XY_bbox[,"X"] <- env_obj$obs_XY_bbox[,"X"] + c(-0.5, 0.5)*diff(env_obj$obs_XY_bbox[,"X"])  
	env_obj$obs_XY_bbox[,"Y"] <- env_obj$obs_XY_bbox[,"Y"] + c(-0.5, 0.5)*diff(env_obj$obs_XY_bbox[,"Y"])  
	
	env_obj$obs_XY_bbox[1,"X"] <- max(env_obj$obs_XY_bbox[1,"X"], env_obj$bbox[1,1])
	env_obj$obs_XY_bbox[2,"X"] <- min(env_obj$obs_XY_bbox[2,"X"], env_obj$bbox[1,2])

	env_obj$obs_XY_bbox[1,"Y"] <- max(env_obj$obs_XY_bbox[1,"Y"], env_obj$bbox[2,1])
	env_obj$obs_XY_bbox[2,"Y"] <- min(env_obj$obs_XY_bbox[2,"Y"], env_obj$bbox[2,2])
	
	
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