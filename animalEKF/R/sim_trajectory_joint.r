interp_trajectory_joint <- function(d, nstates, one_d, dt_lnorm_mu=5, dt_lnorm_sd=1, dt_vals=NULL, centroids=matrix(c(0,0), ncol=2)) {

		ndim <- length(dim(d))
		
		speed_varname <- ifelse(one_d, "velocity", "speed")
		#multiple sharks
		if (ndim==3) {
			shark_names <- dimnames(d)[[ 3 ]] 
			time_range <- range(d[,"date_as_sec", ], na.rm=TRUE)
		}
		else {
			shark_names <- sort(unique(d[,"tag"]))
			time_range <- range(d[,"date_as_sec"], na.rm=TRUE)
		}	
		
		nsharks <- length(shark_names)
		N <- dim(d)[ 1 ]-1
		if (is.null(one_d)) { one_d <- ("Y" %in% dimnames(d)[[ 2 ]]) }

		#if (! ("t_intervals" %in% colnames(d))) {  t_intervals <- rep(NA, nrow(d)) ;  d <- cbind(d, t_intervals=t_intervals) }
		#if (! ("region" %in% colnames(d))) {  region <- rep(NA, nrow(d)) ;  d <- cbind(d, region=region) }
		
		

		#generate as many irregular observations as can be covered by regular intervals
		
		time_span <- diff(time_range)
		
		#if interpolate to regular
		
		#if use fixed values
		adjust_dt_vec_to_span <- function(dtv, time_span) {

			# extend if necessary to cover the time span		
			dtv <- rep(dtv, ceiling(time_span/sum(dtv)))
			
			#trim if necessary
			cs <- cumsum(dtv)
			highest <- min(which(cs >= time_span))
			dtv <- dtv[ 1:highest ]
			
			#if only one
			len <- length(dtv)
			if (len==1) {
				dtv <- time_span
			}
			else {
				dtv[ len ] <- time_span - sum(dtv[ 1:(len-1) ])
			}
			
			dtv
		}
	
		if (! is.null(dt_vals))  {

		  dt_vals <- adjust_dt_vec_to_span(dtv=dt_vals, time_span=time_span)
			
			print("interpolating with given time gaps:")
			print(dt_vals)
			
			#now make into list
			
			
			t_obs <- time_range[1] + cumsum(c(0,dt_vals))
			t_obs <- rep(list(t_obs), nsharks)
			
			#5 is a placeholder
			t2next <- rep(list(c(dt_vals,5)), nsharks)
			names(t2next) <- names(t_obs) <- shark_names
			
			
		}	
		else if (dt_lnorm_sd==0) {

		  emu <- round(exp_safe(dt_lnorm_mu), digits=5)
			print(paste("interpolating with regular timestep approx", round(emu, digits=1)))
			
			t2next <- adjust_dt_vec_to_span(dtv=emu, time_span=time_span)
			
			#dont add 1 so doesn't go above right interval
			t2next <- rep(emu, ceiling(time_span/emu ))
			t_obs <- time_range[1] + cumsum(c(0,t2next))
			t_obs <- rep(list(t_obs), nsharks)

			t2next <- rep(list(c(dt_vals,5)), nsharks)
			names(t2next) <- names(t_obs) <- shark_names
			#t_obs <- t_obs[ -length(t_obs) ]
			#t2next <- rep(list(t2next), nsharks)
			#t_obs <- rep(list(t_obs), nsharks)
			#names(t2next) <- names(t_obs) <- shark_names					
			
		}
		else {

		  #if interpolate to irregular
			t2next <- rep(list(c()), nsharks)
			t_obs <- rep(list(time_range[1]), nsharks)
			names(t2next) <- names(t_obs) <- shark_names
			
		
			for (s in shark_names) {
			
				max_time <- ifelse(ndim==3, max(d[,"date_as_sec", s], na.rm=TRUE), max(d[ d[,"tag"]==s,"date_as_sec"], na.rm=TRUE))
			
				while(t_obs[[ s ]][ length(t_obs[[ s ]]) ] < max(d[,"date_as_sec", s], na.rm=TRUE)) {
				
					#t2next <- c(t2next, 5+rgamma(n=1, shape=gamma_shape, rate=gamma_rate))
					t2next[[ s ]] <- c(t2next[[ s ]], pmax(5, stats::rlnorm(n=1, meanlog=dt_lnorm_mu, sdlog=dt_lnorm_sd)))
					t_obs[[ s ]][ length(t_obs[[ s ]])+1 ] <- t_obs[[ s ]][ length(t_obs[[ s ]]) ] + t2next[[ s ]][ length(t2next[[ s ]]) ]
				
				}
				#make the last observation at the end time, and adjust the time gap accordingly
								
				last_obs <- length(t_obs[[ s ]])
				t_obs[[ s ]][ last_obs ] <-  max_time
				t2next[[ s ]][ last_obs-1 ] <- diff(t_obs[[ s ]][ (last_obs-1):last_obs ])
				#make last one arbitrary since doesn't matter
				t2next[[ s ]] <- c(t2next[[ s ]], 5)
			
			}
		}
		
		#number of irregular obs per shark
		shark_nobs_di <- sapply(t_obs, length)
		print(shark_nobs_di)
		shark_tags <- rep(shark_names, shark_nobs_di) 
		
		di <- matrix(NA, nrow=sum(shark_nobs_di), ncol=dim(d)[ 2 ])
		
		colnames(di) <- dimnames(d)[[ 2 ]]
		
		di[,c("date_as_sec","time_to_next")] <- cbind(unlist(t_obs), unlist(t2next))
		
		#dont interpolate to regular
		
		di[,"t_intervals"] <- as.numeric(as.character(cut(x=di[,"date_as_sec"], breaks=c(d[1,"date_as_sec", 1]-100, d[-1,"date_as_sec", 1]), labels=1:(N), right=TRUE)))  

		#set the states the same as the intervals they fall in 
		for (s in shark_names) {
		
		  st <- di[shark_tags==s,"t_intervals"] #irregular intervals, select those rows from regular dataset
			di[shark_tags==s, "state.guess2"] <- as.numeric(d[ st, "state.guess2", s])
			
			
			#now need to get next states observed by irregular.
			st_omit_first <- st[ st > min(st) ]
			st_omit_last <- st[ st < max(st) ]
		
			tab_st <- as.vector(table(st_omit_last)) #get number of obserations of each
			next_obs_ind <- rep(unique(st_omit_first), times=tab_st)
			
			
			di[shark_tags==s , "next.guess2"][ st < max(st) ] <- as.numeric(d[next_obs_ind, "state.guess2", s])  
			#di[shark_tags==s, "state.guess2"] <- as.numeric(d[ di[shark_tags==s,"t_intervals"], "state.guess2", s])
			
			if (one_d) { sv <- c("velocity") }
			else { sv <- c("speed", "log_speed") }
			
			di[shark_tags==s, sv] <- as.matrix(d[ di[shark_tags==s, "t_intervals"], sv, s])

			t_intervals <- unique(di[ shark_tags==s, "t_intervals" ])
			t_intervals <- t_intervals[ ! is.na(t_intervals) ]
			#if (is.null(reg_dt)) {	t_intervals <- 	t_intervals[ t_intervals < N+1 ] }
			
			for (tt in t_intervals) {
				#if (is.null(reg_dt)) { delta_t <- diff(d[c(tt,tt+1), "date_as_sec", s]) }
				#else { delta_t <- reg_dt }

			  isteps <- which(di[,"t_intervals"] == tt & shark_tags==s)
				
				#fractions
				j <- as.vector((di[isteps,"date_as_sec"] - d[tt,"date_as_sec", s])/diff(d[c(tt,tt+1), "date_as_sec", s]))
				
				if (one_d) {
					change <- matrix(d[tt, "velocity", s]*d[tt, "time_to_next", s], ncol=1, nrow=length(isteps))
					colnames(change) <- "X"
				}
				else {
					change <- matrix(d[tt, "speed", s]*d[tt, "time_to_next", s]*c(cos(d[tt, "bearing.to.east.tonext.rad", s]), sin(d[tt, "bearing.to.east.tonext.rad", s])), ncol=2, nrow=length(isteps), byrow=TRUE)
					colnames(change) <- c("X","Y")
				}
				
				#gives you euclidean distance
				change <- apply(change[,,drop=FALSE], 2, function(x) x*j)
			
			
				if (one_d) {
					di[ isteps,"X"] <- d[rep(tt, length(isteps)),"X", s] + change
				}
				else {
									
					tmp <- matrix(d[rep(tt, length(isteps)),c("X","Y"), s], ncol=2) + change
					colnames(tmp) <- c("X","Y")
					
					di[isteps,"X"] <- tmp[,"X"]
					di[isteps,"Y"] <- tmp[,"Y"]
					
				}
			
			}#loop over intervals

			di[ which(shark_tags==s)[ -shark_nobs_di[ s ]], "dx_to_next"] <- di[which(shark_tags==s)[ -1 ], "X"] - di[which(shark_tags==s)[ -shark_nobs_di[ s ]], "X"]
			
			shark_obs <- which(shark_tags==s)
			di[shark_obs[1], "time_in_state"] <- 0
			for (ii in 2:length(shark_obs)) {
				di[shark_obs[ii], "time_in_state"] <- ifelse(di[shark_obs[ii], "state.guess2"] == di[shark_obs[ii - 1], "state.guess2"], di[shark_obs[ii - 1], "time_in_state"] + diff(di[c(shark_obs[ii - 1], shark_obs[ii]), "date_as_sec"]), 0)
			}
		
		
			if (! one_d) {
				di[which(shark_tags==s)[ -shark_nobs_di[ s ]], "dy_to_next"] <- di[which(shark_tags==s)[ -1 ], "Y"] - di[which(shark_tags==s)[ -shark_nobs_di[ s ]], "Y"]
			}

			#di <- di[ -nrow(di),]
			
			if (! one_d) {
				# d_to_next will be Euclidean distance, which must be positive
				d_to_next <- as.numeric(apply(di[ shark_tags==s,c("dx_to_next","dy_to_next")], 1, function(x) sqrt(sum(x^2))))
			}
			else {
				# d_to_next will be the difference in positions, which can be positive or negative
				d_to_next <- as.numeric(c(diff(di[shark_tags==s,"X"]), prod(di[which(shark_tags==s)[ shark_nobs_di[ s ]], c("time_to_next","velocity")])))
				di[ shark_tags==s,"region"] <- 1
			}
						
			#print(di[,"time_to_next"])
			di[shark_tags==s, speed_varname] <- d_to_next/di[shark_tags==s, "time_to_next"]

			
			#di[ st[ -length(st) ] , "next.guess2"] <- di[ st[ -1 ], "state.guess2"]
			
			if (one_d == FALSE) { 
				di[shark_tags==s, "log_speed"] <- log_safe(di[shark_tags==s, "speed"]) 
			
				#turn angles use atan2
				di[,"bearing.to.east.tonext.rad"] <- atan2(y=di[,"dy_to_next"], x=di[,"dx_to_next"])
				di[which(shark_tags==s)[ -c(1, shark_nobs_di[ s ]) ], "turn.angle.rad"] <- diff(di[which(shark_tags==s)[ -shark_nobs_di[ s ] ], "bearing.to.east.tonext.rad"])
				di[ shark_tags==s,"bearing.to.east.tonext.rad"] <- normalize_angle(di[ shark_tags==s, "bearing.to.east.tonext.rad"])
				di[ shark_tags==s,"turn.angle.rad"] <- normalize_angle(di[ shark_tags==s,"turn.angle.rad"])
				
				#di[shark_tags==s, "turn.angle.rad.logtrans"] <- rad2log(di[shark_tags==s, "turn.angle.rad"])
				if (nrow(centroids)>0) { 
					di[shark_tags==s,"region"] <- apply(di[shark_tags==s, c("X","Y"), drop=FALSE], 1, function(x) which_region(newcoord=x, centroid=centroids))
				}
			}
		
			#if want to get rid of interpolation points 
				
			
		}#loop over sharks
		di[,"lambda"] <- as.numeric(di[,"state.guess2"])-1
		
		di <- as.data.frame(di)
		
		di$tag <- shark_tags
		di$next.guess2 <- factor(di$next.guess2, levels=1:nstates)
		di$state.guess2 <- factor(di$state.guess2, levels=1:nstates)
		
		# sort by time and then shark names
		di <- di[order( di$date_as_sec, di$tag ),]

		
		invisible(di)

		
}		


sim_trajectory_joint <- function(area_map=NULL, centroids=matrix(c(0,0), ncol=2), 
                           transition_matrices=list(matrix(c(10,3,2,9), ncol=2, byrow=TRUE)),
                           mu0_pars=list(alpha=c(-4 ,-1.6), beta=c(0,0)), var0_pars=list(alpha=c(1.6,0.16), beta=c(2,.5)), 
						   N=100, nstates=2, reg_dt=120, gen_irreg=TRUE, one_d=FALSE, dt_lnorm_mu=log(120), dt_lnorm_sd=1,
						   dt_vals=NULL, starting_polygon=area_map, nsharks=1, interact=FALSE, 
						   interact_pars=list(interacting_sharks=c(1:nsharks), time_radius=60*30, spat_radius=200, min_num_neibs=10,
						   eta_mu=c(2,1), rho_sd=c(0.75, 0.75)), time_dep_trans=FALSE, trans_alpha=c(1, 1.5)) {


								
						   
						   
	nstates <- max(1, nstates)
	shark_names <- paste("GSH", 1:nsharks, sep="")
	
	if (nstates==1 & ! is.null(transition_matrices)) { 
	   print("Only simulating with one behavioral state")
	}
	
	if (! one_d) {
	
		# first check if a map exists
		if (is.null(area_map)) {
			if (is.null(starting_polygon)) {
				# create a default shapefile
				print("creating a default rectangular shapefile within which to simulate")
				area_map <- sf::st_geometry(rectangular_shapefile())
				starting_polygon <- area_map
			}
			else {
				print("Using the provided starting_polygon as area_map")
				area_map <- starting_polygon
			}
			
		}
		
	
		# only if 2-D
		if (all(sf::st_is_empty(area_map))) {
			stop('area_map geometry is empty')
		}

		if (all(sf::st_is_empty(starting_polygon))) {
			stop('starting_polygon geometry is empty')
		}
		
		if (! any(binary_A_intersects_B(sf::st_geometry(starting_polygon), sf::st_geometry(area_map)))) {
			stop('starting polygon does not intersect with area_map')
		}
		else {
			# make sure is in map
			starting_polygon <- sf::st_intersection(sf::st_geometry(area_map), sf::st_geometry(starting_polygon))
		}
	}

	normalize_angle <- function(theta) {
	  #needed otherwise total lack of accuracy in modulus
	  theta <- pmin(pmax(theta, -1e15), 1e15)
	  theta <- sign(theta)*(abs(theta)%%(2*pi))
	  if (any(theta < -pi)) {  theta[ theta < -pi ] <- theta[ theta < -pi] +(2*pi) }
	  if (any(theta >  pi)) {  theta[ theta >  pi ] <- theta[ theta > pi] -(2*pi) }
	  theta
	}

    #automatic definition for euclidean distance regardless of dimension
    euc_dist <- function(center, otherXY, is_one_d=one_d) {
		if (is_one_d) { 
			diz <-  abs(center - otherXY) 
		}
		else {
			diz <- dist_func(center=center, otherXY=otherXY)
		}
		diz
	}
	
		
	
	if (one_d) { 
		nregions <- 1
		loc_variables <- c("X")
		
		speed_varname <- "velocity"
		
	}	
	else {
		nregions <- nrow(centroids)
		loc_variables <- c("X","Y")
		
		speed_varname <- "log_speed"
				
					
		if (length(centroids) == 0) {
			nregions <- 1
		}
	}
	
    if (nstates > 1) {
		
		if (! is.list(transition_matrices)) {
			transition_matrices <- list(transition_matrices)
		}
		
		tmat_rows <- sapply(transition_matrices, nrow)
		tmat_cols <- sapply(transition_matrices, ncol)
		
		if ((! all(tmat_rows == nstates)) | (! all(tmat_rows == nstates))) {
			print(transition_matrices)
			stop(paste("If simulating with nstates == ", nstates, ", all transition matrices must be of that dimension square"))
		
		}
		
		if (one_d) {
			transition_matrices <- list(transition_matrices[[ 1 ]])
		}	
		
	
		beta_means <- sapply(transition_matrices, function(x) sum(x[,1])/sum(x))
		#print(beta_means) 
		transition_matrices <- rep(transition_matrices, nregions)[ 1:nregions ]
		transition_matrices <- lapply(transition_matrices, function(x) t(apply(x,1, function(y) y/sum(y))))
			
	}
	else { 
	    transition_matrices <- rep(list(matrix(1, ncol=1,nrow=1)), nregions)
		interact <- FALSE
	}

	# if (nsharks==1) {
		# interact <- FALSE
	# }
	
	if (time_dep_trans) {
		trans_alpha <- matrix(trans_alpha, ncol=nstates, nrow=nregions, byrow=TRUE)
	}
	
	
	if (interact) {
		#sharks that dont interact with others, but influence interacting sharks
		non_interacting_sharks <- shark_names[ -interact_pars$interacting_sharks ]
		interacting_sharks <- shark_names[ interact_pars$interacting_sharks ]
		
		interact_pars$eta_mu <- rep_len(interact_pars$eta_mu, length.out=length(interacting_sharks))
		if (length(non_interacting_sharks)) { interact_pars$eta_mu <- c(interact_pars$eta_mu, rep(0, length(non_interacting_sharks))) }
		
		names(interact_pars$eta_mu) <- c(interacting_sharks, non_interacting_sharks)
		interact_pars$eta_mu <- interact_pars$eta_mu[ order(names(interact_pars$eta_mu)) ]
		print("Interaction mu parameters:")
		print(interact_pars$eta_mu)		
		interact_pars$rho_sd <- rep_len(interact_pars$rho_sd, length(interacting_sharks))
		names(interact_pars$rho_sd) <- interacting_sharks
		
		#number of previous steps to consider
		interact_pars$time_radius <- max(0, interact_pars$time_radius)
		time_radius_nsteps <- floor(interact_pars$time_radius/reg_dt)
		
		if (time_radius_nsteps==0) {
			#window too short to consider
			interact <- FALSE
			non_interacting_sharks <- shark_names
		}	
		
	}
	else {
		#otherwise just all sharks
		non_interacting_sharks <- shark_names
	}
    
	if (one_d) {
		d <- array(NA, dim=c(N+1, 11, nsharks),
					dimnames=list(1:(N+1),
								  c("X","velocity","date_as_sec","state.guess2","lambda","next.guess2","time_to_next","dx_to_next","t_intervals","region","time_in_state"),
								  shark_names))
									
    }
	else {
		
		d <- array(NA, dim=c(N+1, 16, nsharks),
					dimnames=list(1:(N+1),
								  c("X","Y","log_speed","date_as_sec","speed","turn.angle.rad","bearing.to.east.tonext.rad","state.guess2","lambda","next.guess2","time_to_next","region","dx_to_next","dy_to_next","t_intervals","time_in_state"),
									shark_names))
	
    }
	
	d[ 1, "time_in_state",] <- 0	
		
	if (nstates>1) { d[1,"state.guess2",] <- sample(1:nstates, size=nsharks, replace=TRUE) }
	else { d[,"state.guess2",] <- 1 }
    
	if (! one_d) { 
	
		#may want to select a smaller area within which to find the initial points
		# starting_polygon <- rgeos::gIntersection(area_map, starting_polygon)
		
		for (s in shark_names) {		
			d[1,c("X","Y"),s] <- as.vector(sf::st_coordinates(sf::st_sample(starting_polygon, size=1, type="random", exact=TRUE))[1,])
		}	
		d[1,"bearing.to.east.tonext.rad",] <- runif(n=nsharks, min=-pi+ 1e-3, max=pi- 1e-3)
		
	
	}
	else { 
		d[1,"X",] <- 0 
	}
	
		
	if (nregions>1) { d[1,"region",] <- apply(d[1,c("X","Y"),,drop=FALSE], 3, function(x) which_region(newcoord=x, centroid=centroids)) }
	else { d[,"region",] <- 1 }
	
	
	#simulate regular observations, then create irregular one by drawing random times and calculating other stuff  
	
	if (! is.null(reg_dt)) {	
		d[,"time_to_next",] <- reg_dt
		d[,"t_intervals",] <- 1:(N+1)
	}
	else {
		d[,"time_to_next",] <- pmax(5, stats::rlnorm(n=nsharks*(N+1), meanlog=dt_lnorm_mu, sdlog=dt_lnorm_sd))
		gen_irreg <- FALSE
	}

	d[,"date_as_sec",] <- apply(d[,"time_to_next",,drop=FALSE], 3, cumsum) - d[1,"time_to_next",]
	d[,"time_in_state",] <- 1

	z <- d[1,"state.guess2",]
	
	for (tt in 1:N) {
	
		if (tt>1 & interact==TRUE) {
			temp_neib_range <- max(1, tt-time_radius_nsteps):(tt-1)
		}
				
		
		for (s in shark_names) {
			
			
			if (nstates>1 & tt>1) {
				
				
				z_prev <- d[tt-1, "state.guess2", s]
				z_oth <- which(1:nstates != z_prev)
				prev_region <- d[tt-1,"region",s]
				
				if (time_dep_trans) {
					
					pleave <- rbeta(n=1, shape1=trans_alpha[prev_region, z_prev ]*d[tt-1,"time_to_next",s], shape2=1)
					
					transition_matrices[[ prev_region ]][ z_prev, c(z_prev, z_oth)] <- c(1-pleave, pleave)
				
				}
				
				
				#just draw randomly
				if (s %in% non_interacting_sharks) {
					d[ tt,"state.guess2", s] <- z <- sample(1:nstates, size=1, prob=transition_matrices[[ prev_region ]][ z_prev,])	
				}
				else {
					
					other_sharks <- shark_names[ shark_names != s ]
					possible_prev_temp_neibs <- d[temp_neib_range, c(loc_variables,"state.guess2"), other_sharks , drop=FALSE]
					
					neibs <- c()
					
					if (prod(dim(possible_prev_temp_neibs)[ c(1, 3) ]) >= interact_pars$min_num_neibs ) {
						for (ss in other_sharks) {
						
							#calculate distances according to 1d or 2d, as appropriate
							neib_obs <- euc_dist(center=d[ tt, loc_variables, s], otherXY=possible_prev_temp_neibs[, loc_variables, ss])							
							
							#neighboring behaviors	
							neibs <- c(neibs, possible_prev_temp_neibs[ neib_obs < interact_pars$spat_radius, "state.guess2", ss] )
						}	
					}
					if (length(neibs) >= interact_pars$min_num_neibs ) {
					
						#use intensity to draw behavior based on fractions
						neib_fracs <- table(factor(neibs, levels=1:nstates))
						neib_fracs <- neib_fracs/length(neibs)
						#only rescale the P(foraging) since that's what matters
						rho_intensity_draw <- rlnorm(n=1, meanlog=neib_fracs[ 1 ]*interact_pars$eta_mu[ s ], sdlog=interact_pars$rho_sd[ s ]) 
							
					}
					else {
						rho_intensity_draw <- 1
					}
						
					#now rescale by interaction
						
						
					d[ tt,"state.guess2", s] <- z <- sample(1:nstates, size=1, prob=transition_matrices[[ prev_region ]][ z_prev,]*c(rho_intensity_draw, 1) )	
					
					
				}	
				
				#number of regular steps spent in that state
				d[ tt, "time_in_state", s] <- ifelse( d[ tt, "state.guess2", s]==d[ tt-1, "state.guess2", s], d[ tt-1, "time_in_state", s] + 1, 1)  
				
			}#finish drawing the behavior
			
			
			inside <- FALSE
						
			while (! inside) { 
			
				d[tt,speed_varname, s] <- rnorm(n=1, mean=mu0_pars$alpha[ z ], sd=sqrt(var0_pars$alpha[ z ]))
				
				if (! one_d) {	
					
					d[tt,"speed", s] <- exp_safe(d[tt,speed_varname, s])
				
					if (tt>1) {
						
						#d[tt,"turn.angle.rad", s] <- normalize_angle(CircStats::rvm(n=1, mean=turn_pars$mu0[ z ], k=turn_pars$kappa0[ z ])) 
						d[tt,"turn.angle.rad", s] <- normalize_angle(rnorm(n=1, mean=mu0_pars$beta[ z ], sd=sqrt(var0_pars$beta[ z ]))) 
						d[tt, "bearing.to.east.tonext.rad", s] <- normalize_angle(d[tt-1, "bearing.to.east.tonext.rad", s] + d[tt, "turn.angle.rad", s])
						   
					}
								
					#new coordinate
					d[tt+1, c("X","Y"), s] <- d[tt, c("X","Y"), s] + d[tt, "speed", s]*d[tt, "time_to_next", s]*c(cos(d[tt, "bearing.to.east.tonext.rad", s]), sin(d[tt, "bearing.to.east.tonext.rad", s]))
						
					# new_traj <- sp::SpatialLines(list(sp::Lines(sp::Line(d[tt:(tt+1), c("X","Y"), s]), ID=1)))
					# new_traj@proj4string <- area_map@proj4string 
					new_traj <- sf::st_linestring(d[tt:(tt+1), c("X","Y"), s])
					# inside <- rgeos::gContains(area_map, new_traj, byid=FALSE)
					inside <- binary_A_within_B(new_traj, area_map)
					
					if (nregions>1) { d[tt+1, "region", s] <- which_region(d[tt+1, c("X","Y"), s], centroids) }
						
				}
				else {
					inside <- TRUE
					d[tt+1,"X", s] <- d[tt,"X", s] + d[tt,"velocity", s]*d[tt, "time_to_next", s]
					#d[tt,"dx_to_next"] <- d[tt+1,"X"] - d[tt,"X"]
				}
			}

		}#loop over sharks

				
	}#loop over times
	
	#multiply by time step
	d[,"time_in_state",] <- d[,"time_in_state",] * reg_dt 
		
	
	
	if (gen_irreg) {

		di <- interp_trajectory_joint(d=d, nstates=nstates, one_d=one_d, dt_vals=dt_vals,
								dt_lnorm_mu=dt_lnorm_mu, dt_lnorm_sd=dt_lnorm_sd, centroids=centroids)
	
	}#create irregular
	
	
	d[ -nrow(d), "dx_to_next",] <- d[-1, "X",] - d[-nrow(d), "X",]
	
	
	if (one_d == FALSE) {
		d[-nrow(d), "dy_to_next",] <- d[-1, "Y",] - d[-nrow(d), "Y",]
	}
	
	
	d <- d[1:N,,,drop=FALSE]

	if (nstates > 1) {
		
		
		d[-nrow(d),"next.guess2",] <- d[-1,"state.guess2",]
		
		
		# true_foraging_prob <- matrix(NA, nrow=nsharks, ncol=nregions + 1)
		# colnames(true_foraging_prob) <- c(paste("region", 1:nregions),"all")
		# rownames(true_foraging_prob) <- shark_names
		
		# true_transition_prob <- matrix(NA, ncol=nregions + 2, nrow=nstates * nsharks)
		# colnames(true_transition_prob) <- c(paste("region", 1:nregions),"all","type")
		# rownames(true_transition_prob) <- rep(shark_names, each=2)
		# true_transition_prob[,"type"] <- rep(c(12,21), nsharks)
		
		# for (s in shark_names) {
			# tmp <- table(factor(d[,"state.guess2", s], levels=1:nstates), factor(d[,"region", s], levels=1:nregions))
			# true_foraging_prob[ s, "all"] <- sum(tmp[1,])/sum(tmp)
			# true_foraging_prob[ s, 1:nregions] <- tmp[1,]/colSums(tmp)			
				
			# trans_tab <- table(factor(d[,"state.guess2", s], levels=1:nstates), factor(d[,"next.guess2", s], levels=1:nstates), factor(d[,"region", s], levels=1:nregions))
			# trans_tab[1,,] <- trans_tab[1,nstates:1,]
			
			# for (rr in 1:nregions) {
				# true_transition_prob[rownames(true_transition_prob)==s,rr] <- apply(trans_tab[,,rr,drop=FALSE], 1, function(x) x[1]/sum(x))
			# }	
			
			
			# #overall transitions
			# if (nregions==1) {
				# true_transition_prob[rownames(true_transition_prob)==s ,"all"] <- true_transition_prob[rownames(true_transition_prob)==s,1]
			# }
			# else { 
				# all_regions <- table(factor(d[,"state.guess2", s], levels=1:nstates), factor(d[,"next.guess2", s], levels=1:nstates))
				# all_regions[1,] <- all_regions[1,nstates:1]
				# true_transition_prob[rownames(true_transition_prob)==s,"all"] <- apply(all_regions, 1, function(x) x[1]/sum(x))
			# }
			
		
		# }		
	}#transition and foraging probabilities

	if (nsharks==1) { 
		d_ds <- as.data.frame(d[,,shark_names[ 1 ]]) 
		d_ds$tag <- shark_names[ 1 ]
		d_ds$date <- as.Date(as.POSIXlt(d_ds$date_as_sec, origin="1970/01/01"))
		
	}
	else { 
	
		d_ds <- as.data.frame(apply(d, 2, rbind))
		d_ds$tag <- factor(rep(shark_names, each=N), levels=shark_names)
		d_ds$date <- as.Date(as.POSIXlt(d_ds$date_as_sec, origin="1970/01/01"))
		d_ds <- d_ds[ order(d_ds$date_as_sec),]
	}


	d_ds$lambda <- d_ds$state.guess2 -1		
	d_ds$next.guess2 <- factor(d_ds$next.guess2, levels=1:nstates)
	d_ds$state.guess2 <- factor(d_ds$state.guess2, levels=1:nstates)
	
	

	tmp <- list(d=d, d_ds=d_ds)
	if (gen_irreg) { 
		tmp[[ "di" ]] <- di
	}	
	if (nstates > 1) {
		tmp0 <- true_behavior_probabilities(reg_step_ds=d_ds, shark_names=shark_names, nstates=nstates, nregions=nregions) 

		tmp[[ "true_foraging_prob" ]] <- tmp0$true_foraging_prob
		tmp[[ "true_transition_prob" ]] <- tmp0$true_transition_prob
	}
	if (time_dep_trans) {
		tmp[[ "trans_alpha" ]] <- trans_alpha
	}
	if (interact) {
		tmp[[ "interact_pars"]] <- interact_pars
	}

	
	invisible(tmp)
	
   
      
}
         
         
          
      
      
      
