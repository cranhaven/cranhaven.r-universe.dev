spline_interp <- function(di, area_map=NULL, t_reg=NULL, reg_dt=120, max_dt_wo_obs=60*30,
					maxStep=NULL, centroids=matrix(c(0,0), ncol=2), nstates=2, spline_deg=3, split_logv=-3) {
	
	obs_date_range <- range(di$date_as_sec, na.rm=TRUE)
	obs_date_span <- diff(obs_date_range)
	if (obs_date_span == 0) {
		stop("The observed date range contains only one value.  It needs at least two unique times")
	}

	
	if (!("tag" %in% colnames(di))) {  
		print("assuming all observations are for one animal")
		di$tag <- factor("GSH01")
	}
	if (!("date_as_sec" %in% colnames(di))) {
		stop("date_as_sec field needed.  See documentation.")
	}
	if (!("time_to_next" %in% colnames(di))) {
		print("calculating time_to_next field")
		di$time_to_next[ -nrow(di) ] <- diff(di$date_as_sec)
	}
	
	
	notna_obs <- apply(di[,c("tag","X","Y","date_as_sec","time_to_next"), drop=FALSE], 1, function(x) all(! is.na(x)))
	di <- di[ notna_obs, ]
	
	di <- di[ order(di$date_as_sec),]
	#print(head(di))
	
	
	if (is.null(t_reg)) {
		if (is.null(reg_dt)) {
			reg_dt <- obs_date_span
		}
		else {
			reg_dt <- min(reg_dt, obs_date_span)
		}
		t_reg <- seq(from=obs_date_range[1], to=obs_date_range[2], by=reg_dt)
		maxStep <- length(t_reg)
	}
	else {
		# allow to be interpolated at inputted t_reg
		
		t_reg <- t_reg[t_reg >= obs_date_range[1] & t_reg <= obs_date_range[2]]
		if (length(t_reg) == 0) {
			print(paste("No values of submitted t_reg are within the observed time range ", paste(obs_date_range, collapse="--")))
			t_reg <- obs_date_range
			reg_dt <- obs_date_span
		}
		else {
			t_reg <- unique(c(obs_date_range[1], t_reg))
			obs_reg_dt <- unique(diff(t_reg)) 
			if (length(obs_reg_dt) > 1) {	
				stop(paste("submitted t_reg argument has multiple observed time gaps:", paste(obs_reg_dt, collapse=", ")))
			}
			else {
				reg_dt <- obs_reg_dt
			}
			
			# if passes:
			t_reg <- unique(c(t_reg, obs_date_range[2]))
		}
		
		maxStep <- length(t_reg)
	}
	
	print("overall date range")
	print(as.Date(as.POSIXlt(range(t_reg), origin="1970/01/01")))
	

	
	di$tag <- factor(di$tag, levels=sort(unique(di$tag)))
	shark_names <- levels(di$tag)
	print(shark_names)
	nsharks <- length(shark_names)
	
	
	#do this so the first interval captures the first observation at t=0
	
	#t_reg[ 1 ] <- t_reg[ 1 ]- 1e-10
	#di$time_to_next <- NA
	#di$time_to_next[ -nrow(di) ] <- diff(di$date_as_sec)
	di$date_as_sec[ 1 ] <- di$date_as_sec[ 1 ] - 1e-10
	
	#add an extra regular step for coverage
	#if (max(t_reg) < max(di$date_as_sec)) { t_reg <- c(t_reg, max(t_reg)+reg_dt) }
	if (is.null(maxStep)) { maxStep <- length(t_reg) }
	if (maxStep < length(t_reg)) { t_reg <- t_reg[1:maxStep] }
	
	#di <- di[di$date_as_sec <= max(t_reg),]
	di$t_intervals <- as.numeric(as.character(cut(x=di$date_as_sec, breaks=t_reg, labels=1:(maxStep-1), right=TRUE)))
	
	
	#now make regular dataset
	d <- array(NA, dim=c(maxStep, 15, nsharks), dimnames=list(1:maxStep, 
		c("date_as_sec","time_to_next","X","Y","dist_to_next","angle_velocity","logvelocity","region","state.guess2","next.guess2","bearing.to.east.tonext.rad","turn.angle.rad","time_in_state","dx_to_next","dy_to_next"),
		shark_names))
	d[,"date_as_sec",] <- t_reg
    d[,"time_to_next",] <- reg_dt
	
	#first_date <- min(t_reg)
	#as.POSIXct(first_date,origin = "1970-01-01",tz = "GMT")
	#origin_sec <- as.numeric(as.POSIXct("2008-08-05 00:00:00 GMT",origin = "1970-01-01",tz = "GMT"))

	
		
	spline_deg <- max(2, spline_deg)

	splitAt <- function(x, pos) {
		out <- list()
		pos2 <- c(1, pos, length(x)+1)
		for (i in seq_along(pos2[-1])) {
			out[[i]] <- x[pos2[i]:(pos2[i+1]-1)]
		}
		return(out)
	}

	splitAtExclude <- function(x, pos) {
		out <- list()
		pos2 <- c(1, pos, length(x)+1)
		for (i in seq_along(pos2[-1])) {
			out[[i]] <- x[pos2[i]:(pos2[i+1]-1)]
		}
		out <- lapply(out, function(y) y[ !( y %in% x[ pos ]) ])
		#get rid of empty ones
		out <- out[ sapply(out, length) >0 ]
		
		return(out)
	}	
		
	euc_dist <- function(x) {
		sqrt(rowSums(matrix(apply(x, 2, diff)^2, ncol=2)))
	}
	
	for (s in shark_names) {
		 
		shark_obs <- which(di$tag == s)
		#print(di[ di$tag==s, c("tag","date","time_to_next")])
		#print(cbind(shark_obs, di$time_to_next[ shark_obs ]))
		nobs <- length(shark_obs)
		#print(summary(di$time_to_next[ shark_obs ]))
		
		d[ t_reg >= di$date_as_sec[ shark_obs[ 1 ] ] & t_reg < di$date_as_sec[ shark_obs[ nobs  ] ], "dist_to_next", s] <- 0 
		print(paste("Shark", s, nobs, "observations"))
		
		#if there was a large time gap, one observation at least is missing	
		contiguous_obs <- splitAtExclude(x=shark_obs,  pos=which( di$time_to_next[ shark_obs ] > max_dt_wo_obs ))
		print(s)
	
		#need to add the next observation to interpolate
		contiguous_obs <- lapply(contiguous_obs, function(x) c(x, shark_obs[ which(shark_obs== x[ length(x) ]) + 1 ] ))
		contiguous_obs <- lapply(contiguous_obs, function(x) x[ ! is.na(di$time_to_next[ x ]) ])
		
		contiguous_obs <- contiguous_obs[ sapply(contiguous_obs, length) > 1 ]
		
		
		#print(contiguous_obs)
		#need at least two observations to 
		#
		#total length has to be at least reg_dt (but not sufficient)
		#contiguous_obs <- contiguous_obs[ apply(sapply(contiguous_obs, range), 2, function(x) diff(di$time_to_next[ shark_obs ][ x ]) >= reg_dt ) ]
		
		#contiguous_obs <- contiguous_obs[ sapply(contiguous_obs, function(x) diff(di$time_to_next[ shark_obs ][ range(x) ]) >= reg_dt ) ]
		
		print(paste(length(unlist(contiguous_obs)),"observations used for spline"))
		
		#included regular intervals
		included_intervals <- lapply(contiguous_obs, function(x) which( t_reg >= di$date_as_sec[ x[ 1 ] ] & t_reg < di$date_as_sec[ x[ length(x) ] ]))
		
		#shark_obs <- sort(unlist(contiguous_obs))
		

		
		for (pp in 1:length(contiguous_obs)) {
		
			#make cubic splines
			obs_range <- range(contiguous_obs[[ pp ]])
			nobs_pp <- length(contiguous_obs[[ pp ]])
			#ncurves <- ceiling(nsteps/(spline_deg+1))
			
			#print(contiguous_obs)
			
			#generate t=0 to ncurves
				
			t_reg_range <- which(t_reg >= di$date_as_sec[ obs_range[ 1 ] ] & t_reg < di$date_as_sec[ obs_range[ 2 ] ])
			
			#print(range(t_reg_range))
			#print("obs")
			#print(contiguous_obs[[ pp ]])
			#print("obs index")
			#print(paste("obs index", which(shark_obs==obs_range[ 1 ]), "to", which(shark_obs==obs_range[ 2 ])))
			
			#print(di$time_to_next[ contiguous_obs[[ pp ]] ])
			#if (Inf %in% range(t_reg_range)){ print(t_reg_range) }
			
			#if no regular observations, calculate length and add to previous one
			#but need at least one irregular observation to make a spline
			
			#if there are no regular steps in a set of observations, then  do nothing
			
			# if (length(t_reg_range)==0 & length(contiguous_obs[[ pp ]]) >1  & pp >1) {
				# #print("yes")
				# #last observation in previous set
				# prev_t_reg <- max( unlist(included_intervals[ 1:(pp-1) ] ))
				# #print(prev_t_reg)	
					# if (! is.na(prev_t_reg)) {
						# #print("not used")
						# #if no observations, spline is just sum of Euclidean distances
						# d[ prev_t_reg, "dist_to_next", s] <- d[ prev_t_reg, "dist_to_next", s] + sum(euc_dist(di[ contiguous_obs[[ pp ]], c("X","Y"), drop=FALSE]))
					# }	
			# }
			if (length(t_reg_range)>1) {
			
				#print(s)
						
				#print(as.Date(as.POSIXlt(t_reg[ range(t_reg_range ) ], origin="1970/01/01")))
				#print("obs num")
				#print(contiguous_obs[[ pp ]])
				#print("time to next")
				#print(di$time_to_next[ contiguous_obs[[ pp ]] ])
				#print(as.Date(as.POSIXlt(di$date[  contiguous_obs[[ pp ]] ], origin="1970/01/01")))

				
				#need at least 2 regular intervals to do euclidean distance between them
				
				#each group of observations has tfrac length 1	
				
				start_pts <- seq(1, max(spline_deg, nobs_pp), by=spline_deg)
				#if just one curve then last start point should not be number of obs, since this would give you a single points
				if (length(start_pts>1)) { start_pts <- start_pts[ start_pts < nobs_pp ] }
				
				start_obs <- contiguous_obs[[ pp ]][ start_pts ]
							
				end_pts <- unique(c(seq(min(spline_deg+1, nobs_pp), nobs_pp, by=spline_deg), nobs_pp))
				end_obs <- contiguous_obs[[ pp ]][ end_pts ]
				
				
				ncurves <- length(start_obs)	
			
				
				#starting and ending observation coorrdinates
				first_obs_coords <- as.numeric(di[ obs_range[ 1 ], c("X","Y")])
				final_obs_coords <- as.numeric(di[ obs_range[ 2 ], c("X","Y")])
											
				t_fracs <- c(NA)
				
				#if you try to fit, say a cubic to 3 control points, the predicted points will be out of bounds 
				#(i.e. continue out further than the observations, since a cubic needs 4 points and the imaginary 4th point will be far away)
				#fit control points with spline_deg splines as much as possible, then do remaining segment with smaller one
				
				full_degree_curves <- which( (end_pts - start_pts) == spline_deg)
				nfull_degree_curves <- length(full_degree_curves)
				
				#if any full degree
				if (nfull_degree_curves > 0) {
												
					for (ii in 1:nfull_degree_curves) {
					
						t_reg_in_interval <- t_reg[ t_reg >= di$date_as_sec[ start_obs[ ii ] ] & t_reg < di$date_as_sec[ end_obs[ ii ] ] ]
						
						tmp <- c()
						if (length(t_reg_in_interval) >0) { 
							tmp <- (ii-1) + (t_reg_in_interval - di$date_as_sec[ start_obs[ ii ] ])/diff(di$date_as_sec[ c(start_obs[ ii ], end_obs[ ii ]) ])
							#print(length(t_reg_in_interval))
						}
														
						t_fracs <- c(t_fracs, tmp, NA)
				
					} 
				
					breakpoint_pos <- which(is.na(t_fracs))
					t_fracs[ is.na(t_fracs) ] <- c(0,full_degree_curves)
							
					spline_coords <- bezier::bezier(t=t_fracs, p=di[ contiguous_obs[[ pp ]][ 1:end_pts[ nfull_degree_curves ] ]  , c("X","Y")], deg=spline_deg)
					valid_t_reg <- t_reg_range[ t_reg[ t_reg_range[ 1 ] ] >= di$date_as_sec[ start_obs[ 1 ] ] & t_reg[ max(t_reg_range) ] < di$date_as_sec[ end_obs[ nfull_degree_curves ] ] ]
					#d[ valid_t_reg , c("X","Y"), s ] <- spline_coords[ -breakpoint_pos,,drop=FALSE]
					
				}
				#do the last segment, if it exists
				if (nfull_degree_curves < ncurves) {
					
					t_reg_in_interval <- t_reg[ t_reg >= di$date_as_sec[ start_obs[ ncurves ] ] & t_reg < di$date_as_sec[ end_obs[ ncurves ] ] ]
					
					tmp <- c()
					if (length(t_reg_in_interval) >0) { 
				
						tmp <-  (t_reg_in_interval - di$date_as_sec[ start_obs[ ncurves ] ])/diff(di$date_as_sec[ c(start_obs[ ncurves ], end_obs[ ncurves ]) ])
						
					}
													
					t_fracs <- c(0,tmp,1)
					breakpoint_pos2 <- c(1, length(t_fracs))
					#print("tfracs")
					#print(length(t_fracs))
					#automatically figures out degree
					#print(di[ contiguous_obs[[ pp ]][ start_pts[ ncurves ]:end_pts[ ncurves ] ] , c("X","Y")])
					
					spline_coords2 <- bezier::bezier(t=t_fracs, p=di[ contiguous_obs[[ pp ]][ start_pts[ ncurves ]:end_pts[ ncurves ] ] , c("X","Y")])
					
					d[ which(t_reg >= di$date_as_sec[ start_obs[ ncurves ] ] & t_reg < di$date_as_sec[ end_obs[ ncurves ] ]) , c("X","Y"), s ] <- spline_coords2[ -breakpoint_pos2,,drop=FALSE]
					
					#bind to rest to compute euc dist
					if (ncurves ==1) {
						#just one curve, in which case rename
						spline_coords <- spline_coords2
						breakpoint_pos <- breakpoint_pos2
					}
					else {
						#bind to previous, just get rid of redundant starting point
						spline_coords <- rbind(spline_coords, spline_coords2[-1,])
						breakpoint_pos <- c(breakpoint_pos, nrow(spline_coords))
					}
				}

				d[ t_reg_range , c("X","Y"), s ] <- spline_coords[ -breakpoint_pos,,drop=FALSE]
				
				d[ t_reg_range , "dist_to_next", s ] <- c(euc_dist(d[ t_reg_range , c("X","Y"), s ]), NA)
				#print(d[ t_reg_range , c("X","Y"), s ]
				
				#print(length(breakpoint_pos))
				
		#		print(spline_coords[ -breakpoint_pos,,drop=FALSE])
		#		#previous regular step
		
				# if (pp > 1) {
					# if (max(contiguous_obs[[ pp-1 ]])== t_reg_range[ 1 ]-1) {
						
						# d[ t_reg_range[ 1 ]-1, "dist_to_next", s ] <- d[ t_reg_range[ 1 ]-1, "dist_to_next", s ] + euc_dist( rbind(first_obs_coord, d[ t_reg_range[ 1 ], c("X","Y"), s ]))
						
					# }
				# }
				
				#Euclidean distances
				
				
				#segment_dists <- euc_dist(spline_coords[,,drop=FALSE ])
				#complete distances
				#d[ t_reg_range, "dist_to_next", s ] <- euc_dist(spline_coords[-breakpoint_pos,,drop=FALSE ])#segment_dists[ -breakpoint_pos ]
				
				
				
				#time differences to calculate speed
				
				if (length(t_reg_range) >1) { 
					d[ t_reg_range[ -length(t_reg_range) ], "time_to_next", s] <- reg_dt
				}
				#fraction at the end
				#d[ t_reg_range[ length(t_reg_range) ], "time_to_next", s] <- di$date_as_sec[ obs_range[ 2 ] ] - t_reg[ t_reg_range[ length(t_reg_range) ] ]
			
				#d[ t_reg_range ,"bearing.to.east.tonext.rad",s] <- normalize_angle(atan2(x=diff(c(d[t_reg_range,"X",s], final_obs_coords[ 1 ])), y=diff(c(d[t_reg_range,"Y",s], final_obs_coords[ 2 ]))))
				#if bearing is NA then you know that's the end of the piecewise spline
				d[ t_reg_range[ -length(t_reg_range) ] ,"bearing.to.east.tonext.rad",s] <- normalize_angle(atan2(x=diff(d[t_reg_range,"X",s]), y=diff(d[t_reg_range,"Y",s])))
				
				first_obs_bearing <- normalize_angle(atan2(x=diff(c(first_obs_coords[ 1 ], d[t_reg_range[ 1 ],"X",s])), y=diff(c(first_obs_coords[ 2 ], d[t_reg_range[ 1 ],"Y",s]))))
				
				d[ t_reg_range, "turn.angle.rad",s ] <- normalize_angle(diff(c(first_obs_bearing, d[ t_reg_range ,"bearing.to.east.tonext.rad",s])))				
				
						
			
			}
			
		}
	
	}
	
	#check if any interpolated points are outside
	
	if (! is.null(area_map)) {
		print("removing interpolated points outside of map")
	
		for (s in shark_names) {
		
			not_na <- which(! is.na(d[,"X",s]))
			pts <- as.data.frame(matrix(d[not_na, c("X","Y"),s], ncol=2))
			print(head(pts))
			rownames(pts) <- NULL
			# pts <- sp::SpatialPointsDataFrame(coords=pts, data=data.frame(id=1:nrow(pts)))
			# pts@proj4string <- area_map@proj4string
			pts <- sf::st_as_sf(pts, coords=colnames(pts))
			sf::st_crs(pts) <- sf::st_crs(area_map)
				
			# inside <- sp::over(pts, area_map)
			# outside <-  is.na(inside)
			outside <- ! binary_A_within_B(pts, area_map)
			print(outside)
			
			d[ not_na[ outside ],,s] <- NA 
		}
	
	}
	

				
	d[,"angle_velocity", ] <- d[,"dist_to_next", ]/d[ ,"time_to_next", ]
	d[,"logvelocity",]  <- log(d[,"angle_velocity",])
	
	d[-maxStep,"dx_to_next",] <- d[-1,"X",] - d[-maxStep,"X",]
	d[-maxStep,"dy_to_next",] <- d[-1,"Y",] - d[-maxStep,"Y",]
	
	if (nstates>1) {
		print("calculating transition probabilities")
		
		print("calculating state classification")
		nstates <- 2
		d[,"state.guess2",][ ! is.na(d[,"X",]) ]  <- 2
		#d[,"state.guess2",][ d[, "angle_velocity",] < 0.1 & (! is.na(d[,"state.guess2",])) ] <- 1
		d[,"state.guess2",][ d[, "logvelocity",] < split_logv & (! is.na(d[,"state.guess2",])) ] <- 1
		
		d[-maxStep,"next.guess2",] <- d[-1,"state.guess2",]
		
		#time spent in current state
		for (s in shark_names) {
			if (! is.na(d[1,"state.guess2",s])) { d[1,"time_in_state",s] <- 1 }
			for (tt in 2:maxStep) {
				d[ tt,"time_in_state",s] <- ifelse(is.na(d[tt,"state.guess2",s]), 1, 
													ifelse(abs(diff(d[(tt-1):tt,"state.guess2",s])) %in% c(NA, 1), 1,
														d[tt-1,"time_in_state",s]+1))
			}
		}
		
		
		if (! is.null(centroids)) { nregions <- nrow(centroids) }
		else { nregions <- 1 }
		
		
		#foraging probability
		true_foraging_prob <- array(NA, dim=c(nsharks, nregions+1), dimnames=list(shark_names, c(paste("region",1:nregions), "all")))
		true_transition_prob <- array(NA, dim=c(nsharks, nregions+1, 2), dimnames=list(shark_names, c(paste("region",1:nregions), "all"), c("12", "21")))
		
		
		for (s in shark_names) {
		
			print(s)
			
			if (nregions>1) { 
				
				d[ ! is.na(d[,"X",s]),"region",s] <- apply(d[ ! is.na(d[,"X",s]),c("X","Y"), s], 1, function(x) which_region(x, centroid=centroids))
			}
			else {
				d[ ! is.na(d[,"X",s]),"region",s] <- 1
			}
	
			tmp <- table( factor(d[,"state.guess2",s], levels=1:nstates), factor(d[,"region",s], levels=1:nregions))
			
			true_foraging_prob[ s,1:nregions ] <- tmp[1,]/colSums(tmp)
		
			tmp <- table( factor(d[,"state.guess2",s], levels=1:nstates))
			
			
			true_foraging_prob[ s, "all" ] <- tmp[ 1 ]/sum(tmp)
			
			
			tmp <- table( factor(d[,"state.guess2",s], levels=1:nstates), factor(d[,"next.guess2",s], levels=1:nstates), factor(d[,"region",s], levels=1:nregions))
			
			for (rr in 1:nregions) {
				true_transition_prob[s,rr,] <- as.vector(tmp[,,rr][ 3:2 ]/rowSums(tmp[,,rr,drop=FALSE]))
			}
			tmp <- table( factor(d[,"state.guess2",s], levels=1:nstates), factor(d[,"next.guess2",s], levels=1:nstates))
			true_transition_prob[s,"all",] <- as.vector(tmp[ 3:2 ]/rowSums(tmp[,,drop=FALSE]))
				
		}#end over s
		
		
		true_transition_prob <- apply(true_transition_prob, 2, rbind)
		rownames(true_transition_prob) <- rep(shark_names, each=2)
		true_transition_prob <- cbind(true_transition_prob, type=rep(c(12, 21), nsharks))
		
	}
	
	
	if (nsharks==1) { 
		d_ds <- as.matrix(d[,,shark_names[ 1 ]]) 
		d_ds <- as.data.frame(d_ds[ ! is.na(d_ds[,"X"]),]) 
		d_ds$tag <- shark_names[ 1 ]
		d_ds$date <- as.Date(as.POSIXlt(d_ds$date_as_sec, origin="1970/01/01"))
		d_ds$t_intervals <- which(! is.na(d[,"X",shark_names[ 1 ]]))
		#print(head(d_ds))
	}
	else { 
		for (s in shark_names) {
			d[ is.na(d[,"X",s]),,s] <- NA
		} 
		d_ds <- as.data.frame(apply(d, 2, rbind))
		d_ds$t_intervals <- rep(1:(dim(d)[ 1 ]), nsharks)
		d_ds$tag <- factor(rep(shark_names, each=maxStep), levels=shark_names)
		d_ds$date <- as.Date(as.POSIXlt(d_ds$date_as_sec, origin="1970/01/01"))
		d_ds <- d_ds[ ! is.na(d_ds[,"X"]),]
		d_ds <- d_ds[ order(d_ds$date_as_sec),]
		
		
	}
	
	d[,"date_as_sec",] <- t_reg
	tmp <- 	list(d=d, di=di, shark_names=shark_names) 
	
	if (nstates>1) { 
		tmp$true_foraging_prob <- true_foraging_prob
		tmp$true_transition_prob <- true_transition_prob
		
		d_ds$lambda <- d_ds$state.guess2 -1
		d_ds$state.guess2 <- d_ds$state.guess2 
		d_ds$next.guess2 <- d_ds$next.guess2
		
	}	
	tmp$d_ds <- d_ds	
	
	
	
	tmp
}


	
	
	
	