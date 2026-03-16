
density_min2 <- function(x, m1=NULL, m2=NULL) {
	
	if (sum(! is.na(x)) <2) {
		m <- list()
		m$y <- rep(NA, 512)
		m$x <- seq(0, 1, length.out=512)
	}
	else {
		if (is.null(m1) | is.null(m2)) {
			m <- density(x, na.rm=TRUE)
		
		}
		else {
			m <- density(x, from=m1, to=m2, na.rm=TRUE)
		}
	}
	m
}



keep_finite <- function(x) {
	sign(x)*pmin(abs(x), 1e200)
}







dwrpnorm_tmp <- function(x, mean, sd) {
	

	max_len <- max(length(x), length(mean), length(sd))
	x <- rep_len(x, length.out=max_len)
	sd <- rep_len(sd, length.out=max_len)
	mean <- rep_len(mean, length.out=max_len)
	

	vapply(X=1:max_len, FUN=function(ii) sum(dnorm(x=x[ii]+((-5):5)*2*pi, mean=mean[ii], sd=sd[ii])), FUN.VALUE=0.23423)
}


normalize_logv <- function(logv) {
    pmax(pmin(logv, 10), -15)
}


#which region centroid a point is closest to
which_region <- function(newcoord, centroid=matrix(0, ncol=2, nrow=1)) {
   order(dist_func(center=newcoord, otherXY=centroid))[1]
}


exp_safe <- function(x) {
  exp(pmax(pmin(x,150), -150))
}


log_safe <- function(x) {
	log(pmin(pmax(1e-150, x), 1e305))
}
 
#Euclidean distance	

dist_func <- function(center=c(0,0), otherXY) {
   otherXY <- as.matrix(otherXY, ncol=2)
   apply(otherXY, 1, function(x) sqrt(sum((center-x)^2))) 
}

#effective sample size

eff_ss <- function(p) {
 
  p <- p/sum(p)
  1/sum(p^2)
}  



binary_A_B_st_func <- function(A, B, func=sf::st_within) {
	# binarize sf package checks; order may matter
	ind <- as.integer(func(x=A, y=B))
	# if is not within, returns NA
	ind[ is.na(ind) ] <- 0
	as.logical(ind)
}

binary_A_within_B <- function(A, B) { binary_A_B_st_func(A=A, B=B) }
binary_A_intersects_B <- function(A, B) { binary_A_B_st_func(A=A, B=B, func=sf::st_intersects) }


#this estimates by MCMC the probability of being inside the bounds
fraction_inside <- function(mu, cmat, nsim=100, obj) {
        
	pts <- as.data.frame(mvtnorm::rmvnorm(n=nsim, mean=mu, sigma=cmat))
	colnames(pts) <- c("X","Y")
	# make pts a shapefile with the same CRS as area_map
	pts <- sf::st_as_sf(pts, coords=colnames(pts))
	sf::st_crs(pts) <- sf::st_crs(obj$area_map)
	
	# returns NA if not in the map, so replace these with 0s
	point_is_in_map <- sapply(pts, function(x) binary_A_within_B(A=x, B=obj$area_map))
	mean(point_is_in_map)

}

#see if line trajectory of the X-Y points drawn is in the boundary
reject_sampling <- function(mu, cmat, maxiter=500, prev_val, obj) {
    inside <- FALSE
    iter <- -1
	#if (is.null(prev_val)) { print("prev_val is NULL") }
	
	
    while((! inside) & iter < maxiter) {
		sim_val <- mvtnorm::rmvnorm(n=1, mean=mu, sigma=cmat)
		new_traj <- cbind(X=c(prev_val[1], sim_val[1]), Y=c(prev_val[2], sim_val[2]))
	   
		if (obj$enforce_full_line_in_map) {
						
			# new_traj <- sp::SpatialLines(list(sp::Lines(sp::Line(new_traj), ID=1)))
			new_traj <- sf::st_linestring(new_traj)
		}
		else {
			#just consider end point, not full line connecting them
			# new_traj <- sp::SpatialPoints(new_traj[2,,drop=FALSE])
			new_traj <- sf::st_point(new_traj[2,])
		}
		# new_traj@proj4string <- obj$area_map@proj4string
		
		inside <- binary_A_within_B(A=new_traj, B=obj$area_map)

		iter <- iter + 1
			
	}
	
    if (iter == maxiter) { sim_val <- prev_val ; print("exceeded rejection sampling iterations") }
	#we use this both for the interpolated and for the 4-variable
	
	list(val=sim_val, iter=iter)
}
 

getslot <- function(poly, slotname) {
  sapply(methods::slot(poly,"polygons"), function(x) methods::slot(x, slotname))
}


# calculate observed foraging and transition probabilities by region-shark for a regular-step dataset


true_behavior_probabilities <- function(reg_step_ds, shark_names, nstates, nregions) {

	nsharks <- length(shark_names)

	#foraging probability
	foraging <- table(factor(reg_step_ds$tag, levels=shark_names),
					  factor(reg_step_ds$region, levels=1:nregions),
					  factor(reg_step_ds$state.guess2, levels=1:nstates), useNA="no")
	dimnames(foraging)[[2]] <- paste("region", dimnames(foraging)[[2]], sep="")

	shark_region_counts <- apply(foraging, c(1,2), sum)
	shark_counts <- apply(foraging, 1, sum)
	shark_behavior_counts <- apply(foraging, c(1,3), sum)
	shark_foraging_counts <- apply(foraging[,,1,drop=FALSE],1,sum)

	true_foraging_prob <- shark_region_counts
	cts_missing <- shark_region_counts == 0
	true_foraging_prob[ ! cts_missing ] <- foraging[,,1,drop=FALSE][ ! cts_missing ]/true_foraging_prob[ ! cts_missing ]
	true_foraging_prob[ cts_missing ] <- NA

	true_foraging_prob <- cbind(true_foraging_prob, all=shark_foraging_counts / shark_counts)

	#transition probability
	trans <- table(factor(reg_step_ds$tag, levels=shark_names),
				   factor(reg_step_ds$region, levels=1:nregions),
				   factor(reg_step_ds$state.guess2, levels=1:nstates),
				   factor(reg_step_ds$next.guess2, levels=1:nstates), useNA="no")
	dimnames(trans)[[2]] <- paste("region", dimnames(trans)[[2]], sep="")

	true_transition_prob <- matrix(0, ncol=nregions + 2, nrow=nstates * nsharks)
	colnames(true_transition_prob) <- c(paste("region", 1:nregions),"all","type")
	rownames(true_transition_prob) <- rep(shark_names, each=2)
	true_transition_prob[,"type"] <- rep(c(12,21), nsharks)

	start_in_1 <- true_transition_prob[,"type"] ==12
	start_in_2 <- true_transition_prob[,"type"] ==21
	foraging_missing <- foraging[,,1] == 0
	transiting_missing <- foraging[,,2] == 0

	shark_counts_12 <-rowSums(trans[,,1,2,drop=FALSE])
	shark_counts_21 <-rowSums(trans[,,2,1,drop=FALSE])

	true_transition_prob[start_in_1, 1:nregions][ ! foraging_missing ] <- trans[,,1,2][! foraging_missing ] / foraging[,,1,drop=FALSE][! foraging_missing ]
	true_transition_prob[start_in_2, 1:nregions][ ! transiting_missing ] <- trans[,,2,1][! transiting_missing ] / foraging[,,2,drop=FALSE][! transiting_missing ]

	true_transition_prob[start_in_1, 1:nregions][ foraging_missing ] <- NA
	true_transition_prob[start_in_2, 1:nregions][ transiting_missing ] <- NA

	true_transition_prob[start_in_1, "all"] <- shark_counts_12 / shark_behavior_counts[,1]
	true_transition_prob[start_in_2, "all"] <- shark_counts_21 / shark_behavior_counts[,2]

	true_transition_prob[start_in_1, "all"][ shark_behavior_counts[,1] == 0 ] <- NA
	true_transition_prob[start_in_2, "all"][ shark_behavior_counts[,2] == 0 ] <- NA

	list(true_foraging_prob=true_foraging_prob, true_transition_prob=true_transition_prob)
	
}


rectangular_shapefile <- function(xmin=-5000, xmax=5000, ymin=-5000, ymax=5000) {
	# create a default rectangular shapefile if none provided
	if (xmin >= xmax) stop('xmin must be < xmax')
	if (ymin >= ymax) stop('ymin must be < ymax')

	pts <- rbind(c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin))
	sf::st_sfc(sf::st_polygon(list(pts)))

}



	 