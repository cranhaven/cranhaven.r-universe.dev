EKF_1d_interp_joint <- function(d, 
				npart=100, sigma_pars, alpha0_pars=list(mu0=c(5, 9), V0=c(0.25, 0.25)), Errvar0=rep(list(5), 2), 
				Errvar_df=c(20, 20), Particle_errvar0, Particle_err_df=20, delaysample=1,
				dirichlet_init=c(10,3,3,8), maxStep=NULL, state_favor=c(1, 1), nstates=2, lowvarsample=FALSE, 
				time_radius=60*30, spat_radius=300, min_num_neibs=10, interact=TRUE,
				interact_pars=list(mu0=0, precision0=2, known_precision=2), neff_sample=1,  
				time_dep_trans=FALSE, time_dep_trans_init=dirichlet_init, smoothing=FALSE, fix_smoothed_behaviors=TRUE, 
				smooth_parameters=TRUE, reg_dt=120, max_int_wo_obs=NULL, resamp_full_hist=TRUE, 
				compare_with_known=FALSE, known_trans_prob=NULL, known_foraging_prob=NULL, known_regular_step_ds=NULL,
				update_eachstep=FALSE, update_params_for_obs_only=FALSE,
				output_plot=TRUE, loc_pred_plot_conf=0.5, output_dir=getwd(), pdf_prefix="EKF_1D",
				verbose=3) {

	# save old settings and reset
	old_pars <- par(mfrow=par()$mfrow, mfcol=par()$mfcol, las=par()$las)
	on.exit(expr=par(old_pars))
	
	par(mfrow=c(2,2))		

    #current list of inputs
    func_args <- as.list(environment())
	#environment that will contain all relevant objects
	myenv <- new.env(parent=emptyenv())
	
	#and store arguments in environment
	for (aa in names(func_args)) {
		myenv[[ aa ]] <- func_args[[ aa ]]
	}
	

	myenv$npart <- max(2, as.integer(myenv$npart))
	myenv$nregions <- 1
	myenv$states <- myenv$d$state.guess2
	myenv$next_states <- myenv$d$next.guess2

	#make some modifications to the input data
	fix_data_EKF_1d_interp_joint(env_obj=myenv)

	myenv$state_favor <- myenv$state_favor[1:myenv$nstates]

	#initialize arrays for storing history of process values
	initialize_arrays_EKF_1d_interp_joint(env_obj=myenv)

	#initialize arrays for storing of hyperparameters and EKF values
	initialize_SSM_params_EKF_1d_interp_joint(env_obj=myenv)

	#loop over time steps

	#start before, then each time take that +1 to i-1.  
	myenv$steps_to_resamp <- lapply(myenv$shark_intervals, function(x) min(x)-1)
	names(myenv$steps_to_resamp) <- myenv$shark_names

	for (ii in myenv$included_intervals) {

		myenv$i <- ii


		#first simulate shark positions and movement at the beginning of the interval after the first observation they have.
		#later this will be good if we want to have time gaps
		#which observations have a 

		myenv$sharks_first_obs <- as.vector(sapply(myenv$first_intervals, function(x) myenv$i %in% x))
		names(myenv$sharks_first_obs) <- myenv$shark_names
		#print(sharks_first_obs)
		myenv$sharks_first_obs <- names(myenv$sharks_first_obs[ myenv$sharks_first_obs==TRUE ])

		#take the first observation as given
		#should yobs_prev be 
		
		#yobs_prev <- ifelse(i==1,1,yobs)
		
		#include only sharks that have enough observations
		#sharks_in_interval <- sapply(shark_intervals, function(x) i %in% x)
		#sharks_in_interval <- names(sharks_in_interval[ sharks_in_interval ])
		ids <- (myenv$d[,"t_intervals"] == myenv$i) & ! (myenv$tags %in% myenv$sharks_first_obs) & (myenv$tags %in% myenv$shark_names)
		ids <- ids[ !is.na(ids) ]

		
		myenv$ynext <- myenv$d[ ids, , drop=FALSE]
			
		rownames(myenv$ynext) <- myenv$tags[ ids ]
			
		#sometimes a shark will have an observation but wont be "valid", i.e. its too far out of the threshold	
		if (nrow(myenv$ynext) > 0) {
		
			valid <- sapply(myenv$shark_valid_steps[ rownames(myenv$ynext) ], function(x) myenv$i %in% x)
			valid <- names(valid[ valid==TRUE ])
			myenv$ynext <- myenv$ynext[ rownames(myenv$ynext) %in% valid,,drop=FALSE]
			
			if (nrow(myenv$ynext) > 0) {
				myenv$ynext <- myenv$ynext[ order(rownames(myenv$ynext), myenv$ynext[,"date_as_sec",drop=FALSE]), ,drop=FALSE]
				yobs <- nrow(myenv$ynext)
				
				if (myenv$show_prints) {
					print("observations in interval")
					print(myenv$ynext[,c("X","velocity","date_as_sec","t_intervals","state.guess2","shark_obs_index"), drop=FALSE])
				}
			}
		}
		
			
		#sharks with observations
		myenv$sharks_with_obs <- unique(rownames(myenv$ynext))	
		
		#any sharks that have surrounding actual observations but not observed data in the interval
		
		#sharks_to_sim <-  sapply(shark_intervals, function(x) max(x, na.rm=TRUE) > (i -  max_int_wo_obs) & min(x, na.rm=TRUE)<= i)
		myenv$sharks_to_sim <- sapply(myenv$shark_valid_steps, function(x) myenv$i %in% x)
		
		myenv$sharks_to_sim <- names(myenv$sharks_to_sim[ myenv$sharks_to_sim ])
		#this may include some that just had first observation, but that would be the earlier ones
		myenv$sharks_to_sim <- myenv$sharks_to_sim[ ! (myenv$sharks_to_sim %in% myenv$sharks_with_obs) & ! (myenv$sharks_to_sim %in% myenv$sharks_first_obs) ]
		
		
		
		#we need to do two things: for sharks that are 'recent' but without observations there, we have to simulate their positions by uniform
		#if there is a shark with actual observations, we have to simulate and resample based on that.
		
		if (myenv$show_prints) {
		
			print(paste("Regular step", myenv$i))
		
			if (myenv$nsharks > 1) {
				if (length(myenv$sharks_first_obs)) { print(paste("sharks first observed:",paste(myenv$sharks_first_obs, collapse=" "))) }
				if (length(myenv$sharks_to_sim)) { print(paste("sharks to be simulated:", paste(myenv$sharks_to_sim, collapse=" "))) }
				if (length(myenv$sharks_with_obs)) { print(paste("sharks with obs:",paste(myenv$sharks_with_obs, collapse=" "))) }
			}
		}

		
		if (length(myenv$sharks_first_obs)) {
			if (myenv$show_prints) print("generating first observations...")
					
			sharks_first_obs_EKF_1d_interp_joint(env_obj=myenv)
		
		
		
			#if update each time
			
			if (myenv$update_eachstep) {
			
				for (s in myenv$sharks_first_obs) {

					
					z <- myenv$lambda_matrix[,myenv$i,s] 
					newV <- myenv$Xpart_history[myenv$i,"velocity",,s] 
					
					#index matrix for accessing terms and updating
					access_mu <- access_V <- cbind(myenv$state_names[z], "mu", myenv$pnames, s)
					access_V[,2] <- "V"
 										
					mu0 <- myenv$mu[access_mu]
					V0 <-  myenv$mu[access_V]
					
					myenv$mu[access_V] <- 1/(1 + 1/V0)
					myenv$mu[access_mu] <- ((1/V0)*mu0 + newV) * myenv$mu[access_V]
					
					
					access_igamma <- cbind(1:myenv$npart, 2*z)
					myenv$sigma_pars[,,s][ access_igamma ]  <- pmax(1e-2, myenv$sigma_pars[,,s][ access_igamma ] + 0.5*((mu0^2)/V0 + newV^2 - (myenv$mu[access_mu]^2)/myenv$mu[access_V]))
					
					access_igamma2 <- cbind(1:myenv$npart, 2*z - 1)
					myenv$sigma_pars[,,s][ access_igamma2 ] <- myenv$sigma_pars[,,s][ access_igamma2 ] + 0.5
					
				}#loop over sharks		
			
			
			}#if update now
			
			
			

		}#if any are first

		if (length(myenv$sharks_to_sim)) {

			
			if (myenv$show_prints) print("simulating observations...")	
						
			if (myenv$interact) {
				myenv$temp_neib_range <- which((myenv$t_reg < myenv$t_reg[ myenv$i ]) & (myenv$t_reg >= (myenv$t_reg[ myenv$i ] - myenv$time_radius)))
				if (myenv$show_prints) print(paste("temporal range of neighborhood: observations", paste(myenv$temp_neib_range, collapse=" ")))
			}
			
			myenv$part_with_neibs <- matrix(FALSE, ncol=length(myenv$sharks_to_sim), nrow=myenv$npart)
			colnames(myenv$part_with_neibs) <- myenv$sharks_to_sim

			for (ss in myenv$sharks_to_sim) {
							
				myenv$s <- ss
				
				if (myenv$interact) {
						
					sharks_to_sim_interact_EKF_1d_interp_joint(env_obj=myenv)
			
				}#end if interaction
	
				sharks_to_sim_EKF_1d_interp_joint(env_obj=myenv)

			}#loop over sharks
			
			rm("s", envir=myenv)		
		
			if (myenv$update_eachstep) {
				#update the 
				
				sharks_to_sim_update_EKF_1d_interp_joint(env_obj=myenv)
				
				if (myenv$interact) {
				
					for (s in myenv$sharks_to_sim) {
						
							
						mu0 <- matrix(myenv$spatial_interact_pars[, myenv$mu_names ,s], ncol=myenv$nstates-1)
						tau0 <- matrix(myenv$spatial_interact_pars[, myenv$prec_names ,s], ncol=myenv$nstates-1)
						
						#myenv$part_with_neibs <- apply(neib_fracs[,,i,s, drop=FALSE], 1, function(x) any(x> 0))
						if (any(myenv$part_with_neibs[,s])) {
									
							nf_tmp <- myenv$neib_fracs[myenv$part_with_neibs[,s], -myenv$nstates, myenv$i, s]
							
							rho <- myenv$interact_intensity_draw[myenv$part_with_neibs[,s], -myenv$nstates, myenv$i, s]
									
							
							new_epsilons <- myenv$tau_vals[myenv$part_with_neibs[,s],]*(nf_tmp^2) + tau0[myenv$part_with_neibs[,s],]
													
						
							myenv$spatial_interact_pars[myenv$part_with_neibs[,s], myenv$mu_names ,  s] <- (myenv$tau_vals[myenv$part_with_neibs[,s],]*(log_safe(rho)*nf_tmp) + tau0[myenv$part_with_neibs[,s],]*mu0[myenv$part_with_neibs[,s],])/new_epsilons
							myenv$spatial_interact_pars[myenv$part_with_neibs[,s], myenv$prec_names ,s]  <- new_epsilons
							
						}
					}
				}		
	
			}#if update early

			
			myenv$other_type <- "sim"
			myenv$before_samp <- TRUE
			if (myenv$show_plots) plot_diagnostics_step_EKF_1d_interp_joint(env_obj=myenv)					
						
	
		}# if no yobs   
	 	 
        #basically what we do here is assume that there is a single state lambda for x_{t-1}.  Then see which values of the observed match that. 
		if (length(myenv$sharks_with_obs)) {
		
			if (myenv$show_prints) print("simulating to match observations...")
			#different sharks have diff numbers of observations
			
			myenv$yobs_sharks <- table(rownames(myenv$ynext))
			
			#these are sharks that actually fall into the interval
					
		    obs_num <- which(myenv$d[,"t_intervals"] == myenv$i)
			obs_range <- ifelse(length(obs_num)==1, paste("observation", obs_num), paste("observations", paste(min(obs_num), max(obs_num),sep="-")))
			if (myenv$show_prints) print(paste("Step", myenv$i,":",obs_range))
			
			
			if (myenv$time_dep_trans) {	
			
				for (s in myenv$sharks_with_obs) {

					tis <- myenv$Xpart_history[myenv$i-1, "time_in_state",, s]
					#print("tis prev for obs")
					#print(tis)
					#prev_region <- Xpart_history[i-1,"region",,s]
					prev_z <- myenv$lambda_matrix[, myenv$i-1, s]
					#print(prev_region)
					#print(prev_z)
					#if this is true then draw probabilities for each region
										
								
					for (p in 1:myenv$npart) {
						
						z <- prev_z[ p ]
						z_oth <- which(1:myenv$nstates != z)
						
						a_tmp <- myenv$transition_mat[[ s ]][[ p ]]$dirichlet_pars[ 1, c(2*z-1, 2*z) ]
										
						gamma_ratio <- rgamma(n=1, shape=a_tmp[ z_oth ], rate=a_tmp[ z ])
						#if (p==1) { print(gamma_ratio); print(tis[ 1 ]) ; print(z); print(z_oth); print(a_tmp)}	
						#only draw for the state that happened previously, since depends on t_k
						
						myenv$transition_mat[[ s ]][[ p ]]$mat[z,z] <-  rbeta(n=1, shape1=gamma_ratio*tis[ p ], shape2=1)
						myenv$transition_mat[[ s ]][[ p ]]$mat[z,z_oth] <- 1 - myenv$transition_mat[[ s ]][[ p ]]$mat[z,z]
						myenv$prob_draws_hist[ myenv$i, , p, s] <- myenv$transition_mat[[ s ]][[ p ]]$mat[z,]
						
					}	
				}
			}#end time dependent transitions
								
	

			sharks_with_obs_sim_EKF_1d_interp_joint(env_obj=myenv)
				

			myenv$other_type <- "obs"
			myenv$before_samp <- TRUE

			
			if (myenv$show_plots) plot_diagnostics_step_EKF_1d_interp_joint(env_obj=myenv)
	
			#interaction parameters
			
			myenv$part_with_neibs <- matrix(FALSE, ncol=length(myenv$sharks_with_obs), nrow=myenv$npart)
			colnames(myenv$part_with_neibs) <- myenv$sharks_with_obs
			
			if (myenv$interact) {
				
				myenv$temp_neib_range <- which((myenv$t_reg < myenv$t_reg[ myenv$i ]) & (myenv$t_reg >= (myenv$t_reg[ myenv$i ] - myenv$time_radius)))
				if (myenv$show_prints) print(paste("temporal range of neighborhood: observations", paste(myenv$temp_neib_range, collapse=" ")))
				
				sharks_with_obs_interact_EKF_1d_interp_joint(env_obj=myenv)
												
			}#end calculate interactions
			
			#calculate density components for resampling
			calculate_resampling_indices_EKF_1d_interp_joint(env_obj=myenv)
			
			if (length(myenv$sharks_to_resample) > 0) {
				
				if (myenv$show_prints) {
					print(paste("Unique resampling indices selected:", paste(myenv$resample_history[myenv$i, myenv$sharks_with_obs] * myenv$npart, collapse=" ")))
				
					if (myenv$nsharks > 1) {
			
						print("sharks to resample:")
						print(myenv$sharks_to_resample)	
					}			
				}			
				reindex_arrays_after_resampling_EKF_1d_interp_joint(env_obj=myenv)
				
				myenv$before_samp <- FALSE
				myenv$other_type <- "obs"
				
				
				if (myenv$show_plots) plot_diagnostics_step_EKF_1d_interp_joint(env_obj=myenv)										   
			}
			   
											
			
		
			#now sample to decide behaviors
			
			if (myenv$nstates > 1) {	
					
				for (s in myenv$sharks_with_obs) {		
					rs <- rowSums(myenv$densities_bystate[[ s ]])
					myenv$densities_bystate[[ s ]][ rs==0,] <- 0.5	
					
					myenv$lambda_matrix[,myenv$i, s ] <- as.vector(apply(myenv$densities_bystate[[ s ]], 1, function(x) low_var_sample(wts=x, M=1))) 

							
					myenv$state_counts[ ,,s][ cbind(1:myenv$npart, myenv$lambda_matrix[,myenv$i,s]) ] <- myenv$state_counts[ ,,s][ cbind(1:myenv$npart, myenv$lambda_matrix[,myenv$i,s]) ] + 1
											
				}
				myenv$lambda_matrix_beforesamp[,myenv$i,] <- myenv$lambda_matrix[,myenv$i,] 
			}
			
				
			#propagate Xpart_history and update mk and Pk_actual

			sharks_with_obs_propagate_EKF_1d_interp_joint(env_obj=myenv)
																				

				
			if (myenv$nstates > 1) {
			
				sharks_with_obs_update_transition_EKF_1d_interp_joint(env_obj=myenv)
				
				#updates of interaction parameters
				
				if (myenv$interact) {
			
					for (s in myenv$sharks_with_obs) {
					
						steps_tmp <- c(myenv$steps_to_resamp[[ s ]], myenv$i)
						if (! (steps_tmp[ 1 ] %in% myenv$first_intervals[[ s ]])) { steps_tmp <- steps_tmp[-1 ] } 	
						
						#if only update last step (others were already updated before)
						if (myenv$update_eachstep | myenv$update_params_for_obs_only) { steps_tmp <- myenv$i }
			
						
						mu0 <- matrix(myenv$spatial_interact_pars[,myenv$mu_names ,s], ncol=myenv$nstates - 1)
						tau0 <- matrix(myenv$spatial_interact_pars[, myenv$prec_names ,s], ncol=myenv$nstates - 1)
						
				
						for (p in 1:myenv$npart) 	{
						
							steps_with_neibs <- apply(t(myenv$neib_fracs[p,,steps_tmp,s]), 1, function(x) any(x> 0))
															
							if (any(steps_with_neibs)) {
								steps_with_neibs <- steps_tmp[ steps_with_neibs ]
								
								nf_tmp <- t(myenv$neib_fracs[p,,steps_with_neibs,s])
								#print("nf_tmp")
								#print(nf_tmp)
								rho <- t(myenv$interact_intensity_draw[p, -myenv$nstates, steps_with_neibs,s])
								#print("rho")
								#print(rho)
								
								new_epsilons <- myenv$tau_vals[p,]*colSums(nf_tmp[,-myenv$nstates, drop=FALSE]^2) + tau0[p,]
								
								#print(myenv$spatial_interact_pars[p,c("precision1","precision2") ,s])
								myenv$spatial_interact_pars[p, myenv$mu_names ,s] <- (myenv$tau_vals[p,]*colSums(matrix(log_safe(rho)*nf_tmp[,-myenv$nstates, drop=FALSE], ncol=myenv$nstates-1)) + tau0[p,]*mu0[p,])/new_epsilons
								myenv$spatial_interact_pars[p,myenv$prec_names ,s]  <- new_epsilons
								#print("new mu00")
								#print(myenv$spatial_interact_pars[p,c("mu1","mu2") ,s])
						
						   }
						}
					}
			
				}#end interation parameters
			}

			sharks_with_obs_update_params_EKF_1d_interp_joint(env_obj=myenv)
			  
		  
		}#condition on yobs

			
		for (s in unique(c(myenv$sharks_first_obs, myenv$sharks_to_sim, myenv$sharks_with_obs))) {
			if (!((myenv$i + 1) %in% myenv$shark_valid_steps[[ s ]])) {
		
				if (myenv$show_prints) print(paste("shark",s))
		
				for (p in 1:myenv$npart) {
					z <- myenv$Xpart_history[myenv$i, "lambda", p, s]

					if (myenv$nstates > 1) { myenv$Xpart_history[myenv$i + 1,"lambda",p,s]  <- low_var_sample(wts=myenv$transition_mat[[ s ]][[ p ]]$mat[z,], M=1) }
					
					znew <- myenv$Xpart_history[myenv$i + 1,"lambda",p,s]
					sigma_draw <- MCMCpack::rinvgamma(n=1, myenv$sigma_pars[p,2*znew-1,s], myenv$sigma_pars[p,2*znew,s])
					myenv$logv_angle_mu_draw[p,"velocity",znew, s] <- mvtnorm::rmvnorm(n=1, mean=myenv$mu[znew, "mu", p, s], sigma=as.matrix(sigma_draw * myenv$mu[znew, "V", p, s]))
					myenv$mk_prev[,znew,p,s] <- myenv$f(mk=myenv$mk_actual[,p,s], new_logv=rnorm(n=1, mean=myenv$logv_angle_mu_draw[p,"velocity",znew, s], sd=sqrt(sigma_draw)), dtprev=myenv$reg_dt)
					Fx_tmp <- myenv$Fx(mk=myenv$mk_actual[,p,s], dtprev=myenv$reg_dt)
					myenv$Pk_prev[,,znew,p,s] <- as.matrix(Matrix::nearPD(Fx_tmp %*% myenv$Pk_actual[,,p,s] %*% t(Fx_tmp) + myenv$Qt[,,znew,p,s], ensureSymmetry=TRUE)$mat) #R_{t+1}
					myenv$Xpart_history[myenv$i + 1,c("X","velocity"),p,s] <- mvtnorm::rmvnorm(n=1, mean=myenv$mk_prev[,znew,p,s], sigma=myenv$Pk_prev[,,znew,p,s])
				}
			
			myenv$lambda_matrix[,myenv$i + 1,s] <- myenv$Xpart_history[myenv$i + 1, "lambda",,s]
			}	
		}
		
		
       
	}#finish iterating over indices

		

	#smoothing
	
		
	rm("i", envir=myenv)
	rm("indices", envir=myenv)
	#problem is how does this because if we smooth this way we lose the connection of 

	if (myenv$smoothing) {
		if (myenv$show_prints) print("performing smoothing of full history joint density of X...")
		

		smoothing_EKF_1d_interp_joint(env_obj=myenv)
													
	}#end smoothing		
					
		
	if (myenv$compare_with_known) {
	
		
		calculate_compare_with_known(env_obj=myenv)

	}


	# if (myenv$compare_with_known) {

		# myenv$known_regular_step_ds <- myenv$known_regular_step_ds[ order(myenv$known_regular_step_ds$date_as_sec),]
		# myenv$known_regular_step_ds <- myenv$known_regular_step_ds[ myenv$known_regular_step_ds$date_as_sec >= myenv$first_time & myenv$known_regular_step_ds$date_as_sec < max(myenv$t_reg), ]
		# print(myenv$t_reg)
		# print(myenv$included_intervals)
		# myenv$known_regular_step_ds[,"t_intervals"] <- as.numeric(as.character(cut(x=myenv$known_regular_step_ds$date_as_sec, breaks=myenv$t_reg, labels=1:(myenv$N-1), right=TRUE)))
	
		# #cut off at the end of what we have
		# myenv$known_regular_step_ds <- myenv$known_regular_step_ds[  ! is.na(myenv$known_regular_step_ds$t_intervals),]
		# myenv$known_regular_step_ds <- myenv$known_regular_step_ds[  myenv$known_regular_step_ds$t_intervals < myenv$N,]
		# myenv$known_regular_step_ds$tag <- factor(myenv$known_regular_step_ds$tag)
		
		# print("true")
		# print(head(myenv$known_regular_step_ds))
		# print("obs")
		# print(head(myenv$d))
		
	# }

	  

	if (myenv$nstates > 1) {
	
		if (myenv$show_prints) {
			print("Actual distribution of lambdas:")
			print(round(100*(table(myenv$states)/length(myenv$states))))
			print("Particle distribution of lambdas:")
			tmp <- table(factor(myenv$lambda_matrix, levels=1:myenv$nstates))
			print(round(100*tmp/sum(tmp)))
		}	
		#final diagnostics:

		#agree_final <- apply(lambda_matrix[,t_intervals,] == matrix(states, ncol=maxStep, nrow=npart, byrow=TRUE), 2, mean)
		#agree_hist <-  apply(lambda_matrix_beforesamp[,t_intervals] == matrix(states, nrow=npart, ncol=maxStep, byrow=TRUE), 2, mean)
        
		myenv$agree_table <- matrix(NA, ncol=myenv$nsharks, nrow=(1 + myenv$nstates)*(1 + (myenv$compare_with_known)) + 2)
		colnames(myenv$agree_table) <- myenv$shark_names
		if (myenv$compare_with_known) {  rownames(myenv$agree_table) <- c("overall", paste("state", 1:myenv$nstates), paste("true",c("overall", paste("state", 1:myenv$nstates))), "single","most_common") }
		else { rownames(myenv$agree_table) <- c("overall", paste("state", 1:myenv$nstates), "single","most_common") }
		
		if (myenv$smoothing & (myenv$fix_smoothed_behaviors==FALSE)) { myenv$agree_table_smoothed <- myenv$agree_table }

	}


	#overall positions
	#xall <- myenv$Xpart_history[,"X",,]
	#xall <- xall[ ! is.na(xall) ]
		
	
	
	#plotting
	plotting_EKF_1d_interp_joint(env_obj=myenv)
	
	#now clean up a bit

	
	unneeded_vars <-  c("mu_names", "prec_names", "Nnames", "pnames", "rnames", "state_names", "temp_neib_range",
					   "sharks_with_obs", "sharks_to_sim", "XY", "ynext", "yobs_prev", "yobs_sharks",
					   "densities_components", "densities_bystate", "indices", "pred_xt_loc_bystate",
					   "pred_xt_loc_bystate_var", "densities", "igamma_par_names", "logv_angle_mu_draw",
					   "other_type", "f", "Fx", "h", "Hx", "V0_pars", "bbox", "Qt", "neib_fracs", "Errvar_df",
					   "first_time", "j_list", "part_with_neibs", "Errvar0", "sigma_draw","shark_final_obs","interact_pars",
					   "mk_curr", "mk_actual", "mk_curr_smoothed", "Pk_curr", "Pk_actual", "Pk_curr_smoothed", "Pk_prev_smoothed", "y_first",
					   "smooth_iter", "delaysample", "next_states", "mk_prev", "Pk_prev", "errs", "time_dep_trans_init",
					   "min_num_neibs", "mk_prev_smoothed", "steps_to_resamp", "max_int_wo_obs", "trans_names", "Kgain", "region_alphas",
					   "mk", "Pk", "output_dir", "shark_symbols", "tau_vals", "mu0_pars", "sharks_first_obs", "param_sampling_weights",
					   "sharks_to_resample", "mu0_range", "before_samp", "part_with_neibs", "output_plot", "maxStep", "fix_smoothed_behaviors",
					   "MuY", "SigY", "Particle_errvar0", "pred_xt_loc", "dirichlet_init", "nregions", "reg_dt", "Particle_err_df", "Particle_errvar",
					   "XY_errvar_draw", "step_skipped", "smooth_parameters", "SSquare_particle", "num_neibs", "Particle_errvar_smoothed",
					   "mk_actual_history", "verbose")
	
	
	unneeded_vars <- unique(unneeded_vars)
	unneeded_vars <- unneeded_vars[ unneeded_vars %in% names(myenv)]
	rm(list=unneeded_vars, envir=myenv)
	
	invisible(as.list(myenv))
	
}

#UTF coordinates
#https://sites.google.com/a/lakeheadu.ca/yong-luo/blog/convert-utm-to-longlat-in-r
#library(rgdal)
#utmcoor <- SpatialPoints(shark_data[,c("X","Y")], proj4string=CRS("+proj=utm +zone=11"))
#longlat <- as.data.frame(spTransform(utmcoor, CRS("+proj=longlat")))







