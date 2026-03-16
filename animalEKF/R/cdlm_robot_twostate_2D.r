#http://stackoverflow.com/questions/34384907/how-can-put-multiple-plots-side-by-side-in-shiny-r



cdlm_robot_twostate_2D <- function() {

shinyApp(

ui=shinyUI(fluidPage(
  titlePanel("Particle filter example of estimating mean log-speeds and location of 2-D robot\nfor two behavior types"),
  sidebarLayout(position = "left",
                             sidebarPanel("Simulation parameters", width=3,
							   sliderInput('iter',"Progress of simulation",value=1, min=1, max=40, round=TRUE, step=1,
				                 animate=animationOptions(interval=7000, loop=FALSE)),
		      				    actionButton('run',"Accept settings, press play above"),
								sliderInput('max_iter','Maximum number of iterations',value=20, min=1, max=40, round=TRUE, step=1),
								checkboxInput('sep_col',"Color particles differently (not recommended for large number of particles)",value=TRUE),								
								sliderInput('npart','Number of particles',value=6, min=2, max=40, round=TRUE, step=1),
								sliderInput('vel_mu1',"Unknown true mean of log-speed (units/sec), type 1",value=1.5, min=-6, max=6, round=FALSE, step=0.05),
								sliderInput('vel_mu2',"Unknown true mean of log-speed (units/sec), type 2",value=4, min=-6, max=6, round=FALSE, step=0.05),
								sliderInput('vel_var','Known true variance of log-speed',value=0.2, min=0.1, max=1, round=FALSE, step=0.05),
								sliderInput('turn_var','Known true variance of turn angle (not estimated)',value=0.6, min=0.1, max=1.5, round=FALSE, step=0.05),
								sliderInput('delta_t','Time step (sec)',value=10, min=0.1, max=20, round=FALSE, step=0.25),
								sliderInput('conf_level','Posterior ellipse level',value=0.5, min=0.05, max=0.95, round=FALSE, step=0.05),
								sliderInput('mu_mean1','Prior mean (units/sec) of speed mean, type 1',value=1.5, min=-6, max=4, round=FALSE, step=0.05),
								sliderInput('mu_mean2','Prior mean (units/sec) of speed mean, type 2',value=4, min=-6, max=4, round=FALSE, step=0.05),
								sliderInput('mu_var','Prior variance on log-speed means',value=0.4, min=0.1, max=1, round=FALSE, step=0.05),
								sliderInput('p1to2','Transition probability between type 1 and 2',value=0.2, min=0, max=1, round=FALSE, step=0.05),
								sliderInput('p2to1','Transition probability between type 2 and 1',value=0.2, min=0, max=1, round=FALSE, step=0.05),
								sliderInput('state2_favor','Extra multiplicative favor of type 2 behavior',value=5, min=0.1, max=15, round=FALSE, step=0.1),
								checkboxInput('tp_known','Are transition probabilities known?',value=FALSE),
								textInput('dir_prior',"Dirichlet prior values on transition probability.\nEnter 4 positive numbers, comma separated.", value="8,2,2,8"),
							    textInput('xt_var',"Covariance matrix of position and velocity.\nEnter 4 numbers (comma delimited) for a covariance matrix, where the 4th should be the same as the velocity variance.  Coerced to PD matrix.",
							        value="0.1,0,0,0.1"),
								textInput('yt_var','Measurement error covariance of true location.\nEnter 4 numbers (comma delimited) for a covariance matrix.   Coerced to PD matrix.',
									value="0.5,0,0,0.5"),
								sliderInput('render_delay','Render delay (sec)',value=3, min=0.1, max=5, round=TRUE, step=0.5)

								# #textInput('Pk_init',"Covariance matrix of particles.\nEnter 4 numbers (comma delimited) for a covariance matrix.   Coerced to PD matrix.",
							    # #    value="0.25,0,0,0.25")
							   
                              ),
                 mainPanel( 
                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("densplot"), plotOutput("pred_loc")),
							splitLayout(cellWidths = c("50%", "50%"), plotOutput("wt_dist_sep"), plotOutput("pred_loc_resamp")),
							splitLayout(cellWidths = c("50%", "50%"), plotOutput("rugplot"), plotOutput("loc_hist")),
  						    splitLayout(cellWidths = c("50%", "50%"), plotOutput("trans_prob"),plotOutput("agree_hist"))

							 )
						)
               )#end of layout
  
)#end of UI definition
,

server=shinyServer(function(input, output, session) 
{
  
   observeEvent( input$run, {
        
		req(input$run)

		old_pars <- par(mfrow=par()$mfrow, mfcol=par()$mfcol, mar=par()$mar, las=par()$las, xpd=par()$xpd, oma=par()$oma)
		on.exit(expr=par(old_pars))	
   
	
        updateSliderInput(session, "iter",  label="Progress of simulation", value=1, min=1, max=input$max_iter, step=1)
		render_delay_step <- 1000 * input$render_delay / input$npart
	    tmat <- matrix(c(1-input$p1to2, input$p1to2, input$p2to1, 1-input$p2to1), byrow=TRUE, ncol=2)
		
	   
		
		dir_prior <- abs(as.numeric(numextractall(input$dir_prior)))
		dir_prior <- pmax(dir_prior, 1.01)
		
		
		#matrix equations

		f <- function(mk,new_logv, theta, dtprev=input$delta_t) {
		  mk <- as.vector(as.numeric(mk))
		  mk[4] <- normalize_angle(mk[4])
				  
		  mk_new <- matrix(c(rep(exp_safe(mk[3])*dtprev,2),
							new_logv, 
							normalize_angle(mk[4]+theta)), ncol=1)
		  
		  mk_new[1:2,] <- mk[1:2] + mk_new[1:2,]*c(cos(mk[4]), sin(mk[4]))  					 
		 
		  mk_new
		}

		h <- function(mk, dtprev=input$delta_t) {
		  mk <- as.vector(as.numeric(mk))
		  mk[4] <- normalize_angle(mk[4])
		  #mk_new <- undo_trans(x=mk[3])*dtprev*c(cos(mk[4]), sin(mk[4]))
		  mk_new <- (mk[3])*dtprev*c(cos(mk[4]), sin(mk[4]))
		  mk_new <- mk_new + mk[1:2]
		
		  matrix(mk_new, nrow=1)

		}

		#here Hx is just matrix multiplication
		Fx <- function(mk, dtprev=input$delta_t) {
		  P <- diag(c(1,1,0,1))
		  mk <- as.vector(as.numeric(mk))
		 
		  P[1:2,3:4] <- exp_safe(mk[3])*dtprev
		  
		  
		  #P[1:2,3:4] <- undo_trans_df(x=mk[3])*dtprev
		  P[cbind(1:2,3:4)] <- P[cbind(1:2,3:4)]*cos(mk[4])
		  P[cbind(1:2,4:3)] <- c(-1,1)*P[cbind(1:2,4:3)]*sin(mk[4])
		
		  P
		}

		Hx <- function(mk, dtprev=input$delta_t) {
		  #P <- matrix(0, ncol=4, nrow=2)
		  P <- cbind(diag(2),diag(2))
		  mk <- as.vector(as.numeric(mk))
		 
		  P[,3:4] <- (mk[3])*dtprev
		  
		  P[cbind(1:2, 3:4)] <- P[cbind(1:2, 3:4)]*cos(mk[4])
		  P[cbind(1:2, 4:3)] <- c(-1,1)*P[cbind(1:2, 4:3)]*sin(mk[4])
		 
		  P
		}
  
  
        yt <- matrix(NA, ncol=6, nrow=input$max_iter +1)
        colnames(yt) <- c("X","Y","vel","bear","turn","state")
		yt[,"turn"] <- 0
		
		box_bounds <- cbind(c(-200,200),c(-200,200))
		colnames(box_bounds) <- c("X","Y")
		
		inside_box <- function(x=c(0,0)) {
			(x[ 1 ] <= box_bounds[2,"X"] & x[ 1 ] >= box_bounds[1,"X"]) & (x[ 2 ] <= box_bounds[2,"Y"] & x[ 2 ] >= box_bounds[1,"Y"]) 
		}

		# start in lower LH corner, and facing a particular way
		yt[1,c("X","Y")] <- box_bounds[1,c("X","Y")] + 0.15*apply(box_bounds, 2, diff)
		yt[1,"bear"] <- normalize_angle(rnorm(1, mean=0.25*pi, sd=sqrt(0.5*input$turn_var)))
		
		vel_mu <- c(input$vel_mu1, input$vel_mu2)
		
		yt[1,"state"] <- which(vel_mu==max(vel_mu))[1]
		
		
		tmat <- matrix(c(1-input$p1to2, input$p1to2, input$p2to1, 1-input$p2to1), byrow=TRUE, ncol=2)
		
	       
		#sample(x=1:2, size=1, prob=c(sum(dir_prior[c(1,3)]), sum(dir_prior[c(2,4)])))
		for (ii in 1:input$max_iter) {
		  yt[ii+1,"state"] <- sample(x=1:2, size=1, prob=tmat[yt[ii,"state"],])
		}


		ydens_range_wts <- c(0, 1e-2)
		
		#plot(x=-1000, y=-10000, xlim=box_bounds[,1], ylim=box_bounds[,2], pch="1")
		#text(x=yt[1,"X"], y=yt[1,"Y"], labels="1")
		vel_mu <- c(input$vel_mu1, input$vel_mu2)	
		
		for (ii in 1:(input$max_iter)) {
			inside <- FALSE	
			z <- yt[ ii, "state"]

			
			while(! inside) {
			
				yt[ ii, "vel"] <- rnorm(n=1, mean=vel_mu[ z ], sd=sqrt(input$vel_var))
				if (ii > 1) { 
					yt[ii-1,"turn"] <- normalize_angle(rnorm(n=1, mean=0, sd=sqrt(input$turn_var))) 
					yt[ii,"bear"] <- normalize_angle(yt[ii-1,"turn"] + yt[ii-1,"bear"])
				}
				
				
				xy <- h(mk=yt[ii, c("X","Y","vel","bear")])
				inside <- inside_box(xy)
					
					
				if (inside) {
								
					#points(xy, pch=as.character(ii+1))
					yt[ ii+1, c("X","Y")] <- xy
					#text(x=yt[ii+1,"X"], y=yt[ii+1,"Y"], labels=as.character(ii+1))
				
				}
			}
		}


		#add measurement noise
		yt_var <- as.numeric(numextractall(input$yt_var))
		yt_var[ c(1,4) ] <- pmax(0.1, abs(yt_var[ c(1,4) ]))
		

		yt_var <- as.matrix(Matrix::nearPD(matrix(yt_var,ncol=2, byrow=TRUE), ensureSymmetry=TRUE)$mat)

		# yt[ -1, c("Xobs","Yobs")] <- yt[ -1, c("X","Y")] + mvtnorm::rmvnorm(n=input$max_iter, mean=c(0,0), sigma=yt_var)
					

		reject_sampling_shiny <- function(mu=c(0,0), cmat=diag(2), ntry=500, prev_val=c(50,50)) {
			inside <- FALSE
			count <- 0
			
			
			while((! inside) & (count <= ntry)) {
				count <- count +1
				xy <- mvtnorm::rmvnorm(n=1, mean=mu, sigma=cmat)
			
				inside <- inside_box(xy[,c("X","Y")])
			}
			if (count==ntry) { xy <- prev_val }
			
			xy
		}
			
		
		
		
		xt_var <- as.numeric(numextractall(input$xt_var))
		xt_var[1] <- abs(xt_var[1])
        xt_var[4] <- input$vel_var			 
		xt_var <- as.matrix(Matrix::nearPD(matrix(xt_var,ncol=2, byrow=TRUE), ensureSymmetry=TRUE)$mat)
		
		# Pk_init <- as.numeric(numextractall(input$Pk_init))
        # Pk_init[c(1,4)] <- abs(Pk_init[c(1,4)])
		# Pk_init <- as.matrix(Matrix::nearPD(matrix(Pk_init,ncol=2, byrow=TRUE), ensureSymmetry=TRUE)$mat)
			
 

		params<- reactiveValues(dnames= list(c("orig","resamp"), paste("i",1:(input$max_iter+1),sep=""), paste("state",1:2,sep=""), paste("p",1:input$npart,sep=""))) 
		xt_names <- c("X","Y","vel","bear")     

		params$test_counter <- rep(NA, input$npart)
	 
		params$Pk = array(0, dim=c(4,4,2,input$max_iter+1,2,input$npart), dimnames=c(list(xt_names,xt_names),params$dnames))
		params$mk = array(0, dim=c(4,2,input$max_iter+1,2,input$npart), dimnames=c(list(xt_names),params$dnames))
		
		
		params$xpart = array(NA, dim=c(4,3,input$max_iter+1,input$npart), dimnames=c(list(xt_names, c("orig","resamp","resamp_hist")),params$dnames[ -c(1,3) ]))
        params$Kgain =array(NA, dim=c(4,2,2,input$max_iter+1,2,input$npart), dimnames=c(list(xt_names,c("X","Y")),params$dnames))
	    params$mk_prev = array(NA, dim=c(4,2,input$max_iter+1,2,input$npart), dimnames=c(list(xt_names),params$dnames))
        params$Pk_prev = array(NA, dim=c(4,4,2,input$max_iter+1,2,input$npart), dimnames=c(list(xt_names,xt_names),params$dnames))
		
		params$MuY = array(NA, dim=c(2,3,input$max_iter+1,2,input$npart), dimnames=c(list(c("X","Y"),c("orig","resamp","resamp_hist")), params$dnames[ -1 ]))
		params$SigY = array(NA, dim=c(2,2,3,input$max_iter+1,2,input$npart), dimnames=c(list(c("X","Y"),c("X","Y"),c("orig","resamp","resamp_hist")), params$dnames[ -1 ]))

		params$MuY_actual = array(NA, dim=c(2,2,input$max_iter+1,input$npart), dimnames=c(list(c("X","Y"),c("resamp","resamp_hist")), params$dnames[ -c(1,3) ]))
		params$SigY_actual = array(NA, dim=c(2,2,2,input$max_iter+1,input$npart), dimnames=c(list(c("X","Y"),c("X","Y"),c("resamp","resamp_hist")), params$dnames[ -c(1,3) ]))
		
		params$Wishart_mat <- array(0, dim=c(2,2,2,2,2,input$npart), 
			dimnames=c(list(c("X","Y"),c("X","Y"),c("obs","particle")), params$dnames[ -2 ]))
		params$Qt <- array(0, dim=c(4,4,2,input$npart), dimnames=c(list(xt_names,xt_names), params$dnames[ -c(2:3) ]))	
		params$Rt <- array(0, dim=c(2,2,2,2,input$npart), dimnames=c(list(c("X","Y"),c("X","Y")), params$dnames[ -2 ]))	
	
			
		bmat <- matrix(c(1, -0.3, -0.3, 1), ncol=2)*input$delta_t
		
		params$Wishart_mat[,,"obs","orig","state1",] <- sqrt(exp(input$vel_mu1))*bmat
		params$Wishart_mat[,,"obs","orig","state2",] <- sqrt(exp(input$vel_mu2))*bmat	
		
		#second space not used
		params$Wishart_mat[,,"particle","orig",1,] <- 5*bmat		
		
		params$df_error <- array(10, dim=c(2,2,2,input$npart), dimnames=c(list(c("obs","particle")), dimnames=params$dnames[ -2 ]))
		
		params$MuY_actual = array(NA, dim=c(2,3,input$max_iter+1,input$npart), dimnames=c(list(c("X","Y"),c("orig","resamp","resamp_hist")), params$dnames[ -c(1,3) ]))
		params$SigY_actual = array(NA, dim=c(2,2,3,input$max_iter+1,input$npart), dimnames=c(list(c("X","Y"),c("X","Y"),c("orig","resamp","resamp_hist")), params$dnames[ -c(1,3) ]))
				
		params$curr_state=array(NA, dim=c(3,input$max_iter+1,input$npart), dimnames=c(list(c("orig","resamp","resamp_hist")), params$dnames[ -c(1,3) ]))
		params$mu_pars= array(NA, dim=c(2,2, input$max_iter+1,2,input$npart), dimnames=c(list(c("mu_mean","mu_var")),params$dnames))
		params$indices=array(NA,dim=c(input$max_iter+1, input$npart))
		params$wts=array(NA,dim=c(input$max_iter+1,2,input$npart), dimnames=params$dnames[-1])
		params$mu_pars["mu_mean","orig",,"state1",] <- input$mu_mean1
   		params$mu_pars["mu_mean","orig",,"state2",] <- input$mu_mean2
		params$mu_pars["mu_var","orig",,,] <- input$mu_var
		params$dir_params =array(NA, dim=c(4,2,input$max_iter+1,input$npart), dimnames=c(list(c("a11","a12","a21","a22")), params$dnames[ -3 ]))
		params$trans_draws =array(NA, dim=c(2,2,2,input$max_iter+1,input$npart), dimnames=c(list(c("1to","2to"),1:2),params$dnames[ -3 ]))
		
		params$mu_guess = array(NA, dim=c(2,input$max_iter+1,2,input$npart), dimnames=params$dnames)
		params$vel_guess = array(NA, dim=c(2,input$max_iter+1,2,input$npart), dimnames=params$dnames)

		params$resamp_colors = array(NA, dim=c(2,input$max_iter+1, input$npart), dimnames=params$dnames[ -3 ])
		params$is_new_step = rep(TRUE, input$max_iter)

  	    #initial guesses
		params$dir_params[,"orig","i1",] <- dir_prior
		
		#if known just use values
		if (input$tp_known) {
			params$trans_draws["1to",2,,,] <- input$p1to2
			params$trans_draws["1to",1,,,] <- 1-input$p1to2
						
			params$trans_draws["2to",1,,,] <- input$p2to1
			params$trans_draws["2to",2,,,] <- 1-input$p2to1
		}
		else {
			params$trans_draws["1to",1:2,"orig","i1",] <- pmax(apply(params$dir_params[c("a11","a12"),"orig","i1",], 2, function(x) MCMCpack::rdirichlet(n=1, alpha=x)), 1e-20)
			params$trans_draws["2to",1:2,"orig","i1",] <- pmax(apply(params$dir_params[c("a21","a22"),"orig","i1",], 2, function(x) MCMCpack::rdirichlet(n=1, alpha=x)), 1e-20)
		}

		
		
		#know which one you start in
		params$curr_state["orig","i1",] <- yt[1,"state"]

		
   	    if (input$sep_col==TRUE) { #
			rainbow_cols <- colorspace::rainbow_hcl(n=input$npart +1, c=200,l=70, alpha=0.5)[1:input$npart]
			#rainbow_cols <- colorspace::rainbow_hcl(n=input$npart +1, c=200,l=seq(40, 100, length.out=input$npart+1), alpha=0.5)[1:input$npart] }
		}
        else { rainbow_cols <- rep("lightgray",input$npart) }			

			
		for (kk in 1:2) {

			params$mu_guess["orig","i1",kk,] <- rnorm(n=input$npart, mean=params$mu_pars["mu_mean","orig",1,kk,], sd=sqrt(params$mu_pars["mu_var","orig",1,kk,]))
			params$vel_guess["orig","i1",kk,] <- rnorm(n=input$npart, mean=params$mu_guess["orig",1,kk,], sd=sqrt(input$vel_var))
	    }
		#for some reason it gives you a dumb error because it keeps input$iter as the last one for some reason  

		for (ii in 2:(input$max_iter+1)) { 
	 
			params$mu_guess["orig",ii,,] <- params$mu_guess["orig","i1",,] 
			params$vel_guess["orig",ii,,] <- params$vel_guess["orig","i1",,] 
		}
		#initialize
		
		for (nn in 1:input$npart) {

			params$Pk[1:2,1:2,"orig",input$iter,1:2,nn] <- bmat#MCMCpack::riwish(v=params$df_error["particle","orig",1,nn], S=params$Wishart_mat[,,"particle","orig",1,nn])						 

		}
		
			 
		 #initialize the first time.  after 
	    params$mk["vel","orig","i1",,] <- params$vel_guess["orig","i1",,]
		params$mk[-3,"orig","i1",,] <- t(yt[1,c("X","Y","bear")])
		
		for (nn in 1:input$npart) {
			params$xpart[,"orig","i1",nn] <- params$mk[,"orig","i1",yt[1,"state"],nn]
		} 

		xdens_range1 <- input$vel_mu1 + c(-2.5,2.5)*sqrt(input$mu_var)
		xdens_range2 <- input$vel_mu2 + c(-2.5,2.5)*sqrt(input$mu_var)

			
		xdens_range <- nice_range(range(c(xdens_range1, xdens_range2)), ep=0.4)
	
		ydens_range <- c(0, 1.33*dnorm(0, 0, sqrt(input$vel_var)))
		
		part_axis <- seq(1, input$npart, by=min(5,round(input$npart/2)))
		
		dens_pts <- seq(xdens_range[1], xdens_range[2], length.out=150) 

        dens_pts1 <- seq(xdens_range1[1], xdens_range1[2], length.out=75) 
		dens_pts2 <- seq(xdens_range2[1], xdens_range2[2], length.out=75) 
		
		params$Pk[3:4,3:4,"orig",1,,] <- diag(c(input$vel_var, input$turn_var))
		
		#for first, want to make sure stays in the right direction
		params$Qt[3:4,3:4,"orig",] <- diag(c(input$vel_var, 0.05))
				
		
				
		plot.new()
        par(xpd=TRUE, las=1) 	

		
		observeEvent( input$iter, { 
					req(input$run)
					req(input$iter > 1)	
					

					if (params$is_new_step[ input$iter ] & input$iter > 1) {
						#a quick fix to set future values to TRUE
						params$is_new_step[ (input$iter+1):(input$max_iter)] <- TRUE
					
						#params$resamp_colors["orig",input$iter,order(apply(params$mu_guess["orig",input$iter,,],2,min))] <- rainbow_cols 
					    params$resamp_colors["orig",input$iter,] <- rainbow_cols
					    params$wts_counter <- 0
					    params$locs_counter <- 0
						
						for (nn in 1:input$npart) {

							params$Qt[1:2,1:2,"orig",nn] <- MCMCpack::riwish(v=params$df_error["particle","orig",1,nn], S=params$Wishart_mat[,,"particle","orig",1,nn])
						
							for (kk in 1:2) {					
								#print("wish")
								#print(params$Wishart_mat[,,"obs","orig",kk,nn])
								params$Rt[,,"orig",kk,nn] <- MCMCpack::riwish(v=params$df_error["obs","orig",kk,nn], S=params$Wishart_mat[,,"obs","orig",kk,nn])

								
								params$mk_prev[,"orig",input$iter,kk,nn] <- f(mk=params$mk[,"orig",input$iter-1,kk,nn], new_logv=params$vel_guess["orig",input$iter,kk,nn], theta=0) 
							
								Fxmat <- Fx(mk=params$mk_prev[,"orig",input$iter,kk,nn])
								
								params$Pk_prev[,,"orig",input$iter,kk,nn] <- as.matrix(Matrix::nearPD(Fxmat%*%params$Pk[,,"orig",input$iter-1,kk,nn]%*%t(Fxmat) + params$Qt[,,"orig",nn], ensureSymmetry=TRUE)$mat)
								params$MuY[,"orig",input$iter,kk,nn] 	<- h(mk=params$mk_prev[,"orig",input$iter,kk,nn])
								

		
								
								Hxmat <- Hx(mk=params$mk_prev[,"orig",input$iter,kk,nn])
							
								params$SigY[,,"orig",input$iter,kk,nn] 	<- as.numeric(Matrix::nearPD(Hxmat%*%params$Pk_prev[,,"orig",input$iter,kk,nn]%*%t(Hxmat) + params$Rt[,,"orig",kk,nn], ensureSymmetry=TRUE)$mat)
							
							}

						}
						
											
                    }
					
					#density of particle velocities	
					output$densplot <- renderPlot({ 
					    #par(mfrow=c(1,2))
					    if (input$iter > 0) {

							densplot_twostate(dpts=dens_pts, mu_guess=params$mu_guess["orig",input$iter,,], norm_sds=sqrt(input$vel_var), known_mean=c(input$vel_mu1, input$vel_mu2),
									colors=params$resamp_colors["orig",input$iter,], ylims=ydens_range, xlims=xdens_range, before_after="before",
									sep_col=input$sep_col, npart=input$npart, var_name="log-speed")
									
							#densplot(dpts=dens_pts, mu_guess=params$mu_guess["orig",input$iter,"state1",], norm_sds=sqrt(input$vel_var), known_mean=input$vel_mu1,
							#		colors=params$resamp_colors["orig",input$iter,], ylims=ydens_range, xlims=xdens_range, before_after="before",
							#		sep_col=input$sep_col, npart=input$npart, iter=input$iter,
							#		main_title="Velocity guess type 1, t=")
											
									
							#densplot(dpts=dens_pts, mu_guess=params$mu_guess["orig",input$iter,"state2",], norm_sds=sqrt(input$vel_var), known_mean=input$vel_mu2,
							#		colors=params$resamp_colors["orig",input$iter,], ylims=ydens_range, xlims=xdens_range, before_after="before",
							#		sep_col=input$sep_col, npart=input$npart, iter=input$iter,
							#		main_title="Velocity guess type 2, t=")
							par(mfrow=c(1,1))		
						}
					})
					
					
					
					if (params$is_new_step[ input$iter ]) {			
					
						#resample
						
						ynext <- yt[input$iter+1,c("X","Y")]

						for (nn in 1:input$npart) {
														
							params$wts[input$iter,"state1",nn] <- mvtnorm::dmvnorm(x=ynext, mean=params$MuY[,"orig",input$iter,"state1",nn], sigma=params$SigY[,,"orig",input$iter,"state1",nn])
							params$wts[input$iter,"state2",nn] <- mvtnorm::dmvnorm(x=ynext, mean=params$MuY[,"orig",input$iter,"state2",nn], sigma=params$SigY[,,"orig",input$iter,"state2",nn])
							
							
							params$wts[input$iter,,nn] <- pmax(params$wts[input$iter,,nn], 1e-60)

							#multiply by transition probabilities
							params$wts[input$iter,,nn] <- params$wts[input$iter,,nn] * params$trans_draws[ params$curr_state["orig",input$iter-1,nn],, "orig", input$iter-1,nn]
													
						}
						
						# apply state favor
						params$wts[input$iter,"state2",] <- params$wts[input$iter,"state2",] * input$state2_favor
						
						params$wts[input$iter,,] <- pmax(params$wts[input$iter,,], 1e-60)
						
						#params$wts[ input$iter, is.na(params$wts[input$iter,])] <- 1#1e-15
						qwt <- quantile(x=params$wts[input$iter,,], probs=0.85)
						
						params$indices[input$iter,] <- low_var_sample(wts=colSums(params$wts[input$iter,,]), M=input$npart)
						
						
						if (! input$sep_col) { params$resamp_colors["orig",input$iter, colSums(params$wts[input$iter,,]) >qwt ] <- "red" }
						
					}
					
					output$pred_loc <- renderPlot({
						 if (input$iter > 1) {	
							
							pred_loc_2D(yt=yt[1:(input$iter+1),c("X","Y")], bds=box_bounds, start_pt=params$xpart[c("X","Y"),"orig",input$iter-1,], MuY=params$MuY[,"orig",input$iter,,], 
										SigY=params$SigY[,,"orig",input$iter,,], conf=input$conf_level, wts=params$wts[input$iter,,], before_after="before",  npart=input$npart,
										colors=params$resamp_colors["orig",input$iter,], states=NULL) 


						 }	
					})
					
					
					# #next location
					
					#main problem: this is reacting too late because the locations are the same in both before/after plots
				   		
					if (params$is_new_step[ input$iter ] & input$iter > 1) {
						ord <- params$indices[input$iter, ]
							
						params$curr_state["resamp",input$iter-1,] <- params$curr_state["orig",input$iter-1,ord]
						params$curr_state["resamp_hist",input$iter-1,] <- params$curr_state["resamp",input$iter-1,]
						
						# draw new states according to relative wts
						params$curr_state["orig",input$iter,] <- apply(params$wts[input$iter,,ord], 2, function(x) low_var_sample(wts=x, M=1))

						params$mk[,"resamp",input$iter-1,,] <- params$mk[,"orig",input$iter-1,, ord]
						params$mk_prev[,"resamp",input$iter,,] <- params$mk_prev[,"orig",input$iter,, ord]
						params$Pk[,,"resamp",input$iter-1,,] <- params$Pk[,,"orig",input$iter-1,, ord]
						params$Pk_prev[,,"resamp",input$iter,,] <- params$Pk_prev[,,"orig",input$iter,, ord]
						params$mu_guess["resamp",input$iter,,] <- params$mu_guess["orig",input$iter,, ord]
						params$vel_guess["resamp",input$iter,,] <- params$vel_guess["orig",input$iter,, ord]
						params$Kgain[,,"resamp",input$iter-1,,] <- params$Kgain[,,"orig",input$iter-1,, ord]
						
						params$MuY[,"resamp",input$iter,,] <- params$MuY[,"resamp_hist",input$iter,,] <- params$MuY[,"orig",input$iter,, ord]  
						params$SigY[,,"resamp",input$iter,,] <- params$SigY[,,"resamp_hist",input$iter,,] <- params$SigY[,,"orig",input$iter,, ord]      
						params$xpart[,"resamp_hist",input$iter-1,] <- params$xpart[,"resamp",input$iter-1,] <- params$xpart[,"orig",input$iter-1, ord]
						
						params$Wishart_mat[,,,"resamp",,] <- params$Wishart_mat[,,,"orig",,ord] 
						params$Qt[,,"resamp",] <- params$Qt[,,"orig",ord]
						
						
						params$Rt[,,"resamp",,] <- params$Rt[,,"orig",,ord] 
						params$df_error[,"resamp",,] <- params$df_error[,"orig",,ord]
							
						if (input$iter > 2) {
							params$xpart[,"resamp",1:(input$iter-2),] <- params$xpart[,"resamp",1:(input$iter-2),ord]
							
						}

						for (nn in 1:input$npart) {
							z <- params$curr_state["orig",input$iter,nn]
																				
							params$MuY_actual[,"resamp",input$iter,nn] <- params$MuY[,"resamp",input$iter,z,nn]
							params$SigY_actual[,,"resamp",input$iter,nn] <- params$SigY[,,"resamp",input$iter,z,nn]

						}

						# if (input$iter >1 ) {
							# params$curr_state["resamp", 1:(input$iter-1),] <- params$curr_state["resamp", 1:(input$iter-1),ord]
							# params$MuY[,"resamp",1:(input$iter-1),,] <- params$MuY[,"resamp",1:(input$iter-1),,ord]
							# params$SigY[,,"resamp",1:(input$iter-1),,] <- params$SigY[,,"resamp",1:(input$iter-1),,ord]
							# #params$MuY_actual[,"resamp",1:(input$iter-1),] <- params$MuY_actual[,"resamp",1:(input$iter-1),ord]
							# #params$SigY_actual[,,"resamp",1:(input$iter-1),] <- params$SigY_actual[,,"resamp",1:(input$iter-1),ord]
							
							# params$xpart[,"resamp",1:(input$iter-1),] <- params$xpart[,"resamp",1:(input$iter-1),ord]
						# }
						
						
						params$mu_pars[,"resamp",input$iter,,] <- params$mu_pars[,"orig",input$iter,, ord]
						params$resamp_colors["resamp",input$iter,] <- params$resamp_colors["orig",input$iter, ord]
						
						params$dir_params[,"resamp",input$iter-1,] <- params$dir_params[,"orig",input$iter-1,ord]
						
						if (! input$tp_known) { params$trans_draws[,,"resamp",input$iter-1,] <- params$trans_draws[,,"orig",input$iter-1,ord] }
						
				
						
					}

					ydens_range_wts <- max(colSums(params$wts[input$iter,,]))
					ydens_range_wts <- c(0, ydens_range_wts*1.33)
					

						
						
				
					#distribution of weights
					output$wt_dist_sep <- renderPlot({
					   #should have a panel for each state and combined

							
						if (input$iter > 1) {	

							if ((! params$is_new_step[ input$iter ]) & params$is_new_step[ input$iter+1 ]==TRUE) {
					
								invalidateLater(millis=render_delay_step)
								isolate(params$wts_counter <- min(input$npart, params$wts_counter + 1))
							
							
								
							
								m <- wt_dist_twostate_loop(wts=params$wts[input$iter,,], xlims=xdens_range, ylims=ydens_range_wts, known_mean=c(input$vel_mu1, input$vel_mu2), 
													  mu_guess=params$mu_guess["orig", input$iter,,], iter=input$iter,
													  npart=input$npart, colors=params$resamp_colors["orig",input$iter,], sep_col=input$sep_col, 
													  index=params$indices[ input$iter, params$wts_counter], var_name="log-speed",
													  behavior=params$curr_state["orig", input$iter, params$wts_counter])

				
								m
									
							}	
							else {								
					
								wt_dist_twostate(wts=params$wts[input$iter,,], xlims=xdens_range, ylims=ydens_range_wts, known_mean=c(input$vel_mu1, input$vel_mu2), 
												mu_guess=params$mu_guess["orig", input$iter,,], iter=input$iter, var_name="log-speed",
												npart=input$npart, colors=params$resamp_colors["orig",input$iter,], sep_col=input$sep_col,
												behavior=yt[input$iter, "state"])
													
														
							
							}
						
						}	
					})



					
					output$pred_loc_resamp <- renderPlot({
						
						if (input$iter > 1) {	
							
							if (params$is_new_step[ input$iter ]==FALSE & params$is_new_step[ input$iter + 1 ]==TRUE) {

								invalidateLater(millis=render_delay_step)
								isolate(params$locs_counter <- min(input$npart, params$locs_counter + 1))
							
						
								pred_loc_2D_loop(yt=yt[1:(input$iter+1),c("X","Y")], bds=box_bounds, start_pt=params$xpart[c("X","Y"),"resamp",input$iter-1,], MuY=params$MuY_actual[,"resamp",input$iter,], 
												 SigY=params$SigY_actual[,,"resamp",input$iter,], conf=input$conf_level, wts=params$wts[input$iter,,], before_after="after", npart=input$npart,
												 colors=params$resamp_colors["resamp",input$iter,], index=params$locs_counter, states=params$curr_state["orig",input$iter,],
												 resamp_indices=params$indices[input$iter,])

								
							}
							else {
																	
									# pred_loc_2D(yt=yt[1:(input$iter+1),c("X","Y")], bds=box_bounds, start_pt=params$xpart[c("X","Y"),"resamp",input$iter-1,], MuY=params$MuY_actual[,"resamp",input$iter,], 
												# SigY=params$SigY_actual[,,"resamp",input$iter,], conf=input$conf_level, wts=params$wts[input$iter,,], before_after="after", npart=input$npart,
												# colors=params$resamp_colors["resamp",input$iter,], states=params$curr_state["resamp_hist",input$iter-1,]) 

									# pred_loc_2D(yt=yt[1:(input$iter+1),c("X","Y")], bds=box_bounds, start_pt=params$xpart[c("X","Y"),"resamp",input$iter-1,], MuY=params$MuY_actual[,"resamp",input$iter,], 
												# SigY=params$SigY_actual[,,"resamp",input$iter,], conf=input$conf_level, wts=params$wts[input$iter,,], before_after="after", npart=input$npart,
												# colors=params$resamp_colors["resamp",input$iter,], states=params$curr_state["orig",input$iter,]) 
									if (input$iter > 2) {
										# here input$iter flips to next
										pred_loc_2D(yt=yt[1:(input$iter),c("X","Y")], bds=box_bounds, start_pt=params$xpart[c("X","Y"),"resamp",input$iter-2,], MuY=params$MuY_actual[,"resamp",input$iter-1,], 
													SigY=params$SigY_actual[,,"resamp",input$iter-1,], conf=input$conf_level, wts=params$wts[input$iter-1,,], before_after="after", npart=input$npart,
													colors=params$resamp_colors["resamp",input$iter-1,], states=params$curr_state["orig",input$iter-1,]) 
									}
							}
										
						   
						
						 }		
					})
					
					

					
					
					if (params$is_new_step[ input$iter ] & input$iter > 1) {
					

						#draw state according to weights
						
						
						for (nn in 1:input$npart) {
							#now draw for next time
							z <- params$curr_state["orig",input$iter,nn]
							#only update for the predicted state that actually happened
							
							Hxmat <- Hx(mk=params$mk_prev[,"resamp",input$iter,z, nn ])
					
							params$Kgain[,,"orig",input$iter,z, nn ] <- params$Pk_prev[,,"resamp",input$iter,z, nn ]%*%t(Hxmat)%*%MASS::ginv(params$SigY[,,"resamp",input$iter,z, nn ])
							
							params$mk[,"orig",input$iter,z, nn ] <- params$mk_prev[,"resamp",input$iter,z, nn ] + params$Kgain[,,"orig",input$iter,z, nn ]%*%t(ynext-h(mk=params$mk_prev[,"resamp",input$iter,z, nn ]))
							params$Pk[,,"orig",input$iter,z, nn ] <- as.matrix(Matrix::nearPD(params$Pk_prev[,,"resamp",input$iter,z, nn ] - params$Kgain[,,"orig",input$iter,z, nn ]%*%(params$SigY[,,"resamp",input$iter, z, nn ]%*%t(params$Kgain[,,"orig",input$iter,z, nn ])), ensureSymmetry=TRUE)$mat)
							
							z_oth <- which(1:2 !=z)
							params$Kgain[,,"orig",input$iter,z_oth, nn ] <- params$Kgain[,,"resamp",input$iter-1,z_oth, nn ]
							
							params$mk["bear","orig",input$iter,z, nn ] <- normalize_angle(params$mk["bear","orig",input$iter,z, nn ])
							
							
							#for mk, take the actual predicted state and use it to update the other, so that start at the same x location. 
							#the velocity will be done to make mk_prev different
							#for Pk and Kgain only update the one that happened
							params$mk[,"orig",input$iter,z_oth, nn ] <- params$mk[,"orig",input$iter,z, nn ]
							params$Pk[,,"orig",input$iter,z_oth, nn ] <- params$Pk[,,"orig",input$iter,z, nn ]
							
							
						
						}
						
						#now propagate next values
						curr_state <- params$curr_state["orig",input$iter,]
						prev_state <- params$curr_state["resamp",input$iter-1,]
						alpha_index <- paste("a",prev_state,curr_state, sep="")
					
						#set mu pars for next to current, then overwrite with the one observed
						
						params$mu_pars["mu_mean","orig",input$iter+1,,] <- params$mu_pars["mu_mean","resamp",input$iter,,]
						params$mu_pars["mu_var","orig",input$iter+1,,] <-  params$mu_pars["mu_var","resamp",input$iter,,]
					
						#update error matrices
						params$df_error["obs","orig",,][ rbind(curr_state, 1:input$npart) ] <- params$df_error["obs","resamp",,][ rbind(curr_state, 1:input$npart) ] +1
						params$Rt[,,"orig",,] <- params$Rt[,,"resamp",,]
						params$Qt[,,"orig",] <- params$Qt[,,"resamp",]
						params$Qt[3:4,3:4,"orig",] <- diag(c(input$vel_var, input$turn_var))

						for (nn in 1:input$npart) {

							z <- curr_state[ nn ]
						  							
							params$xpart[,"orig",input$iter, nn ] <- reject_sampling_shiny(mu=params$mk[,"orig",input$iter,z,nn], cmat=params$Pk[,,"orig",input$iter,z,nn],
																						prev_val=params$xpart[,"resamp",input$iter-1, nn ])
                           
							# #now just make it the most recent
							# for (kk in 1:2) {
								# params$mk[,"orig",input$iter,kk, nn ] <- params$xpart[1:4,"orig",input$iter,nn]
							# }
						   
						   
						    if (input$iter > 2) {
								pd <- as.vector(params$xpart[c("X","Y"),"resamp",input$iter-1,nn] - h(mk=params$xpart[c("X","Y","vel","bear"),"resamp",input$iter-2,nn]))
								#pd <- as.vector(params$xpart[c("X","Y"),"resamp",input$iter-1,nn] - params$mk[c("X","Y"),"resamp",input$iter-1,nn])
								
								
								#print(params$xpart[,"resamp",1:(input$iter-1),nn])
							
								#print( h(mk=params$xpart[c("X","Y"),"resamp",input$iter-2,nn]))
								#print( pd%*%t(pd))
								params$df_error["particle","orig",1,] <- params$df_error["particle","resamp",1,]+1
								params$Wishart_mat[,,"particle","orig",1,nn] <- as.matrix(Matrix::nearPD(keep_finite(params$Wishart_mat[,,"particle","resamp",1,nn] + pd%*%t(pd)), ensureSymmetry=TRUE)$mat )
							}
						   
							pd <- as.vector(params$MuY[,"resamp",input$iter,z,nn] - ynext)
					
							params$Wishart_mat[,,"obs","orig",z,nn] <-  as.matrix(Matrix::nearPD(keep_finite(params$Wishart_mat[,,"obs","resamp",z,nn] + pd%*%t(pd)), ensureSymmetry=TRUE)$mat )
						   
						
						   # #now update values of parameters for state observed
					       
						   params$mu_pars["mu_var","orig",input$iter+1,z,nn] <- 1/(1/params$mu_pars["mu_var","resamp",input$iter,z,nn] + 1/input$vel_var)
					       params$mu_pars["mu_mean","orig",input$iter+1,z,nn] <- (params$mu_pars["mu_mean","resamp",input$iter,z,nn]/params$mu_pars["mu_var","resamp",input$iter,z,nn] + params$xpart["vel","orig",input$iter,nn]/input$vel_var)*params$mu_pars["mu_var","orig",input$iter+1,z,nn]
					       
						   
				           #params$mu_pars["mu_var","orig",input$iter+1,next_state[ nn ],nn] <- 1/(1/params$mu_pars["mu_var","resamp",input$iter,next_state[ nn ],nn] + 1/input$vel_var)
					       #params$mu_pars["mu_mean","orig",input$iter+1,next_state[ nn ],nn] <- (params$mu_pars["mu_mean","resamp",input$iter,next_state[ nn ],nn]/params$mu_pars["mu_var","resamp",input$iter,next_state[ nn ],nn] + params$xpart[2,"orig",input$iter+1,nn]/input$vel_var)*params$mu_pars["mu_var","orig",input$iter+1,next_state[ nn ],nn]
					       #copy and update only value

						    if (! input$tp_known) {
								params$dir_params[,"orig",input$iter,nn] <- params$dir_params[,"resamp",input$iter-1, nn ]
								#params$dir_params[ alpha_index[ nn ],"orig",input$iter+1,nn] <- params$dir_params[alpha_index[ nn ],"orig",input$iter+1,nn] +1
								params$dir_params[ alpha_index[ nn ],"orig",input$iter,nn] <- params$dir_params[alpha_index[ nn ],"orig",input$iter,nn] +1
						    }
						}
                
                    }

				    # #new parameter draws
					for (kk in 1:2) {
						params$mu_guess["orig",input$iter+1,kk,] <- rnorm(n=input$npart, mean=params$mu_pars["mu_mean","orig",input$iter+1,kk,], sd=sqrt(params$mu_pars["mu_var","orig",input$iter+1,kk,]))
						params$vel_guess["orig",input$iter+1,kk,] <- rnorm(n=input$npart, mean=params$mu_guess["orig",input$iter+1,kk,], sd=sqrt(input$vel_var))      
					}
					
				    if (input$tp_known==FALSE) {
						params$trans_draws["1to",1:2,"orig",input$iter,] <- pmax(apply(params$dir_params[c("a11","a12"),"orig",input$iter,], 2, function(x) MCMCpack::rdirichlet(n=1, alpha=x)), 1e-20)
						params$trans_draws["2to",1:2,"orig",input$iter,] <- pmax(apply(params$dir_params[c("a21","a22"),"orig",input$iter,], 2, function(x) MCMCpack::rdirichlet(n=1, alpha=x)), 1e-20)
					}
					
					output$rugplot <- renderPlot({

						if (input$iter > 0) {
						
							convergence_rugplot_twostate(xlims=xdens_range, known_mean=c(input$vel_mu1, input$vel_mu2), max_iter=input$max_iter, iter=input$iter, 
							 mu_guess=params$mu_guess, npart=input$npart, colors=params$resamp_colors, var_name="log-speed") 
						
							# par(mfrow=c(1,2))
							# convergence_rugplot(xlims=xdens_range, known_mean=input$vel_mu1, max_iter=input$max_iter, iter=input$iter, 
									# mu_guess=params$mu_guess[,,"state1",], npart=input$npart, 
									# main_title="Convergence of log-speed\nmean, type 1", var_name="log-speed") 
									
							# convergence_rugplot(xlims=xdens_range, known_mean=input$vel_mu2, max_iter=input$max_iter, iter=input$iter, 
									# mu_guess=params$mu_guess[,,"state2",], npart=input$npart,
									# main_title="Convergence of log-speed\nmean, type 2", var_name="log-speed") 
						}					
					})


					output$loc_hist <- renderPlot({
					
					    if (input$iter > 1) {
						
							locs_tmp <- array(NA, dim=c(2, input$iter, input$npart), dimnames=list(c("X","Y"), 1:input$iter, 1:input$npart))
							locs_tmp[,1:(input$iter-1),] <- params$xpart[c("X","Y"),"resamp",1:(input$iter-1),]
							locs_tmp[,input$iter,] <- params$xpart[c("X","Y"),"orig",input$iter,]
							
							
							#locations with
							pred_loc_dens_2D(yt=yt[1:(input$iter+1),c("X","Y")], bds=box_bounds, locs=locs_tmp)  
								
							
					  }
					})	

				    output$agree_hist <- renderPlot({
						if (input$iter > 0) {
			       
							#state_agreement(particle_states=params$Ydist_actual["state","resamp_hist",,], actual_states=yt[,"state"], 
							#				iter=input$iter, npart=input$npart) 
							
							states_tmp <- params$curr_state["resamp",,]
							states_tmp[input$iter,] <- params$curr_state["orig",input$iter,]
							
							state_agreement(particle_states=states_tmp, actual_states=yt[,"state"], 
											iter=input$iter, npart=input$npart) 
						}		
					})
					
					output$trans_prob <- renderPlot({

						if (input$iter > 0 & input$tp_known==FALSE) {		
	
							probability_trans(true_probs=c(input$p1to2, input$p2to1), dir_params=params$dir_params[,"orig", input$iter,],
  										      npart=input$npart, colors=params$resamp_colors["resamp",input$iter,])
						}		
					})

									
  				params$is_new_step[ input$iter ] <- FALSE

					

				})#end of iteration 
	
	 
		  }) #if run
		  
 }
)
)

}

