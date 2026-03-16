#source(system.file("cdlm_robot_funcs.R")
cdlm_robot <- function() {
shinyApp(

ui=shinyUI(fluidPage(
  titlePanel("Particle filter example of estimating mean velocity and location of 1-D robot"),
  
  
  sidebarLayout(position = "left",
                             sidebarPanel("Simulation parameters", width=3,
							 #sliderInput('speed',"Animation speed (sec.)",value=3, min=1, max=10, round=TRUE, step=1),
							   #uiOutput("speed"),	
							   #uiOutput("iter"),
				              sliderInput('iter',"Progress of simulation",value=1, min=1, max=40, round=TRUE, step=1,
				                 animate=animationOptions(interval=7000, loop=FALSE)),
							   actionButton('run',"Accept settings, press play above"),
								sliderInput('max_iter','Maximum number of iterations',value=20, min=1, max=40, round=TRUE, step=1),
								checkboxInput('sep_col',"Color particles differently (not recommended for large number of particles)",value=TRUE),								
								sliderInput('npart','Number of particles',value=6, min=2, max=40, round=TRUE, step=1),
								sliderInput('vel_mu',"Unknown true mean of velocity (units/sec)\n(for best results make sure distribution of velocity is\nfirmly positive or negative)",value=4, min=-10, max=10, round=FALSE, step=0.1),
								sliderInput('vel_var','Known true variance of velocity',value=1, min=0.1, max=5, round=FALSE, step=1),
								sliderInput('delta_t','Time step (sec)',value=1, min=0.1, max=10, round=FALSE, step=1),
								sliderInput('mu_mean','Prior mean (units/sec) of velocity mean',value=4, min=-10, max=10, round=FALSE, step=1),
								sliderInput('mu_var','Prior variance on velocity mean',value=2, min=0.1, max=5, round=FALSE, step=1),
								textInput('xt_var',"Covariance error matrix (Qt) of position and velocity.\nEnter 4 numbers (comma delimited) for a covariance matrix, where the 4th should be the same as the velocity variance.  Coerced to PD matrix.",
							        value="0.1,0,0,0.1"),
								sliderInput('yt_var',"Measurement error variance (Rt) of observed location", value=0.05, min=0.001, max=1, round=FALSE, step=0.1),
								textInput('Pk_init',"Covariance matrix of particles.\nEnter 4 numbers (comma delimited) for a covariance matrix.   Coerced to PD matrix.",
							        value="0.25,0,0,0.25"),
								sliderInput('render_delay','Render delay (sec)',value=3, min=0.1, max=5, round=TRUE, step=0.5)

							   ),
                 mainPanel( 
                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("densplot"), plotOutput("pred_loc")),
							splitLayout(cellWidths = c("50%", "50%"), plotOutput("wt_dist"), plotOutput("pred_loc_resamp")),
							splitLayout(cellWidths = c("50%", "50%"), plotOutput("rugplot"),plotOutput("loc_hist"))
							 )
						)
               )#end of layout
  
)#end of UI definition
,

server=shinyServer(function(input, output, session) 
{

        # #define empty params object
        # params <- reactiveValues(dnames=NULL, Pk=NULL, mk=NULL, xpart=NULL, Kgain=NULL, mk_prev=NULL,
		                         # Pk_prev=NULL, Ydist=NULL, mu_pars=NULL, indices=NULL, wts=NULL, resamp_colors=NULL,
								 # is_new_step=NULL, mu_guess=NULL, vel_guess=NULL, reset=0, wts_counter=0, locs_counter=0)
		     
	
    observeEvent( input$run , { 
		req(input$run)
		
		old_pars <- par(mfrow=par()$mfrow, mfcol=par()$mfcol, mar=par()$mar, las=par()$las, xpd=par()$xpd, oma=par()$oma)
		on.exit(expr=par(old_pars))	
		
		updateSliderInput(session, "iter",  label="Progress of simulation", value=1, min=1, max=input$max_iter, step=1)
		render_delay_step <- 1000 * input$render_delay / input$npart

	    #output$speed <- renderUI({
		#	numericInput("speed","Speed Value :",value = 1.5)
		#})
		#output$iter <- renderUI({
		#	sliderInput("iter","Progress of simulation",value=0, min=0, max=input$max_iter, round=TRUE, step=1, 
		#	animate=animationOptions(interval=1500, loop=FALSE))
		#})
		
	    #plot.new()	
	    #updateSliderInput(session, "npart", label="Number of particles",value=input$npart, min=2, max=40, round=TRUE, step=1)	   
	    #updatesliderInput(session, 'vel_mu',label="Unknown true mean of velocity\n(for best results make sure distribution of velocity is\nfirmly positive or negative)",value=4, min=-10, max=10, round=FALSE, step=1)
		#updatesliderInput(session, 'vel_var',label='Known true variance of velocity',value=1, min=0.1, max=5, round=FALSE, step=1)
		#updatesliderInput(session, 'delta_t',label='Time step (sec)',value=1, min=0.1, max=10, round=FALSE, step=1)
		#updatesliderInput(session, 'mu_mean',label='Prior mean of velocity mean',value=4, min=-10, max=10, round=FALSE, step=1)
		#updatesliderInput(session, 'mu_var',label='Prior variance on velocity mean',value=2, min=0.1, max=5, round=FALSE, step=1)								
	 
	    yt <- matrix(0, ncol=2, nrow=input$max_iter +1)
        colnames(yt) <- c("true_v","X")
		
        yt[,"true_v"] <- rnorm(n=input$max_iter +1, mean=input$vel_mu, sd=sqrt(input$vel_var))
	    yt[,"X"] <- cumsum(c(0, input$delta_t*yt[1:input$max_iter ,"true_v"]))
		
		#add measurement noise
		#yt[-1,"obs_loc"] <- yt[-1,"true_loc"] + rnorm(n=input$max_iter, mean=0, sd=sqrt(input$yt_var))
		
		xt_var <- as.numeric(numextractall(input$xt_var))
		xt_var[1] <- abs(xt_var[1])
        xt_var[4] <- input$vel_var			 
		xt_var <- as.matrix(Matrix::nearPD(matrix(xt_var,ncol=2, byrow=TRUE), ensureSymmetry=TRUE)$mat)
		
		Pk_init <- as.numeric(numextractall(input$Pk_init))
        Pk_init[c(1,4)] <- abs(Pk_init[c(1,4)])
		Pk_init <- as.matrix(Matrix::nearPD(matrix(Pk_init,ncol=2, byrow=TRUE), ensureSymmetry=TRUE)$mat)
		
		
		params <- reactiveValues(dnames= list(c("orig","resamp"), paste("i",1:(input$max_iter+1),sep=""),paste("p",1:input$npart,sep="")))      
		
		params$Pk = array(Pk_init, dim=c(2,2,2,input$max_iter+1,input$npart), dimnames=c(list(1:2,1:2),params$dnames))
		params$mk = array(0, dim=c(2,1,2,input$max_iter+1,input$npart), dimnames=c(list(1:2,1),params$dnames))
		params$xpart = array(NA, dim=c(2,3,input$max_iter+1,input$npart), dimnames=c(list(1:2), list(c("orig","resamp","resamp_hist")), params$dnames[ -1 ]))
        params$Kgain =array(NA, dim=c(2,1,2,input$max_iter+1,input$npart), dimnames=c(list(1:2,1),params$dnames))
	    params$mk_prev = array(NA, dim=c(2,1,2,input$max_iter+1,input$npart), dimnames=c(list(1:2,1),params$dnames))
        params$Pk_prev = array(Pk_init, dim=c(2,2,2,input$max_iter+1,input$npart), dimnames=c(list(1:2,1:2),params$dnames))
		params$Ydist = array(NA, dim=c(2,3,input$max_iter+1,input$npart), dimnames=c(list(c("MuY","VarY"),c("orig","resamp","resamp_hist")), params$dnames[2:3]))
		params$mu_pars= array(NA, dim=c(2,2, input$max_iter+1, input$npart), dimnames=c(list(c("mu_mean","mu_var")),params$dnames))
		params$indices=array(NA,dim=c(input$max_iter+1, input$npart))
		params$wts=array(NA,dim=c(input$max_iter, input$npart))
		params$mu_pars["mu_mean","orig",,] <- input$mu_mean
	    params$mu_pars["mu_var","orig",,] <- input$mu_var
		params$resamp_colors = array(NA, dim=c(2,input$max_iter+1, input$npart), dimnames=params$dnames)
		params$is_new_step = rep(TRUE, input$max_iter+1)
		params$reset = FALSE
	
		
  	    #initial guesses
			 
		params$mu_guess = array(NA, dim=c(2,input$max_iter+1,input$npart), dimnames=params$dnames)
		params$mu_guess["orig","i1",] <- rnorm(n=input$npart, mean=params$mu_pars["mu_mean","orig","i1",], sd=sqrt(params$mu_pars["mu_var","orig","i1",]))

   	    if (input$sep_col) { 
			rainbow_cols <- colorspace::rainbow_hcl(n=input$npart +1, c=200,l=70, alpha=0.5)[1:input$npart]
			#rainbow_cols <- colorspace::rainbow_hcl(n=input$npart +1, c=200,l=seq(40, 100, length.out=input$npart+1))[1:input$npart]
		}
        else { rainbow_cols <- rep("lightgray",input$npart) }
 #      print(rainbow_cols)        		
			
	 	params$vel_guess = array(NA, dim=c(2,input$max_iter+1,input$npart), dimnames=params$dnames)
		params$vel_guess["orig","i1",] <- rnorm(n=input$npart, mean=params$mu_guess["orig","i1",], sd=sqrt(input$vel_var))
	    #for some reason it gives you a dumb error because it keeps input$iter as the last one for some reason  
     	   for (ii in 2:(input$max_iter+1)) { 
			    params$mu_guess["orig",ii,] <- params$mu_guess["orig","i1",] 
				params$vel_guess["orig",ii,] <- params$vel_guess["orig","i1",] 
				
			}
			 
		#initialize the first time.  after 
	 
	    params$mk[2,1,"orig","i1",] <- params$vel_guess ["orig","i1",]
		params$xpart[,"orig","i1",] <- params$xpart[,"resamp","i1",] <- params$mk[,,"orig","i1",]
		
		
		loc_axis <- seq(0, ceiling(yt[input$max_iter+1,"X"]), by=max(round(input$max_iter/5), 1)*input$vel_mu)    
        xrange <- nice_range(x=yt[,"X"], ep=0.1)
		#xdens_range <- nice_range(x=params$mu_guess["orig","i1",], ep=0.4)
		xdens_range <- input$mu_mean + c(-3.5,3.5)*sqrt(input$mu_var)
	
	
		ydens_range <- c(0, 1.33*dnorm(0, 0, sqrt(input$vel_var)))
		part_axis <- seq(1, input$npart, by=min(5,round(input$npart/2)))
		
		f <- function(xt, dt=input$delta_t, newV) {
         c(xt[1]+dt*xt[2], newV)
        }
		Fx <- function(xt, dt=input$delta_t, newV) {
         matrix(c(1, dt, 0, 0), ncol=2, byrow=TRUE)
        } 
		#just keep one since just constant
		Fxmat <- Fx(xt=matrix(c(1,1), ncol=1), newV=1)
		
		h <- function(xt, dt=input$delta_t) {
         xt[1]+dt*xt[2]
        }
		Hx <- function(xt, dt=input$delta_t) {
        matrix(c(1, dt), nrow=1)
		}
    	#just keep one since just constant
		Hxmat <- Hx(xt=c(1,1))
		
        dens_pts <- seq(xdens_range[1], xdens_range[2], length.out=75) 
	
			
		observeEvent( input$iter , { 
		            req(input$run)
					req(input$iter>1)
					
                   
 					
  				  			
					if (params$is_new_step[ input$iter ] & input$iter > 1) {
					   #a quick fix to set future values to TRUE
					   #print(length(params$is_new_step))
				       params$is_new_step[ -(1:input$iter)] <- TRUE
					   
					   params$wts_counter <- 0
					   params$locs_counter <- 0

					   #print(params$is_new_step)
					   #reorder everything so that particles are printed in color order (i.e. in order of the mu_guesses
	
					   ord_mu <- order(params$mu_guess["orig",input$iter,])
					
					   future_steps <- (input$iter):(input$max_iter+1)
					
					    params$mu_guess[,future_steps ,] <- params$mu_guess[,future_steps ,ord_mu, drop=FALSE]
					    params$vel_guess[,future_steps ,] <- params$vel_guess[,future_steps ,ord_mu, drop=FALSE]				   
					    params$Pk[,,,future_steps ,] <- params$Pk[,,,future_steps ,ord_mu, drop=FALSE] 
					   
					    params$mk[,,,future_steps ,] <- params$mk[,,,future_steps ,ord_mu, drop=FALSE]
					    params$xpart[,,future_steps,] <- params$xpart[,,future_steps,ord_mu]
					    params$Kgain[,,,future_steps,] <- params$Kgain[,,,future_steps,ord_mu,drop=FALSE]
					   
						params$mk_prev[,,,future_steps,] <- params$mk_prev[,,,future_steps,ord_mu,drop=FALSE]
					    params$Pk_prev[,,,future_steps,] <- params$Pk_prev[,,,future_steps,ord_mu,drop=FALSE]
					    params$Ydist[,,future_steps,ord_mu] <- params$Ydist[,,future_steps,ord_mu,drop=FALSE]
					   
					    params$mu_pars[,,future_steps,] <- params$mu_pars[,,future_steps,ord_mu,drop=FALSE]
					    params$indices[future_steps,] <- params$indices[future_steps ,ord_mu,drop=FALSE]
					    params$wts[future_steps[ -length(future_steps) ],] <- params$wts[future_steps[ -length(future_steps) ],ord_mu,drop=FALSE]
					    
					    params$resamp_colors[,future_steps,] <- params$resamp_colors[,future_steps,ord_mu,drop=FALSE]
					    params$resamp_colors["orig",input$iter,] <- rainbow_cols
				
					  # params$resamp_colors["orig",input$iter,ord_mu] <- rainbow_cols 
					
						for (nn in 1:input$npart) {   
						
							params$mk_prev[,1,"orig",input$iter,nn] <- f(xt=params$mk[,1,"orig",input$iter-1,nn], newV=params$vel_guess["orig",input$iter,nn]) 
							params$Pk_prev[,,"orig",input$iter,nn] <- as.matrix(Matrix::nearPD(Fxmat%*%params$Pk[,,"orig",input$iter-1,nn]%*%t(Fxmat) + xt_var, ensureSymmetry=TRUE)$mat)
							params$Ydist["MuY","orig",input$iter,nn] 	<- h(xt=params$mk_prev[,,"orig",input$iter,nn])
							params$Ydist["VarY","orig",input$iter,nn] 	<- as.numeric(Matrix::nearPD(Hxmat%*%params$Pk_prev[,,"orig",input$iter,nn]%*%t(Hxmat) + input$yt_var, ensureSymmetry=TRUE)$mat)
															
						}
					
                    }
				#	print(dim(params$mu_guess))
					
					#density of particle velocities	
					
					output$densplot <- renderPlot({ 
						if (input$iter > 1) {
							densplot(dpts=dens_pts, mu_guess=params$mu_guess["orig",input$iter,], norm_sds=sqrt(input$vel_var), known_mean=input$vel_mu,
                                colors=params$resamp_colors["orig",input$iter,], ylims=ydens_range, xlims=xdens_range, before_after="before",
                                sep_col=input$sep_col, npart=input$npart)
							}
					})
					
										
					if (params$is_new_step[ input$iter ] & input$iter > 1) {			
					
					#resample
					#print(yt[input$iter+1,"true_loc"])
					ynext <- yt[input$iter+1,"X"]
					
				    #params$wts[input$iter,] <- pmax(dnorm(x=yt[input$iter+1,"true_loc"], mean=params$Ydist["MuY","orig",input$iter,], sd=sqrt(params$Ydist["VarY","orig",input$iter,])), 1e-100)
					params$wts[input$iter,] <- dnorm(x=yt[input$iter+1,"X"], mean=params$Ydist["MuY","orig",input$iter,], sd=sqrt(params$Ydist["VarY","orig",input$iter,]))
					
					
					#not sure why this happens
					#params$wts[ input$iter, is.na(aparams$wts[input$iter,])] <- 1#1e-15
                    qwt <- quantile(x=params$wts[input$iter,], probs=0.85)
					params$indices[input$iter,] <- low_var_sample(wts=params$wts[input$iter,], M=input$npart)
				    if (! input$sep_col) { params$resamp_colors["orig",input$iter, params$wts[input$iter,] > qwt ] <- "red" }
					
					}
					
					#print(yt[ input$iter:(input$iter+1),"true_loc"])
					
					output$pred_loc <- renderPlot({
					if (input$iter >1) {
					
						# if (input$iter==1) {
							# prev_omegas <- matrix(params$xpart[1,"orig",input$iter,], ncol=1)
						# }
						# else {
							# next_omega <- c()
							# for (nn in 1:input$npart) {
								# next_omega[ nn ] <- f(xt=params$xpart[,"resamp_hist",input$iter-1,], newV=params$vel_guess["orig",input$iter,nn])
							# }
							# prev_omegas <- cbind(params$xpart[1,"resamp_hist",input$iter-1,], next_omega)
						# }
						
					
						pred_loc(Ydist1=params$Ydist[,"orig",input$iter,], wts=params$wts[input$iter,,drop=FALSE], xlims=xrange, xticks=loc_axis, before_after="before", 
                              colors=params$resamp_colors["orig",input$iter,], npart=input$npart, sep_col=input$sep_col, 
					          yt=yt[ input$iter:(input$iter+1),"X"], Yindex1=1:input$npart, indices=1:input$npart)
					
						}
					})
					
					
					
					
					
					# #next location
					
					#main problem: this is reacting too late because the locations are the same in both before/after plots
				   		
					if (params$is_new_step[ input$iter ] & input$iter >1) {
						ord <- params$indices[input$iter, ]
					
						params$mk[,,"resamp",input$iter-1,] <- params$mk[,,"orig",input$iter-1, ord]
						params$mk_prev[,,"resamp",input$iter,] <- params$mk_prev[,,"orig",input$iter, ord]
						params$Pk[,,"resamp",input$iter-1,] <- params$Pk[,,"orig",input$iter-1, ord]
						params$Pk_prev[,,"resamp",input$iter,] <- params$Pk_prev[,,"orig",input$iter, ord]
						params$mu_guess["resamp",input$iter,] <- params$mu_guess["orig",input$iter, ord]
						params$vel_guess["resamp",input$iter,] <- params$vel_guess["orig",input$iter, ord]
						params$Kgain[,,"resamp",input$iter-1,] <- params$Kgain[,,"orig",input$iter-1, ord]
					
					
						params$xpart[,"resamp",input$iter-1,]	<- params$xpart[,"resamp_hist",input$iter-1,] <- params$xpart[,"orig",input$iter-1,ord]
						if (input$iter > 2) {
						
							params$xpart[,"resamp",1:(input$iter-2),] <- params$xpart[,"resamp",1:(input$iter-2), ord]
						}
						params$Ydist[,"resamp",1:(input$iter-1),] <- params$Ydist[,"resamp",1:(input$iter-1),ord]
						params$Ydist[,"resamp",input$iter,] <- params$Ydist[,"resamp_hist",input$iter,] <- params$Ydist[,"orig",input$iter,ord]
				
						
						
						
						
						#resample the history as well
						params$mu_pars[,"resamp",input$iter,] <- params$mu_pars[,"orig",input$iter, ord]
					
						params$resamp_colors["resamp",input$iter,] <- params$resamp_colors["orig",input$iter, ord]
							
					
					
					
					output$wt_dist <- renderPlot({
						if (input$iter > 1) {
							
							ydens_range_wts <- c(0, max(params$wts[input$iter,])*1.2)

							
							if ((! params$is_new_step[ input$iter ]) & params$is_new_step[ input$iter+1 ]) {
						
							
								invalidateLater(millis=render_delay_step)
								isolate(params$wts_counter <- min(input$npart, params$wts_counter + 1))
							
								#loop_plot(npart=input$npart, ii=params$wts_counter, iter=input$iter)
								
								
								
								# while(params$wts_counter < input$npart) {
									# print(params$wts_counter)
									# params$wts_counter <- params$wts_counter + 1

								
									# if (params$wts_counter > 1) {
										# print("pause")
										# invalidateLater(millis=render_delay_step)
									# }
								# }
								
					
								
								wt_dist_loop(wts=params$wts[input$iter,,drop=FALSE], xlims=xdens_range, ylims=ydens_range_wts, known_mean=input$vel_mu, mu_guess=params$mu_guess["orig", input$iter,], 
											npart=input$npart, colors=params$resamp_colors["orig",input$iter,], sep_col=input$sep_col, index=params$indices[ input$iter, params$wts_counter])
									
								
							}
							else {
								 wt_dist(wts=params$wts[input$iter,,drop=FALSE], xlims=xdens_range, ylims=ydens_range_wts, known_mean=input$vel_mu, mu_guess=params$mu_guess["orig", input$iter,], 
									npart=input$npart, colors=params$resamp_colors["orig",input$iter,], sep_col=input$sep_col)
								
							}
						}	
					})
					
						
					output$pred_loc_resamp <- renderPlot({
						if (input$iter > 0) {
						# pred_loc(Ydist1=params$Ydist[,"resamp",input$iter,], wts=params$wts[input$iter, ,drop=FALSE], 
						          # xlims=xrange, xticks=loc_axis, before_after="after", colors=params$resamp_colors["resamp",input$iter,], 
								  # npart=input$npart, sep_col=input$sep_col, yt=yt[ input$iter:(input$iter+1),"true_loc"], Yindex1=1:input$npart,
								  # indices=params$indices[input$iter,])
						
						
							
							
							if ((! params$is_new_step[ input$iter ]) & params$is_new_step[ input$iter + 1 ]) {
								
								invalidateLater(millis=render_delay_step)
								isolate(params$locs_counter <- min(input$npart, params$locs_counter + 1))
							
								pred_loc_loop(Ydist1=params$Ydist[,"resamp",input$iter,], wts=params$wts[input$iter, ,drop=FALSE], 
														xlims=xrange, xticks=loc_axis, before_after="after", colors=params$resamp_colors["resamp",input$iter,], 
														npart=input$npart, sep_col=input$sep_col, yt=yt[ input$iter:(input$iter+1),"X"], Yindex1=1:input$npart,
														indices=params$indices[input$iter,], index=params$locs_counter)

									
							}
							else {
								pred_loc(Ydist1=params$Ydist[,"resamp",input$iter,], wts=params$wts[input$iter, ,drop=FALSE], 
						           xlims=xrange, xticks=loc_axis, before_after="after", colors=params$resamp_colors["resamp",input$iter,], 
								   npart=input$npart, sep_col=input$sep_col, yt=yt[ input$iter:(input$iter+1),"X"], Yindex1=1:input$npart,
								   indices=params$indices[input$iter,])	

							}	
					 
						}
					 })
					 
					}
					
	              			
					if (params$is_new_step[ input$iter ] & input$iter > 1) {
					#update distributions
						for (nn in 1:input$npart) {
			
							params$Kgain[,,"orig",input$iter, nn ] <- params$Pk_prev[,,"resamp",input$iter, nn ]%*%t(Hxmat)%*%(1/params$Ydist["VarY","resamp",input$iter, nn ])
							params$mk[,,"orig",input$iter, nn ] <- params$mk_prev[,,"resamp",input$iter, nn ] + params$Kgain[,,"orig",input$iter, nn ]*(ynext-h(xt=params$mk_prev[,,"resamp",input$iter, nn ]))
							
					
							
							
							params$Pk[,,"orig",input$iter, nn ] <- as.matrix(Matrix::nearPD(params$Pk_prev[,,"resamp",input$iter, nn ] - params$Kgain[,,"orig",input$iter, nn ]%*%(params$Ydist["VarY","resamp",input$iter, nn ]%*%t(params$Kgain[,,"orig",input$iter, nn ])), ensureSymmetry=TRUE)$mat)
							#now propagate next values
							
							params$xpart[,"orig",input$iter, nn ] <- mvtnorm::rmvnorm(n=1, mean=params$mk[,,"orig",input$iter,nn], sigma=params$Pk[,,"orig",input$iter,nn])
							
						}
					
					# #now update values of parameters
					
						params$mu_pars["mu_var","orig",input$iter+1,] <- 1/(1/params$mu_pars["mu_var","resamp",input$iter,] + 1/input$vel_var)
						params$mu_pars["mu_mean","orig",input$iter+1,] <- (params$mu_pars["mu_mean","resamp",input$iter,]/params$mu_pars["mu_var","resamp",input$iter,] + params$xpart[2,"orig",input$iter,]/input$vel_var)*params$mu_pars["mu_var","orig",input$iter+1,]
						
													 
								
						# #new parameter draws
						params$mu_guess["orig",input$iter+1,] <- rnorm(n=input$npart, mean=params$mu_pars["mu_mean","orig",input$iter+1,], sd=sqrt(params$mu_pars["mu_var","orig",input$iter+1,]))
						params$vel_guess["orig",input$iter+1,] <- rnorm(n=input$npart, mean=params$mu_guess["orig",input$iter+1,], sd=sqrt(input$vel_var))      
					  
				
					}
				
					output$rugplot <- renderPlot({
						if (input$iter > 1) {
					    convergence_rugplot(xlims=xdens_range, known_mean=input$vel_mu, max_iter=input$max_iter, iter=input$iter, 
											mu_guess=params$mu_guess, npart=input$npart, colors=params$resamp_colors) 
						}				
					})
						
					output$loc_hist <- renderPlot({
						if (input$iter > 1) {
					    #location_history(Ydist=params$Ydist[,"resamp_hist",,], xticks=loc_axis, xlims=xrange, iter=input$iter, 
					    #                 npart=input$npart,colors=params$resamp_colors["resamp",,], yt=yt)
						
						
							omega_tmp <- params$xpart[1,"resamp",1:(input$iter-1),]
							omega_tmp <- rbind(omega_tmp, params$xpart[1,"orig",input$iter,])
						
					
						
						location_history_v2(omega=omega_tmp, xticks=loc_axis, xlims=xrange, iter=input$iter, 
											npart=input$npart, yt=yt[,"X"], colors=params$resamp_colors["resamp",,])
						}
					})			    
					
					
  				params$is_new_step[ input$iter ] <- FALSE
					
		 })#end of iteration 
	
	 })#end of run
		  
 }
)
)
}



