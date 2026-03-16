#convert text to vector
# http://stackoverflow.com/questions/19252663/extracting-decimal-numbers-from-a-string

numextractall <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)), use.names=FALSE)
}

#extend range on each side
nice_range <- function(x, ep=0.1) {
  r <- range(x)
  extend <- abs(max(r)-min(r))
  r <- r + abs(ep)*c(-1*extend, extend)
  r
 }

# make sure only pick one
random_which <- function(condition) {

	ii <- sample(x=which(condition), size=1)
	ii
}


#density plot of predictions

densplot <- function(dpts, mu_guess, norm_sds, known_mean, colors, ylims,
                     xlims, before_after="before", iter,
                     var_name="velocity", sep_col=TRUE, npart)  {

	
	plot(x=dpts, y=dnorm(x=dpts, mean=known_mean, sd=norm_sds),  yaxs="i",
	     col=1, lwd=4, las=1, type="l", ylim=ylims, xlim=xlims, ylab="Density",
             main=paste("Particle distributions of", var_name), cex.main=1.1,
             xlab=paste("Particle guess of mean", var_name), mar=c(4,4,2,2)+0.1)
					abline(h=0, lwd=0.5)
					abline(v=known_mean, lty="dotted", col=1, lwd=3)


	for (nn in 1:npart) {
            lines(x=dpts, y=dnorm(x=dpts, mean=mu_guess[ nn ], sd=norm_sds),
		 col=colors[ nn ], lwd=2, type="l")
	}


	if (sep_col) {
	    legend("topleft", col=c("black","green"), lwd=c(3,2),
                   legend=c(paste("true",var_name),"particles"), ncol=1, cex=0.8)
        }
	else {
            legend("topleft", col=c("black","lightgray","red"), lwd=c(3,2,2),
                   legend=c(paste("true",var_name),"particles","particles (closest 15%)"),
                   ncol=1, cex=0.8)
	}

	rug_multicolor(x=mu_guess, ticksize=-0.05, col_vec=colors)


}



densplot_twostate <- function(dpts, mu_guess, norm_sds, known_mean,
                              colors, ylims, xlims, before_after="before", sep_col=TRUE,
                              npart, var_name="velocity") {


	old_pars <- par(mar=par()$mar, mfrow=par()$mfrow, oma=par()$oma)
	on.exit(expr=par(old_pars))
	
	par(mfrow=c(2,1), mar=c(3,4,2,2)+0.1 ,oma = c(0, 0, 2, 0))

	t1 <- c("true","particle")
	lwd1 <- c(3,2)
	if (sep_col) {
		c1 <- c("blacK","green")
				
	}
	else {	
		c1 <- c("black","lightgray","red")
		t1 <- c(t1, "best 15%")
		lwd1 <- c(lwd1, 2)
	}
	
	for (kk in 1:2) {
		plot(x=dpts, y=dnorm(x=dpts, mean=known_mean[ kk ], sd=norm_sds), 
			 col=1, lwd=3, las=1, type="l", ylim=ylims, xlim=xlims, yaxs="i", cex.main=1.1,
			 ylab="Density",  main=paste(var_name,", type", kk), lty=ifelse(kk==1,1,3), xlab="")
			 
		mtext(side=1, text=var_name, line=2)
			 
		text(x=par()$usr[ 1 ]- 0.12*diff(par()$usr[ 1:2 ]), y=par()$usr[ 4 ], labels=kk, cex=2, xpd=TRUE)	 
		
		for (nn in 1:npart) {
			lines(x=dpts, y=dnorm(x=dpts, mean=mu_guess[ kk, nn ], sd=norm_sds),
					col=colors[ nn ], lwd=2, type="l", lty=ifelse(kk==1,1,3))
			  
		}	
		abline(v=known_mean[ kk ])
		rug_multicolor(x=mu_guess[kk,], ticksize=-0.05, col_vec=colors, plot_side=3)
	
		legend(ifelse(kk==2,"topleft","topright"), legend=t1, col=c1, lwd=lwd1, ncol=1, cex=0.6, lty=ifelse(kk==1,1,3))
	
	}	
		
	mtext(text=paste("Particle distributions of", var_name), outer=TRUE, side=3, cex=1.25, font=2)
	
	
	


}
				
				
				



#predicted locations before/after resampling
pred_loc <- function(Ydist1, wts, xlims, xticks, before_after="before", colors, npart, sep_col, yt, indices, Ydist2=NULL, Yindex1, Yindex2=c()) {

					#Ydist1=params$Ydist[,"orig",input$iter,], wts=params$wts[input$iter,,], xlims=xrange, xticks=loc_axis, before_after="before",
                    #colors=params$resamp_colors["orig",input$iter,], npart=input$npart, sep_col=input$sep_col,
					#yt=yt[ input$iter:(input$iter+1),"true_loc"], indices=1:input$npart, Ydist2=NULL, Yindex1=1:input$npart, Yindex2=1:input$npart

					#par(mgp=c(2, 1, 0))
                    plot(y=-100, x=-10, ylim=c(-.15*npart, npart*1.1), xlim=xlims , las=1,
						 main=paste("Prediction of next location",before_after,"resampling"),
							 ylab="Particle", xlab="Location", xaxt="n", yaxt="n")
							 #, mgp=c(2,1,0)
						axis(side=1, at=xticks, labels=xticks)
						cs <- colSums(wts)

			            qwt <- quantile(wts, probs=0.85, na.rm=TRUE)


						wts1_hi <- wts[1,] >= qwt

						border1 <- rep(NA,length(Yindex1))
						border1[ wts1_hi ] <- "black"
						rect(xleft=Ydist1["MuY",Yindex1]-1.96*sqrt(Ydist1["VarY",Yindex1]),
						    xright=Ydist1["MuY",Yindex1]+1.96*sqrt(Ydist1["VarY",Yindex1]),
							ytop=seq(1.45, npart+.45, by=1)[ Yindex1 ], ybottom=seq(.55, npart-.45, by=1)[ Yindex1 ], col=colors[ Yindex1 ],
							border=border1, lwd=2.5)

						points(x=Ydist1["MuY",Yindex1], y= Yindex1 , pch=19, cex=min(15/npart,1))

						#for (ii in 1:ncol(prev_locs)) {
						#	points(x=prev_locs[,ii], y=1:npart)
						#}

						if (! is.null(Ydist2) & length(Yindex2) > 0) {

						  wts2_hi <- wts[2,] >= qwt

						  #wts2_hi <- wts[2,indices] > qwt
						  #wts2_hi <- wts[2,Yindex2] > qwt
						   #border2 <- colors[ 1:length(Yindex2) ]
						   	border2 <- rep(NA,length(Yindex2))

						   border2[ wts2_hi ] <- "black"
						   
						    rect(xleft=Ydist2["MuY",Yindex2]-1.96*sqrt(Ydist2["VarY",Yindex2]),
								xright=Ydist2["MuY",Yindex2]+1.96*sqrt(Ydist2["VarY",Yindex2]),
								ytop=seq(1.45, npart+.45, by=1)[ Yindex2 ], ybottom=seq(.55, npart-.45, by=1)[ Yindex2 ], col=colors[ Yindex2 ],
								border=NA, lwd=2.5)
							
							#apply crosshatch	
							rect(xleft=Ydist2["MuY",Yindex2]-1.96*sqrt(Ydist2["VarY",Yindex2]),
								xright=Ydist2["MuY",Yindex2]+1.96*sqrt(Ydist2["VarY",Yindex2]),
								ytop=seq(1.45, npart+.45, by=1)[ Yindex2 ], ybottom=seq(.55, npart-.45, by=1)[ Yindex2 ], col="black",
								border=border2[ Yindex2 ], lwd=2.5, density=10, angle=45)	
							
						  
						   
						   # segments(y0=seq(0.55, npart-0.45, by=1)[ Yindex2 ], x0=Ydist2["MuY",Yindex2]-1.96*sqrt(Ydist2["VarY",Yindex2]),
						            # y1=seq(1.45, npart+.45, by=1)[ Yindex2 ], x1=Ydist2["MuY",Yindex2]+1.96*sqrt(Ydist2["VarY",Yindex2]),
									# col=colors[ Yindex2 ], lwd=4)

						   # segments(y0=seq(0.55, npart-0.45, by=1)[ Yindex2 ], x0=Ydist2["MuY",Yindex2]+1.96*sqrt(Ydist2["VarY",Yindex2]),
						            # y1=seq(1.45, npart+.45, by=1)[ Yindex2 ], x1=Ydist2["MuY",Yindex2]-1.96*sqrt(Ydist2["VarY",Yindex2]),
									# col=colors[ Yindex2 ], lwd=4)

						   # segments(y0=1:npart, x0=Ydist2["MuY",]+1.96*sqrt(Ydist2["VarY",]),
						            # y1=1:npart, x1=Ydist2["MuY",]-1.96*sqrt(Ydist2["VarY",]),
									# col=colors, lwd=2.5)
							points(x=Ydist2["MuY",Yindex2], y=Yindex2, pch=19, cex=min(15/npart,1))
							#rect(xleft=Ydist2["MuY",Yindex2][ wts2_hi ]-1.96*sqrt(Ydist2["VarY",Yindex2][ wts2_hi ]),
						    #xright=Ydist2["MuY",Yindex2][ wts2_hi ]+1.96*sqrt(Ydist2["VarY",Yindex2][ wts2_hi ]),
							#ytop=seq(1.45, npart+.45, by=1)[ Yindex2 ][wts2_hi], ybottom=seq(.55, npart-.45, by=1)[ Yindex2 ][wts2_hi],
							#border="black", lwd=2.5)
                        }



						abline(v=yt, lty="dotted")

						arrows(y0=1.09*npart, y1=1.09*npart, x0=yt[1], x1= yt[2], length=.1, lty=1.5)
						if (is.null(Ydist2)) {
						
							if (! sep_col) {
								c1 <- c("lightgrey","black","black","black")
								bg1 <- c(NA, "red", NA, NA)
								pch1 <- c(15,22,19,NA)
								lwd1 <- c(NA,NA,NA,1)
							}
							else {
								c1 <- c("green","black","black","black")
								bg1 <- rep(NA, 4)
								pch1 <- c(15,0,19,NA)
								lwd1 <- c(NA,1.5,NA,1)
							}							
						
						
							legend("bottomleft", lty=c(NA,NA,NA,"dashed"),legend=c("Width=95% interval","Best 15%","Prediction","Observation"), pch=pch1,
						       col=c1, lwd=lwd1, pt.lwd=c(2,3, NA,NA), pt.cex=c(1,1,1,0), ncol=2, cex=0.6, pt.bg=bg1)
							   }
						else {
						
							if (sep_col==FALSE) {
								c1 <- c("lightgrey","lightgrey","black","black","black")
								bg1 <- c(NA,NA, "red", NA, NA)
								pch1 <- c(15,7,22,19,NA)
								lwd1 <- c(NA,NA,NA,NA,1)
							}
							else {
								c1 <- c("green","green","black","black","black")
								bg1 <- rep(NA, 5)
								pch1 <- c(15,7,0,19,NA)
								lwd1 <- c(NA,NA,1.5,NA,1)
							}	
						
						
						
						legend("bottomleft", lty=c(NA,NA,NA,NA,"dashed"),legend=c("Type 1 95% interval","Type 2 95% interval","Best 15%", "Prediction","Observation"), pch=pch1,
						       col=c1, lwd=lwd1, pt.lwd=c(3,1.5,NA,NA), pt.cex=c(1,1,1,1,0), ncol=2, cex=0.6, pt.bg=bg1)
		                      }
    	    	    }


#predicted locations before/after resampling
pred_loc_loop <- function(Ydist1, wts, xlims, xticks, before_after="before", colors, npart, sep_col=TRUE, yt, indices, Ydist2=NULL, Yindex1, Yindex2=NULL, index=1) {

					 #Ydist1=params$Ydist[,"orig",input$iter,], wts=params$wts[input$iter,,], xlims=xrange, xticks=loc_axis, before_after="before",
                     #colors=params$resamp_colors["orig",input$iter,], npart=npart, sep_col=input$sep_col,
					 #yt=yt[ input$iter:(input$iter+1),"true_loc"], indices=1:input$npart, Ydist2=NULL, Yindex1=1:input$npart, Yindex2=NULL, index=1

					#print(wts)
					Yindex1 <- (1:npart) %in% Yindex1
					Yindex2 <- (1:npart) %in% Yindex2


                    plot(y=-100, x=-10, ylim=c(-.15*npart, npart*1.1), xlim=xlims , las=1,
						 main=paste("Prediction of next location",before_after,"resampling"),
							 ylab="Particle", xlab="Location", xaxt="n", yaxt="n")
						#, mgp=c(2,1,0)
						axis(side=1, at=xticks, labels=xticks)
						cs <- colSums(wts)

			            qwt <- quantile(wts, probs=0.85, na.rm=TRUE)

						wts1_hi <- wts[1,indices]	>=qwt #wts2_hi <- (cs[ indices ] >= qwt)

						border1 <- rep(NA,npart)
						border1[ wts1_hi ] <- "black"

						curr_index <- (1:npart) <= index


						if (any(Yindex1[ curr_index ])) {


							rect(xleft=Ydist1["MuY",Yindex1 & curr_index ]-1.96*sqrt(Ydist1["VarY",Yindex1 & curr_index ]),
								xright=Ydist1["MuY",Yindex1 & curr_index ]+1.96*sqrt(Ydist1["VarY",Yindex1 & curr_index ]),
								ytop=seq(1.45, index+0.45, by=1)[ Yindex1[ curr_index]==TRUE ], ybottom=seq(0.55, index- 0.45, by=1)[ Yindex1[ curr_index ]==TRUE ], col=colors[ Yindex1[ curr_index ] ],
								border=border1[ curr_index ], lwd=2.5)

							points(x=Ydist1["MuY",Yindex1 & curr_index ], y= which(Yindex1 & curr_index) , pch=19, cex=min(15/npart,1))
						}

						if (any(Yindex2[ curr_index ])) {

						   wts2_hi <- wts[2,indices] >=qwt
						   
							border2 <- colors[ 1:length(Yindex2) ]
						   border2[ wts2_hi ] <- "black"
						   
						   rect(xleft=Ydist2["MuY",Yindex2 & curr_index ]-1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index ]),
								xright=Ydist2["MuY",Yindex2 & curr_index ]+1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index ]),
								ytop=seq(1.45, index+.45, by=1)[ Yindex2[ curr_index]==TRUE ], ybottom=seq(.55, index-.45, by=1)[ Yindex2[ curr_index ]==TRUE ], col=colors[ Yindex2[ curr_index ]==TRUE ],
								border=NA, lwd=2.5)
						   
						   ##apply crosshatch
						   rect(xleft=Ydist2["MuY",Yindex2 & curr_index ]-1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index ]),
								xright=Ydist2["MuY",Yindex2 & curr_index ]+1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index ]),
								ytop=seq(1.45, index+.45, by=1)[ Yindex2[ curr_index]==TRUE ], ybottom=seq(.55, index-.45, by=1)[ Yindex2[ curr_index ]==TRUE ], col="black",
								border=border2[ curr_index ], lwd=2.5, density=10, angle=45)
						    
								   
						   
						   
						   # segments(y0=seq(0.55, index-0.45, by=1)[ Yindex2[ curr_index ]==TRUE ], x0=Ydist2["MuY", Yindex2 & curr_index ]-1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index ]),
						            # y1=seq(1.45, index+0.45, by=1)[ Yindex2[ curr_index ]==TRUE ], x1=Ydist2["MuY", Yindex2 & curr_index ]+1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index ]),
									# col=colors[ Yindex2 &  curr_index ], lwd=4)

						   # segments(y0=seq(0.55, index-0.45, by=1)[ Yindex2[ curr_index ]==TRUE ], x0=Ydist2["MuY", Yindex2 & curr_index ]+1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index ]),
						            # y1=seq(1.45, index+0.45, by=1)[ Yindex2[ curr_index ]==TRUE ], x1=Ydist2["MuY", Yindex2 & curr_index ]-1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index ]),
									# col=colors[ Yindex2 & curr_index ], lwd=4)

						   # segments(y0=1:npart, x0=Ydist2["MuY",]+1.96*sqrt(Ydist2["VarY",]),
						            # y1=1:npart, x1=Ydist2["MuY",]-1.96*sqrt(Ydist2["VarY",]),
									# col=colors, lwd=2.5)
							points(x=Ydist2["MuY",Yindex2 & curr_index ], y=which(Yindex2 & curr_index ), pch=19, cex=min(15/npart,1))

							# rect(xleft=Ydist2["MuY",Yindex2 & curr_index & wts2_hi]-1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index & wts2_hi]),
								# xright=Ydist2["MuY",Yindex2 & curr_index & wts2_hi]+1.96*sqrt(Ydist2["VarY",Yindex2 & curr_index & wts2_hi]),
								# ytop=seq(1.45, npart+0.45, by=1)[ Yindex2 & wts2_hi & curr_index ], ybottom=seq(.55, npart-0.45, by=1)[ Yindex2 & wts2_hi & curr_index ],
								# border="black", lwd=2.5)
                        }



						abline(v=yt, lty="dotted")
						
						if (is.null(Ydist2)) {
						
							if (sep_col==FALSE) {
								c1 <- c("lightgrey","black","black","black")
								bg1 <- c(NA, "red", NA, NA)
								pch1 <- c(15,22,19,NA)
								lwd1 <- c(NA,NA,NA,1)
							}
							else {
								c1 <- c("green","black","black","black")
								bg1 <- rep(NA, 4)
								pch1 <- c(15,0,19,NA)
								lwd1 <- c(NA,1.5,NA,1)
							}							
						
						
							legend("bottomleft", lty=c(NA,NA,NA,"dashed"),legend=c("Width=95% interval","Best 15%","Prediction","Observation"), pch=pch1,
						       col=c1, lwd=lwd1, pt.lwd=c(2,3, NA,NA), pt.cex=c(1,1,1,0), ncol=2, cex=0.6, pt.bg=bg1)
							   }
						else {
						
							if (sep_col==FALSE) {
								c1 <- c("lightgrey","lightgrey","black","black","black")
								bg1 <- c(NA,NA, "red", NA, NA)
								pch1 <- c(15,7,22,19,NA)
								lwd1 <- c(NA,NA,NA,NA,1)
							}
							else {
								c1 <- c("green","green","black","black","black")
								bg1 <- rep(NA, 5)
								pch1 <- c(15,7,0,19,NA)
								lwd1 <- c(NA,NA,1.5,NA,1)
							}	
						
						
						
						legend("bottomleft", lty=c(NA,NA,NA,NA,"dashed"),legend=c("Type 1 95% interval","Type 2 95% interval","Best 15%", "Prediction","Observation"), pch=pch1,
						       col=c1, lwd=lwd1, pt.lwd=c(3,1.5,NA,NA), pt.cex=c(1,1,1,1,0), ncol=2, cex=0.6, pt.bg=bg1)
		                      }






				}



#predicted locations before/after resampling
pred_loc_2D <- function(yt, bds, start_pt, MuY,	SigY, conf=0.5, wts, before_after="before", npart, colors, states=NULL) {


						#yt=yt[1:(input$iter+1),c("X","Y")], bds=box_bounds, start_pt=params$mk_prev[c("X","Y"),"orig",input$iter,1,], MuY=params$MuY[,"orig",input$iter,,],
						#SigY=params$SigY[,,"orig",input$iter,,], conf=0.5, wts=params$wts[input$iter,,], before_after="before", npart=input$npart,
						#colors=params$resamp_colors["orig",input$iter,], states=NULL)

					n <- nrow(yt)
                    plot(yt, xlim=bds[,"X"], ylim=bds[,"Y"], type="b",lwd=1, pch=19, cex=c(rep(1, n-1), 3),
						 main=paste("Particle guesses of next location",before_after,"resampling"), ylab="Y", xlab="X", las=1, cex.main=1.1)
					#, ifelse(sep_col, "(closest 15% are boxed)","(best 15% are red)")),

					cs <- colSums(wts)

			        qwt <- quantile(wts, probs=0.85, na.rm=TRUE)


					wts1_hi <- wts[1,] >= qwt
					wts2_hi <- wts[2,] >= qwt
	

					if (is.null(states)) {
						for (kk in 1:2) {
							
							points(t(MuY[,kk,]), col=colors, pch=c(0,6)[ kk ], cex=0.5)
							segments(x0=start_pt[1,], y0=start_pt[2,], x1=MuY[1,kk,], y1=MuY[2,kk,], col=colors, lty=kk)

							for (nn in 1:npart) {
								
								lines(ellipse::ellipse(x=SigY[,,kk,nn], centre=MuY[,kk,nn], level=conf), lwd=0.1, col=colors[ nn ], type="l", lty=kk)

							}
						}
					}
					else {

						#use this if doing replay back
						
			
						points(MuY, col=colors, pch=c(0,6)[ states ], cex=0.5)
						
						for (nn in 1:npart) {
		
							segments(x0=start_pt[1,nn], y0=start_pt[2,nn], x1=MuY[1,nn], y1=MuY[2,nn], col=colors, lty=states[ nn ])
												
							lines(ellipse::ellipse(x=SigY[,,nn], centre=MuY[,nn], level=conf), lwd=0.1, col=colors[ nn ], type="l", lty=states[ nn ])

						}

					}
					
					legend("topright", pch=c(19, NA, NA), lty=c(1,1,2), pt.cex=c(1.5, NA, NA), lwd=c(1, 0.5, 0.5), col=c("black", "green","green"),
							legend=c("observed path", paste(round(conf*100),"% posterior ellipse, type ", 1:2, sep="")), ncol=1, cex=0.8)


    	    	}


#predicted locations before/after resampling
pred_loc_2D_loop <- function(yt, bds, start_pt, MuY, SigY, conf=0.5, wts, before_after="before", colors, index=1, states, resamp_indices, npart)  {

						#yt=yt[1:(input$iter+1),c("X","Y")], bds=box_bounds, start_pt=params$mk_prev[c("X","Y"),"orig",input$iter,1,], MuY=params$MuY_actual[,"resamp",input$iter,],
						#SigY=params$SigY_actual[,,"resamp",input$iter,], conf=0.5, wts=params$wts[input$iter,,], before_after="before", npart=input$npart,
						#colors=params$resamp_colors["resamp",input$iter,], index=1, states=params$curr_state["resamp",input$iter,], resamp_indices)


					indices <- 1:index
					resamp_indices <- resamp_indices[ indices ]

					resamp_indices_tab <- as.numeric(table(factor(resamp_indices, levels=1:npart)))

					n <- nrow(yt)
                    plot(yt, xlim=bds[,"X"], ylim=bds[,"Y"], type="b",lwd=1, pch=19, cex=c(rep(1, n-1), 3),
						 main=paste("Particle guesses of next location",before_after,"resampling"), ylab="Y", xlab="X", las=1, cex.main=1.1)
					#, ifelse(sep_col, "(closest 15% are boxed)","(best 15% are red)")),

					cs <- colSums(wts)

			        qwt <- quantile(wts, probs=0.85, na.rm=TRUE)


					wts1_hi <- wts[1,] >= qwt
					wts2_hi <- wts[2,] >= qwt

					cex_pts <- c(rep(0.5, index-1), 1.5)
					pch_pts <- rep(15, index)
					pch_pts[ states[ indices ]==2 ] <- 17
					#extra weight for every time it is resampled

					#print(resamp_indices[ index ])

					#add extra weight for every additional time index is sampled


					#lwd_ellipse <- c(rep(0.1, index-1), 1+resamp_indices_tab[ resamp_indices[ index ]])
					lwd_ellipse <- rep(0.1, npart)
					lwd_ellipse[ resamp_indices[ index ]] <- 1+resamp_indices_tab[ resamp_indices[ index ]]

					#lty_ellipse <- rep(1,index)
					#lty_ellipse[ states[ indices ]==2 ] <- 3
					lty_ellipse <- states


					points(t(MuY[,indices]), col=colors[ indices ], pch=pch_pts, cex=cex_pts)
					segments(x0=start_pt[1,], y0=start_pt[2,], x1=MuY[1,indices], y1=MuY[2,indices], col=colors[ indices ], lty=lty_ellipse)
					for (ii in unique(indices)) {
						lines(ellipse::ellipse(x=SigY[,,ii], centre=MuY[,ii], level=conf), lwd=lwd_ellipse[ ii ], col=colors[ ii ], type="l", lty=lty_ellipse[ ii ])

					}

					legend("topright", pch=c(19, NA, NA), lty=c(1,1,2), pt.cex=c(1.5, NA, NA), lwd=c(1, 0.5, 0.5), col=c("black", "green","green"),
							legend=c("observed path", paste(round(conf*100),"% posterior ellipse, type ", 1:2, sep="")), ncol=1, cex=0.8)
    	    	}





#predicted locations before/after resampling
pred_loc_dens_2D <- function(yt, bds, locs)  {

					#yt=yt[1:(input$iter+1),c("X","Y")], bds=box_bounds, locs=params$xpart[c("X","Y"),"resamp",1:input$iter,]

					if (length(dim(locs))==2) { locs <- as.data.frame(t(locs)) }
					else { locs <- as.data.frame(plyr::adply(locs, .margins=c(2,3))[,c(3,4)]) }
					colnames(locs) <- c("X","Y")


					#https://dannagifford.wordpress.com/2017/10/26/how-to-make-ggplot-look-like-base-r-graphics/
					#http://ggplot2.tidyverse.org/reference/theme.html
					#do this to pass check
					#X <- Y <- NULL

                    g <- ggplot(data=locs, aes(x=.data$X, y=.data$Y)) + theme_bw()

					g <- g + scale_x_continuous(limits=bds[,"X"]) + scale_y_continuous(limits=bds[,"Y"]) + theme(axis.ticks.length=unit(.25, "cm"))
					g <- g + stat_density2d(aes(fill=after_stat(!!str2lang("density"))), geom="tile", contour=FALSE)+ scale_fill_gradient(low="white", high="black") + guides(fill=FALSE)
					g <- g + geom_path(data=make_segments(as.data.frame(yt)),  aes(x=.data$X, y=.data$Y), colour="red", lwd=1.5) + theme(legend.position="bottom", legend.key.width=unit(1.5,"cm"))
					g <- g + ggtitle("Density of particle and true unobserved (red) locations")
					g <- g + theme(plot.title=element_text(size=14.25, face="bold", hjust=0.5, margin=margin(b=20, t=20)), axis.text=element_text(size=12))
					g <- g + theme(plot.margin=unit(c(0.2, 1.2, 0.52, 0.42), "cm"))

					#area between axis title and labels, then between ticks and labels
					g <- g + theme(axis.title.x =element_text(margin = margin(r = 5, t=10)),
								   axis.title.y=element_text(margin = margin(r = 0, t=20)),
								   axis.text.x=element_text(margin=margin(l=10, r=10, t=10, b=10)),
								   axis.text.y=element_text(margin=margin(l=10, r=10, t=10, b=10)))


					theme_set(theme_bw())
					theme_update(
						panel.grid.major = element_blank(),
						panel.grid.minor = element_blank(),
						strip.background = element_blank()
						)
					g
    	    	}



wt_dist <- function(wts, xlims, ylims, known_mean, mu_guess, npart, colors, sep_col=TRUE) {

					#wts=params$wts[input$iter,], xlims=xdens_range, ylims=ydens_range, known_mean=input$vel_mu, mu_guess=params$mu_guess["orig", input$iter,],
                    #npart=input$npart, colors=params$resamp_colors["orig",input$iter,], sep_col=input$sep_col,

					#original plot
						plot(x=-5,y=-10, xlim=xlims, ylim=ylims, xlab="Particle guess of mean velocity", ylab="Density",las=1,
								 main="Resampling weights from predicting next location", cex.main=1.1)
						abline(v=known_mean, lty=2)
						segments(x0=mu_guess, x1=mu_guess, y0=rep(0, npart), y1=wts, col=colors, lwd=3)
						points(x=mu_guess, y=wts, col=colors, pch=19, cex=1.5)
						abline(h=0, lwd=0.5)
					
					if (sep_col==FALSE) { 
								legend("topright", lwd=2, lty=1, pch=19, col=c("red", "lightgray"), legend=c("best 15%","others"))
							}
					
					rug_multicolor(x=mu_guess, ticksize=-0.04, col_vec=colors)
					#original plot

					# }



					}



# wt_dist_twostate <- function(wts, xlims, mu_guess, ylims, known_mean, npart, colors, sep_col=TRUE, iter, behavior) {

					# #wts=params$wts[input$iter,], xlims=xdens_range, ylims=ydens_range, known_mean=input$vel_mu, mu_guess=params$mu_guess["orig", input$iter,],
                    # #npart=input$npart, colors=params$resamp_colors["orig",input$iter,], sep_col=input$sep_col,
					# #main_title="Behavior-conditional distribution of resampling weights\nbased on predictions of next location"

					# if (ylims[ 2 ]>0.01) {

						# ylims <- c(0, round(ylims[ 2 ], digits=2))
						# y_axis <- seq(0, ylims[ 2], length.out=4)
						# y_axis_labs <- sprintf("%.2f",y_axis)
					# }
					# else {
						# y_axis <- seq(0, ylims[ 2], length.out=4)
						# y_axis_labs <- formatC(y_axis, format="e", digits=0)
						# y_axis_labs[ 1 ] <- "0.0"

					# }



					# plot(x=-5,y=-10, xlim=xlims, ylim=c(0, 2*ylims[ 2 ]), xlab="Particle guess of mean velocity", ylab="Density",las=1, yaxt="n",
						 # main=paste("Overall (top) and behavior-conditional (bottom)\nresampling weights, t=",iter,
								# ifelse(sep_col,"","\n(best 15% are red)"), sep=""), cex.main=1)

					# mtext(side=1, text=var_name, line=3)
					# abline(h=c(0, ylims[ 2 ]))

					# axis(side=2, at=y_axis, labels=y_axis_labs, las=1, cex.axis=0.65)
					# axis(side=4, at=y_axis + ylims[ 2 ], labels=y_axis_labs, las=1, cex.axis=0.65)

					# segments(y0=rep(0,2), y1=rep(ylims[ 2 ], 2), x0=known_mean, x1=known_mean, lty=2)

					# #plot unconditional weights
					# segments(x0=mu_guess[behavior,], x1=mu_guess[behavior,], y0=rep(ylims[ 2 ], npart), y1=ylims[ 2 ]+ colSums(wts), col=colors, lwd=3, lty=1)
					# points(x=mu_guess[behavior,], y=ylims[ 2 ]+ colSums(wts), col=colors, pch=19, cex=1.5)


					# #behavior-conditional

					# for (kk in 1:2) {

						# segments(x0=mu_guess[kk,], x1=mu_guess[kk,], y0=rep(0, npart), y1=wts[kk,], col=colors, lwd=3, lty=kk)
						# points(x=mu_guess[kk,], y=wts[kk,], col=colors, pch=c(15,17)[ kk ], cex=1.5)
					# }

					# legend(x=par()$usr[ 1 ], y=ylims[ 2 ], pch=c(15,17), col=rep("blue",2), legend=c("type 1", "type 2"), pt.cex=1.5)

					# rug_multicolor(x=mu_guess, ticksize=-0.05, col_vec=rep(colors,2), plot_side=1)




				# }


wt_dist_twostate <- function(wts, mu_guess, xlims, ylims, known_mean, npart, colors, sep_col=TRUE, iter, behavior, var_name) {

					old_pars <- par(mar=par()$mar, mfrow=par()$mfrow, oma=par()$oma)
					on.exit(expr=par(old_pars))
				
					par(mfrow=c(2,1), mar=c(3,4,2,2)+0.1 ,oma = c(0, 0, 2, 0))


					plot(x=-5,y=-10, xlim=xlims, ylim=ylims, yaxs="i", xlab="", 
						ylab="Density", main=expression("Overall weight"==~w[t]^{"(n)"}),las=1, cex.main=1.1, cex.axis=0.7)
					mtext(side=1, text=var_name, line=2)
					#abline(v=known_mean[ behavior ])

					if (sep_col==FALSE) {
						legend("topleft", col=c("lightgray", "red"), pch=19, legend=c("particle","best 15%"), lwd=3, ncol=2, cex=0.7)
					}

					#plot unconditional weights
					segments(x0=mu_guess[behavior,], x1=mu_guess[behavior,], y0=rep(0, npart), y1= colSums(wts), col=colors, lwd=3, lty=1)
					points(x=mu_guess[behavior,], y= colSums(wts), col=colors, pch=19, cex=1.5)

					#behavior-conditional

					plot(x=-5,y=-10, xlim=xlims, ylim=ylims, yaxs="i", xlab="", main=expression("Behavior-conditional weight"==~w["t|k"]^{"(n)"}),
						ylab="Density",las=1, cex.main=1.1, cex.axis=0.7)
					mtext(side=1, text=var_name, line=2)
					
					abline(v=known_mean)
					
					if (sep_col==FALSE) {
						legend("topleft", col=c("lightgray", "red", "lightgray","lightgray"), legend=c("particle","best 15%"), lwd=2, ncol=2, cex=0.7,
								pch=c(19, 19, 15, 17), pt.cex=1)
							
					}
					else {
						legend("topleft", col="blue", legend=c("type 1","type 2"), ncol=1, cex=0.7, 
								pch=c(15, 17), pt.cex=1)
							
					}
					
					
					for (kk in 1:2) {

						segments(x0=mu_guess[kk,], x1=mu_guess[kk,], y0=rep(0, npart), y1=wts[kk,], col=colors, lwd=3, lty=c(1,3)[ kk ])
						points(x=mu_guess[kk,], y=wts[kk,], col=colors, pch=c(15,17)[ kk ], cex=1.5)

					}

					#legend(x=par()$usr[ 1 ], y=ylims[ 2 ], pch=c(15,17), col=rep("blue",2), legend=c("type 1", "type 2"), pt.cex=1.5, cex=0.8)

					rug_multicolor(x=mu_guess, ticksize=-0.05, col_vec=rep(colors,2), plot_side=1)

					
					mtext(text="Particle resampling weights", outer=TRUE, side=3, cex=1.25, font=2)

				}

wt_dist_loop <- function(wts, xlims, ylims, known_mean, mu_guess, npart, colors, sep_col=TRUE, index=1) {


					#wts=params$wts[input$iter,], xlims=xdens_range, ylims=ydens_range, known_mean=input$vel_mu, mu_guess=params$mu_guess["orig", input$iter,],
                    #npart=input$npart, colors=params$resamp_colors["orig",input$iter,], sep_col=input$sep_col,
					#main_title="Distribution of resampling weights based on\npredictions of next location", index=1

						index <-  (c(1:npart) %in% index)
						
						nbold <- sum(index)

						wts_lwd <- rep(3, npart)
						head_cex <- rep(1.5, npart)
						wts_lwd[ index ] <- 7
						head_cex[ index ] <- 3.5


						plot(x=-5,y=-10, xlim=xlims, ylim=ylims, xlab="Particle guess of mean velocity", ylab="Density",las=1,
										 main="Resampling weights from predicting next location", cex.main=1.1)
						abline(v=known_mean, lty=2)
						abline(h=0, lwd=0.5)
				
						segments(x0=mu_guess[ ! index ], x1=mu_guess[ ! index ], y0=rep(0, npart-nbold), y1=wts[ ! index ], col=colors[ ! index ], lwd=wts_lwd[ ! index ])
						points(x=mu_guess[ ! index ], y=wts[ ! index ], col=colors[ ! index ], pch=19, cex=head_cex[ ! index ])
		
						segments(x0=mu_guess[ index ], x1=mu_guess[ index ], y0=rep(0, nbold), y1=wts[ index ], col=colors[ index ], lwd=wts_lwd[ index ])
						points(x=mu_guess[ index ], y=wts[ index ], col=colors[ index ], pch=19, cex=head_cex[ index ])
			
						if (sep_col==FALSE) { 
							legend("topright", lwd=2, lty=1, pch=19, col=c("red", "lightgray"), legend=c("best 15%","others"))
						}

						rug_multicolor(x=mu_guess, ticksize=-0.04, col_vec=colors)

					}


wt_dist_twostate_loop <- function(wts, mu_guess, xlims, ylims, known_mean, npart, colors, sep_col=TRUE, iter, behavior, index=1, var_name) {

					old_pars <- par(mar=par()$mar, mfrow=par()$mfrow, oma=par()$oma)
					on.exit(expr=par(old_pars))
					
					par(mfrow=c(2,1), mar=c(3,4,2,2)+0.1, oma = c(0, 0, 2, 0))

					wts_lwd <- matrix(3, ncol=2, nrow=npart)
					head_cex <- matrix(1.5, ncol=2, nrow=npart)
					wts_lwd[ index, behavior ] <- 7
					head_cex[ index, behavior ] <- 3.5

					npts <- index
					index <-  (c(1:npart) %in% index)
					nbold <- sum(index)
			
					
					#likely_behavior <- apply(wts, 2, function(x) random_which(condition=c(x== max(x))))
					likely_behavior <- apply(wts, 2, function(x) which(x== max(x))[1])


					x_overall <- mu_guess[ cbind(likely_behavior, 1:npart) ]

					plot(x=-5,y=-10, xlim=xlims, ylim=ylims, yaxs="i", xlab="", 
						ylab="Density", main=expression("Overall weight"==~w[t]^{"(n)"}),las=1, cex.main=1.1, cex.axis=0.7)
					mtext(side=1, text=var_name, line=2)
					#abline(v=known_mean[ behavior ])
					if (sep_col==FALSE) {
						legend("topleft", col=c("lightgray", "red"), pch=19, legend=c("particle","best 15%"), lwd=2, ncol=2, cex=0.7)
					}
					
					#plot unconditional weights
					segments(x0=x_overall, x1=x_overall, y0=rep(0, npart), y1= colSums(wts), col=colors, lwd=wts_lwd[,behavior], lty=1)
					points(x=x_overall, y= colSums(wts), col=colors, pch=19, cex=head_cex[,behavior])


					#behavior-conditional

					plot(x=-5,y=-10, xlim=xlims, ylim=ylims, yaxs="i", xlab="", main=expression("Behavior-conditional weight"==~w["t|k"]^{"(n)"}),
						ylab="Density",las=1, cex.main=1.1, cex.axis=0.7)
					mtext(side=1, text=var_name, line=2)
					abline(v=known_mean)
					
					if (sep_col==FALSE) {
						legend("topleft", col=c("lightgray", "red", "lightgray","lightgray"), legend=c("particle","best 15%"), lwd=2, ncol=2, cex=0.7,
								pch=c(19, 19, 15, 17), pt.cex=1)
							
					}
					else {
						legend("topleft", col="blue", legend=c("type 1","type 2"), ncol=1, cex=0.7, 
								pch=c(15, 17), pt.cex=1)
							
					}
					
					for (kk in 1:2) {

						segments(x0=mu_guess[kk,], x1=mu_guess[kk,], y0=rep(0, npart), y1=wts[kk,], col=colors, lwd=wts_lwd[,kk], lty=c(1,3)[ kk ])
						points(x=mu_guess[kk,], y=wts[kk,], col=colors, pch=c(15,17)[ kk ], cex=head_cex[,kk])
					}
					#legend(x=par()$usr[ 1 ], y=ylims[ 2 ], pch=c(15,17), col=rep("blue",2), legend=c("type 1", "type 2"), pt.cex=1.5, cex=0.8)
					rug_multicolor(x=mu_guess, ticksize=-0.05, col_vec=rep(colors,2), plot_side=1)

					
					mtext(text="Particle resampling weights", outer=TRUE, side=3, cex=1.25, font=2)
				}


# wt_dist_twostate_loop <- function(wts, mu_guess, xlims, ylims, known_mean, npart, colors, sep_col=TRUE, iter, behavior, index=1) {

					# #wts=params$wts[input$iter,], xlims=xdens_range, ylims=ydens_range, known_mean=input$vel_mu, mu_guess=params$mu_guess["orig", input$iter,],
                    # #npart=input$npart, colors=params$resamp_colors["orig",input$iter,], sep_col=input$sep_col,
					# #main_title="Behavior-conditional distribution of resampling weights\nbased on predictions of next location"

					# if (ylims[ 2 ]>0.01) {

						# ylims <- c(0, round(ylims[ 2 ], digits=2))
						# y_axis <- seq(0, ylims[ 2], length.out=4)
						# y_axis_labs <- sprintf("%.2f",y_axis)
					# }
					# else {
						# y_axis <- seq(0, ylims[ 2], length.out=4)
						# y_axis_labs <- formatC(y_axis, format="e", digits=0)
						# y_axis_labs[ 1 ] <- "0.0"

					# }



					# wts_lwd <- matrix(3, ncol=2, nrow=npart)
					# head_cex <- matrix(1.5, ncol=2, nrow=npart)
					# wts_lwd[ index, behavior ] <- 7
					# head_cex[ index, behavior ] <- 3.5

					# npts <- index
					# index <-  (c(1:npart) %in% index)
					# nbold <- sum(index)




					# plot(x=-5,y=-10, xlim=xlims, ylim=c(0, 2*ylims[ 2 ]), xlab="Particle guess of mean velocity", ylab="Density",las=1, yaxt="n",
						 # main=paste("Overall (top) and behavior-conditional (bottom)\nresampling weights, t=",iter,
								# ifelse(sep_col,"","\n(best 15% are red)"), sep=""), cex.main=1)


					# abline(h=c(0, ylims[ 2 ]))

					# axis(side=2, at=y_axis, labels=y_axis_labs, las=1, cex.axis=0.65, mgp=c(3, 0.45, 0))
					# axis(side=4, at=y_axis + ylims[ 2 ], labels=y_axis_labs, las=1, cex.axis=0.65, mgp=c(3, 0.45, 0))


					# segments(y0=rep(0,2), y1=rep(ylims[ 2 ], 2), x0=known_mean, x1=known_mean, lty=2)

					# #plot unconditional weights
					# segments(x0=mu_guess[behavior,], x1=mu_guess[behavior,], y0=rep(ylims[ 2 ], npart), y1=ylims[ 2 ]+ colSums(wts), col=colors, lwd=wts_lwd[,behavior], lty=1)
					# points(x=mu_guess[behavior,], y=ylims[ 2 ]+ colSums(wts), col=colors, pch=19, cex=head_cex[,behavior])


					# #behavior-conditional

					# for (kk in 1:2) {

						# segments(x0=mu_guess[kk,], x1=mu_guess[kk,], y0=rep(0, npart), y1=wts[kk,], col=colors, lwd=wts_lwd[,kk], lty=c(1,3)[ kk ])
						# points(x=mu_guess[kk,], y=wts[kk,], col=colors, pch=c(15,17)[ kk ], cex=head_cex[,kk])
					# }

					# legend(x=par()$usr[ 1 ], y=ylims[ 2 ], pch=c(15,17), col=rep("blue",2), legend=c("type 1", "type 2"), pt.cex=1.5, cex=0.8)

					# rug_multicolor(x=mu_guess, ticksize=-0.05, col_vec=rep(colors,2), plot_side=1)




				# }


# wt_dist_twostate_loop <- function(wts, xlims, ylims, known_mean, mu_guess, npart, colors, sep_col=TRUE,
					# main_title="Behavior-conditional distribution of resampling weights\nbased on predictions of next location", index=1, behavior=1) {

					# #wts=params$wts[input$iter,], xlims=xdens_range, ylims=ydens_range, known_mean=input$vel_mu, mu_guess=params$mu_guess["orig", input$iter,],
                    # #npart=input$npart, colors=params$resamp_colors["orig",input$iter,], sep_col=input$sep_col,
					# #main_title="Behavior-conditional distribution of resampling weights\nbased on predictions of next location", index=1, behavior=1


						# index <-  (c(1:npart) %in% index)
						# nbold <- sum(index)
						# #print(index)
						# #print(nbold)
						# wts_lwd <- matrix(3, ncol=2, nrow=npart)
						# head_cex <- matrix(1.5, ncol=2, nrow=npart)
						# wts_lwd[ index, behavior ] <- 7
						# head_cex[ index, behavior ] <- 3.5


						# plot(x=-5,y=-10, xlim=xlims, ylim=ylims, xlab="Particle guess of mean velocity", ylab="Density",las=1,
										 # main=paste(main_title, ifelse(sep_col,""," (best 15% are red)"),sep=""))
						# abline(v=known_mean, lty="dotted", col=1, lwd=3)
						# abline(h=0, lwd=0.5)


						# for (kk in 1:2) {

							# segments(x0=mu_guess[ kk, ! index ], x1=mu_guess[ kk, ! index ], y0=rep(0, npart-nbold), y1=wts[ kk, ! index ], col=colors[ ! index ], lwd=wts_lwd[ ! index, kk ], lty=kk)
							# points(x=mu_guess[ kk, ! index ], y=wts[ kk, ! index ], col=colors[ ! index ], pch=c(15,17)[ kk ], cex=head_cex[ ! index, kk ])


						# }

						# segments(x0=mu_guess[  , index ], x1=mu_guess[  , index ], y0=rep(0, 2), y1=wts[ , index ], col=colors[ index ], lwd=wts_lwd[ index, ], lty=1:2)
						# points(x=mu_guess[  , index ], y=wts[ , index ], col=colors[ index ], pch=c(15,17), cex=head_cex[ index, ])

						# legend("topright", pch=c(15,17), col=rep("blue",2), legend=c("type 1", "type 2"), pt.cex=1.5)
						# rug_multicolor(x=mu_guess, ticksize=-0.04, col_vec=rep(colors, each=2), lims=xlims)

					# }



convergence_rugplot <- function(xlims, known_mean, max_iter, iter, mu_guess, npart, colors, var_name="velocity") {

				  #xlims=xdens_range, known_mean=input$vel_mu, max_iter=input$max_iter, iter=input$iter,
                                #mu_guess=params$mu_guess, npart=input$npart, var_name="velocity", main_title=paste("Convergence of particle guesses of mean",var_name)

  				    plot(x=10, y=10, ylim=c(-2.1*max_iter, -1), xlim=xlims, yaxt="n", xlab=paste("mean",var_name), ylab="Iteration",
					     main=paste("Convergence of particle guesses of mean",var_name), cex.main=1.1)

					axis(side=2, at=seq(-2, -2*max_iter, by=-8), labels=seq(1,max_iter, by=4), las=1)

					for (ii in 1:iter) {
                        if (ii ==iter) {
						    rrange <- nice_range(mu_guess[,ii,], ep=0.1)
						    rect(xleft=par()$usr[ 1 ], xright=par()$usr[ 2 ],
						         ybottom=-2*ii-0.9, ytop=-2*ii+1, border=NA, col="yellow")
							}

						segments(x0=mu_guess["orig",ii,], x1=mu_guess["orig",ii,],
                                y0=rep(-2*ii+1,npart), y1=rep(-2*ii +0.1,npart), col=colors["orig",ii,], lwd=2)
						segments(x0=mu_guess["resamp",ii,], x1=mu_guess["resamp",ii,],
                                y0=rep(-2*ii  ,npart), y1=rep(-2*ii-0.9 ,npart), col=colors["resamp",ii,], lwd=2)
					    }

					abline(v=known_mean, lty=2)
					#legend(ifelse(abs(xlims[1]-known_mean) > abs(xlims[2]-known_mean), "bottomleft","bottomright"), pch=c(1,1,1,15), col=c("black","red","black","yellow"),
					#       lwd=c(1,1,3,0),
					#       legend=c("orig.","resamp.","true","current"), seg.len=1, pt.lwd=c(1,1,NA,3), pt.cex=c(0,0,0,2), lty=c("solid","solid","dashed","solid"), ncol=2, cex=0.8)

					legend("bottomleft", legend=c("particles","current"), col=c("green","yellow"), lwd=c(2, NA), pch=c(NA,15), pt.lwd=c(1,3), pt.cex=c(0,2), cex=0.8)


					}



convergence_rugplot_twostate <- function(xlims, known_mean, max_iter, iter, mu_guess, npart, colors, var_name="velocity") {

				  #xlims=xdens_range, known_mean=input$vel_mu, max_iter=input$max_iter, iter=input$iter,
                                #mu_guess=params$mu_guess, npart=input$npart, var_name="velocity", main_title=paste("Convergence of particle guesses of mean",var_name)


				    ylims <- c(-1*max_iter, -1)
					ymax <- floor(max_iter/3)

					y_axis <- seq(1, 3*ymax+1, length.out=4)

  				    plot(x=10, y=10, ylim=c(-2*max_iter -1, -1), xlim=xlims, yaxt="n", xlab=paste("mean",var_name), ylab="Iteration",
					     main=paste("Convergence of particle guesses of mean",var_name), cex.main=1.1)

					axis(side=4, at=-1*y_axis, labels=y_axis, las=1, cex.axis=0.65)
					axis(side=2, at=-1*y_axis  -max_iter, labels=y_axis, las=1, cex.axis=0.65)

					abline(h=-1*max_iter -1)



					for (ii in 1:iter) {
						for (kk in 1:2) {
							if (ii ==iter) {

						    rect(xleft=par()$usr[ 1 ], xright=par()$usr[ 2 ],
						         ytop=-1 - (kk-1)*max_iter - (ii-1), ybottom=-1 - (kk-1)*max_iter -ii, border=NA, col="yellow")
							}

						segments(x0=mu_guess["resamp",ii,kk,], x1=mu_guess["resamp",ii,kk,],
                                 y0=rep(-1-(kk-1)*max_iter - (ii-1), npart), y1=rep(-1-(kk-1)*max_iter -ii, npart),
								 col=colors["resamp",ii,], lwd=2)
						}
					}

					segments(y0=c(-1*max_iter-1, par()$usr[ 3 ]), y1=c(par()$usr[ 4 ], -1*max_iter-1), x0=known_mean, x1=known_mean, lty=2)

					text(x=par()$usr[ 1 ] + c(0.75, 0.25)*diff(par()$usr[ 1:2 ]), y= -1 - c(0.15, 1.15)*max_iter, labels=c("type 1","type 2"))
					legend("bottomleft", legend=c("particles","current"), col=c("green","yellow"), lwd=c(2, NA), pch=c(NA,15), pt.lwd=c(1,3), pt.cex=c(0,2), cex=0.8)

					# abline(v=known_mean, lty=2)
					# legend(ifelse(abs(xlims[1]-known_mean) > abs(xlims[2]-known_mean), "bottomleft","bottomright"), pch=c(1,1,1,15), col=c("black","red","black","yellow"),
					       # lwd=c(1,1,3,0),
					       # legend=c("orig.","resamp.","true","current"), seg.len=1, pt.lwd=c(1,1,NA,3), pt.cex=c(0,0,0,2), lty=c("solid","solid","dashed","solid"), ncol=2, cex=0.8)

					}



location_history <- function(Ydist, xticks, xlims, iter, npart, colors, yt) {

					#Ydist=params$Ydist[,"resamp_hist",,], xticks=loc_axis, xlims=xrange, iter=input$iter, npart=input$npart,
                    #         colors=params$resamp_colors["resamp",,], yt=yt

                    plot(y=-100, x=-10, ylim=c(-.1*npart, npart*1.1), xlim=xlims, las=1, cex.main=1.1,
						   main="Particle history of location guesses after resampling", ylab="Particle", xlab="Location", xaxt="n", yaxt="n")
						axis(side=1, at=xticks, labels=xticks)
					    twostate <- "state" %in% dimnames(Ydist)[[1]]
						for (p in 1:npart) {
						   pchar <- rep(19,iter)
                           if (twostate) { pchar[ Ydist["state",1:iter,p]==2 ] <- 4  }

						   points(x=c(0,Ydist["MuY",1:iter,p]), y=rep(p, iter+1), pch=pchar, col=colors[iter,p], lwd=2, cex=min(15/npart,1))
						   #arrows(x0=c(0,params$Ydist["MuY","resamp_hist",1:(iter-1),p]), y0=rep(p, iter+1),
						   #       x1=params$Ydist["MuY","resamp_hist",1:(iter),p], y1=rep(p, iter+1),
						    #	 	  length=0.1, lty=1.5, col=params$resamp_colors["resamp",iter,p])
						   }
						   abline(v=yt[1:(iter+1),"true_loc"], lty="dashed")

						if (twostate) { legend("bottomleft", lty="dashed",legend=c("Observed locations","type 1","type 2"),
						                pch=c(NA,19,4), col=c("black","green","green"), lwd=c(1,NA,NA), pt.lwd=c(NA,2,2), ncol=3) }
						else { legend("bottomleft", lty="dashed",legend="Observed locations") }
                        }


probability_trans <- function(true_probs, dir_params, npart, colors) {
			
                        old_pars <- par(mar=par()$mar, mfrow=par()$mfrow, oma=par()$oma)
						on.exit(expr=par(old_pars))
						par(mfrow=c(2,1), mar=c(3,4,2,2)+0.1, oma = c(0, 0, 2, 0))
						#1 to 2 
						bdens_pts <- seq(0,1,by=0.01)
						
						
						trans_dens_modes <- (dir_params[c("a12","a21"),]-1)/(dir_params[c("a12","a21"),] + dir_params[c("a11","a22"),] -2)
											
		
						trans_dens_mode_dens <- dbeta(x=trans_dens_modes, shape1=dir_params[c("a12","a21"),], shape2=dir_params[c("a11","a22"),])
						ylims <- c(0, 1.1*max(trans_dens_mode_dens, na.rm=TRUE))
						
						
						plot(x=-5, y=-5, xlim=c(0,1), ylim=ylims, xaxs="i", yaxs="i", ylab="Density", xlab="",						
							main=expression("Pr(1"%->%"2)"), cex.main=1.1, las=1, cex.axis=0.7)
						mtext(side=1, text="Probability", line=2)

						abline(v=true_probs[ 1 ], lty=2)

						for (nn in 1:npart) {
							lines(x=bdens_pts, y=dbeta(x=bdens_pts, shape1=dir_params["a12",nn], shape2=dir_params["a11",nn]),
						           col=colors[ nn ], lwd=.5, type="l")
							
						}
						rug_multicolor(dir_params["a12",]/(dir_params["a12",] + dir_params["a11",]), plot_side=3, ticksize=-0.05, col_vec=colors)
						
						#2 to 1
						
						plot(x=-5, y=-5, xlim=c(0,1), ylim=ylims, xaxs="i", yaxs="i", ylab="Density", xlab="", 
							main=expression("Pr(2"%->%"1)"), cex.main=1.1, las=1, cex.axis=0.7)
						mtext(side=1, text="Probability", line=2)
						abline(v=true_probs[ 1 ], lty=2)

						for (nn in 1:npart) {
							lines(x=bdens_pts, y=dbeta(x=bdens_pts, shape1=dir_params["a21",nn], shape2=dir_params["a22",nn]),
						           col=colors[ nn ], lwd=.5, type="l")
							
						}
						rug_multicolor(dir_params["a21",]/(dir_params["a21",] + dir_params["a22",]), plot_side=3, ticksize=-0.05, col_vec=colors)

						abline(v=true_probs[ 2 ], lty=2)
						
						mtext(text="Behavior switching probabilities", outer=TRUE, side=3, cex=1.25, font=2)
						


					}
						
						
						
						

# probability_trans <- function(true_probs, dir_params, npart, colors, ylims) {

						# true_probs=c(input$p1to2, input$p2to1), dir_params=params$dir_params[,"resamp",input$iter,], npart=input$npart,
                              # colors=params$resamp_colors["resamp",input$iter,], ylims=trans_dens_range

                        # par(mfrow=c(1,2))

						# ylims <- c(0, round(ylims[ 2 ], digits=1))

						# y_axis <- seq(0, ylims[ 2], length.out=4)
						# y_axis_labs <- sprintf("%.2f",y_axis)


                        # bdens_pts <- seq(0,1,by=0.01)

						# plot(x=-5, y=-5, xlim=c(0,1), ylim=c(0, 2*ylims[2]), ylab="Density", xlab="Probability", main="Behavior switching probabilities",
							# yaxt="n")

						# axis(side=2, at=y_axis, labels=y_axis_labs, las=1, cex.axis=0.67)
						# axis(side=4, at=y_axis + ylims[ 2 ], labels=y_axis_labs, las=1, cex.axis=0.67)

						# segments(y0=c(0, ylims[ 2 ]), y1=c(ylims[ 2 ], 2*ylims[ 2 ]), x0=true_probs, x1=true_probs, lty=2)



						# for (nn in 1:npart) {
							# lines(x=bdens_pts, y=dbeta(x=bdens_pts, shape1=dir_params["a12",nn], shape2=dir_params["a11",nn])+ylims[ 2 ],
						           # col=colors[ nn ], lwd=.5, type="l")
							# lines(x=bdens_pts, y=dbeta(x=bdens_pts, shape1=dir_params["a21",nn], shape2=dir_params["a22",nn]),
						           # col=colors[ nn ], lwd=.5, type="l")
						# }

						# text(x=c(0.8, 0.8), y=c(0.8, 1.8)*ylims[ 2 ], labels=c("Pr(2 -> 1)", "Pr(1 -> 2)"))

						# abline(h=c(0, ylims[ 2 ]))

						# legend("topright", lwd=3, lty="dashed", legend="true prob.")
						# rug_multicolor(dir_params["a12",]/(dir_params["a12",] + dir_params["a11",]), plot_side=3, ticksize=-0.05, col_vec=colors)
						# rug_multicolor(dir_params["a21",]/(dir_params["a21",] + dir_params["a22",]), plot_side=1, ticksize=-0.05, col_vec=colors)




						# }

state_agreement <- function(particle_states, actual_states, iter, npart) {

						#particle_states=params$curr_state["resamp",,], actual_states=yt[,"state"], iter=input$iter, npart=input$npart
							max_iter <- length(actual_states)
							rownames(particle_states) <- 1:max_iter
							agree_pct <- particle_states[1:iter,,drop=FALSE]==matrix(actual_states[1:iter], nrow=iter, ncol=npart)
							agree_pct <- c(rowSums(agree_pct)/npart, rep(0,max_iter-iter))

							actual_states_col <- actual_states
							actual_states_col[ actual_states==1 ] <- "gray"
							actual_states_col[ actual_states==2 ] <- "black"



							barplot(height=agree_pct, col=actual_states_col, main="Fraction of particles guessing true behavior", xlim=c(0.5,max_iter),
									ylim=c(-0.05,1.2), yaxt="n",border=NA, space=0, xlab="Iteration", ylab="Proportion", las=1)
							box()
							abline(h=c(0,1))
							axis(2, sprintf("%.1f", seq(0,1,by=0.2)), at= seq(0,1,by=0.2), las=1)
							legend("topright", legend=1:2, pch=15, pt.cex=3, col=c("gray","black"), ncol=3, title="true behavior")

						}


location_history_v2 <- function(omega, xticks, xlims, iter, npart, yt, colors) {

						#omega=params$xpart[1,"resamp",,], xticks=loc_axis, xlims=xrange, iter=input$iter, npart=input$npart, yt=yt

					max_iter <- length(yt)-1
					

                    plot(y=-100, x=-10, ylim=c(-2.1*max_iter, -1), xlim=xlims, las=1, cex.main=1.1,
						   main="History of prediction of next location", ylab="Iteration", xlab="Location", yaxt="n", xaxt="n")
						axis(side=1, at=xticks, labels=xticks)
					    axis(side=2, at=seq(-2, -2*max_iter, by=-8), labels=seq(1,max_iter, by=4), las=1)



						for (ii in 1:iter) {



							if (ii==iter) {
								#rrange <- nice_range(Ydist["MuY",ii,], ep=0.1)
								#rrange <- nice_range(omega[ii,], ep=0.1)
								rect(xleft=par()$usr[ 1 ], xright=par()$usr[ 2 ],
						         ybottom=-2*ii-1, ytop=-2*ii+1, border=NA, col="yellow")
							}
							segments(x0=omega[ii,], x1=omega[ii,],
                                 y0=rep(-2*ii+1,npart), y1=rep(-2*ii-1,npart), col=colors[ii,], lwd=2)


						}



						abline(v=yt[ 1:iter ], lty="dotted")
						legend("bottomleft", legend=c("particles","current"), col=c("green","yellow"), lwd=c(2, NA), pch=c(NA,15), pt.lwd=c(1,3), pt.cex=c(0,2), cex=0.8)


						#if (twostate) { legend("bottomleft", lty="dashed",legend=c("Observed locations","type 1","type 2"),
						#                pch=c(NA,19,4), col=c("black","green","green"), lwd=c(1,NA,NA), pt.lwd=c(NA,2,2), ncol=3) }
						#else { legend("bottomleft", lty="dashed",legend="Observed locations") }
                        #}
					}

	


