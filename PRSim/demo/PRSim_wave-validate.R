# short demo
### load data for four stations
data(simulations_multi_sites)
sim <- simulations_multi_sites
### loads simulations_multi_sites
# sim <- list()
# load("~/PRSim-devel/data/simulations_multi_sites/1_stoch_sim.rda")
# sim[[1]] <- out_list$simulation
# load("~/PRSim-devel/data/simulations_multi_sites/2_stoch_sim.rda")
# sim[[2]] <- out_list$simulation
# load("~/PRSim-devel/data/simulations_multi_sites/3_stoch_sim.rda")
# sim[[3]] <- out_list$simulation
# load("~/PRSim-devel/data/simulations_multi_sites/3_stoch_sim.rda")
# sim[[4]] <- out_list$simulation

# setwd("~/PRSim-devel/data")
# save(simulations_multi_sites,file='simulations_multi_sites.rda')
oldpar <- par(mfrow = c(2, 1), mar = c(3, 3, 2, 1))
### define plotting colors
col_sim <- adjustcolor("#fd8d3c",alpha=0.8)
col_sim_tran <- adjustcolor("#fd8d3c",alpha=0.2)
col_obs <- adjustcolor( "black", alpha.f = 0.2)
### greys
col_vect_obs <- c('#cccccc','#969696','#636363','#252525')
### oranges
col_vect_sim <- c('#fdbe85','#fd8d3c','#e6550d','#a63603')

### plot time series for multiple sites
par(mfrow=c(2,1),mar=c(3,3,2,1))
### determine ylim
ylim_max <- max(sim[[1]]$Qobs)*1.5
### observed
plot(sim[[1]]$Qobs[1:1000],ylab=expression(bold(paste("Specific discharge [mm/d]"))),xlab="Time [d]",type="l",col=col_vect_obs[1],ylim=c(0,ylim_max),main='Observations')
for(l in 2:4){
  lines(sim[[l]]$Qobs[1:1000],col=col_vect_obs[l])
}
# legend('topleft',legend=c('Station 1','Station 2','Station 3','Station 4'),lty=1,col=col_vect_obs[1:4])
### simulated (one run)
plot(sim[[1]]$r1[1:1000],ylab=expression(bold(paste("Specific discharge [mm/d]"))),xlab="Time [d]",type="l",col=col_vect_sim[1],ylim=c(0,ylim_max),main='Stochastic simulations')
for(l in 2:4){
  lines(sim[[l]]$r1[1:1000],col=col_vect_sim[l])
}


### plot cross-correlation function
par(mfrow=c(4,4),mar=c(0,0,0,0))
### run through each station comtination
for(j in 1:4){
  for(i in 1:4){
    ### ccf of observations
    data_mat <- matrix(unlist(lapply(sim, "[", , "Qobs")),ncol=length(sim))
    ccf_obs <- ccf(data_mat[,i],data_mat[,j],plot=FALSE)
    ### plot ccfs of observations
    plot(ccf_obs$lag,ccf_obs$acf,col=col_obs,type="l",ylim=c(0,1),main='',xaxt='n',yaxt='n')
      
    ### simulated ccf
    ### run through each simulation run
    for(r in 1:2){
      data_mat_sim <- matrix(unlist(lapply(sim, "[", , paste("r",r,sep=""))),ncol=length(sim))
      ccf_sim <- ccf(na.omit(cbind(data_mat_sim[,i],data_mat_sim[,j]))[,1],na.omit(cbind(data_mat_sim[,i],data_mat_sim[,j]))[,2],plot=FALSE)
      ### add one ccf plot per simulation run
      lines(ccf_obs$lag,ccf_sim$acf,col=col_sim)
    }
    ### overplot observations again
    lines(ccf_obs$lag,ccf_obs$acf,col="black",lwd=2)
  }
}
par(oldpar)