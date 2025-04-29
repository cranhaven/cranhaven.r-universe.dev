# short demo for prsim.weather
### load data for four stations
data(weather_sim_multi_sites)
sim <- weather_sim_multi_sites
# weather_sim_multi_sites <- list(weather_sim_multi_sites[[1]],weather_sim_multi_sites[[2]])
# save(file='weather_sim_multi_sites.rda', weather_sim_multi_sites)

### define plotting colors
col_sim <- adjustcolor("#fd8d3c",alpha=0.8)
col_sim_tran <- adjustcolor("#fd8d3c",alpha=0.2)
col_obs <- adjustcolor( "black", alpha.f = 0.2)
### greys
col_vect_obs <- c('#cccccc','#969696','#636363','#252525')
### oranges
col_vect_sim <- c('#fdbe85','#fd8d3c','#e6550d','#a63603')

### plot time series for multiple sites

### Temperature (first list entry)
oldpar <- par(mfrow = c(2, 1), mar = c(3, 3, 2, 1))
### determine ylim
ylim_max <- max(sim[[1]][[1]]$Temp)*1.5
### observed
plot(sim[[1]][[1]]$Temp[1:1000],ylab=expression(bold(paste("Temperature [degrees]"))),xlab="Time [d]",type="l",col=col_vect_obs[1],ylim=c(0,ylim_max),main='Observations')
for(l in 2){
  lines(sim[[l]][[1]]$Temp[1:1000],col=col_vect_obs[l])
}
# legend('topleft',legend=c('Station 1','Station 2','Station 3','Station 4'),lty=1,col=col_vect_obs[1:4])
### simulated (one run)
plot(sim[[1]][[1]]$r1[1:1000],ylab=expression(bold(paste("Temperature [degrees]"))),xlab="Time [d]",type="l",col=col_vect_sim[1],ylim=c(0,ylim_max),main='Stochastic simulations')
for(l in 2){
  lines(sim[[l]][[1]]$r1[1:1000],col=col_vect_sim[l])
}


### precipitation (second list entry)
ylim_max <- max(sim[[1]][[2]]$Prec)*1
### observed
plot(sim[[1]][[2]]$Prec[1:1000],ylab=expression(bold(paste("Precipitation [mm/d]"))),xlab="Time [d]",type="l",col=col_vect_obs[1],ylim=c(0,ylim_max),main='Observations')
for(l in 2){
  lines(sim[[l]][[2]]$Prec[1:1000],col=col_vect_obs[l])
}
# legend('topleft',legend=c('Station 1','Station 2','Station 3','Station 4'),lty=1,col=col_vect_obs[1:4])
### simulated (one run)
plot(sim[[1]][[2]]$r1[1:1000],ylab=expression(bold(paste("Precipitation [mm/d]"))),xlab="Time [d]",type="l",col=col_vect_sim[1],ylim=c(0,ylim_max),main='Stochastic simulations')
for(l in 2){
  lines(sim[[l]][[2]]$r1[1:1000],col=col_vect_sim[l])
}
par(oldpar)