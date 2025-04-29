# short demo
data(simulations)

sim <- simulations$simulation

# periodogram of deseasonalized
kern <- kernel("modified.daniell",c(10,10))
sp1 <- spec.pgram(sim$Qobs, k=kern, taper=0, log="no", plot=FALSE)
sp2 <- spec.pgram(sim$des, k=kern, taper=0, log="no", plot=FALSE)
plot(sp1, xlim=c(0,.05))     
plot( sp2, add=TRUE, col=2)

# Peaks correspond to the following cycles:
1/sp1$freq[head(order(sp1$spec, decreasing=TRUE))]

# compare periodogram of simulated series
plot(sp1, xlim=c(0,.05))     # would be nice to identify the peaks...
for (i in grep("r",names(sim))) {
  spi <- spec.pgram(sim[,i], k=kern, taper=0, log="no", plot=FALSE)
  plot( spi, add=TRUE, col="gray")
}

sp3 <- spec.pgram(sim$Qobs, taper=0, log="no", plot=FALSE)
1/sp3$freq[head(order(sp3$spec, decreasing=TRUE))]
# Annual, 6 months and 4 months


### plot mean regime for each simulation run and compare to observed regime
### define plotting colors
col_sim <- adjustcolor("#fd8d3c",alpha=0.8)
col_sim_tran <- adjustcolor("#fd8d3c",alpha=0.2)
col_obs <- adjustcolor( "black", alpha.f = 0.2)

year <- unique(sim$YYYY)

### compute mean runoff hydrograph
sim$day_id <- rep(seq(1:365),times=length(year))
mean_hydrograph_obs <- aggregate(sim$Qobs, by=list(sim$day_id), FUN=mean,simplify=FALSE)
plot(unlist(mean_hydrograph_obs[,2]), lty=1, lwd=1, col="black", ylab=expression(paste("Discharge [m"^3,"/s]")),
     xlab="Time [d]", main="Mean hydrographs", ylim=c(0,max(unlist(mean_hydrograph_obs[,2]))*1.5),type="l")

### add mean runoff hydrographs
for(r in 7:(length(names(sim))-1)){
  mean_hydrograph <- aggregate(sim[,r], by=list(sim$day_id), FUN=mean,simplify=FALSE)
  lines(mean_hydrograph, lty=1, lwd=1, col=col_sim)
}
### redo observed mean
lines(mean_hydrograph_obs, lty=1, lwd=1, col="black")

### autocorrelation
acf_mare <- list()
acf_obs <- acf(sim$Qobs, plot=FALSE)
plot(acf_obs$acf, type="l", xlab="Lag", main="Autocorrelation", ylab="ACF")
for(r in 7:(length(names(sim))-2)){
  acf_sim <- acf(sim[,r], plot=FALSE)
  lines(acf_sim$acf, col=col_sim, type="l")
  ### compute mean relative error in the acf
  acf_mare[[r]]<- mean(abs((acf_obs$acf-acf_sim$acf)/acf_obs$acf))
}
lines(acf_obs$acf)


### partial autocorrelation function
pacf_obs <- pacf(sim$Qobs, plot=FALSE)
pacf_mare <- list()
plot(pacf_obs$acf, type="l", xlab="Lag", main="Partial autocorrelation", ylab="PACF")
for(r in 7:(length(names(sim))-2)){
  pacf_sim <- pacf(sim[,r], plot=FALSE)
  lines(pacf_sim$acf, col=col_sim, type="l")
  ### compute mean relative error in the acf
  pacf_mare[[r]] <- mean(abs((pacf_obs$acf-pacf_sim$acf)/pacf_obs$acf))
}
lines(pacf_obs$acf)

### compute seasonal statistics
### Q50,Q05,Q95, boxplots
### define seasons: Winter:12,1,2; spring:3,4,5; summer: 6,7,8; fall: 9,10,11
sim$season <- "winter"
sim$season[which(sim$MM%in%c(3,4,5))] <- "spring"
sim$season[which(sim$MM%in%c(6,7,8))] <- "summer"
sim$season[which(sim$MM%in%c(9,10,11))] <- "fall"

### all simulated series show the same seasonal statistics. plot only one 
boxplot(sim$Qobs[which(sim$season=="winter")], sim$r1[which(sim$season=="winter")],
        sim$Qobs[which(sim$season=="spring")], sim$r1[which(sim$season=="spring")],
        sim$Qobs[which(sim$season=="summer")], sim$r1[which(sim$season=="summer")],
        sim$Qobs[which(sim$season=="fall")], sim$r1[which(sim$season=="fall")],
        border=c("black", col_sim, "black", col_sim, "black", col_sim, "black", col_sim), 
        xaxt="n", main="Seasonal statistics", outline=FALSE)
mtext(side=1, text=c("Winter", "Spring", "Summer", "Fall"), at=c(1.5,3.5,5.5,7.5))

