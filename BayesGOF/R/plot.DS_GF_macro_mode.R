plot.DS_GF_macro_mode <-
function(x, ...){
	mode.mat <- matrix(0, nrow = length(x$model.modes), ncol = 2)
	for(i in 1:length(x$model.modes)){
		mode.mat[i,1] <- x$model.modes[i]-x$mode.sd[i]
		mode.mat[i,2] <- x$model.modes[i]+x$mode.sd[i]
		}
	if(dim(x$prior.fit)[2] == 3){
		par(mar=c(5,5,4,2)+0.3) #changes left margin to make large labels fit
		plot(x$prior.fit$theta.vals, x$prior.fit$ds.prior, 
			type = "l", yaxs = "i", lwd = 2, col = "red3",
			xlab = expression(theta), ylab = "", font.main = 1,
			cex.lab=1.45, cex.axis=1.5, cex.main= 1.75, cex.sub=1.5, ...)
		title(ylab = expression(paste(hat(pi)(theta))), line = 2.3, cex.lab=1.45)
		points(x$model.modes, rep(0, length(x$model.modes)), col = "green", pch = 17, cex = 1.5)
		for(i in 1:length(x$model.modes)){
			axis(1, at=seq(mode.mat[i,1],mode.mat[i,2],length=20),tick=TRUE,
						col="goldenrod4",labels = F,tck=-0.015)
			}
		} else {
		par(mar=c(5,5,4,2)+0.3) #changes left margin to make large labels fit
		plot(x$prior.fit$theta.vals, x$prior.fit$parm.prior, 
			type = "l", yaxs = "i", lwd = 2, col = "red3",
			xlab = expression(theta), ylab = "", font.main = 1,
			cex.lab=1.45, cex.axis=1.5, cex.main= 1.75, cex.sub=1.5, ...)
		title(ylab = expression(paste(hat(pi)(theta))), line = 2.3, cex.lab=1.45)
		points(x$model.modes, rep(0, length(x$model.modes)), col = "green", pch = 17, cex = 1.5)
		for(i in 1:length(x$model.modes)){
			axis(1, at=seq(mode.mat[i,1],mode.mat[i,2],length=20),tick=TRUE,
						col="goldenrod4",labels = F,tck=-0.015)
		}
	}
}
