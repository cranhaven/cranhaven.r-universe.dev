plot.DS_GF_macro_mean <-
function(x, ...){
	if(dim(x$prior.fit)[2] == 2){
	par(mar=c(5,5,4,2)+0.3) #changes left margin to make large labels fit
	plot(x$prior.fit$theta.vals, x$prior.fit$parm.prior, 
		type = "l", yaxs = "i", lwd = 2, col = "red3", 
		xlab = expression(theta), ylab = "", font.main = 1,
		cex.lab=1.45, cex.axis=1.5, cex.main=1.75, cex.sub=1.5, ...)
	title(ylab = expression(paste(hat(pi)(theta))), line = 2.3, cex.lab=1.45)
	points(x$model.mean, 0, col = "green", pch = 17, cex = 2.5)
	axis(1, at=seq(x$model.mean - x$mean.sd,
				   x$model.mean + x$mean.sd,length=20),
				   tick=TRUE, col="goldenrod4",labels = F,tck=-0.015)
	} else {
	par(mar=c(5,5,4,2)+0.3) #changes left margin to make large labels fit
	plot(x$prior.fit$theta.vals, x$prior.fit$ds.prior, 
		type = "l", yaxs = "i", lwd = 2, col = "red3", 
		xlab = expression(theta), ylab = "", font.main = 1,
		cex.lab=1.45, cex.axis=1.5, cex.main=1.75, cex.sub=1.5, ...)
	title(ylab = expression(paste(hat(pi)(theta))), line = 2.3, cex.lab=1.45)
	points(x$model.mean, 0, col = "green", pch = 17, cex = 2.5)
	axis(1, at=seq(x$model.mean - x$mean.sd,
				   x$model.mean + x$mean.sd,length=20),
				   tick=TRUE, col="goldenrod4",labels = F,tck=-0.015)
	}
}
