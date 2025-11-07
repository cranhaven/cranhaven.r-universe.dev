plot.abcrf <- function(x, training, obs=NULL, n.var=20, pdf=FALSE, xlim=NULL, ...)
{
  
  if (!inherits(obs, "data.frame") && !is.null(obs) ) 
    stop("obs needs to be a data.frame object or NULL")
  if (!inherits(training, "data.frame"))
    stop("training needs to be a data.frame object")
  if (!is.null(xlim) && !is.numeric(xlim))
    stop("xlim needs to be a numeric object or NULL")

  if (length(x$group)!=0)
  {
    ngroup <- length(x$group)
    varn <- x$formula[[2]]
    training[[as.character(varn)]] <- as.vector(training[[as.character(varn)]])
    allmod <- unique(training[[as.character(varn)]])
    for (k in 1:ngroup) for (l in 1:length(x$group[[k]])) 
      training[[as.character(varn)]][which(training[[as.character(varn)]]==x$group[[k]][l])] <- paste("g",k,sep="")
    if (!setequal(allmod,unlist(x$group)))
    {
      diffe <- setdiff(allmod,unlist(x$group))
      for (l in 1:length(diffe)) training <- training[-which(training[[as.character(varn)]]==diffe[l]),]
    }
    training[[as.character(varn)]] <- as.factor(training[[as.character(varn)]])
  }
  
	old.par <- par(no.readonly = TRUE)

	if (length(x$model.rf$variable.importance)<20) 
	  n.var <- length(x$model.rf$variable.importance)

	mf <- match.call(expand.dots=FALSE)
	mf <- mf[1]
	mf$formula <- x$formula
	mf$data <- training
	mf[[1L]] <- as.name("model.frame")
	mf <- eval(mf, parent.frame() )
	mt <- attr(mf, "terms")
	modindex <- model.response(mf)
	
	if (x$lda) {
 	  if (pdf) { 
 	    pdf("graph_varImpPlot.pdf")
		  variableImpPlot(x, n.var=n.var, xlim=xlim)
		  dev.off()
 	  }
 	  variableImpPlot(x, n.var=n.var, xlim=xlim)
		nmod <- length(x$model.rf$forest$levels)
		nstat <- x$model.rf$num.independent.variables
		projections <- predict(x$model.lda, training)$x
		if  (!is.null(obs)) projobs <- predict(x$model.lda, obs)$x
		coloris <- rainbow(nmod)
		colo <- coloris[modindex]
		readline("Press <ENTER> to Continue")
    if (nmod > 2) {
      if (pdf)
        {
        pdf("graph_lda.pdf")
        par(mar=par()$mar+c(0,0,0,5), xpd=TRUE)
        plot(projections[,1:2], col=colo, pch=3)
        legend("topright", legend = as.character(x$model.rf$forest$levels), col = coloris, 
               pch = 15, bty = "o", pt.cex = 2, cex = .8, horiz = FALSE, 
               inset = c(-.16, 0), ncol = 2, title = "Models", bg = "white")
	  	  if  (!is.null(obs)) points(projobs[1],projobs[2],pch="*",cex=5.3)
	  	  dev.off()
      }
      par(mar=par()$mar+c(0,0,0,5), xpd=TRUE)
      plot(projections[,1:2], col=colo, pch=3)
      legend("topright", legend = as.character(x$model.rf$forest$levels), col = coloris, 
             pch = 15, bty = "o", pt.cex = 2, cex = .8, horiz = FALSE, 
             inset = c(-.16, 0), ncol = 2, title = "Models", bg = "white")
      if  (!is.null(obs)) points(projobs[1],projobs[2],pch="*",cex=5.3)
    } else {
      l1 <- x$model.rf$forest$levels[1]
      l2 <- x$model.rf$forest$levels[2]
      d1 <- density(projections[modindex == l1])
      d2 <- density(projections[modindex == l2])
      coloris <- c("blue", "orange")
      xrange <- range(c(d1$x, d2$x))
      yrange <- c(0, 1.2*max(c(d1$y, d2$y)))
      if (pdf)
        {
        pdf("graph_lda.pdf")
        plot(d1, xlim = xrange, ylim = yrange,
             col=coloris[1], main="", xlab="")
        lines(d2, col=coloris[2])
        legend("topleft", legend = as.character(x$model.rf$forest$levels), col = coloris, 
               cex = .8, horiz = TRUE, lty=1, bty="o",
               inset = c(.01, .01), title = "Models", bg = "white")
        if  (!is.null(obs)) abline(v=projobs)
        dev.off()
      }
      plot(d1, xlim = xrange, ylim = yrange,
           col=coloris[1], main="", xlab="")
      lines(d2, col=coloris[2])
      legend("topleft", legend = as.character(x$model.rf$forest$levels), col = coloris, 
             cex = .8, horiz = TRUE, lty=1, bty="o",
             inset = c(.01, .01), title = "Models", bg = "white")
      if  (!is.null(obs)) abline(v=projobs)
    }
	} else {
	  if (pdf)
	    {
	    pdf("graph_varImpPlot.pdf")
	    variableImpPlot(x , n.var=n.var, xlim=xlim)
	    dev.off()
	    }
	  variableImpPlot(x , n.var=n.var, xlim=xlim)
	}
	par(old.par)
}