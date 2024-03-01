##JGR scatterplot function
#' @export
scatterplot2.JGR = function(my.data, subset1.name, subset1.val, subset2.name, subset2.val, x, y, main="", xlab="", ylab="",
                             pch=1, col="black", cex.main=1, cex.lab=1, cex=1, cex.axis=1, add.lm=FALSE, add.loess=FALSE, add.conf=FALSE, add.pred=FALSE,
                             conf=.95)
{
	##draws a scatterplot using the JGR dialog box ui input
	
	##my.data		data.frame
	##subset1.name	column name of the first subsetting variable
	##subset1.val	values of the first subsetting variable
	##subset2.name	column name of the second subsetting variable
	##subset2.val	values of the second subsetting variable
	##x				column name containing the x-coordinates
	##y				column name containing the y-coordinates
	##main			plot title
	##xlab			x-axis label
	##ylab			y-axis label
	##pch			plotting character
	##col			plotting color for the data points
	##cex.main		magnification of the plot title
	##cex.lab		magnification of the axes labels
	##cex			magnification of the plotted points and lines
	##cex.axis		magnification of the axes
	##add.lm			indicator to overlay linear regression line
	##add.loess		indicator to overlay loess smooth
	
	#no longer storing results or placing results in a browser window - DAB, 01/07/2010
	
	## Get place to store results
	#resultLocation = genResultSpace()
	
	##NA handling
	iNA = is.na(my.data[,x]) | is.na(my.data[,y])
	my.data = my.data[!iNA,]
	
	##find proper data subset
	n.val1 = length(subset1.val)
	n.val2 = length(subset2.val)
	
	#opening a plot window for each plot - DAB, 01/07/2010
	##set the plotting grid
	#n.panels = max(n.val1*n.val2, n.val1, n.val2, 1)
	#nCol = ceiling(sqrt(n.panels))
	#nRow = ceiling(n.panels/nCol)
	##par(mfrow=c(nRow,nCol))
  
	##draw the scatterplot(s)
	
	## collect points to be drawn on a single scatterplot
	xValues = c()
	yValues = c()
	
	if (n.val1>0 & n.val2>0) {
		for (i in 1:n.val1)
		{
			for (j in 1:n.val2)
			{
				ind = my.data[,subset1.name]%in%subset1.val[i] & my.data[,subset2.name]%in%subset2.val[j]
        xValues = c(xValues, my.data[ind, x])
        yValues = c(yValues, my.data[ind, y])
        
				#png(file=file.path(resultLocation, paste(subset1.val[i]," ",subset2.val[j],".png",sep="")), width=600, height=600)
#				scatterplot2(my.data[ind, x], my.data[ind, y], xName=x, yName=y, main=paste(subset1.val[i], subset2.val[j], main), xlab=xlab, ylab=ylab, pch=pch, col=col,
#								cex.main=cex.main, cex.lab=cex.lab, cex=cex, cex.axis=cex.axis, add.lm=add.lm, add.loess=add.loess, add.conf=add.conf, add.pred=add.pred, conf=conf)
				#dev.off()
			}
		}
	} else if (n.val1 > 0) {
		for (i in 1:n.val1)
		{
			ind = my.data[,subset1.name] %in% subset1.val[i]

			xValues = c(xValues, my.data[ind, x])
			yValues = c(yValues, my.data[ind, y])
			
			#png(file=file.path(resultLocation, paste(subset1.val[i],".png",sep="")), width=600,height=600)
#			scatterplot2(my.data[ind, x], my.data[ind, y], xName=x, yName=y, main=paste(subset1.val[i], main), xlab=xlab, ylab=ylab, pch=pch, col=col,
#							cex.main=cex.main, cex.lab=cex.lab, cex=cex, cex.axis=cex.axis, add.lm=add.lm, add.loess=add.loess, add.conf=add.conf, add.pred=add.pred, conf=conf)
			#dev.off()
      }
	} else if (n.val2 > 0) {        
		for (i in 1:n.val2)
		{
			ind = my.data[,subset2.name] %in% subset2.val[i]
			#png(file=file.path(resultLocation, paste(subset2.val[i],".png",sep="")), width=600,height=600)
			
			xValues = c(xValues, my.data[ind, x])
			yValues = c(yValues, my.data[ind, y])			
#			scatterplot2(my.data[ind, x], my.data[ind, y], xName=x, yName=y, main=paste(subset2.val[i], main), xlab=xlab, ylab=ylab, pch=pch, col=col,
#							cex.main=cex.main, cex.lab=cex.lab, cex=cex, cex.axis=cex.axis, add.lm=add.lm, add.loess=add.loess, add.conf=add.conf, add.pred=add.pred, conf=conf)
			#dev.off()
		}
	} else {
		#png(file=file.path(resultLocation, "Scatterplot.png"), width=600,height=600)
	  xValues=my.data[,x]
	  yValues=my.data[,y]
#		scatterplot2(my.data[,x], my.data[,y], xName=x, yName=y, main=main, xlab=xlab, ylab=ylab, pch=pch, col=col,
#						cex.main=cex.main, cex.lab=cex.lab, cex=cex, cex.axis=cex.axis, add.lm=add.lm, add.loess=add.loess, add.conf=add.conf, add.pred=add.pred, conf=conf)
		#dev.off()
	}

		scatterplot2(xValues, yValues, xName=x, yName=y, main=main, xlab=xlab, ylab=ylab, pch=pch, col=col,
	             cex.main=cex.main, cex.lab=cex.lab, cex=cex, cex.axis=cex.axis, add.lm=add.lm, add.loess=add.loess, add.conf=add.conf, add.pred=add.pred, conf=conf)	
	
  
#buildresultsXML(title="Scatterplots",location=resultLocation)
}

