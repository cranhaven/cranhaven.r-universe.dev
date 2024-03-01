scatterplot2 = function(x, y, xName="x", yName="y", main="", xlab="", ylab="", pch=1,
                          col="black", cex.main=1, cex.lab=1, cex=1, cex.axis=1,
                          add.lm=FALSE, add.loess=FALSE, add.conf=FALSE,
  add.pred=FALSE, conf=.95)
{
  JavaGD(name="scatterplot", width=500, height=400, ps=14)

  if( !add.lm & !add.loess ){
    
    ## Draw just a scatterplot of (x,y)
    plot(x, y, main=main, xlab=xlab, ylab=ylab, pch=pch, col=col,
         cex.main=cex.main, cex.lab=cex.lab, cex=cex, cex.axis=cex.axis)
    
  } else {
    
    ## Set up dummy x values for plotting
    xrange = range(x,na.rm=TRUE)
    yrange = range(y,na.rm=TRUE)
    newx = seq( xrange[1]-.1*diff(xrange), xrange[2]+.1*diff(xrange), length=150 )
    newdata = data.frame(x=newx)

    ## Calculate linear regression, and obtain range for y-axis
    my.fit = lm(y ~ x)
    if(add.lm){

      pred.line = predict(my.fit,newdata)
      yrange = range(c(yrange,pred.line),na.rm=TRUE)
      if(add.conf){
        conf.fit = predict(my.fit,newdata,interval="confidence",level=conf)
        yrange = range(c(yrange,conf.fit[,2:3]),na.rm=TRUE)
      }
      if(add.pred){
        pred.fit = predict(my.fit,newdata,interval="prediction",level=conf)
        yrange = range(c(yrange,pred.fit[,2:3]),na.rm=TRUE)
      }
    }
      
	
    ## Calculate loess smooth if requested, and obtain range for y-axis
    if(add.loess){
      lo.fit = loess(y~x)
      
      lo.pred.line = predict(lo.fit,newdata,se=TRUE)
      yrange = range(c(y,lo.pred.line$fit),na.rm=TRUE)
      if(add.conf){
        lo.conf.shift = lo.pred.line$se.fit * qt( (1+conf)/2, lo.pred.line$df )
        lo.conf.low = lo.pred.line$fit - lo.conf.shift
        lo.conf.high = lo.pred.line$fit + lo.conf.shift
        yrange = range(c(yrange,lo.conf.low,lo.conf.high),na.rm=TRUE)
      }
      if(add.pred){
        lo.pred.shift = sqrt( lo.pred.line$se.fit^2 + lo.pred.line$residual.scale^2 ) *
          qt( (1+conf)/2, lo.pred.line$df )
        lo.pred.low = lo.pred.line$fit - lo.pred.shift
        lo.pred.high = lo.pred.line$fit + lo.pred.shift
        yrange = range(c(yrange,lo.pred.low,lo.pred.high),na.rm=TRUE)
      } 
    }
    
    ## Add a little buffer to the y-axis limits
    yrange = yrange + c(-0.03,0.03) * diff(yrange)

    ## Draw the plot
    plot(x, y, ylim=yrange, main=main, xlab=xlab, ylab=ylab, pch=pch, col=col,
         cex.main=cex.main, cex.lab=cex.lab, cex=cex, cex.axis=cex.axis)

    ## Draw regression output
    if( add.lm ){

      lines(newx,pred.line,lty=1,col=1)
      
      ## Add confidence interval lines
      if(add.conf){
        lines(newx,conf.fit[,2],lty=2,col=3)
        lines(newx,conf.fit[,3],lty=2,col=3)
      }
      
      ## Add prediction interval lines
      if(add.pred){
        lines(newx,pred.fit[,2],lty=3,col=4)
        lines(newx,pred.fit[,3],lty=3,col=4)
      }
      
      ## Add regression info
      my.oper = ifelse(my.fit$coef[1] > 0, " + ", " - ")
      text.y = ifelse(my.fit$coef[2] > 0, 0.95*yrange[2], 1.05*yrange[1])
      text(1.05*min(x), text.y,
           paste("Linear Regression Fit:\n\n", 
                 yName, " = ", signif(my.fit$coef[2],4), "*", xName,
                 my.oper, signif(abs(my.fit$coef[1]),4), sep=""),
           pos=4, cex=1)
      
    }

    ## Draw loess output
    if( add.loess ){

      lines(newx,lo.pred.line$fit,lty=5,col=5)
      
      ## Add confidence interval lines
      if(add.conf){
        lines(newx,lo.conf.low,lty=2,col=3)
        lines(newx,lo.conf.high,lty=2,col=3)
      }
      
      ## Add prediction interval lines
      if(add.pred){
        lines(newx,lo.pred.low,lty=3,col=4)
        lines(newx,lo.pred.high,lty=3,col=4)
      }
      
    }

    ## Add line legend
    if( add.lm | add.loess ){
      loc = ifelse(my.fit$coef[2] > 0, "bottomright", "topright")
      my.legend = character()
      my.lty = my.col = numeric()
      if(add.lm){
        my.legend=c(my.legend,"Linear Regression")
        my.lty = c(my.lty,1)
        my.col = c(my.col,1)
      }
      if(add.loess){
        my.legend=c(my.legend,"Loess")
        my.lty = c(my.lty,5)
        my.col = c(my.col,5)
      }
      if(add.conf){
        my.legend=c(my.legend,"Confidence Interval")
        my.lty = c(my.lty,2)
        my.col = c(my.col,3)
      }
      if(add.pred){
        my.legend=c(my.legend,"Prediction Interval")
        my.lty = c(my.lty,3)
        my.col = c(my.col,4)
      }
    
      legend(loc, legend=my.legend, lty=my.lty, col=my.col)
    }
  }
  
  return()
}
