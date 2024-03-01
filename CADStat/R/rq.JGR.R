#' @export
rq.JGR = function(my.data, subset1.name, subset1.val, subset2.name, subset2.val,
          my.formula, my.tau=0.5, iCI=FALSE, conf.level=0.95, iScatterplot=FALSE, iCIplot=FALSE,
          dep.var, indep.var, browserResults=FALSE)
{
  # fits a quantile regression model returning estimates, confidence intervals, and plots
  # requires quantreg package
  
  # my.data    data.frame
  # subset1.name  column name of the first subsetting variable (panel)
  # subset1.val  values of the first subsetting variable
  # subset2.name  column name of the second subsetting variable (group)
  # subset2.val  values of the second subsetting variable
  # my.formula    string giving the model formula
  # my.tau      numeric vector of quantiles to fit.
  # iCI      indicates whether confidence intervals for model coefficients should be computed
  # conf.level    confidence level
  # iScatterplot  indicates whether a regression scatterplot should be created (only if 1 independent variable)
  # iCIplot    indicates whether plots of coefficient confidence bands should be created.
  # dep.var    string giving the dependent variable
  # indep.var    vector of strings giving the independent variables
  
  # Get place to store results
  if (browserResults) resultLocation = genResultSpace()

  # NA handling
  # use options to set na.action to omit NAs which is used by lm
  options(na.action = "na.omit")
  my.data = gisdt.subset(my.data, 
                         subset1.name=subset1.name, subset1.val=subset1.val, 
                         subset2.name=subset2.name, subset2.val=subset2.val)
  
  # NOTE: the remainder of the code groups subset1.val values together and
  # groups subset2.val values together rather than perform computations for
  # individual subsets

  # test my.tau
  my.tau = my.tau[is.numeric(my.tau) & !is.nan(my.tau)]
  my.tau = my.tau[my.tau > 0 & my.tau < 1]
  if (length(my.tau) < 1) my.tau = 0.5

  # test conf.level
  # don't know conf.level param for rq
  if (!is.numeric(conf.level) | is.nan(conf.level)) {
    conf.level = 0.95
  } else if (conf.level <= 0 | conf.level >= 1) {
    conf.level = 0.95
  } 

  #fit the model
  my.rq = rq(as.formula(my.formula), data=my.data, tau=my.tau)
  
  #output coefficient estimates and CIs (if necessary)
  if (iCI) {
    print("Quantile Confidence Interval Tables")
    print(summary(my.rq))
  } else {
    print("Quantile Coefficient Table")
    print(coef(my.rq))
  }
  
  #build quantile regression summary
  if (length(my.tau) > 1) {
    if (iCI) {
      rq.list = lapply(summary(my.rq),
      					function(x) {
      						coef = data.frame(x$coef)
      						attr(coef,"title") = paste("Coefficients for ", x$tau, " Regression ( confidence level =", conf.level, ")")
      						coef
      					})
    } else {
      rq.list = rep(list(NULL), length(my.tau))
      for (i in 1:length(my.tau))
      {
        coef = data.frame(coefficients=coef(my.rq)[,i])
        attr(coef,"title") = paste("Coefficients for ", my.rq$tau[i]," Regression")
        rq.list[[i]] = coef
      }
    }
  } else {
    rq.list = list(1)
    if (iCI) {
      coefs =  data.frame(summary(my.rq)$coef)
      attr(coefs,"title") = paste("Coefficients for ", my.tau, " Regression ( confidence level =", conf.level, ")")
    } else {
      coefs = data.frame(coef(my.rq))
      attr(coefs,"title") = paste("Coefficients for ",my.tau," Regression")
    }
    
    rq.list[[1]] = coefs
  }

  #create scatterplot with overlays if desired
  if (iScatterplot) {
    if (browserResults) {
      png(filename=file.path(resultLocation,"Quantile Regression Scatterplot.png"), width=600, height=600)    
    } else {
	  JavaGD(width=500, height=400, ps=14)
    }
    rq.plot.JGR(indep.var, dep.var, my.data, my.rq, my.tau)
    if (browserResults) dev.off()
  }

  #create coefficient confidence band plot if desired
  if (iCIplot & length(my.tau)>1) {
    if (browserResults) {
	  png(filename=file.path(resultLocation, "Quantile Regression Confidence Bands.png"), width=600,height=600)
    } else {
	  JavaGD(width=500, height=400, ps=14)
    }
    plot(summary(my.rq))
    if (browserResults) dev.off()
  }

  #output quantile regression summary
  if (browserResults) {
    buildresultsXML(object=rq.list, location=resultLocation, title="Quantile Regression Summary")
  } else {
  	print("Quantile Regression Summary")
  	print(rq.list)
  }

  return(invisible())
}

rq.plot.JGR = function(indep.var,dep.var,my.data,my.rq,my.tau)
{
  x = my.data[,indep.var]
  y = my.data[,dep.var]
    
  plot(x, y, xlab=indep.var, ylab=dep.var, type = "n", cex=0.5, las=1)
  points(x, y, cex=0.75, col="blue")
    
  # add the mean and median fits
  my.lm = lm(y~x)
  abline(my.lm, col="darkblue", lwd = 2)

  # add quantile lines
  xx = seq(min(x,na.rm=TRUE), max(x,na.rm=TRUE), length.out=100)
  f = coef(my.rq)
  yy = cbind(1,xx)%*%f
  
  leg.text = "Mean (LSE) Fit"
  leg.col = "darkblue"
  for(i in 1:length(my.tau)) {
    lines(xx, yy[,i], col=rainbow(5)[i], lwd=2 )
    leg.text = c(leg.text,paste(my.tau[i],"Quantile Fit"))
    leg.col = c(leg.col,rainbow(5)[i])
  }
    
  # add a legend
  legend.loc = ifelse(coef(my.lm)[2] > 0, "bottomright", "topright")
  legend(legend.loc,legend=leg.text,col=leg.col,lwd=2)
}
