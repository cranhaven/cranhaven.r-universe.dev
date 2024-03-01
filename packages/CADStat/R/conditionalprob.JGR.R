#' @export
conditionalprob.JGR = function(my.data, x, y, weights=NULL,
                                cond.val,cond.val.direction,prob.direction,
                                alpha=0.05,R=100,
                                subset1.name=NULL,subset1.val=NULL, 
                                subset2.name=NULL,subset2.val=NULL,
                                main="Conditional Probability Plot", xlab="", ylab="",
                                pch=20, col="black", cex.main=1, cex.lab=1,
                                cex=1, cex.axis=1, browserResults=FALSE, ...)
{

### CADStat Conditional probability function
### modeled after:
### Hollister, J.W., H.A. Walker, J.F. Paul. (In Press) 
###   CProb: A Computational Tool for Conducting Conditional Probability Analysis. 
###   Journal of Environmental Quality
###  Testing the function
###  embed=runif(50,0,100)
###  ept=as.integer(30-(embed+rnorm(50,0,10))/4)
###  test.data=data.frame(cbind(ept,embed))
###  conditionalprob.JGR(my.data=condprob.data,x="percfines",y="epttaxa",weights="Weights",cond.val=9,cond.val.direction="lt",prob.direction="gte",xlab="% embedeness")

### computes and plots conditional probabilities using the JGR dialog box gui input

### my.data      data.frame
### x            column name containing the stressor
### y            column name containing the response
### cond.val     conditioning impairment value
### cond.val.direction le or gt the impairment value
### prob.direction 
### main         plot title
### xlab         x-axis label
### ylab         y-axis label
### pch          plotting character
### col          plotting color for the data points
### cex.main     magnification of the plot title
### cex.lab      magnification of the axes labels
### cex          magnification of the plotted points and lines
### cex.axis     magnification of the axes
### ...          optional paramters passed to "plot"

  #get place to store results
  if (browserResults) resultLocation = genResultSpace()

  #data subsetting
  my.data = gisdt.subset(my.data, 
                         subset1.name=subset1.name, subset1.val=subset1.val, 
                         subset2.name=subset2.name, subset2.val=subset2.val,
                         na.check = c(x,y,weights))

  n = nrow(my.data)

  prob.direction.logical = regexpr("g|>",prob.direction)>0
  xunique = sort(unique(my.data[,x]), decreasing = !prob.direction.logical)
  
  ord = order(my.data[,x],my.data[,y],decreasing=!prob.direction.logical)  
  my.data = my.data[ord,]
  ord = order(my.data[,x])
  if(is.null(weights)) {
    weights = "weights"
    my.data$weights = 1
  }
  condprob = condprob.fn(response=my.data[,y],wts=my.data[,weights],
                         cond.val=cond.val,cond.val.direction=cond.val.direction,
                          stressor=my.data[,x], xunique = xunique,
                          p.direct = prob.direction.logical)

  if (! is.null(R)) {
#  if(!is.null(R) & !(coef(lm(condprob~xunique))[[2]]<0 &
#                     !prob.direction.logical) ){
#    condprod.boot =  matrix(NA,R, n)
    condprod.boot =  matrix(NA,R,length(xunique))
    for(r in 1:R){
      isamp = sample(1:nrow(my.data), replace = TRUE, prob=my.data[, weights])
      my.data.resamp = my.data[isamp, ]
#      sampler  = resample(c(0,1),n,replace=TRUE)
#      fudge    = as.numeric(any(my.data[,y]==0))
#      response = condprob.impute((my.data[,y]+fudge)*sampler,c(1:n),
#                                  prob.direction.logical)
#      condprod.boot[r,] = condprob.fn(response=(response-fudge),
#                                       wts=my.data[,weights],ord=ord,
#                                       cond.val=cond.val,
#                                       cond.val.direction=cond.val.direction)
      condprod.boot[r,] = condprob.fn(response=my.data.resamp[, y],
                                       wts=my.data.resamp[,weights],
                                       cond.val=cond.val,
                                       cond.val.direction=cond.val.direction,
                                       stressor = my.data.resamp[,x],
                                       xunique = xunique,
                                       p.direct = prob.direction.logical)
    }
  }

  if(browserResults){
    # DELETE THESE 3 CATS WHEN DONE TESTING.
    #cat(browserResults,"\n")
    #cat(resultLocation,"\n")
    #cat(file.path(resultLocation,paste(main,".png",sep="")),"\n")    
    png(filename=file.path(resultLocation,paste(main,".png",sep="")),width=600,height=600)
  } else {
    JavaGD(name="Conditional Probability Plot", width=600, height=500, ps=14)
    #par(mfrow=c(1,3))
    #plot(my.data[,x],my.data[,y],xlab=xlab, ylab=ylab, pch=pch, col=col,
    #    main="Scatterplot",cex.lab=cex.lab, cex=cex, cex.axis=cex.axis,las=1)
    #plot(my.data[,x],my.data[,y],xlab=xlab, ylab=ylab, pch=pch, col=col,
    #    main="Cumulative Distribution Function",cex.lab=cex.lab, cex=cex, cex.axis=cex.axis,las=1)
  }
  par(mar=c(5,6,4,2)+0.1)
  plot(xunique,condprob,ylim=c(0,1),
        ylab=paste("Probability of",ylab,cond.val.direction,cond.val,"if X",prob.direction,"Xc"),
        xlab=paste("Xc,",xlab),pch=pch, col=col,
        main=main,cex.lab=cex.lab, cex=cex, cex.axis=cex.axis,las=1)
  if (! is.null(R)) {
#  if(!is.null(R) & !(!(coef(lm(condprob~xunique))[[2]]>0) &
#                     !prob.direction.logical) ){
    points(xunique,(apply(condprod.boot,2,quantile,1-as.numeric(alpha),
                          na.rm = TRUE)),col="grey",type="l")
    points(xunique,(apply(condprod.boot,2,quantile,as.numeric(alpha),
                          na.rm = TRUE)),col="grey",type="l")
  }
#  if( !is.null(R) & coef(lm(condprob~xunique))[[2]]<0 & !prob.direction.logical ){
#    par(lheight=1.5)
#    text(median(xunique),y=0.2,cex=1.25,pos=4,"Confidence intervals are not calculated for\nProbability Direction of '<=' and a decreasing\nstressor-response relationship.")
#  }
  grid()

  if(browserResults){
    dev.off()
    page.text = paste("The conditional probability plot gives the probability of ",y," < ", cond.val," as a function of ",x,sep="")
    buildresultsXML(title=main,location=resultLocation,text=page.text)
  }
  return(invisible())
}

condprob.fn = function(response,wts,cond.val,cond.val.direction,
                        stressor, xunique, p.direct){
  n = length(response)
  nunique = length(xunique)
  Num   = numeric(nunique)
  Denom = numeric(nunique)
  if(regexpr("l|<",cond.val.direction)>0) {
    w.expr = expression(wts[incvec][response[incvec] < cond.val])
  } else {
    w.expr = expression(wts[incvec][response[incvec] > cond.val])
  }
  if(is.null(wts)) wts = rep(1,n)
  if (p.direct) {
    for (i in 1:nunique) {
      incvec = stressor >= xunique[i]
      Num[i] = sum(eval(w.expr))
      Denom[i] = sum(wts[incvec])
    }
  }
  else {
    for (i in 1:nunique) {
      incvec = stressor <= xunique[i]
      Num[i] = sum(eval(w.expr))
      Denom[i] = sum(wts[incvec])
    }
  }
  Num/Denom
}
    

#  if(regexpr("l|<",cond.val.direction)>0) {
#    w.expr = expression(wts[i:n][response[i:n] < cond.val])
#  } else {
#    w.expr = expression(wts[i:n][response[i:n] > cond.val])
#  }
  
#  Num   = numeric(n)
#  Denom = numeric(n)

#  for(i in ord){
#    Num[i]   = sum(eval(w.expr))
#    Denom[i] = sum(wts[i:n])
#  }
#  Num/Denom
#}

condprob.impute = function(x,o,exceed.logical){
  ord      = order(o,decreasing=exceed.logical)
  response = x[order(o,decreasing=exceed.logical)]
  response.nonzero = response[response!=0]
  ord.nonzero      = ord[response!=0]
  n = length(response.nonzero)
  for(i in 1:n){
    response[ord > ord.nonzero[i] & response==0] = response.nonzero[i]
  }
  response[ord < ord.nonzero[n] & response==0] = response.nonzero[n]
  response[order(o,decreasing=exceed.logical)]
}

resample  =  function(x, size, ...)
  if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x
} else sample(x, size, ...)


