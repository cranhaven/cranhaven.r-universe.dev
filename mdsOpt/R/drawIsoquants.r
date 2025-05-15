drawIsoquants<-function(x,y=NULL,number=6,steps=NULL){
  ile=number   
  if(is.null(steps) && is.null(y)){
    stop("neither x or steps cann not be empty") 
  }
  if(is.null(steps)){
    d<-rbind(x,y)
    steps<-dist(d)[1]/number
  }
  if(length(steps)==1){
    steps<-rep(steps,number);
  }else
  if(length(steps)<number){
    steps<-c(steps,rep(steps[length(steps)-1],number-length(steps)));
  }
  print(steps)
  for(i in 1:number){
    draw.circle(x[1],x[2],sum(steps[1:i]))
  }
}


.plot.s<-function(x,adjX=0.1,adjY=2.5,cex = 0.7) 
{
    spp.perc <- sort(x$spp, decreasing = TRUE)
    xaxlab <- names(spp.perc)
    op <- par(mar = c(3, 4, 4, 2))
    on.exit(par(op))
    text(1:length(spp.perc)+adjX, spp.perc+adjY, labels = xaxlab, pos = 3, 
         cex=cex,srt=90)
    for (i in 1:length(spp.perc)) lines(c(i, i), c(spp.perc[i], 
                                                   0), col = "lightgray", lty = 2)
}
