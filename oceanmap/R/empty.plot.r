empty.plot <- function(...,xlab="",ylab="",new=T,add=!new,n=1,axes=F){
  for(i in 1:n){
    if(!new)par(new=T)
    plot(1,lwd=0,xlab=xlab,ylab=ylab,axes=F,...)
    if(axes){
      axis(1,lwd=0,lwd.ticks=1)
      axis(2,lwd=0,lwd.ticks=1)
      box()
    }
  }
} # create empty plot
