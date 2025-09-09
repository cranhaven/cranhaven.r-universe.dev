#' @title Px
#' @description Px
#' @param p1 probability distribution
#' @param p0 probability distribution
#' @param dbg boolean to compute sigma
#' @return Px
#' @export
Px<-function(p1,p0,dbg=FALSE){
  px1 <- minusHp <- NA
  i0 <- which(p1==0)
  if(length(i0)>0){
    p  <- p0[i0]
    px1 <- minusHp <- 0
    for(i in seq_along(p)){
      minusHp <- p$accum1 + p[i]*log10(p[i]) 
      px1     <- px1 + p[i] 
    }
  }
  if(!dbg){
   return(c(px_1=px1))
  }else{
    return(c(px_1=px1,
             KL_nonxalleles=sum(p1[-i0]*log10(p1[-i0]/p0[-i0])),
             H_xalleles = -minusHp))
  }
}