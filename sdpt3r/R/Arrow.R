Arrow <- function(blk,p,f,x,options=0){
  
  s <- 1 + c(0, cumsum(blk[[p,2]]))
  idx1 <- s[1:max(dim(as.matrix(blk[[p,2]])))]
  if(options == 0){
    inprod <- mexqops(blk[[p,2]],f,x,1)
    Fx <- mexqops(blk[[p,2]],f[idx1],x,3) + mexqops(blk[[p,2]], x[idx1],f,3)
    Fx[idx1] <- inprod
  }else{
    gamf2 <- mexqops(blk[[p,2]],f,f,2)
    gamprod <- mexqops(blk[[p,2]],f,x,2)
    alpha <- gamprod/gamf2
    Fx <- mexqops(blk[[p,2]],1/f[idx1],x,3) - mexqops(blk[[p,2]],alpha/f[idx1],f,3)
    Fx[idx1] <- alpha
  }
  return(Fx)
  
}