qops <- function(blk,p,w,f,options,u){
  if(options >= 1 & options <= 4){
    Fu <- mexqops(blk[[p,2]],w,f,options)
  }else if(options == 5){
    s <- 1 + c(0, cumsum(blk[[p,2]]))
    idx1 <- s[1:length(blk[[p,2]])]
    inprod <- mexqops(blk[[p,2]],f,u,1)
    tmp <- (u[idx1] + inprod)/(1+f[idx1])
    Fu <- u + mexqops(blk[[p,2]],tmp,f,3)
    Fu[idx1] <- inprod
    Fu <- mexqops(blk[[p,2]],w,Fu,3)
  }else if(options == 6){
    s <- 1 + c(0, cumsum(blk[[p,2]]))
    idx1 <- s[1:length(blk[[p,2]])]
    gamprod <- mexqops(blk[[p,2]],f,u,2)
    tmp <- (u[idx1] + gamprod)/(1+f[idx1])
    Fu <- u - mexqops(blk[[p,2]],tmp,f,3)
    Fu[idx1] <- gamprod
    Fu <- mexqops(blk[[p,2]],1/w,Fu,3)
  }
  return(Fu)
}