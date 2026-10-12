set.back <-
function(x,g,exp.vals,prmtrs){
  
  zig.hat <- exp.vals$zig.hat[g,]; w.i <- exp.vals$ew[,1,g]; p <- prmtrs$p 
  A <- sum(w.i*zig.hat);
  Z.x <- apply(x,2,weighted.sum,wt=zig.hat);
  n.g <- sum(zig.hat);
  alpha <- (Z.x - n.g*(prmtrs$mu[g,]))/A
  return(alpha)
}
