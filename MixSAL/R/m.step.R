m.step <-
function(x,G,exp.vals,prmtrs){
  # p <- prmtrs$p
  for(g in 1:G){
    zig.hat <- exp.vals$zig.hat[g,]
    w.i <- exp.vals$ew[,1,g]; w.inv <- exp.vals$ew[,2,g];
    A <- sum(w.i*zig.hat);
    B <- sum(w.inv*zig.hat);
    B.x <- apply(x,2,weighted.sum,wt=zig.hat*w.inv);
    Z.x <- apply(x,2,weighted.sum,wt=zig.hat);
    n.g <- sum(zig.hat);
    pi.g <- mean(zig.hat);
    sA <- A/n.g;
    sB <- B/n.g;
    sB.x <- B.x/n.g;
    sZ.x <- Z.x/n.g;
    denom <- (sA*sB - 1);
    prmtrs$alpha[g,] <- (sB*sZ.x - sB.x)/denom;
    prmtrs$talpha[,g] <- prmtrs$alpha[g,]
    prmtrs$mu[g,] <- (sA*sB.x - sZ.x)/denom;
    S <- cov.wt(x=x, wt=zig.hat*w.inv, center=prmtrs$mu[g,], method="ML")$cov*sB;
    r <- as.numeric(sZ.x - prmtrs$mu[g,]);
    a1 <- as.numeric(prmtrs$alpha[g,]);
    prmtrs$sig[,,g] <- S - (outer(a1, r) + outer(r, a1)) + outer(a1,a1)*sA;
    prmtrs$inv.sig[,,g] <- solve(prmtrs$sig[,,g])
    prmtrs$log.det.sig[g] <- log(det(prmtrs$sig[,,g]))
  }
  return(prmtrs);
}
