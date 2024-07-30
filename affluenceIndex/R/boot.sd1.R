boot.sd1 <-
function(x, weight, kp, nsim, boot.index=c("r.hc", "r.is"), gamma){
  if (boot.index == "r.is"){
    r.is1 <- r.is(x, weight, kp)
    
    n <- length(x)
    Rbb <- NULL
    for(i in 1:nsim){
      s <- sample(1:n, n, replace=T)
      ss <- x[s]
      wss <- weight[s] 
      rownames(ss) <- NULL
      
      R <- r.is(ss, wss, kp)
      Rb <- R
      Rbb <- rbind(Rb,Rbb)
    }
    r.se <- sd(Rbb)
    ci1_up <- r.is1 + qnorm(gamma)*r.se
    ci1_low <- r.is1 - qnorm(gamma)*r.se
    ci2_up <- quantile(Rbb,1-(1-gamma)/2)
    ci2_low <- quantile(Rbb,(1-gamma)/2)
    
    tab <- rbind(c(ci1_low, r.is1, ci1_up), c(ci2_low, r.is1, ci2_up))
    colnames(tab) <- c("ci.low", "r.is", "ci.up")
    rownames(tab) <- c("norm", "quantile")
    outlist <- list(se.r.is = r.se, summary = tab, boot.ind = as.vector(Rbb))
  }
  
  if (boot.index == "r.hc"){
    r.hc1 <- r.hc(x, weight, kp)$r.hc
    
    n <- length(x)
    Rbb <- NULL
    for(i in 1:nsim){
      s <- sample(1:n, n, replace = T)
      ss <- x[s]
      wss <- weight[s] 
      rownames(ss) <- NULL
      
      R <- r.hc(ss, wss, kp)$r.hc
      Rb <- R
      Rbb <- rbind(Rb,Rbb)
    }
    
    r.se <- sd(Rbb)
    ci1_up <- r.hc1 + qnorm(gamma)*r.se
    ci1_low <- r.hc1 - qnorm(gamma)*r.se
    ci2_up <- quantile(Rbb,1-(1-gamma)/2)
    ci2_low <- quantile(Rbb,(1-gamma)/2)
    
    tab <- rbind(c(ci1_low, r.hc1, ci1_up), c(ci2_low, r.hc1, ci2_up))
    colnames(tab) <- c("ci.low", "r.hc", "ci.up")
    rownames(tab) <- c("norm", "quantile")
    outlist <- list(se.r.hc = r.se, summary = tab, boot.ind = as.vector(Rbb))
  }
  
  return(outlist)
}
