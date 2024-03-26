nn_flexible = function(W = W, nv = nv){
  # sf::sf_use_s2(FALSE)
  # W <- poly2nb(sf, queen = FALSE)
  fl <- NULL
  fl.list <- list()
  for (f in 1:length(W)){
    if (W[[f]][1] != 0){
      nvi <- length(W[[f]])
      m <- cbind(rep(f,nvi),W[[f]])
      for (k in 1:(nv-2)){
        g = 0
        b2.list <- list()
        for (j in 1:dim(m)[1]){
          g = g + 1
          m1 <- W[[m[j,dim(m)[2]]]]
          a <- m1[!m1 %in% m[j,]]
          la <- length(a)
          if (la > 0){
            b2.list[[g]] <- do.call("cbind",list(matrix(rep(m[j,],la), nrow = la, byrow=TRUE),a))
          }
        }
        m <- do.call("rbind", b2.list)
      }
      fl.list[[f]] <- m
      if ((f %% 10) == 0) {
        # time.taken <- Sys.time() - start.time
        # cat("time for 10 ",time.taken,"\n")
        # start.time <- Sys.time()
        cat("+")}
    }
  }
  fl <- do.call("rbind", fl.list)
  cat("end")
  return(fl)
}
