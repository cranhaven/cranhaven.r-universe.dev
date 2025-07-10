#' @import grDevices
#' @import imguR
density.hsroc=function(samp.gen, testname,K, n.chains, dirc){
  cat("Start saving posterior density plots...\n")
  mcmcSe <- NULL
  dens.Se <- matrix(0, K, 3)
  colnames(dens.Se) <- c("ymax", "xmin", "xmax")
  mcmcSp <- NULL
  dens.Sp <- matrix(0, K, 3)
  colnames(dens.Sp) <- c("ymax", "xmin", "xmax")
  for (i in 1:K) {
    tempSe <- NULL
    tempSp <- NULL
    for (j in 1:n.chains) {
      tempSe <- c(tempSe, as.vector(samp.gen[[j]][, paste("post.se[", i, "]", sep = "")]))
      tempSp <- c(tempSp, as.vector(samp.gen[[j]][, paste("post.sp[", i, "]", sep = "")]))
    }
    mcmcSe[[i]] <- tempSe
    tempdens.Se <- density(tempSe)
    dens.Se[i, ] <- c(max(tempdens.Se$y), quantile(tempSe, 0.001), quantile(tempSe, 0.999))
    mcmcSp[[i]] <- tempSp
    tempdens.Sp <- density(tempSp)
    dens.Sp[i, ] <- c(max(tempdens.Sp$y), quantile(tempSp, 0.001), quantile(tempSp, 0.999))
  }
  ymaxSe <- max(dens.Se[, "ymax"])
  xminSe <- min(dens.Se[, "xmin"])
  xmaxSe <- max(dens.Se[, "xmax"])
  ymaxSp <- max(dens.Sp[, "ymax"])
  xminSp <- min(dens.Sp[, "xmin"])
  xmaxSp <- max(dens.Sp[, "xmax"])
  pdf(file.path(dirc,"density_hsroc.pdf"))
  oldpar<-par(mfrow = c(K, 2), mar = c(5.5, 5.5, 2, 2) + 0.1)
  on.exit(par(oldpar))
  for(i in 1:K){
    plot(density(mcmcSe[[1]]), xlim = c(xminSe, xmaxSe), ylim = c(0,
         ymaxSe), xlab = paste(testname[i], " Se(%)"), ylab = "Density",
         main = "", lty = 1, lwd = 2, cex.axis = 2,
       cex.lab = 2)
    plot(density(mcmcSp[[1]]), xlim = c(xminSp, xmaxSp), ylim = c(0,
         ymaxSp), xlab = paste(testname[i], " Sp(%)"), ylab = "Density",
         main = "",  lty = 1, lwd = 2, cex.axis = 2,
         cex.lab = 2)
  }
  dev.off()
}
