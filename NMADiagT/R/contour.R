#' @importFrom ks Hscv kde
#' @import ggplot2
#' @import reshape2
#' @import grDevices
#' @import imguR
contour=function(samp.gen,K,testname,dirc){
  post.se<-list()
  post.sp<-list()
  fpr<-list()
  mv<-list()
  plot<-list()
  pdf(file.path(dirc,"contour_hsroc.pdf"))
  for(i in 1:K){
    post.se[[i]] <- as.numeric(unlist(samp.gen[,paste("post.se[",i,"]",sep="")]))
    post.sp[[i]] <- as.numeric(unlist(samp.gen[,paste("post.sp[",i,"]",sep="")]))
    fpr[[i]] <- 1-post.sp[[i]]
    mv[[i]] <- data.frame(x=fpr[[i]],y=post.se[[i]])
    hscv1 <- Hscv(mv[[i]])
    fhat1 <- ks::kde(mv[[i]], H=hscv1, compute.cont=TRUE)
    dimnames(fhat1[['estimate']]) <- list(fhat1[["eval.points"]][[1]],
                                          fhat1[["eval.points"]][[2]])
    aa <- melt(fhat1[['estimate']])
    plot[[i]]<-ggplot(aa, aes_string(x='Var1', y='Var2')) +
      geom_tile(aes_string(fill='value')) +
      geom_contour(aes_string(z='value'), breaks=fhat1[["cont"]]["75%"]) +
      geom_contour(aes_string(z='value'), breaks=fhat1[["cont"]]["50%"]) +
      geom_contour(aes_string(z='value'), breaks=fhat1[["cont"]]["25%"]) +
      geom_contour(aes_string(z='value'), breaks=fhat1[["cont"]]["10%"]) +
      geom_contour(aes_string(z='value'), breaks=fhat1[["cont"]]["5%"]) +
      ggtitle(paste(testname[i])) +xlab("False positive rate") + ylab("True positive rate")
    print(plot[[i]])
  }
  dev.off()

}
