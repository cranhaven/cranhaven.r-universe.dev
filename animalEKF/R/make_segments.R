
make_segments <- function(xy, N=nrow(xy)) {

   df <- ggplot2::fortify(as.data.frame(xy))
   df$Xend<- 1; df$Yend <- 1
   df$Xend[1:(N-1)] <- df$X[2:N]
   df$Yend[1:(N-1)] <- df$Y[2:N]
   df <- df[1:(N-1),]

   df
}