plotHDoutliers <-
function( data, indexes=NULL, transform = TRUE, ...) {

    data <- if (transform) dataTrans(data) else as.matrix(data)

    if (any(is.na(data))) stop("missing values not allowed")

    type <- "outliers"
    if (type == "exemplars") indexes <- unlist(lapply( indexes, 
                                                       function(data) data[1]))
    if (ncol(data) == 1) {
      plot( data, rep(0,length(data)),ylim=c(-1,1),type="n",ylab="",yaxt="n",...)
      if (is.null(indexes)) {
        points( data, rep(0,length(data)), pch=1, col = "dodgerblue", cex = 2)
      }
      else {
        good <- setdiff(1:length(data),indexes)
        print(good)
        points( data[good], rep(0,length(data[good])), pch=1, 
                col = "dodgerblue", cex = 2)
        switch( type[1],
           "outliers" = points( data[indexes], rep(0,length(data[indexes])), pch="|",                 col = "black", cex = 2),
           "exemplars" = points( data[indexes], rep(0,length(data[indexes])), pch=1, 
                col = "black", cex = 2),
           "members" = {l <- length(indexes); colors <- rainbow(l);
                for (m in 1:l) {
                points( data[indexes[[m]]], rep(0,length(data[indexes])), pch=1, 
                col = colors[m], cex = 2)
                }},
              stop("type not recognized")
             )
      }
     }
 else {
      if (ncol(data) == 2) {
        z <- data
      }
      else {
        if (is.null(indexes)) {
          z <- as.matrix(data) %*% svd( data, nv=ncol(data))$v[,1:2]
        }
        else {
          z <- as.matrix(data) %*% svd( data[-indexes,,drop=F], nv=ncol(data))$v[,1:2]
        }
      }
      plot( z[,1], z[,2], type="n", ...)
      if (is.null(indexes)) {
        points( z[,1], z[,2], pch=16, col = "dodgerblue", cex = 1)
      }
      else {
        good <- setdiff(1:nrow(data),indexes)
        points( z[good,1], z[good,2], pch=16, 
                col = "dodgerblue", cex = 1)
        switch( type[1],
                "outliers" = points( z[indexes,1], z[indexes,2], pch="*", 
                                     col = "black", cex = 2),
                "exemplars" = points( z[indexes,1], z[indexes,2], pch=16, 
                                     col = "black", cex = 1),
               "members" = {l <- length(indexes); colors <- rainbow(l);
                            for (m in 1:l) {
                               k <- indexes[[m]]
                               points(z[k,1],z[k,2],pch=16,col=colors[m],cex=1)
                           }},
                stop("type not recognized")
             )
      }

  }


 invisible()
}
