objective <- function(x, coord, val)
  sum((sqrt(rowSums(sweep(coord, 2, x, "-")**2)) - val)**2)

disttwoclasses <- function(ltik, prop=c(1, 1)){
  lu <- ltik[,1] - ltik[,2] + log(prop[2]/prop[1])
  sqrt((1 + sqrt(1 + mean(lu**2))) * 2)
}

smartInit <- function(logtik.estim, prop){
  dist <- t(sapply(1:(ncol(logtik.estim)-1),
                   function(j1, logtik.estim){
                     c(rep(0, j1), sapply(as.list((j1+1):ncol(logtik.estim)),
                                          function(logtik.estim, j1, j2){
                                            disttwoclasses(logtik.estim[,c(j1,j2)], prop[c(j1,j2)])                                          },
                                          logtik.estim=logtik.estim,
                                          j1=j1))
                   },
                   logtik.estim=logtik.estim))
  dist <- rbind(dist, rep(0, ncol(dist)))
  dist <- dist + t(dist)
  K <- ncol(dist)
  mu <- matrix(0, K, K-1)
  mu[1,1] <- (dist[1,K])
  if (K > 2){
    for (j in 2:(K-1))
      mu[j,1:j] <- optim(rep(1,j), objective, coord=mu[c(1:(j-1), K), 1:j], val=dist[j,c(1:(j-1),K)])$par
  }
  mu <- mu[-K,]
  mu[lower.tri(mu, diag=TRUE)]
}
