getHDmembers <-
function (data, maxrows = 10000, radius = NULL) 
{
    data <- as.matrix(data)
    n <- nrow(data)
    p <- ncol(data)
    if (is.null(radius)) 
        radius <- 0.1/(log(n)^(1/p))
    if (n <= maxrows) {
      cl <- partuniq(data)  # requires mclust
      U <- unique(cl)
      m <- length(U)
      if (m != n) {
        members <- rep(list(NULL), m)
        j <- 0
        for (u in U) {
             j <- j + 1
             members[[j]] <- which(cl == u)
        }
#       names(members) <- U
      }
      else members <- as.list(1:n)
    }
    else {
        members <- rep(list(NULL), n)
        exemplars <- 1
        members[[1]] <- 1
        for (i in 2:n) {
            KNN <- get.knnx(data = data[exemplars, , drop = F], 
                query = data[i, , drop = F], k = 1)
            m <- KNN$nn.index[1, 1]
            d <- KNN$nn.dist[1, 1]
            if (d < radius) {
                l <- exemplars[m]
                members[[l]] <- c(members[[l]], i)
                next
            }
            exemplars <- c(exemplars, i)
            members[[i]] <- i
        }
    }
    members <- members[!sapply(members, is.null)]
    exemplars <- sapply(members, function(x) x[[1]])
    names(members) <- exemplars
    members
}
