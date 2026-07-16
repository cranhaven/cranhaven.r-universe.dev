dataTrans <-
function (data) 
{
    if (any(is.na(data))) 
        stop("missing values not allowed")
    if (is.null(dim(data))) {
        CAT <- !is.numeric(data)
        data <- if (CAT) 
            as.data.frame(data)
        else as.matrix(data)
    }
    else {
        if (inherits(data, "matrix")) 
            data <- as.data.frame(data)
        CAT <- sapply(data, function(x) !is.numeric(x))
    }
# look for constant variables 
    cte <- apply( data, 2, function(x) length(unique(x)) == 1)
    if (all(cte)) {
      warning("all columns have a constant values")
      return(NULL)
    }
    else if (any(cte)) warning("some columns have a constant values")
    data <- data[,!cte,drop=F]
    CAT <- CAT[!cte]
    if (any(CAT)) {
        data[, CAT] <- sapply(data[, CAT, drop = F], function(x) 
             MCA(as.matrix(x), ncp = 1, graph = F)$ind$coord)
    }
    unitize <- function(z) {
        zrange <- range(z)
        if (!(dif <- diff(zrange))) 
            return(rep(0, length(z)))
        (z - zrange[1])/dif
    }
    apply(as.matrix(data), 2, unitize)
}
