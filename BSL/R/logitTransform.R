paraLogitTransform <- function(theta, bound) {
    p <- length(theta)
    thetaTilde <- numeric(p)
    type <- as.character(is.infinite(bound) %*% c(1,2))
    for (i in 1:p) {
        a <- bound[i, 1]
        b <- bound[i, 2]
        x <- theta[i]
        thetaTilde[i] <- switch(type[i],
                                '0' = log((x-a)/(b-x)),
                                '1' = log(1/(b-x)),
                                '2' = log(x-a),
                                '3' = x
        )
    }
    return(thetaTilde)
}

paraLogitBackTransform <- function(thetaTilde, bound) {
    p <- length(thetaTilde)
    theta <- numeric(p)
    type <- as.character(is.infinite(bound) %*% c(1,2))
    for (i in 1:p) {
        a <- bound[i, 1]
        b <- bound[i, 2]
        y <- thetaTilde[i]
        ey <- exp(y)
        theta[i] <- switch(type[i],
                           '0' = a/(1+ey) + b/(1+1/ey),
                           '1' = b-1/ey,
                           '2' = a+ey,
                           '3' = y
        )
    }
    return(theta)
}

jacobianLogitTransform <- function(thetaTilde, bound, log = TRUE) {
    p <- length(thetaTilde)
    type <- as.character(is.infinite(bound) %*% c(1,2))
    logJ <- numeric(p)
    for (i in 1:p) {
        y <- thetaTilde[i]
        if (type[i] == '0') {
            a <- bound[i, 1]
            b <- bound[i, 2]
            ey <- exp(y)
        }
        logJ[i] <- switch(type[i],
                          '0' = log(b-a) - log(1/ey+2+ey),
                          '1' = y,
                          '2' = y,
                          '3' = 0
        )
    }
    J = sum(logJ)
    if (!log) {
        J <- exp(J)
    }    
    return(J)
}
