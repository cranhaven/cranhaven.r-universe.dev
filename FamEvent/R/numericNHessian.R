numericNHessian <- function (f, t0, eps = 1e-06, ...) 
{ 
    f00 <- f(t0, ...)
    eps2 <- eps * eps
            t01 <- t0
            t10 <- t0
            t11 <- t0
            t01 <- t01 + eps
            t10 <- t10 + eps
            t11 <- t11 + eps
            t11 <- t11 + eps
            f01 <- f(t01, ...)
            f10 <- f(t10, ...)
            f11 <- f(t11, ...)
            H <- (f11 - f01 - f10 + f00)/eps2
       
    return(H)
}
