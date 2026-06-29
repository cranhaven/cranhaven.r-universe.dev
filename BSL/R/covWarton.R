covWarton <- function(S, gamma) {
    if (gamma < 0 || gamma > 1) {
        stop("\"gamma\" must be between 0 and 1")
    }
    ns <- ncol(S)
    D1 <- diag(diag(S)^-0.5)
    D2 <- diag(diag(S)^0.5)
    R <- gamma * D1 %*% S %*% D1 + (1 - gamma) * diag(ns)
    Sigma <- D2 %*% R %*% D2
    return (Sigma)
}


corrWarton <- function(R, gamma) {
    ns <- ncol(R)
    gamma * R + (1 - gamma) * diag(ns)
}