BiasVarTO <- function(times = 100) {
    x1 <- sort(sample(0:100, 100, replace = TRUE))
    x2 <- sort(sample(0:100, 100, replace = TRUE))
    x3 <- sort(sample(0:100, 100, replace = TRUE))
    x4 <- sort(sample(0:100, 100, replace = TRUE))
    x5 <- sort(sample(0:100, 100, replace = TRUE))
    x6 <- sort(sample(0:100, 100, replace = TRUE))
    x7 <- sort(sample(0:100, 100, replace = TRUE))
    T <- 12 + 0.1 * x1 + 0.2 * x2 + 0.3 * x3 + 0.4 * 
        x4 - 0.3 * x5 - 0.2 * x6 - 0.1 * x7
    print(T)
    B <- matrix(NA, times, 8)
    B2 <- numeric(8)
    V <- numeric(8)
    O <- matrix(NA, times, 8)
    for (i in 1:times) {
        y <- T + rnorm(100, 0, 5)
        Q <- data.frame(y, x1, x2, x3, x4, x5, x6, 
            x7)
        fit7 <- lm(y ~ ., data = Q)
        fit6 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, 
            data = Q)
        fit5 <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = Q)
        fit4 <- lm(y ~ x1 + x2 + x3 + x4, data = Q)
        fit3 <- lm(y ~ x1 + x2 + x3, data = Q)
        fit2 <- lm(y ~ x1 + x2, data = Q)
        fit1 <- lm(y ~ x1, data = Q)
        fit0 <- lm(y ~ 1, data = Q)
        
        B[i, 1] <- T[10] - predict(fit0)[10]
        B[i, 2] <- T[10] - predict(fit1)[10]
        B[i, 3] <- T[10] - predict(fit2)[10]
        B[i, 4] <- T[10] - predict(fit3)[10]
        B[i, 5] <- T[10] - predict(fit4)[10]
        B[i, 6] <- T[10] - predict(fit5)[10]
        B[i, 7] <- T[10] - predict(fit6)[10]
        B[i, 8] <- T[10] - predict(fit7)[10]
        
        O[i, 1] <- predict(fit0)[10]
        O[i, 2] <- predict(fit1)[10]
        O[i, 3] <- predict(fit2)[10]
        O[i, 4] <- predict(fit3)[10]
        O[i, 5] <- predict(fit4)[10]
        O[i, 6] <- predict(fit5)[10]
        O[i, 7] <- predict(fit6)[10]
        O[i, 8] <- predict(fit7)[10]
    }
    
    # print(B)
    
    B2[1] <- mean(B[, 1])^2
    B2[2] <- mean(B[, 2])^2
    B2[3] <- mean(B[, 3])^2
    B2[4] <- mean(B[, 4])^2
    B2[5] <- mean(B[, 5])^2
    B2[6] <- mean(B[, 6])^2
    B2[7] <- mean(B[, 7])^2
    B2[8] <- mean(B[, 8])^2
    
    V[1] <- var(O[, 1])
    V[2] <- var(O[, 2])
    V[3] <- var(O[, 3])
    V[4] <- var(O[, 4])
    V[5] <- var(O[, 5])
    V[6] <- var(O[, 6])
    V[7] <- var(O[, 7])
    V[8] <- var(O[, 8])
    
    # cat('B2:\n') print(B2) cat('V:\n') print(V)
    # par(mfrow=c(2,1))
    plot(1:8, B2, xlab = "Number of parameters", ylab = expression(Bias^2), 
        ty = "b", )
    # ylim=c(0, max(B2[-1])))
    title("Bias decreases with number of parameters")
    # dev.copy2eps(file='BiasVarTOa.eps')
    par(ask = TRUE)
    plot(1:8, V, lty = 3, ty = "b", xlab = "Number of parameters", 
        ylab = "Variance")
    title("Variance increases with number of parameters")
    # dev.copy2eps(file='BiasVarTOb.eps')
    par(ask = FALSE)
}
