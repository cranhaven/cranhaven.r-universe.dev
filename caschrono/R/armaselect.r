armaselect = function(y, max.p = 15, max.q = 15, nbmod = 10) {
 matlag <- function(y, maxlag) {
   # input: column matrix or vector 
   # output: matrix with maxlag columns 
   y <- as.matrix(y)
   n <- nrow(y)
   x <- matrix(1, nrow = n, ncol = 1)
   for (i in 1:maxlag) {
     x <- cbind(x, Lag(y,i))
     }
   x <- x[, -1]
   colnames(x) <- paste("Lag_", as.character(1:maxlag), sep = "")
   x
 }
 # step 1) : compute autoregression, step 2) OLS on y lagged and residuals
 n <- length(y)
 pmaxi <- floor(min(n - 1, 10 * log10(n)))
 # centering data
 yc <- y - mean(y)
 # residuals 
 z.tilde = ar(yc, aic = FALSE, order.max = pmaxi, 
              method = "yule-walker", demean = FALSE)$resid
 yret <- matlag(yc, max.p)
 # residuals 
 resret <- matlag(z.tilde, max.q)
 # loop
 mm <- lm(yc ~ 0)
 sbc <- nrow(resret) * log(var(mm$residuals))
 # crieria 
 bic <- AIC(mm, k = log(nrow(resret)))
 resul <- matrix(NA, nrow = (max.p + 1) * (max.q + 1), ncol = 4)
 colnames(resul) <- c("p", "q", "bic", "sbc")
 ili <- 1
 resul[ili, ] <- c(0, 0, bic, sbc)
 # q nul
 for(ip in 1:max.p) {
   ili <- ili + 1
   iq <- 0
   mm <- lm(yc ~ yret[, 1:ip] - 1)
   bic <- AIC(mm, k = log(nrow(resret)))
   sbc <- nrow(resret) * log(var(mm$residuals)) + ip * log(nrow(resret))
   resul[ili, ] <- c(ip, 0, bic, sbc)
 }
 # p nul
 for (iq in 1:max.q) {
   ili <- ili + 1 
   ip <- 0
   mm <- lm(yc ~ resret[, 1:iq] - 1)
   bic <- AIC(mm, k = log(nrow(resret)))
   sbc <- nrow(resret) * log(var(mm$residuals)) + iq * log(nrow(resret))
   resul[ili,] <- c(0, iq, bic, sbc)
 }
 for (ip in 1:max.p) {
   for(iq in 1:max.q) {
     ili <- ili + 1
     mm <- lm(yc ~ yret[, 1:ip] + resret[, 1:iq] - 1)
     bic <- AIC(mm, k = log(nrow(resret)))
     sbc <- nrow(resret) * log(var(mm$residuals)) + (ip + iq) * log(nrow(resret))
     resul[ili,] <- c(ip, iq, bic, sbc)
   }
  }
 ordre <- order(resul[, 4])
 sbc_opt <- resul[ordre,][1:nbmod, c(1, 2, 4)]
 colnames(sbc_opt ) <- c("p", "q", "sbc")
 sbc_opt
}