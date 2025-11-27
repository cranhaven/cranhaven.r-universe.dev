## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",  
  out.extra = 'style="border:0; margin: auto"', 
fig.width=5,
fig.height=5,
out.width="400px",  
out.height="400px"  
)

## ----setup, results="hide", warning=FALSE,message=FALSE-----------------------
library(ordPens)

## -----------------------------------------------------------------------------
library(psy) 
data(ehd)
H <- ehd + 1
head(H)

## -----------------------------------------------------------------------------
ehd_pca1 <- ordPCA(H, p = 2, lambda = 0.5, maxit = 100, crit = 1e-7,
                   qstart = NULL, Ks = apply(H, 2, max), constr = rep(TRUE, ncol(H)),
                   CV = FALSE, k = 5, CVfit = FALSE)

## -----------------------------------------------------------------------------
summary(ehd_pca1$pca)

## -----------------------------------------------------------------------------
ehd_pca1$qs

## ---- fig.align="center"------------------------------------------------------
plot(1:5, ehd_pca1$qs[[9]], type = "b", xlab = "category", ylab = "quantification", 
     col = 2, main = colnames(H)[9], bty = "n")

## -----------------------------------------------------------------------------
ehd_pca2 <- ordPCA(H, p = 2, lambda = c(5, 0.5, 0), maxit = 100, crit = 1e-7,
                   qstart = NULL, Ks = apply(H, 2, max), constr = rep(TRUE, ncol(H)),
                   CV = FALSE)

## -----------------------------------------------------------------------------
summary(ehd_pca2$pca[[1]])

## -----------------------------------------------------------------------------
ehd_pca2$qs[[9]]

## ---- fig.align="center"------------------------------------------------------
plot(ehd_pca2$qs[[9]][,3], type = "b", xlab = "category", ylab = "quantification", col = 1, 
     ylim = range(ehd_pca2$qs[[9]]), main = colnames(H)[9], bty = "n")
lines(ehd_pca2$qs[[9]][,2], type = "b", col = 2, lty = 2, pch = 2, lwd=2)
lines(ehd_pca2$qs[[9]][,1], type = "b", col = 3, lty = 3, pch = 3, lwd=2)

## ----test_a, figures-side, fig.show="hold", fig.width = 3, fig.height = 3, out.width="31%",out.height="30%"----
par(mar = c(4.1, 4.1, 3.1, 1.1))

for(j in c(1, 9, 12, 13, 15, 19)){ 
  plot(ehd_pca2$qs[[j]][,3], type = "b", main = colnames(H)[j], xlab = "category", 
       ylab = "quantification", lwd = 2, bty = "n") 
  lines(ehd_pca2$qs[[j]][,2], type = "b", col = 2, lty = 2, pch = 2, lwd = 2)
  lines(ehd_pca2$qs[[j]][,1], type = "b", col = 3, lty = 3, pch = 3, lwd = 2)
} 

## -----------------------------------------------------------------------------
ehd_pca3 <- ordPCA(H, p = 2, lambda = c(5, 0.5, 0.001), maxit = 100, crit = 1e-7,
                   qstart = NULL, Ks = apply(H, 2, max), constr = rep(TRUE, ncol(H)),
                   CV = TRUE, k = 5)

ehd_pca3$VAFtest

## -----------------------------------------------------------------------------
lambda <- 10^seq(4, -4, by = -0.1)
set.seed(456)
ehd_CV_p2 <- ordPCA(H, p = 2, lambda = lambda, maxit = 100, crit = 1e-7, Ks = apply(H, 2, max),
                   qstart = NULL, constr = rep(TRUE, ncol(H)), CV = TRUE, k = 5, CVfit = FALSE)

lam_p2 <- (lambda)[which.max(apply(ehd_CV_p2$VAFtest,2,mean))]
ehd_CV_p2$VAFtest

## ----test, figures-side, fig.show="hold", fig.width = 5, fig.height = 5, out.width="45%",out.height="50%"----
plot(log10(lambda), apply(ehd_CV_p2$VAFtrain,2,mean), type = "l",
     xlab = expression(log[10](lambda)), ylab = "proportion of variance explained",
     main = "training data", cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)
plot(log10(lambda), apply(ehd_CV_p2$VAFtest,2,mean), type = "l",
     xlab = expression(log[10](lambda)), ylab = "proportion of variance explained",
     main = "validation data", cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.4)
abline(v = log10(lambda)[which.max(apply(ehd_CV_p2$VAFtest,2,mean))])


## ---- fig.align="center"------------------------------------------------------
# evaluate model with optimal lambda for p=2
ehd_pca_p2 <- ordPCA(H, p=2, lambda = lam_p2, Ks=apply(H,2,max), constr=rep(TRUE,ncol(H)))

# evaluate optimal lambda & model for p=1, p=3, p=4
set.seed(456)
ehd_CV_p1 <- ordPCA(H, p = 1, lambda=lambda, constr = rep(TRUE, ncol(H)), CV = TRUE, k = 5)
ehd_CV_p3 <- ordPCA(H, p = 3, lambda=lambda, constr = rep(TRUE, ncol(H)), CV = TRUE, k = 5)
ehd_CV_p4 <- ordPCA(H, p = 4, lambda=lambda, constr = rep(TRUE, ncol(H)), CV = TRUE, k = 5)

lam_p1 <- (lambda)[which.max(apply(ehd_CV_p1$VAFtest,2,mean))]
lam_p3 <- (lambda)[which.max(apply(ehd_CV_p3$VAFtest,2,mean))]
lam_p4 <- (lambda)[which.max(apply(ehd_CV_p4$VAFtest,2,mean))]

ehd_pca_p1 <- ordPCA(H, p=3, lambda=lam_p1, Ks=apply(H,2,max), constr=rep(TRUE,ncol(H)))
ehd_pca_p3 <- ordPCA(H, p=3, lambda=lam_p1, Ks=apply(H,2,max), constr=rep(TRUE,ncol(H)))
ehd_pca_p4 <- ordPCA(H, p=4, lambda=lam_p1, Ks=apply(H,2,max), constr=rep(TRUE,ncol(H)))


## ---- fig.align="center"------------------------------------------------------
# evaluate model with optimal lambda for p=2
ehd_pca_p2 <- ordPCA(H, p=2, lambda = lam_p2, Ks=apply(H,2,max), constr=rep(TRUE,ncol(H)))

# evaluate optimal lambda & model for p=1, p=3, p=4
set.seed(456)
ehd_CV_p1 <- ordPCA(H, p = 1, lambda=lambda, constr = rep(TRUE, ncol(H)), CV = TRUE, k = 5)
ehd_CV_p3 <- ordPCA(H, p = 3, lambda=lambda, constr = rep(TRUE, ncol(H)), CV = TRUE, k = 5)
ehd_CV_p4 <- ordPCA(H, p = 4, lambda=lambda, constr = rep(TRUE, ncol(H)), CV = TRUE, k = 5)

lam_p1 <- (lambda)[which.max(apply(ehd_CV_p1$VAFtest,2,mean))]
lam_p3 <- (lambda)[which.max(apply(ehd_CV_p3$VAFtest,2,mean))]
lam_p4 <- (lambda)[which.max(apply(ehd_CV_p4$VAFtest,2,mean))]

ehd_pca_p1 <- ordPCA(H, p=3, lambda=lam_p1, Ks=apply(H,2,max), constr=rep(TRUE,ncol(H)))
ehd_pca_p3 <- ordPCA(H, p=3, lambda=lam_p1, Ks=apply(H,2,max), constr=rep(TRUE,ncol(H)))
ehd_pca_p4 <- ordPCA(H, p=4, lambda=lam_p1, Ks=apply(H,2,max), constr=rep(TRUE,ncol(H)))
 
plot(ehd_pca_p1$pca$sdev[1:10]^2, bty="n",  xaxt="n", type="o", main=NULL, xlab="", pch=19,
     ylab="Variances", ylim=range(c(ehd_pca_p1$pca$sdev^2, prcomp(H, scale=T)$sdev^2)), col=6)
lines(1:10, ehd_pca_p2$pca$sdev[1:10]^2, col = 2, type = "o", pch = 19)
lines(1:10, ehd_pca_p3$pca$sdev[1:10]^2, col = 3, type = "o", pch = 19)
lines(1:10, ehd_pca_p4$pca$sdev[1:10]^2, col = 4, type = "o", pch = 19)
lines(1:10, prcomp(H, scale = T)$sdev[1:10]^2, col = 1, type = "o", pch = 19)
legend(8, 5, legend=c("p=1","p=2","p=3","p=4","std"), col=c(6,2:4,1), lty=1, bty="n")
axis(1, at = 1:10, labels = 1:10)  

## -----------------------------------------------------------------------------
data(ICFCoreSetCWP) 
H <- ICFCoreSetCWP[, 1:67] + matrix(c(rep(1,50), rep(5,16), 1), 
                                    nrow(ICFCoreSetCWP), 67, byrow = TRUE)
head(H)
xnames <- colnames(H)

## ----test_environment, figures-side, fig.show="hold", fig.width = 5, fig.height = 5, out.width="45%",out.height="50%"----
icf_pca1 <- ordPCA(H, p = 2, lambda = c(5, 0.5, 0.001), maxit = 100, crit = 1e-7, qstart = NULL, 
                   Ks = c(rep(5,50), rep(9,16), 5), 
                   constr = c(rep(TRUE,50), rep(FALSE,16), TRUE), 
                   CV = FALSE, k = 5, CVfit = FALSE) 
 
icf_pca1C <- ordPCA(H, p = 2, lambda = c(5, 0.5, 0.001), maxit = 100, crit = 1e-7, qstart = NULL, 
                    Ks = c(rep(5,50), rep(9,16), 5), constr = rep(TRUE, ncol(H)), 
                    CV = FALSE, k = 5, CVfit = FALSE) 

plot(icf_pca1$qs[[51]][,3], type = "b", xlab = "category", ylab = "quantification", col = 1, 
     ylim = range(icf_pca1$qs[[51]]), main = xnames[51], bty = "n", xaxt = "n") 
lines(icf_pca1$qs[[51]][,2], type = "b", col = 2, lty = 2, pch = 2, lwd=2)
lines(icf_pca1$qs[[51]][,1], type = "b", col = 3, lty = 3, pch = 3, lwd=2)
axis(1, at = 1:length(icf_pca1$qs[[51]][,1]), labels = -4:4)   

plot(icf_pca1C$qs[[51]][,3], type = "b", xlab = "category", ylab = "quantification", col = 1, 
     ylim = range(icf_pca1C$qs[[51]]), main = xnames[51], bty = "n", xaxt = "n")  
lines(icf_pca1C$qs[[51]][,2], type = "b", col = 2, lty = 2, pch = 2, lwd=2)
lines(icf_pca1C$qs[[51]][,1], type = "b", col = 3, lty = 3, pch = 3, lwd=2)
axis(1, at = 1:length(icf_pca1C$qs[[51]][,1]), labels = -4:4)   

## ----test_pca, figures-side, fig.show="hold", fig.width = 3, fig.height = 3, out.width="31%",out.height="30%"----
wx <- which(xnames=="b265"|xnames=="d450"|xnames=="d455"|xnames=="e1101"|xnames=="e460"
            |xnames=="e325") 
xmain <- c()
xmain[wx] <- list("Touch function",
                  "Walking",
                  "Moving around",
                  "Drugs",
                  "Societal attitudes",
                   paste(c("Acquaintances,colleagues,","peers,community members")))

par(mar = c(4.1, 4.1, 3.1, 1.1))
for (j in wx){
plot(icf_pca1$qs[[j]][,3], type = "b", main = xmain[j], xlab = "category", bty = "n", 
     ylim = range(icf_pca1$qs[[j]]), ylab = "quantification", xaxt = "n", cex.main= 1)  
lines(icf_pca1$qs[[j]][,2], type = "b", col = 2, lty = 2, pch = 2, lwd=2)
lines(icf_pca1$qs[[j]][,1], type = "b", col = 3, lty = 3, pch = 3, lwd=2)
axis(1, at = 1:length(icf_pca1$qs[[j]][,1]), 
     labels = ((1:length(icf_pca1$qs[[j]][,1])) - c(rep(1,50), rep(5,16), 1)[j]))   
}

