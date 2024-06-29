library(testthat)
library(fastLaplace)
library(mgcv)
library(fields)
# test whether our function runs well
test_that("fsglmm runs well",
          {
            sigma2 = 1
            phi = 0.2
            beta.true = c(1,1)
            n = 400
            n.pred = 100
            coords.all<- matrix(runif((n+n.pred)*2),ncol=2,nrow=n+n.pred)
            X.all <- matrix(runif((n+n.pred)*2),ncol=2,nrow=(n+n.pred))
            dist.all <- fields::rdist(coords.all,coords.all)
            matern <- function(phi,mat.dist){
              K = (1+sqrt(5)/phi*mat.dist+ 5/(3*phi^2)*mat.dist^2)*exp(-sqrt(5)/phi*mat.dist)
              return(K)
            }
            V.all <- sigma2*matern(phi,dist.all)
            set.seed(1)
            r.e.all <- mgcv::rmvn(1,rep(0,nrow(coords.all)),V.all)
            pi.all <- X.all%*%beta.true + r.e.all
            p.all <- exp(pi.all)/(1+exp(pi.all))
            Y.all <- sapply(p.all, function(x) sample(0:1, 1, prob = c(1-x, x)))
            Y <- as.matrix(Y.all[1:n],nrow = n)
            X <- X.all[1:n,]
            coords <- coords.all[1:n,]
            data <- data.frame(cbind(Y,X))
            colnames(data) = c("Y","X1","X2")
            mod.glm <- glm(Y~-1+X1+X2,family="binomial",data=data)
            mod.glm.esp <- predict(mod.glm,data, type="response")
            mod.glm.s2 <- var(Y - mod.glm.esp)
            mod.glm.phi <- 0.1*max(dist(coords))
            startinit <- c(mod.glm$coef,log(mod.glm.s2),log(mod.glm.phi))
            names(startinit) <- c("X1","X2","logsigma2","logphi")
            X.pred <- X.all[(n+1):(n+n.pred),]
            coords.pred <- coords.all[(n+1):(n+n.pred),]
            result.bin <- try(fsglmm(Y~-1+X1+X2, kappa=2.5, inits = startinit,
                                     data = data,coords = coords,  family = "binomial",
                                     ntrial = 1, offset = NA,method.optim = "CG",
                                     method.integrate = "NR", control = list(maxit=1000,ndeps=rep(1e-2,4),reltol=0.01),
                                     rank = 50),silent = T)
            expect_equal(length(result.bin),9)
          })
