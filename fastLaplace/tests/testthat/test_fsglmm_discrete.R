library(testthat)
library(fastLaplace)
library(ngspatial)
library(mgcv)
# test whether our function runs well
test_that("fsglmm.discrete runs well",
          {
            n = 30
            A = ngspatial::adjacency.matrix(n)
            Q = diag(rowSums(A),n^2) - A
            x = rep(0:(n - 1) / (n - 1), times = n)
            y = rep(0:(n - 1) / (n - 1), each = n)
            X = cbind(x, y)                                 # Use the vertex locations as spatial covariates.
            beta = c(1, 1)                                  # These are the regression coefficients.
            P.perp = diag(1,n^2) - X%*%solve(t(X)%*%X)%*%t(X)
            eig = eigen(P.perp %*% A %*% P.perp)
            eigenvalues = eig$values
            q = 400
            M = eig$vectors[,c(1:q)]
            Q.s = t(M) %*% Q %*% M
            tau = 6
            Sigma = solve(tau*Q.s)
            set.seed(1)
            delta.s = mgcv::rmvn(1, rep(0,q), Sigma)
            lambda = exp( X%*%beta + M%*%delta.s )
            Z = c()
            for(j in 1:n^2){Z[j] = rpois(1,lambda[j])}
            Y = as.matrix(Z,ncol=1)
            data = data.frame("Y"=Y,"X"=X)
            colnames(data) = c("Y","X1","X2")
            linmod <- glm(Y~-1+X1+X2,data=data,family="poisson") # Find starting values
            linmod$coefficients
            starting <- c(linmod$coefficients,"logtau"=log(1/var(linmod$residuals)) )
            result.pois.disc <- fsglmm.discrete(Y~-1+X1+X2, inits = starting,
                                                data=data,family="poisson",ntrial=1,
                                                method.optim="BFGS", method.integrate="NR",rank=50, A=A)
            expect_equal(length(result.pois.disc),4)
          })
