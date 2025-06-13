library("freqdom")
library(pcdpca)

RES = c()

for (run in 1:100){
## Prepare the process
d = 7
n = 100
A = t(t(matrix(rnorm(d*n),ncol=d,nrow=n)))
B = t(t(matrix(rnorm(d*n),ncol=d,nrow=n)))
A = t(t(A) * exp(-(1:d)/(d) ))
B = t(t(B) * exp(-(1:d)/(d) ))
ntotal = 3*n

X = matrix(0,ncol=d,nrow=3*n)
X[3*(1:n) - 1,] = A
X[3*(1:n) - 2,] = B
X[3*(1:n) ,] = 2*A - B

## Hold out some datapoints
train = 1:(ntotal/2)
test = (1 + ntotal/2) : (ntotal)

## Static PCA ##
PR = prcomp(as.matrix(X[train,]))
Y1 = as.matrix(X) %*% PR$rotation
Y1[,-1] = 0
Xpca.est = Y1 %*% t(PR$rotation)

## Dynamic PCA ##
XI.est = dpca(as.matrix(X[train,]),q=sqn,freq=pi*(-150:150/150),Ndpc = 1)  # finds the optimal filter
Y.est = freqdom::filter.process(X, XI.est$filters )
Xdpca.est = freqdom::filter.process(Y.est, t(rev(XI.est$filters)) )    # deconvolution

## Periodically correlated PCA ##
XI.est.pc = pcdpca(as.matrix(X[train,]),q=sqn,freq=pi*(-150:150/150),period=period)  # finds the optimal filter
Y.est.pc = pcdpca.scores(X, XI.est.pc)  # applies the filter
Y.est.pc[,-1] = 0 # forces the use of only one component
Xpcdpca.est = pcdpca.inverse(Y.est.pc, XI.est.pc)  # deconvolution

# ## Results
# cat("NMSE PCA = ")
r0 = MSE(X[test,],Xpca.est[test,]) / MSE(X[test,],0)
# cat(r0)
# cat("\nNMSE DPCA = ")
r1 = MSE(X[test,],Xdpca.est[test,]) / MSE(X[test,],0)
# cat(r1)
# cat("\nNMSE PCDPCA = ")
r2 = MSE(X[test,],Xpcdpca.est[test,]) / MSE(X[test,],0)
# cat(r2)
# cat("\n")
row = c(r0,r1,r2)
print(row)
RES = rbind(RES,row)
}

colnames(RES) = c("PCA","DPCA","PC-DPCA")
df1 = data.frame(RES,row.names = NULL)

colMeans(df1)
summary(df1)
apply(df1, 2, sd)

t.test(df1$DPCA - df1$PC.DPCA)
par(mfrow=c(1,1),ps = 12, cex = 1.8, cex.main = 1.8)
boxplot(df1, main="Simulation study 1", ylab="Normalized mean squared error",ylim=c(0.25,0.9))
