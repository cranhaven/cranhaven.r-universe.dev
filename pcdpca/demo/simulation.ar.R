library("freqdom")
library(pcdpca)

## Prepare the process
period = 2
d = 7
n = 1000

RES = c()

for (run in 1:100){
Psi = c()
for (i in 1:period){
  PsiTmp = c()
  for (j in 1:period){
    P = matrix(rnorm(d*d, sd = exp(-(1:d)/d) ),d)
#    P = P %*% t(P)
    PsiTmp = cbind(PsiTmp,0.9 * P / norm(P))
  }
  Psi = rbind(Psi,PsiTmp)
}
Xd = rar(n = n,Psi = Psi )
X = t(matrix(t(Xd),d))

## Hold out some datapoints
train = 1:(nrow(X)/2)
test = (nrow(X)/2 + 1) : (nrow(X))

## Static PCA ##
PR = prcomp(as.matrix(X[train,]))
Y1 = as.matrix(X) %*% PR$rotation
Y1[,-1] = 0
Xpca.est = Y1 %*% t(PR$rotation)

sqn = floor(sqrt(n))
lags = -sqn:sqn

## Dynamic PCA ##
XI.est = dpca(as.matrix(X[train,]),q=sqn,freq=pi*(-150:150/150),Ndpc = 1)  # finds the optimal filter
Y.est = freqdom::filter.process(X, XI.est$filters )
Xdpca.est = freqdom::filter.process(Y.est, t(rev(XI.est$filters)) )    # deconvolution

## Periodically correlated PCA ##
XI.est.pc = pcdpca(as.matrix(X[train,]),q=sqn,freq=pi*(-150:150/150),period=period)  # finds the optimal filter
Y.est.pc = pcdpca.scores(X, XI.est.pc)  # applies the filter
Y.est.pc[,-1] = 0 # forces the use of only one component
Xpcdpca.est = pcdpca.inverse(Y.est.pc, XI.est.pc)  # deconvolution

## Results
r0 = MSE(X[test,],Xpca.est[test,]) / MSE(X[test,],0)
r1 = MSE(X[test,],Xdpca.est[test,]) / MSE(X[test,],0)
r2 = MSE(X[test,],Xpcdpca.est[test,]) / MSE(X[test,],0)
row = c(r0,r1,r2)
print(row)
RES = rbind(RES,row)
}

colnames(RES) = c("PCA","DPCA","PC-DPCA")
df2 = data.frame(RES,row.names = NULL)

colMeans(df2)
summary(df2)
apply(df2, 2, sd)

t.test(df2$DPCA - df2$PC.DPCA)
par(mfrow=c(1,1),ps = 12, cex = 1.8, cex.main = 1.8)
boxplot(df2, main="Simulation study 2", ylab="Normalized mean squared error",ylim=c(0.25,0.9))
