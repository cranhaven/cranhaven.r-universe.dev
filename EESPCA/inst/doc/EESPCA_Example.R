### R code from vignette source 'EESPCA_Example.Rnw'

###################################################
### code chunk number 1: EESPCA_Example.Rnw:35-39
###################################################
library(MASS)
#library(PMA)
library(rifle)
library(EESPCA)


###################################################
### code chunk number 2: EESPCA_Example.Rnw:45-46
###################################################
set.seed(2)


###################################################
### code chunk number 3: EESPCA_Example.Rnw:51-55
###################################################
p =10 # number of variables
prop.info = 0.4 # proportion of variables that have non-zero loadings on the 1st PC
rho = 0.5 # covariance between informative variables
n = 100 # simulated sample size


###################################################
### code chunk number 4: EESPCA_Example.Rnw:62-68
###################################################
S = matrix(0, nrow=p, ncol=p)
num.info = p*prop.info
S[1:num.info, 1:num.info] = rho
S[p,p-1] = S[p-1,p] = rho
diag(S) = 1 
S


###################################################
### code chunk number 5: EESPCA_Example.Rnw:75-76
###################################################
(eigen.out = eigen(S))


###################################################
### code chunk number 6: EESPCA_Example.Rnw:83-86
###################################################
v1 = eigen.out$vectors[,1]
lambda1 = eigen.out$values[1]
(approx.v1.sq = computeApproxNormSquaredEigenvector(S, v1, lambda1, trace=T))


###################################################
### code chunk number 7: EESPCA_Example.Rnw:93-95
###################################################
X = mvrnorm(n=n, mu=rep(0,p), Sigma=S)
S.hat = cov(X)


###################################################
### code chunk number 8: EESPCA_Example.Rnw:100-103
###################################################
prcomp.out = prcomp(X)
(V.2 = prcomp.out$rotation[,1:2])
prcomp(X)$sdev[1:2]^2


###################################################
### code chunk number 9: EESPCA_Example.Rnw:110-111
###################################################
(pca.error = reconstructionError(X, V.2))


###################################################
### code chunk number 10: EESPCA_Example.Rnw:116-121
###################################################
computeEuclideanDistance = function(x1, x2) {
   # To account for the arbitrary sign of eigenvectors, taking absolute value first.
  return (sqrt(sum((abs(x1) - abs(x2))^2)))
}
(pca.L2 = computeEuclideanDistance(v1, prcomp.out$rotation[,1]))


###################################################
### code chunk number 11: EESPCA_Example.Rnw:126-131
###################################################
v1 = prcomp.out$rotation[,1]
v1^2
lambda1 = prcomp.out$sdev[1]^2
(approx.v1.sq = computeApproxNormSquaredEigenvector(S.hat, v1, lambda1, trace=F))
(ratio = sqrt(approx.v1.sq)/abs(v1))


###################################################
### code chunk number 12: EESPCA_Example.Rnw:136-138
###################################################
eespca.out = eespcaForK(X, k=2, trace=F, compute.sparse.lambda=T)
eespca.out


###################################################
### code chunk number 13: EESPCA_Example.Rnw:143-144
###################################################
(eespca.error = reconstructionError(X, eespca.out$V))


###################################################
### code chunk number 14: EESPCA_Example.Rnw:149-150
###################################################
(eespca.L2 = computeEuclideanDistance(v1, eespca.out$V[,1]))


###################################################
### code chunk number 15: EESPCA_Example.Rnw:156-161
###################################################
p = ncol(X)
default.thresh = 1/sqrt(p)  
sparse.threshold.values=seq(from=0.75*default.thresh, to=1.25*default.thresh, 
	length.out=21)  
cv.out = eespcaCV(X, sparse.threshold.values=sparse.threshold.values)


###################################################
### code chunk number 16: EESPCA_Example.Rnw:165-166
###################################################
eespca.cv.v1 = eespca(X, sparse.threshold=cv.out$best.sparsity, compute.sparse.lambda=T)$v1.sparse


###################################################
### code chunk number 17: EESPCA_Example.Rnw:170-174
###################################################
X.resid = computeResidualMatrix(X, eespca.cv.v1)
cv.out = eespcaCV(X.resid, sparse.threshold.values=sparse.threshold.values)
eespca.cv.v2 =  eespca(X.resid, sparse.threshold=cv.out$best.sparsity, compute.sparse.lambda=T)$v1.sparse
(V = cbind(eespca.cv.v1,eespca.cv.v2))


###################################################
### code chunk number 18: EESPCA_Example.Rnw:179-180
###################################################
(eespca.cv.error = reconstructionError(X, V))


###################################################
### code chunk number 19: EESPCA_Example.Rnw:185-186
###################################################
(eespca.cv.L2 = computeEuclideanDistance(v1, eespca.cv.v1))


###################################################
### code chunk number 20: EESPCA_Example.Rnw:192-193
###################################################
#cv.out =  SPC.cv(X, sumabsv=seq(1, sqrt(p), len=20), niter=10, trace=F)


###################################################
### code chunk number 21: EESPCA_Example.Rnw:197-200
###################################################
#spc.out=SPC(X, sumabsv=cv.out$bestsumabsv, K=2, trace=F)
#spc.out$v
#(spc.error = reconstructionError(X, spc.out$v))


###################################################
### code chunk number 22: EESPCA_Example.Rnw:204-207
###################################################
#spc.1se.out =SPC(X, sumabsv=cv.out$bestsumabsv1se, K=2, trace=F)
#spc.1se.out$v
#(spc.1se.error = reconstructionError(X, spc.1se.out$v))


###################################################
### code chunk number 23: EESPCA_Example.Rnw:212-214
###################################################
#(spc.L2 = computeEuclideanDistance(v1, spc.out$v[,1]))
#(spc.1se.L2 = computeEuclideanDistance(v1, spc.1se.out$v[,1]))


###################################################
### code chunk number 24: EESPCA_Example.Rnw:220-222
###################################################
k.values = round(seq(1, p, len=20))
(optimal.k = tpowerPCACV(X=X, k.values=k.values, nfolds=5))


###################################################
### code chunk number 25: EESPCA_Example.Rnw:226-227
###################################################
v.init = powerIteration(X=S.hat)$v1


###################################################
### code chunk number 26: EESPCA_Example.Rnw:231-239
###################################################
tpower.v1 = tpower(X=S.hat, max.iter=100, k=optimal.k, v1.init=v.init)
X.resid = computeResidualMatrix(X, tpower.v1)
(optimal.k2 = tpowerPCACV(X=X.resid, k.values=k.values, nfolds=5))
X.resid.cov = cov(X.resid)
v2.init = powerIteration(X=X.resid.cov)$v1
tpower.v2 = tpower(X=X.resid.cov, max.iter=100, k=optimal.k2, v1.init=v2.init)
(v = cbind(tpower.v1, tpower.v2))
(tpower.error = reconstructionError(X, v))


###################################################
### code chunk number 27: EESPCA_Example.Rnw:244-245
###################################################
(tpower.L2 = computeEuclideanDistance(v1, tpower.v1))


###################################################
### code chunk number 28: EESPCA_Example.Rnw:251-253
###################################################
k.values = round(seq(1, p, len=20))
(optimal.k = riflePCACV(X=X, k.values=k.values, nfolds=5))


###################################################
### code chunk number 29: EESPCA_Example.Rnw:257-258
###################################################
v.init = rifleInit(X)


###################################################
### code chunk number 30: EESPCA_Example.Rnw:262-270
###################################################
rifle.v1 = rifle(A=S.hat, B=diag(p), init=v.init, k=optimal.k)
X.resid = computeResidualMatrix(X, rifle.v1)
(optimal.k2 = riflePCACV(X=X.resid, k.values=k.values, nfolds=5))
X.resid.cov = cov(X.resid)
v2.init = rifleInit(X.resid)
rifle.v2 = rifle(A=X.resid.cov, B=diag(p), init=v2.init, k=optimal.k2)
(v = cbind(rifle.v1, rifle.v2))
(rifle.error = reconstructionError(X, v))


###################################################
### code chunk number 31: EESPCA_Example.Rnw:274-275
###################################################
(rifle.L2 = computeEuclideanDistance(v1, -rifle.v1))


###################################################
### code chunk number 32: EESPCA_Example.Rnw:280-288
###################################################
estimation.summary = data.frame(pca=c(pca.error, pca.L2),
	#spc=c(spc.error, spc.L2),
	#spc.1se=c(spc.1se.error, spc.1se.L2),
	eespca=c(eespca.error, eespca.L2),
	tpower=c(tpower.error, tpower.L2),
	rifle=c(rifle.error, rifle.L2))
rownames(estimation.summary) = c("Reconstruction Error", "Loadings distance")
estimation.summary


