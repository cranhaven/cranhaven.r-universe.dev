### R code from vignette source 'hmeasure.Rnw'

###################################################
### code chunk number 1: hmeasure.Rnw:65-70
###################################################
require(MASS); require(class); data(Pima.te); 
library(hmeasure)
n <- dim(Pima.te)[1]; ntrain <- floor(2*n/3); ntest <- n-ntrain
pima.train <- Pima.te[seq(1,n,3),]
pima.test <- Pima.te[-seq(1,n,3),]


###################################################
### code chunk number 2: hmeasure.Rnw:73-75
###################################################
true.labels <- pima.test[,8]
str(true.labels)


###################################################
### code chunk number 3: hmeasure.Rnw:78-79
###################################################
lda.pima <- lda(formula=type~., data=pima.train)


###################################################
### code chunk number 4: hmeasure.Rnw:82-85
###################################################
out.lda = predict(lda.pima, newdata=pima.test)
true.labels.01 <- relabel(true.labels)
lda.labels.01 <- relabel(out.lda$class)


###################################################
### code chunk number 5: hmeasure.Rnw:88-89
###################################################
lda.counts <- misclassCounts(true.labels.01, lda.labels.01); lda.counts$conf.matrix


###################################################
### code chunk number 6: hmeasure.Rnw:92-93
###################################################
print(lda.counts$metrics,digits=3)


###################################################
### code chunk number 7: hmeasure.Rnw:104-105
###################################################
relabel(c("Yes","No","No"))


###################################################
### code chunk number 8: hmeasure.Rnw:108-109
###################################################
relabel(c("case","noncase","case"))


###################################################
### code chunk number 9: hmeasure.Rnw:130-131
###################################################
out.lda$posterior[1:3,]


###################################################
### code chunk number 10: hmeasure.Rnw:134-136
###################################################
scores.lda <- out.lda$posterior[,2]; 
all((scores.lda > 0.5) == lda.labels.01)


###################################################
### code chunk number 11: hmeasure.Rnw:139-142
###################################################
lda.counts.T03 <- misclassCounts(scores.lda>0.3, true.labels.01)
lda.counts.T03$conf.matrix
lda.counts.T03$metrics[c('Sens','Spec')]


###################################################
### code chunk number 12: hmeasure.Rnw:148-152
###################################################
class.knn <- knn(train=pima.train[,-8], test=pima.test[,-8],
  cl=pima.train$type, k=9, prob=TRUE, use.all=TRUE)
scores.knn <- attr(class.knn,"prob")
scores.knn[class.knn=="No"] <- 1-scores.knn[class.knn=="No"] 


###################################################
### code chunk number 13: hmeasure.Rnw:155-158
###################################################
scores <- data.frame(LDA=scores.lda,kNN=scores.knn)
results <- HMeasure(true.labels,scores)
class(results)


###################################################
### code chunk number 14: hmeasure.Rnw:168-169
###################################################
plotROC(results)


###################################################
### code chunk number 15: hmeasure.Rnw:178-179
###################################################
summary(results)


###################################################
### code chunk number 16: hmeasure.Rnw:195-196
###################################################
summary(results,show.all=TRUE)


###################################################
### code chunk number 17: hmeasure.Rnw:199-200
###################################################
HMeasure(true.labels,scores,threshold=0.3)$metrics[c('Sens','Spec')]


###################################################
### code chunk number 18: hmeasure.Rnw:203-204
###################################################
HMeasure(true.labels,scores,threshold=c(0.3,0.3))$metrics[c('Sens','Spec')]


###################################################
### code chunk number 19: hmeasure.Rnw:207-208
###################################################
HMeasure(true.labels,scores,threshold=c(0.5,0.3))$metrics[c('Sens','Spec')]


###################################################
### code chunk number 20: hmeasure.Rnw:213-214
###################################################
summary(HMeasure(true.labels,scores,level=c(0.95,0.99)))


###################################################
### code chunk number 21: hmeasure.Rnw:237-238
###################################################
plotROC(results,which=4)


###################################################
### code chunk number 22: hmeasure.Rnw:276-277
###################################################
results$metrics[c('H','KS','ER','FP','FN')]


###################################################
### code chunk number 23: hmeasure.Rnw:280-281
###################################################
summary(pima.test[,8])


###################################################
### code chunk number 24: hmeasure.Rnw:284-287
###################################################
results.SR1 <- HMeasure(
  true.labels, data.frame(LDA=scores.lda,kNN=scores.knn),severity.ratio=1)
results.SR1$metrics[c('H','KS','ER','FP','FN')]


###################################################
### code chunk number 25: hmeasure.Rnw:292-295
###################################################
par(mfrow=c(2,1))
plotROC(results,which=2)
plotROC(results.SR1,which=2)


###################################################
### code chunk number 26: hmeasure.Rnw:303-304
###################################################
plotROC(results,which=3)


