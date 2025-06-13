### R code from vignette source 'IntroductionAggregateData.rnw'

###################################################
### code chunk number 1: IntroductionAggregateData.rnw:84-85
###################################################
library(apc)


###################################################
### code chunk number 2: IntroductionAggregateData.rnw:102-105
###################################################
data.list   <- data.Belgian.lung.cancer()
objects(data.list)
data.list


###################################################
### code chunk number 3: IntroductionAggregateData.rnw:116-117
###################################################
apc.plot.data.all(data.list)


###################################################
### code chunk number 4: IntroductionAggregateData.rnw:122-124
###################################################
graphics.off()
apc.plot.data.sums(data.list)


###################################################
### code chunk number 5: IntroductionAggregateData.rnw:131-134
###################################################
graphics.off()
apc.plot.data.sparsity(data.list)
apc.plot.data.sparsity(data.list,sparsity.limits=c(5,10))


###################################################
### code chunk number 6: IntroductionAggregateData.rnw:141-143
###################################################
graphics.off()
apc.plot.data.within.all.six(data.list,"m")


###################################################
### code chunk number 7: IntroductionAggregateData.rnw:152-153
###################################################
apc.fit.table(data.list,"poisson.dose.response")


###################################################
### code chunk number 8: IntroductionAggregateData.rnw:160-163
###################################################
fit.apc <- apc.fit.model(data.list,"poisson.dose.response","APC")
fit.ad  <- apc.fit.model(data.list,"poisson.dose.response","Ad")
fit.a   <- apc.fit.model(data.list,"poisson.dose.response","A")


###################################################
### code chunk number 9: IntroductionAggregateData.rnw:166-168
###################################################
fit.apc$coefficients.canonical
fit.ad$coefficients.canonical


###################################################
### code chunk number 10: IntroductionAggregateData.rnw:184-188
###################################################
graphics.off()
apc.plot.fit.all(fit.apc)
apc.plot.fit.all(fit.ad)
apc.plot.fit.all(fit.a)


###################################################
### code chunk number 11: IntroductionAggregateData.rnw:204-208
###################################################
graphics.off()
apc.plot.fit(fit.apc)
apc.plot.fit(fit.ad)
apc.plot.fit(fit.a)


###################################################
### code chunk number 12: IntroductionAggregateData.rnw:214-216
###################################################
data.list.subset.1 <- apc.data.list.subset(data.list,0,0,1,0,0,0)
apc.fit.table(data.list.subset.1,"poisson.dose.response")


###################################################
### code chunk number 13: IntroductionAggregateData.rnw:234-242
###################################################
graphics.off()
data.list   <- data.Belgian.lung.cancer()
data.list.subset <- apc.data.list.subset(data.list,2,0,0,0,0,0)
fit.apc     <- apc.fit.model(data.list,"poisson.dose.response","APC")
fit.apc.subset  <- apc.fit.model(data.list.subset,"poisson.dose.response","APC")
apc.plot.fit(fit.apc.subset,
            main.outer="1. Belgian lung cancer: cut first two age groups")
apc.plot.fit(fit.apc,main.outer="2. Belgian lung cancer data: all data")


