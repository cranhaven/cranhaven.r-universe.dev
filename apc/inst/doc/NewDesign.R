### R code from vignette source 'NewDesign.Rnw'

###################################################
### code chunk number 1: NewDesign.Rnw:94-98
###################################################
# attach apc library
library(apc)
# get data from precoded function
data.list <- data.Belgian.lung.cancer()


###################################################
### code chunk number 2: NewDesign.Rnw:102-104
###################################################
# Get a deviance table
apc.fit.table(data.list,"poisson.dose.response")


###################################################
### code chunk number 3: NewDesign.Rnw:108-111
###################################################
# Estimate selected model
apc.fit.ad <- apc.fit.model(data.list,"poisson.dose.response","Ad")
apc.fit.ad$coefficients.canonical


###################################################
### code chunk number 4: NewDesign.Rnw:120-128
###################################################
# Vectorise data
index <- apc.get.index(data.list)
v.response <- data.list$response[index$index.data]
v.dose <- data.list$dose[index$index.data]
# Get design matrix for "Ad" model
get.design <- apc.get.design(index,"Ad")
m.design.ad <- get.design$design
p  <- ncol(m.design.ad)


###################################################
### code chunk number 5: NewDesign.Rnw:144-149
###################################################
# Explore this design matrix
index$age.max
p
get.design$difdif
get.design$slopes


###################################################
### code chunk number 6: NewDesign.Rnw:180-187
###################################################
#  Quadractic age effect: restrict double differences to be equal
m.rest.q  <- matrix(data=0,nrow=p,ncol=4)
m.rest.q[1,1]	<- 1
m.rest.q[2,2]	<- 1
m.rest.q[3,3]	<- 1
m.rest.q[4:p,4]	<- 1
m.design.adq	<- m.design.ad %*% m.rest.q


###################################################
### code chunk number 7: NewDesign.Rnw:209-217
###################################################
#  Cubic age effect: restrict double differences to be linear
m.rest.c	<- matrix(data=0,nrow=p,ncol=5)
m.rest.c[1,1]	<- 1
m.rest.c[2,2]	<- 1
m.rest.c[3,3]	<- 1
m.rest.c[4:p,4]	<- 1
m.rest.c[4:p,5]	<- seq(1,p-3)
m.design.adc	<- m.design.ad %*% m.rest.c


###################################################
### code chunk number 8: NewDesign.Rnw:221-228
###################################################
# Poisson regression for dose-response and with log link
fit.ad <- glm.fit(m.design.ad,v.response,
                  family=poisson(link="log"),offset=log(v.dose))
fit.adc <- glm.fit(m.design.adc,v.response,
                   family=poisson(link="log"),offset=log(v.dose))
fit.adq <- glm.fit(m.design.adq,v.response,
                   family=poisson(link="log"),offset=log(v.dose))


###################################################
### code chunk number 9: NewDesign.Rnw:234-250
###################################################
# Deviance test statistics
dev.ad.c <- fit.adc$deviance - fit.ad$deviance 
dev.ad.q <- fit.adq$deviance - fit.ad$deviance 
# Degrees of freedom
df.ad.c <- ncol(m.design.ad) - ncol(m.design.adc)
df.ad.q <- ncol(m.design.ad) - ncol(m.design.adq)
# p-values
p.ad.c <- pchisq(dev.ad.c,df.ad.c,lower.tail=FALSE)
p.ad.q <- pchisq(dev.ad.q,df.ad.q,lower.tail=FALSE)
# Test for cubic restriction
fit.tab<-matrix(nrow=2,ncol=3)
colnames(fit.tab)<-c("LR.vs.Ad","df.vs.Ad","prob(>chi_sq)")
rownames(fit.tab)<-c("cubic","quadratic")
fit.tab[1,1:3] <- c(dev.ad.c,df.ad.c,p.ad.c)
fit.tab[2,1:3] <- c(dev.ad.q,df.ad.q,p.ad.q)
fit.tab


###################################################
### code chunk number 10: NewDesign.Rnw:256-260
###################################################
#  Coefficients
fit.ad$coefficients
fit.adc$coefficients
fit.adq$coefficients


