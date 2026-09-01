### R code from vignette source 'dlsem_vignette.Rnw'
### Encoding: ISO8859-1

###################################################
### code chunk number 1: dlsem_vignette.Rnw:375-376 (eval = FALSE)
###################################################
## install.packages("dlsem")


###################################################
### code chunk number 2: dlsem_vignette.Rnw:383-384 (eval = FALSE)
###################################################
## update.packages("dlsem")


###################################################
### code chunk number 3: dlsem_vignette.Rnw:416-417
###################################################
require(dlsem)


###################################################
### code chunk number 4: dlsem_vignette.Rnw:422-424
###################################################
data(industry)
summary(industry)


###################################################
### code chunk number 5: dlsem_vignette.Rnw:467-472
###################################################
indus.code <- list(
  Job ~ 1,
  Consum~ecq(Job,0,15),
  Pollution~ecq(Job,0,15)+ecq(Consum,0,15)
  )


###################################################
### code chunk number 6: dlsem_vignette.Rnw:532-534
###################################################
indus.global <- list(adapt=T,max.gestation=3,max.lead=15,min.width=5,sign="+")
indus.local <- list()


###################################################
### code chunk number 7: dlsem_vignette.Rnw:541-549
###################################################
indus.global <- list()
indus.local <- list(
  adapt=c(Consum=T,Pollution=T),
  max.gestation=list(Consum=c(Job=3),Pollution=c(Job=3,Consum=3)),
  max.lead=list(Consum=c(Job=15),Pollution=c(Job=15,Consum=15)),
  min.width=list(Consum=c(Job=5),Pollution=c(Job=5,Consum=5)),
  sign=list(Consum=c(Job="+"),Pollution=c(Job="+",Consum="+"))
  )


###################################################
### code chunk number 8: dlsem_vignette.Rnw:554-560
###################################################
indus.global <- list(adapt=T,min.width=5)
indus.local <- list(
  max.gestation=list(Consum=c(Job=3),Pollution=c(Job=3,Consum=3)),
  max.lead=list(Consum=c(Job=15),Pollution=c(Job=15,Consum=15)),
  sign=list(Consum=c(Job="+"),Pollution=c(Job="+",Consum="+"))
  )


###################################################
### code chunk number 9: dlsem_vignette.Rnw:609-611
###################################################
indus.mod <- dlsem(indus.code,group="Region",exogenous=c("Population","GDP"),
  data=industry,global.control=indus.global,local.control=indus.local,log=T)


###################################################
### code chunk number 10: dlsem_vignette.Rnw:626-627
###################################################
summary(indus.mod)


###################################################
### code chunk number 11: dlsem_vignette.Rnw:645-646 (eval = FALSE)
###################################################
## plot(indus.mod)


###################################################
### code chunk number 12: dlsem_vignette.Rnw:688-689
###################################################
causalEff(indus.mod,from="Job",to="Pollution",cumul=T)


###################################################
### code chunk number 13: dlsem_vignette.Rnw:711-713 (eval = FALSE)
###################################################
## lagPlot(indus.mod,path="Job*Pollution")
## lagPlot(indus.mod,path="Job*Consum*Pollution")


###################################################
### code chunk number 14: dlsem_vignette.Rnw:720-721 (eval = FALSE)
###################################################
## lagPlot(indus.mod,from="Job",to="Pollution")


###################################################
### code chunk number 15: dlsem_vignette.Rnw:751-778
###################################################
# model 2: quadratic decreasing lag shapes
indus.code_2 <- list(
  Job ~ 1,
  Consum~qd(Job,0,15),
  Pollution~qd(Job,0,15)+qd(Consum,0,15)
  )
indus.mod_2 <- dlsem(indus.code_2,group="Region",exogenous=c("Population","GDP"),
  data=industry,global.control=indus.global,local.control=indus.local,log=T,quiet=T)
summary(indus.mod_2)$endogenous
# model 3: linearly decreasing lag shapes
indus.code_3 <- list(
  Job ~ 1,
  Consum~ld(Job,0.5,0.5),
  Pollution~ld(Job,0.5,0.5)+ld(Consum,0.5,0.5)
  )
indus.mod_3 <- dlsem(indus.code_3,group="Region",exogenous=c("Population","GDP"),
  data=industry,global.control=indus.global,local.control=indus.local,log=T,quiet=T)
summary(indus.mod_3)$endogenous
# model 4: gamma lag shapes
indus.code_4 <- list(
  Job ~ 1,
  Consum~gam(Job,0.5,0.5),
  Pollution~gam(Job,0.5,0.5)+gam(Consum,0.5,0.5)
  )
indus.mod_4 <- dlsem(indus.code_4,group="Region",exogenous=c("Population","GDP"),
  data=industry,global.control=indus.global,local.control=indus.local,log=T,quiet=T)
summary(indus.mod_4)$endogenous


###################################################
### code chunk number 16: dlsem_vignette.Rnw:788-789
###################################################
compareModels(list(indus.mod,indus.mod_2,indus.mod_3, indus.mod_4))


