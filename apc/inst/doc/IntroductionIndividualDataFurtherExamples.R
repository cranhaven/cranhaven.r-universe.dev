### R code from vignette source 'IntroductionIndividualDataFurtherExamples.rnw'

###################################################
### code chunk number 1: IntroductionIndividualDataFurtherExamples.rnw:80-91
###################################################
library("ISLR")
data("Wage")
Wage2 <- Wage[Wage$age >= 25 & Wage$age <= 55, ]
names(Wage2)[names(Wage2) %in% c("year","age")] <- c("period","age")
cohort <- Wage2$period - Wage2$age
indust_job <- ifelse(Wage2$jobclass=="1. Industrial", 1, 0)
hasdegree <- ifelse(Wage2$education %in%
        c("4. College Grad", "5. Advanced Degree"), 1, 0)
married <- ifelse(Wage2$maritl == "2. Married", 1, 0)
Wage3 <- cbind(Wage2, cohort, indust_job, hasdegree, married)
rm(Wage, Wage2, cohort, indust_job, hasdegree, married)


###################################################
### code chunk number 2: IntroductionIndividualDataFurtherExamples.rnw:95-99
###################################################
library("plyr")
library("apc")
model1 <- apc.indiv.est.model(Wage3, dep.var="logwage")
apc.plot.fit(model1)


###################################################
### code chunk number 3: IntroductionIndividualDataFurtherExamples.rnw:103-109
###################################################
model2 <- apc.indiv.est.model(Wage3, dep.var = "married",
                              covariates = c("logwage", "hasdegree"),
                              model.design = "AC",
                              model.family = "binomial")
apc.plot.fit(model2)
model2$coefficients.covariates


###################################################
### code chunk number 4: IntroductionIndividualDataFurtherExamples.rnw:113-122
###################################################
Wage3_cc <- Wage3[Wage3$cohort>1950 & Wage3$cohort<1982, ]
model3 <- apc.indiv.est.model(Wage3_cc, dep.var = "married",
                              covariates = c("logwage", "hasdegree"),
                              model.design = "AC",
                              model.family = "binomial",
                              n.coh.excl.end = 3,
                              n.coh.excl.start = 3)
apc.plot.fit(model3)
model3$coefficients.covariates


###################################################
### code chunk number 5: IntroductionIndividualDataFurtherExamples.rnw:126-128
###################################################
library("car")
linearHypothesis(model3$fit, "logwage = hasdegree", test="F")


###################################################
### code chunk number 6: IntroductionIndividualDataFurtherExamples.rnw:132-139
###################################################
model4 <- apc.indiv.est.model(Wage3_cc, dep.var = "hasdegree",
                          model.family = "binomial",
                          covariates = "logwage",
                          model.design = "TS",
                          n.coh.excl.start = 3,
                          n.coh.excl.end = 3)
model4$result


###################################################
### code chunk number 7: IntroductionIndividualDataFurtherExamples.rnw:143-154
###################################################
myspec2 <- list(20,30,.002,"ols",.Machine$double.eps,.002,NULL,NULL)
names(myspec2) <- c("maxit.loop", "maxit.linesearch", "tolerance",
        "init", "inv.tol", "d1.tol", "custom.kappa", "custom.zeta")
model4b <- apc.indiv.est.model(Wage3_cc, dep.var = "hasdegree",
                          model.family = "binomial",
                          covariates = "logwage",
                          model.design = "TS",
                          n.coh.excl.start = 3,
                          n.coh.excl.end = 3,
                          NR.controls = myspec2)
model4b$result


###################################################
### code chunk number 8: IntroductionIndividualDataFurtherExamples.rnw:158-164
###################################################
library("survey")
inv_wt <- runif(nrow(Wage3), 0, 1)
Wage_wt <- cbind(Wage3, inv_wt)
model5 <- apc.indiv.est.model(Wage_wt, dep.var = "logwage",
                              wt.var= "inv_wt")
apc.plot.fit(model5)


###################################################
### code chunk number 9: IntroductionIndividualDataFurtherExamples.rnw:172-185
###################################################
library("AER")
data("PSID7682")
period <- as.numeric(PSID7682$year) + 1975
entry <- period - PSID7682$experience
logwage <- log(PSID7682$wage)
inunion <- ifelse(PSID7682$union == "yes", 1, 0)
insouth <- ifelse(PSID7682$south == "yes", 1, 0)
psid2 <- cbind(PSID7682, period, entry, logwage, inunion, insouth)
names(psid2)[names(psid2) %in% c("experience", "entry")] <-
                                                c("age", "cohort")
psid3 <- psid2[psid2$cohort >=1939, ]

rm(PSID7682, period, entry, logwage, inunion, insouth, psid2)


###################################################
### code chunk number 10: IntroductionIndividualDataFurtherExamples.rnw:189-196
###################################################
library("plm")
model6 <- apc.indiv.est.model(psid3, dep.var = "logwage",
                              covariates = c("inunion", "insouth"),
                              plmmodel = "within", id.var = "id", 
                              model.design = "FAP")
apc.plot.fit(model6)
model6$coefficients.covariates


###################################################
### code chunk number 11: IntroductionIndividualDataFurtherExamples.rnw:200-204
###################################################
model6b <- apc.indiv.est.model(psid3, dep.var = "logwage",
                               plmmodel = "within", id.var = "id", 
                               model.design = "FAP")
waldtest(model6$fit, model6b$fit)


###################################################
### code chunk number 12: IntroductionIndividualDataFurtherExamples.rnw:208-220
###################################################
collinear_1 <- apc.indiv.design.collinear(psid3)
design_1 <- apc.indiv.design.model(collinear_1, dep.var = "logwage",
                           covariates = c("inunion", "insouth"),
                           plmmodel = "random", id.var ="id")
plm_1 <- plm(design_1$model.formula,
              data = collinear_1$full.design.collinear, 
              index = c("id", "period"), model = "random")
design_2 <- apc.indiv.design.model(collinear_1, dep.var = "logwage",
                                   plmmodel = "random", id.var ="id")
fit_2 <- apc.indiv.fit.model(design_2)

waldtest(plm_1, fit_2$fit, test="F")


###################################################
### code chunk number 13: IntroductionIndividualDataFurtherExamples.rnw:231-242
###################################################
library("ISLR")
data("Wage")
Wage2 <- Wage[Wage$age >= 25 & Wage$age <= 55, ]
names(Wage2)[names(Wage2) %in% c("year","age")] <- c("period","age")
cohort <- Wage2$period - Wage2$age
indust_job <- ifelse(Wage2$jobclass=="1. Industrial", 1, 0)
hasdegree <- ifelse(Wage2$education %in%
            c("4. College Grad", "5. Advanced Degree"), 1, 0)
married <- ifelse(Wage2$maritl == "2. Married", 1, 0)
Wage3 <- cbind(Wage2, cohort, indust_job, hasdegree, married)
rm(Wage, Wage2, cohort, indust_job, hasdegree, married)


###################################################
### code chunk number 14: IntroductionIndividualDataFurtherExamples.rnw:246-251
###################################################
test1 <- apc.indiv.model.table(Wage3, dep.var="logwage",
                               test= "Wald", dist="F",
                               model.family="gaussian",
                               TS=TRUE)
test1$table


###################################################
### code chunk number 15: IntroductionIndividualDataFurtherExamples.rnw:255-261
###################################################
test2 <- apc.indiv.model.table(Wage3, dep.var="married",
                               covariates = "hasdegree",
                               test="LR", dist="Chisq",
                               TS=TRUE, model.family="binomial")
test2$table
test2$NR.report


###################################################
### code chunk number 16: IntroductionIndividualDataFurtherExamples.rnw:266-274
###################################################
    inv_wt <- runif(nrow(Wage3), 0, 1)
    Wage_wt <- cbind(Wage3, inv_wt)
    test3 <- apc.indiv.model.table(Wage_wt, dep.var="hasdegree",
                               covariates="logwage", test="Wald",
                               dist="Chisq",
                               model.family="binomial",
                               wt.var="inv_wt")
    test3$table


###################################################
### code chunk number 17: IntroductionIndividualDataFurtherExamples.rnw:282-293
###################################################
library("AER")
data("PSID7682")
period <- as.numeric(PSID7682$year) + 1975
entry <- period - PSID7682$experience
logwage <- log(PSID7682$wage)
inunion <- ifelse(PSID7682$union == "yes", 1, 0)
insouth <- ifelse(PSID7682$south == "yes", 1, 0)
psid2 <- cbind(PSID7682, period, entry, logwage, inunion, insouth)
names(psid2)[names(psid2) %in% c("experience", "entry")] <-
                                                c("age", "cohort")
psid3 <- psid2[psid2$cohort >=1939, ]


###################################################
### code chunk number 18: IntroductionIndividualDataFurtherExamples.rnw:297-303
###################################################
test4 <- apc.indiv.model.table(psid3, dep.var="logwage",
                               covariates = "insouth",
                               plmmodel="random", id.var="id",
                               model.family="gaussian", 
                               test="Wald", dist="Chisq")
test4$table


###################################################
### code chunk number 19: IntroductionIndividualDataFurtherExamples.rnw:307-312
###################################################
test5 <- apc.indiv.model.table(psid3, dep.var="logwage",
                                plmmodel="within", id.var="id",
                                model.family="gaussian", 
                                test="Wald", dist="Chisq")
test5$table


###################################################
### code chunk number 20: IntroductionIndividualDataFurtherExamples.rnw:326-337
###################################################
library("ISLR")
data("Wage")
Wage2 <- Wage[Wage$age >= 25 & Wage$age <= 55, ]
names(Wage2)[names(Wage2) %in% c("year","age")] <- c("period","age")
cohort <- Wage2$period - Wage2$age
indust_job <- ifelse(Wage2$jobclass=="1. Industrial", 1, 0)
hasdegree <- ifelse(Wage2$education %in%
        c("4. College Grad", "5. Advanced Degree"), 1, 0)
married <- ifelse(Wage2$maritl == "2. Married", 1, 0)
Wage3 <- cbind(Wage2, cohort, indust_job, hasdegree, married)
rm(Wage, Wage2, cohort, indust_job, hasdegree, married)


###################################################
### code chunk number 21: IntroductionIndividualDataFurtherExamples.rnw:341-346
###################################################
test1 <- apc.indiv.compare.direct(Wage3, big.model="AP",
                small.model="tP",
                dep.var="logwage", model.family="gaussian",
                test="Wald", dist="F")
test1


###################################################
### code chunk number 22: IntroductionIndividualDataFurtherExamples.rnw:350-355
###################################################
test2 <- apc.indiv.compare.direct(Wage3, big.model="TS",
                small.model="PC",
                dep.var="married", covariates="hasdegree",
                model.family="binomial", test="LR", dist="Chisq")
test2[1:8]


###################################################
### code chunk number 23: IntroductionIndividualDataFurtherExamples.rnw:360-369
###################################################
inv_wt <- runif(nrow(Wage3), 0, 1)
Wage_wt <- cbind(Wage3, inv_wt)
test3 <- apc.indiv.compare.direct(Wage_wt, big.model="APC",
                small.model="P",
                dep.var="logwage", 
                covariates = c("hasdegree", "married"),
                wt.var="inv_wt", test="Wald", dist="Chisq",
                model.family="gaussian")
test3


###################################################
### code chunk number 24: IntroductionIndividualDataFurtherExamples.rnw:377-388
###################################################
library("AER")
data("PSID7682")
period <- as.numeric(PSID7682$year) + 1975
entry <- period - PSID7682$experience
logwage <- log(PSID7682$wage)
inunion <- ifelse(PSID7682$union == "yes", 1, 0)
insouth <- ifelse(PSID7682$south == "yes", 1, 0)
psid2 <- cbind(PSID7682, period, entry, logwage, inunion, insouth)
names(psid2)[names(psid2) %in% c("experience", "entry")] <-
                                                c("age", "cohort")
psid3 <- psid2[psid2$cohort >=1939, ]


###################################################
### code chunk number 25: IntroductionIndividualDataFurtherExamples.rnw:392-398
###################################################
test4 <- apc.indiv.compare.direct(psid3, big.model="Pd",
                    small.model="t",
                    dep.var="logwage", covariates="insouth",
                    plmmodel="random", id.var="id",
                    model.family="gaussian", test="Wald", dist="F")
test4


###################################################
### code chunk number 26: IntroductionIndividualDataFurtherExamples.rnw:402-409
###################################################
test5 <- apc.indiv.compare.direct(psid3, big.model="FAP",
                    small.model="FP",
                    dep.var="logwage", 
                    plmmodel="within", id.var="id",
                    model.family="gaussian", test="Wald", 
                    dist="Chisq")
test5


###################################################
### code chunk number 27: IntroductionIndividualDataFurtherExamples.rnw:419-430
###################################################
library("ISLR")
data("Wage")
Wage2 <- Wage[Wage$age >= 25 & Wage$age <= 55, ]
names(Wage2)[names(Wage2) %in% c("year","age")] <- c("period","age")
cohort <- Wage2$period - Wage2$age
indust_job <- ifelse(Wage2$jobclass=="1. Industrial", 1, 0)
hasdegree <- ifelse(Wage2$education %in%
                c("4. College Grad", "5. Advanced Degree"), 1, 0)
married <- ifelse(Wage2$maritl == "2. Married", 1, 0)
Wage3 <- cbind(Wage2, cohort, indust_job, hasdegree, married)
rm(Wage, Wage2, cohort, indust_job, hasdegree, married)


###################################################
### code chunk number 28: IntroductionIndividualDataFurtherExamples.rnw:434-438
###################################################
library("plyr")
library("apc")
model1 <- apc.indiv.est.model(Wage3, dep.var="logwage")
apc.plot.fit(model1)


