### R code from vignette source 'IntroductionIndividualData.rnw'

###################################################
### code chunk number 1: IntroductionIndividualData.rnw:122-130
###################################################
    library("plyr") 
    library("reshape") 
    library("ISLR")
    data("Wage")
    summary(Wage)
    AP_count <- count(Wage, c("age", "year"))
    AP_show <- cast(AP_count, age~year)
    AP_show[1:10,]


###################################################
### code chunk number 2: IntroductionIndividualData.rnw:138-139
###################################################
    Wage2 <- Wage[Wage$age >= 25 & Wage$age <= 55, ]


###################################################
### code chunk number 3: IntroductionIndividualData.rnw:142-143
###################################################
    names(Wage2)[names(Wage2) %in% c("year","age")] <- c("period","age")


###################################################
### code chunk number 4: IntroductionIndividualData.rnw:146-152
###################################################
    cohort <- Wage2$period - Wage2$age
    indust_job <- ifelse(Wage2$jobclass=="1. Industrial", 1, 0)
    hasdegree <- ifelse(Wage2$education
            %in% c("4. College Grad", "5. Advanced Degree"), 1, 0)
    married <- ifelse(Wage2$maritl == "2. Married", 1, 0)
    Wage3 <- cbind(Wage2, cohort, indust_job, hasdegree, married)


###################################################
### code chunk number 5: IntroductionIndividualData.rnw:166-185
###################################################
    library("ggplot2")
    
    mean_logwage <- ddply(Wage3, .variables=c("period", "age"),
    function(dfr, colnm){mean(dfr[, colnm])}, "logwage")
    names(mean_logwage)[3] <- "Mean_logwage"    
    plot_mean_logwage <- ggplot(mean_logwage, aes(period, age)) + 
    theme_bw() + 
    xlab('\n Period') + 
    ylab('Age\n') +
    geom_tile(aes(fill = Mean_logwage)) +
    scale_fill_gradientn(colours=c("red", "blue"),
    space = 'Lab', name="Mean logwage \n") +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) + 
    theme(axis.text=element_text(size=18),
    axis.title=element_text(size=24, face="bold"),
    legend.title=element_text(size=20, face="bold"),
    legend.key.size = unit(1, "cm"),
    legend.text=element_text(size=18))


###################################################
### code chunk number 6: fig:mean_logwage_RCS
###################################################
    plot_mean_logwage


###################################################
### code chunk number 7: IntroductionIndividualData.rnw:210-216
###################################################
    library("apc")
    library("plyr") 
    library("lmtest") 
    library("car") 
    library("plm")
    library("survey")


###################################################
### code chunk number 8: IntroductionIndividualData.rnw:230-233
###################################################
    logwage_tab <- apc.indiv.model.table(Wage3, dep.var="logwage",
    covariates="hasdegree", model.family="gaussian",
    test="Wald", dist="F", TS=TRUE)


###################################################
### code chunk number 9: logwage_tab
###################################################
    logwage_tab$table


###################################################
### code chunk number 10: IntroductionIndividualData.rnw:251-258
###################################################
    logwage_ad <- apc.indiv.est.model(Wage3, dep.var = "logwage",
    covariates="hasdegree",
    model.family="gaussian",
    model.design="Ad")
    
    logwage_ad$coefficients.covariates
    apc.plot.fit(logwage_ad, main.outer="")


###################################################
### code chunk number 11: IntroductionIndividualData.rnw:281-290
###################################################
    allageDD <- rownames(logwage_ad$coefficients.canonical)[grep("DD_age", 
    rownames(logwage_ad$coefficients.canonical))]
    
    ageDD1 <- allageDD[-1]
    ageDD2 <- allageDD[-length(allageDD)]
    quadratic_hyp <- paste(ageDD2, ageDD1, sep = " = ")
    rm(list=ls(pattern="ageDD"))
    
    linearHypothesis(logwage_ad$fit, quadratic_hyp, test="F")


###################################################
### code chunk number 12: IntroductionIndividualData.rnw:305-311
###################################################
    indust_job_tab <- apc.indiv.model.table(Wage3, dep.var="indust_job",
    covariates="hasdegree",
    model.family="binomial",
    test="LR", dist= "Chisq", TS=TRUE)

    indust_job_tab$table


###################################################
### code chunk number 13: IntroductionIndividualData.rnw:327-332
###################################################
    indust_job_pc <- apc.indiv.est.model(Wage3, dep.var="indust_job",
    covariates="hasdegree", 
    model.family="binomial",
    model.design="PC")
    indust_job_pc$coefficients.covariates


###################################################
### code chunk number 14: fig:indust_job_PC_RCS
###################################################
    apc.plot.fit(indust_job_pc)


###################################################
### code chunk number 15: IntroductionIndividualData.rnw:387-396
###################################################
    library("plyr") 
    library("reshape") 
    library("AER")
    data("PSID7682")
    summary(PSID7682)

    AP_count <- count(PSID7682, c("experience", "year"))
    AP_show <- cast(AP_count, experience~year)
    AP_show[1:10,]


###################################################
### code chunk number 16: IntroductionIndividualData.rnw:402-409
###################################################
    period <- as.numeric(PSID7682$year) + 1975
    entry <- period - PSID7682$experience
    psid <- cbind(PSID7682, period, entry)

    CP_count <- count(psid, c("entry", "year"))
    CP_show <- cast(CP_count, entry~year)
    CP_show[1:10,]


###################################################
### code chunk number 17: IntroductionIndividualData.rnw:417-429
###################################################
    psid2 <- psid[psid$entry >= 1939, ]
    
    # which variables do we want to use?
    logwage <- log(psid2$wage)
    inunion <- ifelse(psid2$union == "yes", 1, 0)
    insouth <- ifelse(psid2$south == "yes", 1, 0)
    bluecollar <- ifelse(psid2$occupation == "blue", 1, 0)
    # also education which is a continuous covariate
    
    psid3 <- cbind(psid2, logwage, inunion, insouth, bluecollar)
    
    names(psid3)[names(psid3) %in% c("experience","entry")] <- c("age","cohort")


###################################################
### code chunk number 18: IntroductionIndividualData.rnw:436-454
###################################################
    library("ggplot2")
    mean_logwage <- ddply(psid3, .variables=c("period", "cohort"),
                      function(dfr, colnm){mean(dfr[, colnm])}, "logwage")
    names(mean_logwage)[3] <- "Mean_logwage"
    plot_mean_logwage <- ggplot(mean_logwage, aes(period, cohort)) + 
      theme_bw() + 
      xlab('\n Period') + 
      ylab('Entry \n') +
      geom_tile(aes(fill = Mean_logwage)) +
      scale_fill_gradientn(colours=c("red", "blue"),
                           space = 'Lab', name="Mean logwage \n") +
      scale_x_continuous(expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) + 
      theme(axis.text=element_text(size=18),
            axis.title=element_text(size=24, face="bold"),
            legend.title=element_text(size=20, face="bold"),
            legend.key.size = unit(1, "cm"),
            legend.text=element_text(size=18))


###################################################
### code chunk number 19: fig:mean_logwage_panel
###################################################
plot_mean_logwage


###################################################
### code chunk number 20: IntroductionIndividualData.rnw:475-479
###################################################
    library(apc)
    panel_tab <- apc.indiv.model.table(psid3, dep.var="logwage",
    model.family = "gaussian", test="Wald", dist="F",
    plmmodel="random", id.var="id")


###################################################
### code chunk number 21: tab:panel_tab
###################################################
    panel_tab$table


###################################################
### code chunk number 22: IntroductionIndividualData.rnw:495-500
###################################################
    panel_apc <- apc.indiv.est.model(psid3, dep.var="logwage",
    model.family="gaussian",
    plmmodel="random", id.var="id")
    
    apc.plot.fit(panel_apc)


###################################################
### code chunk number 23: IntroductionIndividualData.rnw:512-519
###################################################
    panel_tab_fe <- apc.indiv.model.table(psid3, dep.var="logwage",
    covariates = c("inunion", "insouth",
    "bluecollar"),
    model.family = "gaussian", test="Wald", dist="F",
    plmmodel="within", id.var="id")
    
    panel_tab_fe$table


###################################################
### code chunk number 24: IntroductionIndividualData.rnw:524-532
###################################################
    panel_fap <- apc.indiv.est.model(psid3, dep.var="logwage",
    covariates = c("inunion", "insouth",
    "bluecollar", "education"),
    model.family = "gaussian", 
    plmmodel="within", id.var="id",
    model.design="FAP")
    
    panel_fap$coefficients.covariates


###################################################
### code chunk number 25: fig:logwage_fap_panel
###################################################
    apc.plot.fit(panel_fap)


