## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy.opts=list(width.cutoff=100), tidy=TRUE, comment=NA)
options(width=120, max.print=1000)
oldlc <- Sys.getlocale("LC_COLLATE")
on.exit(Sys.setlocale("LC_COLLATE", oldlc))
Sys.setlocale("LC_COLLATE","C")


## ----getstarted, echo=TRUE, eval=FALSE--------------------------------------------------------------------------------
# # load the library, load and preview at demo dataset
# library(haplo.stats)
# ls(name="package:haplo.stats")
# help(haplo.em)
# example(haplo.em)

## ----rsettingsecho=FALSE, eval=TRUE---------------------------------------------------------------
options(width=100)
rm(list=ls())
require(haplo.stats)

## ----hladat, echo=TRUE----------------------------------------------------------------------------
# load and preview demo dataset stored in ~/haplo.stats/data/hla.demo.tab
data(hla.demo)
names(hla.demo)
# attach hla.demo to make columns available in the session
#attach(hla.demo)

## ----label=makegeno, echo=TRUE--------------------------------------------------------------------
geno <- hla.demo[,c(17,18,21:24)]
genolabel <-c("DQB","DRB","B")

## ----summarygeno, echo=TRUE-----------------------------------------------------------------------
geno.desc <- summaryGeno(geno, miss.val=c(0,NA))
print(geno.desc[c(1:10,80:85,135:140),])

## ----tabledesc, echo=TRUE-------------------------------------------------------------------------
# how many samples missing 2 alleles at the 3 markers
table(geno.desc[,3])

## ----rmmiss, echo=TRUE, eval=FALSE----------------------------------------------------------------
# ## create an index of people missing all alleles
# miss.all <- which(geno.desc[,3]==3)
# 
# # use index to subset hla.demo
# hla.demo.updated <- hla.demo[-miss.all,]

## ----setseed, echo=TRUE---------------------------------------------------------------------------
# this is how to set the seed for reproducing results where haplo.em is 
# involved, and also if simulations are run. In practice, don't reset seed.
seed <- c(17, 53, 1, 40, 37, 0, 62, 56, 5, 52, 12, 1)
set.seed(seed)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----label=runEM, echo=TRUE-----------------------------------------------------------------------
save.em <- haplo.em(geno=geno, locus.label=genolabel, miss.val=c(0,NA))
names(save.em)
print(save.em, nlines=10)

## ----summaryEM, echo=TRUE-------------------------------------------------------------------------
summary(save.em, nlines=7)

## ----summaryEMshow, echo=TRUE---------------------------------------------------------------------
# show full haplotypes, instead of codes
summary(save.em, show.haplo=TRUE, nlines=7)

## ----emcontrol, echo=TRUE, eval=FALSE-------------------------------------------------------------
# # demonstrate only the syntax of control parameters
# save.em.try20 <- haplo.em(geno=geno, locus.label=genolabel, miss.val=c(0, NA),
#      control = haplo.em.control(n.try = 20, insert.batch.size=2))

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----groupBin, echo=TRUE--------------------------------------------------------------------------
## run haplo.em on sub-groups
## create ordinal and binary variables
y.bin <- 1*(hla.demo$resp.cat=="low")
group.bin <- haplo.group(y.bin, geno, locus.label=genolabel, miss.val=0)
print(group.bin, nlines=15)

## ----hapPowerQT, echo=TRUE------------------------------------------------------------------------

# load a set of haplotypes 

data(hapPower.demo) 

#### an example using save.em hla markers may go like this.
# keep <- which(save.em$hap.prob > .004)  # get an index of non-rare haps
# hfreq <- save.em$hap.prob[keep]
# hmat <- save.em$haplotype[keep,]
# hrisk <- which(hmat[,1]==31 & hmat[,2]==11)  # contains 3 haps with freq=.01
# hbase <- 4  # 4th hap has mas freq of .103
####

## separate the haplotype matrix and the frequencies
hmat <- hapPower.demo[,-6]
hfreq <- hapPower.demo[,6]

## Define risk haplotypes as those with "1" allele at loc2 and loc3
hrisk <- which(hmat$loc.2==1 & hmat$loc.3==1)

# define index for baseline haplotype
hbase <-  1

hbeta.list <- find.haplo.beta.qt(haplo=hmat, haplo.freq=hfreq, base.index=hbase, 
                                 haplo.risk=hrisk, r2=.01, y.mu=0, y.var=1)
hbeta.list

ss.qt <- haplo.power.qt(hmat, hfreq, hbase, hbeta.list$beta, 
                        y.mu=0, y.var=1, alpha=.05, power=.80)
ss.qt

power.qt <-  haplo.power.qt(hmat, hfreq, hbase, hbeta.list$beta, 
                        y.mu=0, y.var=1, alpha=.05, sample.size=2826)
power.qt

## ----hapPowerCC, echo=TRUE------------------------------------------------------------------------
## get power and sample size for quantitative response
## get beta vector based on odds ratios

cc.OR <- 1.5

# determine beta regression coefficients for risk haplotypes

hbeta.cc <- numeric(length(hfreq))
hbeta.cc[hrisk] <-  log(cc.OR)

# Compute sample size for stated power

ss.cc <- haplo.power.cc(hmat, hfreq, hbase, hbeta.cc, case.frac=.5, prevalence=.1,
               alpha=.05, power=.8)
ss.cc

# Compute power for given sample size

power.cc <- haplo.power.cc(hmat, hfreq, hbase, hbeta.cc, case.frac=.5, prevalence=.1,
               alpha=.05, sample.size=4566)
power.cc

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----scoregauss, echo=TRUE------------------------------------------------------------------------
# score statistics w/ Gaussian trait
score.gaus.add <- haplo.score(hla.demo$resp, geno, trait.type="gaussian",
                              min.count=5, 
                              locus.label=genolabel, simulate=FALSE)
print(score.gaus.add, nlines=10)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----scorebinom, echo=TRUE------------------------------------------------------------------------
# scores, binary trait
y.bin <- 1*(hla.demo$resp.cat=="low")
score.bin <- haplo.score(y.bin, geno, trait.type="binomial",
                        x.adj = NA, min.count=5,
                        haplo.effect="additive", locus.label=genolabel, 
                        miss.val=0, simulate=FALSE)
print(score.bin, nlines=10)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----scoreOrd, echo=TRUE--------------------------------------------------------------------------
# scores w/ ordinal trait
y.ord <- as.numeric(hla.demo$resp.cat)
score.ord <- haplo.score(y.ord, geno, trait.type="ordinal",
                     x.adj = NA, min.count=5,
                     locus.label=genolabel, 
                     miss.val=0, simulate=FALSE)
print(score.ord, nlines=7)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----scoreAdj, echo=TRUE--------------------------------------------------------------------------
# score w/gaussian, adjusted by covariates
x.ma <- with(hla.demo, cbind(male, age))
score.gaus.adj <- haplo.score(hla.demo$resp, geno, trait.type="gaussian",
                        x.adj = x.ma, min.count=5,
                        locus.label=genolabel, simulate=FALSE)
print(score.gaus.adj, nlines=10)

## ----scorePlot, fig=TRUE, echo=TRUE---------------------------------------------------------------
## plot score vs. frequency, gaussian response
plot(score.gaus.add, pch="o")
 
## locate and label pts with their haplotypes
## works similar to locator() function
#> pts.haplo <- locator.haplo(score.gaus)

pts.haplo <- list(x.coord=c(0.05098, 0.03018, .100), 
                  y.coord=c(2.1582, 0.45725, -2.1566), 
                  hap.txt=c("62:2:7", "51:1:35", "21:3:8"))

text(x=pts.haplo$x.coord, y=pts.haplo$y.coord, labels=pts.haplo$hap.txt)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----scoremin10, echo=TRUE------------------------------------------------------------------------
# increase skip.haplo, expected hap counts = 10  
score.gaus.min10 <- haplo.score(hla.demo$resp, geno, trait.type="gaussian",
                          x.adj = NA, min.count=10,
                          locus.label=genolabel, simulate=FALSE)
print(score.gaus.min10)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----scoreDom, echo=TRUE--------------------------------------------------------------------------
# score w/gaussian, dominant effect

score.gaus.dom <- haplo.score(hla.demo$resp, geno, trait.type="gaussian",
                        x.adj=NA, min.count=5,
                        haplo.effect="dominant", locus.label=genolabel, 
                        simulate=FALSE)
print(score.gaus.dom, nlines=10)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----scorePerm, echo=TRUE-------------------------------------------------------------------------
# simulations when binary response
score.bin.sim <- haplo.score(y.bin, geno, trait.type="binomial",
              x.adj = NA, locus.label=genolabel, min.count=5,
              simulate=TRUE, sim.control = score.sim.control() )
print(score.bin.sim)

## ----glmGeno, echo=TRUE---------------------------------------------------------------------------
# set up data for haplo.glm, include geno.glm, 
# covariates age and male, and responses resp and y.bin
geno <- hla.demo[,c(17,18,21:24)]
geno.glm <- setupGeno(geno, miss.val=c(0,NA), locus.label=genolabel)
attributes(geno.glm)
y.bin <- 1*(hla.demo$resp.cat=="low")
glm.data <- data.frame(geno.glm, age=hla.demo$age, male=hla.demo$male, y=hla.demo$resp, y.bin=y.bin)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----glmGauss, echo=TRUE--------------------------------------------------------------------------
# glm fit with haplotypes, additive gender covariate on gaussian response
fit.gaus <- haplo.glm(y ~ male + geno.glm, family=gaussian, data=glm.data, 
        na.action="na.geno.keep", locus.label = genolabel, x=TRUE,
        control=haplo.glm.control(haplo.freq.min=.02))

summary(fit.gaus)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----glmInter, echo=TRUE--------------------------------------------------------------------------
# glm fit haplotypes with covariate interaction
fit.inter <- haplo.glm(formula = y ~ male * geno.glm, 
                   family = gaussian, data=glm.data, 
                   na.action="na.geno.keep", 
                   locus.label = genolabel, 
                   control = haplo.glm.control(haplo.min.count = 10))
summary(fit.inter)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----glmBinom, echo=TRUE--------------------------------------------------------------------------
# gender and haplotypes fit on binary response, 
# return model matrix
fit.bin <- haplo.glm(y.bin ~ male + geno.glm, family = binomial, 
             data=glm.data, na.action = "na.geno.keep",
             locus.label=genolabel,
             control = haplo.glm.control(haplo.min.count=10))
summary(fit.bin)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----glmDom, echo=TRUE----------------------------------------------------------------------------
# control dominant effect of haplotypes (haplo.effect) 
# by using haplo.glm.control
fit.dom <- haplo.glm(y ~ male + geno.glm, family = gaussian,
       data = glm.data, na.action = "na.geno.keep", 
       locus.label = genolabel, 
       control = haplo.glm.control(haplo.effect='dominant', 
       haplo.min.count=8))

summary(fit.dom)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----glmBaseline, echo=TRUE-----------------------------------------------------------------------
# control baseline selection, perform the same exact run as fit.bin, 
# but different baseline by using haplo.base chosen from haplo.common
fit.bin$haplo.common
fit.bin$haplo.freq.init[fit.bin$haplo.common]
fit.bin.base77 <- haplo.glm(y.bin ~ male + geno.glm, family = binomial,
       data = glm.data, na.action = "na.geno.keep",
       locus.label = genolabel,
       control = haplo.glm.control(haplo.base=77, 
       haplo.min.count=8))
summary(fit.bin.base77)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----glmEPS1, eval=TRUE---------------------------------------------------------------------------
glm.data$ybig <- glm.data$y*50
fit.gausbig <- haplo.glm(formula = ybig ~ male + geno.glm, family = gaussian, 
          data = glm.data, na.action = "na.geno.keep", locus.label = genolabel, 
          control = haplo.glm.control(haplo.freq.min = 0.02), x = TRUE)

summary(fit.gausbig)

fit.gausbig$rank.info
fit.gaus$rank.info

## ----echo=FALSE, eval=TRUE------------------------------------------------------------------------
set.seed(seed)

## ----glmEPS2--------------------------------------------------------------------------------------

fit.gausbig.eps <- haplo.glm(formula = ybig ~ male + geno.glm, family = gaussian, 
          data = glm.data, na.action = "na.geno.keep", locus.label = genolabel, 
          control = haplo.glm.control(eps.svd=1e-10, haplo.freq.min = 0.02), x = TRUE)

summary(fit.gausbig.eps)
fit.gausbig.eps$rank.info

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----glmRare2, echo=TRUE--------------------------------------------------------------------------
## set haplo.freq.min and haplo.min.info to same value to show how the 
## rare coefficient may be modeled but standard error estimate is not 
## calculated because all haps are below haplo.min.info
## warning expected
fit.bin.rare02 <- haplo.glm(y.bin ~ geno.glm, family = binomial,
       data = glm.data, na.action = "na.geno.keep", locus.label = genolabel,
       control = haplo.glm.control(haplo.freq.min=.02, haplo.min.info=.02)) 
summary(fit.bin.rare02)

## ----vcovGLM--------------------------------------------------------------------------------------
varmat <- vcov(fit.gaus)
dim(varmat)

varmat <- vcov(fit.gaus, freq=FALSE)
dim(varmat)
print(varmat, digits=2)

## ----anovaGLM-------------------------------------------------------------------------------------
fit.gaus$lrt

glmfit.gaus <- glm(y~male, family="gaussian", data=glm.data)

anova.haplo.glm(glmfit.gaus, fit.gaus)
anova.haplo.glm(fit.gaus, fit.inter)
anova.haplo.glm(glmfit.gaus, fit.gaus, fit.inter)

## ----scoreMerge,echo=TRUE-------------------------------------------------------------------------
# merge haplo.score and haplo.group results
merge.bin <- haplo.score.merge(score.bin, group.bin)
print(merge.bin, nlines=10)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----hapCC, echo=TRUE-----------------------------------------------------------------------------
# demo haplo.cc where haplo.min.count is specified
# use geno, and this function prepares it for haplo.glm
y.bin <- 1*(hla.demo$resp.cat=="low")
cc.hla <- haplo.cc(y=y.bin, geno=geno, locus.label = genolabel, 
                   control=haplo.glm.control(haplo.freq.min=.02))

print(cc.hla, nlines=25, digits=2)
names(cc.hla)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----label=scoreSlide, echo=TRUE, eval=TRUE-------------------------------------------------------
# haplo.score on 11 loci, slide on 3 consecutive loci at a time
geno.11 <- hla.demo[,-c(1:4)]
label.11 <- c("DPB","DPA","DMA","DMB","TAP1","TAP2","DQB","DQA","DRB","B","A")
score.slide.gaus <- haplo.score.slide(hla.demo$resp, geno.11, trait.type =
                "gaussian", n.slide=3, min.count=5, locus.label=label.11)
print(score.slide.gaus)

## ----label=plotSlide, fig=TRUE, echo=TRUE---------------------------------------------------------
# plot global p-values for sub-haplotypes from haplo.score.slide
plot.haplo.score.slide(score.slide.gaus)

## ----eval=TRUE, echo=FALSE------------------------------------------------------------------------
set.seed(seed)

## ----hapScan, echo=TRUE, eval=TRUE----------------------------------------------------------------
geno.11 <- hla.demo[,-c(1:4)]
y.bin <- 1*(hla.demo$resp.cat=="low")
hla.summary <- summaryGeno(geno.11, miss.val=c(0,NA))

# track those subjects with too many possible haplotype pairs ( > 50,000)
many.haps <- (1:length(y.bin))[hla.summary[,4] > 50000]

# For speed, or even just so it will finish, make y.bin and geno.scan 
# for genotypes that don't have too many ambigous haplotypes
geno.scan <- geno.11[-many.haps,]
y.scan <- y.bin[-many.haps]

# scan haplotypes for regions within width of 3 for each locus.
# test statistic measures difference in haplotype counts in cases and controls
# p-values are simulated for each locus and the maximum statistic, 
# we do 100 simuations here, should use default settings for analysis

scan.hla <- haplo.scan(y.scan, geno.scan, width=3,
        sim.control=score.sim.control(min.sim=100, max.sim=100),
        em.control=haplo.em.control())

print(scan.hla)

## ----label=hapDesign1, echo=TRUE------------------------------------------------------------------
# create a matrix of haplotype effect columns from haplo.em result
hap.effect.frame <- haplo.design(save.em)

names(hap.effect.frame)

hap.effect.frame[1:10,1:8]

## ----label=hapDesign2,echo=TRUE-------------------------------------------------------------------
# create haplotype effect cols for haps 4 and 138
hap4.hap138.frame <- haplo.design(save.em, hapcodes=c(4,138), 
                                 haplo.effect="dominant")

hap4.hap138.frame[1:10,]

dat.glm <- data.frame(resp=hla.demo$resp, male=hla.demo$male, age=hla.demo$age, 
                      hap.4=hap4.hap138.frame$hap.4, 
                      hap.138=hap4.hap138.frame$hap.138)

glm.hap4.hap138 <- glm(resp ~ male + age + hap.4 + hap.138, 
                       family="gaussian", data=dat.glm)
summary(glm.hap4.hap138)

