## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  remotes::install_github("mastoffel/partR2")

## ----setup, message=FALSE-----------------------------------------------------
library(partR2)
library(lme4)
data("biomass")
head(biomass)

## ---- warning=FALSE-----------------------------------------------------------
modBM <- lmer(Biomass ~ Year + Temperature + Precipitation + 
             SpeciesDiversity + (1|Population), data = biomass)

## ---- message=FALSE-----------------------------------------------------------
R2_BM <- partR2(modBM, data = biomass, R2_type = "marginal", nboot = 10)
R2_BM

## ---- warning=FALSE-----------------------------------------------------------
R2_BMa <- partR2(modBM, partvars = c("Temperature", "Precipitation"), 
                  R2_type = "marginal", nboot = 10)
R2_BMa

## ---- warning=FALSE-----------------------------------------------------------
summary(R2_BMa)

## ---- warning=FALSE-----------------------------------------------------------
R2_BMb <- partR2(modBM, partvars = c("Temperature", "Precipitation", "Year", 
                                     "SpeciesDiversity"), 
                R2_type = "marginal", max_level = 1, nboot = 10)
R2_BMb

## ---- warning=FALSE-----------------------------------------------------------
R2_BMc <- partR2(modBM, partbatch = list(c("Temperature", "Precipitation")),
                   R2_type = "marginal", nboot = 10)
R2_BMc

## ---- warning=FALSE-----------------------------------------------------------
R2_BMd <- partR2(modBM, partvars = c("SpeciesDiversity"), 
                   partbatch = list(ClimateVars = c("Temperature", "Precipitation")),
                   R2_type = "marginal", nboot = 10)
R2_BMd

## ---- fig.width = 7, fig.height=5---------------------------------------------
library(patchwork)
p1 <- forestplot(R2_BMb, type = "R2", text_size = 10)
p2 <- forestplot(R2_BMb, type = "IR2", text_size = 10)
p3 <- forestplot(R2_BMb, type = "SC", text_size = 10)
p4 <- forestplot(R2_BMb, type = "BW", text_size = 10)
(p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A")

## -----------------------------------------------------------------------------
# An overview
str(R2_BMb, max.level = 1)

## ---- results=FALSE-----------------------------------------------------------
# (a) point estimates and confidence intervals
R2_BMb$R2   # R2s
R2_BMb$SC   # Structure coefficients
R2_BMb$IR2  # inclusive R2s
R2_BMb$BW # Standardised model estimates
R2_BMb$Ests # Model estimates
# (b) bootstrap replicates
R2_BMb$R2_boot
R2_BMb$SC_boot
R2_BMb$IR2_boot
R2_BMb$BW_boot
R2_BMb$Ests_boot

## ---- warning=FALSE-----------------------------------------------------------
R2_BMb$boot_warnings[1:2]
R2_BMb$boot_messages[1:2]

## ---- warning=FALSE-----------------------------------------------------------
biomass$PrecipitationC <- biomass$Precipitation - mean(biomass$Precipitation)
modBMC <- lmer(Biomass ~ Temperature + PrecipitationC + 
               I(PrecipitationC^2) + (1|Population), 
               data = biomass)
R2_BMe <- partR2(modBMC, partvars = c("Temperature"), partbatch=list(c("PrecipitationC", "I(PrecipitationC^2)")), 
                 nboot = 10)
R2_BMe

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(future)
# how many cores do I have?
parallel::detectCores()
# specify plan
# workers can now be set to multiple cores to parallelise 
plan(multisession, workers = 2)
R2_BMf <- partR2(modBM, partvars = c("Temperature", "Precipitation"), 
                        parallel = TRUE, data = biomass)

## ---- fig.width=5, fig.height=3-----------------------------------------------
hist(biomass$Extinction)

## ---- warning=FALSE-----------------------------------------------------------
biomass$YearC <- biomass$Year - mean(biomass$Year)
biomass$TemperatureS <- scale(biomass$Temperature)
biomass$PrecipitationS <- scale(biomass$Precipitation)
biomass$SpeciesDiversityC <- biomass$SpeciesDiversity - mean(biomass$SpeciesDiversity)

modExt <- glmer(Extinction ~ YearC +  TemperatureS + PrecipitationS + 
                SpeciesDiversityC + (1|Population),
                data=biomass, family="poisson")
R2_Ext <- partR2(modExt, partvars = c("TemperatureS", "PrecipitationS"),
                 R2_type = "marginal", nboot=10)
print(R2_Ext, round_to = 3) # rounding decimals in print and summary

## ---- message=FALSE, warning=FALSE--------------------------------------------
modRecov <- glmer(cbind(Recovered, NotRecovered) ~ YearC + 
              TemperatureS + PrecipitationS + SpeciesDiversityC + (1|Population),
              data=biomass, family="binomial")
R2_Recov <- partR2(modRecov, partvars = c("TemperatureS", "PrecipitationS"),
                   R2_type = "marginal", nboot=10)
summary(R2_Recov, round_to=3)

## ---- message=FALSE-----------------------------------------------------------
data(GuineaPigs)
head(GuineaPigs)
GuineaPigs <- subset(GuineaPigs, !is.na(Testo) & !is.na(Rank))
GuineaPigs$TestoTrans <- log(GuineaPigs$Testo)

## ---- warning=FALSE-----------------------------------------------------------
modGP <- lmer(TestoTrans ~ Rank * Time + (1|MaleID), data=GuineaPigs)
R2_modGPa <- partR2(modGP, partvars = c("Rank", "Time", "Rank:Time"), nboot=10)
R2_modGPa

## ---- warning=FALSE-----------------------------------------------------------
R2_modGPb <- partR2(modGP, partbatch = list(c("Rank", "Rank:Time"), 
                                            c("Time", "Rank:Time")),
                        data = GuineaPigs, nboot = 10)
R2_modGPb

## ---- warning=FALSE-----------------------------------------------------------
modGP1 <- lmer(TestoTrans ~ Rank * Time + (1|MaleID), data = GuineaPigs)
R2_GPc_part1 <- partR2(modGP1, partvars = c("Rank:Time"), data = GuineaPigs, 
                       nboot = 10)

modGP2 <- lmer(TestoTrans ~ Rank + Time + (1|MaleID), data = GuineaPigs)
R2_GPc_part2 <- partR2(modGP2, partvars = c("Rank", "Time"), data = GuineaPigs,
                   max_level = 1, nboot = 10)
# the first partR2 object is based on the full model
R2_GPc <- mergeR2(R2_GPc_part1, R2_GPc_part2) 
R2_GPc

## ---- warning=FALSE-----------------------------------------------------------
GuineaPigs$TimeF <- factor(GuineaPigs$Time)
modGPF <- lmer(TestoTrans ~ Rank * TimeF + (1|MaleID), data=GuineaPigs)
R2_modGPFa <- partR2(modGPF, partvars = c("Rank", "TimeF", "Rank:TimeF"), 
                     nboot=10)
R2_modGPFa

## ---- warning=FALSE-----------------------------------------------------------
GuineaPigs <- cbind(GuineaPigs, model.matrix(~ 0 + TimeF, data=GuineaPigs))
GuineaPigs$TimeF2 <- GuineaPigs$TimeF2 - mean(GuineaPigs$TimeF2)
GuineaPigs$TimeF3 <- GuineaPigs$TimeF3 - mean(GuineaPigs$TimeF3)
GuineaPigs$TimeF4 <- GuineaPigs$TimeF4 - mean(GuineaPigs$TimeF4)
GuineaPigs$TimeF5 <- GuineaPigs$TimeF5 - mean(GuineaPigs$TimeF5)

## ---- warning=FALSE-----------------------------------------------------------
batch <- c("TimeF2", "TimeF3", "TimeF4", "TimeF5")
modGPFD <- lmer(TestoTrans ~ (TimeF2 + TimeF3 + TimeF4 + TimeF5) * Rank + 
                (1|MaleID), data=GuineaPigs)
R2_GPFD <- partR2(modGPFD, partvars=c("Rank"), 
                  partbatch=list(TimeF=batch, `TimeF:Rank`=paste0(batch, ":Rank")), 
                  nboot=10)
R2_GPFD

## -----------------------------------------------------------------------------
GuineaPigs$TestoTransS <- scale(GuineaPigs$TestoTrans)
GuineaPigs$TimeS <- scale(GuineaPigs$Time)
GuineaPigs$RankS <- scale(GuineaPigs$Rank)
modGPS <- lmer(TestoTrans ~ RankS * TimeS + (1|MaleID), data = GuineaPigs)

## -----------------------------------------------------------------------------
R2_GPS <- partR2(modGPS, nboot = 10)
summary(R2_GPS, ests = TRUE)

