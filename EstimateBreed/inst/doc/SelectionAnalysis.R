## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(EstimateBreed)

data("maize")
#Extract heterosis and heterobeltiosis
with(maize,het(GEN,GM,GP,PR,REP,param="all"))

#Only extract heterosis
with(maize,het(GEN,GM,GP,PR,REP,param = "het"))

#Extract only heterobeltiosis
with(maize,het(GEN,GM,GP,PR,REP,param = "hetb"))

## -----------------------------------------------------------------------------
library(EstimateBreed)

data("aveia")
# Calculate the industrial yield without extracting the average
with(aveia, rend_ind(GEN,NG2M,MG,MC,RG))

# Calculate the industrial yield by extracting the average per genotype
with(aveia, rend_ind(GEN,NG2M,MG,MC,RG,stat="mean"))

## -----------------------------------------------------------------------------
library(EstimateBreed)

data("trigo")
#Ear viability index
with(trigo,indviab(TEST,NGE,NEE))

#Ear harvest index
with(trigo,indviab(TEST,MGE,ME))

#Spikelet deposition index in the ear
with(trigo,indviab(TEST,NEE,CE))


## -----------------------------------------------------------------------------
library(EstimateBreed)

GEN <- rep(paste("G", 1:5, sep=""), each = 3)
REP <- rep(1:3, times = 5)
MG <- c(78.5, 80.2, 79.1, 81.3, 82.0, 80.8, 76.9, 78.1, 77.5, 83.2,
84.1, 82.9, 77.4, 78.9, 79.3)

data <- data.frame(GEN, REP, MG)

with(data,hw(GEN,MG,crop="trit"))

#Extract the average PH per genotype
with(data,hw(GEN,MG,crop="trit",stat="mean"))

## -----------------------------------------------------------------------------
library(EstimateBreed)

#Obtain environmental deviations
data("desvamb")
head(desvamb)

#Use DPclim for the ISGR function to identify deviations correctly
DPclim <- with(desvamb,desv_clim(ENV,TMED,PREC))

#Calculate the ISGR
data("genot")
head(genot)
isgr_index <- with(genot, isgr(GEN,ENV,NG,MG,CICLO))

#Define the water requirement per stage
isgr_index <- with(genot, isgr(GEN,ENV,NG,MG,CICLO,req=5,stage="rep"))

## -----------------------------------------------------------------------------
library(EstimateBreed)

TEST <- rep(paste("T", 1:5, sep=""), each=3)
REP <- rep(1:3, times=5)
Xi <- rnorm(15, mean=10, sd=2)

data <- data.frame(TEST,REP,Xi)

#Apply the witness variability constraint
Control <- with(data, restr(TEST,REP,Xi,scenario = "restr",zstat = FALSE))

#Apply witness variability restriction with normalization (Z statistic)
Control <- with(data, restr(TEST,REP,Xi,scenario = "restr",zstat = TRUE))

## -----------------------------------------------------------------------------
library(EstimateBreed)

var <- c("A","B","C","D","E")
VF <- c(2.5, 3.0, 2.8, 3.2, 2.7)
VG <- c(1.2, 1.5, 1.3, 1.6, 1.4)
data <- data.frame(var,VG,VF)

#Calculating for just one generation
with(data,COI(var,VG,VF,generation = "F3"))

## -----------------------------------------------------------------------------
library(EstimateBreed)
data("genot2")

#Geting parameters without cheking model assumptions
parameters <- genpar(genot2,Gen,Rep,var =c("VAR1", "VAR2"))
parameters$anova
parameters$gp

#Checking model assumptions
parameters <- genpar(genot2,Gen,Rep,var =c("VAR1", "VAR2"),check=TRUE)
parameters$anova
parameters$gp

