# metaRMST
An R package for meta-analysis of RMSTD

#### R Installation Instructions
```
install.packages("devtools")
library(devtools)
devtools::install_github("iweir/metaRMST")
library(metaRMST)
```

#### Example 
```
# load library
library(metaRMST)

# read in built-in dataset 
data(AorticStenosisTrials)

# demonstration of meta-analysis to obtain combined effect by multivariate meta-analysis model (method="mvma")
mvma_res <- metaRMSTD(AorticStenosisTrials, time_horizons=c(12,24,36), MA_method="mvma")
mvma_res$REresult

# generate figure:
obj <- RMSTcurves(AorticStenosisTrials, time_horizons=c(12,24,36), tmax=40, nboot=50)
RMSTplot(obj, xlim=c(0,40), ylim=c(-0.25,2.75), yby=0.5, ylab="RMSTD (mos)", xlab="Time (mos)")
```
