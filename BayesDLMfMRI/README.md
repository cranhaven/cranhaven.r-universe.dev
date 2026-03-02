# BayesDLMfMRI a R package for Statistical Analysis for Task-Based Fmri Data


## Installation

```{r}
install.packages("remotes")
remotes::install_github("JohnatanLAB/BayesDLMfMRI")
```


## Basic example

```{r}
library(BayesDLMfMRI)
library(oro.nifti)
library(neurobase)


fMRI.data <- get_example_fMRI_data() 
data("covariates", package="BayesDLMfMRI")

res <- ffdEvidenceFETS(ffdc = fMRI.data,
                    covariates = Covariates,
                    m0 = 0, Cova = 100, delta = 0.95,
                    S0 = 1, n0 = 1, Nsimu1 = 100, Cutpos1 = 30,
                    r1 = 1, Test = "LTT", Ncores = 15)
                    
res.auxi <- res[[1]]
data("ffd", package="BayesDLMfMRI") # used for overlay.
Z.visual.c <- nifti(res.auxi, datatype=16)
ortho2(ffd, ifelse(Z.visual.c > 0.95, Z.visual.c, NA),
       col.y = heat.colors(50), ycolorbar = TRUE, ybreaks = seq(0.95, 1, by = 0.001))
```

## Group example

```{r}
library(BayesDLMfMRI)
library(oro.nifti)
library(neurobase)

DataGroups <- get_example_fMRI_data_group()


# load example covaraites and mask
data("covariates", package="BayesDLMfMRI")
data("mask", package="BayesDLMfMRI")

res <- ffdGroupEvidenceFFBS(ffdGroup = DataGroups, covariates = Covariates, 
                            m0=0, Cova=100, delta = 0.95, S0 = 1, n0 = 1, N1 = FALSE, Nsimu1 = 100, 
                            Cutpos = 30, r1 = 1, mask = mask, Ncores = 1)
                            
str(res)        

```
