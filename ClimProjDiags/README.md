## ClimProjDiags

Set of tools to compute various climate indices

### Why should I use it?

The package contains a set of tools to compute metrics and indices for climate analysis. The package provides functions to compute extreme indices, evaluate the agreement between models and combine these models into an ensemble. Multi-model time series of climate indices can be computed either after averaging the 2-D fields from different models provided they share a common grid or by combining time series computed on the model's native grid. Indices can be assigned and/or combined to construct new indices.

### How do I get it?

To install and load the package you can run the next lines in your R session:

```r
install.packages("ClimProjDiags")
library(ClimProjDiags)
```

### How do I use it?

The main functionalities are presented in four different vignettes with step by step examples:

- [Multi-model agreement](https://earth.bsc.es/gitlab/es/ClimProjDiags/blob/master/vignettes/anomaly_agreement.Rmd): computes a projected climate anomaly and provides the percentage of models which agree on the sign of the anomaly.
- [Extreme Indices](https://earth.bsc.es/gitlab/es/ClimProjDiags/blob/master/vignettes/extreme_indices.Rmd):  computes different extreme indices and combine these indices to evalutae how extreme events are projected to change in the future climate.
- [Heat and Cold Wave Duration](https://earth.bsc.es/gitlab/es/ClimProjDiags/blob/master/vignettes/heatcoldwaves.Rmd): computes change in heat wave frequency between present and future conditions.
- [Diurnal temperature variation indicator](https://earth.bsc.es/gitlab/es/ClimProjDiags/blob/master/vignettes/diurnaltemp.Rmd): compares the future projection of diurnal temperature variations with a reference period.


