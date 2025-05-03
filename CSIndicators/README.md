CSIndicators
===============

#### Sectoral Indicators for Climate Services Based on Sub-Seasonal to Decadal Climate Predictions

Set of generalised tools for the flexible computation of climate related indicators defined by the user. Each method represents a specific mathematical approach which is combined with the possibility to select an arbitrary time period to define the indicator. This enables a wide range of possibilities to tailor the most suitable indicator for each particular climate service application (agriculture, food security, energy, water management…). This package is intended for sub-seasonal, seasonal and decadal climate predictions, but its methods are also applicable to other time-scales, provided the dimensional structure of the input is maintained. Additionally, the outputs of the functions in this package are compatible with [CSTools](https://earth.bsc.es/gitlab/external/cstools).  

How to cite
-----------

> Pérez-Zanón, N., Ho, A. Chou, C., Lledó, L., Marcos-Matamoros, R., Rifà, E. and González-Reviriego, N. (2023). CSIndicators: Get tailored climate indicators for applications in your sector. Climate Services. https://doi.org/10.1016/j.cliser.2023.100393  

For details in the methodologies see:  

> Pérez-Zanón, N., Caron, L.-P., Terzago, S., Van Schaeybroeck, B., Lledó, L., Manubens, N., Roulin, E., Alvarez-Castro, M. C., Batté, L., Bretonnière, P.-A., Corti, S., Delgado-Torres, C., Domínguez, M., Fabiano, F., Giuntoli, I., von Hardenberg, J., Sánchez-García, E., Torralba, V., and Verfaillie, D.: Climate Services Toolbox (CSTools) v4.0: from climate forecasts to climate forecast information, Geosci. Model Dev., 15, 6115–6142, https://doi.org/10.5194/gmd-15-6115-2022, 2022.  
Chou, C., R. Marcos-Matamoros, L. Palma Garcia, N. Pérez-Zanón, M. Teixeira, S. Silva, N. Fontes, A. Graça, A. Dell'Aquila, S. Calmanti and N. González-Reviriego (2023). Advanced seasonal predictions for vine management based on bioclimatic indicators tailored to the wine sector. Climate Services, 30, 100343, https://doi.org/10.1016/j.cliser.2023.100343.  
Lledó, Ll., V. Torralba, A. Soret, J. Ramon and F.J. Doblas-Reyes (2019). Seasonal forecasts of wind power generation. Renewable Energy, 143, 91-100, https://doi.org/10.1016/j.renene.2019.04.135.

Installation
------------

You can then install the public released version of CSIndicators from CRAN:
```r
install.packages("CSIndicators")
```
Or the development version from the GitLab repository:
```r
# install.packages("devtools")
devtools::install_git("https://earth.bsc.es/gitlab/es/csindicators.git")
```

Overview
--------

To learn how to use the package see:

- [**Agricultural Indicators**](https://CRAN.R-project.org/package=CSIndicators/vignettes/AgriculturalIndicators.html)
- [**Wind Energy Indicators**](https://CRAN.R-project.org/package=CSIndicators/vignettes/EnergyIndicators.html)

Functions documentation can be found [here](https://CRAN.R-project.org/package=CSIndicators/CSIndicators.pdf).

| Function                       | CST version                        | Indicators                      |
|--------------------------------|------------------------------------|---------------------------------|
|[PeriodMean](R/PeriodMean.R)    |CST_PeriodMean                      |GST, SprTX, DTR, BIO1, BIO2      |
|[PeriodMax](R/PeriodMax.R)      |CST_PeriodMax                       |BIO5, BIO13                      |
|[PeriodMin](R/PeriodMin.R)      |PeriodMin                           |BIO6, BIO14                      |
|[PeriodVariance](R/PeriodVariance.R) |CST_PeriodVariance             |BIO4, BIO15                      |
|[PeriodAccumulation](R/PeriodAccumulation.R) |CST_PeriodAccumulation |SprR, HarR, PRCPTOT, BIO16, ...  | 
|[PeriodPET](R/PeriodPET.R)      |CST_PeriodPET                       |PET, SPEI                        | 
|[PeriodStandardization](R/PeriodStandardization.R)    |CST_PeriodStandardization           |SPEI, SPI                        | 
|[AccumulationExceedingThreshold](R/AccumulationExceedingThreshold.R)  |CST_AccumulationExceedingThreshold  |GDD, R95pTOT, R99pTOT            |
|[TotalTimeExceedingThreshold](R/TotalTimeExceedingThreshold.R)     |CST_TotalTimeExceedingThreshold     |SU35, SU, FD, ID, TR, R10mm, Rnmm|
|[TotalSpellTimeExceedingThreshold](R/TotalSpellTimeExceedingThreshold.R)|CST_TotalSpellTimeExceedingThreshold|WSDI, CSDI                       |
|[WindCapacityFactor](R/WindCapacityFactor.R)              |CST_WindCapacityFactor              |Wind Capacity Factor             |
|[WindPowerDensity](R/WindPowerDensity.R)                |CST_WindPowerDensity                |Wind Power Density               |
 
  	
| Auxiliar function | CST version          |
|-------------------|----------------------|
|[AbsToProbs](R/AbsToProbs.R)         |CST_AbsToProbs        |
|[QThreshold](R/QThreshold.R)         |CST_QThreshold        |
|[Threshold](R/Threshold.R)          |CST_Threshold         |
|[MergeRefToExp](R/MergeRefToExp.R)      |CST_MergeRefToExp     |
|[SelectPeriodOnData](R/SelectPeriodOnData.R) |CST_SelectPeriodOnData|
|[SelectPeriodOnDates](R/SelectPeriodOnDates.R)|                      |

Find the current status of each function in [this link](https://docs.google.com/spreadsheets/d/1arqgw-etNPs-XRyMTJ4ekF5YjQxAZBzssxxr2GMXp3c/edit#gid=0).

> **Note I:** the CST version uses 's2dv_cube' objects as inputs and outputs while the former version uses multidimensional arrays with named dimensions as inputs and outputs.

> **Note II:** All functions computing indicators allows to subset a time period if required, although this temporal subsetting can also be done with functions `SelectPeriodOnData` in a separated step. 

#### Object class s2dv_cube

This package is designed to be compatible with other R packages such as [CSTools](https://CRAN.R-project.org/package=CSTools) through a common object: the `s2dv_cube`, used in functions with the prefix **CST**. 

An `s2dv_cube` is an object to store ordered multidimensional array with named dimensions, specific coordinates and stored metadata. As an example, this is how it looks like (see `CSTools::lonlat_temp_st$exp`): 

```r
's2dv_cube'
Data          [ 279.99, 280.34, 279.45, 281.99, 280.92,  ... ] 
Dimensions    ( dataset = 1, var = 1, member = 15, sdate = 6, ftime = 3, lat = 22, lon = 53 ) 
Coordinates  
 * dataset : dat1 
 * var : tas 
   member : 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 
 * sdate : 20001101, 20011101, 20021101, 20031101, 20041101, 20051101 
   ftime : 1, 2, 3 
 * lat : 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, ...
 * lon : 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, ...
Attributes   
   Dates  : 2000-11-01 2001-11-01 2002-11-01 2003-11-01 2004-11-01 ... 
   varName  : tas 
   metadata :  
      lat 
        units : degrees_north 
        long name : latitude 
      lon 
        units : degrees_east 
        long name : longitude 
      ftime 
        units : hours since 2000-11-01 00:00:00 
      tas 
        units : K 
        long name : 2 metre temperature 
   Datasets  : dat1 
   when  : 2023-10-02 10:11:06 
   source_files  : "/ecmwf/system5c3s/monthly_mean/tas_f6h/tas_20001101.nc" ... 
   load_parameters  : 
       ( dat1 )  : dataset = dat1, var = tas, sdate = 20001101 ... 
```

> **Note:** The current `s2dv_cube` object (CSIndicators > 0.0.2 and CSTools > 4.1.1) differs from the original object used in the previous versions of the packages. More information about the `s2dv_cube` object class can be found here: [description of the s2dv_cube object structure document](https://docs.google.com/document/d/1ko37JFl_h6mOjDKM5QSQGikfLBKZq1naL11RkJIwtMM/edit?usp=sharing).

Contribute
----------

1. Open an issue to ask for help or describe a function to be integrated
2. Agree with maintainers (@ngonzal2, @rmarcos, @nperez and @erifarov) on the requirements
3. Create a new branch from master with a meaningful name
4. Once the development is finished, open a merge request to merge the branch on master

> **Note:** Remember to work with multidimensionals arrays with named dimensions when possible and use [multiApply](https://earth.bsc.es/gitlab/ces/multiApply).

#### Add a function

To add a new function in this R package, follow this considerations:

1. Each function exposed to the users should be in separate files in the R folder
2. The name of the function should match the name of the file (e.g.: `Function()` included in file **Function.R**)
3. The documentation should be in roxygen2 format as a header of the function
4. Once, the function and the documentation is finished, run the command `devtools::document()` in your R terminal to automatically generate the **Function.Rd** file
5. Remember to use R 4.1.2 when doing the development 
6. Code format: include spaces between operators (e.g. +, -, &), before and after ','. The maximum length of lines is of 100 characters (hard limit 80 characters). Number of indentation spaces is 2.
7. Functions computing Climate indicators should include a temporal subsetting option. Use the already existing functions to adapt your code. 
