# CSIndicators 1.1.2 (Release date: 2025-03-27)

### Fixes
- Avoid using 'builtin' type functions in call to multiApply::Apply()
- CST_PeriodStandardization: Set na.rm = TRUE when replacing infinite values with maximum/minimum period values
- CST_PeriodPET(): Add longname and other variable metadata 
- Not drop singleton time dimensions in 
 PeriodAccumulation
 PeriodMean
 PeriodMax
 PeriodMin
 PeriodVariance
 AccumulationExceedingThreshold
 TotalSpellTimeExceedingThreshold
 TotalTimeExceedingThreshold

### Other
- add CONTRIBUTING.md

# CSIndicators 1.1.1 (Release date: 2024-01-24)  

### Fixes
- Corrected error in SelectPeriodOnDates to allow dates to be transposed

### Other
- Included CITATION file in the pacakge

# CSIndicators 1.1.0 (Release date: 2023-11-20)  

### Fixes  
- Improve CST_PeriodMean() and CST_PeriodAccumulation() in order that Dates from the  s2dv_cube reflect time aggregation.
- Correct output coordinates consistency (coords element) in CST functions
- Include again ClimProjDiags and s2dv dependency due to a dependency issue with an external package.
- Change default value of time_dim to be 'time' in all the function.
- Improve documentation of function MergeRefToExp mentioned the method used.

### New features
- New functions to compute SPEI: PeriodPET, PeriodAccumulation with rolling accumulation and PeriodStandardization.
- New functions to compute bioclimatic indicators: PeriodMax, PeriodMin and PeriodVariance.
- Add 'memb_dim' parameter to MergeRefToExp.
- Add reference and improve documentation in MergeRefToExp.
- Substitute CST_Load by CST_Start in vignettes.
- Include new publication in documentation.
- Change to testthat edition 3.

# CSIndicators 1.0.1 (Release date: 2023-05-18)  

### Fixes  
- Add EnergyIndicators vignette figures  
- Remove ClimProjDiags dependency  
- Remove s2dv dependency  

# CSIndicators 1.0.0 (Release date: 2023-04-05) 

### Fixes
- Correct vignettes figures links.  

### New features
- Exceeding Threshold functions to allow between thresholds or equal threshold options.  
- New s2dv_cube object development for all the functions, unit tests, examples and vignettes.  

# CSIndicators 0.0.2 (Release date: 2022-10-21)  

### Fixes
- Correct figures of EnergyIndicators vignette.  
- Sanity check correction in functions CST_PeriodAccumulation, CST_AbsToProbs, CST_AccumulationExceedingThreshold, CST_MergeRefToExp, CST_PeriodMean, CST_QThreshold, CST_SelectPeriodOnData, CST_Threshold, TotalSpellTimeExceedingThreshold, CST_TotalTimeExceedingThreshold, CST_WindCapacityFactor and CST_WindPowerDensity.  
- Revise examples using s2dv::InsertDim in MergeRefToExp().  

# CSIndicators 0.0.1 (Release date: 2021-05-07)  
- This package is intended for sub-seasonal, seasonal and decadal climate predictions, but its methods are also applicable to other time-scales. Additionally, the outputs of the functions in this package are compatible with 'CSTools'. This package was developed in the context of H2020 MED-GOLD (776467) and S2S4E (776787) projects. Lledó et al. (2019) <doi:10.1016/j.renene.2019.04.135>.  
