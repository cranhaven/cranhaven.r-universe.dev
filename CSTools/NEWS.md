# CSTools 5.2.0 (Release date: 25-01-2024) 

### Development
- New function CST_ChangeDimNames
- CST_SplitDim: added dimension names and split also Dates
- CST_SaveExp: save time bounds and global attributes; improved code

### Other
- Updated README
- Added citation file

# CSTools 5.1.1 (Release date: 19-10-2023) 

### Fixes
- Added startR namespace in all CST_Start calls of the vignettes and sample data.

# CSTools 5.1.0 (Release date: 17-10-2023) 

### Fixes
- Calibration() show warnings in atomic function when multiple cores are used. 
- PlotForecastPDF fix background color in different machines.
- Correct CST_Subset indices for metadata.
- CST_Analogs: Add sdate_dim parameter and improve initial checks.
- Remove CST_Anomaly repeated checks in order to accept no member dimensions in obs.  
- CST_SaveExp developments: improve warning, save metadata correctly. 
- Improve print method in order that time_bounds are correctly displayed.

### Development
- PlotWeeklyClim to allow years outside the reference period.
- PlotWeeklyClim to allow setting y limits.
- New function CST_Start().
- PlotCombinedMap() has upper triangle_end; the color bars can have different breaks.
- New print method.
- CST_MultiEOF development treat spatial NAs.
- Correct PlotCombinedMap error.
- Substitute mentions of CST_Load by CST_Start.
- Modify necessary vignettes to the use of CST_Start.

# CSTools 5.0.1 (Release date: 06-06-2023)  
- Resubmit to CRAN because it was archived due to dependency issue  
  
### Fixes
- Standardize the coordinates of 's2dv_cube' by setting the coordinates to vectors with NULL dimensions  
- Include a check for SaveExp output filenames that only contain date information  
- In SaveExp the dates parameter is allowed to have additional time dimensions of length 1  
- Removed dot from the internal function named mergedatasets() used in CST_CategoricalEnsCombination  
  
### Development
- New parameter 'startdates' in CST_SaveExp used to manually decide the name of the output files  
  
### Other 
- Switch to testthat version 3  

# CSTools 5.0.0 (Release date: 05-04-2023)  

### Fixes 
- Correct vignettes: Analogs, MultiModelSkill and MultivarRMSE  
- Add 'ncores' to s2dv function calls in CST_Anomaly  
- Reduce computing time of examples and tests and improve documentation  

### Development
- Add dat_dim parameter in CST_BiasCorrection and CST_Calibration   
- New plotting function for case studies temporal visualisation: PlotWeeklyClim  
- Deprecate indices in dim_anom parameter of CST_Anomaly  
- Allow memb_dim to be NULL in QuantileMapping  
- Uncomment tests in CST_MultivarRMSE due to correction of RMS in s2dv next release (released s2dv 1.4.0 21/03)  
- New s2dv_cube object development for all the functions, unit tests, examples and vignettes  
- New function CST_Subset similar to Subset with 's2dv_cubes'  
- Improved CST_SaveExp function with new features  
- New color set in PlotForecastPDF Vitigeoss colors  
- New function CST_InsertDim  
  
### Other 
- Added contribution from ArticXchange project due to PlotWeeklyClim  
- Update NEWS.md with the correct format  
- Change Licence  

# CSTools 4.1.1 (Release date: 10-11-2022)  

### Fixes   
- CST_Analogs corrected input of ClimProjDiags::Subset()  
- PlotCombinedMap corrected use of 'cex_bar_titles' parameter  
- CST_Anomaly added 'memb_dim', 'dat_dim' and 'ftime_dim' and improved use for 'dim_anom' parameters  
  
# CSTools 4.1.0 (Release date: 25-10-2022)

### Development   
- Dependency on package 's2dverification' is changed to 's2dv'  
- CST_BiasCorrection new parameters 'memb_dim', 'sdate_dim', 'ncores'  
- CST_Calibration is able to calibrate forecast with new parameter 'exp_cor'  
- CST_QuantileMapping uses cross-validation and provides option to remove NAs; new parameters 'memb_dim', 'sdate_dim', 'window_dim' and 'na.rm'; 'sample_dim' and 'sample_length' are removed  
- s2dv_cube() new parameter 'time_dim'  

### Fixes
- as.s2dv_cube() detects latitude and longitude structure in startR_array object  
- Data correction: 'lonlat_data' is renamed to 'lonlat_temp'; 'lonlat_prec' is corrected by one-day shift  
- Typo and parameter correction in vignette 'MostLikelyTercile_vignette'  
- Figure and result correction in vignette 'RainFARM_vignette'  
- PlotMostLikelyQuantileMap() works with s2dv::PlotLayout  
  
# CSTools 4.0.1 (Release date: 05-10-2021)  

### Development  
- Dynamical Bias Correction method: `CST_ProxiesAttractors` and `CST_DynBiasCorrection` (optionally `Predictability`)  
- CST_BiasCorrection and BiasCorrection allows to calibrate a forecast given the calibration in the hindcast by using parameter 'exp_cor'  
- Use cases  
- CST_SaveExp includes parameter extra_string  
- PlotCombinedMap includes parameter cex_bar_titles  

### Fixes
- Calibration retains correlation absolute value   
- Calibration fixed when cal.methodi == rpc-based, apply_to == sign, eval.method == 'leave-one-out' and the correlation is not significant  
- PlotMostLikelyQuantileMap reoder latitudes of an array provided in 'dots' parameter  

# CSTools 4.0.0 (Release date: 23-02-2021)  

### Development
- ADAMONT downscaling method: requires CST_AdamontAnalogs and CST_AdamontQQCor functions  
- Analogs method using Predictors: requires training_analogs and  CST_AnalogsPredictors  
- PlotPDFsOLE includes parameters to modify legend style  
- CST_RFSlope handless missing values in the temporal dimension and new 'ncores' parameter allows parallel computation  
- CST_RFWeights accepts s2dv_cube objects as input and new 'ncores' paramenter allows parallel computation  
- RFWeights is exposed to users  
- CST_RainFARM accepts multi-dimensional slopes and weights and handless missing values in sample dimensions  
- QuantileMapping is exposed to users  
- CST_MultiMetric includes 'rpss' metric and it is addapted to s2dv  
- PlotMostLikelyQuantileMap vignette  
- PlotTriangles4Categories includes two parameters to adjust axis and margins  
- CategoricalEnsCombination is exposed to users  
- CST_SplitDims includes parameter 'insert_ftime'  
- Analogs vignette  
- Data Storage and retrieval vignette  

### Fixes
- PlotForecastPDF correctly displays terciles labels   
- CST_SaveExp correctly save time units  
- CST_SplitDims returns ordered output following ascending order provided in indices when it is numeric  
- qmap library moved from Imports to Depends  
- CST_QuantileMapping correctly handles exp_cor  
- Figures resize option from vignettes has been removed  
- Fix Analogs to work with three diferent criteria  
- Vignette PlotForecastPDF updated plots  
- Decrease package size compresing vignettes figures and removing areave_data sample  
  
# CSTools 3.1.0 (Release date: 02-07-2020)  

### Development   
- EnsClustering vignette  
- EnsClustering has a new parameter 'time_dim'  
- CST_BiasCorrection has na.rm paramter  
- CST_Anomaly allows to smooth the climatology with filter.span parameter  
- PlotTriangles4Categories new plotting function to convert any 3-d numerical array to a grid of coloured triangles  
- CST_WeatherRegimes/WeatherRegimes and CST_RegimeAssign/RegimeAssign  
- PlotPDFsOLE plots two probability density gaussian functions and the optimal linear estimation  
- CST_RFTemp/RF_Temp functions available for downscaling temperature  
- Weather Regimes vignette  

### Fixes
- CST_Anomaly handles exp, obs or both  
- PlotForecastPDF vignette displays figures correctly  
- Calibration function is exposed to users  
- MultiMetric vignette fixed typo text description  
- RainFARM checks 'slope' is not a vector  
- DESCRIPTION specifies the minimum multiApply version required  
- EnsClustering has a fixed 'closest_member' output  
- PlotCombinedMap handles masks correctly  
- CST_SaveExp uses multiApply and save time dimension correctly  

# CSTools 3.0.0 (Release date: 10-02-2020)  

### Development  
- CST_MergeDims and MergeDims  
- Version working with R 3.4.2  
- PlotForecastPDF handles independent terciles, extremes and observations for each panel  

### Fixes
- CST_Calibration handles missing values  
- BEI functions handle missing values  

# CSTools 2.0.0 (Release date: 25-11-2019)

### Development
- CST_Analogs Analogs downscaling method,   
- CST_MultiEOFS for multiple variables,   
- Ensemble Clustering,  
- Categorical Ensemble Combination,  
- new Calibration methods included in CST_Calibration,   
- Best Estimated Index method,   
- CST_QuantileMapping,  
- CST_SplitDim to split dimension, if it is a temporal dimension, it can be split by days, months and years or other inidices,  
- creation and transformation to class 's2dv_cube',   
- CST_SaveExp function for saving experiments to be loadable with CST_Load,   
- Parallelization of RainFARM downscaling  
- Adding unit tests using testthat for BEI and RainFarm functions  
- New vignette Best Estimate Index  
- Addapting CST_Load to use 'as.s2dv_cube' function  
- Adding reference to S2S4E H2020 project into the DESCRIPTION file  
- Adding NEWS.md file  
  
### Fixes  
- Minor fix in CST_BiasCorrection when checking parameter 'obs'  
- Minor fix in data lonlat_prec to be of class 's2dv_cube'  
- Minor fix in RainFARM vignette  
  
### CSTools 1.0.1 (Release date: 19-06-2019)  

### Fixes and development   
- Correcting test of PlotForecastPDF for compatibility with ggplot2 release  
- New function PlotCombinedMap  
- Adding reference to MEDSCOPE ERA4CS Project into the DESCRIPTION file  
- Documentation minor fix in CST_RFWeights  
- Minor fix in PlotMostLikelyQuantileMap for bar_titles  
- MultiModelSkill vignette updated to use PlotCombinedMap  

### CSTools 1.0.0 (Release date: 24-04-2019)  
- Features included: Load, Anomaly, MultiMetric, MultivarRMSE, Calibration, BiasCorrection, RainFARM Downscaling, PlotForecastPDF, PlotMostLikelyQuantileMap  
- Three sample data: lonlat_data, lonlat_prec, areave_data  
- Unit tests using testthat: BiasCorrection, Calibration, MultiMetric, PlotForecast  
- Vignettes: MultiMetric, Multivar and RainFARM  
