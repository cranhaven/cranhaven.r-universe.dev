# s2dv 2.1.0 (Release date: 2023-09-26)

**Bugfixes**
- CDORemap() crop = T bug fix in R >= 4.2.0
- CDORemap() crop = T bug fix when coordinates are sorted in decreasing order 
- PlotEquiMap() and PlotLayout() create color bar correctly when data have infinite values 
- Correct Corr() output dimensions when dat_dim and memb_dim are NULL 
- NAO(): eliminate ftime_dim check when ftime dimension is not required by the function 
- Histo2Hindcast(): Fill array with NA values for time steps before the initial date
- Add warning for default climatology when ref is null in RMSSS() and MSSS()

**Development**
- NAO(): new parameter "exp_cor" to calculate forecast
- New parameter "abs_threshold" in GetProbs()
- New parameter "return_mean" in RPS() and CRPS()
- New parameter "print_sys_msg" in CDORemap()
- New function SprErr()
- New parameter "alpha" in Bias()
- New parameter "N.eff" in RandomWalkTest()

**Other**
- Add citation file

# s2dv 2.0.0 (Release date: 2023-10-11)
The compability break happens at the parameter changes. All the functionality remains 
the same but please pay attention to the parameter changes like name or default value if some error is raised.

**Bugfixes**
- ColorBar() bug fix for an if condition for warning when var_limits is not provided
- PlotEquiMap() and PlotLayout() are able to plot all NAs maps now.

**Development**
- ACC() remove parameter "space_dim". Use "lat_dim" and "lon_dim" instead.
- ACC(), Ano_CrossValid(), RMS(), Corr(), and RatioSDRMS() parameter "memb_dim" is optional for obs
- Change the default value of the parameter "dat_dim" in all the functions to NULL (except Ano_CrossValid(), Clim(), and Consistent_Trend())
- Change parameter "conf.lev" to "alpha" in all appliable functions
- New function: GetProbs(), MSE(), MSSS()
- RPSS() efficiency improvement
- CDORemap() new parameter "ncores" to use multiple cores
- RMSSS(), RPSS(), CRPSS(), AbsBiasSS() have parameter "sig_method.type" to choose the test type of Random Walk test
- CRPSS() has non-cross-validation climatological forecast
- RPS() and RPSS() have new parameter "na.rm" to set the criterion of NA amount

# s2dv 1.4.1 (Release date: 2023-06-02)
- Resubmit to CRAN because it was archived due to dependency issue.

# s2dv 1.4.0 (Release date: 2023-03-21)
**Bugfixes**  
- AbsBiasSS() significance test bugfix  
- RPSS() significance test bugfix  
- Trend() output "p.val" bugfix when NAs exist  
- RMS() bugfix when dat_dim is NULL and conf is FALSE  
- NAO() parameter "ftime_avg" sanity check improvement  
- CDORemap() recognizes the CDO version with non-numeric values  
- CDORemap() reorders the unlimited dimension to the last position in order to save as netCDF correctly  

**Development**  
- Make the argument default values consistent between functions  
- Season() sanity check improvement   
- RMSSS() new parameters: "ref", "memb_dim", "sig_method". RandomWalkTest() is one option for significance test.   
- Corr() new output "sign" and change parameter "conf.lev" to "alpha"  
- CRPSS() uses cross-validation when `ref` is NULL  
- RPS() and RPSS(): New parameter "cross.val" to choose to use cross-validation or not  
- New function: ROCSS()  
- RandomWalkTest(): New parameters "alpha" and "test.type"; Test method options: 'two.sided.approx','two.sided','greater','less'; change from positively oriented to negatively oriented  
- Reorder(): Reorder attribute "dimensions" along with the data reordering. The attribute exists in Load() objects.  
- ProjectField() efficiency improvement  
- NAO(): parameter "ftime_avg" can be NULL so no average is calculated  

# s2dv 1.3.0 (Release date: 2022-10-17)
- New functions: Bias, AbsBiasSS, CRPS, CRPSS 
- split RPSS parameter 'weights' into 'weights_exp' and 'weights_ref'
- The warning message format is consistent; use internal function .warning() for all the cases
- PlotEquiMap() bugfixes when lon vector is not continuous
- PlotEquiMap() parameter "dots", "varu", "varv", and "contours" array latitude and longitude dimension order is flexible
- PlotLayout(): Add parameter to change subplot title size  
- PlotLayout works with CSTools::PlotMostLikelyQuantileMap()
- Parameter "dat_dim" can be NULL in all functions
- Add "dat_dim" in RPS and RPSS to allow multiple datasets to be calculated     
- DiffCorr: Add two-sided significance test. New param "test.type" to specify the one- or two-sided significance test.

# s2dv 1.2.0 (Release date: 2022-06-22)
- Cluster(): Fix a bug of calculating nclusters ("K"): the function didn't use the whole data to calculate "K" if parameter "nclusters" is NULL.; Add missing output dimension names
- Clim(): Correct the output dimensions for some cases; allow dat_dim to be NULL; obs doesn't need to have dat_dim.
- MeanDims(): if the result is a number and drop = T, return a numeric instead of an array
- Load(): Bugfix for R >= 4.0.0 regarding list and vector confusion
- PlotLayout(): Bugfix when param "var" is a list
- ACC(): Add area-weighting into the calculation and ensure the data has a spatial mean of zero. "space_dim" is deprecated and replaced by "lat_dim" and "lon_dim". "dat_dim" can be NULL.
- PlotEquiMap(): Add useRaster = TRUE in image() if possible (i.e., latitude and longitude are regularly spaced.)
- PlotEquiMap(): New parameters xlonshft ylatshft xlabels ylabels for self-defined axis
- PlotEquiMap(): Flexible map longitude range
- New function: DiffCorr, ResidualCorr, RPS, RPSS
- Clim() and MeanDims() efficiency improvement
- CDORemap(): Add arbitrary time metadata to avoid cdo warning like "Warning (find_time_vars): Time variable >time< not found!"
- CDORemap(): Stop printing messages from cdo command.

# s2dv 1.1.0 (Release date: 2021-12-14)
- New functions: RatioPredictableComponents, SignalNoiseRatio  
- CDORemap(): Able to interpolate irregular grid to regular grid; include new cdo methods 'con2', 'laf' and 'nn'
- PlotEquiMap(): Discard the dependency on 'GEOmap' and 'geomapdata' and only use package 'map' now;
new parameters 'country.borders', 'shapefile', 'shapefile_color', and 'shapefile_lwd' for plotting the national borders and shapefile  
- PlotLayout(): new parameter 'layout_by_rows' for changing the layout order  
- MeanDims(): new parameter 'drop' to choose whether to drop the averaged dimension or not;
Bugfix for making the result as array even if the result is only a number  
- Season(): Add dimension name even if the result is only a number

# s2dv 1.0.0 (Release date: 2021-06-16)
- New functions:
ACC, Ano_CrossValid, BrierScore, CDORemap, Cluster, Consistent_Trend, EOF, EuroAtlanticTC, Filter, Histo2Hindcast, 
NAO, Plot2VarsVsLTime, PlotACC, PlotBoxWhisker, PlotVsLTime, ProbBins, ProjectField, RatioRMS, 
RatioSDRMS, REOF, Spectrum, Spread, StatSeasAtlHurr, UltimateBrier
- Season(): Accept one-dimension input.  
- Persistence(): Add parameters checks for 'start' and 'end'; correct the output 'AR.lowCI' and 'AR.highCI'.  
- Corr(): Add parameter 'member' and 'memb_dim'. They allow the existence of the member dimension
 which can have different length between exp and obs, and users can choose to do the ensemble mean 
first before correlation or calculate the correlation for individual member. 
- InsertDim(): Remove Apply() to improve the efficiency.  
- Reorder(): Improve efficiency.  
- Indices functions take the case without 'memb_dim' into consideration. The climatology calculation for the anomaly is member-dependent if member exists.  
- PlotStereoMap(): Add contour and arrow feature.  
- PlotAno(): Add parameter check for 'sdates'.  
- PlotEquiMap(): Add new arguments 'contour_draw_label', 'lake_color', 'lab_dist_x', 'lab_dist_y', and 'degree_sym'. Fix the border error; the border grids are fully plotted now. Add ocean mask feature.

# s2dv 0.1.1 (Release date: 2020-11-16)
- Change the lincense to Apache License 2.0.
 
# s2dv 0.1.0 (Release date: 2020-11-12)
- New functions: Ano(), Composite(), PlotAno(), Smoothing(), AMV(), GSAT(), SPOD(), TPI(), GMST(), Persistence().
- Change the default value of PlotClim() parameter 'fileout' to NULL.
- Change Regression() parameter 'time_dim' to 'reg_dim', and enable the inputs to be vectors.
- Change Trend() parameter 'time_dim' default value from 'sdate' to 'ftime'.
- Change the default of Season() parameter 'time_dim' from 'sdate' to 'ftime'.
- Bugfix for Regression() na.action. 'na.action = na.fail' was not functional before.
- Add p-value by ANOVA in Trend().
- Bugfix for Trend() slope, detrended, and p-value.
- Change MeanDims() na.rm default to FALSE to be in line with mean().
- Remove unecessary parameter checks in Clim().
- Change parameter 'memb_dim' to 'dat_dim', and the default value from 'member' to 'dat' in Corr(), RMS(), and RMSSS().
- Allow RMS() and RMSSS() to have vector data input.
- Bugfix for Load() when start date and first lead time is not consistent.
- Improve Season() performance by using apply() when 'ncores' is not bigger than 1

# s2dv 0.0.1 (Release date: 2020-02-07)
- The package is the advanced version of package 's2dverification', adopting the regime of package 'multiApply' for all the analytic functions. Most of the other functions for plotting and data retrieval in 's2dverification' are also preserved in this package.
- Because of the adoption of 'multiApply' regime, the functions work well with package 'startR'. 
- All the packages mentioned above are developed by BSC-CNS.

