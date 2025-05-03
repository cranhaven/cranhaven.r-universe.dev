# startR v2.4.0 (Release date: 2024-09-10)
- Allow chunking along inner dimensions that go across file dimensions
- Allow more than one file dimension to be specified in "metadata_dims"
- Add check and warning for when special wildcard "$var$" is missing in the path
- Bugfix: Start() retrieve correct time steps when time is across file dimension and the time steps of the first files are skipped
- Bugfix: Generate correct file paths when a file dimension has multiple depending dimensions

# startR v2.3.1 (Release date: 2023-12-22)
- Use Autosubmit as workflow manager on hub 
- New feature: Collect result by Collect() on HPCs
- Bugfix: Correct Collect_autosubmit() .Rds files update
- Bugfix: Collect() correctly recognize the finished chunk (.Rds file) in local ecFlow folder. Prevent neverending Collect() when using `wait = F` in Compute() and Collect() the result later on

# startR v2.3.0 (Release date: 2023-08-31)     
- Load variable metadata when retreive = F     
- Change Compute() "threads_load" to 1 to be consistent with documentation      
- Add Autosubmit as workflow manager 
- SelectorChecker() to recognize class integer 

# startR v2.2.3 (Release date: 2023-06-06)
- Bugfix in Start(): when using parameter `longitude = 'all'` with transform, there was a missing point for some cases.

# startR v2.2.2 (Release date: 2023-03-24)
- Start(): Bugfix when the input parameters are assigned by a variable with NULL value and retrieve = F 
- NcDataReader(): Bugfix for wrong time attributes return when the unit is "month"

# startR v2.2.1 (Release date: 2022-11-17)
- Reduce warning messages from CDO.  
- Reduce repetitive warning messages from CDORemapper() when single core is used. When multiple cores 
are used, there are still repetitive messages.
- Bugfix in Start() about ClimProjDiags::Subset inputs.
- Bugfix when longitude selector range is very close but not global. The transform indices are correctly selected now.

# startR v2.2.0-2 (Release date: 2022-08-25; internally)
- Use the destination grid to decide which indices to take after interpolation.
- Bugfix when Start() parameter "return_vars" is not used.
- Allow netCDF files to not have calendar attributes (force it to be standard calendar)

# startR v2.2.0-1 (Release date: 2022-04-19; internally)
- Bugfix for the case that the variable has units like time, e.g., "days".  
- Development of metadata reshaping. The metadata should correspond to data if data are reshaped by parameter "merge_across_dims" and "split_multiselected_dims", as well as if data selectors are not continuous indices.  
- Development of multiple dependency by array selector. An inner dimension indices can vary with multiple file dimensions.  

# startR v2.2.0 (Release date: 2022-02-11)  
- License changes to Apache License 2.0  
- R version dependency changes to >= 3.6.0  
- The dependency on s2dverification changes to s2dv  
- The transform parameter "crop" for CDORemapper() is deprecated. It is assigned as a vector of four numbers of the range of latitude and longitude selectors automatically by Start().  
- Chunking the transformed dimensions is available.  
- The transform and reorder function works with selector 'all' and indices() now.  
- Initialize big.matrix in Start() as NA when parameter "ObjectBigmemory" is specified or when the job is submitted to remote machine by Compute().  
- Bugfix of naming the chunks submitted to remote machine. It prevents job crashes when chunk_1 and chunk_11 run at the same time, for example.  
- Adjust time attribute to UTC instead of local time zone, and correct the time calculation according to calendar type and units.  
- The default value of Start() parameter "merge_across_dims_narm" is changed to TRUE.  
- The metadata of startR object is refined. Different datasets are recorded separately.  
- Force return_vars to have value when inner dim has dependency on file dim.  
- Correct the wrong names of return_vars. If the names of return_vars are synonyms, change them back to the inner dim names.  
- When merging one inner dimension across files, Start() can notice the different inner dimension length of files and merge without extra NAs. Need to specify "largest_dims_length = TRUE".  
- Bugfixes for several reshaping problems with different combinations of parameters "merge_across_dims", "merge_across_dims_narm", and "split_multiselected_dims".  
- Modify the dimension consistency check to only check margin dimensions. The target dimensions can have different lengths.  

# startR v2.1.0 (Release date: 2020-10-30)
- Bugfix for metadata retrieving when there are more than one dataset and one of them is missing.
- Bugfix for the Start() parameter 'metadata_dims' is set to non-dat dimension.
- Bugfix for wildcard reading when the Start() parameter 'path_glob_permissive' is used.
- /dev/shm automatic cleaning on Compute(). Solve the error 'No space left on device' which happened when the jobs are aborted.
- Add new paramter 'largest_dims_length' in Start(). It can examine all the files to find the largest inner dimension length. It is useful when certain inner dimension among the files does not have consistent length (e.g., different ensemble number).

# startR v2.0.1 (Release date: 2020-09-10)
- /dev/shm automatic cleaning on Compute()

# startR v2.0.1 (Release date: 2020-08-25)
- Bugfix for the function .chunk(). Its name was chunk() before v2.0.0, and there are two parts 
were not renamed to .chunk() in v2.0.0.
- Bugfix for metadata in the condition that reorder or transform is applied and 'return_vars' is NULL.
- Bugfix for the parameter 'metadata_dims'. It did not work correctly for the cases other than 
'1 data set, 1 variable'. For 1 data set case, all the variables should be listed under $common in
the attributes; for more than 1 data set case, the variables should be listed under each $dat.
- Bugfix for the missing first file case. It showed an error before when the first file is not found but now it works.
- Bugfix for the parameter 'path_glob_permissive' of Start().

# startR v2.0.0 (Release date: 2020-08-06)
- Adopt Roxygen2 documentation format  
- Remove Subset() to avoid duplicated function. Use ClimProjDiags::Subset instead.

# startR v1.0.3 (Release date: 2020-06-19)
- Bugfix for requiring the repetitive values from a single file when using 
'merge_across_dims' and 'split_multiselected_dims'. The value positions were not 
correct before.
- Specify the time zone to be 'UTC' regarding time attributes retrieval. The time zone 
was not specified before and it caused problems when the time crosses daylight saving.
  
# startR v1.0.2 (Release date: 2020-05-11)
- Bugfix for longitude transformation when the required grid point across the borders. The bug apprears at v1.0.0 and v1.0.1.  
- Add one new parameter 'merge_across_dims_narm' in Start(). If it is TRUE,
the additional NAs in the across dimension will be removed. It is useful when 
a continuous time series is required, or parameter 'split_multiselected_dims' is
TRUE and expected dimensions are supposed to have no NAs. 
- Bugfix for the possible mixed dimension problem when 'split_multiselected_dims' and 
'merge_across_dims' are both used. 

# startR v1.0.1 (Release date: 2020-04-21)
- Bugfix for global longitude across the borders.  
- Bugfix for longitude transformation when across the borders.
- Bugfix for transform_extra_cells when across the borders.
- Bugfix for un-reorder longitude transformation crop.
  
# startR v1.0.0 (Release date: 2020-03-23)
- Bugfixes of lat and lon assigned by 'values' in Start(). In v0.1.4 it is incorrect when assigned from big to small values.
- Compatiblity break: Develop longitude and latitude reorder convention. 
The reordering functions (i.e., Sort() and CircularSort()) are well-functioning now.

# startR v0.1.4 (Release date: 2020-02-10)
- Bugfixes of transform in Start(). Change the default value of param 'extra_cells' to 2. (issue37)
- Bugfixes of chunk function in Utils.R (issue23)
- Bugfixes of paramter 'split_multiselected_dims' in ByChunk.R
- Bugfixes for chunking at dimensions which are assigned with list in Start() (issue38)
- Documentation improvement

# startR v0.1.3 (Release date: 2019-08-05)
- Add parameter 'use_attributes' in Step().
- Add paramter 'CDO_module' in Compute(cluster = list()). It is mandatory for using functions which depend on cdo (e.g., s2dverification::CDORemap) in operation.

