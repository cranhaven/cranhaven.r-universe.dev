#'Loads Experimental And Observational Data
#'
#'This function loads monthly or daily data from a set of specified 
#'experimental datasets together with data that date-corresponds from a set 
#'of specified observational datasets. See parameters 'storefreq', 
#''sampleperiod', 'exp' and 'obs'.\cr\cr
#'A set of starting dates is specified through the parameter 'sdates'. Data of 
#'each starting date is loaded for each model.
#'\code{Load()} arranges the data in two arrays with a similar format both 
#'with the following dimensions:
#'  \enumerate{
#'    \item{The number of experimental datasets determined by the user through 
#'    the argument 'exp' (for the experimental data array) or the number of 
#'    observational datasets available for validation (for the observational 
#'    array) determined as well by the user through the argument 'obs'.}
#'    \item{The greatest number of members across all experiments (in the 
#'    experimental data array) or across all observational datasets (in the 
#'    observational data array).}
#'    \item{The number of starting dates determined by the user through the 
#'    'sdates' argument.}
#'    \item{The greatest number of lead-times.}
#'    \item{The number of latitudes of the selected zone.}
#'    \item{The number of longitudes of the selected zone.}
#'  }
#'Dimensions 5 and 6 are optional and their presence depends on the type of 
#'the specified variable (global mean or 2-dimensional) and on the selected 
#'output type (area averaged time series, latitude averaged time series, 
#'longitude averaged time series or 2-dimensional time series).\cr
#'In the case of loading an area average the dimensions of the arrays will be 
#'only the first 4.\cr\cr
#'Only a specified variable is loaded from each experiment at each starting 
#'date. See parameter 'var'.\cr
#'Afterwards, observational data that matches every starting date and lead-time 
#'of every experimental dataset is fetched in the file system (so, if two 
#'predictions at two different start dates overlap, some observational values 
#'will be loaded and kept in memory more than once).\cr
#'If no data is found in the file system for an experimental or observational 
#'array point it is filled with an NA value.\cr\cr
#'If the specified output is 2-dimensional or latitude- or longitude-averaged 
#'time series all the data is interpolated into a common grid. If the 
#'specified output type is area averaged time series the data is averaged on 
#'the individual grid of each dataset but can also be averaged after 
#'interpolating into a common grid. See parameters 'grid' and 'method'.\cr
#'Once the two arrays are filled by calling this function, other functions in 
#'the s2dv package that receive as inputs data formatted in this 
#'data structure can be executed (e.g: \code{Clim()} to compute climatologies, 
#'\code{Ano()} to compute anomalies, ...).\cr\cr
#'Load() has many additional parameters to disable values and trim dimensions 
#'of selected variable, even masks can be applied to 2-dimensional variables. 
#'See parameters 'nmember', 'nmemberobs', 'nleadtime', 'leadtimemin', 
#''leadtimemax', 'sampleperiod', 'lonmin', 'lonmax', 'latmin', 'latmax', 
#''maskmod', 'maskobs', 'varmin', 'varmax'.\cr\cr
#'The parameters 'exp' and 'obs' can take various forms. The most direct form 
#'is a list of lists, where each sub-list has the component 'path' associated 
#'to a character string with a pattern of the path to the files of a dataset 
#'to be loaded. These patterns can contain wildcards and tags that will be 
#'replaced automatically by \code{Load()} with the specified starting dates, 
#'member numbers, variable name, etc.\cr
#'See parameter 'exp' or 'obs' for details.\cr\cr
#'Only NetCDF files are supported. OPeNDAP URLs to NetCDF files are also 
#'supported.\cr
#'\code{Load()} can load 2-dimensional or global mean variables in any of the 
#'following formats:
#'  \itemize{
#'    \item{experiments:
#'      \itemize{
#'        \item{file per ensemble per starting date 
#'        (YYYY, MM and DD somewhere in the path)}
#'        \item{file per member per starting date 
#'        (YYYY, MM, DD and MemberNumber somewhere in the path. Ensemble 
#'        experiments with different numbers of members can be loaded in 
#'        a single \code{Load()} call.)}
#'      }
#'    (YYYY, MM and DD specify the starting dates of the predictions)
#'    }
#'    \item{observations:
#'      \itemize{
#'        \item{file per ensemble per month 
#'        (YYYY and MM somewhere in the path)}
#'        \item{file per member per month 
#'        (YYYY, MM and MemberNumber somewhere in the path, obs with different 
#'        numbers of members supported)}
#'        \item{file per dataset (No constraints in the path but the time axes 
#'        in the file have to be properly defined)}
#'      }
#'    (YYYY and MM correspond to the actual month data in the file)
#'    }
#'  }
#'In all the formats the data can be stored in a daily or monthly frequency, 
#'or a multiple of these (see parameters 'storefreq' and 'sampleperiod').\cr
#'All the data files must contain the target variable defined over time and 
#'potentially over members, latitude and longitude dimensions in any order, 
#'time being the record dimension.\cr
#'In the case of a two-dimensional variable, the variables longitude and 
#'latitude must be defined inside the data file too and must have the same 
#'names as the dimension for longitudes and latitudes respectively.\cr
#'The names of these dimensions (and longitude and latitude variables) and the 
#'name for the members dimension are expected to be 'longitude', 'latitude' 
#'and 'ensemble' respectively. However, these names can be adjusted with the 
#'parameter 'dimnames' or can be configured in the configuration file (read 
#'below in parameters 'exp', 'obs' or see \code{?ConfigFileOpen} 
#'for more information.\cr
#'All the data files are expected to have numeric values representable with 
#'32 bits. Be aware when choosing the fill values or infinite values in the 
#'datasets to load.\cr\cr
#'The Load() function returns a named list following a structure similar to 
#'the used in the package 'downscaleR'.\cr
#'The components are the following:
#'  \itemize{
#'    \item{'mod' is the array that contains the experimental data. It has the 
#'    attribute 'dimensions' associated to a vector of strings with the labels 
#'    of each dimension of the array, in order.}
#'    \item{'obs' is the array that contains the observational data. It has 
#'    the attribute 'dimensions' associated to a vector of strings with the 
#'    labels of each dimension of the array, in order.}
#'    \item{'obs' is the array that contains the observational data.}
#'    \item{'lat' and 'lon' are the latitudes and longitudes of the grid into 
#'    which the data is interpolated (0 if the loaded variable is a global 
#'    mean or the output is an area average).\cr
#'    Both have the attribute 'cdo_grid_des' associated with a character
#'    string with the name of the common grid of the data, following the CDO 
#'    naming conventions for grids.\cr
#'    The attribute 'projection' is kept for compatibility with 'downscaleR'.
#'    }
#'    \item{'Variable' has the following components:
#'      \itemize{
#'        \item{'varName', with the short name of the loaded variable as 
#'        specified in the parameter 'var'.}
#'        \item{'level', with information on the pressure level of the variable. 
#'        Is kept to NULL by now.}
#'      }
#'    And the following attributes:
#'      \itemize{
#'        \item{'is_standard', kept for compatibility with 'downscaleR', 
#'        tells if a dataset has been homogenized to standards with 
#'        'downscaleR' catalogs.}
#'        \item{'units', a character string with the units of measure of the 
#'        variable, as found in the source files.}
#'        \item{'longname', a character string with the long name of the 
#'        variable, as found in the source files.}
#'        \item{'daily_agg_cellfun', 'monthly_agg_cellfun', 'verification_time', 
#'        kept for compatibility with 'downscaleR'.}
#'      }
#'    }
#'    \item{'Datasets' has the following components:
#'      \itemize{
#'        \item{'exp', a named list where the names are the identifying 
#'        character strings of each experiment in 'exp', each associated to a 
#'        list with the following components:
#'          \itemize{
#'            \item{'members', a list with the names of the members of the 
#'            dataset.}
#'            \item{'source', a path or URL to the source of the dataset.}
#'          }
#'        }
#'        \item{'obs', similar to 'exp' but for observational datasets.}
#'      }
#'    }
#'    \item{'Dates', with the follwing components:
#'      \itemize{
#'        \item{'start', an array of dimensions (sdate, time) with the POSIX 
#'        initial date of each forecast time of each starting date.} 
#'        \item{'end', an array of dimensions (sdate, time) with the POSIX 
#'        final date of each forecast time of each starting date.}
#'      }
#'    }
#'    \item{'InitializationDates', a vector of starting dates as specified in 
#'    'sdates', in POSIX format.}
#'    \item{'when', a time stamp of the date the \code{Load()} call to obtain 
#'    the data was issued.}
#'    \item{'source_files', a vector of character strings with complete paths 
#'    to all the found files involved in the \code{Load()} call.}
#'    \item{'not_found_files', a vector of character strings with complete 
#'    paths to not found files involved in the \code{Load()} call.}
#'  }
#'
#'@param var Short name of the variable to load. It should coincide with the 
#'  variable name inside the data files.\cr
#'  E.g.: \code{var = 'tos'}, \code{var = 'tas'}, \code{var = 'prlr'}.\cr
#'  In some cases, though, the path to the files contains twice or more times 
#'  the short name of the variable but the actual name of the variable inside 
#'  the data files is different. In these cases it may be convenient to provide 
#'  \code{var} with the name that appears in the file paths (see details on 
#'  parameters \code{exp} and \code{obs}).
#'@param exp Parameter to specify which experimental datasets to load data 
#'  from.\cr
#'  It can take two formats: a list of lists or a vector of character strings. 
#'  Each format will trigger a different mechanism of locating the requested 
#'  datasets.\cr
#'  The first format is adequate when loading data you'll only load once or 
#'  occasionally. The second format is targeted to avoid providing repeatedly 
#'  the information on a certain dataset but is more complex to use.\cr\cr
#'  IMPORTANT: Place first the experiment with the largest number of members 
#'  and, if possible, with the largest number of leadtimes. If not possible, 
#'  the arguments 'nmember' and/or 'nleadtime' should be filled to not miss 
#'  any member or leadtime.\cr
#'  If 'exp' is not specified or set to NULL, observational data is loaded for 
#'  each start-date as far as 'leadtimemax'. If 'leadtimemax' is not provided, 
#'  \code{Load()} will retrieve data of a period of time as long as the time 
#'  period between the first specified start date and the current date.\cr\cr
#'  List of lists:\cr
#'  A list of lists where each sub-list contains information on the location 
#'  and format of the data files of the dataset to load.\cr
#'  Each sub-list can have the following components:
#'    \itemize{
#'      \item{'name': A character string to identify the dataset. Optional.}
#'      \item{'path': A character string with the pattern of the path to the 
#'        files of the dataset. This pattern can be built up making use of some 
#'        special tags that \code{Load()} will replace with the appropriate 
#'        values to find the dataset files. The allowed tags are $START_DATE$, 
#'        $YEAR$, $MONTH$, $DAY$, $MEMBER_NUMBER$, $STORE_FREQ$, $VAR_NAME$, 
#'        $EXP_NAME$ (only for experimental datasets), $OBS_NAME$ (only for 
#'        observational datasets) and $SUFFIX$\cr
#'        Example: /path/to/$EXP_NAME$/postprocessed/$VAR_NAME$/\cr
#'         $VAR_NAME$_$START_DATE$.nc\cr
#'        If 'path' is not specified and 'name' is specified, the dataset 
#'        information will be fetched with the same mechanism as when using 
#'        the vector of character strings (read below).
#'      }
#'      \item{'nc_var_name': Character string with the actual variable name 
#'        to look for inside the dataset files. Optional. Takes, by default, 
#'        the same value as the parameter 'var'.
#'      }
#'      \item{'suffix': Wildcard character string that can be used to build 
#'        the 'path' of the dataset. It can be accessed with the tag $SUFFIX$. 
#'        Optional. Takes '' by default.
#'      }
#'      \item{'var_min': Important: Character string. Minimum value beyond 
#'        which read values will be deactivated to NA. Optional. No deactivation 
#'        is performed by default.
#'      }
#'      \item{'var_max': Important: Character string. Maximum value beyond 
#'        which read values will be deactivated to NA. Optional. No deactivation 
#'        is performed by default.
#'      }
#'    }
#'  The tag $START_DATES$ will be replaced with all the starting dates 
#'  specified in 'sdates'. $YEAR$, $MONTH$ and $DAY$ will take a value for each 
#'  iteration over 'sdates', simply these are the same as $START_DATE$ but 
#'  split in parts.\cr
#'  $MEMBER_NUMBER$ will be replaced by a character string with each member 
#'  number, from 1 to the value specified in the parameter 'nmember' (in 
#'  experimental datasets) or in 'nmemberobs' (in observational datasets). It 
#'  will range from '01' to 'N' or '0N' if N < 10.\cr
#'  $STORE_FREQ$ will take the value specified in the parameter 'storefreq' 
#'  ('monthly' or 'daily').\cr
#'  $VAR_NAME$ will take the value specified in the parameter 'var'.\cr
#'  $EXP_NAME$ will take the value specified in each component of the parameter 
#'  'exp' in the sub-component 'name'.\cr
#'  $OBS_NAME$ will take the value specified in each component of the parameter 
#'  'obs' in the sub-component 'obs.\cr
#'  $SUFFIX$ will take the value specified in each component of the parameters 
#'  'exp' and 'obs' in the sub-component 'suffix'.\cr
#'  Example:
#'  \preformatted{
#'  list(
#'    list(
#'      name = 'experimentA',
#'      path = file.path('/path/to/$DATASET_NAME$/$STORE_FREQ$',
#'                       '$VAR_NAME$$SUFFIX$',
#'                       '$VAR_NAME$_$START_DATE$.nc'),
#'      nc_var_name = '$VAR_NAME$',
#'      suffix = '_3hourly',
#'      var_min = '-1e19',
#'      var_max = '1e19'
#'    )
#'  )
#'  }
#'  This will make \code{Load()} look for, for instance, the following paths, 
#'  if 'sdates' is c('19901101', '19951101', '20001101'):\cr
#'    /path/to/experimentA/monthly_mean/tas_3hourly/tas_19901101.nc\cr
#'    /path/to/experimentA/monthly_mean/tas_3hourly/tas_19951101.nc\cr
#'    /path/to/experimentA/monthly_mean/tas_3hourly/tas_20001101.nc\cr\cr
#'  Vector of character strings:
#'  To avoid specifying constantly the same information to load the same 
#'  datasets, a vector with only the names of the datasets to load can be 
#'  specified.\cr
#'  \code{Load()} will then look for the information in a configuration file 
#'  whose path must be specified in the parameter 'configfile'.\cr
#'  Check \code{?ConfigFileCreate}, \code{ConfigFileOpen}, 
#'  \code{ConfigEditEntry} & co. to learn how to create a new configuration 
#'  file and how to add the information there.\cr
#'  Example: c('experimentA', 'experimentB')
#' 
#'@param obs Argument with the same format as parameter 'exp'. See details on 
#'  parameter 'exp'.\cr
#'  If 'obs' is not specified or set to NULL, no observational data is loaded.\cr
#'@param sdates Vector of starting dates of the experimental runs to be loaded 
#'  following the pattern 'YYYYMMDD'.\cr
#'  This argument is mandatory.\cr
#'  E.g. c('19601101', '19651101', '19701101')
#'@param nmember Vector with the numbers of members to load from the specified 
#'  experimental datasets in 'exp'.\cr
#'  If not specified, the automatically detected number of members of the 
#'  first experimental dataset is detected and replied to all the experimental 
#'  datasets.\cr
#'  If a single value is specified it is replied to all the experimental 
#'  datasets.\cr
#'  Data for each member is fetched in the file system. If not found is 
#'  filled with NA values.\cr
#'  An NA value in the 'nmember' list is interpreted as "fetch as many members 
#'  of each experimental dataset as the number of members of the first 
#'  experimental dataset".\cr
#'  Note: It is recommended to specify the number of members of the first 
#'  experimental dataset if it is stored in file per member format because 
#'  there are known issues in the automatic detection of members if the path 
#'  to the dataset in the configuration file contains Shell Globbing wildcards 
#'  such as '*'.\cr
#'  E.g., c(4, 9) 
#'@param nmemberobs Vector with the numbers of members to load from the 
#'  specified observational datasets in 'obs'.\cr
#'  If not specified, the automatically detected number of members of the 
#'  first observational dataset is detected and replied to all the 
#'  observational datasets.\cr
#'  If a single value is specified it is replied to all the observational 
#'  datasets.\cr
#'  Data for each member is fetched in the file system. If not found is 
#'  filled with NA values.\cr
#'  An NA value in the 'nmemberobs' list is interpreted as "fetch as many 
#'  members of each observational dataset as the number of members of the 
#'  first observational dataset".\cr
#'  Note: It is recommended to specify the number of members of the first 
#'  observational dataset if it is stored in file per member format because 
#'  there are known issues in the automatic detection of members if the path 
#'  to the dataset in the configuration file contains Shell Globbing wildcards 
#'  such as '*'.\cr
#'  E.g., c(1, 5)
#'@param nleadtime Deprecated. See parameter 'leadtimemax'.
#'@param leadtimemin Only lead-times higher or equal to 'leadtimemin' are 
#'  loaded. Takes by default value 1.
#'@param leadtimemax Only lead-times lower or equal to 'leadtimemax' are loaded. 
#'  Takes by default the number of lead-times of the first experimental 
#'  dataset in 'exp'.\cr
#'  If 'exp' is NULL this argument won't have any effect 
#'  (see \code{?Load} description).
#'@param storefreq Frequency at which the data to be loaded is stored in the 
#'  file system. Can take values 'monthly' or 'daily'.\cr
#'  By default it takes 'monthly'.\cr
#'  Note: Data stored in other frequencies with a period which is divisible by 
#'  a month can be loaded with a proper use of 'storefreq' and 'sampleperiod' 
#'  parameters. It can also be loaded if the period is divisible by a day and 
#'  the observational datasets are stored in a file per dataset format or 
#'  'obs' is empty.
#'@param sampleperiod To load only a subset between 'leadtimemin' and 
#'  'leadtimemax' with the period of subsampling 'sampleperiod'.\cr
#'  Takes by default value 1 (all lead-times are loaded).\cr
#'  See 'storefreq' for more information.
#'@param lonmin If a 2-dimensional variable is loaded, values at longitudes 
#'  lower than 'lonmin' aren't loaded.\cr
#'  Must take a value in the range [-360, 360] (if negative longitudes are 
#'  found in the data files these are translated to this range).\cr
#'  It is set to 0 if not specified.\cr
#'  If 'lonmin' > 'lonmax', data across Greenwich is loaded.
#'@param lonmax If a 2-dimensional variable is loaded, values at longitudes 
#'  higher than 'lonmax' aren't loaded.\cr
#'  Must take a value in the range [-360, 360] (if negative longitudes are 
#'  found in the data files these are translated to this range).\cr
#'  It is set to 360 if not specified.\cr
#'  If 'lonmin' > 'lonmax', data across Greenwich is loaded.
#'@param latmin If a 2-dimensional variable is loaded, values at latitudes 
#'  lower than 'latmin' aren't loaded.\cr
#'  Must take a value in the range [-90, 90].\cr
#'  It is set to -90 if not specified.
#'@param latmax If a 2-dimensional variable is loaded, values at latitudes 
#'  higher than 'latmax' aren't loaded.\cr
#'  Must take a value in the range [-90, 90].\cr
#'  It is set to 90 if not specified.
#'@param output This parameter determines the format in which the data is 
#'  arranged in the output arrays.\cr
#'  Can take values 'areave', 'lon', 'lat', 'lonlat'.\cr
#'    \itemize{
#'      \item{'areave': Time series of area-averaged variables over the specified domain.}
#'      \item{'lon': Time series of meridional averages as a function of longitudes.}
#'      \item{'lat': Time series of zonal averages as a function of latitudes.}
#'      \item{'lonlat': Time series of 2d fields.}
#'  }
#'  Takes by default the value 'areave'. If the variable specified in 'var' is 
#'  a global mean, this parameter is forced to 'areave'.\cr
#'  All the loaded data is interpolated into the grid of the first experimental 
#'  dataset except if 'areave' is selected. In that case the area averages are 
#'  computed on each dataset original grid. A common grid different than the 
#'  first experiment's can be specified through the parameter 'grid'. If 'grid' 
#'  is specified when selecting 'areave' output type, all the loaded data is 
#'  interpolated into the specified grid before calculating the area averages.
#'@param method This parameter determines the interpolation method to be used 
#'  when regridding data (see 'output'). Can take values 'bilinear', 'bicubic', 
#'  'conservative', 'distance-weighted'.\cr
#'  See \code{remapcells} for advanced adjustments.\cr
#'  Takes by default the value 'conservative'.
#'@param grid A common grid can be specified through the parameter 'grid' when 
#'  loading 2-dimensional data. Data is then interpolated onto this grid 
#'  whichever 'output' type is specified. If the selected output type is 
#'  'areave' and a 'grid' is specified, the area averages are calculated after 
#'  interpolating to the specified grid.\cr
#'  If not specified and the selected output type is 'lon', 'lat' or 'lonlat', 
#'  this parameter takes as default value the grid of the first experimental 
#'  dataset, which is read automatically from the source files.\cr
#'  Note that the auto-detected grid type is not guarenteed to be correct, and
#'  it won't be correct if the netCDF file doesn't contain global domain.
#'  Please check the warning carefully to ensure the detected grid type is 
#'  expected, or assign this parameter even regridding is not needed.
#'  The grid must be supported by 'cdo' tools. Now only supported: rNXxNY 
#'  or tTRgrid.\cr
#'  Both rNXxNY and tRESgrid yield rectangular regular grids. rNXxNY yields 
#'  grids that are evenly spaced in longitudes and latitudes (in degrees). 
#'  tRESgrid refers to a grid generated with series of spherical harmonics 
#'  truncated at the RESth harmonic. However these spectral grids are usually 
#'  associated to a gaussian grid, the latitudes of which are spaced with a 
#'  Gaussian quadrature (not evenly spaced in degrees). The pattern tRESgrid 
#'  will yield a gaussian grid.\cr
#'  E.g., 'r96x72' 
#'  Advanced: If the output type is 'lon', 'lat' or 'lonlat' and no common 
#'  grid is specified, the grid of the first experimental or observational 
#'  dataset is detected and all data is then interpolated onto this grid. 
#'  If the first experimental or observational dataset's data is found shifted 
#'  along the longitudes (i.e., there's no value at the longitude 0 but at a 
#'  longitude close to it), the data is re-interpolated to suppress the shift. 
#'  This has to be done in order to make sure all the data from all the 
#'  datasets is properly aligned along longitudes, as there's no option so far 
#'  in \code{Load} to specify grids starting at longitudes other than 0. 
#'  This issue doesn't affect when loading in 'areave' mode without a common 
#'  grid, the data is not re-interpolated in that case.
#'@param maskmod List of masks to be applied to the data of each experimental 
#'  dataset respectively, if a 2-dimensional variable is specified in 'var'.\cr
#'  Each mask can be defined in 2 formats:\cr
#'  a) a matrix with dimensions c(longitudes, latitudes).\cr
#'  b) a list with the components 'path' and, optionally, 'nc_var_name'.\cr
#'  In the format a), the matrix must have the same size as the common grid 
#'  or with the same size as the grid of the corresponding experimental dataset 
#'  if 'areave' output type is specified and no common 'grid' is specified.\cr
#'  In the format b), the component 'path' must be a character string with the 
#'  path to a NetCDF mask file, also in the common grid or in the grid of the 
#'  corresponding dataset if 'areave' output type is specified and no common 
#'  'grid' is specified. If the mask file contains only a single variable, 
#'  there's no need to specify the component 'nc_var_name'. Otherwise it must 
#'  be a character string with the name of the variable inside the mask file 
#'  that contains the mask values. This variable must be defined only over 2 
#'  dimensions with length greater or equal to 1.\cr
#'  Whichever the mask format, a value of 1 at a point of the mask keeps the 
#'  original value at that point whereas a value of 0 disables it (replaces 
#'  by a NA value).\cr
#'  By default all values are kept (all ones).\cr
#'  The longitudes and latitudes in the matrix must be in the same order as in 
#'  the common grid or as in the original grid of the corresponding dataset 
#'  when loading in 'areave' mode. You can find out the order of the longitudes 
#'  and latitudes of a file with 'cdo griddes'.\cr
#'  Note that in a common CDO grid defined with the patterns 't<RES>grid' or 
#'  'r<NX>x<NY>' the latitudes and latitudes are ordered, by definition, from 
#'  -90 to 90 and from 0 to 360, respectively.\cr
#'  If you are loading maps ('lonlat', 'lon' or 'lat' output types) all the 
#'  data will be interpolated onto the common 'grid'. If you want to specify 
#'  a mask, you will have to provide it already interpolated onto the common 
#'  grid (you may use 'cdo' libraries for this purpose). It is not usual to 
#'  apply different masks on experimental datasets on the same grid, so all 
#'  the experiment masks are expected to be the same.\cr
#'  Warning: When loading maps, any masks defined for the observational data 
#'  will be ignored to make sure the same mask is applied to the experimental 
#'  and observational data.\cr
#'  Warning: list() compulsory even if loading 1 experimental dataset only!\cr
#'  E.g., list(array(1, dim = c(num_lons, num_lats)))
#'@param maskobs See help on parameter 'maskmod'.
#'@param configfile Path to the s2dv configuration file from which 
#'  to retrieve information on location in file system (and other) of datasets.\cr
#'  If not specified, the configuration file used at BSC-ES will be used 
#'  (it is included in the package).\cr
#'  Check the BSC's configuration file or a template of configuration file in 
#'  the folder 'inst/config' in the package.\cr
#'  Check further information on the configuration file mechanism in 
#'  \code{ConfigFileOpen()}.
#'@param varmin Loaded experimental and observational data values smaller 
#'  than 'varmin' will be disabled (replaced by NA values).\cr
#'  By default no deactivation is performed.
#'@param varmax Loaded experimental and observational data values greater 
#'  than 'varmax' will be disabled (replaced by NA values).\cr
#'  By default no deactivation is performed.
#'@param silent Parameter to show (FALSE) or hide (TRUE) information messages.\cr
#'  Warnings will be displayed even if 'silent' is set to TRUE.\cr
#'  Takes by default the value 'FALSE'.
#'@param nprocs Number of parallel processes created to perform the fetch 
#'  and computation of data.\cr
#'  These processes will use shared memory in the processor in which Load() 
#'  is launched.\cr
#'  By default the number of logical cores in the machine will be detected 
#'  and as many processes as logical cores there are will be created.\cr
#'  A value of 1 won't create parallel processes.\cr
#'  When running in multiple processes, if an error occurs in any of the 
#'  processes, a crash message appears in the R session of the original 
#'  process but no detail is given about the error. A value of 1 will display 
#'  all error messages in the original and only R session.\cr
#'  Note: the parallel process create other blocking processes each time they 
#'  need to compute an interpolation via 'cdo'.
#'@param dimnames Named list where the name of each element is a generic 
#'  name of the expected dimensions inside the NetCDF files. These generic 
#'  names are 'lon', 'lat' and 'member'. 'time' is not needed because it's 
#'  detected automatically by discard.\cr
#'  The value associated to each name is the actual dimension name in the 
#'  NetCDF file.\cr
#'  The variables in the file that contain the longitudes and latitudes of 
#'  the data (if the data is a 2-dimensional variable) must have the same 
#'  name as the longitude and latitude dimensions.\cr
#'  By default, these names are 'longitude', 'latitude' and 'ensemble. If any 
#'  of those is defined in the 'dimnames' parameter, it takes priority and 
#'  overwrites the default value.
#'  E.g., list(lon = 'x', lat = 'y')
#'  In that example, the dimension 'member' will take the default value 'ensemble'.
#'@param remapcells When loading a 2-dimensional variable, spatial subsets can 
#'  be requested via \code{lonmin}, \code{lonmax}, \code{latmin} and 
#'  \code{latmax}. When \code{Load()} obtains the subset it is then 
#'  interpolated if needed with the method specified in \code{method}.\cr
#'  The result of this interpolation can vary if the values surrounding the 
#'  spatial subset are not present. To better control this process, the width 
#'  in number of grid cells of the surrounding area to be taken into account 
#'  can be specified with \code{remapcells}. A value of 0 will take into 
#'  account no additional cells but will generate less traffic between the 
#'  storage and the R processes that load data.\cr
#'  A value beyond the limits in the data files will be automatically runcated 
#'  to the actual limit.\cr
#'  The default value is 2. 
#'@param path_glob_permissive In some cases, when specifying a path pattern 
#'  (either in the parameters 'exp'/'obs' or in a configuration file) one can 
#'  specify path patterns that contain shell globbing expressions. Too much 
#'  freedom in putting globbing expressions in the path patterns can be 
#'  dangerous and make \code{Load()} find a file in the file system for a 
#'  start date for a dataset that really does not belong to that dataset. 
#'  For example, if the file system contains two directories for two different 
#'  experiments that share a part of their path and the path pattern contains 
#'  globbing expressions:
#'    /experiments/model1/expA/monthly_mean/tos/tos_19901101.nc
#'    /experiments/model2/expA/monthly_mean/tos/tos_19951101.nc
#'  And the path pattern is used as in the example right below to load data of 
#'  only the experiment 'expA' of the model 'model1' for the starting dates 
#'  '19901101' and '19951101', \code{Load()} will undesiredly yield data for 
#'  both starting dates, even if in fact there is data only for the 
#'  first one:\cr
#'      \code{
#'  expA <- list(path = file.path('/experiments/*/expA/monthly_mean/$VAR_NAME$',
#'                                '$VAR_NAME$_$START_DATE$.nc')
#'  data <- Load('tos', list(expA), NULL, c('19901101', '19951101'))
#'      }
#'  To avoid these situations, the parameter \code{path_glob_permissive} is 
#'  set by default to \code{'partial'}, which forces \code{Load()} to replace 
#'  all the globbing expressions of a path pattern of a data set by fixed 
#'  values taken from the path of the first found file for each data set, up 
#'  to the folder right before the final files (globbing expressions in the 
#'  file name will not be replaced, only those in the path to the file). 
#'  Replacement of globbing expressions in the file name can also be triggered 
#'  by setting \code{path_glob_permissive} to \code{FALSE} or \code{'no'}. If 
#'  needed to keep all globbing expressions, \code{path_glob_permissive} can 
#'  be set to \code{TRUE} or \code{'yes'}.
#'
#'@details
#'The two output matrices have between 2 and 6 dimensions:\cr
#'  \enumerate{
#'    \item{Number of experimental/observational datasets.}
#'    \item{Number of members.}
#'    \item{Number of startdates.}
#'    \item{Number of leadtimes.}
#'    \item{Number of latitudes (optional).}
#'    \item{Number of longitudes (optional).}
#'  }
#'but the two matrices have the same number of dimensions and only the first 
#'two dimensions can have different lengths depending on the input arguments.    
#'For a detailed explanation of the process, read the documentation attached 
#'to the package or check the comments in the code.
#'
#'@return 
#'\code{Load()} returns a named list following a structure similar to the 
#'used in the package 'downscaleR'.\cr
#'The components are the following:
#'  \itemize{
#'    \item{
#'      'mod' is the array that contains the experimental data. It has the 
#'      attribute 'dimensions' associated to a vector of strings with the 
#'      labels of each dimension of the array, in order. The order of the 
#'      latitudes is always forced to be from 90 to -90 whereas the order of 
#'      the longitudes is kept as in the original files (if possible). The 
#'      longitude values provided in \code{lon} lower than 0 are added 360 
#'      (but still kept in the original order). In some cases, however, if 
#'      multiple data sets are loaded in longitude-latitude mode, the 
#'      longitudes (and also the data arrays in \code{mod} and \code{obs}) are 
#'      re-ordered afterwards by \code{Load()} to range from 0 to 360; a 
#'      warning is given in such cases. The longitude and latitude of the 
#'      center of the grid cell that corresponds to the value [j, i] in 'mod' 
#'      (along the dimensions latitude and longitude, respectively) can be 
#'      found in the outputs \code{lon}[i] and \code{lat}[j]
#'    }
#'    \item{'obs' is the array that contains the observational data. The 
#'      same documentation of parameter 'mod' applies to this parameter.}
#'    \item{'lat' and 'lon' are the latitudes and longitudes of the centers of 
#'      the cells of the grid the data is interpolated into (0 if the loaded 
#'      variable is a global mean or the output is an area average).\cr
#'      Both have the attribute 'cdo_grid_des' associated with a character 
#'      string with the name of the common grid of the data, following the CDO 
#'      naming conventions for grids.\cr
#'      'lon' has the attributes 'first_lon' and 'last_lon', with the first 
#'      and last longitude values found in the region defined by 'lonmin' and 
#'      'lonmax'. 'lat' has also the equivalent attributes 'first_lat' and 
#'      'last_lat'.\cr
#'      'lon' has also the attribute 'data_across_gw' which tells whether the 
#'      requested region via 'lonmin', 'lonmax', 'latmin', 'latmax' goes across 
#'      the Greenwich meridian. As explained in the documentation of the 
#'      parameter 'mod', the loaded data array is kept in the same order as in 
#'      the original files when possible: this means that, in some cases, even 
#'      if the data goes across the Greenwich, the data array may not go 
#'      across the Greenwich. The attribute 'array_across_gw' tells whether 
#'      the array actually goes across the Greenwich. E.g: The longitudes in 
#'      the data files are defined to be from 0 to 360. The requested 
#'      longitudes are from -80 to 40. The original order is kept, hence the 
#'      longitudes in the array will be ordered as follows: 
#'      0, ..., 40, 280, ..., 360. In that case, 'data_across_gw' will be TRUE 
#'      and 'array_across_gw' will be FALSE.\cr
#'      The attribute 'projection' is kept for compatibility with 'downscaleR'.
#'    }
#'    \item{'Variable' has the following components:
#'      \itemize{
#'        \item{'varName', with the short name of the loaded variable as 
#'          specified in the parameter 'var'.
#'        }
#'        \item{'level', with information on the pressure level of the 
#'          variable. Is kept to NULL by now.
#'        }
#'      }
#'    And the following attributes:
#'      \itemize{
#'        \item{'is_standard', kept for compatibility with 'downscaleR', 
#'          tells if a dataset has been homogenized to standards with 
#'          'downscaleR' catalogs.
#'        }
#'        \item{'units', a character string with the units of measure of the 
#'          variable, as found in the source files.
#'        }
#'        \item{'longname', a character string with the long name of the 
#'          variable, as found in the source files.
#'        }
#'        \item{'daily_agg_cellfun', 'monthly_agg_cellfun', 
#'          'verification_time', kept for compatibility with 'downscaleR'.
#'        }
#'      }
#'    }
#'    \item{'Datasets' has the following components:
#'      \itemize{
#'        \item{'exp', a named list where the names are the identifying 
#'          character strings of each experiment in 'exp', each associated to 
#'          a list with the following components:
#'          \itemize{
#'            \item{'members', a list with the names of the members of the dataset.}
#'            \item{'source', a path or URL to the source of the dataset.}
#'          }
#'        }
#'        \item{'obs', similar to 'exp' but for observational datasets.}
#'      }
#'    }
#'    \item{'Dates', with the follwing components:
#'      \itemize{
#'        \item{'start', an array of dimensions (sdate, time) with the POSIX 
#'          initial date of each forecast time of each starting date.
#'        } 
#'        \item{'end', an array of dimensions (sdate, time) with the POSIX 
#'          final date of each forecast time of each starting date.
#'        }
#'      }
#'    }
#'    \item{'InitializationDates', a vector of starting dates as specified in 
#'      'sdates', in POSIX format.
#'    }
#'    \item{'when', a time stamp of the date the \code{Load()} call to obtain 
#'      the data was issued.
#'    }
#'    \item{'source_files', a vector of character strings with complete paths 
#'      to all the found files involved in the \code{Load()} call.
#'    }
#'    \item{'not_found_files', a vector of character strings with complete 
#'      paths to not found files involved in the \code{Load()} call.
#'    }
#'  }
#'
#'@examples
#'# Let's assume we want to perform verification with data of a variable
#'# called 'tos' from a model called 'model' and observed data coming from 
#'# an observational dataset called 'observation'.
#'#
#'# The model was run in the context of an experiment named 'experiment'. 
#'# It simulated from 1st November in 1985, 1990, 1995, 2000 and 2005 for a 
#'# period of 5 years time from each starting date. 5 different sets of 
#'# initial conditions were used so an ensemble of 5 members was generated 
#'# for each starting date.
#'# The model generated values for the variables 'tos' and 'tas' in a 
#'# 3-hourly frequency but, after some initial post-processing, it was 
#'# averaged over every month.
#'# The resulting monthly average series were stored in a file for each 
#'# starting date for each variable with the data of the 5 ensemble members.
#'# The resulting directory tree was the following:
#'#   model
#'#    |--> experiment
#'#          |--> monthly_mean
#'#                |--> tos_3hourly
#'#                |     |--> tos_19851101.nc
#'#                |     |--> tos_19901101.nc
#'#                |               .
#'#                |               .
#'#                |     |--> tos_20051101.nc 
#'#                |--> tas_3hourly
#'#                      |--> tas_19851101.nc
#'#                      |--> tas_19901101.nc
#'#                                .
#'#                                .
#'#                      |--> tas_20051101.nc
#'# 
#'# The observation recorded values of 'tos' and 'tas' at each day of the 
#'# month over that period but was also averaged over months and stored in 
#'# a file per month. The directory tree was the following:
#'#   observation
#'#    |--> monthly_mean
#'#          |--> tos
#'#          |     |--> tos_198511.nc
#'#          |     |--> tos_198512.nc
#'#          |     |--> tos_198601.nc
#'#          |               .
#'#          |               .
#'#          |     |--> tos_201010.nc
#'#          |--> tas
#'#                |--> tas_198511.nc
#'#                |--> tas_198512.nc
#'#                |--> tas_198601.nc
#'#                          .
#'#                          .
#'#                |--> tas_201010.nc
#'#
#'# The model data is stored in a file-per-startdate fashion and the
#'# observational data is stored in a file-per-month, and both are stored in 
#'# a monthly frequency. The file format is NetCDF.
#'# Hence all the data is supported by Load() (see details and other supported 
#'# conventions in ?Load) but first we need to configure it properly.
#'#
#'# These data files are included in the package (in the 'sample_data' folder),
#'# only for the variable 'tos'. They have been interpolated to a very low 
#'# resolution grid so as to make it on CRAN.
#'# The original grid names (following CDO conventions) for experimental and 
#'# observational data were 't106grid' and 'r180x89' respectively. The final
#'# resolutions are 'r20x10' and 'r16x8' respectively. 
#'# The experimental data comes from the decadal climate prediction experiment 
#'# run at IC3 in the context of the CMIP5 project. Its name within IC3 local 
#'# database is 'i00k'. 
#'# The observational dataset used for verification is the 'ERSST' 
#'# observational dataset.
#'#
#'# The next two examples are equivalent and show how to load the variable 
#'# 'tos' from these sample datasets, the first providing lists of lists to 
#'# the parameters 'exp' and 'obs' (see documentation on these parameters) and 
#'# the second providing vectors of character strings, hence using a 
#'# configuration file.
#'#
#'# The code is not run because it dispatches system calls to 'cdo' which is 
#'# not allowed in the examples as per CRAN policies. You can run it on your 
#'# system though. 
#'# Instead, the code in 'dontshow' is run, which loads the equivalent
#'# already processed data in R.
#'#
#'# Example 1: Providing lists of lists to 'exp' and 'obs':
#'#
#'  \dontrun{
#'data_path <- system.file('sample_data', package = 's2dv')
#'exp <- list(
#'         name = 'experiment',
#'         path = file.path(data_path, 'model/$EXP_NAME$/monthly_mean',
#'                          '$VAR_NAME$_3hourly/$VAR_NAME$_$START_DATES$.nc')
#'       )
#'obs <- list(
#'         name = 'observation',
#'         path = file.path(data_path, 'observation/$OBS_NAME$/monthly_mean',
#'                          '$VAR_NAME$/$VAR_NAME$_$YEAR$$MONTH$.nc')
#'       )
#'# Now we are ready to use Load().
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- Load('tos', list(exp), list(obs), startDates,
#'                   output = 'areave', latmin = 27, latmax = 48, 
#'                   lonmin = -12, lonmax = 40)
#'  }
#'#
#'# Example 2: Providing vectors of character strings to 'exp' and 'obs'
#'#            and using a configuration file.
#'#
#'# The configuration file 'sample.conf' that we will create in the example 
#'# has the proper entries to load these (see ?LoadConfigFile for details on 
#'# writing a configuration file). 
#'#
#'  \dontrun{
#'data_path <- system.file('sample_data', package = 's2dv')
#'expA <- list(name = 'experiment', path = file.path(data_path, 
#'             'model/$EXP_NAME$/$STORE_FREQ$_mean/$VAR_NAME$_3hourly',
#'             '$VAR_NAME$_$START_DATE$.nc'))
#'obsX <- list(name = 'observation', path = file.path(data_path,
#'             '$OBS_NAME$/$STORE_FREQ$_mean/$VAR_NAME$',
#'             '$VAR_NAME$_$YEAR$$MONTH$.nc'))
#'
#'# Now we are ready to use Load().
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- Load('tos', list(expA), list(obsX), startDates,
#'                   output = 'areave', latmin = 27, latmax = 48, 
#'                   lonmin = -12, lonmax = 40)
#'#
#'# Example 3: providing character strings in 'exp' and 'obs', and providing
#'# a configuration file.
#'# The configuration file 'sample.conf' that we will create in the example 
#'# has the proper entries to load these (see ?LoadConfigFile for details on 
#'# writing a configuration file). 
#'#
#'configfile <- paste0(tempdir(), '/sample.conf')
#'ConfigFileCreate(configfile, confirm = FALSE)
#'c <- ConfigFileOpen(configfile)
#'c <- ConfigEditDefinition(c, 'DEFAULT_VAR_MIN', '-1e19', confirm = FALSE)
#'c <- ConfigEditDefinition(c, 'DEFAULT_VAR_MAX', '1e19', confirm = FALSE)
#'data_path <- system.file('sample_data', package = 's2dv')
#'exp_data_path <- paste0(data_path, '/model/$EXP_NAME$/')
#'obs_data_path <- paste0(data_path, '/$OBS_NAME$/')
#'c <- ConfigAddEntry(c, 'experiments', dataset_name = 'experiment', 
#'     var_name = 'tos', main_path = exp_data_path,
#'     file_path = '$STORE_FREQ$_mean/$VAR_NAME$_3hourly/$VAR_NAME$_$START_DATE$.nc')
#'c <- ConfigAddEntry(c, 'observations', dataset_name = 'observation', 
#'     var_name = 'tos', main_path = obs_data_path,
#'     file_path = '$STORE_FREQ$_mean/$VAR_NAME$/$VAR_NAME$_$YEAR$$MONTH$.nc')
#'ConfigFileSave(c, configfile, confirm = FALSE)
#'
#'# Now we are ready to use Load().
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- Load('tos', c('experiment'), c('observation'), startDates, 
#'                   output = 'areave', latmin = 27, latmax = 48, 
#'                   lonmin = -12, lonmax = 40, configfile = configfile)
#'  }
#'  \dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'), 
#'                                     c('observation'), startDates,
#'                                     output = 'areave', 
#'                                     latmin = 27, latmax = 48, 
#'                                     lonmin = -12, lonmax = 40) 
#'  } 
#'@import parallel bigmemory methods
#'@importFrom stats ts window na.omit
#'@importFrom abind abind
#'@export
Load <- function(var, exp = NULL, obs = NULL, sdates, nmember = NULL, 
                 nmemberobs = NULL, nleadtime = NULL, leadtimemin = 1, 
                 leadtimemax = NULL, storefreq = 'monthly', sampleperiod = 1, 
                 lonmin = 0, lonmax = 360, latmin = -90, latmax = 90, 
                 output = 'areave', method = 'conservative', grid = NULL, 
                 maskmod = vector("list", 15), maskobs = vector("list", 15), 
                 configfile = NULL, varmin = NULL, varmax = NULL, 
                 silent = FALSE, nprocs = NULL, dimnames = NULL, 
                 remapcells = 2, path_glob_permissive = 'partial') {

  #library(parallel)
  #library(bigmemory)

  # Print a stamp of the call the user issued.
  parameter_names <- ls()
  if (length(parameter_names) < 3 || is.null(var) ||
      is.null(sdates) || (is.null(exp) && is.null(obs))) {
    stop("Error: At least 'var', 'exp'/'obs' and 'sdates' must be provided.")
  }
  load_parameters <- lapply(parameter_names, get, envir = environment())
  names(load_parameters) <- parameter_names
  parameters_to_show <- c('var', 'exp', 'obs', 'sdates', 'nmember', 'leadtimemin',
                          'leadtimemax', 'latmin', 'latmax', 'lonmin', 'lonmax',
                          'output', 'grid', 'storefreq')
  load_parameters <- c(load_parameters[parameters_to_show], load_parameters[-match(parameters_to_show, names(load_parameters))])
  if (!silent) {
    message(paste("* The load call you issued is:\n*   Load(", 
            paste(strwrap(
              paste(unlist(lapply(names(load_parameters[1:length(parameters_to_show)]), 
              function(x) paste(x, '=', 
                if (x == 'sdates' && length(load_parameters[[x]]) > 4) {
                  paste0("c('", load_parameters[[x]][1], "', '", load_parameters[[x]][2], 
                         "', ..., '", tail(load_parameters[[x]], 1), "')")
                } else if ((x %in% c('exp', 'obs')) && is.list(load_parameters[[x]])) {
                  paste0("list(", paste(unlist(lapply(load_parameters[[x]], 
                    function (y) {
                      paste0("list(", 
                        if ('name' %in% names(y)) {
                          paste0('name = "', y[['name']], '", ...')
                        } else {
                          "..."
                        }, ")"
                      )
                    })), collapse = ', '), 
                  ")")
  # Print a stamp of the call the user issued.
                } else {
                  paste(deparse(load_parameters[[x]]), collapse = '')
                }))), 
              collapse = ', '), width = getOption('width') - 9, indent = 0, exdent = 8), collapse = '\n*'),
            ", ...)\n* See the full call in '$load_parameters' after Load() finishes.", sep = ''))

  }

  # Run Load() error-aware, so that it always returns something
  errors <- try({

  # Check and sanitize parameters
  # var
  if (is.null(var) || !(is.character(var) && nchar(var) > 0)) {
    stop("Error: parameter 'var' should be a character string of length >= 1.")
  }

  # exp
  exps_to_fetch <- c()
  exp_info_names <- c('name', 'path', 'nc_var_name', 'suffix', 
                      'var_min', 'var_max', 'dimnames')
  if (!is.null(exp) && !(is.character(exp) && all(nchar(exp) > 0)) && !is.list(exp)) {
    stop("Error: parameter 'exp' should be a vector of strings or a list with information of the experimental datasets to load. Check 'exp' in ?Load for details.")
  } else if (!is.null(exp)) {
    if (!is.list(exp)) {
      exp <- lapply(exp, function (x) list(name = x))
    }
    for (i in 1:length(exp)) {
      if (!is.list(exp[[i]])) {
        stop("Error: parameter 'exp' is incorrect. It should be a list of lists.")
      }
      #if (!(all(names(exp[[i]]) %in% exp_info_names))) {
      #  stop("Error: parameter 'exp' is incorrect. There are unrecognized components in the information of some of the experiments. Check 'exp' in ?Load for details.")
      #}
      if (!('name' %in% names(exp[[i]]))) {
        exp[[i]][['name']] <- paste0('exp', i)
        if (!('path' %in% names(exp[[i]]))) {
          stop("Error: parameter 'exp' is incorrect. A 'path' should be provided for each experimental dataset if no 'name' is provided. See 'exp' in ?Load for details.")
        }
      } else if (!('path' %in% names(exp[[i]]))) {
        exps_to_fetch <- c(exps_to_fetch, i)
      }
      if ('path' %in% names(exp[[i]])) {
        if (!('nc_var_name' %in% names(exp[[i]]))) {
          exp[[i]][['nc_var_name']] <- '$VAR_NAME$'
        }
        if (!('suffix' %in% names(exp[[i]]))) {
          exp[[i]][['suffix']] <- ''
        }
        if (!('var_min' %in% names(exp[[i]]))) {
          exp[[i]][['var_min']] <- ''
        }
        if (!('var_max' %in% names(exp[[i]]))) {
          exp[[i]][['var_max']] <- ''
        }
      }
    }
    if ((length(exps_to_fetch) > 0) && (length(exps_to_fetch) < length(exp))) {
      .warning("'path' was provided for some experimental datasets in 'exp'. Any information in the configuration file related to these will be ignored.")
    }
  }

  # obs
  obs_to_fetch <- c()
  obs_info_names <- c('name', 'path', 'nc_var_name', 'suffix', 
                      'var_min', 'var_max', 'dimnames')
  if (!is.null(obs) && !(is.character(obs) && all(nchar(obs) > 0)) && !is.list(obs)) {
    stop("Error: parameter 'obs' should be a vector of strings or a list with information of the observational datasets to load. Check 'obs' in ?Load for details.")
  } else if (!is.null(obs)) {
    if (!is.list(obs)) {
      obs <- lapply(obs, function (x) list(name = x))
    }
    for (i in 1:length(obs)) {
      if (!is.list(obs[[i]])) {
        stop("Error: parameter 'obs' is incorrect. It should be a list of lists.")
      }
      #if (!(all(names(obs[[i]]) %in% obs_info_names))) {
      #  stop("Error: parameter 'obs' is incorrect. There are unrecognized components in the information of some of the observations. Check 'obs' in ?Load for details.")
      #}
      if (!('name' %in% names(obs[[i]]))) {
        obs[[i]][['name']] <- paste0('obs', i)
        if (!('path' %in% names(obs[[i]]))) {
          stop("Error: parameter 'obs' is incorrect. A 'path' should be provided for each observational dataset if no 'name' is provided. See 'obs' in ?Load for details.")
        }
      } else if (!('path' %in% names(obs[[i]]))) {
        obs_to_fetch <- c(obs_to_fetch, i)
      }
      if ('path' %in% names(obs[[i]])) {
        if (!('nc_var_name' %in% names(obs[[i]]))) {
          obs[[i]][['nc_var_name']] <- '$VAR_NAME$'
        }
        if (!('suffix' %in% names(obs[[i]]))) {
          obs[[i]][['suffix']] <- ''
        }
        if (!('var_min' %in% names(obs[[i]]))) {
          obs[[i]][['var_min']] <- ''
        }
        if (!('var_max' %in% names(obs[[i]]))) {
          obs[[i]][['var_max']] <- ''
        }
      }
    }
    if (length(c(obs_to_fetch, exps_to_fetch) > 1) && (length(obs_to_fetch) < length(obs))) {
      .warning("'path' was provided for some observational datasets in 'obs'. Any information in the configuration file related to these will be ignored.")
    }
  }

  # sdates
  if (is.null(sdates)) {
    stop("Error: parameter 'sdates' must be provided.")
  }
  if (!is.character(sdates) || !all(nchar(sdates) == 8) || anyNA(strtoi(sdates))) {
    stop("Error: parameter 'sdates' is incorrect. All starting dates should be a character string in the format 'YYYYMMDD'.")
  }

  # nmember
  if (!is.null(nmember) && !is.null(exp)) {
    if (!is.numeric(nmember)) {
      stop("Error: parameter 'nmember' is incorrect. It should be numeric.")
    }
    if (length(nmember) == 1) {
      .warning(paste("'nmember' should specify the number of members of each experimental dataset. Forcing to", nmember, "for all experiments."))
      nmember <- rep(nmember, length(exp))
    }
    if (length(nmember) != length(exp)) {
      stop("Error: 'nmember' must contain as many values as 'exp'.")
    } else if (anyNA(nmember)) {
      nmember[which(is.na(nmember))] <- max(nmember, na.rm = TRUE)
    }
  }

  # nmemberobs
  if (!is.null(nmemberobs) && !is.null(obs)) {
    if (!is.numeric(nmemberobs)) {
      stop("Error: parameter 'nmemberobs' is incorrect. It should be numeric.")
    }
    if (length(nmemberobs) == 1) {
      .warning(paste("'nmemberobs' should specify the number of members of each observational dataset. Forcing to", nmemberobs, "for all observations."))
      nmemberobs <- rep(nmemberobs, length(obs))
    }
    if (length(nmemberobs) != length(obs)) {
      stop("Error: 'nmemberobs' must contain as many values as 'obs'.")
    } else if (anyNA(nmemberobs)) {
      nmemberobs[which(is.na(nmemberobs))] <- max(nmemberobs, na.rm = TRUE)
    }
  }

  # nleadtime
  if (!is.null(nleadtime) && !is.numeric(nleadtime)) {
    stop("Error: parameter 'nleadtime' is wrong. It should be numeric.")
  }

  # leadtimemin
  if (is.null(leadtimemin) || !is.numeric(leadtimemin)) {
    stop("Error: parameter 'leadtimemin' is wrong. It should be numeric.")
  }

  # leadtimemax
  if (!is.null(leadtimemax) && !is.numeric(leadtimemax)) {
    stop("Error: parameter 'leadtimemax' is wrong. It should be numeric.")
  }

  # storefreq       
  if (!is.character(storefreq) || !(storefreq %in% c('monthly', 'daily'))) {
    stop("Error: parameter 'storefreq' is wrong, can take value 'daily' or 'monthly'.")
  }
  # sampleperiod
  if (is.null(sampleperiod) || !is.numeric(sampleperiod)) {
    stop("Error: parameter 'sampleperiod' is wrong. It should be numeric.")
  }

  # lonmin
  if (is.null(lonmin) || !is.numeric(lonmin)) {
    stop("Error: parameter 'lonmin' is wrong. It should be numeric.")
  }
  if (lonmin < -360 || lonmin > 360) {
    stop("Error: parameter 'lonmin' must be in the range [-360, 360]")
  }
  if (lonmin < 0) {
    lonmin <- lonmin + 360
  }

  # lonmax
  if (is.null(lonmax) || !is.numeric(lonmax)) {
    stop("Error: parameter 'lonmax' is wrong. It should be numeric.")
  }
  if (lonmax < -360 || lonmax > 360) {
    stop("Error: parameter 'lonmax' must be in the range [-360, 360]")
  }
  if (lonmax < 0) {
    lonmax <- lonmax + 360
  }

  # latmin
  if (is.null(latmin) || !is.numeric(latmin)) {
    stop("Error: parameter 'latmin' is wrong. It should be numeric.")
  }
  if (latmin > 90 || latmin < -90) {
    stop("Error: 'latmin' must be in the interval [-90, 90].")
  }
  
  # latmax
  if (is.null(latmax) || !is.numeric(latmax)) {
    stop("Error: parameter 'latmax' is wrong. It should be numeric.")
  }
  if (latmax > 90 || latmax < -90) {
    stop("Error: 'latmax' must be in the interval [-90, 90].")
  }

  # output
  if (is.null(output) || !(output %in% c('lonlat', 'lon', 'lat', 'areave'))) {
    stop("Error: 'output' can only take values 'lonlat', 'lon', 'lat' or 'areave'.")
  }

  # method
  if (is.null(method) || !(method %in% c('bilinear', 'bicubic', 'conservative', 'distance-weighted'))) {
    stop("Error: parameter 'method' is wrong, can take value 'bilinear', 'bicubic', 'conservative' or 'distance-weighted'.")
  }
  remap <- switch(method, 'bilinear' = 'bil', 'bicubic' = 'bic', 
                  'conservative' = 'con', 'distance-weighted' = 'dis')

  # grid
  if (!is.null(grid)) {
    if (is.character(grid)) {
      if (grid == 'none') {
        grid <- NULL
      } else {
        supported_grids <- list('r[0-9]{1,}x[0-9]{1,}', 't[0-9]{1,}grid')
        grid_matches <- unlist(lapply(lapply(supported_grids, regexpr, grid), .IsFullMatch, grid))
        if (sum(grid_matches) < 1) {
          stop("The specified grid in the parameter 'grid' is incorrect. Must be one of r<NX>x<NY> or t<RES>grid.")
        }
      }
    } else {
      stop("Error: parameter 'grid' should be a character string, if specified.")
    }
  }

  # maskmod
  if (!is.list(maskmod)) {
    stop("Error: parameter 'maskmod' must be a list.")
  }
  if (length(maskmod) < length(exp)) {
    stop("Error: 'maskmod' must contain a numeric mask or NULL for each experiment in 'exp'.")
  }
  for (i in 1:length(maskmod)) {
    if (is.list(maskmod[[i]])) {
      if ((length(maskmod[[i]]) > 2) || !all(names(maskmod[[i]]) %in% c('path', 'nc_var_name'))) {
        stop("Error: all masks in 'maskmod' must be a numeric matrix, or a list with the components 'path' and optionally 'nc_var_name', or NULL.")
      }
    } else if (!(is.numeric(maskmod[[i]]) || is.null(maskmod[[i]]))) {
      stop("Error: all masks in 'maskmod' must be a numeric matrix, or a list with the components 'path' and optionally 'nc_var_name', or NULL.")
    }
  }

  # maskobs
  if (!is.list(maskobs)) {
    stop("Error: parameter 'maskobs' must be a list.")
  }
  if (length(maskobs) < length(obs)) {
    stop("Error: 'maskobs' must contain a numeric mask or NULL for each obseriment in 'obs'.")
  }
  for (i in 1:length(maskobs)) {
    if (is.list(maskobs[[i]])) {
      if ((length(maskobs[[i]]) > 2) || !all(names(maskobs[[i]]) %in% c('path', 'nc_var_name'))) {
        stop("Error: all masks in 'maskobs' must be a numeric matrix, or a list with the components 'path' and optionally 'nc_var_name', or NULL.")
      }
    } else if (!(is.numeric(maskobs[[i]]) || is.null(maskobs[[i]]))) {
      stop("Error: all masks in 'maskobs' must be a numeric matrix, or a list with the components 'path' and optionally 'nc_var_name', or NULL.")
    }
  }

  ## Force the observational masks to be the same as the experimental when
  ## possible.
  if ((output != 'areave' || !is.null(grid)) && length(exp) > 0) {
    if (!all(unlist(lapply(maskobs, is.null)))) {
      .warning("'maskobs' will be ignored. 'maskmod[[1]]' will be applied to observations instead.")
    }
    maskobs <- lapply(maskobs, function(x) x <- maskmod[[1]])
  }

  # configfile
  if (is.null(configfile)) {
    configfile <- system.file("config", "BSC.conf", package = "s2dv")
  } else if (!is.character(configfile) || !(nchar(configfile) > 0)) {
    stop("Error: parameter 'configfile' must be a character string with the path to an s2dv configuration file, if specified.")
  }

  # varmin
  if (!is.null(varmin) && !is.numeric(varmin)) {
    stop("Error: parameter 'varmin' must be numeric, if specified.")
  }

  # varmax
  if (!is.null(varmax) && !is.numeric(varmax)) {
    stop("Error: parameter 'varmax' must be numeric, if specified.")
  }

  # silent
  if (!is.logical(silent)) {
    stop("Error: parameter 'silent' must be TRUE or FALSE.")
  }

  # nprocs
  if (!is.null(nprocs) && (!is.numeric(nprocs) || nprocs < 1)) {
    stop("Error: parameter 'nprocs' must be a positive integer, if specified.")
  }

  # dimnames
  if (!is.null(dimnames) && (!is.list(dimnames))) {
    stop("Error: parameter 'dimnames' must be a list, if specified.")
  }
  if (!all(names(dimnames) %in% c('member', 'lat', 'lon'))) {
    stop("Error: parameter 'dimnames' is wrong. There are unrecognized component names. See 'dimnames' in ?Load for details.")
  }

  # remapcells  
  if (!is.numeric(remapcells) || remapcells < 0) {
    stop("Error: 'remapcells' must be an integer >= 0.")
  }

  # path_glob_permissive
  if (!is.logical(path_glob_permissive) && !(path_glob_permissive %in% c('yes', 'partial', 'no'))) {
    stop("Error: 'path_glob_permissive' must be one of TRUE, 'yes', 'partial', FALSE or 'no'.")
  }
  if (is.logical(path_glob_permissive)) {
    if (path_glob_permissive) {
      path_glob_permissive <- 'yes'
    } else {
      path_glob_permissive <- 'no'
    }
  }
  replace_globs <- path_glob_permissive %in% c('no', 'partial')

  # If not all data has been provided in 'exp' and 'obs', configuration file is read.
  if ((length(exps_to_fetch) > 0 || length(obs_to_fetch) > 0)) {
    if (!silent) {
      .message("Some 'path's not explicitly provided in 'exp' and 'obs', so will now proceed to open the configuration file.")
    }
    data_info <- ConfigFileOpen(configfile, silent, TRUE)

    # Check that the var, exp and obs parameters are right and keep the entries
    # that match for each dataset.
    # Afterwards, the matching entries are applied sequentially (as specified 
    # in ?ConfigFileOpen) and the replace_values are applied to the result.
    # Finally a path pattern for each dataset is provided.
    matches <- ConfigApplyMatchingEntries(data_info, var, sapply(exp[exps_to_fetch], '[[', 'name'), 
                 sapply(obs[obs_to_fetch], '[[', 'name'), show_entries = FALSE, show_result = FALSE)
    # 'replace_values' is a named list that associates a variable name to an 
    # associated value. Initially it is filled with variables and values parsed
    # from the configuration file, but we can add or modify some values during
    # the execution to choose for example which start date we want to load.
    # When '.ConfigReplaceVariablesInString' is called, all the variable accesses 
    # ($VARIABLE_NAME$) that appear in the string given as parameter are 
    # replaced by the associated value in 'replace_values'.
    replace_values <- data_info$definitions
    if (!is.null(exp) && length(exps_to_fetch) > 0) {
      counter <- 1
      exp[exps_to_fetch] <- lapply(matches$exp_info, 
        function (x) {
          x[names(exp[[exps_to_fetch[counter]]])] <- exp[[exps_to_fetch[counter]]]
          x[['path']] <- paste0(x[['main_path']], x[['file_path']])
          counter <<- counter + 1
          x
        })
    }
    if (!is.null(obs) && length(obs_to_fetch) > 0) {
      counter <- 1
      obs[obs_to_fetch] <- lapply(matches$obs_info, 
        function (x) {
          x[names(obs[[obs_to_fetch[counter]]])] <- obs[[obs_to_fetch[counter]]]
          x[['path']] <- paste0(x[['main_path']], x[['file_path']])
          counter <<- counter + 1
          x
        })
    }
    if (!silent) {
      .message("All pairs (var, exp) and (var, obs) have matching entries.")
    }
  } else {
    replace_values <- list(DEFAULT_NC_VAR_NAME = '$VAR_NAME$',
                           DEFAULT_VAR_MIN = '',
                           DEFAULT_VAR_MAX = '',
                           DEFAULT_SUFFIX = '',
                           DEFAULT_DIM_NAME_LONGITUDES = 'longitude',
                           DEFAULT_DIM_NAME_LATITUDES = 'latitude',
                           DEFAULT_DIM_NAME_MEMBERS = 'ensemble')
  }
  # We take the dimnames that haven't been explicitly specified from the 
  # configuration file.
  # If the configuration file wasn't opened, we take the default values from
  # the dictionary 'replace_values'.
  dimnames <- list(lon = ifelse(is.null(dimnames[["lon"]]), 
                                replace_values[["DEFAULT_DIM_NAME_LONGITUDES"]],
                                dimnames[['lon']]),
                   lat = ifelse(is.null(dimnames[["lat"]]), 
                                replace_values[["DEFAULT_DIM_NAME_LATITUDES"]],
                                dimnames[['lat']]),
                   member = ifelse(is.null(dimnames[["member"]]), 
                                   replace_values[["DEFAULT_DIM_NAME_MEMBERS"]],
                                   dimnames[['member']]))
  mandatory_defaults <- c('DEFAULT_EXP_MAIN_PATH', 'DEFAULT_EXP_FILE_PATH', 
                          'DEFAULT_OBS_MAIN_PATH', 'DEFAULT_OBS_FILE_PATH',
                          'DEFAULT_NC_VAR_NAME', 'DEFAULT_SUFFIX', 
                          'DEFAULT_VAR_MIN', 'DEFAULT_VAR_MAX',
                          'DEFAULT_DIM_NAME_LONGITUDES', 
                          'DEFAULT_DIM_NAME_LATITUDES',
                          'DEFAULT_DIM_NAME_MEMBERS')
  extra_vars_with_default_ind <- (1:length(replace_values))[grep('^DEFAULT_', names(replace_values))]
  extra_vars_with_default_ind <- extra_vars_with_default_ind[
                                   grep(paste0(paste0('^', mandatory_defaults), 
                                               collapse = '|'),
                                        names(replace_values)[extra_vars_with_default_ind],
                                        invert = TRUE)
                                 ]
  extra_vars_with_default <- gsub('^DEFAULT_', '', 
                                  names(replace_values)[extra_vars_with_default_ind])
  if (!is.null(exp)) {
    exp <- lapply(exp, function (x) {
      if (!('dimnames' %in% names(x))) {
        x[['dimnames']] <- dimnames
      } else {
        dimnames2 <- dimnames
        dimnames2[names(x[['dimnames']])] <- x[['dimnames']]
        x[['dimnames']] <- dimnames2
      }
      i <- 1
      while (i <= length(extra_vars_with_default)) {
        if (!(extra_vars_with_default[i] %in% names(x))) {
          x[[extra_vars_with_default[i]]] <- replace_values[[extra_vars_with_default_ind[i]]]
        }
        i <- i + 1
      }
      x
    })
  }
  if (!is.null(obs)) {
    obs <- lapply(obs, function (x) {
      if (!('dimnames' %in% names(x))) {
        x[['dimnames']] <- dimnames
      } else {
        dimnames2 <- dimnames
        dimnames2[names(x[['dimnames']])] <- x[['dimnames']]
        x[['dimnames']] <- dimnames2
      }
      i <- 1
      while (i <= length(extra_vars_with_default)) {
        if (!(extra_vars_with_default[i] %in% names(x))) {
          x[[extra_vars_with_default[i]]] <- replace_values[[extra_vars_with_default_ind[i]]]
        }
        i <- i + 1
      }
      x
    })
  }
  single_dataset <- (length(obs) + length(exp) == 1)

  ## We add some predefined values in the dictionary.
  replace_values[["VAR_NAME"]] <- var
  replace_values[["STORE_FREQ"]] <- storefreq

  # Initialize some variables that will take various values along the
  # execution
  latitudes <- longitudes <- NULL
  leadtimes <- NULL
  var_exp <- var_obs <- NULL
  units <- var_long_name <- NULL
  is_2d_var <- data_across_gw <- array_across_gw <- FALSE

  # Start defining the dimensions of the output matrices
  nmod <- length(exp)
  nobs <- length(obs)
  nsdates <- length(sdates)

  # We will iterate over all the experiments, start dates and members and will open
  # the file pointed by the data in the configuration file.
  # If a file is found, we will open it and read its metadata to work out the 
  # remaining dimensions: members, leadtimes, longitudes and latitudes.
  #
  # At each iteration we will build a 'work piece' that will contain information
  # on the data we want to load from a file. For each file we will have one
  # work piece. These work pieces will be packages of information to be sent to
  # the various parallel processes. Each process will need this information to
  # access and manipulate the data according to the output type and other 
  # parameters. 
  if (!silent) {
    .message("Fetching first experimental files to work out 'var_exp' size...")
  }

  dataset_type <- 'exp'
  dim_exp <- NULL
  filename <- file_found <- tmp <- nltime <- NULL
  dims2define <- TRUE
  is_file_per_member_exp <- rep(nmod, FALSE)
  exp_work_pieces <- list()
  jmod <- 1
  while (jmod <= nmod) {
    first_dataset_file_found <- FALSE
    replace_values[["EXP_NAME"]] <- exp[[jmod]][['name']]
    replace_values[["NC_VAR_NAME"]] <- exp[[jmod]][['nc_var_name']]
    replace_values[["SUFFIX"]] <- exp[[jmod]][['suffix']]
    extra_vars <- names(exp[[jmod]])[which(!(names(exp[[jmod]]) %in% exp_info_names))]
    replace_values[extra_vars] <- exp[[jmod]][extra_vars]
    namevar <- .ConfigReplaceVariablesInString(exp[[jmod]][['nc_var_name']], replace_values)
    tags_to_find <- c('START_DATE', 'YEAR', 'MONTH', 'DAY', 'MEMBER_NUMBER')
    position_of_tags <- na.omit(match(tags_to_find, names(replace_values)))
    if (length(position_of_tags) > 0) {
      quasi_final_path <- .ConfigReplaceVariablesInString(exp[[jmod]][['path']], 
                            replace_values[-position_of_tags], TRUE)
    } else {
      quasi_final_path <- .ConfigReplaceVariablesInString(exp[[jmod]][['path']], 
                            replace_values, TRUE)
    }
    if (!grepl('$START_DATE$', quasi_final_path, fixed = TRUE) &&
        !all(sapply(c('$YEAR$', '$MONTH$'), grepl, quasi_final_path, 
                    fixed = TRUE))) {
      stop(paste0("The tag $START_DATE$ or the three tags $YEAR$, $MONTH$, $DAY$ must be somewhere in the path pattern of the experimental dataset '", 
           exp[[jmod]][['name']], "'."))
    }
    is_file_per_member_exp[jmod] <- grepl('$MEMBER_NUMBER$', 
                                          quasi_final_path, fixed = TRUE)
    if (is.null(varmin)) {
      mod_var_min <- as.numeric(.ConfigReplaceVariablesInString(exp[[jmod]][['var_min']], replace_values))
    } else {
      mod_var_min <- varmin
    }
    if (is.null(varmax)) {
      mod_var_max <- as.numeric(.ConfigReplaceVariablesInString(exp[[jmod]][['var_max']], replace_values))
    } else {
      mod_var_max <- varmax
    }
    jsdate <- 1
    while (jsdate <= nsdates) {
      replace_values[["START_DATE"]] <- sdates[jsdate]
      replace_values[["YEAR"]] <- substr(sdates[jsdate], 1, 4)
      replace_values[["MONTH"]] <- substr(sdates[jsdate], 5, 6)
      replace_values[["DAY"]] <- substr(sdates[jsdate], 7, 8)
      if (is_file_per_member_exp[jmod]) {
        replace_values[["MEMBER_NUMBER"]] <- '*'
      }
      # If the dimensions of the output matrices are still to define, we try to read
      # the metadata of the data file that corresponds to the current iteration
      if (dims2define) {
        # We must build a work piece that will be sent to the .LoadDataFile function
        # in 'explore_dims' mode. We will obtain, if success, the dimensions of the
        # data in the file.
        work_piece <- list(dataset_type = dataset_type,
                           filename = .ConfigReplaceVariablesInString(quasi_final_path, replace_values),
                           namevar = namevar, grid = grid, remap = remap, remapcells = remapcells,
                           is_file_per_member = is_file_per_member_exp[jmod],
                           is_file_per_dataset = FALSE,
                           lon_limits = c(lonmin, lonmax),
                           lat_limits = c(latmin, latmax), dimnames = exp[[jmod]][['dimnames']],
                           single_dataset = single_dataset)
        found_data <- .LoadDataFile(work_piece, explore_dims = TRUE, silent = silent)
        found_dims <- found_data$dims
        var_long_name <- found_data$var_long_name
        units <- found_data$units
        if (!is.null(found_dims)) {
          # If a file is found, we can define the dimensions of the output arrays.
          is_2d_var <- found_data$is_2d_var
          if (!is_2d_var && (output != 'areave')) {
            .warning(paste0("'", output, "' output format not allowed when loading global mean variables. Forcing to 'areave'."))
            output <- 'areave'
          }
          if (output == 'lonlat' || output == 'lon') {
            data_across_gw <- found_data$data_across_gw
            array_across_gw <- found_data$array_across_gw
          }
          if (output != 'areave' && is.null(grid)) {
            grid <- found_data$grid
          }
          if (is.null(nmember)) {
            if (is.null(found_dims[['member']])) {
              .warning("loading data from a server but 'nmember' not specified. Loading only one member.")
              nmember <- rep(1, nmod)
            } else {
              nmember <- rep(found_dims[['member']], nmod)
            }
          }
          if (is.null(nleadtime)) {
            nleadtime <- found_dims[['ftime']]
          }
          if (is.null(leadtimemax)) {
            leadtimemax <- nleadtime
          } else if (leadtimemax > nleadtime) {
            stop("Error: 'leadtimemax' argument is greater than the number of loaded leadtimes. Put first the experiment with the greatest number of leadtimes or adjust properly the parameters 'nleadtime' and 'leadtimemax'.")
          }
          leadtimes <- seq(leadtimemin, leadtimemax, sampleperiod)
          latitudes <- found_dims[['lat']]
          longitudes <- found_dims[['lon']]
          
          if (output == 'lon' || output == 'lonlat') {
            dim_exp[['lon']] <- length(longitudes)
          }
          if (output == 'lat' || output == 'lonlat') {
            dim_exp[['lat']] <- length(latitudes)
          }
          dim_exp[['ftime']] <- length(leadtimes)
          dim_exp[['member']] <- max(nmember)
          dim_exp[['sdate']] <- nsdates
          dim_exp[['dataset']] <- nmod
          dims2define <- FALSE
        }
      }
      # Also, we must get rid of the shell globbing expressions in the 
      # quasi_final_path, for safety.
      if (!first_dataset_file_found) {
        found_path <- Sys.glob(.ConfigReplaceVariablesInString(quasi_final_path, replace_values))
        if (length(found_path) > 0) {
          found_path <- head(found_path, 1)
          if (replace_globs) {
            quasi_final_path <- .ReplaceGlobExpressions(quasi_final_path, found_path, 
                                                        replace_values, tags_to_find, 
                                                        exp[[jmod]][['name']],
                                                        path_glob_permissive == 'partial')
          }
          first_dataset_file_found <- TRUE
        }
      }
      # We keep on iterating through members to build all the work pieces.
      if (is_file_per_member_exp[jmod]) {
        jmember <- 1
        while (jmember <= nmember[jmod]) {
          replace_values[["MEMBER_NUMBER"]] <- sprintf(paste("%.", (nmember[jmod] %/% 10) + 1, "i", sep = ''), jmember - 1)
          work_piece <- list(filename = .ConfigReplaceVariablesInString(quasi_final_path, replace_values),
                             namevar = namevar, indices = c(1, jmember, jsdate, jmod), 
                             nmember = nmember[jmod], leadtimes = leadtimes, mask = maskmod[[jmod]],
                             is_file_per_dataset = FALSE, dimnames = exp[[jmod]][['dimnames']],
                             var_limits = c(mod_var_min, mod_var_max), remapcells = remapcells)
          exp_work_pieces <- c(exp_work_pieces, list(work_piece))
          jmember <- jmember + 1
        }
      } else {
        work_piece <- list(filename = .ConfigReplaceVariablesInString(quasi_final_path, replace_values),
                           namevar = namevar, indices = c(1, 1, jsdate, jmod), 
                           nmember = nmember[jmod], leadtimes = leadtimes, mask = maskmod[[jmod]],
                           is_file_per_dataset = FALSE, dimnames = exp[[jmod]][['dimnames']],
                           var_limits = c(mod_var_min, mod_var_max), remapcells = remapcells)
        exp_work_pieces <- c(exp_work_pieces, list(work_piece))
      }
      jsdate <- jsdate + 1
    }
    replace_values[extra_vars] <- NULL
    jmod <- jmod + 1
  }
  if (dims2define && length(exp) > 0) {
    .warning("no data found in file system for any experimental dataset.")
  }

  dims <- dim_exp[na.omit(match(c('dataset', 'member', 'sdate', 'ftime', 'lat', 'lon'), names(dim_exp)))]
  if (is.null(dims[['member']]) || anyNA(unlist(dims)) || any(unlist(dims) == 0)) {
    dims <- 0
    dim_exp <- NULL
  }
  if (!silent) {
    message <- "Success. Detected dimensions of experimental data: "
    .message(paste0(message, paste(unlist(dims), collapse = ', ')))
    .message("Fetching first observational files to work out 'var_obs' size...")
  }

  # If there are no experiments to load we need to choose a number of time steps
  # to load from observational datasets. We load from the first start date to 
  # the current date.
  if (is.null(exp) || identical(dims, 0)) {
    if (is.null(leadtimemax)) {
      diff <- Sys.time() - as.POSIXct(sdates[1], format = '%Y%m%d', tz = "UTC")
      if (diff > 0) {
        .warning("Loading observations only and no 'leadtimemax' specified. Data will be loaded from each starting date to current time.")
        if (storefreq == 'monthly') { 
          leadtimemax <- as.integer(diff / 30)
          if (leadtimemax == 0) leadtimemax <- 1
        } else {
          leadtimemax <- as.integer(diff)
        }
      } else {
        stop("Loading only observational data for future start dates but no 'leadtimemax' specified.")
      }
    }
    if (is.null(nleadtime)) {
      nleadtime <- leadtimemax
    }
    leadtimes <- seq(leadtimemin, leadtimemax, sampleperiod)
  }
  # Now we start iterating over observations. We try to find the output matrix
  # dimensions and we build anyway the work pieces corresponding to the observational
  # data that time-corresponds the experimental data or the time-steps until the
  # current date if no experimental datasets were specified.
  dataset_type <- 'obs'
  dim_obs <- NULL
  dims2define <- TRUE
  lat_indices <- lon_indices <- NULL
  obs_work_pieces <- list()
  is_file_per_dataset_obs <- rep(FALSE, nobs)
  is_file_per_member_obs <- rep(FALSE, nobs)
  jobs <- 1
  while (jobs <= nobs) {
    first_dataset_file_found <- FALSE
    replace_values[["OBS_NAME"]] <- obs[[jobs]][['name']]
    replace_values[["NC_VAR_NAME"]] <- obs[[jobs]][['nc_var_name']]
    replace_values[["SUFFIX"]] <- obs[[jobs]][['suffix']]
    extra_vars <- names(obs[[jobs]])[which(!(names(obs[[jobs]]) %in% obs_info_names))]
    replace_values[extra_vars] <- obs[[jobs]][extra_vars]
    namevar <- .ConfigReplaceVariablesInString(obs[[jobs]][['nc_var_name']], replace_values)
    tags_to_find <- c('START_DATE', 'MONTH', 'DAY', 'YEAR', 'MEMBER_NUMBER')
    position_of_tags <- na.omit(match(tags_to_find, names(replace_values)))
    if (length(position_of_tags) > 0) {
      quasi_final_path <- .ConfigReplaceVariablesInString(obs[[jobs]][['path']], 
                            replace_values[-position_of_tags], TRUE)
    } else {
      quasi_final_path <- .ConfigReplaceVariablesInString(obs[[jobs]][['path']], 
                            replace_values, TRUE)
    }
    is_file_per_dataset_obs[jobs] <- !any(sapply(c("$MONTH$", "$DAY$", "$YEAR$"), 
                                          grepl, quasi_final_path, fixed = TRUE))
    is_file_per_member_obs[jobs] <- grepl("$MEMBER_NUMBER$", quasi_final_path, fixed = TRUE)
    if (is.null(varmin)) {
      obs_var_min <- as.numeric(.ConfigReplaceVariablesInString(obs[[jobs]][['var_min']], replace_values))
    } else {
      obs_var_min <- varmin
    }
    if (is.null(varmax)) {
      obs_var_max <- as.numeric(.ConfigReplaceVariablesInString(obs[[jobs]][['var_max']], replace_values))
    } else {
      obs_var_max <- varmax
    }
    # This file format (file per whole dataset) is only supported in observations.
    # However a file per whole dataset experiment could be seen as a file per
    # member/ensemble experiment with a single start date, so still loadable.
    # Nonetheless file per whole dataset observational files do not need to contain
    # a year and month in the filename, the time correspondance relies on the 
    # month and years associated to each timestep inside the NetCDF file.
    # So file per whole dataset experiments need to have a start date in the filename.
    if (is_file_per_dataset_obs[jobs]) {
      ## TODO: Open file-per-dataset-files only once.
      if (dims2define) {
        work_piece <- list(dataset_type = dataset_type,
                           filename = .ConfigReplaceVariablesInString(quasi_final_path, replace_values),
                           namevar = namevar, grid = grid, remap = remap, remapcells = remapcells,
                           is_file_per_member = is_file_per_member_obs[jobs],
                           is_file_per_dataset = is_file_per_dataset_obs[jobs],
                           lon_limits = c(lonmin, lonmax), 
                           lat_limits = c(latmin, latmax), dimnames = obs[[jobs]][['dimnames']],
                           single_dataset = single_dataset)
        found_data <- .LoadDataFile(work_piece, explore_dims = TRUE, silent = silent)
        found_dims <- found_data$dims
        var_long_name <- found_data$var_long_name
        units <- found_data$units
        if (!is.null(found_dims)) {
          is_2d_var <- found_data$is_2d_var
          if (!is_2d_var && (output != 'areave')) {
            .warning(paste0("'", output, "' output format not allowed when loading global mean variables. Forcing to 'areave'."))
            output <- 'areave'
          }
          if (output == 'lonlat' || output == 'lon') {
            data_across_gw <- found_data$data_across_gw
            array_across_gw <- found_data$array_across_gw
          }
          if (output != 'areave' && is.null(grid)) {
            grid <- found_data$grid
          }
          if (is.null(nmemberobs)) {
            if (is.null(found_dims[['member']])) {
              .warning("loading observational data from a server but 'nmemberobs' not specified. Loading only one member.")
              nmemberobs <- rep(1, nobs)
            } else {
              nmemberobs <- rep(found_dims[['member']], nobs)
            }
          }
          if (is.null(dim_exp)) {
            longitudes <- found_dims[['lon']]
            latitudes <- found_dims[['lat']]
          }
          
          if (output == 'lon' || output == 'lonlat') {
            dim_obs[['lon']] <- length(longitudes)
          }
          if (output == 'lat' || output == 'lonlat') {
            dim_obs[['lat']] <- length(latitudes)
          }
          dim_obs[['ftime']] <- length(leadtimes)
          dim_obs[['member']] <- max(nmemberobs)
          dim_obs[['sdate']] <- nsdates
          dim_obs[['dataset']] <- nobs
          dims2define <- FALSE
        }
      }
      if (!first_dataset_file_found) {
        found_path <- Sys.glob(.ConfigReplaceVariablesInString(quasi_final_path, replace_values))
        if (length(found_path) > 0) {
          found_path <- head(found_path, 1)
          if (replace_globs) {
            quasi_final_path <- .ReplaceGlobExpressions(quasi_final_path, found_path, 
                                                        replace_values, tags_to_find, 
                                                        obs[[jobs]][['name']],
                                                        path_glob_permissive == 'partial')
          }
          first_dataset_file_found <- TRUE
        }
      }
      work_piece <- list(filename = .ConfigReplaceVariablesInString(quasi_final_path, replace_values),
                         namevar = namevar, indices = c(1, 1, 1, jobs), 
                         nmember = nmemberobs[jobs], 
                         mask = maskobs[[jobs]], leadtimes = leadtimes, 
                         is_file_per_dataset = is_file_per_dataset_obs[jobs], 
                         startdates = sdates, dimnames = obs[[jobs]][['dimnames']],
                         var_limits = c(obs_var_min, obs_var_max), remapcells = remapcells)
      obs_work_pieces <- c(obs_work_pieces, list(work_piece))
    } else {
      jsdate <- 1
      while (jsdate <= nsdates) {
        replace_values[["START_DATE"]] <- sdates[jsdate]        
        sdate <- sdates[jsdate]

        if (storefreq == 'daily') {
          day <- substr(sdate, 7, 8)
          if (day == '') {
            day <- '01'
          }
          day <- as.integer(day)
          startdate <- as.POSIXct(paste(substr(sdate, 1, 4), '-',
                       substr(sdate, 5, 6), '-', day, ' 12:00:00', sep = ''), tz = "UTC") + 
                       (leadtimemin - 1) * 86400
          year <- as.integer(substr(startdate, 1, 4))
          month <- as.integer(substr(startdate, 6, 7))
        } else {
          month <- (as.integer(substr(sdate, 5, 6)) + leadtimemin - 2) %% 12 + 1
          year <- as.integer(substr(sdate, 1, 4)) + (as.integer(substr(sdate, 
                  5, 6)) + leadtimemin - 2) %/% 12
        }
        jleadtime <- 1
        while (jleadtime <= length(leadtimes)) {
          replace_values[["YEAR"]] <- paste(year, '', sep = '')
          replace_values[["MONTH"]] <- sprintf("%2.2i", month)
          if (storefreq == 'daily') {
            replace_values[["DAY"]] <- sprintf("%2.2i", day)
            days_in_month <- ifelse(LeapYear(year), 29, 28)
            days_in_month <- switch(paste(month, '', sep = ''), '1' = 31, 
                                    '3' = 31, '4' = 30, '5' = 31, '6' = 30, 
                                    '7' = 31, '8' = 31, '9' = 30, '10' = 31, 
                                    '11' = 30, '12' = 31, days_in_month)
            ## This condition must be fulfilled to put all the month time steps
            ## in the dimension of length nleadtimes. Otherwise it must be cut:
            #(length(leadtimes) - 1)*sampleperiod + 1 - (jleadtime - 1)*sampleperiod >= days_in_month - day + 1
            obs_file_indices <- seq(day, min(days_in_month, (length(leadtimes) - jleadtime) * sampleperiod + day), sampleperiod)
 
          } else {
            obs_file_indices <- 1
          }
          if (is_file_per_member_obs[jobs]) {
            replace_values[["MEMBER_NUMBER"]] <- '*'
          }
          if (dims2define) {
            work_piece <- list(dataset_type = dataset_type,
                               filename = .ConfigReplaceVariablesInString(quasi_final_path, replace_values),
                               namevar = namevar, grid = grid, remap = remap, remapcells = remapcells,
                               is_file_per_member = is_file_per_member_obs[jobs],
                               is_file_per_dataset = is_file_per_dataset_obs[jobs],
                               lon_limits = c(lonmin, lonmax),
                               lat_limits = c(latmin, latmax), 
                               dimnames = obs[[jobs]][['dimnames']], single_dataset = single_dataset)
            found_data <- .LoadDataFile(work_piece, explore_dims = TRUE, silent = silent)
            found_dims <- found_data$dims
            var_long_name <- found_data$var_long_name
            units <- found_data$units
            if (!is.null(found_dims)) {
              is_2d_var <- found_data$is_2d_var
              if (!is_2d_var && (output != 'areave')) {
                .warning(paste0("'", output, "' output format not allowed when loading global mean variables. Forcing to 'areave'."))
                output <- 'areave'
              }
              if (output == 'lonlat' || output == 'lon') {
                data_across_gw <- found_data$data_across_gw
                array_across_gw <- found_data$array_across_gw
              }
              if (output != 'areave' && is.null(grid)) {
                grid <- found_data$grid
              }
              if (is.null(nmemberobs)) {
                if (is.null(found_dims[['member']])) {
                  .warning("loading observational data from a server but 'nmemberobs' not specified. Loading only one member.")
                  nmemberobs <- rep(1, nobs)
                } else {
                  nmemberobs <- rep(found_dims[['member']], nobs)
                }
              }
              if (is.null(dim_exp)) {
                longitudes <- found_dims[['lon']]
                latitudes <- found_dims[['lat']]
              }
              
              if (output == 'lon' || output == 'lonlat') {
                dim_obs[['lon']] <- length(longitudes)
              }
              if (output == 'lat' || output == 'lonlat') {
                dim_obs[['lat']] <- length(latitudes)
              }
              dim_obs[['ftime']] <- length(leadtimes)
              dim_obs[['member']] <- max(nmemberobs)
              dim_obs[['sdate']] <- nsdates
              dim_obs[['dataset']] <- nobs
              dims2define <- FALSE
            }
          }
          if (!first_dataset_file_found) {
            found_path <- Sys.glob(.ConfigReplaceVariablesInString(quasi_final_path, replace_values))
            if (length(found_path) > 0) {
              found_path <- head(found_path, 1)
              if (replace_globs) {
                quasi_final_path <- .ReplaceGlobExpressions(quasi_final_path, found_path, 
                                                            replace_values, tags_to_find, 
                                                            obs[[jobs]][['name']], 
                                                            path_glob_permissive == 'partial')
              }
              first_dataset_file_found <- TRUE
            }
          }
          if (is_file_per_member_obs[jobs]) {
            jmember <- 1
            while (jmember <= nmemberobs[jobs]) {
              replace_values[["MEMBER_NUMBER"]] <- sprintf(paste("%.", (nmemberobs[jobs] %/% 10) + 1, "i", sep = ''), jmember - 1)
              work_piece <- list(filename = .ConfigReplaceVariablesInString(quasi_final_path, replace_values),
                                 namevar = namevar, indices = c(jleadtime, jmember, jsdate, jobs), 
                                 nmember = nmemberobs[jobs], leadtimes = obs_file_indices, 
                                 mask = maskobs[[jobs]], dimnames = obs[[jobs]][['dimnames']],
                                 is_file_per_dataset = is_file_per_dataset_obs[jobs], 
                                 var_limits = c(obs_var_min, obs_var_max), remapcells = remapcells)
              obs_work_pieces <- c(obs_work_pieces, list(work_piece))
              jmember <- jmember + 1
            }
          } else {
            work_piece <- list(filename = .ConfigReplaceVariablesInString(quasi_final_path, replace_values),
                               namevar = namevar, indices = c(jleadtime, 1, jsdate, jobs), 
                               nmember = nmemberobs[jobs], leadtimes = obs_file_indices, 
                               mask = maskobs[[jobs]], dimnames = obs[[jobs]][['dimnames']],
                               is_file_per_dataset = is_file_per_dataset_obs[jobs], 
                               var_limits = c(obs_var_min, obs_var_max), remapcells)
            obs_work_pieces <- c(obs_work_pieces, list(work_piece))
          }
          
          if (storefreq == 'daily') {
            startdate <- startdate + 86400 * sampleperiod * length(obs_file_indices)
            year <- as.integer(substr(startdate, 1, 4))
            month <- as.integer(substr(startdate, 6, 7))
            day <- as.integer(substr(startdate, 9, 10))
          } else {
            month <- month + sampleperiod
            year <- year + (month - 1) %/% 12
            month <- (month - 1) %% 12 + 1
          }
          jleadtime <- jleadtime + length(obs_file_indices)
        }
        
        jsdate <- jsdate + 1
      }
    }
    replace_values[extra_vars] <- NULL
    jobs <- jobs + 1
  }
  if (dims2define && length(obs) > 0) {
    .warning("no data found in file system for any observational dataset.")
  }
  dims <- dim_obs[na.omit(match(c('dataset', 'member', 'sdate', 'ftime', 'lat', 'lon'), names(dim_obs)))]
  if (is.null(dims[['member']]) || anyNA(unlist(dims)) || any(unlist(dims) == 0)) {
    dims <- 0
    dim_obs <- NULL
  }
  if (!silent) {
    message <- "Success. Detected dimensions of observational data: "
    .message(paste0(message, paste(unlist(dims), collapse = ', ')))
  }

  if (!(is.null(dim_obs) && is.null(dim_exp))) {

  # We build two matrices in shared memory for the parallel processes to
  # store their results
  # These matrices will contain data arranged with the following
  # dimension order, to maintain data spacial locality during the 
  # parallel fetch:
  #   longitudes, latitudes, leadtimes, members, startdates, nmod/nobs
  # So [1, 1, 1, 1, 1, 1] will be next to [2, 1, 1, 1, 1, 1] in memory
  pointer_var_exp <- pointer_var_obs <- NULL
  if (!is.null(dim_exp) && (length(unlist(dim_exp)) == length(dim_exp)) && 
      !anyNA(unlist(dim_exp)) && !any(unlist(dim_exp) == 0)) {
    var_exp <- big.matrix(nrow = prod(unlist(dim_exp)), ncol = 1)
    pointer_var_exp <- bigmemory::describe(var_exp)
  }
  if (!is.null(dim_obs) && (length(unlist(dim_obs)) == length(dim_obs)) && 
      !anyNA(unlist(dim_obs)) && !any(unlist(dim_obs) == 0)) {
    var_obs <- big.matrix(nrow = prod(unlist(dim_obs)), ncol = 1)
    pointer_var_obs <- bigmemory::describe(var_obs)
  }
  if (is.null(nprocs)) {
    nprocs <- detectCores()
  }
  # We calculate the % of total progress that each work piece represents so 
  # that progress bar can be updated properly
  exp_work_piece_percent <- prod(unlist(dim_exp)) / (prod(unlist(dim_obs)) + prod(unlist(dim_exp)))
  obs_work_piece_percent <- prod(unlist(dim_obs)) / (prod(unlist(dim_obs)) + prod(unlist(dim_exp)))
  # Add some important extra fields in the work pieces before sending
  exp_work_pieces <- lapply(exp_work_pieces, function (x) c(x, list(dataset_type = 'exp', dims = unlist(dim_exp), out_pointer = pointer_var_exp)))###, progress_amount = exp_work_piece_progress)))
  obs_work_pieces <- lapply(obs_work_pieces, function (x) c(x, list(dataset_type = 'obs', dims = unlist(dim_obs), out_pointer = pointer_var_obs)))###, progress_amount = obs_work_piece_progress)))
  work_pieces <- c(exp_work_pieces, obs_work_pieces)
  # Calculate the progress %s that will be displayed and assign them to the 
  # appropriate work pieces
  if (length(work_pieces)/nprocs >= 2 && !silent) {
    if (length(work_pieces)/nprocs < 10) {
      amount <- 100/ceiling(length(work_pieces)/nprocs)
      reps <- ceiling(length(work_pieces)/nprocs)
    } else {
      amount <- 10
      reps <- 10
    }
    progress_steps <- rep(amount, reps)
    if (length(exp_work_pieces) == 0) {
      selected_exp_pieces <- c()
    } else if (length(exp_work_pieces) < floor(reps*exp_work_piece_percent) + 1) {
      selected_exp_pieces <- length(exp_work_pieces)
      progress_steps <- c(sum(head(progress_steps, 
                              floor(reps*exp_work_piece_percent))),
                          tail(progress_steps,
                               ceiling(reps*obs_work_piece_percent)))
    } else {
      selected_exp_pieces <- round(seq(1, length(exp_work_pieces), 
                                       length.out = floor(reps*exp_work_piece_percent) + 1))[-1]
    }
    if (length(obs_work_pieces) == 0) {
      selected_obs_pieces <- c()
    } else if (length(obs_work_pieces) < ceiling(reps*obs_work_piece_percent) + 1) {
      selected_obs_pieces <- length(obs_work_pieces)
      progress_steps <- c(head(progress_steps, 
                               floor(reps*exp_work_piece_percent)),
                          sum(tail(progress_steps,
                               ceiling(reps*obs_work_piece_percent))))
    } else {
      selected_obs_pieces <- round(seq(1, length(obs_work_pieces), 
                                       length.out = ceiling(reps*obs_work_piece_percent) + 1))[-1]
    }
    selected_pieces <- c(selected_exp_pieces, selected_obs_pieces + length(exp_work_pieces))
    progress_steps <- paste0(' + ', round(progress_steps, 2), '%')
    progress_message <- 'Progress: 0%'
  } else {
    progress_message <- ''
    selected_pieces <- NULL
  }
  piece_counter <- 1
  step_counter <- 1
  work_pieces <- lapply(work_pieces, 
                 function (x) {
                   wp <- c(x, list(is_2d_var = is_2d_var, grid = grid, remap = remap,  
                                   lon_limits = c(lonmin, lonmax), 
                                   lat_limits = c(latmin, latmax), 
                                   output = output, remapcells = remapcells,
                                   single_dataset = single_dataset))
                   if (piece_counter %in% selected_pieces) {
                     wp <- c(wp, list(progress_amount = progress_steps[step_counter]))
                     step_counter <<- step_counter + 1
                   }
                   piece_counter <<- piece_counter + 1
                   wp
                 })
  if (!silent) {
    .message(paste("Will now proceed to read and process ", length(work_pieces), " data files:", sep = ''))
    if (length(work_pieces) < 30) {
      lapply(work_pieces, function (x) .message(x[['filename']], indent = 2))
    } else {
      .message("The list of files is long. You can check it after Load() finishes in the output '$source_files'.", indent = 2, exdent = 5)
    }
    if (length(dim_obs) == 0) {
      bytes_obs <- 0
      obs_dim_sizes <- '0'
    } else {
      bytes_obs <- prod(c(unlist(dim_obs), 8))
      obs_dim_sizes <- paste(na.omit(as.vector(unlist(dim_obs)[c('dataset', 'member', 'sdate', 'ftime', 'lat', 'lon')])), collapse = ' x ')
    }
    if (length(dim_exp) == 0) {
      bytes_exp <- 0
      exp_dim_sizes <- '0'
    } else {
      bytes_exp <- prod(c(unlist(dim_exp), 8))
      exp_dim_sizes <- paste(na.omit(as.vector(unlist(dim_exp)[c('dataset', 'member', 'sdate', 'ftime', 'lat', 'lon')])), collapse = ' x ')
    }
    .message(paste("Total size of requested data: ", bytes_obs + bytes_exp, "bytes."))
    .message(paste("- Experimental data:  (", exp_dim_sizes, ") x 8 bytes =", bytes_exp, "bytes."), indent = 2)
    .message(paste("- Observational data: (", obs_dim_sizes, ") x 8 bytes =", bytes_obs, "bytes."), indent = 2)
    .message("If size of requested data is close to or above the free shared RAM memory, R will crash.")
  }
  # Build the cluster of processes that will do the work and dispatch work pieces.
  # The function .LoadDataFile is applied to each work package. This function will
  # open the data file, regrid if needed, trim (select time steps, longitudes, 
  # latitudes, members), apply the mask, compute and apply the weights if needed,
  # disable extreme values and store in the shared memory matrices. 
  if (nprocs == 1) {
    found_files <- lapply(work_pieces, .LoadDataFile, silent = silent)
  } else {
    cluster <- makeCluster(nprocs, outfile = "")
    # Open connections to keep track of progress
    ###range_progress_ports <- c(49000, 49999)
    ###progress_ports <- as.list(sample(range_progress_ports[2] - range_progress_ports[1], nprocs) + range_progress_ports[1])

    # Open from master side
    ###connection_set_up_job <- mcparallel({
    ###  progress_connections <- vector('list', length(progress_ports))
    ###  for (connection in 1:length(progress_ports)) {
    ###    attempts <- 0
    ###    max_attempts <- 3
    ###    while (is.null(progress_connections[[connection]]) && attempts < max_attempts) {
    ###      Sys.sleep(2)
    ###      suppressWarnings({
    ###        progress_connections[[connection]] <- try({
    ###          socketConnection(port = progress_ports[[connection]], open = 'w+b')
    ###        }, silent = TRUE)
    ###      })
    ###      if (!('sockconn' %in% class(progress_connections[[connection]]))) {
    ###        progress_connections[[connection]] <- NULL
    ###      }
    ###      attempts <- attempts + 1
    ###    }
    ###  }

      # And start polling the sockets and update progress bar
    ###  if (!any( lapply is.null!!! is.null(progress_connections))) {
    ###    progress <- 0.0
    ###    pb <- txtProgressBar(0, 1, style = 3)
    ###    stop_polling <- FALSE
    ###    attempts <- 0
    ###    max_attempts <- 3
    ###    while (progress < 0.999 && !stop_polling) {
    ###      Sys.sleep(3)
    ###      progress_obtained <- lapply(progress_connections, function(x) as.numeric(readBin(x, 'double')))
    ###      total_progress_obtained <- sum(unlist(progress_obtained))
    ###      if (total_progress_obtained > 0) {
    ###        progress <- progress + total_progress_obtained
    ###        setTxtProgressBar(pb, progress)
    ###        attempts <- 0
    ###      } else {
    ###        attempts <- attempts + 1
    ###        if (attempts >= max_attempts) {
    ###          stop_polling <- TRUE
    ###        }
    ###      }
    ###    }
    ###  }
    ###})

    # Open from the workers sidtr(data)
    ###open_connections <- clusterApply(cluster, progress_ports, 
    ###  function (x) {
    ###    progress_connection <<- NULL
    ###    progress_connection <<- try({
    ###      socketConnection(server = TRUE, port = x, open = 'w+b')
    ###    })
    ###    if ('sockconn' %in% class(progress_connection)) {
    ###      TRUE
    ###    } else {
    ###      progress_connection <<- NULL
    ###      FALSE
    ###    }
    ###  })

    ###if (!all(unlist(open_connections))) {
    ###  if (!silent) {
    ###    cat(paste("! Warning: failed to open connections in ports", process_track_ports[1], "to", process_track_ports[2], "to keep track of progress. Progress bar will not be displayed\n"))
    ###  }
    ###}

    if (!silent) {
      .message("Loading... This may take several minutes...")
      if (progress_message != '') {
        .message(progress_message, appendLF = FALSE)
      }
    }
    # Send the heavy work to the workers
    work_errors <- try({
      found_files <- clusterApplyLB(cluster, work_pieces, .LoadDataFile, silent = silent)
    })
    stopCluster(cluster)
  }
  if (!silent) {
    if (progress_message != '') {
      .message("\n")
    }
    if (any(unlist(lapply(found_files, is.null)))) {
      if (sum(unlist(lapply(found_files, is.null))) < 30) {
        warning_text <- "The following files were not found in the file system. Filling with NA values instead.\n"
        warning_text <- paste0(warning_text, do.call(paste, lapply(work_pieces[which(unlist(lapply(found_files, is.null)))], function (x) paste0("  ", x[['filename']], "\n"))))
        .warning(warning_text)
      } else {
        .warning("Some files were not found in the file system. The list is long. You can check it in the output '$not_found_files'. Filling with NA values instead.")
      }
    }
  }
  source_files <- unlist(found_files[which(!unlist(lapply(found_files, is.null)))])
  not_found_files <- unlist(lapply(work_pieces[which(unlist(lapply(found_files, is.null)))], '[[', 'filename'))

  } else {
    error_message <- "Error: No found files for any dataset. Check carefully the file patterns and correct either the pattern or the provided parameters:\n"
    tags_to_find <- c('START_DATE', 'YEAR', 'MONTH', 'DAY', 'MEMBER_NUMBER')
    position_of_tags <- na.omit(match(tags_to_find, names(replace_values)))
    if (!is.null(exp)) {
      lapply(exp, function (x) {
        replace_values[["EXP_NAME"]] <- x[['name']]
        replace_values[["NC_VAR_NAME"]] <- x[['nc_var_name']]
        replace_values[["SUFFIX"]] <- x[['suffix']]
        extra_vars <- names(x)[which(!(names(x) %in% exp_info_names))]
        replace_values[extra_vars] <- x[extra_vars]
        if (length(position_of_tags) > 0) {
          quasi_final_path <- .ConfigReplaceVariablesInString(x[['path']],
                                replace_values[-position_of_tags], TRUE)
        } else {
          quasi_final_path <- .ConfigReplaceVariablesInString(x[['path']],
                                replace_values, TRUE)
        }
        error_message <<- paste0(error_message, paste0(quasi_final_path, '\n'))
        replace_values[extra_vars] <- NULL
      })
    }
    if (!is.null(obs)) {
      lapply(obs, function (x) {
        replace_values[["OBS_NAME"]] <- x[['name']]
        replace_values[["NC_VAR_NAME"]] <- x[['nc_var_name']]
        replace_values[["SUFFIX"]] <- x[['suffix']]
        extra_vars <- names(x)[which(!(names(x) %in% obs_info_names))]
        replace_values[extra_vars] <- x[extra_vars]
        if (length(position_of_tags) > 0) {
          quasi_final_path <- .ConfigReplaceVariablesInString(x[['path']],
                                replace_values[-position_of_tags], TRUE)
        } else {
          quasi_final_path <- .ConfigReplaceVariablesInString(x[['path']],
                                replace_values, TRUE)
        }
        error_message <<- paste0(error_message, paste0(quasi_final_path, '\n'))
        replace_values[extra_vars] <- NULL
      })
    }
    stop(error_message)
  }

  })

  if (inherits(errors, 'try-error')) {
    invisible(list(load_parameters = load_parameters))
  } else {
    # Before ending, the data is arranged in the common format, with the following
    # dimension order:
    #  nmod/nobs, members, startdates, leadtimes, latitudes, longitudes
    # and the metadata is generated following the conventions in downscaleR.
    variable <- list(varName = var, level = NULL)
    attr(variable, 'use_dictionary') <- FALSE
    attr(variable, 'units') <- units
    attr(variable, 'longname') <- var_long_name
    attr(variable, 'description') <- 'none'
    attr(variable, 'daily_agg_cellfun') <- 'none'
    attr(variable, 'monthly_agg_cellfun') <- 'none'
    attr(variable, 'verification_time') <- 'none'
    
    number_ftime <- NULL
    if (is.null(var_exp)) {
      mod_data <- NULL
    } else {
      dim_reorder <- length(dim_exp):1
      dim_reorder[2:3] <- dim_reorder[3:2]
      old_dims <- dim_exp
      dim_exp <- dim_exp[dim_reorder]
      mod_data <-
        aperm(array(bigmemory::as.matrix(var_exp), dim = unlist(old_dims)), dim_reorder)
      attr(mod_data, 'dimensions') <- names(dim_exp)
      names(dim(mod_data)) <- names(dim_exp)
      number_ftime <- dim_exp[["ftime"]]
    }
    
    if (is.null(var_obs)) {
      obs_data <- NULL
    } else {
      dim_reorder <- length(dim_obs):1
      dim_reorder[2:3] <- dim_reorder[3:2]
      old_dims <- dim_obs
      dim_obs <- dim_obs[dim_reorder]
      obs_data <-
        aperm(array(bigmemory::as.matrix(var_obs), dim = old_dims), dim_reorder)
      attr(obs_data, 'dimensions') <- names(dim_obs)
      names(dim(obs_data)) <- names(dim_obs)
      if (is.null(number_ftime)) {
        number_ftime <- dim_obs[["ftime"]]
      }
    }
    if (is.null(latitudes)) {
      lat <- 0
      attr(lat, 'cdo_grid_name') <- 'none'
      attr(lat, 'first_lat') <- 'none'
      attr(lat, 'last_lat') <- 'none'
    } else {
      lat <- latitudes
      attr(lat, 'cdo_grid_name') <-
        if (is.null(grid))
          'none'
      else
        grid
      attr(lat, 'first_lat') <- tail(lat, 1)
      attr(lat, 'last_lat') <- head(lat, 1)
    }
    attr(lat, 'projection') <- 'none'
    
    if (is.null(longitudes)) {
      lon <- 0
      attr(lon, 'cdo_grid_name') <- 'none'
      attr(lon, 'data_across_gw') <- 'none'
      attr(lon, 'array_across_gw') <- 'none'
      attr(lon, 'first_lon') <- 'none'
      attr(lon, 'last_lon') <- 'none'
    } else {
      lon <- longitudes
      attr(lon, 'cdo_grid_name') <-
        if (is.null(grid))
          'none'
      else
        grid
      attr(lon, 'data_across_gw') <- data_across_gw
      attr(lon, 'array_across_gw') <- array_across_gw
      attr(lon, 'first_lon') <- lon[which.min(abs(lon - lonmin))]
      attr(lon, 'last_lon') <- lon[which.min(abs(lon - lonmax))]
    }
    attr(lon, 'projection') <- 'none'
    
    dates <- list()
    ## we must put a start and end time for each prediction c(start date, forecast time)
    if (storefreq == 'minutely') {
      store_period <- 'min'
    } else if (storefreq == 'hourly') {
      store_period <- 'hour'
    } else if (storefreq == 'daily') {
      store_period <- 'DSTday'
    } else if (storefreq == 'monthly') {
      store_period <- 'month'
    }
    
    addTime <- function(date, period, n = 1) {
      seq(date, by = paste(n, period), length = 2)[2]
    }
    
    # We build dates, a list with components start and end.
    # Start is a list with as many components as start dates.
    # Each component is a vector of the initial POSIXct date of each
    # forecast time step
    dates[["start"]] <- do.call(c, lapply(sdates,
      function(x) {
        do.call(c, lapply((0:(number_ftime - 1)) * sampleperiod,
          function(y) {
            addTime(as.POSIXct(x, format = "%Y%m%d", tz = "UTC"),
                    store_period, y + leadtimemin - 1)
          }))
      }))
    attr(dates[["start"]], "tzone") <- "UTC"
    # end is similar to start, but contains the end dates of each forecast 
    # time step
    dates[["end"]] <- do.call(c, lapply(dates[["start"]],
      function(x) {
        do.call(c, lapply(x,
          function(y) {
            addTime(y, store_period)
          }))
      }))
    attr(dates[["end"]], "tzone") <- "UTC"

    tags_to_find <- c('START_DATE', 'MEMBER_NUMBER', 'YEAR', 'MONTH', 'DAY')
    position_of_tags <- na.omit(match(tags_to_find, names(replace_values)))
    if (length(position_of_tags) > 0) {
      replace_values <- replace_values[-position_of_tags]
    }
    models <- NULL
    if (length(exp) > 0 && !is.null(dim_exp)) {
      models <- list()
      for (jmod in 1:length(exp)) {
        member_names <- paste0("Member_", 1:nmember[jmod])

        InitializationDates <- do.call(c, lapply(member_names,
            function(x) {
              do.call(c, lapply(sdates, function(y) {
                as.POSIXct(y, format = "%Y%m%d", tz = "UTC")
              }))
            }))
        attr(InitializationDates, "tzone") <- "UTC"

        models[[exp[[jmod]][["name"]]]] <- list(
          InitializationDates = InitializationDates,
          Members = member_names)

        names(models[[exp[[jmod]][["name"]]]]$InitializationDates) <- member_names
        attr(models[[exp[[jmod]][["name"]]]], 'dataset') <- exp[[jmod]][["name"]]
        attr(models[[exp[[jmod]][["name"]]]], 'source') <- {
          quasi_final_path <- .ConfigReplaceVariablesInString(exp[[jmod]][['path']],
                                replace_values, TRUE)
          if ((nchar(quasi_final_path) -
              nchar(gsub("/", "", quasi_final_path)) > 2) &&
              (length(sdates) > 1 && !is_file_per_member_exp[jmod])) {
            parts <- strsplit(quasi_final_path, "/")[[1]]
            paste(parts[-length(parts)], sep = "", collapse = "/")
          } else {
            quasi_final_path
          }
        }
        attr(models[[exp[[jmod]][["name"]]]], 'URL') <- 'none'
      }
    }

    observations <- NULL
    if (length(obs) > 0 && !is.null(dim_obs)) {
      observations <- list()
      for (jobs in 1:length(obs)) {
        member_names <- paste0("Member_", 1:nmemberobs[jobs])
        observations[[obs[[jobs]][["name"]]]] <- list(
          InitializationDates = lapply(member_names,
            function(x) {
              do.call(c, lapply(sdates, function(y) {
                as.POSIXct(y, format = "%Y%m%d")
              }))
            }),
          Members = member_names)
        names(observations[[obs[[jobs]][["name"]]]]$InitializationDates) <- member_names
        attr(observations[[obs[[jobs]][["name"]]]], 'dataset') <- obs[[jobs]][["name"]]
        attr(observations[[obs[[jobs]][["name"]]]], 'source') <- {
            quasi_final_path <- .ConfigReplaceVariablesInString(obs[[jobs]][['path']],
                                  replace_values, TRUE)
            if ((nchar(quasi_final_path) -
                nchar(gsub("/", "", quasi_final_path)) > 2) &&
                !is_file_per_dataset_obs[jobs]) {
              parts <- strsplit(quasi_final_path, "/")[[1]]
              paste(parts[-length(parts)], sep = "", collapse = "/")
            } else {
              quasi_final_path
            }
          }
        attr(observations[[obs[[jobs]][["name"]]]], 'URL') <- 'none'
      }
    }

    invisible(list(mod = mod_data,
                   obs = obs_data,
                   lon = lon,
                   lat = lat,
                   Variable = variable,
                   Datasets = list(exp = models, obs = observations),
                   Dates = dates,
                   when = Sys.time(),
                   source_files = source_files,
                   not_found_files = not_found_files,
                   load_parameters = load_parameters))
  }
}
