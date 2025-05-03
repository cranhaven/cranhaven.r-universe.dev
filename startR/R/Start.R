#'Declare, discover, subset and retrieve multidimensional distributed data sets
#'
#'See the \href{https://earth.bsc.es/gitlab/es/startR}{startR documentation and
#'tutorial} for a step-by-step explanation on how to use Start().\cr\cr
#'Nowadays in the era of big data, large multidimensional data sets from 
#'diverse sources need to be combined and processed. Analysis of big data in any
#'field is often highly complex and time-consuming. Taking subsets of these data
#'sets and processing them efficiently become an indispensable practice. This 
#'technique is also known as Domain Decomposition, Map Reduce or, more commonly,
#''chunking'.\cr\cr
#'startR (Subset, TrAnsform, ReTrieve, arrange and process large 
#'multidimensional data sets in R) is an R project started at BSC with the aim 
#'to develop a tool that allows the user to automatically process large 
#'multidimensional distributed data sets. It is an open source project that is 
#'open to external collaboration and funding, and will continuously evolve to 
#'support as many data set formats as possible while maximizing its efficiency.\cr\cr
#'startR provides a framework under which a data set (collection of one 
#'or multiple data files, potentially distributed over various remote servers) 
#'are perceived as if they all were part of a single large multidimensional 
#'array. Once such multidimensional array is declared, any user-defined function
#'can be applied to the data in a \code{apply}-like fashion, where startR
#'transparently implements the Map Reduce paradigm. The steps to follow in order
#'to process a collection of big data sets are as follows:\cr
#'\itemize{
#'  \item{
#'Declaring the data set, i.e. declaring the distribution of the data files 
#'involved, the dimensions and shape of the multidimensional array, and the 
#'boundaries of the target data. This step can be performed with the 
#'Start() function. Numeric indices or coordinate values can be used when
#'fixing the boundaries. It is common having the need to apply transformations, 
#'pre-processing or reordering to the data. Start() accepts user-defined 
#'transformation or reordering functions to be applied for such purposes. Once a
#'data set is declared, a list of involved files, dimension lengths, memory size
#'and other metadata is made available. Optionally, the data set can be 
#'retrieved and loaded onto the current R session if it is small enough. 
#'  }
#'  \item{
#'Declaring the workflow of operations to perform on the involved data set(s).
#'This step can be performed with the Step() and AddStep() functions.
#'  }
#'  \item{
#'Defining the computation settings. The mandatory settings include a) how many
#'subsets to divide the data sets into and along which dimensions; b) which 
#'platform to perform the workflow of operations on (local machine or remote 
#'machine/HPC?), how to communicate with it (unidirectional or bidirectional 
#'connection? shared or separate file systems?), which queuing system it uses 
#'(slurm, PBS, LSF, none?); and c) how many parallel jobs and execution threads
#'per job to use when running the calculations. This step can be performed when 
#'building up the call to the Compute() function.
#'  }
#'  \item{
#'Running the computation. startR transparently implements the Map Reduce 
#'paradigm, according to the settings in the previous steps. The progress can 
#'optionally be monitored with the EC-Flow workflow management tool. When the 
#'computation ends, a report of performance timings is displayed. This step can 
#'be triggered with the Compute() function.
#'  }
#'}
#'startR is not bound to a specific file format. Interface functions to
#'custom file formats can be provided for Start() to read them. As this
#'version, startR includes interface functions to the following file formats:
#'\itemize{
#'  \item{
#'NetCDF
#'  }
#'}
#'Metadata and auxilliary data is also preserved and arranged by Start()
#'in the measure that it is retrieved by the interface functions for a specific 
#'file format.
#'
#'@param \dots A selection of custemized parameters depending on the data 
#'format. When we retrieve data from one or a collection of data sets, 
#'the involved data can be perceived as belonging to a large multi-dimensional 
#'array. For instance, let us consider an example case. We want to retrieve data
#'from a source, which contains data for the number of monthly sales of various 
#'items, and also for their retail price each month. The data on source is 
#'stored as follows:\cr\cr
#'\command{
#'\cr #  /data/
#'\cr #    |-> sales/
#'\cr #    |    |-> electronics
#'\cr #    |    |    |-> item_a.data
#'\cr #    |    |    |-> item_b.data
#'\cr #    |    |    |-> item_c.data
#'\cr #    |    |-> clothing
#'\cr #    |         |-> item_d.data
#'\cr #    |         |-> idem_e.data
#'\cr #    |         |-> idem_f.data
#'\cr #    |-> prices/
#'\cr #         |-> electronics
#'\cr #         |    |-> item_a.data
#'\cr #         |    |-> item_b.data
#'\cr #         |    |-> item_c.data
#'\cr #         |-> clothing
#'\cr #              |-> item_d.data
#'\cr #              |-> item_e.data
#'\cr #              |-> item_f.data
#'}\cr\cr
#'Each item file contains data, stored in whichever format, for the sales or 
#'prices over a time period, e.g. for the past 24 months, registered at 100 
#'different stores over the world. Whichever the format it is stored in, each 
#'file can be perceived as a container of a data array of 2 dimensions, time and
#'store. Let us assume the '.data' format allows to keep a name for each of 
#'these dimensions, and the actual names are 'time' and 'store'.\cr\cr
#'The different item files for sales or prices can be perceived as belonging to 
#'an 'item' dimension of length 3, and the two groups of three items to a 
#''section' dimension of length 2, and the two groups of two sections (one with
#'the sales and the other with the prices) can be perceived as belonging also to
#'another dimension 'variable' of length 2. Even the source can be perceived as 
#'belonging to a dimension 'source' of length 1.\cr\cr
#'All in all, in this example, the whole data could be perceived as belonging to
#'a multidimensional 'large array' of dimensions\cr
#'\command{
#'\cr #  source variable  section      item    store    month
#'\cr #       1        2        2         3      100       24
#'}
#'\cr\cr
#'The dimensions of this 'large array' can be classified in two types. The ones 
#'that group actual files (the file dimensions) and the ones that group data 
#'values inside the files (the inner dimensions). In the example, the file 
#'dimensions are 'source', 'variable', 'section' and 'item', whereas the inner 
#'dimensions are 'store' and 'month'.
#'\cr\cr
#'Having the dimensions of our target sources in mind, the parameter \code{\dots} 
#'expects to receive information on:
#'    \itemize{
#'      \item{
#'The names of the expected dimensions of the 'large dataset' we want to 
#'retrieve data from
#'      }
#'      \item{
#'The indices to take from each dimension (and other constraints)
#'      }
#'      \item{
#'How to reorder the dimension if needed
#'      }
#'      \item{
#'The location and organization of the files of the data sets
#'      }
#'    }
#'For each dimension, the 3 first information items can be specified with a set
#'of parameters to be provided through \code{\dots}. For a given dimension 
#''dimname', six parameters can be specified:\cr
#'\command{
#'\cr # dimname = <indices_to_take>,  # 'all' / 'first' / 'last' /
#'\cr #                               # indices(c(1, 10, 20)) /
#'\cr #                               # indices(c(1:20)) /
#'\cr #                               # indices(list(1, 20)) /
#'\cr #                               # c(1, 10, 20) / c(1:20) /
#'\cr #                               # list(1, 20)
#'\cr # dimname_var = <name_of_associated_coordinate_variable>,
#'\cr # dimname_tolerance = <tolerance_value>,
#'\cr # dimname_reorder = <reorder_function>,
#'\cr # dimname_depends = <name_of_another_dimension>,
#'\cr # dimname_across = <name_of_another_dimension>
#'}
#'\cr\cr
#'The \bold{indices to take} can be specified in three possible formats (see 
#'code comments above for examples). The first format consists in using 
#'character tags, such as 'all' (take all the indices available for that 
#'dimension), 'first' (take only the first) and 'last' (only the last). The 
#'second format consists in using numeric indices, which have to be wrapped in a
#'call to the indices() helper function. For the second format, either a
#'vector of numeric indices can be provided, or a list with two numeric indices 
#'can be provided to take all the indices in the range between the two specified
#'indices (both extremes inclusive). The third format consists in providing a 
#'vector character strings (for file dimensions) or of values of whichever type
#'(for inner dimensions). For the file dimensions, the provided character 
#'strings in the third format will be used as components to build up the final 
#'path to the files (read further). For inner dimensions, the provided values in
#'the third format will be compared to the values of an associated coordinate 
#'variable (must be specified in '<dimname>_reorder', read further), and the 
#'indices of the closest values will be retrieved. When using the third format, 
#'a list with two values can also be provided to take all the indices of the 
#'values within the specified range.
#'\cr\cr
#'The \bold{name of the associated coordinate variable} must be a character 
#'string with the name of an associated coordinate variable to be found in the 
#'data files (in all* of them). For this to work, a 'file_var_reader' 
#'function must be specified when calling Start() (see parameter 
#''file_var_reader'). The coordinate variable must also be requested in the 
#'parameter 'return_vars' (see its section for details). This feature only 
#'works for inner dimensions.
#'\cr\cr
#'The \bold{tolerance value} is useful when indices for an inner dimension are 
#'specified in the third format (values of whichever type). In that case, the 
#'indices of the closest values in the coordinate variable are seeked. However 
#'the closest value might be too distant and we would want to consider no real 
#'match exists for such provided value. This is possible via the tolerance,
#'which allows to specify a threshold beyond which not to seek for matching 
#'values and mark that index as missing value.
#'\cr\cr
#'The \bold{reorder_function} is useful when indices for an inner dimension are
#'specified in the third fromat, and the retrieved indices need to be reordered 
#'in function of their provided associated variable values. A function can be 
#'provided, which receives as input a vector of values, and returns as outputs a
#'list with the components \code{$x} with the reordered values, and \code{$ix} 
#'with the permutation indices. Two reordering functions are included in 
#'startR, the Sort() and the CircularSort().
#'\cr\cr
#'The \bold{name of another dimension} to be specified in <dimname>_depends,
#'only available for file dimensions, must be a character string with the name 
#'of another requested \bold{file dimension} in \code{\dots}, and will make 
#'Start() aware that the path components of a file dimension can vary in
#'function of the path component of another file dimension. For instance, in the
#'example above, specifying \code{item_depends = 'section'} will make 
#'Start() aware that the item names vary in function of the section, i.e.
#'section 'electronics' has items 'a', 'b' and 'c' but section 'clothing' has 
#'items 'd', 'e', 'f'. Otherwise Start() would expect to find the same 
#'item names in all the sections.
#'If values() is used to define dimensions, it is possible to provide different 
#'values of the depending dimension for each depended dimension values. For 
#'example, if \code{section = c('electronics', 'clothing')}, we can use
#'\code{item = list(electronics = c('a', 'b', 'c'), clothing = c('d', 'e', 'f'))}.
#'\cr\cr
#'The \bold{name of another dimension} to be specified in '<dimname>_across',
#'only available for inner dimensions, must be a character string with the name 
#'of another requested \bold{inner dimension} in \code{\dots}, and will make 
#'Start() aware that an inner dimension extends along multiple files. For
#'instance, let us imagine that in the example above, the records for each item 
#'are so large that it becomes necessary to split them in multiple files each 
#'one containing the registers for a different period of time, e.g. in 10 files 
#'with 100 months each ('item_a_period1.data', 'item_a_period2.data', and so on).
#'In that case, the data can be perceived as having an extra file dimension, the 
#''period' dimension. The inner dimension 'month' would extend across multiple 
#'files, and providing the parameter \code{month = indices(1, 300)} would make 
#'Start() crash because it would perceive we have made a request out of 
#'bounds (each file contains 100 'month' indices, but we requested 1 to 300). 
#'This can be solved by specifying the parameter \code{month_across = period} (a
#'long with the full specification of the dimension 'period').
#'\cr\cr
#'\bold{Defining the path pattern}
#'\cr
#'As mentioned above, the parameter \dots also expects to receive information 
#'with the location of the data files. In order to do this, a special dimension 
#'must be defined. In that special dimension, in place of specifying indices to 
#'take, a path pattern must be provided. The path pattern is a character string 
#'that encodes the way the files are organized in their source. It must be a 
#'path to one of the data set files in an accessible local or remote file system,
#'or a URL to one of the files provided by a local or remote server. The regions
#'of this path that vary across files (along the file dimensions) must be 
#'replaced by wildcards. The wildcards must match any of the defined file 
#'dimensions in the call to Start() and must be delimited with heading 
#'and trailing '$'. Shell globbing expressions can be used in the path pattern. 
#'See the next code snippet for an example of a path pattern.
#'\cr\cr
#'All in all, the call to Start() to load the entire data set in the 
#'example of store item sales, would look as follows:
#'\cr
#'\command{
#'\cr # data <- Start(source = paste0('/data/$variable$/',
#'\cr #                               '$section$/$item$.data'),
#'\cr #               variable = 'all',
#'\cr #               section = 'all',
#'\cr #               item = 'all',
#'\cr #               item_depends = 'section',
#'\cr #               store = 'all',
#'\cr #               month = 'all')
#'}
#'\cr\cr
#'Note that in this example it would still be pending to properly define the 
#'parameters 'file_opener', 'file_closer', 'file_dim_reader', 
#''file_var_reader' and 'file_data_reader' for the '.data' file format
#'(see the corresponding sections).
#'\cr\cr
#'The call to Start() will return a multidimensional R array with the 
#'following dimensions:
#'\cr
#'\command{
#'\cr #  source variable  section      item    store    month
#'\cr #       1        2        2         3      100       24
#'}
#'\cr
#'The dimension specifications in the \code{\dots} do not have to follow any 
#'particular order. The returned array will have the dimensions in the same order
#'as they have been specified in the call. For example, the following call:
#'\cr
#'\command{
#'\cr # data <- Start(source = paste0('/data/$variable$/',
#'\cr #                               '$section$/$item$.data'),
#'\cr #               month = 'all',
#'\cr #               store = 'all',
#'\cr #               item = 'all',
#'\cr #               item_depends = 'section',
#'\cr #               section = 'all',
#'\cr #               variable = 'all')
#'}
#'\cr\cr
#'would return an array with the following dimensions:
#'\cr
#'\command{
#'\cr #  source    month    store      item  section variable
#'\cr #       1       24      100         3        2        2
#'}
#'\cr\cr
#'Next, a more advanced example to retrieve data for only the sales records, for
#'the first section ('electronics'), for the 1st and 3rd items and for the 
#'stores located in Barcelona (assuming the files contain the variable 
#''store_location' with the name of the city each of the 100 stores are located 
#'at):
#'\cr
#'\command{
#'\cr # data <- Start(source = paste0('/data/$variable$/',
#'\cr #                               '$section$/$item$.data'),
#'\cr #               variable = 'sales',
#'\cr #               section = 'first',
#'\cr #               item = indices(c(1, 3)),
#'\cr #               item_depends = 'section',
#'\cr #               store = 'Barcelona',
#'\cr #               store_var = 'store_location',
#'\cr #               month = 'all',
#'\cr #               return_vars = list(store_location = NULL))
#'}
#'\cr\cr
#'The defined names for the dimensions do not necessarily have to match the 
#'names of the dimensions inside the file. Lists of alternative names to be 
#'seeked can be defined in the parameter 'synonims'.
#'\cr\cr
#'If data from multiple sources (not necessarily following the same structure) 
#'has to be retrieved, it can be done by providing a vector of character strings
#'with path pattern specifications, or, in the extended form, by providing a 
#'list of lists with the components 'name' and 'path', and the name of the 
#'dataset and path pattern as values, respectively. For example:
#'\cr
#'\command{
#'\cr # data <- Start(source = list(
#'\cr #                 list(name = 'sourceA',
#'\cr #                      path = paste0('/sourceA/$variable$/',
#'\cr #                                    '$section$/$item$.data')),
#'\cr #                 list(name = 'sourceB',
#'\cr #                      path = paste0('/sourceB/$section$/',
#'\cr #                                    '$variable$/$item$.data'))
#'\cr #               ),
#'\cr #               variable = 'sales',
#'\cr #               section = 'first',
#'\cr #               item = indices(c(1, 3)),
#'\cr #               item_depends = 'section',
#'\cr #               store = 'Barcelona',
#'\cr #               store_var = 'store_location',
#'\cr #               month = 'all',
#'\cr #               return_vars = list(store_location = NULL))
#'}
#'\cr
#'
#'@param return_vars A named list where the names are the names of the 
#'variables to be fetched in the files, and the values are vectors of 
#'character strings with the names of the file dimension which to retrieve each
#'variable for, or NULL if the variable has to be retrieved only once 
#'from any (the first) of the involved files.\cr\cr
#'Apart from retrieving a multidimensional data array, retrieving auxiliary 
#'variables inside the files can also be needed. The parameter 
#''return_vars' allows for requesting such variables, as long as a 
#''file_var_reader' function is also specified in the call to 
#'Start() (see documentation on the corresponding parameter). 
#'\cr\cr
#'In the case of the the item sales example (see documentation on parameter 
#'\code{\dots)}, the store location variable is requested with the parameter\cr 
#'\code{return_vars = list(store_location = NULL)}.\cr This will cause 
#'Start() to fetch once the variable 'store_location' and return it in 
#'the component\cr \code{$Variables$common$store_location},\cr and will be an 
#'array of character strings with the location names, with the dimensions 
#'\code{c('store' = 100)}. Although useless in this example, we could ask 
#'Start() to fetch and return such variable for each file along the 
#'items dimension as follows: \cr 
#'\code{return_vars = list(store_location = c('item'))}.\cr In that case, the 
#'variable will be fetched once from a file of each of the items, and will be 
#'returned as an array with the dimensions \code{c('item' = 3, 'store' = 100)}.
#'\cr\cr
#'If a variable is requested along a file dimension that contains path pattern 
#'specifications ('source' in the example), the fetched variable values will be 
#'returned in the component\cr \code{$Variables$<dataset_name>$<variable_name>}.\cr 
#'For example:
#'\cr
#'\command{
#'\cr # data <- Start(source = list(
#'\cr #                 list(name = 'sourceA',
#'\cr #                      path = paste0('/sourceA/$variable$/',
#'\cr #                                    '$section$/$item$.data')),
#'\cr #                 list(name = 'sourceB',
#'\cr #                      path = paste0('/sourceB/$section$/',
#'\cr #                                    '$variable$/$item$.data'))
#'\cr #               ),
#'\cr #               variable = 'sales',
#'\cr #               section = 'first',
#'\cr #               item = indices(c(1, 3)),
#'\cr #               item_depends = 'section',
#'\cr #               store = 'Barcelona',
#'\cr #               store_var = 'store_location',
#'\cr #               month = 'all',
#'\cr #               return_vars = list(store_location = c('source',
#'\cr #                                                     'item')))
#'\cr # # Checking the structure of the returned variables
#'\cr # str(found_data$Variables)
#'\cr # Named list
#'\cr # ..$common: NULL
#'\cr # ..$sourceA: Named list
#'\cr # .. ..$store_location: char[1:18(3d)] 'Barcelona' 'Barcelona' ...
#'\cr # ..$sourceB: Named list
#'\cr # .. ..$store_location: char[1:18(3d)] 'Barcelona' 'Barcelona' ...
#'\cr # # Checking the dimensions of the returned variable
#'\cr # # for the source A
#'\cr # dim(found_data$Variables$sourceA)
#'\cr #     item   store
#'\cr #        3       3
#'}
#'\cr\cr
#'The names of the requested variables do not necessarily have to match the 
#'actual variable names inside the files. A list of alternative names to be 
#'seeked can be specified via the parameter 'synonims'.
#'
#'@param synonims A named list where the names are the requested variable or 
#'dimension names, and the values are vectors of character strings with 
#'alternative names to seek for such dimension or variable.\cr\cr
#'In some requests, data from different sources may follow different naming 
#'conventions for the dimensions or variables, or even files in the same source
#'could have varying names. This parameter is in order for Start() to 
#'properly identify the dimensions or variables with different names.
#'\cr\cr
#'In the example used in parameter 'return_vars', it may be the case that 
#'the two involved data sources follow slightly different naming conventions. 
#'For example, source A uses 'sect' as name for the sections dimension, whereas 
#'source B uses 'section'; source A uses 'store_loc' as variable name for the 
#'store locations, whereas source B uses 'store_location'. This can be taken 
#'into account as follows:
#'\cr
#'\command{
#'\cr # data <- Start(source = list(
#'\cr #                 list(name = 'sourceA',
#'\cr #                      path = paste0('/sourceA/$variable$/',
#'\cr #                                    '$section$/$item$.data')),
#'\cr #                 list(name = 'sourceB',
#'\cr #                      path = paste0('/sourceB/$section$/',
#'\cr #                                    '$variable$/$item$.data'))
#'\cr #               ),
#'\cr #               variable = 'sales',
#'\cr #               section = 'first',
#'\cr #               item = indices(c(1, 3)),
#'\cr #               item_depends = 'section',
#'\cr #               store = 'Barcelona',
#'\cr #               store_var = 'store_location',
#'\cr #               month = 'all',
#'\cr #               return_vars = list(store_location = c('source',
#'\cr #                                                     'item')),
#'\cr #               synonims = list(
#'\cr #                 section = c('sec', 'section'),
#'\cr #                 store_location = c('store_loc',
#'\cr #                                    'store_location')
#'\cr #               ))
#'}
#'\cr
#'
#'@param file_opener A function that receives as a single parameter 
#'  'file_path' a character string with the path to a file to be opened, 
#'  and returns an object with an open connection to the file (optionally with 
#'  header information) on success, or returns NULL on failure.
#'\cr\cr
#'This parameter takes by default NcOpener() (an opener function for NetCDF
#'files).
#'\cr\cr
#'See NcOpener() for a template to build a file opener for your own file 
#'format.
#'
#'@param file_var_reader A function with the header \code{file_path = NULL}, 
#'  \code{file_object = NULL}, \code{file_selectors = NULL}, \code{var_name}, 
#'  \code{synonims} that returns an array with auxiliary data (i.e. data from a
#'  variable) inside a file. Start() will provide automatically either a 
#'  'file_path' or a 'file_object' to the 'file_var_reader'
#'  function (the function has to be ready to work whichever of these two is 
#'  provided). The parameter 'file_selectors' will also be provided 
#'  automatically to the variable reader, containing a named list where the 
#'  names are the names of the file dimensions of the queried data set (see 
#'  documentation on \code{\dots}) and the values are single character strings 
#'  with the components used to build the path to the file being read (the one 
#'  provided in 'file_path' or 'file_object'). The parameter 'var_name'
#'  will be filled in automatically by Start() also, with the name of one
#'  of the variales to be read. The parameter 'synonims' will be filled in 
#'  with exactly the same value as provided in the parameter 'synonims' in 
#'  the call to Start(), and has to be used in the code of the variable 
#'  reader to check for alternative variable names inside the target file. The 
#'  'file_var_reader' must return a (multi)dimensional array with named 
#'  dimensions, and optionally with the attribute 'variales' with other 
#'  additional metadata on the retrieved variable.
#'\cr\cr
#'Usually, the 'file_var_reader' should be a degenerate case of the 
#''file_data_reader' (see documentation on the corresponding parameter), 
#'so it is recommended to code the 'file_data_reder' in first place.
#'\cr\cr
#'This parameter takes by default NcVarReader() (a variable reader function
#'for NetCDF files).
#'\cr\cr
#'See NcVarReader() for a template to build a variale reader for your own 
#'file format.
#'
#'@param file_dim_reader A function with the header \code{file_path = NULL}, 
#'  \code{file_object = NULL}, \code{file_selectors = NULL}, \code{synonims} 
#'  that returns a named numeric vector where the names are the names of the 
#'  dimensions of the multidimensional data array in the file and the values are
#'  the sizes of such dimensions. Start() will provide automatically 
#'  either a 'file_path' or a 'file_object' to the 
#'  'file_dim_reader' function (the function has to be ready to work 
#'  whichever of these two is provided). The parameter 'file_selectors'
#'  will also be provided automatically to the dimension reader, containing a
#'  named list where the names are the names of the file dimensions of the 
#'  queried data set (see documentation on \code{\dots}) and the values are 
#'  single character strings with the components used to build the path to the 
#'  file being read (the one provided in 'file_path' or 'file_object'). 
#'  The parameter 'synonims' will be filled in with exactly the same value 
#'  as provided in the parameter 'synonims' in the call to Start(), 
#'  and can optionally be used in advanced configurations.
#'\cr\cr
#'This parameter takes by default NcDimReader() (a dimension reader 
#'function for NetCDF files).
#'\cr\cr
#'See NcDimReader() for (an advanced) template to build a dimension reader
#'for your own file format.
#'
#'@param file_data_reader A function with the header \code{file_path = NULL}, 
#'  \code{file_object = NULL}, \code{file_selectors = NULL}, 
#'  \code{inner_indices = NULL}, \code{synonims} that returns a subset of the 
#'  multidimensional data array inside a file (even if internally it is not an 
#'  array). Start() will provide automatically either a 'file_path'
#'  or a 'file_object' to the 'file_data_reader' function (the 
#'  function has to be ready to work whichever of these two is provided). The
#'  parameter 'file_selectors' will also be provided automatically to the
#'  data reader, containing a named list where the names are the names of the
#'  file dimensions of the queried data set (see documentation on \code{\dots})
#'  and the values are single character strings with the components used to 
#'  build the path to the file being read (the one provided in 'file_path' or 
#'  'file_object'). The parameter 'inner_indices' will be filled in 
#'  automatically by Start() also, with a named list of numeric vectors, 
#'  where the names are the names of all the expected inner dimensions in a file
#'  to be read, and the numeric vectors are the indices to be taken from the 
#'  corresponding dimension (the indices may not be consecutive nor in order).
#'  The parameter 'synonims' will be filled in with exactly the same value 
#'  as provided in the parameter 'synonims' in the call to Start(), 
#'  and has to be used in the code of the data reader to check for alternative 
#'  dimension names inside the target file. The 'file_data_reader' must 
#'  return a (multi)dimensional array with named dimensions, and optionally with
#'  the attribute 'variables' with other additional metadata on the retrieved 
#'  data.
#'\cr\cr
#'Usually, 'file_data_reader' should use 'file_dim_reader'
#'(see documentation on the corresponding parameter), so it is recommended to 
#'code 'file_dim_reder' in first place.
#'\cr\cr
#'This parameter takes by default NcDataReader() (a data reader function 
#'for NetCDF files).
#'\cr\cr
#'See NcDataReader() for a template to build a data reader for your own 
#'file format.
#'
#'@param file_closer A function that receives as a single parameter 
#'  'file_object' an open connection (as returned by 'file_opener') 
#'  to one of the files to be read, optionally with header information, and 
#'  closes the open connection. Always returns NULL.
#'\cr\cr
#'This parameter takes by default NcCloser() (a closer function for NetCDF 
#'files).
#'\cr\cr
#'See NcCloser() for a template to build a file closer for your own file 
#'format.
#'
#'@param transform A function with the header \code{dara_array}, 
#'  \code{variables}, \code{file_selectors = NULL}, \code{\dots}. It receives as
#'  input, through the parameter \code{data_array}, a subset of a 
#'  multidimensional array (as returned by 'file_data_reader'), applies a 
#'  transformation to it and returns it, preserving the amount of dimensions but
#'  potentially modifying their size. This transformation may require data from 
#'  other auxiliary variables, automatically provided to 'transform' 
#'  through the parameter 'variables', in the form of a named list where
#'  the names are the variable names and the values are (multi)dimensional
#'  arrays. Which variables need to be sent to 'transform' can be specified
#'  with the parameter 'transform_vars' in Start(). The parameter 
#'  'file_selectors' will also be provided automatically to 
#'  'transform', containing a named list where the names are the names of 
#'  the file dimensions of the queried data set (see documentation on 
#'  \code{\dots}) and the values are single character strings with the 
#'  components used to build the path to the file the subset being processed 
#'  belongs to. The parameter \code{\dots} will be filled in with other 
#'  additional parameters to adjust the transformation, exactly as provided in 
#'  the call to Start() via the parameter 'transform_params'.
#'@param transform_params A named list with additional parameters to be sent to 
#'  the 'transform' function (if specified). See documentation on parameter
#'  'transform' for details.
#'@param transform_vars A vector of character strings with the names of 
#'  auxiliary variables to be sent to the 'transform' function (if 
#'  specified). All the variables to be sent to 'transform' must also 
#'  have been requested as return variables in the parameter 'return_vars' 
#'  of Start().
#'@param transform_extra_cells An integer of extra indices to retrieve from the 
#'  data set, beyond the requested indices in \code{\dots}, in order for 
#'  'transform' to dispose of additional information to properly apply 
#'  whichever transformation (if needed). As many as 
#'  'transform_extra_cells' will be retrieved beyond each of the limits for
#'  each of those inner dimensions associated to a coordinate variable and sent 
#'  to 'transform' (i.e. present in 'transform_vars'). After 
#'  'transform' has finished, Start() will take again and return a 
#'  subset of the result, for the returned data to fall within the specified 
#'  bounds in \code{\dots}. The default value is 2.
#'@param apply_indices_after_transform A logical value indicating when a 
#'  'transform' is specified in Start() and numeric indices are 
#'  provided for any of the inner dimensions that depend on coordinate variables,
#'  these numeric indices can be made effective (retrieved) before applying the 
#'  transformation or after. The boolean flag allows to adjust this behaviour. 
#'  It takes FALSE by default (numeric indices are applied before sending
#'  data to 'transform').
#'@param pattern_dims A character string indicating the name of the dimension 
#'  with path pattern specifications (see \code{\dots} for details). If not  
#'  specified, Start() assumes the first provided dimension is the pattern 
#'  dimension, with a warning.
#'@param metadata_dims A vector of character strings with the names of the file 
#'  dimensions which to return metadata for. As noted in 'file_data_reader', 
#'  the data reader can optionally return auxiliary data via the attribute 
#'  'variables' of the returned array. Start() by default returns the 
#'  auxiliary data read for only the first file of each source (or data set) in 
#'  the pattern dimension (see \code{\dots} for info on what the pattern 
#'  dimension is). However it can be configured to return the metadata for all 
#'  the files along any set of file dimensions. The default value is NULL, and
#'  it will be assigned automatically as parameter 'pattern_dims'.
#'@param selector_checker A function used internaly by Start() to 
#'  translate a set of selectors (values for a dimension associated to a 
#'  coordinate variable) into a set of numeric indices. It takes by default 
#'  SelectorChecker() and, in principle, it should not be required to 
#'  change it for customized file formats. The option to replace it is left open
#'  for more versatility. See the code of SelectorChecker() for details on
#'  the inputs, functioning and outputs of a selector checker.
#'@param merge_across_dims A logical value indicating whether to merge 
#'  dimensions across which another dimension extends (according to the 
#'  '<dimname>_across' parameters). Takes the value FALSE by default. For 
#'  example, if the dimension 'time' extends across the dimension 'chunk' and 
#'  \code{merge_across_dims = TRUE}, the resulting data array will only contain
#'  only the dimension 'time' as long as all the chunks together.
#'@param merge_across_dims_narm A logical value indicating whether to remove
#'  the additional NAs from data when parameter 'merge_across_dims' is TRUE.
#'  It is helpful when the length of the to-be-merged dimension is different 
#'  across another dimension. For example, if the dimension 'time' extends 
#'  across dimension 'chunk', and the time length along the first chunk is 2 
#'  while along the second chunk is 10. Setting this parameter as TRUE can 
#'  remove the additional 8 NAs at position 3 to 10. The default value is TRUE,
#'  but will be automatically turned to FALSE if 'merge_across_dims = FALSE'.
#'@param split_multiselected_dims A logical value indicating whether to split a 
#'  dimension that has been selected with a multidimensional array of selectors
#'  into as many dimensions as present in the selector array. The default value
#'  is FALSE.
#'@param path_glob_permissive A logical value or an integer specifying how many
#'  folder levels in the path pattern, beginning from the end, the shell glob
#'  expressions must be preserved and worked out for each file. The default 
#'  value is FALSE, which is equivalent to 0. TRUE is equivalent to 1.\cr\cr
#'When specifying a path pattern for a dataset, it might contain shell glob 
#'experissions. For each dataset, the first file matching the path pattern is 
#'found, and the found file is used to work out fixed values for the glob 
#'expressions that will be used for all the files of the dataset. However, in 
#'some cases, the values of the shell glob expressions may not be constant for 
#'all files in a dataset, and they need to be worked out for each file 
#'involved.\cr\cr
#'For example, a path pattern could be as follows: \cr
#'\code{'/path/to/dataset/$var$_*/$date$_*_foo.nc'}. \cr Leaving 
#'\code{path_glob_permissive = FALSE} will trigger automatic seek of the 
#'  contents to replace the asterisks (e.g. the first asterisk matches with 
#'  \code{'bar'} and the second with \code{'baz'}. The found contents will be 
#'  used for all files in the dataset (in the example, the path pattern will be
#'  fixed to\cr \code{'/path/to/dataset/$var$_bar/$date$_baz_foo.nc'}. However, if
#'  any of the files in the dataset have other contents in the position of the
#'  asterisks, Start() will not find them (in the example, a file like \cr
#'  \code{'/path/to/dataset/precipitation_bar/19901101_bin_foo.nc'} would not be
#'  found). Setting \code{path_glob_permissive = 1} would preserve global
#'  expressions in the latest level (in the example, the fixed path pattern
#'  would be\cr \code{'/path/to/dataset/$var$_bar/$date$_*_foo.nc'}, and the
#'  problematic file mentioned before would be found), but of course this would
#'  slow down the Start() call if the dataset involves a large number of
#'  files. Setting \code{path_glob_permissive = 2} would leave the original path
#'  pattern with the original glob expressions in the 1st and 2nd levels (in the
#'  example, both asterisks would be preserved, thus would allow Start()
#'  to recognize files such as \cr
#'  \code{'/path/to/dataset/precipitation_zzz/19901101_yyy_foo.nc'}).\cr\cr
#'Note that each glob expression can only represent one possibility (Start() 
#'chooses the first). Because \code{*} is not the tag, which means it cannot
#'be a dimension of the output array. Therefore, only one possibility can be
#'adopted. For example, if \cr
#'\code{'/path/to/dataset/precipitation_*/19901101_*_foo.nc'}\cr
#'has two matches:\cr
#'\code{'/path/to/dataset/precipitation_xxx/19901101_yyy_foo.nc'} and\cr
#'\code{'/path/to/dataset/precipitation_zzz/19901101_yyy_foo.nc'},\cr
#'only the first found file will be used.
#'@param largest_dims_length A logical value or a named integer vector
#'  indicating if Start() should examine all the files to get the largest 
#'  length of the inner dimensions (TRUE) or use the first valid file of each 
#'  dataset as the returned dimension length (FALSE). Since examining all the 
#'  files could be time-consuming, a vector can be used to explicitly specify
#'  the expected length of the inner dimensions. For those inner dimensions not
#'  specified, the first valid file will be used. The default value is FALSE.\cr\cr
#'  This parameter is useful when the required files don't have consistent 
#'  inner dimension. For example, there are 10 required experimental data files
#'  of a series of start dates. The data only contain 25 members for the first
#'  2 years while 51 members for the later years. If \code{'largest_dims_length = FALSE'},
#'  the returned member dimension length will be 25 only. The 26th to 51st 
#'  members in the later 8 years will be discarded. If \code{'largest_dims_length = TRUE'},
#'  the returned member dimension length will be 51. To save the resource,
#' \code{'largest_dims_length = c(member = 51)'} can also be used.
#'@param retrieve A logical value indicating whether to retrieve the data
#'  defined in the Start() call or to explore only its dimension lengths 
#'  and names, and the values for the file and inner dimensions. The default
#'  value is FALSE.
#'@param num_procs An integer of number of processes to be created for the
#'  parallel execution of the retrieval/transformation/arrangement of the
#'  multiple involved files in a call to Start(). If set to NULL,
#'  takes the number of available cores (as detected by future::availableCores).
#'  The default value is 1 (no parallel execution).
#'@param ObjectBigmemory a character string to be included as part of the 
#'  bigmemory object name. This parameter is thought to be used internally by the
#'  chunking capabilities of startR.
#'@param silent A logical value of whether to display progress messages (FALSE)
#'   or not (TRUE). The default value is FALSE.
#'@param debug A logical value of whether to return detailed messages on the
#'  progress and operations in a Start() call (TRUE) or not (FALSE). The
#'  default value is FALSE.
#'
#'@return If \code{retrieve = TRUE} the involved data is loaded into RAM memory
#'  and an object of the class 'startR_cube' with the following components is
#'  returned:\cr
#'  \item{Data}{
#'  Multidimensional data array with named dimensions, with the data values
#'  requested via \code{\dots} and other parameters. This array can potentially 
#'  contain metadata in the attribute 'variables'.
#'  }
#'  \item{Variables}{
#'  Named list of 1 + N components, containing lists of retrieved variables (as
#'  requested in 'return_vars') common to all the data sources (in the 1st
#'  component, \code{$common}), and for each of the N dara sources (named after 
#'  the source name, as specified in \dots, or, if not specified, \code{$dat1},
#'  \code{$dat2}, ..., \code{$datN}). Each of the variables are contained in a
#'  multidimensional array with named dimensions, and potentially with the
#'  attribute 'variables' with additional auxiliary data.
#'  }
#'  \item{Files}{
#'  Multidimensonal character string array with named dimensions. Its dimensions
#'  are the file dimensions (as requested in \code{\dots}). Each cell in this
#'  array contains a path to a retrieved file, or NULL if the corresponding
#'  file was not found.
#'  }
#'  \item{NotFoundFiles}{
#'  Array with the same shape as \code{$Files} but with NULL in the
#'  positions for which the corresponding file was found, and a path to the
#'  expected file in the positions for which the corresponding file was not
#'  found.
#'  }
#'  \item{FileSelectors}{
#'  Multidimensional character string array with named dimensions, with the same
#'  shape as \code{$Files} and \code{$NotFoundFiles}, which contains the
#'  components used to build up the paths to each of the files in the data
#'  sources.
#'  }
#'  \item{PatternDim}{
#'  Character string containing the name of the file pattern dimension.
#'  }
#'If \code{retrieve = FALSE} the involved data is not loaded into RAM memory and
#'an object of the class 'startR_header' with the following components is
#' returned:\cr
#'  \item{Dimensions}{
#'  Named vector with the dimension lengths and names of the data involved in
#'  the Start() call.
#'  }
#'  \item{Variables}{
#'  Named list of 1 + N components, containing lists of retrieved variables (as
#'  requested in 'return_vars') common to all the data sources (in the 1st
#'  component, \code{$common}), and for each of the N dara sources (named after
#'  the source name, as specified in \dots, or, if not specified, \code{$dat1},
#'  \code{$dat2}, ..., \code{$datN}). Each of the variables are contained in a
#'  multidimensional array with named dimensions, and potentially with the
#'  attribute 'variables' with additional auxiliary data.
#'  }
#'  \item{ExpectedFiles}{
#'  Multidimensonal character string array with named dimensions. Its dimensions
#'  are the file dimensions (as requested in \dots). Each cell in this array
#'  contains a path to a file to be retrieved (which may exist or not).
#'  }
#'  \item{FileSelectors}{
#'  Multidimensional character string array with named dimensions, with the same
#'  shape as \code{$Files} and \code{$NotFoundFiles}, which contains the
#'  components used to build up the paths to each of the files in the data
#'  sources.
#'  }
#'  \item{PatternDim}{
#'  Character string containing the name of the file pattern dimension.
#'  }
#'  \item{StartRCall}{
#'  List of parameters sent to the Start() call, with the parameter
#'  'retrieve' set to TRUE. Intended for calling in order to
#'  retrieve the associated data a posteriori with a call to do.call().
#'  }
#'
#'@examples
#'  data_path <- system.file('extdata', package = 'startR')
#'  path_obs <- file.path(data_path, 'obs/monthly_mean/$var$/$var$_$sdate$.nc')
#'  sdates <- c('200011', '200012')
#'  data <- Start(dat = list(list(path = path_obs)),
#'                var = 'tos',
#'                sdate = sdates,
#'                time = 'all',
#'                latitude = 'all',
#'                longitude = 'all',
#'                return_vars = list(latitude = 'dat', 
#'                                   longitude = 'dat', 
#'                                   time = 'sdate'),
#'                retrieve = FALSE)
#'
#'@import bigmemory multiApply parallel abind future
#'@importFrom utils str
#'@importFrom stats na.omit setNames 
#'@importFrom ClimProjDiags Subset
#'@importFrom methods is
#'@export
Start <- function(..., # dim = indices/selectors, 
                  # dim_var = 'var', 
                  # dim_reorder = Sort/CircularSort, 
                  # dim_tolerance = number, 
                  # dim_depends = 'file_dim', 
                  # dim_across = 'file_dim', 
                  return_vars = NULL, 
                  synonims = NULL, 
                  file_opener = NcOpener, 
                  file_var_reader = NcVarReader, 
                  file_dim_reader = NcDimReader, 
                  file_data_reader = NcDataReader, 
                  file_closer = NcCloser, 
                  transform = NULL, 
                  transform_params = NULL, 
                  transform_vars = NULL, 
                  transform_extra_cells = 2, 
                  apply_indices_after_transform = FALSE, 
                  pattern_dims = NULL,
                  metadata_dims = NULL, 
                  selector_checker = SelectorChecker,
                  merge_across_dims = FALSE,
                  merge_across_dims_narm = TRUE,
                  split_multiselected_dims = FALSE,
                  path_glob_permissive = FALSE,
                  largest_dims_length = FALSE,
                  retrieve = FALSE, 
                  num_procs = 1, 
                  ObjectBigmemory = NULL,
                  silent = FALSE, debug = FALSE) {
  #, config_file = NULL
  #dictionary_dim_names = ,
  #dictionary_var_names =  

  # Specify Subset() is from ClimProjDiags
  Subset <- ClimProjDiags::Subset

  dim_params <- list(...)
  # Take *_var parameters apart
  var_params <- take_var_params(dim_params) 
  
  # Take *_reorder parameters apart
  dim_reorder_params <- take_var_reorder(dim_params)

  # Take *_tolerance parameters apart
  tolerance_params_ind <- grep('_tolerance$', names(dim_params))
  tolerance_params <- dim_params[tolerance_params_ind]
  
  # Take *_depends parameters apart
  depending_file_dims <- take_var_depends(dim_params)

  # Take *_across parameters apart
  inner_dims_across_files <- take_var_across(dim_params)
  
  # Check merge_across_dims
  if (!is.logical(merge_across_dims)) {
    stop("Parameter 'merge_across_dims' must be TRUE or FALSE.")
  }
  if (merge_across_dims & is.null(inner_dims_across_files)) {
    merge_across_dims <- FALSE
    .warning("Parameter 'merge_across_dims' is changed to FALSE because there is no *_across argument.")
  }
  
  # Check merge_across_dims_narm
  if (!is.logical(merge_across_dims_narm)) {
    stop("Parameter 'merge_across_dims_narm' must be TRUE or FALSE.")
  }
  if (!merge_across_dims & merge_across_dims_narm) {
    merge_across_dims_narm <- FALSE
  }
  # Leave alone the dimension parameters in the variable dim_params
  dim_params <- rebuild_dim_params(dim_params, merge_across_dims,
                                   inner_dims_across_files)
  dim_names <- names(dim_params)
  # Look for chunked dims
  chunks <- look_for_chunks(dim_params, dim_names) 

  # Check pattern_dims
  # Function found_pattern_dims may change pattern_dims in the .GlobalEnv
  found_pattern_dim <- found_pattern_dims(pattern_dims, dim_names, var_params,
                                          dim_params, dim_reorder_params)

  # Check if file pattern contains '$var$' substring
  if (any(!grepl("$var$", dim_params[[found_pattern_dim]], fixed = TRUE))) {
    .warning(paste("The special wildcard '$var$' is not present in the file",
                   "path. This might cause Start() to fail if it cannot parse",
                   "the inner dimensions in all the files."))
  }
  # Check all *_reorder are NULL or functions, and that they all have 
  # a matching dimension param.
  i <- 1
  for (dim_reorder_param in dim_reorder_params) {
    if (!is.function(dim_reorder_param)) {
      stop("All '*_reorder' parameters must be functions.")
    } else if (!any(grepl(paste0('^', strsplit(names(dim_reorder_params)[i], 
                                               '_reorder$')[[1]][1], '$'),
                          names(dim_params)))) {
      stop(paste0("All '*_reorder' parameters must be associated to a dimension parameter. Found parameter '", 
                  names(dim_reorder_params)[i], "' but no parameter '", 
                  strsplit(names(dim_reorder_params)[i], '_reorder$')[[1]][1], "'."))
      #} else if (!any(grepl(paste0('^', strsplit(names(dim_reorder_params)[i], 
      #                                           '_reorder$')[[1]][1], '$'),
      #                      names(var_params)))) {
      #  stop(paste0("All '*_reorder' parameters must be associated to a dimension parameter associated to a ",
      #              "variable. Found parameter '", names(dim_reorder_params)[i], "' and dimension parameter '", 
      #              strsplit(names(dim_reorder_params)[i], '_reorder$')[[1]][1], "' but did not find variable ",
      #              "parameter '", strsplit(names(dim_reorder_params)[i], '_reorder$')[[1]][1], "_var'."))
    }
    i <- i + 1
  }
  
  # Check all *_tolerance are NULL or vectors of character strings, and 
  # that they all have a matching dimension param.
  i <- 1
  for (tolerance_param in tolerance_params) {
    if (!any(grepl(paste0('^', strsplit(names(tolerance_params)[i], 
                                        '_tolerance$')[[1]][1], '$'),
                   names(dim_params)))) {
      stop(paste0("All '*_tolerance' parameters must be associated to a dimension parameter. Found parameter '", 
                  names(tolerance_params)[i], "' but no parameter '", 
                  strsplit(names(tolerance_params)[i], '_tolerance$')[[1]][1], "'."))
      #} else if (!any(grepl(paste0('^', strsplit(names(tolerance_params)[i], 
      #                                           '_tolerance$')[[1]][1], '$'),
      #                      names(var_params)))) {
      #  stop(paste0("All '*_tolerance' parameters must be associated to a dimension parameter associated to a ",
      #              "variable. Found parameter '", names(tolerance_params)[i], "' and dimension parameter '", 
      #              strsplit(names(tolerance_params)[i], '_tolerance$')[[1]][1], "' but did not find variable ",
      #              "parameter '", strsplit(names(tolerance_params)[i], '_tolerance$')[[1]][1], "_var'."))
    }
    i <- i + 1
  }
  # Make the keys of 'tolerance_params' to be the name of 
  # the corresponding dimension.
  if (length(tolerance_params) < 1) {
    tolerance_params <- NULL
  } else {
    names(tolerance_params) <- gsub('_tolerance$', '', names(tolerance_params))
  }
  
  # Check metadata_dims
  if (!is.null(metadata_dims)) {
    if (any(is.na(metadata_dims))) {
      metadata_dims <- NULL
    } else if (!is.character(metadata_dims) || (length(metadata_dims) < 1)) {
      stop("Parameter 'metadata' dims must be a vector of at least one character string.")
    }
  } else {
    metadata_dims <- pattern_dims
  }
  
  # Check if pattern_dims is the first item in metadata_dims
  if ((pattern_dims %in% metadata_dims) & metadata_dims[1] != pattern_dims) {
    metadata_dims <- c(pattern_dims, metadata_dims[-which(metadata_dims == pattern_dims)])
  }
  # Check if metadata_dims has more than 2 elements
  if ((metadata_dims[1] == pattern_dims & length(metadata_dims) > 2) ||
      (!(pattern_dims %in% metadata_dims) & length(metadata_dims) > 1)) {
    .warning(paste0("Parameter 'metadata_dims' contains some elements which",
                    "might serve a repetitive purpose: ",
                    metadata_dims[which(metadata_dims != pattern_dims)],
                    ". This could impact the performance of Start()."))
  }                 

  # Once the pattern dimension with dataset specifications is found,
  # the variable 'dat' is mounted with the information of each
  # dataset.
  # Take only the datasets for the requested chunk
  dats_to_take <- get_chunk_indices(length(dim_params[[found_pattern_dim]]), 
                                    chunks[[found_pattern_dim]]['chunk'], 
                                    chunks[[found_pattern_dim]]['n_chunks'],
                                    found_pattern_dim)
  dim_params[[found_pattern_dim]] <- dim_params[[found_pattern_dim]][dats_to_take]
  dat <- dim_params[[found_pattern_dim]]
  #NOTE: This function creates the object 'dat_names'
  dat_names <- c()
  dat <- mount_dat(dat, pattern_dims, found_pattern_dim, dat_names) 

  dim_params[[found_pattern_dim]] <- dat_names

  # Reorder inner_dims_across_files (to make the keys be the file dimensions,
  # and the values to be the inner dimensions that go across it).
  if (!is.null(inner_dims_across_files)) {
    # Reorder: example, convert list(ftime = 'chunk', ensemble = 'member', xx = 'chunk')
    #                        to list(chunk = c('ftime', 'xx'), member = 'ensemble')
    new_idaf <- list()
    for (i in names(inner_dims_across_files)) {
      if (!(inner_dims_across_files[[i]] %in% names(new_idaf))) {
        new_idaf[[inner_dims_across_files[[i]]]] <- i
      } else {
        new_idaf[[inner_dims_across_files[[i]]]] <- c(new_idaf[[inner_dims_across_files[[i]]]], i)
      }
    }
    inner_dims_across_files <- new_idaf
  }
  
  # Check return_vars
  if (is.null(return_vars)) {
    return_vars <- list()
    #    if (length(var_params) > 0) {
    #      return_vars <- as.list(paste0(names(var_params), '_var'))
    #    } else {
    #      return_vars <- list()
    #    }
  }
  if (!is.list(return_vars)) {
    stop("Parameter 'return_vars' must be a list or NULL.")
  }
  if (length(return_vars) > 0 && is.null(names(return_vars))) {
    #    names(return_vars) <- rep('', length(return_vars))
    stop("Parameter 'return_vars' must be a named list.")
  }
  i <- 1
  while (i <= length(return_vars)) {
    #    if (names(return_vars)[i] == '') {
    #      if (!(is.character(return_vars[[i]]) && (length(return_vars[[i]]) == 1))) {
    #        stop("The ", i, "th specification in 'return_vars' is malformed.")
    #      } 
    #      if (!grepl('_var$', return_vars[[i]])) {
    #        stop("The ", i, "th specification in 'return_vars' is malformed.")
    #      }
    #      dim_name <- strsplit(return_vars[[i]], '_var$')[[1]][1]
    #      if (!(dim_name %in% names(var_params))) {
    #        stop("'", dim_name, "_var' requested in 'return_vars' but ",
    #             "no '", dim_name, "_var' specified in the .Load call.")
    #      }
    #      names(return_vars)[i] <- var_params[[dim_name]]
    #      return_vars[[i]] <- found_pattern_dim
    #    } else
    if (length(return_vars[[i]]) > 0) { 
      if (!is.character(return_vars[[i]])) {
        stop("The ", i, "th specification in 'return_vars' is malformed. It ",
             "must be a vector of character strings of valid file dimension ",
             "names.")
      }
    }
    i <- i + 1
  }
  
  # Check synonims
  if (!is.null(synonims)) {
    error <- FALSE
    if (!is.list(synonims)) {
      error <- TRUE
    }
    for (synonim_entry in names(synonims)) {
      if (!(synonim_entry %in% names(dim_params)) &&
          !(synonim_entry %in% names(return_vars))) {
        error <- TRUE
      }
      if (!is.character(synonims[[synonim_entry]]) ||
          length(synonims[[synonim_entry]]) < 1) {
        error <- TRUE
      }
    }
    if (error) {
      stop("Parameter 'synonims' must be a named list, where the names are ",
           "a name of a requested dimension or variable and the values are ",
           "vectors of character strings with at least one alternative name ",
           " for each dimension or variable in 'synonims'.")
    }
  }
  if (length(unique(names(synonims))) < length(names(synonims))) {
    stop("There must not be repeated entries in 'synonims'.")
  }
  if (length(unique(unlist(synonims))) < length(unlist(synonims))) {
    stop("There must not be repeated values in 'synonims'.")
  }
  # Make that all dims and vars have an entry in synonims, even if only dim_name = dim_name
  dim_entries_to_add <- which(!(names(dim_params) %in% names(synonims)))
  if (length(dim_entries_to_add) > 0) {
    synonims[names(dim_params)[dim_entries_to_add]] <- as.list(names(dim_params)[dim_entries_to_add])
  }
  var_entries_to_add <- which(!(names(var_params) %in% names(synonims)))
  if (length(var_entries_to_add) > 0) {
    synonims[names(var_params)[var_entries_to_add]] <- as.list(names(var_params)[var_entries_to_add])
  }
 
  # Check if return_vars name is inner dim name. If it is synonim, change back to inner dim name
  # and return a warning.
  use_syn_names <- which(names(return_vars) %in% unlist(synonims) & 
                         !names(return_vars) %in% names(synonims))
  if (!identical(use_syn_names, integer(0))) {
    for (use_syn_name in use_syn_names) {
      wrong_name <- names(return_vars)[use_syn_name]
      names(return_vars)[use_syn_name] <- names(unlist(
                                            lapply(lapply(synonims, '%in%',
                                                   names(return_vars)[use_syn_name]),
                                            which)))
      .warning(paste0("The name '", wrong_name, "' in parameter 'return_vars' in synonim. ",
                      "Change it back to the inner dimension name, '",
                      names(return_vars)[use_syn_name], "'."))
    }
  }
 
  # Check selector_checker
  if (is.null(selector_checker) || !is.function(selector_checker)) {
    stop("Parameter 'selector_checker' must be a function.")
  }
  
  # Check file_opener
  if (is.null(file_opener) || !is.function(file_opener)) {
    stop("Parameter 'file_opener' must be a function.")
  }
  
  # Check file_var_reader
  if (!is.null(file_var_reader) && !is.function(file_var_reader)) {
    stop("Parameter 'file_var_reader' must be a function.")
  }
  
  # Check file_dim_reader
  if (!is.null(file_dim_reader) && !is.function(file_dim_reader)) {
    stop("Parameter 'file_dim_reader' must be a function.")
  }
  
  # Check file_data_reader
  if (is.null(file_data_reader) || !is.function(file_data_reader)) {
    stop("Parameter 'file_data_reader' must be a function.")
  }
  
  # Check file_closer
  if (is.null(file_closer) || !is.function(file_closer)) {
    stop("Parameter 'file_closer' must be a function.")
  }
  
  # Check transform
  if (!is.null(transform)) {
    if (!is.function(transform)) {
      stop("Parameter 'transform' must be a function.")
    }
  }
  
  # Check transform_params
  if (!is.null(transform_params)) {
    if (!is.list(transform_params)) {
      stop("Parameter 'transform_params' must be a list.")
    }
    if (is.null(names(transform_params))) {
      stop("Parameter 'transform_params' must be a named list.")
    }
  }
  
  # Check transform_vars
  if (!is.null(transform_vars)) {
    if (!is.character(transform_vars)) {
      stop("Parameter 'transform_vars' must be a vector of character strings.")
    }
  }
  if (any(!(transform_vars %in% names(return_vars)))) {
    stop("All the variables specified in 'transform_vars' must also be specified in 'return_vars'.")
  }
  
  # Check apply_indices_after_transform
  if (!is.logical(apply_indices_after_transform)) {
    stop("Parameter 'apply_indices_after_transform' must be either TRUE or FALSE.")
  }
  aiat <- apply_indices_after_transform
  
  # Check transform_extra_cells
  if (!is.numeric(transform_extra_cells)) {
    stop("Parameter 'transform_extra_cells' must be numeric.")
  }
  transform_extra_cells <- round(transform_extra_cells)
  
  # Check split_multiselected_dims
  if (!is.logical(split_multiselected_dims)) {
    stop("Parameter 'split_multiselected_dims' must be TRUE or FALSE.")
  }
  
  # Check path_glob_permissive
  if (!is.numeric(path_glob_permissive) && !is.logical(path_glob_permissive)) {
    stop("Parameter 'path_glob_permissive' must be TRUE, FALSE or an integer.")
  }
  if (length(path_glob_permissive) != 1) {
    stop("Parameter 'path_glob_permissive' must be of length 1.")
  }
  
  # Check largest_dims_length
  if (!is.numeric(largest_dims_length) && !is.logical(largest_dims_length)) {
    stop("Parameter 'largest_dims_length' must be TRUE, FALSE or a named integer vector.")
  }
  if (is.numeric(largest_dims_length)) {
    if (any(largest_dims_length %% 1 != 0) | any(largest_dims_length < 0) | is.null(names(largest_dims_length))) {
      stop("Parameter 'largest_dims_length' must be TRUE, FALSE or a named integer vector.")
    }
  }
  if (is.logical(largest_dims_length) && length(largest_dims_length) != 1) {
    stop("Parameter 'path_glob_permissive' must be TRUE, FALSE or a named integer vector.")
  }

  # Check retrieve
  if (!is.logical(retrieve)) {
    stop("Parameter 'retrieve' must be TRUE or FALSE.")
  }
  
  # Check num_procs
  if (!is.null(num_procs)) {
    if (!is.numeric(num_procs)) {
      stop("Parameter 'num_procs' must be numeric.")
    } else {
      num_procs <- round(num_procs)
    }
  }
  
  # Check silent
  if (!is.logical(silent)) {
    stop("Parameter 'silent' must be logical.")
  }
  
  if (!silent) {
    .message(paste0("Exploring files... This will take a variable amount ",
                    "of time depending on the issued request and the ",
                    "performance of the file server..."))
  }
  
  if (!is.character(debug)) {
    dims_to_check <- c('time')
  } else {
    dims_to_check <- debug
    debug <- TRUE
  }
 
  ############################## READING FILE DIMS ############################
  # Check that no unrecognized variables are present in the path patterns
  # and also that no file dimensions are requested to THREDDs catalogs.
  # And in the mean time, build all the work pieces and look for the 
  # first available file of each dataset.
  array_of_files_to_load <- NULL
  array_of_not_found_files <- NULL
  indices_of_first_files_with_data <- vector('list', length(dat))
  selectors_of_first_files_with_data <- vector('list', length(dat))
  dataset_has_files <- rep(FALSE, length(dat))
  found_file_dims <- vector('list', length(dat))
  expected_inner_dims <- vector('list', length(dat))
  
  #print("A")
  for (i in 1:length(dat)) {
    #print("B")
    dat_selectors <- dim_params
    dat_selectors[[found_pattern_dim]] <- dat_selectors[[found_pattern_dim]][i]
    dim_vars <- paste0('$', dim_names, '$')
    file_dims <- which(sapply(dim_vars, grepl, dat[[i]][['path']], fixed = TRUE))
    if (length(file_dims) > 0) {
      file_dims <- dim_names[file_dims]
    }
    file_dims <- unique(c(pattern_dims, file_dims))
    found_file_dims[[i]] <- file_dims
    expected_inner_dims[[i]] <- dim_names[which(!(dim_names %in% file_dims))]
    # (Check the depending_file_dims).
    if (any(c(names(depending_file_dims), unlist(depending_file_dims)) %in% 
            expected_inner_dims[[i]])) {
      stop(paste0("The dimension dependancies specified in ",
                  "'depending_file_dims' can only be between file ",
                  "dimensions, but some inner dimensions found in ",
                  "dependancies for '", dat[[i]][['name']], "', which ",
                  "has the following file dimensions: ", 
                  paste(paste0("'", file_dims, "'"), collapse = ', '), ".")) 
    } else {
      a <- names(depending_file_dims) %in% file_dims
      b <- unlist(depending_file_dims) %in% file_dims
      ab <- a & b
      if (any(!ab)) {
        .warning(paste0("Detected some dependancies in 'depending_file_dims' with ",
                        "non-existing dimension names. These will be disregarded."))
        depending_file_dims <- depending_file_dims[-which(!ab)]
      }
      if (any(names(depending_file_dims) == unlist(depending_file_dims))) {
        depending_file_dims <- depending_file_dims[-which(names(depending_file_dims) == unlist(depending_file_dims))]
      }
    }
    # (Check the inner_dims_across_files).
    if (any(!(names(inner_dims_across_files) %in% file_dims)) ||
        any(!(unlist(inner_dims_across_files) %in% expected_inner_dims[[i]]))) {
      stop(paste0("All relationships specified in ",
                  "'_across' parameters must be between a inner ",
                  "dimension and a file dimension. Found wrong ",
                  "specification for '", dat[[i]][['name']], "', which ",
                  "has the following file dimensions: ", 
                  paste(paste0("'", file_dims, "'"), collapse = ', '),
                  ", and the following inner dimensions: ", 
                  paste(paste0("'", expected_inner_dims[[i]], "'"), 
                        collapse = ', '), "."))
    }
    # (Check the return_vars).
    j <- 1
    while (j <= length(return_vars)) {
      if (any(!(return_vars[[j]] %in% file_dims))) {
        if (any(return_vars[[j]] %in% expected_inner_dims[[i]])) {
          stop("Found variables in 'return_vars' requested ",
               "for some inner dimensions (for dataset '",
               dat[[i]][['name']], "'), but variables can only be ",
               "requested for file dimensions.")
        } else {
          stop("Found variables in 'return_vars' requested ",
               "for non-existing dimensions.")
        }
      }
      j <- j + 1
    }
    # (Check the metadata_dims).
    if (!is.null(metadata_dims)) {
      if (any(!(metadata_dims %in% file_dims))) {
        stop("All dimensions in 'metadata_dims' must be file dimensions.")
      }
    }

    # Add attributes indicating whether this dimension selector is value or indice
    tmp <- lapply(dat_selectors[which(dim_names != pattern_dims)], add_value_indices_flag)
    dat_selectors <- c(dat_selectors[pattern_dims], tmp)

    ## Look for _var params that should be requested automatically.
    for (dim_name in dim_names[-which(dim_names == pattern_dims)]) {
      ## The following code 'rewrites' var_params for all datasets. If providing different
      ## path pattern repositories with different file/inner dimensions, var_params might
      ## have to be handled for each dataset separately.
      if ((attr(dat_selectors[[dim_name]], 'values') || (dim_name %in% c('var', 'variable'))) &&
          !(dim_name %in% names(var_params)) && !(dim_name %in% file_dims))  {
        if (dim_name %in% c('var', 'variable')) {
          var_params <- c(var_params, setNames(list('var_names'), dim_name))
          .warning(paste0("Found specified values for dimension '", dim_name, "' but no '", 
                          dim_name, "_var' provided. ", '"', dim_name, "_var = '", 
                          'var_names', "'", '"', " has been automatically added to ",
                          "the Start call."))
        } else {
          var_params <- c(var_params, setNames(list(dim_name), dim_name))
          .warning(paste0("Found specified values for dimension '", dim_name, "' but no '", 
                          dim_name, "_var' requested. ", '"', dim_name, "_var = '", 
                          dim_name, "'", '"', " has been automatically added to ",
                          "the Start call."))
        }
      }

      if (attr(dat_selectors[[dim_name]], 'indices') & !(dim_name %in% names(var_params))) {
        if (dim_name %in% transform_vars) {
          var_params <- c(var_params, setNames(list(dim_name), dim_name))
          .warning(paste0("Found dimension '", dim_name, "' is required to transform but no '",
                          dim_name, "_var' provided. ", '"', dim_name, "_var = '",
                          dim_name, "'", '"', " has been automatically added to ",
                          "the Start call."))
        } else if (dim_name %in% names(dim_reorder_params)) {
          var_params <- c(var_params, setNames(list(dim_name), dim_name))
          .warning(paste0("Found dimension '", dim_name, "' is required to reorder but no '",
                          dim_name, "_var' provided. ", '"', dim_name, "_var = '",
                          dim_name, "'", '"', " has been automatically added to ",
                          "the Start call."))
        }
      }
    }

    ## (Check the *_var parameters).
    if (any(!(unlist(var_params) %in% names(return_vars)))) {
      vars_to_add <- which(!(unlist(var_params) %in% names(return_vars)))
      new_return_vars <- vector('list', length(vars_to_add))
      names(new_return_vars) <- unlist(var_params)[vars_to_add]
      return_vars <- c(return_vars, new_return_vars)
      .warning(paste0("All '*_var' params must associate a dimension to one of the ",
                      "requested variables in 'return_vars'. The following variables",
                      " have been added to 'return_vars': ", 
                      paste(paste0("'", unlist(var_params), "'"), collapse = ', ')))
    }
    
    # Examine the selectors of file dim and create 'replace_values', which uses the first 
    # explicit selector (i.e., character) for all file dimensions.
    replace_values <- vector('list', length = length(file_dims))
    names(replace_values) <- file_dims
    for (file_dim in file_dims) {
      if (file_dim %in% names(var_params)) {
        .warning(paste0("The '", file_dim, "_var' param will be ignored since '", 
                        file_dim, "' is a file dimension (for the dataset with pattern ", 
                        dat[[i]][['path']], ")."))
      }
      # If the selector is a vector or a list of 2 without names (represent the value range)
      if (!is.list(dat_selectors[[file_dim]]) || 
          (is.list(dat_selectors[[file_dim]]) && 
           length(dat_selectors[[file_dim]]) == 2 &&
           is.null(names(dat_selectors[[file_dim]])))) {
        dat_selectors[[file_dim]] <- list(dat_selectors[[file_dim]])
      }
      first_class <- class(dat_selectors[[file_dim]][[1]])
      first_length <- length(dat_selectors[[file_dim]][[1]])

      # Length will be > 1 if it is list since beginning, e.g., depending dim is a list with 
      # names as depended dim.
      for (j in 1:length(dat_selectors[[file_dim]])) {
        sv <- selector_vector <- dat_selectors[[file_dim]][[j]]
        if (!inherits(sv, first_class) ||
            !identical(first_length, length(sv))) {
          stop("All provided selectors for depending dimensions must ",
               "be vectors of the same length and of the same class.")
        }
        if (is.character(sv) && !((length(sv) == 1) && (sv[1] %in% c('all', 'first', 'last')))) {
          #NOTE: ???? It doesn't make any changes.
          dat_selectors[[file_dim]][[j]] <- selector_checker(selectors = sv,
                                                             return_indices = FALSE)
          # Take chunk if needed (only defined dim; undefined dims will be chunked later in 
          # find_ufd_value().
          if (chunks[[file_dim]]['n_chunks'] > 1) {
            desired_chunk_indices <- get_chunk_indices(
                                       length(dat_selectors[[file_dim]][[j]]),
                                       chunks[[file_dim]]['chunk'],
                                       chunks[[file_dim]]['n_chunks'],
                                       file_dim)
            dat_selectors[[file_dim]][[j]] <- dat_selectors[[file_dim]][[j]][desired_chunk_indices]
            # chunk the depending dim as well
            if (file_dim %in% depending_file_dims) {
              depending_dim_name <- names(which(file_dim == depending_file_dims))
              # Chunk it only if it is defined dim (i.e., list of character with names of depended dim)
              if (!(length(dat_selectors[[depending_dim_name]]) == 1 &&
                    dat_selectors[[depending_dim_name]] %in% c('all', 'first', 'last'))) {
                if (any(sapply(dat_selectors[[depending_dim_name]], is.character))) {
                  dat_selectors[[depending_dim_name]] <-
                    dat_selectors[[depending_dim_name]][desired_chunk_indices]
                }
              }
            }
          }
        } else if (!(is.numeric(sv) || 
                     (is.character(sv) && (length(sv) == 1) && (sv %in% c('all', 'first', 'last'))) || 
                     (is.list(sv) && (length(sv) == 2) && (all(sapply(sv, is.character)) || 
                                                           all(sapply(sv, is.numeric)))))) {
          stop("All explicitly provided selectors for file dimensions must be character strings.")
        } 
      }
      sv <- dat_selectors[[file_dim]][[1]]
      # 'replace_values' has the first selector (if it's character) or NULL (if it's not explicitly 
      # defined) for each file dimension.
      if (is.character(sv) && !((length(sv) == 1) && (sv[1] %in% c('all', 'first', 'last')))) {
        replace_values[[file_dim]] <- sv[1]
      }
    }
    #print("C")
    # Now we know which dimensions whose selectors are provided non-explicitly.
    undefined_file_dims <- file_dims[which(sapply(replace_values, is.null))]
    defined_file_dims <- file_dims[which(!(file_dims %in% undefined_file_dims))]
    # Quickly check if the depending dimensions are provided properly. The check is only for 
    # if the depending and depended file dims are both explicited defined.
    for (file_dim in file_dims) {
      if (file_dim %in% names(depending_file_dims)) {

        # Return error if depended dim is a list of values while depending dim is not 
        # defined (i.e., indices or 'all')
        if (file_dim %in% defined_file_dims &
            !(depending_file_dims[[file_dim]] %in% defined_file_dims)) {
          stop(paste0("The depended dimension, ", file_dim, ", is explictly defined ",
                      "by a list of values, while the depending dimension, ",
                       depending_file_dims[[file_dim]], ", is not explictly defined. ",
                      "Specify ", depending_file_dims[[file_dim]], " by characters."))
        }

        ## TODO: Detect multi-dependancies and forbid.
        #NOTE: The if statement below is tricky. It tries to distinguish if the depending dim
        #      has the depended dim as the names of the list. However, if the depending dim
        #      doesn't have list names and its length is 2 (i.e., list( , )), Start() thinks
        #      it means the range, just like `lat = values(list(10, 20))`. And because of this,
        #      we won't enter the following if statement, and the error will occur later in
        #      SelectorChecker(). Need to find a way to distinguish if list( , ) means range or
        #      just the values.
        if (all(c(file_dim, depending_file_dims[[file_dim]]) %in% defined_file_dims)) {
          if (length(dat_selectors[[file_dim]]) != length(dat_selectors[[depending_file_dims[[file_dim]]]][[1]])) {
            stop(paste0("If providing selectors for the depending ",
                        "dimension '", file_dim, "', a ",
                        "vector of selectors must be provided for ",
                        "each selector of the dimension it depends on, '",
                        depending_file_dims[[file_dim]], "'."))
          } else if (!all(names(dat_selectors[[file_dim]]) == dat_selectors[[depending_file_dims[[file_dim]]]][[1]])) {
            stop(paste0("If providing selectors for the depending ",
                        "dimension '", file_dim, "', the name of the ",
                        "provided vectors of selectors must match ",
                        "exactly the selectors of the dimension it ",
                        "depends on, '", depending_file_dims[[file_dim]], "'."))
          } else if (is.null(names(dat_selectors[[file_dim]]))) {
            .warning(paste0("The selectors for the depending dimension '", file_dim, "' do not ",
                            "have list names. Assume that the order of the selectors matches the ",
                            "depended dimensions '", depending_file_dims[[file_dim]], "''s order."))
          }
        }
      }
    }

    # Find the possible values for the selectors that are provided as
    # indices. If the requested file is on server, impossible operation.
    if (length(grep("^http", dat[[i]][['path']])) > 0) {
      if (length(undefined_file_dims) > 0) {
        stop(paste0("All selectors for the file dimensions must be ",
                    "character strings if requesting data to a remote ",
                    "server. Found invalid selectors for the file dimensions ",
                    paste(paste0("'", undefined_file_dims, "'"), collapse = ', '), "."))
      }
      dataset_has_files[i] <- TRUE
    } else {
      dat[[i]][['path']] <- path.expand(dat[[i]][['path']])
      # Iterate over the known dimensions to find the first existing file.
      # The path to the first existing file will be used to find the 
      # values for the non explicitly defined selectors.
      first_file <- NULL
      first_file_selectors <- NULL
      if (length(undefined_file_dims) > 0) {
        replace_values[undefined_file_dims] <- '*'
      }
      ## TODO: What if length of defined_file_dims is 0? code might crash (in practice it worked for an example case)
      files_to_check <- sapply(dat_selectors[defined_file_dims], function(x) length(x[[1]]))
      sub_array_of_files_to_check <- array(1:prod(files_to_check), dim = files_to_check)
      j <- 1
      #print("D")
      while (j <= prod(files_to_check) && is.null(first_file)) {
        selector_indices <- which(sub_array_of_files_to_check == j, arr.ind = TRUE)[1, ]
        selectors <- sapply(1:length(defined_file_dims), 
                            function (x) {
                              vector_to_pick <- 1
                              if (defined_file_dims[x] %in% names(depending_file_dims)) {
                                vector_to_pick <- selector_indices[which(defined_file_dims == depending_file_dims[[defined_file_dims[x]]])]
                              }
                              dat_selectors[defined_file_dims][[x]][[vector_to_pick]][selector_indices[x]]
                            })
        replace_values[defined_file_dims] <- selectors
        file_path <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values)
        file_path <- Sys.glob(file_path)
        if (length(file_path) > 0) {
          first_file <- file_path[1]
          first_file_selectors <- selectors
        }
        j <- j + 1
      }
      #print("E")
      # Start looking for values for the non-explicitly defined selectors.
      if (is.null(first_file)) {
        .warning(paste0("No found files for the datset '", dat[[i]][['name']], 
                        "'. Provide existing selectors for the file dimensions ",
                        " or check and correct its path pattern: ", dat[[i]][['path']]))
      } else {
        dataset_has_files[i] <- TRUE
        ## TODO: Improve message here if no variable found:
        if (length(undefined_file_dims) > 0) {
          # Note: "dat[[i]][['path']]" is changed by the function below.
          dat_selectors <- find_ufd_value(undefined_file_dims, dat, i, replace_values,
                                          first_file, file_dims, path_glob_permissive,             
                                          depending_file_dims, dat_selectors, selector_checker,
                                          chunks)
          #print("I")
        } else {
          #NOTE: If there is no non-explicitly defined dim, use the first found file
          #      to modify. Problem: '*' doesn't catch all the possible file. Only use
          #      the first file.
          dat[[i]][['path']] <- .ReplaceGlobExpressions(dat[[i]][['path']], first_file, replace_values, 
                                                        defined_file_dims, dat[[i]][['name']], path_glob_permissive)
        }
      }
    }
    dat[[i]][['selectors']] <- dat_selectors

    # Now fetch for the first available file
    if (dataset_has_files[i]) {
      known_dims <- file_dims
    } else {
      known_dims <- defined_file_dims
    }
    replace_values <- vector('list', length = length(known_dims))
    names(replace_values) <- known_dims
    files_to_load <- sapply(dat_selectors[known_dims], function(x) length(x[[1]]))
    files_to_load[found_pattern_dim] <- 1
    sub_array_of_files_to_load <- array(1:prod(files_to_load), 
                                        dim = files_to_load)
    names(dim(sub_array_of_files_to_load)) <- known_dims
    sub_array_of_not_found_files <- array(!dataset_has_files[i], 
                                          dim = files_to_load)
    names(dim(sub_array_of_not_found_files)) <- known_dims
    
    if (largest_dims_length) {
      if (!exists('selector_indices_save')) {
        selector_indices_save <- vector('list', length = length(dat))
      }
      if (!exists('selectors_total_list')) {
        selectors_total_list <- vector('list', length = length(dat))
      }
      selector_indices_save[[i]] <- vector('list', length = prod(files_to_load))
      selectors_total_list[[i]] <- vector('list', length = prod(files_to_load))
    }

    j <- 1
    # NOTE: This while loop has these objects that are used afterward: 'sub_array_of_files_to_load',
    #       'sub_array_of_not_found_files', 'indices_of_first_files_with_data', 'selectors_of_first_files_with_data';
    #       'selector_indices_save' and 'selectors_total_list' are used if 'largest_dims_length = T'.
    while (j <= prod(files_to_load)) {
      selector_indices <- which(sub_array_of_files_to_load == j, arr.ind = TRUE)[1, ]
      names(selector_indices) <- known_dims

      if (largest_dims_length) {
        tmp <- selector_indices
        tmp[which(known_dims == found_pattern_dim)] <- i
        selector_indices_save[[i]][[j]] <- tmp
      }

      # This 'selectors' is only used in this while loop
      selectors <- sapply(1:length(known_dims), 
                          function (x) {
                            vector_to_pick <- 1
                            if (known_dims[x] %in% names(depending_file_dims)) {
                              vector_to_pick <- selector_indices[which(known_dims == depending_file_dims[[known_dims[x]]])]
                            }
                            dat_selectors[known_dims][[x]][[vector_to_pick]][selector_indices[x]]
                          })
      names(selectors) <- known_dims

      if (largest_dims_length) {
        selectors_total_list[[i]][[j]] <- selectors
        names(selectors_total_list[[i]][[j]]) <- known_dims
      }

      # 'replace_values' and 'file_path' are only used in this while loop
      replace_values[known_dims] <- selectors
      if (!dataset_has_files[i]) {
        if (any(is.na(selectors))) {
          replace_values <- replace_values[-which(names(replace_values) %in% names(selectors[which(is.na(selectors))]))]
        }
        file_path <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values, TRUE)
        sub_array_of_files_to_load[j] <- file_path
        #sub_array_of_not_found_files[j] <- TRUE???
      } else {
        if (any(is.na(selectors))) {
          replace_values <- replace_values[-which(names(replace_values) %in% names(selectors[which(is.na(selectors))]))]
          file_path <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values, TRUE)
          sub_array_of_files_to_load[j] <- file_path
          sub_array_of_not_found_files[j] <- TRUE
        } else {
          file_path <- .ReplaceVariablesInString(dat[[i]][['path']], replace_values)

          #NOTE: After replacing tags, there is still * if path_glob_permissive is not FALSE.
          #      Find the possible value to substitute *.
          if (grepl('\\*', file_path)) {
            found_files <- Sys.glob(file_path)
            file_path <- found_files[1]   # choose only the first file.
            #NOTE: Above line chooses only the first found file. Because * is not tags, which means
            #      it is not a dimension. So it cannot store more than one item. If use * to define
            #      the path, that * should only represent one possibility.
            if (length(found_files) > 1) {
              .warning("Using glob expression * to define the path, but more ",
                       "than one match is found. Choose the first match only.")
            }
          }

          if (!(length(grep("^http", file_path)) > 0)) {
            if (grepl(file_path, '*', fixed = TRUE)) {
              file_path_full <- Sys.glob(file_path)[1]
              if (nchar(file_path_full) > 0) {
                file_path <- file_path_full
              }
            }
          }
          sub_array_of_files_to_load[j] <- file_path
          if (is.null(indices_of_first_files_with_data[[i]])) {
            if (!(length(grep("^http", file_path)) > 0)) {
              if (!file.exists(file_path)) {
                file_path <- NULL
              }
            }
            if (!is.null(file_path)) {
              test_file <- NULL
              ## TODO: suppress error messages
              test_file <- file_opener(file_path)
              if (!is.null(test_file)) {
                selector_indices[which(known_dims == found_pattern_dim)] <- i
                indices_of_first_files_with_data[[i]] <- selector_indices
                selectors_of_first_files_with_data[[i]] <- selectors
                file_closer(test_file)
              }
            }
          }
        }
      }
      j <- j + 1
    }
    # Extend array as needed progressively
    if (is.null(array_of_files_to_load)) {
      array_of_files_to_load <- sub_array_of_files_to_load
      array_of_not_found_files <- sub_array_of_not_found_files
    } else {
      array_of_files_to_load <- .MergeArrays(array_of_files_to_load, sub_array_of_files_to_load,
                                             along = found_pattern_dim)
      ## TODO: file_dims, and variables like that.. are still ok now? I don't think so
      array_of_not_found_files <- .MergeArrays(array_of_not_found_files, sub_array_of_not_found_files,
                                               along = found_pattern_dim)
    }    
  }
  if (all(sapply(indices_of_first_files_with_data, is.null))) {
    stop("No data files found for any of the specified datasets.")
  }
  
  ########################### READING INNER DIMS. #############################
  #print("J")
  ## TODO: To be run in parallel (local multi-core)
  # Now time to work out the inner file dimensions.
  # First pick the requested variables.

#//// This part is moved below the new code////
# NOTE: 'dims_to_iterate' only consider common_return_vars. Move to below can save some work 
#       and get the revised common_return_vars if it is changed.
#  dims_to_iterate <- NULL
#  for (return_var in names(return_vars)) {
#    dims_to_iterate <- unique(c(dims_to_iterate, return_vars[[return_var]]))
#  }
#  if (found_pattern_dim %in% dims_to_iterate) {
#    dims_to_iterate <- dims_to_iterate[-which(dims_to_iterate == found_pattern_dim)]
#  }
#//////////////////////////////////////////////

  # Separate 'return_vars' into 'common_return_vars' and 'return_vars' (those = 'dat').
  common_return_vars <- NULL
  common_first_found_file <- NULL
  common_return_vars_pos <- NULL
  if (length(return_vars) > 0) {
    common_return_vars_pos <- which(sapply(return_vars, function(x) !(found_pattern_dim %in% x)))
  }
  if (length(common_return_vars_pos) > 0) {
    common_return_vars <- return_vars[common_return_vars_pos]
    return_vars <- return_vars[-common_return_vars_pos]
    common_first_found_file <- rep(FALSE, length(which(sapply(common_return_vars, length) == 0)))
    names(common_first_found_file) <- names(common_return_vars[which(sapply(common_return_vars, length) == 0)])
  }

#!!!!!!!Check here. return_vars has removed the common ones, and here remove 'dat' value????
#It seems like it does some benefits to later parts
  return_vars <- lapply(return_vars, 
                        function(x) {
                          if (found_pattern_dim %in% x) {
                            x[-which(x == found_pattern_dim)]
                          } else {
                            x
                          }
                        })
#////////////////////////////////////////////
  # Force return_vars = (time = NULL) to (time = 'sdate') if (1) selector = [sdate = 2, time = 4] or
  # (2) time_across = 'sdate'.
  # NOTE: Not sure if the loop over dat is needed here. In theory, all the dat
  #       should have the same dimensions (?) so expected_inner_dims and
  #       found_file_dims are the same. The selector_array may possible be
  #       different, but then the attribute will be correct? If it's different,
  #       it should depend on 'dat' (but here we only consider common_return_vars)
  for (i in 1:length(dat)) {
    for (inner_dim in expected_inner_dims[[i]]) {
      # The selectors for the inner dimension are taken.
      selector_array <- dat[[i]][['selectors']][[inner_dim]]
      file_dim_as_selector_array_dim <- 1

      if (any(found_file_dims[[i]] %in% names(dim(selector_array)))) {
        file_dim_as_selector_array_dim <- 
          found_file_dims[[i]][which(found_file_dims[[i]] %in% names(dim(selector_array)))]
      }
      if (inner_dim %in% inner_dims_across_files | 
          is.character(file_dim_as_selector_array_dim)) {  #(2) or (1)
        # inner_dim is not in return_vars or is NULL
        need_correct <- FALSE
        if (((!inner_dim %in% names(common_return_vars)) &
             (!inner_dim %in% names(return_vars))) |
            (inner_dim %in% names(common_return_vars) & 
             is.null(common_return_vars[[inner_dim]]))) {
          need_correct <- TRUE
        } else if (inner_dim %in% names(common_return_vars) &
                   (inner_dim %in% inner_dims_across_files) &
                   !is.null(names(inner_dims_across_files))) { #(2)
          if (!names(inner_dims_across_files) %in% common_return_vars[[inner_dim]]) need_correct <- TRUE
          
        } else if (inner_dim %in% names(common_return_vars) &
                   is.character(file_dim_as_selector_array_dim)) { #(1)
          if (!all(file_dim_as_selector_array_dim %in% common_return_vars[[inner_dim]])) {
            need_correct <- TRUE
            file_dim_as_selector_array_dim <- file_dim_as_selector_array_dim[which(!file_dim_as_selector_array_dim %in% common_return_vars[[inner_dim]])]
          }
        }
        if (need_correct) {
          common_return_vars[[inner_dim]] <-
            c(common_return_vars[[inner_dim]],
              correct_return_vars(inner_dim, inner_dims_across_files,
                                  found_pattern_dim, file_dim_as_selector_array_dim))
        }
      }
    }
  }

  # Return info about return_vars when dat > 1
  if (length(dat) > 1 & length(common_return_vars) > 0) {
    .message("\n", "[ATTENTION]", 
             paste0("According to parameter 'return_vars', the inner dimensions: ",
                    paste(names(common_return_vars), collapse = ', '),
                    ", are common among all the datasets. Please be sure that ",
                    "this is expected to avoid potential wrong results, and ",
                    "verify the outputs carefully."),
             "\n", indent = 1)
   }

#////////////////////////////////////////////

# This part was above where return_vars is seperated into return_vars and common_return_vars
# NOTE: 'dims_to_iterate' only consider common_return_vars. Move to here can save some work 
#       and get the revised common_return_vars if it is changed in the part right above.
  dims_to_iterate <- NULL
  for (common_return_var in names(common_return_vars)) {
    dims_to_iterate <- unique(c(dims_to_iterate, common_return_vars[[common_return_var]]))
  }
#////////////////////////////////////////////

  # Change the structure of 'dat'. If the selector is not list or it is list of 2 that represents
  # range, make it as list. The dimensions that go across files will later be extended to have
  # lists of lists/vectors of selectors.
  for (i in 1:length(dat)) {
    if (dataset_has_files[i]) {
      for (inner_dim in expected_inner_dims[[i]]) {
        if (!is.list(dat[[i]][['selectors']][[inner_dim]]) || # not list, or
            (is.list(dat[[i]][['selectors']][[inner_dim]]) &&  # list of 2 that represents range
             length(dat[[i]][['selectors']][[inner_dim]]) == 2 &&
             is.null(names(dat[[i]][['selectors']][[inner_dim]])))) {
          dat[[i]][['selectors']][[inner_dim]] <- list(dat[[i]][['selectors']][[inner_dim]])
        }
      }
    }
  }


  # Use 'common_return_vars' and 'return_vars' to generate the initial picked(_common)_vars,
  # picked(_common)_vars_ordered, and picked(_common)_vars_unorder_indices.
  ## Create 'picked_common_vars'
  if (length(common_return_vars) > 0) {
    picked_common_vars <- vector('list', length = length(common_return_vars))
    names(picked_common_vars) <- names(common_return_vars)
  } else {
    picked_common_vars <- NULL
  }
  picked_common_vars_ordered <- picked_common_vars
  picked_common_vars_unorder_indices <- picked_common_vars

  ## Create 'picked_vars'
  picked_vars <- vector('list', length = length(dat))
  names(picked_vars) <- dat_names
  if (length(return_vars) > 0) {
    picked_vars <- lapply(picked_vars, function(x) {
                                         x <- vector('list', length = length(return_vars))} )
    picked_vars <- lapply(picked_vars, setNames, names(return_vars))
  }
  picked_vars_ordered <- picked_vars

  picked_vars_unorder_indices <- picked_vars

  for (i in 1:length(dat)) {
    if (dataset_has_files[i]) {
      indices_of_first_file <- as.list(indices_of_first_files_with_data[[i]])
      array_file_dims <- sapply(dat[[i]][['selectors']][found_file_dims[[i]]], function(x) length(x[[1]]))
      names(array_file_dims) <- found_file_dims[[i]]
      if (length(dims_to_iterate) > 0) {
        indices_of_first_file[dims_to_iterate] <- lapply(array_file_dims[dims_to_iterate], function(x) 1:x)
      }
      array_of_var_files <- do.call('[', c(list(x = array_of_files_to_load), indices_of_first_file, list(drop = FALSE)))
      array_of_var_indices <- array(1:length(array_of_var_files), dim = dim(array_of_var_files))
      array_of_not_found_var_files <- do.call('[', c(list(x = array_of_not_found_files), indices_of_first_file, list(drop = FALSE)))
      # Create previous_indices. The initial value is -1 because there is no 'previous' before the
      # 1st current_indices.
      previous_indices <- rep(-1, length(indices_of_first_file))
      names(previous_indices) <- names(indices_of_first_file)
      # Create first_found_file for vars_to_read defining. It is for the dim value in return_vars
      # that is NULL or character(0). Because these dims only need to be read once, so 
      # first_found_file indicates if these dims have been read or not. 
      # If read, it turns to TRUE and won't be included in vars_to_read again in the next 
      # 'for j loop'. 
      first_found_file <- NULL
      if (length(return_vars) > 0) {
        first_found_file <- rep(FALSE, length(which(sapply(return_vars, length) == 0)))
        names(first_found_file) <- names(return_vars[which(sapply(return_vars, length) == 0)])
      }

      for (j in 1:length(array_of_var_files)) {
        current_indices <- which(array_of_var_indices == j, arr.ind = TRUE)[1, ]
        names(current_indices) <- names(indices_of_first_file)
        if (!is.na(array_of_var_files[j]) && !array_of_not_found_var_files[j]) {
          changed_dims <- which(current_indices != previous_indices)
          # Prepare vars_to_read for this dataset (i loop) and this file (j loop)
          vars_to_read <- generate_vars_to_read(return_vars, changed_dims, first_found_file, 
                                                common_return_vars, common_first_found_file, i)

          file_object <- file_opener(array_of_var_files[j])
          if (!is.null(file_object)) {
            for (var_to_read in vars_to_read) {
              if (var_to_read %in% unlist(var_params)) {
                associated_dim_name <- names(var_params)[which(unlist(var_params) == var_to_read)]
              }
              var_name_to_reader <- var_to_read
              names(var_name_to_reader) <- 'var'
              var_dims <- file_dim_reader(NULL, file_object, var_name_to_reader, NULL,
                                          synonims)
              # file_dim_reader returns dimension names as found in the file.
              # Need to translate accoridng to synonims:
              names(var_dims) <- replace_with_synonmins(var_dims, synonims)
              if (!is.null(var_dims)) {

                ## (1) common_return_vars
                if (var_to_read %in% names(common_return_vars)) {
                  var_to_check <- common_return_vars[[var_to_read]]
                  list_picked_var_of_read <- generate_picked_var_of_read(
                    var_to_read, var_to_check, array_of_files_to_load, var_dims,
                    array_of_var_files = array_of_var_files[j], file_var_reader,
                    file_object, synonims, associated_dim_name, dim_reorder_params,
                    aiat, current_indices, var_params,
                    either_picked_vars = picked_common_vars[[var_to_read]],
                    either_picked_vars_ordered = picked_common_vars_ordered[[var_to_read]],
                    either_picked_vars_unorder_indices = picked_common_vars_unorder_indices[[var_to_read]]
                  )
                  picked_common_vars[[var_to_read]] <- list_picked_var_of_read$either_picked_vars
                  picked_common_vars_ordered[[var_to_read]] <-
                    list_picked_var_of_read$either_picked_vars_ordered
                  picked_common_vars_unorder_indices[[var_to_read]] <-
                    list_picked_var_of_read$either_picked_vars_unorder_indices

                ## (2) return_vars
                } else {
                  var_to_check <- return_vars[[var_to_read]]
                  list_picked_var_of_read <- generate_picked_var_of_read(
                    var_to_read, var_to_check, array_of_files_to_load, var_dims,
                    array_of_var_files = array_of_var_files[j], file_var_reader,
                    file_object, synonims, associated_dim_name, dim_reorder_params,
                    aiat, current_indices, var_params,
                    either_picked_vars = picked_vars[[i]][[var_to_read]],
                    either_picked_vars_ordered = picked_vars_ordered[[i]][[var_to_read]],
                    either_picked_vars_unorder_indices = picked_vars_unorder_indices[[i]][[var_to_read]]
                  )
                  picked_vars[[i]][[var_to_read]] <- list_picked_var_of_read$either_picked_vars
                  picked_vars_ordered[[i]][[var_to_read]] <-
                    list_picked_var_of_read$either_picked_vars_ordered
                  picked_vars_unorder_indices[[i]][[var_to_read]] <-
                    list_picked_var_of_read$either_picked_vars_unorder_indices
                }
                if (var_to_read %in% names(first_found_file)) {
                  first_found_file[var_to_read] <- TRUE
                }
                if (var_to_read %in% names(common_first_found_file)) {
                  common_first_found_file[var_to_read] <- TRUE
                }
              } else {
                stop("Could not find variable '", var_to_read,
                     "' in the file ", array_of_var_files[j])
              }
            }
            file_closer(file_object)
          }
        }
        previous_indices <- current_indices
      }
    }
  }
  # Once we have the variable values, we can work out the indices
  # for the implicitly defined selectors.

  beta <- transform_extra_cells
  dims_to_crop <- vector('list')
  transformed_vars <- vector('list', length = length(dat))
  names(transformed_vars) <- dat_names
  transformed_vars_ordered <- transformed_vars
  transformed_vars_unorder_indices <- transformed_vars
  transformed_common_vars <- NULL
  transformed_common_vars_ordered <- NULL
  transformed_common_vars_unorder_indices <- NULL
  transform_crop_domain <- NULL

  # store warning messages from transform 
  warnings1 <- NULL
  warnings2 <- NULL

  for (i in 1:length(dat)) {
    if (dataset_has_files[i]) {
      indices <- indices_of_first_files_with_data[[i]]
      if (!is.null(indices)) {
        #//////////////////////////////////////////////////
        # Find data_dims
        ## If largest_dims_length is a number & !merge_across_dims,
        ## directly assign this dim as the number;
        ## If largest_dims_length is a number & this dim is across files, find the dim length of each file
        find_largest_dims_length_by_files <- FALSE
        if (is.numeric(largest_dims_length)) {
          if (names(largest_dims_length) %in% inner_dims_across_files) {
            find_largest_dims_length_by_files <- TRUE
          }
        } else if (largest_dims_length) {
          find_largest_dims_length_by_files <- TRUE
        } 

        if (!find_largest_dims_length_by_files) { # old code
          file_path <- do.call("[", c(list(array_of_files_to_load), as.list(indices_of_first_files_with_data[[i]])))
          # The following 5 lines should go several lines below, but were moved
          # here for better performance.
          # If any of the dimensions comes without defining variable, then we read
          # the data dimensions.
          data_dims <- NULL
#          if (length(unlist(var_params[expected_inner_dims[[i]]])) < length(expected_inner_dims[[i]])) {
            file_to_open <- file_path
            data_dims <- file_dim_reader(file_to_open, NULL, selectors_of_first_files_with_data[[i]],
                                         lapply(dat[[i]][['selectors']][expected_inner_dims[[i]]], '[[', 1),
                                         synonims)
            # file_dim_reader returns dimension names as found in the file.
            # Need to translate accoridng to synonims:
            names(data_dims) <- replace_with_synonmins(data_dims, synonims)
#          }

          if (is.numeric(largest_dims_length)) { # largest_dims_length is a named vector
            # Check if the names fit the inner dimension names
            if (!all(names(largest_dims_length) %in% names(data_dims))) {
              #NOTE: stop or warning?
              stop("Parameter 'largest_dims_length' has inconsistent names with inner dimensions.")
            } else {
              match_ind <- match(names(largest_dims_length), names(data_dims))
              data_dims[match_ind] <- largest_dims_length
            }
          }

        } else {
        ## largest_dims_length = TRUE, or is a number & merge_across_dims is across this dim
          tmp <- find_largest_dims_length(
                   selectors_total_list[[i]], array_of_files_to_load,
                   selector_indices_save[[i]], dat[[i]], expected_inner_dims[[i]],
                   synonims, file_dim_reader)
          data_dims <- tmp$largest_data_dims
          # 'data_dims_each_file' is used when merge_across_dims = TRUE &
          # the files have different length of inner dim.
          data_dims_each_file <- tmp$data_dims_all_files

          # file_dim_reader returns dimension names as found in the file.
          # Need to translate accoridng to synonims:
          names(data_dims) <- replace_with_synonmins(data_dims, synonims)

        } # end if (largest_dims_length == TRUE)
        #//////////////////////////////////////////////////

        # Some dimension is defined in Start() call but doesn't exist in data
        if (!all(expected_inner_dims[[i]] %in% names(data_dims))) {
          tmp <- expected_inner_dims[[i]][which(!expected_inner_dims[[i]] %in% names(data_dims))]
          stop("Could not find the dimension '", tmp, "' in the file. Either ",
               "change the dimension name in your request, adjust the ",
               "parameter 'dim_names_in_files' or fix the dimension name in ",
               "the file.\n", file_path)
        }
        # Not all the inner dims are defined in Start() call
        if (!all(names(data_dims) %in% expected_inner_dims[[i]])) {
          tmp <- names(data_dims)[which(!names(data_dims) %in% expected_inner_dims[[i]])]
          if (data_dims[tmp] != 1) {
            stop("The dimension '", tmp, "' is found in the file ", file_path,
                 " but not defined in the Start call.")
          }
        }


        #///////////////////////////////////////////////////////////////////
        # Transform the variables if needed and keep them apart.
        if (!is.null(transform) && (length(transform_vars) > 0)) {
          if (!all(transform_vars %in% c(names(picked_vars[[i]]), names(picked_common_vars)))) {
            stop("Could not find all the required variables in 'transform_vars' ",
                 "for the dataset '", dat[[i]][['name']], "'.")
          }

          vars_to_transform <- NULL
          # picked_vars[[i]]
          vars_to_transform <- generate_vars_to_transform(vars_to_transform, picked_vars[[i]], transform_vars, picked_vars_ordered[[i]])
          # picked_common_vars
          vars_to_transform <- generate_vars_to_transform(vars_to_transform, picked_common_vars, transform_vars, picked_common_vars_ordered)

          # Save the crop domain from selectors of transformed vars
          # PROB: It doesn't consider aiat. If aiat, the indices are for 
          #       after transformed data; we don't know the corresponding 
          #       values yet.
          transform_crop_domain <- vector('list')
          for (transform_var in transform_vars) {
            transform_crop_domain[[transform_var]] <- dat[[i]][['selectors']][[transform_var]][[1]]
            # Turn indices into values
            if (attr(transform_crop_domain[[transform_var]], 'indices')) {
              if (transform_var %in% names(common_return_vars)) {
                if (transform_var %in% names(dim_reorder_params)) {
                  transform_crop_domain[[transform_var]] <-
                    generate_transform_crop_domain_values(
                      transform_crop_domain[[transform_var]], 
                      picked_vars = picked_common_vars_ordered[[transform_var]], 
                      transform_var)
                } else {
                  transform_crop_domain[[transform_var]] <-
                    generate_transform_crop_domain_values(
                      transform_crop_domain[[transform_var]],
                      picked_vars = picked_common_vars[[transform_var]], 
                      transform_var)
                }
              } else {  # return_vars
                if (transform_var %in% names(dim_reorder_params)) {
                  transform_crop_domain[[transform_var]] <-
                    generate_transform_crop_domain_values(
                      transform_crop_domain[[transform_var]],
                      picked_vars = picked_vars_ordered[[i]][[transform_var]], 
                      transform_var)
                } else {  
                  transform_crop_domain[[transform_var]] <-
                    generate_transform_crop_domain_values(
                      transform_crop_domain[[transform_var]],
                      picked_vars = picked_vars[[i]][[transform_var]], 
                      transform_var)
                }
              }
            } else if (is.atomic(transform_crop_domain[[transform_var]])) {
              # if it is values but vector
              transform_crop_domain[[transform_var]] <-
              c(transform_crop_domain[[transform_var]][1],
                tail(transform_crop_domain[[transform_var]], 1))
            }

            # For CDORemapper (not sure if it's also suitable for other transform functions): 
            # If lon_reorder is not used + lon selector is from big to small, 
            # lonmax and lonmin need to be exchanged. The ideal way is to 
            # exchange in CDORemapper(), but lon_reorder is used or not is not
            # known by CDORemapper().
            # NOTE: lat's order doesn't matter, big to small and small to big 
            #       both work. Since we shouldn't assume transform_var in Start(),
            #       e.g., transform_var can be anything transformable in the assigned transform function,
            #       we exchange whichever parameter here anyway.
            if (!transform_var %in% names(dim_reorder_params) & 
                diff(unlist(transform_crop_domain[[transform_var]])) < 0) {
              transform_crop_domain[[transform_var]] <- rev(transform_crop_domain[[transform_var]]) 
            }

          }

          # Transform the variables
          tmp <- .withWarnings(
            do.call(transform, c(list(data_array = NULL,
                                      variables = vars_to_transform,
                                      file_selectors = selectors_of_first_files_with_data[[i]],
                                      crop_domain = transform_crop_domain),
                                      transform_params))
          )
          transformed_data <- tmp$value
          warnings1 <- c(warnings1, tmp$warnings)

          # Discard the common transformed variables if already transformed before
          if (!is.null(transformed_common_vars)) {
            common_ones <- which(names(picked_common_vars) %in% names(transformed_data$variables))
            if (length(common_ones) > 0) {
              transformed_data$variables <- transformed_data$variables[-common_ones]
            }
          }
          transformed_vars[[i]] <- list()
          transformed_vars_ordered[[i]] <- list()
          transformed_vars_unorder_indices[[i]] <- list()
          # Order the transformed variables if needed
          # 'var_to_read' should be 'transformed_var', but is kept to reuse the same code as above.
          for (var_to_read in names(transformed_data$variables)) {
            if (var_to_read %in% unlist(var_params)) {
              associated_dim_name <- names(var_params)[which(unlist(var_params) == var_to_read)]
              if ((associated_dim_name %in% names(dim_reorder_params))) {
                ## Is this check really needed?
                if (length(dim(transformed_data$variables[[associated_dim_name]])) > 1) {
                  stop("Requested a '", associated_dim_name, "_reorder' for a dimension ",
                       "whose coordinate variable that has more than 1 dimension (after ",
                       "transform). This is not supported.")
                }
                ordered_var_values <- dim_reorder_params[[associated_dim_name]](transformed_data$variables[[associated_dim_name]])
                attr(ordered_var_values, 'variables') <- attr(transformed_data$variables[[associated_dim_name]], 'variables')
                if (!all(c('x', 'ix') %in% names(ordered_var_values))) {
                  stop("All the dimension reorder functions must return a list with the components 'x' and 'ix'.")
                }
                # Save the indices to reorder back the ordered variable values.
                # This will be used to define the first round indices (if aiat) or second round
                # indices (if !aiat).
                unorder <- sort(ordered_var_values$ix, index.return = TRUE)$ix
                if (var_to_read %in% names(picked_common_vars)) {
                  transformed_common_vars_ordered[[var_to_read]] <- ordered_var_values$x
                  transformed_common_vars_unorder_indices[[var_to_read]] <- unorder
                } else {
                  transformed_vars_ordered[[i]][[var_to_read]] <- ordered_var_values$x
                  transformed_vars_unorder_indices[[i]][[var_to_read]] <- unorder
                }
              }
            }
          }
          transformed_picked_vars_names <- which(names(picked_vars[[i]]) %in% names(transformed_data$variables))
          if (length(transformed_picked_vars_names) > 0) {
            transformed_picked_vars_names <- names(picked_vars[[i]])[transformed_picked_vars_names]
            transformed_vars[[i]][transformed_picked_vars_names] <- transformed_data$variables[transformed_picked_vars_names]
          }
          if (is.null(transformed_common_vars)) {
            transformed_picked_common_vars_names <- which(names(picked_common_vars) %in% names(transformed_data$variables))
            if (length(transformed_picked_common_vars_names) > 0) {
              transformed_picked_common_vars_names <- names(picked_common_vars)[transformed_picked_common_vars_names]
              transformed_common_vars <- transformed_data$variables[transformed_picked_common_vars_names]
            }
          }
        }
        # Once the variables are transformed, we compute the indices to be 
        # taken for each inner dimension.
        # In all cases, indices will have to be computed to know which data
        # values to take from the original data for each dimension (if a 
        # variable is specified for that dimension, it will be used to 
        # convert the provided selectors into indices). These indices are
        # referred to as 'first round of indices'.
        # The taken data will then be transformed if needed, together with
        # the dimension variable if specified, and, in that case, indices 
        # will have to be computed again to know which values to take from the 
        # transformed data. These are the 'second round of indices'. In the 
        # case there is no transformation, the second round of indices will
        # be all the available indices, i.e. from 1 to the number of taken
        # values with the first round of indices.
        for (inner_dim in expected_inner_dims[[i]]) {
          if (debug) {
            print("-> DEFINING INDICES FOR INNER DIMENSION:")
            print(inner_dim)
          }
          crossed_file_dim <- NULL
          if (inner_dim %in% unlist(inner_dims_across_files)) {
            crossed_file_dim <- names(inner_dims_across_files)[which(sapply(inner_dims_across_files, function(x) inner_dim %in% x))[1]]
            chunk_amount <- length(dat[[i]][['selectors']][[crossed_file_dim]][[1]])
            names(chunk_amount) <- crossed_file_dim
          } else if (!is.null(names(dim(dat[[i]][['selectors']][[inner_dim]][[1]]))) &
                     inner_dim %in% names(dim(dat[[i]][['selectors']][[inner_dim]][[1]])) &
                     any(found_file_dims[[i]] %in% names(dim(dat[[i]][['selectors']][[inner_dim]][[1]])))) {
            # inner dim is dependent on file dim in the form of selector array (e.g., time = [sdate = 2, time = 4])
            crossed_file_dim <- found_file_dims[[i]][which(found_file_dims[[i]] %in% 
                                                           names(dim(dat[[i]][['selectors']][[inner_dim]][[1]])))]
            if (length(crossed_file_dim) == 1) {
              chunk_amount <- length(dat[[i]][['selectors']][[crossed_file_dim]][[1]])
              names(chunk_amount) <- crossed_file_dim
            } else {
            # e.g., region = [memb = 2, sdate = 3, region = 1]
              chunk_amount <- prod(
                                sapply(lapply(
                                  dat[[i]][['selectors']][crossed_file_dim], "[[", 1), length))
              names(chunk_amount) <- paste(crossed_file_dim, collapse = '.')
            }
          } else {
            chunk_amount <- 1
          }
          # In the special case that the selectors for a dimension are 'all', 'first', ...
          # and chunking (dividing in more than 1 chunk) is requested, the selectors are
          # replaced for equivalent indices.
          if ((any(dat[[i]][['selectors']][[inner_dim]][[1]] %in% c('all', 'first', 'last'))) && 
              (chunks[[inner_dim]]['n_chunks'] != 1)) {
            dat[[i]][['selectors']][[inner_dim]][[1]] <- 
              replace_character_with_indices(selectors = dat[[i]][['selectors']][[inner_dim]][[1]], data_dims = data_dims[[inner_dim]], chunk_amount)
          }

          # The selectors for the inner dimension are taken.
          selector_array <- dat[[i]][['selectors']][[inner_dim]][[1]]
          if (debug) {
            if (inner_dim %in% dims_to_check) {
              print(paste0("-> DEBUG MESSAGES FOR THE DATASET", i, " AND INNER DIMENSION '", inner_dim, "':"))
              print("-> STRUCTURE OF SELECTOR ARRAY:")
              print(str(selector_array))
              print("-> PICKED VARS:")
              print(picked_vars)
              print("-> TRANSFORMED VARS:")
              print(transformed_vars)
            }
          }
          if (is.null(dim(selector_array))) {
            dim(selector_array) <- length(selector_array)
          }
          if (is.null(names(dim(selector_array)))) {
            if (length(dim(selector_array)) == 1) {
              names(dim(selector_array)) <- inner_dim
            } else {
              stop("Provided selector arrays must be provided with dimension ",
                   "names. Found an array of selectors without dimension names ",
                   "for the dimension '", inner_dim, "'.")
            } 
          }
          selectors_are_indices <- FALSE
          if (!is.null(attr(selector_array, 'indices'))) {
            if (!is.logical(attr(selector_array, 'indices'))) {
              stop("The atribute 'indices' for the selectors for the dimension '", 
                   inner_dim, "' must be TRUE or FALSE.")
            }
            selectors_are_indices <- attr(selector_array, 'indices')
          }
          taken_chunks <- rep(FALSE, chunk_amount)
          selector_file_dims <- 1

          #NOTE: Change 'selector_file_dims' (from 1) if selector is an array with a file_dim dimname.
          #      I.e., If time = [sdate = 2, time = 4], selector_file_dims <- c(sdate = 2)
          if (any(found_file_dims[[i]] %in% names(dim(selector_array)))) {
            selector_file_dims <- dim(selector_array)[which(names(dim(selector_array)) %in% found_file_dims[[i]])]
          }

          selector_inner_dims <- dim(selector_array)[which(!(names(dim(selector_array)) %in% found_file_dims[[i]]))]
          var_with_selectors <- NULL
          var_with_selectors_name <- var_params[[inner_dim]]
          var_ordered <- NULL
          var_unorder_indices <- NULL
          with_transform <- FALSE
          #////////////////////////////////////////////////////////////////////
          # If the selectors come with an associated variable
          if (!is.null(var_with_selectors_name)) {
            if ((var_with_selectors_name %in% transform_vars) && (!is.null(transform))) {
              with_transform <- TRUE
              if (!is.null(crossed_file_dim)) {
                stop("Requested a transformation over the dimension '", 
                     inner_dim, "', wich goes across files. This feature ", 
                     "is not supported. Either do the request without the ",
                     "transformation or request it over dimensions that do ",
                     "not go across files.")
              }
            }
            if (debug) {
              if (inner_dim %in% dims_to_check) {
                print("-> NAME OF THE VARIABLE WITH THE SELECTOR VALUES FOR THE CURRENT INNER DIMENSION:")
                print(var_with_selectors_name)
                print("-> NAMES OF THE VARIABLES TO BE TRANSFORMED:")
                print(transform_vars)
                print("-> STRUCTURE OF THE TRANSFORMATION FUNCTION:")
                print(str(transform))
              }
            }
            # For fri
            if (var_with_selectors_name %in% names(picked_vars[[i]])) {
              var_with_selectors <- picked_vars[[i]][[var_with_selectors_name]]
              var_ordered <- picked_vars_ordered[[i]][[var_with_selectors_name]]
              var_unorder_indices <- picked_vars_unorder_indices[[i]][[var_with_selectors_name]]
            } else if (var_with_selectors_name %in% names(picked_common_vars)) {
              var_with_selectors <- picked_common_vars[[var_with_selectors_name]]
              var_ordered <- picked_common_vars_ordered[[var_with_selectors_name]]
              var_unorder_indices <- picked_common_vars_unorder_indices[[var_with_selectors_name]]
            }
            n <- prod(dim(var_with_selectors))
            # if no _reorder, var_unorder_indices is NULL
            if (is.null(var_unorder_indices)) {
              var_unorder_indices <- 1:n
            }
            # For sri
            if (with_transform) {
              ## var in 'dat'
              if (var_with_selectors_name %in% names(transformed_vars[[i]])) {
                m <- prod(dim(transformed_vars[[i]][[var_with_selectors_name]]))
                if (aiat) {
                  var_with_selectors <- transformed_vars[[i]][[var_with_selectors_name]]
                  var_ordered <- transformed_vars_ordered[[i]][[var_with_selectors_name]]
                  var_unorder_indices <- transformed_vars_unorder_indices[[i]][[var_with_selectors_name]]
                }
                # For making sri ordered later
                transformed_var_unordered_indices <- transformed_vars_unorder_indices[[i]][[var_with_selectors_name]]
                if (is.null(transformed_var_unordered_indices)) {
                  transformed_var_unordered_indices <- 1:m
                }
                transformed_var_with_selectors <- transformed_vars[[i]][transformed_picked_vars_names][[var_with_selectors_name]][transformed_var_unordered_indices]
                # Sorting the transformed variable and working out the indices again after transform.
                if (!is.null(dim_reorder_params[[var_with_selectors_name]])) {
                  transformed_var_with_selectors_reorder <- dim_reorder_params[[var_with_selectors_name]](transformed_var_with_selectors)
                  transformed_var_with_selectors <- transformed_var_with_selectors_reorder$x
                  transformed_var_with_selectors_unorder <- transformed_var_with_selectors_reorder$ix
                } else {
                  transformed_var_with_selectors_unorder <- 1:length(transformed_var_with_selectors)
                }

              ## var in common
              } else if (var_with_selectors_name %in% names(transformed_common_vars)) {
                m <- prod(dim(transformed_common_vars[[var_with_selectors_name]]))
                if (aiat) {
                  var_with_selectors <- transformed_common_vars[[var_with_selectors_name]]
                  var_ordered <- transformed_common_vars_ordered[[var_with_selectors_name]]
                  var_unorder_indices <- transformed_common_vars_unorder_indices[[var_with_selectors_name]]
                }
                # For making sri ordered later
                transformed_var_unordered_indices <- transformed_common_vars_unorder_indices[[var_with_selectors_name]]
                if (is.null(transformed_var_unordered_indices)) {
                  transformed_var_unordered_indices <- 1:m
                }
                transformed_var_with_selectors <- transformed_common_vars[[var_with_selectors_name]][transformed_var_unordered_indices]
                # Sorting the transformed variable and working out the indices again after transform.
                if (!is.null(dim_reorder_params[[var_with_selectors_name]])) {
                  transformed_var_with_selectors_reorder <- dim_reorder_params[[var_with_selectors_name]](transformed_var_with_selectors)
                  transformed_var_with_selectors <- transformed_var_with_selectors_reorder$x
                  transformed_var_with_selectors_unorder <- transformed_var_with_selectors_reorder$ix
                } else {
                  transformed_var_with_selectors_unorder <- 1:length(transformed_var_with_selectors)
                }
              }
              if (is.null(var_unorder_indices)) {
                var_unorder_indices <- 1:m
              }
            }

            if (debug) {
              if (inner_dim %in% dims_to_check) {
                print("-> SIZE OF ORIGINAL VARIABLE:")
                print(n)
                print("-> SIZE OF TRANSFORMED VARIABLE:")
                if (with_transform) print(m)
                print("-> STRUCTURE OF ORDERED VAR:")
                print(str(var_ordered))
                print("-> UNORDER INDICES:")
                print(var_unorder_indices)
              }
            }
            var_dims <- var_full_dims <- dim(var_with_selectors)
            var_file_dims <- 1

            # If this inner dim's selector (var_with_selectors) is an array
            # that has file dim as dimension (e.g., across or depend relationship)
            if (any(names(var_dims) %in% found_file_dims[[i]])) {
              if (with_transform) {
                stop("Requested transformation for inner dimension '", 
                     inner_dim, "' but provided selectors for such dimension ",
                     "over one or more file dimensions. This is not ",
                     "supported. Either request no transformation for the ",
                     "dimension '", inner_dim, "' or specify the ",
                     "selectors for this dimension without the file dimensions.")
              }
              var_file_dims <- var_dims[which(names(var_dims) %in% found_file_dims[[i]])]
              var_dims <- var_dims[-which(names(var_dims) %in% found_file_dims[[i]])]
            }
            ##            # Keep the selectors if they correspond to a variable that will be transformed.
            ##            if (with_transform) {
            ##              if (var_with_selectors_name %in% names(picked_vars[[i]])) {
            ##                transformed_var_with_selectors <- transformed_vars[[i]][[var_with_selectors_name]]
            ##              } else if (var_with_selectors_name %in% names(picked_common_vars)) {
            ##                transformed_var_with_selectors <- transformed_common_vars[[var_with_selectors_name]]
            ##              }
            ##              transformed_var_dims <- dim(transformed_var_with_selectors)
            ##              transformed_var_file_dims <- 1
            ##              if (any(names(transformed_var_dims) %in% found_file_dims[[i]])) {
            ##                transformed_var_file_dims <- transformed_var_dims[which(names(transformed_var_dims) %in% found_file_dims[[i]])]
            ##                transformed_var_dims <- tranasformed_var_dims[-which(names(transformed_var_dims) %in% found_file_dims[[i]])]
            ##              }
            ##if (inner_dim %in% dims_to_check) {
            ##print("111m")
            ##print(str(transformed_var_dims))
            ##}
            ##
            ##              m <- prod(transformed_var_dims)
            ##            }
            # Work out var file dims and inner dims.
            if (inner_dim %in% unlist(inner_dims_across_files)) {
              #TODO: if (chunk_amount != number of chunks in selector_file_dims), crash
              if (length(var_dims) > 1) {
                stop("Specified a '", inner_dim, "_var' for the dimension '", 
                     inner_dim, "', which goes across files (across '", crossed_file_dim, 
                     "'). The specified variable, '", var_with_selectors_name, "', has more ",
                     "than one dimension and can not be used as selector variable. ",
                     "Select another variable or fix it in the files.")
              }
            }
            ## TODO HERE::
            #- indices_of_first_files_with_data may change, because array is now extended
            var_full_dims <- dim(var_with_selectors)
          } else if (((is.numeric(selector_array) || is.list(selector_array)) && selectors_are_indices) ||
                     (is.character(selector_array) && (length(selector_array) == 1) &&
                      (selector_array %in% c('all', 'first', 'last')) &&
                      !is.null(file_dim_reader))) {
            #### TODO HERE::
            ###- indices_of_first_files_with_data may change, because array is now extended
            # Lines moved above for better performance.
            ##data_dims <- file_dim_reader(file_path, NULL, selectors_of_first_files_with_data[[i]],
            ##                             lapply(dat[[i]][['selectors']][expected_inner_dims[[i]]], '[[', 1))
          } else {
            stop(paste0("Can not translate the provided selectors for '", inner_dim, 
                        "' to numeric indices. Provide numeric indices and a ",
                        "'file_dim_reader' function, or a '", inner_dim, 
                        "_var' in order to calculate the indices."))
          }
          # At this point, if no selector variable was provided, the variable 
          # data_dims has been populated. If a selector variable was provided,
          # the variables var_dims, var_file_dims and var_full_dims have been 
          # populated instead.
          #////////////////////////////////////////////////////////////////////

          # If the inner dim lengths differ among files,
          # need to know each length to create the indices for each file later.
          # Record 'inner_dim_lengths' here for later usage.
          inner_dim_lengths <- NULL
          if (largest_dims_length & !is.null(crossed_file_dim)) {
            # inner_dim_lengths here includes all the files, but we only want
            # the files of fyear for certain "sdate". We will categorize it later.
            inner_dim_lengths <- tryCatch({
              sapply(data_dims_each_file, '[[', inner_dim)
            }, error = function(x) {
              sapply(data_dims_each_file, '[[', 
                     synonims[[inner_dim]][which(synonims[[inner_dim]] != inner_dim)])
            })

            # Use other file dims as the factors to categorize.
            other_file_dims <- dim(array_of_files_to_load)[which(!found_file_dims[[i]] %in% crossed_file_dim)]
            other_file_dims <- lapply(lapply(other_file_dims, seq, 1), rev)
            other_file_dims_factor <- expand.grid(other_file_dims)
            selector_indices_save_subset <-
              lapply(selector_indices_save[[i]], '[', which(!found_file_dims[[i]] %in% crossed_file_dim))

            # Put the fyear with the same other file dims (sdate, etc.) together, and find the largest length (in theory all of them should be the same)
            inner_dim_lengths_cat <- vector('list', dim(other_file_dims_factor)[1])
            for (i_factor in 1:length(inner_dim_lengths_cat)) {

              inner_dim_lengths_cat[[i_factor]] <- 
                inner_dim_lengths[which(sapply(lapply(
                  selector_indices_save_subset, '==', 
                  other_file_dims_factor[i_factor, ]), all))]
            }
            # Find the largest length of each time step
            inner_dim_lengths <- do.call(pmax, inner_dim_lengths_cat)
            ## NOTE: NA values can be present if the size of a depending 
            ## dimension varies along its depended dim. Removing them allows
            ## retrieval of the common indices. Could cause other issues?
            inner_dim_lengths <- inner_dim_lengths[which(!is.na(inner_dim_lengths))]
          }

          fri <- first_round_indices <- NULL
          sri <- second_round_indices <- NULL
          # This variable will keep the indices needed to crop the transformed 
          # variable (the one that has been transformed without being subset 
          # with the first round indices).
          tvi <- tranaformed_variable_indices <- NULL
          ordered_fri <- NULL
          ordered_sri <- NULL
          if ((length(selector_array) == 1) && is.character(selector_array) &&
              (selector_array %in% c('all', 'first', 'last')) &&
              (chunks[[inner_dim]]['n_chunks'] == 1)) {
            if (is.null(var_with_selectors_name)) {
              fri <- vector('list', length = chunk_amount)
              dim(fri) <- c(chunk_amount)
              sri <- vector('list', length = chunk_amount)
              dim(sri) <- c(chunk_amount)
              if (selector_array == 'all') {
                if (is.null(inner_dim_lengths) | length(unique(inner_dim_lengths)) <= 1) { #old code
                  fri[] <- replicate(chunk_amount, list(1:(data_dims[inner_dim])))
                } else {  # files have different inner dim length
                  for (i_chunk in 1:length(fri)) {
                    fri[[i_chunk]] <- 1:inner_dim_lengths[i_chunk]
                  }
                }
                taken_chunks <- rep(TRUE, chunk_amount)
                #sri <- NULL
              } else if (selector_array == 'first') {
                fri[[1]] <- 1
                taken_chunks[1] <- TRUE
                #sri <- NULL
              } else if (selector_array == 'last') {
                fri[[chunk_amount]] <- data_dims[inner_dim]
                taken_chunks[length(taken_chunks)] <- TRUE
                #sri <- NULL
              }
            } else {
              if (!is.null(crossed_file_dim) & any(!(crossed_file_dim %in% names(var_file_dims)))) {
                stop("The variable '", var_with_selectors_name, "' must also be ",
                     "requested for the file dimension '", crossed_file_dim, "' in ",
                     "this configuration.")
              }
              fri <- vector('list', length = prod(var_file_dims))
              dim(fri) <- var_file_dims
              ordered_fri <- fri
              sri <- vector('list', length = prod(var_file_dims))
              dim(sri) <- var_file_dims
              ordered_sri <- sri
              if (selector_array == 'all') {
                # TODO: Populate ordered_fri
                ordered_fri[] <- replicate(prod(var_file_dims), list(1:n))
                fri[] <- replicate(prod(var_file_dims), list(var_unorder_indices[1:n]))
                taken_chunks <- rep(TRUE, chunk_amount)
                if (!with_transform) {
                  #fri[] <- replicate(prod(var_file_dims), list(1:n))
                  #taken_chunks <- rep(TRUE, chunk_amount)
                  #sri <- NULL
                } else {
                  ordered_sri[] <- replicate(prod(var_file_dims), list(1:m))
                  if (inner_dim %in% names(dim_reorder_params)) {
                    sri[] <- replicate(prod(var_file_dims), list(transformed_var_unordered_indices[1:m]))
                  } else {
                    sri[] <- replicate(prod(var_file_dims), list(1:m))
                  }
                  ## var_file_dims instead??
                  #if (!aiat) {
                  #fri[] <- replicate(prod(var_file_dims), list(1:n))
                  #taken_chunks <- rep(TRUE, chunk_amount)
                  #sri[] <- replicate(prod(transformed_var_file_dims), list(1:m))
                  #} else {
                  #fri[] <- replicate(prod(var_file_dims), list(1:n))
                  #taken_chunks <- rep(TRUE, chunk_amount)
                  #sri[] <- replicate(prod(transformed_var_file_dims), list(1:m))
                  #}
                  tvi <- 1:m
                }
              } else if (selector_array == 'first') {
                taken_chunks[1] <- TRUE
                if (!with_transform) {
                  ordered_fri[[1]] <- 1
                  fri[[1]] <- var_unorder_indices[1]
                  #taken_chunks[1] <- TRUE
                  #sri <- NULL
                } else {
                  if (!aiat) {
                    ordered_fri[[1]] <- 1
                    fri[[1]] <- var_unorder_indices[1]
                    # TODO: TO BE IMPROVED
                    #taken_chunks[1] <- TRUE
                    ordered_sri[[1]] <- 1:ceiling(m / n)
                    sri[[1]] <- 1:ceiling(m / n)
                    tvi <- 1:ceiling(m / n)
                  } else {
                    ordered_fri[[1]] <- 1:ceiling(m / n)
                    fri[[1]] <- var_unorder_indices[1:ceiling(m / n)]
                    #taken_chunks[1] <- TRUE
                    ordered_sri[[1]] <- 1
                    sri[[1]] <- 1
                    tvi <- 1
                  }
                }
              } else if (selector_array == 'last') {
                taken_chunks[length(taken_chunks)] <- TRUE
                if (!with_transform) {
                  ordered_fri[[prod(var_file_dims)]] <- n
                  fri[[prod(var_file_dims)]] <- var_unorder_indices[n]
                  #taken_chunks[length(taken_chunks)] <- TRUE
                  #sri <- NULL
                } else {
                  if (!aiat) {
                    ordered_fri[[prod(var_file_dims)]] <- prod(var_dims)
                    fri[[prod(var_file_dims)]] <- var_unorder_indices[prod(var_dims)]
                    #taken_chunks[length(taken_chunks)] <- TRUE
                    ordered_sri[[prod(var_file_dims)]] <- 1:ceiling(m / n)
                    sri[[prod(var_file_dims)]] <- 1:ceiling(m / n)
                    # TODO: TO BE IMPROVED. THE TVI MAY BE WRONG IF THERE'S BEEN A REORDERING.
                    tvi <- 1:ceiling(m / n)
                  } else {
                    ordered_fri[[prod(var_file_dims)]] <- (n - ceiling(m / n) + 1):n
                    fri[[prod(var_file_dims)]] <- var_unorder_indices[(n - ceiling(m / n) + 1):n]
                    #taken_chunks[length(taken_chunks)] <- TRUE
                    ordered_sri[[prod(var_file_dims)]] <- 1
                    sri[[prod(var_file_dims)]] <- 1
                    tvi <- 1
                  }
                }
              }
            }
            # If the selectors are not 'all', 'first', 'last', ...
          } else {
            if (!is.null(var_with_selectors_name)) {
              unmatching_file_dims <- which(!(names(var_file_dims) %in% names(selector_file_dims)))
              if ((length(unmatching_file_dims) > 0)) {
                raise_error <- FALSE
                if (is.null(crossed_file_dim)) {
                  raise_error <- TRUE
                } else {
                  if (!(length(unmatching_file_dims) == 1 & 
                        names(var_file_dims)[unmatching_file_dims] %in% crossed_file_dim &
                        inner_dim %in% names(selector_inner_dims))) {
                    raise_error <- TRUE
                  }
                }
                if (raise_error) {
                  stop("Provided selectors for the dimension '", inner_dim, "' must have as many ",
                       "file dimensions as the variable the dimension is defined along, '", 
                       var_with_selectors_name, "', with the exceptions of the file pattern dimension ('",
                       found_pattern_dim, "') and any depended file dimension (if specified as ",
                       "depended dimension in parameter 'inner_dims_across_files' and the ",
                       "depending file dimension is present in the provided selector array).")
                }
              }
              if (any(names(selector_file_dims) %in% names(dim(var_with_selectors)))) {
                if (any(dim(var_with_selectors)[names(selector_file_dims)] != selector_file_dims)) {
                  stop("Size of selector file dimensions must match size of the corresponding ",
                       "variable dimensions.")
                }
              }
            }
            ## TODO: If var dimensions are not in the same order as selector dimensions, reorder
            if (is.null(names(selector_file_dims))) {
              if (is.null(crossed_file_dim)) {
                fri_dims <- 1
              } else {
                fri_dims <- chunk_amount
                names(fri_dims) <- crossed_file_dim
              }
            } else {
              fri_dim_names <- names(selector_file_dims)
              if (!is.null(crossed_file_dim)) {
                fri_dim_names <- c(fri_dim_names, crossed_file_dim)
              }
              fri_dim_names <- found_file_dims[[i]][which(found_file_dims[[i]] %in% fri_dim_names)]
              fri_dims <- rep(NA, length(fri_dim_names))
              names(fri_dims) <- fri_dim_names
              fri_dims[names(selector_file_dims)] <- selector_file_dims
              #NOTE: Not sure how it works here, but "chunk_amount" is the same as
              #      "selector_file_dims" above in the cases we've seen so far, 
              #      and it causes problem when crossed_file_dim is more than one.
#              if (!is.null(crossed_file_dim)) {
#                fri_dims[crossed_file_dim] <- chunk_amount
#              }
            }
            fri <- vector('list', length = prod(fri_dims))
            dim(fri) <- fri_dims
            sri <- vector('list', length = prod(fri_dims))
            dim(sri) <- fri_dims
            selector_file_dim_array <- array(1:prod(selector_file_dims), dim = selector_file_dims)
            selector_store_position <- fri_dims
            for (j in 1:prod(dim(selector_file_dim_array))) {
              selector_indices_to_take <- which(selector_file_dim_array == j, arr.ind = TRUE)[1, ]
              names(selector_indices_to_take) <- names(selector_file_dims)
              selector_store_position[names(selector_indices_to_take)] <- selector_indices_to_take
              # "selector_indices_to_take" is an array if "selector_file_dims" is not 1 (if 
              # selector is an array with a file_dim dimname, i.e., time = [sdate = 2, time = 4].
              if (!is.null(names(selector_indices_to_take))) {
                sub_array_of_selectors <- Subset(selector_array, names(selector_indices_to_take),
                                                 as.list(selector_indices_to_take), drop = 'selected')
              } else {
                sub_array_of_selectors <- selector_array
              }

              if (debug) {
                if (inner_dim %in% dims_to_check) {
                  print("-> ITERATING OVER FILE DIMENSIONS OF THE SELECTORS.")
                  print("-> STRUCTURE OF A SUB ARRAY:")
                  print(str(sub_array_of_selectors))
                  print("-> STRUCTURE OF THE VARIABLE WITH SELECTORS:")
                  print(str(var_with_selectors))
                  print(dim(var_with_selectors))
                }
              }
              if (selectors_are_indices) {
                sub_array_of_values <- NULL
                #} else if (!is.null(var_ordered)) {
                #  sub_array_of_values <- var_ordered
              } else {
                if (length(names(var_file_dims)) > 0) {
                  var_indices_to_take <- selector_indices_to_take[which(names(selector_indices_to_take) %in% names(var_file_dims))]
                  if (!is.null(names(var_indices_to_take))) {
                    sub_array_of_values <- Subset(var_with_selectors, names(var_indices_to_take),
                                                  as.list(var_indices_to_take), drop = 'selected')
                  } else {
                    # time across some file dim (e.g., "file_date") but doesn't have 
                    # this file dim as dimension (e.g., time: [sdate, time])
                    sub_array_of_values <- var_with_selectors
                  }
                } else {
                  sub_array_of_values <- var_with_selectors
                }
              }
              if (debug) {
                if (inner_dim %in% dims_to_check) {
                  print("-> STRUCTURE OF THE SUB ARRAY FROM THE VARIABLE CORRESPONDING TO THE SUB ARRAY OF SELECTORS")
                  print(str(sub_array_of_values))
                  print(dim(sub_array_of_values))
                  print("-> NAME OF THE FILE DIMENSION THE CURRENT INNER DIMENSION EXTENDS ALONG:")
                  print(crossed_file_dim)
                }
              }

              # The inner dim selector is an array in which have file dim (e.g., time = [sdate = 2, time = 4],
              # or the inner dim doesn't go across any file dim (e.g., no time_across = 'sdate')
              if ((!is.null(crossed_file_dim) & (any(crossed_file_dim %in% names(selector_file_dims)))) || is.null(crossed_file_dim)) {
                if (length(sub_array_of_selectors) > 0) {
                  if (debug) {
                    if (inner_dim %in% dims_to_check) {
                      print("-> THE INNER DIMENSION DOES NOT GO ACROSS ANY FILE DIMENSION OR IT DOES BUT IS IN THE PROVIDED SELECTOR ARRAY.")
                    }
                  }
                  if (selectors_are_indices) {
                    if (!is.null(var_with_selectors_name)) {
                      max_allowed <- ifelse(aiat, m, n)
                    } else {
                      max_allowed <- data_dims[inner_dim]
                    }
                    if (any(na.omit(unlist(sub_array_of_selectors)) > max_allowed) ||
                        any(na.omit(unlist(sub_array_of_selectors)) < 1)) {
                      stop("Provided indices out of range for dimension '", inner_dim, "' ", 
                           "for dataset '", dat[[i]][['name']], "' (accepted range: 1 to ", 
                           max_allowed, ").")
                    }
                  }
                  
                  # The selector_checker will return either a vector of indices or a list
                  # with the first and last desired indices.
                  #NOTE: goes_across_prime_meridian may be TRUE only if the selector is list of values
                  goes_across_prime_meridian <- FALSE
                  is_circular_dim <- FALSE
                  # If selectors are indices and _reorder = CircularSort() is used, change
                  # is_circular_dim to TRUE.
                  if (!is.null(var_ordered) & selectors_are_indices &
                      !is.null(dim_reorder_params[[inner_dim]])) {
                    if ('circular' %in% names(attributes(dim_reorder_params[[inner_dim]]))) {
                      is_circular_dim <- attr(dim_reorder_params[[inner_dim]], "circular")
                      if (is_circular_dim & is.list(sub_array_of_selectors)) {
                        tmp <- dim_reorder_params[[inner_dim]](unlist(sub_array_of_selectors))$ix
                        goes_across_prime_meridian <-  tmp[1] > tmp[2]
                      }
                    }
                  }

                  # If selectors are values and _reorder is defined.
                  if (!is.null(var_ordered) && !selectors_are_indices) {
                    if (!is.null(dim_reorder_params[[inner_dim]])) {
                      if ('circular' %in% names(attributes(dim_reorder_params[[inner_dim]]))) {
                        is_circular_dim <- attr(dim_reorder_params[[inner_dim]], "circular")
                      }
                      if (is.list(sub_array_of_selectors)) {
                        ## NOTE: The check of 'goes_across_prime_meridian' is moved forward to here.
                        if (is_circular_dim) {
                          # NOTE: Use CircularSort() to put the values in the assigned range, and get the order.
                          # For example, [-10, 20] in CircularSort(0, 360) is [350, 20]. The $ix list is [2, 1].
                          # 'goes_across_prime_meridian' means the selector range across the border. For example,
                          # CircularSort(-180, 180) with selector [170, 190] -> goes_across_prime_meridian = TRUE.
                          # dim_reorder_params is a list of Reorder function, i.e.,
                          # Sort() or CircularSort().
                          tmp <- dim_reorder_params[[inner_dim]](unlist(sub_array_of_selectors))$ix
                          goes_across_prime_meridian <-  tmp[1] > tmp[2]
                        }
                        
                        #NOTE: HERE change to the same code as below (under 'else'). Not sure why originally 
                        #      it uses additional lines, which make reorder not work.
                        # If "_reorder" is used, here 'sub_array_of_selectors' is adjusted to 
                        # follow the reorder rule. E.g., if lat = values(list(-90, 90)) and 
                        # lat_reorder = Sort(decreasing = T), 'sub_array_of_selectors' changes
                        # from list(-90, 90) to list(90, -90).
                        sub_array_of_selectors <- as.list(dim_reorder_params[[inner_dim]](unlist(sub_array_of_selectors))$x)
                        #sub_array_reordered <- dim_reorder_params[[inner_dim]](unlist(sub_array_of_selectors))
                        #sub_array_unorder <- sort(sub_array_reordered$ix, index.return = TRUE)$ix
                        #sub_array_of_selectors <- as.list(sub_array_reordered$x[sub_array_unorder])
                        
                        # Add warning if the boundary is out of range
                        if (min(unlist(sub_array_of_selectors)) < range(var_ordered)[1]) { 
                          show_out_of_range_warning(inner_dim, range = range(var_ordered), 
                                                    bound = 'lower')
                        }
                        if (max(unlist(sub_array_of_selectors)) > range(var_ordered)[2]) {
                          show_out_of_range_warning(inner_dim, range = range(var_ordered),                                                     bound = 'upper')
                        }
                        
                        
                      } else {
                        sub_array_of_selectors <- dim_reorder_params[[inner_dim]](sub_array_of_selectors)$x
                      }
                    }
                    
                    # NOTE: The ideal solution for selecting indices in goes_across_prime_meridian case
                    # is modified SelectorCheckor.R. But now SelectorCheckor doesn't know the info of
                    #goes_across_prime_meridian, so I do the adjustion after calling SelectorCheckor().
                    sub_array_of_indices <- selector_checker(sub_array_of_selectors, var_ordered,
                                                             tolerance = if (aiat) {
                                                               NULL
                                                             } else {
                                                               tolerance_params[[inner_dim]]
                                                             })
                    
                    if (goes_across_prime_meridian & sub_array_of_indices[[1]] < sub_array_of_indices[[2]]) {
                      if (!(sub_array_of_selectors[[1]] %in% var_ordered)){
                        sub_array_of_indices[[1]] <- sub_array_of_indices[[1]] - 1
                      }
                      
                      if (!(sub_array_of_selectors[[2]] %in% var_ordered)){
                        sub_array_of_indices[[2]] <- sub_array_of_indices[[2]] + 1
                      }
                    }
                    
                    #NOTE: the possible case?
                    if (goes_across_prime_meridian & sub_array_of_indices[[1]] > sub_array_of_indices[[2]]) {
                      stop("The case is goes_across_prime_meridian but no adjustion for the indices!")
                    }
                    
                    if (any(is.na(sub_array_of_indices))) {
                      
                      stop(paste0("The selectors of ", inner_dim,
                                  " are out of range [", min(var_ordered),
                                  ", ", max(var_ordered), "]."))
                    }
                    
                  } else {
                    
                    # Add warning if the boundary is out of range
                    if (is.list(sub_array_of_selectors) & !selectors_are_indices) {
                      if (min(unlist(sub_array_of_selectors)) < min(sub_array_of_values)) {
                         show_out_of_range_warning(inner_dim, range = range(sub_array_of_values),
                                                   bound = 'lower')
                      }
                      if (max(unlist(sub_array_of_selectors)) > max(sub_array_of_values)) {
                         show_out_of_range_warning(inner_dim, range = range(sub_array_of_values),
                                                   bound = 'upper')
                      }
                    }
                    
                    # sub_array_of_values here is NULL if selectors are indices, and 
                    # 'sub_array_of_indices' will be sub_array_of_selectors, i.e., the indices
                    # assigned (but rounded).
                    sub_array_of_indices <- selector_checker(sub_array_of_selectors, sub_array_of_values,
                                                             tolerance = if (aiat) {
                                                               NULL
                                                             } else {
                                                               tolerance_params[[inner_dim]]
                                                             })
                    
                    if (any(is.na(sub_array_of_indices))) {
                      
                      stop(paste0("The selectors of ", inner_dim,
                                  " are out of range [", min(sub_array_of_values),
                                  ", ", max(sub_array_of_values), "]."))
                    }
                    
                  }

                  #////////////////////////////////////////////////////////////
                  # If chunking along this inner dim, this part creates the indices for each chunk.
                  # For example, if 'sub_array_of_indices' is c(5:10) and chunked into 2, 
                  # 'sub_array_of_indices' becomes c(5:7) for chunk = 1, c(8:10) for chunk = 2. 
                  # If 'sub_array_of_indices' is list(55, 62) and chunked into 2, it becomes
                  # list(55, 58) for chunk = 1 and list(59, 62) for chunk = 2. 
                  #TODO: The list can be turned into vector here? So afterward no need to judge if
                  #      it is list or vector.
                  #NOTE: chunking cannot be done if goes_across_prime_meridian = TRUE. 
                  #TODO: Change the algorithm to make chunking works for goes_across_prime_meridian = TRUE.
                  #      If goes_across_prime_meridian = TRUE, "sub_array_of_indices" are not 
                  #      continuous numbers. For example, list(37, 1243) means sub_array_of_fri
                  #      that will be generated based on sub_array_of_indices later is c(1:37, 1243:1296).
                  #      the longitude are separated into 2 parts, therefore, cannot be chunked here.
                  if (chunks[[inner_dim]]["n_chunks"] > 1) {
                    if (goes_across_prime_meridian) {
                      stop(paste0("Chunking over ", inner_dim, " that goes across the circular border assigned by '", inner_dim, "_reorder' is not supported by startR now. Adjust the ", inner_dim, " selector to be within the border or change the borders." ))
                    }
                    if (!is.list(sub_array_of_indices)) {
                      sub_array_of_indices <- 
                        sub_array_of_indices[get_chunk_indices(length(sub_array_of_indices), 
                                                               chunks[[inner_dim]]["chunk"], 
                                                               chunks[[inner_dim]]["n_chunks"], 
                                                               inner_dim)]
                    } else {
                      tmp <- 
                        get_chunk_indices(length(sub_array_of_indices[[1]]:sub_array_of_indices[[2]]),
                                          chunks[[inner_dim]]["chunk"], chunks[[inner_dim]]["n_chunks"],
                                          inner_dim)
                      vect <- sub_array_of_indices[[1]]:sub_array_of_indices[[2]]
                      sub_array_of_indices[[1]] <- vect[tmp[1]]
                      sub_array_of_indices[[2]] <- vect[tmp[length(tmp)]]
                    }
                  }
                  # The sub_array_of_indices now contains numeric indices of the values to be taken by each chunk.
                  #////////////////////////////////////////////////////////////


                  #----------------------------------------------------------
                  # 'sub_sub_array_of_values' is for sri chunking. If this inner dim is chunked,
                  # the sri has to follow the chunking of fri. Therefore, we save the original 
                  # value of this chunk here for later use. We'll find the corresponding 
                  # transformed value within 'sub_sub_array_of_values' and chunk sri.
                  if (with_transform & chunks[[inner_dim]]["n_chunks"] > 1) {
                    if (!is.null(var_ordered)) {  #var_ordered
                      input_array_of_values <- var_ordered
                    } else {
                      if (is.null(sub_array_of_values)) {  # selectors are indices
                        #NOTE: Not sure if 'vars_to_transform' is the correct one to use.
                        input_array_of_values <- vars_to_transform[[var_with_selectors_name]]
                      } else {
                        input_array_of_values <- sub_array_of_values
                      }
                    }
                    tmp <- generate_sub_sub_array_of_values(
                             input_array_of_values, sub_array_of_indices,
                             number_of_chunk = chunks[[inner_dim]]["chunk"])
                    sub_sub_array_of_values <- tmp$sub_sub_array_of_values
                    previous_sub_sub_array_of_values <- tmp$previous_sub_sub_array_of_values
                  }
                  #----------------------------------------------------------

                  if (debug) {
                    if (inner_dim %in% dims_to_check) {
                      print("-> TRANSFORMATION REQUESTED?")
                      print(with_transform)
                      print("-> BETA:")
                      print(beta)
                    }
                  }
                  if (with_transform) {
                    # If there is a transformation and selector values are provided, these
                    # selectors will be processed in the same way either if aiat = TRUE or
                    # aiat = FALSE.
                    ## TODO: If sub_array_of_selectors was integer and aiat then... do what's commented 50 lines below.
                    ##       otherwise, do what's coded.
                    if (debug) {
                      if (inner_dim %in% dims_to_check) {
                        print("-> SELECTORS REQUESTED BEFORE TRANSFORM.")
                      }
                    }
                    # Generate sub_array_of_fri 
                    sub_array_of_fri <- generate_sub_array_of_fri(
                      with_transform, goes_across_prime_meridian, sub_array_of_indices, n, beta, 
                      is_circular_dim)
                    # May be useful for crop = T. 'subset_vars_to_transform' may not need
                    # to include extra cells, but currently it shows mistake if not include.
                    sub_array_of_fri_no_beta <- generate_sub_array_of_fri(
                      with_transform, goes_across_prime_meridian, sub_array_of_indices, n, beta,
                      is_circular_dim, add_beta = FALSE)

                    subset_vars_to_transform <- vars_to_transform
                    if (!is.null(var_ordered)) {
                      
                      #NOTE: If var_ordered is common_vars, it doesn't have attributes and it is a vector.
                      #      Turn it into array and add dimension name.
                      if (!is.array(var_ordered)) {
                        var_ordered <- as.array(var_ordered)
                        names(dim(var_ordered)) <- inner_dim
                      }
                      
                      subset_vars_to_transform[[var_with_selectors_name]] <- Subset(var_ordered, inner_dim, sub_array_of_fri)
                    } else {
                      if (!selectors_are_indices) {  # selectors are values
                      #NOTE: It should be redundant because without reordering the var should remain array
                      ## But just stay same with above...
                      if (!is.array(sub_array_of_values)) {
                        sub_array_of_values <- as.array(sub_array_of_values)
                        names(dim(sub_array_of_values)) <- inner_dim
                      }

                      subset_vars_to_transform[[var_with_selectors_name]] <- Subset(sub_array_of_values, inner_dim, sub_array_of_fri)

                      } else {  # selectors are indices
                      subset_vars_to_transform[[var_with_selectors_name]] <-
                        Subset(subset_vars_to_transform[[var_with_selectors_name]],
                               inner_dim, sub_array_of_fri)
                      }
                    }
                    tmp <- .withWarnings(
                      do.call(transform, c(list(data_array = NULL,
                                                variables = subset_vars_to_transform,
                                                file_selectors = selectors_of_first_files_with_data[[i]],
                                                crop_domain = transform_crop_domain),
                                          transform_params))$variables[[var_with_selectors_name]]
                    )
                    transformed_subset_var <- tmp$value
                    warnings2 <- c(warnings2, tmp$warnings)
                    
                    # Sorting the transformed variable and working out the indices again after transform.
                    if (!is.null(dim_reorder_params[[inner_dim]])) {
                      transformed_subset_var_reorder <- dim_reorder_params[[inner_dim]](transformed_subset_var)
                      transformed_subset_var <- transformed_subset_var_reorder$x
                      #NOTE: The fix here solves the mis-ordered lon when across_meridian. 
                      transformed_subset_var_unorder <- transformed_subset_var_reorder$ix
                      #                      transformed_subset_var_unorder <- sort(transformed_subset_var_reorder$ix, index.return = TRUE)$ix
                    } else {
                      transformed_subset_var_unorder <- 1:length(transformed_subset_var)
                    }
                    if (!selectors_are_indices) {  # selectors are values
                      sub_array_of_sri <- selector_checker(
                        sub_array_of_selectors, transformed_subset_var,
                        tolerance = if (aiat) {
                          tolerance_params[[inner_dim]]
                          } else {
                            NULL
                          })
                      if (!is.list(sub_array_of_sri)) {
                        sub_array_of_sri <- unique(sub_array_of_sri)
                      }
                    } else {  # selectors are indices
                      # Need to transfer to values first, then use the values to get the new
                      # indices in transformed_subset_var.
                      if (is.list(sub_array_of_selectors)) {
                        ori_values <- vars_to_transform[[var_with_selectors_name]][sub_array_of_selectors[[1]]:sub_array_of_selectors[[2]]]
                      } else {
                        ori_values <- vars_to_transform[[var_with_selectors_name]][sub_array_of_selectors]
                      }
                      sub_array_of_sri <- selector_checker(
                        ori_values, transformed_subset_var,
                        tolerance = if (aiat) {
                          tolerance_params[[inner_dim]]
                          } else {
                            NULL
                          })
                      # Here may need to further modify considering aiat. If aiat = FALSE,
                      # (i.e., indices are taken before transform), unique() is needed.
                      sub_array_of_sri <- unique(sub_array_of_sri)
                    }
                    
                    # Check if selectors fall out of the range of the transform grid
                    # It may happen when original lon is [-180, 180] while want to regrid to
                    # [0, 360], and lon selector = [-20, -10].
                    if (any(is.na(sub_array_of_sri))) {
                      stop(paste0("The selectors of ",
                                  inner_dim, " are out of range of transform grid '",
                                  transform_params$grid, "'. Use parameter '",
                                  inner_dim, "_reorder' or change ", inner_dim,
                                  " selectors."))
                    }
                    
                    if (goes_across_prime_meridian) {
                      
                      if (sub_array_of_sri[[1]] == sub_array_of_sri[[2]]) {
                        # global longitude
                        sub_array_of_sri <- c(1:length(transformed_subset_var))
                      } else {
                        # the common case, i.e., non-global
#                        # NOTE: Because sub_array_of_sri order is exchanged due to 
#                        # previous development, here [[1]] and [[2]] should exchange
#                        sub_array_of_sri <- c(1:sub_array_of_sri[[1]],
#                                              sub_array_of_sri[[2]]:length(transformed_subset_var))
                        #NOTE: the old code above is not suitable for all the possible cases.
                        #      If sub_array_of_selectors is not exactly the value in transformed_subset_var, sub_array_of_sri[[1]] will be larger than sub_array_of_sri[[2]].
                        #      Though here is not global case, we already have transformed_subset_var cropped as the desired region, so it is okay to use the whole length. Not sure if it will cause other problems...
                        sub_array_of_sri <- 1:length(transformed_subset_var)
                      }
                      
                    } else if (is.list(sub_array_of_sri)) {
                      sub_array_of_sri <- sub_array_of_sri[[1]]:sub_array_of_sri[[2]]
                    }

#========================================================

# Instead of using values to find sri, directly use the destination grid to count.
#NOTE: sub_array_of_sri seems to start at 1 always (because crop = c(lonmin, lonmax, latmin, latmax) already?)
                    if (chunks[[inner_dim]]["n_chunks"] > 1) {
                      sub_array_of_sri <- sub_array_of_sri[get_chunk_indices(
                        length(sub_array_of_sri),
                        chunks[[inner_dim]]["chunk"],
                        chunks[[inner_dim]]["n_chunks"],
                        inner_dim)]
                    }
#========================================================

                    ordered_sri <- sub_array_of_sri
                    sub_array_of_sri <- transformed_subset_var_unorder[sub_array_of_sri]

###########################old##################################
#                    if (chunks[[inner_dim]]["n_chunks"] > 1) {
#                      tmp <- which(transformed_subset_var >= min(sub_sub_array_of_values) &
#                                   transformed_subset_var <= max(sub_sub_array_of_values))
#                      sub_array_of_sri <- sub_array_of_sri[tmp]
#                    }
################################################################

                    # In this case, the tvi are not defined and the 'transformed_subset_var'
                    # will be taken instead of the var transformed before in the code.
                    if (debug) {
                      if (inner_dim %in% dims_to_check) {
                        print("-> FIRST INDEX:")
#                        print(first_index)
                        print("NOTE: Check function generate_sub_array_of_fri() in zzz.R")
                        print("-> LAST INDEX:")
#                        print(last_index)
                        print("NOTE: Check function generate_sub_array_of_fri() in zzz.R")
                        print("-> STRUCTURE OF FIRST ROUND INDICES:")
                        print(str(sub_array_of_fri))
                        print("-> STRUCTURE OF SECOND ROUND INDICES:")
                        print(str(sub_array_of_sri))
                        print("-> STRUCTURE OF TRANSFORMED VARIABLE INDICES:")
                        print(str(tvi))
                      }
                    }
                    ###                    # If the selectors are expressed after transformation
                    ###                    } else {
                    ###if (debug) {
                    ###if (inner_dim %in% dims_to_check) {
                    ###print("-> SELECTORS REQUESTED AFTER TRANSFORM.")
                    ###}
                    ###}
                    ###                      if (goes_across_prime_meridian) {
                    ###                        sub_array_of_indices <- c(sub_array_of_indices[[1]]:m,
                    ###                                                    1:sub_array_of_indices[[2]])
                    ###                      }
                    ###                      first_index <- min(unlist(sub_array_of_indices))
                    ###                      last_index <- max(unlist(sub_array_of_indices))
                    ###                      first_index_before_transform <- max(transform_indices(first_index, m, n) - beta, 1)
                    ###                      last_index_before_transform <- min(transform_indices(last_index, m, n) + beta, n)
                    ###                      sub_array_of_fri <- first_index_before_transform:last_index_before_transform
                    ###                      n_of_extra_cells <- round(beta / n * m)
                    ###                      if (is.list(sub_array_of_indices) && (length(sub_array_of_indices) > 1)) {
                    ###                        sub_array_of_sri <- 1:(last_index - first_index + 1) 
                    ###                        if (is.null(tvi)) {
                    ###                          tvi <- sub_array_of_sri + first_index - 1
                    ###                        }
                    ###                      } else {
                    ###                        sub_array_of_sri <- sub_array_of_indices - first_index + 1
                    ###                        if (is.null(tvi)) {
                    ###                          tvi <- sub_array_of_indices
                    ###                        }
                    ###                      }
                    ###                      sub_array_of_sri <- sub_array_of_sri + n_of_extra_cells
                    sri <- do.call('[[<-', c(list(x = sri), as.list(selector_store_position),
                                             list(value = sub_array_of_sri)))

                  } else {  # !with_transform
                    sub_array_of_fri <- generate_sub_array_of_fri(
                      with_transform, goes_across_prime_meridian, sub_array_of_indices, n, beta,
                      is_circular_dim)
                  }

                  # Reorder sub_array_of_fri if reordering function is used.
                  # It was index in the assigned order (e.g., in [-180, 180] if CircularSort(-180, 180)), and here is changed to the index in the original order.
                  if (!is.null(var_unorder_indices)) {
                    if (is.null(ordered_fri)) {
                      ordered_fri <- sub_array_of_fri
                    }
                    sub_array_of_fri <- var_unorder_indices[sub_array_of_fri]
                  }
                  fri <- do.call('[[<-', c(list(x = fri), as.list(selector_store_position),
                                           list(value = sub_array_of_fri)))

                  #NOTE: This part existed always but never was used. taken_chunks
                  #      is related to merge_across_dims, but I don't know how it is
                  #      used (maybe for higher efficiency?)
#                  if (!is.null(crossed_file_dim)) {
#                    taken_chunks[selector_store_position[[crossed_file_dim]]] <- TRUE
#                  } else {
                  taken_chunks <- TRUE
#                  }
                }
              } else {
              # The inner dim goes across a file dim (e.g., time_across = 'sdate')
                if (debug) {
                  if (inner_dim %in% dims_to_check) {
                    print("-> THE INNER DIMENSION GOES ACROSS A FILE DIMENSION.")
                  }
                }
                # If "<inner_dim>_across = <crossed_file_dim> + merge_across_dims = FALSE + chunk over <inner_dim>", return error because this instance is not logically correct.
                if (chunks[[inner_dim]]["n_chunks"] > 1 & inner_dim %in% inner_dims_across_files &
                    merge_across_dims == FALSE) {
                  stop("Chunk over dimension '", inner_dim, "' is not allowed because '",
                       inner_dim, "' is across '",
                       names(inner_dims_across_files)[which(inner_dim %in% inner_dims_across_files)],
                       "' and 'merge_across_dims' is set to FALSE'.")
                }

                if (inner_dim %in% names(dim(sub_array_of_selectors))) {
                  if (is.null(var_with_selectors_name)) {
                    if (!largest_dims_length | (largest_dims_length & length(unique(inner_dim_lengths)) <= 1)) {  #old code
                      maximal_indice <- data_dims[inner_dim] * chunk_amount
                    } else { # files have different length of inner dim
                      maximal_indice <- sum(inner_dim_lengths)
                    }

                    if (any(na.omit(unlist(sub_array_of_selectors)) < 1) ||
                        any(na.omit(unlist(sub_array_of_selectors)) > maximal_indice)) {
                      stop("Provided indices out of range for dimension '", inner_dim, "' ",
                           "for dataset '", dat[[i]][['name']], "' (accepted range: 1 to ",
                           maximal_indice, ").")
                    }
                  } else {
                    if (inner_dim %in% names(dim(sub_array_of_values))) {
                      # NOTE: Put across-inner-dim at the 1st position.
                      # POSSIBLE PROB!! Only organize inner dim, the rest dims may not in the same order as sub_array_of_selectors below.
                      inner_dim_pos_in_sub_array <- which(names(dim(sub_array_of_values)) == inner_dim)
                      if (inner_dim_pos_in_sub_array != 1) {
                        new_sub_array_order <- (1:length(dim(sub_array_of_values)))[-inner_dim_pos_in_sub_array]
                        new_sub_array_order <- c(inner_dim_pos_in_sub_array, new_sub_array_order)
                        sub_array_of_values <- .aperm2(sub_array_of_values, new_sub_array_order)
                      }
                    }
                  }
                  
                  # NOTE: Put across-inner-dim at the 1st position.
                  # POSSIBLE PROB!! Only organize inner dim, the rest dims may not in the same order as sub_array_of_values above.
                  inner_dim_pos_in_sub_array <- which(names(dim(sub_array_of_selectors)) == inner_dim)
                  if (inner_dim_pos_in_sub_array != 1) {
                    new_sub_array_order <- (1:length(dim(sub_array_of_selectors)))[-inner_dim_pos_in_sub_array]
                    new_sub_array_order <- c(inner_dim_pos_in_sub_array, new_sub_array_order)
                    sub_array_of_selectors <- .aperm2(sub_array_of_selectors, new_sub_array_order)
                  }
                  sub_array_of_indices <- selector_checker(sub_array_of_selectors, sub_array_of_values,
                                                           tolerance = tolerance_params[[inner_dim]])
                  # It is needed to expand the indices here, otherwise for 
                  # values(list(date1, date2)) only 2 values are picked.
                  if (is.list(sub_array_of_indices)) {
                    sub_array_of_indices <- sub_array_of_indices[[1]]:sub_array_of_indices[[2]]
                  }
                  sub_array_of_indices <- sub_array_of_indices[get_chunk_indices(length(sub_array_of_indices),
                                                                             chunks[[inner_dim]]['chunk'],
                                                                             chunks[[inner_dim]]['n_chunks'],
                                                                             inner_dim)]
                  sub_array_is_list <- FALSE
                  if (is.list(sub_array_of_indices)) {
                    sub_array_is_list <- TRUE
                    sub_array_of_indices <- unlist(sub_array_of_indices)
                  }

                  # "indices_chunk" refers to in which file the 
                  # sub_array_of_indices is; "transformed_indices" 
                  # refers to the indices of sub_array_of_indices in each file.
                  if (!largest_dims_length | 
                      (largest_dims_length & length(unique(inner_dim_lengths)) <= 1)) {
                    # old code; all the files have the same length of inner_dim
                    if (is.null(var_with_selectors_name)) {
                      indices_chunk <- floor((sub_array_of_indices - 1) / data_dims[inner_dim]) + 1
                      transformed_indices <- ((sub_array_of_indices - 1) %% data_dims[inner_dim]) + 1
                    } else {
                      indices_chunk <- floor((sub_array_of_indices - 1) / var_full_dims[inner_dim]) + 1
                      transformed_indices <- ((sub_array_of_indices - 1) %% var_full_dims[inner_dim]) + 1
                    }
                  } else {  # files have different inner dim length
                    indices_chunk <- c()
                    for (item in 1:length(inner_dim_lengths)) {
                      tmp <- which(sub_array_of_indices <= cumsum(inner_dim_lengths)[item])
                      indices_chunk <- c(indices_chunk, rep(item, length(tmp) - length(indices_chunk)))
                    }
                    sub_array_of_indices_by_file <- split(sub_array_of_indices, indices_chunk)
                    for (item in names((sub_array_of_indices_by_file))) {
                      # If item is 1, cumsum(inner_dim_lengths)[item - 1] returns numeric(0)
                      if (as.numeric(item) > 1) {
                        sub_array_of_indices_by_file[[item]] <- sub_array_of_indices_by_file[[item]] - cumsum(inner_dim_lengths)[as.numeric(item) - 1]
                      }
                    }
                    transformed_indices <- unlist(sub_array_of_indices_by_file, use.names = FALSE)
                  }

                  if (sub_array_is_list) {
                    sub_array_of_indices <- as.list(sub_array_of_indices)
                  }
                  if (debug) {
                    if (inner_dim %in% dims_to_check) {
                      print("-> GOING TO ITERATE ALONG CHUNKS.")
                    }
                  }

                  for (chunk in 1:chunk_amount) {
                    if (!is.null(names(selector_store_position))) {
                      selector_store_position[crossed_file_dim] <- chunk
                    } else {
                      selector_store_position <- chunk
                    }
                    sub_array_of_indices <- transformed_indices[which(indices_chunk == chunk)]

                    #NOTE: This 'with_transform' part is probably not tested because 
                    #      here is for the inner dim that goes across a file dim, which
                    #      is normally not lat and lon dimension. If in the future, we 
                    #      can interpolate time, this part needs to be examined.
                    if (with_transform) {
                      # If the provided selectors are expressed in the world
                      # before transformation
                      if (!aiat) {
                        first_index <- min(unlist(sub_array_of_indices))
                        last_index <- max(unlist(sub_array_of_indices))
                        sub_array_of_fri <- max(c(first_index - beta, 1)):min(c(last_index + beta, n))
                        sub_array_of_sri <- transform_indices(unlist(sub_array_of_indices) - first_index + 1, n, m)
                        if (is.list(sub_array_of_indices)) {
                          if (length(sub_array_of_sri) > 1) {
                            sub_array_of_sri <- sub_array_of_sri[[1]]:sub_array_of_sri[[2]]
                          }
                        }
                        ##TODO: TRANSFORM SUBSET VARIABLE AS ABOVE, TO COMPUTE SRI
                        # If the selectors are expressed after transformation
                      } else {
                        first_index <- min(unlist(sub_array_of_indices))
                        last_index <- max(unlist(sub_array_of_indices))
                        first_index_before_transform <- max(transform_indices(first_index, m, n) - beta, 1)
                        last_index_before_transform <- min(transform_indices(last_index, m, n) + beta, n)
                        sub_array_of_fri <- first_index_before_transform:last_index_before_transform
                        if (is.list(sub_array_of_indices) && (length(sub_array_of_indices) > 1)) {
                          sub_array_of_sri <- 1:(last_index - first_index + 1) + 
                            round(beta / n * m) 
                        } else {
                          sub_array_of_sri <- sub_array_of_indices - first_index + 1 +
                            round(beta / n * m)
                        }
                        ##TODO: FILL IN TVI
                      }
                      sri <- do.call('[[<-', c(list(x = sri), as.list(selector_store_position),
                                               list(value = sub_array_of_sri)))
                      if (length(sub_array_of_sri) > 0) {
                        taken_chunks[chunk] <- TRUE
                      }
                    } else {
                      sub_array_of_fri <- sub_array_of_indices
                      if (length(sub_array_of_fri) > 0) {
                        taken_chunks[chunk] <- TRUE
                      }
                    }

                    if (!is.null(var_unorder_indices)) {
                      ordered_fri <- sub_array_of_fri
                      sub_array_of_fri <- var_unorder_indices[sub_array_of_fri]
                    }
                    fri <- do.call('[[<-', c(list(x = fri), as.list(selector_store_position),
                                             list(value = sub_array_of_fri)))
                  }
                  if (debug) {
                    if (inner_dim %in% dims_to_check) {
                      print("-> FINISHED ITERATING ALONG CHUNKS")
                    }
                  }
                } else {
                  stop("Provided array of indices for dimension '", inner_dim, "', ",
                       "which goes across the file dimension '", crossed_file_dim, "', but ",
                       "the provided array does not have the dimension '", inner_dim, 
                       "', which is mandatory.")
                }
              }
            }
          }
          if (debug) {
            if (inner_dim %in% dims_to_check) {
              print("-> PROCEEDING TO CROP VARIABLES")
            }
          }
          #if ((length(selector_array) == 1) && (selector_array %in% c('all', 'first', 'last'))) {
          #if (!is.null(var_with_selectors_name) || (is.null(var_with_selectors_name) && is.character(selector_array) &&
          #    (length(selector_array) == 1) && (selector_array %in% c('all', 'first', 'last')))) {
          empty_chunks <- which(!taken_chunks)
          if (length(empty_chunks) >= length(taken_chunks)) {
            stop("Selectors do not match any of the possible values for the dimension '", inner_dim, "'.")
          }
          if (length(empty_chunks) > 0) {
            #                # Get the first group of chunks to remove, and remove them. 
            #                # E.g., from c(1, 2, 4, 5, 6, 8, 9) remove only 1 and 2
            #                dist <- abs(rev(empty_chunks) - c(rev(empty_chunks)[1] - 1, head(rev(empty_chunks), length(rev(empty_chunks)) - 1)))
            #                if (all(dist == 1)) {
            #                  start_chunks_to_remove <- NULL
            #                } else {
            #                  first_chunk_to_remove <- tail(which(dist > 1), 1)
            #                  start_chunks_to_remove <- rev(rev(empty_chunks)[first_chunk_to_remove:length(empty_chunks)])
            #                }
            #                # Get the last group of chunks to remove, and remove them. 
            #                # E.g., from c(1, 2, 4, 5, 6, 8, 9) remove only 8 and 9
            #                dist <- abs(empty_chunks - c(empty_chunks[1] - 1, head(empty_chunks, length(empty_chunks) - 1)))
            #                if (all(dist == 1)) {
            #                  first_chunk_to_remove <- 1
            #                } else {
            #                  first_chunk_to_remove <- tail(which(dist > 1), 1)
            #                }
            #                end_chunks_to_remove <- empty_chunks[first_chunk_to_remove:length(empty_chunks)]
            #                chunks_to_keep <- which(!((1:length(taken_chunks)) %in% c(start_chunks_to_remove, end_chunks_to_remove)))
            chunks_to_keep <- which(taken_chunks)
            dims_to_crop[[crossed_file_dim]] <- c(dims_to_crop[[crossed_file_dim]], list(chunks_to_keep))
            #                found_indices <- Subset(found_indices, crossed_file_dim, chunks_to_keep)
            #                # Crop dataset variables file dims.
            #                for (picked_var in names(picked_vars[[i]])) {
            #                  if (crossed_file_dim %in% names(dim(picked_vars[[i]][[picked_var]]))) {
            #                    picked_vars[[i]][[picked_var]] <- Subset(picked_vars[[i]][[picked_var]], crossed_file_dim, chunks_to_keep)
            #                  }
            #                }
          }
          #}
          dat[[i]][['selectors']][[inner_dim]] <- list(fri = fri, sri = sri)
          # Crop dataset variables inner dims.
          # Crop common variables inner dims.
          types_of_var_to_crop <- 'picked'
          if (with_transform) {
            types_of_var_to_crop <- c(types_of_var_to_crop, 'transformed')
          }
          if (!is.null(dim_reorder_params[[inner_dim]])) {
            types_of_var_to_crop <- c(types_of_var_to_crop, 'reordered')
          }
          for (type_of_var_to_crop in types_of_var_to_crop) {
            if (type_of_var_to_crop == 'transformed') {
              if (is.null(tvi)) {
                if (!is.null(dim_reorder_params[[inner_dim]])) {
                  crop_indices <- unique(unlist(ordered_sri))
                } else {
                  crop_indices <- unique(unlist(sri))
                }
              } else {
                crop_indices <- unique(unlist(tvi))
              }
              vars_to_crop <- transformed_vars[[i]]
              common_vars_to_crop <- transformed_common_vars
            } else if (type_of_var_to_crop == 'reordered') {
              crop_indices <- unique(unlist(ordered_fri))
              vars_to_crop <- picked_vars_ordered[[i]]
              common_vars_to_crop <- picked_common_vars_ordered
            } else {
              #TODO: If fri has different indices in each list, the crop_indices should be 
              #      separated for each list. Otherwise, picked_common_vars later will be wrong.
              crop_indices <- unique(unlist(fri))
              vars_to_crop <- picked_vars[[i]]
              common_vars_to_crop <- picked_common_vars
            }
            for (var_to_crop in names(vars_to_crop)) {
              if (inner_dim %in% names(dim(vars_to_crop[[var_to_crop]]))) {
                if (!is.null(crop_indices)) {
                  if (type_of_var_to_crop == 'transformed') {
                    if (!aiat) {
                      if (!(length(selector_array) == 1 &
                            all(selector_array %in% c('all', 'first', 'last')))) {
                        vars_to_crop[[var_to_crop]] <- Subset(transformed_subset_var, inner_dim, crop_indices)
                      } else {
                        vars_to_crop[[var_to_crop]] <-
                          Subset(transformed_var_with_selectors, inner_dim, crop_indices)
                      }
                    } else {
                      vars_to_crop[[var_to_crop]] <- Subset(vars_to_crop[[var_to_crop]], inner_dim, crop_indices)
                    }
                  } else {
                    vars_to_crop[[var_to_crop]] <- Subset(vars_to_crop[[var_to_crop]], inner_dim, crop_indices)
                  }
                }
              }
            }
            if (i == length(dat)) {
              for (common_var_to_crop in names(common_vars_to_crop)) {
                if (inner_dim %in% names(dim(common_vars_to_crop[[common_var_to_crop]]))) {

                  if (type_of_var_to_crop == 'transformed' & !aiat) {
                    if (!(length(selector_array) == 1 &
                          all(selector_array %in% c('all', 'first', 'last')))) {
                      common_vars_to_crop[[common_var_to_crop]] <- 
                        Subset(transformed_subset_var, inner_dim, crop_indices)
                    } else {
                      common_vars_to_crop[[common_var_to_crop]] <-
                        Subset(transformed_var_with_selectors, inner_dim, crop_indices)
                    }
                  } else {
                    if (!is.null(crossed_file_dim)) {  #merge_across_dims, crossed_file_dim is the depended file dim
                      #NOTE: When is not this case??? Maybe this condition is not needed
                      if (any(crossed_file_dim %in% names(dim(common_vars_to_crop[[common_var_to_crop]])))) {
                        tmp <- common_vars_to_crop[[common_var_to_crop]]
                        tmp_attributes <- attributes(common_vars_to_crop[[common_var_to_crop]])
                        dim_extra_ind <- which(!names(dim(tmp)) %in% c(crossed_file_dim, inner_dim))
                        if (!identical(dim_extra_ind, integer(0))) {
                          tmp_list <- asplit(tmp, dim_extra_ind)
                          dim_file_ind <- which(names(dim(tmp_list[[1]])) %in% crossed_file_dim)
                          tmp_list <- lapply(tmp_list, asplit, dim_file_ind)
                        } else {  # only crossed_file_dim and inner_dim
                          dim_file_ind <- which(names(dim(tmp)) %in% crossed_file_dim)
                          tmp_list <- asplit(tmp, dim_file_ind)
                          # Add another layer to be consistent with the first case above
                          tmp_list <- list(tmp_list)
                        }
                        max_fri_length <- max(sapply(fri, length))
                        for (i_extra_dim in 1:length(tmp_list)) {
                          for (i_fri in 1:length(fri)) {
                            tmp_list[[i_extra_dim]][[i_fri]] <-
                              tmp_list[[i_extra_dim]][[i_fri]][fri[[i_fri]]]
  
                            if (length(tmp_list[[i_extra_dim]][[i_fri]]) != max_fri_length) {
                              tmp_list[[i_extra_dim]][[i_fri]] <-
                                c(tmp_list[[i_extra_dim]][[i_fri]], rep(NA, max_fri_length - length(tmp_list[[i_extra_dim]][[i_fri]])))
                            }
                          }
                        }
                        # Change list back to array
                        tmp_new_dim <- c(max_fri_length, dim(tmp)[crossed_file_dim], dim(tmp)[dim_extra_ind])
                        names(tmp_new_dim) <- c(inner_dim, crossed_file_dim, names(dim(tmp))[dim_extra_ind])
                        common_vars_to_crop[[common_var_to_crop]] <-
                          array(unlist(tmp_list), dim = tmp_new_dim)
                        # Reorder back
                        common_vars_to_crop[[common_var_to_crop]] <-
                          aperm(common_vars_to_crop[[common_var_to_crop]], match(names(dim(tmp)), names(tmp_new_dim)))
                        # Put attributes back
                        tmp <- which(!names(tmp_attributes) %in% names(attributes(common_vars_to_crop[[common_var_to_crop]])))
                        attributes(common_vars_to_crop[[common_var_to_crop]]) <- 
                          c(attributes(common_vars_to_crop[[common_var_to_crop]]), 
                            tmp_attributes[tmp])

                        if ('time' %in% synonims[[common_var_to_crop]]) {
                          # Convert number back to time
                          common_vars_to_crop[[common_var_to_crop]] <-
                            as.POSIXct(common_vars_to_crop[[common_var_to_crop]],
                                       origin = "1970-01-01", tz = 'UTC')
                        }
                      }
                    } else {  # old code

                      common_vars_to_crop[[common_var_to_crop]] <- Subset(common_vars_to_crop[[common_var_to_crop]], inner_dim, crop_indices)
                    }

                  }

                }
              }
            }
            if (type_of_var_to_crop == 'transformed') {
              if (!is.null(vars_to_crop)) {
                transformed_vars[[i]] <- vars_to_crop
              }
              if (i == length(dat)) {
                transformed_common_vars <- common_vars_to_crop
              }
            } else if (type_of_var_to_crop == 'reordered') {
              if (!is.null(vars_to_crop)) {
                picked_vars_ordered[[i]] <- vars_to_crop
              }
              if (i == length(dat)) {
                picked_common_vars_ordered <- common_vars_to_crop
              }
            } else {
              if (!is.null(vars_to_crop)) {
                picked_vars[[i]] <- vars_to_crop
              }
              if (i == length(dat)) {
                #NOTE: To avoid redundant run
                if (inner_dim %in% names(common_vars_to_crop)) {
                  picked_common_vars <- common_vars_to_crop
                }
              }
            }
          }
          #}
        }
        # After the selectors have been picked (using the original variables), 
        # the variables are transformed. At that point, the original selectors
        # for the transformed variables are also kept in the variable original_selectors.
        #print("L")
      }
    }
  }
  #  if (!is.null(transformed_common_vars)) {
  #    picked_common_vars[names(transformed_common_vars)] <- transformed_common_vars
  #  }
  # Remove the trailing chunks, if any.
  for (file_dim in names(dims_to_crop)) {
    #    indices_to_keep <- min(sapply(dims_to_crop[[file_dim]], min)):max(sapply(dims_to_crop[[file_dim]], max))
    ## TODO: Merge indices in dims_to_crop with some advanced mechanism?
    indices_to_keep <- unique(unlist(dims_to_crop[[file_dim]]))
    array_of_files_to_load <- Subset(array_of_files_to_load, file_dim, indices_to_keep)
    array_of_not_found_files <- Subset(array_of_not_found_files, file_dim, indices_to_keep)
    for (i in 1:length(dat)) {
      # Crop selectors
      for (selector_dim in names(dat[[i]][['selectors']])) {
        if (selector_dim == file_dim) {
          for (j in 1:length(dat[[i]][['selectors']][[selector_dim]][['fri']])) {
            dat[[i]][['selectors']][[selector_dim]][['fri']][[j]] <- dat[[i]][['selectors']][[selector_dim]][['fri']][[j]][indices_to_keep]
          }
          for (j in 1:length(dat[[i]][['selectors']][[selector_dim]][['sri']])) {
            dat[[i]][['selectors']][[selector_dim]][['sri']][[j]] <- dat[[i]][['selectors']][[selector_dim]][['sri']][[j]][indices_to_keep]
          }
        }
        if (file_dim %in% names(dim(dat[[i]][['selectors']][[selector_dim]][['fri']]))) {
          dat[[i]][['selectors']][[selector_dim]][['fri']] <- Subset(dat[[i]][['selectors']][[selector_dim]][['fri']], file_dim, indices_to_keep)
          dat[[i]][['selectors']][[selector_dim]][['sri']] <- Subset(dat[[i]][['selectors']][[selector_dim]][['sri']], file_dim, indices_to_keep)
        }
      }
      # Crop dataset variables file dims.
      for (picked_var in names(picked_vars[[i]])) {
        if (file_dim %in% names(dim(picked_vars[[i]][[picked_var]]))) {
          picked_vars[[i]][[picked_var]] <- Subset(picked_vars[[i]][[picked_var]], file_dim, indices_to_keep)
        }
      }
      for (transformed_var in names(transformed_vars[[i]])) {
        if (file_dim %in% names(dim(transformed_vars[[i]][[transformed_var]]))) {
          transformed_vars[[i]][[transformed_var]] <- Subset(transformed_vars[[i]][[transformed_var]], file_dim, indices_to_keep)
        }
      }
    }
    # Crop common variables file dims.
    for (picked_common_var in names(picked_common_vars)) {
      if (file_dim %in% names(dim(picked_common_vars[[picked_common_var]]))) {
        picked_common_vars[[picked_common_var]] <- Subset(picked_common_vars[[picked_common_var]], file_dim, indices_to_keep)
      }
    }
    for (transformed_common_var in names(transformed_common_vars)) {
      if (file_dim %in% names(dim(transformed_common_vars[[transformed_common_var]]))) {
        transformed_common_vars[[transformed_common_var]] <- Subset(transformed_common_vars[[transformed_common_var]], file_dim, indices_to_keep)
      }
    }
  }
  # Calculate the size of the final array.
  total_inner_dims <- NULL
  for (i in 1:length(dat)) {
    if (dataset_has_files[i]) {
      inner_dims <- expected_inner_dims[[i]]
      inner_dims <- sapply(inner_dims, 
                           function(x) {
                             if (!all(sapply(dat[[i]][['selectors']][[x]][['sri']], is.null))) {
                               max(sapply(dat[[i]][['selectors']][[x]][['sri']], length))
                             } else {
                               if (length(var_params[[x]]) > 0) {
                                 if (var_params[[x]] %in% names(transformed_vars[[i]])) {
                                   length(transformed_vars[[i]][[var_params[[x]]]])
                                 } else if (var_params[[x]] %in% names(transformed_common_vars)) {
                                   length(transformed_common_vars[[var_params[[x]]]])
                                 } else {
                                   max(sapply(dat[[i]][['selectors']][[x]][['fri']], length))
                                 }
                               } else {
                                 max(sapply(dat[[i]][['selectors']][[x]][['fri']], length))
                               }
                             }
                           })
      names(inner_dims) <- expected_inner_dims[[i]]
      if (is.null(total_inner_dims)) {
        total_inner_dims <- inner_dims
      } else {
        new_dims <- .MergeArrayDims(total_inner_dims, inner_dims)
        total_inner_dims <- new_dims[[3]]
      }
    }
  }
  new_dims <- .MergeArrayDims(dim(array_of_files_to_load), total_inner_dims)
  final_dims <- new_dims[[3]][dim_names]
  # final_dims_fake is the vector of final dimensions after having merged the 
  # 'across' file dimensions with the respective 'across' inner dimensions, and
  # after having broken into multiple dimensions those dimensions for which 
  # multidimensional selectors have been provided.
  # final_dims will be used for collocation of data, whereas final_dims_fake 
  # will be used for shaping the final array to be returned to the user.
  final_dims_fake <- final_dims
  if (merge_across_dims) {
    final_dims_fake <- dims_merge(inner_dims_across_files, final_dims_fake)
  }
  #=========================================================================
  # Find the dimension to split if split_multiselected_dims = TRUE.
  # If there is no dimension able to be split, change split_multiselected_dims to FALSE.
  all_split_dims <- NULL
  inner_dim_has_split_dim <- NULL
  if (split_multiselected_dims) {
    tmp <- dims_split(dim_params, final_dims_fake)
    final_dims_fake <- tmp[[1]]
    # all_split_dims is a list containing all the split dims
    all_split_dims <- tmp[[2]]

   if (is.null(all_split_dims)) {
     split_multiselected_dims <- FALSE
     .warning(paste0("Not found any dimensions able to be split. The parameter ",
                     "'split_multiselected_dims' is changed to FALSE."))
   } else {
    tmp_fun <- function (x, y) {
      any(names(dim(x)) %in% y)
    }
    if (!is.null(picked_common_vars)) {
      inner_dim_has_split_dim <- names(which(unlist(lapply(
                                   picked_common_vars, tmp_fun, names(all_split_dims)))))
      if (!identical(inner_dim_has_split_dim, character(0))) {
        # If merge_across_dims also, it will be replaced later
        saved_reshaped_attr <- attr(picked_common_vars[[inner_dim_has_split_dim]], 'variables')
      }
    }
   }
  }
  #======================================================================
  # If only merge_across_dims and merge_across_dims_narm and no split_multiselected_dims,
  # the length of inner across dim (e.g., time) needs to be adjusted. Sum up the actual length
  # without potential NAs.
  if (merge_across_dims) {
    # Prepare the arguments for later use
    across_inner_dim <- inner_dims_across_files[[1]]  #TODO: more than one?
    # Get the length of each inner_dim ('time') along each file_dim ('file_date')  
    length_inner_across_dim <- lapply(dat[[i]][['selectors']][[across_inner_dim]][['fri']], length)
    dims_of_merge_dim <- dim(picked_common_vars[[across_inner_dim]])
    # Save attributes for later use. If split_multiselected_dims, this variable has been created above but is replaced here
    saved_reshaped_attr <- attr(picked_common_vars[[across_inner_dim]], 'variables')

    if (merge_across_dims_narm & !split_multiselected_dims) {
      final_dims_fake <- merge_narm_dims(final_dims_fake, across_inner_dim, length_inner_across_dim)
    }
  }
  
  if (!silent) {
    .message("Detected dimension sizes:")
    longest_dim_len <- max(sapply(names(final_dims_fake), nchar))
    longest_size_len <- max(sapply(paste0(final_dims_fake, ''), nchar))
    sapply(names(final_dims_fake), 
           function(x) {
             message(paste0("*   ", paste(rep(' ', longest_dim_len - nchar(x)), collapse = ''), 
                            x, ": ", paste(rep(' ', longest_size_len - nchar(paste0(final_dims_fake[x], ''))), collapse = ''), 
                            final_dims_fake[x]))
           })
    bytes <- prod(c(final_dims_fake, 8))
    dim_sizes <- paste(final_dims_fake, collapse = ' x ')
    if (retrieve) {
      .message(paste("Total size of requested data:"))
    } else {
      .message(paste("Total size of involved data:"))
    }
    .message(paste(dim_sizes, " x 8 bytes =", 
                   format(structure(bytes, class = "object_size"), units = "auto")), 
             indent = 2)
  }
  
  # NOTE: If split_multiselected_dims + merge_across_dims, the dim order may need to be changed.
  #       The inner_dim needs to be the first dim among split dims.
  # TODO: Cannot control the rest dims are in the same order or not...
  #       Suppose users put the same order of across inner and file dims.
  if (split_multiselected_dims & merge_across_dims) {
    # TODO: More than one split?
    inner_dim_pos_in_split_dims <- which(names(all_split_dims[[1]]) == inner_dims_across_files)  

    # if inner_dim is not the first, change!
    if (inner_dim_pos_in_split_dims != 1) {
      # Save the current final_dims_fake for reordering it back later
      final_dims_fake_output <- final_dims_fake
      tmp <- reorder_split_dims(all_split_dims[[1]], inner_dim_pos_in_split_dims, final_dims_fake)
      final_dims_fake <- tmp[[1]]
      all_split_dims[[1]] <- tmp[[2]]
    }
  }
  if (merge_across_dims | split_multiselected_dims) {
    if (!merge_across_dims & split_multiselected_dims & identical(inner_dim_has_split_dim, character(0))) {
      final_dims_fake_metadata <- NULL
    } else {
      if (!merge_across_dims & split_multiselected_dims & !is.null(picked_common_vars)) {
        if (any(names(all_split_dims[[1]]) %in% names(dim(picked_common_vars[[inner_dim_has_split_dim]]))) &
            names(all_split_dims)[1] != inner_dim_has_split_dim) {
          if (inner_dim_has_split_dim %in% names(final_dims)) {
            stop("Detect inner dimension in the split array, but merge_across_dims is not used. The output dimensions will be repeated. Check if the dimensions and parameters are correctly defined.")
          } else {
            # Only split no merge, time dim is not explicitly defined because the 
            # length is 1, the sdate dim to be split having 'time' as one dimension.
            # --> Take 'time' dim off from picked_common_vars.
            dim(picked_common_vars[[inner_dim_has_split_dim]]) <- dim(picked_common_vars[[inner_dim_has_split_dim]])[-which(names(dim(picked_common_vars[[inner_dim_has_split_dim]])) == inner_dim_has_split_dim)]
          }
        }
      }
      final_dims_fake_metadata <- find_final_dims_fake_metadata(
        merge_across_dims, split_multiselected_dims, picked_common_vars = picked_common_vars[[inner_dim_has_split_dim]], across_inner_dim,
        final_dims_fake, dims_of_merge_dim, all_split_dims)
    }
  }

   # store warning messages from transform
   warnings3 <- NULL

  # The following several lines will only run if retrieve = TRUE
  if (retrieve) {
    
    ########## CREATING THE SHARED MATRIX AND DISPATCHING WORK PIECES ###########
    # TODO: try performance of storing all in cols instead of rows
    # Create the shared memory array, and a pointer to it, to be sent
    # to the work pieces.
    if (is.null(ObjectBigmemory)) {
        data_array <- bigmemory::big.matrix(nrow = prod(final_dims), ncol = 1)
    } else {
        data_array <- bigmemory::big.matrix(nrow = prod(final_dims), ncol = 1,
                                            backingfile = ObjectBigmemory,
                                            init = NA)
    }
    shared_matrix_pointer <- bigmemory::describe(data_array)
    if (is.null(ObjectBigmemory)) {
        name_bigmemory_obj <- attr(shared_matrix_pointer, 'description')$sharedName
    } else {
        name_bigmemory_obj <- attr(shared_matrix_pointer, 'description')$filename
    }

    #warning(paste("SharedName:", attr(shared_matrix_pointer, 'description')$sharedName))
    #warning(paste("Filename:", attr(shared_matrix_pointer, 'description')$filename))
    #if (!is.null(ObjectBigmemory)) {
    #  attr(shared_matrix_pointer, 'description')$sharedName <- ObjectBigmemory
    #}
    if (is.null(num_procs)) {
      num_procs <- future::availableCores()
    }
    # Creating a shared tmp folder to store metadata from each chunk
    array_of_metadata_flags <- array(FALSE, dim = dim(array_of_files_to_load))
    if (!is.null(metadata_dims)) {
      metadata_indices_to_load <- as.list(rep(1, length(dim(array_of_files_to_load))))
      names(metadata_indices_to_load) <- names(dim(array_of_files_to_load))
      metadata_indices_to_load[metadata_dims] <- as.list(rep(TRUE, length(metadata_dims)))
      array_of_metadata_flags <- do.call('[<-', c(list(array_of_metadata_flags),  metadata_indices_to_load,
                                                  list(value = rep(TRUE, prod(dim(array_of_files_to_load)[metadata_dims])))))
    }
    metadata_file_counter <- 0
    metadata_folder <- tempfile('metadata')
    dir.create(metadata_folder)
    # Build the work pieces, each with:
    # - file path
    # - total size (dims) of store array
    # - start position in store array
    # - file selectors (to provide extra info. useful e.g. to select variable)
    # - indices to take from file
    work_pieces <- list()
    for (i in 1:length(dat)) {
      if (dataset_has_files[i]) {
        # metadata_file_counter may be changed by the following function
        work_pieces <- build_work_pieces(
                         work_pieces = work_pieces, i = i, selectors = dat[[i]][['selectors']], 
                         file_dims = found_file_dims[[i]],
                         inner_dims = expected_inner_dims[[i]], final_dims = final_dims,
                         found_pattern_dim = found_pattern_dim, 
                         inner_dims_across_files = inner_dims_across_files,
                         array_of_files_to_load = array_of_files_to_load,
                         array_of_not_found_files = array_of_not_found_files,
                         array_of_metadata_flags = array_of_metadata_flags,
                         metadata_file_counter = metadata_file_counter,
                         depending_file_dims = depending_file_dims, transform = transform,
                         transform_vars = transform_vars, picked_vars = picked_vars[[i]],
                         picked_vars_ordered = picked_vars_ordered[[i]],
                         picked_common_vars = picked_common_vars,
                         picked_common_vars_ordered = picked_common_vars_ordered, 
                         metadata_folder = metadata_folder, debug = debug)
      }
    }
    #print("N")
    if (debug) {
      print("-> WORK PIECES BUILT")
    }
    
    # Calculate the progress %s that will be displayed and assign them to 
    # the appropriate work pieces.
    work_pieces <- retrieve_progress_message(work_pieces, num_procs, silent)


    # NOTE: In .LoadDataFile(), metadata is saved in metadata_folder/1 or /2 etc. Before here,
    #       the path name is created in work_pieces but the path hasn't been built yet.
    if (num_procs == 1) {
      tmp <- .withWarnings(
        lapply(work_pieces, .LoadDataFile, 
                            shared_matrix_pointer = shared_matrix_pointer,
                            file_data_reader = file_data_reader, 
                            synonims = synonims,
                            transform = transform, 
                            transform_params = transform_params,
                            transform_crop_domain = transform_crop_domain,
                            silent = silent, debug = debug)
      )
      found_files <- tmp$value
      warnings3 <- c(warnings3, tmp$warnings)
      
    } else {
      cluster <- parallel::makeCluster(num_procs, outfile = "")
      # Send the heavy work to the workers
      ##NOTE: .withWarnings() can't catch warnings like it does above (num_procs == 1). The warnings
      ##      show below when "bigmemory::as.matrix(data_array)" is called. Don't know how to fix it for now.
      work_errors <- try({
        found_files <- parallel::clusterApplyLB(cluster, work_pieces, .LoadDataFile, 
                                                shared_matrix_pointer = shared_matrix_pointer,
                                                file_data_reader = file_data_reader,
                                                synonims = synonims,
                                                transform = transform, 
                                                transform_params = transform_params,
                                                transform_crop_domain = transform_crop_domain,
                                                silent = silent, debug = debug)
      })
      parallel::stopCluster(cluster)
    }
    
    if (!silent) {
      # if (progress_message != '')
      if (length(work_pieces) / num_procs >= 2 && !silent) {
        .message("\n", tag = '')
      }
    }
    #print("P")

    # If merge_across_dims = TRUE, there might be additional NAs due to unequal
    # inner_dim ('time') length across file_dim ('file_date').
    # If merge_across_dims_narm = TRUE, add additional lines to remove these NAs.
    # TODO: Now it assumes that only one '_across'. Add a for loop for more-than-one case. 
    if (merge_across_dims & (split_multiselected_dims | merge_across_dims_narm)) {
      if (!merge_across_dims_narm) {
        data_array_tmp <- array(bigmemory::as.matrix(data_array), dim = final_dims)
        tmp <- match(names(final_dims), names(dims_of_merge_dim))
        if (any(diff(tmp[!is.na(tmp)]) < 0)) { #need to reorder
          picked_common_vars[[across_inner_dim]] <- .aperm2(picked_common_vars[[across_inner_dim]], tmp[!is.na(tmp)])
        }
        metadata_tmp <- picked_common_vars[[across_inner_dim]]
      } else {
        tmp <- remove_additional_na_from_merge(
                 data_array = bigmemory::as.matrix(data_array),
                 merge_dim_metadata = picked_common_vars[[across_inner_dim]],
                 inner_dims_across_files, final_dims,
                 length_inner_across_dim)
        data_array_tmp <- tmp$data_array
        metadata_tmp <- tmp$merge_dim_metadata
      }

      if (length(data_array_tmp) != prod(final_dims_fake)) {
        stop(paste0("After reshaping, the data do not fit into the expected output dimension. ",
                    "Check if the reshaping parameters are used correctly."))
      }
      if (length(metadata_tmp) != prod(final_dims_fake_metadata)) {
        stop(paste0("After reshaping, the metadata do not fit into the expected output dimension. ",
                    "Check if the reshaping parameters are used correctly or contact support."))
      }

      #NOTE: When one file contains values for dicrete dimensions, rearrange the 
      #      chunks (i.e., work_piece) is necessary.
      if (split_multiselected_dims) {
        tmp <- rebuild_array_merge_split(
                 data_array = data_array_tmp, metadata = metadata_tmp, indices_chunk,
                 all_split_dims, final_dims_fake, across_inner_dim, length_inner_across_dim)
        data_array_tmp <- tmp$data_array
        metadata_tmp <- tmp$metadata
      }

      data_array <- array(data_array_tmp, dim = final_dims_fake)
      metadata_tmp <- array(metadata_tmp, dim = final_dims_fake_metadata)

      # If split_multiselected_dims + merge_across_dims, the dimension order may change above.
      # To get the user-required dim order, we need to reorder the array again.
      if (split_multiselected_dims) {
        if (inner_dim_pos_in_split_dims != 1) {
          correct_order <- match(names(final_dims_fake_output), names(final_dims_fake))
          data_array <- .aperm2(data_array, correct_order)
          correct_order_metadata <- match(names(final_dims_fake_output), names(all_split_dims[[across_inner_dim]]))
          metadata_tmp <- .aperm2(metadata_tmp, correct_order_metadata[!is.na(correct_order_metadata)])
        }
      }
      # Convert numeric back to dates
      if ('time' %in% synonims[[across_inner_dim]]) {
        metadata_tmp <- as.POSIXct(metadata_tmp, origin = "1970-01-01", tz = 'UTC')
      }

      picked_common_vars[[across_inner_dim]] <- metadata_tmp
      attr(picked_common_vars[[across_inner_dim]], 'variables') <- saved_reshaped_attr

    } else {  # ! (merge_across_dims + split_multiselected_dims) (old version)
      data_array <- array(bigmemory::as.matrix(data_array), dim = final_dims_fake)
      if (merge_across_dims) {
        # merge_across_dims = TRUE but (merge_across_dims_narm = F & split_multiselected_dims = F)

        inner_dim_pos <- which(names(dims_of_merge_dim) == inner_dims_across_files)
        file_dim_pos <- which(names(dims_of_merge_dim) == names(inner_dims_across_files))
        if (file_dim_pos < inner_dim_pos) {  #need to reorder
          tmp <- seq(1, length(dims_of_merge_dim))
          tmp[inner_dim_pos] <- file_dim_pos
          tmp[file_dim_pos] <- inner_dim_pos
          picked_common_vars[[across_inner_dim]] <- .aperm2(picked_common_vars[[across_inner_dim]], tmp)
        }
        metadata_tmp <- array(picked_common_vars[[across_inner_dim]], dim = final_dims_fake_metadata)
        # Convert numeric back to dates
        if ('time' %in% synonims[[across_inner_dim]]) {
          metadata_tmp <- as.POSIXct(metadata_tmp, origin = "1970-01-01", tz = 'UTC')
        }
        picked_common_vars[[across_inner_dim]] <- metadata_tmp
        attr(picked_common_vars[[across_inner_dim]], 'variables') <- saved_reshaped_attr
      }
      if (split_multiselected_dims & !is.null(picked_common_vars)) {
        if (!identical(inner_dim_has_split_dim, character(0))) {
          metadata_tmp <- array(picked_common_vars[[inner_dim_has_split_dim]], dim = final_dims_fake_metadata)
          # Convert numeric back to dates
          if (inherits(picked_common_vars[[inner_dim_has_split_dim]], 'POSIXct')) {
            metadata_tmp <- as.POSIXct(metadata_tmp, origin = "1970-01-01", tz = 'UTC')
          }
          picked_common_vars[[inner_dim_has_split_dim]] <- metadata_tmp
          attr(picked_common_vars[[inner_dim_has_split_dim]], 'variables') <- saved_reshaped_attr
        }
      }
    }
   
    gc()
    
    # Load metadata and remove the metadata folder
    if (!is.null(metadata_dims)) {
      loaded_metadata_files <- list.files(metadata_folder)

      if (!identical(loaded_metadata_files, character(0))) {  # old code
        loaded_metadata <- lapply(paste0(metadata_folder, '/', loaded_metadata_files), readRDS)
      } else {
        loaded_metadata <- NULL
      }

      unlink(metadata_folder, recursive = TRUE)

      # Create a list of metadata of the variable (e.g., tas) 
      return_metadata <- create_metadata_list(array_of_metadata_flags, metadata_dims, pattern_dims,
                                              loaded_metadata_files, loaded_metadata, dat_names,
                                              dataset_has_files) 
      # TODO: Try to infer data type from loaded_metadata
      # as.integer(data_array) 
    }
    
    failed_pieces <- work_pieces[which(unlist(found_files))]
    for (failed_piece in failed_pieces) {
      array_of_not_found_files <- do.call('[<-', 
                                          c(list(array_of_not_found_files), 
                                            as.list(failed_piece[['file_indices_in_array_of_files']]),
                                            list(value = TRUE)))
    }
    if (any(array_of_not_found_files)) {
      for (i in 1:prod(dim(array_of_files_to_load))) {
        if (is.na(array_of_not_found_files[i])) {
          array_of_files_to_load[i] <- NA
        } else {
          if (array_of_not_found_files[i]) {
            array_of_not_found_files[i] <- array_of_files_to_load[i]
            array_of_files_to_load[i] <- NA
          } else {
            array_of_not_found_files[i] <- NA
          }
        }
      }
    } else {
      array_of_not_found_files <- NULL
    }
    
  } # End if (retrieve)
  else { # if retrieve = FALSE, metadata still needs to reshape

    if (merge_across_dims & (split_multiselected_dims | merge_across_dims_narm)) {
      if (!merge_across_dims_narm) {
        tmp <- match(names(final_dims), names(dims_of_merge_dim))
        if (any(diff(tmp[!is.na(tmp)]) < 0)) { #need to reorder
          picked_common_vars[[across_inner_dim]] <- .aperm2(picked_common_vars[[across_inner_dim]], tmp[!is.na(tmp)])
        }
        metadata_tmp <- picked_common_vars[[across_inner_dim]]
      } else {
        tmp <- remove_additional_na_from_merge(
                 data_array = NULL,
                 merge_dim_metadata = picked_common_vars[[across_inner_dim]],
                 inner_dims_across_files, final_dims,
                 length_inner_across_dim)
        metadata_tmp <- tmp$merge_dim_metadata
      }

      if (length(metadata_tmp) != prod(final_dims_fake_metadata)) {
        stop(paste0("After reshaping, the metadata do not fit into the expected output dimension. ",
                    "Check if the reshaping parameters are used correctly or contact support."))
      }

      #NOTE: When one file contains values for dicrete dimensions, rearrange the 
      #      chunks (i.e., work_piece) is necessary.
      if (split_multiselected_dims) {
        tmp <- rebuild_array_merge_split(
                 data_array = NULL, metadata = metadata_tmp, indices_chunk,
                 all_split_dims, final_dims_fake, across_inner_dim, length_inner_across_dim)
        metadata_tmp <- tmp$metadata
      }
      metadata_tmp <- array(metadata_tmp, dim = final_dims_fake_metadata)

      # If split_multiselected_dims + merge_across_dims, the dimension order may change above.
      # To get the user-required dim order, we need to reorder the array again.
      if (split_multiselected_dims) {
        if (inner_dim_pos_in_split_dims != 1) {
          correct_order <- match(names(final_dims_fake_output), names(final_dims_fake))
#          data_array <- .aperm2(data_array, correct_order)
          correct_order_metadata <- match(names(final_dims_fake_output), names(all_split_dims[[across_inner_dim]]))
          metadata_tmp <- .aperm2(metadata_tmp, correct_order_metadata[!is.na(correct_order_metadata)])
        }
      }
      # Convert numeric back to dates
      if ('time' %in% synonims[[across_inner_dim]]) {
        metadata_tmp <- as.POSIXct(metadata_tmp, origin = "1970-01-01", tz = 'UTC')
      }
      picked_common_vars[[across_inner_dim]] <- metadata_tmp
      attr(picked_common_vars[[across_inner_dim]], 'variables') <- saved_reshaped_attr
    } else {  # ! (merge_across_dims + split_multiselected_dims) (old version)
      if (merge_across_dims) {
        # merge_across_dims = TRUE but (merge_across_dims_narm = F & split_multiselected_dims = F)

        inner_dim_pos <- which(names(dims_of_merge_dim) == inner_dims_across_files)
        file_dim_pos <- which(names(dims_of_merge_dim) == names(inner_dims_across_files))
        if (file_dim_pos < inner_dim_pos) {  #need to reorder
          tmp <- seq(1, length(dims_of_merge_dim))
          tmp[inner_dim_pos] <- file_dim_pos
          tmp[file_dim_pos] <- inner_dim_pos
          picked_common_vars[[across_inner_dim]] <- .aperm2(picked_common_vars[[across_inner_dim]], tmp)
        }
        metadata_tmp <- array(picked_common_vars[[across_inner_dim]], dim = final_dims_fake_metadata)
        # Convert numeric back to dates
        if ('time' %in% synonims[[across_inner_dim]]) {
          metadata_tmp <- as.POSIXct(metadata_tmp, origin = "1970-01-01", tz = 'UTC')
        }
        picked_common_vars[[across_inner_dim]] <- metadata_tmp
        attr(picked_common_vars[[across_inner_dim]], 'variables') <- saved_reshaped_attr
      }
      if (split_multiselected_dims & !is.null(picked_common_vars)) {
        if (!identical(inner_dim_has_split_dim, character(0))) {
          metadata_tmp <- array(picked_common_vars[[inner_dim_has_split_dim]], dim = final_dims_fake_metadata)
          # Convert numeric back to dates
          if (inherits(picked_common_vars[[inner_dim_has_split_dim]], 'POSIXct')) {
            metadata_tmp <- as.POSIXct(metadata_tmp, origin = "1970-01-01", tz = 'UTC')
          }
          picked_common_vars[[inner_dim_has_split_dim]] <- metadata_tmp
          attr(picked_common_vars[[inner_dim_has_split_dim]], 'variables') <- saved_reshaped_attr
        }
      }
    }
    # Retrieve variable metadata
    # Compare array_of_metadata_flags with array_of_files_to_load to know which files to take for metadata
    if (!is.null(metadata_dims)) {
      array_of_metadata_flags <- array(FALSE, dim = dim(array_of_files_to_load))
      metadata_indices_to_load <- as.list(rep(1, length(dim(array_of_files_to_load))))
      names(metadata_indices_to_load) <- names(dim(array_of_files_to_load))
      metadata_indices_to_load[metadata_dims] <- as.list(rep(TRUE, length(metadata_dims)))
      array_of_metadata_flags <- do.call('[<-', c(list(array_of_metadata_flags),  metadata_indices_to_load,
                                                  list(value = rep(TRUE, prod(dim(array_of_files_to_load)[metadata_dims])))))

      if (tail(names(dim(array_of_files_to_load)), 1) != found_pattern_dim) {
        tmp1 <- s2dv::Reorder(array_of_files_to_load, c(2:length(dim(array_of_files_to_load)), 1))
        tmp2 <- s2dv::Reorder(array_of_metadata_flags, c(2:length(dim(array_of_metadata_flags)), 1))
        files_for_metadata <- tmp1[tmp2]
      } else {
        files_for_metadata <- array_of_files_to_load[array_of_metadata_flags]
      }

      # Get variable name
      #NOTE: This part probably will fail when one netCDF file has more than one variable.
      if (found_pattern_dim %in% metadata_dims) {  # metadata_dims has "dat"
        if (any(metadata_dims %in% c('var', 'variable'))) { # metadata_dim is c('dat', 'var')
          how_many_vars <-  length(dat[[1]][['selectors']]$var[[1]])
        } else if (length(metadata_dims) > 1) {  # metadata_dims is c('dat', xxx)
          how_many_vars <- length(dat[[1]][['selectors']][[metadata_dims[which(found_pattern_dim != metadata_dims)]]][[1]])
        } else {  # metadata_dims is 'dat'
          how_many_vars <- 1
        }
        tmp_var <- matrix(NA, how_many_vars, length(dat))
        for (i_dat in 1:dim(array_of_metadata_flags)[found_pattern_dim]) {
          if (any(metadata_dims %in% c('var', 'variable'))) { # metadata_dims has "var"
            tmp_var[, i_dat] <- dat[[i_dat]][['selectors']]$var[[1]]
          } else if (length(metadata_dims) > 1) {  # metadata_dims is c('dat', xxx)
            tmp_var[, i_dat] <- rep(dat[[i_dat]][['selectors']]$var[[1]][1],
                                    length(dat[[1]][['selectors']][[metadata_dims[which(found_pattern_dim != metadata_dims)]]][[1]]))
          } else {  # metadata_dims is 'dat'
            tmp_var[, i_dat] <- dat[[i_dat]][['selectors']]$var[[1]][1]
          }
        }

        # if metadat_dims = c('dat', 'var') and [dat = 2, var = 2], tmp_var has length 4, like c('tas', 'tos', 'tas', 'tos').
        # if metadata_dims = 'dat' and [dat = 2], tmp_var has length 2 like c('tas', 'tos').
        tmp_var <- c(tmp_var)

      } else {  # metadata_dims doesn't have "dat"
        if (any(metadata_dims %in% c('var', 'variable'))) { # metadata_dims has "var"
          tmp_var <- dat[[1]][['selectors']]$var[[1]]
        } else {
          tmp_var <- rep(dat[[1]][['selectors']]$var[[1]][1], length(dat[[1]][['selectors']][[metadata_dims]][[1]]))
        }
        # if metadata_dims = 'var' and [var = 2], tmp_var has length 2 like c('tas', 'tos')
        # if metadata_dims = 'table' and [table = 2], tmp_var has length 1 like 'tas'
      }

      loaded_metadata <- vector('list', length = length(files_for_metadata))
      for (i_file in 1:length(files_for_metadata)) {
        #NOTE: Not use ncatt_get() because it only gets the attr shown with ncdump -h
        tmp <- file_opener(files_for_metadata[i_file])
        if (!is.null(tmp)) {  # if file exists
          loaded_metadata[[i_file]][[1]] <- tmp$var[[tmp_var[i_file]]]
          names(loaded_metadata[[i_file]]) <- tmp_var[i_file]
          file_closer(tmp)
        }
      }
      # Find loaded_metadata_files identical as "retrieve = T" case. If dataset_has_files is F, deduct that dataset from counting
      ind_loaded_metadata_has_values <- which(!sapply(loaded_metadata, is.null)) # c(1, 2, 4)
      if (!all(dataset_has_files)) {  # If dataset_has_files has F, deduct that dataset from counting
        if (found_pattern_dim %in% metadata_dims) {  # metadata_dims has "dat" 
          dataset_has_files_expand <- rep(dataset_has_files, each = how_many_vars)
          i_ind <- 1
          while (i_ind <= length(ind_loaded_metadata_has_values)) { # 3, 4, 8
            if (ind_loaded_metadata_has_values[i_ind] > i_ind) {
              ind_loaded_metadata_has_values[i_ind] <- ind_loaded_metadata_has_values[i_ind] - length(which(!dataset_has_files_expand[1:dataset_has_files_expand[i_ind]]))
            }
            i_ind <- i_ind + 1
          }
        }
      }
      loaded_metadata_files <- as.character(ind_loaded_metadata_has_values)
      loaded_metadata <- loaded_metadata[which(!sapply(loaded_metadata, is.null))]
      return_metadata <- create_metadata_list(array_of_metadata_flags, metadata_dims, pattern_dims,
                                              loaded_metadata_files, loaded_metadata, dat_names,
                                              dataset_has_files)
    }
  }
  # Print the warnings from transform
  if (!is.null(c(warnings1, warnings2, warnings3))) {
    transform_warnings_list <- lapply(c(warnings1, warnings2, warnings3), function(x) {
      return(x$message)
    })
    transform_warnings_list <- unique(transform_warnings_list)
    for (i in 1:length(transform_warnings_list)) {
      .warning(transform_warnings_list[[i]])
    }
  }
 
  # Change final_dims_fake back because retrieve = FALSE will use it for attributes later
  if (exists("final_dims_fake_output")) {
    final_dims_fake <- final_dims_fake_output
  }
  # Replace the vars and common vars by the transformed vars and common vars
  for (i in 1:length(dat)) {
    if (length(names(transformed_vars[[i]])) > 0) {
      picked_vars[[i]][names(transformed_vars[[i]])] <- transformed_vars[[i]]
    } else if (length(names(picked_vars_ordered[[i]])) > 0) {
      picked_vars[[i]][names(picked_vars_ordered[[i]])] <- picked_vars_ordered[[i]]
    }
  }
  if (length(names(transformed_common_vars)) > 0) {
    picked_common_vars[names(transformed_common_vars)] <- transformed_common_vars
  } else if (length(names(picked_common_vars_ordered)) > 0) {
    picked_common_vars[names(picked_common_vars_ordered)] <- picked_common_vars_ordered
  }
  if (debug) {
    print("-> THE TRANSFORMED VARS:")
    print(str(transformed_vars))
    print("-> THE PICKED VARS:")
    print(str(picked_vars))
  }
  
  file_selectors <- NULL
  for (i in 1:length(dat)) {
    file_selectors[[dat[[i]][['name']]]] <- dat[[i]][['selectors']][which(names(dat[[i]][['selectors']]) %in% found_file_dims[[i]])]
  }

  # Prepare attr Variables
  if (all(sapply(return_metadata, is.null))) {
    # We don't have metadata of the variable (e.g., tas). The returned metadata list only 
    # contains those are specified in argument "return_vars".
    Variables_list <- c(list(common = picked_common_vars), picked_vars)
    .warning(paste0("Metadata cannot be retrieved. The reason may be the ",
                    "non-existence of the first file. Use parameter 'metadata_dims'",
                    " to assign to file dimensions along which to return metadata, ",
                    "or check the existence of the first file."))
  } else {
    # Add the metadata of the variable (e.g., tas) into the list of picked_vars or
    # picked_common_vars.
    Variables_list <- combine_metadata_picked_vars(
                        return_metadata, picked_vars, picked_common_vars,
                        metadata_dims, pattern_dims, length(dat))
  }

  if (retrieve) {
    if (!silent) {
      .message("Successfully retrieved data.")
    }

    attributes(data_array) <- c(attributes(data_array), 
                                list(Variables = Variables_list,
                                     Files = array_of_files_to_load, 
                                     NotFoundFiles = array_of_not_found_files,
                                     FileSelectors = file_selectors,
                                     PatternDim = found_pattern_dim,
                                     ObjectBigmemory = name_bigmemory_obj) #attr(shared_matrix_pointer, 'description')$sharedName)
    )
    attr(data_array, 'class') <- c('startR_array', attr(data_array, 'class'))
    data_array

  } else { # retrieve = FALSE
    if (!silent) {
      .message("Successfully discovered data dimensions.")
    }
    start_call <- match.call()
    for (i in 2:length(start_call)) {
      if (class(start_call[[i]]) %in% c('name', 'call')) {
        tmp <- eval.parent(start_call[[i]])
        if (is.null(tmp)) {
          start_call[i] <- list(NULL)
        } else {
          start_call[[i]] <- eval.parent(start_call[[i]])
        }
      }
    }
    start_call[['retrieve']] <- TRUE
    attributes(start_call) <- c(attributes(start_call),
                                list(Dimensions = final_dims_fake,
                                     Variables = Variables_list,
                                     ExpectedFiles = array_of_files_to_load,
                                     FileSelectors = file_selectors,
                                     PatternDim = found_pattern_dim,
                                     MergedDims = if (merge_across_dims) {
                                       inner_dims_across_files
                                     } else {
                                       NULL
                                     },
                                     SplitDims = if (split_multiselected_dims) {
                                       all_split_dims
                                     } else {
                                       NULL
                                     })
    )
    attr(start_call, 'class') <- c('startR_cube', attr(start_call, 'class'))
    start_call
  }
}

# This function is the responsible for loading the data of each work
# piece.
.LoadDataFile <- function(work_piece, shared_matrix_pointer, 
                          file_data_reader, synonims,
                          transform, transform_params, transform_crop_domain = NULL,
                          silent = FALSE, debug = FALSE) {
  #warning(attr(shared_matrix_pointer, 'description')$sharedName)
  #  suppressPackageStartupMessages({library(bigmemory)})
  ### TODO: Specify dependencies as parameter
  #  suppressPackageStartupMessages({library(ncdf4)})
  
  #print("1")
  store_indices <- as.list(work_piece[['store_position']])
  first_round_indices <- work_piece[['first_round_indices']]
  second_round_indices <- work_piece[['second_round_indices']]
  #print("2")
  file_to_open <- work_piece[['file_path']]
  # Get data and metadata
  sub_array <- file_data_reader(file_to_open, NULL, 
                                work_piece[['file_selectors']],
                                first_round_indices, synonims)
  if (debug) {
    if (all(unlist(store_indices[1:6]) == 1)) {
      print("-> LOADING A WORK PIECE")
      print("-> STRUCTURE OF READ UNTRANSFORMED DATA:")
      print(str(sub_array))
      print("-> STRUCTURE OF VARIABLES TO TRANSFORM:")
      print(str(work_piece[['vars_to_transform']]))
      print("-> COMMON ARRAY DIMENSIONS:")
      print(str(work_piece[['store_dims']]))
    }
  }
  if (!is.null(sub_array)) {
    # Apply data transformation once we have the data arrays.
    if (!is.null(transform)) {
      if (debug) {
        if (all(unlist(store_indices[1:6]) == 1)) {
          print("-> PROCEEDING TO TRANSFORM ARRAY")
          print("-> DIMENSIONS OF ARRAY RIGHT BEFORE TRANSFORMING:")
          print(dim(sub_array))
        }
      }
      sub_array <- do.call(transform, c(list(data_array = sub_array,
                                             variables = work_piece[['vars_to_transform']],
                                             file_selectors = work_piece[['file_selectors']],
                                             crop_domain = transform_crop_domain),
                                        transform_params))
      if (debug) {
        if (all(unlist(store_indices[1:6]) == 1)) {
          print("-> STRUCTURE OF ARRAY AND VARIABLES RIGHT AFTER TRANSFORMING:")
          print(str(sub_array))
          print("-> DIMENSIONS OF ARRAY RIGHT AFTER TRANSFORMING:")
          print(dim(sub_array$data_array))
        }
      }
      sub_array <- sub_array$data_array
      # Subset with second round of indices
      dims_to_crop <- which(!sapply(second_round_indices, is.null))
      if (length(dims_to_crop) > 0) {
        dimnames_to_crop <- names(second_round_indices)[dims_to_crop]
        sub_array <- ClimProjDiags::Subset(sub_array, dimnames_to_crop, 
                                           second_round_indices[dimnames_to_crop])
      }
      if (debug) {
        if (all(unlist(store_indices[1:6]) == 1)) {
          print("-> STRUCTURE OF ARRAY AND VARIABLES RIGHT AFTER SUBSETTING WITH 2nd ROUND INDICES:")
          print(str(sub_array))
        }
      }
    }
    
    metadata <- attr(sub_array, 'variables')
    
    names_bk <- names(store_indices)
    store_indices <- lapply(names(store_indices), 
                            function (x) {
                              if (!(x %in% names(first_round_indices))) {
                                store_indices[[x]]
                              } else if (is.null(second_round_indices[[x]])) {
                                1:dim(sub_array)[x]
                              } else {
                                if (is.numeric(second_round_indices[[x]])) {
                                  ## TODO: Review carefully this line. Inner indices are all 
                                  ## aligned to the left-most positions. If dataset A has longitudes
                                  ## 1, 2, 3, 4 but dataset B has only longitudes 3 and 4, then
                                  ## they will be stored as follows:
                                  ## 1, 2, 3, 4
                                  ## 3, 4, NA, NA
                                  ##x - min(x) + 1
                                  1:length(second_round_indices[[x]])
                                } else {
                                  1:length(second_round_indices[[x]])
                                }
                              }
                            })
    names(store_indices) <- names_bk
    if (debug) {
      if (all(unlist(store_indices) == 1)) {
        print("-> STRUCTURE OF FIRST ROUND INDICES FOR THIS WORK PIECE:")
        print(str(first_round_indices))
        print("-> STRUCTURE OF SECOND ROUND INDICES FOR THIS WORK PIECE:")
        print(str(second_round_indices))
        print("-> STRUCTURE OF STORE INDICES FOR THIS WORK PIECE:")
        print(str(store_indices))
      }
    }
    
    store_indices <- lapply(store_indices, as.integer)
    store_dims <- work_piece[['store_dims']]
    
    # split the storage work of the loaded subset in parts
    largest_dim_name <- names(dim(sub_array))[which.max(dim(sub_array))]
    max_parts <- length(store_indices[[largest_dim_name]])
    
    # Indexing a data file of N MB with expand.grid takes 30*N MB
    # The peak ram of Start is, minimum, 2 * total data to load from all files
    # due to inefficiencies in other regions of the code
    # The more parts we split the indexing done below in, the lower
    # the memory footprint of the indexing and the fast. 
    # But more than 10 indexing iterations (parts) for each MB processed 
    # makes the iteration slower (tested empirically on BSC workstations).
    subset_size_in_mb <- prod(dim(sub_array)) * 8 / 1024 / 1024
    best_n_parts <- ceiling(subset_size_in_mb * 10)
    # We want to set n_parts to a greater value than the one that would 
    # result in a memory footprint (of the subset indexing code below) equal
    # to 2 * total data to load from all files.
    # s = subset size in MB
    # p = number of parts to break it in
    # T = total size of data to load
    # then, s / p * 30 = 2 * T
    # then, p = s * 15 / T
    min_n_parts <- ceiling(prod(dim(sub_array)) * 15 / prod(store_dims))
    # Make sure we pick n_parts much greater than the minimum calculated
    n_parts <- min_n_parts * 10
    if (n_parts > best_n_parts) {
      n_parts <- best_n_parts
    }
    # Boundary checks
    if (n_parts < 1) {
      n_parts <- 1
    }
    if (n_parts > max_parts) {
      n_parts <- max_parts
    }
    
    if (n_parts > 1) {
      make_parts <- function(length, n) {
        clusters <- cut(1:length, n, labels = FALSE)
        lapply(1:n, function(y) which(clusters == y))
      }
      part_indices <- make_parts(max_parts, n_parts)
      parts <- lapply(part_indices, 
                      function(x) {
                        store_indices[[largest_dim_name]][x]
                      })
    } else {
      part_indices <- list(1:max_parts)
      parts <- store_indices[largest_dim_name]
    }
    
    # do the storage work
    weights <- sapply(1:length(store_dims), 
                      function(i) prod(c(1, store_dims)[1:i]))
    part_indices_in_sub_array <- as.list(rep(TRUE, length(dim(sub_array))))
    names(part_indices_in_sub_array) <- names(dim(sub_array))
    data_array <- bigmemory::attach.big.matrix(shared_matrix_pointer)
    for (i in 1:n_parts) {
      store_indices[[largest_dim_name]] <- parts[[i]]
      # Converting array indices to vector indices
      matrix_indices <- do.call("expand.grid", store_indices)
      # Given a matrix where each row is a set of array indices of an element
      # the vector indices are computed
      matrix_indices <- 1 + colSums(t(matrix_indices - 1) * weights)
      part_indices_in_sub_array[[largest_dim_name]] <- part_indices[[i]]
      data_array[matrix_indices] <- as.vector(do.call('[',
                                                      c(list(x = sub_array), 
                                                        part_indices_in_sub_array)))
    }
    rm(data_array)
    gc()
    
    if (!is.null(work_piece[['save_metadata_in']])) {
      saveRDS(metadata, file = work_piece[['save_metadata_in']])
    }
  }
  if (!is.null(work_piece[['progress_amount']]) && !silent) {
    message(work_piece[['progress_amount']], appendLF = FALSE)
  }
  is.null(sub_array)
}
