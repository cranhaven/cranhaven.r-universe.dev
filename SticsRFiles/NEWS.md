# SticsRFiles 1.5.0 _2024-11-13_
* FUNCTIONS
  * New functions for generating xml general parameters files
  * New function for copying .mod, .obs, .lai and weather (climat.txt) files 
  
* FIXES
  * pattern to get obs files for intercrop usms or sole crops usms
  * generation of xml parameters files
  
* DATA
  * Added information about the latest STICS version 10.2.0


# SticsRFiles 1.4.0 _2024-07-16_

* FUNCTIONS
  * New functions for computing dates from days numbers with reference to
   starting year
  * compute_day_from_date now uses a start year instead of a date, and date may
  of type character
  * get_param_txt, set_param_txt, get_param_xml, set_param_xml now use values
  ids for getting or setting specific parameter vector values 
  specific values
  * New functions for getting cultivars list and extracting cultivars parameters
  as a data.frame

* FIXES
  * management of STICS version consistency with text files content using
  get_param_txt or set_param_txt


# SticsRFiles 1.3.0 _2024-04-10_

* FUNCTIONS
   * Message improvement for missing xml files detected before text files
   generation
   * New functions for computing days numbers from dates with reference to
   starting dates

* FIXES
   * Updating STICS versions information 
   * get_file using usm names (regex pattern)
   * upgrade _ini.xml file (magrain0 parameter position)
   * xml files generation: missing data management when loading xls/csv 
   files
   * text files generation: plant and tec files were not generated in
   intercropping usms for the associated crop

* DOC
   * added license file


# SticsRFiles 1.2.0 _2024-02-21_

* FUNCTIONS
   * Text files generation from xml files are now done without using JavaStics 
   command line (the speedup is about 10 times)
   * Getting parameters information (get_param_info) do not use XML files 
   exploration anymore, the speed is largely improved
   * Improvement of XML files manipulations/generation for reducing memory 
   use/freezing in R when intensive use is performed
   * tests coverage improvement

* FIXES
   * update of a bunch of unit tests according to other packages evolutions
   * STICS versions management and XML files upgrade

* DATA
   * examples, templates files for different files format added for the latest
   STICS version 10.1.0
   * versions meta-data update
   

# SticsRFiles 1.1.3 _2023-07-10_

* FUNCTIONS
   * New function for generating the new_travail.usm STICS file
   * New function for generating weather data files (climat.txt)
   * New unit tests added

* FIXES
   * get_param_value: forcing to get a character returned value
   * get_var_info: for variables names vector
   * get_param_txt: removed global variables use, looking for parameters names
    including parentheses 
   * code formatting, comments, file names, variables names, function names,...
   * package namespace use in packages functions calls
   * pattern for replacing parameters names were are not consistent 
     between XML files and inputs.csv
   * pattern for selecting parameters in Excel files, filtering parameter 
     starting with "code_"
   * renaming plant initialisation parameters according to 
     crop number tag (Crop1, Crop2)
   * tidyselect and filter syntax
   * homogenous naming of JavaSTICS and STICS

* DATA
   * added: meta-data and xsl files for converting XML files to 
     text files for STICS
   * examples files for different files format used in the package are now
     distributed as zip files for lightening the extdata directory
   

# SticsRFiles 1.1.2 _2022-12-16_

* FIX:
   * Fix for cff file generation using a GitHub action
   
   
# SticsRFiles 1.1.1 _2022-12-08_

* FUNCTIONS
   * Changes in plant files upgrading function according to a fix done in JavaSTICS 1.5.1 for plant files
   (without any impact on R packages functioning)

* FIXES:
   * Little compatibility fix in STICS plant files upgrading function 
   

# SticsRFiles 1.1.0 _2022-10-28_

Following the changes done in the previous release 1.0.0, related to the adaptation to the new versions 10.0 of STICS and 1.5.0 of JavaSTICS.

* FUNCTIONS:
  * Completion of the upgrade function for converting plant files from STICS 9.x versions to the latest STICS version 10.0

* FIXES:
  * Getting parameters values attached to a formalism
  * Parameters with no values are now handled (*_tec.xml files) with NA values
  * Some fixes about use of XML objects or lists of (kind, content) in tests


# SticsRFiles 1.0.0 _2022-07-22_

The main changes are mainly related to the adaptation to the new versions 10.0 of STICS  
and 1.5.0 of JavaSTICS, to homogenization of arguments names in functions and to fixes.

* FUNCTIONS:
  * New set of functions for upgrading XML files of STICS version 9.2 to STICS version 10.0 (either each kind of XML file or a whole workspace) 
  * Homogenization of arguments names in functions
  * Some functions have been moved from the SticsOnR package into SticsRFiles
  * Improvement of functions dedicated to java/JavaSTICS command line use, added checks

* FIXES:
  * Extraction of vectorized parameters from _tec.xml and _ini.xml files
  * Functions for getting and setting parameters values in STICS input text files:
    * parameters of _ini.txt files now taken into account
    * taking into account special characters in parameter names ("_", "(", ")")
    * get function now returning result in an homogenized format (list of parameters) whatever is the value of argument param
  * Reading STICS daily outputs: NA or missing values now handled 
  * Whitespaces now taken into account in dir/file path
  
* DOC:
  * New vignette on functions for upgrading XML files of STICS version 9.2 to version 10.0 format
  * Adaptation of the vignettes to STICS version 10.0
  * SticsRFiles tutorial moved into SticsRPacks tutorial

* DATA:
  New files for STICS 10.0
    * XML example files added
    * Data set of text parameters files 
    * XL file example for generating XML files
  
* TESTS:
  * Adaptation of existing tests to STICS 10.0
  * New tests for XML files generation
  
  
  

# SticsRFiles 0.4.2 _2021-10-07_

* Prepared templates for STICS V10.

* FIXES:

  * Bugs fixed for generation of soil, ini and tec XML files from excel files.

  * Fixed reading of observation and simulation results files in case of USM selection (arg. usm_name)

  * tec file of associated plant no more searched in case of a single plant in gen_usms_xml2txt

* DOC:

  * Completed doc for generation of XML files from excel files.

  * Updated general doc.




# SticsRFiles 0.4.1 _2021-07-02_

## Changes

  * FIX: force_param_values() did not work for parameters which names included parenthesis

  * Added dependency to dplyr >= 1.0.0 due to use of relocate function

  * Typos in doc

# SticsRFiles 0.4.0 (2021-06-22)

## General changes

  * DESCRIPTION / NAMESPACE: updates linked to new functions
    
  * Tests 
    * updates, new ones added (get_obs, get_sim) 

  * Functions:
    * new: get_lai_forcing, check_usms_files
    * help updates
    * help reference site update (gen_obs function)

  * Files:
    * some useless files removed 
    * new STICS dirs added (needed by tests)
    * fix: example dirs names in inst
    
  * Tutorial: minor fix for use-xml-files (working dir creation)
    
## Specific changes
  * get_sim / get_obs / get_file / get_file_ 
    refactoring
    added some controls
	now taking into account a usms file path in functions
	fixed issues related to intercrops data loading
	added data.frame columns detection for filtering data on dates

  * functions for manipulating xml files 
    taking into account a new kind of STICS parameter dependency (for plant and soil layers dependent parameters)
    
  * set_param_txt: fix for setting values for varietal parameters
    
  * gen_obs: fixes 
    generating multiple files from a sheet
    generating files from a single observed variable
  
  * gen_usms_xml2txt
    now compatible with lai forcing
    added optional argument for checking files
    now removing files before each files generation



# SticsRFiles 0.3.0 _2021-04-15_

## General changes

  * DESCRIPTION
    * Imports: lifecycle

  * Tests 
    * new ones added for : gen_tec_xml, gen_ini_xml

  * Functions:
    * Now managing functions life cycle

  * Files:
    * useless XML template files have been removed  
    
## Specific changes
  * get_sim: new function replacing get_daily_results
  
  * get_daily_results: has been set to deprecated (a warning is displayed)
  
  * set_param_value: changed error output to warning if any parameter name does not exist
  
  * gen_tec_xml: now taking into account specific parameters linked to cut crops 
  management
  
  * gen_ini_xml: fix for crop identifiers management (plant 1 or 2)
  
  * get_usms_list: changed error output to warning  when the usms file does not exist
  
  * force_param_value: now uses either parameters names including () or _ for indexed ones 
  (for example lai(n) or lai_n)
  
  * get_param_xml, set_param_xml: checks have been added for detecting parameters 
  duplicates in tec files



# SticsRFiles 0.2.0 _2021-01-15_

## General changes

* DESCRIPTION
  * Suggests: formatR, readxl
  * added missing packages: curl, tidyr, tibble
  * license changed to License for CeCILL-C
  * Travis configuration removed

* README:
  * Travis badge removed
  * GitHub Actions R-CMD-check badge added
  
* Functions:
  * useless functions have been removed
  * some functions have been renamed or generalized

* Tests 
  * new ones added for : get_var_info, get_obs, get_daily_results, 
  force_param_values 
  * test-get*: now using get_examples_path function, some fixes

* Documentation
  * vignettes and tutorial: anchor_sections set to FALSE, fixes on warnings and errors, using paged html tables
  * functions help: content fixes and completion (parameters, examples, setting examples to dontrun, ...)

* Fixes
  * exported/unexported functions list
  * CRAN issues for checks

* Automatic testing: 
  * Github Actions added for checks
  * Travis checks removed
  * SticsRTests Travis tests triggering removed



## Specific changes
* get_obs, get_daily_results: removed useless columns from return, simplification 

* download_usm_xl, download_usm_csv: added verbose argument for masking warnings/displays, added overwriting case

* read_params_table : new function for getting parameters tables either from csv files or excel files sheets

* get_param_txt: added varieties management, earlier return when param is null, catching names ending with numerical indices, get varietal parameter, reading now parameters attached to several layers or fertilizations, added examples files for STICS version 8.5

* Manipulating_Stics_XML_files.Rmd: using XML file for the latest model version for displaying files fragments as examples

* get_stics_versions_compat, manage_stics_versions.R: added NA management in data

* get_file: added plant names in output (for obs and daily data), fixed issue and tests added for intercropping case

* get_daily_results: renamed class attribute to cropr_simulation, use of tibble::as_tibble(), mixed calculation using get_plants_nb, always returns a list, added optional usm_name arg, uses now get_plant_name instead, added javastics_path argument if plant files are located in it, dual variables name syntax (i.e. lai_n or lai(n) for example), taking into account intercrops management

* functions refactoring for getting observations and daily outputs

* get_var_info: now using partial matching on *name* column also

* static_help: new function for generating html files from package functions help, some fixes and updates for vignettes

* new function for downloading csv files from examples

* new functions for managing model versions information and examples

* get_obs* functions: fixes and optimization, added associated data and tests

* force_param_values: fix for one line tibble

* set_param_xml: output dir checking

* force_param_values: new function for forcing model parameters

* get_report_results: new function for extracting data from reports files

* get_param_info: added using of a specific XML file, attribute for STICS version, unique for parameters names list, building a filter before dplyr::filter 
* functions added to manage STICS versions data

* get_obs, get_plant_name: using usms.xml file outside of a STICS workspace, parsing mixed crops, file checking,usms_list renamed as usm_name

* gen_*_xml functions: fix for usms name column

* get_plant_name: getting now plant files list outside mapply call

* get_param_value: returns a named list even if only one parameter, fix for param_name nb > 1, without parameters names in args;returned  for each file

* xmlDocument class: fix for checking ids validity

* removed useless scripts for generating XML files

* get_plants_nb: is now an interface to get either plants number from an xml file or a text file, returning an usms named vector

* download_data: fix select
 
* gen_usms_xml2txt: silent error when blanks in workspace_path, fix javastics workspace use,

* get_stics_versions_compat: all data as character, study_case_1 added 

* gen_usms_obs: added usms_list

* All functions code and help now using the get_examples_path function

* get_examples_path: new function for getting files examples according to their type (txt, XML, csv,...) and embedded STICS versions data

* gen_varmod: forcing to add variable not in STICS output variables for a given version, now checks if a variable exists, dual variable syntax (i.e. lai_n or lai(n))

* set_param_txt: set value per soil layer, per variety

* get_usms_files: taking into account 2 plant files folders (but without duplicates checks), taking into account varieties management

* get_usms_list: returns now a character vector of usms names

* get_param_xml: without parameters names in args;returned for each file, 
name of the XML file added to the output



# SticsRFiles 0.1.0.9004 _2020-02-03_

* Added a `NEWS.md` file to track changes to the package.
