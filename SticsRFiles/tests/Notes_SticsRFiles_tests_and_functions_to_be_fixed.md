# SticsRFiles tests and functions to be fixed or modified.

## Notes about functions evolutions...

* upgrade_usms.xml    
take into account that usms data may contain or not **fobs** fields.  
* get_usms_list    
add select key: i.e. get all usms names
with the same soil, plant 1,...
* set_param_txt: not using get_param_txt to load parameters
replace values in the list and rewrite the txt file => new function playing with the list!
* get_tec_txt: 
  * not returning the same object either giving no param names as input nor giving a names vector ?
  * the function should give results for multiple parameters names!
  * is it possible to make compatible the reading of the content of the tec file whatever the version is (at the moment 9.2 (list filling is hard coded) vs 10.0?
* how to generalize tests to make them passing over STICS versions knowing that variables, parameters and values may change over versions. Not only a question of functions defects.  

## <u>test-force_param_values.R</u>: TO FIX
  * tests : 
    * keeping V9.2 for the moment 
    * force_param_value: problem detected
       generation of *param.sti* and *new_travail.usm* in
       inst/extdata/txt/V10.0 and not in the given workspace
       "/home/plecharpent/R/x86_64-pc-linux-gnu-library/3.6/SticsRFiles/extdata/txt/V10.0"
       got from get_example_path("txt) ?

  * functions:
    * to fix: for version 10.0
       * **get_param_txt**
       * **get_tec_txt**: **IN PROGRESS**

## <u>test-set_get_param_txt.R</u> TO FIX
* functions:
  * same problem with get_param_txt and get_tec_txt
  * set_param_txt: fix must BE EVALUATED
       
## <u>test-gen_ini_xml.R</u>: OK
  * functions  
    * gen_ini_xml: OK
    * get_param_xml: OK
  * added param names management according to the version    
  for getting the right names according to a version
  * **tests failing with V10.0**   
because the xl file does not contain all parameters existing in the xml file (just replacing param values to be overloaded in template)   
**/!\ Fixed filtering only existing xl parameters in xml parameters**


## <u>test-gen_tec_xml.R</u> OK

  * functions
    * set_param_value: OK    
    error with engrais parameter considered as a scalar parameter   
    **/!\ Fixed adding suffix in xl file : engrais_1**


## <u>test-get_obs.R</u> OK

  * Data update    
      * creation of a obs/V10.0 as in obs/BASE changing usms.xml files adding fobs
  fields    
      * creation of a data set : study_case_intercrop for V10.0 on SticsRPacks/data


## <u>test-get_param_gen.R</u> OK


## <u>test-get_param_info.R</u> OK

* fix: tec file content for getting paramaters attached to codeclaircie which are now    
specific to intervention 
* minor changes in tests for taking into account a new parameters in V10.0
containing "lai"

## <u>test-get_param_ini.R</u> OK

parameters names adapted according to version >= 10.0

## <u>test-get_param_names_xml.R</u> OK

* functions:
  * **get_param_names_xml**

* problems when loading parameters with duplicated bounds
Warning messages:
1: In fix_dup_bounds(values, bounds_name, param_name) :
  Found different values in bound(s) 
 max 
for parameter profhum 
A single value has been selected.
 max: 150
2: In fix_dup_bounds(values, bounds_name, param_name) :
  Found different values in bound(s) 
 min, max 
for parameter cfes 
A single value has been selected.
 min: -10
max: 10

* how to avoid this bcse these warnings will be considered as errors in checks
> For the moment : added bounds = FALSE when calling get_param_names_xml
for soil and param_gen
**TODO: fix bounds in files !**

* to be treated: OK
  * Done: 
    * conditioning test execution on version num relatively to < 10
    * added tests adapted to > 10 



## <u>test-get_param_newform.R</u> OK

parameters names adapted according to version >= 10.0


## <u>test-get_param_plt.R</u> OK


## <u>test-get_param_sols.R</u> OK

* fix: commented miscanthus soil, fixed argi value for solcanne !,


## <u>test-get_param_sta.R</u> OK


## <u>test-get_param_tec.R</u> OK


## <u>test-get_param_usms.R</u> OK


## <u>test-get_sim.R</u> OK


## <u>test-get_usms_list.R</u> OK


## <u>test-get_var_info.R</u> OK

* functions:
  * **get_var_info**: Fixed   
    Not loading the correct columns of the outputs.csv file between the 9.1 and the following versions 9.2 and 10.0   
    The content of the file has changed between 9.1 and > 9.2
    9.1: name, definition, unit, type
    > 9.2: name, definition, unit, origin, type, daily vec., var indice
  * Fix:
    * columns index are defined according to version < 9.2 and >= 9.2
    * minor changes in test: on last and previous version, a variable name changed

## <u>test-keepattr.R</u> OK
Nothing to do with version



