params <-
list(javastics_path = "/tmp/JavaSTICS-1.5.2-STICS-10.2.0", workspace_path = "/tmp/JavaSTICS-1.5.2-STICS-10.2.0/example", 
    output_path = "/tmp/example", eval = FALSE)

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
#  library(SticsRFiles)

## ----include=FALSE------------------------------------------------------------
#  suppressWarnings(library(SticsRFiles))

## ----echo = FALSE, eval = TRUE------------------------------------------------
javastics_path <- params$javastics_path
workspace_path <- params$workspace_path
output_path <- params$output_path
chunk_eval <- params$eval
#java_cmd <- params$java_cmd
gen_usms_xml2txt <- SticsRFiles::gen_usms_xml2txt

## ----paths_def, eval = FALSE--------------------------------------------------
#  # Specifying the JavaSTICS folder
#  javastics_path <- "/path/to/JavaSTICS-1.5.2-STICS-10.2.0"
#  
#  # Specifying a workspace as a subfolder of JavaSTICS
#  workspace_path <- "example"
#  # or an absolute path to an external folder
#  # workspace_path <- "/path/to/javastics/workspace"
#  
#  # Specifying an output folder path
#  output_path <- "/path/to/output/folder"
#  

## ----convert_to_separate_1, eval = chunk_eval---------------------------------
#  ## Generating files for all the usms contained in the workspace
#  
#  # Into the workspace directory
#  gen_usms_xml2txt(javastics_path,
#                   workspace_path)
#  #> ℹ USM 'SugarCane' successfully created
#  #> ℹ USM 'potato' successfully created
#  #> ℹ USM 'banana' successfully created
#  #> ℹ USM 'sorghum' successfully created
#  #> ℹ USM 'barley' successfully created
#  #> ℹ USM 'sugarbeet' successfully created
#  #> ℹ USM 'wheat' successfully created
#  #> ℹ USM 'maize' successfully created
#  #> ℹ USM 'soybean' successfully created
#  #> ℹ USM 'lettuce' successfully created
#  #> ℹ USM 'tomato' successfully created
#  #> ℹ USM 'DurumWheat' successfully created
#  #> ℹ USM 'rapeseed' successfully created
#  #> ℹ USM 'sunflower' successfully created
#  #> ℹ USM 'grass' successfully created
#  #> ℹ USM 'BareSoil' successfully created
#  #> ! Obs file not found for USM
#  #> "demo_Wheat1": '/tmp/JavaSTICS-1.5.2-STICS-10.2.0/example/demo_Wheat1.obs'
#  #> ℹ USM 'demo_Wheat1' successfully created
#  #> ! Obs file not found for USM
#  #> "demo_BareSoil2": '/tmp/JavaSTICS-1.5.2-STICS-10.2.0/example/demo_BareSoil2.obs'
#  #> ℹ USM 'demo_BareSoil2' successfully created
#  #> ! Obs file not found for USM
#  #> "demo_maize3": '/tmp/JavaSTICS-1.5.2-STICS-10.2.0/example/demo_maize3.obs'
#  #> ℹ USM 'demo_maize3' successfully created
#  #> ! Obs file not found for USM
#  #> "DurumWheat_4years": '/tmp/JavaSTICS-1.5.2-STICS-10.2.0/example/DurumWheat_4years.obs'
#  #> ℹ USM 'DurumWheat_4years' successfully created
#  #> ! Obs file not found for USM
#  #> "maize_4years": '/tmp/JavaSTICS-1.5.2-STICS-10.2.0/example/maize_4years.obs'
#  #> ℹ USM 'maize_4years' successfully created
#  #> ℹ USM 'strawberry' successfully created
#  #> ℹ USM 'pea' successfully created
#  #> ℹ USM 'vine' successfully created
#  #> ℹ USM 'fescue' successfully created
#  #> ℹ USM 'flax' successfully created
#  #> ! Obs file not found for USM
#  #> "intercrop_pea_barley": '/tmp/JavaSTICS-1.5.2-STICS-10.2.0/example/intercrop_pea_barley.obs'
#  #> ℹ USM 'intercrop_pea_barley' successfully created
#  #> ℹ USM 'timothy' successfully created
#  #> ℹ USM 'DurumWheat_snow' successfully created
#  #> ℹ USM 'Turmeric' successfully created
#  #> ℹ USM 'cc_BristleOat' successfully created
#  #> ℹ USM 'cc_mustard' successfully created
#  #> ℹ USM 'cc_ItalianRyegrass' successfully created
#  #> ℹ USM 'cc_vetch' successfully created
#  #> ℹ USM 'cc_CrimsonClover' successfully created
#  #> ℹ USM 'proto_rice' successfully created
#  

## ----convert_to_separate_2, eval = chunk_eval---------------------------------
#  
#  # Into a specific output folder, with verbose mode turned off
#  gen_usms_xml2txt(javastics_path,
#                   workspace_path,
#                   out_dir = output_path,
#                   verbose = FALSE)
#  
#  
#  ## Generating files for a subset of usms
#  # Into the workspace directory
#  gen_usms_xml2txt(javastics_path,
#                   workspace_path,
#                   usm = c("banana", "wheat"))
#  #> ℹ USM 'banana' successfully created
#  #> ℹ USM 'wheat' successfully created
#  
#  # Into a specific folder
#  gen_usms_xml2txt(javastics_path,
#                   workspace_path,
#                   usm = c("banana", "wheat"),
#                   out_dir = output_path)
#  #> ℹ USM 'banana' successfully created
#  #> ℹ USM 'wheat' successfully created
#  

## ----convert_to_separate_3, eval = chunk_eval---------------------------------
#  ## Getting returned information about files generation
#  gen_info <- gen_usms_xml2txt(javastics_path,
#                               workspace_path,
#                               usm = c("banana", "wheat"),
#                               out_dir = output_path)
#  #> ℹ USM 'banana' successfully created
#  #> ℹ USM 'wheat' successfully created
#  
#  
#  gen_info
#  #> $usms_path
#  #> [1] "/path/to/output/folder/banana"
#  #> [2] "/path/to/output/folder/wheat"
#  #>
#  #> $files
#  #>  [1] "climat.txt"      "param.sol"       "ficini.txt"      "ficplt1.txt"
#  #>  [5] "fictec1.txt"     "station.txt"     "new_travail.usm" "tempopar.sti"
#  #>  [9] "tempoparv6.sti"  "ficplt2.txt"     "fictec2.txt"
#  #>
#  #> $copy_status
#  #> [1] TRUE TRUE
#  #>
#  #> $obs_copy_status
#  #> [1] TRUE TRUE
#  #>
#  #> $lai_copy_status
#  #> [1] FALSE FALSE

## ----convert_to_one, eval = chunk_eval----------------------------------------
#  # Generating files directly in the workspace or a specific folder
#  # (no usm sub-folder)
#  # In this case the model files are overwritten at each gen_usms_xml2txt call !
#  # In the workspace
#  gen_usms_xml2txt(javastics_path,
#                   workspace_path,
#                   usm = "banana",
#                   dir_per_usm_flag = FALSE,
#                   verbose = FALSE)
#  
#  # In a specific folder
#  gen_usms_xml2txt(javastics_path,
#                   workspace_path,
#                   usm = "banana",
#                   out_dir = output_path,
#                   dir_per_usm_flag = FALSE,
#                   verbose = FALSE)

