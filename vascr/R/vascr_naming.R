#' Import an impedance datafile to vascr
#'
#' @param instrument Instrument to import from, either ECIS, xCELLigence or cellZscope
#' @param raw Path to raw data file
#' @param modeled Path to modeled data file from manufacturer's software
#' @param experiment Name for the experiment being imported
#' @param shear True or False, is a shear plate used, as these have a different electrode layout. Defaults to False.
#' 
#' @importFrom stringr str_to_lower
#'
#' @returns A vascr dataset for subsequent analysis
#' 
#' @export
#'
#' @examples
#' \donttest{
#' 
#' # ECIS
#' raw = system.file('extdata/instruments/ecis_TimeResample.abp', package = 'vascr')
#' modeled = system.file('extdata/instruments/ecis_TimeResample_RbA.csv', package = 'vascr')
#' vascr_import("ECIS", raw, modeled, "ECIS_Data")
#' 
#' # xCELLigence
#' raw = system.file('extdata/instruments/xcell.plt', package = 'vascr')
#' # No modeling for this system
#' vascr_import("xCELLigence", raw, experiment = "xCELLigence")
#' 
#' # cellZscope
#' model = system.file("extdata/instruments/zscopemodel.txt", package = "vascr")
#' raw = system.file("extdata/instruments/zscoperaw.txt", package = "vascr")
#' vascr_import("cellzscope", raw, model, "cellZscope")
#' 
#' #' # ScioSpec
#' raw = system.file("extdata/instruments/ScioSpec", package = "vascr")
#' vascr_import("sciospec", raw, model, "ScioSpec")
#' 
#' }
vascr_import = function(instrument = NULL, raw = NULL, modeled = NULL, experiment = NULL, shear = FALSE){
  
  instrument = str_to_lower(instrument)
  
  if(is.null(experiment)){experiment == ""}
  
  if(instrument == "ecis")
  {
    return(ecis_import(raw, modeled, experiment))
  } else if (instrument == "xcelligence")
  {
    return(import_xcelligence(raw, experiment))
  } else if (instrument == "cellzscope")
  {
    return(cellzscope_import(raw, modeled, experiment))
  } else if (instrument == "sciospec")
  {
    return(import_sciospec(raw, experiment = experiment, shear = shear))
  }
  else{
    vascr_notify("error", "Data didn't import, wrong instument typed")
  }
  
  
}


#' Create a blank vascr data frame
#'
#' @returns A blank vascr data frame
#' 
#' @noRd
#'
#' @examples
#' vascr_blank_df()
#' 
vascr_blank_df = function(){
  vascr::growth.df %>% filter(FALSE) %>% mutate(Excluded = FALSE) %>% vascr_remove_metadata()
}


#' Import a vascr platemap
#'
#' @param file_content a tibble or file path to import
#'
#' @return the imported plate map, fully lengthened to remove duplication
#' 
#' @importFrom dplyr cur_group_id mutate ungroup
#' @importFrom tidyr separate_rows
#' 
#' @noRd
#' 
#' @importFrom tidyr separate_rows
#' @importFrom dplyr relocate group_by_all
#'
#' @examples
#'map_1 = tribble(~Row, ~Column, ~Sample,
#'                "A", "1 2 3", "10 nM Treatment 1 + 1nm water",
#'                "B", "1 2 3", "100 nM Treatment 1 + 1nm water",
#'                "C", "4 5 6", "10 nM Treatment 2 + 1nm water",
#'              "D", "1 2 3", "100 nM Treatment 2 + 1nm water")
#'              
#'vascr_import_map(map_1)
#'
#'lookup = tribble(~Row, ~Column, ~Sample,
#'           "A B C D E F G H", "2", "NZB11 + Media")
#'           
#'           
#'vascr_import_map(lookup)
#'
#'lookup = tribble(~Well, ~Sample,
#'           "A01 A02", "NZB11 + Media")
#'           
#'           
#'vascr_import_map(lookup)
#'
#'lookup = system.file('extdata/instruments/eciskey.csv', package = 'vascr')
#'lookupmap = vascr_import_map(lookup)
vascr_import_map = function(lookup) {
  
  if(is.character(lookup))
  {
    file_content = read.csv(lookup)
  } else
  {
    file_content = lookup
  }
  
  # Check for duplicate sample names
  # vascr_check_duplicate(file_content, "Sample")
  
  # Add a Sample ID automatically if not already set
  if(!"SampleID" %in% colnames(file_content)) {
    if("Well" %in% colnames(file_content)) {
      file_content = file_content %>% group_by(across(-'Well')) %>% mutate(SampleID = cur_group_id())
    } else {
      file_content = file_content %>% group_by(across(c(-'Column', -'Row'))) %>% mutate(SampleID = cur_group_id())
    }
  } else { # If samples are set, check for duplicate ID rows
    #vascr_check_duplicate(file_content, c("Experiment","SampleID"))
  }
  
  # Lengthen out imported names
  if("Well" %in% colnames(file_content)) {
    file_map = file_content %>%
      separate_rows("Well", sep = " ") %>%
      mutate(Well = vascr_standardise_wells(.data$Well))
  }else if("Row" %in% colnames(file_content) & "Column" %in% colnames(file_content)){
    file_map = file_content %>% 
      mutate(Row = trimws(.data$Row), Column = trimws(.data$Column)) %>%
      separate_rows("Row", sep = " ") %>%
      separate_rows("Column", sep = " ") %>%
      mutate(Well = paste(.data$Row, .data$Column, sep = ""), Well = vascr_standardise_wells(.data$Well)) %>%
      mutate(Row = NULL, Column = NULL)
  }else {
    vascr_notify("error","Either `Row` and `Column' or `Well` must be specified in the input file")
  }
  
  #vascr_check_duplicate(file_map, "Well") # Check if each well is defined more than once
  
  if(!"Sample" %in% colnames(file_map))
  {
    file_map = vascr_implode(file_map %>% ungroup())
  }
  
  file_map = ungroup(file_map)
  
  return(file_map)
}


#' Apply a map to a vascr dataset
#'
#' @param data.df the dataset to apply to
#' @param map the dataset to apply
#'
#' @return a named vascr dataset
#' 
#' @export
#'
#' @examples
#' lookup = system.file('extdata/instruments/eciskey.csv', package = 'vascr')
#' vascr_apply_map(data.df = growth.df, map = lookup)
#' 
#' vascr_apply_map(growth.df %>% vascr_subset(well = c("A1")), lookup)
#' 
vascr_apply_map = function(data.df, map){
  
  map.df = vascr_import_map(map)
  
  # print("mapping")
  # print(map.df)
  
  data.df  = data.df %>% vascr_remove_cols(c("Sample", "SampleID"))
  
  datwells = unique(data.df$Well)
  mapwells = unique(map.df$Well)
  
  if(any(!datwells %in% mapwells))
  {
    dw = datwells[!datwells %in% mapwells] %>% paste(collapse = " ")
    vascr_notify("warning", glue("Wells found in imported data but not map: {dw}"))
  }
  
  if(any(!mapwells %in% datwells))
  {
    dw = datwells[!mapwells %in% datwells] %>% paste(collapse = " ")
    vascr_notify("warning", glue("Wells found in map but not imported data: {dw}"))
  }
  
  toreturn = data.df %>% left_join(map.df)
  
  toreturn = toreturn %>% mutate(Experiment = as.factor(.data$Experiment)) %>%
            mutate(Sample = as.factor(.data$Sample))
  
  toreturn = as_tibble(toreturn)
  
  return(toreturn)
}


#' Print out a template of a vascr data frame for mapping
#' 
#' @returns A blank dataframe for further use
#' 
#' @noRd
#'
#' @examples
#' vascr_map_template()
#' 
vascr_map_template = function(){
  tribble(~`Experiment`, ~`Well`, ~`Sample`, ~`SampleID`, ~`Excluded`)
}


#' Regenerate a map back from a vascr datafile
#'
#' @param data.df The dataset to regenerate the map used from
#' 
#' @importFrom dplyr select distinct group_by summarise as_tibble
#' @importFrom rlang .data
#'
#' @returns A map for a vascr dataset
#' 
#' @noRd
#' 
#' @examples
#' vascr_regenerate_map(growth.df)
#' 
vascr_regenerate_map = function(data.df){

  data.df %>% select("Experiment", "Well", "Sample", "SampleID", "Excluded") %>%
    distinct() %>%
    group_by(.data$Experiment, .data$Sample, .data$SampleID, .data$Excluded) %>%
    summarise(Well = paste(.data$Well, collapse = " ")) %>%
    as_tibble()
  
  
}


#' Implode individual samples from a vascr dataset
#'
#' @param data.df A vascr dataset to be imploded
#' @param cols  The columns to implode
#'
#' @return A vascr dataset with individual wells imploded
#' 
#' @export
#' 
#' @importFrom dplyr bind_rows as_tibble all_of
#' @importFrom foreach foreach `%do%`
#'
#' @examples
#' vascr_implode(growth.df)
#' 
vascr_implode = function(data.df, cols = NULL){
  
  if(is.null(cols)) {
  toimplode = vascr_cols(data.df, set = "exploded")
  }else{
    toimplode = vascr_find_col(data.df, cols)
  }
  
  
  
  smallframe = data.df %>% select(all_of(c(toimplode, "SampleID")))  %>%
    distinct()
  
  to_merge = toimplode
  
  smallframe
  
  r = 0
  
  names = foreach(r = c(1:nrow(smallframe))) %do%
    {
      row = smallframe[r,]
      
        all_cols = foreach (c = to_merge) %do%
        {
          if(as.character(row[,c]) %in% c("NA", "0") || is.na(row[,c])){
            return()
          } else {
            paste(row[,c], c)
          }
        }
        
        row$Sample = paste(unlist(all_cols), collapse = " + ")
        return(row)
    }
  
  newnames = bind_rows(names)
  
  if("Sample" %in% colnames(data.df))
  {
    data.df = data.df %>% select(-"Sample")
  }
  
  newnames %>%
    ungroup() %>%
    select("SampleID", "Sample") %>%
    left_join(data.df, by = "SampleID") %>%
    as_tibble()
  
}


#' Separate names in a vascr data frame
#'
#' @param data.df the dataset to separate
#'
#' @return a separated vascr dataset, with additional columns for each variable
#' 
#' @importFrom dplyr select distinct mutate left_join join_by as_tibble
#' @importFrom tidyr separate_longer_delim separate_wider_delim pivot_wider
#' @importFrom stringr regex
#' 
#' @export
#'
#' @examples
#' vascr_explode(growth.df)
vascr_explode = function(data.df) {
  
# Check an appropriate data set has been created
  vascr_check_col_exists(data.df, "SampleID")
  vascr_check_col_exists(data.df, "Sample")
  
  # Check what cols exist
  
  core_data.df = data.df %>% select(vascr_cols(data.df)) %>% as_tibble()
  
# Break out the data
  distinct_samples = core_data.df %>%
  select("SampleID", "Sample") %>%
  distinct() 

# Check there isn't duplication in Sample or SampleID pairs as this may muck things up later
  #vascr_check_duplicate(distinct_samples, "SampleID")
  #vascr_check_duplicate(distinct_samples, "Sample")

# Generate the expanded cols, based on SampleID as the unique key
samples = distinct_samples %>%
  separate_longer_delim("Sample", " + ") %>%
  separate_wider_delim("Sample", delim = regex("(_| )"), names = c("value", "name"), too_many = "merge", cols_remove = FALSE) %>%
  mutate(name = trimws(.data$name)) %>%
  distinct() %>%
  pivot_wider(names_from = "name", id_cols = "SampleID") %>%
  mutate(`NA` = NULL)

# Attach the full data set back onto the data frame
  fulldata = data.df %>% left_join(samples, by = join_by("SampleID")) %>% as_tibble()

return(fulldata)

}


#' Edit a sample name in a vascr dataframe
#'
#' @param data.df The data set to edit
#' @param to_remove The sample to remove
#' @param to_add The sample to replace with
#' 
#' @importFrom dplyr mutate left_join
#' @importFrom tidyr as_tibble 
#'
#' @returns An edited vascr dataset
#' 
#' @export
#'
#' @examples
#' vascr_edit_name(growth.df,"HCMEC D3", "HCMEC/D3")
vascr_edit_name = function(data.df, to_remove, to_add = ""){
  
  data.df %>% select("Sample") %>% distinct() %>%
    mutate(Clean_Sample = str_replace_all(.data$Sample, to_remove, to_add)) %>%
    mutate(Clean_Sample = ifelse(.data$Clean_Sample == "", "Vehicle", .data$Clean_Sample)) %>%
    right_join(data.df, by = c("Sample" = "Sample")) %>%
    mutate(Sample = .data$Clean_Sample, Clean_Sample = NULL) %>%
    as_tibble()
  
}


#' #' Edit a sample name in a vascr dataframe
#' #'
#' #' @param data.df The data set to edit
#' #' @param to_remove The sample to remove
#' #' @param to_add The sample to replace with
#' #' 
#' #' @importFrom dplyr mutate left_join
#' #' @importFrom tidyr as_tibble 
#' #'
#' #' @returns An edited vascr dataset
#' #' 
#' #' @export
#' #'
#' #' @examples
#' #' changed = vascr_change_name(growth.df, 1 , "Sample One")
#' #' 
#' #' 
#' vascr_change_name = function(data.df, sampleid, new_name = ""){
#'   
#'   data.df %>% select("Sample", "SampleID") %>% distinct() %>%
#'     mutate(Clean_Sample = ifelse(.data$SampleID == sampleid, new_name, .data$Clean_Sample)) %>%
#'     right_join(data.df, by = c("Sample" = "Sample")) %>%
#'     mutate(Sample = .data$Clean_Sample, Clean_Sample = NULL) %>%
#'     as_tibble()
#'   
#' }



#' Reassign sample ID's to a dataset
#'
#' @param data.df The dataset to regenerate IDs for
#' 
#' @noRd
#'
#' @returns A dataset with sample ID's reassigned based on sample names
#'
#' @examples
#' growth.df %>% vascr_assign_sampleid()
#' 
vascr_assign_sampleid = function(data.df, force = FALSE){
  
  if(!("SampleID" %in% colnames(data.df)) | isTRUE(force))
  {
  
  data.df = data.df %>% group_by(.data$Sample) %>%
    mutate(SampleID = cur_group_id()) %>%
    as_tibble()
  }
  
  return(data.df)
  
}





