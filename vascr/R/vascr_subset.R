#' Subset a vascr data set based on a number of factors
#'
#' @param data.df vascr data set to subset
#' @param time Specified times. Individual values in a list will be subset out. If vectors are present in the list, values between the two most extreme values will be returned.
#' @param unit Units to subset. These are checked for integrity against possible units and the dataset itself
#' @param well Wells to select
#' @param frequency Frequencies to include in the data set.
#' @param experiment Experiments to include in the data set. Can be addressed either by name, or by the numerical order that they were loaded into vascr_combine in
#' @param instrument Which instruments to include values from
#' @param subsample Frequency values should be sub-sampled to
#' @param sampleid List of ID's to be used. Sample names will be re-ordered accordingly for display.
#' @param sample Sample to subset
#' @param remove_na_value Should NA values be removed (default true)
#' @param remove_excluded Should excluded values be removed (default true)
#'
#' @return The subset dataset, based on the values selected
#' 
#' @importFrom dplyr as_tibble
#' @importFrom stringr str_count
#' 
#' @export
#'
#' @examples
#' vascr_subset(growth.df)
#' vascr_subset(growth.df, time = 40)
#' vascr_subset(growth.df, time = NULL)
#'  
#' vascr_subset(growth.df, unit = "Rb")
#' vascr_subset(growth.df, unit = "R")
#' vascr_subset(growth.df, well = "A1")
#' 
#' vascr_subset(growth.df, time = c(5,20))
vascr_subset = function(data.df, 
                        time = NULL, 
                        unit = NULL, 
                        well = NULL, 
                        frequency = NULL,
                        experiment = NULL,
                        instrument = NULL,
                        sampleid = NULL,
                        sample = NULL,
                        subsample = NULL,
                        remove_na_value =TRUE,
                        remove_excluded = TRUE)
{
  
  
  subset.df = data.df
  
  
  if(!"SampleID" %in% colnames(subset.df)) {subset.df$Excluded = "no"}
  
  if(isTRUE(remove_excluded)){
  subset.df = subset.df %>% dplyr::filter(.data$Excluded != "yes")
  }
  
  if(!"SampleID" %in% colnames(subset.df)) {subset.df$SampleID = 1}
  
  if(isTRUE(remove_na_value))
  {
    subset.df = subset(subset.df, !is.na(subset.df$Value))
  }
  
  # Subsample (this is the cheapest so let's do it first)
  if(!is.null(subsample))
  {
    subset.df = vascr_subsample(subset.df, subsample)
  }
  
  
  # Unit
  if(!is.null(unit))
  {
    units = vascr_find_unit(subset.df, unit)
    subset.df = subset(subset.df, subset.df$Unit %in% unique(units))
    subset.df$Unit = factor(subset.df$Unit, unique(units))
  }
  
  
  # Frequency
  if(!is.null(frequency))
  {
    if(typeof(subset.df$Frequency) != "double")
    {
      subset.df = subset.df %>% mutate(Frequency = as.double(.data$Frequency))
    }
    
    frequencies = vascr_find_frequency(subset.df, frequency)
    subset.df = subset(subset.df, subset.df$Frequency %in% frequencies)
  }
  
  
  # Well
  if(!is.null(well))
  {
    include = vector()
    exclude = vector()
    
    for(w in 1:length(well))
    {
      if(str_count(well[w], "\\-") == 1)
      {
        exclude = append(exclude, well[w])
      } else
      {
        include = append(include, well[w])
      }
    }
    
    include = vascr_find_well(subset.df, include)
    exclude = vascr_find_well(subset.df, exclude)
    
    if(length(include>0))
    {
    subset.df = subset(subset.df, subset.df$Well %in% include)
    } else
    {
    subset.df = subset(subset.df, !(subset.df$Well %in% exclude))
    }
    
  }
  
  
  
  # Time
  if(!is.null(time))
  {
    times = vascr_find_time(subset.df, time)
    subset.df = subset(subset.df, subset.df$Time %in% times)
  }
  
  # Experiment
  if(!is.null(experiment))
  {
    experiments = vascr_find_experiment(subset.df, experiment)
    subset.df = subset(subset.df, subset.df$Experiment %in% experiments)
  }
  
  # Instrument
  if(!is.null(instrument))
  {
    instruments = vascr_find_instrument(subset.df, instrument)
    subset.df = subset(subset.df, subset.df$Instrument %in% instruments)
  }
  
  # Sample
  
  if(!is.null(sampleid))
  {
    subset.df = vascr_subset_sampleid(subset.df, sampleid)
  }
  
  if(!is.null(sample))
  {
    samples = vascr_find_sample(subset.df, sample)
    subset.df = subset(subset.df, subset.df$Sample %in% samples) %>%
      mutate(Sample = factor(.data$Sample, samples))
      
  }
  
  
  # Check if there is still some data here, and if not sound a warning
  if(nrow(subset.df)==0)
  {
    vascr_notify("warning","No data returned from dataset subset. Check your frequencies, times and units are present in the dataset")
  }
  
  
  subset.df = as_tibble(subset.df)
  
  return(subset.df)
}

#' Sub setting function for sample IDs
#'
#' @param data.df the vascr dataset to subsample
#' @param samplelist the list of ids to test against
#'
#' @return A subset data frame
#' @noRd
#'
#' @examples
#' vascr_subset_sampleid(growth.df, c(3,4))
vascr_subset_sampleid = function (data.df, samplelist){
  
  # First subset the dataset
  subset.df = data.df %>% filter(.data$SampleID %in% samplelist)
  
  id_list = subset.df %>% select("Sample", "SampleID") %>% distinct()
  
  id_list$order = match(id_list$SampleID,samplelist)
  
  id_list = id_list %>% arrange(.data$order)
  
  subset.df = subset.df %>% mutate(Sample = factor(.data$Sample, id_list$Sample))
  
  return(subset.df)
  
}


#' Exclude samples from a vascr dataset
#' 
#' @param data.df the vascr data set to exclude things from
#' @param well wells to exclude
#' @param experiment experiments to exclude
#' @param sampleid sampleID (or vector or sampleIDs) to exclude from analysis
#'
#' @return A smaller vascr dataset
#' 
#' @export
#'
#' @examples
#' vascr_exclude(growth.df, c("A01", "E01"))
#' vascr_exclude(growth.df, sampleid = 1)
#' 
#' 
vascr_exclude = function(data.df, well = NULL, experiment = NULL, sampleid = NULL){
  # data.df = data.df %>% filter(!.data$Well %in% well & !.data$Experiment %in% experiment)
  
  well = vascr_find_well(data.df, well)
  experiment = vascr_find_experiment(data.df, experiment)
  sampleid = vascr_find_sampleid(data.df, sampleid)
  
  data.df = data.df %>% filter(!((.data$Well %in% well) & (.data$Experiment %in% experiment) & (.data$SampleID %in% sampleid)))
  
  return(data.df %>% as_tibble())
}



#' Rename a sample in a vascr dataset
#' 
#' Renames samples in a vascr dataset, either replacing the whole sample or parts of the string.
#'
#' @param data.df Vascr dataset to update
#' @param change_list List of vectors containing pairs of search and replacement terms to replace
#' @param partial TRUE or FALSE, defines if partial matches should be changed
#' @param escape TRUE or FALSE, whether to escape special characters passed into the function
#'
#' @returns An updated vascr data frame
#' 
#' @importFrom stringr str_escape str_replace_all
#' @importFrom dplyr select distinct arrange mutate left_join
#' @importFrom foreach foreach `%do%`
#' 
#' @export
#'
#' @examples
#' to_rename = growth.df %>% vascr_subset(sample = c("0 cells","20,000 cells", "10,000 cells"))
#' 
#' to_rename$Sample %>% unique()
#' 
#' renamed = vascr_edit_sample(to_rename, change_list = list(c("0_cells", "Cell Free")))
#' print(renamed$Sample %>% unique())
vascr_edit_sample = function(data.df, change_list, partial = TRUE, escape = TRUE){
  
  mini = data.df %>% select("SampleID", "Sample") %>% 
         distinct() %>% 
         arrange(.data$Sample) %>%
         mutate(Original_Sample = .data$Sample)
  
  #create change to keep CRAN happy
  change = NA
  
  foreach (change = change_list) %do% {
    
    if(length(change) != 2)
    {
      vascr_notify("error", "Vector length for replacement incorrect: {change}")
    }
    
    change = unlist(change)
    
    old = change[1]
    new_val = change[2]
  
    
    if(isTRUE(partial)){
      old = old %>% str_escape()
      mini = mini %>% mutate(Sample = str_replace_all(.data$Sample, old, new_val))
      
    } else{
      old = vascr_find(data.df, "Sample", old)
      
     mini = mini %>%
         mutate(Sample = ifelse(is.na(.data$Sample), "NA", as.character(.data$Sample))) %>%
         mutate(Sample = ifelse(as.character(.data$Sample) == old, new_val, as.character(.data$Sample)))
      }
  }
  
  # mini %>% filter(Sample != Original_Sample)
  
  mini  = mini %>% mutate(Sample = factor(.data$Sample, unique(.data$Sample)))
  
  
  return(data.df %>% select(-"Sample") %>% left_join(mini, by = c("SampleID")))
  
}



