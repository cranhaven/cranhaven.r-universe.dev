#' Check for duplicates in a column of a data frame
#'
#' @param df the dataframe to check against
#' @param variable the col
#' 
#' 
#'@importFrom dplyr reframe group_by_at filter left_join
#' @importFrom utils capture.output
#' 
#' 
#' @return No return value, but throws a warning if duplicates are present
#' 
#' # Don't export as an internal convenience function
#' @noRd
#'
#' @examples
#' vascr_check_duplicate(df = growth.df %>% vascr_subset(well = c("A01", "A02")), col = "Well")
#' 
#' 
vascr_check_duplicate = function(df, col)
{
  tabulated = df %>%
    group_by_at(col) %>%
    reframe('Freq' = n())
  
  duplicated = tabulated %>%
    dplyr::filter(.data$Freq>1) %>%
    left_join(df, by = col)
  
  # Check that there are no duplicate wells in the table
  if(nrow(duplicated)>0){
    
    warn_text = paste(col, paste("[",unique(duplicated[[col]]), "] ", collapse = " ")," defined more than once \n") # Generate the string warning of duplicates
    
    vascr_notify("warning",paste0(c(warn_text,capture.output(duplicated)), collapse = "\n")) # Add the warning string to the beginning of a table showing the repeated data
    
  }
  return()
}

#' Test if a vascr dataset is interpolated
#'
#' @param data.df a vascr dataset
#'
#' @return the dataset, or a re sampled version of it if it was not originally normalized
#' 
#' # Don't export as this is an internal integrity check
#' @noRd
#'
#' @examples
#' vascr_force_resampled(growth.df)
#' vascr_force_resampled(growth_unresampled.df)
vascr_force_resampled = function(data.df)
{
  if(!vascr_check_resampled(data.df))
  {
    vascr_notify("warning","Data is not resampled, resampling to allow further analytics")
    data.df = vascr_resample_time(data.df)
  }
  
  return(data.df %>% as_tibble())
}

#' Check if a vascr data set is re sampled
#'
#' @param data.df The vascr data set to check if it has been re sampled
#' 
#' @importFrom rlang hash
#'
#' @return A boolean, TRUE if re sampled otherwise FALSE
#' 
#' # Internal function so don't export
#' @noRd
#'
#' @examples
#' vascr_check_resampled(growth.df)
#' vascr_check_resampled(growth_unresampled.df)
#' 
#' 
#' 
#' 
vascr_check_resampled = function(data.df)
{
  
  experiment_times = data.df %>% 
    group_by(.data$Unit, .data$Experiment, .data$Well, .data$Instrument, .data$Sample) %>% 
    summarise(hash = rlang::hash(.data$Time)) %>% 
    group_by(.data$Unit) %>% 
    reframe(n = length(unique(hash)))
  
  samples = mean(experiment_times$n) == 1
  
  return(samples)
  
}


#' Check if a column exists
#'
#' @param df the data frame to test
#' @param col the column to test
#'
#' @return a boolean, true if it exists otherwise false
#' 
#' @noRd
#'
#' @examples
#' vascr_check_col_exists(growth.df, "Well")
#' 
vascr_check_col_exists = function(df, col){
  if(!c(col) %in% colnames(df))
  {
    vascr_notify("warning",paste(col, "not found in dataframe. Please check"))
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}




#' Confirm if a dataset is complete with replicates accross experiments
#'
#' @param growth.df vascr dataset to confirm how replicates are repeated
#'
#' @returns true if all square, else a dataframe describing the issues
#' 
#' @importFrom dplyr select distinct group_by reframe
#' @importFrom rlang is_null .data
#' 
#' @noRd
#'
#' @examples
#' d1 = growth.df %>% vascr_subset(sampleid = 3, experiment = c(1,2))
#' d2 = growth.df %>% vascr_subset(sampleid = c(1,2))
#' data.df = rbind(d1,d2)
#' 
#' vascr_check_replicates(growth.df)
#' 
vascr_check_replicates = function(data.df) {
  
  replication = data.df %>% select("SampleID", "Sample", "Experiment") %>%
    distinct() %>%
    group_by(.data$Sample, .data$SampleID) %>%
    reframe(count = n(), experiments = paste(.data$Experiment, collapse = " | "))
  
  if(nrow(replication)>0 ){
    return (replication)
  } else {
    return (TRUE)
  }
  

}


