#' Find vascr variables
#' 
#' These functions are utility functions that will detect if arguments are invalid, 
#' and attempt to repair them. Each type of variable has rules related to what values
#' are possible in a valid vascr dataset.
#'
#' @param data.df The vascr dataset to reference, will default to the growth.df dataset if not specified
#' @param paramater The parameter to search. Options are "Time", "Unit", "Well", "Frequency", "Sample", "Experiment", "SampleID" or "resampled"
#' @param value the value to look up
#'
#' @return The valid vascr dataset.
#' 
#' @export
#' 
#' @examples
#' 
#' vascr_find(growth.df, "Time")
#' vascr_find(growth.df, "Time", 66.97)
#' vascr_find(growth.df, "Time", NULL)
#' 
#' vascr_find(growth.df, "Unit")
#' vascr_find(growth.df, "Unit", "Rb")
#' vascr_find(growth.df, "Unit", NULL)
#' 
#' vascr_find(growth.df, "Well")
#' vascr_find(growth.df, "Well", "A1")
#' 
#' vascr_find(growth.df, "Sample")
#' vascr_find(growth.df, "Sample", "5000 cells")
#' 
#' vascr_find(growth.df, "Frequency")
#' vascr_find(growth.df, "Frequency", 4000)
#' 
#' vascr_find(growth.df, "Experiment")
#' vascr_find(growth.df, "Experiment", 1)
#' 
#' vascr_find(growth.df, "SampleID")
#' vascr_find(growth.df, "SampleID", 5)
#' 
#' vascr_find(growth.df, "resampled")
#' 
#' vascr_find(growth.df, "all")
vascr_find = function(data.df = vascr::growth.df , paramater, value = NA){
  
  param = NULL
  
  if(paramater == "Time"){param = (vascr_find_time(data.df, value))}
  if(paramater == "Unit"){param = (vascr_find_unit(data.df, value))}
  if(paramater == "Well"){param = (vascr_find_well(data.df, value))}
  if(paramater == "Sample"){param = (vascr_find_sample(data.df, value))}
  if(paramater == "Frequency"){param = (vascr_find_frequency(data.df, value))}
  if(paramater == "Experiment"){param = (vascr_find_experiment(data.df, value))}
  if(paramater == "SampleID"){param = (vascr_find_sampleid(data.df, value))}
  
  if(paramater == "resampled"){param = (vascr_check_resampled(data.df))}
  
  if(paramater == "all"){param = (vascr_find_metadata(data.df))}
  
  if(!is_null(param)){
    
      return(param)
    
  }
  
  vascr_notify("error","Paramater not something vascr can search for, please check spelling")
  
}

#' Match col name to the nearest thing actually in the dataset
#'
#' @param data.df The dataset to reference to
#' @param names Names of the cols to parse
#'
#' @returns A vector of columns, guaranteed to be in the dataset
#' 
#' @noRd
#'
#' @examples
#' vascr_find_col(growth.df, "HCMEC/D3")
#' vascr_find_col(growth.df, "line")
#' 
vascr_find_col = function(data.df, names){
  
 cn = data.df %>% colnames()

 repaired = vascr_match(names, cn)
 
 return(repaired)
  
}


#' Detect if an ECIS dataset has been normalized
#'
#' @param data.df an ECIS dataset
#'
#' @return The time the data was normalised to, or FALSE if not normalised
#' 
#' @importFrom dplyr reframe group_by
#' @importFrom stats sd
#' 
#' @noRd
#'
#' @examples
#' standard = growth.df
#' normal = vascr_normalise(growth.df, 100)
#' vascr_find_normalised(standard)
#' vascr_find_normalised(normal)

vascr_find_normalised = function(data.df)
{
  timecrushed = data.df %>% group_by(.data$Time) %>% 
    reframe(deviation = sd(.data$Value)) %>%
    filter(.data$deviation == 0)
  if (nrow(timecrushed)==0)
  {
    return (FALSE)
  }
  return(timecrushed$Time)
}


#' Find the median of a dataset, forced to a value from which the median is calculated
#' 
#' Usually, this would be the mean of the two centre most values, but that is not appropriate in some situations. Hence this function exists.
#'
#' @param vector Values to find the median of
#' @param round Should it be rounded "up" (default) or "down"
#'
#' @return The forced median value
#' 
#' @noRd
#'
#' @examples
#' # vector = unique(growth.df$Frequency)
#' # vascr_force_median(vector)
vascr_force_median = function(vector, round = "up")
{
  vector = as.numeric(vector)
  
  if(round == "up")
  {
    vector = sort(vector, decreasing = TRUE)
  }
  else
  {
    vector =  sort(vector, decreasing = FALSE)
  }
  
  median = median(vector)
  
  forced_median = vector[which.min(abs(vector-median))]
  return(forced_median)
}

params = c("time", "unit", "frequency", "cat")

vascr_force_single = function (data.df, params){
  
  param_options = c("time", "unit", "frequency")
  
  if("time" %in% param_options){
    
  }

  
  
}

#' Match a string with the closest available option
#'
#' @param tomatch The string to match
#' @param vector The vector to match into
#'
#' @return A character string of the closest matched string
#' 
#' @importFrom utils adist
#' @importFrom dplyr arrange
#' 
#' @noRd
#'
#' @examples
#' vector = vascr_find_unit(growth.df, "all")
#' vascr_match("Re", vector)
#' vascr_match("Rb", vector)
#' vascr_match(c("Rb", "Cm"), vector)
vascr_match = function(match, vector)
{
  toreturn = c()
  
  for(tomatch in match)
  {
    # If an exact match is present, return the matched value
    if(tomatch %in% vector)
    {
      toreturn = c(toreturn, tomatch)
    }
    else
    {
      # Make a data table of the distances between the tables 
      match_table = data.frame(vector)
      match_distance = as.vector(adist(tomatch, vector))
      match_table$Distance = match_distance
      
      # Calculate the change in length, so this can be used as a secondary differentiating factor
      match_table$Delta_Length = abs(str_length(match_table$vector) - str_length(tomatch))
      
      # Sort the table
      match_table = arrange(match_table, .data$Distance, .data$Delta_Length)
      
      matched = match_table[1,1]
      
      string = paste("[",tomatch, "] corrected to [", matched, "]. Please check the argeuments for your functions are correctly typed.", sep = "")
      
      vascr_notify("warning",string)
      
      toreturn = c(toreturn, matched)
    }
    
  }
  
  return(toreturn)
  
}



#' Find a time aligned with a vascr data set
#'
#' @param data.df The dataset to align to
#' @param time The time to align
#'
#' @return A single time that aligns with the dataset
#' 
#' @noRd
#'
#' @examples 
#' #vascr_find_time(growth.df, 43.78)
vascr_find_single_time = function(data.df, time)
{
  if(is.null(time))
  {
    return(unique(data.df$Time))
  }
  
  if(length(time)>1)
  {
    vascr_notify("warning","Vascr_find_single_time deals with only one time in one call. Use find times if more parsing is needed.")
    return("NA")
  }
  
  if(!is.data.frame(data.df))
  {
    vascr_notify("error","Data frame not provided to find a time in")
  }
  
  
  times = unique(data.df$Time)
  numberinlist = which.min(abs(times - time))
  timetouse = times[numberinlist]
  if(!(timetouse == time))
  {
    stringtoprint = paste("[",time,"]", " corrected to ","[",timetouse,"]. Please check the variables used.")
    vascr_notify("warning",stringtoprint)
  }
  
  return(timetouse)
}


#' Align times
#' 
#' When running analysis, you can only run stats on a timepoint that exists in the dataset. These are not always logical or easy to remember. This function rounds the number given to the nearest timepoint that is actually in the dataset.
#'
#' @param data.df A standard ECIS data frame
#' @param time The time point that needs rounding
#'
#' @return A timepoint that exactly aligns with a measured data point
#' 
#' @noRd
#'
#' @examples
#' #vascr_find_time(growth.df, 146.2)
#' #vascr_find_time(growth.df)
#' #vascr_find_time(growth.df, Inf)
#' #vascr_find_time(growth.df, NULL)
#' 
#' #vascr_find_time(growth.df, time = c(5,20))
#' 
vascr_find_time = function(data.df, time = NULL) {
  
  if(is.null(time))
  {
    times = unique(data.df$Time)
    return(times)
  }
  
  
  if(is.list(time))
  {
    times = c()
    
    for(tim in time)
    {
      times = c(times, vascr_find_single_time(data.df, tim))
    }
    
    return(times)
  }
  
  if(all(is.infinite(time)))
  {
    return(unique(data.df$Time))
  }
  
  if(all(is.na(time)))
  {
    times = unique(data.df$Time)
    return(vascr_force_median(times))
  }
  
  
  
  if (length(time) == 2) # If a vector of length 2 was submitted (ie two times) then we subset to that
  {
    times = unique(data.df$Time)
    times = times[(times >= time[1])]
    times = times[(times <= time[2])]
    return(times)
  }
  
  times = vascr_find_single_time(data.df, time)
  return(times)
}

#' Find a well in a local dataset
#'
#' @param data.df The dataset to detect from
#' @param well The well to find
#'
#' @return The string of a valid well to return
#' 
#' @noRd
#'
#' @examples
#' # vascr_find_well(growth.df, "A1")
#' # vascr_find_well(growth.df, NULL)
#' 
#' # vascr_find_well(growth.df, c("A1", "B3"))
#' 
vascr_find_well = function(data.df, well)
{
  if(is.null(well))
  {
    return(unique(data.df$Well))
  }
  
  # Standardize the well
  well = vascr_standardise_wells(well)
  
  # Check that the well is actually in the data set
  well = vascr_match(well, unique(data.df$Well))
  
  # Return said well
  return(well)
}


#' Align frequencies
#' 
#' When running analysis, you can only subset or plot a time that exists in the dataset. These are not always logical or easy to remember. This function rounds the number given to the nearest frequency that is actually in the dataset.
#'
#' @param data.df A standard ECIS data frame
#' @param frequency The frequency that needs rounding
#'
#' @return A timepoint that exactly aligns with a measured data point
#' 
#' @noRd
#'
#' @examples
#' vascr_find_frequency(growth.df, 4382)
#' vascr_find_frequency(growth.df, 4000)
#' vascr_find_frequency(growth.df, NULL)
#' vascr_find_frequency(growth.df, NA)
#' vascr_find_frequency(growth.df, Inf)
#' 
#' vascr_find_frequency(growth.df, "raw")
#' 
#' vascr_find_frequency(data.df = growth.df, frequency = c("raw", 0))
#' 
#' vascr_find_frequency(growth.df, frequency = c(100,200))
#' 
vascr_find_frequency = function(data.df, frequency) {
  
  freq = frequency
  toreturn = frequency
  
    if(is.null(frequency))
    {
    toreturn = c(toreturn,unique(data.df$Frequency))
    return(toreturn)
    }
  
  if(length(freq)>1)
  {
    
    # freqs <- vector(mode = "numeric", length = length(freq))
    
    # Define I to keep CMD CHECK happy
    i = 1

    block = foreach (i = freq) %do%
    {
      vascr_find_frequency(data.df, i)
    }
    
    freqs = unlist(block)
    
    return(freqs)
    
  }
  

  
  if(is.na(frequency))
  {
    toreturn = vascr_find_frequency(data.df, 4000)
    return(toreturn)
  }
  
  if(is.infinite(frequency))
  {
    toreturn = max(frequency)
    return(toreturn)
  }
  

      
      if(freq == "raw")
      {
        rawfrequencies = subset(unique(data.df$Frequency), unique(data.df$Frequency)>0)
        return(rawfrequencies)
      } else if (freq== "model"){
        return(0)
      }

      
      
      data.df = data.df %>% mutate("Frequency" = as.double(.data$Frequency))
      times = unique(data.df$Frequency)
      freq = as.numeric(freq)
      numberinlist = which.min(abs(times - freq))
      timetouse = times[numberinlist]
      toreturn = c(timetouse)

    
  
  if(!(toreturn == frequency))
  {
    vascr_notify("warning",paste("Frequency corrected from", frequency, "to", toreturn))
  }
  
  
  return(unique(toreturn) %>% as.numeric())
  
}




#' Find a vascr sample
#'
#' @param data.df vascr dataset to reference
#' @param sample sample name to try and find
#'
#' @return the name, or names, of the vascr dataset
#' 
#' @noRd
#' 
#' @examples
#' 
#' vascr_find_sample(growth.df, "3000 cells")
#' vascr_find_sample(growth.df, c("3000 cells", "35000_cells"))
#' vascr_find_sample(growth.df, 5)
#' vascr_find_sample(growth.df, "none")
#' vascr_find_sample(growth.df)
#' vascr_find_sample(growth.df, NA)
vascr_find_sample = function(data.df, sample = NULL){
  
  if(all(is.null(sample))){
    return(unique(data.df$Sample))
  }
  
  if(all(is.na(sample))){
    return(unique(data.df$Sample)[1])
  }
  
  
  if(is.numeric(sample)){
    
    sample_from_id = (data.df %>% select("SampleID", "Sample") %>% filter(.data$SampleID == sample) %>% distinct())$"Sample"
    
    if(length(sample_from_id)>0){
      sample = sample_from_id
    }
    
  }
  
  batched = for(sam in sample){
    if (sam == "none"){
      return("none")
    } else {
    return(vascr_match(sample, unique(c(data.df$Sample %>% as.character()))) %>% as.character())
  }
  
    
  }
  
  return(batched)
}


# Instrument --------------------------------------------------------------


#' Returns a list of the instruments supported by the VASCR package
#'
#' @return A vector of all the supported instrument names
#' 
#' @noRd
#'
#' @examples
#' # vascr_instrument_list()
vascr_instrument_list = function()
{
  return(c("ECIS", "xCELLigence", "cellZscope"))
}


#' Find an instrument known to vascr
#'
#' @param data.df A vascr dataset to analyse
#' @param instrument The instrument to search for
#'
#' @return A vector of strings that match
#' 
#' @noRd
#'
#' @examples
#' vascr_find_instrument(growth.df, "Rb")
#' vascr_find_instrument(growth.df, "cellZScope")
#' vascr_find_instrument(growth.df, c("cellZscope", "ECIS"))
#' 
#' vascr_find_instrument(growth.df, NULL)
vascr_find_instrument = function(data.df, instrument = NULL)
{
  
  if(is.null(instrument))
  {
    return(unique(data.df$Instrument))
  }
  
  returnvector =c()
  
  for (ins in instrument){
    instruments = vascr_instrument_list()
    repaired = vascr_match(ins, instruments)
    
    if(!repaired %in% unique(data.df$Instrument))
    {
      string = paste(repaired, " data is not present in the dataset. Use with care", sep = "")
      vascr_notify("warning",string)
    }
    else
    {
      returnvector = c(returnvector, repaired)
    }
    
  }
  
  if(length(returnvector)==0)
  {
    vascr_notify("warning","No selected instruments present in dataset. Use with care.")
  }
  else
  {
    returnvector = unique(returnvector)
  }
  
  return(returnvector)
}


#' Find a valid unit in the dataset, and throw an error if the unit selected is not appropriate
#'
#' @param data.df The dataset to find the unit in
#' @param unit The unit to find
#'
#' @return A vector of units that have been identified
#' 
#' @noRd
#'
#' @examples
#' vascr_find_unit(growth.df, "raw")
#' vascr_find_unit(growth.df, "modeled")
#' vascr_find_unit(growth.df, "all")
#' vascr_find_unit(growth.df, "Cm")
#' vascr_find_unit(growth.df, NA)
#' vascr_find_unit(growth.df, unit = c("Ci", "Rb"))
#' 
vascr_find_unit = function(data.df, unit = NA)
{
  
  if(is.null(unit))
  {
    return(unique(data.df$Unit))
  }
  
  
  toreturn = c()
  
  for(uni in unit)
  {
    
    if(is.na(uni))
    {
      
      instruments = unique(data.df$Instrument)
      
      for(instrumentused in instruments)
      {
        if(instrumentused == "ECIS")
        {
          toreturn = c(toreturn, "R")
        }
        if(instrumentused == "cellZscope")
        {
          toreturn = c(toreturn, "TER")
        }
        if(instrumentused == "xCELLigence")
        {
          toreturn = c(toreturn, "CI")
        }
      }
      
    } else if(tolower(uni) == "raw")
    {
      toreturn = (vascr_units_table() %>% filter(.data$Modeled == FALSE & .data$Instrument %in% unique(data.df$Instrument)))$Unit
    } else if(tolower(uni) == "modeled")
    {
      toreturn = (vascr_units_table() %>% filter(.data$Modeled == TRUE & .data$Instrument %in% unique(data.df$Instrument)))$Unit
    } else if (tolower(uni) == "all")
    {
      toreturn = (vascr_units_table() %>% filter(.data$Instrument %in% unique(data.df$Instrument)))$Unit
    } else
    {
      all_options = vascr_units_table()$Unit
      uni = vascr_match(uni, all_options)
      toreturn = c(toreturn,uni)
    }

  }
  
  toreturn = unique(toreturn) %>% as.character()
  return(toreturn)
  
}

#' Table of units used in the vascr package
#'
#' @return A data frame of units, their content and if they are modeled
#' 
#' @importFrom dplyr as_tibble
#' 
#' @noRd
#'
#' @examples
#' vascr_units_table()
#' 
vascr_units_table = function()
{
  allunits = vascr_instrument_units("all")
  vascr_unit_table = data.frame(allunits)
  colnames(vascr_unit_table) = "Unit"
  
  vascr_unit_table$Content = vascr_titles_vector(vascr_unit_table$Unit)
  
  vascr_unit_table$Modeled = vascr_is_modeled_unit(vascr_unit_table$Unit)
  
  vascr_unit_table$Instrument = vascr_instrument_from_unit(vascr_unit_table$Unit)
  
  vascr_unit_table = vascr_unit_table %>% separate_longer_delim("Instrument", delim =" + ") %>% as_tibble()
  
  return(vascr_unit_table)
}


#' Check if a selected unit is modelled
#'
#' @param unit The vascr symbol for the unit
#'
#' @return A boolean, true if it is modelled, false if it is raw electrical data
#' 
#' @noRd
#'
#' @examples
#' #vascr_is_modeled_unit("R")
#' #vascr_is_modeled_unit("Rb")
#' #vascr_is_modeled_unit(c("R", "Rb"))
#' 
vascr_is_modeled_unit = function(unit)
{
  return = c()
  model_units = c("Rb","Cm","Alpha","RMSE","Drift","CPE_A" ,"CPE_n" ,"TER" , "Ccl", "Rmed")
  
  for(uni in unit)
  {
    return = c(return, uni %in% model_units)
  }
  
  return(return)
}

#' Return the units created by a certain instrument
#'
#' @param instrument The instrument to find the units for
#'
#' @return a vector of units provided by an instrument
#' 
#' @noRd
#'
#' @examples
#' #vascr_instrument_units("ECIS")
#' #vascr_instrument_units("xCELLigence")
#' #vascr_instrument_units("cellZscope")
#' 
vascr_instrument_units =  function(instrument)
{
  instrument = tolower(instrument)
  
  if(instrument =="ecis") {return (c("Alpha" ,"Cm"   , "Drift", "Rb"   , "RMSE" , "C"   ,  "P"     ,"R"   ,  "X"  ,   "Z"))}
  if(instrument =="xcelligence") {return(xcelligence = c("Z", "CI"))}
  if(instrument =="cellzscope") { return(c("CPE_A", "CPE_n", "TER", "Ccl", "Rmed", "C"   ,  "P"     ,"R"   ,  "X"  ,   "Z"))}
  
  # If all are selected, build the full list by calling this same function recusivley
  if(instrument =="all"){return(unique(c(vascr_instrument_units("ecis"), vascr_instrument_units("xcelligence"), vascr_instrument_units("cellzscope"))))}
}


#' Find an experiment in a vascr dataset
#'
#' @param data.df The dataset to find the experiment in
#' @param experiment The experiment to find. Either a name or a number from when the experiment is combined can be used.
#'
#' @return A character string of the most closley matched experiment
#' 
#' @noRd
#'
#' @examples
#' vascr_find_experiment(growth.df, 1)
#' vascr_find_experiment(growth.df, experiment = "2")
#' vascr_find_experiment(growth.df, "1 : Experiment 1")
#' 
vascr_find_experiment = function(data.df, experiment)
{
  
  if(is.null(experiment))
  {
    return(unique(data.df$Experiment))
  }
  
  if(is.numeric(experiment))
  {
    fullexperiment = as.vector(unique(data.df$Experiment))
    fullexperiment = sort(fullexperiment)
    return(fullexperiment[experiment])
  }
  
  experiment = vascr_match(experiment, unique(data.df$Experiment) %>% as.character())
  return(experiment)
  
}


#' Generate human readable versions of the unit variable for graphing
#'
#' @param unit The unit to submit
#' @param frequency The frequency to submit
#' 
#' @importFrom glue glue
#'
#' @return An expression containing the correct data label for the unit
#' 
#' @noRd
#'
#' @examples
#' vascr_titles("Rb")
#' vascr_titles("R")
#' vascr_titles("Rb", explanatory = TRUE)
#' 
#' vascr_titles(unit = growth.df %>% vascr_subset(unit = "R", frequency = 4000))
#' 
vascr_titles= function (unit, frequency = 0, prefix = "", explanatory = FALSE, normalised = FALSE)
{
  
  if(is.data.frame(unit)){
    unit.df = unit
    unit = unique(unit.df$Unit)
    frequency = unique(unit.df$Frequency)
    normalised = !isFALSE(vascr_find_normalised(unit.df))
  } 
  
  if(length(unit)>1)
  {
    unit = unique(unit)[1]
  }

  name_table = tribble(~Unit, ~Title, ~Explanatory, ~Normalised,
          
          # Electrical quantaties
          "C", glue("{prefix}Capacitance (nF, {frequency} Hz)"), FALSE, FALSE,
          "R", glue("{prefix}Resistance (ohm, {frequency} Hz)"),FALSE, FALSE,
          "P", glue("{prefix}Phase (radians, {frequency} Hz)"),FALSE, FALSE,
          "X", glue("{prefix}Capacative Reactance (ohm, {frequency} Hz)"),FALSE, FALSE,
          "Z", glue("{prefix}Impedance (ohm, {frequency} Hz)"), FALSE, FALSE,
          
          # Electrical quantaties
          "C", glue("{prefix}Fold change in capacitance (nF, {frequency} Hz)"), FALSE, TRUE,
          "R", glue("{prefix}Fold change in resistance (ohm, {frequency} Hz)"),FALSE, TRUE,
          "P", glue("{prefix}Fold change in phase (radians, {frequency} Hz)"),FALSE, TRUE,
          "X", glue("{prefix}Fold change in capacative Reactance (ohm, {frequency} Hz)"),FALSE, TRUE,
          "Z", glue("{prefix}Fold change in impedance (ohm, {frequency} Hz)"), FALSE, TRUE,
          
          
          "C", glue("**Overall capacitance**<br>{prefix}Capacitance (nF, {frequency} Hz)"), TRUE, FALSE,
          "R", glue("**Overall resistance**<br>{prefix}Resistance (ohm, {frequency} Hz)"),TRUE, FALSE,
          "P", glue("{prefix}Phase (radians, {frequency} Hz)"),TRUE, FALSE,
          "X", glue("{prefix}Capacative Reactance (ohm, {frequency} Hz)"),TRUE, FALSE,
          "Z", glue("{prefix}Impedance (ohm, {frequency} Hz)"), TRUE, FALSE,
          
          "C", glue("**Overall capacitance**<br>{prefix}Fold change in capacitance ({frequency} Hz)"), TRUE, TRUE,
          "R", glue("**Overall resistance**<br>{prefix}Fold change in resistance ({frequency} Hz)"),TRUE, TRUE,
          "P", glue("**Overall phase**<br>{prefix}Fold change in phase{prefix}Phase (radians, {frequency} Hz)"),TRUE, TRUE,
          "X", glue("**Overall reactance**<br>{prefix}Fold change in reactance{prefix}Capacative Reactance ({frequency} Hz)"),TRUE, TRUE,
          "Z", glue("**Overall impedance**<br>{prefix}Fold change in impedance{prefix}Impedance ({frequency} Hz)"), TRUE, TRUE,
  
          # ECIS parameters
          
          "Rb", glue("{prefix}Rb (ohm cm<sup>2</sup>)"),FALSE, FALSE,
          "Cm", glue("{prefix}Cm (&#956;F/cm<sup>2</sup>)"),FALSE, FALSE,
          "Alpha", glue("{prefix}ohm<sup>1/2</sup> cm"),FALSE, FALSE,
          "RMSE", glue("{prefix}Model Fit RMSE"),FALSE, FALSE,
          "Drift", glue("{prefix}Drift (%)"), FALSE, FALSE,
          
          "Rb", glue("**Cell-Cell Adhesion**<br></br>{prefix}Rb (ohm cm<sup>2</sup>)"),TRUE, FALSE,
          "Cm", glue("**Membrane Capacitance**<br></br>{prefix}Cm (&#956;F/cm<sup>2</sup>)"),TRUE, FALSE,
          "Alpha", glue("**Basolateral Adhesion**<br></br>{prefix}ohm<sup>1/2</sup> cm"),TRUE, FALSE,
          "RMSE", glue("**Model Fit Error**<br></br>{prefix}Model Fit RMSE"),TRUE, FALSE,
          "Drift", glue("**Electrode Drift**<br></br>{prefix}Drift (%)"), TRUE, FALSE,
          
          "Rb", glue("**Cell-Cell Adhesion**<br></br>{prefix}Fold change in Rb"),TRUE, TRUE,
          "Cm", glue("**Membrane Capacitance**<br>{prefix}Fold change in Cm)"),TRUE, TRUE,
          "Alpha", glue("**Basolateral Adhesion**<br>{prefix}Fold change in alpha"),TRUE, TRUE,
          "RMSE", glue("**Model Fit Error**<br>{prefix}Model Fit RMSE"),TRUE, TRUE,
          "Drift", glue("**Electrode Drift**<br>{prefix}Drift (%)"), TRUE, TRUE,
          
          # xCELLigence
          "CI", glue("{prefix}Cell Index") ,FALSE, FALSE,
          
          # cellZscope
          "CPE_A", glue("{prefix}CPE_A (s<sup>n-1</sup>&#956;F/cm<sup>2</sup>)"),FALSE, FALSE,
          "CPE_n", glue("{prefix}CPE_n"),FALSE, FALSE,
          "TER", glue("{prefix}TER (&#937; cm<sup>2</sup>)"),FALSE, FALSE,
          "Ccl", glue("{prefix}Ccl (&#956;F/cm<sup>2</sup>)"),FALSE, FALSE,
          "Rmed", glue("{prefix}Rmed (ohm)"),FALSE, FALSE)

      
  
  # if(isTRUE(explanatory)){
  #   
  # }
  
  toreturn = name_table %>% filter(.data$Unit == unit) %>% 
    filter(.data$Explanatory == explanatory) %>% 
    filter(.data$Normalised == normalised)
  
  if(nrow(toreturn) != 1){
    
    return(unit)
    
  }
  
  # If not found, return what was input
  return(toreturn$Title)
  
}


#'
#' @param units A vector of units to return
#'
#' @return A vector of names of the units returned
#' 
#' @noRd
#'
#' @examples
#' vascr_titles_vector(c("Rb", "R", "Cm"))
#' 
vascr_titles_vector = function(units)
{
  return = c()
  
  for(uni in units)
  {
    
     parsed = vascr_titles(uni)
    
    
    return = c(return, parsed)
  }
  
  return(return)
  
}


#' Return the units created by a certain instrument
#'
#' @param instrument The instrument to find the units for
#'
#' @return a vector of units provided by an instrument
#' 
#' @noRd
#'
#' @examples
#' #vascr_instrument_units("ECIS")
#' #vascr_instrument_units("xCELLigence")
#' #vascr_instrument_units("cellZscope")
#' 
vascr_instrument_units =  function(instrument)
{
  instrument = tolower(instrument)
  
  if(instrument =="ecis") {return (c("Alpha" ,"Cm"   , "Drift", "Rb"   , "RMSE" , "C"   ,  "P"     ,"R"   ,  "X"  ,   "Z"))}
  if(instrument =="xcelligence") {return(xcelligence = c("Z", "CI"))}
  if(instrument =="cellzscope") { return(c("CPE_A", "CPE_n", "TER", "Ccl", "Rmed", "C"   ,  "P"     ,"R"   ,  "X"  ,   "Z"))}
  
  # If all are selected, build the full list by calling this same funciton recusivley
  if(instrument =="all"){return(unique(c(vascr_instrument_units("ecis"), vascr_instrument_units("xcelligence"), vascr_instrument_units("cellzscope"))))}
}

#' Work out which instrument(s) generated a unit
#'
#' @param unit The unit(s) to test
#'
#' @return The instrument(s) separated by "+" that could have generated that value. If more than one unit was entered a stirng will be generated for each unit.
#' 
#' @noRd
#'
#' @examples
#' 
#' #vascr_instrument_from_unit("Rb")
#' #vascr_instrument_from_unit("CI")
#' #vascr_instrument_from_unit("TER")
#' 
#' #vascr_instrument_from_unit(c("Rb", "TER"))
#' 
#' #vascr_instrument_from_unit("NA")
vascr_instrument_from_unit = function(unit)
{
  
  ecis = vascr_instrument_units("ecis")
  xcelligence = vascr_instrument_units("xcelligence")
  cellzscope = vascr_instrument_units("cellzscope")
  instruments = c()
  return = c()
  
  for (uni in unit)
  {
    if (uni %in% ecis)
    {
      instruments = c(instruments, "ECIS")
    }
    if (uni %in% xcelligence)
    {
      instruments = c(instruments, "xCELLigence")
    }
    if (uni %in% cellzscope)
    {
      instruments = c(instruments, "cellZscope")
    }
    
    return = c(return,(paste(instruments, collapse = " + ")))
    instruments = c()
  }
  
  return(return) 
}



#' Check the level of a vascr data frame
#'
#' @param data The data frame to analyse
#'
#' @return The level of the dataset analysed
#' 
#' @export
#'
#' @examples
#' vascr_find_level(growth.df)
#' vascr_find_level(vascr_summarise(growth.df %>% vascr_subset(unit= "Rb"), level = "experiments"))
#' vascr_find_level(vascr_summarise(growth.df %>% vascr_subset(unit= "Rb"), level = "summary"))
vascr_find_level = function(data)
{
  if("totaln" %in% colnames(data))
  {
    return("summary")
  }
  else if ("n" %in% colnames(data))
  {
    return("experiments")
  }
  else
  {
    return("wells")
  }
}

#' Find the Sample ID in a dataset
#'
#' @param data.df the dataset to reference
#' @param sampleid the sampleID to look up
#'
#' @importFrom cli cli_alert_danger
#'
#' @return the sampleID, but warns if the SampleID is not found
#' 
#' @noRd
#'
#' @examples
#' vascr_find_sampleid(growth.df, 3)
#' vascr_find_sampleid(growth.df, 300)
#' vascr_find_sampleid(growth.df)
vascr_find_sampleid = function(data.df, sampleid = NULL){
  
  if(is.null(sampleid))
  {
    return(unique(data.df$SampleID))
  }
  
  if(!sampleid %in% unique(data.df$SampleID))
  {
    cli_alert_danger("Sample ID not found")
  }
  
  return(sampleid)
  
}


#' Validate a file
#' 
#' Validates if a file exists, and if it has the correct file extension. This is used at the start of all import files to allow them to fail fast, rather than running intensive computations on files that are not found. Also prevents the generation of cryptic errors from downstream functions, that fail due to being presented with data in a strange format.
#'
#' @param file_name Character string specifying the file name
#' @param extension Character string containing a file extension that will be matched against the file type of file_name. Case insensitive.
#'
#' @return TRUE if it passes, FALSE if it does not. Also spits out warnings that will help the user correct the error
#' 
#' @noRd
#'
#' @examples
#' # check a file that does not exist fails
#' #vascr_validate_file("R/AAA_TODOOO.R", "P")
#' # Check a file with the wrong extension fails
#' #vascr_validate_file("R/AAA_TODO.R", "P")
#' # Check a file with the wrong extensions fail
#' #vascr_validate_file("R/AAA_TODO.R", c("P", "q"))
#' # Check a file with the right extension passes
#' #vascr_validate_file("R/AAA_TODO.R", "R")
#' # Check a file with one of two right extensions passes
#' #vascr_validate_file("R/AAA_TODO.R", extension = c("p", "r"))

vascr_validate_file = function(file_name, extension)
{
  
  if(!(isTRUE(file.exists(file_name))))
  {
    vascr_notify("error",paste("File ", file_name,"  not found. Please check file path and try again"))
  }
  else
  {
    exists = TRUE
  }
  
  ## Check for the correct file extension
  
  if(!missing(extension))
  {
    
    # If file extension is specified, check it matches
    
    split_name <- strsplit(basename(file_name), split="\\.")[[1]]
    file_extension = split_name[-1]
    
    extensioncorrect = any(toupper(file_extension) %in% toupper(extension))
    
    if(extensioncorrect)
    {
      correct = TRUE
    }
    else
    {
      filetypes = paste(extension, collapse = " or ")
      
      vascr_notify("error",paste("File extension is", file_extension, "not the required extension(s) ",filetypes,". Please check you have the correct file in the correct argument and try again."))
    }
    
  }
  
  # Return true if all conditions are met
  return(all(exists,correct))
}





#' Standardize well names across import types
#' 
#' Replaces A1 in strings with A01. Important for importing ABP files which may use either notation. Returns NA if the string could not be normalised, which can be configured to throw a warning in import code.
#'
#' @param well The well to be standardized 
#'
#' @return Standardized well names
#' 
#' @importFrom dplyr if_else
#' 
#' @noRd
#'
#' @examples 
#' #vascr_standardise_wells('A01')
#' #vascr_standardise_wells('A 1')
#' #vascr_standardise_wells('tortoise') # Non-standardize able values become NA
#' #vascr_standardise_wells(growth.df$Well)
#' 
vascr_standardise_wells = function(well) {
  
  
  uniquewell = unique(well)
  original_unique = uniquewell
  
  # First try and fix the user input
  uniquewell = toupper(uniquewell) # Make it upper case
  uniquewell = gsub(" ", "", uniquewell, fixed = TRUE) # Remove spaces
  uniquewell = gsub("[^0-9A-Za-z///' ]","" , uniquewell ,ignore.case = TRUE)
  uniquewell = gsub("(?<![0-9])([0-9])(?![0-9])", "0\\1", uniquewell, perl = TRUE) # Add 0's
  uniquewell = gsub("00", "0", uniquewell)
  
  # Check that it now conforms
  
  validnames = vascr_96_well_names()
  uniquewell = if_else(uniquewell %in% validnames, uniquewell, "NA" )
  
  if(any(uniquewell == "NA"))
  {
    vascr_notify("warning",paste("Well", uniquewell, "is not a valid well name, please check your input data"))
  }
  
  exchange = data.frame(well = original_unique, uniquewell)
  wells = data.frame(well)
  
  return = wells %>% left_join(exchange,  by = "well")
  
  return(return$uniquewell)
}

#' All the well names of a 96 well plate
#'
#' @return Vector containing all wells of a 96 well plate
#' 
#' @noRd
#'
#' @examples
#' vascr_96_well_names()
#' 
vascr_96_well_names = function()
{
  
  wells = expand.grid(LETTERS[1:8],c(1:12))
  wells$pasted = paste(wells$Var1,wells$Var2)
  wells = as.vector(wells$pasted)
  wells = gsub(" ", "", wells, fixed = TRUE) # Remove spaces
  wells = gsub("(?<![0-9])([0-9])(?![0-9])", "0\\1", wells, perl = TRUE) # Add O's
  wells = c(wells, "NC")
  return(wells)
}

#' Generate ggplot color hues, for manually specifying colors that match the default ggplot theme
#'
#' @param n Number of variables to access
#' 
#' @importFrom grDevices hcl
#'
#' @return A vector of ggplot colors
#' 
#' @noRd
#' 
#' @examples
#' vascr_gg_color_hue(5)
vascr_gg_color_hue <- function(n, start = 15, values_needed = c(1:n), l = 65, c = 100) {
  hues = seq(0,365+start, length = n + 1)
  hues = hues + start
  hue_codes = hcl(h = hues, l = l, c = c)[1:n]
  
  hue_codes[values_needed] %>% as.vector()
}


#' #' Current acquisition rate
#' #'
#' #' @param data.df The data frame to compute the current data acquisition frequency of
#' #'
#' #' @return The current acquisition rate of the data frame
#' #' 
#' #' 
#' #' @noRd
#' #'
#' #' @examples
#' #' 
#' #' #vascr_current_frequency(growth.df)
#' #' 
#' #' 
#' vascr_current_frequency = function (data.df)
#' {
#'   times = unique (data.df$Time) # Make a list of unique data points 
#'   times = sort(times) # Sort them
#'   difftimes = diff(times) # Calculate differences
#'   
#'   if (!(mean(difftimes) == getmode(difftimes)))
#'   {
#'     vascr_notify("warning","Gaps in the dataset, use resampling with care")
#'   }
#'   return(getmode(difftimes))
#' }

#' Return the ECIS cols used in this package
#' 
#' Can return either the core set of columns required for an ECIS package, the continous or categorical variables, or the exploded variables. Set will return the lot as a list.
#'
#' @param data The dataset to work off. Only required if non-standard cols are requested
#' @param set The set of columns to request. Default is core.
#'
#' @return A vector of the columns requested
#' 
#' @noRd
#'
#' @examples
#' 
#' vascr_cols()
#' vascr_cols(growth.df, set = "exploded")
#' vascr_cols(growth.df, set = "core")
#' 
#' 
vascr_cols  = function(data = NULL, set = "core")
{
  if(set == "core")
  {
    options = c("Time", "Unit", "Value", "Well", "Sample", "Frequency", "Experiment", "Instrument", "SampleID", "Excluded")
  }
  
  else if (set == "exploded") {
    # Return the non-core cols
    options = setdiff(colnames(data), vascr_cols())
  } else {
    vascr_notify("warning","Inappropriate set selected, please use another")
    return(NULL)
  }
  
  if(is.data.frame(data)){
    return(options[options %in% colnames(data)])
  }
  else{
    return(options)
  }
  
}



#' List out the samples currently in a vascr data set
#'
#' @param data.df The vascr data set to analyse
#'
#' @returns A printout of the samples, Sample ID's and experiments where they occur
#' 
#' @export
#'
#' @examples
#' vascr_samples(growth.df)
#' 
vascr_samples = function(data.df){
  data.df %>% select("SampleID", "Sample", "Experiment", "Well") %>%
    distinct() %>%
    group_by(.data$SampleID, .data$Sample, .data$Experiment) %>%
    reframe(Wells = paste(.data$Well, collapse = " ")) %>%
    mutate(Experiment = paste(.data$Experiment, .data$Wells)) %>%
    group_by(.data$SampleID, .data$Sample) %>%
    reframe(Experiment = paste(.data$Experiment, collapse = " | ")) %>%
    arrange(.data$Sample) %>%
    distinct()
}



#' Print out the characteristics of the vascr data frame
#'
#' @param data.df the vascr data frame to interrogate
#'
#' @return prints out the parameters of the dataframe in question
#' 
#' @importFrom cli cli_div cli_h1 cli_end
#' 
#' @export
#'
#' @examples
#' vascr_find_metadata(growth.df)
#' 
vascr_find_metadata = function(data.df)
{
  d <- cli_div(theme = list(h1 = list(color = "blue",
                                      "font-weight" = "bold")))
  
  cli_h1("Timepoints")
  
  cli_end(d)
  
  # p_title = function(toprint, subtitle = NULL)
  # {
  #   cat(bold$blue(toprint))
  #   
  #   # Make the subtitle section if needed
  #   if(!is.null(subtitle))
  #   {
  #     cli_h1(title)
  #     cat(paste(subtitle, sep = ", "))
  #   }
  # }
  # 
  # p_table = function(toprint)
  # {
  #   print(toprint, row.names = FALSE)
  #   cat("\n")
  # }
  # 
  # p_title("Timepoints", unique(data.df$Time))
  # p_title("Units", unique(data.df$Unit))
  # p_title("Frequencies", unique(data.df$Frequency))
  # p_title("Experiments", unique(data.df$Experiment))
  # p_title("Instruments", unique(data.df$instrument))
  # 
  # p_title("Samples in data frame")  
  #       table(data.df %>% 
  #               select("SampleID", "Sample") %>%
  #               distinct())
  
}



#' Convert a sample name into sampleID
#'
#' @param data.df 
#' @param sampleID 
#'
#' @returns a valid vascr sampleid
#' 
#' @noRd
#'
#' @examples
#' vascr_find_sampleid_from_sample(growth.df, "5,000_cells + HCMEC D3_line")
#' 
vascr_find_sampleid_from_sample = function(data.df, sample){
  find.df = data.df %>% select("Sample", "SampleID") %>%
    distinct() %>%
    filter(.data$Sample == sample)
  
  find.df$SampleID[[1]]
}


#' Find the number of times each well is sampled
#'
#' @param data.df A vascr dataset
#'
#' @return The number of times each well is sampled
#' 
#' # Internal tool so not exportable
#' @noRd
#' 
#' @importFrom dplyr group_by reframe
#'
#' @examples
#' vascr_find_count_timepoints(growth.df)
vascr_find_count_timepoints = function(data.df)
{
  
  experiment_times = data.df %>% group_by(.data$Unit, .data$Experiment, .data$Well, .data$Instrument, .data$Frequency) %>% 
    reframe(n = n())
  
  samples = min(experiment_times$n)
  
  return(samples)
  
}

