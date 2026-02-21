#' Lengthen out an exCELLigence plate map
#' 
#' Switches the xCELLigence format of having each column as a column and each row as a row, then stacking time points to a standardized tidy data format
#'
#' @param data The raw xCELLigence data set to deal with
#' 
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom stringr str_remove
#' 
#' @noRd
#'
#' @return A slightly tidier data set
#' 
xcelligence_lengthen_platemap = function(data)
{
  # Pivot table longer, and merge cols together to give well ID. Standardize
  lookuptable = pivot_longer(data, cols = starts_with("C"), names_to = "Cols", values_to = "Value")
  lookuptable$Cols = str_remove(lookuptable$Cols, "C")
  lookuptable$Well = paste(lookuptable$Row, lookuptable$Cols, sep = "")
  lookuptable$Well = vascr_standardise_wells(lookuptable$Well)
  # Clean up
  lookuptable$Cols = NULL
  lookuptable$Row = NULL
  
  return(lookuptable)
}




#' Generate CI from xcelligence data
#'
#' @param data.df The data set to generate CI from
#'
#' @return An enlarged dataset
#' 
#' @noRd
#'
xcelligence_import_generate_CI = function(data.df)
{
  data.df$Excluded = "no"
  cidata = vascr_normalise(data.df, normtime = 0, divide = TRUE)
  cidata$Unit = "CI"
  
  returndata = rbind(cidata, data.df)
  return(returndata)
}


#' Import an xcelligence file
#' 
#' Uses odbc dbConnect odbc dbDisconnect as soft dependencies
#' DBI dbReadTable
#'
#' @param file The file to import
#' @param key A keyfile to apply. Optional, as the xCELLigence internal definitions will be used if no file is specified
#' @param experimentname Name of the experiment to be built into the dataset
#' 
#' @importFrom tidyr separate pivot_wider
#' @importFrom dplyr left_join as_tibble
#' @importFrom stringr str_replace
#' @importFrom rlang check_installed
#'
#' @return A vascr datafile
#' 
#' @noRd
#' 
#' @examples
#' # xCELLigence test
#' rawdata = system.file('extdata/instruments/xcell.plt', package = 'vascr')
#' xcell = import_xcelligence(rawdata = rawdata,"TEST7")
#' 
#'  
import_xcelligence = function(rawdata, experimentname = NULL, password = "RTCaDaTa")
{
 
  rlang::check_installed(c("odbc", "DBI"), reason = "is needed to deal with the xCELLigence data format`")
  
  file = rawdata
  
  vascr_validate_file(file, "plt")
  
  
  # Make a temporary copy of the file, with the correct extension so Microsoft Access can open it
  #folder = dirname(file)
  tempfile = paste(tempdir(),"/","TEMPMDBFORIMPORT.mdb", sep = "")
  if(!file.copy(from = file, to = tempfile))
  {
    vascr_notify("error","ERROR - file could not be duplicated to be opened. Ensure you have write capabilities in the new folder, and the file TEMPMDBFORIMPORT.mdb does not exist. Also check that the plt file is not open when you run this command")
  }
  file = tempfile
  
  # Hard code in the list of tables we need to import from access. This will be pruned later to speed things up
  # tables = c("Calibration","ENotes", "ErrLog", "ETimes", "Index1", "Index2", "Index3", "Layout", "Messages", "mIndex1", "Org10K", "Org25K", "Org50K", "ScanPlate", "ScanPlateData", "StepStatus", "TTimes", "WellColor")
  
  
  connection <- tryCatch({odbc::dbConnect(odbc::odbc(), .connection_string = paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                            DBQ=",tempfile,";
                            PWD=RTCaDaTa", sep = ""))},
                         error = function(e) {
                           message("Connection not found. Returning NULL.")
                           return(NULL)
                         })
  
  if(is.null(connection))
  {
    return(NULL)
  }
  
  Org10K <- DBI::dbReadTable(connection , "Org10K")
  Org25K <- DBI::dbReadTable(connection , "Org25K")
  Org50K <- DBI::dbReadTable(connection , "Org50K")
  TTimes <- DBI::dbReadTable(connection , "TTimes")
  Layout <- DBI::dbReadTable(connection , "Layout")
  
  odbc::dbDisconnect(connection)
  
  
  # Delete the temporary file now we have what we need in R
  file.remove(tempfile)
  
  # Where needed, set the appropriate frequencies into the org data files, then bind them together
  Org10K$Frequency = 10000
  Org25K$Frequency = 25000
  Org50K$Frequency = 50000
  MasterOrg = rbind(Org10K, Org25K, Org50K)
  
  # Pivot the xcelligence columns into one row per data point, then remove the C's and attach them together to give well ID's
  LongOrg = xcelligence_lengthen_platemap(MasterOrg)
  
  # Clean up the mess created in making well ID's and standardise
  LongOrg$StepID = NULL
  
  
  # Add times (in hours) to each time point by looking them up in another sheet
  TimeOrg = left_join(LongOrg, TTimes, by = "TimePoint") #Stick timepoints to the dataset
  TimeOrg$TestTime = as.character(TimeOrg$TestTime) # Make them characers (so the next line works)
  TimeOrg$TestTime = as.POSIXct(TimeOrg$TestTime) # Parse the dates
  TimeOrg$TestTime = as.numeric(TimeOrg$TestTime) # Convert to numbers
  TimeOrg$TestTime = TimeOrg$TestTime - min(TimeOrg$TestTime) # Subtract time 0
  TimeOrg$TestTime = TimeOrg$TestTime/60/60 # Convert seconds to hours
  
  TimeOrg$Time = TimeOrg$TestTime # Clean up
  TimeOrg$TestTime = NULL
  TimeOrg$TimePoint = NULL
  TimeOrg$StepID = NULL
  
  TimeOrg$Unit = "Z" # Assign impedance (Z) as the unit for all time points. This is all the CellZScope can capture.
  
  # Assign experiment name
  if(is.null(experimentname))
  {
    TimeOrg$Experiment = basename(file)
  }
  else
  {
    TimeOrg$Experiment = experimentname
  }
  
  
  TimeOrg$Instrument = "xCELLigence" # Assign instrument name
  
  # Code for assigning samples from file
  
    # Pivot table longer, and merge cols together to give well ID. Standardise
    lookuptable = xcelligence_lengthen_platemap(Layout)
    
    lookuptable$Sample = lookuptable$Value
    lookuptable$Value = NULL
    
    labeleddata = left_join(TimeOrg, lookuptable, by = "Well")
    
    labeleddata = data.frame(Sample = unique(labeleddata$Sample), SampleID = c(1:length(unique(labeleddata$Sample)))) %>%
      right_join(labeleddata, by = join_by("Sample"))
    toreturn = xcelligence_import_generate_CI(labeleddata)
    
    toreturn = as_tibble(toreturn)
  
  return(toreturn)
}





