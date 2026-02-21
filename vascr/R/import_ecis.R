#' ECIS raw data importer
#' 
#' Raw data importer, generates a r data frame from a raw ABP file
#'
#' @param rawdata An ABP file containing the un-modeled data
#' @param sampledefine A CSV file containing well numbers and their corresponding sample names
#' @param experimentname Name of the experiment to be built into the dataset
#'
#' @return Data frame containing all the raw data readings from the ECIS Z0 instrument
#' 
#' @importFrom utils read.delim
#' @importFrom tidyr separate spread gather
#' @importFrom stringr str_detect str_replace
#' @importFrom dplyr left_join mutate distinct select
#' 
#' 
#' @noRd
#'
#' @examples
#' 
#' #First determine the locatins of your files relative to your dataset. 
#' #Here we use system.file to pull a default out of the filesystem, 
#' #but you can use a path relative to the file you are working on. 
#' #E.G 'Experiment1/Raw.abp'
#' 
#' rawdata = system.file('extdata/instruments/ecis_TimeResample.abp', package = 'vascr')
#' 
#' #Then run the import
#' 
#' data1 = ecis_import_raw(rawdata)
#'
#' 
ecis_import_raw =  function(rawdata, cache = hash_file_md5(rawdata)) {
  
  cache
  
  # check the files to be imported exist and are of the correct format
  vascr_validate_file(rawdata, "abp")
  
  # Grab all the rows of the file and dump them into a data frame
  vascr_notify("info", "Reading file")
  file.df = read.delim(rawdata, as.is = TRUE, sep = "\n", strip.white = TRUE)
  colnames(file.df) = "Data"
  
  # Generate a data frame containing the titles
  vascr_notify("info", "Extracting data")
  titles.df = subset(file.df, str_detect(file.df$Data, "Index, Time,"))
  titlestring = titles.df[1, 1]
  titles = unlist(strsplit(titlestring, split = ","))
  titles = trimws(titles)
  
  
  # Import the meaty part of the data and clean up the dat types
  fulldata.df = subset(file.df, str_detect(file.df$Data, "^T[0-9]"))
  
  fulldata.df = fulldata.df %>% tidyr::separate("Data", titles, ",|=")
  
  # Clean out the row data from each well's ID
  
  fulldata.df = fulldata.df %>% tidyr::separate("Index", c("TimeID", "ID"), "W")
  fulldata.df$TimeID = NULL
  
  # Find the cell correlates
  format = subset(file.df, str_detect(file.df$Data, "WellNum"))
  
  if (format[1, 1] == "WellNum = 16") {
    id_to_well.df = structure(list(ID = 1:16, Well = structure(1:16, .Label = c("A1", 
                                                                                "A2", "A3", "A4", "A5", "A6", "A7", "A8", "B1", "B2", "B3", "B4", "B5", "B6", 
                                                                                "B7", "B8"), class = "factor")), class = "data.frame", row.names = c(NA, -16L))
  }
  
  if (format[1, 1] == "WellNum = 96") {
    id_to_well.df = structure(list(ID = 1:96, Well = structure(c(1L, 13L, 25L, 37L, 49L, 
                                                                 61L, 73L, 85L, 5L, 17L, 29L, 41L, 53L, 65L, 77L, 89L, 6L, 18L, 30L, 42L, 54L, 
                                                                 66L, 78L, 90L, 7L, 19L, 31L, 43L, 55L, 67L, 79L, 91L, 8L, 20L, 32L, 44L, 56L, 
                                                                 68L, 80L, 92L, 9L, 21L, 33L, 45L, 57L, 69L, 81L, 93L, 10L, 22L, 34L, 46L, 58L, 
                                                                 70L, 82L, 94L, 11L, 23L, 35L, 47L, 59L, 71L, 83L, 95L, 12L, 24L, 36L, 48L, 60L, 
                                                                 72L, 84L, 96L, 2L, 14L, 26L, 38L, 50L, 62L, 74L, 86L, 3L, 15L, 27L, 39L, 51L, 
                                                                 63L, 75L, 87L, 4L, 16L, 28L, 40L, 52L, 64L, 76L, 88L), .Label = c("A1", "A10", 
                                                                                                                                   "A11", "A12", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "B1", "B10", "B11", 
                                                                                                                                   "B12", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "C1", "C10", "C11", "C12", 
                                                                                                                                   "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "D1", "D10", "D11", "D12", "D2", 
                                                                                                                                   "D3", "D4", "D5", "D6", "D7", "D8", "D9", "E1", "E10", "E11", "E12", "E2", "E3", 
                                                                                                                                   "E4", "E5", "E6", "E7", "E8", "E9", "F1", "F10", "F11", "F12", "F2", "F3", "F4", 
                                                                                                                                   "F5", "F6", "F7", "F8", "F9", "G1", "G10", "G11", "G12", "G2", "G3", "G4", "G5", 
                                                                                                                                   "G6", "G7", "G8", "G9", "H1", "H10", "H11", "H12", "H2", "H3", "H4", "H5", "H6", 
                                                                                                                                   "H7", "H8", "H9"), class = "factor")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                              -96L))
  }
  
  fulldata.df$ID = as.integer(fulldata.df$ID)
  
  id_to_well.df$Well = vascr_standardise_wells(id_to_well.df$Well)
  
  
  # Correlate the generated cell lookup table to ECIS's internal well id's
  fulldata.df = left_join(fulldata.df, id_to_well.df, by = "ID")
  
  
  # Make the wide dataset long
  vascr_notify("info", "Lengthening the dataset")
  fulldata_long.df = fulldata.df %>% tidyr::gather("Type", "Value", -"Well", -"Time", -"ID")
  fulldata_long.df = fulldata_long.df %>% mutate(Value = as.numeric(.data$Value))
  
  separate = fulldata_long.df %>% select("Type") %>% distinct() %>% 
    tidyr::separate("Type", c("Unit", "Frequency"), remove = FALSE)
  
  # Split out frequency and R/C as needed
  fulldata_long.df = fulldata_long.df %>% left_join(separate, by = "Type")
  
  fulldata_long.df$ID = NULL
  fulldata_long.df$Type = NULL
  
  # Generate the other physical quantities
  vascr_notify("info", "Generating other physical quantaties")
  
  # Wrangle data so it is in columns
  child1.df = fulldata_long.df
  child1.df$Value = abs(child1.df$Value)
  widedata.df = tidyr::spread(child1.df, "Unit", "Value") %>% 
                  mutate(Frequency = as.numeric(.data$Frequency))
  
  # Calculate the new derivative values
  widedata.df$Z = sqrt(widedata.df$X^2 + widedata.df$R^2)
  widedata.df$C = 1/(2 * pi * widedata.df$Frequency * widedata.df$X) * 10^9
  widedata.df$P = 90 - (atan(widedata.df$X/widedata.df$R)/(2 * pi) * 360)
  
  # Change format back
  longdata.df = tidyr::gather(widedata.df, "Unit", "Value", -"Well", -"Time", -"Frequency")
  
  vascr_notify("info", "Cleaning up")
  # Fix data types
  longdata.df$Unit = factor(longdata.df$Unit)
  longdata.df$Well = as.character(longdata.df$Well)
  longdata.df = longdata.df %>% mutate(Time = as.numeric(.data$Time))
  

  
  longdata.df$Instrument = "ECIS"
  
  gc(verbose = FALSE)
  
  # Explicitly return
  return(longdata.df)
}




#' Import raw modeled data
#'
#' @param modeleddata Raw modeled data in APB format
#' @param sampledefine CSV file containing which wells correspond to which values
#' @param experimentname Name of the experiment to be built into the dataset
#'
#' @return Data frame containing modeled data
#' 
#' @noRd
#' 
#' @importFrom stringr str_detect
#' @importFrom tidyr separate gather pivot_longer
#' @importFrom dplyr '%>%'
#' @importFrom utils read.csv
#' @importFrom cli hash_file_md5
#' 
#'
#' @examples
#' modeled = system.file('extdata/instruments/ecis_TimeResample_RbA.csv', package = 'vascr')
#' ecis_import_model(modeled)
ecis_import_model = function(modeleddata, cache = hash_file_md5(modeleddata)) {
  
  cache
  
  # Validate that the files are readable
  vascr_validate_file(modeleddata, "csv")
  # vascr_validate_file(sampledefine, c("csv", "xlsx"))
  
  rawdata = modeleddata
  
  vascr_notify("info", "Reading file into R")
  file.df = read.delim(rawdata, as.is = TRUE, sep = "\n", strip.white = TRUE)
  colnames(file.df) = "Data"
  
  # Import the dataset in segments so that you can get rid of the ECIS crap
  vascr_notify("info", "Extracting useful data")
  cells.df = subset(file.df, str_detect(file.df$Data, "Well ID"))
  unit.df = subset(file.df, str_detect(file.df$Data, "Time "))
  data.df = subset(file.df, str_detect(file.df$Data, "^[0-9]"))
  
  if(nrow(unit.df)==0 || nrow(data.df)==0)
  {
    vascr_notify("warning", "No data imported, check the modeled data you are trying to import is correctly specified and an intact file")
  }
  
  cells = cells.df[1, 1]
  cells = unlist(strsplit(cells, split = ","))
  cells = base::trimws(cells)
  cells.df = cells
  
  unit = unit.df[1, 1]
  unit = unlist(strsplit(unit, split = ","))
  unit = base::trimws(unit)
  unit.df = unit
  
  # Rename the units something sensible
  vascr_notify("info", "Renaming units")
  unit.df = replace(unit.df, unit.df == "Rb (ohm.cm^2)", "Rb")
  unit.df = replace(unit.df, unit.df == "Alpha (cm.ohm^0.5)", "Alpha")
  unit.df = replace(unit.df, unit.df == "CellMCap(uF/cm^2)", "Cm")
  unit.df = replace(unit.df, unit.df == "Drift (%)", "Drift")
  unit.df = replace(unit.df, unit.df == "RMSE", "RMSE")
  
  # Generate unique names vector
  uniquenamesvector = paste(unit.df, cells.df, sep = "_")
  
  # Merge well ID and unit variables together
  
  vascr_notify("info", "Naming dataset")
  data.df = data.df %>% tidyr::separate("Data", uniquenamesvector, ",", extra = "drop")
  alldata.df = rbind(cells.df, data.df)
  
  alldata.df = alldata.df[-1,]
  colnames(alldata.df)[1] = "Time"
  
  vascr_notify("info", "Creating long dataframe")
  combined.df = alldata.df %>% pivot_longer(cols = -"Time", values_to = "Value") %>%
    separate("name", into = c("Unit", "Well"))
  
  
  # Fix up the data types
  vascr_notify("info", "Finishing up")
  combined.df$Time = as.numeric(combined.df$Time)
  combined.df$Value = as.numeric(combined.df$Value)
  combined.df$Unit = factor(combined.df$Unit)
  combined.df$Well = factor(combined.df$Well)
  
  combined.df$Well = vascr_standardise_wells(combined.df$Well)
  
  
  
  # import the naming tags
  combined2.df = combined.df
  
  combined2.df$Frequency = 0
  

  combined2.df$Instrument = "ECIS"
  combined2.df$Time = as.numeric(combined2.df$Time)
  
  return(combined2.df)
}


#' Import all ECIS values, a child of ecis_import_raw and ecis_import_model
#'
#' @param raw A raw ABP file to import
#' @param modeled  A modeled APB file for import
#' @param experimentname Name of the experiment to be built into the dataset
#'
#' @return A data frame containing all the data APB generated from an experiment 
#' 
#' @importFrom dplyr tribble as_tibble
#' 
#' @export
#'
#' @examples
#' 
#' raw = system.file('extdata/instruments/ecis_TimeResample.abp', package = 'vascr')
#' modeled = system.file('extdata/instruments/ecis_TimeResample_RbA.csv', package = 'vascr')
#' experimentname = "TEST"
#' 
#' #Then run the import
#' 
#' data = ecis_import(raw ,modeled,experimentname)
#' #head(data)
ecis_import = function(raw = NULL, modeled = NULL, experimentname = NULL) {
  
  vascr_notify("info", "Starting import")
  
  # Validate files exist and are correct. Will be done in the internal functions, but doing it here saves time on failure
  if(!is.null(raw)){vascr_validate_file(raw, "abp")}
  if(!is.null(modeled)){vascr_validate_file(modeled, "csv")}
  
  masterdata.df = tribble(~"Time", ~"Unit", ~"Well", ~"Value", ~"Frequency", ~"Instrument")

  # If a raw data file is specified, import it
  if(!is.null(raw))
  {
    vascr_notify("info", "Importing raw data")
    raw.df = ecis_import_raw(raw)
    masterdata.df = rbind(masterdata.df, raw.df)
  }
  
  # If a modeled data file is specified, import it
  if(!is.null(modeled))
  {
    vascr_notify("info", "Importing model data")
    model.df = ecis_import_model(modeled)
    masterdata.df = rbind(masterdata.df, model.df)
  }
  
  if(!is.null(experimentname))
  {
    masterdata.df$Experiment = experimentname
  }
  
  masterdata.df$Excluded = "no"
  
  masterdata.df = as_tibble(masterdata.df)
  
  masterdata.df$Experiment = experimentname
  masterdata.df$Sample = "NA"
  
  vascr_notify("success", "Import complete")
  
  
  return(masterdata.df)
}


