#' Import modeled cellZscope data
#' 
#' Imports modeled data previously exported from the cellZscope software. Will not assign names unless a keyfile or raw dataset is provided, as no well naming data is provided in this filetype.
#' 
#'
#' @param model The location of modeled data exported from the cellZscope data
#' 
#' @importFrom stringr str_remove str_split
#' @importFrom tidyr separate fill pivot_longer
#' @importFrom dplyr %>%
#'
#' @return An vascr compatible data set
#'
#' @noRd
#'
#' @examples
#' model = system.file("extdata/instruments/zscopemodel.txt", package = "vascr")
#' 
#' output = cellzscope_import_model(model)
#' output = vascr_subset(output, time = c(0,50))
#' #vascr_plot(output, unit = "TER", frequency = 0,replication = "wells")
#' 
cellzscope_import_model  = function(model)
{
  
  # Check that the file is correct
  vascr_validate_file(model, "txt")

  
  # Read in the file to a data table
  spectrafile = readLines(model)
  data.df = as.data.frame(spectrafile)
  
  # Clean out the strange encoding marks and units (not required, will be substituted in later)
  data.df$spectrafile = str_remove(data.df$spectrafile, "(s\u00e2\u0081\u00bf\u00e2\u0081\u00bb\u00c2\u00b9\u00c2\u00b7\u00c2\u00b5F/cm\u00c2\u00b2)")
  data.df$spectrafile = str_remove(data.df$spectrafile, "(\u00ce\u00a9\u00c2\u00b7cm\u00c2\u00b2)")
  data.df$spectrafile = str_remove(data.df$spectrafile, "(\u00c2\u00b5F/cm\u00c2\u00b2)")
  data.df$spectrafile = str_remove(data.df$spectrafile, "(\u00ce\u00a9\u00c2\u00b7cm\u00c2\u00b2)")
  data.df$spectrafile = str_remove(data.df$spectrafile, "(\u00c2\u00b5F/cm\u00c2\u00b2)")
  data.df$spectrafile = str_remove(data.df$spectrafile, "(\u00ce\u00a9)")
  data.df$spectrafile = str_remove(data.df$spectrafile, "\\(\\)")
  
  
  # Make a dedicated column for the units (expressed as titles) and copy them down
  separatedata = separate(data.df, "spectrafile", into = c("Data", "Unit"), remove = TRUE, sep = "Parameter : ", fill = "right")
  separatedata = separatedata %>% fill(names(separatedata), .direction = "down")
  
  # Generate the new column titles and use them to split the main dataset
  newcols = subset(separatedata, grepl("time \\(h\\)", separatedata$Data))
  
  newcols = strsplit(newcols[1,1], ",")
  newcols = unlist(newcols)
  newcols[1] = "Time"
  separatedata = separate(separatedata, col = .data$Data, into = newcols, sep = ",", fill = "right")
  
  # Clean up the  wide dataset
  separatedata = subset(separatedata, !is.na(separatedata[,2]))
  separatedata = subset(separatedata, separatedata$Time !="time (h)")
  separatedata = subset(separatedata, separatedata$Time !="")
  
  # Pivot the table so each well expressed as a column becomes it's own row
  separatedata = pivot_longer(separatedata, cols = c(-"Unit", -"Time"), names_to = "Well", values_to = "Value")
  
  # Add variables fixed throughout the import
    separatedata$Experiment = basename(model)

  
  separatedata$Sample = "NA"
  separatedata$Frequency = 0
  separatedata$Instrument = "cellZscope"
  
  # Fix data types
  separatedata$Time = as.numeric(separatedata$Time)
  separatedata$Value = as.numeric(separatedata$Value)
  separatedata$Well = vascr_standardise_wells(separatedata$Well)
  
  
  separatedata$Unit %>% unique()
  
  return(separatedata)
}



#' Import a raw cellZScope data set
#'
#' Data must first be exported from the cellZscope software. 
#'
#' @param raw the location of spectral data exported from the cellZscope software.
#' @param key Optional, allows for sample names to be assigned to wells
#' @param experimentname Name of the experiment to be built into the data set
#'
#' @return A vascar compatable dataset
#' 
#' @importFrom stringr str_remove
#' @importFrom tidyr separate fill pivot_longer pivot_wider
#' @importFrom dplyr %>%
#' 
#' 
#' @noRd
#'
#' @examples
#' raw = system.file("extdata/instruments/zscoperaw.txt", package = "vascr")
#' cellzscope_import_raw(raw)
cellzscope_import_raw = function(raw)
{
  
  vascr_validate_file(raw, "txt")
  
  # Read into a data frame and remove garbage
  spectrafile = readLines(raw)
  data.df = as.data.frame(spectrafile)
  
  #Separate out the data that needs to be carried down
  separatedata = separate(data.df, "spectrafile", into = c("Data", "Well"), remove = TRUE, sep = "Well: ", fill = "right")
  separatedata = separate(separatedata, "Data", into = c("Data", "Time"), remove = FALSE, sep = "Spectrum:", fill = "right")
  separatedata = separate(separatedata, "Time", into = c("Run", "Date"), remove = FALSE, sep = "measured at ")
  separatedata = separate(separatedata, "Data", into = c("Frequency", "I", "P"), sep = ",", fill = "right")
  separatedata = separate(separatedata, "Well", into = c("Well", "Sample"), sep = "-")
  
  #Copy down the data
  separatedata = separatedata %>% fill(names(separatedata), .direction = "down")
  
  
  #Cleanup the data
  separatedata = subset(separatedata, separatedata$F !="frequency (Hz)")
  separatedata = subset(separatedata, separatedata$Date !="")
  separatedata = subset(separatedata, separatedata$F !="")
  
  separatedata$Run = str_remove(separatedata$Run, " Run.")
  
  # Convert the date stamps to times, running them through an external array
  dat = as.POSIXct(separatedata$Date, format='%d/%m/%Y %I:%M:%S %p')
  dat = as.numeric(dat)
  dat = dat-min(dat)
  dat = dat/60/60
  separatedata$Time = dat
  
  # Add experiment metadata
  separatedata$Experiment = basename(raw)
  separatedata$Instrument = "cellZscope"
  
  # Make longer
  separatedata2 = pivot_longer(separatedata, cols = c("I", "P"), names_to = "Unit", values_to = "Value")
  
  # Clean up data types
  separatedata2$Value = as.numeric(separatedata2$Value)
  separatedata2$Frequency = as.numeric(separatedata2$Frequency)
  separatedata2$Well = vascr_standardise_wells(separatedata2$Well)
  
  # Remove internal columns that are no longer required
  separatedata2$Date = NULL
  separatedata2$Run = NULL
  
  output2 = separatedata2
  output2$Unit = str_replace(output2$Unit, "I", "Z")
  
  # Wrangle data so it is in columns
  child1.df = output2
  child1.df$Value = abs(child1.df$Value)
  
  widedata.df = pivot_wider(child1.df, names_from = "Unit", values_from = "Value")
  widedata.df$Frequency = as.numeric(widedata.df$Frequency)
  
  # Calculate new values
  widedata.df$Pr = widedata.df$P / 360 * pi
  widedata.df$X = sin(widedata.df$Pr) * widedata.df$Z
  widedata.df$R = sin(widedata.df$Pr) * widedata.df$Z
  widedata.df$C = 1/(2 * pi * widedata.df$Frequency * widedata.df$X) * 10^9
  
  longdata.df = tidyr::gather(widedata.df, "Unit", "Value", -"Well", -"Time", -"Frequency", -"Sample", -"Instrument", -"Experiment")
  
  # Fix data types
  longdata.df$Unit = factor(longdata.df$Unit)
  longdata.df$Well = as.character(longdata.df$Well)
  longdata.df$Time = as.numeric(longdata.df$Time)
  
  
  return(longdata.df)
  
}

#' Import cellSZcope data
#' 
#' Data must first be exported in two parts from the cellZScope software. This can then be imported to a standard vascar dataset with this function
#'
#' @param raw File location of the raw dataset
#' @param model File locaiton of the modeled dataset
#' @param key Location of a vascar standard lookup table. Optional, but can be used to import more granular data than possible with the built in row names
#' @param experimentname Name of the experiment to be built into the dataset
#' 
#' @importFrom dplyr select left_join 
#' 
#' @noRd
#'
#' @return a standard vascr data set
#'
#' @examples
#' 
#' model = system.file("extdata/instruments/zscopemodel.txt", package = "vascr")
#' raw = system.file("extdata/instruments/zscoperaw.txt", package = "vascr")
#' 
#' alldatakey = cellzscope_import(raw, model)
#' vascr_plot(alldatakey, unit = "TER", frequency = 0, time = c(0,50))
#' 
cellzscope_import = function(raw, model, experimentname = NULL)
{
  
  vascr_validate_file(raw, "txt")
  vascr_validate_file(model, "txt")

  
  # Import both files. Don't specify a key as this will be applied global at the end
  modeleddata = cellzscope_import_model(model)
  rawdata = cellzscope_import_raw(raw)
  
  # Combine and run checks
  alldata = rbind(modeleddata, rawdata)
  
  
    # Build a look up table of sample names from the modeled data imported and apply them to the raw data imported
    nametable = select(rawdata, "Well", "Sample")
    nametable = unique(nametable)
    alldata$Sample = NULL
    alldatanamed = left_join(alldata, nametable, by = c("Well"))

  
  if(!is.null(experimentname))
  {
    alldatanamed$Experiment = experimentname
  }
  
  return(alldatanamed)
  
}

