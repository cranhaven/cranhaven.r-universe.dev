#' Format raw qPCR fluorescence file data for results_Table or standardCurveResults_Table.
#'
#' @description Function formats raw qPCR fluoresence file data from MIC, StepOnePlus or
#' Biomeme two3/Franklin machines into table that includes well location names and fluorescence
#' data for each reaction cycle. The table is written to the local machine directory as a CSV
#' file. The formatted data can be copied and pasted into the results_Table or
#' standardCurveResults_Table, which are used in the MDMAPR 2.0 MySQL database.
#'
#' @param rawFluorescenceFile defines raw qPCR fluorescence file to read in.
#' @param platform defines platform fluorescence file was generated from.
#' @param outputFileName defines output name for CSV file that data will be written to.
#'
#' @return Writes CSV file to local directory.
#' @export
#'
#' @usage formatRawFluorescenceFile(rawFluorescenceFile, platform, outputFileName)
#'

formatRawFluorescenceFile <-function(rawFluorescenceFile, platform, outputFileName){

  #MIC

  if (platform == "MIC") {
  #Step 1: Read in raw fluorescence data
  MIC_fluorescence_data <- read.csv(rawFluorescenceFile)

  #Step 2: transform into matrix where each row is a sample and each column is a cycle
  formattedMICfluorescencedata <-  process_MIC_uploaded_file(MIC_fluorescence_data)

  #Step 3: Add well location
  formattedMICfluorescencedata$wellLocation <- 1:nrow(formattedMICfluorescencedata)
  formattedMICfluorescencedata <-  formattedMICfluorescencedata %>% select(41, 1:40)

  #Step 4: Write csv with formatted fluorescence data to machine
  write.csv(formattedMICfluorescencedata , outputFileName, row.names=FALSE)}


  #StepOnePlus

  else if (platform == "StepOnePlus") {
  #Step 1: Read in raw fluorescence data
  SOP_fluorescence_data <- read_excel(rawFluorescenceFile, 4)

  #Step 2: transform into matrix where each row is a sample and each column is a cycle
  formattedSOPfluorescencedata <-  process_SOP_uploaded_file_with_well_names(SOP_fluorescence_data)

  #Step 3: Write csv with formatted fluorescence data to machine
  write.csv(formattedSOPfluorescencedata, outputFileName, row.names=FALSE)

  }

  #Biomeme two3/Franklin

  else if (platform == "Biomeme two3/Franklin") {
  #Step 1: Read in raw fluorescence data
  Biomemetwo3_Franklink_fluorescence_data <- read.csv(rawFluorescenceFile)

  #Step 2: transform into matrix where each row is a sample and each column is a cycle
  formattedBiomemetwo3_Franklinkfluorescencedata <- process_biomeme23_uploaded_file_with_well_names(Biomemetwo3_Franklink_fluorescence_data)

  #Step 3: Write csv with formatted fluorescence data to machine
  write.csv(formattedBiomemetwo3_Franklinkfluorescencedata, outputFileName, row.names=FALSE)

  }

  #Invalid File type
  else {
    return(print("Invalid File Type."))
  }


}






#' Add systemCalculatedThresholdValue, systemCalculatedCqValue, and userProvidedCqValue to
#' results_Table or standardCurveResults_Table files.
#'
#' @description Function adds systemCalculatedThresholdValue and systemCalculatedCqValue to
#' results_Table or standardCurveResults_Table files. User can also add userProvidedCqValue to
#' file by setting calculateUserProvidedCq parameter to "Yes". If user wants system to calculate
#' userProvidedCqValue they must provide userProvidedThresholdValue's.
#'
#' @param file results_Table or standardCurveResults_Table file that is read in.
#' @param calculateUserProvidedCq defines if user wants UserProvidedCq value to be calculated and
#' added to file. Update variable to "Yes" to have value added or "No" if not.
#'
#' @return Writes updated CSV file to local directory.
#'
#' @export
#'
#' @usage addThresholdCq(file, calculateUserProvidedCq)





addThresholdCq <- function(file, calculateUserProvidedCq){
  if (calculateUserProvidedCq == "Yes") {

  #Read In results_table file
  results_table <- read.csv(file)
  results_table <- results_table  %>% filter(is.na(Cycle_Number1)== FALSE)
  fluorescence_data <- results_table[ ,14:53]

  #Calculate systemCalculatedThresholdValue
  systemCalculatedthresholdValue <- calculate_threshold_value(fluorescence_data)
  colnames(systemCalculatedthresholdValue) <-"userProvidedThresholdValue"
  fluorescence_data$userProvidedCqValue <- ""

  #Calculate systemCalculatedCqValue
  fluorescence_data <- add_CqValue(fluorescence_data,
                                   systemCalculatedthresholdValue)

  #Add systemCalculatedThresholdValue and systemCalculatedCqValue to results_table
  results_table$systemCalculatedThresholdValue <- systemCalculatedthresholdValue$userProvidedThresholdValue

  results_table$systemCalculatedCqValue <- fluorescence_data$userProvidedCqValue

  #Calculate userProvidedCqValue
  userProvidedThreshold <- as.data.frame(results_table$userProvidedThresholdValue)
  colnames(userProvidedThreshold) <- "userProvidedThresholdValue"

  fluorescence_data <- add_CqValue(fluorescence_data,
                                   userProvidedThreshold)


  #Add userProvidedCqValue to results_table
  results_table$userProvidedCqValue <- fluorescence_data$userProvidedCqValue


  #Adjust extreme values
  results_table$userProvidedCqValue <- as.double(results_table$userProvidedCqValue)
  results_table$userProvidedCqValue[results_table$userProvidedCqValue < 0] <- 40
  results_table$userProvidedCqValue[results_table$userProvidedCqValue > 40] <- 40

  #write_updated file to current working directory
  write.csv(results_table, paste(strsplit(file, ".csv"), "_updated.csv", sep =""), na="")

  }

  else {
    #Read In results_table file
    results_table <- read.csv(file)
    results_table <- results_table  %>% filter(is.na(Cycle_Number1)== FALSE)
    fluorescence_data <- results_table[ ,14:53]

    #Calculate systemCalculatedThresholdValue
    systemCalculatedthresholdValue <- calculate_threshold_value(fluorescence_data)
    colnames(systemCalculatedthresholdValue) <-"userProvidedThresholdValue"
    fluorescence_data$userProvidedCqValue <- ""

    #Calculate systemCalculatedCqValue
    fluorescence_data <- add_CqValue(fluorescence_data,
                                     systemCalculatedthresholdValue)

    #Add systemCalculatedThresholdValue and systemCalculatedCqValue to results_table
    results_table$systemCalculatedThresholdValue <- systemCalculatedthresholdValue$userProvidedThresholdValue

    results_table$systemCalculatedCqValue <- fluorescence_data$userProvidedCqValue

    #write_updated file to current working directory
    write.csv(results_table, paste(strsplit(file, ".csv"), "_updated.csv", sep =""), na="")


  }

}



#Function to format raw MIC fluorescence file in matrix where each row
# represent a plate well and each column has the fluorescence value for a cycle.
process_MIC_uploaded_file <- function(fluorescence_file) {

  #Transpose fluorescence dataframe
  qpcr_MIC_fluorescence <- as.data.frame(t(fluorescence_file))

  #Change column names in fluorescence dataframe
  total_runs <- ncol(qpcr_MIC_fluorescence)
  colnames(qpcr_MIC_fluorescence) <-
    c(paste0("Cycle_Number", 1:total_runs))

  #Remove first row with cycle numbers
  qpcr_MIC_fluorescence <- qpcr_MIC_fluorescence[-c(1),]


  #change all values to numeric
  cols = c(1:total_runs)
  qpcr_MIC_fluorescence[, cols] <-
    apply(qpcr_MIC_fluorescence[, cols], 2, function(x)
      as.numeric(as.character(x)))
  str(qpcr_MIC_fluorescence)

  return(qpcr_MIC_fluorescence)

}



#Function to add new rows and column names to stepOneplus dataframe
reformat_SOP_Flur_data <- function (flur_data) {
  flur_df <- as.data.frame(matrix(nrow = 0, ncol = 40))
  colnames(flur_df) <-  c(paste0("Cycle_Number", 1:40))

  for (i in seq(from = 1, to = 3840, by = 40))
  {
    start <- i
    end <- i + 39

    one_row <- flur_data[1, start:end]
    colnames(one_row) <- c(paste0("Cycle_Number", 1:40))

    flur_df  <-  rbind(flur_df, one_row)

  }
  return(flur_df)
}


#Function to format raw StepOnePlus fluorescence file in matrix where each row
#represent a plate well and each column has the fluorescence value for a cycle.
process_SOP_uploaded_file <- function(fluorescence_file) {

  #Remove rows above main table
  fluorescence_file <- fluorescence_file[-c(1:6), ]

  #Extract first row with column names
  colnames(fluorescence_file) <- c(fluorescence_file[1,])

  #Remove first row of table since it has column names in it
  fluorescence_file <- fluorescence_file[-c(1),]

  #Only keep first 3 columns (application can only deal with one dye currently)
  fluorescence_file <- fluorescence_file[, 1:3]

  #Changing Cycle class to numeric
  fluorescence_file$Cycle <- as.numeric(fluorescence_file$Cycle)

  #Sort well names then cycles
  fluorescence_file <- fluorescence_file[order(fluorescence_file$Well, fluorescence_file$Cycle),]

  #Creating empty data frame to populate
  run_location <- as.data.frame(matrix(nrow = 96, ncol = 1))

  #Populating Well name column with unique well names (96 names)
  run_location[, 1] <- unique(fluorescence_file$Well)
  colnames(run_location) <- c("run_location")


  #Getting fluorescence values

  #Turning StepOnePlus raw fluorescence data in to 1 by 3840 matrix with just fluorescence data
  stepOnePlus_fluorescence_matrix <-  as.data.frame(t(fluorescence_file[,-c(1:2)]))

  #Creating matrix, each row is a sample with the fluorescene values for each cycle
  SOP_flur <- reformat_SOP_Flur_data(stepOnePlus_fluorescence_matrix)

  return(SOP_flur)

}


#Function to format raw StepOnePlus fluorescence file in matrix where each row
#represent a plate well and each column has the fluorescence value for a cycle, with well names.
process_SOP_uploaded_file_with_well_names <- function(fluorescence_file) {

  #Remove rows above main table
  fluorescence_file <- fluorescence_file[-c(1:6), ]

  #Extract first row with column names
  colnames(fluorescence_file) <- c(fluorescence_file[1,])

  #Remove first row of table since it has column names in it
  fluorescence_file <- fluorescence_file[-c(1),]

  #Only keep first 3 columns (application can only deal with one dye currently)
  fluorescence_file <- fluorescence_file[, 1:3]

  #Changing Cycle class to numeric
  fluorescence_file$Cycle <- as.numeric(fluorescence_file$Cycle)

  #Sort well names then cycles
  fluorescence_file <- fluorescence_file[order(fluorescence_file$Well, fluorescence_file$Cycle),]

  #Creating empty data frame to populate
  run_location <- as.data.frame(matrix(nrow = 96, ncol = 1))

  #Populating Well name column with unique well names (96 names)
  run_location[, 1] <- unique(fluorescence_file$Well)
  colnames(run_location) <- c("run_location")


  #Getting fluorescence values

  #Turning StepOnePlus raw fluorescence data in to 1 by 3840 matrix with just fluorescence data
  stepOnePlus_fluorescence_matrix <-  as.data.frame(t(fluorescence_file[,-c(1:2)]))

  #Creating matrix, each row is a sample with the fluorescene values for each cycle
  SOP_flur <- reformat_SOP_Flur_data(stepOnePlus_fluorescence_matrix)

  SOP_flur$wellLocation <-  run_location$run_location

  SOP_flur <- SOP_flur %>% select(41, 1:40)

  return(SOP_flur)

}



#Function to format raw Biomeme two3/Franklin fluorescence file in matrix where each row
# represent a plate well and each column has the fluorescence value for a cycle.
process_biomeme23_uploaded_file <- function(fluorescence_file) {

  #Creating dataframe with fluorescence values and ct intensity value from biomem raw qPCR file
  end_row <-(which(grepl('Raw Fluorescence', fluorescence_file$Run.Name))) - 2

  qpcr_biomem23_fluorescence <- fluorescence_file[11:end_row, 2:41]


  #Changing column names to consistent with naming covention in database
  total_runs <- ncol(qpcr_biomem23_fluorescence)
  colnames(qpcr_biomem23_fluorescence) <- c(paste0("Cycle_Number", 1:total_runs))


  #change all values to numeric
  cols = c(1:total_runs)
  qpcr_biomem23_fluorescence[, cols] <- apply(qpcr_biomem23_fluorescence[, cols], 2, function(x)  as.numeric(as.character(x)))

  return (qpcr_biomem23_fluorescence)
}


#Function to format raw Biomeme two3/Franklin fluorescence file in matrix where each row
# represent a plate well and each column has the fluorescence value for a cycle with well names.
process_biomeme23_uploaded_file_with_well_names <- function(fluorescence_file) {

  #Creating dataframe with fluorescence values from biomem raw qPCR file
  end_row <-(which(grepl('Raw Fluorescence', fluorescence_file$Run.Name))) - 2
  qpcr_biomem23_fluorescence <- fluorescence_file[11:end_row, 2:41]

  #Changing column names to consistent with naming covention in database
  total_runs <- ncol(qpcr_biomem23_fluorescence)
  colnames(qpcr_biomem23_fluorescence) <- c(paste0("Cycle_Number", 1:total_runs))


  #change all values to numeric
  cols = c(1:total_runs)
  qpcr_biomem23_fluorescence[, cols] <- apply(qpcr_biomem23_fluorescence[, cols], 2, function(x)  as.numeric(as.character(x)))

  qpcr_biomem23_fluorescence$wellLocation <- fluorescence_file[11:end_row, 1]

  qpcr_biomem23_fluorescence <- qpcr_biomem23_fluorescence %>% select(41, 1:40)

  return (qpcr_biomem23_fluorescence)
}


#Function is used to calculated Cq value from a set of fluorescence data
add_CqValue <- function(flur_file, meta_file) {

  #remove one to exclude CqValue column
  cycle_number <- seq(ncol(flur_file) - 1)

  #Number of samples
  number_of_rows <- nrow(flur_file)

  for (i in 1:number_of_rows) {

    if (meta_file$userProvidedThresholdValue[i] == "Unable to Determine Threshold")
    {flur_file$userProvidedCqValue[i] <- 40}

    else{

      flur_file$userProvidedCqValue[i] <-
        th.cyc(
          cycle_number,
          as.numeric(flur_file[i, -c(ncol(flur_file))]),
          r = round(as.numeric(
            meta_file$userProvidedThresholdValue
          )[i], 3),
          linear = TRUE
        )[1]


    }}

  return(flur_file)

}
