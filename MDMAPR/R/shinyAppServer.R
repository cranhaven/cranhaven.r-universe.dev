#' @import RMySQL
#' @import shinydashboard
#' @importFrom DBI dbGetQuery
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @import leaflet
#' @import leaflet.extras
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom shinyjs reset
#' @importFrom shinyjs alert
#' @importFrom shinyjs useShinyjs
#' @import ggplot2
#' @import dplyr
#' @import readxl
#' @import reactable
#' @import writexl
#' @importFrom xfun file_ext
#' @importFrom berryFunctions is.error
#' @importFrom plotly plotlyOutput
#' @importFrom plotly style
#' @importFrom plotly layout
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @import htmltools
#' @importFrom shiny div
#' @importFrom shiny downloadHandler
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny need
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveVal
#' @importFrom shiny renderText
#' @importFrom shiny req
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateSliderInput
#' @importFrom shiny validate
#' @importFrom shiny a
#' @importFrom shiny actionButton
#' @importFrom shiny br
#' @importFrom shiny column
#' @importFrom shiny downloadLink
#' @importFrom shiny em
#' @importFrom shiny fileInput
#' @importFrom shiny fluidRow
#' @importFrom shiny h1
#' @importFrom shiny h3
#' @importFrom shiny h4
#' @importFrom shiny HTML
#' @importFrom shiny icon
#' @importFrom shiny numericInput
#' @importFrom shiny p
#' @importFrom shiny radioButtons
#' @importFrom shiny selectInput
#' @importFrom shiny sliderInput
#' @importFrom shiny strong
#' @importFrom shiny tabPanel
#' @importFrom shiny tabsetPanel
#' @importFrom shiny shinyApp
#' @importFrom shiny textOutput
#' @import methods
#' @importFrom utils read.csv
#' @importFrom utils str
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils zip
#' @importFrom stats family
#' @importFrom stats lm
#' @importFrom stats mad
#' @importFrom stats na.exclude
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats residuals
#' @importFrom bslib is_bs_theme
#' @import htmlwidgets


shinyAppServer <- function(input, output, session) {


  #MySQL Database connection and table querying ---------------------------

  #Running the MDMAPR 2.0 without a database connection: dbExists <- 'No'.
  #Running the MDMAPR 2.0 with a database connection: dbExists <- 'Yes'. (NOTE: Update the 'user', 'password', 'dbname', and 'host' variables in the dbConnect statement (line 44) to reference a specific database instance.)
  dbExists <-  db_int

  #MySQL connection to MDMAPR 2.0 Database
  if (dbExists == 'Yes') {

    #MySQL connection to MDMAPR Database
    con <- dbConnect(MySQL(), user = db_user, password = db_password, dbname = db_name, host = db_host)

    #Retrieving project table
    myQuery1 <- "select * from project_Table"
    project_data <- dbGetQuery(con, myQuery1)
    str(project_data)

    #Retrieving geographic region table
    myQuery2 <- "select * from geographicRegion_Table"
    geo_data <- dbGetQuery(con, myQuery2)
    str(geo_data)

    #Retrieving site table
    myQuery3 <- "select * from site_Table"
    site_data <- dbGetQuery(con, myQuery3)
    str(site_data)

    #Retrieving station table
    myQuery4 <- "select * from station_Table"
    station_data <- dbGetQuery(con, myQuery4)
    str(station_data)

    #Retrieving replicate table
    myQuery5 <- "select * from replicate_Table"
    replicate_data <- dbGetQuery(con, myQuery5)
    str(replicate_data)

    #Retrieving extract table
    myQuery6 <- "select * from extract_Table"
    extract_data <- dbGetQuery(con, myQuery6)
    str(extract_data)

    #Retrieving results table
    myQuery7 <- "select * from results_Table"
    result_data <- dbGetQuery(con, myQuery7)
    str(result_data)

    #Retrieving pcr chemistry table
    myQuery8 <- "select * from pcrChemistry_Table"
    pcrChemistry_data <- dbGetQuery(con, myQuery8)
    str(pcrChemistry_data)

    #Retrieving run information table
    myQuery9 <- "select * from runInformation_Table"
    runInformation_data <- dbGetQuery(con, myQuery9)
    str(runInformation_data)

    #Retrieving assay table
    myQuery10 <- "select * from assay_Table"
    assay_data <- dbGetQuery(con, myQuery10)
    str(assay_data)

    #Retrieving taxon table
    myQuery11 <- "select * from taxonomic_Table"
    taxon_data <- dbGetQuery(con, myQuery11)
    str(taxon_data)

    #Retrieving standard curve table
    myQuery12 <- "select * from standardCurve_Table"
    standardCurve_data <- dbGetQuery(con, myQuery12)
    str(standardCurve_data)

    #Retrieving standard curve results table
    myQuery13 <- "select * from standardCurveResults_Table"
    standardCurveResults_data <- dbGetQuery(con, myQuery13)
    str(standardCurveResults_data)

    #Disconnecting database after data has been read in.
    dbDisconnect(con)


    #Merge Experimental qPCR Data from MySQL tables ---------------------------
    my_sql_data <- left_join(result_data,  runInformation_data, by = "runID")
    my_sql_data <- left_join(my_sql_data,  pcrChemistry_data, by = "pcrChemistryID")
    my_sql_data <- left_join(my_sql_data,  assay_data, by = "assayID")
    my_sql_data <- left_join(my_sql_data,  taxon_data, by = "taxonID")
    my_sql_data <- left_join(my_sql_data,  extract_data, by = "extractID")
    my_sql_data <- left_join(my_sql_data, replicate_data, by = "replicateID")
    my_sql_data <- left_join(my_sql_data, station_data, by = "stationID")
    my_sql_data <- left_join(my_sql_data, site_data, by = "siteID" )
    my_sql_data <- left_join(my_sql_data, geo_data, by = "geographicRegionID")
    my_sql_data <- left_join(my_sql_data, project_data, by = "projectID")
    my_sql_data <- left_join(my_sql_data, standardCurve_data, by = "assayID")


    # Merge standardCurve Data from MySQL tables ---------------------------
    mysql_standardCurve_data <- left_join(standardCurveResults_data, runInformation_data, by = "runID")
    mysql_standardCurve_data  <- left_join(mysql_standardCurve_data , pcrChemistry_data, by = "pcrChemistryID")
    mysql_standardCurve_data  <- left_join(mysql_standardCurve_data , standardCurve_data, by = "standardCurveID")


    #The qPCR intensity can be categorized into five intensity levels including “none” (absence), “weak”, “moderate”, “strong”, “very strong”. In code below we are adding CqIntensity column to table based on user provided Cq values and adding another column called CqIntensitySystemCalculated based on system calculated Cq values. The intensity ranges include  0<=very strong <10, 10<= strong <20, "20 <= Moderate < 30", "30 <= Weak < 40",  and "None > 40".
    my_sql_data$CqIntensity <- cut(my_sql_data$userProvidedCqValue,
                                   breaks = c(0, 10, 20, 30, 40, 1000), right = FALSE,
                                   labels = c("0 < Very strong < 10",
                                              "10 <= Strong < 20",
                                              "20 <= Moderate < 30",
                                              "30 <= Weak < 40",
                                              "None > 40"))

    my_sql_data$CqIntensitySystemCalculated <- cut(my_sql_data$systemCalculatedCqValue,
                                                   breaks = c(0, 10, 20, 30, 40, 1000),
                                                   right = FALSE,
                                                   labels = c("0 < Very strong < 10",
                                                              "10 <= Strong < 20",
                                                              "20 <= Moderate < 30",
                                                              "30 <= Weak < 40",
                                                              "None > 40"))


    #Reformat date fields to be of the class type "Date"
    my_sql_data$runDate <- as.Date(my_sql_data$runDate, "%Y-%m-%d")
    my_sql_data$extractionDate <- as.Date(my_sql_data$extractionDate, "%Y-%m-%d")
    my_sql_data$assayDate <- as.Date(my_sql_data$assayDate, "%Y-%m-%d")
    my_sql_data$projectCreationDate <- as.Date(my_sql_data$projectCreationDate, "%Y-%m-%d")
    my_sql_data$collectionDate <- as.Date(my_sql_data$collectionDate, "%Y-%m-%d")
    my_sql_data$projectCreationDate <- as.Date(my_sql_data$projectCreationDate, "%Y-%m-%d")
    mysql_standardCurve_data$runDate <- as.Date(mysql_standardCurve_data$runDate, "%Y-%m-%d")

    #Remove LOD and LOQ variables from main table
    my_sql_data <- my_sql_data[ , -c(213, 214)]  }



  #Data Submission page functions  ---------------------------

  #Function takes in user uploaded data and parses it into Project table.
  create_project_table <- function(uploaded_data) {
    project_table <-
      distinct(uploaded_data[, c(
        "projectID",
        "projectCreationDate",
        "projectName",
        "projectRecordedBy",
        "projectOwner",
        "projectContactEmail",
        "projectDescription",
        "InstitutionID",
        "projectDataNotes"
      )])

    return(project_table)
  }



  #Function takes in user uploaded data and parses it into Geographic Region table.
  create_geographicRegion_Table <- function(uploaded_data) {
    geographicRegion_table <-
      distinct(uploaded_data[, c(
        "geographicRegionID",
        "projectID",
        "continent",
        "country",
        "stateProvince",
        "municipality"
      )])

    return(geographicRegion_table)
  }


  #Function takes in user uploaded data and parses it into Site table.
  create_site_Table <- function(uploaded_data) {
    site_table <-
      distinct(uploaded_data[, c(
        "siteID",
        "geographicRegionID",
        "locality",
        "estimatedPerimeter",
        "estimatedSurfaceArea(m2)",
        "siteType",
        "siteLength(m2)"
      )])

    return(site_table)
  }



  #Function takes in user uploaded data and parses it into Station table.
  create_station_Table <- function(uploaded_data) {
    station_Table <-
      distinct(uploaded_data[, c("stationID",
                                 "siteID",
                                 "stationName",
                                 "decimalLongitude",
                                 "decimalLatitude")])

    return(station_Table)
  }



  #Function takes in user uploaded data and parses it into Replicate table.
  create_replicate_Table <- function(uploaded_data) {
    replicate_Table <-
      distinct(uploaded_data[, c(
        "replicateID",
        "stationID",
        "collectorName",
        "replicateName",
        "collectionDate",
        "collectionTime",
        "storageID",
        "DateOfStorage",
        "methodOfStorage",
        "minimumElevationInMeters",
        "maximumElevationInMeters",
        "verbatimElevation",
        "minimumDepthInMeters",
        "maximumDepthInMeters",
        "verbatimDepth",
        "flowRate(m/s)",
        "filterType",
        "filtrationDuration(mins)",
        "volumeFiltered",
        "processLocation",
        "replicationNumber",
        "riparianVegetationPercentageCover",
        "dissolvedOxygen(mg/L)",
        "waterTemperature(C)",
        "pH",
        "TSS(mg/L)",
        "EC(uS/cm)",
        "turbidity(NTU)",
        "discharge",
        "tide",
        "chlorophyl",
        "salinity(ppt)",
        "contaminants(ng/g)",
        "traceMetals(mg/kg)",
        "organicContent(%)",
        "microbialActivity",
        "grainSize",
        "replicateDataNotes"
      )])

    return(replicate_Table)
  }



  #Function takes in user uploaded data and parses it into Extract table.
  create_extract_Table <- function(uploaded_data) {
    extract_Table <-
      distinct(uploaded_data[, c(
        "extractID",
        "replicateID",
        "extractName",
        "analyst",
        "extractionDate",
        "extractionTime",
        "location",
        "extractionMethod",
        "methodCitation",
        "extractionNotes",
        "tubePlateID",
        "frozen",
        "fixed",
        "dnaStorageLocation",
        "extractMethodOfStorage",
        "dnaVolume",
        "quantificationMethod",
        "concentration(ng/ul)"
      )])

    return(extract_Table)
  }


  ##Function takes in user uploaded data and parses it into Result table.
  create_results_Table <- function(uploaded_data) {
    results_Table <-
      uploaded_data[, c(
        "resultID",
        "runID",
        "assayID",
        "pcrChemistryID",
        "extractID",
        "wellLocation",
        "sampleName",
        "copyNumber",
        "control",
        "userProvidedThresholdValue",
        "userProvidedCqValue",
        "systemCalculatedThresholdValue",
        "systemCalculatedCqValue",
        "Cycle_Number1",
        "Cycle_Number2",
        "Cycle_Number3",
        "Cycle_Number4",
        "Cycle_Number5",
        "Cycle_Number6",
        "Cycle_Number7",
        "Cycle_Number8",
        "Cycle_Number9",
        "Cycle_Number10",
        "Cycle_Number11",
        "Cycle_Number12",
        "Cycle_Number13",
        "Cycle_Number14",
        "Cycle_Number15",
        "Cycle_Number16",
        "Cycle_Number17",
        "Cycle_Number18",
        "Cycle_Number19",
        "Cycle_Number20",
        "Cycle_Number21" ,
        "Cycle_Number22",
        "Cycle_Number23",
        "Cycle_Number24",
        "Cycle_Number25",
        "Cycle_Number26",
        "Cycle_Number27",
        "Cycle_Number28",
        "Cycle_Number29",
        "Cycle_Number30",
        "Cycle_Number31",
        "Cycle_Number32",
        "Cycle_Number33",
        "Cycle_Number34",
        "Cycle_Number35",
        "Cycle_Number36",
        "Cycle_Number37",
        "Cycle_Number38",
        "Cycle_Number39",
        "Cycle_Number40",
        "Cycle_Number41",
        "Cycle_Number42",
        "Cycle_Number43",
        "Cycle_Number44",
        "Cycle_Number45",
        "Cycle_Number46",
        "Cycle_Number47",
        "Cycle_Number48",
        "Cycle_Number49",
        "Cycle_Number50",
        "Cycle_Number51",
        "Cycle_Number52",
        "Cycle_Number53",
        "Cycle_Number54",
        "Cycle_Number55",
        "Cycle_Number56",
        "Cycle_Number57",
        "Cycle_Number58",
        "Cycle_Number59",
        "Cycle_Number60",
        "Cycle_Number61",
        "Cycle_Number62",
        "Cycle_Number63",
        "Cycle_Number64",
        "Cycle_Number65",
        "Cycle_Number66",
        "Cycle_Number67",
        "Cycle_Number68",
        "Cycle_Number69",
        "Cycle_Number70"
      )]

    return(results_Table)
  }



  #Function takes in user uploaded data and parses it into Assay table.
  create_assay_Table <- function(uploaded_data) {
    assay_table <-
      distinct(uploaded_data[, c(
        "assayID",
        "taxonID",
        "establishmentMeans",
        "assayName",
        "assayOwnership",
        "assayDescription",
        "assayCitation",
        "assayDate",
        "geneTarget",
        "geneSymbol",
        "dilutions",
        "replicates",
        "primerR",
        "primerF",
        "probe",
        "ampliconLength (bp)",
        "probeFluorescentTag",
        "dye(s)",
        "quencher",
        "probeModification"
      )])

    return(assay_table)

  }



  #Function takes in user uploaded data and parses it into Taxon table.
  create_taxon_Table <- function(uploaded_data) {
    taxon_table <-
      distinct(uploaded_data[, c(
        "taxonID",
        "kingdom",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "subgenus",
        "species",
        "vernacularName",
        "organismScope"
      )])

    return(taxon_table)

  }



  ##Function takes in user uploaded data and parses it into PCR Chemsitry table.
  create_pcrChemistry_Table <- function(uploaded_data, SC_uploaded_data) {

    pcrChemistry_table <-
      distinct(uploaded_data[, c(
        "pcrChemistryID",
        "reactionConditions",
        "reactionVolume",
        "templateAmount",
        "forwardPrimerBatch",
        "reversePrimerBatch",
        "dNTPConcentration",
        "primerConcentration",
        "probeConcentration",
        "Mg2+Concentration",
        "polymeraseBatch",
        "polymeraseConcentrations",
        "thermocyclerParameters",
        "pcrDataNotes"
      )])


    SCpcrChemistry_table <-
      distinct(SC_uploaded_data[, c(
        "pcrChemistryID",
        "reactionConditions",
        "reactionVolume",
        "templateAmount",
        "forwardPrimerBatch",
        "reversePrimerBatch",
        "dNTPConcentration",
        "primerConcentration",
        "probeConcentration",
        "Mg2+Concentration",
        "polymeraseBatch",
        "polymeraseConcentrations",
        "thermocyclerParameters",
        "pcrDataNotes"
      )])


    pcrChemistry_table <-
      rbind(pcrChemistry_table, SCpcrChemistry_table)

    return(pcrChemistry_table)

  }



  #Function takes in user uploaded data and parses it into Standard Curve table.
  create_standardCurve_Table <- function(uploaded_data) {
    standardCurve_Table <-
      distinct(uploaded_data[, c(
        "standardCurveID",
        "assayID",
        "standardCurveName",
        "SCdate",
        "SCrecordedBy",
        "SCdataNotes",
        "LOD",
        "LOQ"
      )])

    return(standardCurve_Table)

  }



  #Function takes in user uploaded data and parses it into Standard Curve Result table.
  create_standardCurveResults_Table <- function(uploaded_data) {
    standardCurveResults_Table <-
      uploaded_data[, c(
        "SCresultID",
        "runID",
        "pcrChemistryID",
        "standardCurveID",
        "wellLocation",
        "sampleName",
        "copyNumber",
        "control",
        "standardConc",
        "userProvidedThresholdValue",
        "userProvidedCqValue",
        "systemCalculatedThresholdValue",
        "systemCalculatedCqValue",
        "Cycle_Number1",
        "Cycle_Number2",
        "Cycle_Number3",
        "Cycle_Number4",
        "Cycle_Number5",
        "Cycle_Number6",
        "Cycle_Number7",
        "Cycle_Number8",
        "Cycle_Number9",
        "Cycle_Number10",
        "Cycle_Number11",
        "Cycle_Number12",
        "Cycle_Number13",
        "Cycle_Number14",
        "Cycle_Number15",
        "Cycle_Number16",
        "Cycle_Number17",
        "Cycle_Number18",
        "Cycle_Number19",
        "Cycle_Number20",
        "Cycle_Number21",
        "Cycle_Number22",
        "Cycle_Number23",
        "Cycle_Number24",
        "Cycle_Number25",
        "Cycle_Number26",
        "Cycle_Number27",
        "Cycle_Number28",
        "Cycle_Number29",
        "Cycle_Number30",
        "Cycle_Number31",
        "Cycle_Number32",
        "Cycle_Number33",
        "Cycle_Number34",
        "Cycle_Number35",
        "Cycle_Number36",
        "Cycle_Number37",
        "Cycle_Number38",
        "Cycle_Number39",
        "Cycle_Number40",
        "Cycle_Number41",
        "Cycle_Number42",
        "Cycle_Number43",
        "Cycle_Number44",
        "Cycle_Number45",
        "Cycle_Number46",
        "Cycle_Number47",
        "Cycle_Number48",
        "Cycle_Number49",
        "Cycle_Number50",
        "Cycle_Number51",
        "Cycle_Number52",
        "Cycle_Number53",
        "Cycle_Number54",
        "Cycle_Number55",
        "Cycle_Number56",
        "Cycle_Number57",
        "Cycle_Number58",
        "Cycle_Number59",
        "Cycle_Number60",
        "Cycle_Number61",
        "Cycle_Number62",
        "Cycle_Number63",
        "Cycle_Number64",
        "Cycle_Number65",
        "Cycle_Number66",
        "Cycle_Number67",
        "Cycle_Number68" ,
        "Cycle_Number69",
        "Cycle_Number70"
      )]

    return(standardCurveResults_Table)

  }



  #Function takes in user uploaded data and parses it into Run Information table.
  create_runInformation_Table <-
    function(uploaded_data, SC_uploaded_data) {
      runInformation_Table <-
        distinct(uploaded_data[, c("runID",
                                   "runRecordedBy",
                                   "runDate",
                                   "runTime",
                                   "runPlatform",
                                   "machineID")])

      runInformation_Table2 <-
        distinct(SC_uploaded_data[, c("runID",
                                      "runRecordedBy",
                                      "runDate",
                                      "runTime",
                                      "runPlatform",
                                      "machineID")])


      runInformation_Table <-
        rbind(runInformation_Table, runInformation_Table2)

      return(runInformation_Table)

    }



  #Uploaded User File Validation functions ---------------------------

  #Funtion that is opposite on 'is in' function
  '%ni%' <- Negate('%in%')

  #Function runs validation tests on user uploaded fluorescence file and metadata file to see if they are the correct file types and if they are compatible with each other.
  user_uploaded_file_validate <- function(fluor_file, metadata_file, platform_type){

    if (is.null(fluor_file) | is.null(metadata_file))
    {return(TRUE)}

    else if (file_ext(fluor_file$datapath) %ni% c("csv", "xlsx", "xls"))
    {return(TRUE)}

    else if (platform_type == 'None')
    {return(TRUE)}

    #Both uploaded files must be csv format
    else if (file_ext(metadata_file$datapath) %ni% c("xlsx", "xls"))
    {return(TRUE)}


    else if (length(excel_sheets(metadata_file$datapath)) < 5)
    {return(TRUE)}


    #Checks to see if any of the sheets are empty (i.e: only contain field names)
    else if (nrow(read_excel(metadata_file$datapath, sheet = 1)) == 0 |
             nrow(read_excel(metadata_file$datapath, sheet = 2)) == 0 |
             nrow(read_excel(metadata_file$datapath, sheet = 3)) == 0 |
             nrow(read_excel(metadata_file$datapath, sheet = 4)) == 0 |
             nrow(read_excel(metadata_file$datapath, sheet = 5)) == 0)
    {return(TRUE)}


    #If files are MIC
    else if (platform_type == "MIC")
    {if (file_ext(fluor_file$datapath) != "csv")
    {return(TRUE)}


      #check if fluorescence file if biomeme file
      else if (read.csv(fluor_file$datapath)[1, 1] == 'Date & Time')
      {return(TRUE)}


      #Check is fluorescence file processing  function work
      else if (is.error(process_MIC_uploaded_file(read.csv(fluor_file$datapath))) == TRUE)
      {return(TRUE)}

      #Check is metadata parsing function works on file
      else if (is.error(format_qPCR_metadata(metadata_file$datapath)) == TRUE)
      {return(TRUE)}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_MIC_uploaded_file(read.csv(fluor_file$datapath))) != nrow(format_qPCR_metadata(metadata_file$datapath)))
      {return(TRUE)}


      else
      {return(FALSE)} }


    #If files are step one plus
    else if (platform_type == "StepOnePlus")
    {if (file_ext(fluor_file$datapath) %ni% c("xlsx", "xls"))
    {return(TRUE)}


      #Check is fluorescence file processing  function work
      else if (is.error(process_SOP_uploaded_file(read_excel(fluor_file$datapath, 4))) == TRUE)
      {return(TRUE)}


      #Check is metadata parsing function works on file
      else if (is.error(format_qPCR_metadata(metadata_file$datapath)) == TRUE)
      {return(TRUE)}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_SOP_uploaded_file(read_excel(fluor_file$datapath, 4))) != nrow(format_qPCR_metadata(metadata_file$datapath)))
      {return(TRUE)}


      else
      {return(FALSE)} }


    #If files are step one plus Biomeme two3/Franklin
    else if (platform_type ==  "Biomeme two3/Franklin")
    {if (file_ext(fluor_file$datapath) != "csv")
    {return(TRUE)}


      #Check is fluorescence file processing  function work
      else if (is.error(process_biomeme23_uploaded_file(read.csv(fluor_file$datapath))) == TRUE)
      {return(TRUE)}


      #Check is metadata parsing function works on file
      else if (is.error(format_qPCR_metadata(metadata_file$datapath)) == TRUE)
      {return(TRUE)}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_biomeme23_uploaded_file(read.csv(fluor_file$datapath))) != nrow(format_qPCR_metadata(metadata_file$datapath)))
      {return(TRUE)}


      else
      {return(FALSE)}
    }


    else {return(FALSE)}

  }



  #Function runs validation tests on user uploaded standard curve file.

  user_uploaded_standard_curve_file_validation <- function(fluor_file, metadata_file, platform_type){

    if (is.null(fluor_file) | is.null(metadata_file))
    {return(TRUE)}

    else if (file_ext(fluor_file$datapath) %ni% c("csv", "xlsx", "xls"))
    {return(TRUE)}

    else if (platform_type == 'None')
    {return(TRUE)}

    else if (file_ext(metadata_file$datapath) %ni% c("xlsx", "xls"))
    {return(TRUE)}

    else if (length(excel_sheets(metadata_file$datapath)) < 5)
    {return(TRUE)}


    #Checks to see if any of the sheets are empty (i.e: only contain field names)
    else if (nrow(read_excel(metadata_file$datapath, sheet = 1)) == 0 |
             nrow(read_excel(metadata_file$datapath, sheet = 2)) == 0 |
             nrow(read_excel(metadata_file$datapath, sheet = 3)) == 0 |
             nrow(read_excel(metadata_file$datapath, sheet = 4)) == 0 |
             nrow(read_excel(metadata_file$datapath, sheet = 5)) == 0)
    {return(TRUE)}


    #If files are MIC
    else if (platform_type == "MIC")
    {if (file_ext(fluor_file$datapath) != "csv")
    {return(TRUE)}


      #check if fluorescence file if biomeme file
      else if (read.csv(fluor_file$datapath)[1, 1] == 'Date & Time')
      {return(TRUE)}


      #Check is fluorescence file processingfunction work
      else if (is.error(process_MIC_uploaded_file(read.csv(fluor_file$datapath))) == TRUE)
      {return(TRUE)}


      #Check is metadata parsing function works on file
      else if (is.error(format_standardCurve_metadata(read_xlsx(metadata_file$datapath, sheet = 5))) == TRUE)
      {return(TRUE)}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_MIC_uploaded_file(read.csv(fluor_file$datapath))) != nrow(format_standardCurve_metadata(read_xlsx(metadata_file$datapath, sheet = 5))))
      {return(TRUE)}


      else
      {return(FALSE)}
    }


    #If files are step one plus
    else if (platform_type == "StepOnePlus")
    {if (file_ext(fluor_file$datapath) %ni% c("xlsx", "xls"))
    {return(TRUE)}


      #Check is fluorescence file processing  function work
      else if (is.error(process_SOP_uploaded_file(read_excel(fluor_file$datapath, 4))) == TRUE)
      {return(TRUE)}


      #Check is metadata parsing function works on file
      else if (is.error(format_standardCurve_metadata(read_xlsx(metadata_file$datapath, sheet = 5))) == TRUE)
      {return(TRUE)}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_SOP_uploaded_file(read_excel(fluor_file$datapath, 4))) != nrow(format_standardCurve_metadata(read_xlsx(metadata_file$datapath, sheet = 5))))
      {return(TRUE)}


      else
      {return(FALSE)}
    }


    #If files are step one plus Biomeme two3/Franklin
    else if (platform_type ==  "Biomeme two3/Franklin")
    {if (file_ext(fluor_file$datapath) != "csv")
    {return(TRUE)}


      #Check is fluorescence file processing  function work
      else if (is.error(process_biomeme23_uploaded_file(read.csv(fluor_file$datapath))) == TRUE)
      {return(TRUE)}


      #Check is metadata parsing function works on file
      else if (is.error(format_standardCurve_metadata(read_xlsx(metadata_file$datapath, sheet = 5))) == TRUE)
      {return(TRUE)}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_biomeme23_uploaded_file(read.csv(fluor_file$datapath))) != nrow(format_standardCurve_metadata(read_xlsx(metadata_file$datapath, sheet = 5))))
      {return(TRUE)}


      else
      {return(FALSE)} }


    else {return(FALSE)}

  }



  #Function to give pop-up validation messages for uploaded fluorescence file.
  fluorescence_file_validation_msgs <- function(flur_file) {

    if (file_ext(flur_file$datapath) %ni% c("csv", "xlsx", "xls"))
    {shinyjs::alert("ERROR: Fluorescence file is not an accepted file type.")}

    else {shinyjs::alert("Successful file upload.")}

  }


  #Function to give pop-up validation messages for uploaded metatdata file.
  metadata_file_validation_msgs <- function(meta_file){

    if (file_ext(meta_file$datapath) %ni% c("xlsx", "xls"))
    {shinyjs::alert("ERROR: Metadata file is not an accepted file type.")}

    #Check how many sheets are in uploaded metadata file
    else if (length(excel_sheets(meta_file$datapath)) < 5)
    {shinyjs::alert("ERROR: Incorrect Metadata template used.")}


    #Checks to see if any of the sheets are empty (i.e: only contain field names)
    else if (nrow(read_excel(meta_file$datapath, sheet = 1)) == 0 |
             nrow(read_excel(meta_file$datapath, sheet = 2)) == 0 |
             nrow(read_excel(meta_file$datapath, sheet = 3)) == 0 |
             nrow(read_excel(meta_file$datapath, sheet = 4)) == 0 |
             nrow(read_excel(meta_file$datapath, sheet = 5)) == 0)
    {shinyjs::alert("ERROR: One or more required sheets on Metadata file is empty.")}


    #Check is metadata parsing function works on file
    else if (is.error(format_qPCR_metadata(meta_file$datapath)) == TRUE)
    {shinyjs::alert("ERROR: Metadata file is missing one or more necessary columns.")}


    else {shinyjs::alert("Successful file upload.")}}


  #Function to give pop-up validation messages for fluorescence file and metadata file based on selected machine type.
  selected_platform_validation_msgs <- function(flur_file, meta_file, platform){

    #If files are MIC
    if (platform == "MIC")

    {if (file_ext(flur_file$datapath) != "csv")
    {shinyjs::alert("ERROR: MIC fluorescence file must be csv.")}


      #check if fluorescence file if biomeme file
      else if (read.csv(flur_file$datapath)[1, 1] == 'Date & Time')
      {shinyjs::alert("ERROR: Fluorescence file is not correct file type for MIC platform.")}


      #Check is fluorescence file processing  function work
      else if (is.error(process_MIC_uploaded_file(read.csv(flur_file$datapath))) == TRUE)
      {return(shinyjs::alert("ERROR: Fluorescence file is not correct file type for MIC platform."))}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_MIC_uploaded_file(read.csv(flur_file$datapath))) != nrow(format_qPCR_metadata(meta_file$datapath)))
      {shinyjs::alert("ERROR: Fluorescence file and metadata file have information for different number of wells.")}

      else {shinyjs::alert("Successful platform selected.")}}



    else if (platform == "StepOnePlus")

    {if (file_ext(flur_file$datapath) %ni% c("xlsx", "xls"))
    {shinyjs::alert("ERROR: Step One Plus fluorescence file must be xlsx/xls.")}

      #Check is fluorescence file processingfunction work
      else if (is.error(process_SOP_uploaded_file(read_excel(flur_file$datapath, 4))) == TRUE)
      {return(shinyjs::alert("ERROR: Fluorescence file is not correct file type for Step One Plus platform."))}

      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_SOP_uploaded_file(read_excel(flur_file$datapath, sheet = 4))) != nrow(format_qPCR_metadata(meta_file$datapath)))
      {shinyjs::alert("ERROR: Fluorescence file and metadata file have information for different number of wells.")}

      else {shinyjs::alert("Successful platform selected.")}}



    #If files are Biomeme two3/Franklin
    else if (platform == "Biomeme two3/Franklin")

    {if (file_ext(flur_file$datapath) != "csv")
    {shinyjs::alert("ERROR: Biomeme two3/Franklin fluorescence file must be csv.")}


      #Check is fluorescence file processing  function work
      else if (is.error(process_biomeme23_uploaded_file(read.csv(flur_file$datapath))) == TRUE)
      {return(shinyjs::alert("ERROR: Fluorescence file is not correct file type for Biomeme two3/Franklin platform."))}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_biomeme23_uploaded_file(read.csv(flur_file$datapath))) != nrow(format_qPCR_metadata(meta_file$datapath)))
      {shinyjs::alert("ERROR: Fluorescence file and metadata file have information for different number of wells.")}

      else {shinyjs::alert("Successful platform selected.")}}


    #qPCR platform not selected
    else {
      #shinyjs::alert("Please select qPCR platform type.")
    }

  }



  #Function to give pop-up validation messages for fluorescence file,standard curve fluorescence file, and metadata file based on selected machine type
  selected_platform_validation_msgs_SC_and_Experimental_flur <- function(flur_file, SC_flur_file, meta_file, platform) {

    #If files are MIC
    if (platform == "MIC") {


      if (file_ext(flur_file$datapath) != "csv")
      {shinyjs::alert("ERROR: MIC fluorescence file must be csv.")}


      else if (file_ext(SC_flur_file$datapath) != "csv")
      {shinyjs::alert("ERROR: MIC standard curve fluorescence file must be csv.")}


      #check if fluorescence file if biomeme file
      else if (read.csv(flur_file$datapath)[1, 1] == 'Date & Time')
      {shinyjs::alert("ERROR: Fluorescence file is not correct file type for MIC platform.")}


      #check if standard curve fluorescence file if biomeme file
      else if (read.csv(SC_flur_file$datapath)[1, 1] == 'Date & Time')
      {shinyjs::alert("ERROR: Standard curve fluorescence file is not correct file type for MIC platform.")}


      #Check if fluorescence file processing function work
      else if (is.error(process_MIC_uploaded_file(read.csv(flur_file$datapath))) == TRUE)
      {return(shinyjs::alert("ERROR: Fluorescence file is not correct file type for MIC platform."))}

      #Check if  fluorescence file processing function work on standard curve fluorescence
      else if (is.error(process_MIC_uploaded_file(read.csv(SC_flur_file$datapath))) == TRUE)
      {return(shinyjs::alert("ERROR: Standard curve fluorescence file is not correct file type for MIC platform."))}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_MIC_uploaded_file(read.csv(flur_file$datapath))) != nrow(format_qPCR_metadata(meta_file$datapath)))
      {shinyjs::alert("ERROR: Fluorescence file and metadata file have information for different number of wells.")}


      #Check that standard curve formatting function works on metadata
      else if(is.error(format_standardCurve_metadata(read_xlsx(meta_file$datapath, sheet = 5))) == TRUE)
      {shinyjs::alert("ERROR: Error in standardCurveResults_Table in Metadata file.")}


      #Check that standard curve fluorescence data and standard curve data have information for same number of wells
      else if(nrow(process_MIC_uploaded_file(read.csv(SC_flur_file$datapath))) != nrow(format_standardCurve_metadata(read_xlsx(meta_file$datapath, sheet = 5))))
      {shinyjs::alert("ERROR: Standard curve fluorescence file and metadata file have information for different number of wells.")}

      else {shinyjs::alert("Successful platform selected.")}}


    else if (platform == "StepOnePlus"){

      if (file_ext(flur_file$datapath) %ni% c("xlsx", "xls"))
      {shinyjs::alert("ERROR: Step One Plus fluorescence file must be xlsx/xls.")}


      else if (file_ext(SC_flur_file$datapath) %ni% c("xlsx", "xls"))
      {shinyjs::alert("ERROR:Step One Plus standard curve fluorescence file must be xlsx/xls.")}


      #Check is fluorescence file processing function works
      else if (is.error(process_SOP_uploaded_file(read_excel(flur_file$datapath, 4))) == TRUE)
      {return(shinyjs::alert("ERROR: Fluorescence file is not correct file type for Step One Plus platform."))}


      #Check if fluorescence file processing function works on standard curve fluorescence
      else if (is.error(process_SOP_uploaded_file(read_excel(SC_flur_file$datapath, 4))) == TRUE)
      {return(shinyjs::alert("ERROR: Fluorescence file is not correct file type for Step One Plus platform."))}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_SOP_uploaded_file(read_excel(flur_file$datapath, sheet = 4))) != nrow(format_qPCR_metadata(meta_file$datapath)))
      {shinyjs::alert("ERROR: Fluorescence file and metadata file have information for different number of wells")}


      #check standard curve formatting function works on metadata
      else if(is.error(format_standardCurve_metadata(read_xlsx(meta_file$datapath, sheet = 5))) == TRUE)
      {shinyjs::alert("ERROR: Error in standardCurveResults_Table in Metadata file.")}


      #check that formatted standard curve fluorescene data and formatted standard curve metadata have same number of rows
      else if(nrow(process_SOP_uploaded_file(read_excel(flur_file$datapath, sheet = 4))) != nrow(format_standardCurve_metadata(read_xlsx(meta_file$datapath, sheet = 5))))
      {shinyjs::alert("ERROR: Standard curve fluorescence file and metadata file have information for different number of wells.")}


      else {shinyjs::alert("Successful platform selected.")}}


    #If files are Biomeme two3/Franklin
    else if (platform == "Biomeme two3/Franklin") {
      if (file_ext(flur_file$datapath) != "csv")
      {shinyjs::alert("ERROR:Biomeme two3/Franklin fluorescence file must be csv.")}


      else if (file_ext(SC_flur_file$datapath) != "csv")
      {shinyjs::alert("ERROR:Biomeme two3/Franklin standard curve fluorescence file must be csv.")}


      #Check if fluorescence file processing function works
      else if (is.error(process_biomeme23_uploaded_file(read.csv(flur_file$datapath))) == TRUE)
      {return(shinyjs::alert("ERROR: Fluorescence file not correct file type for Biomeme two3/Franklin platform."))}


      #Check if fluorescence file processing  function work on standard curve fluorescence
      else if (is.error(process_biomeme23_uploaded_file(read.csv(SC_flur_file$datapath))) == TRUE)
      {return(shinyjs::alert("ERROR: Standard curve fluorescence file not correct file type for Biomeme two3/Franklin platform."))}


      #Check that formatted fluorescence file and formatted metadata file have the same number of rows
      else if (nrow(process_biomeme23_uploaded_file(read.csv(flur_file$datapath))) != nrow(format_qPCR_metadata(meta_file$datapath)))
      {shinyjs::alert("ERROR: Fluorescence file and metadata file have information for different number of wells.")}


      #Check that standard curve formatting function works on metadata
      else if(is.error(format_standardCurve_metadata(read_xlsx(meta_file$datapath, sheet = 5))) == TRUE)
      {shinyjs::alert("ERROR: Error in standardCurveResults_Table in Metadata file.")}


      #Check that standard curve fluorescence data and standard curve data have information for same number of wells
      else if(nrow(process_biomeme23_uploaded_file(read.csv(SC_flur_file$datapath))) != nrow(format_standardCurve_metadata(read_xlsx(meta_file$datapath, sheet = 5))))
      {shinyjs::alert("ERROR: Standard curve fluorescence file and metadata file have information for different number of wells.")}


      else {shinyjs::alert("Successful platform selected.")}}


    #qPCR platform not selected
    else {
      #shinyjs::alert("Please select qPCR platform type")
    }}


  #Data Analysis Functions for Mapping Dashboard and Fata Overview page  ---------------------------


  #Function removes null records (controls or empty runs) from associated fluorescence file based on if sample name contains 'null' in metadata file.
  null_records_to_remove_flur <- function(meta_data, fluor_file) {
    if (length(which(grepl("null", tolower(meta_data$sampleName)))) != 0)
    {return (fluor_file[-c(which(grepl("null", tolower(meta_data$sampleName)))), ])}

    else
    {return(fluor_file)}

  }


  #Function removes null records (controls or empty runs) from metadata file if sample name contains 'null'.
  null_records_to_remove_meta <- function(meta_data) {
    if (length(which(grepl("null", tolower(meta_data$sampleName)))) != 0)
    {return (meta_data[-c(which(grepl("null", tolower(meta_data$sampleName)))), ])}

    else
    {return(meta_data)}

  }


  #Removing non template control records from standard curve fluorescene table
  control_records_to_remove <-
    function(meta_data) {
      if (length(which(grepl("Y", toupper(meta_data$control)))) != 0)
      {return (meta_data[-c(which(grepl("Y", toupper(meta_data$control)))), ])}

      else
      {return(meta_data)}
    }



  #Script to read in metadata file and format for qPCR experimental data
  format_qPCR_metadata <- function(metadataFile) {

    #Read in sheets
    project_Table <- read_xlsx(metadataFile,sheet = 1)
    replicate_Table <- read_xlsx(metadataFile,sheet = 2)
    assay_Table <-  read_xlsx(metadataFile, sheet = 3)
    results_Table <- read_xlsx(metadataFile, sheet = 4)
    standardCurve_Table <- read_xlsx(metadataFile, sheet = 5)


    #Creating metadata table
    metadata_table <-
      distinct(left_join(assay_Table, standardCurve_Table[, c("standardCurveID", "assayID", "standardCurveName", "SCdate", "SCrecordedBy", "SCdataNotes")], by = "assayID"))

    metadata_table <-  left_join(results_Table, metadata_table, by = "assayID")

    metadata_table <- left_join(metadata_table, replicate_Table, by = "extractID")

    metadata_table <- left_join(metadata_table, project_Table, by = "stationID")


    #Organize columns
    metadata_table <- as.data.frame(metadata_table[, c(
      "resultID","runID", "assayID","pcrChemistryID","extractID","wellLocation","sampleName", "copyNumber","control","userProvidedThresholdValue", "userProvidedCqValue","runRecordedBy", "runDate","runTime","runPlatform","machineID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch","reversePrimerBatch","dNTPConcentration", "primerConcentration","probeConcentration","Mg2+Concentration","polymeraseBatch","polymeraseConcentrations","thermocyclerParameters","pcrDataNotes","taxonID","establishmentMeans","assayName","assayOwnership","assayDescription","assayCitation","assayDate","geneTarget","geneSymbol","dilutions","replicates","primerR","primerF","probe","ampliconLength (bp)","probeFluorescentTag","dye(s)","quencher","probeModification","kingdom","phylum","class","order","family","genus","subgenus","species","vernacularName","organismScope","replicateID","extractName","analyst", "extractionDate", "extractionTime", "location", "extractionMethod", "methodCitation", "extractionNotes","tubePlateID","frozen", "fixed","dnaStorageLocation","extractMethodOfStorage","dnaVolume","quantificationMethod", "concentration(ng/ul)","stationID","collectorName","replicateName","collectionDate","collectionTime","storageID","DateOfStorage","methodOfStorage","minimumElevationInMeters","maximumElevationInMeters","verbatimElevation","minimumDepthInMeters","maximumDepthInMeters","verbatimDepth", "flowRate(m/s)", "filterType","filtrationDuration(mins)","volumeFiltered","processLocation","replicationNumber","riparianVegetationPercentageCover","dissolvedOxygen(mg/L)","waterTemperature(C)","pH","TSS(mg/L)","EC(uS/cm)","turbidity(NTU)","discharge","tide","chlorophyl","salinity(ppt)","contaminants(ng/g)","traceMetals(mg/kg)","organicContent(%)","microbialActivity","grainSize","replicateDataNotes","siteID","stationName","decimalLongitude","decimalLatitude","geographicRegionID","locality","estimatedPerimeter","estimatedSurfaceArea(m2)","siteType","siteLength(m2)","projectID","continent","country","stateProvince","municipality","projectCreationDate","projectName", "projectRecordedBy","projectOwner","projectContactEmail","projectDescription","InstitutionID","projectDataNotes","standardCurveID","standardCurveName","SCdate","SCrecordedBy","SCdataNotes")])

    return(metadata_table)

  }



  #Combing data from formatted fluorescence file and formatted metdata file (Same for MIC, Biomeme two3/Franklin, and StepOnePlus)
  merge_metadata_fluorescence_file <-  function(fluorescence_File, metadata) {

    #Remove null records (controls or empty runs) and associated fluorescence
    fluorescence_File <- null_records_to_remove_flur(metadata, fluorescence_File)
    metadata <- null_records_to_remove_meta(metadata)


    #Add CqValue row to dataframe. If user does not provided Cq value with assoicated threshold value then the function will calculate it.
    user_Cq <- function (flur, meta) {

      if (any(is.na(meta$userProvidedCqValue)) == TRUE) {
        flur$userProvidedCqValue <- ""

        flur <- add_CqValue(flur, meta)

        return(flur)}

      else {

        flur$userProvidedCqValue <- meta$userProvidedCqValue

        return(flur)
      }
    }


    fluorescence_File <- user_Cq(fluorescence_File, metadata)


    #Handeling extreme values
    fluorescence_File$userProvidedCqValue <- as.numeric(fluorescence_File$userProvidedCqValue)
    fluorescence_File$userProvidedCqValue[fluorescence_File$userProvidedCqValue < 0] <- 40
    fluorescence_File$userProvidedCqValue[fluorescence_File$userProvidedCqValue > 40] <- 40


    #Making dataframe with empty fluorescence data
    total_runs <- ncol(fluorescence_File) - 1  #substract one for Cq value column
    flur_col <- as.data.frame(matrix(NA ,
                                     nrow =  nrow(fluorescence_File),
                                     ncol = (70 - total_runs)))

    colnames(flur_col) <- c(paste0("Cycle_Number", c(((total_runs + 1):70))))


    #Calculate threshold
    fluorescence_data_for_threshold <- fluorescence_File[1:(ncol(fluorescence_File) - 1)]
    systemCalculatedThreshold <- calculate_threshold_value(fluorescence_data_for_threshold)
    colnames(systemCalculatedThreshold) <-"userProvidedThresholdValue"

    #Calculated Cq value with calulcated threshold
    calculated_system_cq <- add_CqValue(fluorescence_File, systemCalculatedThreshold)
    names(calculated_system_cq)[ncol(calculated_system_cq)] <- "systemCalculatedCqValue"
    colnames(systemCalculatedThreshold) <-"systemCalculatedThresholdValue"

    #Ensure threshold value and Cq value are numeric
    calculated_system_cq$systemCalculatedCqValue <- as.numeric(calculated_system_cq$systemCalculatedCqValue)


    #Merging fluorescence and metadata files
    merged_file <- cbind(metadata[1:10],
                         fluorescence_File[ncol(fluorescence_File)], #user provided Cq value
                         systemCalculatedThreshold[1],
                         calculated_system_cq[ncol(calculated_system_cq)],
                         fluorescence_File[1:(ncol(fluorescence_File) - 1)],
                         flur_col,
                         metadata[12:140])


    merged_file$CqIntensity <-cut(merged_file$userProvidedCqValue,
                                  breaks = c(0, 10, 20, 30, 40, 1000),
                                  right = FALSE,
                                  labels = c("0 < Very strong < 10",
                                             "10 <= Strong < 20",
                                             "20 <= Moderate < 30",
                                             "30 <= Weak < 40",
                                             "None > 40"))


    merged_file$CqIntensitySystemCalculated <- cut(merged_file$systemCalculatedCqValue,
                                                   breaks = c(0, 10, 20, 30, 40, 1000),
                                                   right = FALSE,
                                                   labels = c("0 < Very strong < 10",
                                                              "10 <= Strong < 20",
                                                              "20 <= Moderate < 30",
                                                              "30 <= Weak < 40",
                                                              "None > 40"))


    #Changing wellLocation column class to character
    merged_file$wellLocation <- as.character(merged_file$wellLocation)

    merged_file$projectCreationDate <- as.Date(as.character(merged_file$projectCreationDate), "%Y-%m-%d")
    merged_file$collectionDate <- as.Date(as.character(merged_file$collectionDate ), "%Y-%m-%d")
    merged_file$extractionDate <- as.Date(as.character(merged_file$extractionDate), "%Y-%m-%d")
    merged_file$runDate <- as.Date(as.character(merged_file$runDate), "%Y-%m-%d")

    merged_file$assayDate <- as.Date(as.character(merged_file$assayDate), "%Y-%m-%d")

    #Return merged dataframe
    return(merged_file)
  }



  #format standard curve metadata
  format_standardCurve_metadata <- function (standardCurve_metadata) {
    standardCurve_Table <- as.data.frame(standardCurve_metadata[, c("SCresultID",
                                                                    "runID",
                                                                    "pcrChemistryID",
                                                                    "standardCurveID",
                                                                    "wellLocation",
                                                                    "sampleName",
                                                                    "copyNumber",
                                                                    "control",
                                                                    "standardConc",
                                                                    "userProvidedThresholdValue",
                                                                    "userProvidedCqValue",
                                                                    "runRecordedBy",
                                                                    "runDate",
                                                                    "runTime",
                                                                    "runPlatform",
                                                                    "machineID",
                                                                    "reactionConditions",
                                                                    "reactionVolume",
                                                                    "templateAmount",
                                                                    "forwardPrimerBatch",
                                                                    "reversePrimerBatch",
                                                                    "dNTPConcentration",
                                                                    "primerConcentration",
                                                                    "probeConcentration",
                                                                    "Mg2+Concentration",
                                                                    "polymeraseBatch",
                                                                    "polymeraseConcentrations",
                                                                    "thermocyclerParameters",
                                                                    "pcrDataNotes",
                                                                    "assayID",
                                                                    "standardCurveName",
                                                                    "SCdate",
                                                                    "SCrecordedBy",
                                                                    "SCdataNotes",
                                                                    "LOD",
                                                                    "LOQ")])
    return (standardCurve_Table)
  }



  #Combing data from formatted fluorescence file and formatted standard curve metdata file (Same for MIC, biomeme23, and SOP)
  merge_standardCurve_metadata_fluorescence_file <- function(fluorescence_File, metadata) {

    #Remove null records (controls or empty runs) and associated fluorescence
    fluorescence_File <- null_records_to_remove_flur(metadata, fluorescence_File)
    metadata <- null_records_to_remove_meta(metadata)


    #Add CqValue row to dataframe
    user_Cq <- function (flur, meta) {

      if (any(is.na(meta$userProvidedCqValue)) == TRUE)

      {flur$userProvidedCqValue <- ""
      flur <- add_CqValue(flur, meta)

      return(flur)}

      else {
        flur$userProvidedCqValue <- meta$userProvidedCqValue
        # flur <- add_CqValue(flur, meta)

        return(flur)
      }
    }


    fluorescence_File <- user_Cq(fluorescence_File, metadata)

    #Handeling extreme values
    fluorescence_File$userProvidedCqValue <- as.numeric(fluorescence_File$userProvidedCqValue)
    fluorescence_File$userProvidedCqValue[fluorescence_File$userProvidedCqValue < 0] <-40
    fluorescence_File$userProvidedCqValue[fluorescence_File$userProvidedCqValue > 40] <- 40


    #Making dataframe with empty fluorescence data
    total_runs <- ncol(fluorescence_File) - 1  #substract one for Cq value column
    flur_col <- as.data.frame(matrix(NA, nrow =  nrow(fluorescence_File),ncol = (70 - total_runs)))
    colnames(flur_col) <- c(paste0("Cycle_Number", c(((total_runs + 1):70))))


    #System calculated threshold and CqValue
    #Calculate threshold
    fluorescence_data_for_threshold <- fluorescence_File[1:(ncol(fluorescence_File) - 1)]
    systemCalculatedThreshold <- calculate_threshold_value(fluorescence_data_for_threshold)
    colnames(systemCalculatedThreshold) <-"userProvidedThresholdValue"

    #Calculated Cq value with calulcated threshold
    calculated_system_cq <- add_CqValue(fluorescence_File, systemCalculatedThreshold)
    names(calculated_system_cq)[ncol(calculated_system_cq)] <- "systemCalculatedCqValue"
    colnames(systemCalculatedThreshold) <-"systemCalculatedThresholdValue"

    #Ensure threshold value and cq value are numeric
    calculated_system_cq$systemCalculatedCqValue <- as.numeric(calculated_system_cq$systemCalculatedCqValue)

    #Change class to date
    metadata$runDate <- as.Date(as.character(metadata$runDate), "%Y-%m-%d")
    metadata$SCdate <- as.Date(as.character(metadata$SCdate), "%Y-%m-%d")


    #Mreging fluorescence and metadata files
    merged_file <-
      cbind(
        metadata[1:10],
        fluorescence_File[ncol(fluorescence_File)],
        systemCalculatedThreshold[1],
        calculated_system_cq[ncol(calculated_system_cq)],
        fluorescence_File[1:(ncol(fluorescence_File) - 1)],
        flur_col,
        metadata[12:36]
      )


    #Changing wellLocation column class to character
    merged_file$wellLocation <- as.character(merged_file$wellLocation)

    #Ensure columns are numeric
    merged_file$userProvidedThresholdValue <- as.numeric(merged_file$userProvidedThresholdValue)
    merged_file$userProvidedCqValue <- as.numeric(merged_file$userProvidedCqValue)
    merged_file$standardConc <- as.numeric(merged_file$standardConc)

    #Return merged dataframe
    return(merged_file)

  }




  #Mapping Dashboard page ---------------------------

  #Code for ValueBoxes
  #Icons from https://fontawesome.com/icons?from=io

  sample_number <- reactive({

    if (!is.null(uploaded_data())| dbExists == "Yes") {
      filt_data <- as.data.frame(filtered())
      return(length(unique(filt_data$extractID)))}

    else { return (0)}
  })




  output$sampleBox <- renderValueBox({

    val <- as.numeric(sample_number())

    valueBox(
      paste0(sample_number()),
      "Samples",
      icon = icon("vials"),
      color = "yellow"
    )})




  platform_number <- reactive({
    if (!is.null(uploaded_data()) | dbExists == "Yes") {
      filt_data <- as.data.frame(filtered())
      return(length(unique(filt_data$runPlatform)))}

    else { return (0)}
  })


  output$platformBox <- renderValueBox({

    valueBox(
      paste0(platform_number()),
      "Unique qPCR Platforms",
      icon = icon("hdd"),
      color = "yellow"
    )})




  taxon_number <- reactive({
    if (!is.null(uploaded_data()) | dbExists == "Yes") {
      filt_data <- as.data.frame(filtered())
      return(length(unique(filt_data$taxonID)))}

    else { return (0)}
  })



  output$taxonBox <- renderValueBox({

    valueBox(
      paste0(taxon_number()),
      "Taxon",
      icon = icon("frog"),
      color = "yellow"
    )})



  assay_number <- reactive({
    if (!is.null(uploaded_data()) | dbExists == "Yes") {
      filt_data <- as.data.frame(filtered())
      return(length(unique(filt_data$assayID)))}

    else { return (0)}
  })




  output$assayBox <- renderValueBox({

    valueBox(
      paste0(assay_number()),
      "Unique Assays",
      icon = icon("flask"),
      color = "yellow"
    )})



  #File validate errors messages.
  error_message <- reactive({
    validate(need(input$qpcr_file, 'No fluorescence file uploaded'),
             need(input$metadata_file, 'No metadata file uploaded'),
             need(try(file_ext(input$qpcr_file) == "csv" | file_ext(input$qpcr_file) == "xlsx" | file_ext(input$qpcr_file) == "xls") ,
                  "fluorescence file must be csv or xlsx/xls"),
             need(try(file_ext(input$metadata_file) == "xlsx" | file_ext(input$metadata_file) == "xls") , "Metadata file must be xlsx/xls"))
  })


  output$error_msg <- renderText({
    error_message()})




  #Pop-up validation messages for uploaded fluorescence file.
  observeEvent(input$qpcr_file,fluorescence_file_validation_msgs(input$qpcr_file))


  #Pop-up validation messages for uploaded metadata file,
  observeEvent(input$metadata_file, metadata_file_validation_msgs(input$metadata_file))


  #Pop-up validation messages for uploaded uploaded fluorescence and metadata file, based on machine type.
  observeEvent(req(input$qpcr_file, input$metadata_file,input$platform),
               selected_platform_validation_msgs(input$qpcr_file,
                                                 input$metadata_file,
                                                 input$platform))


  #Function process the user uploaded fluorescence file and metadata file to be analyzed on the mapping dashboard. The MDMAPR currently accepts fluorescence files from the following qPCR platforms: MIC, StepOnePlus and Biomeme23/Franklin.

  #initalize uploaded_data reactive variable
  uploaded_data <- reactiveVal(value = NULL)

  observeEvent(input$submit, {


    #Validate content of user uploaded files
    if (user_uploaded_file_validate(input$qpcr_file, input$metadata_file, input$platform) == TRUE)
    {return(uploaded_data(NULL))}


    else{

      if (input$platform == "Biomeme two3/Franklin") {

        #Read in raw qPCR data
        qpcr_biomem23_raw <- process_biomeme23_uploaded_file(read.csv(input$qpcr_file$datapath))
        #Read in metadata
        metadata_biomem23 <- format_qPCR_metadata(input$metadata_file$datapath)

        #Function to process and merge files
        merged_biomem23_file <- merge_metadata_fluorescence_file(qpcr_biomem23_raw,metadata_biomem23)

        #This merged datatable that will be used to populate map
        return(uploaded_data(merged_biomem23_file))

      }


      ### Machine Option 2: MIC ###
      else if (input$platform == "MIC") {

        #Read in raw qPCR data
        qpcr_MIC_raw  <- process_MIC_uploaded_file(read.csv(input$qpcr_file$datapath))

        #Read in metadata
        metadata_MIC <- format_qPCR_metadata(input$metadata_file$datapath)

        #Function to process and merge files
        merged_mic_file <- merge_metadata_fluorescence_file(qpcr_MIC_raw, metadata_MIC)

        #This merged datatable that will be used to populate map
        return(uploaded_data(merged_mic_file))
      }


      ### Machine Option 3: StepOnePlus ###
      else if (input$platform == "StepOnePlus") {

        #Read in raw qPCR data
        qpcr_StepOnePlus_raw <- process_SOP_uploaded_file(read_excel(input$qpcr_file$datapath, 4))

        #Read in metadata
        metadata_StepOnePlus <- format_qPCR_metadata(input$metadata_file$datapath)

        #Function to process and merge files
        merged_StepOnePlus_file <-  merge_metadata_fluorescence_file(qpcr_StepOnePlus_raw, metadata_StepOnePlus)
        #This merged datatable that will be used to populate map
        return(uploaded_data(merged_StepOnePlus_file))

      }
    }})




  #Update filtering option values if user uploads compatible fluorescence file and metadata file.

  #Updated Family list
  family_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes' ) {
      data <- as.data.frame(mergedData())
      family_data <- as.character(unique(data$family))
      return(family_data)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "family_input",
                      choices = family_list(),
                      selected = family_list())
  })



  #Updated Genus list
  genus_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      genus_data <- as.character(unique(data$genus))
      return(genus_data)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "genus_input",
                      choices = genus_list(),
                      selected = genus_list())
  })



  #Updated Species list
  species_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      species_data <- as.character(unique(data$species))
      return(species_data)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "species_input",
                      choices = species_list(),
                      selected = species_list())
  })




  #Update qPCR machine list
  machine_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      run_platform_add <- as.character(unique(data$runPlatform))
      return(run_platform_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "machine_input",
                      choices = machine_list(),
                      selected = machine_list() )
  })




  #Update target gene list
  targetGene_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      targetGene_add <- as.character(unique(data$geneSymbol))
      return(targetGene_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "targetGene_input",
                      choices = targetGene_list(),
                      selected = targetGene_list() )
  })




  #Update project list
  project_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      project_add <- as.character(unique(data$projectName))
      return(project_add)} else {
        return(NULL)}
  })


  observe({
    updatePickerInput(session, "projectID_input",
                      choices = project_list(),
                      selected = project_list() )
  })



  #Update assay list
  assay_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      assay_add <- as.character(unique(data$assayName))
      return(assay_add)} else {
        return(NULL)}
  })


  observe({
    updatePickerInput(session, "assay_input",
                      choices = assay_list(),
                      selected = assay_list())
  })




  #Update continent list
  continent_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      continent_add <- as.character(unique(data$continent))
      return(continent_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "continent_input",
                      choices = continent_list(),
                      selected = continent_list() )
  })


  #Update country list
  country_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      country_add <- as.character(unique(data$country))
      return(country_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "country_input",
                      choices = country_list(),
                      selected = country_list() )
  })




  #Update state/province list
  stateProvince_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      stateProvince_add <- as.character(unique(data$stateProvince))
      return(stateProvince_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "stateProvince_input",
                      choices = stateProvince_list(),
                      selected = stateProvince_list() )
  })



  #Update locality list
  locality_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      locality_add <- as.character(unique(data$locality))
      return(locality_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "locality_input",
                      choices = locality_list(),
                      selected = locality_list())
  })




  #Update establishment means list
  establishmentMeans_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      establishmentMeans_add <- as.character(unique(data$establishmentMeans))
      return(establishmentMeans_add)} else {
        return(NULL)}
  })

  observe({
    updatePickerInput(session, "establishmentMeans_input",
                      choices = establishmentMeans_list(),
                      selected = establishmentMeans_list() )
  })


  #Update Cq intensity list
  ctIntensity_list <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      ctintensity_updated <- as.character(unique(c(as.character(unique(data$CqIntensity)),
                               as.character(unique(data$CqIntensitySystemCalculated)))))
      return(ctintensity_updated)}


    else {
      return(NULL)}
  })


  observe({
    updatePickerInput(session, "CqIntensity_input",
                      choices = ctIntensity_list(),
                      selected = ctIntensity_list() )
  })



  #Update minimum date on date range slider
  #Update Cq intensity list
  min_date <- reactive({
    if (!is.null(uploaded_data()) | dbExists == 'Yes') {
      data <- as.data.frame(mergedData())
      min_date_updated <- min(as.Date(data$collectionDate, "%Y-%m-%d"))
      return(as.character(min_date_updated))}

    else {
      return(NULL)}
  })



  observe({

    updateSliderInput(session, "date_input",
                      min = as.Date(min_date(),"%Y-%m-%d"),
                      max = as.Date(Sys.Date(), "%Y-%m-%d"),
                      value = range(c(as.Date(min_date(),"%Y-%m-%d"),
                                      as.Date(Sys.Date(), "%Y-%m-%d"))),
                      step=1)
  })




  #merge uploaded data with data from MySQL connection
  mergedData <- reactive({

    if(dbExists == 'No' && is.null(uploaded_data())) {

      return("No_Data")}


    else if (dbExists == 'Yes' && is.null(uploaded_data())){

      my_sql_data <-  control_records_to_remove(my_sql_data)

      return(my_sql_data)
    }


    else if (dbExists == 'No' && !is.null(uploaded_data())) {

      return (mysql_and_uploaddata <- control_records_to_remove(as.data.frame(uploaded_data())))
    }

    else {

      mysql_and_uploaddata <- as.data.frame(rbind(my_sql_data, uploaded_data()))

      mysql_and_uploaddata <-  control_records_to_remove(mysql_and_uploaddata)

      return(mysql_and_uploaddata)

    }

  })



  #Mapped markers filtered by widgets on dashboard page.
  filtered <- reactive({

    data_final <- mergedData()

    if (!is.null(uploaded_data()) | dbExists == 'Yes') {

      data_final <- data_final[data_final[ , cq_column()] >= input$range[1] &
                                 data_final[ , cq_column()] <= input$range[2] &
                                 data_final$collectionDate >= input$date_input[1] &
                                 data_final$collectionDate <= input$date_input[2], ] %>%
        filter(family %in% input$family_input)  %>%
        filter(species %in% input$species_input)  %>%
        filter(genus %in% input$genus_input)  %>%
        filter(runPlatform %in% input$machine_input) %>%
        filter(geneSymbol %in% input$targetGene_input) %>%
        filter(projectName%in% input$projectID_input) %>%
        filter(assayName %in% input$assay_input) %>%
        filter(country %in% input$country_input) %>%
        filter(continent %in% input$continent_input) %>%
        filter(stateProvince %in% input$stateProvince_input) %>%
        filter(locality %in% input$locality_input) %>%
        filter(establishmentMeans %in% input$establishmentMeans_input)


      if (input$thresholdValueButton == 10)
      {data_final <- data_final %>%  filter(CqIntensity %in% input$CqIntensity_input)

      return(data_final)}

      else  { data_final <- data_final %>%  filter(CqIntensitySystemCalculated %in% input$CqIntensity_input)

      return(data_final)}
    }
  })


  #Download button for downloading CSV of filtered mapping data
  output$downloadFilteredData <- downloadHandler(

    filename = 'MDMAP_Mapping_data.csv',

    content = function(file) {

      write.csv(filtered(), file,  row.names=FALSE)
    })


  #Dynamic data table with Mapping marker information
  output$mapping_data <- renderDataTable({

    mapping_data <- as.data.frame(filtered()[, c(
      "projectName","projectDescription",	"InstitutionID", "sampleName", "wellLocation", "copyNumber",	"control","runRecordedBy", "runDate",	"runTime",	"runPlatform", "machineID",	"reactionConditions",	"reactionVolume", "templateAmount",	"forwardPrimerBatch",	"reversePrimerBatch", "dNTPConcentration",	"primerConcentration",	"probeConcentration","Mg2+Concentration",	"polymeraseBatch",	"polymeraseConcentrations", "thermocyclerParameters",	"pcrDataNotes",	"taxonID","establishmentMeans",	"assayName",	"assayOwnership","assayDescription",	"assayCitation", "assayDate",	"geneTarget", "geneSymbol",	"dilutions",	"replicates","primerR",	"primerF",	"probe","ampliconLength (bp)",	"probeFluorescentTag",	"dye(s)","quencher",	"probeModification", "kingdom",	"phylum",	"class","order",	"family",	"genus","subgenus",	"species",	"vernacularName","organismScope",	"replicateID",	"extractName", "analyst",	"extractionDate",	"extractionTime","location",	"extractionMethod",	"methodCitation","extractionNotes",	"tubePlateID",	"frozen","fixed",	"dnaStorageLocation",	"extractMethodOfStorage", "dnaVolume",	"quantificationMethod",	"concentration(ng/ul)","stationID",	"collectorName",	"replicateName", "collectionDate",	"collectionTime",	"storageID","DateOfStorage",	"methodOfStorage",	"minimumElevationInMeters","maximumElevationInMeters",	"verbatimElevation",	"minimumDepthInMeters","maximumDepthInMeters",	"verbatimDepth", "flowRate(m/s)",	"filterType",	"filtrationDuration(mins)", "volumeFiltered",	"processLocation",	"replicationNumber","riparianVegetationPercentageCover",	"dissolvedOxygen(mg/L)",	"waterTemperature(C)","pH",	"TSS(mg/L)",	"EC(uS/cm)", "turbidity(NTU)",	"discharge",	"tide", "chlorophyl",	"salinity(ppt)",	"contaminants(ng/g)","traceMetals(mg/kg)",	"organicContent(%)",	"microbialActivity", "grainSize",	"replicateDataNotes",	"siteID", "stationName",	"decimalLongitude",	"decimalLatitude", "geographicRegionID",	"locality",	"estimatedPerimeter","estimatedSurfaceArea(m2)",	"siteType",	"siteLength(m2)","projectID",	"continent",	"country","stateProvince",	"municipality",	"standardCurveID",	"standardCurveName","SCdate",	"SCrecordedBy",	"SCdataNotes", "CqIntensity",	"CqIntensitySystemCalculated", "userProvidedThresholdValue",	"userProvidedCqValue",	"systemCalculatedThresholdValue","systemCalculatedCqValue", "Cycle_Number1",	"Cycle_Number2", "Cycle_Number3",	"Cycle_Number4",	"Cycle_Number5", "Cycle_Number6",	"Cycle_Number7",	"Cycle_Number8", "Cycle_Number9",	"Cycle_Number10",	"Cycle_Number11","Cycle_Number12",	"Cycle_Number13",	"Cycle_Number14","Cycle_Number15",	"Cycle_Number16",	"Cycle_Number17","Cycle_Number18",	"Cycle_Number19",	"Cycle_Number20","Cycle_Number21",	"Cycle_Number22",	"Cycle_Number23","Cycle_Number24",	"Cycle_Number25",	"Cycle_Number26","Cycle_Number27",	"Cycle_Number28",	"Cycle_Number29", "Cycle_Number30",	"Cycle_Number31",	"Cycle_Number32","Cycle_Number33",	"Cycle_Number34",	"Cycle_Number35","Cycle_Number36",	"Cycle_Number37",	"Cycle_Number38","Cycle_Number39",	"Cycle_Number40",	"Cycle_Number41", "Cycle_Number42",	"Cycle_Number43",	"Cycle_Number44","Cycle_Number45",	"Cycle_Number46",	"Cycle_Number47","Cycle_Number48",	"Cycle_Number49",	"Cycle_Number50","Cycle_Number51",	"Cycle_Number52",	"Cycle_Number53","Cycle_Number54",	"Cycle_Number55",	"Cycle_Number56", "Cycle_Number57",	"Cycle_Number58",	"Cycle_Number59",  "Cycle_Number60",	"Cycle_Number61",	"Cycle_Number62", "Cycle_Number63",	"Cycle_Number64",	"Cycle_Number65", "Cycle_Number66",	"Cycle_Number67",	"Cycle_Number68", "Cycle_Number69",	"Cycle_Number70" )])

    datatable(mapping_data,
              options = list(
                scrollX = TRUE
              ))
  })



  #What Cq value to use based on user select threshold value on Mapping Dashboard.
  cq_column <- reactive({if (input$thresholdValueButton == 10)
  {return(11)}
    else{return(13)}
  })


  #Data that will appear in each popup window for the mapped markers.
  popup_data <- reactive({

    data <- as.data.frame(filtered())


    content.output <- paste("<strong><h5>Species:", data$species, "</strong>",
                            "<strong><h5> NCBI Taxon ID:", "<a href='https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=",
                            data$taxonID, "'>", data$taxonID, "</a>", "</strong>",
                            "<strong><h5>Cq Value:", data[ , cq_column()], "</strong>",
                            "<strong><h5>Threshold Value:", data[ , as.numeric(input$thresholdValueButton)], "</strong>",
                            "<br><h6>Sample Name:", data$extractName,
                            "<br><h6>Well Location:", data$wellLocation,
                            "<br><h6>qPCR Device:", data$runPlatform,
                            "<br><h6>Common Name:", data$vernacularName,
                            "<br><h6>Event Date:", data$collectionDate,
                            "<h6>Event Coordinate(Lat, Lon):", data$decimalLatitude,",", data$decimalLongitude,
                            "<h6>Event Identifer(s):", data$analyst)
    return(content.output)
  })


  #Updated marker colour based on what threshold value was selected
  cq_intensity_column <- reactive({ if (input$thresholdValueButton == 10)
  {return(as.data.frame(filtered())$CqIntensity)}
    else  {return(as.data.frame(filtered())$CqIntensitySystemCalculated)}
  })



  #Define the colour Palette for leaflet map
  palette_map_ct <- colorFactor( palette = c("#fbd300",
                                             "#ff8600",
                                             "#ea5f94",
                                             "#9d02d7",
                                             "#0000ff"),
                                 levels = c("0 < Very strong < 10",
                                            "10 <= Strong < 20",
                                            "20 <= Moderate < 30",
                                            "30 <= Weak < 40",
                                            "None > 40"))


  #Default static leaflet map before filtering parameters are applied.
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%

      addLegend(position = "bottomright",
                pal = palette_map_ct,
                values =  c("0 < Very strong < 10",
                            "10 <= Strong < 20",
                            "20 <= Moderate < 30",
                            "30 <= Weak < 40",
                            "None > 40"),
                title = 'qPCR Cq Signal Intensity',
                opacity = 0.6) %>%

      #View map full screen (note: only works in web broswer)
      addFullscreenControl() %>%


      #Default map view --> Change to Guelph
      setView(lng = -80.2262, lat = 43.5327, zoom = 3) %>%

      #Change leaflet map tiles
      addProviderTiles(providers$Esri.WorldStreetMap)

  })


  # Observe will track if any filtering widgets are used. If any filters are implemented the   # Leaflet map will clear the existing markers and update the markers to relflect the
  # current filtering conditions.
  observe({

    #when leaflet map is not on first page you need to use req in order for marker points to appear on map
    req(input$tab_being_displayed == "dashboard")

    if (!is.null(uploaded_data()) | dbExists == 'Yes') {

      leafletProxy("mymap", data = as.data.frame(filtered())) %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearPopups() %>%
        #Adding labels to markers on map
        addCircleMarkers(lng = ~decimalLongitude,
                         lat = ~decimalLatitude,
                         color = ~palette_map_ct(cq_intensity_column()),
                         clusterOptions = markerClusterOptions(),
                         popup= popup_data()) }


  })






  #Reset uploaded file input and fitler widget selections to return map to its default view.
  observeEvent(input$reset, {

    if (dbExists == "Yes") {
      shinyjs::reset("platform")
      updateSelectInput(session,
                        inputId = "platform",
                        label = "qPCR Platform",
                        choices = c("None",
                                    "StepOnePlus",
                                    "Biomeme two3/Franklin",
                                    "MIC"),
                        selected = "None")

      shinyjs::reset("qpcr_file")
      shinyjs::reset("metadata_file")
      shinyjs::reset("range") #ct range slider

      #Reset date range slider
      updateSliderInput(session, "date_input",
                        value = range(c(min(as.Date("2010-01-01", "%Y-%m-%d")), as.Date(Sys.Date(), "%Y-%m-%d"))))

      #update value of upload_data
      uploaded_data(NULL)}

    else {

      shinyjs::reset("platform")
      updateSelectInput(session,
                        inputId = "platform",
                        label = "qPCR Platform",
                        choices = c("None",
                                    "StepOnePlus",
                                    "Biomeme two3/Franklin",
                                    "MIC"),
                        selected = "None")

      shinyjs::reset("qpcr_file")
      shinyjs::reset("metadata_file")
      shinyjs::reset("range") #ct range slider
      shinyjs::reset("family_input") #family dropdown box
      shinyjs::reset("genus_input") #genus dropdown box
      shinyjs::reset("species_input") #species dropdown box
      shinyjs::reset("CqIntensity_input") #ct intensity dropdown box
      shinyjs::reset("machine_input") #machine input
      shinyjs::reset("targetGene_input") #target gene (gene symbol) input
      shinyjs::reset("projectID_input") #project ID input
      shinyjs::reset("assay_input") #assay input
      shinyjs::reset("continent_input") #continent input
      shinyjs::reset("country_input") #country input
      shinyjs::reset("stateProvince_input") #state/province input
      shinyjs::reset("locality_input") #locality input
      shinyjs::reset("establishmentMeans_input") #establishment means input

      #Reset date range slider
      updateSliderInput(session, "date_input",
                        value = range(c(min(as.Date("2010-01-01", "%Y-%m-%d")), as.Date(Sys.Date(), "%Y-%m-%d"))))

      #Clear markers from leaflet map
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearMarkerClusters() %>%
        clearPopups() }

  })


  #qPCR Data Overview page ---------------------------

  #Uploaded file validation
  #Return popup message regarding uploaded fluorescence file.
  observeEvent(input$qPCR_fluorescence_file,
               fluorescence_file_validation_msgs(input$qPCR_fluorescence_file))

  #Return popup message regarding uploaded standard curve fluorescence file.
  observeEvent(input$SC_fluorescence_file,
               fluorescence_file_validation_msgs(input$SC_fluorescence_file))

  #Return popup messaged regarding uploaded metadata file.
  observeEvent(input$qPCR_metadata_file,
               metadata_file_validation_msgs(input$qPCR_metadata_file))

  #Return popup messaged regarding uploaded fluorescence and metadata file, based on machine type.
  observeEvent(req(input$qPCR_fluorescence_file,
                   input$SC_fluorescence_file,
                   input$qPCR_metadata_file,
                   input$DA_platform),

               selected_platform_validation_msgs_SC_and_Experimental_flur(input$qPCR_fluorescence_file,
                                                                          input$SC_fluorescence_file,
                                                                          input$qPCR_metadata_file,
                                                                          input$DA_platform))


  #Process user uploaded standard curve data
  user_uploaded_standard_curve_data <-  reactive({
    input$Uploaded_DA_submit

    isolate(

      #Validate content of user uploaded files
      if (user_uploaded_standard_curve_file_validation(input$SC_fluorescence_file,
                                                       input$qPCR_metadata_file,
                                                       input$DA_platform) == TRUE)

      {return(NULL)}


      else{

        if (input$DA_platform == "Biomeme two3/Franklin") {

          #Read in raw qPCR data
          qpcr_biomem23_raw <- process_biomeme23_uploaded_file(read.csv(input$SC_fluorescence_file$datapath))

          #Read in metadata
          metadata_biomem23 <-  format_standardCurve_metadata(read_xlsx(input$qPCR_metadata_file$datapath, sheet = 5))

          #Function to process and merge files
          merged_biomem23_file <- merge_standardCurve_metadata_fluorescence_file(qpcr_biomem23_raw, metadata_biomem23)


          #This merged datatable that will be used to populate map
          return(merged_biomem23_file)

        }


        ### Machine Option 2: MIC ###
        else if (input$DA_platform == "MIC") {

          #Read in raw qPCR data
          qpcr_MIC_raw  <- process_MIC_uploaded_file(read.csv(input$SC_fluorescence_file$datapath))

          #Read in metadata
          metadata_MIC <- format_standardCurve_metadata(read_xlsx(input$qPCR_metadata_file$datapath, sheet = 5))

          #Function to process and merge files
          merged_mic_file <- merge_standardCurve_metadata_fluorescence_file(qpcr_MIC_raw,
                                                                            metadata_MIC)

          #This merged datatable that will be used to populate map
          return(merged_mic_file)

        }

        ### Machine Option 3: StepOnePlus ###
        else if (input$DA_platform == "StepOnePlus") {

          #Read in standard curve fluorescence file
          standardCurve_StepOnePlus_raw <- process_SOP_uploaded_file(read_excel(input$SC_fluorescence_file$datapath, sheet = 4))

          #Read in standard curve metadata
          standardCurve_metadata_StepOnePlus <- format_standardCurve_metadata(read_xlsx(input$qPCR_metadata_file$datapath, sheet = 5))

          #Function to process and merge files
          merged_StepOnePlus_file <- merge_standardCurve_metadata_fluorescence_file(standardCurve_StepOnePlus_raw, standardCurve_metadata_StepOnePlus)


          return(merged_StepOnePlus_file)
        }
      })
  })


  #merge uploaded data with data from MySQL connection
  standardCurve_mergedData <- reactive({
    input$Uploaded_DA_submit


    if(dbExists == 'No' && is.null(user_uploaded_standard_curve_data()))
      return(NULL)

    else if (dbExists == 'Yes' && is.null(user_uploaded_standard_curve_data()))
    {return (mysql_standardCurve_data)}

    else if (dbExists == 'No' && !is.null(user_uploaded_standard_curve_data()))
    { mysql_and_uploaddata <- user_uploaded_standard_curve_data()
    mysql_and_uploaddata$userProvidedCqValue <- as.numeric(mysql_and_uploaddata$userProvidedCqValue)
    mysql_and_uploaddata$standardConc <-as.numeric(mysql_and_uploaddata$standardConc)

    return (mysql_and_uploaddata)}

    else {
      mysql_and_uploaddata <- rbind(mysql_standardCurve_data, user_uploaded_standard_curve_data())
      mysql_and_uploaddata$userProvidedCqValue <- as.numeric(mysql_and_uploaddata$userProvidedCqValue)
      mysql_and_uploaddata$standardConc <-as.numeric(mysql_and_uploaddata$standardConc)

      return(mysql_and_uploaddata)

    }
  })


  #Process user uploaded qPCR fluorescence data.

  user_uploaded_qPCR_data <-  reactive({

    input$Uploaded_DA_submit

    isolate(

      #Validate content of user uploaded files
      if (user_uploaded_file_validate(input$qPCR_fluorescence_file,
                                      input$qPCR_metadata_file,
                                      input$DA_platform) == TRUE)

      {return(NULL)}


      else{

        if (input$DA_platform == "Biomeme two3/Franklin") {

          #Read in raw qPCR data
          qpcr_biomem23_raw <- process_biomeme23_uploaded_file(read.csv(input$qPCR_fluorescence_file$datapath))

          #Read in metadata
          metadata_biomem23 <- format_qPCR_metadata(input$qPCR_metadata_file$datapath)

          #Function to process and merge files
          merged_biomem23_file <- merge_metadata_fluorescence_file(qpcr_biomem23_raw, metadata_biomem23)
          #This merged datatable that will be used to populate map
          return(merged_biomem23_file)
        }

        ### Machine Option 2: MIC ###
        else if (input$DA_platform == "MIC") {

          #Read in raw qPCR data
          qpcr_MIC_raw  <- process_MIC_uploaded_file(read.csv(input$qPCR_fluorescence_file$datapath))

          #Read in metadata
          metadata_MIC <- format_qPCR_metadata(input$qPCR_metadata_file$datapath)


          #Function to process and merge files
          merged_mic_file <- merge_metadata_fluorescence_file(qpcr_MIC_raw, metadata_MIC)


          #This merged datatable that will be used to populate map
          return(merged_mic_file)
        }

        ### Machine Option 3: StepOnePlus ###
        else if (input$DA_platform == "StepOnePlus") {

          #Read in standard curve fluorescence file
          standardCurve_StepOnePlus_raw <- process_SOP_uploaded_file(read_excel(input$qPCR_fluorescence_file$datapath, 4))


          #Read in standard curve metadata
          standardCurve_metadata_StepOnePlus <- format_qPCR_metadata(input$qPCR_metadata_file$datapath)

          #Function to process and merge files
          merged_StepOnePlus_file <- merge_metadata_fluorescence_file(standardCurve_StepOnePlus_raw, standardCurve_metadata_StepOnePlus)


          return(merged_StepOnePlus_file)
        }
      }
    )
  })


  #merge uploaded data with data from MySQL connection
  MySQL_mergedData <- reactive({

    if(dbExists == 'No' && is.null(user_uploaded_qPCR_data()))
      return(NULL)

    else if (dbExists == "Yes" && is.null(user_uploaded_qPCR_data()))
    { return(my_sql_data)
    }

    else if (dbExists == "No" && !is.null(user_uploaded_qPCR_data()))
    {return (user_uploaded_qPCR_data())}

    else {mysql_and_uploaddata <- rbind(my_sql_data, user_uploaded_qPCR_data())

    return(mysql_and_uploaddata)
    }
  })


  #Update dropdown menus based on selection in previous dropdown menu

  #Update assay list on page when additional file is uploaded
  SC_assay_list <- reactive({

    if (!is.null(MySQL_mergedData())) {

      data <- as.data.frame(MySQL_mergedData())

      assay_data <- append('None', as.character(unique(data$assayName)))

      return(assay_data)}

    else {return(NULL)}
  })

  observe({updatePickerInput(session,
                             "SC_assay_input",
                             choices = SC_assay_list(),
                             selected = 'None')})


  #Update machine based on assay selection
  SC_runPlatform_list <- reactive({

    if (input$SC_assay_input != 'None') {

      data <- as.data.frame(MySQL_mergedData())

      updated_list <- data[data$assayName == input$SC_assay_input, ]

      runPlatform_list <-  as.character(unique(updated_list$runPlatform))

      return(runPlatform_list)}

    else {return(NULL)}
  })

  observe({updatePickerInput(session,
                             "SC_machine_input",
                             choices = append('None', SC_runPlatform_list()),
                             selected = 'None')})


  #Update project list based on assay selection
  SC_project_list <- reactive({

    if (input$SC_machine_input != 'None') {

      data <- as.data.frame(MySQL_mergedData())

      updated_list <- data[data$assayName == input$SC_assay_input, ]
      updated_list <- updated_list[updated_list$runPlatform == input$SC_machine_input, ]
      project_list <-  as.character(unique(updated_list$projectName))

      return(project_list)}

    else {return(NULL)}
  })


  observe({updatePickerInput(session,
                             "SC_project_input",
                             choices = append('None', SC_project_list()),
                             selected = 'None') })


  #Update standard curve list based on selected project
  SC_version_list <- reactive({

    if (input$SC_project_input != 'None') {

      data <- as.data.frame(MySQL_mergedData())
      SC_data <- standardCurve_mergedData()

      updated_list <- data[data$assayName == input$SC_assay_input, ]
      updated_list <- updated_list[updated_list$runPlatform == input$SC_machine_input, ]
      updated_list <- updated_list[updated_list$projectName == input$SC_project_input, ]

      unique_standardCurveID <-  as.character(unique(updated_list$standardCurveID))

      return(unique_standardCurveID)}

    else {return(NULL)}
  })


  observe({updatePickerInput(session, "SC_input",
                             choices = append('None', SC_version_list()),
                             selected = 'None')})







  ## Standard curve plot

  #Transform dataframe for plot based on assay filters
  standard_curve_data_for_plot <- reactive({

    if (dbExists == "Yes" | !is.null(user_uploaded_qPCR_data())) {

      data <- MySQL_mergedData()

      SC_data <- standardCurve_mergedData()

      SC_filtered_data <- data[data$assayName == input$SC_assay_input, ]
      SC_filtered_data <- SC_filtered_data[SC_filtered_data$runPlatform == input$SC_machine_input, ]
      SC_filtered_data <- SC_filtered_data[SC_filtered_data$projectName == input$SC_project_input, ]
      SC_filtered_data <-  SC_data[SC_data$standardCurveID  == input$SC_input, ]
      SC_filtered_data <-  SC_filtered_data %>% filter(SC_filtered_data$userProvidedCqValue != 0)

      return(SC_filtered_data)
    }

  })


  #Standard curve plot only appears when submit button is pressed
  observeEvent(input$DA_submit, isolate({

    output$standardCurve_plot <- renderPlotly({

      #Add data to plot
      SC_plot_data <- standard_curve_data_for_plot()

      #Remove control records
      SC_plot_data <- control_records_to_remove(SC_plot_data)

      #Change standard concentration value to numeric and then take log value
      SC_plot_data$standardConc <- as.numeric(SC_plot_data$standardConc)
      SC_plot_data$standardConc <- log(SC_plot_data$standardConc)

      #Change user provided Cq to numeric value to numeric
      SC_plot_data$userProvidedCqValue <- as.numeric(SC_plot_data$userProvidedCqValue)
      SC_plot_data$LOQ<- as.numeric(SC_plot_data$LOQ)
      SC_plot_data$LOD<- as.numeric(SC_plot_data$LOD)

      #Add column with residual values to data set
      regression_line <- lm(as.numeric(userProvidedCqValue) ~ as.numeric(standardConc), SC_plot_data)
      SC_plot_data$Residual <- abs(residuals(regression_line))

      #Code to get R squared
      #Adapted from: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph
      # Code to get equation of the line and R-squared
      # Adapted from: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

      lm_eq <- function(df){

        model1 <- lm(userProvidedCqValue ~ standardConc, df, na.action=na.exclude)

        b = format(unname(coef(model1)[1]), digits = 2)

        mx = paste("(", format(unname(coef(model1)[2]), digits = 2), "x", ")",  sep = "")

        r2 = format(summary(model1)$r.squared, digits = 3)

        equation_of_line <- paste("y", " = ", mx, " + ", b, ",   ", "R-squared", " = ", r2,  sep = "")

        return (equation_of_line)

      }



      print(

        ggplotly(height = 700,

                 ggplot(data = SC_plot_data, aes(x = standardConc,
                                                 y = userProvidedCqValue,
                                                 color = Residual)) +

                   geom_point(size = 10, alpha = .3) +

                   geom_smooth(method = "lm",
                               se = FALSE,
                               alpha = .15,
                               color = 'black',
                               formula = y ~ x) +

                   geom_vline(xintercept = log(SC_plot_data$LOD),
                              color = "#ea5f94",
                              linetype="dashed") +

                   geom_vline(xintercept = log(SC_plot_data$LOQ),
                              color = "#0178A1",
                              linetype="dotted") +

                   geom_text(aes(x= log(LOD),
                                 label="LOD",
                                 y= mean(userProvidedCqValue)),
                             colour="#ea5f94",
                             angle=90,
                             vjust = 1.2,
                             size=5) +

                   geom_text(aes(x= log(LOQ),
                                 label="LOQ",
                                 y= mean(userProvidedCqValue)*1.2),
                             colour="#0178A1",
                             angle=90,
                             vjust = 1.2,
                             size=5) +


                   xlab("log DNA Copy Number") +

                   ylab("Cq") +

                   theme_minimal() +

                   scale_color_gradient(low = "#fbd300", high = "#0000ff") +

                   theme( axis.title.x = element_text( size=13),
                          axis.title.y = element_text( size=13),
                          axis.text.x = element_text(size=12),
                          axis.text.y = element_text(size=12),
                          plot.title = element_text(hjust = 0.5, size=18)) +

                   ggtitle(paste0("Standard Curve for ", input$SC_project_input,  "\n",
                                  lm_eq(SC_plot_data)))

        ) %>%

          layout(margin = list(l=50, r=50, b=100, t=100, pad=4),
                 annotations = list(x = 1.1, y = -0.17,
                                    text = "Source: MDMAPR-CC-BY",
                                    showarrow = F,
                                    xref='paper',
                                    yref='paper',
                                    font=list(size=12, color="darkblue"))))
    })
  })
  )


  ##Presence Absence table

  #Colour cell value by Cqvalue to indicate if species is present or absent
  what_clr <- function(value) {
    if (value >= input$cqValueCutoff)
    {return ("#8FBACB")}

    else
    {return("#ffb14e")}
  }


  available_threshold <- function(value) {
    if (value == "Unable to Determine Threshold")
    {return ("#ffd700")}
  }


  #Transform dataframe for presence/absence table based on filtered
  presence_absence_table_data <- reactive({

    if (dbExists == "Yes" | !is.null(user_uploaded_qPCR_data())) {

      data <- MySQL_mergedData()

      PA_filtered_data <- data[data$assayName == input$SC_assay_input, ]
      PA_filtered_data <- PA_filtered_data[PA_filtered_data$runPlatform == input$SC_machine_input, ]
      PA_filtered_data <- PA_filtered_data[PA_filtered_data$projectName == input$SC_project_input, ]

    }
  })


  #Presence/absence table
  observeEvent(input$DA_submit, isolate ({

    output$presence_absence_table <- renderReactable({
      data <- as.data.frame(presence_absence_table_data())

      prescence_abscence_table <- data[ , c("projectName", "runID", "extractName", "control", "geneSymbol", "runPlatform", "wellLocation", "userProvidedThresholdValue", "userProvidedCqValue", "systemCalculatedThresholdValue", "systemCalculatedCqValue" )]

      reactable(prescence_abscence_table,

                #Table columns
                columns = list(

                  projectName = colDef(name = "Project Name",align = "center", width = 300),
                  runID = colDef(name = "Plate ID", align = "center"),
                  extractName = colDef(name = "Sample Name", align = "center", width = 200),
                  control = colDef(name = "Control", align = "center"),
                  geneSymbol = colDef(name = "Gene", align = "center"),
                  runPlatform = colDef(name = "Machine", align = "center"),
                  wellLocation = colDef(name = "Well Location", align = "center", width = 200),

                  userProvidedThresholdValue = colDef(name = "User Provided Threshold",
                                                      align = "center",
                                                      width = 300),

                  userProvidedCqValue = colDef(name = "User Provided Cq Value",
                                               width = 250,
                                               style = function(value) {
                                                 color  <- what_clr(value)
                                                 list(background = color)}),

                  systemCalculatedThresholdValue = colDef(name = "System Calculated Threshold",
                                                          width = 300,
                                                          align = "center",
                                                          style = function(value) {
                                                            color  <- available_threshold(value)
                                                            list(background = color)}),

                  systemCalculatedCqValue = colDef(name = "System Calculated Cq Value",
                                                   width = 250,
                                                   style = function(value) {
                                                     color  <- what_clr(value)
                                                     list(background = color)})),

                #Filter each column by text
                filterable = TRUE,

                #Type in page number to jump to a page
                paginationType = "jump",

                #Minimum rows shown on page
                minRows = 20,

                #Number of rows to show
                defaultPageSize = 20,

                #Adding outline around cells
                outlined = TRUE,

                #Color every other row
                striped = TRUE,

                #Hover above row to highlight it
                highlight = TRUE,

                #Default record selected from table
                defaultSelected = 1,

                #Check box
                selection = "single",

                #Wrap text in column
                wrap = FALSE,

                theme = reactableTheme(rowSelectedStyle = list(backgroundColor = "#eee",
                                                               boxShadow = "inset 2px 0 0 0 #ffa62d"))
      )
    })
  }))


  ## Amplification plot

  #Get selected row for amplification plot
  selected <- reactive(getReactableState("presence_absence_table", "selected"))

  #Created amplifcation plot based on selected well sample

  observeEvent(input$DA_submit, isolate({

    output$selected <-  renderPlotly({

      #Created dataframe of filtered presence/absence data
      data <- as.data.frame(presence_absence_table_data())[selected(), ]


      #Create data frame for amplification curve
      amp_curve_data <- na.omit(as.data.frame(t(data[ , c(14:83)])))
      colnames(amp_curve_data) <- "Fluorescence"
      amp_curve_data$cycles <- c(1:nrow(amp_curve_data))


      #Created plot
      print(
        ggplotly(height = 700,

                 ggplot(amp_curve_data, aes(x = cycles, y = as.numeric(Fluorescence))) +

                   geom_point(aes(colour = "Absorbances") , size = 2) +

                   geom_hline(aes(yintercept = as.numeric(data$userProvidedThresholdValue),
                                  color = "User Provided Threshold"),
                              linetype="dashed", size = 1) +

                   geom_hline(aes(yintercept = as.numeric(data$systemCalculatedThresholdValue),
                                  color = "System Calculated Threshold"),
                              linetype="dotted", size = 1) +


                   ggtitle(paste0( "Well ", data$wellLocation, " Amplification Curve")) +

                   labs(x = " Cycle", y = "Absorbance") +

                   theme_gray() +

                   scale_colour_manual("",
                                       breaks = c("Absorbances",
                                                  "User Provided Threshold",
                                                  "System Calculated Threshold"),
                                       values = c("User Provided Threshold"="#ea5f94",
                                                  "Absorbances"="#0000ff",
                                                  "System Calculated Threshold"="#ff8600")) +

                   theme(plot.title = element_text(hjust = 0.5, size=18),
                         axis.title.x = element_text( size=13),
                         axis.title.y = element_text( size=13),
                         axis.text.x = element_text(size=12),
                         axis.text.y = element_text(size=12),
                         legend.text = element_text(size = 10),
                         legend.background = element_rect(fill="lightblue")))  %>%

          layout(legend = list(orientation = "h", x = 0.02, y = -0.16), #legend position
                 margin = list(l=50, r=60, b=140, t=100, pad=4),
                 annotations = list(x = 1, y = -0.31,
                                    text = "Source: MDMAPR-CC-BY",
                                    showarrow = F,
                                    xref='paper',
                                    yref='paper',
                                    font=list(size=12,
                                              color="darkblue"))))})
  }))


  #Reset dropdown menu selections for standard curve plot
  observeEvent(input$DA_reset,
               {shinyjs::reset("SC_assay_input") #assay selected
                 shinyjs::reset("SC_machine_input") #machine selected
                 shinyjs::reset("SC_project_input") #project selected
                 shinyjs::reset("SC_input") #standard curve selected
               })


  #Reset Uploaded files
  observeEvent(input$Uploaded_DA_reset,


               {shinyjs::reset("SC_fluorescence_file") #standard curve fluorescence file
                 shinyjs::reset("SC_metadata_file") #reset uploaded standard curve metadata file
                 shinyjs::reset("qPCR_fluorescence_file") #reset uploaded qPCR fluorescene file
                 shinyjs::reset("qPCR_metadata_file") #reset uploaded qPCR metadata file
                 updateSelectInput(session,
                                   "DA_platform",
                                   label = "qPCR Platform",
                                   choices = c("None",
                                               "StepOnePlus",
                                               "Biomeme two3/Franklin",
                                               "MIC"),
                                   selected = "None") #reset selected platform
               })


  #Standarc Curve Data table
  output$SC_overview_table  <- renderDataTable({

    data <- standard_curve_data_for_plot()[ , -c(1, 2, 3, 4, 102)]

    datatable(data,
              options = list(scrollX = TRUE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '500px', targets = c(84)))))})



  #Data Submission Page   ---------------------------

  #Return popup messaged regarding uploaded fluorescence file
  observeEvent(input$DS_qpcr_file,
               fluorescence_file_validation_msgs(input$DS_qpcr_file))


  #Return popup messaged regarding uploaded standard curve fluorescence file
  observeEvent(input$DS_standardCurve_fluorescence_file,
               fluorescence_file_validation_msgs(input$DS_standardCurve_fluorescence_file))


  #Return popup messaged regarding uploaded metadata file file
  observeEvent(input$DS_metadata_file,
               metadata_file_validation_msgs(input$DS_metadata_file))


  #Return popup messaged regarding uploaded fluorescence and metadata file, based on machine type.
  observeEvent(req(input$DS_qpcr_file,
                   input$DS_standardCurve_fluorescence_file,
                   input$DS_metadata_file,
                   input$DS_platform),

               selected_platform_validation_msgs_SC_and_Experimental_flur(input$DS_qpcr_file,
                                                                          input$DS_standardCurve_fluorescence_file,
                                                                          input$DS_metadata_file,
                                                                          input$DS_platform))

  #Validate messaged based on uploaded data on Data Submission page.
  DS_error_message <- reactive({
    validate(
      need(input$DS_qpcr_file, 'No fluorescence file uploaded'),
      need(input$DS_metadata_file, 'No metadata file uploaded'),
      need(try(file_ext(input$DS_qpcr_file) == "csv" |
                 file_ext(input$DS_qpcr_file) == "xlsx" |
                 file_ext(input$DS_qpcr_file) == "xls")
           ,
           "fluorescence file must be csv or xlsx/xls"),
      need(try(file_ext(input$DS_metadata_file) == "xlsx" |
                 file_ext(input$DS_metadata_file) == "xls")
           , "Metadata file must be xlsx/xls")
    )
  })

  output$DS_error_msg <- renderText({DS_error_message()})


  #Function to process uploaded qPCR fluorescence and metadata on data submission page
  uploaded_data_dataSubmissionPage <-  reactive({

    #No qPCR file input
    if (user_uploaded_file_validate(input$DS_qpcr_file,
                                    input$DS_metadata_file,
                                    input$DS_platform) == TRUE)
    {return(NULL)}


    else{
      if (input$DS_platform == "Biomeme two3/Franklin") {

        #Read in raw qPCR data
        qpcr_biomem23_raw <- process_biomeme23_uploaded_file(read.csv(input$DS_qpcr_file$datapath))

        #Read in metadata
        metadata_biomem23 <- format_qPCR_metadata(input$DS_metadata_file$datapath)


        #Function to process and merge files
        merged_biomem23_file <- merge_metadata_fluorescence_file(qpcr_biomem23_raw, metadata_biomem23)

        #This merged datatable that will be used to populate map
        return(merged_biomem23_file)
      }


      ### Machine Option 2: MIC ###
      else if (input$DS_platform == "MIC") {

        #Read in raw qPCR data
        qpcr_MIC_raw  <- process_MIC_uploaded_file(read.csv(input$DS_qpcr_file$datapath))


        #Read in metadata
        metadata_MIC <- format_qPCR_metadata(input$DS_metadata_file$datapath)


        #Function to process and merge files
        merged_mic_file <- merge_metadata_fluorescence_file(qpcr_MIC_raw, metadata_MIC)


        #This merged datatable that will be used to populate map
        return(merged_mic_file)
      }


      ### Machine Option 3: StepOnePlus ###
      else if (input$DS_platform == "StepOnePlus") {


        #Read in raw qPCR data
        qpcr_StepOnePlus_raw <- process_SOP_uploaded_file(read_excel(input$DS_qpcr_file$datapath,
                                                                     4))

        #Read in metadata
        metadata_StepOnePlus <-format_qPCR_metadata(input$DS_metadata_file$datapath)


        #Function to process and merge files
        merged_StepOnePlus_file <- merge_metadata_fluorescence_file(qpcr_StepOnePlus_raw,
                                                                    metadata_StepOnePlus)

        return(merged_StepOnePlus_file)

      }
    }})


  #Function to process uploaded standard curve fluorescence and metadata on data submission page

  uploaded_standard_curve_data_dataSubmissionPage <-  reactive({

    #No qPCR file input
    if (user_uploaded_standard_curve_file_validation(input$DS_standardCurve_fluorescence_file,
                                                     input$DS_metadata_file, input$DS_platform) == TRUE)
    {return(NULL)}


    else{

      if (input$DS_platform == "Biomeme two3/Franklin") {

        #Read in raw qPCR data
        qpcr_biomem23_raw <- process_biomeme23_uploaded_file(read.csv(input$DS_standardCurve_fluorescence_file$datapath))

        #Read in metadata
        metadata_biomem23 <- format_standardCurve_metadata(read_xlsx(input$DS_metadata_file$datapath, sheet = 5))

        #Function to process and merge files
        merged_biomem23_file <- merge_standardCurve_metadata_fluorescence_file(qpcr_biomem23_raw, metadata_biomem23)

        #This merged datatable that will be used to populate map
        return(merged_biomem23_file)

      }


      ### Machine Option 2: MIC ###
      else if (input$DS_platform == "MIC") {

        #Read in raw qPCR data
        qpcr_MIC_raw  <- process_MIC_uploaded_file(read.csv(input$DS_standardCurve_fluorescence_file$datapath))

        #Read in metadata
        metadata_MIC <- format_standardCurve_metadata(read_xlsx(input$DS_metadata_file$datapath, sheet = 5))

        #Function to process and merge files
        merged_mic_file <- merge_standardCurve_metadata_fluorescence_file(qpcr_MIC_raw, metadata_MIC)


        #This merged datatable that will be used to populate map
        return(merged_mic_file)
      }

      ### Machine Option 3: StepOnePlus ###
      else if (input$DS_platform == "StepOnePlus") {

        #Read in raw qPCR data
        qpcr_StepOnePlus_raw <- process_SOP_uploaded_file(read_excel(input$DS_standardCurve_fluorescence_file$datapath, 4))

        #Read in metadata
        metadata_StepOnePlus <- format_standardCurve_metadata(read_xlsx(input$DS_metadata_file$datapath, sheet = 5))

        #Function to process and merge files
        merged_StepOnePlus_file <- merge_standardCurve_metadata_fluorescence_file(qpcr_StepOnePlus_raw,
                                                                                  metadata_StepOnePlus)


        return(merged_StepOnePlus_file)

      }

    }})



  #Show tables of uploaded data on data Submission Page ************************

  #project information Table
  project_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    project_table <- create_project_table(uploaded_merged_data)

  })


  output$projectTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)

    datatable(project_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })



  #Geographic Region Table
  geographicRegion_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    geographicRegion_table <- create_geographicRegion_Table(uploaded_merged_data)

  })


  output$geographicRegionTable <- renderDataTable({
    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)

    datatable(geographicRegion_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })


  #Site Table
  site_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    site_table <- create_site_Table(uploaded_merged_data)

  })


  output$siteTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)

    datatable(site_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })


  #Station Table
  station_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    station_table <- create_station_Table(uploaded_merged_data)

  })


  output$stationTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)

    datatable(station_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })



  #Replicate Table
  replicate_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    replicate_table <- create_replicate_Table(uploaded_merged_data)

  })


  output$replicateTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)

    datatable(replicate_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })


  #Extract Table
  extract_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    extract_table <- create_extract_Table(uploaded_merged_data)
  })


  output$extractTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)

    datatable(extract_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })



  #Run Information Table
  runInformation_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    uploaded_SC_merged_data <- as.data.frame(uploaded_standard_curve_data_dataSubmissionPage())

    runInformation_table <- create_runInformation_Table(uploaded_merged_data, uploaded_SC_merged_data)
  })


  output$runInformationTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)
    req(input$DS_standardCurve_fluorescence_file)

    datatable(runInformation_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })



  #Results Table
  results_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    results_table <- create_results_Table(uploaded_merged_data)

  })


  output$resultsTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)

    datatable(results_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })


  #Assay Table
  assay_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    assay_table <- create_assay_Table(uploaded_merged_data)

  })


  output$assayTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)

    datatable(assay_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })


  #taxonomic Table
  taxon_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    taxon_table <- create_taxon_Table(uploaded_merged_data)

  })


  output$taxonomicTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)

    datatable(taxon_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })


  #PCR Chemistry Table
  pcrChemistry_data <- reactive({

    uploaded_merged_data <- as.data.frame(uploaded_data_dataSubmissionPage())
    uploaded_SC_merged_data <- as.data.frame(uploaded_standard_curve_data_dataSubmissionPage())
    pcrChemistry_table <- create_pcrChemistry_Table(uploaded_merged_data, uploaded_SC_merged_data)

  })


  output$pcrChemistryTable <- renderDataTable({

    req(input$DS_qpcr_file)
    req(input$DS_metadata_file)
    req(input$DS_standardCurve_fluorescence_file)

    datatable(pcrChemistry_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })


  #Standard Curve Table
  standardCurve_data <- reactive({

    uploaded_SC_merged_data <- as.data.frame(uploaded_standard_curve_data_dataSubmissionPage())
    standardCurve_table <- create_standardCurve_Table(uploaded_SC_merged_data)

  })


  output$standardCurveTable <- renderDataTable({

    req(input$DS_standardCurve_fluorescence_file)
    req(input$DS_metadata_file)

    datatable(standardCurve_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })


  #Standard Curve Results Table
  standardCurveResults_data <- reactive({

    uploaded_SC_merged_data <- as.data.frame(uploaded_standard_curve_data_dataSubmissionPage())
    standardCurveResults_table <- create_standardCurveResults_Table(uploaded_SC_merged_data)
  })


  output$standardCurveResultsTable <- renderDataTable({

    req(input$DS_standardCurve_fluorescence_file)
    req(input$DS_metadata_file)

    datatable(standardCurveResults_data(),
              rownames = FALSE,
              options = list(scrollX = TRUE))
  })


  # Download Data Submission files
  #Adapted from: https://groups.google.com/forum/#!topic/shiny-discuss/zATYJCdSTwk
  output$downloadData <- downloadHandler(
    filename = 'MySQL_Upload_data.zip',
    content = function(fname) {
      fs <- c("projectDataTable.csv",
              "geographicRegionTable.csv",
              "siteTable.csv",
              "stationTable.csv",
              "replicateTable.csv",
              "extractTable.csv",
              "runInformationTable.csv",
              "resultsTable.csv",
              "assayTable.csv",
              "taxonTable.csv",
              "pcrChemistryTable.csv",
              "standardCurveTable.csv",
              "standardCurveResultsTable.csv")

      write.csv(
        project_data(),
        file = "projectDataTable.csv",
        sep = ",",
        row.names = FALSE,
        na = "NULL")

      write.csv(
        geographicRegion_data(),
        file = "geographicRegionTable.csv",
        sep = ",",
        row.names = FALSE,
        na = "NULL")

      write.csv(site_data(),
                file = "siteTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(station_data(),
                file = "stationTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(replicate_data(),
                file = "replicateTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(extract_data(),
                file = "extractTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(runInformation_data(),
                file = "runInformationTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(results_data(),
                file = "resultsTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(assay_data(),
                file = "assayTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(taxon_data(),
                file = "taxonTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(pcrChemistry_data(),
                file = "pcrChemistryTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(standardCurve_data(),
                file = "standardCurveTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      write.csv(standardCurveResults_data(),
                file = "standardCurveResultsTable.csv",
                sep = ",",
                row.names = FALSE,
                na="NULL")

      zip(zipfile=fname, files=fs)
    },

    contentType = "application/zip")


  #Reset uploaded file input and selected machine type on data submission page
  observeEvent(input$DS_reset, {
    updateSelectInput(session, "DS_platform",
                      label = "qPCR Platform",
                      choices = c("None",
                                  "StepOnePlus",
                                  "Biomeme two3/Franklin",
                                  "MIC"),
                      selected = "None")

    shinyjs::reset("DS_qpcr_file")
    shinyjs::reset("DS_metadata_file")
    shinyjs::reset("DS_standardCurve_fluorescence_file")})


  ## Get Started Page (Creat downloadable metadata template) -----

  #Created  metadata template for users to download
  project_sheet <- data.frame(matrix(ncol = 24, nrow = 1))

  colnames(project_sheet) <- c("projectID", "projectCreationDate","projectName","projectRecordedBy","projectOwner","projectContactEmail","projectDescription","InstitutionID","projectDataNotes","geographicRegionID", "continent","country", "stateProvince","municipality","siteID", "locality","estimatedPerimeter","estimatedSurfaceArea(m2)","siteType","siteLength(m2)", "stationID","stationName", "decimalLongitude", "decimalLatitude")


  replicate_sheet <- data.frame(matrix(ncol = 55, nrow = 1))
  colnames(replicate_sheet) <- c("replicateID", "stationID", "collectorName","replicateName","collectionDate","collectionTime","storageID","DateOfStorage","methodOfStorage","minimumElevationInMeters","maximumElevationInMeters","verbatimElevation","minimumDepthInMeters","maximumDepthInMeters","verbatimDepth","flowRate(m/s)", "filterType","filtrationDuration(mins)","volumeFiltered","processLocation","replicationNumber","riparianVegetationPercentageCover","dissolvedOxygen(mg/L)","waterTemperature(C)","pH","TSS(mg/L)","EC(uS/cm)","turbidity(NTU)","discharge","tide","chlorophyl","salinity(ppt)","contaminants(ng/g)","traceMetals(mg/kg)","organicContent(%)","microbialActivity","grainSize","replicateDataNotes", "extractID", "extractName","analyst", "extractionDate", "extractionTime", "location", "extractionMethod", "methodCitation", "extractionNotes","tubePlateID","frozen", "fixed","dnaStorageLocation","extractMethodOfStorage","dnaVolume","quantificationMethod", "concentration(ng/ul)")


  assay_sheet <- data.frame(matrix(ncol = 30, nrow = 1))
  colnames(assay_sheet) <- c( "assayID", "establishmentMeans","assayName","assayOwnership","assayDescription", "assayCitation", "assayDate", "geneTarget", "geneSymbol","dilutions", "replicates", "primerR", "primerF", "probe","ampliconLength (bp)", "probeFluorescentTag", "dye(s)","quencher","probeModification", "taxonID", "kingdom","phylum","class","order","family", "genus", "subgenus", "species", "vernacularName","organismScope")

  results_sheet <- data.frame(matrix(ncol = 29, nrow = 1))
  colnames(results_sheet) <- c("resultID","assayID", "extractID", "wellLocation","sampleName", "copyNumber", "control", "userProvidedThresholdValue", "userProvidedCqValue", "runID", "runRecordedBy", "runDate", "runTime","runPlatform","machineID", "pcrChemistryID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration","probeConcentration", "Mg2+Concentration", "polymeraseBatch","polymeraseConcentrations","thermocyclerParameters", "pcrDataNotes")

  standardCurveResults_sheet <- data.frame(matrix(ncol = 36, nrow = 1))
  colnames(standardCurveResults_sheet ) <- c("SCresultID","wellLocation","sampleName", "copyNumber", "control","standardConc",   "userProvidedThresholdValue", "userProvidedCqValue","runID", "runRecordedBy", "runDate", "runTime", "runPlatform","machineID", "standardCurveID","assayID", "standardCurveName", "SCdate", "SCrecordedBy", "SCdataNotes", "LOD","LOQ", "pcrChemistryID","reactionConditions","reactionVolume","templateAmount","forwardPrimerBatch", "reversePrimerBatch", "dNTPConcentration", "primerConcentration","probeConcentration", "Mg2+Concentration", "polymeraseBatch","polymeraseConcentrations","thermocyclerParameters", "pcrDataNotes")


  data_list <- (list( project_Table = project_sheet,
                      replicate_Table = replicate_sheet,
                      assay_Table = assay_sheet,
                      results_Table = results_sheet,
                      standardCurveResults_Table = standardCurveResults_sheet ))

  output$downloadTemplate <- downloadHandler(
    filename = 'MDMap_metadata_template.xlsx',
    content = function(file) {write_xlsx(data_list, file)})



  #Available Data page ---------------------------

  output$availableDataTable <- renderReactable({

    if (dbExists == "Yes") {

      availableAssays <- my_sql_data[ , c("taxonID", "assayID", "assayName", "assayOwnership", "assayDescription", "assayCitation", "assayDate", "establishmentMeans", "geneTarget", "geneSymbol", "dilutions", "replicates", "primerR", "primerF", "probe", "ampliconLength (bp)", "probeFluorescentTag", "dye(s)", "quencher",  "probeModification")]

      availableAssays  <- distinct(availableAssays)

      taxonData <- as.data.frame(unique(my_sql_data[, c("taxonID", "kingdom", "phylum", "class", "order", "family", "genus", "subgenus", "species", "vernacularName", "organismScope")]))


      reactable(taxonData,
                details = function(index) {
                  assay_data <- availableAssays[availableAssays$taxonID == taxonData$taxonID[index], ]
                  htmltools::div(style = "padding: 16px",
                                 reactable(assay_data[ , -c(1)],

                                           columns = list(
                                             assayID = colDef(name = "Assay ID", align = "left"),
                                             assayName = colDef(name = "Name", align = "left"),
                                             assayOwnership = colDef(name = "Ownership",
                                                                     align = "left"),

                                             assayDescription = colDef(name = "Description",
                                                                       align = "left"),

                                             assayCitation = colDef(name = "Citation",
                                                                    align = "left"),

                                             assayDate = colDef(name = "Date", align = "left"),

                                             establishmentMeans = colDef(name = "Establishment Means",
                                                                         align = "left",
                                                                         minWidth = 500),

                                             geneTarget = colDef(name = "Gene Target",
                                                                 align = "left",
                                                                 minWidth = 500),

                                             geneSymbol = colDef(name = "Gene Symbol",
                                                                 align = "left"),

                                             dilutions = colDef(name = "Dilutions",
                                                                align = "left"),

                                             replicates = colDef(name = "Replicates",
                                                                 align = "left"),

                                             primerR = colDef(name = "Reverse Primer",
                                                              align = "left"),

                                             primerF = colDef(name = "Forward Primer",
                                                              align = "left"),

                                             probe = colDef(name = "Probe", align = "left"),

                                             `ampliconLength (bp)` = colDef(name = "Amplicon Length (bp)",
                                                                            align = "left",
                                                                            minWidth = 500),

                                             probeFluorescentTag = colDef(name = "Probe Fluorescent Tag",
                                                                          align = "left",
                                                                          minWidth = 500),


                                             quencher = colDef(name = "Quencher", align = "left"),

                                             probeModification = colDef(name = "Probe Modification",
                                                                        align = "left",
                                                                        minWidth = 500)),

                                           outlined = TRUE,

                                           wrap =  FALSE,

                                           defaultColDef = colDef(minWidth = 300),
                                           theme = reactableTheme(
                                             rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d"))))},

                columns = list(
                  taxonID = colDef(name = "Taxon ID", align = "left"),
                  kingdom = colDef(name = "Kingdom", align = "left"),
                  phylum = colDef(name = "Phylum", align = "left"),
                  class = colDef(name = "Class", align = "left"),
                  order = colDef(name = "Order", align = "left"),
                  family = colDef(name = "Family", align = "left"),
                  genus = colDef(name = "Genus", align = "left"),
                  subgenus = colDef(name = "Subgenus", align = "left"),
                  species = colDef(name = "Species", align = "left", minWidth = 500),
                  vernacularName = colDef(name = "Vernacular Name",
                                          align = "left",
                                          minWidth = 500),
                  organismScope = colDef(name = "Organism Scope",
                                         align = "left",
                                         minWidth = 500)),

                theme = reactableTheme(
                  rowSelectedStyle = list(backgroundColor = "#eee",
                                          boxShadow = "inset 2px 0 0 0 #ffa62d")),

                #Wrap column text
                wrap = FALSE,

                #Default column width
                defaultColDef = colDef(minWidth = 300),

                #Type in page number to jump to a page
                paginationType = "jump",

                #Minimum rows shown on page
                minRows = 10,

                #Number of rows to show
                defaultPageSize = 10,

                #Adding outline around cells
                outlined = TRUE,

                striped = TRUE,

                highlight = TRUE)} })




}
