# Written by Rob Young at the University of Guelph in Ontario Canada, May, 2024
# ********************************************Main program section*************

################# Server Function #############################################

#server <- function(input, output, session) {
shinyAppServer <- function(input, output, session, verbose) {

  ############## Initialize variables ############################################

  #Flag to stop the message about the data loaded not appearing the first time
  dataFlag <- reactiveValues(data=0)

  #dada implement reactive values
  primerFile <- reactiveValues(data = NA)
  dadaLocation <- reactiveValues(data = NA)

  #dada combine reactive values
  dadaCombineLoc <- reactiveValues(data = NA)

  #Make a BLASTable data base reactive values
  makeBlastDBFileLoc <- reactiveValues(data = NA)
  makeblastdbPath <- reactiveValues(data = NA)
  makeBlastTaxaDBLoc <- reactiveValues(data = NA)

  #Make a seqBLAST reactive values
  BLASTDatabasePath <- reactiveValues(data = NA)
  blastnPath <- reactiveValues(data = NA)
  querySeqPath <- reactiveValues(data = NA)

  # Taxon Assign
  taxaAssignFileLoc <- reactiveValues(data = NA)
  taxaAssignDBLoc <- reactiveValues(data = NA)

  #Combine Taxa Files
  combineTaxaFileLoc <- reactiveValues(data = NA)

  #Reduce Taxa Files
  reduceTaxaFileLoc <- reactiveValues(data = NA)

  #Combine Reduce Taxa Files
  combineReducedTaxaFileLoc <- reactiveValues(data = NA)

  #Other Variables
  ASVFile <- reactiveValues(data = NA)
  provenanceDataFile <- reactiveValues(data = NA)
  output$ASVFileOut <- shiny::renderText({as.character("No data file selected")})
  output$provenanceDataFileOut <- shiny::renderText({as.character("No data file selected")})
  ASVFileTable<-reactiveValues(data = NA)
  provenanceDataFileTable<-reactiveValues(data = NA)

  #Filtering variables
  markerColumns <- NULL #this is the number of columns for the combine markers output file
  maxReads <- reactiveValues(data = NA)
  mergedTable <- reactiveValues(data = NA)
  workMergedTable <- reactiveValues(data = NA)
  firstLoadFlag <- reactiveValues(data = 0)

  #Dada files
  dadaDirectoryDisplayString<-reactiveValues(data = NA)
  primerFileDisplayString<-reactiveValues(data = NA)
  #Dada Combine
  dadaCombineFileDisplayString<-reactiveValues(data = NA)
  #Make DB
  makeBlastDBFileLocDisplayString<-reactiveValues(data = NA)
  makeblastdbPathDisplayString<-reactiveValues(data = NA)
  makeBlastTaxaDBLocDisplayString<-reactiveValues(data = NA)
  #BLAST
  BLASTDatabasePathDisplayString<-reactiveValues(data = NA)
  blastnPathDisplayString<-reactiveValues(data = NA)
  querySeqPathDisplayString<-reactiveValues(data = NA)
  #Taxon Assign
  taxaAssignFileLocDisplayString<-reactiveValues(data = NA)
  taxaAssignDBLocDisplayString<-reactiveValues(data = NA)
  #Combine Taxa Assign
  combineTaxaFileLocDisplayString<-reactiveValues(data = NA)
  #Reduce taxa assign
  reduceTaxaFileLocDisplayString<-reactiveValues(data = NA)
  #Combine Reduced Taxa assign
  combineReducedTaxaFileLocDisplayString<-reactiveValues(data = NA)

  #Mapping
  ASVFileDisplayString<-reactiveValues(data = NA)
  provenanceDataFileDisplayString<-reactiveValues(data = NA)
  filteValues<-reactiveValues(AVal = NA, BVal = NA, CVal = NA, DVal = NA,
                              EVal = NA, FVal = NA, GVal = NA,HVal = NA,
                              IVal = NA, JVal = NA, CamVal = NA, KVal = NA, LVal = NA,
                              MVal = NA, NVal = NA, OVal = NA, PVal = NA,
                              QVal = NA)

  # Get the path where all of the folders containing the fastq files are located
  volumes = shinyFiles::getVolumes()

  ###################### Initialize the Map ######################################
  #Building the initial map and using the
  #Default static leaflet map before filtering parameters are applied.


  #Define the colour Palette for leaflet map
  palette_map <- leaflet::colorFactor(palette = c("#fbd300",
                                                  "#ff8600",
                                                  "#ea5f94",
                                                  "#9d02d7",
                                                  "#0000ff",
                                                  "#00008b"),
                                      domain = c("10", "100", "1000", "10000", "100000", "1000000"))

  output$mymap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addLegend(position = "topright",
                         pal = palette_map,
                         values = c("10", "100", "1000", "10000", "100000", "1000000"),
                         title = "Reads",
                         opacity = 0.6) %>%

      #View map full screen (note: only works in web browser)
      leaflet.extras::addFullscreenControl() %>%

      #Default map view --> Change to Guelph
      leaflet::setView(lng = -80.2262, lat = 43.5327, zoom = 3) %>%

      #Change leaflet map tiles
      leaflet::addProviderTiles(providers$Esri.WorldStreetMap)
  })

  ################## mappingDashboard Click observe event #######################

  shiny::observeEvent(input$map_filter_table_tabbox, {
    if (input$map_filter_table_tabbox == "Mapping"){
      if(!is.data.frame(ASVFileTable$data) && !is.data.frame(provenanceDataFileTable$data)){
        shiny::showModal(shiny::modalDialog(
          title = "Mapping Dashboard - No Data Loaded - 1",
          "There are no data loaded in this instance of DBTCShine. Please go to the Data Import tab to upload data."
        ))
      }else if(length(ASVFileTable$data) == 0 && length(provenanceDataFileTable$data) == 0){
        shiny::showModal(shiny::modalDialog(
          title = "Mapping Dashboard - No Data Loaded - 2",
          "There are no data loaded in this instance of DBTCShine. Please go to the Data Import tab to upload data."
        ))

      }else if (!is.data.frame(mergedTable$data) && !is.data.frame(workMergedTable$data)){
        shiny::showModal(shiny::modalDialog(
          title = "Mapping Dashboard - No Data Loaded - 3",
          "There are no data loaded in this instance of DBTCShine. Please go to the Data Import tab to upload data."
        ))
      }else if (length(mergedTable$data)==0 && length(workMergedTable$data)==0 ){
        shiny::showModal(shiny::modalDialog(
          title = "Mapping Dashboard - No Data Loaded - 4",
          "There are no data loaded in this instance of DBTCShine. Please go to the Data Import tab to upload data."
        ))
      }else{
        #Resetting the data mapping points for the first time
        setMappingDataPoints()
      }
    }else if(input$map_filter_table_tabbox == "Data Filtering"){
      if(is.data.frame(workMergedTable$data)){
        filterOptionsUpdate()
      }
    }else if(input$map_filter_table_tabbox == "Data Table"){
      if(is.data.frame(workMergedTable$data)){
        setDataTable()
      }
    }
  },ignoreInit = TRUE)



  ################## Dada Submit Function #####################################
  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "dadaDirectory", roots = volumes, session = session)

  # Get the file with the primer data for this analysis
  shiny::observeEvent(input$dadaDirectory, {

    if(!is.null(input$dadaDirectory)){
      tryCatch(
        expr = {

          dadaDirectory <- shinyFiles::parseFilePaths(volumes, input$dadaDirectory)
          dadaDirectoryDisplayString$data <- as.character(dadaDirectory$datapath)
          output$dadaDirectoryDisplay <- shiny::renderText({as.character(dadaDirectoryDisplayString$data)})

        },
        error = function(e){
          if(verbose){
            print("Error - Dada Location Button choose file cancelled")
          }
        },
        warning = function(w){
          if(verbose){
            print("Warning - Dada Location Button choose file cancelled")
          }
        }
      )
    }
  },ignoreInit = TRUE)

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "primerFile", roots = volumes, session = session)

  # Get the file with the primer data for this analysis
  shiny::observeEvent(input$primerFile, {
    if(!is.null(input$primerFile)){
      tryCatch(
        expr = {

          primerFile <- shinyFiles::parseFilePaths(volumes, input$primerFile)
          primerFileDisplayString$data <- as.character(primerFile$datapath)
          output$primerFileDisplay <- shiny::renderText({as.character(primerFileDisplayString$data)})

        },
        error = function(e){
          if(verbose){
            print("Error - Primer Location Button choose file cancelled")
          }
        },
        warning = function(w){
          if(verbose){
            print("Warning - Primer Location Button choose file cancelled")
          }

        }
      )
    }
  },ignoreInit = TRUE)

  shiny::observeEvent(input$dadaSubmit, {
    if (!is.na(dadaDirectoryDisplayString$data) && is.character(dadaDirectoryDisplayString$data) && length(dadaDirectoryDisplayString$data) != 0){
      # Create variables to call the dada_implement so that there are no conflicts
      # with the multithreading and the shiny app
      runFolderLoc <- force(dadaDirectoryDisplayString$data)
      primerFile <- force(primerFileDisplayString$data)

      #This try catch sets up the Shiny to send the appropriate value of NA if the
      # primerFile is not included or if the selection window is cancelled.
      tryCatch(
        expr = {
          if(length(primerFile)==0){
            primerFile = NA
          }
        },
        error = function(e){
          primerFile = NA
        },
        warning = function(w){
          primerFile = NA
        })

      if (force(input$uniOrbidirectional) == "Unidirectional"){
        unidirectional = TRUE
        bidirectional = FALSE
        fwdIdent <- ""
        revIdent <- ""
      }else if(force(input$uniOrbidirectional) == "Bidirectional"){
        unidirectional = FALSE
        bidirectional = TRUE
        fwdIdent <- force(input$fwdIdent)
        revIdent <- force(input$revIdent)
      }else{
        unidirectional = TRUE
        bidirectional = TRUE
        fwdIdent <- force(input$fwdIdent)
        revIdent <- force(input$revIdent)
      }
      printQualityPdf <- force(input$printQualityPdf)
      maxPrimeMis <- force(input$maxPrimeMis)
      fwdTrimLen <- force(input$fwdTrimLen)
      revTrimLen <- force(input$revTrimLen)
      maxEEVal <- force(input$maxEEVal)
      truncQValue <- force(input$truncQValue)
      truncLenValueF <- force(input$truncLenValueF)
      truncLenValueR <- force(input$truncLenValueR)
      error <- force(input$error)
      nbases <- force(input$nbases)
      maxMismatchValue <- force(input$maxMismatchValue)
      minOverlapValue <- force(input$minOverlapValue)
      trimOverhang <- force(input$trimOverhang)
      minFinalSeqLen <- force(input$minFinalSeqLen)

      tryCatch(
        expr = {
          #Run the Dada function here.

          shiny::showModal(shiny::modalDialog(
            title = "Dada analysis is underway.",
            "Processing, please stand by...", footer=""

          ))

          DBTC::dada_implement(runFolderLoc = runFolderLoc,
                         primerFile = primerFile,
                         fwdIdent = fwdIdent,
                         revIdent = revIdent,
                         unidirectional = unidirectional,
                         bidirectional = bidirectional,
                         printQualityPdf = printQualityPdf,
                         maxPrimeMis = maxPrimeMis,
                         fwdTrimLen = fwdTrimLen,
                         revTrimLen = revTrimLen,
                         maxEEVal = maxEEVal,
                         truncQValue = truncQValue,
                         truncLenValueF = truncLenValueF,
                         truncLenValueR = truncLenValueR,
                         error = error,
                         nbases = nbases,
                         maxMismatchValue = maxMismatchValue,
                         minOverlapValue = minOverlapValue,
                         trimOverhang = trimOverhang,
                         minFinalSeqLen = minFinalSeqLen)
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Dada analysis is complete",
            "Please see output files in the target
           directory."
          ))
        },
        error = function(e){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Dada Implement - Please refer to the R consol for more information - 1"
          ))
          if(verbose){
            print("Error - Dada Implement - 1")
          }
        },
        warning = function(w){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Warning",
            "Dada Implement - Please refer to the R consol for more information - 2"
          ))
          if(verbose){
            print("Warning - Dada Implement - 2")
          }
        })
    }else{
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "Please select an appropriate fastq file location and try submitting again!"
      ))
      if(verbose){
        print("Warning - Dada Implement primer file and dada location incorrect - 3")
      }
    }
  },ignoreInit = TRUE)

  ################## Dada Combine Function ####################################
  #Get the location of the dada output files you would like to combine

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "dadaCombineFile", roots = volumes, session = session)

  # Get the file with the primer data for this analysis
  shiny::observeEvent(input$dadaCombineFile, {

    if(!is.null(input$dadaCombineFile)){

      tryCatch(
        expr = {
          dadaCombineFile <- shinyFiles::parseFilePaths(volumes, input$dadaCombineFile)
          dadaCombineFileDisplayString$data <- as.character(dadaCombineFile$datapath)
          output$dadaCombineDisplay <- shiny::renderText({as.character(dadaCombineFileDisplayString$data)})
        },
        error = function(e){
          if(verbose){
            print("Error - Incorrect Dada Combine File(s) location or Dada Combine choose button cancelled.")
          }
        },
        warning = function(w){
          if(verbose){
            print("Warning - Incorrect Dada Combine File(s) location or Dada Combine choose button cancelled.")
          }
        }
      )
    }
  })

  #Running the data combine
  shiny::observeEvent(input$dadaCombine, {
    if (!is.na(dadaCombineFileDisplayString$data) && is.character(dadaCombineFileDisplayString$data) && length(dadaCombineFileDisplayString$data) != 0){
      # Create variables for the arguments to avoid conflicts between the multithreading
      # and the shiny
      fileLoc= force(dadaCombineFileDisplayString$data)
      minLen = force(input$dadaCombineMinLen)
      tryCatch(
        expr = {
          #Run the Dada function here.
          shiny::showModal(shiny::modalDialog(
            title = "Dada combine analysis is underway.",
            "Processing, please stand by...", footer=""
          ))

          #Run the Dada combine function here.
          DBTC::combine_dada_output(fileLoc = fileLoc, minLen = minLen)

          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Dada combine analysis complete",
            "Please see output files in the target directory."
          ))
        },
        error = function(e){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        },
        warning = function(w){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Warning",
            "Please refer to the R consol for more information."
          ))
        }
      )

    }else{
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "Please select the file location where all of the DBTC dada output files are located that you wish to combine!"
      ))
    }
  },ignoreInit = TRUE)

  ################## Make BLAST DB Function ###################################

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "makeBlastDBFileLoc", roots = volumes, session = session)

  # Get the fasta file you want to use to build your db
  shiny::observeEvent(input$makeBlastDBFileLoc, {
    tryCatch(
      expr = {

        makeBlastDBFileLoc <- shinyFiles::parseFilePaths(volumes, input$makeBlastDBFileLoc)
        makeBlastDBFileLocDisplayString$data <- as.character(makeBlastDBFileLoc$datapath)
        output$makeBlastDBFileLocDisplay <- shiny::renderText({as.character(makeBlastDBFileLocDisplayString$data)})

      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        makeBlastDBFileLoc$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        makeBlastDBFileLoc$data <- NA
      }
    )
  },ignoreInit = TRUE)

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "makeblastdbPath", roots = volumes, session = session)

  # Select where the makeblastdb program is on your computer
  shiny::observeEvent(input$makeblastdbPath, {
    tryCatch(
      expr = {

        makeblastdbPath <- shinyFiles::parseFilePaths(volumes, input$makeblastdbPath)
        makeblastdbPathDisplayString$data <- as.character(makeblastdbPath$datapath)
        output$makeblastdbPathDisplay <- shiny::renderText({as.character(makeblastdbPathDisplayString$data)})

      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        makeblastdbPath$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        makeblastdbPath$data <- NA
      }
    )
  },ignoreInit = TRUE)

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "makeBlastTaxaDBLoc", roots = volumes, session = session)

  # Point to the NCBI taxonomic data base
  shiny::observeEvent(input$makeBlastTaxaDBLoc, {
    tryCatch(
      expr = {

        makeBlastTaxaDBLoc <- shinyFiles::parseFilePaths(volumes, input$makeBlastTaxaDBLoc)
        makeBlastTaxaDBLocDisplayString$data <- as.character(makeBlastTaxaDBLoc$datapath)
        output$makeBlastTaxaDBLocDisplay <- shiny::renderText({as.character(makeBlastTaxaDBLocDisplayString$data)})

      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        makeBlastTaxaDBLoc$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        makeBlastTaxaDBLoc$data <- NA
      }
    )
  },ignoreInit = TRUE)

  # Run the making BLAST db code.
  shiny::observeEvent(input$makeBlastDB, {
    if(is.na(makeblastdbPathDisplayString$data)){
      makeblastdbPathDisplayString$data <- "makeblastdb"
    }
    if (!is.na(makeBlastDBFileLocDisplayString$data) && is.character(makeBlastDBFileLocDisplayString$data) && length(makeBlastDBFileLocDisplayString$data) != 0 &&
        !is.na(makeBlastTaxaDBLocDisplayString$data) && is.character(makeBlastTaxaDBLocDisplayString$data) && length(makeBlastTaxaDBLocDisplayString$data)!= 0 ){

      # Create local variables to avoid conflicts with shiny and multithread
      fileLoc = force(makeBlastDBFileLocDisplayString$data)

      if(is.character(makeblastdbPathDisplayString$data) && length(makeblastdbPathDisplayString$data) != 0){

        makeblastdbPath = force(makeblastdbPathDisplayString$data)

      } else {

        makeblastdbPath = "makeblastdb"

      }

      taxaDBLoc = force(makeBlastTaxaDBLocDisplayString$data)
      dbName = force(input$dbName)
      minLen = force(input$makeBLASTDBMinLen)

      tryCatch(
        expr = {
          #Run the Dada function here.
          shiny::showModal(shiny::modalDialog(
            title = "Make BLAST database is underway.",
            "Processing, please stand by...", footer=""

          ))
          #Run the function
          DBTC::make_BLAST_DB(fileLoc = fileLoc,
                        makeblastdbPath = makeblastdbPath,
                        taxaDBLoc = taxaDBLoc,
                        dbName = dbName,
                        minLen = minLen)
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Make BLAST database is complete",
            "Please see output files in the target
           directory."
          ))
        },
        error = function(e){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "There was an error in running the makeBLASTDB function. Make sure you have permissions for the target folder. Please see the R output for further details."
          ))
        },
        warning = function(w){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "There was an error in running the makeBLASTDB function. Make sure you have permissions for the target folder. Please see the R output for further details."
          ))
        })

    }else{
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "Please fill in all of the necessary fields and submit again!"
      ))
    }
  },ignoreInit = TRUE)


  ################## BLAST sequences Function #################################

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "BLASTDatabasePath", roots = volumes, session = session)

  # Point to the NCBI taxonomic data base
  shiny::observeEvent(input$BLASTDatabasePath, {
    tryCatch(
      expr = {
        BLASTDatabasePath <- shinyFiles::parseFilePaths(volumes, input$BLASTDatabasePath)
        BLASTDatabasePathDisplayString$data <- as.character(BLASTDatabasePath$datapath)
        output$BLASTDatabasePathDisplay <- shiny::renderText({as.character(BLASTDatabasePathDisplayString$data)})
      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        BLASTDatabasePath$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        BLASTDatabasePath$data <- NA
      }
    )
  },ignoreInit = TRUE)

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "blastnPath", roots = volumes, session = session)

  # Get the path where all of the folders containing the fastq files are located
  shiny::observeEvent(input$blastnPath, {
    tryCatch(
      expr = {
        blastnPath <- shinyFiles::parseFilePaths(volumes, input$blastnPath)
        blastnPathDisplayString$data <- as.character(blastnPath$datapath)
        output$blastnPathDisplay <- shiny::renderText({as.character(blastnPathDisplayString$data)})
      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        blastnPath$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        blastnPath$data <- NA
      }
    )
  },ignoreInit = TRUE)

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "querySeqPath", roots = volumes, session = session)

  # Point to the location of the fasta files you want to BLAST
  shiny::observeEvent(input$querySeqPath, {
    tryCatch(
      expr = {
        querySeqPath <- shinyFiles::parseFilePaths(volumes, input$querySeqPath)
        querySeqPathDisplayString$data <- as.character(querySeqPath$datapath)
        output$querySeqPathDisplay <- shiny::renderText({as.character(querySeqPathDisplayString$data)})
      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        querySeqPath$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        querySeqPath$data <- NA
      }
    )
  },ignoreInit = TRUE)

  shiny::observeEvent(input$blastSequences, {

    if(is.na(blastnPathDisplayString$data) | is.null(blastnPathDisplayString$data)){
      blastnPathDisplayString$data <- "blastn"
    }

    if (!is.na(BLASTDatabasePathDisplayString$data) && is.character(BLASTDatabasePathDisplayString$data) && length(BLASTDatabasePathDisplayString$data) != 0 &&
        !is.na(blastnPathDisplayString$data) && is.character(blastnPathDisplayString$data) && length(blastnPathDisplayString$data) != 0 &&
        !is.na(querySeqPathDisplayString$data) && is.character(querySeqPathDisplayString$data) && length(querySeqPathDisplayString$data) != 0) {

      # Create local variables to avoid conflicts with shiny and multithread
      databasePath = force(BLASTDatabasePathDisplayString$data)
      blastnPath = force(blastnPathDisplayString$data)
      querySeqPath = force(querySeqPathDisplayString$data)
      minLen = force(input$BLASTminLen)
      BLASTResults = force(input$BLASTResults)
      if(.Platform$OS.type == 'windows'){numCores=1}else{numCores = force(input$blastSeqNumCores)}

      tryCatch(
        expr = {
          #Run the Dada function here.

          shiny::showModal(shiny::modalDialog(
            title = "Sequence BLAST is underway.",
            "Processing, please stand by...", footer=""

          ))

          #Run the function
          DBTC::seq_BLAST(databasePath = databasePath,
                    blastnPath = blastnPath,
                    querySeqPath = querySeqPath,
                    minLen = minLen,
                    BLASTResults = BLASTResults,
                    numCores = numCores)

          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Sequence BLAST is complete",
            "Please see output files in the target directory."
          ))
        },
        error = function(e){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        },
        warning = function(w){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        }
      )
    }else{
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "Please fill in all of the necessary fields and submit again!"
      ))
    }
  },ignoreInit = TRUE)

  ################## Taxon Assign Function ####################################

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "taxaAssignFileLoc", roots = volumes, session = session)

  # Point to the BLAST output files to get taxonomic assignment
  shiny::observeEvent(input$taxaAssignFileLoc, {
    tryCatch(
      expr = {
        taxaAssignFileLoc <- shinyFiles::parseFilePaths(volumes, input$taxaAssignFileLoc)
        taxaAssignFileLocDisplayString$data <- as.character(taxaAssignFileLoc$datapath)
        output$taxaAssignFileLocDisplay <- shiny::renderText({as.character(taxaAssignFileLocDisplayString$data)})
      },
      error = function(e){
        if(verbose){
          print("Taxa Assign Error")
        }
        taxaAssignFileLoc$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Taxa Assign Warning")
        }
        taxaAssignFileLoc$data <- NA
      }
    )
  },ignoreInit = TRUE)

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "taxaAssignDBLoc", roots = volumes, session = session)

  # Point to the NCBI taxonomic data base
  shiny::observeEvent(input$taxaAssignDBLoc, {
    tryCatch(
      expr = {

        taxaAssignDBLoc <- shinyFiles::parseFilePaths(volumes, input$taxaAssignDBLoc)
        taxaAssignDBLocDisplayString$data <- as.character(taxaAssignDBLoc$datapath)
        output$taxaAssignDBLocDisplay <- shiny::renderText({as.character(taxaAssignDBLocDisplayString$data)})

      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        taxaAssignDBLoc$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        taxaAssignDBLoc$data <- NA
      }
    )
  },ignoreInit = TRUE)

  shiny::observeEvent(input$taxonAssign, {

    if (!is.na(taxaAssignFileLocDisplayString$data) && is.character(taxaAssignFileLocDisplayString$data) && length(taxaAssignFileLocDisplayString$data) != 0 && !is.na(taxaAssignDBLocDisplayString$data) && is.character(taxaAssignDBLocDisplayString$data) && length(taxaAssignDBLocDisplayString$data) != 0) {

      # Create local variables to avoid conflicts with shiny and multithread
      fileLoc = force(taxaAssignFileLocDisplayString$data)

      taxaDBLoc = force(taxaAssignDBLocDisplayString$data)
      numCores = force(input$taxaAssignNumCores)
      coverage = force(input$coverage)
      ident = force(input$ident)
      propThres = force(input$propThres)
      coverReportThresh = force(input$coverReportThresh)
      identReportThresh = force(input$identReportThresh)
      includeAllDada = force(input$includeAllDada)

      tryCatch(
        expr = {
          #Run the function here.
          shiny::showModal(shiny::modalDialog(
            title = "Taxonomic assingment is underway.",
            "See the R terminal for estimated time to completion.
             Processing, please stand by...", footer=""

          ))
          #Run the function
          DBTC::taxon_assign(fileLoc = fileLoc,
                       taxaDBLoc = taxaDBLoc,
                       numCores = numCores,
                       coverage = coverage,
                       ident = ident,
                       propThres = propThres,
                       coverReportThresh = coverReportThresh,
                       identReportThresh = identReportThresh,
                       includeAllDada = includeAllDada)
          removeModal()

          shiny::showModal(shiny::modalDialog(
            title = "Taxonomic assingment is complete",
            "Please see output files in the target directory."
          ))
        },
        error = function(e){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        },
        warning = function(w){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        }
      )

    }else{
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "Please fill in all of the necessary fields and submit again!"
      ))
    }
  })

  ################## Combine Taxa Assign Function #############################

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "combineTaxaFileLoc", roots = volumes, session = session)

  # Point to the folder with the Taxon Assign records you would like to combine
  shiny::observeEvent(input$combineTaxaFileLoc, {
    tryCatch(
      expr = {

        combineTaxaFileLoc <- shinyFiles::parseFilePaths(volumes, input$combineTaxaFileLoc)
        combineTaxaFileLocDisplayString$data <- as.character(combineTaxaFileLoc$datapath)
        output$combineTaxaFileLocDisplay <- shiny::renderText({as.character(combineTaxaFileLocDisplayString$data)})

      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        combineTaxaFileLoc$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        combineTaxaFileLoc$data <- NA
      }
    )
  },ignoreInit = TRUE)

  shiny::observeEvent(input$combineTaxa, {

    if (!is.na(combineTaxaFileLocDisplayString$data) && is.character(combineTaxaFileLocDisplayString$data) && length(combineTaxaFileLocDisplayString$data) != 0) {

      # Create local variables to avoid conflicts with shiny and multithread
      fileLoc = force(combineTaxaFileLocDisplayString$data)
      numCores = force(input$combineTaxaNumCores)

      tryCatch(
        expr = {
          #Run the function here.
          shiny::showModal(shiny::modalDialog(
            title = "Combining taxa assingment files is underway.",
            "Processing, please stand by...", footer=""

          ))
          #Run the function
          DBTC::combine_assign_output(fileLoc = fileLoc,
                                numCores = numCores)
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Combining taxa assingment files is complete",
            "Please see output files in the target directory."
          ))
        },
        error = function(e){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        },
        warning = function(w){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        }
      )
    }else{
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "Please fill in all of the necessary fields and submit again!"
      ))
    }
  })

  ################## Reduce Taxa Assign Function ##############################

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "reduceTaxaFileLoc", roots = volumes, session = session)

  # Point to the folder with the Taxon Assign records you would like to combine
  shiny::observeEvent(input$reduceTaxaFileLoc, {
    tryCatch(
      expr = {

        reduceTaxaFileLoc <- shinyFiles::parseFilePaths(volumes, input$reduceTaxaFileLoc)
        reduceTaxaFileLocDisplayString$data <- as.character(reduceTaxaFileLoc$datapath)
        output$reduceTaxaFileLocDisplay <- shiny::renderText({as.character(reduceTaxaFileLocDisplayString$data)})

      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        reduceTaxaFileLocDisplayString$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        reduceTaxaFileLocDisplayString$data <- NA
      }
    )
  },ignoreInit = TRUE)

  shiny::observeEvent(input$reduceTaxa, {

    if (!is.na(reduceTaxaFileLocDisplayString$data) && is.character(reduceTaxaFileLocDisplayString$data) && length(reduceTaxaFileLocDisplayString$data) != 0) {

      # Create local variables to avoid conflicts with shiny and multithread
      fileLoc = force(reduceTaxaFileLocDisplayString$data)
      numCores = force(input$reduceTaxaNumCores)

      tryCatch(
        expr = {
          #Run the function here.

          shiny::showModal(shiny::modalDialog(
            title = "Reduce ASV results with taxonomic assignment to unique taxa is underway.",
            "Processing, please stand by...", footer=""

          ))

          #Run the function
          DBTC::reduce_taxa(fileLoc = fileLoc,
                      numCores = numCores)

          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Reduce ASV results with taxonomic assignment to unique taxa is complete",
            "Please see output files in the target directory."
          ))
        },
        error = function(e){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        },
        warning = function(w){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        }
      )
    }else{
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "Please fill in all of the necessary fields and submit again!"
      ))
    }
  })

  ################## Combine Reduce Taxa Assign Function ######################

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "combineReducedTaxaFileLoc", roots = volumes, session = session)

  # Point to the folder with the Taxon Assign records you would like to combine
  shiny::observeEvent(input$combineReducedTaxaFileLoc, {
    tryCatch(
      expr = {

        combineReducedTaxaFileLoc <- shinyFiles::parseFilePaths(volumes, input$combineReducedTaxaFileLoc)
        combineReducedTaxaFileLocDisplayString$data <- as.character(combineReducedTaxaFileLoc$datapath)
        output$combineReducedTaxaFileLocDisplay <- shiny::renderText({as.character(combineReducedTaxaFileLocDisplayString$data)})

      },
      error = function(e){
        if(verbose){
          print("Error")
        }
        combineReducedTaxaFileLocDisplayString$data <- NA
      },
      warning = function(w){
        if(verbose){
          print("Warning")
        }
        combineReducedTaxaFileLocDisplayString$data <- NA
      }
    )
  },ignoreInit = TRUE)

  shiny::observeEvent(input$combineReduceTaxa, {

    if (!is.na(combineReducedTaxaFileLocDisplayString$data) && is.character(combineReducedTaxaFileLocDisplayString$data) && length(combineReducedTaxaFileLocDisplayString$data) != 0) {

      # Create local variables to avoid conflicts with shiny and multithread
      fileLoc = force(combineReducedTaxaFileLocDisplayString$data)
      presenceAbsence = force(input$presenceAbsence)

      tryCatch(
        expr = {
          #Run the function here.

          shiny::showModal(shiny::modalDialog(
            title = "Combine reduced taxonomic results for multiple markers for the same samples is underway.",
            "Processing, please stand by...", footer=""

          ))

          #Run the function
          DBTC::combine_reduced_output(fileLoc = fileLoc, presenceAbsence = presenceAbsence)

          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Combine reduced taxonomic results for multiple markers for the same samples is complete",
            "Please see output files in the target directory."
          ))
        },
        error = function(e){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        },
        warning = function(w){
          removeModal()
          shiny::showModal(shiny::modalDialog(
            title = "Error",
            "Please refer to the R consol for more information."
          ))
        }
      )
    }else{
      shiny::showModal(shiny::modalDialog(
        title = "Missing Data",
        "Please fill in all of the necessary fields and submit again!"
      ))
    }
  })















  ################## GPS/Grouping Import File Function Buttons ###################

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "ASVFile", roots = volumes, session = session)

  # Show modal when button is clicked.
  shiny::observeEvent(input$ASVFile, {

    tryCatch(
      expr = {
        ASVFile <- shinyFiles::parseFilePaths(volumes, input$ASVFile)
        ASVFileDisplayString$data <- as.character(ASVFile$datapath)
        output$ASVFileOut <- shiny::renderText({as.character(ASVFileDisplayString$data)})
      },
      error = function(e){
        if(verbose){
          print("Error - Data Input Submit choose file cancelled - 1")
        }
        output$ASVFileOut <- shiny::renderText({as.character("No data file selected")})
      },
      warning = function(w){
        if(verbose){
          print("Warning - Data Input Submit choose file cancelled - 2")
        }
        output$ASVFileOut <- shiny::renderText({as.character("No data file selected")})
      }
    )

  },ignoreInit = TRUE)

  #Connect this to the shinyChooseButton
  shinyFiles::shinyFileChoose(input, "provenanceDataFile", roots = volumes, session = session)

  # Show modal when button is clicked.
  shiny::observeEvent(input$provenanceDataFile, {
    tryCatch(
      expr = {
        provenanceDataFile <- shinyFiles::parseFilePaths(volumes, input$provenanceDataFile)
        #Filter and trim
        provenanceDataFileDisplayString$data <- as.character(provenanceDataFile$datapath)
        output$provenanceDataFileOut <- shiny::renderText({as.character(provenanceDataFileDisplayString$data)})
      },
      error = function(e){
        if(verbose){
          print("Error - Data Input Submit choose file cancelled - 1")
        }
        output$provenanceDataFileOut <- shiny::renderText({as.character("No data file selected")})
      },
      warning = function(w){
        if(verbose){
          print("Warning - Data Input Submit choose file cancelled - 2")
        }
        output$provenanceDataFileOut <- shiny::renderText({as.character("No data file selected")})
      }
    )
  },ignoreInit = TRUE)

  ################## GPS/Grouping Reset Data Import Function #####################

  #Data reset button
  shiny::observeEvent(input$resetDataImport, {
    output$ASVFileOut <- shiny::renderText({as.character("No data file selected")})
    output$provenanceDataFileOut <- shiny::renderText({as.character("No data file selected")})

    ASVFileTable$data <- NA
    provenanceDataFileTable$data <- NA
    mergedTable$data <- NA
    workMergedTable <-NA
    markerColumns <- NULL
    firstLoadFlag$data = 0

    shiny::updateNumericInput(session, "abundanceLow", label = paste0("Enter a Lower Value (min NA):"), value = NULL, min = NULL, max = NULL)
    shiny::updateNumericInput(session, "abundanceHigh", label = paste0("Enter a Higher Value (max NA):"),value = NULL, min = NULL, max = NULL)
    shinyWidgets::updatePickerInput(session, "finalRankInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "kingdomFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "phylumFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "classFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "orderFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "familyFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "genusFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "speciesFilterInput", choices = NULL, selected = NULL)
    shiny::updateRadioButtons(session, "SFATButton", selected = "Yes")
    shiny::updateRadioButtons(session, "SANFButton", selected = "Yes")
    shiny::updateRadioButtons(session, "BIRTButton", selected = "Yes")
    shiny::updateRadioButtons(session, "BCRTButton", selected = "Yes")
    shiny::updateRadioButtons(session, "TBATButton", selected = "Yes")
    shinyWidgets::updatePickerInput(session, "campaignInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "sampleFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "runFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "labFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "typeFilterInput", choices = NULL, selected = NULL)
    shinyWidgets::updatePickerInput(session, "markerFilterInput", choices = NULL, selected = NULL)
    shiny::updateSliderInput(session = session, inputId = "dateInput", min = NULL, max = NULL, value=NULL,step = 1)

    shiny::showModal(shiny::modalDialog(
      title = "Cleared Data",
      "The data selection has been cleared from this instance!"
    ))

  },ignoreInit = TRUE)

  ################## GPS/Grouping Data Processing Upon Hitting Submit ################

  shiny::observeEvent(input$submitDataImport, {

    if(grepl("\\.[Tt][Ss][Vv]$", ASVFileDisplayString$data)){

      if(grepl("\\.[Tt][Ss][Vv]$", provenanceDataFileDisplayString$data)){

        #Loading in the data files
        tryCatch(
          expr = {

            shiny::showModal(shiny::modalDialog("Loading the data files, please wait...",  footer=NULL))

            #Load in the associated GPS and other provenance data
            provenanceDataFileTable$data<-utils::read.table(provenanceDataFileDisplayString$data, header = TRUE, check.names=FALSE, sep="\t", dec=".")

            #get the directory of interest
            fileLoc <- dirname(provenanceDataFileDisplayString$data)

            #Set the working directory
            setwd(fileLoc)

            #Get the all files in the selected file folder with full paths of the files
            files <- as.data.frame(list.files(path = fileLoc, pattern = "*[.]*", full.names = TRUE))
            # Get the local paths
            files[,2] <- list.files(path = fileLoc, pattern = "*[.]*")
            #Get all files with the '_taxaReduced' string
            files <- files[grepl("_taxaReduced.*", files[,2]),]
            # Get the names of the files
            files[,3] <- gsub("_taxaReduced.*","",files[,2])

            # Define a custom function to read each file with check.names = FALSE
            read_file <- function(file_path) {
              utils::read.delim(file_path, check.names = FALSE)
            }

            # Read all files and store data frames in a list
            ASVFileObject <- lapply(files[,1], read_file)

            #For loop flattening and combining all of the elements in the ASVFileObject
            for(ASVFileObjectRecords in 1:length(ASVFileObject)){

              #Add a column to the front of the data frame with unique identifiers.
              ASVFileTableTemp <- cbind(Number = 1:nrow(ASVFileObject[[ASVFileObjectRecords]]), ASVFileObject[[ASVFileObjectRecords]])

              # Rename the first column to ID
              names(ASVFileTableTemp)[1] <- "ID"

              #Get the sample names
              resultsIndex<-which(names(ASVFileTableTemp) == "Results")
              sampleNames<-column_names_after_results <- names(ASVFileTableTemp)[(resultsIndex + 1):ncol(ASVFileTableTemp)]

              # Reshape the data to long format
              ASVFileTableTemp <- stats::reshape(ASVFileTableTemp,
                                          idvar = c("ID"),
                                          varying = sampleNames,
                                          v.names = "Abundance",
                                          times = sampleNames,
                                          direction = "long")

              #Remove the cluncky row names
              row.names(ASVFileTableTemp) <- NULL

              #Rename column times to sample
              colnames(ASVFileTableTemp)[colnames(ASVFileTableTemp) == "time"] <- "Sample"

              #Remove the 0 from the Abundance column
              ASVFileTableTemp <- ASVFileTableTemp[ASVFileTableTemp$Abundance != 0, ]

              #Split the Final_Taxa column to keep the quality values in separate columns to use in the mapping
              #"Num_Rec", "Coverage", "Identity", "Max_eVal"

              #Split the column by ( and replace the close bracket )
              entries <- as.data.frame(do.call('rbind', strsplit(as.character(ASVFileTableTemp$Final_Taxa),'(',fixed = TRUE)), check.names=FALSE)
              entries[,2] <- gsub(")","", as.character(entries[,2]))

              #Split the values column by ,
              entries <- cbind(entries[,1],data.frame(do.call('rbind', strsplit(as.character(entries[,2]),',', fixed = TRUE)), check.names=FALSE), row.names = NULL)

              #Name the total results temp columns
              colnames(entries) <- c("Final_Taxa", "AvgMinNumRec", "AvgMinCoverage", "AvgMinIdentity", "AvgMaxeVal")

              #Build the data frame again with the new data columns
              ASVFileTableTemp <- cbind(
                ASVFileTableTemp[,c(1:(which(colnames(ASVFileTableTemp) == "Final_Taxa")-1))],
                entries,
                ASVFileTableTemp[,c((which(colnames(ASVFileTableTemp) == "Final_Taxa")+1):ncol(ASVFileTableTemp))])

              # Remove brackets and contents from all values in the data frame
              ASVFileTableTemp[,c(which(names(ASVFileTableTemp) %in% "superkingdom"):which(names(ASVFileTableTemp) %in% "species"))] <- lapply(ASVFileTableTemp[,c(which(names(ASVFileTableTemp) %in% "superkingdom"):which(names(ASVFileTableTemp) %in% "species"))], function(x) gsub("\\(.*?\\)", "", x))

              #Change all NA values to N/A so that they are easier to deal with in the dropdown menus
              ASVFileTableTemp[is.na(ASVFileTableTemp)] <- "N/A"

              #Add a category column to place the data points on the map
              ASVFileTableTemp$AbundanceCategory <- cut(
                ASVFileTableTemp$Abundance,
                breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000),
                labels = c("10", "100", "1000", "10000", "100000", "1000000"),
                include.lowest = TRUE
              )

              if (is.na(ASVFileTable$data)){

                ASVFileTable$data <-ASVFileTableTemp

              }else{

                ASVFileTable$data <- rbind(ASVFileTable$data, ASVFileTableTemp)

              }

            }

            ########################Process the submitted data files###################

            #Merge the flattened file with the GPS file.
            mergedTable$data <- merge(ASVFileTable$data, provenanceDataFileTable$data, by = "Sample")
            workMergedTable$data <- mergedTable$data

            #Initialize the data filters on the filter page
            setFilterOptions()

            #Remove the processing files modal
            removeModal()

          },
          error = function(e){
            shiny::showModal(shiny::modalDialog(
              title = "Incorrect File Type - 1",
              "Error - Loading one of the data files, please check the files and try again - 1"
            ))
          },
          warning = function(w){
            shiny::showModal(shiny::modalDialog(
              title = "Incorrect File Type - 2",
              "Error - Loading one of the data files, please check the files and try again - 2"
            ))
          }
        )#Closing trycatch
      }else{#Closing off the if grep provenanceDataFile
        shiny::showModal(shiny::modalDialog(
          title = "Incorrect File Type - 3",
          "Incorrect file type, please select a properly formatted '.tsv' file and resubmit."
        ))
      }
    }else{#The file extension was incorrect
      shiny::showModal(shiny::modalDialog(
        title = "Incorrect File Type - 4",
        "Incorrect file type, please select a properly formatted '.tsv' file and resubmit."
      ))
    }#End of if-else right extension

  })# end of the observeEvent Input Submit

  ############### Filtering Button ###########################################

  shiny::observeEvent(input$updateFilterMappingButton, {

    if(!is.data.frame(ASVFileTable$data) && !is.data.frame(provenanceDataFileTable$data)){
      shiny::showModal(shiny::modalDialog(
        title = "Update Filtering - No Data Loaded - 1",
        "There are no data loaded in this instance of DBTCShine. Please go to the Data Import tab to upload data."
      ))

    }else if (length(mergedTable$data)==0){

      shiny::showModal(shiny::modalDialog(
        title = "Update Filtering - No Data Loaded - 2",
        "There are no data loaded in this instance of DBTCShine. Please go to the Data Import tab to upload data."
      ))

    }else{

      if (nrow(workMergedTable$data)<0){
        shiny::showModal(shiny::modalDialog(
          title = "Update Filtering - No Data - 3",
          "The data have been filtered out. Reset the filters and apply one at a time to ensure data remain for mapping."
        ))

      }else{

        filterOptionsUpdate()

      }

    }

  })

  ############### Reset Filtering Button ###########################################

  shiny::observeEvent(input$resetFilterMappingButton, {

    shiny::showModal(shiny::modalDialog("Updating, please standby...",  footer=NULL))

    workMergedTable$data <- mergedTable$data
    setFilterOptions()
    removeModal()

  })

  ####  Create a global function for the initial setting and resetting of the map #####
  #####################################################################################
  setMappingDataPoints <- function() {

    leaflet::leafletProxy("mymap", data = workMergedTable$data) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearPopups() %>%
      #Adding labels to markers on map
      addCircleMarkers(lng = ~North,
                       lat = ~West,
                       color = ~palette_map(workMergedTable$data$AbundanceCategory),
                       options = leafletOptions(zoomDelta = 0), # Disable zooming
                       clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5),
                       popup= paste("<strong><h5>", workMergedTable$data$Final_Rank,":", workMergedTable$data$Final_Taxa, "</strong>",
                                    "<br><h6>Sample:", workMergedTable$data$Sample,
                                    "<br><h6>Marker:", paste0(workMergedTable$data$Marker,", Reads:", workMergedTable$data$Abundance),
                                    "<br><h6>Date:", workMergedTable$data$Date,
                                    "<br><h6>Run:", workMergedTable$data$Run,
                                    "<br><h6>Type:", workMergedTable$data$Type,
                                    "<br><h6>Lab:", workMergedTable$data$Lab,
                                    "<h6>Coord(Lat, Lon):", workMergedTable$data$North,",", workMergedTable$data$West)
  )}

  #Create a function to update the data table page when selected
  ######################################################################
  setDataTable <- function() {

    #Set up the table for visualization
    dataTableOutputData<-workMergedTable$data[, !names(workMergedTable$data) %in% c("ID", "Top_BLAST", "Final_Common_Names", "RepSequence", "AbundanceCategory")]

    #Set the dataTable output
    output$dataTable <- DT::renderDT({
      DT::datatable(
        dataTableOutputData,
        filter = "top",  # Show filter boxes on top of columns
        options = list(
          pageLength = 50,scrollX = TRUE, scrollY = TRUE  # Display 50 rows per page
        )
      )
    })

  }

  #Create a function to set the filtering page
  ######################################################################
  setFilterOptions <- function() {

    shiny::updateNumericInput(session, "abundanceLow", label = paste0("Enter a Lower Value (min ", min(workMergedTable$data$Abundance),"):"), value = min(workMergedTable$data$Abundance), min = min(workMergedTable$data$Abundance), max = max(workMergedTable$data$Abundance))
    shiny::updateNumericInput(session, "abundanceHigh", label = paste0("Enter a Higher Value (max ", max(workMergedTable$data$Abundance),"):"),value = max(workMergedTable$data$Abundance), min = min(workMergedTable$data$Abundance), max = max(workMergedTable$data$Abundance))
    shinyWidgets::updatePickerInput(session, "finalRankInput", choices = sort(unique(workMergedTable$data$Final_Rank), na.last = TRUE), selected = sort(unique(workMergedTable$data$Final_Rank), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "kingdomFilterInput", choices = sort(unique(workMergedTable$data$superkingdom), na.last = TRUE), selected = sort(unique(workMergedTable$data$superkingdom), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "phylumFilterInput", choices = sort(unique(workMergedTable$data$phylum), na.last = TRUE), selected = sort(unique(workMergedTable$data$phylum), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "classFilterInput", choices = sort(unique(workMergedTable$data$class), na.last = TRUE), selected = sort(unique(workMergedTable$data$class), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "orderFilterInput", choices = sort(unique(workMergedTable$data$order), na.last = TRUE), selected = sort(unique(workMergedTable$data$order), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "familyFilterInput", choices = sort(unique(workMergedTable$data$family), na.last = TRUE), selected = sort(unique(workMergedTable$data$family), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "genusFilterInput", choices = sort(unique(workMergedTable$data$genus), na.last = TRUE), selected = sort(unique(workMergedTable$data$genus), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "speciesFilterInput", choices = sort(unique(workMergedTable$data$species), na.last = TRUE), selected = sort(unique(workMergedTable$data$species), na.last = TRUE))

    shiny::updateRadioButtons(session, "SFATButton", selected = "Yes")
    shiny::updateRadioButtons(session, "SANFButton", selected = "Yes")
    shiny::updateRadioButtons(session, "BIRTButton", selected = "Yes")
    shiny::updateRadioButtons(session, "BCRTButton", selected = "Yes")
    shiny::updateRadioButtons(session, "TBATButton", selected = "Yes")

    shinyWidgets::updatePickerInput(session, "campaignInput", choices = sort(unique(workMergedTable$data$Campaign), na.last = TRUE), selected = sort(unique(workMergedTable$data$Campaign), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "sampleFilterInput", choices = sort(unique(workMergedTable$data$Sample), na.last = TRUE), selected = sort(unique(workMergedTable$data$Sample), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "runFilterInput", choices = sort(unique(workMergedTable$data$Run), na.last = TRUE), selected = sort(unique(workMergedTable$data$Run), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "labFilterInput", choices = sort(unique(workMergedTable$data$Lab), na.last = TRUE), selected = sort(unique(workMergedTable$data$Lab), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "typeFilterInput", choices = sort(unique(workMergedTable$data$Type), na.last = TRUE), selected = sort(unique(workMergedTable$data$Type), na.last = TRUE))
    shinyWidgets::updatePickerInput(session, "markerFilterInput", choices = sort(unique(workMergedTable$data$Marker), na.last = TRUE), selected = sort(unique(workMergedTable$data$Marker), na.last = TRUE))
    shiny::updateSliderInput(session = session, inputId = "dateInput", min = as.Date(min(unique(workMergedTable$data$Date)),"%Y-%m-%d"), max = as.Date(max(unique(workMergedTable$data$Date)),"%Y-%m-%d"), value=c(as.Date(min(unique(workMergedTable$data$Date)),"%Y-%m-%d"),as.Date(max(unique(workMergedTable$data$Date)),"%Y-%m-%d")),step = 1)

  }


  #Create a function to update the filtering page
  ######################################################################
  filterOptionsUpdate <- function() {

    if(!is.data.frame(ASVFileTable$data) && !is.data.frame(provenanceDataFileTable$data)){
      shiny::showModal(shiny::modalDialog(
        title = "Update Filtering - No Data Loaded - 1",
        "There are no data loaded in this instance of DBTCShine. Please go to the Data Import tab to upload data."
      ))

    }else if (length(mergedTable$data)==0){

      shiny::showModal(shiny::modalDialog(
        title = "Update Filtering - No Data Loaded - 2",
        "There are no data loaded in this instance of DBTCShine. Please go to the Data Import tab to upload data."
      ))

    }else{

      #This section is getting the data after applying the filters
      workMergedTable$data <- mergedTable$data
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Abundance >= input$abundanceLow,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Abundance <= input$abundanceHigh,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Final_Rank %in% input$finalRankInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$superkingdom %in% input$kingdomFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$phylum %in% input$phylumFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$class %in% input$classFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$order %in% input$orderFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$family %in% input$familyFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$genus %in% input$genusFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$species %in% input$speciesFilterInput,,drop=FALSE]

      #Filter the dataset based on the radio button selections
      if(input$SFATButton == "No"){
        workMergedTable$data <- workMergedTable$data[!grepl("SFAT", workMergedTable$data$Result_Code), ]
      }
      if(input$SANFButton == "No"){
        workMergedTable$data <- workMergedTable$data[!grepl("SANF", workMergedTable$data$Result_Code), ]
      }
      if(input$BIRTButton == "No"){
        workMergedTable$data <- workMergedTable$data[!grepl("BIRT", workMergedTable$data$Result_Code), ]
      }
      if(input$BCRTButton == "No"){
        workMergedTable$data <- workMergedTable$data[!grepl("BCRT", workMergedTable$data$Result_Code), ]
      }
      if(input$TBATButton == "No"){
        workMergedTable$data <- workMergedTable$data[!grepl("TBAT", workMergedTable$data$Result_Code), ]
      }

      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Campaign %in% input$campaignInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Sample %in% input$sampleFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Run %in% input$runFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Lab %in% input$labFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Type %in% input$typeFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Marker %in% input$markerFilterInput,,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Date >= input$dateInput[1],,drop=FALSE]
      workMergedTable$data <- workMergedTable$data[workMergedTable$data$Date <= input$dateInput[2],,drop=FALSE]

      if (nrow(workMergedTable$data)<1){
        workMergedTable$data <- mergedTable$data
        setFilterOptions()
        shiny::showModal(shiny::modalDialog(
          title = "Update Filtering - No Data - 3",
          "All data have been filtered out with the selected filters. The data has been reset, please try again by applying one filter at a time."
        ))

      }else{

        AVal <- input$abundanceLow
        BVal <- input$abundanceHigh
        CVal <- input$finalRankInput
        DVal <- input$kingdomFilterInput
        EVal <- input$phylumFilterInput
        FVal <- input$classFilterInput
        GVal <- input$orderFilterInput
        HVal <- input$familyFilterInput
        IVal <- input$genusFilterInput
        JVal <- input$speciesFilterInput
        CamVal <- input$campaignInput
        KVal <- input$sampleFilterInput
        LVal <- input$runFilterInput
        MVal <- input$labFilterInput
        NVal <- input$typeFilterInput
        OVal <- input$markerFilterInput
        SFATVal <- input$SFATButton
        SANFVal <- input$SANFButton
        BIRTVal <- input$BIRTButton
        BCRTVal <- input$BCRTButton
        TBATVal <- input$TBATButton
        PVal <- input$dateInput[1]
        QVal <- input$dateInput[2]

        shiny::updateNumericInput(session, "abundanceLow", label = paste0("Enter a Lower Value (min ", min(workMergedTable$data$Abundance),"):"), value = AVal, min = min(workMergedTable$data$Abundance), max = max(workMergedTable$data$Abundance))
        shiny::updateNumericInput(session, "abundanceHigh", label = paste0("Enter a Higher Value (max ", max(workMergedTable$data$Abundance),"):"),value = BVal, min = min(workMergedTable$data$Abundance), max = max(workMergedTable$data$Abundance))
        shinyWidgets::updatePickerInput(session, "finalRankInput", choices = sort(unique(workMergedTable$data$Final_Rank), na.last = TRUE), selected = CVal)
        shinyWidgets::updatePickerInput(session, "kingdomFilterInput", choices = sort(unique(workMergedTable$data$superkingdom), na.last = TRUE), selected = DVal)
        shinyWidgets::updatePickerInput(session, "phylumFilterInput", choices = sort(unique(workMergedTable$data$phylum), na.last = TRUE), selected = EVal)
        shinyWidgets::updatePickerInput(session, "classFilterInput", choices = sort(unique(workMergedTable$data$class), na.last = TRUE), selected = FVal)
        shinyWidgets::updatePickerInput(session, "orderFilterInput", choices = sort(unique(workMergedTable$data$order), na.last = TRUE), selected = GVal)
        shinyWidgets::updatePickerInput(session, "familyFilterInput", choices = sort(unique(workMergedTable$data$family), na.last = TRUE), selected = HVal)
        shinyWidgets::updatePickerInput(session, "genusFilterInput", choices = sort(unique(workMergedTable$data$genus), na.last = TRUE), selected = IVal)
        shinyWidgets::updatePickerInput(session, "speciesFilterInput", choices = sort(unique(workMergedTable$data$species), na.last = TRUE), selected = JVal)
        shiny::updateRadioButtons(session, "SFATButton", selected = SFATVal)
        shiny::updateRadioButtons(session, "SANFButton", selected = SANFVal)
        shiny::updateRadioButtons(session, "BIRTButton", selected = BIRTVal)
        shiny::updateRadioButtons(session, "BCRTButton", selected = BCRTVal)
        shiny::updateRadioButtons(session, "TBATButton", selected = TBATVal)
        shinyWidgets::updatePickerInput(session, "campaignInput", choices = sort(unique(workMergedTable$data$Campaign), na.last = TRUE), selected = CamVal)
        shinyWidgets::updatePickerInput(session, "sampleFilterInput", choices = sort(unique(workMergedTable$data$Sample), na.last = TRUE), selected = KVal)
        shinyWidgets::updatePickerInput(session, "runFilterInput", choices = sort(unique(workMergedTable$data$Run), na.last = TRUE), selected = LVal)
        shinyWidgets::updatePickerInput(session, "labFilterInput", choices = sort(unique(workMergedTable$data$Lab), na.last = TRUE), selected = MVal)
        shinyWidgets::updatePickerInput(session, "typeFilterInput", choices = sort(unique(workMergedTable$data$Type), na.last = TRUE), selected = NVal)
        shinyWidgets::updatePickerInput(session, "markerFilterInput", choices = sort(unique(workMergedTable$data$Marker), na.last = TRUE), selected = OVal)
        shiny::updateSliderInput(session = session, inputId = "dateInput", min = as.Date(PVal,"%Y-%m-%d"), max = as.Date(QVal,"%Y-%m-%d"), value=c(as.Date(PVal,"%Y-%m-%d"),as.Date(QVal,"%Y-%m-%d")),step = 1)
      }
    }
  }

} # End of Server
