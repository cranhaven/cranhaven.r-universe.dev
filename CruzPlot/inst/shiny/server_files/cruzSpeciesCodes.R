# cruzSpecies for CruzPlot
#   load and save SpCodes data
#   cruzSpeciesMammals() returns data frame of all species information from mammals in .dat file
#   cruzSpeciesTurtles() returns data frame of all species information from turtles in .dat file


###############################################################################
# Read in species code information
spcodes_default_read <- eventReactive(input$das_spcodes_default, {
  cruz.list$sp.codes <- NULL
  cruz.list$sp.codes.name <- NULL
  shinyjs::reset("das_spcodes_file")

  x <- try(cruzSpeciesRead(system.file("SpCodes.dat", package = "CruzPlot")),
           silent = TRUE)

  validate(
    need(x, "Error reading the default SpCodes.dat file; please try reinstalling CruzPlot")
  )

  cruz.list$sp.codes <- x
  cruz.list$sp.codes.name <- "default"

  ""
})

spcodes_user_read <- eventReactive(input$das_spcodes_file, {
  cruz.list$sp.codes <- NULL
  cruz.list$sp.codes.name <- NULL

  x <- try(cruzSpeciesRead(input$das_spcodes_file$datapath),
           silent = TRUE)

  validate(
    need(x, "Error reading the loaded SpCodes.dat file; please try another file")
  )

  cruz.list$sp.codes <- x
  cruz.list$sp.codes.name <- input$das_spcodes_file$name

  ""
})


###############################################################################
# Species information
# cruzSpecies <- reactive({
#   cruz.list$sp.codes <- cruzSpeciesRead("SpCodes.dat")
#
#   sp.codes
# })

cruzSpeciesMammals <- reactive({
  sp.codes <- req(cruz.list$sp.codes)
  # validate(need(sp.codes, "Please load a species code file in the 'Data' window"))
  ind.mammals <- which(!sp.codes$Code %in% turtle.codes)

  sp.codes[ind.mammals, ]
})

cruzSpeciesTurtles <- reactive({
  sp.codes <- req(cruz.list$sp.codes)
  # validate(need(sp.codes, "Please load a species code file in the 'Data' window"))
  ind.turtles <- which(sp.codes$Code %in% turtle.codes)

  sp.codes[ind.turtles, ]
})

###############################################################################
