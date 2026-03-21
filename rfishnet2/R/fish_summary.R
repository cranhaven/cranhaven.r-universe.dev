#' Summarize a set of records downloaded from FishNet2
#'
#' Creates a simple summary of data returned by a FishNet2 search.
#'
#' @export
#' @param input A dataframe in FishNet2 standard format (by using read.csv())
#' @param verbose Print progress and information messages. Default: TRUE
#' @return A list of summary statistics
#'
#' # summarize occurrence records
#'


fishsummary <- function(input, verbose = TRUE) {
  # recs <- number of records in the data frame
  recs <- nrow(input)

  # coords <- number of records with viable lat and long data
  # errest <- number of "coords" records with viable coordinate uncertainty estimate
  if (is.null(input$Latitude) & is.null(input$Longitude)) {
    coords <- 0
  } else{
    coords <- NULL
  }

  if (is.null(input$CoordinateUncertaintyInMeters)) {
    errest <- 0
  } else {
    errest <- NULL
  }
  if (is.null(coords)) {
    coords <- sum(stats::complete.cases(input[, c('Latitude','Longitude')]))
    # checking for good lat/long data (if not, use only the above line)
    input$Latitude <- as.numeric(as.character(input$Latitude))
    input$Longitude <- as.numeric(as.character(input$Longitude))
    if (is.null(errest)) {
      input$CoordinateUncertaintyInMeters <- as.numeric(as.character(input$CoordinateUncertaintyInMeters))
    }
    mappable <- input[stats::complete.cases(input[,c('Latitude','Longitude')]),]
    #mappable <- subset(mappable, input$decimallatitude < 90 & input$decimallatitude > -90)
    #mappable <- subset(mappable, input$decimallongitude < 180 & input$decimallongitude > -180)
    if (nrow(mappable) < coords) {
      bad <- coords - nrow(mappable)
      # mssg(verbose, paste(bad, " record(s) with bad coordinates"))
      coords <- coords - bad
    }
    if (is.null(errest)) {
      mappable <- subset(mappable, input$CoordinateUncertaintyInMeters > 0 &
                           input$CoordinateUncertaintyInMeters < 20020000)
      if ((errest <- nrow(mappable)) < coords) {
        bad <- coords - errest
      }
    }
  }

  # instcoll <- number of records from each institution+collection
  removeDups <- function(x) {
    paste(unique(unlist(strsplit(x, split = " "))), collapse = " ")
  }
  if (inherits(input$InstitutionCode, "NULL") & inherits(input$CollectionCode, "NULL")) {
    instcoll <- NA
  } else {
    instcoll <- as.matrix(paste(input$InstitutionCode,
                                input$CollectionCode, sep = "/"))
    instcoll <- table(apply(instcoll, 1, removeDups))
  }

  # country <- number of records from each country

  if (is.null(input$Country)) {
    country <- NA
  }  else {
    country <- c()
    # replace United States with USA for consistency
    for (c in 1:length(input$Country)) {
      if(input$Country[c] == "United States"){
        country <- c(country, "USA")
      } else{
        country <- c(country, as.character(input$Country[c]))
      }
    }
    country <- table(country)
  }

  # year <- number of records by year
  if (is.null(input$YearCollected)){
    year <- NA
  } else {
    year <- table(input$YearCollected)
  }

  # family <- number of records by family name
  if (is.null(input$Family)){
    family <- NA
  } else {
    family <- table(input$Family)
  }

  # scientific name <- number of records by scientific name
  if (is.null(input$ScientificName)){
    scientific_name <- NA
  } else {
    scientific_name <- table(input$ScientificName)
  }

  #tissues <- number of records with tissues
  if (is.null(input$Tissues)){
    tissues <- "None"
  } else {
    tissues <- table(input$Tissues)
  }

  # preparation_type
  preparation_type <- c()
  for (i in input$PreparationType) {
    if(is.na(i)==TRUE){
      preparation_type <- c(as.factor("None"), preparation_type)
    } else{
      preparation_type <- c(as.factor(i), preparation_type)
    }

  }

  #input$PreparationType <- as.factor(gsub(NA, "none", input$PreparationType))
  if (is.null(input$PreparationType)){
    preparation_type <- "None"
  } else {
    preparation_type <- table(input$PreparationType)
  }

  # return summary
  res = structure(list("recs" = recs, "coords" = coords, "errest" = errest,
                 "instcoll" = instcoll, "country" = country, "year" = year,
                 "family" = family, "scientific_name" = scientific_name, "preparation_type" = preparation_type, "tissues" = tissues))

  print.fishsummary <- function(x, ...){
    cat(paste0("Number of records ($recs): ", x$recs), sep = "\n")
    cat(paste("Records with decimal lat/long (-90<lat<90, -180<long<180) ($coords): ", x$coords, sep = ""), sep = "\n")
    cat(paste("Records with lat/long and coordinate uncertainty estimate (0<errest<20020000) ($errest): ", x$errest), sep = "\n")
    cat("Record count by institution/collection ($instcoll): ", sep = "\n")
    print(x$instcoll)
    cat("\nRecord count by country ($country): ", sep = "\n")
    print(x$country)
    cat("\nRecord count by year ($year): ", sep = "\n")
    print(x$year)
    cat("\nRecord count by familiy ($family): ", sep = "\n")
    print(x$family)
    cat("\nRecord count by Scientific Name ($scientific_name): ", sep = "\n")
    print(x$scientific_name)
    cat("\nRecord count by preparation type ($preparation_type): ", sep = "\n")
    print(x$preparation_type)
    cat("\nRecords with tissues ($tissues: ", sep = "\n")
    print(x$tissues)
  }
  if(verbose == TRUE ){
    print.fishsummary(res)
  }
  return(res)
}

