#' Retrieve data from the OpenCoesione data base per region.
#'
#' The \code{get_data_region_OC} function retrieves data from one or more Italian regions using associated region codes. It offers filtering options based on project start/end dates, province, and municipality codes. Additionally, it provides geospatial references.
#'
#' @param cod_reg character. Vector specifying one or more region of interest. To get information about the codes associated to each region use the function \code{\link{get_info_OC}}
#' @param cod_prov character. The ISTAT province code is used to filter data based on one or more specific provinces of interest.(See \code{\link{get_codes}} function)
#' @param cod_mun character. The ISTAT municipality code is used to filter data based on one or more specific provinces of interest.(See \code{\link{get_codes}} function)
#' @param start (format YYYY-mm-dd). Effective starting date of the project. This date can be of interest for filtering and analyzing relevant data.
#' @param end (format YYYY-mm-dd). Effective ending date of the project. This date can be of interest for filtering and analyzing relevant data.
#' @param geo_ref character, The georeference data can be specified using the \code{geo_ref} argument. If set to \code{A}, the function returns shape polygons of each municipality. If set to \code{C}, it retrieves the coordinates of the centroids of each municipality.
#' @param soil_defense Logical.  By default set to \code{FALSE}. If only soil defense data are of interest set the argument to \code{TRUE}.
#' @param verbose Logic value (TRUE or FALSE). Toggle warnings and messages. If 'verbose = TRUE' (default) the function
#' prints on the screen some messages describing the progress of the tasks. If 'verbose = FALSE' any message about
#' the progression is suppressed.
#' @returns Object of classe \code{data.frame} showing 42 variables:
#' Descriptive Variables:
#' \itemize{
#' \item{Local Project Code (\code{character})}
#' \item{CUP (\code{character})}
#' \item{Intervention (\code{character})}
#' }
#'
#' Financial Variables:
#' \itemize{
#' \item{EU Funding (\code{numeric})}
#' \item{FESR EU Funding (\code{numeric})}
#' \item{FSE EU Funding (\code{numeric})}
#' \item{FEASR EU Funding (\code{numeric})}
#' \item{FEAMP EU Funding (\code{numeric})}
#' \item{IOG EU Funding (\code{numeric})}
#' \item{Fondo di Rotazione ITA (\code{numeric})}
#' \item{FSC ITA Funding (\code{numeric})}
#' \item{PAC ITA Funding (\code{numeric})}
#' \item{Completamenti ITA Funding (\code{numeric})}
#' \item{Other Measures ITA Funding (\code{numeric})}
#' \item{Region Funding (\code{numeric})}
#' \item{Province Funding (\code{numeric})}
#' \item{Municipality Funding (\code{numeric})}
#' \item{Released Resources (\code{numeric})}
#' \item{Other Public Funding (\code{numeric})}
#' \item{Foreign State Funding (\code{numeric})}
#' \item{Private Funding (\code{numeric})}
#' \item{Total Public Funding (\code{numeric})}
#' \item{Total Funding (\code{numeric})}
#' }
#' Geographical References:
#' \itemize{
#' \item{DEN_REGION (\code{character})}
#' \item{DEN_PROVINCE (\code{character})}
#' \item{DEN_MUNICIPALITY (\code{character})}
#' \item{COD_REGION (\code{character})}
#' \item{COD_PROVINCE (\code{character})}
#' \item{COD_MUNICIPALITY (\code{character})}
#' \item{geom (\code{character})}
#' }
#'
#' Legislative process main steps:
#' \itemize{
#'  \item{Feasibility Study Starting Date (\code{character})}
#'  \item{Feasibility Study Ending Date (\code{character})}
#'  \item{Preliminary Design Starting Date (\code{character})}
#'  \item{Preliminary Design Ending Date (\code{character})}
#'  \item{Definitive Design Starting Date (\code{character})}
#'  \item{Definitive Design Ending Date (\code{character})}
#'  \item{Executive Design Starting Date (\code{character})}
#'  \item{Executive Design Ending Date (\code{character})}
#'  \item{Effective Design Starting Date (\code{character})}
#'  \item{Effective Design Ending Date (\code{character})}
#'  \item{Works Execution Starting Date (\code{character})}
#'  \item{Works Execution Ending Date (\code{character})}
#'  \item{Conclusion Starting Date (\code{character})}
#'  \item{Conclusion Ending Date (\code{character})}
#'  }
#'
#'@references \href{https://opencoesione.gov.it/en/dati/}{Open Coesione}
#'
#'@author Lorena Ricciotti
#'
#'@examples
#'  dati_VDA <- get_data_region_OC("VDA", cod_mun = "007002")
#'  # #Retrieving data for the municipality with code 007002 in the Valle d'Aosta region.
#' @export
#'
get_data_region_OC <- function(cod_reg, cod_prov = NULL, cod_mun = NULL,  start = NULL, end = NULL, geo_ref = NULL, soil_defense = FALSE, verbose = TRUE) {

  #Check
  if(!is.list(cod_reg) && !is.character(cod_reg)) {
    warning("The argument 'cod_reg' must be a vector of regional codes.")
  }

  df <- data.frame()
  for (cod in cod_reg) {
    temp_dir <- tempdir()

    #Create the destination directory
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir, recursive = TRUE)
    }
    url <- paste0("https://opencoesione.gov.it/it/opendata/regioni/progetti_esteso_", toupper(cod), ".zip")
    dest_file <- file.path(temp_dir, paste0(cod, ".zip"))

    utils::download.file(url, destfile = dest_file, mode = "wb",quiet = T)
    file <- utils::unzip(dest_file, exdir = temp_dir)

    dati <- utils::read.csv(file, sep = ";", dec = ",")

    #Transform variables codes

    dati$COD_PROVINCIA <- substr(dati$COD_PROVINCIA, 4, nchar(dati$COD_PROVINCIA))
    dati$COD_COMUNE <- substr(dati$COD_COMUNE, 4, nchar(dati$COD_COMUNE))
    dati$COD_REGIONE <- substr(dati$COD_REGIONE, 2, nchar(dati$COD_REGIONE))

    unlink(dest_file)
    unlink(paste0("dati_", cod, ".csv"))
    unlink(temp_dir)

    df <- rbind(dati, df)
  }

  if(!is.null(geo_ref)){
    #Get georeferenced data
    geo <- data.frame()
    #Switch the code from letters to numbers
    cod_reg_num <- sapply(tolower(cod_reg), function(x) {
      switch(x,
             pie = "01",
             vda = "02",
             lom = "03",
             tn_bz = "04",
             ven = "05",
             fvg = "06",
             lig = "07",
             emr = "08",
             tos = "09",
             umb = "10",
             mar = "11",
             laz = "12",
             abr = "13",
             mol = "14",
             cam = "15",
             pug = "16",
             bas = "17",
             cal = "18",
             sic = "19",
             sar = "20",
             default = NA)
    })


    #Define the endpoint
    endpoint <- "https://dati.isprambiente.it/sparql"

    for(cod in cod_reg_num){
      #Define the query
      sparql <- paste0("
    select distinct str(?registat) AS ?reg_istat str(?istat) AS ?COD_COMUNE ?name ?geometry where {
      ?s a <https://w3id.org/italia/env/onto/place/Municipality>;
      <https://w3id.org/italia/env/onto/place/istat> ?istat;
      <https://w3id.org/italia/env/onto/place/hasRegion> ?reguri;
      <https://w3id.org/italia/env/onto/place/hasGeometry> ?geom ;
      <https://w3id.org/italia/env/onto/top/name> ?name2 .
      ?geom <https://w3id.org/italia/env/onto/place/geometry>  ?geometry .
      ",
                       if (!is.null(cod_mun) && !is.null(cod_reg_num)) paste0("FILTER (str(?istat) = '", cod_mun, "')"),
                       if (!is.null(cod_reg_num) && is.null(cod_mun)) paste0("FILTER (str(?registat) = '", cod, "')"),
                       if (!is.null(geo_ref) && geo_ref == "A") "FILTER(STRENDS(str(?geom), 'polygon'))",
                       if (!is.null(geo_ref) && geo_ref == "C") "FILTER(STRENDS(str(?geom), 'point'))",
                       "?reguri <https://w3id.org/italia/env/onto/place/istat> ?registat.
      BIND(str(?name2) AS ?name)
    }
    ORDER BY ?name"
      )


      #Get data from the query
      encoded_query <- utils::URLencode(sparql)
      full_url <- paste0(endpoint, "?query=", encoded_query)
      response <- httr::GET(full_url)
      data <- httr::content(response, as = "parsed")


      #Extracting variables names
      head_vars <- data$head$vars

      #Extracting the results
      results <- data$results$bindings

      #Function to extract a single row from the results
      extract_row <- function(result) {
        values <- sapply(head_vars, function(var) {
          if (length(result[[var]]) == 0) {
            NA
          } else {
            result[[var]]$value
          }
        })
        data.frame(t(values), stringsAsFactors = FALSE)
      }

      # Applying the function to each result
      rows <- lapply(results, extract_row)

      # Combining the rows into a data frame
      loc <- do.call(rbind, rows)

      # Renaming the columns
      colnames(loc) <- head_vars
      geo <- rbind(loc, geo)
    }
    df <- df %>% dplyr::left_join(geo %>% dplyr::select(.data$COD_COMUNE, .data$geometry),
                                  by = "COD_COMUNE", relationship = "many-to-many") %>%
      dplyr::mutate(geom = .data$geometry) %>%
      dplyr::select(-.data$geometry)
    # df$geom <- ifelse(df$COD_COM %in% geo$com_istat ,
    #                   geo$geometry[match(df$COD_COM, geo$com_istat)],NA)

    df <- df[!is.na(df$geom),]


  }

#Handling Dates

  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(EffectiveDesignStartingDate = ifelse(all(is.na(c(.data$DATA_INIZIO_EFF_STUDIO_FATT, .data$DATA_INIZIO_EFF_PROG_PREL, .data$DATA_INIZIO_EFF_PROG_DEF, .data$DATA_INIZIO_EFF_PROG_ESEC))), NA, pmin(.data$DATA_INIZIO_EFF_STUDIO_FATT, .data$DATA_INIZIO_EFF_PROG_PREL, .data$DATA_INIZIO_EFF_PROG_DEF, .data$DATA_INIZIO_EFF_PROG_ESEC, na.rm = TRUE))) %>%
    dplyr::mutate(EffectiveDesignEndingDate = ifelse(all(is.na(c(.data$DATA_FINE_EFF_STUDIO_FATT,.data$DATA_FINE_EFF_PROG_PREL, .data$DATA_FINE_EFF_PROG_DEF, .data$DATA_FINE_EFF_PROG_ESEC))), NA, pmax(.data$DATA_FINE_EFF_STUDIO_FATT,.data$DATA_FINE_EFF_PROG_PREL, .data$DATA_FINE_EFF_PROG_DEF, .data$DATA_FINE_EFF_PROG_ESEC, na.rm = TRUE))) %>% dplyr::ungroup() %>% as.data.frame()

  if (!is.null(start)) {
    df <- df %>%
         dplyr::filter(.data$EffectiveDesignStartingDate >= start)
  }

  if (!is.null(end)) {
    df <- df %>%
      dplyr::filter(.data$EffectiveDesignEndingDate <= end)

  }

  #Filters for province code
  if (!is.null(cod_prov)) {
    if(!is.list(cod_prov) && !is.character(cod_prov)){
      if (verbose == TRUE){
      cat("The argument 'cod_prov' must be a character vector of province code.")}
    }

    if(is.list(cod_prov)){
      cod_prov <- unlist(cod_prov)
    }

    df <- df %>% dplyr::filter(.data$COD_PROVINCIA %in% cod_prov)

  }

  #Filter for municipality code
  if (!is.null(cod_mun)) {
    if(!is.list(cod_mun) && !is.character(cod_mun)){
      if(verbose == TRUE){
      cat("The argument 'cod_mun' must be a character vector of municipality codes.")}
    }

    if(is.list(cod_mun)){
      cod_mun <- unlist(cod_mun)
    }
    df <- df %>% dplyr::filter(.data$COD_COMUNE %in% cod_mun)


  }

  #Filter for soil defense data
  if(soil_defense == TRUE) {
    df <- df %>% dplyr::filter(.data$CUP_DESCR_SOTTOSETTORE == "DIFESA DEL SUOLO")

  }
  df$OC_SINTESI_PROGETTO[df$OC_SINTESI_PROGETTO == " "] <- df$OC_TITOLO_PROGETTO[which(df$OC_SINTESI_PROGETTO == " ")]
  df <- df[,-c(3,5:45,52:54,73,75:113,115,117,119,121,123,125,127,129:137,139,141,143,145:199)]

  colnames(df)[1:40] <- c("LocalProjectCode", "CUP", "Intervention","COD_REGION", "DEN_REGION","COD_PROVINCE","DEN_PROVINCE",
                          "COD_MUNICIPALITY","DEN_MUNICIPALITY", "EuFunding", "FESR_EuFunding","FSE_EuFunding",
                          "FEASR_EuFunding", "FEAMP_EuFunding", "IOG_EuFunding", "FondoDiRotazioneITA", "FSC_FundingITA",
                          "PAC_FundingITA","CompletamentiFunding_ITA", "OtherMeasuresFundingITA", "RegionFunding",
                          "ProvinceFunding", "MunicipalityFunding", "ReleasedResources", "OtherPublicFunding",
                          "ForeignStateFunding","PrivateFunding", "TotalPublicFunding",
                          "FeasibilityStudyStartingDate", "FeasibilityStudyEndingDate", "PreliminaryDesignStartingDate",
                          "PreliminaryDesignEndingDate", "DefinitiveDesignStartingDate", "DefinitiveDesignEndingDate",
                          "ExecutiveDesignStartingDate", "ExecutiveDesignEndingDate", "WorksExecutionStartingDate",
                          "WorksExecutionEndingDate", "ConclusionStartingDate", "ConclusionEndingDate")

  if(length(df) == 44){
  df <- df %>% dplyr::select(1:36, 43:44, dplyr::everything()) %>% dplyr::mutate(dplyr::across(29:43, ~lubridate::ymd(.)))  %>% dplyr::mutate(dplyr::across(29:43 , ~as.character(.)))
  }
    if(length(df) == 42){
      df <- df %>% dplyr::mutate(dplyr::across(29:42, ~lubridate::ymd(.)))  %>% dplyr::mutate(dplyr::across(29:42 , ~as.character(.)))
    }
  if(length(df) == 43){
    df <- df %>% dplyr::select(1:36, 42:43, dplyr::everything()) %>% dplyr::mutate(dplyr::across(29:42, ~lubridate::ymd(.)))  %>% dplyr::mutate(dplyr::across(29:42 , ~as.character(.)))
  }

  return(df)
}
