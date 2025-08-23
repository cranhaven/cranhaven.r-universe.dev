#' Retrieve Data from OpenCoesione Database by Theme's Project
#'
#' The \code{get_data_theme_OC} function allows users to fetch data from the OpenCoesione database based on specific themes related to projects.
#'
#' @param themes   character. Vector specifying one or more theme of interest. To get information about the codes associated to each theme use the function \code{\link{get_info_OC}.}
#' @param cod_reg character. The ISTAT regional code is used to filter data based on one or more specific regions of interest. (See \code{\link{get_codes}} function)
#' @param cod_prov character. The ISTAT province code is used to filter data based on one or more specific provinces of interest. (See \code{\link{get_codes}} function)
#' @param cod_mun character. The ISTAT municipality code is used to specify one or more municipalities of interest within the region(s) of interest. (See \code{\link{get_codes}} function)
#' @param start character (format YYYY-mm-dd). Effective starting date of the project. This date can be of interest for filtering and analyzing relevant data.
#' @param end character (format YYYY-mm-dd). Effective ending date of the project. This date can be of interest for filtering and analyzing relevant data.
#' @param geo_ref character. The georeference data can be specified using the \code{geo_ref} argument. If set to \code{A}, the function returns shape polygons of each municipality. If set to \code{C}, it retrieves the coordinates of the centroids of each municipality.
#' @param soil_defense Logical, default set to \code{FALSE}. If only soil defense data are of interest set the argument to \code{TRUE}.
#' @param verbose Logic value (TRUE or FALSE). Toggle warnings and messages. If 'verbose = TRUE' (default) the function
#' prints on the screen some messages describing the progress of the tasks. If 'verbose = FALSE' any message about
#' the progression is suppressed.
#' @returns Object of classe \code{data.frame} showing 42 variables:
#' Descriptive Variables:
#' \itemize{
#'  \item{Local Project Code (\code{character})}
#'   \item{CUP (\code{character})}
#'   \item{Intervention (\code{character})}
#'   }
#'
#'   Financial Variable:
#'   \itemize{
#'   \item{EU Funding (\code{numeric})}
#'   \item{FESR EU Funding (\code{numeric})}
#'   \item{FSE EU Funding (\code{numeric})}
#'   \item{FEASR EU Funding (\code{numeric})}
#'   \item{FEAMP EU Funding (\code{numeric})}
#'   \item{IOG EU Funding (\code{numeric})}
#'   \item{Fondo di Rotazione ITA (\code{numeric})}
#'   \item{FSC ITA Funding (\code{numeric})}
#'   \item{PAC ITA Funding (\code{numeric})}
#'   \item{Completamenti ITA Funding (\code{numeric})}
#'   \item{Other Measures ITA Funding (\code{numeric})}
#'   \item{Region Funding (\code{numeric})}
#'   \item{Province Funding (\code{numeric})}
#'   \item{Municipality Funding (\code{numeric})}
#'   \item{Released Resources (\code{logic})}
#'   \item{Other Public Funding (\code{numeric})}
#'   \item{Foreign State Funding (\code{numeric})}
#'   \item{Private Funding (\code{numeric})}
#'   \item{Total Public Funding (\code{numeric})}
#'   \item{Total Funding (\code{numeric})}
#'   }
#'
#'   Geographical References:
#'   \itemize{
#'   \item{DEN_MUNICIPALITY (\code{character})}
#'   \item{DEN_REGION (\code{character})}
#'   \item{DEN_PROVINCE (\code{character})}
#'   \item{COD_REGION (\code{character})}
#'   \item{COD_PROVINCE (\code{character})}
#'   \item{COD_MUNICIPALITY (\code{character})}
#'   \item{geom (\code{character})}
#'   }
#'
#'   Legislative process main steps:
#'   \itemize{
#'   \item{Feasibility Study Starting Date (\code{integer})}
#'   \item{Feasibility Study Ending Date (\code{integer})}
#'   \item{Preliminary Design Starting Date (\code{integer})}
#'   \item{Preliminary Design Ending Date (\code{integer})}
#'   \item{Definitive Design Starting Date (\code{integer})}
#'   \item{Definitive Design Ending Date (\code{integer})}
#'   \item{Executive Design Starting Date (\code{integer})}
#'   \item{Executive Design Ending Date (\code{integer})}
#'   \item{Works Execution Starting Date (\code{integer})}
#'   \item{Works Execution Ending Date (\code{integer})}
#'   \item{Conclusion Starting Date (\code{character})}
#'   \item{Conclusion Ending Date (\code{character})}
#'   }
#'
#'
#'@references \href{https://opencoesione.gov.it/en/dati/}{Open Coesione}
#'
#'@author Lorena Ricciotti
#'
#'@examples
#'\donttest{data <- get_data_theme_OC("AMBIENTE", start = "2022-01-01", end = "2022-12-31")}
# #Retrieving data for one theme project
# filtering for the starting and ending dates
#'
#'@export
get_data_theme_OC <- function(themes, cod_reg =NULL, cod_prov = NULL, cod_mun = NULL, start = NULL, end = NULL, geo_ref = NULL, soil_defense = FALSE, verbose = TRUE) {
  if(!is.list(themes) && !is.character(themes)) {
    if(verbose == TRUE){
    cat("The 'theme' argument must be a vector of theme project.")
    }
  }
  df <- data.frame()
  for (theme in themes) {

    temp_dir <- tempdir()

    #Create the destination directory
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir, recursive = TRUE)
    }

    url <- paste0("https://opencoesione.gov.it/it/opendata/temi/progetti_esteso_", toupper(theme), ".zip")
    dest_file <- file.path(temp_dir, paste0(theme, ".zip"))

    utils::download.file(url, destfile = dest_file, mode = "wb", quiet = T)
    file <- utils::unzip(dest_file, exdir = temp_dir)

    dati <- utils::read.csv(file, sep = ";", dec = ",")

    #Tranform variable codes
    dati$COD_PROVINCIA <- substr(dati$COD_PROVINCIA, 4, nchar(dati$COD_PROVINCIA))
    dati$COD_COMUNE <- substr(dati$COD_COMUNE, 4, nchar(dati$COD_COMUNE))
    dati$COD_REGIONE <- substr(dati$COD_REGIONE, 2, nchar(dati$COD_REGIONE))

    unlink(dest_file)
    unlink(paste0("dati_", theme, ".csv"))
    df <- rbind(dati, df)
  }
  if(!is.null(geo_ref)){
    #Get georeferenced data
    geo <- data.frame()
    #Define the endpoint
    endpoint <- "https://dati.isprambiente.it/sparql"

if(!is.null(cod_reg)){
    for(cod in cod_reg){
      #Define the query
      sparql <- paste0("
    select distinct str(?registat) AS ?reg_istat str(?istat) AS ?com_istat ?name ?geometry where {
      ?s a <https://w3id.org/italia/env/onto/place/Municipality>;
      <https://w3id.org/italia/env/onto/place/istat> ?istat;
      <https://w3id.org/italia/env/onto/place/hasRegion> ?reguri;
      <https://w3id.org/italia/env/onto/place/hasGeometry> ?geom ;
      <https://w3id.org/italia/env/onto/top/name> ?name2 .
      ?geom <https://w3id.org/italia/env/onto/place/geometry>  ?geometry .
      ",
                       if (!is.null(cod_mun) && !is.null(cod_reg)) paste0("FILTER (str(?istat) = '", cod_mun, "')"),
                       if (!is.null(cod_reg) && is.null(cod_mun)) paste0("FILTER (str(?registat) = '", cod, "')"),
                       if (!is.null(geo_ref) && geo_ref == "A") "FILTER(STRENDS(str(?geom), 'polygon'))",
                       if (!is.null(geo_ref) && geo_ref == "C") "FILTER(STRENDS(str(?geom), 'point'))",
                       "?reguri <https://w3id.org/italia/env/onto/place/istat> ?registat.
      BIND(str(?name2) AS ?name)
    }
    ORDER BY ?name"
      )


      #get data from the query
      encoded_query <- utils::URLencode(sparql)
      full_url <- paste0(endpoint, "?query=", encoded_query)
      response <- httr::GET(full_url)
      data <- httr::content(response, as = "parsed")


      #Extracting variables names
      head_vars <- data$head$vars

      # Extracting the results
      results <- data$results$bindings

      # Function to extract a single row from the results
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
}
    else{
      #Define the query
      sparql <- paste0("
    select distinct str(?registat) AS ?reg_istat str(?istat) AS ?com_istat ?name ?geometry where {
      ?s a <https://w3id.org/italia/env/onto/place/Municipality>;
      <https://w3id.org/italia/env/onto/place/istat> ?istat;
      <https://w3id.org/italia/env/onto/place/hasRegion> ?reguri;
      <https://w3id.org/italia/env/onto/place/hasGeometry> ?geom ;
      <https://w3id.org/italia/env/onto/top/name> ?name2 .
      ?geom <https://w3id.org/italia/env/onto/place/geometry>  ?geometry .
      ",
                       if (!is.null(cod_mun) && !is.null(cod_reg)) paste0("FILTER (str(?istat) = '", cod_mun, "')"),
                       if (!is.null(cod_reg) && is.null(cod_mun)) paste0("FILTER (str(?registat) = '", cod, "')"),
                       if (!is.null(geo_ref) && geo_ref == "A") "FILTER(STRENDS(str(?geom), 'polygon'))",
                       if (!is.null(geo_ref) && geo_ref == "C") "FILTER(STRENDS(str(?geom), 'point'))",
                       "?reguri <https://w3id.org/italia/env/onto/place/istat> ?registat.
      BIND(str(?name2) AS ?name)
    }
    ORDER BY ?name"
      )


      #get data from the query
      encoded_query <- utils::URLencode(sparql)
      full_url <- paste0(endpoint, "?query=", encoded_query)
      response <- httr::GET(full_url)
      data <- httr::content(response, as = "parsed")


      #Extracting variables names
      head_vars <- data$head$vars

      # Extracting the results
      results <- data$results$bindings

      # Function to extract a single row from the results
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
    df$geom <- ifelse(df$COD_COM %in% geo$com_istat ,
                      geo$geometry[match(df$COD_COM, geo$com_istat)],NA)

    df <- df[!is.na(df$geom),]

  }
  #Handling Dates

  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(EffectiveDesignStartingDate = ifelse(all(is.na(c(.data$DATA_INIZIO_EFF_STUDIO_FATT, .data$DATA_INIZIO_EFF_PROG_PREL, .data$DATA_INIZIO_EFF_PROG_DEF, .data$DATA_INIZIO_EFF_PROG_ESEC))), NA, pmin(.data$DATA_INIZIO_EFF_STUDIO_FATT, .data$DATA_INIZIO_EFF_PROG_PREL, .data$DATA_INIZIO_EFF_PROG_DEF, .data$DATA_INIZIO_EFF_PROG_ESEC, na.rm = TRUE))) %>%
    dplyr::mutate(EffectiveDesignEndingDate = ifelse(all(is.na(c(.data$DATA_FINE_EFF_STUDIO_FATT,.data$DATA_FINE_EFF_PROG_PREL, .data$DATA_FINE_EFF_PROG_DEF, .data$DATA_FINE_EFF_PROG_ESEC))), NA, pmax(.data$DATA_FINE_EFF_STUDIO_FATT,.data$DATA_FINE_EFF_PROG_PREL, .data$DATA_FINE_EFF_PROG_DEF, .data$DATA_FINE_EFF_PROG_ESEC, na.rm = TRUE)))

  if (!is.null(start)) {
    df <- df %>%
      dplyr::filter(.data$EffectiveDesignStartingDate >= start)
  }

  if (!is.null(end)) {
    df <- df %>%
      dplyr::filter(.data$EffectiveDesignEndingDate <= end)

  }

  #Filter for regional code
  if (!is.null(cod_reg)) {
    if(!is.list(cod_reg) && !is.character(cod_reg)){
      if(verbose == TRUE) {
      stop("The parameter 'cod_reg'must be a character vector of regional codes.")
      }
    }

    if(is.list(cod_reg)){
      cod_reg <- unlist(cod_reg)
    }

    df <- df %>% dplyr::filter(.data$COD_REGIONE %in% cod_reg)


  }
  #Filter for province code
  if (!is.null(cod_prov)) {
    if(!is.list(cod_prov) && !is.character(cod_prov)){
      if(verbose == TRUE){
      stop("The argument 'cod_prov' must be a character vector of province codes.")
      }
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
      cat("The argument 'cod_mun'must be a character vector of municipality codes.")
        }
    }

    if(is.list(cod_mun)){
      cod_mun <- unlist(cod_mun)
    }

    df <- df %>% dplyr::filter(.data$COD_COMUNE %in% cod_mun)

  }

  if(soil_defense == TRUE) {
    df <- df %>% dplyr::filter(.data$CUP_DESCR_SOTTOSETTORE == "DIFESA DEL SUOLO")

  }
  df$OC_SINTESI_PROGETTO[df$OC_SINTESI_PROGETTO == " "] <- df$OC_TITOLO_PROGETTO[which(df$OC_SINTESI_PROGETTO == " ")]
  df <- df[,-c(3,5:45,52:54,73,75:100,102:113,115,117,119,121,123,125,127,129:137,139,141,143,145:199)]
   colnames(df)[1:41] <- c("LocalProjectCode", "CUP", "Intervention","COD_REGION", "DEN_REGION","COD_PROVINCE","DEN_PROVINCE","COD_MUNICIPALITY","DEN_MUNICIPALITY", "EuFunding", "FESR_EuFunding","FSE_EuFunding", "FEASR_EuFunding", "FEAMP_EuFunding", "IOG_EuFunding", "FondoDiRotazioneITA", "FSC_FundingITA", "PAC_FundingITA","CompletamentiFundingITA", "OtherMeasuresFundingITA", "RegionFunding", "ProvinceFunding", "MunicipalityFunding", "ReleasedResources", "OtherPublicFunding", "ForeignStateFunding","PrivateFunding", "TotalPublicFunding", "TotalFunding", "FeasibilityStudyStartingDate", "FeasibilityStudyEndingDate", "PreliminaryDesignStartingDate", "PreliminaryDesignEndingDate", "DefinitiveDesignStartingDate", "DefinitiveDesignEndingDate", "ExecutiveDesignStartingDate", "ExecutiveDesignEndingDate", "WorksExecutionStartingDate", "WorksExecutionEndingDate", "ConclusionStartingDate", "ConclusionEndingDate")
   if(length(df) == 44){
     df <- df %>% dplyr::select(1:37, 43:44, dplyr::everything()) %>% dplyr::mutate(dplyr::across(30:43, ~lubridate::ymd(.)))  %>% dplyr::mutate(dplyr::across(30:43 , ~as.character(.)))
   }else{
     df <- df %>% dplyr::select(1:37, 42:43, dplyr::everything()) %>% dplyr::mutate(dplyr::across(30:43, ~lubridate::ymd(.)))  %>% dplyr::mutate(dplyr::across(30:43 , ~as.character(.)))
   }


  return(df)

}
