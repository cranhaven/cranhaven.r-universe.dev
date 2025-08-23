#' Retrieve financial data on public works from the OpenBDAP data base.
#'
#'get_data_OBDAP function retrieves data from one or more Italian regions using ISTAT region codes. It allows filtering based on: municipality code, and the project's starting and/or ending dates. Additionally, it provides geospatial references.
#'
#'
#' @param cod_reg character vector. The ISTAT regional code is used to specify one or more regions of interest when retrieving data. (See \code{\link{get_codes}} function)
#' @param cod_prov character vector. The ISTAT province code is used to specify one or more provinces of interest when retrieving data. (See \code{\link{get_codes}} function)
#' @param cod_mun character vector The ISTAT municipal code is used to specify one or more municipalities of interest when retrieving data. (See \code{\link{get_codes}} function)
#' @param start character (format YYYY-mm-dd). Effective starting date of design refers to the specific phase of a public project that marks the beginning of its design process. This date can be of interest for filtering and analyzing relevant data.
#' @param end character (format YYYY-mm-dd). Effective ending date of design refers to the specific phase of a public project that marks the conclusion of its design process. This date can be of interest for filtering and analyzing relevant data.
#' @param geo_ref character. The georeference data can be specified using the \code{geo_ref} argument. If set to \code{A}, the function returns shape polygons of each municipality. If set to \code{C}, it retrieves the coordinates of the centroids of each municipality.
#' @param soil_defense logical.  By default set to \code{FALSE}. If only soil defense data are of interest set the argument to \code{TRUE}.
#' @param verbose Logic value (TRUE or FALSE). Toggle warnings and messages. If 'verbose = TRUE' (default) the function
#' prints on the screen some messages describing the progress of the tasks. If 'verbose = FALSE' any message about
#' the progression is suppressed.
#' @import magrittr
#' @importFrom rlang .data
#' @import PublicWorksFinanceIT
#' @returns Object of class \code{data.frame} showing 22 variables.Descriptive Variables:
#' \itemize{
#' \item{Local Project Code (\code{character})}
#' \item{CUP (\code{character})}
#' \item{Intervention (\code{character})}
#' }
#' Financial Variables:
#' \itemize{
#' \item{State Funding (\code{numeric})}
#' \item{EU Funding (\code{numeric})}
#' \item{Local Authorities Funding (\code{numeric})}
#' \item{Private Funding (\code{numeric})}
#' \item{Other Funding (\code{numeric})}
#' }
#'
#' Geographical References:
#'  \itemize{
#'  \item{DEN_REGION (\code{character})}
#'  \item{DEN_PROVINCE (\code{character})}
#'  \item{DEN_MUNICIPALITY (\code{character})}
#'  \item{COD_REGION (\code{character})}
#'  \item{COD_PROVINCE (\code{character})}
#'  \item{COD_MUNICIPALITY (\code{character})}
#'  \item{geom (\code{character})}
#'  }
#'
#'  Legislative process main steps:
#'  \itemize{
#'  \item{Executive Design Starting Date (\code{character})}
#'  \item{Executive Design Ending Date (\code{character})}
#'  \item{Works Execution Starting Date (\code{character})}
#'  \item{Works Execution Ending Date (\code{character})}
#'  \item{Conclusion Starting Date (\code{character})}
#'  \item{Conclusion Ending Date (\code{character})}
#'  \item{Operability (\code{character})}
#'  }
#'@references \href{https://bdap-opendata.rgs.mef.gov.it/tema/opere-pubbliche}{Open BDAP}
#'@author Lorena Ricciotti
#' @examples
#' \donttest{data <- get_data_OBDAP("14")}
#' # Retrieve data for one region filtering for soil defense interventions.
#' @export
get_data_OBDAP <- function(cod_reg, cod_prov = NULL, cod_mun= NULL, start = NULL, end = NULL, geo_ref = NULL, soil_defense = FALSE, verbose = TRUE) {

  if (!is.list(cod_reg) && !is.character(cod_reg)) {
    warning("The argument cod_reg must be a vector of regional codes.")
  }

  df <- data.frame()
  temp_dir <- tempdir()
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  for (cod in cod_reg) {

    url <- paste0("https://bdap-opendata.rgs.mef.gov.it/SpodCkanApi/api/1/rest/dataset/spd_mop_prg_mon_reg",
                  cod, "_01_9999.csv")

    nome_file <- file.path(temp_dir, paste0("Progetti_Opere_Pubbliche_Region_", cod, ".csv"))

    utils::download.file(url, nome_file, mode = "wb", quiet =T)
    if(verbose == TRUE){
    cat(paste("Downloaded:", nome_file, "\n"))
      }
    dataset <- utils::read.csv(nome_file, sep = ";", dec = ".", check.names = F, fileEncoding = "latin1")
    dataset <- dataset[,-49]

    #Get georeferenced data
    #geo_OBDAP <- PublicWorksFinanceIT::geo_OBDAP
    url <- "https://bdap-opendata.rgs.mef.gov.it/SpodCkanApi/api/1/rest/dataset/spd_mop_loc_mon_local_01_9999.csv"
    nome_file <- file.path(temp_dir, "loc.csv")
    utils::download.file(url, nome_file, mode = "wb", quiet =T, timeout = 90)
    geo_OBDAP <- utils::read.csv(nome_file, sep = ";")

    # Join data localization
    names(dataset)[2] <- "Codice.CUP"
    dataset <- dataset %>%
      dplyr::left_join(geo_OBDAP %>% dplyr::select(.data$Codice.CUP, .data$Codice.Regione, .data$Descrizione.Regione,
                                                   .data$Codice.Provincia, .data$Descrizione.Provincia, .data$Codice.Comune,
                                                   .data$Descrizione.Comune),
                       by = "Codice.CUP", relationship = "many-to-many") %>%
      dplyr::mutate(DEN_REGIONE = .data$Descrizione.Regione, COD_REGIONE = .data$Codice.Regione,
                    DEN_PROVINCIA = .data$Descrizione.Provincia, COD_PROVINCIA = .data$Codice.Provincia,
                    DEN_COMUNE = .data$Descrizione.Comune, COD_COMUNE = .data$Codice.Comune) %>%
      dplyr::select(-c(.data$Codice.Regione, .data$Descrizione.Regione, .data$Codice.Provincia, .data$Descrizione.Provincia,
                       .data$Codice.Comune, .data$Descrizione.Comune)) %>%
      dplyr::mutate(COD_COMUNE =
               dplyr::case_when(nchar(.data$COD_COMUNE) == 1  ~ paste0("0", .data$COD_PROVINCIA, "00", .data$COD_COMUNE),
                         nchar(.data$COD_COMUNE) == 2 ~ paste0("0",.data$COD_PROVINCIA, "0",.data$COD_COMUNE)))

    df <- rbind(dataset, df)
    unlink(temp_dir, recursive = TRUE)

  }


  if(!is.null(geo_ref)){
    geo <- data.frame()
    #Get georeferenced data

    #Define the endpoint
    endpoint <- "https://dati.isprambiente.it/sparql"

    for(cod in cod_reg){
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

      #dataset$PRO_COM <- paste0("0",dataset$COD_PROVINCIA,"0",dataset$COD_COMUNE)
      geo <- rbind(loc,geo)
    }

    df$COD_COMUNE <- as.numeric(df$COD_COMUNE)
    geo$COD_COMUNE <- as.numeric(geo$COD_COMUNE)

    df <- df %>% dplyr::left_join(geo %>% dplyr::select(.data$COD_COMUNE,.data$geometry),
                                  by = "COD_COMUNE") %>%
      dplyr::mutate(geom = .data$geometry) %>% dplyr::select(-.data$geometry)
    # df$geom <- ifelse(df$COD_COMUNE %in% geo$com_istat ,
    # geo$geometry[match(df$COD_COMUNE, geo$com_istat)],NA)

    df <- df[!is.na(df$geom),]


  }

  # Filter for dates
  if (!is.null(start)) {

    df <- df[df$`Inizio progettazione effettiva` >= start, ]

  }

  if (!is.null(end)) {

    df <- df[df$`Fine progettazione effettiva` <= end, ]

  }

  #Filters for municipality codes
  if (!is.null(cod_mun)) {
    if(!is.list(cod_mun) && !is.character(cod_mun)){
      warning("The argument 'cod_mun' must be a vector of municipality codes.")
    }

    if(is.list(cod_mun)){
      cod_mun <- unlist(cod_mun)
    }
    df <- df %>% dplyr::filter(.data$COD_COMUNE %in% cod_mun)


  }

  #Filter for province codes
  df$COD_PROVINCIA <- paste0(0, df$COD_PROVINCIA)
  if (!is.null(cod_prov)) {
    if(!is.list(cod_prov) && !is.character(cod_prov)){
      warning("The argument 'cod_prov' must be a vector of provinces codes.")
    }

    if(is.list(cod_prov)){
      cod_prov <- unlist(cod_prov)
    }
    df <- df %>% dplyr::filter(.data$COD_PROVINCIA %in% cod_prov)


  }

  #Filter soil defense data
  if(soil_defense == TRUE){
    df <- df %>% dplyr::filter(.data$`Sottosettore Interv Inv` == "DIFESA DEL SUOLO")

  }
  # Return the dataset
  df <- df[, -c(4:23,26:27,30:31,34,36:41,47:48)]
  colnames(df)[c(1:21)] <- c("LocalProjectCode", "CUP", "Intervention", "EffectiveDesignStartingDate",
                             "EffectiveDesignEndingDate", "WorksExecutionStartingDate", "WorksExecutionEndingDate", "ConclusionStartingDate", "ConclusionEndingDate", "Operability", "StateFunding", "EuFunding", "LocalAuthoritiesFunding", "PrivateFunding", "OtherFunding","DEN_REGION","COD_REGION", "DEN_PROVINCE","COD_PROVINCE","DEN_MUNICIPALITY","COD_MUNICIPALITY")
  return(df)
}
