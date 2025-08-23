#' Retrieve data from the ReNDiS database on soil defense public works.
#'
#' The \code{get_data_RENDIS} function enables the retrieval of data from one or more region or type of intervention using associated codes. It allows filtering based on: municipality code, and the project's starting and/or ending dates. Additionally, it provides geospatial references.
#'
#' @param cod_reg character. The ISTAT regional code is used to filter data based on one or more specific regions of interest. (See \code{\link{get_codes}} function)
#' @param cod_prov character. The ISTAT province code is used to specify one or more provinces of interest within the region(s) of interest. (See \code{\link{get_codes}} function)
#' @param cod_mun character. The ISTAT municipality code is used to specify one or more municipalities of interest within the region(s) of interest. (See \code{\link{get_codes}} function)
#' @param start character (format YYYY-mm-dd). Effective starting date of design refers to the specific phase of a public project that marks the beginning of its design process. This date can be of interest for filtering and analyzing relevant data.
#' @param end character (format YYYY-mm-dd). Effective ending date of design refers to the specific phase of a public project that marks the conclusion of its design process. This date can be of interest for filtering and analyzing relevant data.
#' @param type character. a character string on which type of intervetion data needs to be retrieved. To get information about type see \code{\link{get_type_RENDIS}} function.
#' @param geo_ref character. The georeference data can be specified using the \code{geo_ref} argument. If set to \code{A}, the function returns shape polygons of each municipality. If set to \code{C}, it retrieves the coordinates of the centroids of each municipality.
#'
#' @returns Object of class \code{tbl_df, tbl, data.frame} showing 25 variables.
#' Descriptive Variables:
#' \itemize{
#' \item{CUP (\code{character})}
#' \item{Intervention (\code{character})}
#' \item{Type (\code{character})}
#' }
#'
#' Financial Variable:
#' \itemize{
#'  \item{Finance (\code{numeric})}
#'  }
#'
#'  Geographical References:
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
#'  \item{Feasibility Study Starting Date (\code{character})}
#'  \item{Feasibility Study Ending Date (\code{character})}
#'  \item{Preliminary Design Starting Date (\code{character})}
#'  \item{Preliminary Design Ending Date (\code{character})}
#'  \item{Definitive Design Starting Date (\code{character})}
#'  \item{Definitive Design Ending Date (\code{character})}
#'  \item{Executive Design Starting Date (\code{character})}
#'  \item{Executive Design Ending Date (\code{character})}
#'  \item{Works Execution Starting Date (\code{character})}
#'  \item{Works Execution Ending Date (\code{character})}
#'  \item{Conclusion Starting Date (\code{character})}
#'  \item{Conclusion Ending Date (\code{character})}
#'  \item{Intervention Closed (\code{character})}
#'  \item{Operability (\code{character})}
#'  }
#'
#'@references \href{http://www.rendis.isprambiente.it/rendisweb/}{ReNDiS}
#'
#'@author Lorena Ricciotti
#'
#'@examples
#'data_12 <- get_data_RENDIS("12", cod_prov = c("258", "059"), geo_ref = "C")
#' #Data for the Lazio region filtering for Rome and Latina provinces with point georeferences.
#' @encoding UTF-8
#'
#' @export
get_data_RENDIS <- function(cod_reg, cod_prov = NULL, cod_mun = NULL, start = NULL, end = NULL,  type = NULL, geo_ref = NULL) {
  # Define the SPARQL endpoint
  endpoint <- "https://dati.isprambiente.it/sparql"

  df <- data.frame()
  loc <- data.frame()

  for (current_cod_reg in cod_reg) {
    # Define SPARQL query with parameters for interventions
    sparql_query <- paste0(
      'SELECT DISTINCT (str(?cup) AS ?CUP) (str(?int) AS ?Intervention) (str(?type) AS ?Type) (str(?imp) AS ?Finance) (str(?com) AS ?DEN_COMUNE) str(?reg) AS ?DEN_REGIONE str(?reg_istat) AS ?COD_REGIONE (str(?codist) AS ?com_istat) (str(?prov) AS ?DEN_PROVINCE)
(str(?cod_prov) AS ?COD_PROVINCE) (str(?code) AS ?COD_phase) (str(?phase) AS ?DEN_phase)  (str(?name) AS ?BDAP_phase) (str(?date) AS ?Date)
      WHERE {
        ?i a <https://w3id.org/italia/env/onto/core#Intervention>;
        <https://w3id.org/italia/env/onto/core#officialInstabilityType> ?type;
        <http://www.w3.org/2000/01/rdf-schema#label> ?int;
        <https://w3id.org/italia/env/onto/core#amountFinanced> ?imp;
        <https://w3id.org/italia/env/onto/core#primaryGeographicalFeature> ?pl.

        ?pl <http://www.w3.org/2000/01/rdf-schema#label> ?com;
        <https://w3id.org/italia/env/onto/top/hasUniqueIdentifier> ?codisturi;
        <https://w3id.org/italia/env/onto/place/hasDirectHigherRank>?pro.
?pro <http://www.w3.org/2000/01/rdf-schema#label> ?prov;
<https://w3id.org/italia/env/onto/place/istat> ?cod_prov;

        <https://w3id.org/italia/env/onto/place/hasRegion> ?r.

        ?r <http://www.w3.org/2000/01/rdf-schema#label> ?reg;
        <https://w3id.org/italia/env/onto/top/hasUniqueIdentifier> ?codreg.

        ?codreg <https://w3id.org/italia/env/onto/top/identifier> ?reg_istat.

        ?codisturi <https://w3id.org/italia/env/onto/top/identifier> ?codist.

        ?c a <https://w3id.org/italia/env/onto/core#Contract>;
        <https://w3id.org/italia/env/onto/core#isLotOf> ?i;
        <https://w3id.org/italia/env/onto/core#cup> ?cup;
        <http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?lat;
        <http://www.w3.org/2003/01/geo/wgs84_pos#long> ?lon.

?l a <http://www.w3.org/2004/02/skos/core#Concept>;
<http://www.w3.org/2004/02/skos/core#inScheme> <https://w3id.org/italia/env/ld/skos/ihi/steptype/vocabolario>;
<https://w3id.org/italia/env/onto/core#phase> ?phase;
<http://www.w3.org/2000/01/rdf-schema#label> ?name;
<https://w3id.org/italia/env/onto/core#sequencePhase> ?code.

?d a <https://w3id.org/italia/env/onto/core#LotStep>;
<https://w3id.org/italia/env/onto/core#hasStep> ?l;
<https://w3id.org/italia/env/onto/core#lot> ?c;
<http://purl.org/dc/elements/1.1/date> ?date.
      FILTER (str(?code)  IN ("82",
"102" ,
"115" ,
"125",
"132" ,
"142" ,
"152" ,
"172" ,
"184" ,
"215" ,
"217" ,
"223",
"227",
"229" )
)',
      # if (!is.null(cod_mun) && !is.null(cod_reg)) paste0("FILTER (str(?codist) = '", cod_mun, "') "),
      if (!is.null(cod_reg)) paste0("FILTER (str(?reg_istat) = '", current_cod_reg, "') "),
      if (!is.null(type)) paste0("FILTER (str(?type) = '", type, "') "),
      #if(!is.null(start) && !is.null(end)) paste0("FILTER(str(?date) >= '", start, "' && (str(?date) <= '", end, "')"),
      #if( !is.null(end)) paste0("FILTER (str(?date) <= '", end, "') "),
      #if(!is.null(start)) paste0("FILTER (str(?date) >= '", start, "') "),

      '} ',
      'ORDER BY ?reg ?cup ?code'
    )

    # Make the SPARQL query for interventions
    response_interventions <- httr::POST(
      url = endpoint,
      httr::add_headers("Content-Type" = "application/x-www-form-urlencoded"),
      body = list(query = sparql_query),
      encode = "form"
    )

    # Get the content of the response for interventions
    data_interventions <- httr::content(response_interventions, "parsed")

    head_vars_interventions <- data_interventions$head$vars
    results_interventions <- data_interventions$results$bindings

    extract_row_interventions <- function(result) {
      values <- sapply(head_vars_interventions, function(var) {
        if (length(result[[var]]) == 0) {
          NA
        } else {
          result[[var]]$value
        }
      })
      data.frame(t(values), stringsAsFactors = FALSE)
    }

    rows_interventions <- lapply(results_interventions, extract_row_interventions)
    current_df_interventions <- do.call(rbind, rows_interventions)
    colnames(current_df_interventions) <- head_vars_interventions

    suppressWarnings(
    current_df_interventions <- current_df_interventions %>% dplyr::arrange(as.numeric(.data$COD_phase)) %>%
      tidyr::pivot_wider(id_cols = c(.data$CUP, .data$Intervention, .data$Type, .data$Finance, .data$DEN_COMUNE, .data$DEN_REGIONE, .data$COD_REGIONE, .data$com_istat, .data$COD_PROVINCE, .data$DEN_PROVINCE),
                  names_from = .data$BDAP_phase,
                  values_from = .data$Date)
    )

    df <- rbind(df, current_df_interventions)
    if(anyDuplicated(df$CUP)){
      df <- df[!duplicated(df$CUP),]
    }

    # Define the SPARQL query with parameters for georeferenced data
    sparql_query_geo <- paste0(
      'SELECT DISTINCT str(?registat) AS ?reg_istat str(?istat) AS ?com_istat ?name  str(?prov) AS ?DEN_PROVINCE str(?cod_prov) AS ?COD_PROVINCE ?geometry
      WHERE {
        ?s a <https://w3id.org/italia/env/onto/place/Municipality>;
        <https://w3id.org/italia/env/onto/place/istat> ?istat;
        <https://w3id.org/italia/env/onto/place/hasRegion> ?reguri;
        <https://w3id.org/italia/env/onto/place/hasGeometry> ?geom ;
        <https://w3id.org/italia/env/onto/top/name> ?name2;
<https://w3id.org/italia/env/onto/place/hasDirectHigherRank> ?pro.
?pro<http://www.w3.org/2000/01/rdf-schema#label> ?prov;
<https://w3id.org/italia/env/onto/place/istat> ?cod_prov.

        ?geom <https://w3id.org/italia/env/onto/place/geometry>  ?geometry .
        ',
      # if (!is.null(cod_mun) && !is.null(cod_reg)) paste0("FILTER (str(?istat) = '",cod_mun, "')"),
      if (!is.null(cod_reg)) paste0("FILTER (str(?registat) = '", current_cod_reg, "')"),
      if (!is.null(geo_ref) && geo_ref == "A") "FILTER(STRENDS(str(?geom), 'polygon'))",
      if (!is.null(geo_ref) && geo_ref == "C") "FILTER(STRENDS(str(?geom), 'point'))",
      '?reguri <https://w3id.org/italia/env/onto/place/istat> ?registat.
      BIND(str(?name2) AS ?name)
    }
    ORDER BY ?name'
    )

    # Make the SPARQL query for georeferenced data
    response_geo <- httr::POST(
      url = endpoint,
      httr::add_headers("Content-Type" = "application/x-www-form-urlencoded"),
      body = list(query = sparql_query_geo),
      encode = "form"
    )

    # Get the content of the response for georeferenced data
    data_geo <- httr::content(response_geo, as = "parsed")

    head_vars_geo <- data_geo$head$vars

    results_geo <- data_geo$results$bindings

    # Function to extract a single row from the results for georeferenced data
    extract_row_geo <- function(result) {
      values <- sapply(head_vars_geo, function(var) {
        if (length(result[[var]]) == 0) {
          NA
        } else {
          result[[var]]$value
        }
      })
      data.frame(t(values), stringsAsFactors = FALSE)
    }

    # Apply the function to each result for georeferenced data
    rows_geo <- lapply(results_geo, extract_row_geo)

    # Combining the rows into a data frame for georeferenced data
    loc_current <- do.call(rbind, rows_geo)

    # Renaming the columns for georeferenced data
    colnames(loc_current) <- head_vars_geo

    loc <- rbind(loc, loc_current)

  }

  # Assign geometries
   df <- df %>% dplyr::left_join(loc %>% dplyr::select(.data$com_istat, .data$geometry), by = "com_istat") %>%
     dplyr::mutate(geom = .data$geometry) %>%
     dplyr::select(-.data$geometry)

  df <- df[!is.na(df$geom), ]

  # Convert imp to numeric
  df$Finance <- as.numeric(df$Finance)

  # Rename columns
  names(df)[names(df) == "com_istat"] <- "COD_COMUNE"

  #Filter for municipality code
  if (!is.null(cod_mun)) {
    if(!is.list(cod_mun) && !is.character(cod_mun)){
      stop("The argument 'cod_mun' must be a vector of municipality codes.")
    }

    if(is.list(cod_mun)){
      cod_mun <- unlist(cod_mun)
    }
    df <- df %>% dplyr::filter(.data$COD_COMUNE %in% cod_mun)


  }

  if (!is.null(cod_prov)) {
    if(!is.list(cod_prov) && !is.character(cod_prov)){
      stop("The argument 'cod_prov' must be a vector of municipality codes.")
    }

    if(is.list(cod_prov)){
      cod_prov <- unlist(cod_prov)
    }
    df <- df %>% dplyr::filter(.data$COD_PROVINCE %in% cod_prov)


  }

  #Handling Dates
  # df <- df %>% dplyr::rowwise() %>%
  #   dplyr::mutate(EffectiveDesignStartingDate = ifelse(all(is.na(df[,c(11,13,15,17)])), NA, pmin(df[11,13,15,17], na.rm = TRUE))) %>%
  #   dplyr::mutate(EffectiveDesignEndingDate = ifelse(all(is.na(df[,c(12,14,16,18)])), NA, pmax(df[12,14,16,18], na.rm = TRUE))) %>% dplyr::ungroup() %>% as.data.frame()
 colnames(df)[11:12] <- c("(BDAP - inizio Studio di fattibilita)", "(BDAP - Fine Studio di fattibilita)")
 df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(EffectiveDesignStartingDate = ifelse(all(is.na(c(.data$`(BDAP - inizio Studio di fattibilita)`, .data$`(BDAP - inizio Progettazione Preliminare)`, .data$`(BDAP - Inizio progettazione definitiva)`, .data$`(BDAP - Inizio progettazione esecutiva)`))), NA, pmin(.data$`(BDAP - inizio Studio di fattibilita)`, .data$`(BDAP - inizio Progettazione Preliminare)`, .data$`(BDAP - Inizio progettazione definitiva)`, .data$`(BDAP - Inizio progettazione esecutiva)`, na.rm = TRUE))) %>%
    dplyr::mutate(EffectiveDesignEndingDate = ifelse(all(is.na(c(.data$`(BDAP - Fine Studio di fattibilita)`,.data$`(BDAP - Fine Progettazione Preliminare)`, .data$`(BDAP - fine progettazione definitiva)`, .data$`(BDAP - fine progettazione esecutiva)`))), NA, pmax(.data$`(BDAP - Fine Studio di fattibilita)`,.data$`(BDAP - Fine Progettazione Preliminare)`, .data$`(BDAP - fine progettazione definitiva)`, .data$`(BDAP - fine progettazione esecutiva)`, na.rm = TRUE))) %>% dplyr::ungroup() %>% as.data.frame()


  if(!is.null(start)){
    df <- df %>%
      dplyr::filter(.data$EffectiveDesignStartingDate >= start)

  }

  if(!is.null(end)){
    df <- df %>%
      dplyr::filter(.data$EffectiveDesignEndingDate <= end)
  }
colnames(df)[5:8] <- c("DEN_MUNICIPALITY", "DEN_REGION", "COD_REGION", "COD_MUNICIPALITY")

   if(length(df) == 27){
 colnames(df)[11:24] <- c( "FeasibilityStudyStartingDate", "FeasibilityStudyEndingDate", "PreliminaryDesignStartingDate", "PreliminaryDesignEndingDate", "DefinitiveDesignStartingDate", "DefinitiveDesignEndingDate", "ExecutiveDesignStartingDate", "ExecutiveDesignEndingDate", "WorksExecutionStartingDate", "WorksExecutionEndingDate", "ConclusionStartingDate", "ConclusionEndingDate", "InterventionClosed", "Operability")

 df <- df %>% dplyr::select(1:4,7,6,9,10,8,5,11:18, 26:27, dplyr::everything())
    } else{

    colnames(df)[11:23] <- c( "FeasibilityStudyStartingDate", "FeasibilityStudyEndingDate", "PreliminaryDesignStartingDate", "PreliminaryDesignEndingDate", "DefinitiveDesignStartingDate", "DefinitiveDesignEndingDate", "ExecutiveDesignStartingDate", "ExecutiveDesignEndingDate", "WorksExecutionStartingDate", "WorksExecutionEndingDate", "ConclusionStartingDate", "ConclusionEndingDate", "InterventionClosed")

    df <- df %>% dplyr::mutate(Operability = NA) %>% dplyr::select(1:4, 7,6,9,10, 8,5,11:18, 25:26,27, dplyr::everything())
}

  return(df)
}
