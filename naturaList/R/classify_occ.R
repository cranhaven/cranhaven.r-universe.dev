#' Classify occurrence records in levels of confidence in species identification
#'
#' @description Classifies occurrence records in levels of confidence in species
#' identification
#'
#' @param occ data frame with occurrence records information.
#' @param spec data frame with specialists' names. See details.
#' @param na.rm.coords logical. If \code{TRUE}, remove occurrences with \code{NA}
#'  in \code{decimal.latitude} or \code{decimal.longitude}
#' @param crit.levels character. Vector with levels of confidence in decreasing
#'  order. The criteria allowed are \code{det_by_spec}, \code{not_spec_name},
#'  \code{image}, \code{sci_collection}, \code{field_obs}, \code{no_criteria_met}.
#'  See details.
#' @param ignore.det.names character vector indicating strings in
#'  \code{identified.by} that should be ignored as a taxonomist. See details.
#' @param spec.ambiguity character. Indicates how to deal with ambiguity in
#'  specialists names. \code{not.spec} solve ambiguity by classifying the
#'  identification as done by a non-specialist;\code{is.spec} assumes the
#'  identification was done by a specialist; \code{manual.check} enables the
#'  user to manually check all ambiguous names. Default is \code{not.spec}.
#' @param occurrence.id column name of \code{occ} with link or code for the
#'  occurrence record. See in
#'  \href{https://dwc.tdwg.org/terms/#dwc:occurrenceID}{Darwin Core Format}
#' @param occ.id deprecated, use \code{occurrence.id} instead
#' @param species column name of \code{occ} with the species names.
#' @param scientific.name deprecated, use \code{species} instead.
#' @param identified.by column name of \code{occ} with the name of who
#'  determined the species.
#' @param determined.by deprecated, use \code{identified.by} instead
#' @param decimal.longitude column name of \code{occ} longitude in decimal
#'  degrees.
#' @param longitude deprecated, use \code{decimal.longitude} instead
#' @param decimal.latitude column name of \code{occ} latitude in decimal
#'  degrees.
#' @param latitude deprecated, use \code{decimal.latitude} instead
#' @param basis.of.record column name with the specific nature of the data
#'  record. See details.
#' @param basis.of.rec deprecated, use \code{basis.of.record} instead.
#' @param media.type column name of \code{occ} with the media type of recording.
#'  See details.
#' @param institution.code column name of \code{occ} with the name (or acronym)
#'  in use by the institution having custody of the object(s) or information
#'  referred to in the record.
#' @param institution.source deprecated, use \code{institution.code} instead.
#' @param collection.code column name of \code{occ} with The name, acronym,
#'  code, or initials identifying the collection or data set from which the
#'  record was derived.
#' @param catalog.number column name of \code{occ} with an identifier
#'  (preferably unique) for the record within the data set or collection.
#' @param year Column name of \code{occ} the four-digit year in which the
#'  Event occurred, according to the Common Era Calendar.
#' @param year.event deprecated, use \code{year} instead.
#' @param date.identified Column name of \code{occ} with the date on which the
#'  subject was determined as representing the Taxon.
#'
#' @return The \code{occ} data frame plus the classification of each record
#' in a new column, named \code{naturaList_levels}.
#'
#' @details \code{spec} data frame must have columns separating \code{LastName},
#' \code{Name} and \code{Abbrev}. See {\link[naturaList]{create_spec_df}}
#' function for a easy way to produce this data frame.
#'
#' @details When \code{ignore.det.name = NULL} (default), the function ignores
#' strings with \code{"RRC ID Flag", "NA", "", "-" and "_".} When a character
#' vector is provided, the function adds the default strings to the provided
#' character vector and ignore all these strings as being a name of a taxonomist.
#'
#' @details The function classifies the occurrence records in six levels of
#' confidence in species identification. The six levels are:
#'  \itemize{
#'  \item \code{det_by_spec} - when the identification was made by a specialists
#'     which is present in the list of specialists provided in the \code{spec}
#'     argument;
#'  \item \code{not_spec_name} - when the identification was made by a name who is
#'     not a specialist name provide in \code{spec};
#'  \item \code{image} - the occurrence have not name of a identifier, but present
#'     an image associated;
#'  \item \code{sci_collection} - the occurrence have not name of a identifier,
#'    but preserved in a scientific collection;
#'  \item \code{field_obs} - the occurrence have not name of a identifier,
#'    but it was identified in field observation;
#'  \item\code{no_criteria_met} - no other criteria was met.
#' }
#'  The (decreasing) order of the levels in the character vector determines the
#'  classification level order.
#'
#' @details \code{basis.of.record} is a character vector with one of the following
#' types of record: \code{PRESERVED_SPECIMEN}, \code{PreservedSpecimen},
#'  \code{HUMAN_OBSERVATION} or \code{HumanObservation}, as in GBIF data
#'  'basisOfRecord'.
#' @details \code{media.type} uses the same pattern as GBIF mediaType column,
#' indicating the existence of an associated image with \code{stillImage}.
#'
#' @examples
#' data("A.setosa")
#' data("speciaLists")
#' occ.class <- classify_occ(A.setosa, speciaLists)
#'
#'
#' @seealso \code{\link{speciaLists}}
#'
#' @author Arthur V. Rodrigues
#'
#' @importFrom rlang .data
#' @import tm
#' @export

classify_occ <- function(
  occ,
  spec = NULL,
  na.rm.coords = TRUE,
  crit.levels = c(
    "det_by_spec",
    "not_spec_name",
    "image",
    "sci_collection",
    "field_obs",
    "no_criteria_met"
    ),
  ignore.det.names = NULL,
  spec.ambiguity = "not.spec",
  institution.code ="institutionCode",
  collection.code = "collectionCode",
  catalog.number = "catalogNumber",
  year = "year",
  date.identified = "dateIdentified",
  species = "species",
  identified.by = "identifiedBy",
  decimal.latitude = "decimalLatitude",
  decimal.longitude = "decimalLongitude",
  basis.of.record = "basisOfRecord",
  media.type = "mediaType",
  occurrence.id = "occurrenceID",
  institution.source , #deprecated
  year.event, #deprecated
  scientific.name, #deprecated
  determined.by, #deprecated
  latitude, #deprecated
  longitude, #deprecated
  basis.of.rec, #deprecated
  occ.id #deprecated
  ){


# new arguments  ----------------------------------------------------------

  if (!missing(institution.source)) {
    warning("argument 'institution.source' is deprecated; please use 'institution.code' instead.",
            call. = FALSE)
    institution.code <- institution.source
  }

  if (!missing(year.event)) {
    warning("argument 'year.event' is deprecated; please use 'year' instead.",
            call. = FALSE)
    year <- year.event
  }

  if (!missing(scientific.name)) {
    warning("argument 'scientific.name' is deprecated; please use 'species' instead.",
            call. = FALSE)
    species <- scientific.name
  }

  if (!missing(determined.by)) {
    warning("argument 'determined.by' is deprecated; please use 'identified.by' instead.",
            call. = FALSE)
    identified.by <- determined.by
  }

  if (!missing(latitude)) {
    warning("argument 'latitude' is deprecated; please use 'decimal.latitude' instead.",
            call. = FALSE)
    decimal.latitude <- latitude
  }

  if (!missing(longitude)) {
    warning("argument 'longitude' is deprecated; please use 'decimal.longitude' instead.",
            call. = FALSE)
    decimal.longitude <- longitude
  }

  if (!missing(basis.of.rec)) {
    warning("argument 'basis.of.rec' is deprecated; please use 'basis.of.record' instead.",
            call. = FALSE)
    basis.of.record <- basis.of.rec
  }

  if (!missing(occ.id)) {
    warning("argument 'occ.id' is deprecated; please use 'occurrence.id' instead.",
            call. = FALSE)
    occurrence.id <- occ.id
  }

  c.tax <- crit.levels == "taxonomy"
  if(any(c.tax)){
    warning("string 'taxonomy' in crit.levels argument is depreceated, please use 'not_spec_name' instead.")
    crit.levels[c.tax] <- 'not_spec_name'
  }

  c.sci <- crit.levels == "sci_colection"
  if(any(c.sci)){
    warning("string 'sci_colection' in crit.levels argument was mispelled and is depreceated, please use 'sci_collection' instead.")
    crit.levels[c.sci] <- 'sci_collection'
  }


  natList_column <- "naturaList_levels" %in% colnames(occ)
  if(natList_column){
    col.number <- grep("naturaList_levels", colnames(occ))
    occ <- occ[, -col.number]

    warning("'occ' already had classification. The classification was remade")
  }

  if(is(occ, "tbl")){
    occ <- as.data.frame(occ)
  }

  if(!any(spec.ambiguity %in% c("not.spec", "is.spec", "manual.check"))){
    stop("argument 'spec.ambiguity' must be one 'not.spec', 'is.spec' or 'manual.check'. Please see ?classify.occ for details")
  }

  r.occ <- reduce.df(occ,
                     institution.code = institution.code,
                     collection.code = collection.code,
                     catalog.number = catalog.number,
                     year = year,
                     date.identified = date.identified,
                     species = species,
                     identified.by = identified.by,
                     decimal.longitude = decimal.longitude,
                     decimal.latitude = decimal.latitude,
                     basis.of.record = basis.of.record,
                     media.type = media.type,
                     occurrence.id = occurrence.id,
                     na.rm.coords = na.rm.coords)

  if(!is.null(spec)){
    spec.list <- as.data.frame(lapply(spec, as.character), stringsAsFactors = F)
  }

  ## Classification
  lowest_level <- paste0(length(crit.levels), "_", "no_criteria_met")
  naturaList_levels <- rep(lowest_level, nrow(r.occ))

  for(i in length(crit.levels):1){

    if(crit.levels[i] == "field_obs"){
      field_obs_level <- paste0(i, "_", "field_obs")
      FObs <- which(toupper(r.occ$basis.of.record) %in%
                      c("HUMAN_OBSERVATION", "HumanObservation"))
      naturaList_levels[FObs] <- field_obs_level
    }
    if(crit.levels[i] == "sci_collection"){
      sci_col_level <- paste0(i, "_", "sci_collection")
      SCol <- which(toupper(r.occ$basis.of.record) %in%
                      c("PRESERVED_SPECIMEN", "PreservedSpecimen"))
      naturaList_levels[SCol] <- sci_col_level
    }
    if(crit.levels[i] == "image"){
      image_level <- paste0(i, "_", "image")
      Img <- which(toupper(r.occ$media.type) %in% "STILLIMAGE")
      naturaList_levels[Img] <- image_level
    }
    if(crit.levels[i] == "not_spec_name"){
      tax_level <- paste0(i, "_", "not_spec_name")
      Tax <- has.det.ID(r.occ, ignore.det.names)
      naturaList_levels[Tax] <- tax_level
    }
    if(crit.levels[i] == "det_by_spec"){


      if(!is.null(spec)){
        DSpec <- unlist(lapply(1:nrow(spec.list),
                               function(x) func.det.by.esp(r.occ, x, spec.list)))

        if(length(DSpec) > 0){
          naturaList_levels[DSpec] <- apply(
            r.occ[DSpec,], 1, function(x) {
              specialist.conference(x, spec.list)
          })
        }

        if(any(naturaList_levels == "1_det_by_spec_verify")){

          # filtra apenas as linha com ambiguidade
          cl.verify <- naturaList_levels == "1_det_by_spec_verify"

          # Cria uma tabela tidy para conferir especialistas
          cl.tidy <- dplyr::tibble(row = r.occ$rowID[cl.verify],
                            id = as.character(naturaList_levels)[cl.verify],
                            det = as.character(r.occ$identified.by)[cl.verify])

          # remove pontos, barras e traços
          cl.tidy$det <-  gsub("\\/|-|\\.", " ", cl.tidy$det)

          # document term matrix (dtm)
          word_counts_dtm <- cl.tidy %>%
            tidytext::unnest_tokens(.data$word, det) %>% # cria token
            dplyr::count(row, .data$word, sort = TRUE) %>% # contagem
            dplyr::ungroup() %>%
            tidytext::cast_dtm(row, .data$word, .data$n) # document term matrix (dtm)

          # dtm para matrix
          mtx.word <- as.matrix(word_counts_dtm)
          if(nrow(mtx.word) > 1){
            mtx.word <- mtx.word[order(as.numeric(row.names(mtx.word))),]
          }

          confer.spec <- sapply(1:nrow(mtx.word), function(i){
            # words para a linha
            row.words <- colnames(mtx.word)[mtx.word[i,] != 0]
            # especialistas relacionados à linha
            row.num.spec <- which(tolower(spec$LastName) %in% row.words)

            if(length(row.num.spec) != 0){
              max.match <- max(sapply(row.num.spec, function(x){
                spec.vec <- as.character(spec[x,])
                names(spec.vec) <- names(spec)

                spec.vec <- spec.vec[spec.vec != ""]
                sum(tolower(spec.vec) %in% row.words)
              }))

              match.spec <- max.match
            } else {
              match.spec <- 0
            }

            match.spec

          })

          naturaList_levels[cl.verify][confer.spec > 2] <- "1_det_by_spec"
        }
      }
    }
  } # end of classification

  rowID <- r.occ$rowID

  classified.occ <- cbind(occ[rowID, ], naturaList_levels)

  if(spec.ambiguity == "is.spec"){
    sub <- classified.occ$naturaList_levels == "1_det_by_spec_verify"
    classified.occ$naturaList_levels[sub] <- "1_det_by_spec"
  }

  if(spec.ambiguity == "not.spec"){
    sub <- classified.occ$naturaList_levels == "1_det_by_spec_verify"
    classified.occ$naturaList_levels[sub] <- "2_not_spec_name"
  }

  if(spec.ambiguity == "manual.check"){
    classified.occ <- check.spec(classified.occ, crit.levels, identified.by)
    classified.occ$naturaList_levels <- as.character(classified.occ$naturaList_levels)
  }

  return(classified.occ)
}
