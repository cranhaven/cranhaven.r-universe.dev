#' Internal function of naturaList - reduce data.frame of occurrence for a minimal
#' column length
#'
#'
#' Reduce columns of occurrence data.frame required by
#' {\link[naturaList]{classify_occ}} to facilitate internal operation
#'
#' @param df occurrence data frame provided in {\link[naturaList]{classify_occ}}
#' @param institution.code institution.code = "institutionCode"
#' @param collection.code collection.code = "collectionCode"
#' @param catalog.number catalog.number = "catalogNumber"
#' @param year year = "year",
#' @param date.identified date.identified = "dateIdentified"
#' @param species species = "species"
#' @param identified.by identified.by = "identifiedBy"
#' @param decimal.longitude decimal.longitude = "decimalLongitude"
#' @param decimal.latitude decimal.latitude = "decimalLatitude"
#' @param basis.of.record basis.of.record = "basisOfRecord"
#' @param media.type media.type = "mediaType"
#' @param occurrence.id occ.id = "occurrenceID"
#' @param na.rm.coords na.rm.coords = TRUE
#'
#' @return a data frame with only the columns required for the \code{naturaList}
#'   package
#' @seealso {\link[naturaList]{classify_occ}}
#'
#' @keywords internal

reduce.df <- function(df,
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
                      na.rm.coords = TRUE) {


  institution.code <- as.character(df[, institution.code])
  collection.code <- as.character(df[, collection.code])
  catalog.number <- as.character(df[, catalog.number])
  year <- df[, year]
  date.identified <- df[, date.identified]
  species <- as.character(df[, species])
  identified.by <- as.character(df[, identified.by])
  decimal.longitude <- df[, decimal.longitude]
  decimal.latitude <- df[, decimal.latitude]
  basis.of.record <- df[, basis.of.record]
  media.type <- df[, media.type]
  occurrence.id <- df[, occurrence.id]
  rowID <- rownames(df)


  data <- data.frame(
    rowID, occurrence.id, species, decimal.longitude, decimal.latitude,
    year, identified.by, date.identified,
    institution.code, collection.code,
    catalog.number, media.type, basis.of.record,
    stringsAsFactors = FALSE
  )
  if (na.rm.coords) {
    ll.na <- is.na(data$decimal.longitude)
    data <- data[!ll.na, ]
    lat.na <- is.na(data$decimal.latitude)
    data <- data[!lat.na, ]
  }


  data
}

#' Internal function of naturaList - Return abbreviation collapsed
#'
#' Return collapsed abbreviation for a specific line of specialist data frame.
#' It is used as pattern in grep function inside
#' {\link[naturaList]{classify_occ}}
#'
#' @param df spec data frame provided in {\link[naturaList]{classify_occ}}
#' @param line specifies the line of the data frame to be collapsed
#'
#' @return a list with two elements in regex format:
#'   \item{[[1]]}{the abbreviation of the first name;}
#'   \item{[[2]]}{regex pattern with all names and abbreviations.}
#' @seealso {\link[naturaList]{classify_occ}}
#' @keywords internal

abrev.pttn <- function(df, line) {
  abv.num <- grep("Abbrev", colnames(df))

  abv1 <- abv.num[1]
  first.L <- paste0("\\<", df[line, abv1])

  nonblank <- df[line, -1] != ""
  nonblank <- c(FALSE, nonblank)

  pttn <- paste(unlist(df[line, nonblank]), collapse = "|")

  if (length(df[line, nonblank]) < 2) pttn <- df[line, abv1]


  res <- list(first.L, pttn)
  return(res)
}

#' Internal function of naturaList - Detect if a string has a specialist name
#'
#' Detect if a string with identifiers name has a specialist name. It is used inside
#'  {\link[naturaList]{classify_occ}}
#'
#' @param sp.df reduced version of occurrence data frame provided
#' in {\link[naturaList]{classify_occ}}
#' @param i row number of specialist data frame
#' @param specialist specialist data
#'
#' @return integer with the row numbers of the \code{sp.df} data frame which was
#'  identified by the specialist name in row \code{i}.
#'
#' @keywords internal

func.det.by.esp <- function(sp.df, i, specialist) {
  padr.det <- abrev.pttn(specialist, i)

  g.det <- unique(grep(paste(specialist[i, 1]),
    ignore.case = T,
    sp.df$identified.by
  ))

  g.det.1 <- unique(grep(padr.det[1],
    ignore.case = F,
    sp.df$identified.by[g.det]
  ))

  g.det.ok <- unique(grep(padr.det[2],
    ignore.case = F,
    sp.df$identified.by[g.det[g.det.1]]
  ))

  g.det[g.det.1[g.det.ok]]
}

#' Internal function of naturaList - Return specialists names in a collapsed
#' string
#'
#' Return specialists names in a collapsed string to be used in the internal
#' function {\link[naturaList]{specialist.conference}}
#'
#' @param specialist specialist data frame
#'
#' @return character. A regex pattern for the specialist full name
#'
#' @keywords internal

pttn.all.specialist <- function(specialist) {
  pttn <- character(nrow(specialist))
  for (i in 1:nrow(specialist)) {
    nonblank <- specialist[i, ] != ""
    pttn[i] <- paste(specialist[i, nonblank], collapse = "|")
  }

  pttn.all <- paste(pttn, collapse = "|")
  pttn.all
}


#' Internal function of naturaList - Verify if a string has unambiguous specialist
#' name
#'
#' Based on pattern generated by {\link[naturaList]{pttn.all.specialist}} it
#' verifies if a string has unambiguous specialist name. It is used in internal
#' function {\link[naturaList]{specialist.conference}}
#'
#' @param pattern a pattern from {\link[naturaList]{pttn.all.specialist}} function
#' @param string string with the name of who identified the specimen
#'
#' @return character. \code{""} or \code{"_verify"}.
#'
#' @keywords internal

verify.specialist <- function(pattern, string) {
  collection.new <- gsub(pattern, "", string, ignore.case = T)

  g_zero <- stringr::str_replace_all(collection.new,
    pattern = "\\(.+\\)|[0-9]|[[:punct:]]",
    ""
  )

  zero <- nchar(stringr::str_replace_all(g_zero, pattern = "\\s+", "")) == 0

  if (zero == T) {
    return("")
  }
  if (zero == F) {
    return("_verify")
  }
}

#' Internal function of naturaList - Confirm if an occurrence record was identified by
#' a specialist without ambiguity
#'
#'  Confirm if an occurrence record was identified by
#' a specialist without ambiguity. It is used inside
#'  {\link[naturaList]{classify_occ}}
#'
#' @param pt.df a line of the reduced version of the occurrence data frame
#' @param specialist specialist data frame
#'
#' @return character with naturaList level code \code{"1_det_by_spec"} or
#'   \code{"1_det_by_spec_verify"}
#'
#' @keywords internal

specialist.conference <- function(pt.df, specialist) {
  spe.obs <- which(lapply(
    specialist[, 1],
    function(x) grep(x, pt.df["identified.by"], ignore.case = T)
  ) == 1)

  pttn.all <- pttn.all.specialist(specialist[spe.obs, ])
  verify <- verify.specialist(pttn.all, pt.df["identified.by"])

  crt <- paste0("1_det_by_spec", verify)
  crt
}


#' Internal function of naturaList - Identifies if a occurrence has a name for
#' the identifier of the specimen
#'
#' Identifies if a occurrence has a name for the identifier of the specimen.
#'  It is used inside {\link[naturaList]{classify_occ}}
#'
#' @param sp.df reduced version of occurrence data frame provided in
#' {\link[naturaList]{classify_occ}}
#' @param ignore.det.names ignore.det.names character vector indicating
#'  strings in the identified.by column that should be ignored as a
#'  taxonomist. See {\link[naturaList]{classify_occ}}.
#'
#'  @return an integer vector indicating the rows which have 'identified by' ID
#'
#' @keywords internal

has.det.ID <- function(sp.df, ignore.det.names = NULL) {
  if (is.null(ignore.det.names)) {
    sem.det <- paste(c(
      "^NA$",
      "RRC ID Flag",
      "^NA $",
      "^ $",
      "^$",
      "^-$",
      "^_$"
    ), collapse = "|")
  }

  if (!is.null(ignore.det.names)) {
    sem.det <- paste(c(
      "^NA$",
      "RRC ID Flag",
      "^NA $",
      "^ $",
      "^$",
      "^-$",
      "^_$",
      ignore.det.names
    ), collapse = "|")
  }

  g.sem.det <- grep(sem.det, sp.df$identified.by)
  sem.det.ID <- c(which(is.na(sp.df$identified.by)), g.sem.det)

  ID <- !seq_along(sp.df$identified.by) %in% sem.det.ID

  which(ID)
}

#' Internal function of naturaList - Manual check of ambiguity in specialist's
#' name
#'
#' Creates interaction with user in which the user check if a string with the
#' identifier of a specimen has a specialist name. It solves ambiguity in classify
#' an occurrence as identified by a specialist. It is used inside
#' {\link[naturaList]{classify_occ}}
#'
#' @param class.occ internal data frame with observation classified according
#' {\link[naturaList]{classify_occ}} criteria
#' @param crit.levels crit.levels choose by user in {\link[naturaList]{classify_occ}}
#' @param identified.by same as identified.by argument in {\link[naturaList]{classify_occ}}
#'
#' @return a character vector with 'naturaList_levels" ID.
#' @keywords internal

check.spec <- function(class.occ, crit.levels, identified.by) {
  sub <- class.occ$naturaList_levels == "1_det_by_spec_verify"
  spec.unique <- unique(class.occ[sub, identified.by])

  if (any(sub)) {
    cat(paste("There is", length(spec.unique), "specialists' names to be checked"))

    answer <- vector("character", length(spec.unique))

    for (i in 1:length(spec.unique)) {
      ask <- paste(spec.unique[i], "\n", "Is this a specialist? (y/n)")
      answer[i] <- readline(ask)
      if (!any(answer[i] %in% c("y", "n"))) stop("Answer only 'y' or 'n'")
    }

    c.spec <- ifelse(answer == "y", 1, 2)
    levels_checked <- paste0(c.spec, "_", crit.levels[c.spec])

    for (i in 1:length(spec.unique)) {
      g.spec <- grep(spec.unique[i], class.occ[, identified.by])
      for (j in 1:length(g.spec)) {
        class.occ[g.spec[j], "naturaList_levels"] <- levels_checked[i]
      }
    }
  }
  class.occ
}

#' Internal function of naturaList - Remove duplicate occurrence
#'
#' Remove duplicated occurrence based on coordinates. It is used in
#' {\link[naturaList]{grid_filter}}
#'
#' @param x  data frame with filtered occurrences
#' @param decimal.latitude name of column with decimal.latitude
#' @param decimal.longitude name of column with decimal.longitude
#'
#' @return data frame with occurrence records
#'
#' @keywords internal
#'
rm.coord.dup <- function(x, decimal.latitude, decimal.longitude) {
  unique.row <- !duplicated(x[, c(decimal.latitude, decimal.longitude)])
  res <- x[unique.row, ]
  row.names(res) <- 1:nrow(res)

  res
}


#' Internal function of naturaList - Get coordinates from polygons created in leaflet
#' map
#'
#' Get coordinates from polygons created in leaflet map. It is used in
#'  {\link{map_module}}
#'
#' @param input.polig an interactive polygon from leaflet map.
#' \code{input$map_draw_all_features$features[[i]]}
#'
#' @return a data frame with the coordinates
#'
#' @keywords internal
#'
pol.coords <- function(input.polig) {
  pol.coords <- data.frame(x = numeric(), y = numeric())
  total <- length(input.polig$geometry$coordinates[[1]])
  long.pol <- numeric()
  lat.pol <- numeric()
  for (i in 1:total) {
    long.pol <- input.polig$geometry$coordinates[[1]][[i]][[1]]
    lat.pol <- input.polig$geometry$coordinates[[1]][[i]][[2]]


    coords <- data.frame(x = long.pol, y = lat.pol)

    pol.coords <- rbind(pol.coords, coords)
  }

  pol.coords
}


#' Internal function of naturaList - Create SpatialPolygons from a list of coordinates
#'
#' Create SpatialPolygons from a list of coordinates. It is used in {\link{map_module}}
#'
#' @param df a data frame provided by {\link{pol.coords}}
#'
#' @return  a \code{SpatialPolygon} object
#'
#' @keywords internal
#'
make.polygon <- function(df) {

  # and then something like this
  sp <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(df)), 1)))
  sp
}
