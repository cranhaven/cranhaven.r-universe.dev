#' Check legal
#'
#' Checks if this combination of table and release is acceptable.
#'
#' @param table "mean", "stack", "forced_mean", "detection"
#' @param release "dr2", "dr1"
#'
#' @keywords internal
#'
checklegal <- function(table, release) {
  releaselist <- c("dr1", "dr2")

  checkmate::assert_choice(release, releaselist)

  if (release == "dr1") {
    tablelist <- c("mean", "stack")
  } else {
    tablelist <- c("mean", "stack", "forced_mean", "detection")
  }
  checkmate::assert_choice(table, tablelist)
}

#' Get preloaded metadata
#'
#'
#' @param table "mean", "stack", "detection"
#' @param release "dr2", "dr1"
#'
#' @return A data frame
#'
#' @keywords internal
#'
get_metadata <- function(table, release) {
  return(.meta[[paste0(table, "_", release)]])
}

#' Convert types.
#'
#' Helper function.
#'
#'
#' @param dt data.table or data.frame
#' @param cols which cols convert
#' @param types "char", "long", "unsignedByte", "int", "short", "double", "float"
#'
#' @return a data frame with correct data types
#'
#' @keywords internal
#'
#'
convert_types <- function(dt, cols, types) {

  convert_function <- list(
    "char" = as.character,
    "long" = bit64::as.integer64,
    "unsignedByte" = as.integer,
    "int" = as.integer,
    "short" = as.integer,
    "double" = as.double,
    "float" = as.double
  )

  #
  # dt[cols] <- lapply(seq_along(cols), function(i) convert_function[[types[i]]](dt[[i]]))
  dt[cols] <- lapply(seq_along(cols), function(i) {
    f <- convert_function[[types[i]]]

    if (is.null(f)) {
      return(as.character(dt[[i]]))
    }

    f(dt[[i]])
  })


  return(dt)
}


#' Metadata from PS1
#'
#' Return metadata for the specified catalog and table
#'
#' @param table "mean", "stack", "forced_mean" or "detection"
#' @param release "dr1" or "dr2"(default)
#'
#' @return Returns data.frame with columns: name, type, description
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_metadata()
#' }
ps1_metadata <- function(table = "mean", release = "dr2") {

  table <- table[1]
  release <- release[1]

  baseurl <- "https://catalogs.mast.stsci.edu/api/v0.1/panstarrs"

  checklegal(table, release)

  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  resp <- tryCatch(
    httr::RETRY(
      "GET",
      paste(baseurl, release[1], table[1], "metadata", sep = "/"),
      panstarrs_user_agent(),
      times = 3,
      pause_base = 1.3
    ),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )

  if (! inherits(resp, "response")) {
    message(resp)
    return(invisible(NULL))
  }

  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }

  meta_info <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8")
    )
  return(meta_info[, c("name", "type", "description")])
}


#' Do a general search of the PS1 catalog (possibly without ra/dec/radius)
#'
#' @param table "mean", "stack", "detection" or "forced_mean"
#' @param release "dr1" or "dr2"(default)
#' @param columns list of column names to include (NULL means use defaults)
#' @param verbose print info about request
#' @param ... other parameters (e.g., nDetections.min = 2).
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_search(
#' table='detection',
#' release='dr2',
#' objid = '190361393344112894')
#'
#' ps1_search(
#' table='mean',
#' release='dr2',
#' objid = '190361393344112894',
#' columns = c('objName', 'raMean', 'decMean', 'rMeanPSFMag'))
#' }
ps1_search <- function(table = c("mean", "stack", "detection", "forced_mean"),
                       release = c("dr2", "dr1"),
                       columns = NULL,
                       verbose = FALSE,
                       ...) {

  release <- release[1]
  tabel <- table[1]
  checklegal(table, release)


  data <-  list(...)
  checkmate::assert_list(data, min.len = 1L, .var.name = "...")

  baseurl <- "https://catalogs.mast.stsci.edu/api/v0.1/panstarrs"
  url <- paste0(baseurl, "/", release, "/", table, ".json")

  metadata <- get_metadata(table, release)

  if (! is.null(columns)) {

    # Check that column values are legal
    cols_meta <- tolower(metadata$name)
    columns2 <- tolower(columns)
    badcols <- columns[which(!(columns2 %in% cols_meta))]

    if (length(badcols) != 0L)
      message(paste0("Some columns not found in table: ",
                     paste(badcols, collapse = ", ")
             )
      )

    data['columns'] <- jsonlite::toJSON(columns)
  }

  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  resp <- tryCatch(
    httr::RETRY(
      "GET",
      url,
      query = data,
      panstarrs_user_agent(),
      times = 3,
      pause_base = 1.3
    ),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )


  if(verbose)
    print(resp)

  if (! inherits(resp, "response")) {
    message(resp)
    return(invisible(NULL))
  }

  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }

  cont <- jsonlite::fromJSON(
    txt = httr::content(resp, as = 'text', encoding = "UTF-8"),
    simplifyVector = FALSE,
    bigint_as_char = TRUE
  )


  json_colnames <- vapply(cont$info, function(x) x$name, character(1L))


  template <- as.list(rep(NA, length(json_colnames)))
  names(template) <- json_colnames
  cont$data[[1]] <- template


  dt <- data.table::rbindlist(cont$data, use.names = TRUE, fill = TRUE)
  data.table::setnames(dt, json_colnames)

  # convert bigint characters to int64
  types <- metadata$type[json_colnames %in% metadata$name]
  cols <- json_colnames[json_colnames %in% metadata$name]

  dt <- convert_types(dt, cols, types)
  return(dt)
}

#' Do a cone search of the PS1 catalog
#'
#' @param ra (degrees) J2000 Right Ascension
#' @param dec (degrees) J2000 Declination
#' @param r_arcmin (arcmins) Search radius (<= 30 arcmins)
#' @param table "mean"(default), "stack", "detection" or "forced_mean"
#' @param release "dr1" or "dr2"(default)
#' @param columns list of column names to include (NULL means use defaults)
#' @param verbose print info about request
#' @param ... other parameters (e.g., nDetections.min = 2)
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_cone(ra = 139.334,dec = 68.635,r_arcmin = 0.05, nDetections.gt = 1)
#' }
ps1_cone <- function(ra,
                     dec,
                     r_arcmin = 0.05,
                     table = c("mean", "stack", "detection", "forced_mean"),
                     release = c("dr2", "dr1"),
                     columns = NULL,
                     verbose = FALSE,
                     ...) {

  validate_radec(ra, dec, .length = 1L)

  ps1_search(
    table = table[1],
    release = release[1],
    columns = columns,
    verbose = verbose,
    ra = ra,
    dec = dec,
    radius = r_arcmin / 60.0
  )
}



#' Do a cross-match with PS1 catalog
#'
#' @param ra (degrees) numeric vector of J2000 Right Ascension
#' @param dec (degrees) numeric vector of J2000 Declination
#' @param r_arcmin (arcmins) Search radius (<= 30 arcmins)
#' @param table "mean"(default), "stack", "detection", "forced_mean"
#' @param release "dr1" or "dr2"(default)
#' @param verbose print info about request
#'
#' @return data.frame
#' @export
#'
#'
#' @examples
#' \dontrun{
#' ps1_crossmatch(ra = c(268.70342, 168.87258), dec = c(71.54292, 60.75153))
#' }
ps1_crossmatch <- function(ra,
                           dec,
                           r_arcmin = 0.05,
                           table = c("mean", "stack", "detection", "forced_mean"),
                           release = c("dr2", "dr1"),
                           verbose = FALSE) {

  base_url <- 'https://catalogs.mast.stsci.edu/api/v0.1/panstarrs'

  table <- table[1]
  release <- release[1]

  checklegal(table, release)
  validate_radec(ra, dec)
  url <- paste0(base_url, "/", release, "/", table, "/crossmatch/")

  # API restriction
  checkmate::assert_vector(ra, max.len = 5000L, min.len = 1L)

  sources_json <- jsonlite::toJSON(data.frame(ra = ra, dec = dec))

  data <- list(
    targets = sources_json,
    resolve = FALSE,
    radius = r_arcmin/60,
    ra_name = "ra",
    dec_name = "dec"
  )

  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }



  resp <- tryCatch(
    httr::RETRY(
      "POST",
      url,
      query = data,
      panstarrs_user_agent(),
      times = 3,
      pause_base = 1.3
    ),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )

  if(verbose)
    print(resp)

  if (! inherits(resp, "response")) {
    message(resp)
    return(invisible(NULL))
  }

  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }

  cont <- jsonlite::fromJSON(
    httr::content(resp, as = 'text', encoding = "UTF-8"),
    bigint_as_char = TRUE
  )

  metadata <- get_metadata(table, release)

  dt <- data.table::as.data.table(cont$data)

  ii <- which(colnames(dt) %in% metadata$name)
  cols <- colnames(dt)[ii]
  types <- metadata$type[which(metadata$name %in% cols)]
  dt <- convert_types(dt, cols, types)
  data.table::setorder(dt, "_searchID_")

  return(dt)
}

#' Get the RA and Dec for objects from PanSTARRS catalog.
#'
#' Only works for "north" objects with decl > -30. For all objects see function `ps1_mast_resolve`.
#'
#' @param target_names character vector of target names (see example)
#' @param verbose print info about request
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_resolve(c('Andromeda', "SN 2005D", 'Antennae', 'ANTENNAE'))
#' }
ps1_resolve <- function(target_names, verbose = FALSE) {

  table <- "mean"
  release <- "dr2"
  base_url <- 'https://catalogs.mast.stsci.edu/api/v0.1/panstarrs'

  checklegal(table, release)
  url <- paste0(base_url, "/", release, "/", table, "/crossmatch/")


  # API restriction
  checkmate::assert_vector(target_names, max.len = 5000L, min.len = 1L)

  # Create json list
  sources_df <- data.frame(
    target = target_names,
    search_id = 0:(length(target_names)-1)
  )

  sources_json <- jsonlite::toJSON(subset(sources_df, select = "target"))

  data <- list(
    resolve = TRUE,
    target_name = "target",
    targets = sources_json
  )

  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  resp <- tryCatch(
    httr::RETRY(
      "POST",
      url,
      query = data,
      panstarrs_user_agent(),
      times = 3,
      pause_base = 1.3
    ),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )

  if(verbose)
    print(resp)

  if (! inherits(resp, "response")) {
    message(resp)
    return(invisible(NULL))
  }

  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }

  cont <- jsonlite::fromJSON(
    httr::content(resp, as = 'text', encoding = "UTF-8"),
    bigint_as_char = TRUE
  )

  dt <- data.table::as.data.table(cont$data)
  data.table::setnames(dt, c("_ra_", "_dec_", "_searchID_"), c("ra", "dec", "search_id"))
  dt <- data.table::merge.data.table(dt, sources_df, by = 'search_id')

  return(dt)
}


#' Perform a MAST query.
#'
#' @param request The MAST request json object
#'
#' @return Returns response
#'
#' @keywords internal
#'
ps1_mast_query <- function(request){

  requestString <- jsonlite::toJSON(request, auto_unbox = TRUE)

  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }


  resp <- tryCatch(
    httr::RETRY(
      "GET",
      url = "https://mast.stsci.edu/api/v0/invoke",
      query = list(request = requestString),
      encode = "form",
      panstarrs_user_agent(),
      times = 3,
      pause_base = 1.3
    ),
    error = function(e) conditionMessage(e),
    warning = function(w) conditionMessage(w)
  )

  return(resp)
}

#' Get the RA and Dec for an object using the MAST name resolver
#'
#' @param name Name of object
#'
#' @return list of ra, decl
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_mast_resolve('Acrux')
#' }
ps1_mast_resolve <- function(name) {
  resolverRequest <- list(
    "service" = "Mast.Name.Lookup",
    "params" = list(
      "input" = name[1],
      "format" = "json"
    )
  )

  resp <- ps1_mast_query(resolverRequest)

  if (! inherits(resp, "response")) {
    message(resp)
    return(invisible(NULL))
  }

  # Then stop if status > 400
  if (httr::http_error(resp)) {
    httr::message_for_status(resp)
    return(invisible(NULL))
  }

  resolvedObject <- httr::content(resp)

  tryCatch(
    resolvedObject$resolvedCoordinate[[1]][c("ra", "decl")],
    # error = function(e) conditionMessage(e)
    error =  function(e) message(paste0("Object '", name, "' not found"))
  )

}



