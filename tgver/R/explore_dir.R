#' A function to read and serve at least one file from a directory.
#'
#' Current version only tries to:
#' Find two files, one .csv and the other .geojson and
#' pass them to TGVE as `defaultURL` and `geographyURL` respectively.
#' It will also look at their column names and try to find matching columns.
#' If there is one file, it passes it to `explore_file` function.
#'
#' @param path character of a data directory.
#' @param background logical value whether to run instance in `callr`.
#' @return no value returned, depending on `background` either a or not
#' blocking `plumber::pr` instance is started. A message is displayed with
#' instance details.
#'
#' @export
#' @examples {
#' p = file.path(tempdir(), "data")
#' dir.create(p)
#' gURL = paste0("https://raw.githubusercontent.com/saferactive/",
#' "tgve/main/pf-only-name.geojson")
#' dURL = "https://raw.githubusercontent.com/saferactive/tgve/main/ksi-pf.csv"
#' download.file(gURL, destfile = file.path(p, "pf.geojson"))
#' download.file(dURL, destfile = file.path(p, "data.csv"))
#'
#' ps = explore_dir(p, background = TRUE)
#' ps$kill()
#' unlink(p, recursive = TRUE)
#' }
explore_dir = function(path, background = FALSE) {
  dir_check(path)

  # how many files in?
  files = list.files(path, full.names = TRUE)
  if(length(files) == 0)
    stop("directory is empty.")

  if(length(files) == 1L) {
    message("One file found in: ", path)
    message("Trying tgver::explore_file")
    explore_file(files[1])
  } else {
    if(length(files) == 2) {
      # is there a geography file and a data file
      # for now a geojson & a csv file?
      geo.file = list.files(path, pattern = "geojson", full.names = TRUE)
      dat.file = list.files(path, pattern = "csv", full.names = TRUE)
      if(length(geo.file) == 1L && length(dat.file) == 1) {
        explore_data_and_geography(dat.file, geo.file, background)
      } else {
        stop("explore_dir meeds at least one csv and one geojson file.")
      }
    } else {
      # TODO: select two files and run above
      # for now fail
      stop("there are more than two files in the given directory")
    }
  }
}

explore_data_and_geography = function(dat.file, geo.file, background) {
  for(x in c(dat.file, geo.file)) {
    stopifnotonecharacter(x)
    stopifnotvalidfile(x)
  }

  if(!is.logical(background))
    stop("background value must be logical")

  # find matching column to pass to the TGVE
  data.csv = paste(readLines(dat.file), collapse = "\n")
  geo.json = paste(readLines(geo.file), collapse = "")

  dat.cols = names(geojsonsf::geojson_sf(geo.json))
  geo.cols = names(utils::read.csv(text = data.csv))
  comm.col = intersect(geo.cols, dat.cols)

  if(length(comm.col) == 0) {
    # no matching columns?
    # message("Found two files: ", geo.file, ", and ", dat.file)
    stop("no matching columns found between the csv and geojson
                 files in the given directory")
  } else {
    # more than one matching oclumns
    message("More than one matching columns found between the two files
                    using the first matchin column as `geographyColumn` value.")
    comm.col = comm.col[1]
  }

  # TODO: can the rest of this be abstracted out in one function with
  # explore_geojson

  base.url = "http://127.0.0.1:8000"
  geoURL = paste0(base.url, "/geoURL")
  datURL = paste0(base.url, "/data.csv")

  path = tempInstance()
  server = tgve_server(path = path, run = FALSE)

  # flexible variable names
  server$handle("GET", "/geoURL", function(res){
    res$headers$`Content-type` = "application/json"
    res$body = geo.json # not from disk again
    res
  })

  server$handle("GET", "/data.csv", function(res){
    res$headers$`Content-type` = "text/csv"
    res$body = data.csv
    res
  })

  cat(datURL, geoURL, comm.col)

  nav.url = get_url(base.url,
        defaultURL = datURL,
        geographyURL = geoURL,
        geographyColumn = comm.col)

  if(background) {
    return(background_run(server))
  }

  message("Serving data from ", datURL)
  message("Serving geography from: ", geoURL)
  message("Browsing ", nav.url)
  openURL(nav.url)
  server$run(port = 8000, docs = FALSE)
}
