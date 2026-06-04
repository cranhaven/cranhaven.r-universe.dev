#' @title .send_tiger_url
#'
#' @description An internal function for sending http requests to
#'   the US Census Bureau TIGER api via httr::GET() function.
#'
#' @param a_url A required string that defines the url to be sent in the http GET request.
#' @param output_dir A full directory path where the shapefile and its associated files will be downloaded.
#' @param delete_files A logical which if \code{TRUE} will delete the shapefile and associated files in 'output_dir'.
#'   The default is \code{TRUE}.
#' @param set_crs A numeric or character string which if non-NULL calls \code{sf::st_crs()} to set the crs of the geometries.
#' @param transform_crs A numeric or character string which if non-NULL calls \code{sf::st_transform()}
#'   to perform a crs transform of the geometries.
#' @param sf_info A logical which if \code{TRUE} displays info on the resulting simple feature object.
#' @param do_progress A logical which if \code{TRUE} displays a progress bar during the download.
#' @param caller A string that identifies the function making the call.
#'
#' @return A data frame object of class sf, data frame
#'
#' @importFrom downloader download
#' @importFrom utils unzip
#' @importFrom sf st_read
#'
#' @keywords internal
.send_tiger_url <- function(
    a_url = NULL,
    output_dir = NULL,
    delete_files = TRUE,
    set_crs = NULL,
    transform_crs = NULL,
    sf_info = FALSE,
    do_progress = FALSE,
    caller = NULL
) {
  if(is.null(output_dir) | !file.exists(output_dir)){
    stop(paste0("The output directory ", output_dir ," does not exist. Please create this directory."))
  }

  zip_file_name <- base::basename(a_url)
  zip_dest_path <- file.path(output_dir, zip_file_name)

  tryCatch({
    resp <- downloader::download(url = a_url, destfile = zip_dest_path, mode = "wb", quiet = !do_progress)
    if(do_progress){
      cat(paste0("File downloaded successfully: ", resp))
    }
    utils::unzip(zip_dest_path, exdir = output_dir, overwrite = TRUE)
    #shape_file_path <- list.files(path = output_dir, full.names = TRUE, recursive = TRUE, pattern = "\\.shp$")
    #tiger_sf <- sf::st_read(dsn = shape_file_path)

    suppressWarnings(tiger_sf <- sf::st_read(dsn = output_dir, quiet = !sf_info))

    if(!is.null(set_crs)){
      sf::st_crs(tiger_sf) <- set_crs
      tiger_sf <- tiger_sf |>
        sf::st_transform(set_crs)
    }

    if(!is.null(transform_crs)){
      tiger_sf <- tiger_sf |>
        sf::st_transform(transform_crs)
    }

    if(delete_files){
      # Get all files in the directories, recursively
      f <- list.files(output_dir, include.dirs = TRUE, full.names = TRUE, recursive = TRUE)
      # remove the files
      file.remove(f)
    }
    return(tiger_sf)
  }, error = function(err){
    cat("Error downloading file: ", err$message, "\n")
    return(NULL)
  })
}
