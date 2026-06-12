#' Get list of images
#'
#' Query ps1filenames.py service to get a list of images.
#'
#' src: https://ps1images.stsci.edu/ps1image.html
#'
#' @param ra ra position in degrees
#' @param dec dec position in degrees
#' @param size image size in pixels (0.25 arcsec/pixel)
#' @param filters string with filters to include
#'
#' @return table with the results
#' @export
#'
#' @examples
#' \dontrun{
#' # Crab nebulae image
#' ps1_image_list(ra = 83.633210, dec = 22.014460, size = 1280, filters = "grz")
#' }
#'
ps1_image_list <- function(ra, dec, size = 240, filters = "grizy") {

  validate_radec(ra, dec, .length = 1L)

  service <- "https://ps1images.stsci.edu/cgi-bin/ps1filenames.py"

  url <- paste0(
    service,
    "?ra=", ra,
    "&dec=", dec,
    "&size=", size,
    "&format=fits",
    "&filters=", filters
    )

  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  resp <- tryCatch(
    httr::RETRY(
      "GET",
      url,
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

  df <- data.table::fread(httr::content(resp, as = 'text'), sep = " ")

  return(df)
}


#' Get URL of images
#'
#' @param ra ra position in degrees
#' @param dec dec position in degrees
#' @param size extracted image size in pixels (0.25 arcsec/pixel)
#' @param output_size output (display) image size in pixels (default = size).
#' output_size has no effect for fits format images.
#' @param filters string with filters to include
#' @param format data format (options are "jpg", "png" or "fits")
#' @param color if TRUE, creates a color image (only for jpg or png format). Default is return a list of URLs for single-filter grayscale images.
#'
#' @return string with the URL
#' @export
#'
#'
#' @examples
#' \dontrun{
#' ps1_image_url(
#' ra = 83.633210,
#' dec = 22.014460,
#' size = 1280,
#' format = "jpg",
#' filters = "grz",
#' color = T)
#' }
#'
ps1_image_url <- function(ra, dec, size = 240, output_size = NULL, filters = "grizy", format = "jpg", color = FALSE) {

  validate_radec(ra, dec, .length = 1L)

  checkmate::assert_choice(format, c("jpg", "png", "fits"))

  if (color && format == "fits")
    stop("color images are available only for jpg or png formats")

  table <- ps1_image_list(ra, dec, size = size, filters = filters)
  url <- paste0(
    "https://ps1images.stsci.edu/cgi-bin/fitscut.cgi?",
    "ra=", ra,
    "&dec=", dec,
    "&size=", size,
    "&format=", format
  )

  if (!is.null(output_size)) url <- paste0(url, "&output_size=", output_size)

  table$filter <- factor(table$filter, levels = c("y", "z", "i", "r", "g"))
  data.table::setorder(table, "filter")

  if (color) {
    if (nrow(table) > 3) table <- table[1:3, ]

    color_labels <- c("red", "green", "blue")
    url_color_part <- paste(
      paste0("&", color_labels, "=", table$filename),
      collapse = ""
    )
    url <- paste0(url, url_color_part)

  } else {
    url <- paste0(url, "&red=", table$filename)
  }
  return(url)
}


#' Get grayscale image at a sky position
#'
#'
#' @param ra ra position in degrees
#' @param dec dec position in degrees
#' @param size extracted image size in pixels (0.25 arcsec/pixel)
#' @param output_size output (display) image size in pixels (default = size).
#' output_size has no effect for fits format images.
#' @param filter string with filter to extract (one of grizy)
#' @param format data format (options are "jpg", "png")
#'
#' @return the image
#' @export
#'
#'
#' @examples
#' \dontrun{
#' ps1_image_gray(ra = 83.633210, dec = 22.014460, size = 1280, filter = "i")
#' }
ps1_image_gray <- function(ra, dec, size = 240, output_size = NULL, filter = "g", format = "jpg") {
  checkmate::assert_choice(format, c("jpg", "png"))
  checkmate::assert_choice(filter, c("g", "r", "i", "z", "y"))

  url <- ps1_image_url(
    ra,
    dec,
    size = size,
    filters = filter,
    output_size = output_size,
    format = format
  )

  return(url[1])
}

#' Get color image at a sky position
#'
#' @param ra ra position in degrees
#' @param dec dec position in degrees
#' @param size extracted image size in pixels (0.25 arcsec/pixel)
#' @param output_size output (display) image size in pixels (default = size).
#' output_size has no effect for fits format images.
#' @param filters string with filters to include
#' @param format data format (options are "jpg", "png")
#'
#' @return the image url
#' @export
#'
#'
#' @examples
#' \dontrun{
#' ps1_image_color(ra = 83.633210, dec = 22.014460, size = 1280, filters="grz")
#' }
ps1_image_color <- function(ra, dec, size = 240, output_size = NULL, filters = "grizy", format = "jpg") {
  checkmate::assert_choice(format, c("jpg", "png"))

  url <- ps1_image_url(ra, dec,
    size = size,
    filters = filters,
    output_size = output_size,
    format = format,
    color = TRUE
  )
  return(url)
}
