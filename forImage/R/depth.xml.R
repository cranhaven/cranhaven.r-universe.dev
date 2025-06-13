#' Import image object depth (Z)
#'
#' The function retrieves the object depth information from image metadata as source.
#'
#' @param x file containing metadata, if xml type: \code{'.xml', '.tif_meta.xml'}
#' @param ... other arguments.
#'
#' @return A `data.frame` with the following information:
#' \itemize{
#'   \item {file} : {filename}
#'   \item {z_depth} : {measured focus range depth (z)}}
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' #Path to example file from package
#' meta <- system.file("extdata", "foram.tif_meta.xml", package="forImage")
#'
#' #retrieve z-depth data
#' depth.xml(meta)}
#'
#' @export depth.xml
#' @rdname depth
depth.xml <- function(x, ...) {

  xml_file <- x

  if (is.null(xml_file)) {
    stop("Object not specified.")
  }

  filename <- basename(sub('\\.tif_meta.xml$', '', xml_file))

  data <- xml2::read_xml(xml_file)

  # check data
  f <- data %>%
      xml2::xml_find_all("//Tags/I58") %>%
      xml2::xml_text() %>%
      as.integer()

  g <- data %>%
      xml2::xml_find_all("//Tags/I59") %>%
      xml2::xml_text() %>%
      as.integer()

  if(f[1] == 16777608 && g[1] == 16777611) {
      # First
      one <- data %>%
          xml2::xml_find_all("//Tags/V58") %>%
          xml2::xml_text() %>%
          as.integer()

      #Second
      two <- data %>%
          xml2::xml_find_all("//Tags/V59") %>%
          xml2::xml_text() %>%
          as.integer()


  } else {
      # First
      one <- data %>%
          xml2::xml_find_all("//Tags/V59") %>%
          xml2::xml_text() %>%
          as.integer()

      #Second
      two <- data %>%
          xml2::xml_find_all("//Tags/V60") %>%
          xml2::xml_text() %>%
          as.integer()
  }


  z_depth <- abs(one[1] - two[1])
  result <- data.frame(filename, z_depth)


  return(result)


}


