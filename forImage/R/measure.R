#' Foraminifera image measurement
#'
#' @description
#' This function measures dimensions in photomicrographs.
#'
#' @usage measure(file, scale = NULL, ref_scale = NULL,
#' unit = "um", pco = FALSE, save = FALSE, path = NULL)
#' @param file image file with or without metadata.
#' @param scale (optional) image physical pixel size (metric / pixel).
#' @param ref_scale (optional) reference scale available on image (in micrometers or millimeters). This scale should be inserted under the main object.
#' @param unit scale unit. Needs to agree with reference scale or metric per pixel. (Default "um")
#' @param pco (optional) will assess proportion of cell occupancy inside the shell. Outlined proportion. This argument is still being tested and should be used with caution.
#' @param save If TRUE save the result image in a specified folder (see \code{path}) or the working directory, if FALSE it does not save the image with measures. (Default FALSE)
#' @param path (optional) path to save result file.
#'
#'
#' @return A `data.frame` containing the file name, surface area and major and minor axis. The unit is dependent on the pixel per metric scale. It also returns a PNG file with the measured dimensions if \code{save = TRUE}.
#'
#' @author Thaise R. Freitas \email{thaisericardo.freitas@@gmail.com}
#'
#' @seealso \code{\link{bio.volume}}, \code{\link{volume.total}}
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' #Path to example file from package
#' img <- system.file("extdata", "foram.tif", package="forImage")
#'
#' #measure individual dimension
#' measure(img)
#' }
#'

measure <- function(file, scale = NULL, ref_scale = NULL,
                    unit = "um", pco = FALSE, save = FALSE, path = NULL) {

  ##set python path and initiate modules

  python_path <- system.file("python", package = "forImage")

  utilities <- reticulate::import_from_path("cv_utilities", path = python_path)
  measure_dim <- reticulate::import_from_path("measure_dim", path = python_path)

  os <- reticulate::import("os")
  utils <- utilities$Utilities()
  cv <- measure_dim$ComputerVision()


  ## Pixel per metric - manual

  if(!missing(scale)) {
    scale <- scale
  }

  ## Pixel per metric through scale in image

  if(!missing(ref_scale)) {
    ref_scale <- ref_scale
  }


  ## Pixel per metric through meta_file - Just for Axiovision for now

  if(missing(scale) && missing(ref_scale)){

    ppm <- reticulate::import_from_path("ppm", path = python_path)

    px <- ppm$Pixels()

    xml_file <- os$path$splitext(file)[[1]]
    xml_file <- paste(xml_file, "tif_meta.xml", sep = ".", collapse = "")

    scale <- px$get_pixels_axio(xml_file = xml_file)

  }

  ## Write image result file

  ## Define different path to save the image
  if(!missing(path)) {
    if(!endsWith(path, '/'))
      path <- paste0(path, "/")
    else {
      path <- path
    }
  }


  dim <- cv$measure_object_dimension(file, scale = scale, reference_scale = ref_scale, unit = unit, save = save, path = path)

  if(isTRUE(pco)){
    protoplasm <- reticulate::import_from_path("protoplasm", path = python_path)

    prot <- protoplasm$Protoplasm()

    sc <- dim$scale

    p <- prot$measure(file = file, scale = sc)

    df <- dplyr::bind_cols(dim, p)
  } else {

    df <- data.frame(dim)

  }


  return(df)

}

