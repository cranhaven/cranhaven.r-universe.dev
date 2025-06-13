#' Biovolume calculus
#'
#' @description
#' This function calculates Foraminifera biovolume, through geometric approximation.
#' To compute others organisms cell volume use \code{\link{volume.total}} function
#'
#' @param data a numeric vector or data frame with size data.
#' Size data parameters by model see \code{\link{volume}} details.
#' @param genus (optional) character informing foraminifera genus to calculate individual biovolume.
#' See all genera available in \code{\link{data_pco}}
#' @param pco (optional) vector informing percent of cell occupancy in the test.
#' Default value set for specific genus in \code{\link{data_pco}}.
#' @param model (optional if genus unknown) character informing geometric model to calculate volume.
#' See all models available in \code{\link{volume.total}}
#'
#' @return A `data.frame` or numeric object, consisting of calculated individual volume (if not available), biovolume and model (if \code{genus} is informed).
#'
#' @details
#' The function calculates the biovolume of different individuals from the available genera.
#'
#' @author
#' Thaise R. Freitas \email{thaisericardo.freitas@@gmail.com}
#'
#' @seealso \code{\link{volume.total}}
#' @seealso \code{\link{biomass}}
#' @seealso \code{\link{measure}}
#' @importFrom magrittr %>%
#' @examples
#' # Calculate biovolume for different genera
#' #Ammonia size data
#' data("ammonia")
#'
#' bio.volume(ammonia, genus= "ammonia")
#'
#' # Calculate biovolume for unknown genus
#' df <- data.frame(h = 10, d_one = 10,
#' d_two = 10, area = 10, width = 10, length = 10)
#' bio.volume(df, model = "10hl", pco = 0.76)
#'
#' @export

bio.volume <- function(data, pco = 0.76, genus = NULL, model = NULL){

  x <- data.frame(data)


  if ("genus" %in% colnames(x)) {
    genus <- x$genus
  }

  if ("model" %in% colnames(x) && missing(genus)) {
    model <- x$model
  }

  if (missing(genus) && missing(model)) {
    stop("Please inform genus or model to be applied.")

  }

  if (missing(model) && !missing(genus)){

    d_pco <- forImage::data_pco

    if (any(genus == d_pco)) {

      x$model <- (d_pco[match(genus, d_pco$genera), ]$model)
    }

  }


  if ("pco" %in% colnames(x) && !missing(genus)) {
    pco <- x$pco

  }

  if ("pco" %in% colnames(x) && missing(genus)) {
    pco <- x$pco

  }

  if (!("pco" %in% colnames(x)) && !missing(genus)) {

    d_pco <- forImage::data_pco

    if (any(genus == d_pco)) {

      pco <- (d_pco[match(genus, d_pco$genera), ]$mean)/100

    }
  }



  v <- forImage::volume.total(x, model = model)$vol
  bv <- v * pco
  result <- x %>%
    tibble::as_tibble(.) %>%
    dplyr::rowwise(.) %>%
    tibble::add_column(vol = v, biovol = bv)
  result

  return(result)
}






