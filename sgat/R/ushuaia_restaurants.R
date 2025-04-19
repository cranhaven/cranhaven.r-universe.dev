#' Retrieves a vector of Ushuaian's restaurants and bar's names and adresses, ready for unambiguous Google searches.
#'
#' @return Vector of restaurants and bars in Ushuaia city, Tierra del Fuego, Argentina
#' @export
#'
#' @examples
#' restaurants <- ushuaia_restaurants()
#' head(restaurants)
ushuaia_restaurants <- function() {
  address <- "https://www.interpatagonia.com/ushuaia/comidas.html"
  txt <- RCurl::getURL(address)
  entre <- qdapRegex::ex_between(txt, '<span itemprop="name"> ', '</span> </p> <p class="')[[1]]
  nombre.lugar <- sub("</span>.*", "", entre)
  nombre.lugar <- nombre.lugar[2:length(nombre.lugar)]
  nombre.lugar <- trimws(nombre.lugar) # saca los espacios de adelante y atras
  nombre.lugar <- gsub("[[:punct:] ]+", " ", nombre.lugar)
  direccion.lugar <- sub(".*streetAddress\">", "", entre)
  direccion.lugar <- direccion.lugar[2:length(direccion.lugar)]
  direccion.lugar <- sub("-.*", "", direccion.lugar)
  direccion.lugar <- trimws(direccion.lugar)
  direccion.lugar <- gsub("[[:punct:] ]+", " ", direccion.lugar)
  nombre.lugar <- nombre.lugar[which(!duplicated(direccion.lugar))]
  direccion.lugar <- unique(direccion.lugar)
  lugares <- paste(nombre.lugar, direccion.lugar, "Ushuaia", sep = ", ")
}
