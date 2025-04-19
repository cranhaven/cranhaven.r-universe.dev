#' Opens firefox, searches in Google lugar.a.buscar place on the dia.semana weekday. Then it retrieves the "Popular Times" and geolocation information. If the search finds said information, the function returns a data.frame, if not, NULL. If you are looking the "Popular Times" information for the whole week use \code{\link{sgat}} instead.
#'
#' @param lugar.a.buscar What you want to search in Google
#' @param dia.semana Day of the week from when you want to retrieve the "Popular Times" information
#' @param tiempo.espera Time measure of how much time you think it's sufficient given your internet connection to load a Google search page and not finding the information,
#'
#' @return dataframe with retrieved information for single weekday
#' @export
#'
#' @examples
#' \dontrun{
#' museo.miercoles <- sgat_day("museo nacional de bellas artes, buenos aires, argentina", "miercoles")
#' head(museo.miercoles)
#' }
#'
sgat_day <- function(lugar.a.buscar, dia.semana, tiempo.espera = 10) {
  if (missing(lugar.a.buscar) | missing(dia.semana)) {
    stop('"lugar.a.buscar" and "dia.semana" must be specified')
  }
  remDr$open(silent = TRUE) # abre firefox
  remDr$navigate("https://www.google.com.ar") # va a google.com.ar
  webElem <- remDr$findElement(using = "name", value = "q") # selecciona el recuadro de busqueda
  webElem$sendKeysToElement(list(paste(lugar.a.buscar, "horarios", dia.semana), "\uE007")) # escribe el lugar.a.buscar y hace la busqueda

  apertura <- NA_character_ # para empezar el loop que sigue
  concurrencia <- NA_character_ # para empezar el loop que sigue
  x <- 0
  while (x <= tiempo.espera & replace(apertura[1], is.na(apertura[1]), "0") != "Cerrado" & is.na(concurrencia[1])) {
    source <- remDr$getPageSource()[[1]] # codigo de fuente de la pagina de google
    apertura <- qdapRegex::ex_between(source, '"TLou0b JjSWRd">', "<")[[1]] # entre estos characteres, google dice si el lugar esta cerrado o abierto este dia, si esta cerrado se cierra firefox y se vuelve a empezar
    concurrencia <- qdapRegex::ex_between(source, 'class="cwiwob', "px")[[1]] # extrae la cantidad de concurrencia en unidades de pixel que aparece en el grafico de concurrencia
    x <- x + 1
  }
  if (!("Cerrado" %in% replace(apertura[1], is.na(apertura[1]), "0"))) {
    if (!is.na(concurrencia[1])) {
      coordenadas <- qdapRegex::ex_between(source, 'data-url="/maps/place/', ",15z")
      remDr$close() # cierra firefox, ya no se necesita

      coordenadas <- sub(".*@", "", coordenadas)
      latitud <- sub(",.*", "", coordenadas)
      longitud <- sub(".*,", "", coordenadas)
      concurrencia <- as.numeric(sub(".*:", "", concurrencia)) # me quedo solo con la parte interesante del string
      hora <- qdapRegex::ex_between(source, "data-hour=", " jsaction")[[1]] # extrae la hora a la que corresponden las concurrencias
      hora <- as.numeric(gsub("[^0-9.-]", "", hora)) # me quedo solo con la parte interesante del string

      # En este punto se genera un problema: si la busqueda se realiza en una hora en la que el local esta abierto, google agrega adem?s la concurrencia actual observada, por lo que hay mas numeros de concurrencia que de horarios, este if sirve para ignorar ese numero de mas

      if (length(concurrencia) != length(hora)) {
        loc.concurrencia <- data.table::data.table(concurrencia, data.frame(stringr::str_locate_all(source, 'class="cwiwob'))) # ubicaciones de los characteres encontrados
        loc.hora <- data.table::data.table(hora, data.frame(stringr::str_locate_all(source, "data-hour=")))
        data.table::setkey(loc.concurrencia, "start") # para hacer el join
        data.table::setkey(loc.hora, "start")
        df <- as.data.frame(loc.concurrencia[loc.hora, roll = "nearest"]) # join menos estricto, joinea segun cercania de caracteres
        df <- df[, c(4, 1)]
      } else {
        df <- data.frame(hora, concurrencia) # si no hay esa concurrencia de mas, directamente junto la hora y la concurrencia
      }
      # aca hago mas linda la tabla
      df$lugar <- lugar.a.buscar
      df$dia <- dplyr::case_when(
        hora > 23 & dia.semana == "lunes" ~ "martes",
        hora > 23 & dia.semana == "martes" ~ "miercoles",
        hora > 23 & dia.semana == "miercoles" ~ "jueves",
        hora > 23 & dia.semana == "jueves" ~ "viernes",
        hora > 23 & dia.semana == "viernes" ~ "sabado",
        hora > 23 & dia.semana == "sabado" ~ "domingo",
        hora > 23 & dia.semana == "domingo" ~ "lunes",
        TRUE ~ dia.semana
      )
      df$hora <- dplyr::if_else(df$hora > 23, df$hora - 24, df$hora)
      df$fecha.de.busqueda <- Sys.Date() # este lo agrego por las dudas, no se cuanto cambia segun el dia que se busca
      df$latitud <- as.numeric(latitud)
      df$longitud <- as.numeric(longitud)
      df <- df[, c(3, 4, 1, 2, 6, 7, 5)]
      df
    } else {
      remDr$close()
      return("Sin datos de concurrencia")
    }
  } else {
    remDr$close()
  }
}
