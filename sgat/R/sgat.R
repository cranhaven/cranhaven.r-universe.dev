#' Iteration over \code{\link{sgat_day}} on every day of the week and finally save the retrieved data as a csv file. If you are looking for the "Popular Times" information for a single weekday use \code{\link{sgat_day}} instead.
#'
#' @param lugar.a.buscar What you want to search in Google
#' @param tiempo.espera Time measure of how much time you think it's sufficient given your internet connection to load a Google search page and not finding the information,
#' @param carpeta.guardado Name of the directory where the csv files will be saved. If the directory doesn't exist, it will be created automatically.
#'
#' @return Data frame with retrieved information for all weekdays
#' @export
#'
#' @examples
#' \dontrun{
#' museo <- sgat(lugar.a.buscar = "museo nacional de bellas artes, buenos aires, argentina")
#' head(museo)
#' }
sgat <- function(lugar.a.buscar, tiempo.espera = 10, carpeta.guardado = NULL) {
  dias.semana <- c("martes", "miercoles", "jueves", "viernes", "sabado", "domingo")
  df <- sgat::sgat_day(lugar.a.buscar, "lunes", tiempo.espera)
  if (!is.data.frame(df)) {
    if (length(df) == 0) {
      i <- TRUE
    } else if (df == "Sin datos de concurrencia") {
      i <- FALSE
    }
  } else {
    i <- TRUE
  }
  if (i) {
    for (dia.semana in dias.semana) {
      datos <- sgat::sgat_day(lugar.a.buscar, dia.semana, tiempo.espera)
      df <- rbind(df, datos)
    }
    if (is.character(carpeta.guardado)) {
      dir.create(carpeta.guardado, showWarnings = FALSE) # crea la carpeta concurrencia si no existe aun
      utils::write.csv(df, file = paste(carpeta.guardado, "/Concurrencia ", lugar.a.buscar, " ", as.character(Sys.Date()), ".csv", sep = ""))
    }
    df
  } else {
    NULL
  }
}
