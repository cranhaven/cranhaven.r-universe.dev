#' @title
#' Fix the time zone
#'
#' @description
#' Allows you to correct the timezone based on a date column and another time column
#'
#' @param data Data frame containing the data
#' @param col_date Column containing the dates
#' @param col_hour Column containing the time. It must be in the format "hh", "hh:mm", or "hh:mm:ss"; only the hours "hh" will be used for conversion.
#' @param fuso Time zone for correction. Query OlsonNames()
#'
#' @return
#' Data frame with the corrected timezone
#'
#' @export
#'
#' @import tidyr
#' @import dplyr
#' @import lubridate
#' @import rlang
#'
#' @examples
#' address <-
#'  base::system.file("extdata",
#'                     "ex1_inmet.CSV",
#'                     package = "DataMetProcess")
#'
#' df <-
#'   read.table(
#'     address,
#'     h=TRUE,
#'     sep = ";",
#'     dec = ",",
#'     skip = 8,
#'     na.strings = -9999,
#'     check.names = FALSE
#'   )
#'
#' df$Data = as.Date(df$Data,format = "%d/%m/%Y")
#'
#'
#' df <-
#'   adjustDate(df,
#'              colnames(df)[1],
#'              colnames(df)[2],
#'              fuso = "America/Bahia")
#'
#' head(df[1:2])
#'

adjustDate <- function(data = NULL, col_date = NULL, col_hour = NULL, fuso = NULL) {

  # Função robusta para tratar diferentes formatos de hora
  pad_two_digits <- function(x) {
    x <- as.character(x)

    # Extrair qualquer padrão de HHMM ou HH, ignorando texto extra
    extracted <- stringr::str_extract(x, "[0-9]{2,4}")

    # Se HHMM (ex: "0000" ou "1300"), converter para HH
    extracted <- ifelse(
      nchar(extracted) == 4,
      substr(extracted, 1, 2),
      extracted
    )

    # Formatar para HH
    sprintf("%02d", as.integer(extracted))
  }

  Date_Hour <- NULL

  # Aplicar a limpeza
  data[[col_hour]] <- pad_two_digits(data[[col_hour]])

  # Unir data e hora
  data <- tidyr::unite(
    data, "Date_Hour",
    {{ col_date }}, {{ col_hour }},
    remove = TRUE, sep = " "
  )

  # Converter para datetime com timezone correto
  data <- dplyr::mutate(
    data,
    Date_Hour = lubridate::as_datetime(
      base::format(
        base::as.POSIXct(
          base::strptime(Date_Hour, "%Y-%m-%d %H"),
          usetz = TRUE, tz = "Etc/GMT-0"
        ),
        tz = fuso
      )
    )
  )

  return(data)
}

