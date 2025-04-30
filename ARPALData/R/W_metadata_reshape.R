#' @keywords internal
#' @noRd

W_metadata_reshape <-
  function() {

    '%notin%' <- Negate('%in%')

    ##### Check online availability for weather metadata
    temp <- tempfile()
    res <- suppressWarnings(try(curl::curl_fetch_disk("https://www.dati.lombardia.it/resource/nf78-nj6b.csv", temp), silent = TRUE))
    if(res$status_code != 200) {
      message(paste0("The internet resource for weather stations metadata is not available at the moment. Status code: ",res$status_code,".\nPlease, try later. If the problem persists, please contact the package maintainer."))
      return(invisible(NULL))
    } else {
      Metadata <- RSocrata::read.socrata("https://www.dati.lombardia.it/resource/nf78-nj6b.json")
    }

    Metadata <- Metadata %>%
      dplyr::rename(IDSensor = .data$idsensore, IDStation = .data$idstazione, NameStation = .data$nomestazione,
                    Measure = .data$tipologia, Unit_meas = .data$unit_dimisura,
                    Altitude = .data$quota, Province = .data$provincia,
                    DateStart = .data$datastart, DateStop = .data$datastop,
                    Latitude = .data$lat, Longitude = .data$lng) %>%
      dplyr::mutate(Altitude = as.numeric(.data$Altitude),
                    DateStart = lubridate::ymd(.data$DateStart),
                    DateStop = lubridate::ymd(.data$DateStop)) %>%
      dplyr::select(.data$IDSensor, .data$IDStation, .data$Measure, .data$Unit_meas, .data$NameStation, .data$Altitude,
                    .data$Province, .data$DateStart, .data$DateStop, .data$Latitude, .data$Longitude) %>%
      dplyr::mutate(Measure = dplyr::recode(.data$Measure,
                                            "Altezza Neve" = "Snow_height",
                                            "Direzione Vento" = "Wind_direction",
                                            "Livello Idrometrico" = "Water_height",
                                            "Precipitazione" = "Rainfall",
                                            "Radiazione Globale" = "Global_radiation",
                                            "Temperatura" = "Temperature"),
                    Measure = ifelse(.data$Measure == rlang::as_utf8_character("Umidit\u00e0 Relativa"),"Relative_humidity",.data$Measure),
                    Measure = ifelse(.data$Measure == rlang::as_utf8_character("Velocit\u00e0 Vento"),"Wind_speed",.data$Measure))

    ### Name stations
    # dplyr::across(c(.data$NameStation), ~ stringi::stri_trans_general(str = .x, id="Latin-ASCII")),
    Metadata <- Metadata %>%
      dplyr::mutate(dplyr::across(c(.data$NameStation), toupper),
                    dplyr::across(c(.data$NameStation), ~ gsub("\\-", " ", .x)),
                    dplyr::across(c(.data$NameStation), ~ stringr::str_replace_all(.x, c("S\\."="San ",
                                                                                         "V\\."="Via ",
                                                                                         "V\\.LE" = "Viale",
                                                                                         " D\\`" = " D\\' ",
                                                                                         " D\\` " = " D\\'",
                                                                                         "D\\`" = " D\\'",
                                                                                         "D\\'" = " D\\' ",
                                                                                         "P\\.ZZA" = "Piazza",
                                                                                         "C\\.SO" = "Corso",
                                                                                         "LOC\\." = "Localita"))),
                    dplyr::across(c(.data$NameStation), tm::removePunctuation),
                    dplyr::across(c(.data$NameStation), tm::removeNumbers),
                    dplyr::across(c(.data$NameStation), tm::stripWhitespace),
                    dplyr::across(c(.data$NameStation), stringr::str_to_title),
                    dplyr::across(c(.data$NameStation), ~ stringr::str_replace_all(.x, c(" D " = " D\\'",
                                                                                         " Xi " = " XI ",
                                                                                         " Xxv " = " XXV ",
                                                                                         " Xxiv " = " XXIV ",
                                                                                         " Via " = " - Via ",
                                                                                         " Viale " = " - Viale ",
                                                                                         " Corso " = " - Corso ",
                                                                                         " Localita " = rlang::as_utf8_character(" - Localit\u00e0 "),
                                                                                         " Piazza " = " - Piazza ",
                                                                                         " Via Le " = " Viale ",
                                                                                         "Smr" = "SMR",
                                                                                         "Bagolino Sp" = "Bagolino SP669",
                                                                                         "Borgoforte Ss" = "Borgoforte SS12",
                                                                                         "Capriolo Sp" = "Capriolo SP12",
                                                                                         "G Matteotti" = "Matteotti",
                                                                                         "Via Novembre" = "Via IV Novembre",
                                                                                         "Clusone Sp" = "Clusone SP671",
                                                                                         "Cremona Sp" = "Cremona SP10",
                                                                                         "Darfo Boario Terme Ss" = "Darfo Boario Terme SS42",
                                                                                         "Gavardo Sp" = "Gavardo SP116",
                                                                                         "Idro Ss" = "Idro SS237",
                                                                                         "Lainate Sp" = "Lainate SP109",
                                                                                         "Lomello Ss" = "Lomello SS211",
                                                                                         "Orio Litta Ss" = "Orio Litta SS234",
                                                                                         "Pavia Ss" = "Pavia SS35",
                                                                                         "Pralboino Sp" = "Pralboino SP64",
                                                                                         "Rivolta D'Adda Sp" = "Rivolta d'Adda SP4",
                                                                                         "Salerano Sul Lambro Sp" = "Salerano sul Lambro SP115",
                                                                                         "Sermide E Felonica Sp" = "Sermide e Felonica SP91",
                                                                                         "Vigevano Ss" = "Vigevano SS494"))))
    Metadata <- Metadata %>%
      filter(.data$IDStation %notin% c(891))
    # Isola San Antonio 891 (AL) --> Fuori regione

    return(Metadata)
  }
