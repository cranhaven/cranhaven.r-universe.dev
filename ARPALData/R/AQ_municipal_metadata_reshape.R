AQ_municipal_metadata_reshape <-
  function() {

    ##### Check online availability for AQ municipal metadata
    temp <- tempfile()
    res <- suppressWarnings(try(curl::curl_fetch_disk("https://www.dati.lombardia.it/resource/5rep-i3mj.csv", temp), silent = TRUE))
    if(res$status_code != 200) {
      message(paste0("The internet resource for municipal air quality stations metadata is not available at the moment. Status code: ",res$status_code,".\nPlease, try later. If the problem persists, please contact the package maintainer."))
      return(invisible(NULL))
    } else {
      Metadata <- RSocrata::read.socrata("https://www.dati.lombardia.it/resource/5rep-i3mj.csv")
    }


    # if(res$status_code != 200) {
    #   message(paste0("The internet resource for air quality of municipalities metadata is not available at the moment, try later.\nIf the problem persists, please contact the package maintainer."))
    #   return(invisible(NULL))
    # } else {
    #   Metadata <- RSocrata::read.socrata("https://www.dati.lombardia.it/resource/5rep-i3mj.csv")
    # }

    Metadata <- Metadata %>%
      dplyr::rename(IDSensor = .data$idsensore, IDStation = .data$idstazione,
                    Pollutant = .data$nometiposensore,
                    Province = .data$provincia, NameStation = .data$comune,
                    DateStart = .data$datastart, DateStop = .data$datastop) %>%
      dplyr::mutate(DateStart = lubridate::ymd(.data$DateStart),
                    DateStop = lubridate::ymd(.data$DateStop)) %>%
      dplyr::select(.data$IDSensor, .data$IDStation, .data$Pollutant,
                    .data$Province, .data$NameStation, .data$DateStart, .data$DateStop) %>%
      dplyr::mutate(Pollutant = dplyr::recode(.data$Pollutant,
                                              "Ammoniaca" = "Ammonia",
                                              "Arsenico" = "Arsenic",
                                              "Benzene" = "Benzene",
                                              "Benzo(a)pirene" = "Benzo_a_pyrene",
                                              "Biossido di Azoto" = "NO2",
                                              "Monossido di Azoto" = "NO",
                                              "Ossidi di Azoto" = "NOx",
                                              "Biossido di Zolfo" = "Sulfur_dioxide",
                                              "BlackCarbon" = "BlackCarbon",
                                              "Monossido di Carbonio" = "CO",
                                              "Nikel" = "Nikel",
                                              "Ozono" = "Ozone",
                                              "Cadmio" = "Cadmium",
                                              "PM10 (SM2005)" = "PM10",
                                              "PM10" = "PM10",
                                              "Particelle sospese PM2.5" = "PM2.5",
                                              "Particolato Totale Sospeso" = "PM_tot",
                                              "Piombo" = "Lead"))

    ### Name stations (municipalities)
    Metadata <- Metadata %>%
      dplyr::mutate(dplyr::across(c(.data$NameStation), toupper),
                    dplyr::across(c(.data$NameStation), ~ gsub("\\-", " ", .x)),
                    dplyr::across(c(.data$NameStation), ~ stringr::str_replace_all(.x, c("S\\."="San ","s\\."="San ",
                                                                                         "V\\."="Via ","v\\."="Via ",
                                                                                         " D\\`" = " D\\' ", " D\\` " = " D\\'",
                                                                                         "D\\`" = " D\\'", "D\\'" = " D\\' "))),
                    dplyr::across(c(.data$NameStation), tm::removePunctuation),
                    dplyr::across(c(.data$NameStation), tm::removeNumbers),
                    dplyr::across(c(.data$NameStation), tm::stripWhitespace)) %>%
      dplyr::mutate(NameStation = dplyr::recode(.data$NameStation,
                                                "CASASCO DINTELVI" = "CASASCO INTELVI",
                                                "CERANO DINTELVI" = "CERANO INTELVI",
                                                "SAN GIORGIO BIGARELLO" = "BIGARELLO",
                                                "PUEGNAGO DEL GARDA" = "PUEGNAGO SUL GARDA",
                                                "FELONICA" = "SERMIDE E FELONICA",
                                                "GERRE DE CAPRIOLI" = "GERRE DECAPRIOLI")) %>%
      dplyr::mutate(dplyr::across(c(.data$NameStation), stringr::str_to_title),
                    dplyr::across(c(.data$NameStation), ~ stringr::str_replace_all(.x,
                                                                                   c(" D " = " D\\'",
                                                                                     "Sermide E Felonica" = "Sermide e Felonica"))))

    structure(list(Metadata = Metadata))
    attr(Metadata, "class") <- c("ARPALdf","tbl_df","tbl","data.frame")
    return(Metadata)
  }

