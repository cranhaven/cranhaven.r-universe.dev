#' @keywords internal
#' @noRd

AQ_metadata_reshape <-
  function() {

    '%notin%' <- Negate('%in%')

    ##### Check online availability for AQ metadata
    temp <- tempfile()
    res <- suppressWarnings(try(curl::curl_fetch_disk("https://www.dati.lombardia.it/resource/ib47-atvt.csv", temp), silent = TRUE))
    if(res$status_code != 200) {
      message(paste0("The internet resource for air quality stations metadata is not available at the moment. Status code: ",res$status_code,".\nPlease, try later. If the problem persists, please contact the package maintainer."))
      return(invisible(NULL))
    } else {
      Metadata <- RSocrata::read.socrata("https://www.dati.lombardia.it/resource/ib47-atvt.csv")
    }

    Metadata <- Metadata %>%
      dplyr::rename(IDSensor = .data$idsensore, IDStation = .data$idstazione,
                    Pollutant = .data$nometiposensore, NameStation = .data$nomestazione,
                    Altitude = .data$quota, Province = .data$provincia, City = .data$comune,
                    DateStart = .data$datastart, DateStop = .data$datastop,
                    Latitude = .data$lat, Longitude = .data$lng) %>%
      dplyr::mutate(DateStart = lubridate::make_date(year = lubridate::year(.data$DateStart),
                                                     month = lubridate::month(.data$DateStart),
                                                     day = lubridate::day(.data$DateStart)),
                    DateStop = lubridate::make_date(year = lubridate::year(.data$DateStop),
                                                    month = lubridate::month(.data$DateStop),
                                                    day = lubridate::day(.data$DateStop))) %>%
      dplyr::select(.data$IDSensor, .data$IDStation, .data$Pollutant, .data$NameStation, .data$Altitude,
                    .data$Province, .data$City, .data$DateStart, .data$DateStop, .data$Latitude, .data$Longitude) %>%
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

    ### Name stations
    # dplyr::across(c(.data$NameStation,.data$City), ~ stringi::stri_trans_general(str = .x, id="Latin-ASCII"))
    Metadata <- Metadata %>%
      dplyr::mutate(dplyr::across(c(.data$NameStation,.data$City), toupper),
                    dplyr::across(c(.data$NameStation,.data$City), ~ gsub("\\-", " ", .x)),
                    dplyr::across(c(.data$NameStation,.data$City), ~ stringr::str_replace_all(.x, c("S\\."="San ",
                                                                                                    "V\\."="Via ",
                                                                                                    "V\\.LE" = "Viale",
                                                                                                    " D\\`" = " D\\' ",
                                                                                                    " D\\` " = " D\\'",
                                                                                                    "D\\`" = " D\\'",
                                                                                                    "D\\'" = " D\\' ",
                                                                                                    "P\\.ZZA" = "Piazza",
                                                                                                    "C\\.SO" = "Corso",
                                                                                                    "LOC\\." = "Localita"))),
                    dplyr::across(c(.data$NameStation,.data$City), tm::removePunctuation),
                    dplyr::across(c(.data$NameStation,.data$City), tm::removeNumbers),
                    dplyr::across(c(.data$NameStation,.data$City), tm::stripWhitespace),
                    dplyr::across(c(.data$NameStation,.data$City), stringr::str_to_title),
                    dplyr::across(c(.data$NameStation,.data$City), ~ stringr::str_replace_all(.x, c(" D " = " D\\'",
                                                                                                    " Xi " = " XI ",
                                                                                                    " Xxv " = " XXV ",
                                                                                                    " Via " = " - Via ",
                                                                                                    " Corso " = " - Corso ",
                                                                                                    " Localita " = rlang::as_utf8_character(" - Localit\u00e0 "),
                                                                                                    " Piazza " = " - Piazza ",
                                                                                                    " Via Le " = " Viale ",
                                                                                                    "F Turati" = "Turati",
                                                                                                    "Via Serafino Delluomo" = "Via Serafino Dell'Uomo",
                                                                                                    "Via Dellartigianato" = "Via Dell'Artigianato",
                                                                                                    "Montanaso Lombardo Sp" = "Montanaso Lombardo SP202",
                                                                                                    "Sp Casa Dellalpino" = " - SP27 Casa Dell'Alpino",
                                                                                                    "Spino D'Adda Sp" = "Spino D'Adda SP1",
                                                                                                    "Villasanta - Via A Volta" = "Villasanta - Via Volta",
                                                                                                    "Vimercate - Via Dellospedale" = "Vimercate - Via Dell'Ospedale",
                                                                                                    "Ss Sempione" = "SS Sempione"))))


    ### Add extra information from ARPA offices (uploaded on Paolo Maranzano's GitHub page)
    # ARPA_zone = ARPA Lombardia zoning of the region: https://www.arpalombardia.it/Pages/Aria/Rete-di-rilevamento/Zonizzazione.aspx
    # ARPA_stat_type = stations type: https://www.arpalombardia.it/Pages/Aria/Rete-di-rilevamento/Criteri-di-rilevamento/Tipologia-delle-stazioni.aspx?firstlevel=Ieri

    ##### Check online availability for further AQ metadata from GitHub
    temp <- tempfile()
    res <- curl::curl_fetch_disk("https://raw.githubusercontent.com/PaoloMaranzano/ARPALData/main/AQ_stations_ARPA_Lombardia.csv", temp)
    if(res$status_code != 200) {
      stop(paste0("The internet resource for further air quality stations metadata (from GitHub) is not available at the moment, try later.
                  If the problem persists, please contact the package maintainer."))
    } else {
      Metadata_ARPA_url <- "https://raw.githubusercontent.com/PaoloMaranzano/ARPALData/main/AQ_stations_ARPA_Lombardia.csv"
    }
    Metadata_ARPA <- readr::read_csv(Metadata_ARPA_url)
    Metadata_ARPA <- Metadata_ARPA %>%
      dplyr::select(.data$IDStation,.data$ARPA_zone,.data$ARPA_stat_type) %>%
      dplyr::distinct()
    Metadata <- dplyr::left_join(Metadata,Metadata_ARPA,by = c("IDStation"))
    Metadata <- Metadata %>%
      filter(.data$IDStation %notin% c(518,602,603,612,694,698,700))
    # Galliate 518 (NO) --> Fuori regione, chiusa nel 2017
    # Melegnano 602 --> Chiusa dal 2017 --> Chiusa dal 2017
    # Filago via Fermi Marne (612) --> Chiusa dal 2017
    # Castiraga 603 --> Chiusa dal 2017
    # Salionze 694 (VR) --> Fuori regione
    # Ceneselli 698 (RO) --> Fuori regione
    # Melara 700 (RO) --> Fuori regione, chiusa dal 2018


    structure(list(Metadata = Metadata))
    attr(Metadata, "class") <- c("ARPALdf","tbl_df","tbl","data.frame")
    return(Metadata)
  }
