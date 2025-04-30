#' @keywords internal
#' @noRd

url_W_year_par <-
  function(WPar,Year) {
    WPar <- as.character(WPar)
    url <- switch(WPar,
                  ##### Temperature
                  # Link to the data
                  Temperature = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/resource/w9wd-u6jh.json",
                                                 Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/resource/d4kj-kbpj.json",
                                                 Year <= 2010 ~ "https://www.dati.lombardia.it/resource/6eu4-4tja.json"),
                  # Web page for check
                  Temperature_check = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/Ambiente/Temperatura-dal-2021/w9wd-u6jh/about_data",
                                                       Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/Ambiente/Temperatura-dal-2011-al-2020/d4kj-kbpj/about_data",
                                                       Year <= 2010 ~ "https://www.dati.lombardia.it/Ambiente/Temperatura-fino-al-2010/6eu4-4tja/about_data"),
                  ##### Wind speed
                  # Link to the data
                  Wind_speed = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/resource/hu5q-68e3.json",
                                                Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/resource/x9gp-z9xx.json",
                                                Year <= 2010 ~ "https://www.dati.lombardia.it/resource/ed5f-uim8.json"),
                  # Web page for check
                  Wind_speed_check = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/Ambiente/Velocit-del-vento-dal-2021/hu5q-68e3/about_data",
                                                      Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/Ambiente/Velocit-del-vento-dal-2011-al-2020/x9gp-z9xx/about_data",
                                                      Year <= 2010 ~ "https://www.dati.lombardia.it/Ambiente/Velocit-del-vento-fino-al-2010/ed5f-uim8/about_data"),
                  ##### Wind direction
                  # Link to the data
                  Wind_direction = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/resource/purm-rsjf.json",
                                                    Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/resource/ypty-e75m.json",
                                                    Year <= 2010 ~ "https://www.dati.lombardia.it/resource/cptm-adt5.json"),
                  # Web page for check
                  Wind_direction_check = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/Ambiente/Direzione-del-vento-dal-2021/purm-rsjf/about_data",
                                                          Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/Ambiente/Direzione-del-vento-dal-2011-al-2020/ypty-e75m/about_data",
                                                          Year <= 2010 ~ "https://www.dati.lombardia.it/Ambiente/Direzione-del-vento-fino-al-2010/cptm-adt5/about_data"),
                  ##### Wind gust (raffica di vento) --> Same sensors of wind speed
                  # Link to the data
                  Wind_speed_gust = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/resource/gi9u-2aez.json",
                                                     Year <= 2020 ~ "https://www.dati.lombardia.it/resource/ppwf-2tzh.json"),
                  # Web page for check
                  Wind_speed_gust_check = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/Ambiente/Raffica-del-vento-dal-2021/gi9u-2aez/about_data",
                                                           Year <= 2020 ~ "https://www.dati.lombardia.it/Ambiente/Raffica-del-vento-dal-2012-al-2020/ppwf-2tzh/about_data"),
                  ##### Relative humidity
                  # Link to the data
                  Relative_humidity = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/resource/823w-fh4c.json",
                                                       Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/resource/xpun-8722.json",
                                                       Year <= 2010 ~ "https://www.dati.lombardia.it/resource/nure-mhrg.json"),
                  # Web page for check
                  Relative_humidity_check = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/Ambiente/Umidit-relativa-dal-2021/823w-fh4c/about_data",
                                                             Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/Ambiente/Umidit-relativa-dal-2011-al-2020/xpun-8722/about_data",
                                                             Year <= 2010 ~ "https://www.dati.lombardia.it/Ambiente/Umidit-relativa-fino-al-2010/nure-mhrg/about_data"),
                  ##### Rainfall
                  # Link to the data
                  Rainfall = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/resource/pstb-pga6.json",
                                              Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/resource/2kar-pnuk.json",
                                              Year <= 2010 ~ "https://www.dati.lombardia.it/resource/e7r2-7m84.json"),
                  # Web page for check
                  Rainfall_check = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/Ambiente/Precipitazioni-dal-2021/pstb-pga6/about_data",
                                                    Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/Ambiente/Precipitazioni-dal-2011-al-2020/2kar-pnuk/about_data",
                                                    Year <= 2010 ~ "https://www.dati.lombardia.it/Ambiente/Precipitazioni-fino-al-2010/e7r2-7m84/about_data"),
                  ##### Global solar radiation
                  # Link to the data
                  Global_radiation = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/resource/cxym-eps2.json",
                                              Year <= 2020 ~ "https://www.dati.lombardia.it/resource/63ns-e4tv.json"),
                  # Web page for check
                  Global_radiation_check = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/Ambiente/Radiazione-Globale-dal-2021/cxym-eps2/about_data",
                                                    Year <= 2020 ~ "https://www.dati.lombardia.it/Ambiente/Radiazione-Globale-fino-al-2020/63ns-e4tv/about_data"),
                  ##### Water height (livello idrometrico)
                  # Link to the data
                  Water_height = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/resource/3e8b-w7ay.json",
                                                  Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/resource/gsyu-uxt3.json",
                                                  Year <= 2010 ~ "https://www.dati.lombardia.it/resource/xubc-puka.json"),
                  # Web page for check
                  Water_height_check = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/Ambiente/Livello-idrometrico-dal-2021/3e8b-w7ay/about_data",
                                                        Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/Ambiente/Livello-idrometrico-dal-2011-al-2020/gsyu-uxt3/about_data",
                                                        Year <= 2010 ~ "https://www.dati.lombardia.it/Ambiente/Livello-idrometrico-fino-al-2010/xubc-puka/about_data"),
                  ##### Snow height
                  # Link to the data
                  Snow_height = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/resource/uqbu-tt6m.json",
                                                  Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/resource/id9e-7wf3.json",
                                                  Year <= 2010 ~ "https://www.dati.lombardia.it/resource/5kig-ayub.json"),
                  # Web page for check
                  Snow_height_check = dplyr::case_when(Year >= 2021 ~ "https://www.dati.lombardia.it/Ambiente/Altezza-neve-dal-2021/uqbu-tt6m/about_data",
                                                        Year %in% 2011:2020 ~ "https://www.dati.lombardia.it/Ambiente/Altezza-neve-dal-2011-al-2020/id9e-7wf3/about_data",
                                                        Year <= 2010 ~ "https://www.dati.lombardia.it/it/Ambiente/Altezza-neve-fino-al-2010/5kig-ayub/about_data"),


    )

    return(url)
  }

