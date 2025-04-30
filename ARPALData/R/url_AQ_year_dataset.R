#' @keywords internal
#' @noRd

url_AQ_year_dataset <-
  function(Stat_type, Year) {
    url <- switch(Stat_type,
                  ##### Air quality from ground network
                  AQ = dplyr::case_when(Year %in% 2024:2025 ~ "https://www.dati.lombardia.it/resource/nicp-bhqi.json",
                                        Year %in% 2018:2023 ~ "https://www.dati.lombardia.it/resource/g2hp-ar79.json",
                                        Year %in% 2010:2017 ~ "https://www.dati.lombardia.it/resource/nr8w-tj77.json",
                                        Year %in% 2000:2009 ~ "https://www.dati.lombardia.it/resource/cthp-zqrr.json",
                                        Year %in% 1968:1999 ~ "https://www.dati.lombardia.it/resource/evzn-32bs.json"),

                  ##### Municipal data
                  AQ_municipal = dplyr::case_when(Year %in% 2023:2025 ~ "https://www.dati.lombardia.it/resource/ysm5-jwrn.json",
                                                  Year == 2022 ~ "https://www.dati.lombardia.it/resource/fqaz-7ste.json",
                                                  Year == 2021 ~ "https://www.dati.lombardia.it/download/56c9-hxta/application%2Fzip",
                                                  Year == 2020 ~ "https://www.dati.lombardia.it/download/ej5v-5krk/application%2Fzip",
                                                  Year == 2019 ~ "https://www.dati.lombardia.it/download/dupr-g65c/application%2Fzip",
                                                  Year == 2018 ~ "https://www.dati.lombardia.it/download/v75z-59qh/application%2Fzip",
                                                  Year == 2017 ~ "https://www.dati.lombardia.it/download/a7tn-gnv9/application%2Fzip",
                                                  Year %in% 2011:2016 ~ "https://www.dati.lombardia.it/download/yjvq-g3tp/application%2Fzip"),

                  ##### Web pages of the municipal data
                  AQ_municipal_check = dplyr::case_when(Year %in% 2023:2025 ~ "https://www.dati.lombardia.it/Ambiente/Dati-stime-comunali/ysm5-jwrn",
                                                        Year == 2022 ~ "https://www.dati.lombardia.it/Ambiente/Dati-stime-comunali-2022/fqaz-7ste",
                                                        Year == 2021 ~ "https://www.dati.lombardia.it/Ambiente/Dati-Stime-Comunali-2021/56c9-hxta",
                                                        Year == 2020 ~ "https://www.dati.lombardia.it/Ambiente/Dati-Stime-Comunali-2020/ej5v-5krk",
                                                        Year == 2019 ~ "https://www.dati.lombardia.it/Ambiente/Dati-Stime-Comunali-2019/dupr-g65c",
                                                        Year == 2018 ~ "https://www.dati.lombardia.it/Ambiente/Dati-Stime-Comunali-2018/v75z-59qh",
                                                        Year == 2017 ~ "https://www.dati.lombardia.it/Ambiente/Dati-Stime-Comunali-2017/a7tn-gnv9",
                                                        Year %in% 2011:2016 ~ "https://www.dati.lombardia.it/Ambiente/Dati-Stime-Comunali-2011-2016/yjvq-g3tp")
    )

    return(url)
  }

