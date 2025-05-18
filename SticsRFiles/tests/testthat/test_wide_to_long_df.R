test <- function() {
workspace <- "/home/plecharpent/Téléchargements/JavaSTICS-1.5.1-STICS-10.0.0/example"
tec_file <- file.path(workspace, "Ble_tec.xml")
tec_long_df <- SticsRFiles:::get_xml_files_param_df(file_path = tec_file)
tec_wide_df <- SticsRFiles:::df_wider(tec_long_df)

tec_wide_to_long_df <- tidyr::pivot_longer(tec_wide_df, !c("name", "type"), names_to="param", values_to = "value", values_transform = as.character)

tec_wide_to_long_df <- tidyr::pivot_longer(tec_wide_df,
                                           !c("name", "type"),
                                           names_to=c("param", "id", "crop"),
                                           values_to = "value",
                                           values_transform = as.character,
                                           names_pattern = "(.*)?[_](.*)?_[cC]rop(.*)")


ini_file <- file.path(workspace, "ble_ini.xml")
ini_long_df <- SticsRFiles:::get_xml_files_param_df(file_path = ini_file)
ini_wide_df <- SticsRFiles:::df_wider(ini_long_df)
ini_wide_to_long_df <- tidyr::pivot_longer(ini_wide_df,
                                           !c("name", "type"),
                                           names_to=c("param", "id", "crop"),
                                           values_to = "value",
                                           values_transform = as.character,
                                           names_pattern = "(.*)(_[0-9]*)?_[cC]rop([0-9]*)")


}
