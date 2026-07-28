## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  
#  form_imported_from_a_spreadsheet <-
#    preregr::form_fromSpreadsheet(
#      paste0(
#        "https://docs.google.com/spreadsheets/d/",
#        "1bHDzpCu4CwEa5_3_q_9vH2691XPhCS3e4Aj_HLhw_U8"
#      )
#    );
#  
#  form_imported_from_a_spreadsheet |>
#    preregr::form_show(
#      section = "extraction"
#    );
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  form_initialized_from_a_spreadsheet <-
#    preregr::prereg_initialize(
#      paste0(
#        "https://docs.google.com/spreadsheets/d/",
#        "1bHDzpCu4CwEa5_3_q_9vH2691XPhCS3e4Aj_HLhw_U8"
#      )
#    );
#  
#  form_initialized_from_a_spreadsheet |>
#    preregr::prereg_show_item_completion(
#      section = "extraction"
#    );
#  

