.onLoad <- function(libname = find.package("EMAtools"), pkgname = "EMAtools"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("respondent_id","survey_id","timestamp_event","timezone_offset",
                             "Effect_Size","Power","Resp","Response_Rate","aes","geom_line","geom_vline","scale_x_continuous","scale_y_continuous","theme_classic","xlab",
                             "scale_color_discrete","scale_linetype","ylab","NoDays"))
}
