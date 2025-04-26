.onLoad <- function(libname = find.package("E4tools"), pkgname = "E4tools"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("ts_time","EDA_HighLowPass","E4_serial","EDA_reject_CAT",
                             ":=","E4_serial", "E4serial", "EDA_HighLowPass", "EDA_reject_CAT",
                             "NUMB", "TEMP_C", "TEMP_F", "ts", "ts_time","NumbCoresUse"))
}



.onLoad <- function(libname = find.package("E4tools"), pkgname = "E4tools")
{

  E4tools.env <- new.env()
}


