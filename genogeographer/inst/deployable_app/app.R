library(genogeographer)

#' The object `db_list` should be initialised prior to lauching
#' For the settings at Department of Forensic Medicine, Section of Forensic Genetics, 
#' University of Copenhagen, Copenhagen, Denmark we collected population data that is 
#' stored in a file "RGA_db.RData". 
#' It contains three databases `KK164`, `KK164_seldin` and `KK164_kidd` generated using `pops_to_DB()`
#' on a spreadsheet data containing the necessary columns and information. 
#' Below, we use these data to construct the required object (Note the naming can be arbritary):

## load("REFERENCE_DATA_BASE.RData") ## ASSUMES TO LOAD DB_1, DB_2 and DB_3
db_list <- list("DB 1" = DB_1, "DB 2" = DB_2, "DB 3" = DB_3) ## "Long name to appear in drop-out" = DB
reporting_panel <- TRUE ## FALSE

shinyApp(
  ui = genogeographer:::ui_fct(),
  server = function(input, output, session){
    genogeographer:::server_fct(input = input, output = output, session = session)
    }
)

