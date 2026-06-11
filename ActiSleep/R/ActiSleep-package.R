#' @import lubridate
#' @importFrom stringr str_pad
#' @importFrom DBI dbConnect dbDisconnect dbReadTable
#' @importFrom RSQLite SQLite
#' @importFrom dplyr mutate select
#' @importFrom tibble as_tibble
#' @importFrom lazyeval interp
#' @importFrom accelerometry weartime
#' @importFrom stats quantile sd aggregate median
#' @importFrom utils str
#' @importFrom methods show new is
#' @useDynLib ActiSleep, .registration = TRUE, .fixes = "C_"
NULL
#> NULL
