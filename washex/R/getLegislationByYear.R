#' Get legislation by year
#'
#' Get a list of all bills introduced during the year
#'
#' @inheritParams getLegislation
#' @param year Character or numeric vector representing the year(s) to be
#'      searched.
#'
#' @return \code{getLegislationByYear} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' \dontrun{getLegislationByYear("2007")}
getLegislationByYear <- function(year, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)
  year <- as.character(year)

  if(!all(grepl(year_pattern, year))) {
    stop("Year formatted incorrectly. Use ?getLegislationByYear for more information")
  } else if(!all(as.numeric(year) >= 1991)) {
    stop("Year out of range. Information is available going back to 1991")
  }

  if(type == "df") {
    out <- data.frame()

    for(i in 1:length(year)) {
      path <- paste(prefix, "legislationservice.asmx/GetLegislationByYear?year=",
                    gsub(" ", "%20", year[i]), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToDataFrame(tbl,
                                 stringsAsFactors = FALSE)
      if(nrow(tbl) > 0) {
        tbl$Year <- year[i]
        tbl <- tbl[c("Year",
                     setdiff(names(tbl), "Year"))]
        out <- dplyr::bind_rows(out, tbl)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(i in 1:length(year)) {
      path <- paste(prefix, "legislationservice.asmx/GetLegislationByYear?year=",
                    gsub(" ", "%20", year[i]), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(tbl)
      list <- list(tbl)
      names(list) <- year[i]
      if(length(tbl) > 0) {
        out <- c(out, list)
      }
    }
  } else if(type == "xml") {
    out <- c()

    for(i in 1:length(year)) {
      path <- paste(prefix, "legislationservice.asmx/GetLegislationByYear?year=",
                    gsub(" ", "%20", year[i]), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      out <- c(out, tbl)
    }
    names(out) <- year
  }
  return(out)
}
