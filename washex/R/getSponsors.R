#' Get legislators
#'
#' Get a list of all sponsors (all congressmembers) for a given biennium
#'
#' @inheritParams getLegislation
#'
#' @return \code{getSponsors} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' getSponsors("2007-08")
getSponsors <- function(biennium, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getSponsors for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  }

  if(type == "df") {
    out <- data.frame()

    for(year in biennium) {
      path <- paste(prefix,
                    "sponsorservice.asmx/GetSponsors?biennium=",
                    year, sep = "")
      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToDataFrame(tbl,
                                 stringsAsFactors = FALSE)
      if(nrow(tbl) > 0) {
        tbl$Biennium <- year
        tbl <- tbl[c("Biennium",
                   setdiff(names(tbl), "Biennium"))]
        out <- dplyr::bind_rows(out, tbl)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(year in biennium) {
      path <- paste(prefix,
                    "sponsorservice.asmx/GetSponsors?biennium=",
                    year, sep = "")
      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(tbl)
      list <- list(tbl)
      names(list) <- year
      if(length(tbl) > 0) {
        out <- c(out, list)
      }
    }
  } else if(type == "xml") {
    out <- c()

    for(year in biennium) {
      path <- paste(prefix,
                    "sponsorservice.asmx/GetSponsors?biennium=",
                    year, sep = "")
      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlParse(tbl)

      out <- c(out, tbl)
    }
    names(out) <- biennium
  }
  return(out)
}
