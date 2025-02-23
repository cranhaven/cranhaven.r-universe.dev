#' Get legislative committees
#'
#' Get a list of all committees that were active during the biennium,
#'     along with their respective committee code
#'
#' @inheritParams getSponsors
#'
#' @return \code{getCommittees} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' getCommittees("2007-08")
getCommittees <- function(biennium, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getCommittees for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  }

  if(type == "df") {
    out <- data.frame()

    for(i in 1:length(biennium)) {
      path <- paste(prefix,
                    "CommitteeService.asmx/GetCommittees?biennium=",
                    biennium[i], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToDataFrame(tbl,
                                 stringsAsFactors = FALSE)
      if(nrow(tbl) > 0) {
        tbl$Biennium <- biennium[i]
        tbl <- tbl[c("Biennium",
                     setdiff(names(tbl),"Biennium"))]
        out <- dplyr::bind_rows(out, tbl)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(i in 1:length(biennium)) {
      path <- paste(prefix,
                    "CommitteeService.asmx/GetCommittees?biennium=",
                    biennium[i], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(tbl)
      list <- list(tbl)
      names(list) <- biennium[i]
      if(length(tbl) > 0) {
        out <- c(out, list)
      }
    }
  } else if(type == "xml") {
    out <- c()

    for(i in 1:length(biennium)) {
      path <- paste(prefix,
                    "CommitteeService.asmx/GetCommittees?biennium=",
                    biennium[i], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      out <- c(out, tbl)
    }
    names(out) <- biennium
  }
  return(out)
}
