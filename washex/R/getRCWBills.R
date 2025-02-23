#' Search for bills based on Revised Code (RCW) citations
#'
#' Get a list of all bills which reference or amend a particular
#'      portion of the Revised Code of Washington (RCW)
#'
#' @inheritParams getLegislation
#' @param rcwCite Character vector for the citation in the RCW to pull
#'      legislation from. Optional extensions for title, chapter, and section
#'      are allowed. For more information, see
#'      \url{https://apps.leg.wa.gov/rcw/}
#'
#' @return \code{getRCWBills} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' getRCWBills("2007-08", "13.40.0357")
getRCWBills <- function(biennium, rcwCite, paired = FALSE, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)
  rcwCite <- as.character(rcwCite)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getLegislation for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(rcw_pattern, rcwCite))) {
    stop("RCW reference formatted incorrectly. Use ?getRCWBills for more information")
  }

  if(length(biennium) == length(rcwCite) & paired) {
    request <- data.frame(biennium = biennium, rcwCite = rcwCite)
  } else {
    request <- expand.grid(biennium, rcwCite, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "RcwCiteAffectedService.asmx/GetLegislationAffectingRcw?biennium=",
                    request[bill,1], "&rcwCite=", request[bill,2], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToDataFrame(tbl,
                                 stringsAsFactors = FALSE)
      if(nrow(tbl) > 0) {
        tbl$Biennium <- request[bill,1]
        tbl$rcwCite <- request[bill,2]
        tbl <- tbl[c("Biennium", "rcwCite",
                     setdiff(names(tbl), c("Biennium", "rcwCite")))]
        out <- dplyr::bind_rows(out, tbl)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "RcwCiteAffectedService.asmx/GetLegislationAffectingRcw?biennium=",
                    request[bill,1], "&rcwCite=", request[bill,2], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(tbl)
      list <- list(tbl)
      names(list) <- request[bill,2]
      if(length(tbl) > 0) {
        out <- c(out, list)
      }
    }
  } else if(type == "xml") {
    out <- c()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "RcwCiteAffectedService.asmx/GetLegislationAffectingRcw?biennium=",
                    request[bill,1], "&rcwCite=", request[bill,2], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      out <- c(out, tbl)
    }
    names(out) <- paste(request[,1], request[,2], sep = "//")
  }
  return(out)
}
