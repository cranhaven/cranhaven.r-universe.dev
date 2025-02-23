#' Get all bills signed into law
#'
#' Get a dataframe containing all of the bills that originated in a
#'     given chamber and were eventually signed into law
#'
#' @inheritParams getCommitteeMembers
#'
#' @return \code{getLegislationSigned} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' ## get all bills signed into law from the Senate between 2007-2010
#' bienniums <- c("2007-08", "2009-10")
#' getLegislationSigned(bienniums, "Senate")
getLegislationSigned <- function(biennium, agency = c("House", "Senate"), paired = FALSE, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getLegislationSigned for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  }

  agency <- paste(toupper(substr(agency,1,1)),
                  substr(agency,2,nchar(agency)), sep = "")

  if(!all(agency %in% c("House", "Senate"))) {
    stop("Agency name invalid. Make sure to use one of 'House' or 'Senate'")
  }

  if(length(biennium) == length(agency) & paired) {
    request <- data.frame(biennium = biennium, agency = agency)
  } else {
    request <- expand.grid(biennium, agency, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetLegislationGovernorSigned?biennium=",
                    request[bill,1], "&agency=", request[bill,2], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToDataFrame(tbl,
                                 stringsAsFactors = FALSE)

      if(nrow(tbl) > 0) {
        tbl$Biennium <- request[bill,1]
        tbl$Agency <- request[bill,2]
        tbl <- tbl[c("Biennium", "Agency",
                     setdiff(names(tbl), c("Biennium", "Agency")))]
        out <- dplyr::bind_rows(out, tbl)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetLegislationGovernorSigned?biennium=",
                    request[bill,1], "&agency=", request[bill,2], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(tbl)
      list <- list(tbl)
      names(list) <- paste(request[bill,1], request[bill,2], sep = "_")
      if(length(tbl) > 0) {
        out <- c(out, list)
      }
    }
  } else if(type == "xml") {
    out <- c()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetLegislationGovernorSigned?biennium=",
                    request[bill,1], "&agency=", request[bill,2], sep = "")

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
