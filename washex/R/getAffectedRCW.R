#' Link bills to Revised Code of Washington (RCW)
#'
#' Get a listing of all RCW citations affected by a given bill
#'
#' @inheritParams getBillSponsors
#' @inheritParams getLegislation
#'
#' @return \code{getAffectedRCW} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#'
#' @export
#' @examples
#' ## usage for a single bill case, XML form
#' getAffectedRCW("2005-06", "HB 1427", type = "xml")
#'
#' ## generates a dataframe of affected codes from all bills in 2007
#' \dontrun{
#' bills <- getLegislationByYear("2007")
#' codesAffected <- getAffectedRCW("2007-08", bills$BillId)}
#'
#' @section Note: for more information on RCW codes, see
#'     \url{https://apps.leg.wa.gov/rcw/}
getAffectedRCW <- function(biennium, billId, paired = TRUE, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getAffectedRCW for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billId_pattern, billId))) {
    stop("Bill ID formatted incorrectly. Use ?getAffectedRCW for more information")
  }

  if(length(biennium) == length(billId) & paired) {
    request <- data.frame(biennium = biennium, billId = billId)
  } else {
    request <- expand.grid(biennium, billId, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetRcwCitesAffected?biennium=",
                    request[bill,1], "&billId=", gsub(" ", "%20", request[bill,2]), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToDataFrame(tbl,
                                 stringsAsFactors = FALSE)
      if(nrow(tbl) > 0) {
        tbl$Biennium <- request[bill,1]
        tbl$BillId <- request[bill,2]
        tbl <- tbl[c("Biennium", "BillId",
                     setdiff(names(tbl),c("Biennium","BillId")))]
        out <- dplyr::bind_rows(out, tbl)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetRcwCitesAffected?biennium=",
                    request[bill,1], "&billId=", gsub(" ", "%20", request[bill,2]), sep = "")

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
                    "legislationservice.asmx/GetRcwCitesAffected?biennium=",
                    request[bill,1], "&billId=", gsub(" ", "%20", request[bill,2]), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      out <- c(out,tbl)
    }
    names(out) <- paste(request[,1],request[,2],sep="//")
  }
  return(out)
}
