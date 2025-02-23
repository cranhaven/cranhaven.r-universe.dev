#' Get sponsor information for a bill
#'
#' @inheritParams getLegislation
#' @param billId Character vector containing the bill(s) to be retrieved.
#'      Each argument should take the form "XX YYYY", where XX
#'      is the prefix (HB, SB, etc.) and YYYY is the bill number.
#'
#' @return \code{getBillSponsors} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' ## get the list of all sponsors on a set of bills, filtered for primary sponsorship
#'
#' spons <- getBillSponsors("2007-08", c("HB 1001", "HB 1002", "HB 1003"))
#' if(!is.null(spons)) sponsP <- subset(spons, Type == "Primary")
getBillSponsors <- function(biennium, billId, paired = TRUE, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getBillSponsors for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billId_pattern, billId))) {
    stop("Bill ID formatted incorrectly. Use ?getBillSponsors for more information")
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
                    "legislationservice.asmx/GetSponsors?biennium=",
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
                    "legislationservice.asmx/GetSponsors?biennium=",
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
                    "legislationservice.asmx/GetSponsors?biennium=",
                    request[bill,1], "&billId=", gsub(" ", "%20", request[bill,2]), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      out <- c(out,tbl)
    }
    names(out) <- paste(request[,1],request[,2],sep = "//")
  }
  return(out)
}
