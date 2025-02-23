#' Get amendments to a bill
#'
#' Get a list of all proposed amendments (accepted and rejected) on the bill,
#'     including the URL to the amendment text
#'
#' @inheritParams getLegislation
#'
#' @return \code{getAmendments} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' ## get amendments for a single bill
#' getAmendments("2007-08", "1001")
#'
#' ## get amendments for a specific set of bills
#' years <- c("2005-06","2007-08","2007-08")
#' bills <- c(1447,1219,1001)
#'
#' getAmendments(years, bills, paired = TRUE, type = "df")
getAmendments <- function(biennium, billNumber, paired = TRUE, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)
  billNumber <- as.character(billNumber)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getAmendments for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getAmendments for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetAmendmentsForBiennium?biennium=",
                    request[bill,1], "&billNumber=", request[bill,2], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToDataFrame(tbl,
                                 stringsAsFactors = FALSE)
      if(nrow(tbl) > 0) {
        tbl$Biennium <- request[bill,1]
        tbl$BillNumber <- request[bill,2]
        tbl <- tbl[c("Biennium", "BillNumber",
                     setdiff(names(tbl),c("Biennium", "BillNumber")))]
        out <- dplyr::bind_rows(out, tbl)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetAmendmentsForBiennium?biennium=",
                    request[bill,1], "&billNumber=", request[bill,2], sep = "")

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
                    "legislationservice.asmx/GetAmendmentsForBiennium?biennium=",
                    request[bill,1], "&billNumber=", request[bill,2], sep = "")

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
