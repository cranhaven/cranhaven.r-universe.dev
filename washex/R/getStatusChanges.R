#' Track historical progess on a bill
#'
#' Get a complete history of all status changes that occurred on a particular
#'     bill
#'
#' @inheritParams getLegislation
#'
#' @return \code{getStatusChanges} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' getStatusChanges("2007-08", "1001", type = "list")
getStatusChanges <- function(biennium, billNumber, paired = TRUE, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)
  billNumber <- as.character(billNumber)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getStatusChanges for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getStatusChanges for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNum = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  beginDate <- paste(substr(request[1,1],1,4),"01","01",sep = "-")
  endDate <- paste(substr(request[1,1],1,2),substr(request[1,1],6,7),"-12","-31",sep = "")

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      beginDate <- paste(substr(request[bill,1],1,4),"01","01",sep = "-")
      endDate <- paste(substr(request[bill,1],1,2),substr(request[bill,1],6,7),"-12","-31",sep = "")

      path <- paste(prefix,
                    "legislationservice.asmx/GetLegislativeStatusChangesByBillNumber?biennium=",
                    gsub(" ", "%20", request[bill,1]), "&billNumber=", gsub(" ", "%20", request[bill,2]),
                    "&beginDate=", beginDate, "&endDate=", endDate, sep="")

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
                     setdiff(names(tbl), c("Biennium", "BillNumber")))]
        out <- dplyr::bind_rows(out, tbl)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      beginDate <- paste(substr(request[bill,1],1,4),"01","01",sep = "-")
      endDate <- paste(substr(request[bill,1],1,2),substr(request[bill,1],6,7),"-12","-31",sep = "")

      path <- paste(prefix,
                    "legislationservice.asmx/GetLegislativeStatusChangesByBillNumber?biennium=",
                    gsub(" ", "%20", request[bill,1]), "&billNumber=", gsub(" ", "%20", request[bill,2]),
                    "&beginDate=", beginDate, "&endDate=", endDate, sep="")

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
      beginDate <- paste(substr(request[bill,1],1,4),"01","01",sep = "-")
      endDate <- paste(substr(request[bill,1],1,2),substr(request[bill,1],6,7),"-12","-31",sep = "")

      path <- paste(prefix,
                    "legislationservice.asmx/GetLegislativeStatusChangesByBillNumber?biennium=",
                    gsub(" ", "%20", request[bill,1]), "&billNumber=", gsub(" ", "%20", request[bill,2]),
                    "&beginDate=", beginDate, "&endDate=", endDate, sep="")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlParse(tbl)

      out <- c(out, tbl)
    }
    names(out) <- paste(request[,1], request[,2], sep = "//")
  }
  return(out)
}
