#' Get bill status
#'
#' Get the current status of a given bill
#'
#' @inheritParams getLegislation
#'
#' @return \code{getCurrentStatus} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' getCurrentStatus("2007-08", "1001")
#'
#' ## get final status for all bills written in 2011
#' \dontrun{
#' billNums <- getLegislationByYear("2011")
#' status <- getCurrentStatus("2011-12", billNums$billNumber)}
#'
#' @section Note:
#' This function returns the bill's status as of today. If a bill
#'     was never passed, it lists the most recent status. To
#'     get a bill's complete history, use \code{\link{getStatusChanges}}
getCurrentStatus <- function(biennium, billNumber, paired = TRUE, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)
  billNumber <- as.character(billNumber)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getCurrentStatus for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getCurrentStatus for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix, "legislationservice.asmx/GetCurrentStatus?biennium=",
                    gsub(" ", "%20", request[bill,1]), "&billNumber=",
                    gsub(" ", "%20", request[bill,2]), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(tbl)
      df <- data.frame(t(matrix(unlist(tbl))),
                         stringsAsFactors = FALSE)

      colnames(df) <- names(tbl)
      rownames(df) <- ""

      if(nrow(df) > 0) {
        df$Biennium <- request[bill,1]
        df$BillNumber <- request[bill,2]
        df <- df[c("Biennium", "BillNumber",
                     setdiff(names(df), c("Biennium", "BillNumber")))]
        out <- dplyr::bind_rows(out, df)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix, "legislationservice.asmx/GetCurrentStatus?biennium=",
                    gsub(" ", "%20", request[bill,1]), "&billNumber=",
                    gsub(" ", "%20", request[bill,2]), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(tbl)
      list <- list(tbl)
      names(list) <- request[bill,2]
      if(length(tbl) > 0) {
        out <- c(out, tbl)
      }
    }
  } else if(type == "xml") {
    out <- c()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix, "legislationservice.asmx/GetCurrentStatus?biennium=",
                    gsub(" ", "%20", request[bill,1]), "&billNumber=",
                    gsub(" ", "%20", request[bill,2]), sep = "")

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
