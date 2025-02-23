#' Get hearings regarding a bill
#'
#' Get a list of dates, locations, and descriptions of all
#'     committee hearings on a particular bill
#'
#' @inheritParams getLegislation
#'
#' @return \code{getHearings} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' ## get hearings for all senate bills in 2011
#' bills <- getLegislationByYear("2011")
#' if(!is.null(bills)) billsSenate <- subset(bills, OriginalAgency == "Senate")
#'
#' \dontrun{getHearings(billsSenate$Biennium, billsSenate$BillNumber, paired = TRUE, type = "df")}
#'
#' @section Note: Due to the nature of the resulting XML document,
#'     the function trims data from excessively nested lists when
#'     \code{type = "df"}. In order to access the full information, use
#'     \code{type = "list"} instead.
getHearings <- function(biennium, billNumber, paired = TRUE, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)
  billNumber <- as.character(billNumber)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getHearings for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getHearings for more information")
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
                    "legislationservice.asmx/GetHearings?biennium=",
                    request[bill,1], "&billNumber=", request[bill,2], sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- purrr::map(XML::xmlToList(tbl), purrr::flatten)
      tbl <- purrr::map(tbl, ~ purrr::discard(.x, is.null))
      tbl <- purrr::map(tbl, ~ toss(.x, "Committees"))

      if(length(tbl) > 0) {
        tbl <- purrr::map(tbl, ~ data.frame(t(matrix(unlist(.x), nrow = length(.x), dimnames = list(names(.x)))),
                                            stringsAsFactors = FALSE))

        df <- dplyr::bind_rows(tbl)
        out <- dplyr::bind_rows(out, df)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetHearings?biennium=",
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
                    "legislationservice.asmx/GetHearings?biennium=",
                    request[bill,1], "&billNumber=", request[bill,2], sep = "")

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
