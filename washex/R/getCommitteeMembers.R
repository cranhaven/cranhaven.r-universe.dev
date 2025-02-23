#' Get committee members
#'
#' @inheritParams getLegislation
#' @param agency One of "House" or "Senate", or a vector with these as its
#'      elements.
#' @param name Character vector of committee names. To get the committee names
#'      for a particular session, see \code{\link{getCommittees}}.
#'
#' @return \code{getCommitteeMembers} returns an object of type equal to the
#'     \code{type} argument (defaults to dataframe)
#' @export
#'
#' @examples
#' ## get all committee members for a select number of committees and years
#' years <- c("2011-12","2013-14")
#' comms <- c("Education","Judiciary")
#'
#' getCommitteeMembers(years, agency = "House", comms, paired = TRUE)
getCommitteeMembers <- function(biennium, agency = c("House", "Senate"), name, paired = FALSE, type = c("df", "list", "xml")) {
  type <- rlang::arg_match(type)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getCommitteeMembers for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  }

  agency <- paste(toupper(substr(agency,1,1)),
                  substr(agency,2,nchar(agency)), sep = "")

  if(!all(agency %in% c("House", "Senate"))) {
    stop("Agency name invalid. Make sure to use one of 'House' or 'Senate'")
  }

  if(length(biennium) == length(agency) &
     length(biennium) == length(name) & paired) {
    request <- data.frame(biennium = biennium, agency = agency, name = name)
  } else {
    request <- expand.grid(biennium, agency, name, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "CommitteeService.asmx/GetCommitteeMembers?biennium=",
                    request[bill,1], "&agency=", request[bill,2], "&committeeName=",
                    gsub("&", "%26", gsub(" ", "%20", request[bill,3])), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToDataFrame(tbl,
                                 stringsAsFactors = FALSE)
      if(nrow(tbl) > 0) {
        tbl$Biennium <- request[bill,1]
        tbl$Agency <- request[bill,2]
        tbl$CommitteeName <- request[bill,3]
        tbl <- tbl[c("Biennium", "Agency", "CommitteeName",
                     setdiff(names(tbl), c("Biennium", "Agency", "CommitteeName")))]
        out <- dplyr::bind_rows(out, tbl)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "CommitteeService.asmx/GetCommitteeMembers?biennium=",
                    request[bill,1], "&agency=", request[bill,2], "&committeeName=",
                    gsub("&", "%26", gsub(" ", "%20", request[bill,3])), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(tbl)
      list <- list(tbl)
      names(list) <- request[bill,3]
      if(length(tbl) > 0) {
        out <- c(out, list)
      }
    }
  } else if(type == "xml") {
    out <- c()

    for(bill in 1:nrow(request)) {
      path <- paste(prefix,
                    "CommitteeService.asmx/GetCommitteeMembers?biennium=",
                    request[bill,1], "&agency=", request[bill,2], "&committeeName=",
                    gsub("&", "%26", gsub(" ", "%20", request[bill,3])), sep = "")

      tbl <- fetch(path)
      if(is.null(tbl)) {
        return(NULL)
      }

      out <- c(out, tbl)
    }
    names(out) <- paste(request[,1], request[,2], request[,3], sep = "//")
  }
  return(out)
}
