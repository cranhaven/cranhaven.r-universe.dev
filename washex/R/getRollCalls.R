#' Get roll call votes
#'
#' Get an XML containing roll call information for all recorded votes
#'     on a bill
#'
#' @inheritParams getLegislation
#'
#' @return \code{getRollCalls.xml} returns a list of XML objects for each bill.
#'     \code{getRollCalls.summary} and \code{getRollCalls.votes}
#'     return objects of type equal to the
#'     \code{type} argument (defaults to dataframe)
#'
#' @examples
#' votes <- getRollCalls.summary("2007-08", "1001") # get roll call votes
#' if(!is.null(votes)) {
#'   length(votes) # total number of roll call votes recorded
#'   votes$CountYeas[3] # number of yea votes on roll call vote #3
#' }
#'
#' ## example: get member id's for all representatives voting against the bill
#' ## on final passage
#' votes <- getRollCalls.votes("2007-08", "1001")
#' if(!is.null(votes)) {
#'   nay_votesFP <- subset(votes, (Motion == "Final Passage" & Vote == "Nay"))
#'   print(nay_votesFP$MemberId)
#' }
#'
#' @section Note: Due to the nested nature of the resulting document,
#'     we provide various functions to present simplified views of the data
#'     that are compatible with more parsimonious data structures. To see the
#'     full, original data, use \code{getRollCalls.xml} instead.
#'
#' @name getRollCalls
NULL

#' @export
#' @rdname getRollCalls
getRollCalls.xml <- function(biennium, billNumber, paired = TRUE) {
  billNumber <- as.character(billNumber)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getRollCalls for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getRollCalls for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  out <- c()

  for(bill in 1:nrow(request)) {
    path <- paste(prefix,
                  "legislationservice.asmx/GetRollCalls?biennium=",
                  biennium, "&billNumber=", billNumber, sep = "")

    tbl <- fetch(path)
    if(is.null(tbl)) {
      return(NULL)
    }

    out <- c(out, tbl)
  }
  names(out) <- paste(request[,1], request[,2], sep = "//")
  return(out)
}

#' @export
#' @rdname getRollCalls
getRollCalls.summary <- function(biennium, billNumber, paired = TRUE, type = c("df", "list")) {
  type <- rlang::arg_match(type)
  billNumber <- as.character(billNumber)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getRollCalls for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getRollCalls for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      xml <- unname(getRollCalls.xml(request[bill,1], request[bill,2]))
      if(is.null(xml)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(xml[[1]])

      if(length(tbl) > 0) {
        tbl <- purrr::map(tbl, ~ data.frame(Agency = .x[["Agency"]],
                                          BillId = .x[["BillId"]],
                                          Biennium = .x[["Biennium"]],
                                          Motion = .x[["Motion"]],
                                          SequenceNumber = .x[["SequenceNumber"]],
                                          VoteDate = .x[["VoteDate"]],
                                          CountYeas = .x[["YeaVotes"]]$Count,
                                          CountNays = .x[["NayVotes"]]$Count,
                                          CountAbsent = .x[["AbsentVotes"]]$Count,
                                          CountExcused = .x[["ExcusedVotes"]]$Count))

        df <- dplyr::bind_rows(tbl)
        out <- dplyr::bind_rows(out, df)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      xml <- unname(getRollCalls.xml(request[bill,1], request[bill,2]))

      tbl <- purrr::map(XML::xmlToList(xml[[1]]), purrr::flatten)
      tbl <- purrr::map(tbl, ~ toss(.x, c("MembersVoting", "Vote")))
      #tbl <- purrr::map(tbl, ~ toss(.x, "Vote"))
      if(length(tbl) > 0) {
        for(i in 1:length(tbl)) {
          names(tbl[[i]]) <- c("Agency",
                             "BillId",
                             "Biennium",
                             "Motion",
                             "SequenceNumber",
                             "VoteDate",
                             "CountYeas",
                             "CountNays",
                             "CountAbsent",
                             "CountExcused")
        }
        list <- list(tbl)
        names(list) <- request[bill,2]
        out <- c(out, list)
      }
    }
  }
  return(out)
}

#' @export
#' @rdname getRollCalls
getRollCalls.votes <- function(biennium, billNumber, paired = TRUE, type = c("df", "list")) {
  type <- rlang::arg_match(type)
  billNumber <- as.character(billNumber)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getRollCalls for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getRollCalls for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      xml <- unname(getRollCalls.xml(request[bill,1], request[bill,2]))
      if(is.null(xml)) {
        return(NULL)
      }

      tbl <- XML::xmlToList(xml[[1]])
      tbl <- purrr::map(tbl, purrr::flatten)
      tbl <- purrr::map(tbl, ~ toss(.x, c("SequenceNumber", "Count", "MembersVoting")))

      if(length(tbl) > 0) {
        tbl <- purrr::map(tbl, ~ data.frame(Agency = .x[["Agency"]],
                                            BillId = .x[["BillId"]],
                                            Biennium = .x[["Biennium"]],
                                            Motion = .x[["Motion"]],
                                            VoteDate = .x[["VoteDate"]],
                                            MemberId = unlist(purrr::map(.x, "MemberId")),
                                            Name = unlist(purrr::map(.x, "Name")),
                                            Vote = unlist(purrr::map(.x, "VOte"))))
        # yes there is supposed to be a typo on "VOte"

        df <- dplyr::bind_rows(tbl)
        out <- dplyr::bind_rows(out, df)
        out <- out[!duplicated(out),]
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      xml <- unname(getRollCalls.xml(request[bill,1], request[bill,2]))

      tbl <- XML::xmlToList(xml[[1]])
      tbl <- purrr::map(tbl, purrr::flatten)
      tbl <- purrr::map(tbl, ~ toss(.x, c("SequenceNumber", "Count", "MembersVoting")))

      if(length(tbl) > 0) {
        list <- list(tbl)
        names(list) <- request[bill,2]
        out <- c(out, list)
      }
    }
  }
  return(out)
}
