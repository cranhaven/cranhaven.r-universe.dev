#' @title find_any
#'
#' @description Return any bridge hand - May not be an opener
#'
#' @return id and seat of compliant hand


find_any <- function() {
  testHand <- bridgeHand(createGraphic = FALSE)

  seat <- sample(1:4, 1)

  # Return the id and seat
  return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
}

#' @title find_opener
#'
#' @description Return a bridge hand that is likely to open
#'
#' @return id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points

find_opener <- function(HC_low = 12) {
  repeat {
    testHand <- bridgeHand(createGraphic = FALSE)

    for (seat in 1:4) {
      HC <- testHand$handPoints[seat, 2]

      # Test hand for conditions
      result <- case_when(
        HC < HC_low ~ FALSE, # Test for points
        TRUE ~ TRUE
      )

      # Return the id and seat
      if (result) {
        return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
      }
    }
  }

  return("Failure in find_any")
}

#' @title find_1major
#'
#' @description Return a bridge hand that will open 1 of a major
#'
#'  Assumes that a 5 card minor will be bid before 4 card major, except if "canape" set to TRUE, then a 6 card minor
#'  will be opened before a 4 card major
#'
#'  Assumes a weak 1NT, so HC_low is the first point outside the range of 1NT.
#'
#' @return id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points, otherwise 2-level bid is possible
#' @param cardLen_min The minimum number of cards in the major
#' @param canape Whether a 4 card major will be opened before a 5 card minor

find_1major <- function(HC_low = 15, HC_high = 19, cardLen_min = 4, canape = FALSE) {
  repeat {
    testHand <- bridgeHand(createGraphic = FALSE)

    for (seat in 1:4) {
      HC <- testHand$handPoints[seat, 2]
      shape <- testHand$handShapes[, seat]

      # Test hand for conditions ~ Need 15 points to avoid 1NT
      result <- case_when(
        HC < HC_low | HC > HC_high ~ FALSE, # Test for points
        all(shape[1, ] < cardLen_min, shape[2, ] < cardLen_min) ~ FALSE, # Both majors are too short
        all(shape[3, ] < (cardLen_min + 1 + canape * 1), shape[4, ] < (cardLen_min + 1 + canape * 1)) ~ FALSE, # Minor suit, won't be bid first

        TRUE ~ TRUE
      )

      # Return the id and seat
      if (result) {
        return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
      }

      # Test hand for conditions ~ Need 12-14 if 5 card suit
      result <- case_when(
        HC < HC_low - 3 ~ FALSE, # Test for points
        all(shape[1, ] < (cardLen_min + 1), shape[2, ] < (cardLen_min + 1)) ~ FALSE, # Both majors are too short
        any(shape[3, ] >= (cardLen_min + 1 + canape * 1), shape[4, ] >= (cardLen_min + 1 + canape * 1)) ~ FALSE, # Minor suit, won't be bid first

        TRUE ~ TRUE
      )

      # Return the id and seat
      if (result) {
        return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
      }
    }
  }

  return("Failure in find_1major")
}

#' @title find_weakNT
#'
#' @description Find hands that comply with a no trump opening
#'
#' @return id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_weakNT <- function(HC_low = 12, HC_high = 14, cardLen_low = 2, cardLen_high = 4) {
  repeat {
    testHand <- bridgeHand(createGraphic = FALSE)

    for (seat in 1:4) {
      HC <- testHand$handPoints[seat, 2]
      shape <- testHand$handShapes[, seat]

      # If 5 card major, but less than 4 points, K, Q + J, Q, or J, then allow 5
      # Don't pass points by hand & suit, so can't extract data for now

      # Test hand for conditions
      result <- case_when(
        HC < HC_low ~ FALSE, # Test for points
        HC > HC_high ~ FALSE, # Test for points
        any(shape < cardLen_low) ~ FALSE, # Test for length
        any(shape[1:2, ] > cardLen_high) ~ FALSE, # Test for length - majors
        any(shape[3:4, ] > cardLen_high + 1) ~ FALSE, # Test for length - minors (allow cardLen_high+1)
        sum(shape == cardLen_low) > 1 ~ FALSE, # Test for only 1 two-card suit
        TRUE ~ TRUE
      )

      # Return the id and seat
      if (result) {
        return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
      }
    }
  }

  # Will never reach here
  return("Failure in find_weakNT")
}

#' @title find_strongNT
#'
#' @description Find hands that comply with a weak no trump opening
#'
#' @return id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_strongNT <- function(HC_low = 15, HC_high = 17, cardLen_low = 2, cardLen_high = 5) {
  repeat{
    testHand <- bridgeHand(createGraphic = FALSE)

    for (seat in 1:4) {
      HC <- testHand$handPoints[seat, 2]
      shape <- testHand$handShapes[, seat]

      # Test hand for conditions
      result <- case_when(
        HC < HC_low ~ FALSE, # Test for points
        HC > HC_high ~ FALSE, # Test for points
        any(shape < cardLen_low) ~ FALSE, # Test for length
        any(shape[1:2, ] > cardLen_high) ~ FALSE, # Test for length - majors
        any(shape[3:4, ] > cardLen_high) ~ FALSE, # Test for length - minors
        sum(shape == cardLen_low) > 1 ~ FALSE, # Test for only 1 two-card suit
        TRUE ~ TRUE
      )

      # Return the id and seat
      if (result) {
        return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
      }
    }
  }

  return("Failure in find_strong")
}

#' @title find_strong
#'
#' @description Find hands that are strong enough to open strong
#'
#' @return id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_strong <- function(HC_low = 19, HC_high = 35, cardLen_low = 1, cardLen_high = 5) {
  repeat{
    testHand <- bridgeHand(createGraphic = FALSE)

    for (seat in 1:4) {
      HC <- testHand$handPoints[seat, 2]
      shape <- testHand$handShapes[, seat]

      # Test hand for conditions
      result <- case_when(
        HC < HC_low | HC > HC_high ~ FALSE, # Test for points
        any(shape <= cardLen_low) | any(shape >= cardLen_high) ~ FALSE, # Test for having a long or short suit
        TRUE ~ TRUE
      )

      # Return the id and seat
      if (result) {
        return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
      }
    }
  }

  return("Failure in find_strong")
}


#' @title find_4441
#'
#' @description Find hands that comply with a 4441 shape and opening point count
#'
#' @return id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_4441 <- function(HC_low = 12, HC_high = 35, cardLen_low = 5, cardLen_high = 13) {
  repeat{
    testHand <- bridgeHand(createGraphic = FALSE)

    for (seat in 1:4) {
      HC <- testHand$handPoints[seat, 2]
      shape <- testHand$handShapes[, seat]

      # Test hand for conditions
      result <- case_when(
        HC < HC_low | HC > HC_high ~ FALSE, # Test for points
        sum(shape == 4) != 3 ~ FALSE, # Test for length
        TRUE ~ TRUE
      )

      # Return the id and seat
      if (result) {
        return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
      }
    }
  }

  return("Failure in find_4441")
}

#' @title find_2preempt
#'
#' @description Find hands that are likely to preempt at the 2 level in a major
#'
#' @return id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_2preempt <- function(HC_low = 5, HC_high = 10, cardLen_low = 6, cardLen_high = 7) {
  repeat{
    testHand <- bridgeHand(createGraphic = FALSE)

    for (seat in 1:4) {
      HC <- testHand$handPoints[seat, 2]
      shape <- testHand$handShapes[, seat]

      # Test hand for conditions
      result <- case_when(
        HC < HC_low | HC > HC_high ~ FALSE, # Test for points
        sum(shape[1:2, ] == cardLen_low) != 1 | sum(shape == cardLen_high) >= 1 ~ FALSE, # Test for only 1 two-card suit
        TRUE ~ TRUE
      )

      # Return the id and seat
      if (result) {
        return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
      }
    }
  }

  return("Failure in find_2preempt")
}

#' @title find_3preempt
#'
#' @description Find hands that are likely to preempt at the 3 level
#'
#' @return FALSE if not compliant, or id and seat of compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit

find_3preempt <- function(HC_low = 5, HC_high = 10, cardLen_low = 7, cardLen_high = 8) {
  repeat{
    testHand <- bridgeHand(createGraphic = FALSE)

    for (seat in 1:4) {
      HC <- testHand$handPoints[seat, 2]
      shape <- testHand$handShapes[, seat]

      # Test hand for conditions
      result <- case_when(
        HC < HC_low | HC > HC_high ~ FALSE, # Test for points
        sum(shape == cardLen_low) < 1 ~ FALSE, # Test for only 1 two-card suit
        TRUE ~ TRUE
      )

      # Return the id and seat
      if (result) {
        return(invisible(list(id = testHand$id, seat = c("N", "E", "S", "W")[seat])))
      }
    }
  }
  return("Failure in find_3preempt")
}
