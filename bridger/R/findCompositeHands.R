#' @title find_weak1NT_LHOx
#'
#' @description Find hands where South will open a weak 1NT and West will likely double
#'
#' @return id and seat of a compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param HC_high The maximum number of high-card points
#' @param cardLen_low The minimum length of a suit
#' @param cardLen_high The maximum length of a suit
#' @param pointsForDouble Minimum number of points for West to double

find_weak1NT_LHOx <- function(HC_low = 12, HC_high = 14, cardLen_low = 2, cardLen_high = 5, pointsForDouble = 15) {
  repeat {
    # Identify a weak 1NT hand
    weakNT <- find_weakNT()

    dealer <- weakNT$seat

    # Re-download the hand, ensuring that South is always now the dealer
    weakNT <- bridgeHand(weakNT$id, weakNT$seat, createGraphic = FALSE)

    # Check if West would double or bid
    HC_lho <- weakNT$handPoints[4, 2]
    shape_lho <- weakNT$handShapes[, 4]


    result <- case_when(
      HC_lho < pointsForDouble ~ FALSE, # Test for points to double
      any(shape_lho < 2) | any(shape_lho > 5) ~ FALSE, # Test for flat hand
      TRUE ~ TRUE
    )

    if (result) {
      return(invisible(list(id = weakNT$id, seat = dealer))) # Dealer of the original hand
    }
  }

  return("Error in find_weak1NT_LHOx")
}

#' @title find_weak1NT_LHObid
#'
#' @description Find hands where South will open a weak 1NT and West will likely bid
#'
#' @return id and seat of a compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param cardLen_low The minimum length of a suit
#'

find_weak1NT_LHObid <- function(HC_low = 7, cardLen_low = 6) {
  repeat {
    # Identify a weak 1NT hand
    weakNT <- find_weakNT()

    id <- weakNT$id
    seat <- weakNT$seat

    # Re-download the hand, ensuring that South is always now the dealer
    weakNT <- bridgeHand(id, seat, createGraphic = FALSE)

    # Check if West (left-hand opponent) would bid
    HC_lho <- weakNT$handPoints[4, 2]
    shape_lho <- weakNT$handShapes[, 4]

    result <- case_when(
      HC_lho < HC_low ~ FALSE, # Test for points to bid
      # Test for one of 1) Long suit, e.g. 6, 2) two medium long suits, e.g. 5 or 3) "45" in the majors
      any(cardLen_low <= shape_lho) ~ TRUE,
      sum(cardLen_low - 1 <= shape_lho) == 2 ~ TRUE,
      sum(shape_lho[c(1, 2), ]) == 9 ~ TRUE,
      TRUE ~ FALSE
    )

    if (result) {
      return(invisible(list(id = id, seat = seat))) # Dealer is from the first call to find compliant hand
    }
  }

  return("Error in find_weak1NT_LHObid")
}

#' @title find_weak1NT_RHObid
#'
#' @description Find hands where South will open a weak 1NT, East and North with pass, and West will likely bid
#'
#' @return id and seat of a compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param cardLen_low The minimum length of a suit
#'
#' @return id and seat of a compliant hand

find_weak1NT_RHObid <- function(HC_low = 7, cardLen_low = 6) {
  repeat {
    # Identify a weak 1NT hand
    weakNT <- find_weakNT()

    id <- weakNT$id
    seat <- weakNT$seat

    # Re-download the hand, ensuring that South is always now the dealer
    weakNT <- bridgeHand(id, seat, createGraphic = FALSE)

    # Check if Wast (left-hand opponent) would bid
    HC_lho <- weakNT$handPoints[4, 2]
    shape_lho <- weakNT$handShapes[, 4]

    result <- case_when(
      HC_lho < HC_low ~ FALSE, # Test for points to bid
      # Test for one of 1) Long suit, e.g. 6, 2) two medium long suits, e.g. 5 or 3) "45" in the majors
      any(cardLen_low <= shape_lho) ~ TRUE,
      sum(cardLen_low - 1 <= shape_lho) == 2 ~ TRUE,
      sum(shape_lho[c(1, 2), ]) == 9 ~ TRUE,
      TRUE ~ FALSE
    )

    # If LHO will bid, then fail
    if (result) {
      next
    }

    # Check if partner would likely bid
    HC_part <- weakNT$handPoints[1, 2]
    shape_part <- weakNT$handShapes[, 1]

    result <- case_when(
      HC_part >= 11 ~ TRUE, # Test for points to bid again
      # Test for one of 1) Long suit, e.g. 6, 2) two medium long suits, e.g. 5 or 3) "45" in the majors
      any(shape_part[c(1, 2), ] >= 5) ~ TRUE, # 5 card major
      any(shape_part[c(3, 4), ] >= 6) ~ TRUE, # 6 card minor, assume playing Stayman
      TRUE ~ FALSE
    )

    # If partner will bid, then fail
    if (result) {
      next
    }

    # Check if East (right-hand opponent) would bid
    HC_rho <- weakNT$handPoints[2, 2]
    shape_rho <- weakNT$handShapes[, 2]

    result <- case_when(
      HC_rho < HC_low ~ FALSE, # Test for points to bid
      # Test for one of 1) Long suit, e.g. 6, 2) two medium long suits, e.g. 5 or 3) "45" in the majors
      any(cardLen_low <= shape_rho) ~ TRUE,
      sum(cardLen_low - 1 <= shape_rho) == 2 ~ TRUE,
      sum(shape_rho[c(1, 2), ]) == 9 ~ TRUE,
      TRUE ~ FALSE
    )

    # If LHO will bid, then fail
    if (result) {
      return(invisible(list(id = id, seat = seat)))
    }
  }

  return("Error in find_weak1NT_RHObid")
}

#' @title find_1major_jacoby2NT
#'
#' @description Find hands where South opens one of a major, and North will bid 2NT, to show 4 card support and points for game
#'
#' @return id and seat of a compliant hand
#'
#' @param HC_low The minimum number of high-card points
#' @param cardLen_low The minimum length of a suit


find_1major_jacoby2NT <- function(HC_low = 13, cardLen_low = 4) {
  repeat {
    # Identify a weak 1NT hand
    firstHand <- find_1major()

    dealer <- firstHand$seat

    # Re-download the hand, ensuring that South is always now the dealer
    firstHand <- bridgeHand(firstHand$id, firstHand$seat, createGraphic = FALSE)

    # Check if West would double or bid
    HC_part <- firstHand$handPoints[1, 2]
    shape_part <- firstHand$handShapes[, 1]

    # Which major was selected - 1 = Spades, 2 = Hearts
    suitbid <- as.numeric(ifelse(firstHand$handShapes[1, 3] >= firstHand$handShapes[2, 3], 1, 2))

    result <- case_when(
      HC_part < HC_low ~ FALSE, # Test for points to double
      shape_part[suitbid, ] < cardLen_low ~ FALSE, # Test length in bid suit
      # Check that partner's spade suit isn't longer than hearts, if hearts is the opening bid
      all(suitbid == 2, shape_part[1, ] > shape_part[2, ]) ~ FALSE,
      TRUE ~ TRUE
    )

    if (result) {
      return(invisible(list(id = firstHand$id, seat = dealer))) # Dealer of the original hand
    }
  }

  return("Error in find_1major_jacoby2NT")
}
