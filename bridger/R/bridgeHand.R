#' @title bridgeHand
#'
#' @description Generate a bridge hand
#'
#' @note
#' To change the hand evaluation pass high card values (HCValues) and shape values (shapeValues) in the arguments.
#'
#'   HCValues is a string of five digits specifying the value of the Ace, King, Queen, Jack and 10.
#'   The default is the Milton Work scale of 4, 3, 2, 1, 0.
#'   shapeValues is a string of eight digits specifying the value of a suit with no cards/"Void", 1-card/"Singleton", ... 7-cards.
#'   The default is c(3, 2, 1, 0, 0, 1, 2, 3)
#'   Losing Trick Count (LTCSchema) 'Original' or 'New' as described at https://en.wikipedia.org/wiki/Losing-Trick_Count.
#'   This assumes a fit will be found.  It is currently not implemented.
#'
#' @return List: Hand ID, Dealer, Hand graphic, Hand points, Hand shape, vulnerability
#'
#' @param handNumber An integer for generating a hand, or "auto" to use a random number generator
#' @param seat If not false, makes the specified seat South and dealer, so all bidding starts with South and the specified hand type
#' @param createGraphic Whether the graphic should be created
#' @param LTC Whether to include losing trick count - FALSE for none, "original" or "new" for schema
#' @param ... Other parameters used in  hand evaluation
#'
#' @examples
#' \dontrun{
#' # Produce a bridge hand
#' hand <- bridgeHand()
#'
#' # Produce a bridge hand '500' ensuring South as dealer
#' hand500 <- bridgeHand(handNumber = 500, seat = "S") # Seat can be any compass point
#' }
#' @export

bridgeHand <- function(handNumber = "auto", seat = FALSE, createGraphic = TRUE, LTC = "original", ...) {

  # Check for valid handNumber
  if (handNumber != "auto" & !is.numeric(handNumber)) {
    stop("Only numeric seeds allowed for handNumbers")
  }

  # Apply if more than one handId given ----
  if (length(handNumber) > 1) {
    lapply(handNumber, bridgeHand, seat = seat, createGraphic = createGraphic, LTC = LTC, ...)
  }

  # Parse out the optional parameters ----
  args <- as.list(list(...))

  if ("HCValues" %in% names(args)) {
    HCValues <- args$HCValues
    stopifnot(length(HCValues) == 5, all(is.numeric(HCValues)))
  } else {
    HCValues <- c(4, 3, 2, 1, 0)
  }

  if ("shapeValues" %in% names(args)) {
    shapeValues <- args$HCValues
    stopifnot(length(shapeValues) == 8, all(is.numeric(shapeValues)))
  } else {
    shapeValues <- c(3, 2, 1, 0, 0, 1, 2, 3)
  }

  if ("LTC" != FALSE) {
    LTCSchema <- LTC
  } else {
    LTCSchema <- FALSE
  }

  # Two optional parameters to make the deal a little wacky by affecting sample()'s probability
  # Random effects, rather than direct control of wackiness of the deal

  if ("wackyFrom" %in% names(args)) {
    wackyFrom <- args$wackyFrom
    stopifnot(length(wackyFrom) == 1, all(is.numeric(wackyFrom)))
  } else {
    wackyFrom <- 1
  }

  if ("wackyTo" %in% names(args)) {
    wackyTo <- args$wackyTo
    stopifnot(length(wackyTo) == 1, all(is.numeric(wackyTo)))
  } else {
    wackyTo <- 1
  }

  # Constants ----
  suits <- c("S", "H", "D", "C")
  compassPoints <- c("N", "E", "S", "W")

  # Set seed - Use given seed or choose a random one ----
  if (handNumber != "auto") {
    handNo <- handNumber
  } else {
    handNo <- round(runif(1) * 10000000, 0)
  }

  set.seed(handNo)

  # Set dealer and vulnerability
  # TODO This will be fixed on the same seed number - Check?
  vuln <- c("None", "NS", "EW", "Both")[sample(1:4, 1)]

  # If a specified seat is given, then always set dealer to that seat
  if (seat != FALSE) {
    dealer <- "S"
  } else {
    dealer <- compassPoints[handNo %% 4 + 1]
  }

  # Create pack and shuffle ----
  # Create pack
  pack <- expand.grid(rank = c("A", 2:9, "T", "J", "Q", "K"), suit = suits) %>%
    as_tibble(.name_repair = "minimal") %>%
    mutate(card = paste(suit, rank, sep = "-"))

  # Divide cards into hands
  for (i in 1:4) {
    temp <- sample(pack$card, 13,
      replace = FALSE,
      prob = rep(seq(from = wackyFrom, to = wackyTo, length.out = 13), 5 - i)
    ) %>%
      as_tibble(.name_repair = "minimal") %>%
      separate(value, sep = "-", into = c("suit", "rank")) %>%
      mutate(
        suit = factor(suit, levels = c("S", "H", "D", "C")),
        rank = factor(rank, levels = c("A", "K", "Q", "J", "T", 9:2, " "))
      ) %>%
      arrange(suit, rank) %>%
      unite("card", sep = "-")

    colnames(temp) <- compassPoints[i]

    assign(glue::glue("hand{i}"), temp)

    pack <- pack %>%
      filter(!card %in% unname(unlist(temp)))
  }

  # Assemble pack
  pack <- hand1 %>%
    cbind(hand2) %>%
    cbind(hand3) %>%
    cbind(hand4)

  # Extract a hand at a time
  for (j in compassPoints) {
    temp_hand <- pack[j] %>%
      bind_cols(order = 1:13, .) %>%
      separate(!!j, sep = "-", into = c("suit", "rank")) %>%
      mutate(suit = factor(suit, levels = c("S", "H", "D", "C")), rank = factor(rank, levels = c("A", "K", "Q", "J", "T", 9:2, " ", "10"))) %>%
      arrange(suit, rank) %>%
      pivot_wider(names_from = "suit", values_from = "rank") %>%
      select(-order)

    # Add back suits, if missing
    if (all(!colnames(temp_hand) %in% "S")) {
      temp_hand <- cbind(temp_hand, S = c("Void", rep(NA, 12)))
    }

    if (all(!colnames(temp_hand) %in% "H")) {
      temp_hand <- cbind(temp_hand, H = c("Void", rep(NA, 12)))
    }

    if (all(!colnames(temp_hand) %in% "D")) {
      temp_hand <- cbind(temp_hand, D = c("Void", rep(NA, 12)))
    }

    if (all(!colnames(temp_hand) %in% "C")) {
      temp_hand <- cbind(temp_hand, C = c("Void", rep(NA, 12)))
    }

    temp_hand <- temp_hand %>%
      select(S, H, D, C)


    # Remove the NAs and then put them back in at the end
    for (i in suits) {
      temp_suit <- na.omit(temp_hand[i])

      while (nrow(temp_suit) < 13) {
        temp_suit <- rbind(temp_suit, NA)
      }

      temp_hand[i] <- temp_suit
    }

    temp_hand <- temp_hand %>%
      filter_all(any_vars(!is.na(.)))

    temp_hand <- temp_hand %>%
      replace(is.na(.), " ")

    assign(glue::glue("hand{j}"), temp_hand)
  }

  # Rearrange hands so that South is always the dealer
  if (seat != FALSE) {
    otherHands <- setdiff(compassPoints, seat)
    hand_temp_S <- get(glue::glue("hand{seat}"))
    hand_temp_W <- get(glue::glue("hand{otherHands[1]}"))
    hand_temp_N <- get(glue::glue("hand{otherHands[2]}"))
    hand_temp_E <- get(glue::glue("hand{otherHands[3]}"))

    handS <- hand_temp_S
    handW <- hand_temp_W
    handN <- hand_temp_N
    handE <- hand_temp_E
  }

  # Assess hands ----
  # Count high-card points
  names(HCValues) <- c("A", "K", "Q", "J", "10")

  points <- tibble(Hand = compassPoints, HC = 0L, Shape = 0L, LTC = 0L)

  for (i in compassPoints) {
    temp <- get(glue::glue("hand{i}")) %>%
      rowid_to_column() %>%
      pivot_longer(-rowid) %>%
      filter(value != " ") %>%
      select(value) %>%
      table() %>%
      as_tibble(.name_repair = "minimal")

    points[points$Hand == i, "HC"] <- round(sum(
      unname(unlist(temp[temp$. == "A", "n"])) * HCValues[["A"]],
      unname(unlist(temp[temp$. == "K", "n"])) * HCValues[["K"]],
      unname(unlist(temp[temp$. == "Q", "n"])) * HCValues[["Q"]],
      unname(unlist(temp[temp$. == "J", "n"])) * HCValues[["J"]],
      unname(unlist(temp[temp$. == "T", "n"])) * HCValues[["10"]]
    ), 0)
  }

  # Identify voids, singletons and long suits for shape points
  for (i in compassPoints) {
    hand_shape <- get(glue::glue("hand{i}")) %>%
      rowid_to_column() %>%
      pivot_longer(-rowid) %>%
      filter(value != " ") %>%
      group_by(name) %>%
      summarise(shape = max(rowid), .groups = "drop") %>%
      ungroup() %>%
      select(shape) %>%
      unname() %>%
      unlist()

    temp_points <-
      sum(shapeValues[2] * (hand_shape == 1)) +
      sum(shapeValues[3] * (hand_shape == 2)) +
      sum(shapeValues[4] * (hand_shape == 3)) +
      sum(shapeValues[5] * (hand_shape == 4)) +
      sum(shapeValues[6] * (hand_shape == 5)) +
      sum(shapeValues[7] * (hand_shape == 6)) +
      sum(shapeValues[8] * (hand_shape == 7))

    # Look for Void and add 2 points (3 - 1 for counted singleton)
    temp_points <- temp_points + sum(get(glue::glue("hand{i}"))[1, ] == "Void") * shapeValues[1]
    points[points$Hand == i, "Shape"] <- temp_points
  }

  # Calculate total points
  points <- points %>%
    rowwise() %>%
    mutate(Total = sum(HC + Shape)) %>%
    relocate(LTC, .after = Total)

  # Calculate losing trick count ---
  if (LTCSchema != FALSE) {
    for (i in compassPoints) {
      current_hand <- get(glue::glue("hand{i}")) %>%
        slice(1:3) %>%
        mutate(across(.cols = everything(), as.character)) %>%
        mutate(across(.cols = everything(), ~ ifelse(.x %in% c("A", "K", "Q", " ", "Void"), .x, "x")))

      # Start with 0 or 1 if there is no ace in the suit
      ltc <- ifelse(any(stringr::str_detect(unname(unlist(current_hand)), "A")), 0, 1)

      for (j in suits) {
        suit_shape <- select(current_hand, all_of(j)) %>%
          unname() %>%
          unlist() %>%
          glue::glue_collapse()

        # Alternative counting and adjustments possible
        # https://en.wikipedia.org/wiki/Losing-Trick_Count#Refinements
        if (LTCSchema == "original") {
          temp_ltc <- 0 +
            stringr::str_count(suit_shape, "Void|A  |AK |AKQ") * 0 + # For completeness
            stringr::str_count(suit_shape, "AQ |Ax |AKx|AQx|K  |KQ |Kx |KQx|Q  |x  ") * 1 +
            stringr::str_count(suit_shape, "Axx|Kxx|Qx |Qxx|xx ") * 2 +
            stringr::str_count(suit_shape, "xxx") * 3
        } else if (LTCSchema == "new") {
          temp_ltc <- 0 +
            stringr::str_count(suit_shape, "Void|A  |AK |AKQ") * 0 + # For completeness
            stringr::str_count(suit_shape, "AKx") * 0.5 +
            stringr::str_count(suit_shape, "AQx|Ax |AQ ") * 1 +
            stringr::str_count(suit_shape, "K  |Q  |x  |Axx|KQx|KQ |Kx ") * 1.5 +
            stringr::str_count(suit_shape, "Kxx") * 2 +
            stringr::str_count(suit_shape, "Qx |Qxx|xx ") * 2.5 +
            stringr::str_count(suit_shape, "xxx") * 3
        }
        ltc <- ltc + ceiling(temp_ltc) # Rounded up and add to previous
      }

      points[points$Hand == i, "LTC"] <- ltc
    }
  } else {
    points[points$Hand == i, "LTC"] <- NA
  }

  # Rename LTC column in points
  points <- points %>%
    rename_with(~ glue::glue("LTC ({stringr::str_to_title(LTCSchema)})"), LTC)

  # Create the graphic object ----
  if (createGraphic) {
    hand_graphic <- createGraphic(handNo, handN, handE, handS, handW, dealer, vuln, points)
  } else {
    hand_graphic <- "Not requested"
  }

  # Collate the hand shapes
  handShapes <- tibble(
    N = colSums(handN != " "),
    E = colSums(handE != " "),
    S = colSums(handS != " "),
    W = colSums(handW != " ")
  )

  # Return
  invisible(list(
    id = handNo, # ID
    dealer = dealer, # Dealer
    graphic = hand_graphic, # Hand graphic
    handPoints = points[, c("Hand", "HC")], # Hand points
    handShapes = handShapes, # Hand shape
    vuln = vuln # Vulnerability
  ))
}
