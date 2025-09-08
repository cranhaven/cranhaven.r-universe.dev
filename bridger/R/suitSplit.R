#' @title suitSplit
#'
#' @description Provides the probabilities with with a number of cards  will split between two hands, given a number of unknown cards in each hand.
#'   Unknown hands are assumed to be West and East.
#'
#'   If there is no information to indicate different numbers of unknown cards in both hands, then symmetrical probabilities will be returned.
#'   However, if one hand is expected to have a different number of cards to the other, then these can be specified.  For example, if during the
#'   bidding East overcalled in spades, indicating a 5 card suit, then when looking at hearts, East has fewer cards.  While the number of assumed
#'   cards in West's hand is 13 ('cards_W = 13'), the assumed cards in East should be reduced to 8 ('cards_E = 8')
#'
#' @param missingCards The number of cards held by the two hands
#' @param cards_W Cards in West hands
#' @param cards_E Cards in East hands
#'
#' @return Tibble of probabilities
#'
#' @examples
#' suitSplit(missingCards = 6, cards_W = 13, cards_E = 8)
#' @export

suitSplit <- function(missingCards = 5, cards_W = 13, cards_E = 13) {
  outTable <- tibble(
    "Cards held by West" = 0:missingCards,
    "Cards held by East" = missingCards:0,
    Probability = 0
  )

  for (i in outTable$"Cards held by West") {
    unknown_W <- i
    unknown_E <- missingCards - i

    temp <- factorial(unknown_W + unknown_E) / (factorial(unknown_W) * factorial(unknown_E)) *
      (factorial(cards_W) * factorial(cards_E) * factorial(cards_W + cards_E - unknown_W - unknown_E)) /
      (factorial(cards_W + cards_E) * factorial(cards_W - unknown_W) * factorial(cards_E - unknown_E)) *
      ifelse(cards_W == cards_E, (2 - abs(unknown_W == unknown_E)), 1) # This line and the slice function for symmetrical hands

    outTable[i + 1, "Probability"] <- round(temp, 2)
  }

  if (cards_W == cards_E) {
    outTable <- slice(outTable, 0:ceiling(nrow(outTable) / 2))
    colnames(outTable) <- c("Cards in one hand", "Cards in other hand", "Probability")
  }

  xaxis <- outTable[, 1] %>%
    unname() %>%
    unlist()

  if (cards_W == cards_E) {
    subtitleText <- glue::glue("Symmetrical probabilities reflecting {cards_W} unknown cards held by both West and East")
  } else {
    subtitleText <- glue::glue("Asymmetrical probabilities reflecting {cards_W} unknown cards in West and {cards_E} in East")
  }

  graph <- ggplot(outTable) +
    geom_col(aes(x = xaxis, y = Probability, fill = factor(xaxis + 1))) +
    geom_label(aes(
      x = xaxis, y = Probability, label = scales::percent(Probability, accuracy = 0.1),
      vjust = ifelse(Probability > 0.1, 1.6, -0.4)
    ), fill = "white", fontface = "bold", label.size = NA) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = xaxis) +
    scale_fill_manual(values = c(
      "#c72e29", "#016392", "#be9c2e", "#098154", "#fb832d", "red", "steelblue",
      "purple", "darkred", "lightblue", "darkgrey", "darkgreen", "lightgrey"
    )) +
    labs(
      title = glue::glue("Probable distribution of {missingCards} cards between two hands"),
      subtitle = subtitleText,
      x = colnames(outTable[1]), y = NULL
    ) +
    guides(fill = "none") +
    theme_minimal() +
    theme(
      title = element_text(size = rel(1.2)),
      axis.text = element_text(size = rel(1))
    )

  plot(graph)

  outTable$Probability <- scales::percent(outTable$Probability, accuracy = 0.1)

  return(outTable)
}
