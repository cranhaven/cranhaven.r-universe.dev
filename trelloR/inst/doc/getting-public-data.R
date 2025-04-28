## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)

## ---- results='hide'----------------------------------------------------------
library(trelloR)
board = "https://trello.com/b/wVWPK9I4/r-client-for-the-trello-api"
param = list(fields = "id,name,idList,labels")
cards = get_board_cards(board, query = param, limit = 5)

## -----------------------------------------------------------------------------
cards[, 1:3]

## ---- results='hide'----------------------------------------------------------
get_card_updates = function(id, ...) {
  get_resource(parent = "card", child  = "actions", id = id,
               filter = "updateCard", ...)
}
card_updates = get_card_updates(cards[["id"]][4])

## -----------------------------------------------------------------------------
card_updates[, 1:5]

## ---- results='hide'----------------------------------------------------------
error = get_card_actions(id = "wrong_id", on.error = "message")

## -----------------------------------------------------------------------------
error

## -----------------------------------------------------------------------------
sessionInfo()

