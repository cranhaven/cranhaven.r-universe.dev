# Class defined for future use


#' Staged.tree
#'
#'  A staged tree is an event tree embellished with colours using a probabilistic measure. 
#'  Two situations are said to be in the same stage if they have equivalent probabilistic space and
#'  identical conditional probabilities. Each stage is associated with a different colour.
#'
#'
#' @slot event.tree Event.tree.
#'
#'@include event_tree.R

setClass(
  "Staged.tree",
  representation(
    event.tree = "Event.tree"
  )
)
