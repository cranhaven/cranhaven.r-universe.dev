#' @keywords internal

get_eqn_categories <- function (model.filename)
{
  parse.eqn <- function(x) {
    branches <- unique(x[, 2])
    l.tree <- length(branches)
    tree <- vector("expression", l.tree)
    for (branch in 1:l.tree) {
      tree[branch] <- parse(text = paste(x[x$V2 == branches[branch],
                                           "V3"], collapse = " + "))
    }
    tree
  }
  #browser()
  tmp.in <- utils::read.table(model.filename, skip = 1, stringsAsFactors = FALSE)
  tmp.ordered <- tmp.in[order(tmp.in$V1), , drop = FALSE]
  tmp.spl <- split(tmp.ordered, factor(tmp.ordered$V1))
  tmp.spl <- lapply(tmp.spl, function(d.f) d.f[order(d.f[, 2]), ])
  as.character(unlist(lapply(tmp.spl, function(x) unique(x$V2))))
  # model <- lapply(tmp.spl, parse.eqn)
  # names(model) <- NULL
  # model
}

#' @keywords internal

get_eqn_trees <- function(model_file) {
  
  tmp <- utils::read.table(
    file = model_file
    , skip = 1
    , stringsAsFactors = FALSE
  )
  tmp_ordered <- tmp[order(tmp$V1), , drop = FALSE]
  tmp_split <- split(tmp_ordered, factor(tmp_ordered$V1))
  # print(tmp_split)
  n_cat <- lapply(X = tmp_split, FUN = function(x) {
    length(unique(x$V2))
  })
  as.character(unlist(mapply(FUN = rep, each = n_cat, x = names(tmp_split))))
}