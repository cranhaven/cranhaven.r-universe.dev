#' Collect Model Equations and Data per Tree
#'
#' @description Helper function that collects model equation and data per tree
#'   for further analysis.
#'
#' @return A list. If \code{autosave = TRUE}, the list is also saved in the
#'   current working directory.
#'
#' @inheritParams fit_mpt
#' @param include_data If \code{FALSE} (the default) the response frequencies
#'   are not part of the output, but only the number of observations per tree.
#'   If \code{TRUE}, the full data is part of the output.
#' @param autosave If \code{TRUE} (the default) the results are automatically
#'   saved in the current working directory in a file with name derived from
#'   both model and data.
#'
#' @importFrom utils read.table
#' @export
get_info <- function(
  model
  , dataset
  , data
  , id = NULL
  , condition = NULL
  , include_data = FALSE
  , core = NULL
  , autosave = TRUE
) {

  # catch the function call that was used,
  # and other stuff that should be save along the results
  matched_call <- match.call()
  ##used_model <- utils::read.table(model, skip = 1, stringsAsFactors = FALSE)

  # prepare data ----
  if (missing(data)) {
    data <- as.data.frame(readr::read_csv(dataset))
  }

  if(is.null(condition)) {
    data$ExpCond <- "no_condition"
    condition <- "ExpCond"
  }

  if (is.null(data[[id]])) {
    stop("id column is not in data.", call. = FALSE)
  }

  if(is.null(id)) {
    data$Subject <- 1:nrow(data)
    id <- "Subject"
  }

  # Ensure that all variables are character
  data$ExpCond <- as.character(data[[condition]])
  data$Subject <- as.character(data[[id]])


  # check MPT file
  mpt_model <- TreeBUGS::readEQN(model)

  if(!is.data.frame(mpt_model)) {
    "I can't comprehend your .eqn file."
  }


  # remove extraneous colums and check if all specified columns are present
  # in data
  freq_cols <- get_eqn_categories(model)
  valid_cols <- c(id, condition, freq_cols)
  check_cols <- valid_cols %in% colnames(data)

  if(!all(check_cols)) {
    stop("Variable \"", paste(valid_cols[!check_cols], collapse = ", "), "\" not found in data.frame.")
  }

  data <- data[, valid_cols]


  # Check NAs ----
  nas_found <- unlist(lapply(X = data, FUN = anyNA))

  if(any(nas_found)) {
    stop("Variable \"", paste(valid_cols[nas_found], collapse = ", "), "\" contains missing values.")
  }

  # Check whether freqencies are integer ----
  not_integer <- unlist(lapply(X = data[, freq_cols], FUN = function(x) {
    any(as.integer(x)!=x)
  }
  ))

  if(any(not_integer)) {
    stop("Variable \"", paste(freq_cols[not_integer], collapse = ", "), "\" contains non-integer values.")
  }

  # Ensure that id and condition are character, also drops unused levels
  data[[id]] <- as.character(data[[id]])
  data[[condition]] <- as.character(data[[condition]])

  # summarize the design of your study
  participants <- table(data[[condition]])
  trees <- get_eqn_trees(model)
  names(trees) <- freq_cols
  n_per_tree <- vector(mode = "list", length = length(unique(trees)))
  names(n_per_tree) <- unname(unique(trees))

  model_branches <- try(read.EQN.model(model))
  try(names(model_branches) <- unname(unique(trees)))

  for(i in trees) {
    n_per_tree[[i]] <- rowSums(data[, names(trees[trees == i])])
  }
  if (include_data) {
    data_out <- data
  } else {
    data_out <- NULL
  }

  data_tree <- cbind(data[,c(id, condition)], n_per_tree)

  out <- list(
    model = basename(model)
    , model_eqn = mpt_model
    , model_branches = model_branches
    , dataset = basename(dataset)
    , data = data_out
    , data_tree = data_tree
    , core = core
  )
  if (autosave) {
    outname <- make.names(paste(basename(dataset), basename(model), "info", sep = "_"))
    assign(outname, out)
    save(list = outname, file = paste0(outname, ".RData"))
  }
  return(out)

}

read.EQN.model <- function (model.filename)
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
    tmp.in <- read.table(model.filename, skip = 1, stringsAsFactors = FALSE)
    tmp.ordered <- tmp.in[order(tmp.in$V1), ]
    tmp.spl <- split(tmp.ordered, factor(tmp.ordered$V1))
    tmp.spl <- lapply(tmp.spl, function(d.f) d.f[order(d.f[,
        2]), ])
    model <- lapply(tmp.spl, parse.eqn)
    names(model) <- NULL
    model
}

