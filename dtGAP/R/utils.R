.simplify_pred <- function(tab, id, nam) {
  # @references https://github.com/trangdata/treeheatr/blob/85be4a61e35a62285c95b553f03729721bb18a0b/R/utils.R
  # Modified from partykit:::.simplify_pred()
  # https://github.com/cran/partykit/blob/597245ef3dfc98411ce919b74c68ba565f077c47/R/party.R#L497-L520
  # to add as.numeric() after combining the list components with do.call()

  if (all(sapply(tab, length) == 1) & all(sapply(tab, is.atomic))) {
    ret <- do.call("c", tab) %>% as.numeric()
    names(ret) <- names(tab)
    ret <- if (is.factor(tab[[1]])) {
      factor(
        ret[as.character(id)],
        levels = 1:length(levels(tab[[1]])),
        labels = levels(tab[[1]]),
        ordered = is.ordered(tab[[1]])
      )
    } else {
      ret[as.character(id)]
    }
    names(ret) <- nam
  } else if (length(unique(sapply(tab, length))) == 1 & all(sapply(tab, is.numeric))) {
    ret <- matrix(unlist(tab), nrow = length(tab), byrow = TRUE)
    colnames(ret) <- names(tab[[1]])
    rownames(ret) <- names(tab)
    ret <- ret[as.character(id), , drop = FALSE]
    rownames(ret) <- nam
  } else {
    ret <- tab[as.character(id)]
    names(ret) <- nam
  }
  ret
}


#' Performs transformation on continuous variables.
#'
#' Performs transformation on continuous variables for the heatmap color scales.
#' @references
#' \url{https://github.com/trangdata/treeheatr/blob/85be4a61e35a62285c95b553f03729721bb18a0b/R/utils.R}
#' @param x Numeric vector.
#' @inheritParams sorted_mat
#' @return Numeric vector of the transformed `x`.
#' @export
#' @examples
#' scale_norm(1:5, "normalize")
#'
scale_norm <- function(x,
                       trans_type = c("percentize", "normalize", "scale", "none")) {
  trans_type <- match.arg(trans_type)

  switch(
    trans_type,
    percentize = stats::ecdf(x)(x),
    scale = as.numeric(scale(x)),
    normalize = my_norm(x),
    none = x
  )
}

my_norm <- function(x) {
  x <- x - min(x, na.rm = T)
  x <- x / max(x, na.rm = T)
  x
}


# --- Helpers for custom `fit` parameter in dtGAP() ---

#' Detect model type from a fitted tree object
#' @noRd
detect_model_type <- function(fit) {
  cls <- class(fit)
  if (inherits(fit, "party"))    return("party")
  if (inherits(fit, "rpart"))    return("rpart")
  if (inherits(fit, "C5.0"))     return("C50")
  if (inherits(fit, "train"))    return("caret")
  stop("Unsupported fit class: ", paste(cls, collapse = ", "),
       ". Supported: party, rpart, C5.0, train (caret).")
}

#' Convert a fitted tree object to a party object
#' @noRd
convert_to_party <- function(fit, model_type) {
  switch(model_type,
    party = fit,
    rpart = partykit::as.party(fit),
    C50   = partykit::as.party(fit),
    caret = partykit::as.party(fit$finalModel),
    stop("Cannot convert model type '", model_type, "' to party.")
  )
}

#' Extract variable importance from a fitted tree
#' @noRd
extract_var_imp <- function(fit, model_type) {
  vi <- tryCatch({
    switch(model_type,
      rpart = {
        tmp <- tempfile()
        sink(tmp)
        s <- summary(fit)
        sink()
        s$variable.importance
      },
      party = {
        partykit::varimp(fit)
      },
      C50 = {
        C50::C5imp(fit, metric = "usage")[, "Overall"]
      },
      caret = {
        summary(fit)$variable.importance
      }
    )
  }, error = function(e) NULL)

  if (is.null(vi) || length(vi) == 0) return(NULL)
  round(vi / sum(vi), 2)
}
