
#' Multiple trilogit fields
#'
#' Methods of `S3` class `multifield3logit` handle multiple `fields3logit`
#' objects simultaneously and permit new `multifield3logit` objects to be
#' easily created by means of the sum operator "`+`".
#'
#' @param x,y,model,object object of class `field3logit` or `multifield3logit`.
#' @inheritParams field3logit
#' @param ... other arguments passed to or from other methods.
#' @param maxitems maximum number of items to be enumerated when an object of
#'   class `multifield3logit` is printed.
#' @param col,legend graphical parameters if `Ternary` package is used.
#' @param i index of the `field3logit` object to be selected.
#' @param drop if `TRUE`, a `field3logit` object is returned if the
#'   subsetted `multifield3logit` object has length one.
#'
#' @returns `S3` object of class `multifield3logit` structured as a named
#' `list`.
#'
#' @seealso
#' [field3logit()].
#'
#' @examples
#' \donttest{
#' data(cross_1year)
#'
#' mod0 <- nnet::multinom(employment_sit ~ ., data = cross_1year)
#' mod0
#'
#' field_Sdur <- field3logit(mod0, 'durationShort',
#'   label = 'Short duration')
#' field_Hfgr <- field3logit(mod0, 'finalgradeHigh',
#'   label = 'High final grade')
#'
#' gg3logit(field_Sdur + field_Hfgr) +
#'   stat_field3logit()
#'   facet_wrap(~ label)
#'
#' refpoint <- list(c(0.7, 0.15, 0.15))
#'
#' field_Sdur <- field3logit(mod0, 'durationShort',
#'   label = 'Short duration', p0 = refpoint, narrows = 1)
#' field_Ldur <- field3logit(mod0, 'durationLong',
#'   label = 'Long duration', p0 = refpoint, narrows = 1)
#' field_Hfgr <- field3logit(mod0, 'finalgradeHigh',
#'   label = 'High final grade', p0 = refpoint, narrows = 1)
#' field_Lfgr <- field3logit(mod0, 'finalgradeLow',
#'   label = 'Low final grade', p0 = refpoint, narrows = 1)
#'
#' mfields <- field_Sdur + field_Ldur  + field_Lfgr + field_Hfgr
#' mfields
#'
#' gg3logit(mfields) +
#'   stat_field3logit(aes(colour = label)) +
#'   theme_zoom_L(0.45)
#' }
#'
#' @export
multifield3logit <- function(x, ...) {
  if (is.null(x)) { return(NULL) }
  
  if (!inherits(x, c('field3logit','multifield3logit'))) {
  	stop('Only objects of class "field3logit" and "multifield3logit" are allowed')
  } else if (inherits(x, 'field3logit')) {
  	x %<>%
  	  list %>%
  	  structure(class = c('multifield3logit', 'Hfield3logit'))
  }
  return(x)
}



#' @rdname multifield3logit
#' @export
`+.Hfield3logit` <- function(x, y) {
  c(multifield3logit(x), multifield3logit(y)) %>%
    structure(class = c('multifield3logit', 'Hfield3logit')) %>%
    return()
}



#' @rdname multifield3logit
#' @export
print.multifield3logit <- function(x, maxitems = 10, ...) {
  cat(' Object of class "multifield3logit"\n')
  cat('------------------------------------\n')
  cat('Number of fields         :', length(x), '\n')
  cat('Labels\n')
  
  x %>%
    labels %>%
    extract(1:min(maxitems, length(x))) %>%
    nchar %>%
    max -> depoL
  for (j in 1:min(length(x), maxitems)) {
  	cat('  ', j, '. ', labels(x)[j],
  	  paste0(rep(' ', depoL - nchar(labels(x)[j])), collapse = ''),
  	  '  (dX: ', paste(x[[j]]$delta, collapse = ', '), ')\n', sep = '')
  }
  if (length(x) > maxitems) {
  	cat('  [output truncated - see argument "maxlabels"]\n')
  }
  cat('\n')
  
  invisible(x)
}



#' @rdname multifield3logit
#' @export
plot.multifield3logit <- function(x, y = NULL, add = FALSE, col = NA,
  legend = TRUE, ...) {
  	
  if (is.null(col)) { col <- 'black' }
  if (is.na(col)) { col <- seq_along(x) }
  if (length(col) == 1) { col %<>% rep(length(x)) }
  
  lapply(seq_along(x), function(j) {
  	plot(x[[j]], add = (j > 1) + (j == 1) * add, col = col[j], ...)
  }) -> void
  
  
  list(
    legend_text = labels(x),
    legend_col = col
  ) %>%
    invisible()
}



#' @rdname multifield3logit
#' @export
as_tibble.multifield3logit <- function(x, ..., wide = TRUE) {
  lapply(x, as_tibble.field3logit, wide = wide) %>%
    purrr::reduce(bind_rows) %>%
    mutate(group = forcats::fct_anon(factor(paste0(.$label, .$idarrow)), 'H')) %>%
    return()
}



#' @rdname multifield3logit
#' @export
as.data.frame.multifield3logit <- function(x, ..., wide = TRUE) {
  as_tibble.multifield3logit(x, ..., wide = wide) %>%
    as.data.frame %>%
    return()
}



#' @rdname multifield3logit
#' @export
fortify.multifield3logit <- function(model, data, ..., wide = TRUE) {
  as_tibble.multifield3logit(model, ..., wide = wide)
}



#' @rdname multifield3logit
#' @export
tidy.multifield3logit <- function(x, ..., wide = TRUE) {
  as_tibble.multifield3logit(x, ..., wide = wide)
}



#' @rdname multifield3logit
#' @export
labels.multifield3logit <- function(object, ...) {
  object %>% lapply(labels) %>% unlist %>% return()
}



#' @rdname multifield3logit
#' @export
`labels<-.multifield3logit` <- function(object, value) {
  if(length(object) != length(value)) { stop('length mismatch') }
  
  for (i in seq_along(object)) { labels(object[i]) <- value[i] }

  return(object)
}



#' @rdname multifield3logit
#' @export
`[.multifield3logit` <- function(x, i, drop = TRUE) {
  out <- NextMethod()
  if (drop & (length(out) == 1)) {
  	out <- out[[1]]
  } else {
  	class(out) <- class(x)
  }
  return(out)
}


#' @rdname multifield3logit
#' @export
`[<-.multifield3logit` <- function(x, i, value) {
  if (!inherits(value, 'field3logit')) {
  	stop('Only objects of class "field3logit" are allowed')
  }
  
  x[[i]] <- value
  return(x)
}


