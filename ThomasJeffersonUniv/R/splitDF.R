


#' @title Split \link[base]{data.frame} by Row
#' 
#' @description
#' \link[base]{split.data.frame} into individual rows.
#' 
#' @param x \link[base]{data.frame}
#' 
#' @note
#' We use \link[base]{split.data.frame} with argument `f` being `attr(x, which = 'row.names', exact = TRUE)` instead of
#' `seq_len(.row_names_info(x, type = 2L))`,
#' not only because the former is faster, but also \link[base]{.rowNamesDF<-} enforces 
#' that \link[base]{row.names.data.frame} must be unique.
#' 
#' @returns
#' Function [splitDF] returns a \link[base]{list} of \link[base]{nrow}-1 \link[base]{data.frame}s.
#' 
#' @examples
#' splitDF(head(mtcars)) # data.frame with rownames
#' splitDF(head(warpbreaks)) # data.frame without rownames
#' splitDF(data.frame()) # exception
#' @export
splitDF <- function(x) {
  split.data.frame(x, f = attr(x, which = 'row.names', exact = TRUE))
}






#' @title Match Rows of One \link[base]{data.frame} to Another
#' 
#' @description
#' To \link[base]{match} the rows of one \link[base]{data.frame}
#' to the rows of another \link[base]{data.frame}.
#' 
#' @param x \link[base]{data.frame}, the rows of which to be matched.
#' 
#' @param table \link[base]{data.frame}, the rows of which to be matched *against*.
#' 
#' @param by \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param by.x,by.table \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param view.table (optional) \link[base]{character} scalar or \link[base]{vector},
#' variable names of `table` to be printed in fuzzy suggestion (if applicable)
#'  
#' @param trace \link[base]{logical} scalar, to provide detailed diagnosis information, default `FALSE`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [matchDF] returns a \link[base]{integer} \link[base]{vector}
#' 
#' @note
#' Unfortunately, R does not provide case-insensitive \link[base]{match}.
#' Only case-insensitive \link[base]{grep} methods are available.
#' 
#' @examples
#' DF = swiss[sample(nrow(swiss), size = 55, replace = TRUE), ]
#' matchDF(DF)
#' @importFrom stringdist stringdist
#' @importFrom utils write.csv
#' @export
matchDF <- function(
    x, table = unique.data.frame(x),
    by = names(x), by.x = character(), by.table = character(),
    view.table = character(),
    trace = FALSE,
    ...
) {
  
  if (!is.data.frame(x)) stop('`x` must be data.frame')
  
  tab <- table; table <- NULL # dont want to confuse with ?base::table
  if (!is.data.frame(tab)) stop('`table` must be data.frame')
  
  nm.x <- names(x)
  nm.tab <- names(tab)
  
  by.x <- unique.default(c(by, by.x))
  by.tab <- unique.default(c(by, by.table))
  if (any(id <- is.na(match(by.x, table = nm.x)))) stop('Colnames ', paste(sQuote(by.x[id]), collapse = ','), ' absent from `x`')
  if (any(id <- is.na(match(by.tab, table = nm.tab)))) stop('Colnames ', paste(sQuote(by.tab[id]), collapse = ','), ' absent from `table`')
  
  nby <- length(by.x)
  if (nby != length(by.tab)) stop('`by.x` and `by.table` must be same length')
  
  #nm_x <- setdiff(nm.x, by.x)
  #nm_table <- setdiff(nm.tab, by.tab)
  #if (length(nm_ <- intersect(nm_x, nm_table))) stop('do not allow same colnames ', paste(sQuote(nm_), collapse = ','), ' in `x` and `table` (except for `by`)')
  
  x0 <- x[by.x]; .rowNamesDF(x0) <- NULL
  tab0 <- tab[by.tab]; names(tab0) <- by.x; .rowNamesDF(tab0) <- NULL
  # otherwise, if `!identical(by.x, by.tab)`, ?base::match wont work
  if (anyDuplicated.data.frame(tab0)) stop('do not allow duplicated ', sQuote(paste0(by.tab, collapse = '+')), ' in `table`')
  
  id <- match(x = splitDF(x0), table = splitDF(tab0), nomatch = NA_integer_)
  
  if (any(duplicated.default(id))) { # rows with multiple matches
    tmp1 <- split.default(seq_along(id), f = factor(id))
    tmp2 <- tmp1[lengths(tmp1, use.names = FALSE) > 1L]
    tmp <- lapply(tmp2, FUN = `+`, 1L) # Excel rows, +1 for row header
    if (trace) lapply(format_named(tmp, sep = 'th unique row appears on Excel rows '), FUN = message)
  } # rows with multiple matches
  
  if (any(na1 <- is.na(id))) { # rows without a match
    
    x_ <- x0[na1, , drop = FALSE]
    x_uid <- !duplicated.data.frame(x_)
    
    x_u <- x_[x_uid, , drop = FALSE]
    
    for (i in c(rev.default(seq_len(nby - 1L)), 0L)) { # (i = nby - 1L)
      if (i == 0L) break # to indicate nothing full-match
      iseq <- seq_len(i)
      idx <- match(x = splitDF(unique.data.frame(x_u[iseq])), 
                   table = splitDF(unique.data.frame(tab0[iseq])), 
                   nomatch = NA_integer_)
      idok <- !is.na(idx)
      if (trace) message(sprintf(fmt = '\u2756 Matched %d/%d by %s and %s', sum(idok), length(idx), style_interaction(by.x[iseq]), style_interaction(by.tab[iseq])))
      if (all(idok)) break
    }
    
    if (i == 0L) {
      x_dx <- x_u
      tab_dx <- tab0
    } else {
      x_dx <- x_u[, -seq_len(i), drop = FALSE]
      tab_dx <- tab0[, -seq_len(i), drop = FALSE]
    }
    
    min_dist_0 <- lapply(seq_len(length(x_dx)), FUN = function(i) { # (i = 1L)
      tmp <- lapply(x_dx[[i]], FUN = stringdist, b = tab_dx[[i]], method = 'lcs')
      vapply(tmp, FUN = which.min, FUN.VALUE = 0L, USE.NAMES = FALSE)
    })
    min_dist_1 <- .mapply(FUN = c, dots = min_dist_0, MoreArgs = NULL)
    min_dist <- lapply(min_dist_1, FUN = unique.default)
    
    view_table <- if (length(view.table)) tab[view.table] else tab
    fuzzy_suggest <- data.frame(
      x_dx[rep(seq_along(min_dist), times = lengths(min_dist)), , drop = FALSE],
      view_table[unlist(min_dist, use.names = FALSE), , drop = FALSE]
    )
    fuzzy_csv <- tempfile(pattern = 'fuzzy_', fileext = '.csv')
    message(sprintf(
      fmt = '\u261e %s %d (%d unique) %s having no exact match to %s\n', # extra line feed!!
      style_basename(fuzzy_csv),
      sum(na1), sum(x_uid), 
      style_interaction(by.x), style_interaction(by.tab)))
    write.csv(x = fuzzy_suggest, file = fuzzy_csv, row.names = FALSE)
    system(paste0('open ', dirname(fuzzy_csv)))
    
    id_agree <- (lengths(min_dist, use.names = FALSE) == 1L)
    if (any(id_agree)) {  
      #stop('Create REDCap file for Ayako')
      # think what to do next
    }
    
  } # rows without a match
  
  attr(id, which = 'by.x') <- by.x
  attr(id, which = 'by.table') <- by.tab
  return(id)
  
}






#' @title An Alternative Merge Operation
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{data.frame}, on which new columns will be added.
#' All rows of `x` will be retained in the returned object, *in their original order*.
#' 
#' @param table \link[base]{data.frame}, columns of which will be added to `x`.
#' Not all rows of `table` will be included in the returned object
#' 
#' @param by \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param by.x,by.table \link[base]{character} scalar or \link[base]{vector}
#' 
#' @param ... additional parameters of [matchDF]
#' 
#' @note
#' We avoid \link[base]{merge.data.frame} as much as possible,
#' because it's slow and 
#' even `sort = FALSE` may not completely retain the original order of input `x`.
#' 
#' @returns 
#' Function [mergeDF] returns a \link[base]{data.frame}.
#' 
#' @examples
#' # examples inspired by ?merge.data.frame 
#' 
#' (authors = data.frame(
#'  surname = c('Tukey', 'Venables', 'Tierney', 'Ripley', 'McNeil'),
#'  nationality = c('US', 'Australia', 'US', 'UK', 'Australia'),
#'  deceased = c('yes', rep('no', 4))))
#' (books = data.frame(
#'  name = c('Tukey', 'Venables', 'Tierney', 'Ripley', 
#'   'Ripley', 'McNeil', 'R Core', 'Diggle'),
#'  title = c(
#'   'Exploratory Data Analysis',
#'   'Modern Applied Statistics',
#'   'LISP-STAT', 'Spatial Statistics', 'Stochastic Simulation',
#'   'Interactive Data Analysis', 'An Introduction to R',
#'   'Analysis of Longitudinal Data'),
#'  other.author = c(
#'   NA, 'Ripley', NA, NA, NA, NA, 'Venables & Smith',
#'   'Heagerty & Liang & Scott Zeger')))
#' 
#' (m = mergeDF(books, authors, by.x = 'name', by.table = 'surname'))
#' attr(m, 'nomatch')
#' 
#' @export
mergeDF <- function(
    x, table, 
    by = character(), by.x = character(), by.table = character(),
    ...
) {
  
  id <- matchDF(x = x, table = table, by = by, by.x = by.x, by.table = by.table, ...)
  by.x <- attr(id, which = 'by.x', exact = TRUE)
  by.table <- attr(id, which = 'by.table', exact = TRUE)
  
  nm_table <- setdiff(names(table), by.table)
  if (length(nm_ <- intersect(
    x = setdiff(names(x), by.x), 
    y = nm_table
  ))) stop('do not allow same colnames ', style_interaction(nm_), ' in `x` and `table` (except for `by`)')
  
  ret <- data.frame(x, table[id, nm_table, drop = FALSE])
  rownames(ret) <- rownames(x) # otherwise be overriden by rownames(table[...])
  return(ret)
  
}