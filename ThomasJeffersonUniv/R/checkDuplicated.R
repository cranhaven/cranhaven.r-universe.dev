

#' @title Inspect Duplicated Records in a \link[base]{data.frame}
#' 
#' @description
#' To inspect duplicated records in a \link[base]{data.frame}.
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param f \link[stats]{formula},
#' criteria of duplication, e.g., 
#' use `~ mrn` to identify duplicated `mrn`, or 
#' use `~ mrn + visitdt` to identify duplicated `mrn:visitdt`
#'
#' @param dontshow (optional) \link[base]{character} scalar or \link[base]{vector},
#' variable names to be omitted in output diagnosis `file`
#' 
#' @param file \link[base]{character} scalar,
#' path of diagnosis file, 
#' print out of substantial duplicates
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [checkDuplicated] returns a \link[base]{data.frame}.
#' 
#' @examples
#' (d1 = data.frame(A = c(1, 1), B = c(NA_character_, 'text')))
#' 
#' 
#' (d2 = data.frame(A = c(1, 2), B = c(NA_character_, 'text')))
#' 
#' @importFrom writexl write_xlsx
#' @export
checkDuplicated <- function(
    data, f, 
    dontshow = character(length = 0L),
    file = tempfile(pattern = 'checkDuplicated_', fileext = '.xlsx'),
    ...
) {
  
  dup_txt <- style_interaction(f)
  
  rid <- split_int_(data = data, f = f)
  rid_n <- lengths(rid, use.names = FALSE)
  
  rid_n1 <- (rid_n == 1L)
  if (all(rid_n1)) {
    message(sprintf(fmt = '\u2714 No duplicated %s\n', dup_txt))
    return(invisible(data))
  }
  
  # rows of `data` without duplication
  r0 <- sort.int(unlist(rid[rid_n1], use.names = FALSE))
  
  rid_dup <- rid[rid_n > 1L]
  nm_dup <- format(sQuote(names(rid_dup)), justify = 'left')
  
  # slow with big `data`!!
  ds_dup <- mapply(FUN = function(i, nm) {
    message('\rCreating subset ', nm, appendLF = FALSE)
    data[i, , drop = FALSE]
  }, i = rid_dup, nm = sprintf(fmt = '%s - %d of %d', nm_dup, seq_along(nm_dup), length(nm_dup)), SIMPLIFY = FALSE)
  cat('\r')
  
  ds_coalesce <- lapply(ds_dup, FUN = function(d) {
    # attempt column-wise coalesce?
    tryCatch(expr = lapply(d, FUN = unique_), error = identity)
  })
  
  id_truedup <- vapply(ds_coalesce, FUN = inherits, what = 'error', FUN.VALUE = NA)
  
  d_coalesce <- if (any(!id_truedup)) {
    message(sprintf(fmt = '\u2756 %d %s with trivial (i.e., coalesce-able) duplicates', sum(!id_truedup), dup_txt))
    as.data.frame.list(
      x = do.call(what = mapply, args = c(
        ds_coalesce[!id_truedup], 
        list(FUN = c, SIMPLIFY = FALSE))),
      check.names = FALSE)
  }# else NULL
  
  r1_truedup <- if (any(id_truedup)) {
    n_truedup <- sum(id_truedup)
    
    dontshow_nc <- match(dontshow, table = names(data))
    show_nc <- -dontshow_nc[!is.na(dontshow_nc)]
    if (!length(show_nc)) show_nc <- TRUE
    
    # slow with big `data`!!
    tmp <- mapply(FUN = function(d, nm) {
      message('\rFinding duplicated columns ', nm, appendLF = FALSE)
      not_unique_(d[show_nc])
    }, d = ds_dup[id_truedup], nm = sprintf(fmt = '%s - %d of %d', nm_dup[id_truedup], seq_len(n_truedup), n_truedup), SIMPLIFY = FALSE)
    cat('\r')
    
    write_xlsx(x = tmp, path = file)
    message(sprintf(fmt = '\u261e %s %d %s with substantial duplicates', style_basename(file), n_truedup, dup_txt))
    system(paste0('open ', dirname(file)))
    
    vapply(rid_dup[id_truedup], FUN = `[`, 1L, FUN.VALUE = NA_integer_)
  } # else NULL
  
  ret <- rbind.data.frame(data[c(r0, r1_truedup), , drop = FALSE], d_coalesce)
  message(sprintf('\u21ac %s after %s duplicates removed\n', style_samplesize(nrow(ret)), dup_txt))
  return(ret)
  
}



unique_ <- function(x) {
  x0 <- x[!is.na(x)]
  if (!length(x0)) return(NA)
  # not using my [unique_allequal]
  if (length(x1 <- unique(x0)) != 1L) stop('non-unique entries!')
  return(x1)
}



not_unique_ <- function(data) {
  unique_id <- vapply(data, FUN = function(x) {
    #inherits(tryCatch(unique_(x), error = identity), what = 'error')
    # base::tryCatch too slow
    x0 <- x[!is.na(x)]
    if (!length(x0)) return(TRUE)
    x1 <- unique(x0) # not using my [unique_allequal]
    return(length(x1) == 1L)
  }, FUN.VALUE = NA)
  data[!unique_id]
}


