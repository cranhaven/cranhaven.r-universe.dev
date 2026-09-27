#' @exportS3Method iteror data.frame
#' @export
#' @rdname iteror.array
iteror.data.frame <- count_template(
  input = alist(obj=),
  options = alist(by=c("column", "row")),
  preamble = alist(
    if (is.character(by)) by <- match.arg(by),
    count <- switch(by,
                  row = nrow(obj),
                  column = ncol(obj),
                  stop("`by` must be 'row' or 'column'"))),
  output = function(ix, len) substitute(
    switch(by,
           row = obj[ix, ],
           column = obj[, ix])
  ),
  output_chunk = function(ix, len) substitute(
    switch(by,
           row = obj[ix + seq_len(len), ],
           column = obj[, ix + seq_len(len)])
  )
)
