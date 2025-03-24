#' update.gstudy
#' 
#' @method update gstudy
#' @keywords internal
#' @export
update.gstudy <- function(object, data, colname.objects, colname.scores, colname.strata = NULL, ...) {
  dstudy.out <- object
  if("within" %in% names(dstudy.out)) {
    if(is.null(colname.strata)) {
      stop("Please specify the name of the column containing strata.")
    } else {
      for(stratum in names(dstudy.out$within)) {
        cases.keep <- is.na(data[, colname.strata]) == F & 
          data[, colname.strata] == stratum
        dstudy.out$within[[stratum]][["components"]] <- update(
          object = dstudy.out$within[[stratum]][["components"]], 
          data = data[cases.keep, ], 
          colname.objects = colname.objects, 
          colname.scores = colname.scores
        )
      }
    }
  } else {
    dstudy.out$components <- update(
      object = dstudy.out$components, 
      data = data, 
      colname.objects = colname.objects, 
      colname.scores = colname.scores
    )
  }
  class(dstudy.out) <- ifelse(class(dstudy.out) == "gstudy", "dstudy", class(dstudy.out))
  dstudy.out
}