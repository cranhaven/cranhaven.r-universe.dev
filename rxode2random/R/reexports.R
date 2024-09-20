#' @importFrom lotri lotri
#' @export
lotri::lotri

#' @importFrom rxode2parse .convertId
#' @export
rxode2parse::.convertId

# for backward compatibility
convertId <- function(a) {
  rxode2parse::.convertId(a)
}
