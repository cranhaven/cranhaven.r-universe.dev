.assert_SlotsDetails <- function(x) {
  stopifnot(all(colnames(SlotsDetails()) %in% colnames(x)))
}
