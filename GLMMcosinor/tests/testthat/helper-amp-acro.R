expect_amp_acro <- function(x) {
  lapply(
    x,
    function(obj) {
      if (inherits(obj, "formula")) {
        obj <- as.character(obj)
      }
      expect_snapshot(obj)
    }
  )
}
