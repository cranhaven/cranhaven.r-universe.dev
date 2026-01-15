mvpEqual <- function(pol1, pol2){
  identical(pol1[["names"]], pol2[["names"]]) &&
    identical(pol1[["power"]], pol2[["power"]]) &&
    isTRUE(all.equal(pol1[["coeffs"]], pol2[["coeffs"]]))
}
