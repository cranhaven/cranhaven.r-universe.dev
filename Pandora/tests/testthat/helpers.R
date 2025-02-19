passOnErrorMsg <- function(expr) {
  res <- try({
    expr
  })
  
  if (!inherits(res, "try-error")) return(res) else return(res[[1]])
}

foundResource <- function(testLoaded) {
  !(inherits(testLoaded, "character") && grepl(pattern = "No resource found", testLoaded))
}
