mround <- function(x, base) {
  base * round(x/base)
}

cround <- function(x, base) {
  base * ceiling(x/base)
}

fround <- function(x, base) {
  base * floor(x/base)
}

getDigits <- function(x) {
  if (abs(x) < 1) {
    digi <- nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    quant <- x + 5/(10 ^(digi + 1))
  } else {
    digi <- (-(nchar(as.character(round(x)))-1))
    quant <- x + 5*10^abs(digi)
  }
  return(mround(quant, digi))
}

check_mosek <- function() {
  skip.fun <- !rlang::is_installed("Rmosek")
  
  if(skip.fun) {
    testthat::skip("Rmosek not found for tests")
  } else {
    mosek.err <- tryCatch(
      !is.character(Rmosek::mosek_version()),
      error = function(e) {TRUE}
    )
    if (mosek.err) testthat::skip("Rmosek installed but mosek optimizer not found for tests.")
  }
}

check_gurobi <- function() {
  skip.fun <- !rlang::is_installed("gurobi")
  if(skip.fun) {
    testthat::skip("gurobi not found for tests")
  }
}

register_solver <- function(solution.method) {
  switch(solution.method, 
         cone = ROI::ROI_require_solver("ecos"),
         lp =  ROI::ROI_require_solver("lpsolve"),
         cplex = ROI::ROI_require_solver("cplex")
  )
}