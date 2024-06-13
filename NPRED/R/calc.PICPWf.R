#' Calculate PIC
#'
#' @param x       A vector of response.
#' @param y       A vector of new predictor.
#' @param z       A matrix of pre-existing predictors that could be NULL if no prior predictors exist.
#' @param nnmax   The maximum number of sample size.
#' @param nvarmax The maximum number of potential predictors.
#'
#' @return A list of 2 elements: the partial mutual information (pmi), and partial informational correlation (pic).
#' @export
#'
#' @useDynLib NPRED, .registration=TRUE
#' @references Sharma, A., Mehrotra, R., 2014. An information theoretic alternative to model a natural system using observational information alone. Water Resources Research, 50(1): 650-660.

calc.PIC <- function(x, y, z, nnmax=10000,nvarmax=100) {
  x <- as.matrix(x)
  y <- as.matrix(y)
  n <- nrow(x)
  if (missing(z)) {
    nd <- 0
    z <- as.matrix(0 * x)
  } else {
    z <- as.matrix(z)
    nd <- ncol(z)
  }

  pic <- pmi <- NULL
  for(j in seq_len(ncol(y))){
    y1 <- y[,j]
    output <- .Fortran("pmi", as.double(x), as.double(y1), as.double(z),
      as.integer(nd), as.integer(n),
      ppms = double(1L), ppic = double(1L),
      as.integer(nnmax), as.integer(nvarmax), PACKAGE = "NPRED"
    )
    
    pmi <- c(pmi, output$ppms)
    pic <- c(pic, output$ppic)
  }

  return(list(pmi = pmi, pic = pic))
}

#' Calculate Partial Weight
#'
#' @param x       A vector of response.
#' @param py      A matrix containing possible predictors of x.
#' @param cpy     The column numbers of the meaningful predictors (cpy).
#' @param cpyPIC  Partial informational correlation (cpyPIC).
#'
#' @return A vector of partial weights(pw) of the same length of z.
#' @export
#'
#' @useDynLib NPRED, .registration=TRUE
#' @references Sharma, A., Mehrotra, R., 2014. An information theoretic alternative to model a natural system using observational information alone. Water Resources Research, 50(1): 650-660.

calc.PW <- function(x, py, cpy, cpyPIC) {
  x <- as.matrix(x)
  py <- as.matrix(py)
  z <- as.matrix(py[, cpy])
  n <- nrow(x)
  nz <- ncol(z)
  nnmax <- n
  nvmax <- nz
  output <- .Fortran("pic2wt", as.double(x), as.double(py),
    as.integer(nz), as.integer(cpy), as.double(cpyPIC),
    pw = double(nvmax),
    as.integer(n), as.integer(nnmax), as.integer(nvmax), PACKAGE = "NPRED"
  )

  return(list(pw = output$pw))
}