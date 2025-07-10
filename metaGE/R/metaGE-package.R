#' @keywords internal
#' 
#' @details The main functions of the package corresponding to the steps of a genome-wide association meta-analysis are:
#' \itemize{
#' \item \code{\link{metaGE.collect}}: Collect the results of genome-wide association studies data from different files.
#' \item \code{\link{metaGE.cor}}: Infer the correlation between studies.
#' \item \code{\link{metaGE.fit}}: Perform global meta-analysis test procedure for quantitative trait loci detection (using a Fixed or Random effect model)
#' \item \code{\link{metaGE.test}}: Perform tests of contrast or meta-regression investigate 
#' the relationship between marker effects and some qualitative or quantitative covariate, respectively.
#' \item \code{\link{metaGE.lscore}}: Apply the local score approach as multiple tests correction.
#' \item \code{\link{metaGE.manhattan}}: Draw a Manhattan plot. 
#' }
#' 
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
