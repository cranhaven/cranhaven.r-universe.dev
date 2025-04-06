#' Linear combination fitting function
#'
#' This function performs the LC fitting of the input sample/samples. It outputs the fitting results with the R-Factors as fitting statistics.
#' @param sample The sample spectrum
#' @param standards The standards spectra
#' @param LC.vals Values for ranges of linear combination fitting, with respect to the edge-step
#' @param float Set float parameters, defaults to NULL
#' @param E.zero Set E0, defaults to NULL
#' @param ex.smaller Set value to exclude small portions (as portion of 1), defaults to NULL
#' @keywords normalization, correction
#' @export
#' @examples
#' data(stdmix)
#' corr.spec.standards  <- initial_load(specdat[1:4], 
#'   corr.norm = c(-36, -15, 37, 58))
#' corr.spec.samples    <- initial_load(specdat[5:8], 
#'   corr.norm = c(-36, -15, 37, 58))
#' fit.standards <- std_df(sample = corr.spec.samples[[1]], 
#'   all.standards = corr.spec.standards)
#' fit.result <- LC_fit(sample = corr.spec.samples[[1]], 
#'   standards = fit.standards, LC.vals = c(-14, 46))
#' print(fit.result)

LC_fit <- function (sample, standards, LC.vals, float = NULL,
                    E.zero = NULL, ex.smaller = NULL) {
  
  ## set exclude to zero or stop if not set correctly
  if (is.null(ex.smaller)) {
    ex.smaller <- 0
  } else {
    if (ex.smaller >= 1 | ex.smaller < 0) stop("You can only exclude portions between 0 and 1, e.g. 0.02 for 2 %")
  }
  
  ## set E zero
  if (is.null(E.zero)) {
    E.zero <- sample$data$E0
  }
  
  ## set fitting range parameters relative to E zero
  LC.pre <- LC.vals[1]
  LC.post <- LC.vals[2]
  
  ## check, if float paramters are set
  if (is.null(float)) {
    
    ## use intially corrected samples
    corr.spec <- sample$data$corr.spec
    
  } else {
    
    ## re-build the corrected sample with new float parameters
    corr.spec <- bkg_corr(raw.spec = sample, corr.norm = float)
  }
  
  ## find ranges that have to be fitted
  range.pre <- which(abs(corr.spec[["energy"]]-(E.zero+LC.pre)) == min(abs(corr.spec[["energy"]]-(E.zero+LC.pre))))
  range.post <- which(abs(corr.spec[["energy"]]-(E.zero+LC.post)) == min(abs(corr.spec[["energy"]]-(E.zero+LC.post))))
  
  ## extract standards and sample in given range
  LC.sample <- corr.spec["cor.absorption"][range.pre:range.post,]
  LC.standards <- standards[range.pre:range.post,]
  
  ## solve the fitting as Quadratic Programming problem
  result <- LCF_solve_QP(LCF.stds = LC.standards, LCF.samp = LC.sample)
  
  ## extract the standard names
  LC.standard.names.start <- colnames(LC.standards)
  LC.standard.names <- colnames(LC.standards)
  
  ## check which coefficients are below exclution limit
  fit.vals <- which(result[LC.standard.names.start] < ex.smaller)
  
  ## loop to process fitting until no standards are excluded any more  
  while (length(fit.vals) > 0) {
    
    ## subset the remaining standards and their names
    LC.standards <- LC.standards[-fit.vals]
    LC.standard.names <- colnames(LC.standards)
    
    ## solve the fitting as Quadratic Programming problem
    result <- LCF_solve_QP(LCF.stds = LC.standards,  LCF.samp = LC.sample)
    
    ## check for standards below smaller % value
    fit.vals <- which(result[LC.standard.names] < ex.smaller)
    
    ## close while loop
  }
  
  ## find the names of the now fitted standards
  fit.stds <- match(LC.standard.names, LC.standard.names.start)
  
  ## create a dummy coefficient vector and fill it with the new fitting values
  dum.coef <- as.data.frame(t(rep(0, length(LC.standard.names.start))))
  colnames(dum.coef) <- LC.standard.names.start
  dum.coef[fit.stds] <- as.numeric(result[LC.standard.names])
  
  ## fill a result vector with the proportions and the statistics
  end.result <- cbind(dum.coef, R.fac = result$R.fac)
  
  ## return result
  return(end.result)
  
  ## close function
}
