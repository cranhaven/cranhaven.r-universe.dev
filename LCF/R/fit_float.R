#' Central fitting function with float environment
#'
#' This function allows to process all samples, especially written for the float environment.
#' @param all.samples List of all samples
#' @param all.standards List of all standards
#' @param LC.vals The fitting range values for the linear combination fitting
#' @param float Let vary the energy range paramerters
#' @param ex.smaller Exclude portions smaller than a given value (decimal form), default to NULL
#' @param file.output Possibility to have a file output, default to NULL
#' @param best.fits Possibility to output more than the best fit (e.g. the first 10 best fits), default to 1
#' @keywords float parameters
#' @export
#' @importFrom utils head setTxtProgressBar txtProgressBar write.csv2 
#' @examples
#' data(stdmix)
#' corr.spec.standards  <- initial_load(specdat[1:4], 
#'   corr.norm = c(-36, -15, 37, 58))
#' corr.spec.samples    <- initial_load(specdat[5:8], 
#'   corr.norm = c(-36, -15, 37, 58))
#' ## Select parameters for baseline correction and edge-step normalization
#' param.float <- expand.grid(pre.adj.1 = seq(-42,-30,6), 
#'   pre.adj.2 = seq(-19,-9,5), post.adj.1 = seq(35,40,5), 
#'   post.adj.2 = seq(50,65,5))
#' length(param.float[,1])
#' float.fit <- fit_float(all.samples = corr.spec.samples,
#'   all.standards = corr.spec.standards, 
#'   LC.vals = c(-14, 46), float = param.float, best.fits = 20)
#' print(float.fit)
#' 
#' ###### Using next configuration can be very time consuming 
#' 
#' param.float.2 <- expand.grid(pre.adj.1 = seq(-43,-30,1), 
#'   pre.adj.2 = seq(-19,-9,.5), post.adj.1 = seq(34,40,.5), 
#'   post.adj.2 = seq(50,65,1))

fit_float <- function (all.samples, all.standards, LC.vals, 
                       float, ex.smaller = NULL, 
                       file.output = NULL, best.fits = NULL) {
  
  ## set exclude to zero or stop if not set correctly
  if (is.null(ex.smaller)) {
    ex.smaller <- 0
  } else {
    if (ex.smaller >= 1 | ex.smaller < 0) stop("You can only exclude portions between 0 and 1, e.g. 0.02 for 2 %")
  }
  
  ## check if file output is set, default is FALSE
  if(is.null(file.output)) {
    file.output <- FALSE
  }
  
  ## set best fits to one, if not set (for full result output)
  if(is.null(best.fits)) {
    best.fits <- 1
  }
  
  ## create progress bar
  try(pb <- txtProgressBar(min = 1, max = length(all.samples), style = 3), silent = TRUE)
  
  ## transform float parameters into list
  float.list <- as.list(as.data.frame(t(float)))
  
  ## create dummy vector for float parameter results
  float.results <- NULL
  
  ## loop all samples
  for (i in 1:length(all.samples)) {
    
    ## set progress bar
    cat(paste("Overall progress:\n", sep = ""))
    try(setTxtProgressBar(pb, i), silent = TRUE)
    
    ## print sample name and the number of fitting combinations
    cat(paste("\nFitting sample: ", all.samples[[i]]$name, ", with ", length(float[,1]), " combinations, starting: ", Sys.time(), "\n", sep = ""))
    
    ## create data frame of all standards with the correct energy of the given sample
    fit.standards <- std_df(sample = all.samples[[i]], all.standards = all.standards)
    
    ## apply LC_fit function to the sample with all float parameters
    new.results <- lapply(float.list, function(X) LC_fit(sample = all.samples[[i]], 
                                                         float = as.numeric(X), 
                                                         standards = fit.standards, 
                                                         LC.vals = LC.vals, 
                                                         ex.smaller = ex.smaller, 
                                                         E.zero = all.samples[[i]]$data$E0)
    )
    
    ## transform result list to data frame    
    new.results <- do.call(rbind.data.frame, new.results)
    
    ## add float parameters to fitting results
    new.results <- cbind(new.results, float)
    
    ## sort fitting results by R-factor
    new.results.sorted <- new.results[order(new.results$R.fac),]
    
    ## write fitting result in file, if set
    if (file.output == TRUE) {
      write.csv2(x = new.results.sorted, file = paste("LCF.float", all.samples[[i]]$name, "csv", sep = "."), row.names = FALSE)
    }
    
    ## extract top fits, if set
    best.fit <- head(new.results.sorted, best.fits)
    
    ## erase all row names and rename the best fit
    row.names(best.fit) <- NULL
    row.names(best.fit)[1] <- all.samples[[i]]$name
    
    ## bind sample result to full result
    float.results <- rbind(float.results, best.fit)
    write.csv2(float.results, "temp.float.csv", row.names = TRUE)  
    
    ## fitting finishing time
    cat(paste("Sample finished: ", Sys.time(), "\n", sep = ""))
    
    ## close all samples loop
  }
  
  ## close progress bar
  try(close(pb), silent = TRUE)
  
  ## retrun full float result
  return(float.results)
  
  ## close function
}