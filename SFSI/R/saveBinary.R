
# Save a file as binary

saveBinary <- function(X, file = paste0(tempdir(),"/file.bin"),
                       precision.format = c("double","single"),
                       verbose = TRUE)
{
  precision.format <- match.arg(precision.format)

  dm <- dim(X)
  if(length(dm) == 2L){
    X <- as.matrix(X)
  }else{
    dm <- c(length(X),0)
  }

  type <- storage.mode(X)
  if(!type %in% c("logical","double","integer")){
     stop("'storage.mode(X)' must be either 'integer', 'logical', or 'double'")
  }

  unlink(file)

  doubleprecision <- as.logical(precision.format=="double")

  #dyn.load("c_functions.so")
  out <- .Call('R_writeBinFile', file, dm[1], dm[2], X, doubleprecision, verbose)
  #dyn.unload("c_functions.so")

}
