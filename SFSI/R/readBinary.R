
# rows = NULL; cols = NULL; verbose = TRUE
# Read a file as binary
readBinary <- function(file = paste0(tempdir(),"/file.bin"),
                       rows = NULL, cols = NULL,
                       drop = TRUE, verbose = TRUE)
{
  if(!file.exists(file)){
    stop("File '",file,"' does not exist")
  }

  # Read lines
  #dyn.load("c_functions.so")
  return(.Call("R_readBinFile", file, rows-1, cols-1, drop, verbose))
  #dyn.unload("c_functions.so")

}
