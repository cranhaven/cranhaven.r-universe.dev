
install_EBImage <- function(){
  if(!requireNamespace("EBImage", quietly = TRUE)) {
    
    if(!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager", quiet = TRUE)
    }
    BiocManager::install("EBImage",
                         update = FALSE,
                         ask = FALSE,
                         quiet = TRUE)
  }
  #require(raster)
}
