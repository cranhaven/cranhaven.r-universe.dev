# @importFrom base packageStartupMessage paste0
#' @importFrom crayon green bold
#' @importFrom utils install.packages
# --------------------------------------
# on loading

.onAttach = function(library, pkg){
if(interactive()) {
  #vers <-  read.dcf(file.path(libname, pkgname, "DESCRIPTION"),appendLF=TRUE), "Version")
  packageStartupMessage(crayon::green("############################################################"),appendLF=TRUE)
  packageStartupMessage(crayon::green(paste0("Obrigado por utilizar o ", crayon::bold("ExpImage") )),appendLF=TRUE)#, vers)
  packageStartupMessage(crayon::green("Author: Alcinei Mistico Azevedo (ICA-UFMG)"),appendLF=TRUE)
  packageStartupMessage(crayon::green("Veja tutoriais sobre este e outros pacotes no youtube:"),appendLF=TRUE)
  packageStartupMessage(crayon::green("https://www.youtube.com/channel/UCDGyvLCJnv9RtTY1YMBMVNQ"),appendLF=TRUE)
  packageStartupMessage(crayon::green("Se inscreva e compartilhe para ajudar o canal a crescer."),appendLF=TRUE)
  packageStartupMessage(crayon::green("############################################################"),appendLF=TRUE)
}
  invisible()
}




#Instala o EBImage, caso nao esteja instalado.


ebimage <- function(){
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


IM3=function(m){
  #m=(ImagemSeg>0)*1

  mm=array(NA,dim = c(nrow(m),ncol(m),3))
  mm[,,1]=m
  mm[,,2]=m
  mm[,,3]=m
  mmm=EBImage::as.Image(mm)
  EBImage::colorMode(mmm)=2

  return(mmm)
}


extract_band=function(im2,band,norm){
  print("extract")
  aa=raster(im2,band)
  t(as.matrix(aa))/norm
}
#bwlabel=EBImage::bwlabel
#combine=EBImage::combine
#dilate=EBImage::dilate
#display=EBImage::display
#erode=EBImage::erode
#is.Image=EBImage::is.Image
#readImage=EBImage::readImage












