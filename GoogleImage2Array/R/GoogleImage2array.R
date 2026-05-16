##' @title GoogleImage2array: create array from image thumbnails.
##'
##' @description This function is to create the R array/tensor from
##' 2D image obtained from Google image search.
##' This function provides an array consisted of 20 images per run.
##'
##' @param Query a character vector to search images
##' @param wh a value of pixels in height and width.
##' @param Save a logical. Whether to save images locally or not.
##' if TRUE, save locally.
##' @param Col a logical. Whether to handle color or gray images.
##' if TRUE, use color mode.
##' @param gl a character to show a region information. ex. us, ja etc
##' @param file_path a character. a directory to save the image file.
##'
##' @return array
##' @author Satoshi Kume
##'
##' @importFrom xml2 read_html
##' @importFrom rvest html_nodes
##' @importFrom rvest html_attr
##' @importFrom EBImage readImage
##' @importFrom EBImage resize
##' @importFrom EBImage Image
##' @importFrom magrittr %>%
##'
##' @export GoogleImage2array
##'
##' @examples \donttest{
##' library(EBImage)
##'
##' # Simple examples
##' query <- "persian cat"
##' CatImg <- GoogleImage2array(query)
##'
##' #show info
##' str(CatImg)
##'
##' query <- "Shiba inu"
##' DogImg <- GoogleImage2array(query)
##'
##' #show info
##' str(DogImg)
##'
##' #Bind arrays
##' ImgDat <- EBImage::abind(CatImg$array, DogImg$array, along=1)
##'
##' #show info
##' str(ImgDat)
##'
##' }
##'

GoogleImage2array <- function(Query,
                              wh=64,
                              Col=TRUE,
                              Save=FALSE,
                              file_path=NULL,
                              gl="us"){

#Decide hl
switch(gl,
       "us" = lan <- "en",
       "ja" = lan <- "ja",
       lan <- "en")

#percent-encode
url <- utils::URLencode(paste0("https://www.google.com/search?q=", Query,
                               "&hl=", lan, "&gl=", gl,
                               "&btnG=Google+Search&tbs=0&safe=off&tbm=isch"))

#get URLs
imgsrc <- xml2::read_html(url) %>%
        rvest::html_nodes(xpath = '//img') %>%
        rvest::html_attr('src')
imgsrc <- imgsrc[startsWith(imgsrc, "https://")]

#Variable to store array data
Dat <- NULL

for(n in 1:length(imgsrc)){
#Extract one URL
imgsrc00 <- imgsrc[n]

#Save it locally
if(Save){

if(is.null(file_path)){
  path <- paste0(tempdir(), "/")
}else{
  path <- paste0(sub("/$", "", file_path), "/")
}

utils::download.file(imgsrc00,
              paste0(path, "Image_", formatC(n, width = 3, flag = "0"),".jpg"), mode = "wb")

}

#Read image with color or gray
if(Col){
  Img <- EBImage::readImage(imgsrc00, type="jpg")
}else{
  Img <- EBImage::readImage(imgsrc00, type="jpg")
  Img <- EBImage::Image(Img, colormode = "gray", dim = c(dim(Img)[-3], 1))
}

#Resize images
ImgR <- EBImage::resize(Img, w = wh, h=wh)

#Convert to the array
ImgRArray <- array(ImgR, dim=dim(ImgR))

#Store as a list
Dat[[n]] <- ImgRArray

}

#Convert from list to 4d array
ImgTensor <- base::aperm(base::simplify2array(Dat), c(4, 1, 2, 3))

ImgTensorList <- list(array=ImgTensor,
                      query=Query)
#Output
return(ImgTensorList)

}

