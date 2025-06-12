
#' Computes the Interference Table
#'@author Olivier Eterradossi, \email{olivier.eterradossi@mines-ales.fr}
#' @param spectr A vector of wavelengths (in nanometers).
#' @param birefVect A vector of birefringence values.
#' @param thickVect A vector of thickness values (in micrometers).
#' @description
#' \code{InterferenceTable} is the package workhorse. It computes retardations
#' for specified sets of wavelength, birefringence and thickness values
#' and turns the corresponding transmission matrix into color coordinates
#' in the sRGB colorspace. For now the colorimetric setting is nested into
#' the function. It makes use of D65 illuminant and CIE 1931 2 degrees Color 
#' Matching Functions in order to reproduce Sorensen's paper setting. 
#' Computations may be slow for high resolutions.
#'
#' @return A data frame with 6 variables
#' (columns) named thickness, biref, R, G, B and retardation
#'
#' @references
#' Sorensen, B.E. (2013) A revised Michel-Levy interference colour chart based
#' on first-principles calculations. Eur. J. Mineral., 2013, 25, 5-10.
#' DOI:10.1127/0935-1221/2013/0025-2252 
#'
#' @importFrom colorSpec product
#' @importFrom colorSpec neutralMaterial
#' @importFrom colorSpec wavelength
#' @importFrom colorSpec calibrate
#' @importFrom colorSpec officialXYZ
#' @importFrom colorSpec colorSpec
#' @importFrom colorSpec RGBfromXYZ
#' @importFrom colorSpec DisplayRGBfromLinearRGB
#'
#' @examples
#'\dontrun{
#' test.IC<- InterferenceTable(spectr=seq(360,830,by=5),
#' birefVect=seq(0.0002,0.1,by=0.0005),thickVect=seq(0.01,50,length.out=50))
#'}
#' @seealso \code{\link[colorspace]{sRGB}},
#' \code{\link[colorSpec]{D65}},
#' \code{\link[colorSpec]{xyz1931}},
#' \pkg{colorSpec}
#' @export

InterferenceTable<-function(spectr=seq(360,830,by=5),
birefVect=seq(0.0002,0.1,by=0.0005),thickVect=seq(0.01,50,length.out=50)){

D65.eye <- colorSpec::product( colorSpec::D65.1nm, "artwork",
colorSpec::xyz1931.1nm, wave='auto' )
PRD <- colorSpec::neutralMaterial( 1, colorSpec::wavelength(D65.eye) )
D65.eye <- colorSpec::calibrate( D65.eye, stimulus=PRD,
response=colorSpec::officialXYZ('D65'), method='scaling' )
 
  stock.st<-NULL
  
  for(dd in thickVect){
    retardation<-NULL     
    thickness<-NULL     
    biref<-NULL  
    
    stock<-matrix(NA,nrow=length(spectr),ncol=length(birefVect))
    for (j in 1:length(birefVect)){
      thickness<-c(thickness,dd)
      biref<-c(biref,birefVect[j])
      retardation<-c(retardation,1000*dd*birefVect[j])
      for (i in 1:length(spectr)){
        stock[i,j]<-L(lambda=spectr[i],d=dd,biref=birefVect[j])
        
      }
    }
    colorSpec.MAT<-colorSpec::colorSpec(data=as.matrix(stock),
    wavelength=spectr,quantity="reflectance")
    XYZ1<-colorSpec::product(colorSpec.MAT, D65.eye, wave='auto')
    RGB1<-colorSpec::RGBfromXYZ(XYZ1, 'sRGB' )
    RGB4<-RGB1
    RGB4[,1]<-(RGB1[,1]-min(RGB1[,1]))/(max(RGB1[,1])-min(RGB1[,1]))
    RGB4[,2]<-(RGB1[,2]-min(RGB1[,2]))/(max(RGB1[,2])-min(RGB1[,2]))
    RGB4[,3]<-(RGB1[,3]-min(RGB1[,3]))/(max(RGB1[,3])-min(RGB1[,3]))
    RGB4b<-colorSpec::DisplayRGBfromLinearRGB(RGB4, gamma="sRGB")
    interm<-cbind(thickness,biref)
    interm<-cbind(interm,RGB4b[,1])
    interm<-cbind(interm,RGB4b[,2])
    interm<-cbind(interm,RGB4b[,3])
    interm<-cbind(interm,retardation)
    stock.st<-rbind(stock.st,interm)
  }
  InterferenceTable<-stock.st
}
