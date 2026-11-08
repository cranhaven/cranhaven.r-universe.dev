#' Calculates six geographic range measures at specified sample sizes for a single taxon
#' @param TName - The name of the taxon of interest, as labeled in OccMatrix, as a string
#' @param OccMatrix - A matrix where columns are taxon occurrences and also having at least longitude and latitude values
#' @param LongPos - The column index value of longitudinal coordinates in the OccMatrix
#' @param LatPos - The column index value of latitudinal coordinates in the OccMatrix
#' @param iter - The number of times a taxon's locations are resampled at each step size
#' @param CellSize - The size of each cell in degree X degree
#' @param longBounds - Array of longitudinal boundaries in decimal degrees
#' @param latBounds - Array of latitudinal boundaries in decimal degrees
#' @param steps - Array of the values representing the number of points to be subsampled for each taxon
#' @param replacePts - A logical value indicating whether points are allowed to be sampled more than once during each subsampling iteration
#' @details The nLocCut parameter is the minimum number of distinct geographic locations a taxon must be observed at to have geographic range measures calcualted, if below retruns NA.
#' The steps parameter typically begins with a 1 representing that all points should be used in calculations but this is not required.
#' The replacePts parameter must be set to TRUE if any of the steps require a greater number of points be locations be sampled than are available for a taxon, otherwise the function will fail.
#' @note If PEE values are to be calculated as a next step the steps parameter needs to have a value of 1 as its first value
#' @examples
#' \dontrun{
#' data(BivalvePBDB)
#' BivalveMatrix<-CoordList_PBDB(BivalvePBDB)
#' GeoRarefaction_SingleTaxon(TName=names(BivalveMatrix)[3],OccMatrix=BivalveMatrix,replacePts=TRUE)
#' }
#' @return Returns a list of each geographic range measure as a separate matrix of values. Measures include minimum spanning tree distance, minimum convex hull area, maximum pairwise great circle distance, latitudinal range, longitudinal range, and number of degree X degree cells occupied.
#' @export
GeoRarefaction_SingleTaxon<-function(TName="Bird1",OccMatrix,LongPos=1,LatPos=2,iter=10,CellSize=5,longBounds=c(-180,180),latBounds=c(-90,90),steps=c(1,50,40,30,20,10,5),replacePts=FALSE){
  permMST_mat<-data.frame(matrix(NA,nrow=iter,ncol=length(steps)))
  permCH_mat<-data.frame(matrix(NA,nrow=iter,ncol=length(steps)))
  permGCD_mat<-data.frame(matrix(NA,nrow=iter,ncol=length(steps)))
  permLatRg_mat<-data.frame(matrix(NA,nrow=iter,ncol=length(steps)))
  permLonRg_mat<-data.frame(matrix(NA,nrow=iter,ncol=length(steps)))
  permCellCount_mat<-data.frame(matrix(NA,nrow=iter,ncol=length(steps)))
  count<-0
  namePos<-which(as.character(names(OccMatrix[1,]))==TName)
  taxPos<-which(OccMatrix[,namePos]!=0)
  longs<-OccMatrix[taxPos,LongPos]
  lats<-OccMatrix[taxPos,LatPos]
  nCoords<-length(lats)
  MSTmat<-NA
  for(i in steps){
    count<-count+1
    for(j in 1:iter){
      if(i==1){
        MSTCalc<-MSTDist(longs,lats)
        longs<-MSTCalc$Longitude
        lats<-MSTCalc$Latitude
        nCoords<-length(longs)
        permMST_mat[1:iter,count]<-MSTCalc$MST_km
        MSTmat<-MSTCalc$MST_DistMat
        CHCalc<-CHullAreaEarth(longs,lats)
        permCH_mat[1:iter,count]<-CHCalc
        GCDCalc<-max(MSTmat,na.rm=TRUE)
        permGCD_mat[1:iter,count]<-GCDCalc
        LatRgCalc<-LatRg(lats)
        permLatRg_mat[1:iter,count]<-LatRgCalc$KmSpan
        LonRgCalc<-LongRg(longs)
        permLonRg_mat[1:iter,count]<-LonRgCalc$KmSpan
        CellCountCalc<-CellCount(longs,lats,CellSize=CellSize,longBounds=longBounds,latBounds=latBounds)
        permCellCount_mat[1:iter,count]<-CellCountCalc$NumCellsOcc
        nCoords<-length(longs)
        break
      }
      else{
        if(replacePts==FALSE){
          if(i>nCoords){
            print("Number of points to resample is greater that number of actual points. Set the replacePts parameter to TRUE")
          }
          RandPoints<-sample.int(nCoords,i,replace=FALSE)
          RandPoints<-sort(RandPoints)
        }
        if(replacePts==TRUE){
          RandPoints<-sample.int(nCoords,i,replace=TRUE)
          RandPoints<-sort(RandPoints)
        }
        longzSub<-longs[RandPoints]
        latzSub<-lats[RandPoints]
        SubCollapse<-CoordCollapse(longzSub,latzSub)
        MSTmatTemp<-MSTmat[unique(RandPoints),unique(RandPoints)]
        MSTCalc<-MSTDist_FromMat(SubCollapse[,1],SubCollapse[,2],MSTmatTemp)
        permMST_mat[j,count]<-MSTCalc$MST_km
        CHCalc<-CHullAreaEarth(SubCollapse[,1],SubCollapse[,2])
        permCH_mat[j,count]<-CHCalc
        GCDCalc<-GCD(SubCollapse[,1],SubCollapse[,2])
        permGCD_mat[j,count]<-GCDCalc
        LatRgCalc<-LatRg(SubCollapse[,2])
        permLatRg_mat[j,count]<-LatRgCalc$KmSpan
        LonRgCalc<-LongRg(SubCollapse[,1])
        permLonRg_mat[j,count]<-LonRgCalc$KmSpan
        CellCountCalc<-CellCount(SubCollapse[,1],SubCollapse[,2],CellSize,longBounds,latBounds)
        permCellCount_mat[j,count]<-CellCountCalc$NumCellsOcc
      }
    }
  }
  rows_mat<-c(rep("MST",iter))
  rowNums<-c(rep(1:iter))
  rows_mat<-c(paste(rows_mat,rowNums))
  row.names(permMST_mat)<-rows_mat
  names(permMST_mat)<-steps

  rows_mat<-c(rep("CH",iter))
  rowNums<-c(rep(1:iter))
  rows_mat<-c(paste(rows_mat,rowNums))
  row.names(permCH_mat)<-rows_mat
  names(permCH_mat)<-steps

  rows_mat<-c(rep("GCD",iter))
  rowNums<-c(rep(1:iter))
  rows_mat<-c(paste(rows_mat,rowNums))
  row.names(permGCD_mat)<-rows_mat
  names(permGCD_mat)<-steps

  rows_mat<-c(rep("LatRg",iter))
  rowNums<-c(rep(1:iter))
  rows_mat<-c(paste(rows_mat,rowNums))
  row.names(permLatRg_mat)<-rows_mat
  names(permLatRg_mat)<-steps

  rows_mat<-c(rep("LonRg",iter))
  rowNums<-c(rep(1:iter))
  rows_mat<-c(paste(rows_mat,rowNums))
  row.names(permLonRg_mat)<-rows_mat
  names(permLonRg_mat)<-steps

  rows_mat<-c(rep("CellCount",iter))
  rowNums<-c(rep(1:iter))
  rows_mat<-c(paste(rows_mat,rowNums))
  row.names(permCellCount_mat)<-rows_mat
  names(permCellCount_mat)<-steps

  return(list(TaxonName=TName,MST_mat=permMST_mat,CH_mat=permCH_mat,GCD_mat=permGCD_mat,LatRg_mat=permLatRg_mat,LonRg_mat=permLonRg_mat,CellCount_mat=permCellCount_mat))
}
