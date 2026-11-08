#' Function to tabulate number of occurrences/locations, six geographic range measures, minimum and maximum latitude and longitude for each taxon in a dataset
#' @param OccMatrix - A matrix where columns are taxon occurrences and also having at least longitude and latitude values
#' @param TaxaStart - The column index value where taxon records start
#' @param LongPos - The column index value of longitudinal coordinates in the OccMatrix
#' @param LatPos - The column index value of latitudinal coordinates in the OccMatrix
#' @param CellSize - The size of each cell in degree X degree
#' @param longBounds - Array of longitudinal boundaries in decimal degrees
#' @param latBounds - Array of latitudinal boundaries in decimal degrees
#' @return Returns a matrix of taxa by geographic range measures, including number of observations, number of unique locations observed at, minimum spanning tree distance, minimum convex hull area, maximum pairwise great circle distance, latitudinal range, longitudinal range, and number of degree X degree cells occupied
#' @examples
#' \dontrun{
#' data(BivalvePBDB)
#' BivalveMatrix<-CoordList_PBDB(BivalvePBDB)
#' GeoRange_MultiTaxa(OccMatrix=BivalveMatrix,TaxaStart=3)
#' }
#' @note Calculates the number of observations, localities, minimum spanning tree distance, convex hull area, longitudinal range, latitudinal range, and cell count
#' @seealso See the velociraptr package for details of the downloadPBDB() function
#' @export
GeoRange_MultiTaxa<-function(OccMatrix,TaxaStart,LongPos=1,LatPos=2,CellSize=5,longBounds=c(-180,180),latBounds=c(-90,90)){
  nTaxa<-length(OccMatrix[1,])
  TNames<-names(OccMatrix[1,])[TaxaStart:nTaxa]
  Geo_mat<-data.frame(matrix(NA,nrow=nTaxa-TaxaStart+1,ncol=12),row.names=TNames)
  for(i in TaxaStart:nTaxa){
    taxPos<-which(OccMatrix[,i]!=0)
    Geo_mat[i-TaxaStart+1,1]<-length(taxPos)
    if(length(taxPos)==0){
      Geo_mat[i-TaxaStart,2]<-0
    }
    else{
      longs<-OccMatrix[taxPos,LongPos]
      lats<-OccMatrix[taxPos,LatPos]
      MSTCalc<-MSTDist(longs,lats)
      Geo_mat[i-TaxaStart-1,2]<-length(MSTCalc$Latitude)
      Geo_mat[i-TaxaStart-1,3]<-MSTCalc$MST_km
      Geo_mat[i-TaxaStart-1,4]<-CHullAreaEarth(longs,lats)
      if(length(taxPos)>1){
        Geo_mat[i-TaxaStart-1,5]<-max(MSTCalc$MST_DistMat,na.rm=TRUE)
      }
      else{
        Geo_mat[i-TaxaStart,5]<-0
      }
      LatRgCalc<-LatRg(lats)
      Geo_mat[i-TaxaStart-1,6]<-LatRgCalc$KmSpan
      Geo_mat[i-TaxaStart-1,7]<-LatRgCalc$CoordSpan[1]
      Geo_mat[i-TaxaStart-1,8]<-LatRgCalc$CoordSpan[2]
      LongRgCalc<-LongRg(longs)
      Geo_mat[i-TaxaStart-1,9]<-LongRgCalc$KmSpan
      Geo_mat[i-TaxaStart-1,10]<-LongRgCalc$CoordSpan[1]
      Geo_mat[i-TaxaStart-1,11]<-LongRgCalc$CoordSpan[2]
      CellCountCalc<-CellCount(longs,lats,CellSize=CellSize,longBounds=longBounds,latBounds=latBounds)
      Geo_mat[i-TaxaStart-1,12]<-CellCountCalc$NumCellsOcc
    }
  }
  ColHeads<-c("NObs","NLocs","MST","CH","GCD","LatRg","MinLat","MaxLat","LongRg","MinLong","MaxLong","CellCount")
  names(Geo_mat)<-ColHeads
  return(Geo_mat)
}
