#' Calculates six geographic range measures at resampled a number of time from specified sample sizes
#' @param nLocCut - The minimum number of locations a taxon must be seen at to have geographic range measures calculated
#' @param OccMatrix - A matrix where columns are taxon occurrences and also having at least longitude and latitude values
#' @param TaxaStart - The column index value where taxon records start
#' @param LongPos - The column index value of longitudinal coordinates in the OccMatrix
#' @param LatPos - The column index value of latitudinal coordinates in the OccMatrix
#' @param iter - The number of times a taxon's locations are resampled at each step size
#' @param CellSize - The size of each cell in degree X degree
#' @param longBounds - Array of longitudinal boundaries in decimal degrees
#' @param latBounds - Array of latitudinal boundaries in decimal degrees
#' @param steps - Array of the values representing the number of points to be subsampled for each taxon
#' @param replacePts - A logical value indicating whether points are allowed to be sampled more than once during each subsampling iteration
#' @return Returns a vector where each element is a taxon with a list of each geographic range measure as a separate matrix of values. Measures include minimum spanning tree distance, minimum convex hull area, maxmimum pairwise great circle distance, latitudinal range, longitudinal range, and number of degree X degree cells occupied.
#' @details The nLocCut parameter is the minimum number of distinct geographic locations a taxon must be observed at to have geographic range measures calcualted, if below retruns NA.
#' The steps parameter typically begins with a 1 representing that all points should be used in calculations but this is not required.
#' The replacePts parameter must be set to TRUE if any of the steps require a greater number of points to be sampled than there are actual locations for a taxon, otherwise the function will fail.
#' @note If PEE values are to be calculated as a next step the steps parameter needs to have a value of 1 as its first value
#' @examples
#' \dontrun{
#' data(BivalvePBDB)
#' BivalveMatrix<-CoordList_PBDB(BivalvePBDB)
#' GeoRarefaction_MultiTaxa(nLocCut=20,OccMatrix=BivalveMatrix,TaxaStart=3,replacePts=TRUE)
#' }
#' @seealso See the velociraptr package for details of the downloadPBDB() function
#' @export
#nLocCut parameter is the minimum number of locations a taxon must be seen at to have geo range calculated, if below returns NA
GeoRarefaction_MultiTaxa<-function(nLocCut=3,OccMatrix,TaxaStart,LongPos=1,LatPos=2,iter=10,CellSize=5,longBounds=c(-180,180),latBounds=c(-90,90),steps=c(1,50,40,30,20,10,5),replacePts=FALSE){
  n<-length(OccMatrix[1,])
  a<-vector("list",n-(TaxaStart-1))
  YesPos<-c()
  for(k in TaxaStart:n){
    TaxonID<-names(OccMatrix[1,])[k]
    taxPos<-which(OccMatrix[,k]!=0)
    if(length(taxPos)!=0){
      nCoordCheck<-CoordCollapse(OccMatrix[taxPos,LongPos],OccMatrix[taxPos,LatPos])
      nCoords<-length(nCoordCheck[,1])
      print(paste(TaxonID,k-(TaxaStart-1)))
      if(nCoords>=nLocCut){
        b<-GeoRarefaction_SingleTaxon(TName=TaxonID,OccMatrix=OccMatrix,LongPos=LongPos,LatPos=LatPos,iter=iter,CellSize=CellSize,longBounds=longBounds,latBounds=latBounds,steps=steps,replacePts=replacePts)
        a[[k-(TaxaStart-1)]]<-b
        YesPos<-c(YesPos,k-(TaxaStart-1))
      }
      else{
        b<-list(TaxonName=TaxonID,MST_mat=NA,CH_mat=NA,GCD_mat=NA,LatRg_mat=NA,LonRg_mat=NA,CellCount_mat=NA)
        a[[k-(TaxaStart-1)]]<-b
      }
    }
    else{
      b<-list(TaxonName=TaxonID,MST_mat=NA,CH_mat=NA,GCD_mat=NA,LatRg_mat=NA,LonRg_mat=NA,CellCount_mat=NA)
      a[[k-(TaxaStart-1)]]<-b
    }
  }
  YesLen<-length(YesPos)
  if(YesLen>0){
    c<-vector("list",YesLen)
    for(i in 1:YesLen){
      c[[i]]<-a[[YesPos[i]]]
    }
    return(c)
  }
  else{
    print("")
    print("No taxa above nLocCut")
    return(a)
  }
}
