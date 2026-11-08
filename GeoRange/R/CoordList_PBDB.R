#' Creates an occurrence matrix of taxa by coordinates from the Paleobiology Database
#' @param pbdb_data - Matrix of occurrence records from the Paleobiology Database, see downloadPBDB function in velociraptr package
#' @return Returns a taxa by coordinates matrix of occurrences
#' @examples
#' data(BivalvePBDB)
#' CoordList_PBDB(BivalvePBDB)
#' @details Cuts out records for which there is no paleogeographic information known
#' @seealso See the velociraptr package for more details on downloading PBDB data
#' @export
CoordList_PBDB<-function(pbdb_data){
  LocUnknown<-which(is.na(pbdb_data$paleolng))
  if(length(LocUnknown)!=0){
    pbdb_data<-pbdb_data[-LocUnknown,]
  }
  LocUnknown<-which(is.na(pbdb_data$paleolat))
  if(length(LocUnknown)!=0){
    pbdb_data<-pbdb_data[-LocUnknown,]
  }
  nOccs<-length(pbdb_data[,1])
  LongList<-c()
  LatList<-c()
  for(i in 1:nOccs){
    LongList<-c(LongList,pbdb_data$paleolng[i])
    LatList<-c(LatList,pbdb_data$paleolat[i])
  }
  Coords<-CoordCollapse(LongList,LatList)
  nCoords<-length(Coords[,1])
  TNames<-unique(as.character(pbdb_data$accepted_name))
  nTaxa<-length(TNames)
  TaxaLoc_mat<-data.frame(matrix(NA,nrow=nCoords,ncol=nTaxa+2),row.names=seq(from=1,to=nCoords,by=1))
  TaxaLoc_mat[,1]<-Coords[,1]
  TaxaLoc_mat[,2]<-Coords[,2]
  for(j in 1:nTaxa){
    taxPos<-which(pbdb_data$accepted_name==TNames[j])
    TaxaLongList<-c(pbdb_data$paleolng[taxPos])
    TaxaLatList<-c(pbdb_data$paleolat[taxPos])
    TaxaCoords<-CoordCollapse(TaxaLongList,TaxaLatList)
    nTaxaCoords<-length(TaxaCoords[,1])
    PosList<-c()
    for(m in 1:nTaxaCoords){
      LongPos<-which(TaxaLoc_mat[,1]==TaxaCoords[m,1])
      LatPos<-which(TaxaLoc_mat[,2]==TaxaCoords[m,2])
      nLongPos<-length(LongPos)
      nLatPos<-length(LatPos)
      for(n in 1:nLongPos){
        for(p in 1:nLatPos){
          if(LongPos[n]==LatPos[p]){
            TaxaLoc_mat[LongPos[n],j+2]<-1
          }
        }
      }
    }
    TaxaLoc_mat[PosList,j+2]<-1
  }
  ColHeads<-c("Longitude","Latitude",TNames)
  names(TaxaLoc_mat)<-ColHeads
  return(TaxaLoc_mat)
}
