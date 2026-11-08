#' Calculates the minimum spanning tree distance, in kilometers, using Prim's Algorithm [1] and a previously calculated pairwsie distance matrix
#' @param longs - Longitudinal occurrences in decimal degrees
#' @param lats - Latitudinal occurrences in decimal degrees
#' @param DistMat - Pairwsie distance matrix of coordinates, from the PWMatrix() function
#' @details Uses Prim's algorithm for finding the minimum spanning tree
#' @return Returns the minimum spanning tree distance in kilometers, the pairwise distance matrix of occurrences, the order points were connected in, and a 2-column array of coordinates
#' @examples
#' MSTCalc<-MSTDist(longs=c(22,44,-12,67),lats=c(-77,56,22,56))
#' MSTDist_FromMat(MSTCalc$Longitude,MSTCalc$Latitude,MSTCalc$MST_DistMat)
#' @references
#' [1] Prim, R.C. 1957. Shortest Connection Networks and Some Generalizations. The Bell System Technical Journal 36:1389-1401.
#' @export
MSTDist_FromMat<-function(longs,lats,DistMat){
  GCDMatrix<-DistMat
  Coords<-CoordCollapse(longs,lats)
  sites<-length(Coords$Latitude)
  if(sites==1){
    res_mat<-list(MST_km=0,MST_DistMat=NA,MST_pt_connects=1,Longitude=Coords$Longitude,Latitude=Coords$Latitude)
  }
  if(sites>1){
    GCDMatrixClone<-GCDMatrix
    Vert_Start<-sample.int(length(GCDMatrix[,1]),1)
    VertDist<-0
    VertList<-c(Vert_Start)
    VertPairConnects<-c()
    p<-1
    ConnectedVertInd<-Vert_Start
    while(p < sites){
      NextVertInd<-0
      NextVert_A<-min(GCDMatrix[,VertList],na.rm=TRUE)
      NextVert_B<-min(GCDMatrix[VertList,],na.rm=TRUE)
      if(NextVert_A<=NextVert_B){
        NextVert<-which(GCDMatrix==NextVert_A)
      }
      else if(NextVert_A>NextVert_B){
        NextVert<-which(GCDMatrix==NextVert_B)
      }
      pathChoose<-sample.int(length(NextVert),1)
      NextVert_row<-NextVert[pathChoose]%%length(GCDMatrix[,1])
      if(NextVert_row==0){
        NextVert_row=length(GCDMatrix[,1])
      }
      NextVert_col<-ceiling(NextVert[pathChoose]/length(GCDMatrix[,1]))
      TruthCheck<-0
      for(i in VertList){
        if(NextVert_row==as.integer(i)){
          TruthCheck<-1
        }
      }
      if(TruthCheck==1){
        NextVertInd<-NextVert_col
        ConnectedVertInd<-NextVert_row
      }
      else{
        NextVertInd<-NextVert_row
        ConnectedVertInd<-NextVert_col
      }
      VertPairConnects<-c(VertPairConnects,ConnectedVertInd,NextVertInd)
      VertDist<-VertDist+GCDMatrix[NextVert_row,NextVert_col]
      VertList<-c(VertList,NextVertInd)
      GCDMatrix[VertList[which(c(VertList)<NextVertInd)],NextVertInd]=NA
      GCDMatrix[NextVertInd,VertList[which(c(VertList)>NextVertInd)]]=NA
      if(NextVertInd==ConnectedVertInd){
        print("ERROR, points trying to connect to themselves!")
        stop()
      }
      p<-length(unique(VertList))
    }
    res_mat<-list(MST_km=VertDist,MST_DistMat=GCDMatrixClone,MST_pt_connects=VertPairConnects,Longitude=Coords$Longitude,Latitude=Coords$Latitude)
  }
  return(res_mat)
}

