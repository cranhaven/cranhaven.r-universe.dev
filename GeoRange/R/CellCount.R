#' Calculates degree x degree cell counts of a specified size
#' @param longs - Array of longitudinal occurrence values in decimal degrees
#' @param lats - Array of latitudinal occurrence values in decimal degrees
#' @param CellSize - Size of each cell in degree X degree
#' @param longBounds - Array of longitudinal boundaries in decimal degrees
#' @param latBounds - Array of latitudinal boundaries in decimal degrees
#' @return Returns the number of cells occupied, specified cell size, and coordinate list
#' @note This method uses grids cells constructed by equal degrees not area. So high latitude cells will have smaller areas than those at low latitude
#' @examples
#' longs<-c(22,55,-144)
#' lats<-c(-12,22,-12)
#' CellCount(longs,lats,CellSize=5,longBounds=c(-180,180),latBounds=c(-90,90))
#' @export
CellCount<-function(longs,lats,CellSize=5,longBounds=c(-180,180),latBounds=c(-90,90)){
  longSpan<-longBounds[2]-longBounds[1]
  latSpan<-latBounds[2]-latBounds[1]
  CellOcc_List<-c()
  if(longSpan%%CellSize != 0 | latSpan%%CellSize !=0){
    print("Cell size must be able to evenly divide bounding space!")
    stop()
  }
  Coords<-CoordCollapse(longs,lats)
  if(length(Coords[,1])>0){
    TempLongs<-Coords[,1]
    TempLongs[which(TempLongs<0)]<-TempLongs[which(TempLongs<0)]+360
    TempLats<-Coords[,2]+90
    nCoords<-length(Coords[,1])
    matrixCellKey_row<-c(seq(from=latBounds[1],to=latBounds[2],by=CellSize))
    matrixCellKey_row<-((matrixCellKey_row+90)/CellSize)+1
    matrixCellKey_col<-c(seq(from=longBounds[1],to=longBounds[2],by=CellSize))
    matrixCellKey_col[which(matrixCellKey_col<0)]<-matrixCellKey_col[which(matrixCellKey_col<0)]+360
    matrixCellKey_col<-(matrixCellKey_col/CellSize)+1

    for(i in 1:nCoords){
      tempRow<-trunc((TempLats[i]/CellSize)+1,0)
      tempCol<-trunc((TempLongs[i]/CellSize)+1,0)
      if(tempCol==max(matrixCellKey_col)+1){
        tempCol<-1
      }
      CellOcc_List<-c(CellOcc_List,tempRow+(tempCol/100))
    }
    CellCount<-length(unique(CellOcc_List))
    res<-list(NumCellsOcc=CellCount,CellSize=CellSize,Coordinates=Coords)
  }
  else{
    res<-list(NumCellsOcc=0,CellSize=CellSize,Coordinate=Coords)
  }
  return(res)
}

