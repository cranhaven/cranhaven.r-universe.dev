#' Creates a sparse pairwise distance matrix of a coordinate set
#' @param Coords - Two-dimensional array of longitudinal and latitudinal coordinates output from CoordCollapse() function
#' @return Returns a sparse pairwise distance matrix of great circle distances between pairs of points
#' @examples
#' longs<-runif(10,-22,33)
#' lats<-runif(10,-22,33)
#' Coords<-CoordCollapse(longs,lats)
#' PWMatrix(Coords)
#' @export
PWMatrix<-function(Coords){
  NLats<-length(Coords$Latitude)
  for(t in 1:NLats){
    Coords[,1][t]<-Coords[,1][t]+runif(1,min=-0.0001,max=0.0001)
    Coords[,2][t]<-Coords[,2][t]+runif(1,min=-0.0001,max=0.0001)
  }
  GCD_matrix<-data.frame(matrix(NA,nrow=NLats,ncol=NLats))
  for(m in 1:NLats){
    for(n in 1:NLats){
      if(m<n){
        GCD_matrix[m,n]<-gcd_vif(Coords[,1][m],Coords[,2][m],Coords[,1][n],Coords[,2][n])
        if(match(NA,GCD_matrix[m,n],nomatch=0)>0){
          GCD_matrix[m,n]<-gcd_hf(Coords[,1][m],Coords[,2][m],Coords[,1][n],Coords[,2][n])
        }
      }
    }
  }
  return(GCD_matrix)
}

