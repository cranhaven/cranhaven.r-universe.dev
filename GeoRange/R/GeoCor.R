#' Function to calculate the correlation coefficient for pairwise comparisons between geographic range measures
#' @param GeoRange - A matrix of taxa by geographic range calculations, as from the GeoRange_MultiTaxa function
#' @param Start - The column index value where geographic range measures to be compared starts
#' @param method - The correlation method to be used. See the cor() function for available inputs
#' @return Returns a sparse pairwsie matrix of correlation coefficients
#' @examples
#' \dontrun{
#' data(BivalvePBDB)
#' BivalveMatrix<-CoordList_PBDB(BivalvePBDB)
#' testBivalve<-GeoRange_MultiTaxa(OccMatrix=BivalveMatrix,TaxaStart=3)
#' GeoCor(testBivalve,Start=1,method="kendall")
#' }
#' @note The correlation calculation uses the "pairwise.complete.obs" option from the cor function so that only complete pairs of observations are used, pairs containing an NA are ignored
#' @seealso See the velociraptr package for details of the downloadPBDB() function
#' @importFrom stats cor
#' @export
GeoCor<-function(GeoRange,Start=1,method="pearson"){
  nMes<-length(GeoRange[1,])
  Cor_mat<-data.frame(matrix(NA,nrow=nMes-(Start-1),ncol=nMes-(Start-1)),row.names=names(GeoRange[1,Start:nMes]))
  for(i in Start:nMes){
    for(j in Start:nMes){
      if(i<j){
        Cor_mat[i,j]<-cor(GeoRange[,i],GeoRange[,j],method=method,use="pairwise.complete.obs")
      }
    }
  }
  names(Cor_mat)<-names(GeoRange[1,Start:nMes])
  return(Cor_mat)
}
