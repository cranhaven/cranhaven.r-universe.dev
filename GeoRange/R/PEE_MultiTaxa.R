#' Function to compile the PBDB_PEE_SingleTaxon output for a list of taxa
#' @param GeoRare_Multi - The list of geographic range measures calculated from the GeoRarefaction_MultiTaxa function
#' @return Returns a vector list of six geographic range measures matrix with percent error of estimates [1] for each value
#' @examples
#' \dontrun{
#' data(BivalvePBDB)
#' BivalveMatrix<-CoordList_PBDB(BivalvePBDB)
#' BivalveGeo<-GeoRarefaction_MultiTaxa(nLocCut=20,OccMatrix=BivalveMatrix,TaxaStart=3,replacePts=TRUE)
#' PEE_MultiTaxa(BivalveGeo)
#' }
#' @seealso See the velociraptr package for details of the downloadPBDB() function
#' @references
#' [1] Russell, M.P. & D.R. Lindberg. 1988. Real and Random Patterns Associated with Molluscan Spatial and Temporal Distributions. Paleobiology 14:322-330.
#' @export
PEE_MultiTaxa<-function(GeoRare_Multi){
  n<-length(GeoRare_Multi)
  a<-vector("list",n)
  for(k in 1:n){
    print(paste(GeoRare_Multi[[k]]$TaxonName,k))
    b<-PEE_SingleTaxon(GeoRare=GeoRare_Multi[[k]],TName=GeoRare_Multi[[k]]$TaxonName)
    a[[k]]<-b
  }
  return(a)
}
