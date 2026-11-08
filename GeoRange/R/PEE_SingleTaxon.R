#' Function to calculate PEE [1] matrices for all the geographic range measures
#' @param GeoRare - The list of geographic range measures calculated from the GeoRarefaction_SingleTaxon or GeoRarefaction_MultiTaxa functions
#' @param TName - Name of the target taxon for analysis as a string
#' @return Returns a list of six geographic range measures matrix with percent error of estimates for each value
#' @examples
#' \dontrun{
#' data(BivalvePBDB)
#' BivalveMatrix<-CoordList_PBDB(BivalvePBDB)
#' BivalveGeo<-GeoRarefaction_MultiTaxa(nLocCut=50,OccMatrix=BivalveMatrix,TaxaStart=3,iter=20)
#' PEE_SingleTaxon(GeoRare=BivalveGeo,TName=names(BivalveGeo)[3])
#' }
#' @seealso See the velociraptr package for details of the downloadPBDB() function
#' @references
#' [1] Russell, M.P. & D.R. Lindberg. 1988. Real and Random Patterns Associated with Molluscan Spatial and Temporal Distributions. Paleobiology 14:322-330.
#' @export
PEE_SingleTaxon<-function(GeoRare,TName="Brach 1"){
  TrueMST<-GeoRare$MST_mat[1,1]
  TrueCH<-GeoRare$CH_mat[1,1]
  TrueGCD<-GeoRare$GCD_mat[1,1]
  TrueLatRg<-GeoRare$LatRg_mat[1,1]
  TrueLongRg<-GeoRare$LonRg_mat[1,1]
  TrueCellCount<-GeoRare$CellCount_mat[1,1]

  MST_dif<-GeoRare$MST_mat
  CH_dif<-GeoRare$CH_mat
  GCD_dif<-GeoRare$GCD_mat
  LatRg_dif<-GeoRare$LatRg_mat
  LonRg_dif<-GeoRare$LonRg_mat
  CellCount_dif<-GeoRare$CellCount_mat

  nSteps<-length(GeoRare$MST_mat[,1])
  nIters<-length(GeoRare$MST_mat[1,])

  for(i in 1:nSteps){
    for(j in 1:nIters){
      MST_dif[i,j]<-((TrueMST-MST_dif[i,j])/TrueMST)*100
      CH_dif[i,j]<-((TrueCH-CH_dif[i,j])/TrueCH)*100
      GCD_dif[i,j]<-((TrueGCD-GCD_dif[i,j])/TrueGCD)*100
      LatRg_dif[i,j]<-((TrueLatRg-LatRg_dif[i,j])/TrueLatRg)*100
      LonRg_dif[i,j]<-((TrueLongRg-LonRg_dif[i,j])/TrueLongRg)*100
      CellCount_dif[i,j]<-((TrueCellCount-CellCount_dif[i,j])/TrueCellCount)*100
    }
  }
  return(list(TaxonName=TName,MST_dif=MST_dif,CH_dif=CH_dif,GCD_dif=GCD_dif,LatRg_dif=LatRg_dif,LonRg_dif=LonRg_dif,CellCount_dif=CellCount_dif))
}
