comparisonTable=function(df){
  nmtx = pairwiseComparisonMatrix(df)
  crt=consistencyRatio(nmtx)
  nmtx=nmtx@values
  return(nmtx)
}
