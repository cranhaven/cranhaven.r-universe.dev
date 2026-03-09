suitability=function(df,data){
  total <- 20
  amtx = pairwiseComparisonMatrix(df)
  wts = calculateWeights(amtx)
  for(i in 1:10){
    Sys.sleep(0.5)
    setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
  }
  outdat=calculateAHP(wts, data)

  for(i in 10:total){
    Sys.sleep(0.5)
    setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
  }
  return(outdat)
}
