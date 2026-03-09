NDVIcoverfactor=function(ndvi,model="kniff"){
  ndvi=ifelse(ndvi<0,NA,ndvi)
  ndvi = ifelse(ndvi>1,NA,ndvi)
  if(model=="patil"){cafc=harmonization(ndvi,A=-1.21,B=1.02)}
  if(model=="almagro"){cafc=harmonization(ndvi,A=-0.05, B=0.05)}
  if(model=="jamshidi"){cafc=harmonization(ndvi,A=-4.61333,B=1.2079)}
  if(model=="dejong"){cafc=harmonization(ndvi,A=-0.805, B=0.431)}
  if(model=="toumi") {cafc=harmonization(ndvi,A=-1.1667, B=0.9167)}
  if(model=="gitas") {cafc=harmonization(ndvi,A=-0.5953, B=0.407)}
  if(model=="joshi") {cafc=harmonization(ndvi,A=-1.612,  B=1.056)}
  if(model=="durigon"){cafc=harmonization(ndvi,A=-0.5, B=0.5)}
  covexpr1=function(x,A,B){
    y=A*exp(B*x)
    return(y)}
    if(model=="wickama"){cafc=covexpr1(ndvi, A=0.227, B=-0.997)}
  if(model=="suriyaprasit"){cafc=covexpr1(ndvi, A=0.227, B=-7.337)}
  if(model=="kniff"){cafc=exp(-2*ndvi/(1-ndvi))}
  if(model=="lin"){cafc=1.1119*ndvi^2-2.0967*ndvi+0.9944}
  if(model=="bahrawi"){cafc=0.625*ndvi^2-1.4793*ndvi+0.8771}
  if(model=="kulikov"){cafc=exp(-2.9298*ndvi-0.7842)}
  if(model=="power"){cafc=((1-ndvi)/2)^(1+ndvi)}

  if(model=="modis"){
    fg = 108.49*ndvi+0.717
    cafc=ifelse(fg<0.11,1,ifelse(fg<78.4,0.6508-0.3436*log10(ndvi),0.0001))
  }

  cover=ifelse(cafc<0,NA,ifelse(cafc>1,1,cafc))
  return(cover)
}

