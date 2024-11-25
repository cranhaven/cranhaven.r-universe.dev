saltRating=function(ec,ph,criterion="FAO"){
  ph=round(ph*1,3)
  b1=ifelse(ec<0.75,6,ifelse(ec<2,8,ifelse(ec<4,9,ifelse(ec<8,10,ifelse(ec<15,11,12)))))
  b2=ifelse(ec<0.048,6,ifelse(ec<0.128,8,ifelse(ec<0.256,9,ifelse(ec<0.512,10,ifelse(ec<0.96,11,12)))))
    if(criterion=="FAO"){
    saltClass=ifelse(ph<8.2,b1,ifelse(ec<4,4,5))
  }
  else if(criterion=="USDA"){
    saltClass=ifelse(ph<8.5,b1,ifelse(ec<4,4,5))
  }
  else if(criterion=="PSALT"){
    saltClass=ifelse(ph<8.2,b2,ifelse(ec<0.25,4,5))
  }
  return(salinity=saltClass)
}
