fertilityRating=function(value,indicator="nitrogen"){
  if(indicator=="nitrogen"){CLSS=ifelse(value<0.01,4,ifelse(value<0.05,3,ifelse(value<0.15,2,1)))}
  else if(indicator=="phosphorus"){CLSS=ifelse(value<8,4,ifelse(value<16,3,ifelse(value<24,2,1)))}
  else if(indicator=="potassium"){CLSS=ifelse(value<39,4,ifelse(value<78,3,ifelse(value<156,2,1)))}
  else if(indicator=="carbon"){CLSS=ifelse(value<0.15,4,ifelse(value<0.75,3,ifelse(value<2.25,2,1)))}
  else if(indicator=="iron"){CLSS=ifelse(value<2,4,ifelse(value<4,3,ifelse(value<6,2,1)))}
  else if(indicator=="zinc"){CLSS=ifelse(value<0.5,4,ifelse(value<1,3,ifelse(value<3.5,2,1)))}
  else if(indicator=="manganese"){CLSS=ifelse(value<0.5,4,ifelse(value<1.2,3,ifelse(value<3.5,2,1)))}
  else if(indicator=="boron"){CLSS=ifelse(value<0.05,4,ifelse(value<0.2,3,ifelse(value<0.65,2,1)))}
  else if(indicator=="sulfur"){CLSS=ifelse(value<3,4,ifelse(value<10,3,ifelse(value<20,2,1)))}
  else if(indicator=="cec") {CLSS=ifelse(value<3,4,ifelse(value<10,3,ifelse(value<20,2,1)))}
  else if(indicator=="copper"){CLSS=ifelse(value<0.1,4,ifelse(value<0.3,3,ifelse(value<0.8,2,1)))}
  return(CLSS)

}
