RotCtempcorrection=function(temperature)   {
  if(temperature<(-18.2)){
    tempfact=0
  }
  else {
    tempfact= 47.9/(1 + exp(106/(temperature + 18.3)))
  }
  return(tempfact)
}
