VegCOV=function(pcover, model="cai"){
  fg=ifelse(pcover<0,NA,ifelse(pcover>100,100,pcover))
  covexpr1=function(x,A,B){
    y=A*exp(B*x)
    return(y)}
  if(model=="cai"){cafc=ifelse(fg<0.0001,1,ifelse(fg<78.4,0.6508-0.3436*log10(fg),0.001))}
  if(model=="xu"){cafc=harmonization(fg,A=-0.01, B=1)}
  if(model=="hurni"){cafc=covexpr1(fg,A=0.25, B=-0.0529)}
  if(model=="jin"){cafc=covexpr1(fg,A=0.992, B=-0.034)}
  if(model=="liu"){cafc=0.221-0.595*log10(fg/100)}
  cover=ifelse(cafc<0,NA,ifelse(cafc>1,1,cafc))
  return(cover)
}
