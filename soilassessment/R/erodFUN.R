erodFUN=function(sand,silt,clay,OC,texture,Struct,method="Wisch1"){
  total <- 20
  M = silt*(100-clay)
  OM=1.72*OC
  SN=1-(sand/100)
  Dg = exp(0.01*(sand*log(1.025)+silt*log(0.026)+clay*log(0.00125)))
  A1=0.2+0.3*exp(-0.0256*sand*(1-silt/100))
  B1=(silt/(clay+silt))^0.3
  C1=(1-0.25*clay/(clay+exp(3.72-2.95*clay)))
  D1=(1-0.7*SN/(SN+exp(-5.41+22.9*SN)))
  permeb=permeabilityClass(texture)
  if(method=="WSmith"){K=((2.1*(M^1.14)*(10^-4)*(12-OM))+(3.25*(permeb-2))+(2.5*(Struct-3)))/100}#Wishmeier and Smith (1978)
  else if (method=="Yang"){K=(((2.1*(M^1.14)*(10^-4)*(12-OM))+(3.25*(permeb-2))+(2.5*(Struct-3)))/100)*0.1317}
  else if(method=="Renard"){K=(0.0034+0.0405*exp(-0.5*(((log10(Dg)+1.659))/0.7101)^2))*7.594}#{K=(0.0034+0.0387*exp(-0.5*(((log10(Dg)+1.533)^2)/0.7671^2)))} #
  else if(method=="Bouyoucos"){K=0.01*(sand+silt)/clay}
  else if(method=="Denardin"){K=0.00000748*M+0.00448059*Struct-0.0631175*Dg+0.010396*OM/Dg}
  else if(method=="Wang"){K=(0.0667-0.0013*((log(OM/Dg)-5.6706)^2)-0.015*exp(-28.9589*((log10(Dg)+1.827)^2)))*7.594}
  else if(method=="Wisch1"){K=2.73*10^-6*M^1.14*(12-OM)+3.25*10^-2*(permeb-2)+2.5*10^-2*(Struct-3)}#Wischmeier et al.(1971)
  else if(method=="Sharpley"){K=A1*B1*C1*D1*0.1317}
  else if(method=="Wisch2"){K=2.77*10^-6*M^1.14*(12-OM)+0.0043*(permeb-2)+0.0033*(4-Struct)}#Wischmeier (1970)
  else if (method=="cheng"){K=0.2+0.3*exp(0.0256*sand*SN)*((silt/(clay+silt))^0.3)*(1-0.25*OC/(OC+exp(3.72-2.95*OC)))*(1-((0.7*SN)/(SN+exp(-5.51+22.92*SN))))}
  else if(method=="Auer"){
    if(OM<=4){K2=(12-OM)/10}else{K2=0.8}
    if(silt<=70){K1=2.77*10^-5*M^1.14}else{K1=1.75*10^-5*M^1.14+0.0024*silt+0.16}
    if(K1*K2>0.2){K=K1*K2+0.043*(permeb-2)+0.033*(Struct-3)}else{K=0.091-0.34*K1*K2+1.79*(K1*K2)^2+0.24*K1*K2*permeb+0.033*(Struct-3)} #Auer at al. (2014)
  }
  for(i in 1:total){
    Sys.sleep(0.1)
    setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
  }
  return(K)
}
