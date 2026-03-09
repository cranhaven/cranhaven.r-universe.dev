permeabilityClass=function(texture){
  #permeability classes:1-VerySlow, 2-Slow; 3-ModerateSlow; 4-Moderate; 5-ModerateRapid; 6-Rapid; 7-VeryRapid
  if(missing(texture)){stop("texture missing")}
  if(is.character(texture)){
  if(texture=="Cl"){permclass=1}
  else if(texture=="SiCl"){permclass=2}
  else if(texture=="SiClLo"){permclass=3}
  else if(texture=="SiLo"){permclass=4}
  else if(texture=="SaLo"){permclass=5}
  else if(texture=="Si"){permclass=4}
  else if(texture=="ClLo"){permclass=3}
  else if(texture=="SaCl"){permclass=2}
  else if(texture=="SaClLo"){permclass=3}
  else if(texture=="LoSa"){permclass=6}
  else if(texture=="Lo"){permclass=4}
  else if(texture=="Sa"){permclass=6}
  else if(texture=="CSa"){permclass=7}
  else if(texture=="MSa"){permclass=7}
  else if(texture=="FSa"){permclass=7}
  else if(texture=="HCl"){permclass=1}
  else{warning("texture not defined")}
  }
  else {
    permclass=ifelse(texture==1,1,ifelse(texture==8,2,ifelse(texture==2,2,ifelse(texture==7,3,ifelse(texture==9,3,ifelse(texture==3,3,
                                                                                                                         ifelse(texture==15,4,ifelse(texture==11,4,ifelse(texture==4,4,ifelse(texture==6,4,ifelse(texture==5,5,ifelse(texture==10,6,
                                                                                                                                                                                                                                      ifelse(texture==12,6,ifelse(texture==13,7,ifelse(texture==16,1,0)))))))))))))))

  }
  return(permclass)
}
