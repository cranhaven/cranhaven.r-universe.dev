ECconversion1=function(ec,texture,method="FAO", extract="1:5",oc=NULL,clay=NULL){
     OM = oc*1.721
   { a1=ifelse(OM<0.5,5,ifelse(OM<1,6,ifelse(OM<2,8,ifelse(OM<4,13,ifelse(OM<8,21,35)))))
    a2=ifelse(OM<0.5,8,ifelse(OM<1,9,ifelse(OM<2,11,ifelse(OM<4,16,ifelse(OM<8,24,38)))))
    a3=ifelse(OM<0.5,10,ifelse(OM<1,11,ifelse(OM<2,13,ifelse(OM<4,18,ifelse(OM<8,26,40)))))
    a4=ifelse(OM<0.5,14,ifelse(OM<1,15,ifelse(OM<2,17,ifelse(OM<4,22,ifelse(OM<8,30,45)))))
    a5=ifelse(OM<0.5,17,ifelse(OM<1,18,ifelse(OM<2,20,ifelse(OM<4,25,ifelse(OM<8,34,49)))))
    a6= ifelse(OM<0.5,19,ifelse(OM<1,20,ifelse(OM<2,22,ifelse(OM<4,27,ifelse(OM<8,36,51)))))
    a7=ifelse(OM<0.5,22,ifelse(OM<1,23,ifelse(OM<2,26,ifelse(OM<4,31,ifelse(OM<8,39,55)))))
    a8=ifelse(OM<0.5,25,ifelse(OM<1,26,ifelse(OM<2,29,ifelse(OM<4,34,ifelse(OM<8,42,58)))))
    a9=ifelse(OM<0.5,28,ifelse(OM<1,29,ifelse(OM<2,32,ifelse(OM<4,37,ifelse(OM<8,46,62)))))
    a10= ifelse(OM<0.5,32,ifelse(OM<1,33,ifelse(OM<2,36,ifelse(OM<4,41,ifelse(OM<8,50,67)))))
    a11= ifelse(OM<0.5,44,ifelse(OM<1,46,ifelse(OM<2,48,ifelse(OM<4,53,ifelse(OM<8,63,80)))))
    a12= ifelse(OM<0.5,51,ifelse(OM<1,53,ifelse(OM<2,55,ifelse(OM<4,60,ifelse(OM<8,70,88)))))
    a13= ifelse(OM<0.5,63,ifelse(OM<1,65,ifelse(OM<2,68,ifelse(OM<4,73,ifelse(OM<8,83,102)))))
    a14= ifelse(OM<0.5,105,ifelse(OM<1,107,ifelse(OM<2,110,ifelse(OM<4,116,ifelse(OM<8,126,147)))))

  }
    a15=ifelse(clay<10,a4,a7)
    a16=ifelse(clay<10,a5,a9)
  
   textu=ifelse(texture=="SaCl",3,ifelse(texture=="SiCl",3,
                                        ifelse(texture=="Cl",3,ifelse(texture=="HCl",3,ifelse(texture=="SiCl",4,
                                                                                              ifelse(texture=="Si",2,ifelse(texture=="ClLo",2,ifelse(texture=="SaClLo",2,ifelse(texture=="SiClLo",2,1)))))))))

  {ccfactor=ifelse(texture=="CS",a1,ifelse(texture=="MS",a2,ifelse(texture=="FS",a3,ifelse(texture=="LoSa",a4,
                                                                                           ifelse(texture=="SiLo",a16,ifelse(texture=="Si",a6,
                                                                                                                             ifelse(texture=="SaLo",a15,ifelse(texture=="Lo",a8,ifelse(texture=="SaClLo",a10,ifelse(texture=="ClLo",a11,
                                                                                                                                                                                                                    ifelse(texture==8,a12,ifelse(texture==2,a13,ifelse(texture==3,a11,ifelse(texture==1,a13,a14))))))))))))))}
  if (method=="FAO")  {
    if(is.null(oc))stop("OC is missing")
    else if(is.null(clay))stop("Clay content is missing")
    else if(extract=="1:1"){ElectConduct=ec*100/ccfactor}
    else if(extract=="1:1.25"){ElectConduct=ec*125/ccfactor}
    else if(extract=="1:1.5"){ElectConduct=ec*150/ccfactor}
    else if(extract=="1:2"){ElectConduct=ec*200/ccfactor}
    else if(extract=="1:2.5"){ElectConduct=ec*250/ccfactor}
    else if(extract=="1:3"){ElectConduct=ec*300/ccfactor}
    else if(extract=="1:5"){ElectConduct=ec*500/ccfactor}
    else if(extract=="1:10"){ElectConduct=ec*1000/ccfactor}

  }
  else if(method=="sonmez") {
    if(extract=="1:1"){ElectConduct=ifelse(textu==3,harmonization(ec,2.72,-1.27),ifelse(textu==2,harmonization(ec,2.15,-0.44),harmonization(ec,2.03,-0.41)))}
    else if(extract=="1:2.5"){ElectConduct=ifelse(textu==3,harmonization(ec,4.3,0.17),ifelse(textu==2,harmonization(ec,3.84,0.35),harmonization(ec,3.68,0.22)))}
    else if(extract=="1:5"){ElectConduct=ifelse(textu==3,harmonization(ec,8.22,-0.33),ifelse(textu==2,harmonization(ec,7.58,0.06),harmonization(ec,7.36,-0.24)))}
  }
  else if(method=="hogg") {
    if(extract=="1:1"){ElectConduct=ifelse(textu==3,harmonization(ec,3.01,-0.06),ifelse(textu==2,harmonization(ec,3.01,-0.77),harmonization(ec,2.66,-0.97)))}
    else if(extract=="1:2"){ElectConduct=ifelse(textu==3,harmonization(ec,3.58,0.10),ifelse(textu==2,harmonization(ec,3.23,-0.67),harmonization(ec,3.12,-0.59)))}
  }


  return(ElectConduct)
}
