imageIndices=function(blue, green,red,nir,swir1,swir2,index="NDVI"){

  if(index=="NSI"){ratio=(swir1-swir2)/(swir1-nir+0.001)}
  else if(index=="SI1"){ratio=sqrt(green*red)}
  else if(index=="SI2"){ratio=sqrt(blue*red)}
  else if(index=="SI3"){ratio=sqrt((green)^2+(red)^2)}
  else if(index=="SI4"){
    rat=(nir*swir1-(swir1)^2)
    ratio=ifelse(rat<0,0,rat/nir)}
  else if(index=="SI5"){ratio=blue/red}
  else if(index=="SI6"){ratio=red*nir/green}
  else if(index=="SAVI"){
    rat=1.5*(nir-red)
    rat1=(0.5+nir+red)
    ratio=ifelse(rat<0,0,ifelse(rat1<0,0,(rat/(rat1))))
  }
  else if(index=="VSSI"){ratio=2*green-5*(red+nir)}
  else if(index=="NDSI"){ratio=(red-nir)/(red+nir)}
  else if(index=="NDVI"){ratio=(nir-red)/(nir+red)}
  else if(index=="SR"){ratio=(green-red)/(blue+red)}
  else if(index=="CRSI"){
    rat=(nir*red-blue*green)
    rat1 = (nir*green+blue*green)
    ratio=ifelse(rat<0,0,ifelse(rat1<0,0,sqrt(rat/(rat1))))
    }
  else if(index=="BI"){ratio=sqrt(green^2+red^2+nir^2)}
  else if(index=="NDSnI"){ratio=(green-swir1)/(green+swir1)}
  else if(index=="ROCK"){ratio=nir/swir1}
  else if(index=="NDBI"){ratio=(swir1-nir)/(swir1+nir)}
  else if(index=="NBR"){ratio=(swir1-swir2)/(swir1+swir2)}
  else if(index=="CLAY"){ratio=swir1/swir1}
  else if(index=="NDMI"){ratio=(nir-0.5*(swir1+swir2))/(nir+0.5*(swir1+swir2))}
  else if(index=="NDWI"){ratio=(green-nir)/(green+nir)}
  else if(index=="TNDVI"){
    rat=(nir-red)/(nir+red)
    ratio=ifelse(rat<(-0.5),0,sqrt(rat+0.5))
    }
  return(ratio)
}
