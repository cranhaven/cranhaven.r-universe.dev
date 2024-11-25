PHConversion=function(ph, model="kabala",phtype="kcl"){
  if(phtype=="kcl"){
    if(model=="kabala"){PHconvert=harmonization(log10(ph),11.58,-1.95)}
    else if (model=="sadovski"){PHconvert=harmonization(ph,0.88787,1.53466)}
  }
  else if(phtype=="cacl2"){
    if(model=="miller"){PHconvert=harmonization(ph,0.9259259,0.9009259)}
    else if(model=="davies"){PHconvert=harmonization(ph,0.9569378,0.8382775)}
    else if(model=="brennan"){PHconvert=harmonization(ph,1.089325,0.387364)}
    else if (model=="ahern"){PHconvert=harmonization(ph,1.083424,0.4041172)}
  }
  PHconvert=round(PHconvert,3)
  return(PHconvert)
}

