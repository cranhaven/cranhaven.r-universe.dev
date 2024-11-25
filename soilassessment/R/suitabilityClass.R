suitabilityClass=function(value,crop,factor="rain"){
  if(factor=="rain"){
    crop==crop
    suitclass=rainSuit(value,crop)
  }
  else if(factor=="lgp"){
    crop==crop
    suitclass=LGPSuit(value,crop)
  }
  else if(factor=="temperature"){
    crop==crop
    suitclass=tempSuit(value,crop)

  }
  else if(factor=="carbon"){
    crop==crop
    suitclass=SOCSuit(value,crop)
  }
  else if(factor=="depth"){
    crop==crop
    suitclass=depthSuit(value,crop)
  }
  else if(factor=="carbonate"){
    crop==crop
    suitclass=carbonateSuit(value,crop)
  }
  else if(factor=="EC"){
    crop==crop
    suitclass=ECSuit(value,crop)
  }
  else if(factor=="ph"){
    crop==crop
    suitclass=PHSuit(value,crop)
  }
  else if(factor=="stone"){
    crop==crop
    suitclass=stoneSuit(value,crop)
  }
  else if(factor=="CEC"){
    crop==crop
    suitclass=CECSuit(value,crop)
  }
  else if(factor=="texture"){
    crop==crop
    suitclass=textureSuit(value,crop)
  }
  else if(factor=="slope"){
    crop==crop
    suitclass=slopeSuit(value,crop)
  }
  else if(factor=="drainage"){
    crop==crop
    suitclass=drainageSuit(value,crop)
  }
  else if(factor=="ESP"){
    crop==crop
    suitclass=ESPSuit(value,crop)
  }
  else if(factor=="fertility"){
    crop==crop
    suitclass=fertilitySuit(value,crop)
  }
  return(suitclass)
}
