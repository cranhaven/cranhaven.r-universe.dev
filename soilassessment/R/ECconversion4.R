ECconversion4=function(x,target="TDS"){

  if(target=="TDS"){
    outSalt=ifelse(x<3200.001,x/640,x/800)
  }
  else if(target=="TSS"){
    outSalt=x/10
  }
  
  else if(target=="mmho"){
    outSalt=1*x
  }
   return(outSalt)
}

