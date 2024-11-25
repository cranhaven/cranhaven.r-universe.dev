negData=function(vg="ec",x){
  if(vg=="ec"){y=ifelse(x<0,NA,x)}
  else if(vg=="esp"){y=ifelse(x<0,NA,x)}
  else if(vg=="ph"){
    y=ifelse(x<1,NA,ifelse(x<=14,x,NA))
  }
  return(y)
}
