"%==%"<- function(a,b){
  if (length(a)==1){
    (1:length(b))[a == b]
  }else if(length(a) > 1){
    for (i in 1:length(a)) {
      if (i==1){location=list()}
      location.i=(1:length(b))[a[i] == b]
      location=c(location,list(location.i))
      names(location)[length(location)]=a[i]
    }
    location
  }
}
