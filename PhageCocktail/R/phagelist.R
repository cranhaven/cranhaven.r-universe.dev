
phagelist<-function(phage_names,Result){
  list_pn<-c()
  for (i in 1:(Result[1])){
    for (n in 1:length(phage_names)){
      if (phage_names[n]==Result[i+1]){
        list_pn=c(list_pn,n)}}}

  return(list_pn)


}

