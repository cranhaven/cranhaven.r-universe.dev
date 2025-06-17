STI <-
function(n=NA,a=NA,av=NA,d=NA,h=NA,m=NA){

  cat("STI model", sep="\n")
  
  if (sum(is.na(h)==T)==length(h)|sum(is.na(av)==T)==length(av)){
    cat("Values for h and av are necessary to determinate the optimal orders.", sep="\n")
  }  else { 
    if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
      #caso demanda "d" desconocida
      if (sum(is.na(m)!=T)==length(m)&sum(is.na(d)==T)==length(d)){
        pedido<-2*(a+av)*m/h
      } else {
        if (sum(is.na(d)==T)!=length(d)){
          pedido<-sqrt(2*(a+av)*d/h)
          m=d/pedido
        }
      }
      coste_pedido<-2*(a+av)*m
      
    } else {
      cat("Values for d or m are necessary to determinate the optimal orders.", sep="\n")
    }
  }

if (sum(is.na(h)==T)==length(h)){
  a<-0
}  else { 
  if ((sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d))&sum(is.na(av)==T)!=length(av)){ 
    #caso demanda "d" desconocida
    sol<-list(pedido,coste_pedido)
    names(sol)<-c("Optimal order","Order cost")
    return(sol)
  } else {
    a<-0
  }
}

}
  

