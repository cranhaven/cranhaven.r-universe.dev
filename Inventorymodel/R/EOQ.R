EOQ <-
function(n=NA,a=NA,d=NA,h=NA,m=NA){
  
  cat("EOQ model", sep="\n")
  
  if (sum(is.na(h)==T)==length(h)){
    cat("A value for h is necessary to determinate the optimal orders.", sep="\n")
  }  else { 
    if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
      #caso demanda "d" desconocida
      if (sum(is.na(m)!=T)==length(m)&sum(is.na(d)==T)==length(d)){
        pedido<-2*a*m/h
      } else {
        if (sum(is.na(d)==T)!=length(d)){
          pedido<-sqrt(2*a*d/h)
          m=d/pedido
        }
      }
    } else {
      cat("Values for d or m are necessary to determinate the optimal orders.", sep="\n")
    }
  }
  coste_pedido<-2*a*m

  
  if (sum(is.na(h)==T)==length(h)){
    a<-0
  }  else { 
    if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
      #caso demanda "d" desconocida
        sol<-list(pedido,coste_pedido)
        names(sol)<-c("Optimal order","Order cost")
        return(sol)
      } else {
      a<-0
    }
  }

} 
