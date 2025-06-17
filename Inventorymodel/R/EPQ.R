EPQ <-
function(n=NA,a=NA,d=NA,h=NA,m=NA,r=NA,b=NA){
  

  
  cat("EPQ model", sep="\n")
  
  s<-b
  if (sum(is.na(h)==T)==length(h)){
    cat("A value for h is necessary to determinate the optimal orders.", sep="\n")
  }  else { 
    if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
      #caso demanda "d" conocida
      if (sum(is.na(m)!=T)==length(m)&sum(is.na(d)==T)==length(d)){
        pedido<-2*a*m/h
      } else {
        if (sum(is.na(h)==T)==length(h)|sum(is.na(s)==T)==length(s)|sum(is.na(r)==T)==length(r)){
          cat("Values for r and s are necessary to determinate the optimal orders and shortages.", sep="\n")
        }  else {
          if (sum(r>d)==length(r)){
            pedido<-sqrt(2*a*d*(h+s)/(h*(1-d/r)*s))
            faltantes<-sqrt(2*a*d*h*(1-d/r)/(s*(h+s)))
            m=d/pedido
          } else {
            cat("Check that the values of r_i are bigger that d_i.", sep="\n")
          }
        }
      }
    } else {
      cat("A value of d or m are necessary to determinate the optimal orders.", sep="\n")
    }
  }
  
  
coste_pedido<-2*a*m

if (sum(is.na(h)==T)==length(h)){
  a<-0
}  else { 
  if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
    #caso demanda "d" conocida
    if (sum(is.na(m)!=T)==length(m)&sum(is.na(d)==T)==length(d)){
      sol<-list(pedido,coste_pedido)
      names(sol)<-c("Optimal order","Order costs")
      return(sol)
    } else {
      if (sum(is.na(h)==T)==length(h)|sum(is.na(s)==T)==length(s)|sum(is.na(r)==T)==length(r)){
        a<-0
      }  else {
        if (sum(r>d)==length(r)){
          sol<-list(pedido,faltantes,coste_pedido)
          names(sol)<-c("Optimal order","Optimal shortages","Order costs")
          return(sol)
        }
      }
    }
  } else {
    cat("A value of d, h or m are necessary to determinate the optimal orders.", sep="\n")
  }
}





}
 