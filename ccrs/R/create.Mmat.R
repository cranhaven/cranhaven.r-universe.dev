
create.Mmat <- function(q){

  tvec<-(c(0,(q/2),q))/q

  ##Mmat for estimation (boundaries)
  x.bounds.es <-c(1:(q-1))/q
  tvec.es <-tvec
  Mmat.q1<- cds::ispline(x.bounds.es, tvec = tvec.es,intercept=TRUE)

  ###Mmat for cleaning data
  x.bounds.data <- (c(1:q) - 0.5)/q
  tvec.data<-tvec
  Mmat.q <- cds::ispline(x.bounds.data, tvec = tvec.data,intercept=TRUE)

  list(Mmat.q1=Mmat.q1,Mmat.q=Mmat.q)

}
