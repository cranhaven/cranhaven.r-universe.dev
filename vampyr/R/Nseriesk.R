Nseriesk<-function(N,k){

  r <- k
  nod <- N

  contador<-matrix(1,nod^r,r)

  c<-size(contador)[1]
  tmp<-size(contador)[2]

  for (i in 2:c){

    pos <- 1
    again <- 1

    while (again){

      if (contador[i-1,pos] < nod){
        if (pos==1){
          contador[i,] <- contador[i-1,]
        }
        else {
          contador[i,pos:tmp] <- contador[i-1,pos:tmp]
        }
        contador[i,pos] <- contador[i,pos]+1
        again <- 0
      }
      else {
        pos <- pos+1
      }
    }
  }

  tmp <- numeric()
  for (i in r:1){
    tmp <- rbind(tmp,contador[,i])
  }
  contador <- transpose(tmp)

}
