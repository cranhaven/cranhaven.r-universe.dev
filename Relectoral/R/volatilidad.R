# Script para volatilidad

#'@title Dimensionality of voting. Volatility
#'
#'@description This function calculates the total volatility of Pederson (1983:31 and 32),
#'and the volatility between blocks. The formulas for calculating these values can be
#'found in Oñate and Ocaña's book "Analysis of electoral data", page 45. This dimension
#'compares the behaviour of the electorate in two different elections, so as to see the
#'transfer of votes, either between blocks or between political parties.
#'
#'@param dat1 It is a data.frame with two columns. The first column contains the acronym
#'of the political party and the second the votes or seats obtained from the first period
#'of time, depending on whether you want to calculate the electoral or parliamentary
#'volatility respectively.
#'@param dat2 It is a data.frame with two columns. The first column contains the acronym
#'of the political party and the second the votes or seats obtained from the second period
#'of time, depending on whether you want to calculate the electoral or parliamentary
#'volatility respectively.
#'
#'@param enlace It is a data.frame that serves to link the parties, coalitions or groupings
#'that you want to compare between the two periods under study. This data.frame contains a
#'total of 22 columns. The first column contains the corresponding name of the political
#'parties. The next 10 columns (called p1_i for i=1,2,...,10) are used to indicate the
#'number of the row(s) of the party(ies) of the first electoral period to be grouped. The
#'next 10 columns (called p2_i for i=1,2,...,10) are used to indicate the number of the
#'row(s) of the party(ies) of the second electoral period to be grouped. When some of the
#'10 columns are not needed, the remaining columns are filled in with zeros. The data in each
#'row is used with following purpose: For the first block of 10 columns, the votes/seats of the
#'parties appearing in that block are added up. For the second block of 10 columns, the
#'votes/seats in that second block are also added up. These two values will be used later on
#'for comparison. The last column of this data.frame (called "block"), will contain the values
#'"D" or "I" indicating "Right", "Left", respectively.
#'
#'To clarify all these concepts execute in R *browseVignettes("Relectoral")* or *vignette("Volatility")*
#'
#' @return It returns a list with two objects. The first is the total volatility, and the second
#' is the inter-block volatility.
#'
#'
#'
#'@export
#'@md
volatilidad <- function(dat1,dat2,enlace){
  if(ncol(dat1) != 2) stop("The data.frame dat1 must contain 2 columns")
  if(ncol(dat2) != 2) stop("The data.frame dat2 must contain 2 columns")
  if(ncol(enlace) != 22) stop("The data.frame 'enlace' must have 22 columns")
  res <- matrix(NaN, ncol = 4,nrow = nrow(enlace))
  colnames(res) <- c("DENO","V1","V2","BLOQUE")
  res <- data.frame(res)
  for(h in 1:nrow(res)){
    # incluimos la sigla del partido
    res[h,1] <- enlace[h,1]
    # incluyo el bolque
    res[h,4] <- enlace[h,ncol(enlace)]
    # calculo los votos del primer proceso electoral
    p1=0
    for(m in 1:10){
      if(enlace[h,1+m]>0){
        p1 = p1+dat1[enlace[h,1+m],2]
      }
    }
    res[h,2]<-p1
    #calculo los votos del segundo proceso electoral
    p2=0
    for(m in 1:10){
      if(enlace[h,11+m]>0){
        p2 = p2+dat2[enlace[h,11+m],2]
      }
    }
    res[h,3] <- p2
  }
  #Calculo las columnas con los porcentajes
  res$V1_p <- (res$V1/sum(res$V1))*100
  res$V2_p <- (res$V2/sum(res$V2))*100
  # calculo el valor absoluto de las diferencias
  res$dif <- abs(res$V2_p-res$V1_p)
  VT= 0.5*sum(res$dif)
  #Volatilidad entre bloques
  Derecha=0
  izquierda=0
  for(m in 1:nrow(enlace)){
    if(enlace[m,ncol(enlace)]=="D"){
      Derecha=Derecha+res[m,"V2_p"]-res[m,"V1_p"]
    }else{
      izquierda=izquierda+res[m,"V2_p"]-res[m,"V1_p"]
    }
  }

  VB=0.5*(abs(Derecha)+abs(izquierda))
  lista=list(Total=VT,Entre=VB)

  return(lista)
}

#s<-volatilidad(dat1,dat2,enlace)
