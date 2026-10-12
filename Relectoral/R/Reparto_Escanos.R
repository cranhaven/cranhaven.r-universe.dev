# Functions for seat allocation

# Function for implementing the Largest remainder method
#'@title Largest remainder method
#'
#'@seealso \code{\link{reparto_div}} for the allocation using divisor method
#'@seealso [https://en.wikipedia.org/wiki/Largest_remainder_method](https://en.wikipedia.org/wiki/Largest_remainder_method) on Wikipedia.
#'
#'
#'
#'@description This function is used to translate votes into seats,
#'following the criterion called "Largest Remains". In summary, this method is
#'calculated as follows: the total number of votes is divided by the total number of seats
#'(cost of each seat). The votes of each party are then divided by the above
#'ratio. The integer part of the above results are taken, which will be the
#'initial seats for each party. The remaining seats to be distributed are
#'allocated to the parties with the largest remainders (see https://en.wikipedia.org/wiki/Largest_remainder_method ).
#'
#'The cost of each seat is what differentiates one method from another. Thus
#'if n is the total number of seats and m the sum of all votes, the denomination
#'of the methods is as follows, depending on the ratio that is taken:
#'
#'\enumerate{
#'\item \bold{Hare (Hare)}: q=m/n
#'\item \bold{Droop (Droop)}:q=1+(m/(1+n))
#'\item \bold{Imperiali (Imperiali)}: q=m/(n+2)
#'\item \bold{Imperiali modificado (Mod_Imperiali)}: q=m/(n+3)
#'\item \bold{Hangenbach Bischof (hangenbach-bischo)}: q=m/(n+1)
#'}
#'
#'In all cases the value of q is rounded to the nearest integer.
#'
#'
#'@param partidos A character vector containing the name of the parties.
#'@param votos An integer vector with the votes of each party.
#'@param escanos An integer number containing the total number of seats to be allocated.
#'@param metodo This is the method to be used. The allowed values are: "Hare", "Droop",
#'"Imperiali", "Mod_Imperiali", "hangenbach-bischo". The default value is "Hare".
#'
#'
#'@return A dataframe with the political parties and the votes assigned to each.
#'
#'@import readxl
#'
#'@examples
#'Restos_Mayores(c("A","B","C","D","E","F","G"),
#'  c(391000,311000,184000,73000,27000,12000,2000),
#'  21,metodo = "Imperiali")
#'
#'@seealso \code{\link{reparto_div}}
#'
#'
#'@export
#'@md
Restos_Mayores<-function(partidos,votos,escanos,metodo="Hare"){
  if(length(partidos)!=length(votos)) stop("The dimension of vectors 'partidos' and 'votes' must be the same")
  if(escanos<=0) stop("The number of seats must be higher or equal to 1")

  if(!(metodo %in% c("Hare","Droop","Imperiali","Mod_Imperiali",
                     "hangenbach-bischo")))
  {
    stop("Invalid method")
  }
  tvotos<-sum(votos) # total de votos
  # Calculo del valor de q
  if(metodo == "Hare"){
    cociente=round(tvotos/escanos,0)
  }else if(metodo =="Droop"){
    cociente=round(1+(tvotos/(1+escanos)),0)
  }else if(metodo == "Imperiali"){
    cociente=round(tvotos/(escanos+2),0)
  }else if(metodo == "Mod_Imperiali"){
    cociente=round(tvotos/(escanos+3),0)
  }else if(metodo == "hangenbach-bischo"){
    cociente=round(tvotos/(escanos+1),0)
  }

  # Tomo la parte entera
  ente<-floor(votos/cociente)
  # tomo la parte decimal
  frac<-(votos/cociente)-ente

  Entera<-data.frame(parti=partidos,esca=ente)
  Fracional<-data.frame(parti=partidos,fra=frac)
  # Calculo resto de escaños quedan a repartir
  #por la parte fraccionaria
  resto_escanos<-escanos-sum(Entera$esca)
  Fracional<-Fracional[order(-Fracional$fra),][1:resto_escanos,]
  # Asigno un escaño partidos con mayor resto
  Fracional$fra<-1
  Total<-merge(Entera,Fracional,all.x=TRUE)
  # Obtengo escaños finales
  Total$escaT<-apply(Total[,2:3],1,sum,na.rm=TRUE)
  return(Total[,c(1,4)])
}

#Restos_Mayores(c("A","B","C","D","E","F","G"),
#               c(391000,311000,184000,73000,27000,12000,2000),
#               21,metodo = "Imperiali")



#### Ley d'Hondt. Esta función no va al paquete
dHont<-function(candidaturas,votos,escanos){
  tmp<-data.frame(
    candi<-rep(candidaturas,each=escanos),
    scor<-as.vector(sapply(votos,function(x) x/1:escanos))
  )
  tmp<-tmp$candi[order(-tmp$scor)][1:escanos]
  return(table(tmp))
}

dHont(c("A","B","C"),c(25000,16000,9000),5)

# esto sí va al paquete
# Repartos por cocientes
# Esta función la utilizo de apoyo
# Sirve para hacer las divisiones, dependiendo del método usado
coci<-function(x,metodo,escanos){
  if(metodo == "dhondt"){
    a<- seq.int(1,escanos)
    #re<-x/seq.int(1,escanos)
  }
  else if(metodo == "saint_lague"){
    # Calculo secuencia divisores y me quedo con tantos
    #como indique la variable escanos
    a<- seq.int(1,4*escanos,by=2)[1:escanos]
    #re<-x/a
  }
  else if(metodo=="saint_lague_Mod"){
    a<-c(1.4,seq.int(3,4*escanos,by=2))[1:escanos]
    re<-x/a
  }else if(metodo=="Danish"){
    a<-seq.int(1,4*escanos,by=3)[1:escanos]
  }
  else if(metodo=="Imperiali"){
    a<-seq.int(2,escanos+1)[1:escanos]
  }
  else if(metodo=="Hill_Huntington"){
    a1<-1:escanos
    a2<-2:(escanos+1)
    a<-sqrt(a1*a2)
  }
  else if(metodo=="Dean"){
    a1<-1:escanos
    a2<-2:(escanos+1)
    a11<-(2*a1)*a2
    a22<-2*a1+1
    a<-a11/a22
  }

  # Calculo el valor de las divisiones
  re<-x/a
  return(re)
}

#Function with the divisor methods
#'@title Allocation using divisor methods
#'@seealso \code{\link{Restos_Mayores}} for allocation using the largest remainder method
#'@seealso [https://en.wikipedia.org/wiki/D%27Hondt_method](https://en.wikipedia.org/wiki/D%27Hondt_method) on Wikipedia
#'@seealso [https://bit.ly/3aEidM9](https://bit.ly/3aEidM9) on Wikipedia.
#'@seealso [https://bit.ly/2Q0a0u0](https://bit.ly/2Q0a0u0) on the internet.
#'
#'@description This option uses various methods whose main common feature is that the number of
#'votes obtained by the political candidatures are divided by a series of numbers. The name of the
#'method used depends on the numbers which compose that series. In this sense, the methods
#'admitted are the following:
#'
#'\enumerate{
#'\item \bold{dhont}. It is the so-called D'Hondt Law and is the procedure used in Spain
#'to transform votes into seats in the Congress of Deputies. The series of numbers used
#'as a quotient are the integer numbers: 1,2,...n , where n is the number of seats to be distributed.
#'
#'\item \bold{saint_lague}. This is the Sainte Laguë method, Webster's method or odd-numbered divisors.
#'The set of divisors is formed by the odd numbers, that is {1,3,..,2n+1}.
#'
#'\item \bold{saint_lague_Mod}.It is the modified Sainte Laguë method, a variant of the Sainte Lagüe
#'method, according to which the initial quotient is v/1.4 and once each party has a seat the
#'standard formula v/(2n+1) is used.
#'
#'\item \bold{Danish}. In this method the divisors go from three to three units, that is to say
#'it is formed by the numbers: 1,4,7,10,13... and the n-th divisor would be 3*n-2.
#'
#'\item \bold{Imperiali}. The divisors are the positive integers but starting from number 2
#'onwards, that is: 2,3,4,...
#'
#'\item \bold{Hill_Huntington}. In this case the divisors are formed by the following sequence:
#'sqrt(2),sqrt(6),sqrt(12),...., sqrt(n*(n+1))
#'
#'\item \bold{Dean}.The set of divisors consists of the following numbers:
#'4/3, 12/5, 24/7,40/0,....,(2*n)(n+1)/(2*n+1)
#'
#'}
#'In all cases the value of q is rounded up to the nearest integer.
#'
#'
#'@param candidaturas  A text vector containing the name of the parties.
#'@param votos An integer vector with the votes of each party.
#'@param escanos An integer number stating the total number of seats to be allocated.
#'@param metodo It is the method to be used. The allowd values are:
#'  c("dhondt","saint_lague","saint_lague_Mod",
#' "Danish","Imperiali","Hill_Huntington",
#' "Dean")
#'
#'
#'@return Returns a data.frame with the names of the political parties and the number of Members
#'allocated with the chosen method.
#'
#'@import readxl
#'
#'@examples
#'
#'reparto_div(c("A","B","C","D","E"),c(340000,280000,160000,60000,15000),7,metodo="dhondt")
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='saint_lague')
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='saint_lague_Mod')
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='Danish')
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='Imperiali')
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='Dean')
#'
#'
#'@export
#'@md
reparto_div<-function(candidaturas,votos,escanos,metodo){
  # Primero compruebo que el método existe
  if(!(metodo %in% c("dhondt","saint_lague","saint_lague_Mod",
                     "Danish","Imperiali","Hill_Huntington",
                     "Dean"))){
    stop("The requested method is not implemented in this function")
  }
  scor2<-as.vector(sapply(votos,coci,metodo,escanos))

  tmp<-data.frame(
    candi = rep(candidaturas,each=escanos),
    scor = scor2
  )
  tmp<-tmp$candi[order(-tmp$scor)][1:escanos]
  r <- as.data.frame(table(tmp))
  colnames(r) <- c("Candidatura", "Escanos")
  return(r)

}

