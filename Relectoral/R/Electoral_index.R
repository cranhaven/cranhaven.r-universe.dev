#En este fichero desarrollamos todos los indices electrorales

#'@title Disproportionality index of Rae - 1971 ("R")
#'
#'@describeIn Disproportionality indicators
#'@section Indicator
#'
#'@description This index measures electoral disproportionality based on the result obtained in an election.
#'It has the disadvantage that it is highly influenced by the number of small parties that compete in
#'the elections. The formula used is as follows:
#'\deqn{R=\frac{\sum_{i=1}^{n}|E_{i}-V_{i}|}{n}}{R = (1/n)*sum(|Vi-Ei|)}
#'for \eqn{i = 1, 2, \ldots,n}
#'
#'To compute its value, an integer vector corresponding to the votes obtained by each party and another with
#'the corresponding seats must be provided as parameters of the formula. The R code will be in charge of
#'calculating the corresponding percentages to obtain the index.
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns the value obtained for the index
#'
#'@examples
#'Rae(c(1200, 30, 4000),c(10,6,8))
#'
#'@export
Rae <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  n <- length(votes)
  res <- sum(abs(p_votes - p_seats))/n
  return(res)
}

Rae(c(1200, 30, 4000),c(10,6,8))


#'@title Corrected Rae's disproportionality index ("Rco")
#'
#'@description It was Rae himself who realised the problems in calculating the RAE index by taking
#'all parties into account. For this reason he reviewed the basis of the index and decided to exclude
#'from the calculation all parties that did not reach 0.5 percent of the votes. The function created
#'to calculate this index takes this corrective value by default, although it is allowed to enter another value,
#'as it can be seen in the example below.
#'
#'To compute its value, the integer vectors corresponding to the votes obtained by each party and
#'their seats must be provided as parameters of the formula. The R code will be in charge of
#'calculating the corresponding percentages in order to obtain the index.
#'
#'@param votes It is a vector of integers, containing ALL the votes of all the candidates.
#'@param seats It is a vector of integers, containing ALL the seats obtained by each of the candidates.
#'@param correc This is a decimal value that indicates the minimum percentage of votes needed in order to
#'be taken into account for the formula. By default its value is 0.5.
#'
#'@return Returns the value obtained for the index
#'
#'@importFrom stats line lm weighted.mean
#'
#'@examples
#'Rae_corregido(c(1200, 30, 4000),c(10,6,8), correc = 1)
#'Rae_corregido(c(1200, 30, 4000),c(10,6,8))
#'
#'@export
Rae_corregido <- function(votes, seats, correc = 0.5){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")

  lim <- sum(votes)*correc*10^-2
  seats <- seats[votes>lim]
  votes <- votes[votes>lim]

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  n <- length(votes)
  res <- sum(abs(p_votes - p_seats))/n
  return(res)
}

Rae_corregido(c(1200, 30, 4000),c(10,6,8), correc = 1)
Rae_corregido(c(1200, 30, 4000),c(10,6,8))

#' @title Disproportionality index of Loosemore and Hanby - 1971 ("LH")
#'
#' @description Loosemore and Hanby disproportionality index ( 1971 ) aims to address the difficulties
#' encountered with the Rae index. To calculate this disproportionality index, the absolute values of
#' the differences between votes and seats are added together and the result is divided by two. The
#' specific formula used is as follows:
#' '\deqn{LH=\frac{\sum_{i=1}^{n}|E_{i}-V_{i}|}{2}}{LH = (1/2)*sum(|Vi-Ei|)}
#'for \eqn{i = 1, 2, \ldots,n}
#'
#' @param votes It is a vector of integers, containing the votes of all the candidates.
#' @param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#' @return Returns the value obtained for the index
#'
#' @examples
#' Loos_Hanby(c(1200, 30, 4000),c(10,6,8))
#'
#' @export
Loos_Hanby <-  function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  res <- sum(abs(p_votes - p_seats))/2
  return(res)
}

Loos_Hanby(c(1200, 30, 4000),c(10,6,8))

#' @title Gallagher's disproportionality index of minimum squares - 1991 (Gcm)
#'
#' @description The Gallagher's electoral disproportionality index of minimum squares uses
#' a somewhat more elaborate formula than the previous indices (R, LH), for which it calculates
#' the differences squared between the votes and seats, adds them up and divides the result by
#' two and then calculates its square root. It therefore gives appropriate weight to the
#' distortions of disproportionality. Its mathematical formula is as follows:
#'\deqn{Gcm=\sqrt{\frac{\sum(V_{i}-E_{i})^{2}}{2}}}{Gcm =sqrt(sum(Vi-Ei)^2*(1/2)) }
#'for \eqn{i =  1, 2, \ldots,n}
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns the value obtained for the index
#'
#' @examples
#'
#' Rae(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#' Gallagher(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'
#'@export
Gallagher <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  res <- sqrt(0.5*sum((p_votes-p_seats)^2))
  return(res)
}

Rae(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
Gallagher(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

#'@title Sainte-Lague index of disproportionality (SL)
#'
#'@description This index is designed to study the disproportionality of those electoral systems that use
#'the procedure of distribution of seats that has the same name (Sainte-Lague method). It should be noted
#'that this indicator takes into account the relative difference between the seats-votes of each party,
#'as well as that of the system as a whole. The formula used for its calculation is as follows:
#'\deqn{SL=\sqrt{\sum\frac{(E_{i}-V_{i})^{2}}{V_{i}}}}{SL =sqrt(sum((Vi-Ei)^2*(1/Vi)) }
#'for \eqn{i =  1, 2, \ldots,n vi>0}
#'
#'
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns the value obtained for the index
#'
#'@examples
#'
#'Sainte_Lague(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
Sainte_Lague <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")

  seats <- seats[votes>0]
  votes <- votes[votes>0]

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  res <- sum(((p_seats - p_votes)^2)/p_votes)
  return(res)
}

Sainte_Lague(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

#'@title Index of disproportionality of maximum deviation (Lmax)
#'
#'@description An indicator of disproportionality that is appropriate for those systems
#'that use the D'Hondt Law for seats distribution, as is the case in Spain
#'(except in the Senate elections), presents the disadvantage that it only takes into
#'account the difference between seats and votes for the most voted political force.
#'The formula used for its calculation is as follows:
#'\deqn{Lmax=max[|V_i-E_i]}{Lmax=max[|Vi-Ei|]}
#'for \eqn{i =  1, 2, \ldots,n }
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns the value obtained for the index
#'
#'@examples
#'
#'L_max(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export

L_max <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  res <- max(abs(p_seats - p_votes))
  return(res)
}

L_max(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

#'@title   Cox_Shugart disproportionality index - 1991 ( CS )
#'
#'@description Cox_Shugart's (1991) disproportionality index measures the disproportion in the
#'distribution of seats by means of a regression line between the percentage of seats and the
#'percentage of votes. If the distribution were approximately proportional, the slope of the
#'regression line would be one. If it moves away from that value there will be disproportionality
#'in the distribution of seats.
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns the value obtained for the index
#'
#'@examples
#'
#'Cox_Shugart(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))
#'
#'@export
Cox_Shugart <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  model <- lm(p_seats ~ p_votes)
  res <- model$coefficients[2]
  #browser()
  return(as.numeric(res))
}

Cox_Shugart(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))

#'@title   Cox_Shugart disproportionality index corrected ( CS_correg )
#'
#'@description This index tries to rectify the defect in the Cox_Shugart index, since the latter is
#'very sensitive to the presence of small political parties. To correct this deficiency,
#'this indicator is calculated by not taking into account parties that do not obtain
#'parliamentary representation. In fact, when its value is calculated the only thing
#'that is done is to eliminate the votes for parties that do not have obtained any seats and
#'those results are passed to the "Cox_Shugart()" function.
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns the value obtained for the index
#'
#'@examples
#'
#'Cox_Shugart_correg(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))
#'
#'@export
Cox_Shugart_correg<- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")
  # Nos quedamos con los partidos con representación
  rep<- seats>0
  vot<- votes[rep]
  sea<- seats[rep]
  return(Cox_Shugart(vot,sea))
}

Cox_Shugart_correg(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))

#'@title Disproportionality index. Tuckey's line (LT)
#'
#'@description This indicator is also known as the robust bias index. Its value is obtained
#'by using the slope of the well-known "Tuckey Line" and its use is recommended when there is
#'a "small" group of parties that behave significantly differently from the rest. This
#'procedure is qualified as "robust" because the presence of behaviors of certain parties
#'different from the rest does not substantially alter the line obtained, unlike what happened
#'when it was used in the least squares criteria to make the regression adjustment.#'
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns the value obtained for the index
#'
#'@examples
#'
#'L_Tukey(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))
#'
#'@export
L_Tukey <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  model <- line(p_seats ~ p_votes)
  res <- model$coefficients[2]
  return(res)
}

L_Tukey(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))

#### Party System Voting Dimension ###

#'@title Rae's fragmentation (electoral and parliamentary) vote dimension (F)
#'
#'@description This indicator seeks to summarize in one value the level of dispersion or
#'concentration of political power, that is to say, whether or not
#'the seats or votes received are concentrated in a reduced number of political parties. The values
#'of the index vary between 0 and 1, so that a value of zero indicates no fragmentation
#'(all the votes go to a single party), while a value close to one indicates strong
#'electoral fragmentation
#'
#'Este indicador pretende resumir en un valor el nivel de dispersión o concentración del
#'poder político, es decir y de forma resumida si se encuentra concentrado o no los escaños o votos
#'recibidos en una serie de partidos polítcos. Los valores del índice varían entre 0 y 1, de forma que
#'un valor cero indica no hay ninguna fragmentación ( todos los votos van a un sólo partido), mientras
#'que un valor dercano a uno indica fuerte fragmentación electoral
#'
#'The formula used for its calculation is as follows:
#'
#'\deqn{1-\sum_{i}q_{i}^{2}}{1-sum(qi^2)]}
#'for \eqn{i =  1, 2, \ldots,n }
#'
#'Where q_i are the proportions (as parts per unit) of the number of seats or votes (electoral
#'or parliamentary indicator), of the votes obtained by each candidate.
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns a dataframe, with the columns "electoral" and "parliamentary" to store respectively
#'the value of the electoral or parliamentary fragmentation index.
#'
#'@examples
#'
#'fragmentacion_rae(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export

fragmentacion_rae <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")

  p_votes2 <- (votes/sum(votes))^2
  p_seats2 <- (seats/sum(seats))^2

  res_electoral <- 1-sum(p_votes2)
  res_parlamento <- 1-sum(p_seats2)

  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

fragmentacion_rae(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))


### Effective number of political parties ####
#'@title Voting dimension index. Effective number of political parties (electoral and parliamentary) (N)
#'
#'@description This indicator (Laakso and Taagepera 1979) is complementary to the electoral
#'fragmentation indicator, and its objective is to measure the number of parties that actually
#'compete in an electoral process. The mathematical formula used to calculate this indicator is as follows:
#'
#'\deqn{N=\frac{1}{\sum p_{i}^{2}}}{N=1/sum(pi^2)}
#'for \eqn{i = 1, 2, \ldots,n } and p_i the percentages (as parts per unit) of votes or seats of the party i
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns a dataframe, with the columns "electoral" and "parliamentary" to store the value of
#'the index of the effective number of parties in the electoral or parliamentary way respectively
#'
#'@examples
#'
#'nep(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
nep <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")
  p_votes2 <- (votes/sum(votes))^2
  p_seats2 <- (seats/sum(seats))^2

  res_electoral <- 1/sum(p_votes2)
  res_parlamento <- 1/sum(p_seats2)

  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

nep(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

####  Indice hiperfraccionamiento. ###
# Esta fubción es auxiliar
R_hiperfragmentacion2 <- function(datos){
  # d es un vetor conteniendo los datos
  # Lo pasamos a proporciones
  x_por <- datos/sum(datos)
  x_por1 <- x_por[x_por>0]
  x_por2 <- log(x_por1)
  return(exp(-sum(x_por1*x_por2)))
}

#'@title Voting Dimension Index. Hyperfractionality index (electoral and parliamentary) (I)
#'
#'@description The hyperfractionality index, proposed by Kesselman (1996) and Wilden (1991),
#'is another indicator used to measure the number of parties that are relevant in any type
#'of election. This index is very sensitive to the presence of small
#'parties since it gives them more relevance than they really have. The mathematical formula
#'used to calculate the value of this index is as follows:
#'
#'\deqn{I=exp(-\sum p_{i}ln(p_{i}))}{I=exp(-sum(pi*ln(pi)))}
#'for \eqn{i = 1, 2, \ldots,n } and p_i>0 the percentages (as parts per unit) of votes or seats of the party i
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns a dataframe, with the columns "electoral" and "parliamentary" to store the value of
#'the hyperfractionality index in the electoral or parliamentary way respectively
#'
#'@examples
#'
#'hiper(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
hiper <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")
  res_electoral <- R_hiperfragmentacion2(votes)
  res_parlamento <- R_hiperfragmentacion2(seats)
  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

hiper(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

#### Molinar's effective number of political parties ###
# No documento estas funciones porque son auxiliares y no las quiero hacer visibles
R_nepMolinar2 <- function(datos){
  # x es un vetor conteniendo los datos
  # Lo pasamos a proporciones
  x_por <- datos/sum(datos)
  N <- 1/sum(x_por^2) # Número efecivo de partidos
  return(1+(N^2)*sum((x_por[2:length(datos)])^2))
}

#'@title Voting Dimension Index. Molinar's index of number of parties (electoral and parliamentary) (NP)
#'
#'@description This indicator proposed by Molinar (1991), attempts to evaluate the relevant number
#'of parties that actually exist in an electoral process. It can be said that this index shows a
#'better performance than the indexes of the number of parties and hyperfractionality, both in the
#'weighting it gives to the winning party and in the difference between the first and second parties,
#'as well as in the degree of concentration of the minority parties. The formula used for the
#'calculation of this index is the following:
#'
#'\deqn{NP=1+N^{2}\sum_{i=2}p_{i}^{2}}{NP=1+N^2*sum(pi^2, for i>=2)}
#'for \eqn{i =  2, \ldots,n } and p_i the percentages (as parts per unit) of votes or seats of the party i
#'
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns a dataframe, with the columns "electoral" and "parliamentary" to store the value of
#'the index of the Number of parties of Molinar in the electoral or the parliamentary way respectively.
#'
#'@examples
#'
#'nepMolinar(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
nepMolinar <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")
  res_electoral <- R_nepMolinar2(votes)
  res_parlamento <- R_nepMolinar2(seats)
  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

#nepMolinar(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

##### Concentration index #####
# No lo documento pues no quiero sea transparente a usuario final
R_concentracion <- function(datos){
  # x es un vetor conteniendo los datos
  # Lo pasamos a proporciones
  x_por <- datos/sum(datos)
  # odenamos en orden descendente
  x_por_ord <- x_por[order(-x_por)]
  return(x_por_ord[1]+x_por_ord[2])
}

#'@title Voting Dimension Index. Concentration of votes (electoral and parliamentary)
#'
#'@description Another very important indicator of the party system is the concentration
#'of votes, which indicates what percentage of votes or seats are taken by the two
#'parties that have the largest number of votes. The formula used to calculate this
#'indicator is as follows:
#'
#'\deqn{concentration=p_1+p_2}{concentration= p1+p2}
#'
#'The maximum value that this indicator takes is 1, so the closer it is to 1, the more concentrated the vote is.
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns a dataframe, with the columns "electoral" and "parliamentary" to store the value of
#'the index of the concentration of votes in the electoral or the parliamentary way respectively.
#'
#'@examples
#'
#'concentracion(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
concentracion <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")
  res_electoral <- R_concentracion(votes)
  res_parlamento <- R_concentracion(seats)
  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

concentracion(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

### Competitiveness ###
# Función auxiliar no la documento
R_competitividad <- function(datos){
  # x es un vetor conteniendo los datos
  # Lo pasamos a procentajes
  x_por <- datos/sum(datos)
  # odenamos en orden descendente
  x_por_ord <- x_por[order(-x_por)]
  return(1-(x_por_ord[1]-x_por_ord[2]))
}

#'@title Voting Dimension Index. Competitiveness (electoral and parliamentary).
#'
#'@description This index will measure the level of electoral competition that exists in a given
#'party system between the first and second party that have obtained more votes in an specific election.
#'This indicator will therefore show how much rivalry there is between the two parties that
#'have received the most votes. To measure this phenomenon, the proximity or remoteness of the results
#'of these two political formations are used. This indicator is calculated using the following formula:
#'
#'\deqn{competitiveness=1-(p_1-p_2)}{competitiveness=1-(p1-p2)}
#'
#'Where p1 and p2 are the percentages of votes (or seats), as parts per unit, of the two most voted political
#'formations. A value close to 1 (p1=p2) indicates strong competitiveness, while a value of zero
#'(p1=1 p2=0) will indicate there is no competitiveness.
#'
#'@param votes It is a vector of integers, containing the votes of all the candidates.
#'@param seats It is a vector of integers, containing the seats obtained by each of the candidates.
#'
#'@return Returns a dataframe, with the columns "electoral" and "parliamentary" to store the value of
#'the index of the competitiveness index in the electoral or the parliamentary way respectively.
#'
#'@examples
#'
#'competitividad(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
competitividad <-function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: The vectors for the votes and seats must be of the same length")
  if(sum(votes<0) != 0) stop("ERROR: There cannot be a negative number of votes")
  if(sum(seats<0) != 0) stop("ERROR: There cannot be a negative number of seats")
  res_electoral <- R_competitividad(votes)
  res_parlamento <- R_competitividad(seats)
  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

competitividad(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))





### Party Nationalization Index (INP) ####
#'@title Party Nationalization Index (INP)
#'
#'@description Jones and Mainwaring (2003), proposed the calculation of this index
#'based on the Gini income concentration index. This index will take values between
#'0 and 1. A value close to 1 indicates that the party is highly nationalized, and a
#'value close to zero will indicate the opposite.
#'
#'This index measures the degree of homogeneity of the votes a party receives in all
#'the electoral districts where it competes. In a very brief way, the calculation
#'formula for this indicator is as follows:
#'
#'\deqn{IPN=1- Gini coefficient}{IPN=1- Gini coefficient}
#'
#'@param dat Vector containing the proportions (as parts per unit) of the party's vote in each district
#'
#'@return A real number containing the value of the indicator.
#'
#'@examples
#'
#'INP(c(0.15,0.22,0.24,0.26,0.27,0.27,0.27,0.32,0.33,0.34))
#'
#'INP(c(0.3,0.1,0.2))
#'
#'@export
INP<-function(dat){
  #dat son los tantos por uno del voto del partido en cada circunscripción
  # porcentajes en cada circunscripción ordenado
  porcen1<-dat[order(dat)]

  # Obtenemos los datos que se pasan en el vector
  Ndatos<-length(dat)
  # representación sobre resto de unidades
  c<-rep(1/Ndatos,Ndatos)
  # Calculo los porcentajes de porcen1
  porcen2<-porcen1/sum(porcen1)

  # Suma acumulada del voto
  Pc <- cumsum(porcen2)
  # reprresentación sobre el resto acumulado
  Qc <- cumsum(c)
  # El minuendo
  s1<-sum(Pc[2:(Ndatos)]*Qc[1:(Ndatos-1)])


  # El sustraendo
  r1<-sum(Pc[1:(Ndatos-1)]*Qc[2:Ndatos])
  #browser()
  return(1-s1+r1)
}


#### Computation of the Nationalisation Index of the Political Party System ###

#'@title Nationalisation Index of the Political Party System (INSP)
#'
#'@description This indicator is based on the nationalization index of each party. Its
#'calculation is computed using the aggregation of party nationalisation scores weighted
#'with the value of electoral weight, that is, the number of votes obtained. As with the Party
#'Nationalisation Index (INP), values close to 1 will indicate strong nationalisation and
#'values close to zero will indicate the opposite. The formula used for this
#'indicator is as follows:
#'
#'\deqn{INSP=sum(INPi x pi)}{INSP=sum(INPi x pi)}
#'
#'Where INPi is the nationalization index of party i, and pi is the number of votes obtained by party i.
#'
#'@param datos It is a data.frame with the first column containing the name of the electoral district,
#'the rest of the columns correspond to the parties presented and each column will contain the
#'number of votes that each party has obtained in the corresponding electoral district.
#'
#'@return A real number containing the value of the indicator.
#'
#'@examples
#'
#'a <- data.frame(prov=c("Alava","Albacete","Valladolid"),
#'  PP=c(20,30,10),PSOE=c(34,12,45),   stringsAsFactors = FALSE)
#'
#'INSP(a)
#'
#'
#'b<-data.frame(prov=c("1","2"),A=c(400,190),B=c(200,1000),stringsAsFactors = FALSE)
#'INSP(b)
#'
#'@export
INSP <- function(datos){
  # Las datos deben ser un dataframe, primera columna con los nombres de las provincias
  # Una columna para cada partido
  # calculamos los porcentajes por filas
  # primero la suma por filas, quito la primera columna para sumar
  sum_filas <- apply(datos[,-1],1,sum)
  #datos_porcen va a contener los porcentajes por filas
  datos_porcen <-datos
  for ( i in 2:ncol(datos)){
    datos_porcen[,i] <- datos[,i]/sum_filas
  }

  # Calculamos el INP de cada partido
  INPi <- rep(NA,ncol(datos)-1)
  for (i in 1:length(INPi)){
    INPi[i] <-INP(datos_porcen[,i+1])
  }

  # Total votos por partido
  Tvot<-colSums(datos[,-1])
  # Proporcion votos cada artido nivel nacional
  Pj <- Tvot/sum(Tvot)

  return(sum(Pj*INPi))
}

### Nationalisation Index of Montero and Lago

#'@title Nationalisation Index of Montero and Lago (2010)
#'
#'@description This nationalisation index, whose value ranges between 0 and 1,
#'was proposed by Lago and Montero (2010) and is based on the decision of the
#'parties to take part in the electoral process in all or only some districts. It
#'is calculated by taking into account the electoral results of the parties, as
#'well as the number of seats in the districts where they are elegible. The
#'formula that is used is as follows:
#'
#'\deqn{E=\sum_{j=1}^{J}p_{j}^{e}*q_{j}}{E=sum(p_j*q_j;j=1,2,..,J)}
#'
#'Where:
#'p_j is the number of votes obtained in all the territories over the total expressed as parts per unit.
#'q_j is the number of seats (out of the total number of seats) in the districts where political
#'formation j is presented, expressed as parts per unit.
#'
#'Two procedures have been enabled for the calculation of this index, one will be called 'automatic' and
#'the other one 'manual'. With the automatic procedure it will use the data of the Spanish
#'Ministry of Home Office and, without further intervention, the indicator for the elections to the
#'General Courts that has been indicated with the parameters of the function will be calculated.
#'For the manual procedure, the data to be processed must be passed as an input of the function, as
#'indicated below in the section concerning the parameters.
#'
#'@param Ano (Only for automatic case) It represents the year of the data you want to download.
#'This data can be numeric or character (preferably numeric).
#'@param Mes (For automatic case only) This is the month in which the election was made. It must
#'be of the character type and two digits, i.e. "04" is valid but "4" is not.
#'@param Ruta (Only for automatic case) A path of the local hard disk where the file is downloaded
#'from the Ministry of Home Office must be indicated. Once the download is finished
#'the downloaded file is deleted automatically.
#'@param n_escanos (Only for automatic case) This is the total number of seats to be covered. By
#'default, it has a value of 350 which are the deputies who are elected to the Congress of Deputies in Spain.
#'@param Auto It contains a logical value that by default is TRUE, indicating that the automatic procedure
#'is done. If you pass to the function the FALSE value then the procedure would be manual.
#'@param d_votos (Sólo para procedimiento manual). Es un data.frame, conteniendo en la primera
#'columna la denominación de las regiones o circunscripciones. El resto de las columnas se
#'debe haber una por cada partido político que contenga los votos que ese partido ha tenido
#'en cada una de las circunscripciones, y la denominación de esa columna, serán las
#'siglas de ese partido.
#'@param d_escanos (For manual procedure only). It is a data.frame, containing in the first
#'column the name of the regions or districts. The rest of the columns must be one for each
#'political party containing the votes that each party has obtained in each of the districts, and
#'the name of that column will be the acronym of that party.
#'
#'@return The returned value is a list object, with three positions. The first position
#'contains the value of the index (it is called 'V_indice'), the second one the vector containing
#'the votes with respect to the national total expressed as part per unit (it is called 'Porcentaje_votos'),
#'and the third component are the disputed seats (expressed as part per unit) of the districts where the
#'political party under study is presented. Its name is 'Porcentaje_escanos'.
#'
#'@examples
#'#Procedimiento automático
#'s<-IN_LAGO_MONTERO(2019,"04","F:/",n_escanos = 350)
#'if(!(is.null(s) || is.na(s))){
#' print(s$V_indice)
#' print(s$Porcentaje_votos)
#' print(s$Porcentaje_escanos)
#'}
#'
#'#Procedimiento manual
#'da1 <- data.frame( # Contains the votes
#'  Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
#'  PSOE=c(400,300,0,50,25),
#'  PP=c(300,200,10,150,2),
#'  Cs=c(400,0,3,300,45),
#'  Uno=c(465,23,341,263,0))
#'
#'da2 <- data.frame( # Contains the total seats of each province
#'  Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
#'  escanos=c(2,3,6,5,4))
#'
#'s2<-IN_LAGO_MONTERO(Auto = FALSE,d_votos = da1,d_escanos = da2)
#'@export
IN_LAGO_MONTERO<-function(Ano,Mes,Ruta,n_escanos=350,Auto=TRUE,d_votos,d_escanos){
  if(Auto == TRUE){
    out <-tryCatch(
      {
        if( class(Mes) != "character") stop( "The parameter 'Mes' must be a character")
        if(nchar(Mes) != 2) stop("The parameter 'Mes' must be composed by two characters")
        if(is.null(Ruta)) stop("The 'Ruta' paramenter cannot be empty'")
        # Nationalisation Index of Montero and Lago

        datos_MIR<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",Ruta,Borrar=TRUE))
        datos_MIR<-datos_MIR[1:52,]
        #Me quedo con la columna de los votos
        # Mediante expresión regular selecciono las columnas que tienen el número de votos
        columnas_Votos<-grep("V_",colnames(datos_MIR))
        # Me quedo solo con las columna de votos
        datos_MIR_Votos<-as.data.frame(datos_MIR[,columnas_Votos])
        # Transformo los datos a numéricos
        datos_MIR_Votos <- as.data.frame(apply(datos_MIR_Votos,2,as.integer))
        #calculo los totales
        totales<-colSums(datos_MIR_Votos)
        #Tantos por uno
        totales_por<-totales/sum(totales)
        #Calculo para cada provincia los escaños que tiene

        # Mediante expresión regular selecciono las columnas que tienen el número de escanos
        columnas_escanos<-grep("D_",colnames(datos_MIR))
        #extraigo las columnas con datos de escaños
        datos_MIR_escanos <- as.data.frame(datos_MIR[,columnas_escanos])
        #transformo los datos a numericos
        datos_MIR_escanos <- as.data.frame(apply(datos_MIR_escanos,2,as.integer))
        #Ahora saco los escaños que tiene cada provncia
        escanos_prov<-rowSums(datos_MIR_escanos)

        #Detectamos en que provincia se han presentado los partidos mediante
        # el número de votos, si es cero no se presentan y en caso contrario si
        # Se hace mediante na matriz de valores lógicos

        presentado<-datos_MIR_Votos>0
        # Calculo lo escaños de la provincias donde se presentan
        n<-ncol(datos_MIR_escanos)
        esca_presentado<-rep(NA,n)
        for(i in 1:n){
          esca_presentado[i]<-sum(presentado[,i]*escanos_prov)

        }
        esca_presentado<-esca_presentado/n_escanos
        re<-list()
        re[[1]]<-sum(esca_presentado*totales_por)
        re[[2]]<-as.numeric(totales_por)
        re[[3]]<-as.numeric(esca_presentado)
        names(re)<-c("V_indice","Porcentaje_votos","Porcentaje_escanos")
        #re$Porcentaje_votos
        #re$Porcentaje_escanos
        return(re)
      },
      warning = function(cond){
        message("A warning has been generated, the path may not exist or cannot be written to")
        message("The warning message is the following:")
        message(cond)
        return(NULL)
      },
      error= function(cond){
        message("The path provided may not exist or cannot be written to")
        message("The error message is the following:")
        message(cond)
        return(NA)
      }
    )
    return(out)
  }#Fin del TRUE
  else{
        if(missing(d_votos)) stop("A data.frame must be provided with the voting data using the parameter 'd_votos'")
        if(missing(d_escanos)) stop("A data.frame must be provided with the seat data using the parameter 'd_escanos'")
        if(class(d_votos) != "data.frame") stop("The parameter 'd_votos' must be a data.frame")
        if(class(d_escanos) != "data.frame") stop("The parameter 'd_escanos' must be a data.frame")

        # compruebo que el numero de filas es el mismo
        if(nrow(d_votos) != nrow(d_escanos)) stop("The number of rows in the two data.frames must match")
        #Comparo que los nombres de las regiones son los mismo
        b <- as.character(d_votos[,1]) == as.character(d_escanos[,1])
        if(sum(b) != nrow(d_votos)) stop("The names of the regions expressed in the first column must be the
                                          same in both data.frames")
        #Calculo los totales de los votos
        totales <- colSums(d_votos[,-c(1)])
        #Tantos por uno a nivel nacional
        totales_por <- totales/sum(totales)

        #Detectamos en que provincia se han presentado los partidos mediante
        # el número de votos, si es cero no se presentan y en caso contrario si
        # Se hace mediante na matriz de valores lógicos

        presentado<-d_votos[,-c(1)]>0
        # Entresaco los escaños de cada provincia
        # Me quedo solo con la columna de los escaños
        escanos_prov <- d_escanos[,2]
        n <- ncol(d_votos[,-c(1)])
        esca_presentado <- rep(NA,n)
        for(i in 1:n){
          esca_presentado[i] <- sum(presentado[,i]*escanos_prov)
        }

        #Calculo el total de escaños
        n_escanos <- sum(escanos_prov)
        esca_presentado_por <- esca_presentado/n_escanos
        # Los datos los saco en la siguiente lista
        re <-list()
        re[[1]] <- sum(esca_presentado_por*totales_por)
        re[[2]] <- as.numeric(totales_por)
        re[[3]] <- as.numeric(esca_presentado_por)
        names(re)<-c("V_indice","Porcentaje_votos","Porcentaje_escanos")
        return(re)
      }#Fin del else
}


### Mathematical injustice (IM) ####

#'@title utilities.Mathematical Injustice ( IM )
#'
#'@description This indicator was proposed by Edward V. Huntington and measures the degree
#'of "injustice" in an electoral system by translating the votes obtained by political parties
#'into seats. In this sense, Edward V.Huntington defined this mathematical injustice
#'between two parties competing in an electoral process as the difference in absolute value
#'between the ratios of seats and votes obtained by those two political parties. That is,
#'for each political party the ratio between seats and votes is obtained, and the
#'mathematical injustice will be the difference in absolute value of those ratios.

#'
#'If we express the above in a mathematical formula we get the following:
#'
#'\deqn{IM_{ij}=\left|\frac{e_{i}}{v_{i}}-\frac{e_{j}}{v_{j}}\right|}{IM_ij=abs((ei/vi)-(ej/vj))}
#'for \eqn{i =1,  2, \ldots,n } and vi the votes of the party i and ei its seats
#'
#'@param dates It is a data.frame object that contains the information as follows:
#'The first column is reserved for the name of the political parties. The second column
#'contains the votes obtained, and the third column is used for the seats obtained by
#'that political party.
#'
#'@return The result is a matrix object, of dimension n x n, with n equal to the
#'number of parties provided in the input data.frame. The names of the rows and columns
#'coincide with the names of the parties provided in the input data.frame in the first column.
#'
#'@examples
#'a<- data.frame(par=c('A','B','C','D'),
#'               vot=c(200,300,100,24),sea=c(3,4,1,0), stringsAsFactors = FALSE)
#'
#'InjusticiaM(a)
#'@export
InjusticiaM <-function(dates){
  #dates es un dataframe con la primera columna conteniendo los partidos
  #La segunda columna conteniendo los votos y la tercera conteniendo los escaños
  if(class(dates) != "data.frame") stop("ERROR: 'dates' must be a data.frame" )
  # Si la primera columna no es de tipo character genero un error

  if(class(dates[,1]) != "character") stop("ERROR: the first column of the data.frame 'dates' must be of character type")


  n <- nrow(dates)
  #Defino la matriz donde se almacenan los resultados
  resul <-matrix(NA,nrow = n,ncol = n)
  for(i  in 1:n){
    for(j in 1:n){
      resul[i,j]<-abs((dates[i,3]/dates[i,2])-(dates[j,3]/dates[j,2]))
    }
  }
  colnames(resul) <- dates[,1]
  rownames(resul) <- dates[,1]
  return(resul)
}
