# Calculo de la polaridad
# se necesita un data.frame. Primera columna nombre partido;
# segunda:ubicación ideologica;
# tercera columna: nº votos o escaños
# Tipo=1 se utilizan desviaciones de la media al cuadrado
# Tipo=2 se utilizan desviaciones de la media en valor absoluto

#'@title Dimensionality of voting. Polarization index. ( PP )
#'
#'@description This dimension is one of the most important in democratic systems, as it explains part
#'of the stability and bankruptcy problems of democracies. The index that is calculated with this
#'function needs the data of the ideological location of the parties, which are difficult to obtain, and
#'that is something as simple as a scale of values between 1 and 10, in such a way that the closer to 1 the
#'valuation of a political party is, it will be more classified as left-wing ideology, and the opposite case will
#'occur when a political formation has a rating at the other end of the scale. The value obtained for this
#'ideological position will be placed in the second column of the data.frame called  "data", which is
#'the first parameter that is passed to the function and whose structure is explained later, in the
#'parameters section. If the indicator takes a value close to zero, it means there will be
#'no polarization, and the polarization will increase as the indicator takes higher values.
#'
#'In this section the weighted polarization index is calculated. For its calculation the following
#'formulas will be used:
#'
#'\deqn{Pp=\sum_{i=1}^{n}p_{i}(x_{i}-\overline{x}_{p})^{2}}{Pp = sum(p_j*(x_j-mean(x_p))^2)}
#'for \eqn{i = 1, 2, \ldots,n}
#'
#'\deqn{Pp2=\sum_{i=1}^{n}p_{i}\left|x_{i}-\overline{x}_{p}\right|}{Pp2 = sum(p_j*|x_j-mean(x_p)|)}
#'for \eqn{i = 1, 2, \ldots,n}
#'
#'@param datos It is a data.frame with three columns: the first one contains the name of the political parties.
#'The second contains the value of the ideological thinking location (scale from 1 to 10), and the third the
#'number of votes obtained (electoral polarization), or the number of seats obtained (parliamentary polarization).
#'
#'@param Tipo It can take either value 1 or 2. 1 To indicate that the weighted formula is obtained but by
#'calculating the distance to the weighted mean squared. If this parameter takes the value of 2, the
#'formula that gets the absolute value of the differences to the weighted average will be used instead.
#'
#'@return Returns a numeric value of this indicator.
#'
#'@importFrom stats line lm weighted.mean
#'
#'@examples
#'d <- data.frame(partidos=c("RN","PDC","PS","PPD","UDI","PRSC","otros"),
#'  ubicacion=c(6.36,5.31,2.73,4.13,7.04,4.00,5.33),
#'  c(19,20,15,21,33,7,5))
#'polarizacion(d,Tipo = 2)
#'
#'@export

polarizacion <- function(datos,Tipo=1){
  if(class(datos) != "data.frame") stop("A data.frame has to be provided")
  if(ncol(datos) != 3) stop("The input data.frame must have three columns")
  if(Tipo !=1 & Tipo != 2 ) stop("The parameter 'Tipo' has to take the values of 1 or 2")

  #Asignamos nombre a las columnas
  colnames(datos)<-c("partidos","ubicacion","vot_escanos")
  # calculo la ubicacion ideológica media ponderada
  media_pon <- weighted.mean(datos$ubicacion,datos$vot_escanos)
  # Si Tipo= 1
  if(Tipo == 1){
    dif_cuadrado <- (datos$ubicacion-media_pon)^2
    res <- weighted.mean(dif_cuadrado,datos$vot_escanos)
  } else if(Tipo == 2){
    dif_abs <- abs(datos$ubicacion-media_pon)
    res <- weighted.mean(dif_abs,datos$vot_escanos)
  }

  return(res)
}

#d <- data.frame(partidos=c("RN","PDC","PS","PPD","UDI","PRSC","otros"),
#                ubicacion=c(6.36,5.31,2.73,4.13,7.04,4.00,5.33),
#                c(19,20,15,21,33,7,5))
#polarizacion(d,Tipo = 2)

# Polarizacion de Dalton
# se necesita un data.frame. Primera columna nombre partido;
# segunda:ubicación ideologica;
# tercera columna: porcentaje de votos


#'@title Dimensionality of voting. Adapted version of the polarisation index from Dalton (2008) (Pd)
#'
#'@description With this function the slightly adapted version of the polarisation index from Dalton (2008)
#'is computed to measure polarization. This index weights the ideological positions of the parties by their
#'election results. This indicator ranges from 0 (in the hypothetical case that all parties occupy the same
#'ideological position) to 10 if the parties are at the extremes of the scale position. The formula to
#'compute it is as follows:
#'
#'\deqn{Pd=\sqrt{\sum_{i=1}^{n}p_{i}\left[\frac{\overline{x}_{i}-\overline{x}_{p}}{4.5}\right]^{2}}}
#'{Pd = sqrt(sum(p_j*(x_j-mean(x_p))/4.5)^2)}
#'for \eqn{i = 1, 2, \ldots,n}
#'

#'
#'@param datos It is a data.frame with three columns: the first one contains the name of the political
#'parties. The second must have the value of the ideological location (scale from 1 to 10), and the third
#'one the percentage of votes obtained with respect to all the parties presented, whether or not they are
#'taken into account to calculate the formula.
#'
#'
#'@return Returns the numerical value of this indicator
#'
#'@examples
#'d2<- data.frame(partidos=c("RN","PDC","PS","PPD","UDI","PRSC"),
#'  ubicacion=c(6.36,5.31,2.73,4.13,7.04,4.00),
#'  c(14.12,20.76,10.05,15.42,22.36,3.54))
#'
#'polarizacion_Dalton(d2)
#'
#'
#'@export

polarizacion_Dalton <- function(datos){
  if(class(datos) != "data.frame") stop("A data.frame must be provided")
  if(ncol(datos) != 3) stop("The input data.frame must have three columns")
  if(sum(datos[,3])>100) stop("The sum of the third column values may not exceed 100")
  if(sum(datos[,3])<=1) stop("The sum of the third column values cannot be less than 1")
  #Asignamos nombre a las columnas
  colnames(datos)<-c("partidos","ubicacion","votos")
  # calculo la ubicacion ideológica media ponderada
  media_pon <- weighted.mean(datos$ubicacion,datos$votos)
  a <- datos$votos*((datos$ubicacion-media_pon)/4.5)^2
  res <- sqrt(sum(a))
  return(res)
}
