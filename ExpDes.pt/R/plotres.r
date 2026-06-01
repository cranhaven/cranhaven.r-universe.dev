#' Conjunto de graficos dos residuos do modelo
#'
#' \code{plotres} Graficos dos residuos da saida do modelo.
#' Conjunto de quatro graficos sao produzidos: (1) Histograma,
#' (2) Grafico da probabilidade normal dos residuos com bandas
#' de confianca pela estatistica de ordem, (3) Residuos
#' Padronizados versus Valores Ajustados e (4) box-plot
#' (Residuos Padronizados).
#' @param a Objeto contendo a saida da analise do experimento,
#' tendo esta sido feita utilizando o pacote ExpDes.pt.
#' @references STEEL, R. G. D.; TORRIE, J. H. \emph{Principles
#' and procedures in Statistics: a biometrical approach}.
#' McGraw-Hill, New York, NY. 1980.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#'  @author Denismar Alves Nogueira
#'  @note Esta pode ser utilizada para contrucao dos graficos
#'  dos residuos de qualquer modelo do ExpDes.pt.
#' @seealso \code{\link{graficos}}.
#' @examples
#' data(ex1)
#' attach(ex1)
#' a<-dic(trat, ig)
#' plotres(a)
#' @export

plotres<-function(a){
  resid<-a$residuos
  df.resid<-a$gl.residual
  fitted.val<-a$valores.ajustados
  var.res<-sum(resid^2)/df.resid
  respad<-resid/sqrt(var.res)
  par(mfrow=c(2,2))
  #Grafico1
  hist(respad, xlab="Residuos padronizados", ylab="Densidade", main="Histograma", freq=FALSE)
  x<-c()
  curve(dnorm(x,mean=0,sd=1),col=2,lty=1,lwd=1,add=TRUE)
  #Grafico2
  good<-!is.na(resid)
  ord<-order(resid[good])
  ord.x<-resid[good][ord]
  n<-length(ord.x)
  P<-ppoints(n)
  z<-qnorm(P)
  plot(z,ord.x, xlab="z", ylab="Residuos")
  coef<-coef(lm(ord.x~z)) #rlm
  b0<-coef[1]
  b1<-coef[2]
  abline(b0,b1,col="red",lwd=2)
  conf<-0.95
  zz<-qnorm(1-(1-conf)/2)
  SE<-(b1/dnorm(z))*sqrt(P*(1-P)/n)
  fit.value<-b0+b1*z
  upper<-fit.value+zz*SE
  lower<-fit.value-zz*SE
  lines(z,upper,lty=2,lwd=2,col="red")
  lines(z,lower,lty=2,lwd=2,col="red")
  title("Normal Q-Q (95%)")
  # Grafico3
  plot(fitted.val,respad,xlab="Valores ajustados",ylab="Residuos padronizados")
  abline(h=0,col="red",lty=3)
  title("Residuos padronizados vs Valores ajustados")
  # Grafico4
  boxplot(respad)
  title("Residuos padronizados")
  par(mfrow=c(1,1))
}
