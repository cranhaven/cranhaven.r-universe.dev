#' Grafico de modelos de regressao
#'
#' \code{graphicos} Constroi graficos para os modelos de
#' regressao ajustados na ANOVA.
#' @param a Objeto contendo a saida da analise do experimento,
#' tendo esta sido feita utilizando o pacote ExpDes.pt.
#' @param grau Determina o modelo a ser utilizado na construcao
#' do grafico. Para modelos polinomiais, 1 (reta) e o
#' \emph{default}, 2 (parabola), 3 (cubica), "pot" (modelo
#' potencia) e "exp" (modelo exponencial).
#' @param mod Logico. Da a opcao de imprimir o modelo ajustado
#' e seu coeficiente de determinacao ($R^2$) no topo do
#' grafico. O \emph{default} e TRUE.
#' @param main Titulo do grafico. Vazio e o \emph{default}.
#' @param sub Subtitulo do grafico. Vazio e o \emph{default}.
#' @param xlab Nome do eixo X.
#' @param ylab Nome do eixo Y.
#' @param pch Tipo de caractere que se deseja marcar os
#' valores observados.
#' @param xlim Limites do eixo X.
#' @param ylim Limites do eixo Y.
#' @param bty Tipo de caixa em que o grafico estara inserido.
#' O \emph{default} e "o".
#' @references STEEL, R. G. D.; TORRIE, J. H. \emph{Principles
#' and procedures in Statistics: a biometrical approach}.
#' McGraw-Hill, New York, NY. 1980.
#' @author Eric B Ferreira,
#'  \email{eric.ferreira@@unifal-mg.edu.br}
#' @seealso \code{\link{reg.poly}}, \code{\link{plotres}}.
#' @examples
#' data(ex1)
#' attach(ex1)
#' a<-dic(trat, ig, quali=FALSE, nl=FALSE)
#' graficos(a, grau=1)
#' graficos(a, grau=2)
#' graficos(a, grau=3)
#' @importFrom "grDevices" "dev.new"
#' @importFrom "graphics" "abline" "boxplot" "curve" "hist"
#' "lines" "mtext" "par" "points" "title"
#' @export

graficos<-function(a, grau = 1, mod = TRUE, main = " ", sub = " ",
         xlab = "Niveis (X)", ylab = "Resposta (Y)", pch = 19,
         xlim = NULL, ylim = NULL, bty = "o"){

a<-a$reg
xob<-as.numeric(as.vector(a$'Quadro de medias'[,1]))
x<-seq(min(xob), max(xob), by=0.1)

if(grau==1 && is.numeric(a$'Coeficientes reta')) {
dev.new()
b0<-a$'Coeficientes reta'[1]; b1<-a$'Coeficientes reta'[2]
y<-b0 + b1*x
yob<-as.numeric(as.vector(a$'Quadro de medias'[,2]))
if(is.null(ylim)==TRUE) ylim=c(min(y,yob), max(y,yob))
plot(x,y,'l', main=main, sub=sub, bty=bty, xlab=xlab, ylab=ylab,
     xlim=xlim, ylim=ylim)
if(mod==TRUE) mtext(paste('y =',round(b0,3),'+',round(b1,3),
     'x  ', ' R^2 = ', round(a$'R2 reta'*100,2),'%'),side=3)
points(xob, yob, pch=pch)
             }

if(grau==2 && is.numeric(a$'Coeficientes parabola')) {
dev.new()
b0<-a$'Coeficientes parabola'[1]; b1<-a$'Coeficientes parabola'[2]
b2<-a$'Coeficientes parabola'[3]
y<-b0 + b1*x + b2*x^2
yob<-as.numeric(as.vector(a$'Quadro de medias'[,2]))
if(is.null(ylim)==TRUE) ylim=c(min(y,yob), max(y,yob))
plot(x,y,'l', main=main, sub=sub, bty=bty, xlab=xlab, ylab=ylab,
     xlim=xlim, ylim=ylim)
if(mod==TRUE) mtext(paste('y = ',round(b0,3),'+',round(b1,3),'x+',
     round(b2,3),'x^2  ',' R^2 = ', round(a$'R2 parabola'*100,2),'%'),
     side=3)
points(xob, yob, pch=pch)
             }

if(grau==3 && is.numeric(a$'Coeficientes cubica')) {
dev.new()
b0<-a$'Coeficientes cubica'[1]; b1<-a$'Coeficientes cubica'[2]
b2<-a$'Coeficientes cubica'[3]; b3<-a$'Coeficientes cubica'[4]
y<-b0 + b1*x + b2*x^2 + b3*x^3
yob<-as.numeric(as.vector(a$'Quadro de medias'[,2]))
if(is.null(ylim)==TRUE) ylim=c(min(y,yob), max(y,yob))
plot(x,y,'l', main=main, sub=sub, bty=bty, xlab=xlab, ylab=ylab,
     xlim=xlim, ylim=ylim)
if(mod==TRUE) mtext(paste('y = ',round(b0,3),'+',round(b1,3),'x+',
     round(b2,3),'x^2+',round(b3,3),'x^3  ',' R^2 = ',
     round(a$'R2 cubica'*100,2),'%'),side=3)
points(xob, yob, pch=pch)

}

#reg nlinear

if(grau=='pot' && is.numeric(a$'Coeficientes modelo potencia')) {
  if(is.numeric(a$'Coeficientes modelo potencia')==FALSE) { print(a$'AIC modelo potencia') } # estava por fora do if
  dev.new()
  b0<-a$'Coeficientes modelo potencia'[1]; b1<-a$'Coeficientes modelo potencia'[2]
  y<-b0*x^b1
  yob<-as.numeric(as.vector(a$'Quadro de medias'[,2]))
  if(is.null(ylim)==TRUE) ylim=c(min(y,yob), max(y,yob))
  plot(x,y,'l', main=main, sub=sub, bty=bty, xlab=xlab, ylab=ylab,
       xlim=xlim, ylim=ylim)
  if(mod==TRUE) mtext(paste('y = ',round(b0,3),'* x^',round(b1,3),' R^2 aprox = ',
                            round(a$'R2 aprox modelo potencia'*100,2),'%'),side=3)
  points(xob, yob, pch=pch)
}

if(grau=='exp' && is.numeric(a$'Coeficientes modelo exponencial')) {
  if(is.numeric(a$'Coeficientes modelo exponencial')==FALSE) { print(a$'AIC modelo exponencial') } # estava por fora do if
  dev.new()
  b0<-a$'Coeficientes modelo exponencial'[1]; b1<-a$'Coeficientes modelo exponencial'[2]
  y<-b0*exp(b1*x)
  yob<-as.numeric(as.vector(a$'Quadro de medias'[,2]))
  if(is.null(ylim)==TRUE) ylim=c(min(y,yob), max(y,yob))
  plot(x,y,'l', main=main, sub=sub, bty=bty, xlab=xlab, ylab=ylab,
       xlim=xlim, ylim=ylim)
  if(mod==TRUE) mtext(paste('y = ',round(b0,3),'* exp(',round(b1,3),'x)   R^2 aprox = ',
                round(a$'R2 aprox modelo exponencial'*100,2),'%'),side=3)
  points(xob, yob, pch=pch)
}

if(grau=='log' && is.numeric(a$'Coeficientes modelo logistico')) {
  if(is.numeric(a$'Coeficientes modelo logistico')==FALSE) { print(a$'AIC modelo logistico') } # estava por fora do if
  dev.new()
  b0<-a$'Coeficientes modelo logistico'[1]
  b1<-a$'Coeficientes modelo logistico'[2]
  b2<-a$'Coeficientes modelo logistico'[3]
  y<-b0/(1+exp(b1-(b2*x)))
  yob<-as.numeric(as.vector(a$'Quadro de medias'[,2]))
  if(is.null(ylim)==TRUE) ylim=c(min(y,yob), max(y,yob))
  plot(x,y,'l', main=main, sub=sub, bty=bty, xlab=xlab, ylab=ylab,
       xlim=xlim, ylim=ylim)
if(mod==TRUE) mtext(paste('y = ',
    round(b0,3),'/(1+exp(',round(b1,3),'-(',round(b2,3),'*x)))   R^2 aprox = ',
    round(a$'R2 aprox modelo logistico'*100,2),'%'),side=3)
  points(xob, yob, pch=pch)
}

if(grau=='gomp' && is.numeric(a$'Coeficientes modelo Gompertz')) {
  if(is.numeric(a$'Coeficientes modelo Gompertz')==FALSE) { print(a$'AIC modelo Gompertz') } # estava por fora do if
  dev.new()
  b0<-a$'Coeficientes modelo Gompertz'[1]
  b1<-a$'Coeficientes modelo Gompertz'[2]
  b2<-a$'Coeficientes modelo Gompertz'[3]
  y<-b0*exp(-exp(b1-(b2*x)))
  yob<-as.numeric(as.vector(a$'Quadro de medias'[,2]))
  if(is.null(ylim)==TRUE) ylim=c(min(y,yob), max(y,yob))
  plot(x,y,'l', main=main, sub=sub, bty=bty, xlab=xlab, ylab=ylab,
       xlim=xlim, ylim=ylim)
if(mod==TRUE) mtext(paste('y = ',
  round(b0,3),'*exp(-exp(',round(b1,3),'-(',round(b2,3),'*x)))   R^2 aprox = ',
  round(a$'R2 aprox modelo Gompertz'*100,2),'%'),side=3)
  points(xob, yob, pch=pch)
}

}
