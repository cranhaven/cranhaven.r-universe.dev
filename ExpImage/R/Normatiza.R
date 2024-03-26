#' Normatiza a matriz para que os valores variem entre valores pre-estabelecidos.
#'
#' @description Esta funcao faz a padronizacao da matriz a fim de retirar a escala.
#' Para isso, os valores das matrizes sao calculados a fim de variar entre um
#' "LimiteInferior" e um "LimiteSuperior".
#' @usage Normatiza(DadosEntrada, DadosBase=NULL, LimiteInferior=0, LimiteSuperior=1,Metodo=1)
#' @param DadosEntrada Matriz contendo os dados sendo normatizados.
#' @param DadosBase Matriz contendo o conjunto de dados referencia para a normatizacao.
#' Se for "NULL" essa matriz de referencia sera a propria matriz de entrada.
#' @param LimiteInferior Numero cujo menor valor devera corresponder.
#' @param LimiteSuperior Numero cujo maior valor devera corresponder.
#' @param Metodo indica a forma que a normatizacao sera feita. Pode receber o valor
#' 1 ou 2:
#' \itemize{
#'  \item  1 = A normatizacao sera feita considerando os dados de cada coluna individualmente.
#'  \item  2 = A normatizacao sera feita considerando os dados de toda a matriz simultaneamente.
#'  }
#'
#' @return Retorna a matriz normatizada.
#' @seealso \code{\link{gray_scale}}
#' @references
#' PlayList "Curso de Analise Multivariada":
#'  https://www.youtube.com/playlist?list=PLvth1ZcREyK72M3lFl7kBaHiVh5W53mlR
#'
#'
#' CRUZ, C.D. and CARNEIRO, P.C.S.  Modelos biometricos aplicados ao
#'   melhoramento genetico. 3nd Edition. Vicosa, UFV, v.2, 2014. 668p.  (ISBN: 8572691510)
#'
#' FERREIRA, D.F. Estatistica Multivariada. (2018) 3ed. UFLA. 624p. (ISBN 13:978 8581270630)
#'
#'  HAIR, J.F. Multivariate Data Analysis.  (2016) 6ed. Pearson Prentice HalL.
#'   (ISBN 13:978 0138132637)
#' @examples
#' end=example_image(2)
#' ima=read_image(end,plot=TRUE)
#' VARI=gray_scale(ima,method = "VARI",plot=TRUE)
#' VARIb=VARI*2-1
#' min(VARIb)
#' max(VARIb)
#'
#' VARI2=Normatiza(VARIb,LimiteInferior=0, LimiteSuperior=1,Metodo=2)
#' min(VARI2)
#' max(VARI2)

#' @export





Normatiza=function(DadosEntrada, DadosBase=NULL,
                   LimiteInferior=0, LimiteSuperior=1,Metodo=1){
  if(suppressWarnings(class(DadosEntrada)[1]=="Distancia")){DadosEntrada=DadosEntrada$Distancia}
  if(suppressWarnings(class(DadosBase)[1]=="Distancia")){DadosBase=DadosBase$Distancia}
  DadosEntrada[is.nan(DadosEntrada)]=NA

  #DadosEntrada=as.matrix(DadosEntrada)
  if(is.null(DadosBase)){DadosBase=DadosEntrada}
  #DadosBase=as.matrix(DadosBase)
  if(Metodo==1){
  valMax = apply(DadosBase,2,max,na.omit=T)
  valMin = apply(DadosBase,2,min,na.omit=T)
  valMax2=DadosEntrada
  valMin2=DadosEntrada
  Normatizado=DadosEntrada
  for (i in 1:ncol(DadosEntrada)){
    for (j in 1:nrow(DadosEntrada)){
      valMax2[j, i] = valMax[i]
      valMin2[j, i] = valMin[i]
    }}
  Normatizado=(LimiteSuperior - LimiteInferior)*(DadosEntrada- valMax2)/(valMax2- valMin2)
  Normatiza=Normatizado+LimiteSuperior

  }

  if(Metodo==2){
    dbase=c(DadosBase)
    dentrada=c(DadosEntrada)
    dbase[is.infinite(dbase)]=NA
    dentrada[is.infinite(dentrada)]=NA

    valMax = max(dbase,na.rm=T)
    valMin = min(dbase,na.rm=T)
    valMax2=max(dentrada,na.rm=T)
    valMin2=min(dentrada,na.rm=T)

    Normatizado=(LimiteSuperior - LimiteInferior)*(DadosEntrada- valMax2)/(valMax2- valMin2)
    Normatiza=Normatizado+LimiteSuperior



  }
  #class(Normatiza)="matrix"
  return(as.matrix(Normatiza))
}
