#' Image segmentation function (Funcao para a segmentacao de imagens).
#'
#' @description Esta funcao possibilita a segmentacao de imagens por meio de um
#'   limiar. Para isso, pode-se escolher um valor arbtrario ou considerar o
#'   valor estabelecido pelo metodo otsu.
#' @usage segmentation_steps(img,indexes=c("b","b"),threshold=c("otsu",0.6),
#' selectHigher=c(TRUE,FALSE),fillHull=c(TRUE,FALSE),fillBack=c(TRUE,FALSE),
#' thresholdFinish="otsu", selectHigherFinish=TRUE, fillHullFinish=TRUE,
#' fillBackFinish=TRUE,plot=FALSE)
#' @param img    :Este objeto deve ser obrigatoriamente imagem colorida do formato EBImage/ExpImage.
#' @param indexes    : Deve ser um vetor contendo os indices a serem utilizados em cada etapa
#' de segmentacao. As opcoes de indices sao:\cr
#'    "r" = extrair a banda de vermelho\cr
#'    "g" = extrair a banda de verde\cr
#'     "b" = extrair a banda de azul\cr
#'    "rg" = considera a media da banda de vermelho e verde: (r+g)/2\cr
#'    "rb" = considera a media da banda de vermelho e azul: (r+b)/2\cr
#'    "gb" = considera a media da banda de verde e azul: (g+b)/2\cr
#'   "rgb" = considera a media das 3 bandas: (r+g+b)/3\cr
#'    "BI"=sqrt((r^2+g^2+b^2)/3)\cr
#'    "BIM"=sqrt((2r+2g+2b)/3)\cr
#'    "SCI"=(r-g)/(r+g)\cr
#'    "GLI"=(2g-r-b)/(2g+r+b)\cr
#'    "HI"=(2r-g-b)/(g-b)\cr
#'    "NGRDI"=(g-r)/(g+r)\cr
#'    "SI"=(r-b)/(r+b)\cr
#'    "VARI"=(g-r)/(g+r-b)\cr
#'    "HUE"=atan(2(b-g-r)/30.5(g-r))\cr
#'    "MGVRI"=(g^2-r^2)/(g^2+r^2)\cr
#'    "GLI"=(2g-r-b)/(2g+r+b)\cr
#'    "MPRI"=(g-r)/(g+r)\cr
#'    "RGVBI"=(g-(br))/(g^2(br))\cr
#'    "ExG"=(2*g-r-b)\cr
#'    "VEG"=(g/(g^0.66667*b^0.66667))\cr
#'
#' @param threshold    : E um vetor com valores numericos entre 0 e 1 a ser considerado como
#'   limiar. O usuario pode tambem usar o argumento "ostu", caso queira
#'   considerar o limiar estabelecido por essa metodologia.
#' @param selectHigher    :Este argumento deve se um vetor com as palavras TRUE ou
#'   FALSE. TRUE e quando se quer selecionar pixels de valores  maiores que o
#'   limiar. FALSE quando se deseja selecionar valores menores.
#' @param fillHull    :Este argumento deve se um vetor com as palavras TRUE quando se
#'   pretende desconsiderar valores vazios dentro do foreground, caso contrario
#'   FALSE.
#' @param fillBack    :Este argumento deve se um vetor com as TRUE quando se
#'   pretende desconsiderar valores vazios dentro do background, caso contrario
#'   FALSE.
#' @param thresholdFinish  E um valor entre 0 e 1 a ser considerado como
#'   limiar na ultima segmentacao. O usuario pode tambem usar o argumento "ostu", caso queira
#'   considerar o limiar estabelecido por essa metodologia.
#' @param selectHigherFinish argumento com as palavras TRUE ou
#'   FALSE. TRUE (default) e quando se quer selecionar pixels de valores  maiores que o
#'   limiar na ultima segmentacao. FALSE quando se deseja selecionar valores menores.
#' @param fillHullFinish  :Este argumento deve ser TRUE quando se
#'   pretende desconsiderar valores vazios dentro do foreground na ultima segmentacao, caso contrario
#'   FALSE.
#' @param fillBackFinish  :Este argumento deve ser TRUE quando se
#'   pretende desconsiderar valores vazios dentro do background na ultima segmentacao, caso contrario
#'   FALSE.
#' @param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem segmentada.
#' @return Imagem segmentada
#' @seealso  \code{\link{segmentation_logit}}
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#' @export
#' @examples
#'\donttest{
#'############################################################################
#'#Obtendo o numero de ovos em uma folha
#'############################################################################
#'
#' #Carregar imagem de exemplo
#' im=read_image(example_image(2),plot=TRUE)

#' ## Fazendo a segmentacao na imagem
#' m=segmentation_steps(img=im,
#' indexes=c("b","b"),
#' threshold=c(0.39,0.7),
#' selectHigher=c(FALSE,TRUE),
#' fillHull=c(TRUE,TRUE),
#' fillBack=c(FALSE,FALSE),
#' thresholdFinish = 0.5,
#' selectHigherFinish = TRUE,
#' fillHullFinish = FALSE,
#' plot=TRUE)
#' measure_image(m,splitConnected = FALSE)
#' #Ver a mascara sobre os ovos na foto
#' im3=mask_pixels(im,m==1,plot=TRUE)
#'}




segmentation_steps=function(img,
                            indexes=c("b","b"),
                            threshold=c("otsu",0.6),
                            selectHigher=c(TRUE,FALSE),
                            fillHull=c(TRUE,FALSE),
                            fillBack=c(TRUE,FALSE),
                            thresholdFinish="otsu",
                            selectHigherFinish=TRUE,
                            fillHullFinish=TRUE,
                            fillBackFinish=TRUE,
                            plot=FALSE){
  na=c(length(indexes),length(threshold),length(selectHigher),length(fillHull),
      length(fillBack))


 if(length(unique(na))>1){stop("Os parametros indexes, threshold, selectHigher,fillHull, e fillBack devem ter o mesmo comprimento \n
                      The parameters indexes, threshold, selectHigher, fillHull, and fillBack must have the same length ")}

 n=mean(na)


  for(i in 1:n){
 m=gray_scale(img,method = indexes[i],plot=F)
 ms=segmentation(m,
                               threshold = threshold[i],
                               fillHull = fillHull[i],
                               fillBack = fillBack[i],
                               selectHigher = selectHigher[i],plot=F)

 bk=c(0,0,0)
 if(selectHigher[i]) bk=c(1,1,1)

 im2=extract_pixels(img,target =ms,valueTarget =1,
                    valueSelect = bk,plot=FALSE )

 img=im2
}


 b=gray_scale(img,method = "rgb")


 res=segmentation(b,threshold=thresholdFinish,selectHigher = selectHigherFinish,plot = plot,
                    fillHull=fillHullFinish,fillBack = fillBackFinish)
 return(res)
 }
