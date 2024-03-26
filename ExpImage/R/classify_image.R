#' Filter values in an array or image (Filtrar valores em uma matriz ou imagem).
#'
#' @description Function to filter values in an array or image
#' (Funcao para filtrar valores em uma matriz ou imagem).
#' @usage classify_image(img,filter)
#' @param img Este objeto deve conter uma imagem no formato do EBImage ou na forma
#'de uma matriz (This  object must contain an image in EBImage format or a matrix).
#' @param filter Vetor com tres valores, o valor minimo, maximo e o argumento a ser substituido
#' (Vector with three values, the minimum, maximum value and the argument to be substituted).
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#' @return Returns an array (retorna uma matriz).
#' end=example_image(3)
#' im=read_image(end,plot=TRUE)
#' m=gray_scale(im,method = "r",plot=TRUE)
#'
#' m2=classify_image(m,filter = c(0.5,Inf,NA))
#' plot_image(m2)
#'
#'@export

classify_image=function(img,filter){
  mat=img
  filtro=filter
  id=(mat>filtro[1])&(mat<=filtro[2])
  mat[id]=filtro[3]
  mat}
