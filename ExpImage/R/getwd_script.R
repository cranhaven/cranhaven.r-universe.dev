
#' Obter o diretorio onde esta o script. (Get the script directory.)
#'@description Esta funcao retorna o edereco da pasta onde esta o scritp atual
#'(This function returns the directory of current script ).
#'@usage getwd_script()
#'@export

getwd_script=function(){
  nome=rstudioapi::getSourceEditorContext()$path
  nome2=unlist(strsplit(nome,"/"))
  paste(nome2[-length(nome2)], sep="/", collapse="/")
}


