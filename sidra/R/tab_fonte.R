#' Obtenção de dados via API SIDRA - IBGE
#'
#' Esta função retorna a fonte de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @return uma `string` (vetor de caracteres de comprimento 1) com o nome da fonte
#' dos dados da tabela solicitada
#' @keywords IBGE SIDRA metadados fonte
#' @export
#' @examples
#' fonte_ipcaq <- tab_fonte(1705)
#' tab_fonte(1705) # imprime o nome da fonte

tab_fonte <- function(tabela) {
  basedf <- tab_meta(tabela)
  if (is.null(basedf)){
    return(invisible(NULL))
  }
  basedf$pesquisa
}
