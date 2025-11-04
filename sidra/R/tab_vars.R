#' Obtenção de dados via API SIDRA - IBGE
#'
#' Esta função retorna uma lista com variáveis de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @return um `data.frame` (`data.table`) com as seguintes colunas:
#' \itemize{
#'   \item{\code{id}: ids dos indicadores}
#'   \item{\code{nome}: nome dos indicadores}
#'   \item{\code{unidade}: unidade de medida do indicador.}
#'   \item{\code{sumarizacao}: tipo de agregação.}
#' }
#' @keywords IBGE SIDRA dados variáveis
#' @export
#' @examples
#' vars_ipcaq <- tab_vars(1705)
#' tab_vars(1705) # imprime os classificadores com sua descrição

tab_vars <- function(tabela) {
  basedf <- tab_meta(tabela)
  if (is.null(basedf)){
    return(invisible(NULL))
  }
  basedf$variaveis

}
