#' Obtenção de dados via API SIDRA - IBGE
#'
#' Esta função retorna uma lista com periodos de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @return um vetor `integer` ou `character` com os períodos disponíveis da tabela solicitada
#' @keywords IBGE SIDRA dados periodos
#' @export
#' @examples
#' periodos_ipcaq <- tab_periodos(1705)
#' tab_periodos(1705) # imprime os períodos disponíveis da tabela

tab_periodos <- function(tabela) {
  basedf <- tab_meta(tabela)
  if (is.null(basedf)) {
    return(invisible(NULL))
  }
  basedf$periodos

}
