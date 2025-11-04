#' Obtenção de dados via API SIDRA - IBGE
#'
#' Esta função retorna uma lista com classificadores de uma das tabelas da SIDRA.
#' @param tabela Número da tabela.
#' @keywords IBGE SIDRA dados classificadores
#' @return uma lista de `data.frame`s, onde cada um representa um classificador e suas categorias.
#' @export
#' @examples
#' class_ipcaq <- tab_class(1705)
#' tab_class(1705) # imprime os classificadores com sua descrição

tab_class <- \(tabela) {
  basedf <- tab_meta(tabela)
  if (is.null(basedf)){
    return(invisible(NULL))
  }

  basedf$classificacoes

}
