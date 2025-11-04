#' Obtenção de tabelas por agregado via API SIDRA - IBGE
#'
#' Esta função retorna uma lista com Todas as tabelas para  agregado indicado
#' @param agregado Número do agregado.
#' @return Uma lista contendo dois `data.table`s:
#' \itemize{
#'   \item{\code{pesquisas}: Um `data.table` com o ID e o nome das pesquisas relacionadas.}
#'   \item{\code{tabelas}: Um `data.table` com o ID e o nome das tabelas (agregados) disponíveis para a consulta indicada.}
#' }
#' @keywords IBGE SIDRA dados variáveis
#' @export
#' @examples
#' tabs_a70 <- tab_agr('A70')
#' tab_agr('A70') # imprime os agregados com sua descrição
#'
#'


## Função auxiliar
tab_agr <- \(agregado) {
  baseag <- "https://servicodados.ibge.gov.br/api/v3/agregados"
  rc <- substr(agregado,1,1)
  x <- substr(agregado,2,nchar(agregado))
  rota <- sidra::agregados[sidra::agregados$id==rc,]$rota
  url <- paste0(baseag,"?",rota,"=",x)
  resp <- call_ibge({httr::GET(url,config = httr::timeout(2))})
  if (is.null(resp)){
    return(invisible(NULL))
  }

  resp <- httr::content(resp)
  pesquisas <- try(
    data.table::rbindlist(lapply(seq_along(resp),
                                 \(x) resp[[x]][c("id","nome")]),fill=T,use.names = F))
  tabelas <- try(
    data.table::rbindlist(unlist(lapply(seq_along(resp),
                                 \(x) resp[[x]]$agregados),
                                 recursive=F),fill=T,use.names = F))
  resp <- list(pesquisas=pesquisas,
               tabelas=tabelas)
  return(resp)
}
