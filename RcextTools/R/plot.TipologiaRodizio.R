#' Metodo S3 que plota na tela uma representacao visual do grafo do tipo `igraph` contido no objeto da classe `TipologiaRodizio`
#' @author Bruno M. S. S. Melo
#' @description Os diferentes agrupamentos representam empresas suspeitas de praticarem alguma acao colusiva num determinado mercado. As arestas apontam na direcao de um perdedor para um vencedor de licitacao. Empresas sao sempre perdedoras sao representadas por quadrados cinzas.
#' @param x objeto da classe `TipologiaRodizio`.
#' @param ...	eventuais parametros adicionais.
#' @examples
#' \dontrun{
#' casosSuspeitos <- TipologiaRodizio(dados)
#' plot(casosSuspeitos)
#' }
#' @export
plot.TipologiaRodizio <- function(x, ...){

  dadosGrafo <- visNetwork::toVisNetworkData(x$grafo)
  dadosGrafo$nodes$group <- sapply(dadosGrafo$nodes$id, FUN = function(n){
    group <- x$tabela[x$tabela$CNPJ == n,]$MERCADO_ATUACAO[1]
  })
  dadosGrafo$nodes[!complete.cases(dadosGrafo$nodes),]$group <- 0

  # o operador %>% foi removido para solucionar erro gerado pelo "CRAN check":

  # visNetwork::visNetwork(nodes = dadosGrafo$nodes, edges = dadosGrafo$edges) %>%
  #   visNetwork::visInteraction( navigationButtons = TRUE, multiselect = TRUE ) %>%
  #   visNetwork::visOptions(nodesIdSelection = TRUE) %>%
  #   visNetwork::visEdges(arrows = 'to') %>%
  #   visNetwork::visGroups(groupname  = '0', color = 'grey', shape = "square")

  vn <- visNetwork::visNetwork(nodes = dadosGrafo$nodes, edges = dadosGrafo$edges)
  vn <- visNetwork::visOptions(vn, nodesIdSelection = TRUE)
  vn <- visNetwork::visEdges(vn, arrows = 'to')
  visNetwork::visGroups(vn, groupname  = '0', color = 'grey', shape = "square")
}
