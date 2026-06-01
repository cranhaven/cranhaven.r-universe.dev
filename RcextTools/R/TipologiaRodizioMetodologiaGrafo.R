#' Identifica potencias mercados de risco de praticas colusivas a partir de grafo de licitacoes
#' @param grLicitacoes objeto do tipo \code{igraph} contendo um grafo direcionado de vencedores e participantes
#'          de licitacoes;
#' @return objeto do tipo environment, contendo os seguintes objetos:
#' \itemize{
#'         \item \strong{cmMercados} objeto do tipo \code{community} contendo todos as comunidades (mercados) obtidas a partir do grafo \code{grLicitacoes};
#'         \item \strong{grMercadosRisco} grafo do tipo \code{igraph} contendo os mercados de risco extraidos do grafo \code{grLicitacoes};
#'         \item \strong{vcMercadosRisco} vetor do tipo \code{numeric} contendo os identificadores dos mercados considerados de risco;
#'         \item \strong{vcEmpresasRisco} vetor do tipo \code{numeric} contendo os identificadores dos mercados de risco a que pertencem as empresas
#'               consideradas suspeitas de praticarem acoes colusivas. As empresas sao identificadas pelo atributo \code{names}.
#'          }
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' grafoLic <- TipologiaRodizioMetodologiaGrafo(grLicitacoes)
#' }
#' @seealso \code{igraph}
TipologiaRodizioMetodologiaGrafo <- function(grLicitacoes) {

  e <- new.env(parent = emptyenv())

  # extrai informacoes das arestas do grafo para um data.frame
  dfArestas <- igraph::get.data.frame(grLicitacoes)

  # identifica "comunidades" (mercados)
  wc <- igraph::walktrap.community(grLicitacoes)

  # inclui no environment "e" uma copia do grafo "grLicitacoes" a partir da qual sera construido
  # o grafo contendo unicamente as comunidades correspondentes a "mercados de risco"
  e$grMercadosRisco <- grLicitacoes

  # inicializa o vetor que ira conter as comunidades (mercados) consideradas como "de risco"
  e$vcMercadosRisco <- numeric()

  # inicializa o vetor que ira conter as empresas suspeitas de pratica colusiva
  e$vcEmpresasRisco <- numeric()

  # cria um environment para as ser utilizado como uma estrutura do tipo map para armazenar os page
  # ranks das empresas num dado mercado cujo identificador sera utilizado como key
  e$mapPageRanks <- new.env(parent = emptyenv())


  calculaPageRankIntraComunitario <- function(id_comunidade, comunidades, grafo){
    # empresas que pertencem a comunidade g (mercado)
    empresas_comunidade_g <- which(igraph::membership(comunidades)==id_comunidade)

    # extrai subgrafo correspondente a comunidade g
    subg<-igraph::induced.subgraph(grafo, empresas_comunidade_g)

    # calcula page rank intracomunitario
   igraph::page.rank(subg)$vector
  }

  # selecao de empresas a partir do page rank intra-comunitario
  sapply(sort(unique(igraph::membership(wc))), function(g) {

    # calcula page rank intra-comunitario
    pr <- calculaPageRankIntraComunitario(g, wc, grLicitacoes)

    # determina o rearranjo necessario para ordenar as empresas em ordem decrescente de page rank
    ordem_dec <- order(pr, decreasing = T)

    # reordena de forma decrescente o vetor de page ranks
    pr <- pr[ordem_dec]

    # vetor de empresas que pertencem a comunidade g em orderm decrescente de page.rank
    empresas_comunidade_g <- names(pr)

    # seleciona as empresas de maior page_rank até que o rank acumulado seja de 0.6
    selec_emp <- cumsum(pr)<.6

    # o numero de empresas acima selecionadas não devera ultrapassar 30% do total ou 10 empresas
    max_emp <- min(ceiling(0.3*length(pr)), 10)

    # ... nem ser inferior a 3
    min_emp <- 3

    if ((sum(empresas_comunidade_g[selec_emp] %in% dfArestas$to)>1) & # caso nenhuma empresa escolhida tenha sido vencedora, descarta a comunidade
        (sum(selec_emp) <= max_emp) &
        (sum(selec_emp) >= min_emp)) {

      # insere a comunidade (mercado) na listagem de mercados de risco
      e$vcMercadosRisco <- c(e$vcMercadosRisco, g)

      # armazena o vetor de page ranks no environment e
      eval(parse(text = paste("e$mapPageRanks$'", "' <- pr", sep = as.character(g))))

      # atualiza o vetor de empresas suspeitas
      vcEmpresasRisco <- rep(g, sum(selec_emp))
      names(vcEmpresasRisco) <- empresas_comunidade_g[selec_emp]
      e$vcEmpresasRisco <- c(e$vcEmpresasRisco, vcEmpresasRisco)
    } else {
      # retira todo o subgrafo (comunidade) do grafo principal
      e$grMercadosRisco <- igraph::delete.vertices(
        e$grMercadosRisco,
        empresas_comunidade_g)
    }
  })

  # preserva no grafo apenas as empresas consideradas de risco
  e$grMercadosRisco <- igraph::delete.vertices(
    e$grMercadosRisco,
    names(igraph::V(e$grMercadosRisco))[!(names(igraph::V(e$grMercadosRisco)) %in% names(e$vcEmpresasRisco))])

  # detecta nos isolados
  isolados <- which(igraph::degree(e$grMercadosRisco, mode = 'all') == 0)

  # e a seguir os retira do grafo
  e$grMercadosRisco <- igraph::delete.vertices(
    e$grMercadosRisco,
    names(igraph::V(e$grMercadosRisco))[isolados])

  # faz nova deteccao de comunidades (mercados)
  e$cmMercados <- igraph::walktrap.community(e$grMercadosRisco)
  e$vcMercadosRisco <- sort(unique(e$cmMercados$membership))
  e$vcEmpresasRisco <- igraph::membership(e$cmMercados)

  # recalcula page.ranks
  e$lsPageRanks <- lapply(X = sort(unique(igraph::membership(e$cmMercados))),
                           FUN = calculaPageRankIntraComunitario,
                           e$cmMercados, e$grMercadosRisco)
  names(e$lsPageRanks) <- sort(unique(igraph::membership(e$cmMercados)))

  return(e)
}
