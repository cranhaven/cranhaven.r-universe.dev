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
#' grafoLic <- rodizioMetodologiaGrafoPageRank(grLicitacoes)
#' }
#' @seealso \code{igraph}#' @importFrom igraph walktrap.community
rcextRiscoAcaoColusivaAux <- function(grLicitacoes) {
  .Deprecated("TipologiaRodizioMetodologiaGrafo")
  TipologiaRodizioMetodologiaGrafo(grLicitacoes)
}
