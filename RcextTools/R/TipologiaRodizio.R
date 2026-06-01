#' Identifica potenciais mercados de risco de praticas colusivas a partir de grafo de licitacoes
#' @param dados data.frame contendo as seguintes colunas:
#' \itemize{
#'         \item \strong{CNPJ} coluna do tipo \code{character} contendo cnpj, com 14 caracteres (sem .,-, ou /),
#'          da empresa participante do certame;
#'         \item \strong{ID_LICITACAO} coluna do tipo \code{character} que identifica de forma unica o certame;
#'         \item \strong{ID_ITEM} coluna do tipo \code{character} que identifica de forma unica o item do objeto a que
#'         a empresa esteja concorrendo. Caso o objeto da licitacao nao tenha sido dividido em itens, este campo
#'         \item \strong{VENCEDOR} coluna do tipo \code{logical} contendo um valor booleano indicando se o licitante foi
#'         vitorioso no certame.
#'         \item \strong{VALOR_ESTIMADO} coluna do tipo \code{numeric} correspondente ao valor estimado para o objeto ou
#'         serviço sendo licitado. Podera assumir o valor NA caso tal informacao nai esteja disponivel.
#'         \item \strong{VALOR_HOMOLOGADO} coluna do tipo \code{numeric} correspondente ao valor homologado da proposta
#'         vencedora para o fornecimento do objeto ou serviço sendo licitado. Podera assumir o valor NA caso tal
#'         informacao nai esteja disponivel.
#'         }
#' @param considerarDesconto parametro do tipo \code{logical} indicando se o desconto obtido (diferenca entre o valor
#' homologado e o valor estimado) devera ser levado em consideracao na atribuicao dos pesos das relacoes perdedor-vencedor.
#' Por padrao este parametro tem valor \code{TRUE}
#' @return objeto S3 da classe \code{TipologiaRodizio}, contendo os seguintes atributos:
#' \itemize{
#'         \item \strong{mercados} objeto do tipo \code{igraph::communities} contendo todos as comunidades (mercados) obtidas a partir do grafo \code{grLicitacoes};
#'         \item \strong{grafo} grafo do tipo \code{igraph} contendo os mercados de risco presentes no grafo \code{mercados};
#'         \item \strong{tabela} objeto do tipo \code{data.frame} contendo informacoes dos contratos considerados como suspeitos. Dentre os campos nela presentes, destacamos:
#'         \itemize{
#'                 \item \strong{MERCADO_ATUACAO} identificador do mercado a que o contrato pertence, relacionando-o ao atributo \code{mercados}; e
#'                 \item \strong{PROB_FAVORECIMENTO_NO_MERCADO} probabilidade, estimada com base no PageRank intracomunitario, de o contrato ter sido fruto de alguma acao colusiva naquele mercado especifico.
#'                 }
#'         }
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#'
#'  # carrega dados de licitacoes da base fornecida pelo pacote RcextTools
#'  data("part_lic")
#'
#'  dtDados <- part_lic[!is.na(part_lic$COD_LICITACAO),]
#'
#'  dtDados <- data.frame(
#'    CNPJ = dtDados$CNPJCPF_FORNECEDORES,
#'    ID_LICITACAO = dtDados$COD_LICITACAO,
#'    ID_ITEM = dtDados$ID_ITEM,
#'    VENCEDOR = ifelse(dtDados$VENCEDOR == 'S', T, F),
#'    VALOR_ESTIMADO = NA,
#'    VALOR_HOMOLOGADO = as.numeric(dtDados$VALOR_FINAL),
#'    DESC_OBJETO = dtDados$RESUMO_OBJETO,
#'    stringsAsFactors = F
#'  )
#'
#'  casosSuspeitos <- TipologiaRodizio(dtDados)
#'
#'  # imprime dataframe com resultados
#'  print(casosSuspeitos)
#'
#'  # plota grafo
#'  plot(casosSuspeitos)
#' }
#' @seealso \code{igraph}
#' @importFrom stats complete.cases
#' @import data.table
#' @export
TipologiaRodizio <- function(dados, considerarDesconto = F) {

  # para passar nos checks do CRAN:
  CNPJ = NULL
  MERCADO_ATUACAO = NULL
  VALOR_HOMOLOGADO = NULL
  VENCEDOR = NULL

  dados <- data.table(dados)

  # Geracao do grafo para ser analisado
  grafo <- TipologiaRodizioCriaGrafo(dados)

  # Identificacao de empresas e "mercados" de maior risco de acao colusiva
  e <- TipologiaRodizioMetodologiaGrafo(grafo$grLicitacoes)

  # Mantem apenas as empresas suspeitas
  dtResultados <- dados[VENCEDOR == T & CNPJ %in% names(e$vcEmpresasRisco),]

  # Inclui informacao de mercado
  dtResultados$MERCADO_ATUACAO <- apply(
    X = dtResultados,
    MARGIN = 1,
    FUN = function(contratos, empresas = e$vcEmpresasRisco){
      empresas[contratos['CNPJ'] == names(empresas)]
    }
  )

  # Ordena resultados por mercado de atuação e materialidade
  dtResultados <- dtResultados[order(MERCADO_ATUACAO,-VALOR_HOMOLOGADO)]


  # Inclui coluna com valor homologado em formato texto do tipo moeda
  numericoParaTextoMoeda <- function(x){
    paste0("R$",
           formatC(as.numeric(x),
                   format="f",
                   digits=2,
                   big.mark=".",
                   decimal.mark = ",")
    )
  }

  dtResultados$TEXTO_VALOR_HOMOLOGADO <- NA_character_
  dtResultados$TEXTO_VALOR_HOMOLOGADO <- numericoParaTextoMoeda(dtResultados$VALOR_HOMOLOGADO)
  dtResultados <- unique(dtResultados[!is.na(VALOR_HOMOLOGADO),])

  dtResultados$PROB_FAVORECIMENTO_NO_MERCADO <- 0
  for(m in names(e$lsPageRanks)){
    vecPageRank <- eval(expr = parse(text = paste0("e$lsPageRanks$`",m,"`")))
    for(pr in 1:length(vecPageRank)){
      dtResultados[MERCADO_ATUACAO == m & CNPJ == names(vecPageRank)[pr]]$PROB_FAVORECIMENTO_NO_MERCADO <- vecPageRank[pr]
    }
  }

  obj <- list(
    tabela = dtResultados,
    grafo = e$grMercadosRisco,
    mercados = e$cmMercados
  )

  class(obj) <- "TipologiaRodizio"

  return(obj)
}
