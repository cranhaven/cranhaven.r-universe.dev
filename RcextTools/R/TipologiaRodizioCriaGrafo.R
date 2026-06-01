#' Cria um grafo de vencedores e participantes de licitacoes publicas
#'
#' Utiliza-se um grafo direcionado para representar a relacao entre as empresas participantes das licitacoes,
#' da seguinte forma:
#' \itemize{
#'         \item cada empresa e representada por um no;
#'         \item as empresas que participaram de um mesmo certame estarao associadas por
#'          relacoes do tipo “perdedor-vencedor”. Tal relacao e representada por uma aresta
#'          que se inicia em no representativo da empresa participante perdedora para um no
#'          representativo da licitante vencedora.
#'         \item o desconto ofertado (diferenca entre o valor estimado e o valor homologado)
#'         podera influenciar, de forma inversamente proporcional, o peso das relacoes
#'         perdedor-vencedor. Quanto menor o desconto ofertado pelo vencedor, maior sera o peso
#'         da referida relacao.
#' }
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
#' }
#' @param tipoRetorno especifica o objeto a ser retornado pela funcao. As opcoes sao as que se seguem:
#' \itemize{
#'         \item 0 retorna um objeto do tipo \code{environment}, contendo um objeto do tipo \code{igraph}
#'          (grLicitacoes) e um \code{data.frame} (dfLicitacoes) a partir do qual o mesmo foi criado. E o valor padrao;
#'         \item 1 retorna um objeto do tipo \code{igraph} contendo um grafo direcionado de vencedores e participantes
#'          de licitacoes;
#'         \item 2 retorna um objeto do tipo \code{data.frame} a partir do qual podera ser criado um grafo
#'          por meio da funcao \code{igraph::graph.data.frame()}
#'          }
#' @param agregarArestas parametro do tipo \code{logical} indicando se arestas repetidas deverao ser agregadas numa unica
#' aresta cujo peso seja as soma dos pesos individuais. Por padrao este parametro tem valor \code{TRUE}.
#' @param considerarDesconto parametro do tipo \code{logical} indicando se o desconto obtido (diferenca entre o valor
#' homologado e o valor estimado) devera ser levado em consideracao na atribuicao dos pesos das relacoes perdedor-vencedor.
#' Por padrao este parametro tem valor \code{FALSE}.
#' @return o retorno depende do valor especificado para o parâmetro \code{tipoRetorno}.
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' grafoLic <- TipologiaRodizioCriaGrafo(dados = dfDadosLic, tipoRetorno = 0, considerarDesconto = F)
#' }
#' @seealso \code{igraph}
#' @importFrom sqldf sqldf
#' @import data.table
#' @export
TipologiaRodizioCriaGrafo <- function(dados, tipoRetorno = 0, agregarArestas = T, considerarDesconto = F) {

  # para passar nos checks do CRAN:
  VENCEDOR = NULL
  . = NULL
  `:=` = NULL
  from = NULL
  to = NULL
  weight = NULL
  VALOR_ESTIMADO = NULL
  VALOR_HOMOLOGADO = NULL
  PESO_RELACAO = NULL
  CNPJ_PERDEDOR = NULL
  CNPJ_VENCEDOR = NULL
  PESO_RELACAO = NULL
  ID_ITEM = NULL

  #library("data.table")

  if ((!is.numeric(tipoRetorno)) | (!(tipoRetorno %in% 0:2))) {
    tipoRetorno <- 0
  }

  dfPERDEDOR <- subset(x = dados, VENCEDOR == F)
  dfVENCEDOR <- subset(x = dados, VENCEDOR == T)

  envGrafo <- new.env(parent = emptyenv())

  suppressWarnings(
    envGrafo$dfLicitacoes <- data.table::data.table(
      sqldf::sqldf(
        'SELECT DISTINCT
            dfPERDEDOR.CNPJ CNPJ_PERDEDOR,
            dfVENCEDOR.CNPJ CNPJ_VENCEDOR,
            dfVENCEDOR.ID_LICITACAO,
            dfVENCEDOR.ID_ITEM,
            dfVENCEDOR.VALOR_ESTIMADO,
            dfVENCEDOR.VALOR_HOMOLOGADO
        FROM
            dfVENCEDOR
        INNER JOIN
            dfPERDEDOR
              ON (dfVENCEDOR.ID_LICITACAO = dfPERDEDOR.ID_LICITACAO
                  AND dfVENCEDOR.ID_ITEM = dfPERDEDOR.ID_ITEM
                  AND dfVENCEDOR.CNPJ != dfPERDEDOR.CNPJ )'
      )
    )
  )

  if (considerarDesconto){
    # retira registros que nao possuam valores estimados e valores homologados
    envGrafo$dfLicitacoes <- envGrafo$dfLicitacoes[complete.cases(envGrafo$dfLicitacoes[, c("VALOR_ESTIMADO", "VALOR_HOMOLOGADO")]),]

    # calcula peso da relacao perdedor-vencedor
    envGrafo$dfLicitacoes$PESO_RELACAO <- envGrafo$dfLicitacoes$VALOR_HOMOLOGADO/envGrafo$dfLicitacoes$VALOR_ESTIMADO

    # Melhor fazer isso mercado a mercado (noutro momento)
    # # retira registros cujos pesos calculados sejam outliers
    # tukey <- fivenum(envGrafo$dfLicitacoes$PESO_RELACAO)
    # Q3 <- tukey[4]
    # Q1 <- tukey[2]
    # envGrafo$dfLicitacoes <- envGrafo$dfLicitacoes[envGrafo$dfLicitacoes$PESO_RELACAO < (Q3 + 1.5*(Q3-Q1)),]

  } else{
    envGrafo$dfLicitacoes[,ID_ITEM:=NULL]
    envGrafo$dfLicitacoes[,VALOR_ESTIMADO:=NULL]
    envGrafo$dfLicitacoes[,VALOR_HOMOLOGADO:=NULL]
    envGrafo$dfLicitacoes <- unique(envGrafo$dfLicitacoes)
    envGrafo$dfLicitacoes$PESO_RELACAO <- 1
  }

  dtGrafoLicitacoes <- data.table(
    from = envGrafo$dfLicitacoes[, CNPJ_PERDEDOR],
    to = envGrafo$dfLicitacoes[, CNPJ_VENCEDOR],
    weight = envGrafo$dfLicitacoes[, PESO_RELACAO],
    stringsAsFactors = FALSE
  )

  if( agregarArestas ){
    dtGrafoLicitacoes <- dtGrafoLicitacoes[,.(weight = sum(weight)), by = .(from, to)]
  }

  envGrafo$grLicitacoes <- igraph::graph.data.frame(dtGrafoLicitacoes)


  return(envGrafo)
}
