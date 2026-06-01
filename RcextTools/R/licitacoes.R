#' Dados de licitacoes realizadas por orgaos de um ente federativo brasileiro
#' no periodo de 2011 a 2015
#'
#' IMPORTANTE: Todos os campos que pudessem identificar o ente federativo, orgaos, empresas
#' e certames tiveram seus valores alterados e embaralhados.
#'
#' Um data frame contendo 8679 registros de 10 campos.
#'
#' Os campos cujos valores foram alterados estao identificados por um asteristico (*):
#' \itemize{
#'   \item{\code{CNPJ_UGR [*]}}{ - CNPJ da unidade gestora responsavel pelo certame.}
#'   \item{\code{SIGLA_UGR [*]}}{ - Sigla da unidade gestora responsavel pelo certame.}
#'   \item{\code{COD_LICITACAO [*]}}{ - Codigo que identifica a licitacao na base de dados.}
#'   \item{\code{RESUMO_OBJ}}{ - Descricao resumida do objeto da licitacao.}
#'   \item{\code{VALOR_ESTIMADO}}{ - Valor estimado do objeto.}
#'   \item{\code{DATA_ABERT_SESSAO}}{ - Data de abertura da sessão.}
#'   \item{\code{STATUS_LICITACAO}}{ - Status relativo a situacao do certame.}
#'   \item{\code{TIPO_LICITACAO}}{ - Tipo da licitacao.}
#'   \item{\code{VALOR_TOTAL_HOMOLOGADO}}{ - Valor final homologado para a totalidade do objeto sendo licitado.}
#'   \item{\code{DATA_HOMOLOGACAO}}{ - Data da homologacao do certame.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name licitacoes
#' @usage data(licitacoes)
#' @format Um data frame contendo 8679 registros de 10 campos.
"licitacoes"
