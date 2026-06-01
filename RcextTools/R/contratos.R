#' Dados de contratos relativos a base de licitacoes realizadas por orgaos de um ente federativo brasileiro
#' no periodo de 2011 a 2015
#'
#' IMPORTANTE: Todos os campos que pudessem identificar o ente federativo, orgaos, empresas
#' e certames tiveram seus valores alterados e embaralhados.
#'
#' contratos - Dados relativos aos contratos resultantes das licitacoes.
#'
#' Um data frame contendo 127449 registros de 15 campos.
#'
#' Os campos cujos valores foram alterados estao identificados por um asteristico (*):
#' \itemize{
#'   \item{\code{SIGLA_UGR [*]}}{ - Sigla da unidade gestora responsavel pelo certame.}
#'   \item{\code{UGR [*]}}{ - Descricao da unidade gestora responsavel pelo certame.}
#'   \item{\code{TIPO_CONTRATACAO}}{ - Tipo da contratacao.}
#'   \item{\code{COD_LICITACAO [*]}}{ - Codigo que identifica a licitacao na base de dados.}
#'   \item{\code{NUMERO_CONTRATO [*]}}{ - Numero do contrato.}
#'   \item{\code{COD_ARTIGO}}{ - Codigo identificador do artigo, que se refere a uma descricao generica universal do item contratado.}
#'   \item{\code{ARTIGO}}{ - Descricao generica universal do item contratado.}
#'   \item{\code{ID_ITEM}}{ - Identificador do item do objeto a que o participante concorre para fornecer.}
#'   \item{\code{DESC_ITEM}}{ - Descricao do item do objeto a que o participante concorre para fornecer.}
#'   \item{\code{VALOR_INICIAL_CONTRATACAO}}{ - Valor inicial do contrato.}
#'   \item{\code{VALOR_FINAL_CONTRATACAO}}{ - Valor final do contrato.}
#'   \item{\code{CNPJ_FORNECEDOR [*]}}{ - CNPJ do forncedor contratado.}
#'   \item{\code{NOME_FORNECEDOR [*]}}{ - Nome do forncedor contratado.}
#'   \item{\code{DATA_INICIO_VIGENCIA}}{ - Data do inicio da vigencia do contrato.}
#'   \item{\code{DATA_FIM_VIGENCIA}}{ - Data do final da vigencia do contrato.}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name contratos
#' @usage data(contratos)
#' @format Um data frame contendo 127449 registros de 15 campos.
"contratos"
