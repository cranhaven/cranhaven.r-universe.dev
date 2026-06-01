#' Processa e agrega uma Demonstracao do Resultado do Exercicio (DRE) em categorias contabeis
#'
#' Esta funcao realiza o pre-processamento de um \code{data.frame} contendo contas da Demonstracao
#' do Resultado do Exercicio (DRE). O procedimento envolve a normalizacao dos nomes das contas,
#' a conversao dos valores financeiros expressos no formato brasileiro para valores numericos
#' padrao e a classificacao das contas nas categorias contabeis predefinidas em \code{categorias_dre}.
#'
#' Apos a categorizacao, os valores sao agregados por categoria, permitindo a analise consolidada
#' da estrutura de resultados da empresa. O objetivo e padronizar demonstracoes provenientes de
#' diferentes fontes, facilitando comparacoes, calculos de indicadores e analises financeiras.
#'
#' A primeira coluna do \code{data.frame} deve conter os nomes das contas, enquanto as demais
#' colunas devem conter valores financeiros representados como texto no formato contabil brasileiro
#' (uso de ponto como separador de milhar e virgula como separador decimal, com parenteses
#' indicando valores negativos).
#'
#' @param df Um \code{data.frame} contendo a DRE. A primeira coluna deve corresponder aos nomes
#'           das contas, e as colunas seguintes devem conter valores financeiros em formato de
#'           texto (por exemplo, \code{"(45.300,50)"}).
#'
#' @return Uma lista contendo dois objetos:
#' \describe{
#'   \item{\code{agregado}}{Um \code{data.frame} com os valores financeiros agregados por categoria da DRE.}
#'   \item{\code{original}}{O \code{data.frame} original, com os valores convertidos e a coluna \code{Categoria} adicionada.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   Conta = c("Receita Bruta de Vendas", "ICMS", "Custo das Mercadorias Vendidas"),
#'   X2023 = c("100.000,00", "(15.000,00)", "(40.000,00)")
#' )
#'
#' resultado <- padronizar_dre(df)
#' resultado$agregado
#'
#' @importFrom dplyr mutate group_by summarise across %>%
#' @importFrom magrittr %>%
#'
#' @export
padronizar_dre <- function(df){
  `%>%` <- magrittr::`%>%`

  # Verifica e renomeia a primeira coluna como "Conta"
  colnames(df)[1] <- "Conta"

  # Normaliza os textos da coluna Conta
  df$Conta <- sapply(df$Conta, normalizar_texto)

  # Identifica colunas com texto que nao sao "Conta"
  cols_num <- names(df)[sapply(df, is.character) & names(df) != "Conta"]

  # Converte colunas numericas (tratando formato brasileiro)
  df[cols_num] <- lapply(df[cols_num], conv_br_numeric)

  # Aplica categorizacao das contas da DRE
  dados <- df %>%
    dplyr::mutate(Categoria = sapply(Conta, classificar_conta_dre))

  # Agrega valores por categoria
  df_agregado <- dados %>%
    dplyr::group_by(Categoria) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

  # Mensagem de sucesso
  message("A Demonstracao de Resultado foi padronizada com sucesso.")

  # Retorna os dois data frames
  return(list(agregado = df_agregado, original = dados))
}
