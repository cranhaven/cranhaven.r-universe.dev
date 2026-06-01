#' Processa e agrega um balanco patrimonial em categorias contabeis
#'
#' Esta funcao realiza o pre-processamento de um data frame contendo contas do balanco patrimonial,
#' convertendo os valores numericos no formato brasileiro para formato numerico padrao do R,
#' classificando as contas em categorias contabeis predefinidas e agregando os valores por categoria.
#'
#' A primeira coluna do data frame deve conter os nomes das contas, enquanto as demais colunas devem
#' conter os valores financeiros em formato textual brasileiro (com ponto como separador de milhar e virgula decimal).
#'
#' @param df Um \code{data.frame} contendo o balanco patrimonial. A primeira coluna deve conter os nomes das contas,
#'           e as colunas seguintes devem conter valores financeiros em formato de texto (ex: \code{"(1.234,56)"}).
#'
#' @return Uma lista com dois objetos:
#' \describe{
#'   \item{\code{agregado}}{Um \code{data.frame} com os valores agregados por categoria contabil.}
#'   \item{\code{original}}{O \code{data.frame} original com os valores convertidos e a nova coluna \code{categorias_bp}.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   Conta = c("Caixa e equivalentes de caixa", "Fornecedores"),
#'   X2022 = c("1.000,00", "(500,00)"),
#'   X2023 = c("1.200,00", "(600,00)")
#' )
#'
#' # Padronizando o balanço
#' resultado <- padronizar_balanco(df)
#' resultado$agregado
#'
#' @importFrom dplyr mutate group_by summarise across %>%
#'
#' @export
padronizar_balanco <- function(df){
  `%>%` <- magrittr::`%>%`

  # Verifica e renomeia a primeira coluna como "Conta"
  colnames(df)[1] <- "Conta"

  # Normaliza os textos da coluna Conta
  df$Conta <- sapply(df$Conta, normalizar_texto)

  # Identifica colunas que sao texto e que nao sao "Conta"
  cols_num <- names(df)[sapply(df, is.character) & names(df) != "Conta"]

  # Converte os valores para numerico com tratamento do formato brasileiro
  df[cols_num] <- lapply(df[cols_num], conv_br_numeric)

  # Aplica a categorizacao das contas
  dados <- df %>%
    dplyr::mutate(Categoria = sapply(Conta, classificar_conta_bp)) # %>%
    # filter(!is.na(Categorias))  # Remove contas sem categoria atribuida

  # Agrega os valores por categoria
  df_agregado <- dados %>%
    dplyr::group_by(Categoria) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")

  # Exibir mensagem de conclusao
  message("O Balaco Patrimonial foi padronizada com sucesso.")

  # Retorna os dois data frames como uma lista
  return(list(agregado = df_agregado, original = dados))
}
