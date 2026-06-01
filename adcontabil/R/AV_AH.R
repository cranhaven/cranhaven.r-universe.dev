#' Calcular Analise Vertical e Horizontal com projecao
#'
#' Esta funcao realiza a Analise Vertical (AV) e Analise Horizontal (AH) com base em dados contabeis de empresas, podendo ser aplicada tanto a dados agregados quanto a dados detalhados (ex: Balanco Patrimonial individualizado). Alem disso, gera uma projecao para o ano seguinte, assumindo um crescimento de 5% nos valores.
#'
#' @param df Um data frame contendo os dados contabeis. Deve conter colunas com valores numericos para diferentes anos, alem de uma coluna identificadora da natureza das contas (por exemplo, \code{"Categoria"} ou \code{"Conta"}).
#' @param tipo Um parametro do tipo \code{character} que indica a estrutura do data frame. Deve ser \code{"agregado"} quando os dados estao organizados por categorias (ex: ACO, ANC, PL etc.), ou outro valor (ex: \code{"detalhado"}) quando as contas individuais estao identificadas por uma coluna chamada \code{"Conta"}.
#'
#' @details
#' A Analise Vertical (AV) expressa cada item patrimonial como uma proporcao do total do ativo ou passivo correspondente no mesmo ano.
#'
#' A Analise Horizontal (AH) compara a evolucao dos valores ao longo dos anos, em relacao ao primeiro ano da base de dados (ano base).
#'
#' A funcao ainda projeta valores para o ano seguinte com base em um crescimento linear de 5% sobre os valores do ultimo ano disponivel.
#'
#' @return Uma lista com dois data frames:
#' \describe{
#'   \item{\code{AV_AH}}{Data frame contendo os valores originais, os resultados da Analise Vertical (com sufixo \code{_AV}) e da Analise Horizontal (com sufixo \code{_AH}).}
#'   \item{\code{Projecao}}{Data frame contendo a projecao de valores para o ano seguinte, com base em um crescimento de 5\%.}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate summarise select rename_with across filter relocate left_join
#' @importFrom dplyr where all_of ends_with everything
#' @importFrom data.table :=
#'
#' @examples
#' df <- data.frame(
#'   Conta = c(
#'     "Caixa e equivalentes de caixa",
#'     "Clientes",
#'     "Estoques",
#'     "Fornecedores",
#'     "Emprestimos e financiamentos"
#'   ),
#'   X2022 = c(1000, 2500, 1800, 2100, 1500),
#'   X2023 = c(1200, 2600, 1700, 2300, 1600)
#' )
#'
#' resultado <- padronizar_balanco(df)
#' av_ah <- calcular_AV_AH(resultado$agregado, tipo = "agregado")
#
#' @export
calcular_AV_AH <- function(df, tipo) {
  `%>%` <- magrittr::`%>%`

  # --- detectar coluna de categoria existente ---
  possiveis_cat <- c("categorias_bp", "Categorias", "Categoria", "categoria")
  cat_encontradas <- intersect(possiveis_cat, colnames(df))

  if (tipo == "agregado") {
    if (length(cat_encontradas) == 0) {
      stop("Nao foi encontrada coluna de categoria no data.frame (procure por 'categorias_bp', 'Categoria' ou 'Categorias').")
    }
    nome_coluna_categoria <- cat_encontradas[1]
  } else {
    # tipo detalhado: usar "Conta" (ou verificar existencia)
    if ("Conta" %in% colnames(df)) {
      nome_coluna_categoria <- "Conta"
    } else {
      stop("Para tipo != 'agregado' a coluna 'Conta' deve existir no data.frame.")
    }
  }

  # Garantir tipos corretos: transformar colunas que parecem numericas em numericas
  # (assume que padronizar_balanco ja fez a conversao; isto e apenas seguranca)
  df <- df %>%
    dplyr::mutate(dplyr::across(where(is.character), as.character),
                  dplyr::across(where(is.numeric), as.numeric))

  # Identificar colunas de anos: usar colunas numericas para maior robustez
  anos <- colnames(df)[sapply(df, is.numeric)]
  if (length(anos) == 0) stop("Nenhuma coluna numerica (anos) encontrada no data.frame.")

  # --- Totais para Analise Vertical (Ativo e Passivo) ---
  if (tipo == "agregado") {
    df_totais <- df %>%
      dplyr::filter(.data[[nome_coluna_categoria]] %in% c("ACO", "ACF", "ANC")) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(anos), sum, na.rm = TRUE)) %>%
      dplyr::rename_with(~ paste0(.x, "_Ativo_Total"))

    df_totais_passivo <- df %>%
      dplyr::filter(.data[[nome_coluna_categoria]] %in% c("PCO", "PCF", "PNC", "PL")) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(anos), sum, na.rm = TRUE)) %>%
      dplyr::rename_with(~ paste0(.x, "_Passivo_Total"))
  } else {
    df_totais <- df %>%
      dplyr::filter(tolower(.data[[nome_coluna_categoria]]) == "ativo total") %>%
      dplyr::select(dplyr::all_of(anos)) %>%
      dplyr::rename_with(~ paste0(.x, "_Ativo_Total"))

    df_totais_passivo <- df %>%
      dplyr::filter(tolower(.data[[nome_coluna_categoria]]) == "passivo total") %>%
      dplyr::select(dplyr::all_of(anos)) %>%
      dplyr::rename_with(~ paste0(.x, "_Passivo_Total"))
  }

  # Se os totais estiverem vazios, criar vetores zeros para evitar erros subsequentes
  if (nrow(df_totais) == 0) {
    df_totais <- as.data.frame(matrix(0, nrow = 1, ncol = length(anos)))
    colnames(df_totais) <- paste0(anos, "_Ativo_Total")
  }
  if (nrow(df_totais_passivo) == 0) {
    df_totais_passivo <- as.data.frame(matrix(0, nrow = 1, ncol = length(anos)))
    colnames(df_totais_passivo) <- paste0(anos, "_Passivo_Total")
  }

  # --- Analise Vertical (cria colunas com sufixo _AV) ---
  df_av <- df %>%
    # juntar os totais como colunas auxiliares (via join cartesiano)
    dplyr::left_join(df_totais, by = character()) %>%
    dplyr::left_join(df_totais_passivo, by = character()) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(anos),
      ~ ifelse(
        .data[[nome_coluna_categoria]] %in% c("ACO", "ACF", "ANC"),
        # dividir pelo total do ativo (coluna criada em df_totais)
        . / get(paste0(dplyr::cur_column(), "_Ativo_Total")),
        # caso contrario, dividir pelo total do passivo
        . / get(paste0(dplyr::cur_column(), "_Passivo_Total"))
      ),
      .names = "{.col}_AV"
    )) %>%
    dplyr::select(-dplyr::ends_with("_Ativo_Total"), -dplyr::ends_with("_Passivo_Total"))

  # --- Analise Horizontal (base no primeiro ano identificado) ---
  ano_base <- anos[1]
  df_ah <- df_av %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(anos), ~ . / get(ano_base), .names = "{.col}_AH"))

  # --- Projecao 5% sobre os valores do ultimo ano (mantendo linhas correspondentes) ---
  df_ano_seguinte <- df_ah %>%
    dplyr::select(dplyr::all_of(anos)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ . * 1.05))

  # adicionar coluna de categoria ao data.frame de projecao (mantendo correspondencia linha a linha)
  df_ano_seguinte <- df_ano_seguinte %>%
    dplyr::mutate(!!nome_coluna_categoria := paste0("Ano Seguinte_", df[[nome_coluna_categoria]])) %>%
    dplyr::relocate(!!nome_coluna_categoria, .before = 1)

  return(list(
    AV_AH = df_ah,
    Projecao = df_ano_seguinte
  ))
}

utils::globalVariables(c(".data"))
