#' Calcula indicadores financeiros a partir do Balanco Patrimonial e da Demonstracao do Resultado
#'
#' Esta funcao recebe dois data frames padronizados: um representando o Balanco Patrimonial
#' (bp) e outro representando a Demonstracao do Resultado (dre). A partir deles,
#' calcula indicadores classicos de liquidez, estrutura de capital, margens operacionais,
#' eficiencia e rentabilidade.
#'
#' Caso a DRE contenha as categorias LUCRO_BRUTO, DESPESAS_OPERACIONAIS e
#' RESULTADO_FINANCEIRO, a funcao calcula automaticamente o EBIT, utilizado nos indicadores
#' de margem operacional e na razao EBIT/Ativo.
#'
#' @param bp Um data.frame contendo o Balanco Patrimonial, com uma coluna Categoria e
#' colunas adicionais representando anos. As categorias devem seguir o padrao:
#' - ACO: Ativo Circulante Operacional
#' - ACF: Ativo Circulante Financeiro
#' - PCO: Passivo Circulante Operacional
#' - PCF: Passivo Circulante Financeiro
#' - ANC: Ativo Nao Circulante
#' - PNC: Passivo Nao Circulante
#' - PL: Patrimonio Liquido
#'
#' @param dre Um data.frame contendo a Demonstracao do Resultado, tambem com coluna
#' Categoria e colunas representando anos. As principais categorias esperadas incluem:
#' - RECEITA_LIQUIDA
#' - CUSTO_BENS_SERVICOS
#' - DESPESAS_OPERACIONAIS
#' - RESULTADO_FINANCEIRO
#' - RESULTADO_LIQUIDO
#'
#' @return Uma lista contendo tres data.frames:
#' - indicadores_bp: Liquidez e estrutura de capital
#' - indicadores_dre: Margens operacionais
#' - indicadores_conjuntos: Indicadores integrados (inclui DuPont)
#'
#' @details Indicadores calculados:
#'
#' Balanco Patrimonial:
#' - Liquidez Corrente = (ACO + ACF) / (PCO + PCF)
#' - Liquidez Seca = ACO / (PCO + PCF)
#' - Liquidez Imediata = ACF / (PCO + PCF)
#' - Endividamento Geral = (PCO + PCF + PNC) / (ACO + ACF + ANC)
#' - Composicao do Endividamento = (PCO + PCF) / (PCO + PCF + PNC)
#' - Imobilizacao do PL = ANC / PL
#'
#' Demonstracao do Resultado:
#' - Margem Bruta = (RECEITA_LIQUIDA - CUSTO_BENS_SERVICOS) / RECEITA_LIQUIDA
#' - Margem Operacional = EBIT / RECEITA_LIQUIDA
#' - Margem Liquida = RESULTADO_LIQUIDO / RECEITA_LIQUIDA
#'
#' Indicadores Integrados (DuPont e rentabilidade):
#' - ROA = RESULTADO_LIQUIDO / Ativo Total
#' - ROE = RESULTADO_LIQUIDO / PL
#' - Giro do Ativo = RECEITA_LIQUIDA / Ativo Total
#' - Alavancagem Financeira = Ativo Total / PL
#' - EBIT/Ativo = EBIT / Ativo Total
#'
#' @examples
#' # Exemplo de dados padronizados para o Balanço Patrimonial
#' bp <- data.frame(
#'   Categoria = c("ACO", "ACF", "PCO", "PCF", "ANC", "PNC", "PL"),
#'   `2023` = c(50000, 20000, 30000, 10000, 80000, 40000, 90000),
#'   `2024` = c(54000, 22000, 32000, 12000, 82000, 45000, 92000),
#'   check.names = FALSE
#' )
#'
#' # Exemplo de dados padronizados para a Demonstração do Resultado
#' dre <- data.frame(
#'   Categoria = c(
#'     "RECEITA_LIQUIDA",
#'     "CUSTO_BENS_SERVICOS",
#'     "DESPESAS_OPERACIONAIS",
#'     "RESULTADO_FINANCEIRO",
#'     "RESULTADO_LIQUIDO"
#'   ),
#'   `2023` = c(150000, 90000, 20000, -5000, 25000),
#'   `2024` = c(160000, 95000, 21000, -6000, 27000),
#'   check.names = FALSE
#' )
#'
#' # Cálculo dos indicadores
#' resultado <- indicadores(bp, dre)
#'
#' # Visualizando cada conjunto de resultados
#' resultado$indicadores_bp        # Liquidez, estrutura de capital
#' resultado$indicadores_dre       # Margens operacionais e líquidas
#' resultado$indicadores_conjuntos # ROA, ROE, DuPont etc.
#'
#' @export
indicadores <- function(bp = NULL, dre = NULL) {

  get_cat_val <- function(df, categoria, ano) {
    if (is.null(df)) return(NA_real_)
    if (!("Categoria" %in% colnames(df))) return(NA_real_)
    if (!(categoria %in% df$Categoria)) return(NA_real_)
    vals <- df[df$Categoria == categoria, ano]
    if (length(vals) == 0) return(NA_real_)
    return(sum(as.numeric(vals), na.rm = TRUE))
  }

  safe_div <- function(a, b) {
    a <- as.numeric(a); b <- as.numeric(b)
    if (is.na(b) || b == 0) return(NA_real_)
    return(a / b)
  }

  # Ajuste automatico do EBIT
  if (!is.null(dre)) {
    if (!all(c("LUCRO_BRUTO", "DESPESAS_OPERACIONAIS", "RESULTADO_FINANCEIRO") %in% dre$Categoria)) {
      warning("Para calcular o EBIT, as categorias LUCRO_BRUTO, DESPESAS_OPERACIONAIS e RESULTADO_FINANCEIRO devem existir na DRE.")
    } else {
      anos_dre <- colnames(dre)[colnames(dre) != "Categoria"]
      ebit_vals <- dre[dre$Categoria == "LUCRO_BRUTO", anos_dre] -
        dre[dre$Categoria == "DESPESAS_OPERACIONAIS", anos_dre] +
        dre[dre$Categoria == "RESULTADO_FINANCEIRO", anos_dre]

      dre <- rbind(
        dre,
        cbind(Categoria = "EBIT", ebit_vals)
      )
    }
  }

  # Indicadores BP
  indicadores_bp <- NULL
  if (!is.null(bp)) {
    anos_bp <- colnames(bp)[colnames(bp) != "Categoria"]
    indicadores_bp <- data.frame(Indicador = c(
      "Liquidez Corrente", "Liquidez Seca", "Liquidez Imediata",
      "Endividamento Geral", "Composicao do Endividamento", "Imobilizacao do PL"
    ), stringsAsFactors = FALSE)

    for (ano in anos_bp) {
      aco <- get_cat_val(bp, "ACO", ano)
      acf <- get_cat_val(bp, "ACF", ano)
      pco <- get_cat_val(bp, "PCO", ano)
      pcf <- get_cat_val(bp, "PCF", ano)
      anc <- get_cat_val(bp, "ANC", ano)
      pnc <- get_cat_val(bp, "PNC", ano)
      pl  <- get_cat_val(bp, "PL", ano)

      indicadores_bp[[ano]] <- c(
        safe_div(aco + acf, pco + pcf),
        safe_div(aco, pco + pcf),
        safe_div(acf, pco + pcf),
        safe_div(pco + pcf + pnc, aco + acf + anc),
        safe_div(pco + pcf, pco + pcf + pnc),
        safe_div(anc, pl)
      )
    }
  }

  # Indicadores DRE
  indicadores_dre <- NULL
  if (!is.null(dre)) {
    anos <- colnames(dre)[colnames(dre) != "Categoria"]
    indicadores_dre <- data.frame(
      Indicador = c("Margem Bruta", "Margem Operacional", "Margem Liquida"),
      stringsAsFactors = FALSE
    )

    for (ano in anos) {
      receita <- get_cat_val(dre, "RECEITA_LIQUIDA", ano)
      custo   <- get_cat_val(dre, "CUSTO_BENS_SERVICOS", ano)
      lucro   <- get_cat_val(dre, "RESULTADO_LIQUIDO", ano)
      ebit    <- get_cat_val(dre, "EBIT", ano)

      indicadores_dre[[ano]] <- c(
        safe_div(receita - custo, receita),
        safe_div(ebit, receita),
        safe_div(lucro, receita)
      )
    }
  }

  # Indicadores integrados DuPont
  indicadores_conjuntos <- NULL
  if (!is.null(bp) && !is.null(dre)) {
    anos <- intersect(colnames(bp)[-1], colnames(dre)[-1])
    indicadores_conjuntos <- data.frame(
      Indicador = c("ROA", "ROE", "Giro do Ativo", "Alavancagem Financeira", "EBIT_Ativo"),
      stringsAsFactors = FALSE
    )

    for (ano in anos) {
      ativo <- get_cat_val(bp, "ACO", ano) + get_cat_val(bp, "ACF", ano) + get_cat_val(bp, "ANC", ano)
      pl <- get_cat_val(bp, "PL", ano)
      receita <- get_cat_val(dre, "RECEITA_LIQUIDA", ano)
      lucro <- get_cat_val(dre, "RESULTADO_LIQUIDO", ano)
      ebit <- get_cat_val(dre, "EBIT", ano)

      indicadores_conjuntos[[ano]] <- c(
        safe_div(lucro, ativo),
        safe_div(lucro, pl),
        safe_div(receita, ativo),
        safe_div(ativo, pl),
        safe_div(ebit, ativo)
      )
    }
  }

  return(list(
    indicadores_bp = indicadores_bp,
    indicadores_dre = indicadores_dre,
    indicadores_conjuntos = indicadores_conjuntos
  ))
}
