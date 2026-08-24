#' Funcao pre.teste() para sugestao de teste estatistico
#'
#' Identifica automaticamente se os dados sao numericos ou categoricos e sugere o teste estatistico mais adequado.
#'
#' @param ... Dois ou mais vetores (numericos ou categoricos)
#' @param alpha Nivel de significancia. Padrao = 0.05
#' @param ajuda Logico. Se TRUE, mostra ajuda detalhada
#' @param verbose Logico. Se TRUE, imprime mensagens informativas
#' @importFrom tidyselect everything
#' @return Lista invisivel com resultados dos testes de normalidade, homogeneidade ou tabela de contingencia e recomendacao do teste
#' @export

pre.teste <- function(..., alpha = 0.05, ajuda = FALSE, verbose = TRUE) {

  # Captura os argumentos
  args <- list(...)

  # Se for só uma dataframe com >= 2 colunas, extrai as colunas
  if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) >= 2) {
    grupos <- lapply(args[[1]], function(col) col)
    nomes <- colnames(args[[1]])
  } else {
    grupos <- args
    nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
    nomes <- sub("^.*\\$", "", nomes_raw)
  }

  if (ajuda || length(grupos) < 2) {
    if (verbose) {
      message("
Funcao pre.teste()

Descricao:
  Identifica automaticamente se os dados sao numericos ou categoricos e sugere o teste estatistico adequado.
  Pode receber diretamente uma dataframe com 2 ou mais colunas.

Exemplo com dataframe:
    df <- data.frame(controle = c(1,2,3,4,5),
                     tratamento = c(2,3,4,5,6))
    pre.teste(df)
")
    }
    return(invisible(NULL))
  }

  # Detectar tipo de dado
  sao_numericos <- all(sapply(grupos, is.numeric))
  sao_categoricos <- all(sapply(grupos, function(x) is.factor(x) || is.character(x)))

  if (!sao_numericos && !sao_categoricos) {
    stop("Os dados devem ser todos numericos ou todos categoricos.")
  }

  if (sao_numericos) {
    # --- DADOS NUMERICOS ---
    valores <- unlist(grupos)
    grupo <- factor(rep(nomes, times = sapply(grupos, length)))
    dados <- data.frame(valor = valores, grupo = grupo)

    aplicar_teste_normalidade <- function(x) {
      n <- length(x)
      if (n <= 50) {
        p <- stats::shapiro.test(x)$p.value
        metodo <- "Shapiro-Wilk"
      } else if (n <= 300) {
        p <- nortest::ad.test(x)$p.value
        metodo <- "Anderson-Darling"
      } else {
        ks_res <- stats::ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
        p <- ks_res$p.value
        metodo <- "Kolmogorov-Smirnov"
      }
      list(p = p, metodo = metodo)
    }

    normalidade <- lapply(grupos, aplicar_teste_normalidade)
    p_values <- sapply(normalidade, function(x) x$p)
    metodos <- sapply(normalidade, function(x) x$metodo)
    normalidade_df <- data.frame(
      grupo = nomes,
      n = sapply(grupos, length),
      metodo = metodos,
      p_valor = signif(p_values, 3),
      normal = p_values > alpha
    )

    # Teste de homogeneidade
    if (length(grupos) > 2) {
      p_levene <- car::leveneTest(valor ~ grupo, data = dados)$`Pr(>F)`[1]
    } else {
      p_levene <- stats::var.test(grupos[[1]], grupos[[2]])$p.value
    }
    homogeneo <- p_levene > alpha

    # Sugestao de teste
    if (length(grupos) == 2) {
      if (all(normalidade_df$normal)) {
        if (homogeneo) {
          recomendacao <- "teste t"
        } else {
          recomendacao <- "Welch's t-test"
        }
      } else {
        recomendacao <- "teste de Mann-Whitney"
      }
    } else { # mais de 2 grupos
      if (all(normalidade_df$normal) & homogeneo) {
        recomendacao <- "ANOVA com pos teste de Tukey"
      } else {
        recomendacao <- "Kruskal-Wallis com pos teste de Dunn"
      }
    }

    if (verbose) {

      sep <- paste0(rep("=", 50), collapse = "")
      message("Resultados dos testes de normalidade:")
      message(sep)
      print(normalidade_df)
      message(sep)
      message(sprintf("Homogeneidade de variancias: %s (p = %.3f)",
                      ifelse(homogeneo, "homogeneas", "heterogeneas"),
                      p_levene))
      message(sep)
      message(sprintf("Recomendacao de teste: %s", recomendacao))
      message(sep)
    }

    return(invisible(list(
      normalidade = normalidade_df,
      p_levene = p_levene,
      homogeneo = homogeneo,
      recomendacao = recomendacao
    )))
  }

  if (sao_categoricos) {
    # --- DADOS CATEGORICOS ---
    if (is.data.frame(args[[1]]) && ncol(args[[1]]) == 2) {
      df <- args[[1]]
      nomes <- colnames(df)

      # Reformata o data frame em formato longo
      df_long <- tidyr::pivot_longer(df,
                                     cols = everything(),
                                     names_to = "grupo",
                                     values_to = "categoria")

      tabela <- table(df_long$grupo, df_long$categoria)
    } else {
      categorias <- lapply(grupos, as.factor)
      tabela <- table(categorias[[1]], categorias[[2]])
    }

    recomendacao <- if (any(tabela < 5)) "Teste exato de Fisher" else "Teste do Qui-quadrado"

    if (verbose) {
      message("Tabela de contingencia:")
      print(tabela)
      message("Recomendacao do teste de associacao: ", recomendacao)
    }

    return(invisible(list(
      tabela = tabela,
      recomendacao = recomendacao
    )))
  }
}
