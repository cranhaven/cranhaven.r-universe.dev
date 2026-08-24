#' Teste t de Student (com diagnostico automatico)
#'
#' Executa o teste t para comparacao de medias entre dois grupos independentes,
#' com verificacao automatica de normalidade e homogeneidade de variancias.
#' Caso os pressupostos nao sejam atendidos, o teste de Mann-Whitney e aplicado
#' automaticamente (sem gerar grafico).
#'
#' @param ... Dois vetores numericos ou um data frame com exatamente duas colunas.
#' @param titulo Titulo do grafico (string). Default: "Teste t".
#' @param x Nome do eixo x no grafico (string). Default: "Grupo".
#' @param y Nome do eixo y no grafico (string). Default: "Valor".
#' @param estilo Estetica do plot gerado pela funcao.
#' @param ajuda Logico. Se TRUE, mostra explicacao detalhada da funcao. Default: FALSE.
#' @param verbose Se TRUE, imprime mensagens detalhadas. Default: TRUE.
#' @return Lista invisivel com resumo, resultado do teste, metodo e (opcionalmente) grafico.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   controle = rnorm(30, 10),
#'   tratamento = rnorm(30, 12)
#' )
#' teste.t(df)
teste.t <- function(..., titulo = "Teste t", x = "Grupo", y = "Valor",
                    ajuda = FALSE, verbose = TRUE, estilo = 1) {

  args <- list(...)

  # --- Ajuda rapida ---
  if (ajuda) {
    if (verbose) {
      message("
Funcao teste.t()

Descricao:
  Executa o teste t de Student para comparar as medias de dois grupos independentes,
  com verificacao automatica de normalidade (Shapiro-Wilk) e homogeneidade (Levene).

Se os pressupostos forem violados, o teste de Mann-Whitney e aplicado
automaticamente e o grafico nao e exibido.

Pode receber:
  - Dois vetores numericos (ex: grupo1, grupo2)
  - Um data frame com exatamente duas colunas numericas.

Exemplo:
  df <- data.frame(
    controle = rnorm(30, 10),
    tratamento = rnorm(30, 12)
  )
  teste.t(df)
")
    }
    return(invisible(NULL))
  }

  # --- Detecta se foi passado um data.frame ---
  if (length(args) == 1 && is.data.frame(args[[1]])) {
    df <- args[[1]]
    if (ncol(df) != 2) stop("O data frame deve conter exatamente duas colunas numericas.")
    grupos <- as.list(df)
    nomes <- colnames(df)
  } else {
    grupos <- args
    if (length(grupos) != 2) stop("Forneca exatamente dois grupos ou um data frame com duas colunas.")
    nomes <- sapply(substitute(list(...))[-1], deparse)
  }

  # --- Validacao ---
  if (!all(sapply(grupos, is.numeric))) stop("Ambos os grupos devem ser numericos.")
  if (any(sapply(grupos, function(g) sd(g, na.rm = TRUE) == 0))) {
    stop("Um dos grupos possui variancia zero (dados constantes).")
  }

  # --- Pacotes necessarios ---
  pacotes <- c("ggplot2", "dplyr", "car", "ggdist")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("O pacote '", pkg, "' nao esta instalado. Instale com install.packages('", pkg, "')"))
    }
  }

  # --- Monta dataframe long ---
  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)

  # --- Teste de normalidade (Shapiro-Wilk) ---
  normalidade <- sapply(grupos, function(g) {
    if (length(g) < 3) return(NA)
    stats::shapiro.test(g[!is.na(g)])$p.value
  })
  normal <- all(normalidade > 0.05, na.rm = TRUE)

  # --- Teste de homogeneidade (Levene) ---
  homogeneidade <- tryCatch(
    car::leveneTest(valor ~ grupo, data = dados, na.action = na.omit),
    error = function(e) data.frame(`Pr(>F)` = NA)
  )
  homo <- !is.na(homogeneidade$`Pr(>F)`[1]) && homogeneidade$`Pr(>F)`[1] > 0.05

  # --- Escolha do teste ---
  if (normal && homo) {
    resultado <- stats::t.test(grupos[[1]], grupos[[2]])
    metodo <- "t de Student"
    gera_plot <- TRUE
  } else {
    resultado <- stats::wilcox.test(grupos[[1]], grupos[[2]])
    metodo <- "Mann-Whitney (aplicado por violacao dos pressupostos)"
    gera_plot <- FALSE
  }

  pval <- resultado$p.value

  # --- Resumo estatistico ---
  resumo <- dados |>
    dplyr::group_by(grupo) |>
    dplyr::summarise(
      Media = round(mean(valor, na.rm = TRUE), 2),
      Desvio_Padrao = round(sd(valor, na.rm = TRUE), 2),
      .groups = "drop"
    )

  if (verbose) {
    message("\nResumo estatistico por grupo:")
    print(resumo)
  }

  # --- Geracao condicional do grafico ---
  if (gera_plot) {
    p_label <- if (pval < 0.001) "p < 0.001" else paste0("p = ", signif(pval, 3))
    signif_label <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else ""
    y_pos <- max(valores, na.rm = TRUE) + 0.1 * diff(range(valores, na.rm = TRUE))

    cores_vivas <- scales::hue_pal()(length(unique(dados$grupo)))

      # --------------------------
      # ESTILO 1 (boxplot + jitter)
      # --------------------------
    if (estilo == 1) {
      g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
        ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.1, alpha = 0.4, color = "black") +
        ggplot2::annotate("text", x = mean(1:2), y = y_pos, label = signif_label, size = 6) +
        ggplot2::theme_minimal() +
        ggplot2::scale_fill_brewer(palette = "Set1") +
        ggplot2::labs(
          title = paste0(titulo, " (", metodo, ")"),
          subtitle = p_label, x = "", y = y
        ) +
        ggplot2::theme(
          legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    }

    # --------------------------
    # ESTILO 2 (violin clean + boxplot minimalista)
    # --------------------------
    if (estilo == 2) {
      g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
        ggplot2::geom_violin(
          trim = FALSE,
          alpha = .55,
          color = NA,
          adjust = .6
        ) +
        ggplot2::geom_boxplot(
          width = .18,
          outlier.shape = NA,
          color = "gray20",
          linewidth = .4
        ) +
        ggplot2::geom_point(
          position = ggplot2::position_jitter(width = .1),
          alpha = .4,
          size = 1.8,
          color = "gray25"
        ) +
        ggplot2::annotate(
          "text",
          x = mean(1:2),
          y = y_pos,
          label = signif_label,
          size = 7
        ) +
        ggplot2::scale_fill_manual(values = cores_vivas) +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::labs(
          title = paste0(titulo, " (", metodo, ")"),
          subtitle = p_label,
          x = "",
          y = y
        ) +
        ggplot2::theme(
          legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    }


    # --------------------------
    # ESTILO 3 (monocromático premium)
    # --------------------------
    if (estilo == 3) {
      g <- ggplot2::ggplot(dados, ggplot2::aes(grupo, valor)) +
        ggplot2::geom_violin(fill = "gray85", color = NA) +
        ggplot2::geom_boxplot(width = .18, fill = "white") +
        ggplot2::geom_point(
          position = ggplot2::position_jitter(width = .1),
          color = "gray20", alpha = .4
        ) +
        ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 7) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(title = titulo, subtitle = p_label, x = NULL, y = y) +
        ggplot2::theme(legend.position = "none",
                       axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }

    # --------------------------
    # ESTILO 4 (ggdist half-eye + median point)
    # --------------------------
    if (estilo == 4) {
      g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
        ggdist::stat_halfeye(
          adjust = 0.6,
          width = 0.6,
          .width = c(0.5, 0.8, 0.95),
          justification = -0.2,
          slab_color = "gray20",
          interval_color = "gray20"
        ) +
        ggplot2::geom_point(
          position = ggplot2::position_nudge(x = 0.15),
          size = 1.1, alpha = 0.4, color = "black"
        ) +
        ggdist::stat_pointinterval(
          position = ggplot2::position_nudge(x = 0.2),
          point_color = "black",
          interval_color = "black",
          .width = 0.95
        ) +
        ggplot2::annotate("text", x = mean(1:2), y = y_pos,
                          label = signif_label, size = 6, fontface = "bold") +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::scale_fill_brewer(palette = "Set1") +
        ggplot2::labs(
          title = paste0(titulo, " (", metodo, ")"),
          subtitle = p_label, x = "", y = y
        ) +
        ggplot2::theme(
          legend.position = "none",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    }

    print(g)

  } else {
    if (verbose) {
      message("\nPressupostos violados: grafico nao gerado.")
    }
    g <- NULL
  }


  invisible(list(
    resumo = resumo,
    resultado = resultado,
    metodo = metodo,
    normalidade = normalidade,
    homogeneidade = homogeneidade,
    grafico = g
  ))
}


