#' Teste de ANOVA com checagem automatica de pressupostos
#'
#' Realiza ANOVA (e Tukey HSD) se os dados atenderem aos pressupostos de normalidade e homogeneidade.
#' Caso contrario, recomenda automaticamente o uso de Kruskal-Wallis/Dunn.
#'
#' @param ... Vetores ou um data.frame com >= 2 colunas.
#' @param titulo Titulo do grafico.
#' @param x Rotulo do eixo X.
#' @param y Rotulo do eixo Y.
#' @param estilo Estetica do plot gerado pela funcao.
#' @param ajuda Se TRUE, mostra a ajuda.
#' @param verbose Se TRUE, mostra mensagens detalhadas.
#' @importFrom stats aov sd aggregate shapiro.test var.test
#' @return Objeto `aov` ou mensagem de recomendacao.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   controle = rnorm(30, 10),
#'   tratamento = rnorm(30, 12),
#'   teste = rnorm(30, 11)
#' )
#' teste.anova(df)

teste.anova <- function(..., titulo = "ANOVA/Tukey HSD", x = "Eixo x", y = "Eixo y",
                        estilo = 1, ajuda = FALSE, verbose = TRUE) {

  # --- Bloco de ajuda rapida ---
  if (ajuda || length(list(...)) == 0) {
    message("
Funcao teste.anova()

Descricao:
  Executa ANOVA entre grupos numericos, seguida do teste de Tukey HSD,
  caso os pressupostos de normalidade e homogeneidade sejam atendidos.
  Caso contrario, recomenda o uso de Kruskal-Wallis/Dunn.

Exemplo:
  df <- data.frame(
  controle = rnorm(30, 10, sd = 1),
  tratamento = rnorm(30, 12, sd = 1),
  teste = rnorm(30, 11, sd = 1)
  )

teste.anova(df)
")
    return(invisible(NULL))
  }

  pacotes_necessarios <- c("ggplot2", "multcompView", "car")
  for (pkg in pacotes_necessarios) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Instale o pacote: ", pkg))
    }
  }

  dots <- list(...)
  if (length(dots) == 1 && is.data.frame(dots[[1]])) {
    df <- dots[[1]]
    grupos <- as.list(df)
    nomes <- names(df)
  } else {
    grupos <- dots
    nomes <- sapply(substitute(list(...))[-1], deparse)
  }

  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)))
  dados <- data.frame(valor = valores, grupo = grupo)

  aplicar_teste_normalidade <- function(x) {
    if (length(x) < 3) NA else shapiro.test(x)$p.value
  }
  p_normal <- sapply(grupos, aplicar_teste_normalidade)
  normal <- all(p_normal > 0.05, na.rm = TRUE)

  p_levene <- tryCatch({
    if (length(grupos) > 2) {
      car::leveneTest(valor ~ grupo, data = dados)$`Pr(>F)`[1]
    } else {
      var.test(grupos[[1]], grupos[[2]])$p.value
    }
  }, error = function(e) NA)
  homogeneo <- !is.na(p_levene) && p_levene > 0.05

  # -------------------------
  # Se pressupostos falham
  # -------------------------
  if (!normal || !homogeneo) {
    if (verbose) message("\nPressupostos falharam. Sugestao: Kruskal-Wallis/Dunn")

    medias_dp <- aggregate(valor ~ grupo, data = dados,
                           function(x) c(media = mean(x), dp = sd(x)))
    medias_dp <- do.call(data.frame, medias_dp)
    colnames(medias_dp)[2:3] <- c("media", "dp")

    return(invisible(list(
      tipo = "ANOVA - pressupostos nao atendidos",
      normal = normal,
      p_normal = p_normal,
      homogeneo = homogeneo,
      p_levene = p_levene,
      recomendacao = "Use Kruskal-Wallis/Dunn",
      medias_dp = medias_dp
    )))
  }

  # -------------------------
  # ANOVA
  # -------------------------
  modelo <- aov(valor ~ grupo, data = dados)
  p_anova <- summary(modelo)[[1]][["Pr(>F)"]][1]

  # rotulo automatico para o grafico
  p_label <- paste0("ANOVA: p = ", signif(p_anova, 3))

  # Tabela Tukey
  tukey_res <- TukeyHSD(modelo)$grupo
  tukey_df <- as.data.frame(tukey_res)

  # Pares significativos do Tukey (p < 0.05)
  tukey_pairs <- data.frame(
    Comparison = rownames(tukey_res),
    diff = tukey_res[, "diff"],
    lwr  = tukey_res[, "lwr"],
    upr  = tukey_res[, "upr"],
    p_adj = tukey_res[, "p adj"],
    stringsAsFactors = FALSE
  )

  pares_signif <- subset(tukey_pairs, p_adj < 0.05)

  tukey_df$Comparacao <- rownames(tukey_res)
  rownames(tukey_df) <- NULL

  # Letras de significancia
  letras <- multcompView::multcompLetters4(modelo, TukeyHSD(modelo))
  letras_df <- data.frame(
    grupo = names(letras$grupo$Letters),
    letra = letras$grupo$Letters,
    stringsAsFactors = FALSE
  )

  # calcular posicao vertical para cada letra
  max_df <- aggregate(valor ~ grupo, data = dados, max)
  letras_df <- merge(letras_df, max_df, by = "grupo")
  letras_df$valor <- letras_df$valor * 1.05

  # Medias e desvios
  medias_dp <- aggregate(valor ~ grupo, data = dados,
                         function(x) c(media = mean(x), dp = sd(x)))
  medias_dp <- do.call(data.frame, medias_dp)
  colnames(medias_dp)[2:3] <- c("media", "dp")

  # Impressao amigavel
  if (verbose) {
    sep <- paste0(rep("=", 40), collapse = "")
    message("\nResumo dos grupos (media / dp)")
    message(sep)
    print(
      data.frame(
        Grupo = medias_dp$grupo,
        Media = round(medias_dp$media, 3),
        DP = round(medias_dp$dp, 3)))
    message(sep)

    # imprimir pares significativos depois do resumo
    if (nrow(pares_signif) == 0) {
      message("Nenhuma comparacao pos-hoc significativa (Tukey, p < 0.05).")
    } else {
      message("\nPares significativos (Tukey HSD):")
      for (i in seq_len(nrow(pares_signif))) {
        linha <- pares_signif[i, ]
        label_comp <- gsub("\\s*\\-\\s*", "-", linha$Comparison)
        label_comp <- gsub("-", " - ", label_comp)
        message(sprintf("(%s, p = %s)",
                        label_comp,
                        signif(linha$p_adj, 3)))
      }
    }
  }

  cores_vivas <- scales::hue_pal()(length(unique(dados$grupo)))

  # --------------------------
  # Estilos de grafico
  # --------------------------
  if (estilo == 1) {
    g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
      ggplot2::geom_text(data = letras_df, ggplot2::aes(x = grupo, y = valor, label = letra),
                         size = 4, vjust = 0) +
      ggplot2::labs(title = titulo, subtitle = p_label, x = "", y = y) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  if (estilo == 2) {
    g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
      ggplot2::geom_violin(trim = FALSE, alpha = 0.55, color = NA, adjust = 0.6) +
      ggplot2::geom_boxplot(width = 0.18, outlier.shape = NA, color = "gray20", linewidth = 0.4) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.1), alpha = 0.4, size = 1.8, color = "gray25") +
      ggplot2::geom_text(data = letras_df, ggplot2::aes(x = grupo, y = valor, label = letra),
                         size = 4, vjust = 0) +
      ggplot2::labs(title = titulo, subtitle = p_label, x = "", y = y) +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  if (estilo == 3) {
    g <- ggplot2::ggplot(dados, ggplot2::aes(grupo, valor)) +
      ggplot2::geom_violin(fill = "gray85", color = NA) +
      ggplot2::geom_boxplot(width = 0.18, fill = "white") +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.1), color = "gray20", alpha = 0.4) +
      ggplot2::geom_text(data = letras_df, ggplot2::aes(x = grupo, y = valor, label = letra),
                         size = 4, vjust = 0) +
      ggplot2::labs(title = titulo, subtitle = p_label, x = "", y = y) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  if (estilo == 4) {
    if (!requireNamespace("ggdist", quietly = TRUE)) {
      stop("Para o estilo 4, instale o pacote 'ggdist'")
    }
    g <- ggplot2::ggplot(dados, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
      ggdist::stat_halfeye(adjust = 0.6, width = 0.6, .width = c(0.5, 0.8, 0.95),
                           justification = -0.2, slab_color = "gray20", interval_color = "gray20") +
      ggplot2::geom_point(position = ggplot2::position_nudge(x = 0.15), size = 1.1, alpha = 0.4, color = "black") +
      ggdist::stat_pointinterval(position = ggplot2::position_nudge(x = 0.2),
                                 point_color = "black", interval_color = "black", .width = 0.95) +
      ggplot2::geom_text(data = letras_df, ggplot2::aes(x = grupo, y = valor, label = letra),
                         size = 4, vjust = 0) +
      ggplot2::labs(title = titulo, subtitle = p_label, x = "", y = y) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  print(g)
  return(invisible(list(
    tipo = "ANOVA",
    p_anova = p_anova,
    normal = normal,
    p_normal = p_normal,
    homogeneo = homogeneo,
    p_levene = p_levene,
    medias_dp = medias_dp,
    tukey = tukey_df,
    pares_signif = pares_signif,
    letras = letras_df,
    modelo = modelo
  )))
}
