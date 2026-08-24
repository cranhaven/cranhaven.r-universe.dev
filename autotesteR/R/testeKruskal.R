#' Teste de Kruskal-Wallis com pos-teste de Dunn
#'
#' Realiza o teste de Kruskal-Wallis para comparacao de multiplos grupos independentes,
#' seguido do pos-teste de Dunn com correcao de Bonferroni.
#'
#' @param ... Vetores numericos ou um data frame com duas ou mais colunas (cada uma representando um grupo)
#' @param titulo Titulo do grafico (padrao = "Kruskal-Wallis + Dunn")
#' @param x Nome do eixo x (padrao = "Grupo")
#' @param y Nome do eixo y (padrao = "Valor")
#' @param estilo Estetica do plot gerado pela funcao.
#' @param ajuda Se TRUE, exibe mensagem de ajuda
#' @param verbose Se TRUE, imprime mensagens detalhadas (padrao = TRUE)
#' @return Objeto do teste de Kruskal-Wallis
#' @export

teste.kruskal <- function(..., titulo = "Kruskal-Wallis + Dunn", x = "Grupo", y = "Valor", estilo = 1,
                          ajuda = FALSE, verbose = TRUE) {

  # Captura dos argumentos
  args <- list(...)

  # Permitir data frame como entrada
  if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) >= 2) {
    grupos <- lapply(args[[1]], function(col) col)
    nomes <- colnames(args[[1]])
  } else {
    grupos <- args
    nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
    nomes <- sub("^.*\\$", "", nomes_raw)
  }

  # Mensagem de ajuda
  if (ajuda || length(grupos) < 2) {
    message("
Funcao teste.kruskal()

Descricao:
  Teste de Kruskal-Wallis (ANOVA nao-parametrica) com pos-teste de Dunn.
  Ideal para comparar tres ou mais grupos independentes com dados nao-normais.

Exemplo:
  df <- data.frame(
    controle = c(5, 6, 7),
    tratamento = c(8, 9, 10),
    tratamento2 = c(2, 3, 4)
  )
  teste.kruskal(df)
")
    return(invisible(NULL))
  }

  # Pacotes necessarios
  pacotes <- c("ggplot2", "FSA", "dplyr", "multcompView")
  lapply(pacotes, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("O pacote '%s' nao esta instalado.", pkg), call. = FALSE)
    }
  })

  # Preparacao dos dados
  dados <- data.frame(
    valor = unlist(grupos),
    grupo = factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  )

  # Teste de Kruskal-Wallis
  kruskal <- stats::kruskal.test(valor ~ grupo, data = dados)
  p_kruskal <- kruskal$p.value
  p_label <- ifelse(p_kruskal < 0.001, "Kruskal-Wallis: p < 0.001",
                    paste0("Kruskal-Wallis: p = ", signif(p_kruskal, 3)))

  # Medias e desvios padrao — SEM PRINT
  medias_dp <- aggregate(valor ~ grupo, data = dados, function(x) c(media = mean(x), dp = sd(x)))
  medias_dp <- do.call(data.frame, medias_dp)
  colnames(medias_dp)[2:3] <- c("media", "dp")

  if (verbose) {
    sep <- paste0(rep("=", 50), collapse = "")
    message("Medias e desvios padrao por grupo")
    message(sep)
    print(medias_dp)
    message(sep)
  }

  # -----------------------------
  # Pos-teste de Dunn
  # -----------------------------
  suppressMessages({
    suppressWarnings({
      dunn_out <- FSA::dunnTest(valor ~ grupo, data = dados, method = "bonferroni")
    })
  })

  dunn_df <- dunn_out$res
  pares_signif <- subset(dunn_df, P.adj < 0.05)

  if (verbose) {
    if (nrow(pares_signif) == 0) {
      message("Nenhuma comparacao pos-hoc significativa (p < 0.05).")
    } else {
      msg <- paste0("(", pares_signif$Comparison, ", p = ", signif(pares_signif$P.adj, 3), ")")
      sep <- paste0(rep("=", 40), collapse = "")
      message("Pares significativos (Dunn, Bonferroni):")
      message(paste(msg, collapse = "\n"))
    }
  }

  # Letras de significancia
  comparacoes <- setNames(dunn_df$P.adj, gsub(" ", "", dunn_df$Comparison))
  letras_df <- multcompView::multcompLetters(comparacoes)$Letters
  letras_df <- data.frame(grupo = names(letras_df), letra = unname(letras_df))

  # Ajuste da posicao das letras
  maximos <- aggregate(valor ~ grupo, data = dados, max)
  letras_df <- merge(maximos, letras_df, by = "grupo")
  letras_df$valor <- letras_df$valor + 0.05 * max(letras_df$valor, na.rm = TRUE)

  # --- Preparacao de labels e cores ---
  y_pos <- max(dados$valor, na.rm = TRUE) + 0.1 * diff(range(dados$valor))
  cores_vivas <- RColorBrewer::brewer.pal(length(unique(dados$grupo)), "Set1")
  signif_label <- ifelse(p_kruskal < 0.001, "***", ifelse(p_kruskal < 0.01, "**", ifelse(p_kruskal < 0.05, "*", "")))

  # --------------------------
  # ESTILO 1: Boxplot + jitter
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

  # --------------------------
  # ESTILO 2: Violin + boxplot minimalista
  # --------------------------
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

  # --------------------------
  # ESTILO 3: Monocromático premium
  # --------------------------
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

  # --------------------------
  # ESTILO 4: Half-eye ggdist
  # --------------------------
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

  invisible(list(
    p_kruskal = p_kruskal,
    dunn = dunn_df,
    medias = medias_dp
  ))
}
