#' Testes multiplos entre grupos numericos (t ou Mann-Whitney), com diagnostico automatico e grafico anotado
#'
#' Executa comparacoes multiplas entre grupos numericos (como colunas de um
#' data frame ou vetores nomeados) usando teste t ou Mann-Whitney (MW),
#' escolhidos automaticamente com base na normalidade (Shapiro-Wilk) e
#' homogeneidade de variancias (teste de Levene).
#'
#' A funcao tambem:
#' - aceita diferentes formatos de entrada para definir as comparacoes
#'   (lista de pares, vetores de nomes, strings, etc.);
#' - imprime um resumo (medias/DP) e destaca comparacoes significativas;
#' - gera graficos com barras de significancia e quatro estilos visuais personalizaveis;
#' - retorna resultados completos para uso programatico.
#'
#' @param ... Data frame contendo apenas colunas numericas, ou varios vetores
#'   numericos nomeados (cada vetor corresponde a um grupo).
#' @param comparacoes Formato flexivel. Pode ser:
#'   * `NULL` (padrao), testando todas as comparacoes 2 a 2;
#'   * `list(c(\"A\",\"B\"), c(\"C\",\"D\"))`;
#'   * vetor de nomes com tamanho par, ex.: `c(\"A\",\"B\",\"C\",\"D\")`;
#'   * par unico, ex.: `c(\"A\",\"B\")`;
#'   * string como `"A,B"` ou `"A - B"`.
#' @param titulo Titulo do grafico.
#' @param x Rotulo do eixo x.
#' @param y Rotulo do eixo y.
#' @param estilo Inteiro 1-4 definindo o estilo visual do grafico.
#'   1 = boxplot;
#'   2 = violino + boxplot;
#'   3 = violino minimalista;
#'   4 = half-eye (requer ggdist).
#' @param verbose Se `TRUE` (padrao), imprime mensagens e resumo estatistico.
#' @param ajuda mostrar ajuda rapida
#' @return Uma lista invisivel com:
#'   - `resultados`: tibble com grupo1, grupo2, metodo (t/MW) e p-valor;
#'   - `pares_signif`: subconjunto significativo (p < 0.05);
#'   - `grafico`: objeto ggplot final;
#'   - `dados_long`: dados reorganizados em formato longo.
#'
#' @examples
#' df <- data.frame(
#'   controle   = rnorm(30, 10),
#'   tratamento = rnorm(30, 12),
#'   teste1     = rnorm(30, 11),
#'   teste2     = rnorm(30, 15)
#' )
#'
#' teste.tmulti(df)
#'
#' teste.tmulti(df,
#'   comparacoes = list(c("controle", "tratamento"),
#'                      c("tratamento", "teste1"))
#' )
#'
#' @export

teste.tmulti <- function(...,
                         comparacoes = NULL,
                         titulo = "Comparacoes multiplas (t / MW)",
                         x = "",
                         y = "Valor",
                         estilo = 1,
                         ajuda = FALSE,
                         verbose = TRUE) {

  # --- Ajuda interna rapida ---
  if (ajuda || length(list(...)) == 0) {
    message("
Funcao teste.tmulti()

Descricao:
  Executa comparacoes multiplas entre grupos numericos usando teste t ou
  Mann-Whitney (MW), escolhidos automaticamente conforme normalidade e
  homogeneidade. Gera grafico com barras e estrelas de significancia.

Exemplo:
  df <- data.frame(
    controle   = rnorm(30, 10),
    tratamento = rnorm(30, 12),
    teste1     = rnorm(30, 11),
    teste2     = rnorm(30, 15)
  )

  teste.tmulti(df)

  teste.tmulti(df,
  comparacoes = list(c('controle', 'tratamento'),
  c('tratamento', 'teste1'))
")
    return(invisible(NULL))
  }

  # ------------------------------
  # checar pacotes minimos
  # ------------------------------
  pacotes_req <- c("ggplot2", "purrr", "tibble", "tidyr", "dplyr", "scales", "car")
  for (pkg in pacotes_req) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Instale o pacote '%s' para usar teste.tmulti()", pkg), call. = FALSE)
    }
  }
  # ggdist usado apenas para estilo 4 (opcional)
  tem_ggdist <- requireNamespace("ggdist", quietly = TRUE)

  # ------------------------------
  # capturar entrada (df ou vetores)
  # ------------------------------
  dots <- list(...)
  if (length(dots) == 0) stop("Forneca um data.frame ou vetores nomeados como argumentos.")
  if (length(dots) == 1 && is.data.frame(dots[[1]])) {
    df <- as.data.frame(dots[[1]])
    grupos_ordem <- colnames(df)
  } else {
    # vetores separados -> transformar em data.frame mantendo nomes passados
    grupos <- dots
    nomes <- sapply(substitute(list(...))[-1], deparse)
    df <- as.data.frame(grupos, stringsAsFactors = FALSE)
    names(df) <- nomes
    grupos_ordem <- nomes
  }

  if (!all(sapply(df, is.numeric))) stop("Todas as colunas do data frame precisam ser numericas.")

  # ------------------------------
  # normalizar 'comparacoes' em lista de pares
  # aceita:
  # - NULL -> todas vs todas
  # - list(c("A","B"), c("C","D"))
  # - c("A","B") -> Unica comparacaoo
  # - c("A","B","C","D") -> pares consecutivos (A,B) e (C,D)
  # - "A,B" ou "A - B" -> string convertida
  # - list("A","B","C","D") -> convertida para pares
  # ------------------------------
  to_pairs <- function(cmp) {
    # verificar se e uma lista de pares
    if (is.list(cmp) && all(sapply(cmp, function(x) is.character(x) && length(x) == 2))) {
      return(cmp)
    }
    # tamanho do vetor de caracteres 2 -> par simples
    if (is.character(cmp) && length(cmp) == 2) return(list(cmp))
    # string simples como "A,B" or "A - B"
    if (is.character(cmp) && length(cmp) == 1) {
      if (grepl("[-,;]", cmp)) {
        parts <- trimws(unlist(strsplit(cmp, "[-,;]+")))
        if (length(parts) == 2) return(list(parts))
      }
    }
    # vetor de caracteres com o mesmo tamanho -> divisao em pares
    if (is.character(cmp) && length(cmp) > 2) {
      if (length(cmp) %% 2 == 0) {
        n_pairs <- length(cmp) / 2
        idx <- rep(1:n_pairs, each = 2)
        m <- split(cmp, idx)
        return(m)
      } else stop("Quando passar um vetor de nomes em 'comparacoes' (caracter), o numero de elementos deve ser par.")
    }
    # lista de nomes simples -> coercao se tiver o mesmo tamanho
    if (is.list(cmp) && all(sapply(cmp, is.character)) && !any(sapply(cmp, length) > 1)) {
      flat <- unlist(cmp)
      if (length(flat) %% 2 == 0) {
        n_pairs <- length(flat)/2
        idx <- rep(1:n_pairs, each = 2)
        return(split(flat, idx))
      } else stop("Lista em 'comparacoes' tem numero impar de nomes; precisa ser par.")
    }
    # fallback: tentativa de coercao se ja e uma lista, mas com forma misturadas: manter sublistas de tamanho 2
    if (is.list(cmp)) {
      good <- Filter(function(x) is.character(x) && length(x) == 2, cmp)
      if (length(good) > 0) return(good)
    }
    stop("Formato de 'comparacoes' nao reconhecido. Use list(c('A','B'), c('C','D')) ou vetores pares.")
  }

  if (is.null(comparacoes)) {
    comb <- combn(grupos_ordem, 2)
    comparacoes <- split(t(comb), 1:ncol(comb))
  } else {
    comparacoes <- to_pairs(comparacoes)
  }

  # ------------------------------
  # verificar se nomes existem no df
  # ------------------------------
  todos_nomes <- unique(unlist(comparacoes))
  faltantes <- setdiff(todos_nomes, names(df))
  if (length(faltantes) > 0) {
    stop(sprintf("As seguintes variaveis nao existem no dataframe: %s", paste(faltantes, collapse = ", ")))
  }

  # ------------------------------
  # montar dados long mantendo ordem original
  # ------------------------------
  dados_long <- tibble::as_tibble(df)
  dados_long <- tidyr::pivot_longer(
    dados_long,
    cols = tidyselect::everything(),
    names_to = "grupo",
    values_to = "valor"
  )
  dados_long$grupo <- factor(dados_long$grupo, levels = grupos_ordem)

  # ------------------------------
  # funcao de teste para um par
  # ------------------------------
  testar_par <- function(pares) {
    g1 <- pares[1]; g2 <- pares[2]
    v1 <- df[[g1]]; v2 <- df[[g2]]

    # normalidade (se possivel)
    p_norm1 <- if (length(na.omit(v1)) >= 3) tryCatch(stats::shapiro.test(na.omit(v1))$p.value, error = function(e) NA) else NA
    p_norm2 <- if (length(na.omit(v2)) >= 3) tryCatch(stats::shapiro.test(na.omit(v2))$p.value, error = function(e) NA) else NA
    normal <- all(c(p_norm1, p_norm2) > 0.05, na.rm = TRUE)

    # homogeneidade por Levene (usa subset long)
    lev <- tryCatch({
      car::leveneTest(valor ~ grupo, data = dados_long[dados_long$grupo %in% c(g1, g2), ])
    }, error = function(e) NA)
    homo <- if (is.list(lev) && "Pr(>F)" %in% colnames(lev)) {
      lev$`Pr(>F)`[1] > 0.05
    } else {
      FALSE
    }

    # escolher teste
    if (normal && homo) {
      res <- tryCatch(t.test(v1, v2), error = function(e) NA)
      metodo <- "t"
      pval <- if (is.list(res)) res$p.value else NA
    } else {
      res <- tryCatch(wilcox.test(v1, v2), error = function(e) NA)
      metodo <- "MW"
      pval <- if (is.list(res)) res$p.value else NA
    }

    tibble::tibble(grupo1 = g1, grupo2 = g2, metodo = metodo, p = as.numeric(pval))
  }

  # ------------------------------
  # rodar testes (seguro)
  # ------------------------------
  resultados <- purrr::map_dfr(comparacoes, testar_par)

  # ------------------------------
  # resumo (media e dp) e impressao
  # ------------------------------
  medias_dp <- aggregate(valor ~ grupo, data = dados_long, function(x) c(media = mean(x, na.rm = TRUE), dp = sd(x, na.rm = TRUE)))
  medias_dp <- do.call(data.frame, medias_dp)
  colnames(medias_dp)[2:3] <- c("media", "dp")

  if (verbose) {
    sep <- paste0(rep("=", 50), collapse = "")
    message("\nResumo dos grupos (media / dp)")
    message(sep)
    print(data.frame(Grupo = medias_dp$grupo, Media = round(medias_dp$media, 3), DP = round(medias_dp$dp, 3)))
    message(sep)

    # pares significativos depois do resumo
    pares_signif <- resultados[resultados$p < 0.05 & !is.na(resultados$p), , drop = FALSE]
    if (nrow(pares_signif) == 0) {
      message("Nenhuma comparacao pos-hoc significativa (p < 0.05).")
    } else {
      message("\nPares significativos (p < 0.05):")
      apply(pares_signif, 1, function(r) {
        cat(sprintf("(%s - %s, p = %s)\n", r["grupo1"], r["grupo2"], signif(as.numeric(r["p"]), 3)))
      })
    }
  }

  # ------------------------------
  # preparar plot base e cores
  # ------------------------------
  cores_vivas <- scales::hue_pal()(length(grupos_ordem))

  # funcao auxiliar para desenhar as barras + estrelas
  add_sig_bars <- function(plot_obj, res_df) {
    ymax <- max(dados_long$valor, na.rm = TRUE)
    step <- 0.07 * diff(range(dados_long$valor, na.rm = TRUE))
    if (step == 0 || is.na(step)) step <- 0.1 * abs(ymax + 1)

    # ordenar para que barras nao se sobreponham mal (mantem ordem de input 'resultados')
    for (i in seq_len(nrow(res_df))) {
      r <- res_df[i, ]
      g1 <- r$grupo1; g2 <- r$grupo2; pval <- r$p
      label <- if (is.na(pval)) "na" else if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else "ns"
      x1 <- which(grupos_ordem == g1)
      x2 <- which(grupos_ordem == g2)
      y_pos <- ymax + i * step

      plot_obj <- plot_obj +
        ggplot2::annotate("segment", x = x1, xend = x2, y = y_pos, yend = y_pos, linewidth = 0.5) +
        ggplot2::annotate("segment", x = x1, xend = x1, y = y_pos, yend = y_pos - 0.02 * diff(range(dados_long$valor, na.rm = TRUE))) +
        ggplot2::annotate("segment", x = x2, xend = x2, y = y_pos, yend = y_pos - 0.02 * diff(range(dados_long$valor, na.rm = TRUE))) +
        ggplot2::annotate("text", x = mean(c(x1, x2)), y = y_pos + 0.02 * diff(range(dados_long$valor, na.rm = TRUE)), label = label, size = 5)
    }
    plot_obj
  }

  # ------------------------------
  # construir grafico segundo estilo
  # ------------------------------
  if (estilo == 1) {
    g <- ggplot2::ggplot(dados_long, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.12, alpha = 0.45, color = "black", size = 1.8) +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = titulo, x = x, y = y) +
      ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else if (estilo == 2) {
    g <- ggplot2::ggplot(dados_long, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
      ggplot2::geom_violin(trim = FALSE, alpha = 0.55, color = NA, adjust = 0.6) +
      ggplot2::geom_boxplot(width = 0.16, outlier.shape = NA, color = "gray20", linewidth = 0.35) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.12), alpha = 0.4, size = 1.6, color = "gray25") +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = titulo, x = "", y = y) +
      ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else if (estilo == 3) {
    g <- ggplot2::ggplot(dados_long, ggplot2::aes(x = grupo, y = valor)) +
      ggplot2::geom_violin(fill = "gray90", color = NA) +
      ggplot2::geom_boxplot(width = 0.16, fill = "white", outlier.shape = NA) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.12), color = "gray20", alpha = 0.45, size = 1.6) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = titulo, x = "", y = y) +
      ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else if (estilo == 4) {
    if (!tem_ggdist) {
      warning("Pacote 'ggdist' nao encontrado - estilo 4 requer 'ggdist'. Usando estilo 1 como fallback.")
      g <- ggplot2::ggplot(dados_long, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
        ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.12, alpha = 0.45, color = "black", size = 1.8) +
        ggplot2::scale_fill_manual(values = cores_vivas) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(title = titulo, x = "", y = y) +
        ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    } else {
      # ggdist half-eye style
      g <- ggplot2::ggplot(dados_long, ggplot2::aes(x = grupo, y = valor, fill = grupo)) +
        ggdist::stat_halfeye(adjust = 0.6, width = 0.6, .width = c(0.5, 0.8, 0.95),
                             justification = -0.2, slab_color = "gray20", interval_color = "gray20") +
        ggplot2::geom_point(position = ggplot2::position_nudge(x = 0.15), size = 1.0, alpha = 0.45, color = "black") +
        ggdist::stat_pointinterval(position = ggplot2::position_nudge(x = 0.2), point_color = "black", interval_color = "black", .width = 0.95) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(title = titulo, x = "", y = y) +
        ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }
  } else {
    stop("estilo invalido: use 1, 2, 3 ou 4.")
  }

  # ------------------------------
  # adicionar barras + estrelas
  # ------------------------------
  p_final <- add_sig_bars(g, resultados)

  if (verbose) print(p_final)

  # ------------------------------
  # saida
  # ------------------------------
  invisible(list(
    resultados = resultados,
    pares_signif = resultados[resultados$p < 0.05 & !is.na(resultados$p), , drop = FALSE],
    grafico = p_final,
    dados_long = dados_long
  ))
}
