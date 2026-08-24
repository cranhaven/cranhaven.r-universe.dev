#' Teste t pareado com visualizacoes avancadas
#'
#' Realiza o teste t pareado entre dois vetores numericos (ex.: antes e depois)
#' ou entre duas colunas numericas de um data frame.
#' Inclui quatro estilos de visualizacao (boxplot, violino, monocromatico e half-eye).
#'
#' @param ... Dois vetores numericos com mesmo comprimento, ou
#'   um data frame com exatamente duas colunas numericas.
#' @param titulo Titulo do grafico.
#' @param xlab Rotulo do eixo x.
#' @param ylab Rotulo do eixo y.
#' @param estilo Estilo do grafico:
#'   \itemize{
#'     \item \code{1} Boxplot premium
#'     \item \code{2} Violino + box minimalista
#'     \item \code{3} Monocromatico
#'     \item \code{4} Half-eye (ggdist)
#'   }
#' @param conectar Logico. Se TRUE, conecta pares (apenas teste pareado).
#' @param ajuda Se TRUE, exibe explicacoes detalhadas.
#' @param verbose Se TRUE, mostra mensagens de progresso.
#'
#' @return Lista invisivel contendo:
#' \describe{
#'   \item{resumo}{Medias e desvios-padrao dos grupos}
#'   \item{resultado}{Objeto do teste t (stats::t.test)}
#'   \item{dados}{Data frame usado no grafico}
#'   \item{plot}{Objeto ggplot2}
#' }
#'
#' @export
#'
#' @examples
#' antes <- c(10, 12, 11, 13)
#' depois <- c(9, 11, 10, 10)
#' teste.tpareado(antes, depois)
#'
#' df <- data.frame(A = antes, B = depois)
#' teste.tpareado(df, estilo = 3)

teste.tpareado <- function(
    ...,
    titulo = "Teste t pareado",
    xlab = "",
    ylab = "Valor",
    estilo = 1,
    conectar = TRUE,
    ajuda = FALSE,
    verbose = TRUE
) {

  args <- list(...)

  # ------------------------------
  # Ajuda
  # ------------------------------
  if (ajuda) {
    message(
      "Funcao teste.tpareado()

Entrada aceita:
 - Dois vetores numericos do mesmo comprimento
 - Ou um data frame com duas colunas numericas

# Exemplo 1
antes <- c(10, 12, 11, 13)
depois <- c(9, 11, 10, 10)
teste.tpareado(antes, depois)

# Exemplo 2
df <- data.frame(A = antes, B = depois)
teste.tpareado(df, estilo = 3)

Retorno:
 Lista com resumo, t.test, dados e grafico"
    )
    return(invisible(NULL))
  }

  # ------------------------------
  # Input flexivel
  # ------------------------------
  if (length(args) == 1 && is.data.frame(args[[1]]) && ncol(args[[1]]) == 2) {
    df <- args[[1]]
    if (!all(sapply(df, is.numeric)))
      stop("O data.frame deve ter exatamente duas colunas numericas.")
    x <- df[[1]]
    y <- df[[2]]
    nomes <- colnames(df)
  } else {
    if (length(args) != 2)
      stop("Forneca dois vetores numericos ou um data.frame com duas colunas.")

    x <- args[[1]]
    y <- args[[2]]

    if (!is.numeric(x) || !is.numeric(y))
      stop("Os dois vetores devem ser numericos.")

    if (length(x) != length(y))
      stop("Os vetores devem ter o mesmo comprimento (teste pareado).")

    nomes <- as.character(match.call(expand.dots = FALSE)$...)[1:2]
  }

  # Remover NA
  ok <- complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]

  if (length(x) < 3) stop("E necessario pelo menos 3 pares validos.")

  # ------------------------------
  # Resultados estatisticos
  # ------------------------------
  resultado <- stats::t.test(x, y, paired = TRUE)
  p <- resultado$p.value

  p_label <- if (p < 0.001) "p < 0.001" else paste0("p = ", signif(p, 3))
  signif_label <- if (p < 0.001) "***"
  else if (p < 0.01) "**"
  else if (p < 0.05) "*"
  else ""

  resumo <- data.frame(
    Grupo = nomes,
    Media = c(mean(x), mean(y)),
    Desvio = c(sd(x), sd(y))
  )

  if (verbose) print(resumo)

  # ------------------------------
  # Data frame final
  # ------------------------------
  dados <- data.frame(
    id = seq_along(x),
    grupo = rep(nomes, each = length(x)),
    valor = c(x, y)
  )

  y_pos <- max(dados$valor) * 1.08

  # ------------------------------
  # Camada opcional de linhas entre pares
  # ------------------------------
  camada_linhas <- function() {
    if (!conectar) return(NULL)
    ggplot2::geom_line(
      data = dados,
      ggplot2::aes(x = grupo, y = valor, group = id),
      color = "gray40",
      linewidth = 0.5,
      alpha = 0.6
    )
  }

  cores_vivas <- scales::hue_pal()(length(unique(dados$grupo)))

  # ------------------------------
  # Estilos de grafico
  # ------------------------------

  # ---- ESTILO 1 ------------------------------------------------
  if (estilo == 1) {
    g <- ggplot2::ggplot(dados, ggplot2::aes(grupo, valor, fill = grupo)) +
      ggplot2::geom_boxplot(alpha = .7, outlier.shape = NA) +
      camada_linhas() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        alpha = .4
      ) +
      ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 7) +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(title = titulo, subtitle = p_label, x = xlab, y = ylab) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }


  # ---- ESTILO 2 ------------------------------------------------
  if (estilo == 2) {
    g <- ggplot2::ggplot(dados, ggplot2::aes(grupo, valor, fill = grupo)) +
      ggplot2::geom_violin(
        trim = FALSE,
        alpha = .4,
        color = NA,
        linewidth = .4,
        adjust = .6
      ) +
      ggplot2::geom_boxplot(
        width = .18,
        outlier.shape = NA,
        color = "gray20",
        linewidth = .4
      ) +
      camada_linhas() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        alpha = .55, size = 1.8, color = "gray25"
      ) +
      ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 7) +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::labs(title = titulo, subtitle = p_label, x = xlab, y = ylab) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }

  # ---- ESTILO 3 ------------------------------------------------
  if (estilo == 3) {
    g <- ggplot2::ggplot(dados, ggplot2::aes(grupo, valor)) +
      ggplot2::geom_violin(fill = "gray85", color = NA) +
      ggplot2::geom_boxplot(width = .18, fill = "white") +
      camada_linhas() +
      ggplot2::geom_point(
        position = ggplot2::position_jitter(width = .1),
        color = "gray20", alpha = .4
      ) +
      ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 7) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::labs(title = titulo, subtitle = p_label, x = xlab, y = ylab) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  # ---- ESTILO 4 (ggdist) ---------------------------------------
  if (estilo == 4) {
    if (!requireNamespace("ggdist", quietly = TRUE))
      stop("O pacote ggdist e necessario para estilo = 4.")

    g <- ggplot2::ggplot(dados, ggplot2::aes(grupo, valor, fill = grupo)) +
      ggdist::stat_halfeye(alpha = .6, adjust = .6) +
      camada_linhas() +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = .1), alpha = .4) +
      ggplot2::annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 7) +
      ggplot2::scale_fill_manual(values = cores_vivas) +
      ggplot2::theme_minimal(base_size = 15) +
      ggplot2::labs(title = titulo, subtitle = p_label, x = xlab, y = ylab) +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  print(g)

  invisible(list(
    resumo = resumo,
    resultado = resultado,
    dados = dados,
    plot = g
  ))
}
