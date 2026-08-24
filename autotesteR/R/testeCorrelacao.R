#' Teste de correlacao (Pearson, Spearman ou Kendall)
#'
#' Realiza teste de correlacao entre duas variaveis numericas, escolhendo automaticamente
#' entre Pearson, Spearman ou Kendall com base na normalidade, empates e outliers.
#' Opcionalmente exibe graficos de diagnostico e grafico da correlacao com linha de tendencia.
#'
#' @param x Vetor numerico ou data frame com duas colunas numericas
#' @param y Vetor numerico (opcional se x for data frame)
#' @param metodo Metodo de correlacao: "auto" (padrao), "pearson", "spearman" ou "kendall"
#' @param estilo Estetica do plot gerado pela funcao.
#' @param ajuda Se TRUE, exibe explicacao detalhada da funcao
#' @param verbose Se TRUE, imprime mensagens sobre o metodo escolhido e testes de normalidade
#' @param plot_normalidade Se TRUE, gera QQ-plots para avaliacao da normalidade dos dados
#' @importFrom stats shapiro.test cor.test qqnorm qqline quantile
#'
#' @return Objeto de classe htest invisivel com o resultado do teste de correlacao
#'
#' @examples
#' # Pearson
#' set.seed(123)
#' x <- rnorm(30, sd = 1)
#' y <- x + rnorm(30, sd = 1)
#' teste.correlacao(x, y)
#'
#' # Spearman
#' set.seed(123)
#' x <- runif(300)
#' y <- log(x + 0.1) + rnorm(300, sd = 0.05)
#' teste.correlacao(x, y)
#'
#' # Kendall
#' set.seed(123)
#' x  <- rnorm(200)
#' y  <- x + rnorm(200, sd = 0.2)
#' x_out <- runif(20, -10, 10)
#' y_out <- runif(20, -10, 10)
#' x <- c(x, x_out)
#' y <- c(y, y_out)
#' teste.correlacao(x, y)
#'
#' @export

teste.correlacao <- function(x, y = NULL, metodo = "auto", estilo = 1, ajuda = FALSE,
                               verbose = TRUE, plot_normalidade = FALSE) {

    if (ajuda || missing(x)) {
      if (verbose) message("
Funcao teste.correlacao()

Descricao:
  Testa a correlacao entre duas variaveis numericas, escolhendo automaticamente entre Pearson, Spearman e Kendall.
  Pode exibir testes graficos de normalidade (QQ-plot).

Uso:
  teste.correlacao(x, y, metodo = 'auto', plot_normalidade = TRUE)

Argumentos:
  x, y             Vetores numericos de mesma dimensao ou data frame com duas colunas numericas
  metodo           'auto', 'pearson', 'spearman' ou 'kendall'
  verbose          Se TRUE, exibe qual metodo foi usado e o motivo
  ajuda            Se TRUE, exibe esta mensagem com exemplo
  plot_normalidade Se TRUE, exibe QQ-plots

Exemplo:

# Pearson
  set.seed(123)
  x <- rnorm(30, sd = 1)
  y <- x + rnorm(30, sd = 1)
  teste.correlacao(x, y)

# Spearman
  set.seed(123)
  x <- runif(300)
  y <- log(x + 0.1) + rnorm(300, sd = 0.05)
  teste.correlacao(x, y)

# Kendall
  set.seed(123)
  x  <- rnorm(200)
  y  <- x + rnorm(200, sd = 0.2)
  x_out <- runif(20, -10, 10)
  y_out <- runif(20, -10, 10)
  x <- c(x, x_out)
  y <- c(y, y_out)
  teste.correlacao(x, y)
")
      return(invisible(NULL))
    }

    # Pacotes necessarios
    pacotes <- c("ggplot2", "ggExtra")
    lapply(pacotes, function(pkg) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("O pacote '%s' nao esta instalado.", pkg), call. = FALSE)
      }
    })

    # preparar dados
    if (is.data.frame(x)) {
      if (ncol(x) != 2) stop("O data frame deve conter exatamente duas colunas numericas.")
      if (!all(sapply(x, is.numeric))) stop("As duas colunas do data frame devem ser numericas.")
      nome_x <- colnames(x)[1]; nome_y <- colnames(x)[2]
      dados <- data.frame(x = x[[1]], y = x[[2]])
    } else {
      if (is.null(y)) stop("Forneca dois vetores numericos ou um data frame com duas colunas.")
      if (!is.numeric(x) || !is.numeric(y)) stop("Ambos os vetores devem ser numericos.")
      if (length(x) != length(y)) stop("Os vetores devem ter o mesmo comprimento.")
      nome_x <- deparse(substitute(x)); nome_y <- deparse(substitute(y))
      dados <- data.frame(x = x, y = y)
    }

    # checks e diagnósticos
    possui_empates <- anyDuplicated(dados$x) > 0 || anyDuplicated(dados$y) > 0

    aplicar_teste_normalidade <- function(x) {
      n <- length(x)

      if (n <= 50) {
        p <- stats::shapiro.test(x)$p.value
        metodo <- "Shapiro-Wilk"

      } else if (n <= 300) {
        if (!requireNamespace("nortest", quietly = TRUE)) {
          stop("Instale o pacote 'nortest' para Anderson-Darling.")
        }
        p <- nortest::ad.test(x)$p.value
        metodo <- "Anderson-Darling"

      } else {
        ks_res <- stats::ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
        p <- ks_res$p.value
        metodo <- "Kolmogorov-Smirnov"
      }

      list(p = p, metodo = metodo)
    }

    norm_x  <- aplicar_teste_normalidade(dados$x)
    norm_y  <- aplicar_teste_normalidade(dados$y)

    normal_x <- norm_x$p > 0.05
    normal_y <- norm_y$p > 0.05


    if (plot_normalidade) {
      oldpar <- graphics::par(mfrow = c(1,2)); on.exit(graphics::par(oldpar), add = TRUE)
      stats::qqnorm(dados$x, main = paste("QQ-Plot de", nome_x)); stats::qqline(dados$x, col = "red")
      stats::qqnorm(dados$y, main = paste("QQ-Plot de", nome_y)); stats::qqline(dados$y, col = "red")
    }

    detect_outliers <- function(v) {
      q1 <- stats::quantile(v, 0.25); q3 <- stats::quantile(v, 0.75); iqr <- q3 - q1
      which(v < (q1 - 1.5 * iqr) | v > (q3 + 1.5 * iqr))
    }
    out_total <- max(length(detect_outliers(dados$x)) / nrow(dados),
                     length(detect_outliers(dados$y)) / nrow(dados))

    metodo_usado <- if (metodo == "auto") {
      if (out_total > 0.05) {
        "kendall"
      } else if (normal_x && normal_y && !possui_empates) {
        "pearson"
      } else {
        "spearman"
      }
    } else {
      metodo
    }

    if (verbose && metodo == "auto") {
      message(sprintf("Metodo escolhido: %s", metodo_usado))

        # Exibir detalhes da normalidade
        if (!normal_x || !normal_y) {
          message("- Dados nao normalmente distribuidos")
        }

        if (possui_empates) message("- Foram encontrados empates nos dados")
        if (out_total > 0.05) message(sprintf("- Presenca de outliers detectada: %.1f%%",
                                              out_total * 100))
      }
      if (possui_empates) message("- Foram encontrados empates nos dados")
      if (out_total > 0.05) message(sprintf("- Presenca de outliers detectada: %.1f%%", out_total * 100))

    # teste estatistico
    teste <- stats::cor.test(dados$x, dados$y, method = metodo_usado)
    coef_val <- round(unname(teste$estimate), 3)
    p <- teste$p.value
    p_texto <- if (p < 0.001) "p < 0.001" else paste0("p = ", signif(p, 3))

    interprete <- cut(abs(coef_val),
                      breaks = c(-Inf, 0.3, 0.5, 0.7, 0.9, Inf),
                      labels = c("muito fraca ou inexistente", "fraca", "moderada", "forte", "muito forte"),
                      right = FALSE)

    simbolo <- ifelse(metodo_usado == "kendall", "tau", "r")
    subtitulo <- paste0(simbolo, " = ", coef_val, " | ", p_texto, " | ", interprete)

    # escolher metodo de suavização para linha de tendência
    smooth_method <- if (tolower(metodo_usado) == "pearson") "lm" else "loess"

    # criar plots por estilo (usando dados explicitamente)
    if (estilo == 1) {
      g <- ggplot2::ggplot(dados, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.60, size = 2.5, col = "grey30") +
        ggplot2::geom_smooth(method = smooth_method, se = FALSE, linetype = "dashed") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(title = paste("Correlacao de", tools::toTitleCase(metodo_usado)),
                      subtitle = subtitulo, x = nome_x, y = nome_y)
    } else if (estilo == 2) {
      g <- ggplot2::ggplot(dados, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(alpha = 0.45, size = 2) +
        ggplot2::geom_density_2d(color = "grey30", alpha = 0.7) +
        ggplot2::geom_smooth(method = smooth_method, se = FALSE) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(title = paste("Correlacao de", tools::toTitleCase(metodo_usado)),
                      subtitle = subtitulo, x = nome_x, y = nome_y)
    } else if (estilo == 3) {
      g <- ggplot2::ggplot(dados, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_hex() +
        ggplot2::scale_fill_gradientn(
          colours = c("#0000FF", "#00FF00", "#FFFF00", "#FF0000"),
          trans = "sqrt",
          name = "Densidade"
        ) +
        ggplot2::geom_smooth(method = smooth_method, se = FALSE, color = "white") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(
          title = paste("Correlacao de", tools::toTitleCase(metodo_usado)),
          subtitle = subtitulo, x = nome_x, y = nome_y
        )

    } else if (estilo == 4) {
      # marginais com ggExtra (fallback para estilo 1 se nao instalado)
      if (!requireNamespace("ggExtra", quietly = TRUE)) {
        warning("Para estilo = 4 instale o pacote 'ggExtra'. Usando estilo 1 como fallback.")
        g <- ggplot2::ggplot(dados, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_point(alpha = 0.65, size = 2.5) +
          ggplot2::geom_smooth(method = smooth_method, se = FALSE, linetype = "dashed") +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::labs(title = paste("Correlacao de", tools::toTitleCase(metodo_usado)),
                        subtitle = subtitulo, x = nome_x, y = nome_y)
      } else {
        p_base <- ggplot2::ggplot(dados, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_point(alpha = 0.6, size = 2, col = "grey30") +
          ggplot2::geom_smooth(method = smooth_method, se = FALSE) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::labs(title = paste("Correlacao de", tools::toTitleCase(metodo_usado)),
                        subtitle = subtitulo, x = nome_x, y = nome_y)
        g <- ggExtra::ggMarginal(p_base, type = "density", size = 5, fill = "#c8d3e0")
    }

    }  else {
      stop("estilo invalido: use 1, 2, 3 ou 4.")
    }

    print(g)
    invisible(teste)
  }
