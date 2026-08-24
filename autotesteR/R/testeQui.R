#' Teste de qui-quadrado
#'
#' Aplica o teste qui-quadrado ou Fisher para associacao entre duas variaveis categoricas.
#'
#' @param var1 Vetor categorico ou data frame com duas colunas (grupo 1 e 2).
#' @param var2 Vetor categorico (grupo 2). Obrigatorio se var1 for vetor.
#' @param titulo Titulo do grafico (string). Default: "Teste Qui-Quadrado".
#' @param x Rotulo do eixo x no grafico (string). Default: NULL (usa nome da variavel).
#' @param y Rotulo do eixo y no grafico (string). Default: "Proporcao".
#' @param mostrar_tabela Logico. Se TRUE, exibe a tabela de contingencia no console. Default: TRUE.
#' @param estilo Estetica do plot gerado pela funcao.
#' @param ajuda Logico. Se TRUE, exibe explicacao detalhada da funcao. Default: FALSE.
#' @param verbose Logico. Se TRUE, imprime mensagens sobre o teste e frequencias esperadas. Default: TRUE.
#' @return Resultado do teste e tabela de contingencia.
#' @export
#'
#' @examples
#'
#' dados <- data.frame(controle = c(rep('saudavel', 50), rep('doente', 150)),
#'                     tratamento = c(rep('saudavel', 100), rep('doente', 100)))
#' teste.qui(dados)

teste.qui <- function(var1, var2 = NULL, titulo = "Teste Qui-Quadrado",
                      x = NULL, y = "Proporcao", estilo = 1, mostrar_tabela = TRUE,
                      ajuda = FALSE, verbose = TRUE) {

  # Ajuda
  if (ajuda || missing(var1)) {
    if (verbose) {
      message(
        "Funcao teste.qui()

Descricao:
  Realiza o Teste Qui-Quadrado de Pearson para avaliar associacao entre duas variaveis categoricas.

Quando usar:
  - Duas variaveis categoricas com amostras grandes
  - A tabela de contingencia pode ter mais de 2 categorias por variavel
  - Frequencias esperadas nas celulas devem ser >= 5

Limitacoes:
  - Nao recomendado para tabelas pequenas; use 'teste.fisher()' nesses casos."
      )
    }
    return(invisible(NULL))
  }

  # Pacotes necessarios
  pacotes <- c("ggplot2", "dplyr", "tidyr", "vcd")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("O pacote '", pkg, "' nao esta instalado. Instale com install.packages('", pkg, "')"), call. = FALSE)
    }
  }

  # Caso var1 seja um data frame com duas colunas
  if (is.data.frame(var1)) {
    if (ncol(var1) != 2) stop("O data frame deve conter exatamente duas colunas categoricas.", call. = FALSE)

    # Guardar nomes originais
    nomes_colunas <- colnames(var1)

    # Converter para formato long
    dados_long <- tidyr::pivot_longer(
      var1,
      cols = tidyselect::everything(),
      names_to = "var1",
      values_to = "var2"
    )

    # Usar nomes reais das colunas
    dados_long$var1 <- factor(dados_long$var1, levels = nomes_colunas, labels = nomes_colunas)

    var1 <- dados_long$var1
    var2 <- dados_long$var2
    nome1 <- nomes_colunas[1]
    nome2 <- nomes_colunas[2]
  } else {
    # Se var1 e var2 forem vetores
    if (is.null(var2)) stop("var2 deve ser fornecido se var1 for vetor.", call. = FALSE)
    if (length(var1) != length(var2)) stop("As variaveis devem ter o mesmo comprimento.", call. = FALSE)

    nome1 <- deparse(substitute(var1))
    nome2 <- deparse(substitute(var2))
    nome1 <- sub(".*\\$", "", nome1)
    nome2 <- sub(".*\\$", "", nome2)
  }

  if (is.null(x)) x <- nome1
  if (is.null(y)) y <- "Proporcao"

  # Tabela de contingencia
  tabela <- table(var1, var2)
  if (verbose && mostrar_tabela) {
    message("Tabela de contingencia (observada):")
    print(tabela)
  }

  # Teste Qui-Quadrado
  teste <- suppressWarnings(stats::chisq.test(tabela))
  freq_esperadas <- teste$expected
  if (verbose && any(freq_esperadas < 5)) {
    stop("Algumas frequencias esperadas sao < 5. Considere usar o teste exato de Fisher para maior precisao.")
  }

  if (verbose) {
    message("Resultado do Teste Qui-Quadrado de Pearson:")
    message("--------------------------------------------")
    message("Estatistica X2: ", signif(teste$statistic, 4))
    message("Graus de liberdade: ", teste$parameter)
    message("p-valor: ", signif(teste$p.value, 4))
  }

  # Preparacao para grafico
  df_plot <- data.frame(var1 = var1, var2 = var2)
  df_prop <- df_plot |>
    dplyr::group_by(var1, var2) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(var1) |>
    dplyr::mutate(prop = n / sum(n))

  # --------------------------
  # ESTILO 1 (Barra empilhada)
  # --------------------------
  if (estilo == 1) {
    g <- ggplot2::ggplot(df_prop, ggplot2::aes(x = var1, y = prop, fill = var2)) +
      ggplot2::geom_bar(stat = "identity", color = NA) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::labs(
        title = titulo,
        x = "",
        y = y,
        fill = nome2
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "right",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }

  # --------------------------
  # ESTILO 2 (Barras Lado-a-Lado)
  # --------------------------
  if (estilo == 2) {
    g <- ggplot2::ggplot(df_prop, ggplot2::aes(x = var1, y = prop, fill = var2)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.8)) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::labs(
        title = titulo,
        x = "",
        y = y,
        fill = nome2
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "right",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }

  # --------------------------
  # ESTILO 3 (Mosaic)
  # --------------------------
  if (estilo == 3) {
    if (!requireNamespace("vcd", quietly = TRUE)) {
      stop("O estilo 3 requer o pacote 'vcd'. Instale com install.packages('vcd')")
    }

    # tabela de contingencia
    tab <- table(var1, var2)

    # mosaico
    vcd::mosaic(tab,
                shade = TRUE,
                legend = TRUE,
                main = titulo)
  }

  # --------------------------
  # ESTILO 4 (Pizza)
  # --------------------------
  if (estilo == 4) {
    g <- ggplot2::ggplot(df_prop, ggplot2::aes(x = "", y = prop, fill = var2)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
      ggplot2::coord_polar("y") +
      ggplot2::facet_wrap(~ var1) +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::labs(
        title = titulo,
        fill = nome2,
        x = NULL,
        y = NULL
      ) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 12),
        plot.title = ggplot2::element_text(hjust = 0.5)
      )
  }

  if (estilo != 3) print(g)
  invisible(teste)
}
