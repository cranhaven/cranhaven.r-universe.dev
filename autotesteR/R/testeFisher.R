#' Teste exato de Fisher
#'
#' Realiza o teste exato de Fisher a partir de dois vetores categoricos ou um data frame de duas colunas, construindo uma tabela de contingencia.
#'
#' @param var1 Vetor categorico ou data frame com duas colunas.
#' @param var2 Vetor categorico (obrigatorio se var1 for vetor).
#' @param titulo Titulo do grafico (string). Default: "Teste Exato de Fisher"
#' @param x Nome do eixo x no grafico (string). Default: NULL (usa nome da variavel)
#' @param y Nome do eixo y no grafico (string). Default: "Proporcao"
#' @param mostrar_tabela Logico. Se TRUE, exibe a tabela de contingencia no console. Default: TRUE
#' @param estilo Estetica do plot gerado pela funcao.
#' @param ajuda Logico. Se TRUE, mostra explicacao detalhada da funcao. Default: FALSE
#' @param verbose Logico. Se TRUE, imprime mensagens detalhadas sobre o teste. Default: TRUE
#' @return Objeto invisivel do teste e grafico.
#' @export
#'
#' @examples
#' dados <- data.frame(controle = c("saudavel","saudavel","doente","doente","doente"),
#'                     tratamento = c("saudavel","saudavel","saudavel","saudavel","doente"))
#' teste.fisher(dados)
teste.fisher <- function(var1, var2 = NULL, titulo = "Teste Exato de Fisher",
                         x = NULL, y = "Proporcao", estilo = 1, mostrar_tabela = TRUE,
                         ajuda = FALSE, verbose = TRUE) {

  # Ajuda
  if (ajuda || missing(var1)) {
    if (verbose) {
      message(
        "Funcao teste.fisher()

Descricao:
  Realiza o Teste Exato de Fisher para avaliar se ha associacao significativa entre duas variaveis categoricas.

Quando usar:
  - Duas variaveis categoricas (fatores)
  - Tabelas pequenas (especialmente 2x2)
  - Frequencias esperadas menores que 5

Diferenca entre Fisher e Qui-quadrado:
  - Fisher: probabilidade exata das combinacoes observadas
  - Qui-quadrado: aproximacao teorica, exige amostras maiores"
      )
    }
    return(invisible(NULL))
  }

  # Pacotes necessarios
  pacotes <- c("ggplot2", "dplyr", "tidyr", "vcd")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("O pacote '", pkg, "' nao esta instalado. Instale com install.packages('", pkg, "')"))
    }
  }

  # Caso var1 seja um data frame de duas colunas
  if (is.data.frame(var1)) {
    if (ncol(var1) != 2) stop("O data frame deve conter exatamente duas colunas categoricas.")

    nomes_colunas <- colnames(var1)

    dados_long <- tidyr::pivot_longer(
      var1,
      cols = everything(),
      names_to = "grupo",
      values_to = "categoria"
    )

    dados_long$grupo <- factor(dados_long$grupo, levels = nomes_colunas, labels = nomes_colunas)

    var1 <- dados_long$grupo
    var2 <- dados_long$categoria
    nome1 <- nomes_colunas[1]
    nome2 <- nomes_colunas[2]

  } else {
    # Se var1 e var2 forem vetores
    if (is.null(var2)) stop("var2 deve ser fornecido se var1 for vetor.")
    if (length(var1) != length(var2)) stop("As variaveis devem ter o mesmo comprimento.")

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

  if (any(dim(tabela) > 2) && verbose) {
    message("A tabela tem dimensao maior que 2x2. O teste de Fisher pode ser computacionalmente pesado e o p-valor e uma aproximacao.")
  }

  # Teste de Fisher
  teste <- stats::fisher.test(tabela)
  if (verbose) {
    message("Teste Exato de Fisher")
    message("---------------------")
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
