#' Grafica los casos de COVID-19
#'
#' @description
#' `plot_covid` Intenta graficar automaticamente la base de datos de covid generados por [casos()]
#'
#' @param datos_covid (**obligatorio**) Lista de `tibble`s resultante de [casos()], [cfr()],
#' [chr()], [positividad()] o [rt()]
#'
#' @param df_name (**opcional**) Nombre de la base de datos dentro de la lista `datos_covid`
#'
#' @param df_date_index (**opcional**) Nombre de la variable que contiene la fecha
#'
#' @param df_variable (**opcional**) Nombre de la variable que se va a graficar en el eje y
#'
#' @param df_covariates (**opcional**) Covariables para el `facet_wrap` (maximo 2)
#'
#' @param facet_scale (**opcional**) Escala para el [ggplot2::facet_wrap()]
#'
#' @param facet_ncol  (**opcional**)  Numero de columnas para el [ggplot2::facet_wrap()]
#'
#' @param date_break_format (**opcional**) Breaks para el eje x [ggplot2::scale_x_date()]
#'
#' @param date_labels_format (**opcional**) Formato de fecha para el eje x [ggplot2::scale_x_date()]
#'
#' @param type (**opcional**) Tipo de grafica (`line`, `area`, `spline` o `point`)
#'
#' @param plot_theme (**opcional**) Tema para el `ggplot2` (ejemplo [ggplot2::theme_classic()]).
#'
#' @param ... (**opcional**) Parametros adicionales para [ggformula::geom_spline()] en caso de elegir
#'  `type="spline"`
#'
#' @return Un `ggplot2` con la imagen graficada.
#'
#' @examples
#'
#' # Para el ejemplo usaremos los datos precargados (datosabiertos) pero tu puedes
#' # correr el ejemplo descargando informacion mas reciente:
#' datos_covid <- datosabiertos
#'
#' # Aqui muchos aparecen en cero si usas el default de datosabiertos
#' # porque la base de datosabiertos tiene muy pocos casos
#' datos_covid |>
#'   casos(list_name = "casos_for_plot", group_by_entidad = FALSE) |>
#'   plot_covid(df_name = "casos_for_plot")
#'
#' # Grafica de casos nacional
#' \donttest{
#' datos_covid |>
#'   casos(group_by_entidad = FALSE, list_name = "plot_nal") |>
#'   plot_covid(df_name = "plot_nal")
#'
#' # Ajuste mediante splines
#' datos_covid |>
#'   casos(group_by_entidad = FALSE, list_name = "spline_nacional") |>
#'   plot_covid(df_name = "spline_nacional", type = "spline", spar = 0.5)
#'
#' # Graficacion por covariables
#' # el objeto devuelto es un objeto de ggplot2 al que se le puede dar formato
#' if (!requireNamespace("ggplot2", quietly = TRUE)) {
#'   datos_covid |>
#'     chr(
#'       group_by_entidad = TRUE, list_name = "plot_nal", .grouping_vars = c("SEXO"),
#'       entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR")
#'     ) |>
#'     plot_covid(
#'       df_name = "plot_nal",
#'       date_break_format = "1 week",
#'       date_labels_format = "%d/%B/%Y",
#'       df_covariates = c("SEXO", "ENTIDAD_FEDERATIVA"),
#'       type = "area"
#'     ) +
#'     ggplot2::ggtitle("Plot nacional")
#' }
#'
#' # Puedes tambien primero editar el tibble que usaras por ejemplo poniendo
#' # los nombres de los sexos
#' datos_covid <- datos_covid |>
#'   chr(
#'     group_by_entidad = TRUE, list_name = "plot_nal", .grouping_vars = c("SEXO"),
#'     entidades = c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR")
#'   )
#' }
#'
#' # Finalmente desconectamos
#' datos_covid$disconnect()
#'
#' @seealso [casos()]
#' @export

plot_covid <- function(datos_covid,
                       df_name = "casos",
                       df_date_index = stringr::str_subset(
                         colnames(datos_covid[df_name][[1]]),
                         "FECHA|fecha|Fecha"
                       ),
                       df_variable = NULL,
                       df_covariates = c(),
                       facet_scale = "free_y",
                       facet_ncol = 4,
                       date_break_format = "2 months",
                       date_labels_format = "%B-%y",
                       type = c("point", "line", "spline", "area"),
                       plot_theme = ggplot2::theme(
                         panel.background = ggplot2::element_rect(fill = "white"),
                         plot.background = ggplot2::element_rect(fill = "white"),
                         axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
                         axis.line.x = ggplot2::element_line(color = "black"),
                         legend.position = "none"
                       ), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort(
      "Para graficar, por favor instala {.code ggplot2} y {.code scales} haciendo
      {.code install.packages(c('ggplot2','scales'))}"
    )
  }

  if (tibble::is_tibble(datos_covid)) {
    datos_covid <- list("datos_covid" = datos_covid)
    df_name <- "datos_covid"
  }

  # Checamos la variable 1
  if (is.null(df_variable)) {
    df_variable <- colnames(datos_covid[df_name][[1]] |> dplyr::select_if(is.numeric))
    df_variable <- df_variable[!(df_variable %in% df_covariates)][1]
    cli::cli_alert_warning(
      "{.code df_variable} no fue especificada. Usaremos la columna {df_variable}"
    )
  }

  if (!requireNamespace("ggformula", quietly = TRUE) & type[1] == "spline") {
    cli::cli_abort(
      "Necesitas instalar {.code ggformula} para {.code splines}",
      call. = FALSE
    )
  }

  if (is.null(df_covariates)) {
    anti_cols <- !(colnames(datos_covid[df_name][[1]]) %in%
      c(df_date_index, df_variable, "ENTIDAD_UM", "ABREVIATURA"))
    df_covariates <- colnames(datos_covid[df_name][[1]])[anti_cols]
    cli::cli_alert_warning(
      "{.code df_covariates} no fue especificada. Usaremos `{df_covariates}`"
    )
  }

  # Checamos la variable 1
  if (length(df_date_index) > 1) {
    cli::cli_abort(
      "Hay dos indices de fecha. Especifica {.code df_date_index} para seleccionar el adecuado"
    )
  }

  if (length(df_covariates) > 2) {
    cli::cli_warn("No se recomiendan mas de dos covariables para los plots automaticos")
  }

  # Hack para cuando es el nacional y no tiene covariables
  if (length(df_covariates) == 0) {
    datos_covid[df_name][[1]][, "covar"] <- ""
    df_covariates <- "covar"
  }

  plot <- datos_covid[df_name][[1]] |>
    dplyr::mutate_at(df_variable, as.numeric) |>
    ggplot2::ggplot() +
    ggplot2::facet_wrap(stats::as.formula(paste("~", paste(df_covariates, collapse = " + "))),
      scales = facet_scale, ncol = facet_ncol
    ) +
    ggplot2::scale_x_date(date_breaks = date_break_format, date_labels = date_labels_format) +
    ggplot2::labs(
      x       = stringr::str_replace_all(df_date_index, "_", " "),
      y       = stringr::str_replace_all(df_variable, "_", " "),
      title   = toupper(df_name),
      caption = "Elaborado mediante covidmx | Github: RodrigoZepeda/covidmx"
    )


  # Specific format for some variables
  if (df_variable == "n") {
    plot <- plot +
      ggplot2::scale_y_continuous(labels = scales::comma)
  }

  type <- tolower(type)
  if (type[1] == "point") {
    plot <- plot +
      ggplot2::geom_point(ggplot2::aes(
        x = as.Date(.data[[df_date_index]]), y = .data[[df_variable]],
        color = .data[[df_covariates[1]]]
      ))
  } else if (type[1] == "line") {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(
        x = as.Date(.data[[df_date_index]]), y = .data[[df_variable]],
        color = .data[[df_covariates[1]]]
      ))
  } else if (type[1] == "spline") {
    plot <- plot +
      ggformula::geom_spline(ggplot2::aes(
        x = as.Date(.data[[df_date_index]]), y = .data[[df_variable]],
        color = .data[[df_covariates[1]]]
      ), ...)
  } else if (type[1] == "area") {
    plot <- plot +
      ggplot2::geom_area(ggplot2::aes(
        x = as.Date(.data[[df_date_index]]), y = .data[[df_variable]],
        fill = .data[[df_covariates[1]]]
      ), ...)
  }

  plot <- plot + plot_theme

  return(plot)
}
