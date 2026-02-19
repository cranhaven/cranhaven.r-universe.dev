#' Cree fácilmente un widget para visualizar los resultados de la prueba Saber Pro
#' en tablas HTML usando el paquete `DT`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Esta función está diseñada para facilitar la creación de tablas para informes
#' y publicaciones produciendo un widget HTML para visualizar un data frame utilizando
#' el paquete `DT`. La forma en que esta función maneja las cosas por usted significa
#' que a menudo no tiene que preocuparse por los pequeños detalles para obtener
#' un resultado impresionante y listo para usar.
#'
#' @param datos Igual uso que en [Tabla()]
#' @param variable Análogo al argumento `categoria` de la función [Tabla()]
#' @param encabezado Igual uso que en [Tabla()]
#' @param leyenda Igual uso que en [Tabla()] con la excepción de que, si no se
#'   introduce ningún valor, el valor por defecto será una nota explicando a qué
#'   hace referencia los valores y columnas de la tabla.
#' @param tituloPdf Igual uso que en [Tabla()]
#' @param mensajePdf Igual uso que en [Tabla()]
#' @param ajustarNiveles Igual uso que en [Tabla()]
#' @param scrollX Igual uso que en [Tabla()]
#' @param colorHead Igual uso que en [Tabla()]
#' @param colorear Igual uso que en [Tabla()]
#' @param estilo Una lista compuesta por dos parámetros:
#'   * `PaletaYear`: Vector de caracteres que especifica los colores de fondo para
#'     los años.
#'   * `PaletaCategoria`: Vector de caracteres que especifica los colores de fuente
#'     para las distintas categorías de la `variable`.
#'
#' @details
#' Esta función se basa enteramente del paquete `DT`, el cual proporciona una
#' interfaz para `R` a la biblioteca `DataTables` de `JavaScript`. Los data frames
#' de `R` se pueden mostrar como tablas en páginas HTML, proporcionando opciones
#' de filtrado, paginación, clasificación y muchas otras características en las
#' tablas.
#'
#' @returns
#' Retorna la tabla creada mediante `DT` la cual pertenece a la clase "datatables" y "htmlwidget".
#'
#' @examples
#' if (require("dplyr")) {
#'   VariosYears <- ejConsolidadoSaberPro2019 |>
#'     mutate(YEAR = replace(YEAR, YEAR==2019, 2020)) |>
#'     bind_rows(ejConsolidadoSaberPro2019)
#' }
#' Msj <- "\u00c9sta es una descripci\u00f3n de la tabla diferente al valor por default."
#' Tabla.SaberPro(
#'   datos      = VariosYears,
#'   variable   = "SEXO",
#'   encabezado = "PUNTAJES POR SEXO",
#'   leyenda    = Msj,
#'   colorHead  = "#FF5B5B",
#'   estilo     = list(
#'     PaletaYear = c("#F9CA00", "#F68118"),
#'     PaletaCategoria = c("#2458C5", "#F0006D", "#42C501")
#'   )
#' )
#' Tabla.SaberPro(
#'   datos      = VariosYears,
#'   variable   = "SEDE",
#'   encabezado = "PUNTAJES POR SEDE",
#'   leyenda    = Msj,
#'   colorHead  = "#F9CA00",
#'   estilo     = list(
#'     PaletaYear = c("#AEF133", "#19EE9F"),
#'     PaletaCategoria = c("#DD1C1A", "#FF6700", "#7E10DE","#0096F2", "#42C501")
#'   )
#' )
#'
#' @export
#'
#' @import DT
#' @import dplyr
#' @importFrom htmltools withTags tag
#' @importFrom tidyr pivot_wider
#' @importFrom methods missingArg
#' @importFrom grDevices topo.colors
Tabla.SaberPro <- function(
    datos, variable, encabezado = "Encabezados de los Niveles de la Categor\u00eda",
    leyenda, tituloPdf = NULL, mensajePdf = "", ajustarNiveles = TRUE,
    scrollX = TRUE, colorHead = "#FFFFFF", colorear = FALSE, estilo) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if(missingArg(datos) || missingArg(variable)) {
    stop("\u00a1Por favor introduzca un conjunto de datos y una variable!", call. = FALSE)
  }
  variable <- tolower(variable)
  if (!(variable %in% datos$Variable)) {
    stop("\u00a1Por favor introduzca alguno de los niveles que se encuentran dentro de la columna 'Variable'!", call. = FALSE)
  }
  if (variable == "total") {
    stop("\u00a1Seleccione un nivel diferente a 'total' de la columna 'Variable', pues este sirve como complemento para los dem\u00e1s niveles!", call. = FALSE)
  }
  if (!is.logical(ajustarNiveles)) {
    stop("\u00a1El argumento 'ajustarNiveles' debe ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!is.logical(scrollX)) {
    stop("\u00a1El argumento 'scrollX' debe ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!is.character(colorHead)) {
    stop("\u00a1El argumento 'colorHead' debe ser un car\u00e1cter que indique un color con el nombre ('red'), c\u00f3digo hexadecimal ('#FF0000') o RGB (rgb(1, 0, 0))!", call. = FALSE)
  }
  if (!is.logical(colorear)) {
    stop("\u00a1El argumento 'colorear' debe ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (missingArg(tituloPdf)) { tituloPdf <- encabezado }
  if (missingArg(leyenda)) {
    Leyenda <- htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;',
                                       "Nota: ", htmltools::em("Los valores presentados entre par\u00e9ntesis hacen referencia a la desviaci\u00f3n est\u00e1ndar."),
                                       htmltools::br(htmltools::em("(*) hace referencia al total de estudiantes evaluados.")))
  } else {
    Leyenda <- htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', "Nota: ", htmltools::em(leyenda))
  }
  AjusteNiveles <- ifelse(ajustarNiveles == TRUE, "compact nowrap hover row-border", "display")

  thead <- function(...) { htmltools::tag("thead", ...) }
  th <- function(...) { htmltools::tag("th", ...) }
  tr <- function(...) { htmltools::tag("tr", ...) }

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA TABLA
  DataFrame <- datos |>
    # Convertir a columnas las observaciones dispersas en múltiples filas
    filter(Variable == variable) |>
    mutate("Valor" = paste0(Total, " (", desv, ")")) |>
    select(-c(Variable, Total, desv)) |>
    pivot_wider(names_from = Componente, values_from = Valor) |>
    relocate(n, .after = last_col()) |>
    mutate(YEAR = factor(YEAR), Clase = factor(Clase))
  Total_IESB <- datos |>
    filter(Variable == "total") |>
    mutate("Valor" = paste0(Total, " (", desv, ")")) |>
    select(-c(Variable, Total, desv)) |>
    pivot_wider(names_from = Componente, values_from = Valor) |>
    relocate(n, .after = last_col()) |>
    mutate(YEAR = factor(YEAR), Clase = factor(Clase))

  DataFrame <- bind_rows(DataFrame, Total_IESB)
  Componentes <- datos |>
    filter(Variable == variable) |>
    group_by(Componente) |> distinct(Componente)
  # Custom Table Container (Nombre de los Encabezados)
  sketch <- htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(rowspan = 2, "A\u00f1o"),
        th(rowspan = 2, "Categor\u00eda"),
        th(colspan = n_groups(Componentes), encabezado),
        th(rowspan = 2, "N*")
      ),
      tr( lapply(Componentes |> pull(), th) )
    )
  ))

  # CREACIÓN DE LA TABLA A RETORNAR
  TablaFinal <- datatable(
    DataFrame,
    class      = AjusteNiveles,
    rownames   = FALSE,
    container  = sketch,
    caption    = Leyenda,
    escape     = FALSE,
    filter     = list(position = "top", clear = TRUE, plain = FALSE),
    extensions = c("Buttons", "KeyTable"),
    options    = list(autoWidth  = TRUE,
                      columnDefs = list(list(className = "dt-center", targets = 0:(n_groups(Componentes)+2)),
                                        list(targets = 2:(n_groups(Componentes)+1), searchable = FALSE),
                                        list(width = "65px", targets = 0)),
                      pageLength = 8,
                      order = list(list(0, "desc"), list(1, "asc")),
                      dom   = "Bfrtip",
                      keys  = TRUE,
                      searchHighlight = TRUE,
                      scrollX = scrollX,
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color':", paste0("'", colorHead, "'"), ", 'color': '#000000'});","}"),
                      language = list(
                        processing     = "Procesando...",
                        lengthMenu     = "Mostrar _MENU_ registros",
                        zeroRecords    = "No se encontraron resultados",
                        emptyTable     = "Ning\u00fan dato disponible en esta tabla",
                        info           = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                        infoEmpty      = "Mostrando registros del 0 al 0 de un total de 0 registros",
                        infoFiltered   = "(filtrado de un total de _MAX_ registros)",
                        infoPostFix    = "",
                        search         = "Buscar:",
                        url            = "",
                        infoThousands  = ",",
                        loadingRecords = "Cargando...",
                        paginate = list(
                          first    = "Primero",
                          last     = "\u00daltimo",
                          `next`   = "Siguiente",
                          previous = "Anterior"
                        ),
                        aria = list(
                          sortAscending  = "Activar para ordenar la columna de manera ascendente",
                          sortDescending = "Activar para ordenar la columna de manera descendente"
                        )
                      ),
                      buttons = list(
                        list(extend = "copy", text = "Copiar"), "csv", "excel",
                        list(extend = "pdf", pageSize = "A4", filename = "pdf",
                             message = mensajePdf, title = tituloPdf
                        ),
                        list(extend = "print", text = "Imprimir", pageSize = "A4",
                             message = mensajePdf, title = tituloPdf
                        )
                      )
    )
  )

  if (colorear && missingArg(estilo)) {
    TablaFinal <- TablaFinal |>
      formatStyle(
        "YEAR", target = "cell", fontWeight = "bold",
        backgroundColor = styleEqual( unique(DataFrame$YEAR), topo.colors(nlevels(DataFrame$YEAR)) )
      ) |>
      formatStyle(
        "Clase", target = "cell", fontWeight = "bold",
        color = styleEqual( unique(DataFrame$Clase), rainbow(nlevels(DataFrame$Clase), v = 0.8) )
      )
  } else if (!missingArg(estilo)) {
    TablaFinal <- TablaFinal |>
      formatStyle(
        "YEAR", target = "cell", fontWeight = "bold",
        backgroundColor = styleEqual( unique(DataFrame$YEAR), estilo$PaletaYear )
      ) |>
      formatStyle(
        "Clase", target = "cell", fontWeight = "bold",
        color = styleEqual( unique(DataFrame$Clase), estilo$PaletaCategoria )
      )
  }

  return(TablaFinal)
}
