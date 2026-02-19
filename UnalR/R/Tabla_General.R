#' Cree fácilmente un widget para visualización de tablas HTML usando el paquete `DT`
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Esta función simplifica la producción de tablas para presentaciones visualmente
#' atractivas, pues está diseñada para facilitar la creación de tablas para informes
#' y publicaciones produciendo un widget HTML para visualizar un data frame utilizando
#' el paquete `DT`.
#'
#' @param datos Un data frame o una matriz.
#' @param colNames Vector de caracteres que especifica los nombres de las columnas
#'   de la tabla a retornar. Si no se introduce algún valor se tomará el mismo
#'   nombre de las columnas presentes en `datos`.
#' @param filtros Si es `FALSE` (*valor predeterminado*) no se habilitará/aplicará
#'   los filtros por columna. Establézcalo en `TRUE` si desea generar filtros de
#'   columna automáticamente.
#' @param colFilters Vector numérico que especifica las columnas a las cuales les
#'   desea agregar la opción de poder filtrar. Si no se introduce algún valor todas
#'   las columnas tendrán habilitada la opción de poder filtrar.
#' @param encabezado Igual uso que en [Tabla()]
#' @param leyenda Igual uso que en [Tabla()]
#' @param tituloPdf Igual uso que en [Tabla()]
#' @param mensajePdf Igual uso que en [Tabla()]
#' @param ajustarNiveles Igual uso que en [Tabla()]
#' @param scrollX Igual uso que en [Tabla()]
#' @param colorHead Igual uso que en [Tabla()]
#' @param estilo Una lista compuesta por listas las cuales en su interior contiene
#'   argumentos válidos de la función [formatStyle()][DT:: formatStyle()], esto
#'   con la finalidad de que pueda aplicar estilos CSS a la tabla, tales como color
#'   de la fuente, color de fondo, tamaño de fuente, etc. Puede encontrar mayor
#'   información de los argumentos disponibles \href{https://rstudio.github.io/DT/functions.html}{aquí}.
#'
#' @details
#' Esta función se basa enteramente del paquete `DT`, el cual proporciona una interfaz
#' para `R` a la biblioteca `DataTables` de `JavaScript`. Los data frames de `R`
#' se pueden mostrar como tablas en páginas HTML, proporcionando opciones de
#' filtrado, paginación, clasificación y muchas otras características en las tablas.
#'
#' Al establecer `filtros = FALSE` no elimina ni modifica el filtro global
#' (*cuadro de búsqueda en la parte superior derecha*).
#'
#' Para el argumento `colFilters` recuerde que la numeración inicia en 0, es decir,
#' la primera columna tiene asociado el índice 0, la segunda el 1, y así sucesivamente.
#'
#' @returns
#' Retorna la tabla creada mediante `DT` la cual pertenece a la clase "datatables"
#' y "htmlwidget".
#'
#' @examplesIf require("datasets")
#' Tabla.General(datos = datasets::mtcars)
#'
#' @examplesIf all(require("dplyr"), require("tidyr"), require("DT"))
#' # library("dplyr"); library("tidyr"); library("DT")
#' df <- ejGraduados |>
#'   filter(TIPO_NIVEL == "Pregrado") |>
#'   group_by(YEAR, SEMESTRE, DEP_NAC, CIU_NAC, SEXO, CAT_EDAD, ESTRATO, PROGRAMA) |>
#'   summarise(Total = n(), .groups = "drop") |>
#'   mutate(across(where(is.character), \(x) replace_na(x, replace = "SIN INFO")))
#'
#' Nombres <- c("<em>A\u00f1o</em>", "Semestre", "Departamento",
#'              "Municipio", "Sexo", "Edad", "Estrato", "Carrera", "Total"
#'              )
#' Titulo  <- paste(
#'   "<b>HIST\u00d3RICO DEL TOTAL DE GRADUADOS DE PREGRADO ",
#'   "DEPENDIENDO DE LAS VARIABLES SELECCIONADAS</b>"
#' )
#' Tabla.General(
#'   datos          = df,
#'   colNames       = Nombres,
#'   filtros        = TRUE,
#'   colFilters     = 0:3,
#'   encabezado     = Titulo,
#'   leyenda        = "N\u00famero de graduados de pregrado por lugar de procedencia.",
#'   tituloPdf      = "Este es un t\u00edtulo provisional para el PDF",
#'   mensajePdf     = "Este es un mensaje provisional para el PDF",
#'   ajustarNiveles = TRUE,
#'   colorHead      = "#4CFF49",
#'   estilo         = list(
#'     list(
#'       columns = "YEAR", target = "cell", fontWeight = "bold",
#'       backgroundColor = styleEqual(unique(df$YEAR), c("#FF6400", "#01CDFE", "#FF0532"))
#'     ),
#'     list(
#'       columns = "SEMESTRE", target = "cell", fontWeight = "bold",
#'       color = styleEqual(unique(df$SEMESTRE), c("#3D3397", "#AE0421"))
#'     ),
#'     list(columns = "DEP_NAC", color = "#FFFFFF", backgroundColor = "#4D1B7B"),
#'     list(columns = "CIU_NAC", color = "#FFFFFF", backgroundColor = "#F59E11")
#'   )
#' )
#'
#' @export
#'
#' @import DT
#' @import dplyr
#' @importFrom htmltools withTags tag
#' @importFrom methods missingArg
Tabla.General <- function(
    datos, colNames, filtros = FALSE, colFilters, encabezado = "", leyenda = "",
    tituloPdf = NULL, mensajePdf = "", ajustarNiveles = TRUE, scrollX = TRUE,
    colorHead = "#FFFFFF", estilo) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (missingArg(datos)) {
    stop("\u00a1Por favor introduzca un conjunto de datos!", call. = FALSE)
  }
  if (missingArg(colNames)) {
    colNames <- colnames(datos)
  } else {
    if (length(colNames) != ncol(datos)) {
      stop(paste0(
        "\u00a1El n\u00famero de nombres para las columnas de la tabla no coincide con el n\u00famero de columnas presentes en 'datos'!",
        "\n\t", length(colNames), " != ", ncol(datos)
        ), call. = FALSE
      )
    }
  }
  if (!all(is.logical(filtros), is.logical(ajustarNiveles), is.logical(scrollX))) {
    stop("\u00a1Los argumentos 'filtros', 'ajustarNiveles' y 'scrollX' deben ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!is.character(colorHead)) {
    stop("\u00a1El argumento 'colorHead' debe ser un car\u00e1cter que indique un color con el nombre ('red'), c\u00f3digo hexadecimal ('#FF0000') o RGB (rgb(1, 0, 0))!", call. = FALSE)
  }
  if (missingArg(tituloPdf)) { tituloPdf <- encabezado }
  if (!missingArg(leyenda)) {
    leyenda <- htmltools::tags$caption(style = "caption-side: bottom; text-align: center;", "Tabla: ", htmltools::em(leyenda))
  }
  AjusteNiveles <- ifelse(ajustarNiveles == TRUE, "compact nowrap hover row-border", "display")

  if (filtros) {
    Filtros <- list(position = "top", clear = TRUE, plain = FALSE)

    if (missingArg(colFilters)) {
      dots <- list()
    } else {
      if (max(colFilters) >= length(colNames) || min(colFilters) < 0) {
        stop("\u00a1El vector ingresado para seleccionar las columnas con filtro debe estar entre [0, n-1] donde n representa el total de columnas!", call. = FALSE)
      } else {
        U    <- 0:(length(colNames) - 1)
        dots <- list(targets = setdiff(U, colFilters), searchable = FALSE)
      }
    }
  } else {
    if (!missingArg(colFilters)) {
      warning("\u00a1El valor para el argumento 'colFilters' que ha ingresado queda deshabilitado debido a que 'filtros = FALSE'!", call. = FALSE)
    }
    Filtros <- "none"; dots <- list()
  }

  thead <- function(...) { htmltools::tag("thead", ...) }
  th <- function(...) { htmltools::tag("th", ...) }
  tr <- function(...) { htmltools::tag("tr", ...) }

  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA TABLA
  DataFrame <- datos %>% mutate_all(., as.factor)
  # Custom Table Container (Nombre de los Encabezados Agrupados)
  sketch <- htmltools::withTags(table(
    class = "display",
    thead(
      tr(
        th(colspan = length(colNames), encabezado)
      ),
      tr(lapply(colNames, th))
    )
  ))
  # CREACIÓN DE LA TABLA A RETORNAR
  TablaFinal <- datatable(
    DataFrame,
    class      = AjusteNiveles,
    rownames   = FALSE,
    colnames   = colNames,
    container  = sketch,
    caption    = leyenda,
    escape     = FALSE,
    filter     = Filtros,
    extensions = c("Buttons", "KeyTable"),
    options    = list(
      autoWidth  = TRUE,
      columnDefs = list(
        list(className = "dt-center", targets = 0:(length(colNames) - 1)),
        dots, list(width = "65px", targets = 0)
      ),
      pageLength = 8,
      order = list(list(0, "desc"), list(1, "asc")),
      dom   = "Bfrtip",
      keys  = TRUE,
      searchHighlight = TRUE,
      scrollX = scrollX,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color':", paste0("'", colorHead, "'"), ", 'color': '#000000'});", "}"
      ),
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
        list(
          extend = "pdf", pageSize = "A4", filename = "pdf",
          message = mensajePdf, title = tituloPdf
        ),
        list(
          extend = "print", text = "Imprimir", pageSize = "A4",
          message = mensajePdf, title = tituloPdf
        )
      )
    )
  )

  if (!missingArg(estilo)) {
    for (i in seq_len(length(estilo))) {
      Temp <- do.call(formatStyle, append(list(table = TablaFinal), estilo[[i]]))
      TablaFinal <- Temp
    }
  }

  return(TablaFinal)
}
