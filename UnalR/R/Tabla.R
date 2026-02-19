#' Cree fácilmente un widget para visualización de tablas HTML usando el paquete `DT`
#'
#' Esta función está diseñada para facilitar/simplificar la creación/producción de tablas para informes, presentaciones y
#' publicaciones, produciendo un widget HTML para visualizar un data frame
#' utilizando el paquete `DT`. La forma en que esta función maneja las cosas por
#' usted significa que a menudo no tiene que preocuparse por los pequeños detalles
#' para obtener un resultado impresionante y listo para usar.
#'
#' @param datos Un data frame.
#' @param df Argument deprecated, use `datos` instead.
#' @param rows Una variable categórica dentro del data frame ingresado en `datos`.
#' @param pivotCat Variable categórica que contiene los niveles/factores que desea
#'   pivotear como columnas. Si omite este parámetro se da por hecho que no desea
#'   pivotear nada sino graficar tal cual su data frame
#' @param pivotVar Variable numérica que contiene los valores que desea colocar
#'   en cada celda al realizar el pivotaje.
#' @param columnNames Vector de caracteres que especifica los nombres de las columnas
#'   de la tabla a retornar. Si no se introduce algún valor se tomará el mismo
#'   nombre de las columnas presentes en `datos`.
#' @param filtros Si es `FALSE` (*valor predeterminado*) no se habilitará/aplicará
#'   los filtros por columna. Establézcalo en `TRUE` si desea generar filtros de
#'   columna automáticamente.
#' @param colFilters Vector numérico que especifica las columnas a las cuales les
#'   desea agregar la opción de poder filtrar. Si no se introduce algún valor todas
#'   las columnas tendrán habilitada la opción de poder filtrar.
#' @param estadistico X.
#' @param encabezado Cadena de caracteres que describe los distintos niveles de
#'   la variable `categoria`.
#' @param leyenda Cadena de caracteres que describe información adicional de la
#'   tabla, ésta se sitúa en la parte inferior de la tabla de manera centrada,
#'   dicho texto se visualizará en todas las opciones de descarga. Su valor por
#'   defecto es `NULL`.
#' @param tituloPdf Cadena de caracteres que proporciona un título a la tabla al
#'   momento de generar el `.pdf` como al hacer clic al botón de imprimir. Su valor
#'   por defecto es el introducido en el argumento `encabezado`.
#' @param mensajePdf Cadena de caracteres que proporciona un mensaje situado entre
#'   el título y la tabla. Se visualizará tanto al generar el `.pdf` como al
#'   hacer clic al botón de imprimir.
#' @param ajustarNiveles Si es `TRUE` (*valor predeterminado*) se buscará optimizar
#'   el espacio entre las columnas, colocando todos los nombres de las columnas de
#'   forma horizontal y eliminando al máximo el espacio entre éstas.
#' @param scrollX Si es `TRUE` (*valor predeterminado*) se habilitará la propiedad
#'   Scroller para el eje X. Tenga presente que cuando su df contiene muchas columnas
#'   es de utilidad (*pues no permite que se salga la tabla por ancho*), sin embargo,
#'   asegúrese de desactivarlo cuando presente pocas columnas, pues se verá un
#'   desplazamiento de los encabezados debido a un conflicto interno.
#' @param fillContainer Valor booleano para indicar si desea que la tabla rellene
#'   automáticamente el elemento que lo contiene.
#' @param colorHead Cadena de caracteres que indica el color de fondo de la cabecera
#'   de la tabla. Puede indicar el color con el nombre (`"red"`), código hexadecimal
#'   (`"#FF0000"`) o RGB (`rgb(1, 0, 0)`). El valor por defecto es "blanco" (`"#FFFFFF"`).
#' @param estilo Una lista compuesta por listas las cuales en su interior contiene
#'   argumentos válidos de la función [formatStyle()][DT:: formatStyle()], esto
#'   con la finalidad de que pueda aplicar estilos CSS a la tabla, tales como color
#'   de la fuente, color de fondo, tamaño de fuente, etc. Puede encontrar mayor
#'   información de los argumentos disponibles \href{https://rstudio.github.io/DT/functions.html}{aquí}.
#'   * `Tema`: Modifica el tema con el cual se creará la tabla Los posibles
#'     valores son un número entero entre \eqn{[1, 14]} el cual hace referencia
#'     a diferentes temas disponibles para `gt`/`gtExtras` (los primeros 6 se
#'     obtienen con `opt_stylize(style = i, color = "gray")`, `gt_theme_538`,
#'     `gt_theme_dark`, `gt_theme_dot_matrix`, `gt_theme_espn`, `gt_theme_excel`,
#'     `gt_theme_guardian`, `gt_theme_nytimes` y `gt_theme_pff` respectivamente).
#'   * `Titulo`: Cadena de caracteres indicando el título principal de la tabla.
#'   * `Padding`: Vector numérico de longitud 2, que tiene como primera coordenada
#'     el padding vertical, es decir si aumenta o disminuye el relleno vertical.
#'     Y como segunda coordenada el padding horizontal.
#'   * `Color`: Lista compuesta de listas, en la cual cada una de ellas detalla,
#'     qué columna se va a afectar y con qué colores de relleno, es decir,
#'     `list(columns = Columna , backgroundColor = Colores)`.
#'
#' @param estatico Si es `FALSE` (*valor predeterminado*) la tabla a retornar será
#'   dinámica (*usando la librería* `DT`), en caso contrario se retornará una tabla
#'   estática construida con `gt` y `gtExtras`.
#'
#' @details
#' Esta función se basa enteramente del paquete `DT`, el cual proporciona una
#' interfaz para `R` a la biblioteca `DataTables` de `JavaScript`. Los data frames
#' de `R` se pueden mostrar como tablas en páginas HTML, proporcionando opciones
#' de filtrado, paginación, clasificación y muchas otras características en las
#' tablas.
#'
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
#' @examplesIf all(require("DT"), require("dplyr"), require("tidyr"))
#' # library(DT); library(dplyr); library(tidyr)
#' # Example of R Combinations with Dot (".") and Pipe (%>%) Operator
#' # UnalR::Agregar(
#' #   datos      = UnalData::Graduados,
#' #   formula    = SEDE_NOMBRE_ADM ~ YEAR + SEMESTRE,
#' #   frecuencia = list("Year" = 2009:2022, "Period" = 1:2)
#' #   ) |>
#' #   select(-Variable) |>
#' #   rename(Year = YEAR, Semester = SEMESTRE, Cat = Clase) %>%
#' #   Tabla(
#' #     ., rows = vars(Year, Semester), pivotCat = Cat, pivotVar = Total
#' #   )
#' Tabla(
#'   datos       = ejConsolidadoGrad |> dplyr::filter(Variable == "SEDE_NOMBRE_ADM") |>
#'     dplyr::select(-Variable),
#'   rows        = vars(YEAR, SEMESTRE),
#'   pivotCat    = Clase,
#'   pivotVar    = Total,
#'   columnNames = c("Año", "Semestre", "Total"),
#'   estadistico = "Suma",
#'   encabezado  = "TOTAL DE ESTUDIANTES \u00d7 SEDE DE GRADUACI\u00d3N",
#'   leyenda     = "Distribuci\u00f3n de estudiantes graduados (desde el 2009-I al 2021-I) por sede.",
#'   tituloPdf   = "ESTUDIANTES GRADUADOS POR SEDE",
#'   colorHead   = "#8CC63F",
#'   estilo      = list(
#'     list(
#'       columns = "YEAR", target = "cell", fontWeight = "normal",
#'       backgroundColor = styleEqual(
#'         unique(ejConsolidadoGrad$YEAR), rainbow(13, alpha = 0.5, rev = TRUE)
#'       )
#'     ),
#'     list(
#'       columns = "SEMESTRE", target = "cell", fontWeight = "bold",
#'       color = styleEqual(unique(ejConsolidadoGrad$SEMESTRE), c("#EB0095", "#9D45FD"))
#'     )
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' VariosYears <- ejConsolidadoSaberPro2019         |>
#'   mutate(YEAR = replace(YEAR, YEAR==2019, 2020)) |>
#'   bind_rows(ejConsolidadoSaberPro2019)           |>
#'   filter(Variable == "sede") |> select(-Variable, -desv)
#'
#' Msj <- "\u00c9sta es una descripci\u00f3n de la tabla diferente al valor por default."
#' Tabla(
#'   datos       = VariosYears,
#'   rows        = vars(YEAR, Clase, n),
#'   pivotCat    = Componente,
#'   pivotVar    = Total,
#'   columnNames = c("Año", "Sede", "n", "M\u00e1ximo"),
#'   estadistico = "Max",
#'   encabezado  = "PUNTAJES \u00d7 SEDE",
#'   leyenda     = Msj,
#'   colorHead   = "#F9CA00",
#'   estilo      = list(
#'     list(
#'       columns = "YEAR", target = "cell", fontWeight = "normal",
#'       backgroundColor = styleEqual(unique(VariosYears$YEAR), c("#AEF133", "#19EE9F"))
#'     ),
#'     list(
#'       columns = "Clase", target = "cell", fontWeight = "bold",
#'       color = styleEqual(unique(VariosYears$Clase), c("#42C501", "#7E10DE", "#FF6700", "#0096F2"))
#'     )
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' Tabla(datos = datasets::mtcars)
#'
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
#' Tabla(
#'   datos          = df,
#'   columnNames    = Nombres,
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
#' @examplesIf all(require("tibble"))
#' # library(tibble)
#' # ---------------------------------------------------------------------------
#' # Ejemplo Usando Directamente un Consolidado de Microdatos (Compose functions with Agregar)
#' set.seed(2023)
#' AcademyAwards <- tibble(
#'   year     = sample(1939:1945, 100, TRUE),
#'   season   = sample(1:2, 100, TRUE),
#'   category = sample(
#'     c("Best Picture", "Best Director", "Best Actor", "Best Actress", "Best Sound"),
#'     100, TRUE
#'   ),
#'   location = sample(c("Roosevelt Hotel", "Dolby Theatre", "NBC Century Theatre"), 100, TRUE)
#' )
#' Agregar(
#'   datos      = AcademyAwards,
#'   formula    = category + location ~ year + season,
#'   frecuencia = list("Year" = 1939:1945, "Period" = 1:2)
#' ) %>%
#'   Tabla(., pivotCat = "location", columnNames = c("Year", "Season"),
#'         encabezado = "LOCATION OF CEREMONIES", scrollX = FALSE
#'   )
#'
#' @examplesIf all(FALSE)
#' # library(gt); library(gtExtras)
#' # ---------------------------------------------------------------------------
#' # Ejemplo usando el caso estático (gt)
#' tableGT <- Tabla(
#'   datos       = UnalR::ejConsolidadoGrad |> filter(Variable == "SEDE_NOMBRE_ADM"),
#'   rows        = vars(YEAR),
#'   pivotCat    = Clase,
#'   pivotVar    = Total,
#'   encabezado  = "TOTAL DE ESTUDIANTES \u00d7 SEDE DE GRADUACI\u00d3N",
#'   leyenda     = paste(
#'     "Distribuci\u00f3n de estudiantes graduados ",
#'     "(desde el 2009-I al 2021-I) por sede."
#'   ),
#'   colorHead   = "#8CC63F",
#'   estatico    = TRUE,
#'   estilo      = list(
#'     Tema = 11, Padding = c(0, 0.5), Titulo = "Summary Table:",
#'     Color = list(
#'       list(columns = "YEAR"     , backgroundColor = rainbow(12, alpha = 0.5, rev = TRUE)),
#'       list(columns = "Palmira"  , backgroundColor = "ggsci::red_material"),
#'       list(columns = "Manizales", backgroundColor = "viridis")
#'     )
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' # Ejemplo usando algunos parámetros adicionales de personalización de gt/gtExtras
#' #   (para ver el alcance que puede tener)
#' tableGT <-
#'   Tabla(
#'     datos       = UnalR::ejConsolidadoGrad |> filter(Variable == "SEDE_NOMBRE_ADM"),
#'     rows        = vars(YEAR, SEMESTRE),
#'     pivotCat    = Clase,
#'     pivotVar    = Total,
#'     estadistico = "Suma",
#'     encabezado  = "TOTAL DE ESTUDIANTES \u00d7 SEDE DE GRADUACI\u00d3N",
#'     leyenda     = paste(
#'       "Distribuci\u00f3n de estudiantes graduados ",
#'       "(desde el 2009-I al 2021-I) por sede."
#'     ),
#'     colorHead   = "#AA0000",
#'     estatico    = TRUE,
#'     estilo      = list(
#'       Tema = 14, Padding = c(0, 0.5), Titulo = "SUMMARY TABLE",
#'       Color = list(
#'         list(columns = "YEAR"     , backgroundColor = rainbow(12, alpha = 0.5, rev = TRUE)),
#'         list(columns = "SEMESTRE" , backgroundColor = c("#EB0095", "#9D45FD"))
#'       )
#'     )
#'   )
#'
#' Win <- "<span style=\"color:green\">&#128170;</span>"
#' Loss <- "<span style=\"color:red\">&#128165;</span>"
#' tableGT |>
#'   # __________________ INSERTANDO UN PIE DE PÁGINA ADICIONAL ___________________
#'   tab_source_note(source_note = "Source: Dirección Nacional de Planeación y Estadística (DNPE).") |>
#'   # ___________________ CREANDO UN GRUPO/COLECCIÓN DE FILAS ____________________
#'   tab_row_group(label = "< 2010"       , rows = 1:2)   |>
#'   tab_row_group(label = "[2010 - 2019]", rows = 3:22)  |>
#'   tab_row_group(label = ">= 2020"      , rows = 23:25) |>
#'   # __________ MODIFICANDO LA ALINEACIÓN DE CADA UNA DE LAS COLUMNAS ___________
#'   cols_align(align = "center", columns = Amazonía:Tumaco)  |>
#'   cols_align(align = "left"  , columns = where(is.factor)) |>
#'   # ___________________ COLOREANDO LAS CELDAS DE UNA COLUMNA ___________________
#'   data_color(
#'     columns = Statistic,
#'     method  = "bin",
#'     bins    = c(0, 3000, 4500, 10000),
#'     palette = c("#F44336", "#34AEC6", "#76CF44")
#'   ) |>
#'   # ___________ MODIFICANDO ASPECTOS GENERALES/GLOBALES DE LA TABLA ____________
#'   tab_options(
#'     heading.align = "right", heading.background.color = "#490948",
#'     table.font.size = px(12), heading.title.font.size = px(16)
#'   ) |>
#'   # ______________ CAMBIANDO EL FORMATO DE LOS VALORES NUMÉRICOS _______________
#'   fmt_currency(columns = c(Orinoquía:Palmira), currency = "USD") |>
#'   fmt_percent(columns = Tumaco, decimals = 1) |>
#'   # _ AÑADIENDO ALGUNOS DE LOS ESTILOS PERSONALIZADOS DISPONIBLES A LAS CELDAS _
#'   tab_style(
#'     style = cell_fill(color = "#C90076"), locations = cells_column_spanners()
#'   ) |>
#'   tab_style(
#'     style = list(cell_text(color = "#A5FD45", style = "italic")),
#'     locations = cells_body(columns = SEMESTRE, rows = SEMESTRE == "1")
#'   ) |>
#'   tab_style_body(
#'     style = cell_text(color = "#0CEAC0", weight = "bold"),
#'     columns = Amazonía,
#'     fn = function(x) between(x, 5, 20)
#'   ) |>
#'   text_transform(
#'     fn = function(x) paste(x, Win),
#'     locations = cells_body(columns = "Caribe", rows = Bogotá > 3*Medellín)
#'   ) |>
#'   text_transform(
#'     fn = function(x) paste(x, Loss),
#'     locations = cells_body(columns = "Caribe", rows = Bogotá <  3*Medellín)
#'   ) |>
#'   # __________________________ MODIFICANDO LA FUENTE ___________________________
#'   opt_table_font(
#'     # font = google_font(name = "Merriweather"),
#'     stack = "rounded-sans",
#'     weight = "bolder"
#'   ) |>
#'   # ____________ OPCIONES ADICIONALES CON LIBRERÍAS COMPLEMENTARIAS ____________
#'   gtExtras::gt_highlight_rows(rows = 18, fill = "#FEEF05", font_weight = "bold") |>
#'   gtExtras::gt_add_divider(Bogotá, color = "#F94D00", style = "dotted", weight = px(4)) |>
#'   gtExtras::gt_plt_bar_pct(Medellín, fill = "#2A8A9C", background = "#0DC8A7", scaled = FALSE)
#'
#' # Use el siguiente comando si desea guardar la tabla estática obtenida:
#' # gtsave(tableGT, "TablaResumen.html") # O .tex, docx
#'
#' @export
#'
#' @import DT
#' @import gt
#' @import gtExtras
#' @import dplyr
#' @importFrom htmltools withTags tag
#' @importFrom tidyr pivot_wider
#' @importFrom utils tail
#' @importFrom methods missingArg
Tabla <- function(
    datos, df, rows, pivotCat, pivotVar, columnNames, filtros = FALSE, colFilters,
    estadistico = c("Suma", "Promedio", "Mediana", "Varianza", "SD", "CV", "Min", "Max"),
    encabezado = "Encabezados de los Niveles de la Categor\u00eda", leyenda = "",
    tituloPdf = NULL, mensajePdf = "", ajustarNiveles = TRUE, scrollX = TRUE,
    fillContainer = NULL, colorHead = "#FFFFFF", estilo, estatico = FALSE) {

  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  # Adición temporal (para dar un periodo de adaptación antes de la eliminación del argumento)
  if (!missing(df)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "Tabla(df)",
      with = "Tabla(datos)",
      details = "Please replace the use of argument 'df' with 'datos'. Before the argument is removed."
    )
    datos <- df
  }
  if (!all(is.logical(filtros), is.logical(ajustarNiveles), is.logical(scrollX))) {
    stop("\u00a1Los argumentos 'filtros', 'ajustarNiveles' y 'scrollX' deben ser un booleano (TRUE o FALSE)!", call. = FALSE)
  }
  if (!is.character(colorHead)) {
    stop("\u00a1El argumento 'colorHead' debe ser un car\u00e1cter que indique un color con el nombre ('red'), c\u00f3digo hexadecimal ('#FF0000') o RGB (rgb(1, 0, 0))!", call. = FALSE)
  }
  if (missingArg(tituloPdf)) { tituloPdf <- encabezado }
  if (missingArg(leyenda)) {
    # htmltools::br(htmltools::em(""))
    Leyenda <- NULL
  } else {
    Leyenda <- htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;', htmltools::em(leyenda))
  }
  AjusteNiveles <- ifelse(ajustarNiveles == TRUE, "compact nowrap hover row-border", "display")

  thead <- function(...) { htmltools::tag("thead", ...) }
  th <- function(...) { htmltools::tag("th", ...) }
  tr <- function(...) { htmltools::tag("tr", ...) }

  # ----------------------------------------------------------------------------
  # CREACIÓN DEL DATAFRAME CON EL CUAL SE CREARÁ LA TABLA
  if (all(missingArg(rows), missingArg(pivotCat), missingArg(pivotVar))) {
    DataFrame <- datos %>% mutate_all(., as.factor)
    colNames  <- colnames(datos)

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

    colsDefs <- list(
      list(className = "dt-center", targets = "_all"),
      dots, list(width = "65px", targets = 0)
    )

    flagGeneral <- TRUE
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(colspan = length(colNames), encabezado, class = "dt-center")
        ),
        tr(lapply(colNames, th))
      )
    ))
  } else {
    # Ajuste para detectar cuando lo que se ingresa es un agregado y omitir sintaxis
    if (all(missingArg(rows), !missingArg(pivotCat), missingArg(pivotVar))) {
      datos <- datos |> filter(Variable == pivotCat) |> select(-Variable)
      rows     <- setdiff(colnames(datos), c("Clase", "Total"))
      rows     <- vars(!!!syms(rows))
      pivotCat <- sym("Clase")
      pivotVar <- sym("Total")
    }
    # Ajuste para que independientemente del número de variables a agrupar no se repitan filas
    #   @ Pues la celda correspondiente no puede ser <dbl [n]> sino un valor numérico
    datos <- datos |>
      group_by(!!!vars(!!!rows, {{pivotCat}}), .drop = FALSE) |>
      summarise({{ pivotVar }} := sum({{ pivotVar }}, na.rm = TRUE), .groups = "drop")
      # summarise({{ pivotVar }} := sum({{ pivotVar }}), na.rm = TRUE, .by = c(!!!vars(!!!rows, {{pivotCat}})))
    # Creación de la Tabla Pivoteada de Acuerdo con los Parámetros Ingresados
    DataFrame <- datos |> pivot_wider(names_from = {{ pivotCat }}, values_from = {{ pivotVar }})
    nCat      <- datos |> group_by({{ pivotCat }}) |> distinct({{ pivotCat }})

    if(!missingArg(estadistico)) {
      Statistic <- match.arg(estadistico)
      Groups    <- datos |> group_by(!!!rows, .drop = FALSE)
      addGlobal <- switch(
        Statistic,
        Suma     = Groups |> summarise("Statistic" = sum({{ pivotVar }}   , na.rm = TRUE), .groups = "drop"),
        Promedio = Groups |> summarise("Statistic" = mean({{ pivotVar }}  , na.rm = TRUE), .groups = "drop"),
        Mediana  = Groups |> summarise("Statistic" = median({{ pivotVar }}, na.rm = TRUE), .groups = "drop"),
        Varianza = Groups |> summarise("Statistic" = var({{ pivotVar }}   , na.rm = TRUE), .groups = "drop"),
        SD       = Groups |> summarise("Statistic" = sd({{ pivotVar }}    , na.rm = TRUE), .groups = "drop"),
        CV       = Groups |> summarise("Statistic" = cv({{ pivotVar }}    , na.rm = TRUE), .groups = "drop"),
        Min      = Groups |> summarise("Statistic" = min({{ pivotVar }}   , na.rm = TRUE), .groups = "drop"),
        Max      = Groups |> summarise("Statistic" = max({{ pivotVar }}   , na.rm = TRUE), .groups = "drop")
      )
      # Creación de la Columna Total Global (Total x Fila)
      addGlobal <- addGlobal |> mutate(Statistic = round(Statistic, 2))
      DataFrame <- DataFrame |> left_join(addGlobal)
      nameFlag  <- TRUE
    } else { nameFlag  <- FALSE }
    colsDefs <- list(
      list(className = "dt-center", targets = "_all"),
      list(width = "20px", targets = 0)
    )
    DataFrame <- DataFrame |> mutate_at(rows, factor)
    # Custom Table Container (Nombre de los Encabezados)
    Txt <- ""; j <- 0
    if (!missingArg(columnNames)) {
      if (nameFlag) { lastCol <- tail(columnNames, n = 1); j <- 1 }
      for (i in 1:(length(columnNames)-j)) { Txt <- paste0(Txt, paste0('th(rowspan = 2, "', columnNames[i], '"), ')) }

    } else {
      for (i in seq_len(length(rows))) { Txt <- paste0(Txt, paste0('th(rowspan = 2, "Col', i, '"), ')) }
      if (nameFlag) { lastCol <- "Total" }
    }

    if (nameFlag) {
      txtStatistic <- paste0('th(rowspan = 2, "', lastCol , '")')
    } else {
      txtStatistic <- ""
    }

    TxtFinal <- paste0(
      'htmltools::withTags(table(class = "display",
        thead(
          tr(',
      Txt,
      ' th(colspan = n_groups(nCat), encabezado, class = "dt-center"), ',
      txtStatistic,
      '),
          tr( lapply(nCat |> pull(), th) )
        )
       ))'
    )
    sketch  <- eval(parse(text = TxtFinal))
    Filtros <- "none"; flagGeneral <- FALSE
  }
  # print(sketch)
  # ----------------------------------------------------------------------------
  # CREACIÓN DE LA TABLA A RETORNAR
  if (!estatico) {
    TablaFinal <- datatable(
      DataFrame,
      class      = AjusteNiveles,
      rownames   = FALSE,
      container  = sketch,
      caption    = Leyenda,
      escape     = FALSE,
      filter     = Filtros,
      fillContainer = fillContainer,
      extensions = c("Buttons", "KeyTable"),
      options    = list(
        autoWidth  = TRUE,
        columnDefs = colsDefs,
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

    if (!missingArg(estilo)) {
      for (i in seq_len(length(estilo))) {
        Temp <- do.call(formatStyle, append(list(table = TablaFinal), estilo[[i]]))
        TablaFinal <- Temp
      }
    }
    # if (!missingArg(estilo)) {
    #   formatCols <- function(table, varTXT, list) {
    #     if (is.null(list$color)) { colorFinal <- NULL } else {
    #       colorFinal <- styleEqual(unique(DataFrame[[varTXT]]), list$color)
    #     }
    #     if (is.null(list$background)) { backgFinal <- NULL } else {
    #       backgFinal <- styleEqual(unique(DataFrame[[varTXT]]), list$background)
    #     }
    #     return(
    #       formatStyle(
    #         table = table, varTXT, target = "cell", fontWeight = list$font,
    #         color = colorFinal, backgroundColor = backgFinal
    #       )
    #     )
    #   }
    #   listVars <- as.character(substitute(rows))[-1]
    #   for (i in 1:length(listVars)) {
    #     TablaFinal <- TablaFinal |> formatCols(varTXT = listVars[i], list = estilo[[i]])
    #   }
    # }
  } else {
    if (!missingArg(columnNames)) {
      originalCols <- colnames(DataFrame)
      if (nameFlag) {
        colnames(DataFrame)[seq_len(length(rows))] <- columnNames[1:(length(columnNames)-1)]
        colnames(DataFrame)[length(colnames(DataFrame))] <- tail(columnNames, n = 1)
      } else {
        colnames(DataFrame)[seq_len(length(rows))] <- columnNames
      }
    }

    TablaFinal <- gt(data = DataFrame) |>
      tab_source_note(source_note = leyenda) |>
      fmt_number(columns = everything(), decimals = 0, use_seps = TRUE)

    if (!(missingArg(estilo) || is.null(estilo$Tema))) {
      TablaFinal <- switch(
        estilo$Tema,
        "1"  = opt_stylize(TablaFinal, style = 1, color = "gray"),
        "2"  = opt_stylize(TablaFinal, style = 2, color = "gray"),
        "3"  = opt_stylize(TablaFinal, style = 3, color = "gray"),
        "4"  = opt_stylize(TablaFinal, style = 4, color = "gray"),
        "5"  = opt_stylize(TablaFinal, style = 5, color = "gray"),
        "6"  = opt_stylize(TablaFinal, style = 6, color = "gray"),
        "7"  = gtExtras::gt_theme_538(TablaFinal),
        "8"  = gtExtras::gt_theme_dark(TablaFinal),
        "9"  = gtExtras::gt_theme_dot_matrix(TablaFinal),
        "10" = gtExtras::gt_theme_espn(TablaFinal),
        "11" = gtExtras::gt_theme_excel(TablaFinal),
        "12" = gtExtras::gt_theme_guardian(TablaFinal),
        "13" = gtExtras::gt_theme_nytimes(TablaFinal),
        "14" = gtExtras::gt_theme_pff(TablaFinal)
      )
    }

    if (!(missingArg(estilo) || is.null(estilo$Titulo))) {
      TablaFinal <- TablaFinal |> tab_header(title = estilo$Titulo)
    }
    if (!(missingArg(estilo) || is.null(estilo$Padding))) {
      TablaFinal <- TablaFinal |>
        opt_vertical_padding(scale = estilo$Padding[1]) |>
        opt_horizontal_padding(scale = estilo$Padding[2])
    }
    if (!flagGeneral) {
      TablaFinal <- TablaFinal %>%
        tab_spanner(label = encabezado, columns = one_of(nCat |> pull() |> as.character()))
    }
    TablaFinal <- TablaFinal |> tab_options(column_labels.background.color = colorHead)
    if (!(missingArg(estilo) || is.null(estilo$Color))) {
      for (i in seq_len(length(estilo$Color))) {
        parms <- estilo$Color[[i]]
        TablaFinal <- TablaFinal |> data_color(columns = parms$columns, palette = parms$backgroundColor)
      }
    }
  }

  return(TablaFinal)
}
