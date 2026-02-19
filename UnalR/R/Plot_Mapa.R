#' Cree un widget para visualizar datos geográficos en un mapa interactivo usando
#' el paquete `Leaflet`
#'
#' Esta función está planeada para facilitar la creación de mapas interactivos
#' compatible con plataformas móviles y de escritorio, además de estar diseñada
#' pensando en la simplicidad y el rendimiento. Esta utilidad produce mapas que
#' tienen controles para hacer zoom, desplazarse y alternar capas y puntos entre
#' mostrar y ocultar. Igualmente, permite incrustar mapas en webs, documentos
#' `R Markdown` y aplicaciones `Shiny`. Todo lo anterior basado enteramente en la
#' librería `Leaflet`, la cual es la biblioteca `JavaScript` de código abierto más
#' popular para mapas interactivos.
#'
#' @inheritParams Plot.Series
#' @param datos Un data frame, no un vector numérico.
#' @param df Argument deprecated, use `datos` instead.
#' @param depto Una variable numérica dentro del data frame ingresado en `datos`,
#'   que contiene los códigos de los departamentos, de acuerdo con la codificación
#'   de la División Político-Administrativa de Colombia (*DIVIPOLA*) dispuesta
#'   por el `DANE`.
#' @param mpio Una variable numérica dentro del data frame ingresado en `datos`,
#'   que contiene los códigos de los municipios, de acuerdo con la codificación
#'   de la División Político-Administrativa de Colombia (*DIVIPOLA*) dispuesta
#'   por el `DANE`.
#' @param variable Variable auxiliar con la cual se calculará el estadístico
#'   previamente seleccionado. Para el caso en que la estadística a calcular sea
#'   el conteo no es necesario (*no se usará*) especificar dicha variable numérica.
#' @param agregado Si es `TRUE` (*valor predeterminado*) indica que el data frame
#'   ingresado se deberá agrupar porque no es un consolidado, sino que se cuenta
#'   con los microdatos. Por el contrario, si se especifica en `FALSE` quiere
#'   decir que cada fila es única y se cuenta con el estadístico ya calculado,
#'   además de poder ingresar únicamente uno de los argumentos `depto` o `mpio`.
#' @param zoomIslas Valor booleano que indica si desea realizar un zoom al
#'   archipiélago de San Andrés, Providencia y Santa catalina. El valor a retornar
#'   no será un único plot sino una lista compuesta por dos figuras, una para el
#'   mapa sin las islas y otro únicamente con ellas, para que usted realice
#'   posteriormente la unión de ambos. Aplica únicamente para el caso estático y
#'   su valor por defecto es `FALSE`.
#' @param estadistico Cadena de caracteres que indica el estadístico a graficar
#'   en el mapa. Los valores permitidos son `"Conteo"` (*valor predeterminado*),
#'   `"Promedio"`, `"Mediana"`, `"Varianza"`, `"SD"`, `"CV"`, `"Min"` y `"Max"`.
#' @param tipo Cadena de caracteres que indica el tipo de mapa a graficar. Los
#'   valores permitidos son "Deptos", "SiNoMpios", "Mpios" y "DeptoMpio"
#'   (*valor predeterminado*). Se emparejará parcialmente.
#' @param SiNoLegend Vector de caracteres de longitud 2 que permite editar las
#'   opciones de la etiqueta de la leyenda de los mapas `"SiNoMpios"`. El valor
#'   por defecto es `("0", "1 o más")`.
#' @param titulo Cadena de caracteres indicando la segregación que presenta el
#'   mapa y el periodo al que hace referencia éste, separados por un espacio, por
#'   ejemplo, "Admitidos 2021-I".
#' @param naTo0 Si es `TRUE` (*valor predeterminado*) los valores introducidos
#'   como `NA` (*not available*) se cambiarán por el valor de 0. Ajústelo a `FALSE`
#'   para que no se realice tal cambio y mostrar en la caja de información la
#'   leyenda "Sin Información".
#' @param colNA Cadena de caracteres indicando el color que tendrá la categoría
#'   `NA` (*si esta se presenta*). El valor por defecto es un gris muy claro.
#' @param centroideMapa Cadena de caracteres indicando el departamento que servirá
#'   de centroide al momento de graficar el mapa. El valor por defecto es "CUNDINAMARCA".
#'   Se emparejará parcialmente.
#' @param zoomMapa Valor numérico que indica el nivel de zoom del mapa
#'   (\emph{usado por la función} [setView()][leaflet::setView()]). El valor por
#'   defecto es `6`, entre mayor sea su valor más zoom se aplicará al mapa.
#' @param baldosas Vector de caracteres indicando los mapas base con los que se
#'   realizará el mapa, sean los popularizados por Google Maps o por terceros.
#'   Los valores aceptados son los admitidos por la función
#'   [addProviderTiles()][leaflet::addProviderTiles()], así mismo los valores por
#'   defecto son `c("CartoDB.Positron", "Esri.WorldStreetMap", "Esri.NatGeoWorldMap")`,
#'   algunos otros valores pueden ser:
#'
#'   * "Esri.DeLorme"
#'   * "Esri.WorldTerrain"
#'   * "Esri.WorldShadedRelief"
#'   * "Esri.WorldPhysical"
#'   * "Esri.OceanBasemap"
#'   * "Esri.WorldGrayCanvas"
#'   * "Esri.WorldImagery"
#'   * "Stamen.Toner"
#'   * "Stamen.TonerLite"
#'   * "Stamen.TonerLines"
#'   * "Stamen.Watercolor"
#'   * "Stamen.TonerHybrid"
#'
#'   La lista completa la puede consultar [aquí](http://leaflet-extras.github.io/leaflet-providers/preview/index.html)
#' @param cortes Vector numérico indicando los cortes con los cuales se crearán
#'   los intervalos. No aplica para el tipo de mapa "SiNoMpios", pues este es
#'   binario. Para el tipo de mapa "DeptoMpio" se debe pasar una lista de la
#'   siguiente manera `list(Deptos = c(), Mpios = c())`, pues requiere dos cortes,
#'   uno para departamentos y otro para municipios.
#' @param colores Vector de caracteres indicando los colores para cada uno de los
#'   intervalos con los que cuenta el mapa. Si no se introduce algún vector, se
#'   usará una paleta predeterminada dependiendo del tipo de mapa.
#' @param showSedes Si es `TRUE` (*valor predeterminado*) en el control de capas
#'   (*usado en la función* [addLayersControl()][leaflet::addLayersControl()])
#'   aparecerá el grupo destinado a mostrar o no la ubicación de las distintas
#'   sedes de la Universidad Nacional de Colombia. Ajústelo a `FALSE` para que en
#'   el control de capas no aparezca dicha opción.
#' @param colSedes Vector de caracteres (*de longitud 9*) indicando los colores
#'   del icono de ubicación de las distintas sedes de la Universidad Nacional de
#'   Colombia. Los colores permitidos son los que acepta la función
#'   [makeAwesomeIcon()][leaflet::makeAwesomeIcon()], es decir, "red", "darkred",
#'   "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue",
#'   "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "white",
#'   "gray", "lightgray", "black".
#' @param opacidad Un número entre \eqn{[0, 1]} que indica la opacidad de las capas.
#' @param colBorde Cadena de caracteres indicando el color del borde de los
#'   polígonos al momento de pasar el cursor sobre él.
#' @param compacto Si es `TRUE` (*valor predeterminado*) el control de capas se
#'   representará como un icono que se expande cuando se coloca el cursor sobre
#'   él. Ajústelo a `FALSE` para que el control de capas siempre aparezca en su
#'   estado expandido.
#' @param textSize Valor numérico que indica el tamaño del texto de las etiquetas
#'   de los municipios. El valor para los departamentos será \eqn{+2px}.
#' @param limpio Si es `FALSE` (*valor predeterminado*) se mostrará el MiniMapa,
#'   la barra de escala y los botones para ver en pantalla completa, retornar zoom
#'   y localización. Ajústelo a `TRUE` si desea omitir dichas herramientas adicionales
#'   al mapa.
#' @param estilo Lista compuesta por varios parámetros, los cuales van a ser usados
#'   para graficar el mapa **estático** y cuyo objetivo es personalizar pequeños
#'   detalles de éste.
#'   * `anchoBorde`: Número decimal que indica el ancho de la línea de contorno
#'     de los polígonos del mapa. El valor por defecto es `0.5`.
#'   * `labelX`, `labelY`: Cadena de caracteres indicando la etiqueta del eje
#'     respectivo. El valor por defecto es `"Longitud"` y `"Latitud"` respectivamente.
#'   * `xlim`, `ylim`: Vector numérico que especifica los límites de los ejes respectivos.
#'   * `Legend`: Igual uso que `gg.Legend` en [Plot.Series()]
#'   * `Labs`: Igual uso que `gg.Texto` en [Plot.Series()]. Con la adición del
#'     argumento `fill`, el cual especifica el título de leyenda. El valor por
#'     defecto es `"Statistic"`. Si es `NULL`, se omitirá dicho título.
#'   * `Theme`: Igual uso que `gg.Tema` en [Plot.Series()]
#'   * `Style`: Cadena de caracteres que indica el tipo de mapa a graficar.
#'     Los valores permitidos son `"Intervalo"`, `"SiNo"`, `"Calor"` y `NA`
#'     (*valor predeterminado*). Se emparejará parcialmente.
#'   * `Text`: Lista que especifica aspectos como el color y tamaño de los títulos
#'     de los polígonos. Para más información, consulte la función [geom_sf_text()][ggplot2::geom_sf_text()].
#'   * `scaleX`, `scaleY`: Vector numérico que especifica los puntos o cortes a
#'     graficar en dicho eje. Sacado de la función [scale_x_continuous()][ggplot2::scale_x_continuous()].
#' @param ... Abc.
#'
#' @details
#' Los vectores `depto` y `mpio` introducidos hacen referencia a atributos atómicos,
#' es decir, la pareja formada por `(depto, mpio)` debe corresponder a un individuo.
#' En los argumentos no se acepta la entrada de objetos espaciales o polígonos.
#'
#' @returns
#' Retorna el mapa (*objeto widget de HTML*) creado mediante `Leaflet`, el cual
#' pertenece a la clase "leaflet" y "htmlwidget".
#'
#' @examplesIf require("dplyr")
#' # library(dplyr)
#' df <- ejGraduados |> filter(YEAR == 2021) |>
#'   select(
#'     COD_DEP_NAC, COD_CIU_NAC, DEP_NAC, CIU_NAC, LON_CIU_NAC, LAT_CIU_NAC
#'   )
#' Plot.Mapa(
#'   datos    = df,
#'   depto    = COD_DEP_NAC,
#'   mpio     = COD_CIU_NAC,
#'   tipo     = "SiNoMpios",
#'   titulo   = "Graduados 2021-I",
#'   baldosas = c(
#'     "Esri.WorldPhysical", "Esri.DeLorme", "Esri.WorldShadedRelief",
#'     "Esri.WorldTerrain", "Esri.OceanBasemap"
#'   ),
#'   colores  = c("#10F235", "#00BCB5"),
#'   colSedes = rep("green", 9),
#'   opacidad = 0.6,
#'   colBorde = "#FF4D00",
#'   compacto = FALSE,
#'   textSize = 16,
#'   limpio   = TRUE
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Mapa(
#'   datos  = df,
#'   depto  = COD_DEP_NAC,
#'   mpio   = COD_CIU_NAC,
#'   tipo   = "DeptoMpio",
#'   titulo = "Graduados 2021-I",
#'   cortes = list(
#'     Deptos = c(0, 10, 20, 50, 500, Inf), Mpios = c(0, 1, 5, 10, 100, Inf)
#'   ),
#'   colores = list(
#'     Deptos = c("#6812F2", "#5769F6", "#F6ED0D", "#EE6115", "#EC2525"),
#'     Mpios  = c("#E7F15D", "#ACBD37", "#E15E32", "#A82743", "#5C323E")
#'   )
#' )
#' @examplesIf all(FALSE) && require("dplyr") && require("magrittr")
#' # ---------------------------------------------------------------------------
#' # library(dplyr); library(magrittr)
#' ejSaberPro2020 %>%
#'   select(
#'     Code_Dept = COD_DEP_NAC,
#'     Code_Mun  = COD_CIU_NAC,
#'     Edad      = EDAD_MOD,
#'     PBM       = PBM_ORIG,
#'     ScoreGlobal   = PUNTAJE_GLOBAL,
#'     ScoreCompCiud = PUNT_COMP_CIUD,
#'     ScoreComuEscr = PUNT_COMU_ESCR,
#'     ScoreIngles   = PUNT_INGLES,
#'     ScoreLectCrit = PUNT_LECT_CRIT,
#'     ScoreRazCuant = PUNT_RAZO_CUANT
#'   ) %$%
#'   Plot.Mapa(
#'     datos         = .,
#'     depto         = Code_Dept,
#'     mpio          = Code_Mun,
#'     estadistico   = "Mediana",
#'     variable      = ScoreGlobal,
#'     tipo          = "DeptoMpio",
#'     titulo        = "P.Global 2020",
#'     naTo0         = FALSE,
#'     colNA         = "#472985",
#'     centroideMapa = "ANTIOQUIA",
#'     zoomMapa      = 8,
#'     cortes        = list(
#'       Deptos = c(0, 155, 170, 180, 185, Inf), Mpios = c(0, 50, 178, 200, 250, Inf)
#'     ),
#'     colores       = list(
#'       Deptos = c("#FF7D5A", "#FDBD7D", "#E5DF73", "#63D2A8", "#0055A1"),
#'       Mpios  = c("#E7F15D", "#ACBD37", "#E15E32", "#A82743", "#5C323E")
#'     ),
#'     showSedes     = FALSE
#'   )
#' @examplesIf all(FALSE) && require("cowplot")
#' # ---------------------------------------------------------------------------
#' # Ejemplo usando el caso estático (ggplot2)
#' # library(cowplot)
#' Plot.Mapa(
#'   datos     = df,
#'   depto     = COD_DEP_NAC,
#'   mpio      = COD_CIU_NAC,
#'   zoomIslas = TRUE,
#'   tipo      = "Mpios",
#'   titulo    = "N\u00daM. DE GRADUADOS \u00d7 MUNICIPIO",
#'   colNA     = "#4ACB46",
#'   cortes    = c(0, 1, 10, Inf),
#'   colores   = c("#009CC8", "#EE6115", "#EC2525"),
#'   opacidad  = 0.6,
#'   estatico  = TRUE,
#'   estilo    = list(
#'     Style = "Intervalo", Theme = 2, anchoBorde = 0.5,
#'     Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
#'     Labs   = list(subtitle = "A\u00f1o 2021", caption = "(*) Lugar de Nacimiento", tag = "\u00ae")
#'   )
#' ) -> listMaps
#' # library(cowplot); library(ggplot2)
#' # |    Tema    |             Mapa             |   y  | width |
#' # |:----------:|:----------------------------:|:----:|:-----:|
#' # | 1:6, 10:11 |          San Andrés          | 0.36 |  0.06 |
#' # |            | Providencia y Santa Catalina | 0.39 | 0.055 |
#' # |      9     |             ° ° °            | 0.36 |  0.07 |
#' # |            |             * * *            | 0.39 | 0.065 |
#' # |    12:14   |             ° ° °            | 0.33 |  0.11 |
#' # |            |             * * *            | 0.36 |  0.10 |
#' ggdraw() +
#'   draw_plot(listMaps$M_COL) +
#'   draw_plot(listMaps$M_SanAndres  , x = 0.27, y = 0.36, width = 0.060) +
#'   draw_plot(listMaps$M_Providencia, x = 0.33, y = 0.39, width = 0.055)
#' # ggplot2::ggsave("COL.png", width = 12, height = 10, dpi = 550)
#' @examplesIf all(FALSE) && require("tibble") && require("cowplot")
#' # ---------------------------------------------------------------------------
#' # library(tibble)
#' PIB <- tibble(
#'   DIVIPOLA = c(91,05,81,08,11,13,15,17,18,85,19,20,27,23,25,94,95,
#'                41,44,47,50,52,54,86,63,66,88,68,70,73,76,97,99),
#'   Valor    = c(883,177837,6574,52961,301491,NA,31208,19782,4718,17810,21244,
#'                23244,5069,20842,73592,443,943,19837,14503,16370,41923,18259,
#'                18598,4253,9837,19531,1711,74737,9842,25143,116238,337,799)
#' )
#' Plot.Mapa(
#'   datos     = PIB,
#'   depto     = DIVIPOLA,
#'   variable  = Valor,
#'   agregado  = FALSE,
#'   zoomIslas = TRUE,
#'   tipo      = "Deptos",
#'   naTo0     = FALSE,
#'   colNA     = "#543619",
#'   titulo    = "PIB \u00d7 DEPARTAMENTO",
#'   cortes    = c(0, 500, 5000, 20000, 30000, Inf),
#'   colores   = c("#FED600", "#02D46E", "#006389", "#FA006E", "#FC553C"),
#'   colBorde  = "#3A0F2D",
#'   estatico  = TRUE,
#'   textSize  = 0,
#'   estilo    = list(
#'     Style = "Intervalo", Theme = 7,
#'     Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
#'     Labs = list(
#'       fill = "PIB", subtitle = "A Precios Corrientes",
#'       caption = "Para el periodo de 2021P (en miles de millones de pesos)"
#'     )
#'     # scaleX = seq(-82, -64, by = 1), scaleY = seq(-4, 14, by = 1)
#'   )
#' ) -> listMaps
#' # Caso 1:
#' #   Dejando solamente la isla principal (San Andrés) y omitiendo
#' #   las contiguas (Providencia y Santa Catalina)
#' ggdraw() +
#'   draw_plot(listMaps$M_COL) +
#'   draw_plot(listMaps$M_SanAndres, x = 0.26, y = 0.8, width = 0.4)
#' # ggsave("COL.png", width = 12, height = 10, dpi = 550)
#' # Caso 2:
#' #   Conservando todo el departamento y sus distancias reales (no se modifica su polígono espacial)
#' ggdraw() +
#'   draw_plot(listMaps$M_COL) +
#'   draw_plot(listMaps$M_SanAndres, x = 0.27, y = 0.35, width = 0.08)
#' # ggsave("COL.png", width = 12, height = 10, dpi = 550)
#' @examplesIf all(FALSE)
#' # ---------------------------------------------------------------------------
#' AreaVichada <- tibble(
#'   MunCode = c(99001, 99524, 99624, 99773), Area = c(12409, 20141, 2018, 65674)
#' )
#' Plot.Mapa(
#'   datos    = AreaVichada,
#'   mpio     = MunCode,
#'   variable = Area,
#'   agregado = FALSE,
#'   tipo     = "Mpios",
#'   titulo   = "\u00c1REA DE LOS MUNICIPIOS DEL\nDEPARTAMENTO DE VICHADA",
#'   naTo0    = FALSE,
#'   centroideMapa = "VICHADA",
#'   estatico = TRUE,
#'   estilo   = list(
#'     Style = "Calor", Theme = 9,
#'     Labs  = list(caption = "(*) En Metros Cuadrados"),
#'     Text  = list(color = "#011532", size = 3)
#'   )
#' )
#'
#' @export
#'
#' @import leaflet
#' @import dplyr
#' @importFrom leaflet.extras addFullscreenControl addSearchFeatures searchFeaturesOptions
#' @importFrom tidyr replace_na
#' @importFrom stringr str_to_title
#' @importFrom htmltools HTML
#' @importFrom stats median sd var
#' @importFrom methods missingArg
#' @importFrom sp coordinates
#' @importFrom sf st_point_on_surface st_as_sf
#' @importFrom lifecycle deprecate_warn
Plot.Mapa <- function(
  datos,
  df,
  depto,
  mpio,
  variable,
  agregado = TRUE,
  zoomIslas = FALSE,
  estadistico = c(
    "Conteo",
    "Promedio",
    "Mediana",
    "Varianza",
    "SD",
    "CV",
    "Min",
    "Max"
  ),
  tipo = c("Deptos", "SiNoMpios", "Mpios", "DeptoMpio"),
  SiNoLegend,
  titulo,
  naTo0 = TRUE,
  colNA = "#EEEEEE",
  centroideMapa,
  zoomMapa = 6,
  baldosas,
  cortes,
  colores,
  showSedes = TRUE,
  colSedes,
  opacidad = 0.7,
  colBorde,
  compacto = TRUE,
  textSize = 10,
  limpio = FALSE,
  estatico = FALSE,
  estilo,
  ...
) {
  # ================== COMANDOS DE VERIFICACIÓN Y VALIDACIÓN ===================
  if (missingArg(datos) && missingArg(df)) {
    stop(
      '[\u00d7] Por favor, introduzca el dataframe que contiene la informaci\u00f3n necesaria.',
      call. = FALSE
    )
  }
  # Adición temporal (para dar un periodo de adaptación antes de la eliminación del argumento)
  if (missingArg(datos) && !missingArg(df)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "Plot.Mapa(df)",
      with = "Plot.Mapa(datos)",
      details = "Please replace the use of argument 'df' with 'datos'. Before the argument is removed."
    )
    datos <- df
  }

  tipo <- tolower(tipo)
  if (tipo %NotIN% c("deptos", "sinompios", "mpios", "deptompio")) {
    stop(
      '[\u00d7] Por favor, introduzca un tipo de mapa v\u00e1lido. Las opciones son: "Deptos", "SiNoMpios", "Mpios" y "DeptoMpio".',
      call. = FALSE
    )
  }
  if (tipo == "sinompios") {
    if (!missingArg(SiNoLegend)) {
      if (length(SiNoLegend) != 2L) {
        stop(
          '[\u00d7] La longitud del argumento "SiNoLegend" debe ser un vector con 2 elementos.',
          call. = FALSE
        )
      }
      YN_Legend <- SiNoLegend
    } else {
      YN_Legend <- c("0 ", "1 o m\u00e1s ")
    }
  }

  if (agregado) {
    if (missingArg(depto) && missingArg(mpio)) {
      stop(
        '[\u00d7] Por favor, introduzca tanto el nombre de la columna "depto" como el nombre de la columna "mpio".',
        call. = FALSE
      )
    }
    Statistic <- match.arg(estadistico)
  } else {
    if (missingArg(depto) && missingArg(mpio)) {
      stop(
        '[\u00d7] Por favor, introduzca el nombre de la columna "depto" o "mpio".',
        call. = FALSE
      )
    }
    if (missingArg(depto) && tipo == "deptos") {
      stop(
        '[\u00d7] Por favor, introduzca el nombre de la columna "depto" para su mapa estilo "Deptos".',
        call. = FALSE
      )
    }
    if (missingArg(mpio) && tipo == "mpios") {
      stop(
        '[\u00d7] Por favor, introduzca el nombre de la columna "mpio" para su mapa estilo "Mpios".',
        call. = FALSE
      )
    }
    if (missingArg(mpio) && tipo == "sinompios") {
      stop(
        '[\u00d7] Por favor, introduzca el nombre de la columna "mpio" para su mapa estilo "SiNoMpios".',
        call. = FALSE
      )
    }
    if (tipo == "deptompio") {
      stop(
        '[\u00d7] No tiene sentido utilizar este tipo de mapa con datos agregados, ya que se necesita el microdato para que el estad\u00edstico sea preciso.',
        call. = FALSE
      )
    }
    if (
      !missingArg(estadistico) &&
        (missingArg(estilo) || is.null(estilo$labelCursor))
    ) {
      warning(
        '\u25ba \u00a1El valor ingresado en "estadistico" ser\u00e1 omitido! Puede especificar el significado de la variable ingresada en el argumento "estilo$labelCursor".',
        call. = FALSE
      )
    }
    Statistic <- "Max"
  }

  if (Statistic == "Conteo") {
    dataframe <- datos |>
      select(codeDept := {{ depto }}, codeMun := {{ mpio }}) |>
      filter(!is.na(codeDept))
    if (!missingArg(variable)) {
      warning(
        paste0(
          '\u25ba \u00a1Para el estad\u00edstico "Conteo" no es necesario especificar una variable para el c\u00e1lculo! La variable',
          ' "',
          deparse(substitute(variable)),
          '" ingresada ser\u00e1 omitida.'
        ),
        call. = FALSE
      )
    }
  } else {
    if (missingArg(variable)) {
      stop(
        paste0(
          '[\u00d7] Por favor, ingrese una variable auxiliar para calcular el/la ',
          tolower(Statistic),
          '.'
        ),
        call. = FALSE
      )
    } else if (!is.numeric(datos |> select({{ variable }}) |> pull())) {
      stop(
        '[\u00d7] La variable auxiliar ingresada contiene valores no num\u00e9ricos.',
        call. = FALSE
      )
    }
    if (agregado) {
      dataframe <- datos |>
        select(
          codeDept := {{ depto }},
          codeMun := {{ mpio }},
          Variable := {{ variable }}
        ) |>
        filter(!is.na(codeDept))
    } else {
      if (missingArg(depto)) {
        dataframe <- datos |>
          select(
            codeDept := {{ mpio }},
            codeMun := {{ mpio }},
            Variable := {{ variable }}
          ) |>
          filter(!is.na(codeMun))
      } else if (missingArg(mpio)) {
        dataframe <- datos |>
          select(
            codeDept := {{ depto }},
            codeMun := {{ depto }},
            Variable := {{ variable }}
          ) |>
          filter(!is.na(codeDept))
      } else {
        warning(
          '\u25ba \u00a1Se han proporcionado ambos valores ("depto" y "mpio"), pero solo se utilizar\u00e1 uno de ellos!',
          call. = FALSE
        )
        dataframe <- datos |>
          select(
            codeDept := {{ depto }},
            codeMun := {{ mpio }},
            Variable := {{ variable }}
          ) |>
          filter(!is.na(codeDept), !is.na(codeMun))
      }
    }
  }

  if (!missingArg(titulo)) {
    if (!is.character(titulo)) {
      stop(
        '[\u00d7] El argumento "titulo" debe ser una cadena de texto.',
        call. = FALSE
      )
    }
  } else {
    titulo <- "\u00bf ?"
  }
  if (missingArg(baldosas)) {
    Baldosas <- c(
      "CartoDB.Positron",
      "Esri.WorldStreetMap",
      "Esri.NatGeoWorldMap"
    )
    Baldosas.names <- c(
      "Ligero",
      "Street",
      "Sat\u00e9lite<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;NatGeo"
    )
  } else {
    Baldosas <- Baldosas.names <- baldosas
  }
  if (!missingArg(colSedes)) {
    if (length(colSedes) != 9L) {
      stop(
        paste0(
          '[\u00d7] La cantidad de colores en el vector "colSedes" no corresponde con la cantidad de sedes presentes.',
          '\n\t - Ingrese un vector con 9 colores correspondientes a las sedes:',
          '\n\t -- Medell\u00edn, Bogot\u00e1, Manizales, De La Paz, Tumaco, Palmira, Orinoqu\u00eda, Caribe y Amazonas, respectivamente.'
        ),
        call. = FALSE
      )
    } else {
      Icons_Col <- colSedes
    }
  } else {
    Icons_Col <- c(
      "orange",
      "green",
      "darkblue",
      "purple",
      "gray",
      "darkpurple",
      "lightred",
      "red",
      "blue"
    )
  }
  if (!(is.numeric(zoomMapa) && is.numeric(opacidad) && is.numeric(textSize))) {
    stop(
      '[\u00d7] Los argumentos "zoomMapa", "opacidad" y "textSize" deben ser valores num\u00e9ricos.',
      call. = FALSE
    )
  }
  opacidadClic <- ifelse(opacidad != 1, opacidad + 0.2, opacidad)
  textDeptos <- paste0(textSize + 2, "px")
  textMpios <- paste0(textSize, "px")
  if (!missing(colBorde)) {
    if (!is.character(colBorde)) {
      stop(
        '[\u00d7] El argumento "colBorde" debe ser un car\u00e1cter que indique un color con el nombre ("red"), c\u00f3digo hexadecimal ("#FF0000") o RGB (rgb(255, 0, 0)).',
        call. = FALSE
      )
    }
  }
  if (
    !(is.logical(naTo0) &&
      is.logical(showSedes) &&
      is.logical(compacto) &&
      is.logical(limpio))
  ) {
    stop(
      '[\u00d7] Los argumentos "naTo0", "showSedes", "compacto" y "limpio" deben ser booleanos (TRUE o FALSE).',
      call. = FALSE
    )
  }

  Mensaje <- paste0(
    '[\u00d7] El argumento "colores" no tiene la longitud correcta en relaci\u00f3n con el argumento "cortes" ingresado, o viceversa.',
    '\n\t - Recuerde que si realiza (n) cortes necesita (n-1) colores para cada intervalo creado.',
    '\n\t -- Por ejemplo, si los cortes son c(0, 50, Inf) necesitar\u00e1 2 colores para dichos intervalos.'
  )

  # ============================================================================
  Msj <- 'Map created by <a href="https://github.com/JeisonAlarcon">Jeison Alarc\u00f3n</a> &mdash; Data source: &copy; <a href="http://estadisticas.unal.edu.co/home/">DNPE</a>'
  # codeMun != 25001 -> Cundinamarca no tiene capital, pues Bogotá D.C. está en otro departamento.
  Capitales <- Cabeceras |> filter(Code_Mun %% 1000 == 1, Code_Mun != 25001)

  # Hace referencia al n* con el cual se calculará la estadística, no al n total que puede incluir NA's.
  #   ■ Para el caso del conteo es diferente, pues para calcular este no se requiere de ninguna variable auxiliar.
  if (naTo0) {
    GroupBy_Dept <- dataframe |> group_by(codeDept) %>% replace(., is.na(.), 0)
    GroupBy_Mpio <- dataframe |> group_by(codeMun) %>% replace(., is.na(.), 0)

    Conteo_ByDept <- GroupBy_Dept |> summarise("n" = n())
    Conteo_ByMun <- GroupBy_Mpio |> summarise("n" = n())
  } else {
    GroupBy_Dept <- dataframe |> group_by(codeDept)
    GroupBy_Mpio <- dataframe |> group_by(codeMun)
    if (Statistic != "Conteo") {
      Conteo_ByDept <- GroupBy_Dept |> summarise("n" = sum(!is.na(Variable)))
      Conteo_ByMun <- GroupBy_Mpio |> summarise("n" = sum(!is.na(Variable)))
    } else {
      Conteo_ByDept <- GroupBy_Dept |> summarise("n" = n())
      Conteo_ByMun <- GroupBy_Mpio |> summarise("n" = n())
    }
  }

  Datos_ByDept <- switch(
    Statistic,
    Conteo = GroupBy_Dept |> summarise("Statistic" = n()),
    Promedio = GroupBy_Dept |>
      summarise("Statistic" = mean(Variable, na.rm = TRUE)),
    Mediana = GroupBy_Dept |>
      summarise("Statistic" = median(Variable, na.rm = TRUE)),
    Varianza = GroupBy_Dept |>
      summarise("Statistic" = var(Variable, na.rm = TRUE)),
    SD = GroupBy_Dept |> summarise("Statistic" = sd(Variable, na.rm = TRUE)),
    CV = GroupBy_Dept |> summarise("Statistic" = cv(Variable, na.rm = TRUE)),
    Min = GroupBy_Dept |> summarise("Statistic" = min(Variable, na.rm = TRUE)),
    Max = GroupBy_Dept |> summarise("Statistic" = max(Variable, na.rm = TRUE))
  )
  Datos_ByMun <- switch(
    Statistic,
    Conteo = GroupBy_Mpio |> summarise("Statistic" = n()),
    Promedio = GroupBy_Mpio |>
      summarise("Statistic" = mean(Variable, na.rm = TRUE)),
    Mediana = GroupBy_Mpio |>
      summarise("Statistic" = median(Variable, na.rm = TRUE)),
    Varianza = GroupBy_Mpio |>
      summarise("Statistic" = var(Variable, na.rm = TRUE)),
    SD = GroupBy_Mpio |> summarise("Statistic" = sd(Variable, na.rm = TRUE)),
    CV = GroupBy_Mpio |> summarise("Statistic" = cv(Variable, na.rm = TRUE)),
    Min = GroupBy_Mpio |> summarise("Statistic" = min(Variable, na.rm = TRUE)),
    Max = GroupBy_Mpio |> summarise("Statistic" = max(Variable, na.rm = TRUE))
  )

  # ============================================================================
  # Carga del Departamentos
  Polygons_Depto <- Depto_Final
  Polygons_Depto@data$Statistic <- Polygons_Depto@data |>
    left_join(Datos_ByDept, by = join_by(Code_Dept == codeDept)) |>
    select(Statistic) |>
    pull() |>
    round(3)
  Polygons_Depto@data$n <- Polygons_Depto@data |>
    left_join(Conteo_ByDept, by = join_by(Code_Dept == codeDept)) |>
    select(n) |>
    pull() |>
    round(3)

  # Carga de Municipios
  Polygons_Mpio <- Mpio_Final
  Polygons_Mpio@data$Statistic <- Polygons_Mpio@data |>
    left_join(Datos_ByMun, by = join_by(Code_Mun == codeMun)) |>
    select(Statistic) |>
    pull() |>
    round(3)
  Polygons_Mpio@data$n <- Polygons_Mpio@data |>
    left_join(Conteo_ByMun, by = join_by(Code_Mun == codeMun)) |>
    select(n) |>
    pull() |>
    round(3)

  if (naTo0) {
    Polygons_Depto@data$Statistic <- Poly_Dept_ShowStatistic <- Polygons_Depto@data$Statistic %>%
      replace(., is.na(.), 0)
    Polygons_Depto@data$n <- Poly_Dept_ShowN <- Polygons_Depto@data$n %>%
      replace(., is.na(.), 0)

    Polygons_Mpio@data$Statistic <- Poly_Mpio_ShowStatistic <- Polygons_Mpio@data$Statistic %>%
      replace(., is.na(.), 0)
    Polygons_Mpio@data$n <- Poly_Mpio_ShowN <- Polygons_Mpio@data$n %>%
      replace(., is.na(.), 0)
  } else {
    Poly_Dept_ShowStatistic <- Polygons_Depto@data$Statistic %>%
      replace(., is.na(.), "Sin Informaci\u00f3n")
    Poly_Dept_ShowN <- Polygons_Depto@data$n %>%
      replace(., is.na(.), "Sin Informaci\u00f3n")

    Poly_Mpio_ShowStatistic <- Polygons_Mpio@data$Statistic %>%
      replace(., is.na(.), "Sin Informaci\u00f3n")
    Poly_Mpio_ShowN <- Polygons_Mpio@data$n %>%
      replace(., is.na(.), "Sin Informaci\u00f3n")
  }

  # Corrección de algunos municipios cuyo nombre es homónimo de otro.
  Homonimos <- Polygons_Mpio@data |>
    group_by(Municipio) |>
    summarise(veces = n()) |>
    filter(veces > 1)

  Polygons_Mpio@data <- Polygons_Mpio@data |>
    mutate(
      Municipio = if_else(
        condition = Municipio %in% Homonimos$Municipio,
        true = paste(Municipio, substring(Departamento, 1, 3)),
        false = Municipio
      )
    )

  # ============================================================================
  if (!estatico) {
    centroideMapa <- ifelse(
      missingArg(centroideMapa),
      "CUNDINAMARCA",
      toupper(centroideMapa)
    )
    # ..........................................................................
    # Ubicación de los centroides correspondientes a cada departamento (extraídos del objeto 'Polygons_Depto' y no de 'Capitales')
    #   ■ El centroide de Colombia corresponde al centroide del departamento de Cundinamarca
    NumDept <- length(Polygons_Depto)
    Coordenadas <- sp::coordinates(Polygons_Depto)
    Centroides_Deptos <- data.frame(
      "Departamento" = Polygons_Depto@data$Departamento,
      "Lon" = Coordenadas[, 1],
      "Lat" = Coordenadas[, 2]
    )

    NumMpios <- length(Polygons_Mpio)
    #   ■ Corrección del centroide de los municipios, pues en algunos casos éste se sale del polígono
    Coordenadas <- suppressWarnings(sf::st_point_on_surface(sf::st_as_sf(
      Polygons_Mpio
    )))
    Coordenadas <- data.frame(sf::st_coordinates(Coordenadas))
    Centroides_Mpios <- tibble(
      "Municipio" = Polygons_Mpio@data$Municipio,
      "Lon" = Coordenadas$X,
      "Lat" = Coordenadas$Y
    )

    Centroide_Col <- Centroides_Deptos |> filter(Departamento == centroideMapa)

    # ..........................................................................
    # Filtrar sedes de la Universidad Nacional de Colombia
    Sedes <- Cabeceras |>
      filter(
        Code_Mun %in%
          c(5001, 11001, 17001, 20621, 52835, 76520, 81001, 88001, 91001)
      ) |>
      mutate(
        Sede = c(
          "Medell\u00edn",
          "Bogot\u00e1",
          "Manizales",
          "De La Paz",
          "Tumaco",
          "Palmira",
          "Orinoqu\u00eda",
          "Caribe",
          "Amazonas"
        )
      )
    Icons_Sedes <- makeAwesomeIcon(
      markerColor = Icons_Col,
      iconColor = "white",
      fontFamily = "Leonardian",
      text = "un"
    )

    # Extracción de la segregación y el periodo en cuestión del título ingresado
    TitleSplit <- strsplit(titulo, "\\s+")
    Periodo <- TitleSplit[[1]][2]
    if (agregado) {
      Segregacion <- TitleSplit[[1]][1]
    } else {
      Segregacion <- NULL
    }
    TitleDepto <- paste0(
      "<center>",
      Segregacion,
      " por<br/>departamento<br/>",
      Periodo,
      "</center>"
    )
    TitleMpio <- paste0(
      "<center>",
      Segregacion,
      " por<br/>municipio<br/>",
      Periodo,
      "</center>"
    )

    # Labels
    Labels_Sedes <- sprintf("<strong>%s %s</strong>", "Sede", Sedes$Sede) |>
      lapply(htmltools::HTML)
    if (!agregado || is.null(Statistic)) {
      if (missingArg(estilo) || is.null(estilo$labelCursor)) {
        labelCursor <- NULL
      } else {
        labelCursor <- estilo$labelCursor
      }
      Statistic <- labelCursor
      rawLabels_Deptos <- paste0(
        "<strong> %s </strong> <br>",
        Statistic,
        ": %s"
      )
      rawLabels_Mpios <- paste0(
        "<strong> %s </strong> (%s) <br> %s ",
        Statistic
      )
      if (
        any(
          is.character(Poly_Dept_ShowStatistic),
          is.character(Poly_Mpio_ShowStatistic)
        )
      ) {
        Labels_Deptos <- sprintf(
          rawLabels_Deptos,
          Polygons_Depto@data$Departamento,
          Poly_Dept_ShowStatistic
        ) |>
          lapply(htmltools::HTML)
        Labels_Mpios <- sprintf(
          rawLabels_Mpios,
          Polygons_Mpio@data$Municipio,
          Polygons_Mpio@data$Departamento,
          Poly_Mpio_ShowStatistic
        ) |>
          lapply(htmltools::HTML)
      } else {
        Labels_Deptos <- sprintf(
          rawLabels_Deptos,
          Polygons_Depto@data$Departamento,
          scales::number_format(
            accuracy = 0.02,
            big.mark = ".",
            decimal.mark = ",",
            scale_cut = c(B = 10^9, T = 10^12)
          )(Poly_Dept_ShowStatistic)
        ) |>
          lapply(htmltools::HTML)
        Labels_Mpios <- sprintf(
          rawLabels_Mpios,
          Polygons_Mpio@data$Municipio,
          Polygons_Mpio@data$Departamento,
          scales::number_format(
            accuracy = 0.02,
            big.mark = ".",
            decimal.mark = ",",
            scale_cut = c(B = 10^9, T = 10^12)
          )(Poly_Mpio_ShowStatistic)
        ) |>
          lapply(htmltools::HTML)
      }
    } else {
      if (
        any(
          is.character(Poly_Dept_ShowStatistic),
          is.character(Poly_Mpio_ShowStatistic)
        )
      ) {
        if (Statistic != "Conteo") {
          Labels_Deptos <- sprintf(
            paste0("<strong> %s </strong> <br>", Statistic, ": %s <br> n: %s"),
            Polygons_Depto@data$Departamento,
            Poly_Dept_ShowStatistic,
            Poly_Dept_ShowN
          ) |>
            lapply(htmltools::HTML)
          Labels_Mpios <- sprintf(
            paste0(
              "<strong> %s </strong> (%s) <br>",
              Statistic,
              ": %s <br> n: %s"
            ),
            Polygons_Mpio@data$Municipio,
            Polygons_Mpio@data$Departamento,
            Poly_Mpio_ShowStatistic,
            Poly_Mpio_ShowN
          ) |>
            lapply(htmltools::HTML)
        } else {
          Labels_Deptos <- sprintf(
            paste0("<strong> %s </strong> <br> %s ", tolower(Segregacion)),
            Polygons_Depto@data$Departamento,
            Poly_Dept_ShowStatistic
          ) |>
            lapply(htmltools::HTML)
          Labels_Mpios <- sprintf(
            paste0("<strong> %s </strong> (%s) <br> %s ", tolower(Segregacion)),
            Polygons_Mpio@data$Municipio,
            Polygons_Mpio@data$Departamento,
            Poly_Mpio_ShowStatistic
          ) |>
            lapply(htmltools::HTML)
        }
      } else {
        if (Statistic != "Conteo") {
          Labels_Deptos <- sprintf(
            paste0("<strong> %s </strong> <br>", Statistic, ": %s <br> n: %s"),
            Polygons_Depto@data$Departamento,
            scales::number_format(
              accuracy = 0.02,
              big.mark = ".",
              decimal.mark = ",",
              scale_cut = c(B = 10^9, T = 10^12)
            )(Poly_Dept_ShowStatistic),
            Poly_Dept_ShowN
          ) |>
            lapply(htmltools::HTML)
          Labels_Mpios <- sprintf(
            paste0(
              "<strong> %s </strong> (%s) <br>",
              Statistic,
              ": %s <br> n: %s"
            ),
            Polygons_Mpio@data$Municipio,
            Polygons_Mpio@data$Departamento,
            scales::number_format(
              accuracy = 0.02,
              big.mark = ".",
              decimal.mark = ",",
              scale_cut = c(B = 10^9, T = 10^12)
            )(Poly_Mpio_ShowStatistic),
            Poly_Mpio_ShowN
          ) |>
            lapply(htmltools::HTML)
        } else {
          Labels_Deptos <- sprintf(
            paste0("<strong> %s </strong> <br> %s ", tolower(Segregacion)),
            Polygons_Depto@data$Departamento,
            scales::number_format(
              big.mark = ".",
              decimal.mark = ",",
              scale_cut = c(B = 10^9, T = 10^12)
            )(Poly_Dept_ShowStatistic)
          ) |>
            lapply(htmltools::HTML)
          Labels_Mpios <- sprintf(
            paste0("<strong> %s </strong> (%s) <br> %s ", tolower(Segregacion)),
            Polygons_Mpio@data$Municipio,
            Polygons_Mpio@data$Departamento,
            scales::number_format(
              big.mark = ".",
              decimal.mark = ",",
              scale_cut = c(B = 10^9, T = 10^12)
            )(Poly_Mpio_ShowStatistic)
          ) |>
            lapply(htmltools::HTML)
        }
      }
    }

    if (tipo == "deptos") {
      # MAPA POR DEPARTAMENTOS
      if (missingArg(colBorde)) {
        colBorde <- "#5A0028"
      }
      if (!(missingArg(cortes) && missingArg(colores))) {
        if (length(colores) != length(cortes) - 1L) {
          stop(Mensaje, call. = FALSE)
        } else {
          Colors <- colores
          Cortes <- cortes
        }
      } else {
        Colors <- c("#E1F5C4", "#EDE574", "#F9D423", "#FC913A", "#FF4E50")
        Cortes <- c(0, 20, 50, 200, 1000, Inf)
      }
      pal <- colorBin(Colors, bins = Cortes, na.color = colNA)

      Mapa <- leaflet(data = Polygons_Depto) |> addTiles(attribution = Msj)
      for (i in seq_len(length(Baldosas))) {
        Mapa <- Mapa |>
          addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
      }

      Mapa <- Mapa |>
        # Definiendo el centro del mapa.
        setView(
          lat = Centroide_Col$Lat,
          lng = Centroide_Col$Lon,
          zoom = zoomMapa
        ) |>
        # Adición de grupos de control de capas.
        addLayersControl(
          baseGroups = Baldosas.names,
          options = layersControlOptions(collapsed = compacto)
        ) |>
        # Adición de los polígonos, su relleno y etiquetas con el nombre y número de graduados.
        addPolygons(
          stroke = TRUE,
          fillColor = ~ pal(Statistic),
          weight = 2,
          opacity = opacidad,
          color = "#005A32",
          dashArray = "",
          fillOpacity = opacidad,
          highlightOptions = list(
            weight = 4,
            color = colBorde,
            dashArray = "",
            fillOpacity = opacidadClic,
            bringToFront = TRUE
          ),
          label = Labels_Deptos,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~Statistic,
          opacity = opacidadClic,
          labFormat = labelFormat(prefix = "[", suffix = ")"),
          title = titulo
        ) |>
        # Adición de los textos indicando el departamento y su respectivo municipio capital.
        addLabelOnlyMarkers(
          lng = Centroides_Deptos$Lon,
          lat = Centroides_Deptos$Lat,
          label = paste0(sapply(Centroides_Deptos$Departamento, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            textsize = textDeptos
          )
        ) |>
        addLabelOnlyMarkers(
          lng = Capitales$Longitud,
          lat = Capitales$Latitud,
          label = paste0(sapply(Capitales$Municipio, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(
            noHide = TRUE,
            textOnly = TRUE,
            textsize = textMpios
          )
        ) |>
        # Adición del botón de control de búsqueda (lupa).
        addSearchFeatures(
          targetGroups = "lupa",
          options = searchFeaturesOptions(
            zoom = 8,
            openPopup = TRUE,
            textErr = "Ubicaci\u00f3n No Encontrada",
            textCancel = "Cancelar",
            textPlaceholder = "Buscar...",
            position = "topleft",
            hideMarkerOnCollapse = FALSE
          )
        ) |>
        # Adición de los marcadores circulares para las capitales de los departamentos.
        addCircleMarkers(
          lng = Capitales$Longitud,
          lat = Capitales$Latitud,
          radius = 2,
          stroke = TRUE,
          color = "#D95F02",
          fill = TRUE,
          fillColor = "orangelight",
          fillOpacity = 0.9
        )
    } else if (tipo == "sinompios") {
      # MAPA POR MUNICIPIO (SI/NO)
      if (missingArg(colBorde)) {
        colBorde <- "#16ABAB"
      }
      if (!missingArg(cortes)) {
        warning(
          paste0(
            '\u25ba \u00a1Se ha proporcionado un valor para el argumento "cortes" y se ha seleccionado el tipo de mapa "SiNoMpios"!',
            '\n\t El valor ingresado ser\u00e1 omitido, ya que este tipo de mapa no permite un cambio en los cortes debido a que utiliza un intervalo binario.'
          ),
          call. = FALSE
        )
      }
      if (!missingArg(colores)) {
        if (length(colores) != 2L) {
          stop(
            '[\u00d7] La longitud del argumento "colores" es diferente a 2. Este mapa es binario, por lo cual necesita especificar exactamente 2 colores.',
            call. = FALSE
          )
        } else {
          colBinary <- colores
        }
      } else {
        colBinary <- c("#FDAE61", "#A6D96A")
      }
      Pal_Binary <- colorBin(
        palette = colBinary,
        bins = c(0, 1, Inf),
        na.color = colNA
      )

      Mapa <- leaflet(data = Polygons_Mpio) |> addTiles(attribution = Msj)
      for (i in seq_len(length(Baldosas))) {
        Mapa <- Mapa |>
          addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
      }

      Mapa <- Mapa |>
        setView(
          lat = Centroide_Col$Lat,
          lng = Centroide_Col$Lon,
          zoom = zoomMapa
        ) |>
        addLayersControl(
          baseGroups = Baldosas.names,
          options = layersControlOptions(collapsed = compacto)
        ) |>
        addPolygons(
          stroke = TRUE,
          fillColor = ~ Pal_Binary(Statistic),
          weight = 1,
          opacity = opacidad,
          color = "gray",
          dashArray = "3",
          fillOpacity = opacidad,
          highlightOptions = list(
            weight = 4,
            color = colBorde,
            dashArray = "",
            fillOpacity = opacidadClic,
            bringToFront = TRUE
          ),
          label = Labels_Mpios,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) |>
        addPolylines(
          data = Polygons_Depto,
          stroke = TRUE,
          color = "white",
          weight = 2,
          smoothFactor = 0.05
        ) |>
        addLegend(
          position = "bottomright",
          values = ~Statistic,
          bins = c(0, 1, 35000),
          colors = colBinary,
          opacity = opacidadClic,
          labels = c(
            paste0(YN_Legend[1], Segregacion),
            paste0(YN_Legend[2], Segregacion)
          ),
          title = titulo
        ) |>
        addLabelOnlyMarkers(
          lng = Centroides_Deptos$Lon,
          lat = Centroides_Deptos$Lat,
          label = paste0(sapply(Centroides_Deptos$Departamento, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            textsize = textDeptos
          )
        ) |>
        addLabelOnlyMarkers(
          lng = Centroides_Mpios$Lon,
          lat = Centroides_Mpios$Lat,
          label = paste0(sapply(Centroides_Mpios$Municipio, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(noHide = FALSE)
        ) |>
        addSearchFeatures(
          targetGroups = "lupa",
          options = searchFeaturesOptions(
            zoom = 10,
            openPopup = TRUE,
            textErr = "Ubicaci\u00f3n No Encontrada",
            textCancel = "Cancelar",
            textPlaceholder = "Buscar...",
            position = "topleft",
            hideMarkerOnCollapse = FALSE
          )
        ) |>
        addCircleMarkers(
          lng = Capitales$Longitud,
          lat = Capitales$Latitud,
          radius = 2,
          stroke = TRUE,
          color = "#377EB8",
          fill = TRUE,
          fillColor = "purplelight",
          fillOpacity = 0.9
        )
    } else if (tipo == "mpios") {
      # MAPA TOTAL DE GRADUADOS POR MUNICIPIO
      if (missingArg(colBorde)) {
        colBorde <- "#AB1616"
      }
      if (!(missingArg(cortes) && missingArg(colores))) {
        if (length(colores) != length(cortes) - 1L) {
          stop(Mensaje, call. = FALSE)
        } else {
          Colors <- colores
          Cortes <- cortes
        }
      } else {
        Colors <- c("#B9FFDC", "#CFFFAF", "#FFEA80", "#FFA652", "#ED6753")
        Cortes <- c(0, 1, 3, 10, 100, Inf)
      }
      pal <- colorBin(
        Colors,
        domain = Polygons_Mpio@data$Statistic,
        bins = Cortes,
        na.color = colNA
      )

      Mapa <- leaflet(data = Polygons_Mpio) |> addTiles(attribution = Msj)
      for (i in seq_len(length(Baldosas))) {
        Mapa <- Mapa |>
          addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
      }

      Mapa <- Mapa |>
        setView(
          lat = Centroide_Col$Lat,
          lng = Centroide_Col$Lon,
          zoom = zoomMapa
        ) |>
        addLayersControl(
          baseGroups = Baldosas.names,
          options = layersControlOptions(collapsed = compacto)
        ) |>
        addPolygons(
          stroke = TRUE,
          fillColor = ~ pal(Statistic),
          weight = 1,
          opacity = opacidad,
          color = "gray",
          dashArray = "3",
          fillOpacity = opacidad,
          highlightOptions = list(
            weight = 4,
            color = colBorde,
            dashArray = "",
            fillOpacity = opacidadClic,
            bringToFront = TRUE
          ),
          label = Labels_Mpios,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) |>
        addPolylines(
          data = Polygons_Depto,
          stroke = TRUE,
          color = "#252525",
          weight = 2,
          smoothFactor = 0.05
        ) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~Statistic,
          opacity = opacidadClic,
          labFormat = labelFormat(prefix = "[", suffix = ")"),
          title = titulo
        ) |>
        addLabelOnlyMarkers(
          lng = Centroides_Deptos$Lon,
          lat = Centroides_Deptos$Lat,
          label = paste0(sapply(Centroides_Deptos$Departamento, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            textsize = textDeptos
          )
        ) |>
        addLabelOnlyMarkers(
          lng = Centroides_Mpios$Lon,
          lat = Centroides_Mpios$Lat,
          label = paste0(sapply(Centroides_Mpios$Municipio, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(noHide = FALSE)
        ) |>
        addSearchFeatures(
          targetGroups = "lupa",
          options = searchFeaturesOptions(
            zoom = 10,
            openPopup = TRUE,
            textErr = "Ubicaci\u00f3n No Encontrada",
            textCancel = "Cancelar",
            textPlaceholder = "Buscar...",
            position = "topleft",
            hideMarkerOnCollapse = FALSE
          )
        ) |>
        addCircleMarkers(
          lng = Capitales$Longitud,
          lat = Capitales$Latitud,
          radius = 2,
          stroke = TRUE,
          color = "#377EB8",
          fill = TRUE,
          fillColor = "purplelight",
          fillOpacity = 0.9
        )
    } else {
      # MAPA COMBINADO DEPARTAMENTO-MUNICIPIO
      if (missingArg(colBorde)) {
        colBorde <- "#5A0028"
      }
      if (!(missingArg(cortes) && missingArg(colores))) {
        if (
          (length(colores$Deptos) != length(cortes$Deptos) - 1L) ||
            (length(colores$Mpios) != length(cortes$Mpios) - 1L)
        ) {
          stop(Mensaje, call. = FALSE)
        } else {
          Colors_Deptos <- colores$Deptos
          Cortes_Deptos <- cortes$Deptos
          Colors_Mpios <- colores$Mpios
          Cortes_Mpios <- cortes$Mpios
        }
      } else {
        Colors_Deptos <- c(
          "#E1F5C4",
          "#EDE574",
          "#F9D423",
          "#FC913A",
          "#FF4E50"
        )
        Cortes_Deptos <- c(0, 20, 50, 200, 1000, Inf)
        Colors_Mpios <- c("#B9FFDC", "#CFFFAF", "#FFEA80", "#FFA652", "#ED6753")
        Cortes_Mpios <- c(0, 1, 3, 10, 100, Inf)
      }
      Pal_Deptos <- colorBin(
        palette = Colors_Deptos,
        bins = Cortes_Deptos,
        na.color = colNA
      )
      Pal_Mpios <- colorBin(
        palette = Colors_Mpios,
        domain = Polygons_Mpio@data$Statistic,
        bins = Cortes_Mpios,
        na.color = colNA
      )

      Mapa <- leaflet(data = Polygons_Mpio) |> addTiles(attribution = Msj)
      for (i in seq_len(length(Baldosas))) {
        Mapa <- Mapa |>
          addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
      }

      Mapa <- Mapa |>
        setView(
          lat = Centroide_Col$Lat,
          lng = Centroide_Col$Lon,
          zoom = zoomMapa
        ) |>
        addLayersControl(
          baseGroups = Baldosas.names,
          overlayGroups = "Mostrar<br/>Departamentos",
          options = layersControlOptions(
            collapsed = compacto,
            autoZindex = TRUE
          )
        ) |>
        addPolygons(
          stroke = TRUE,
          fillColor = ~ Pal_Mpios(Statistic),
          weight = 1,
          opacity = opacidad,
          color = "gray",
          dashArray = "3",
          fillOpacity = opacidad,
          highlightOptions = list(
            weight = 4,
            color = colBorde,
            dashArray = "",
            fillOpacity = opacidadClic,
            bringToFront = TRUE
          ),
          label = Labels_Mpios,
          group = "Municipios",
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) |>
        addPolygons(
          data = Polygons_Depto,
          stroke = TRUE,
          fillColor = ~ Pal_Deptos(Statistic),
          weight = 2,
          opacity = opacidad,
          color = "#005A32",
          dashArray = "",
          fillOpacity = opacidad,
          highlightOptions = list(
            weight = 4,
            color = "#5A0028",
            dashArray = "",
            fillOpacity = opacidadClic,
            bringToFront = TRUE
          ),
          label = Labels_Deptos,
          group = "Mostrar<br/>Departamentos",
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) |>
        showGroup("Municipios") |>
        hideGroup("Mostrar<br/>Departamentos") |>
        addPolylines(
          data = Polygons_Depto,
          stroke = TRUE,
          color = "#005A32",
          weight = 2,
          smoothFactor = 0.05,
          group = "Municipios"
        ) |>
        addLegend(
          position = "bottomright",
          pal = Pal_Mpios,
          values = ~Statistic,
          opacity = opacidadClic,
          labFormat = labelFormat(prefix = "[", suffix = ")"),
          title = TitleMpio
        ) |>
        addLegend(
          position = "bottomright",
          pal = Pal_Deptos,
          values = ~Statistic,
          opacity = opacidadClic,
          labFormat = labelFormat(prefix = "[", suffix = ")"),
          title = TitleDepto
        ) |>
        addLabelOnlyMarkers(
          lng = Centroides_Deptos$Lon,
          lat = Centroides_Deptos$Lat,
          label = paste0(sapply(Centroides_Deptos$Departamento, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            textsize = textDeptos
          )
        ) |>
        addLabelOnlyMarkers(
          lng = Capitales$Longitud,
          lat = Capitales$Latitud,
          label = paste0(sapply(Capitales$Municipio, str_to_title)),
          labelOptions = labelOptions(
            noHide = TRUE,
            textOnly = TRUE,
            textsize = textMpios
          )
        ) |>
        addLabelOnlyMarkers(
          lng = Centroides_Mpios$Lon,
          lat = Centroides_Mpios$Lat,
          label = paste0(sapply(Centroides_Mpios$Municipio, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(noHide = FALSE)
        ) |>
        addSearchFeatures(
          targetGroups = "lupa",
          options = searchFeaturesOptions(
            zoom = 10,
            openPopup = TRUE,
            textErr = "Ubicaci\u00f3n No Encontrada",
            textCancel = "Cancelar",
            textPlaceholder = "Buscar...",
            position = "topleft",
            hideMarkerOnCollapse = FALSE
          )
        ) |>
        addCircleMarkers(
          lng = Capitales$Longitud,
          lat = Capitales$Latitud,
          radius = 2,
          stroke = TRUE,
          color = "#D95F02",
          fill = TRUE,
          fillColor = "orangelight",
          fillOpacity = 0.9,
          group = "Municipios"
        ) |>
        addCircleMarkers(
          lng = Capitales$Longitud,
          lat = Capitales$Latitud,
          radius = 2,
          stroke = TRUE,
          color = "#D95F02",
          fill = TRUE,
          fillColor = "orangelight",
          fillOpacity = 0.9,
          group = "Mostrar<br/>Departamentos"
        )
    }

    if (showSedes) {
      if (tipo %in% c("deptos", "sinompios", "mpios")) {
        Mapa <- Mapa |>
          # Adición de grupos de control de capas.
          addLayersControl(
            baseGroups = Baldosas.names,
            overlayGroups = "Mostrar<br/>sedes UNAL",
            options = layersControlOptions(collapsed = compacto)
          ) |>
          # Adición de los marcadores de iconos para las distintas sedes de presencia nacional de la UNAL, y deseleccionándola por defecto.
          addAwesomeMarkers(
            lng = Sedes$Longitud,
            lat = Sedes$Latitud,
            group = "Mostrar<br/>sedes UNAL",
            icon = Icons_Sedes,
            label = Labels_Sedes,
            labelOptions = labelOptions(
              style = list("font-weight" = "large", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
          ) |>
          hideGroup("Mostrar<br/>sedes UNAL")
      } else {
        Mapa <- Mapa |>
          addLayersControl(
            baseGroups = Baldosas.names,
            overlayGroups = c(
              "Mostrar<br/>Departamentos",
              "Mostrar<br/>sedes UNAL"
            ),
            options = layersControlOptions(
              collapsed = compacto,
              autoZindex = TRUE
            )
          ) |>
          addPolygons(
            data = Polygons_Depto,
            stroke = TRUE,
            fillColor = ~ Pal_Deptos(Statistic),
            weight = 2,
            opacity = opacidad,
            color = "#005A32",
            dashArray = "",
            fillOpacity = opacidad,
            highlightOptions = list(
              weight = 4,
              color = "#5A0028",
              dashArray = "",
              fillOpacity = opacidadClic,
              bringToFront = TRUE
            ),
            label = Labels_Deptos,
            group = "Mostrar<br/>Departamentos",
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "12px",
              direction = "auto"
            )
          ) |>
          showGroup("Municipios") |>
          hideGroup("Mostrar<br/>Departamentos") |>
          addCircleMarkers(
            lng = Capitales$Longitud,
            lat = Capitales$Latitud,
            radius = 2,
            stroke = TRUE,
            color = "#D95F02",
            fill = TRUE,
            fillColor = "orangelight",
            fillOpacity = 0.9,
            group = "Mostrar<br/>Departamentos"
          ) |>
          addAwesomeMarkers(
            lng = Sedes$Longitud,
            lat = Sedes$Latitud,
            group = "Mostrar<br/>sedes UNAL",
            icon = Icons_Sedes,
            label = Labels_Sedes,
            labelOptions = labelOptions(
              style = list("font-weight" = "large", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
          ) |>
          hideGroup("Mostrar<br/>sedes UNAL")
      }
    }
    if (!limpio) {
      TextJS <- paste0(
        "function(btn, map){ map.setView(L.latLng(",
        Centroide_Col$Lat,
        ", ",
        Centroide_Col$Lon,
        "), ",
        zoomMapa,
        "); }"
      )
      Mapa <- Mapa |>
        # Adición del minimapa para ayuda en la navegación.
        addMiniMap(
          position = "bottomleft",
          zoomAnimation = TRUE,
          toggleDisplay = TRUE,
          autoToggleDisplay = TRUE
        ) |>
        # Adición de botones simples para ver en pantalla completa, restablecer el zoom y localización.
        addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE) |>
        addEasyButton(easyButton(
          icon = "fa-globe",
          title = "Retornar",
          onClick = JS(TextJS)
        )) |>
        addEasyButton(easyButton(
          icon = "fa-crosshairs",
          title = "Ub\u00edcame",
          onClick = JS("function(btn, map){ map.locate({setView: true}); }")
        )) |>
        # Adicción de la barra de escala.
        addScaleBar(
          position = "bottomleft",
          options = scaleBarOptions(metric = TRUE, imperial = FALSE)
        )
    }
  } else {
    if (tipo == "deptos") {
      COL_Final_SF <- sf::st_as_sf(Polygons_Depto)
      varText <- "Departamento"
    } else {
      COL_Final_SF <- sf::st_as_sf(Polygons_Mpio)
      varText <- "Municipio"
    }

    LineWidth <- ifelse(
      missingArg(estilo) || is.null(estilo$anchoBorde),
      0.5,
      estilo$anchoBorde
    )
    LineColor <- ifelse(missingArg(colBorde), "#999999", colBorde)
    labelX <- ifelse(
      missingArg(estilo) || is.null(estilo$labelX),
      "Longitud",
      estilo$labelX
    )
    labelY <- ifelse(
      missingArg(estilo) || is.null(estilo$labelY),
      "Latitud",
      estilo$labelY
    )
    if (missingArg(estilo) || is.null(estilo$xlim)) {
      Xlim <- NULL
    } else {
      Xlim <- estilo$xlim
    }
    if (missingArg(estilo) || is.null(estilo$ylim)) {
      Ylim <- NULL
    } else {
      Ylim <- estilo$ylim
    }
    if (missingArg(estilo) || is.null(estilo$Legend)) {
      ParmsLegend <- list(
        legend.position = "right",
        legend.direction = "vertical"
      )
    } else {
      ParmsLegend <- estilo$Legend
    }
    if (missingArg(estilo) || is.null(estilo$Labs)) {
      ParmsLabs <- list(
        fill = "Statistic",
        subtitle = NULL,
        caption = NULL,
        tag = NULL
      )
    } else {
      ParmsLabs <- estilo$Labs
    }

    if (!(missingArg(estilo) || is.null(estilo$Theme))) {
      ThemeGG <- switch(
        estilo$Theme,
        "1" = theme_light(),
        "2" = theme_bw(),
        "3" = theme_classic(),
        "4" = theme_linedraw(),
        "5" = theme_gray(),
        "6" = ggthemes::theme_hc(),
        "7" = ggthemes::theme_pander(),
        "8" = ggthemes::theme_gdocs(),
        "9" = ggthemes::theme_fivethirtyeight(),
        "10" = ggthemes::theme_economist(),
        "11" = ggthemes::theme_solarized()
      )
    } else {
      ThemeGG <- theme_DNPE()
    }

    if (is.na(estilo$Style)) {
      cortesCute <- waiver()
      if (missingArg(colores)) {
        nLevels <- COL_Final_SF |>
          select({{ variable }}) |>
          as.data.frame() |>
          select({{ variable }}) |>
          pull() |>
          n_distinct()
        colores <- rainbow(nLevels, alpha = 0.7)
      }
    } else {
      if (tolower(estilo$Style) == "intervalo") {
        if (!(missingArg(cortes) && missingArg(colores))) {
          if (length(colores) != length(cortes) - 1L) {
            stop(Mensaje, call. = FALSE)
          }
        } else {
          colores <- c("#E1F5C4", "#EDE574", "#F9D423", "#FC913A", "#FF4E50")
          cortes <- c(0, 20, 50, 200, 1000, Inf)
        }

        cortesCute <- prettyNum(
          cortes[-1],
          big.mark = ".",
          decimal.mark = ",",
          scientific = FALSE
        )
        NewVar <- cut(
          COL_Final_SF |>
            select(Statistic) |>
            as.data.frame() |>
            select(Statistic) |>
            pull(),
          breaks = cortes,
          dig.lab = 5
        )
        COL_Final_SF <- COL_Final_SF |> mutate(Statistic := NewVar)
      } else if (tolower(estilo$Style) == "sino") {
        if (!missingArg(cortes)) {
          warning(
            paste0(
              '\u25ba \u00a1Se ha proporcionado un valor para el argumento "cortes" y se ha seleccionado el tipo de mapa "SiNo"!',
              '\n\t El valor ingresado ser\u00e1 omitido, ya que este tipo de mapa no permite un cambio en los cortes debido a que utiliza un intervalo binario.'
            ),
            call. = FALSE
          )
        }
        if (!missingArg(colores)) {
          if (length(colores) != 2L) {
            stop(
              '[\u00d7] La longitud del argumento "colores" es diferente a 2. Este mapa es binario, por lo cual necesita especificar exactamente 2 colores.',
              call. = FALSE
            )
          }
        } else {
          colores <- c("#FDAE61", "#A6D96A")
        }

        cortesCute <- waiver()
        NewVar <- if_else(
          COL_Final_SF |>
            select(Statistic) |>
            as.data.frame() |>
            select(Statistic) |>
            pull() >
            0,
          YN_Legend[1],
          YN_Legend[2]
        )
        COL_Final_SF <- COL_Final_SF |> mutate(Statistic := NewVar)
      }
    }

    dots_geom_sf <- list(
      mapping = aes(fill = Statistic),
      linewidth = LineWidth,
      color = LineColor
    )

    if (!missingArg(centroideMapa)) {
      centroideMapa <- toupper(centroideMapa)
      COL_Final_SF <- COL_Final_SF |> filter(Departamento == centroideMapa)
      if (!missing(`...`)) {
        COL_Final_SF <- COL_Final_SF |> filter(Municipio == ...)
      }
    } else {
      COL_Final_SF <- COL_Final_SF |>
        filter(
          Departamento !=
            "ARCHIPI\u00c9LAGO DE SAN ANDR\u00c9S, PROVIDENCIA Y SANTA CATALINA"
        )
    }

    Mapa <- ggplot(data = COL_Final_SF) +
      do.call(geom_sf, dots_geom_sf) +
      labs(
        title = titulo,
        subtitle = ParmsLabs$subtitle,
        x = labelX,
        y = labelY,
        caption = ParmsLabs$caption,
        tag = ParmsLabs$tag,
        fill = ParmsLabs$fill
      ) +
      coord_sf(expand = FALSE, xlim = Xlim, ylim = Ylim) +
      ThemeGG +
      do.call(ggplot2::theme, ParmsLegend)

    if (
      is.na(estilo$Style) ||
        tolower(estilo$Style) == "intervalo" ||
        tolower(estilo$Style) == "sino"
    ) {
      Mapa <- Mapa +
        scale_fill_manual(
          values = alpha(colores, opacidad),
          labels = cortesCute,
          drop = FALSE,
          na.value = colNA,
          guide = guide_legend(
            direction = "horizontal",
            keyheight = 0.5,
            keywidth = 2.5,
            title.position = "top",
            title.hjust = 0.5,
            label.hjust = 0.5,
            nrow = 1,
            byrow = TRUE,
            reverse = FALSE,
            label.position = "bottom"
          )
        )
    } else if (tolower(estilo$Style) == "calor") {
      Mapa <- Mapa +
        scale_fill_viridis_c(
          option = "C",
          trans = "sqrt",
          alpha = opacidad,
          na.value = colNA,
          labels = scales::label_comma()
        )
    }

    if (textSize != 0) {
      if (missingArg(estilo) || is.null(estilo$Text)) {
        ParmsText <- list(color = "#282828", size = 1.5)
      } else {
        ParmsText <- estilo$Text
      }
      dots_geom_text <- append(
        list(aes(label = !!sym(varText)), check_overlap = TRUE),
        ParmsText
      )
      Mapa <- Mapa + do.call(geom_sf_text, dots_geom_text)
    }

    if (!(missingArg(estilo) || is.null(estilo$scaleX))) {
      Mapa <- Mapa + scale_x_continuous(breaks = estilo$scaleX)
    }
    if (!(missingArg(estilo) || is.null(estilo$scaleY))) {
      Mapa <- Mapa + scale_y_continuous(breaks = estilo$scaleY)
    }
  }

  # ============================================================================
  if (zoomIslas) {
    if (tipo == "deptos") {
      thisCall <- match.call(expand.dots = TRUE)
      thisCall$zoomIslas <- FALSE
      thisCall$centroideMapa <- "ARCHIPI\u00c9LAGO DE SAN ANDR\u00c9S, PROVIDENCIA Y SANTA CATALINA"
      Mapa_ADZ <- eval.parent(thisCall)
      Mapa_ADZ <- Mapa_ADZ +
        guides(fill = "none") +
        labs(
          title = NULL,
          subtitle = NULL,
          x = NULL,
          y = NULL,
          caption = NULL,
          tag = NULL,
          fill = NULL
        ) +
        theme(
          legend.background = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()
        )
      return(list(M_COL = Mapa, M_SanAndres = Mapa_ADZ))
    } else {
      thisCall <- match.call(expand.dots = TRUE)
      thisCall$zoomIslas <- FALSE
      thisCall$centroideMapa <- "ARCHIPI\u00c9LAGO DE SAN ANDR\u00c9S, PROVIDENCIA Y SANTA CATALINA"
      thisCall$`...` <- "SAN ANDR\u00c9S ARC"
      # thisCall[[1]] <- as.name("Plot.Mapa")
      Mapa_ADZ1 <- eval.parent(thisCall)
      thisCall$`...` <- "PROVIDENCIA ARC"
      Mapa_ADZ2 <- eval.parent(thisCall)

      # Mapa_ADZ <- cowplot::plot_grid(
      Mapa_ADZ1 <- Mapa_ADZ1 +
        guides(fill = "none") +
        labs(
          title = NULL,
          subtitle = NULL,
          x = NULL,
          y = NULL,
          caption = NULL,
          tag = NULL,
          fill = NULL
        ) +
        theme(
          legend.background = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()
        )
      Mapa_ADZ2 <- Mapa_ADZ2 +
        guides(fill = "none") +
        labs(
          title = NULL,
          subtitle = NULL,
          x = NULL,
          y = NULL,
          caption = NULL,
          tag = NULL,
          fill = NULL
        ) +
        theme(
          legend.background = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank()
        )
      return(list(
        M_COL = Mapa,
        M_SanAndres = Mapa_ADZ1,
        M_Providencia = Mapa_ADZ2
      ))
    }
  }

  return(Mapa)
}
