#' Cree un mapa dinámico y flexible para visualizar datos geográficos de países
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
#' @inheritParams Plot.Mapa
#' @param paises Una variable dentro del data frame ingresado en `datos`, que
#'   contiene los códigos de los países, de acuerdo con la codificación
#'   ISO3166, ya sea el nombre o el código alpha2 o alpha3.
#' @param grupo Variable auxiliar con la cual se segmentará los datos, para que
#'   se grafique en un único plot divido en parcelas por dicha variable.
#' @param centroideMapa Cadena de caracteres indicando el país que servirá de
#'   centroide al momento de graficar el mapa. El valor por defecto es "MEXICO".
#'   Se emparejará parcialmente.
#'
#' @returns
#' Retorna el mapa (*objeto widget de HTML*) creado mediante `Leaflet`, el cual
#' pertenece a la clase "leaflet" y "htmlwidget".
#'
#' @examples
#' LATAM <- data.frame(
#'   Country = c("Chile", "Venezuela", "Colombia", "Argentina", "Brazil"),
#'   PIB = c(1, 10, 100, 1000, 10000)*1:5
#' )
#' Plot.Mundo(
#'   datos    = LATAM,
#'   paises   = Country,
#'   variable = PIB,
#'   tipo     = "Pais",
#'   titulo   = "PIB 2023-Q1",
#'   naTo0    = FALSE,
#'   colNA    = "#3DBC25",
#'   centroideMapa = "Peru",
#'   zoomMapa = 3,
#'   cortes   = c(0, 10, 100, 1000, 10000, Inf),
#'   colores  = c("#DFA86A", "#FFBB00", "#FF5100", "#F20034", "#76009D"),
#'   opacidad = 0.5,
#'   colBorde = "#0E9BEE",
#'   compacto = TRUE,
#'   textSize = 4,
#'   limpio   = FALSE
#' )
#' @examplesIf all(FALSE)
#' # ---------------------------------------------------------------------------
#' Plot.Mundo(
#'   datos    = LATAM,
#'   paises   = Country,
#'   variable = PIB,
#'   tipo     = "SiNoPais",
#'   titulo   = "PIB 2023-Q1",
#'   centroideMapa = "Senegal",
#'   colores  = c("#45C9FF", "#FF153F"),
#'   opacidad = 0.6,
#'   colBorde = "#3DBC25",
#'   compacto = FALSE,
#'   textSize = 10,
#'   limpio   = FALSE
#' )
#' # ---------------------------------------------------------------------------
#' set.seed(123)
#' LATAM <- data.frame(
#'   Pais = c("Bolivia", "Colombia", "Ecuador", "Panama", "Venezuela",
#'            "Peru", "Argentina", "Brazil", "Chile", "Uruguay"),
#'   PIB  = runif(10, -10, 10),
#'   Bolivariano = c("Sí", "Sí", "Sí", "Sí", "Sí", "Sí", "No", "No", "No", "No")
#' )
#' Plot.Mundo(
#'   datos    = LATAM,
#'   paises   = Pais,
#'   variable = Bolivariano,
#'   tipo     = "Pais",
#'   titulo   = "PA\u00cdSES BOLIVARIANOS EN AM\u00c9RICA LATINA",
#'   naTo0    = FALSE,
#'   estatico = TRUE,
#'   estilo   = list(Style = NA, Theme = 2)
#' )
#' # ---------------------------------------------------------------------------
#' Plot.Mundo(
#'   datos    = LATAM,
#'   paises   = Pais,
#'   variable = PIB,
#'   # grupo  = Bolivariano,
#'   tipo     = "Pais",
#'   titulo   = "PROYECCI\u00d3N DEL PIB",
#'   opacidad = 0.4,
#'   colBorde = "#876445",
#'   estatico = TRUE,
#'   estilo   = list(
#'     Style = "Calor",
#'     showISO    = list(color = "#00468A", size = 3.5, fontface = "bold.italic"),
#'     xlim       = c(-82, -34),
#'     ylim       = c(-60, 14),
#'     scaleX     = seq(-82, 34, by = 8),
#'     scaleY     = seq(-60, 14, by = 5),
#'     anchoBorde = 1,
#'     Theme = 6, Legend = list(legend.position = "bottom", legend.direction = "horizontal"),
#'     Labs = list(subtitle = "Para Suram\u00e9rica en el 2023",
#'                 caption  = "Datos simulados para el ejemplo ilustrativo",
#'                 tag = "\u00ae"
#'                 )
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' Territories <- data.frame(
#'   Country = c("RUS", "FRA", "UKR", "ESP", "SWE", "DEU", "DZA", "COD", "SDN", "LBY", "TCD", "NER"),
#'   Area    = rep(10^(5:0), 2)
#' )
#' Plot.Mundo(
#'   datos    = Territories,
#'   paises   = Country,
#'   variable = Area,
#'   tipo     = "Pais",
#'   titulo   = "ALGUNOS DE LOS PA\u00cdSES CON MAYOR \u00c1REA",
#'   # showISO    = list(),
#'   naTo0    = FALSE,
#'   colNA    = "#0C1C2B",
#'   cortes   = c(-.0001, 10, 100, 1000, 10000, Inf),
#'   colores  = c("#DFA86A", "#FFBB00", "#FF5100", "#F20034", "#76009D"),
#'   opacidad = 0.4,
#'   limpio   = TRUE,
#'   estatico = TRUE,
#'   estilo   = list(
#'     Style = "Intervalo", continente = c("Africa", "Europe"), Theme = 4
#'   )
#' )
#' # ---------------------------------------------------------------------------
#' COL <- data.frame(Pais = "CO", IDH = 100000)
#' Plot.Mundo(
#'   datos    = COL,
#'   paises   = Pais,
#'   variable = IDH,
#'   tipo     = "Pais",
#'   titulo   = "",
#'   colores  = c("#10F235", "#00BCB5"),
#'   naTo0    = TRUE,
#'   estatico = TRUE,
#'   estilo   = list(Style = "SiNo", Theme = 3)
#' )
#'
#' @export
#'
#' @import leaflet
#' @import dplyr
#' @importFrom leaflet.extras addFullscreenControl addSearchFeatures searchFeaturesOptions
#' @importFrom tidyr replace_na
#' @importFrom stringr str_to_title str_length
#' @importFrom htmltools HTML
#' @importFrom stats median sd var
#' @importFrom methods missingArg
#' @importFrom sp coordinates CRS SpatialPolygonsDataFrame
#' @importFrom sf st_as_sf st_centroid
#' @importFrom maps map
#' @importFrom ggspatial annotation_north_arrow annotation_scale north_arrow_fancy_orienteering
#' @importFrom lifecycle deprecate_warn
Plot.Mundo <- function(
  datos,
  paises,
  variable,
  grupo,
  tipo = c("Pais", "SiNoPais"),
  titulo,
  naTo0 = TRUE,
  colNA = "#EEEEEE",
  centroideMapa,
  zoomMapa = 2,
  baldosas,
  cortes,
  colores,
  opacidad = 0.7,
  colBorde,
  compacto = TRUE,
  textSize = 10,
  limpio = FALSE,
  estatico = FALSE,
  estilo
) {
  options("rgdal_show_exportToProj4_warnings" = "none")
  # COMANDOS DE VERIFICACIÓN Y VALIDACIÓN
  if (missingArg(datos)) {
    stop('\u00a1Por favor introduzca un DataFrame!', call. = FALSE)
  }
  if (missingArg(paises)) {
    stop(
      '\u00a1Por favor introduzca el nombre de una variable presente en "datos" que contenga el nombre o c\u00f3digo del pa\u00eds!',
      call. = FALSE
    )
  }
  if (missingArg(variable)) {
    stop(
      '\u00a1Por favor introduzca el nombre de una variable presente en "datos" que contenga la variable num\u00e9rica o cualitativa!',
      call. = FALSE
    )
  }
  tipo <- tolower(tipo)
  if (tipo %NotIN% c("pais", "sinopais")) {
    stop(
      '\u00a1Por favor introduzca un tipo de mapa correcto! Las opciones son: "Pais" y "SiNoPais"',
      call. = FALSE
    )
  }
  if (!missingArg(titulo)) {
    if (!is.character(titulo)) {
      stop(
        "\u00a1El argumento 'titulo' debe ser una cadena de texto!",
        call. = FALSE
      )
    }
  } else {
    titulo <- "\u00bf ?"
  }
  if (!all(is.logical(naTo0), is.logical(compacto), is.logical(limpio))) {
    stop(
      "\u00a1Los argumentos 'naTo0', 'compacto' y 'limpio' deben ser un booleano (TRUE o FALSE)!",
      call. = FALSE
    )
  }
  if (!missing(colBorde)) {
    if (!is.character(colBorde)) {
      stop(
        '\u00a1El argumento "colBorde" debe ser un car\u00e1cter que indique un color con el nombre ("red"), c\u00f3digo hexadecimal ("#FF0000") o RGB (rgb(255, 0, 0))!',
        call. = FALSE
      )
    }
  }
  IsAgregado <- datos |> summarise(n = n(), .by = {{ paises }})
  if (!all(IsAgregado$n == 1)) {
    stop(
      "\u00a1El dataframe ingresado no corresponde a un agregado!
         \t -  Pues cada fila no corresponde a un registro \u00fanico.
         \t -- Al agrupar por el argumento 'paises' se obtiene m\u00e1s de una fila x cada nivel",
      call. = FALSE
    )
  }
  Mensaje <- paste0(
    '\u00a1El argumento "colores" no tiene la longitud correcta respecto al argumento "cortes" ingresado, o viceversa!',
    '\n\t - Recuerde que si realiza (n) cortes necesita (n-1) colores para cada intervalo creado.',
    '\n\t -- Por ejemplo, si los cortes son c(0, 50, Inf) necesitar\u00e1 2 colores para dichos intervalos.'
  )

  # ____________________________________________________________________________
  World <- maps::map("world", fill = TRUE, col = 1, plot = FALSE)
  World_IDs <- sapply(strsplit(World$names, ':'), function(x) x[1])
  World_sp <- map2SpatialPolygons(
    map = World,
    IDs = World_IDs,
    proj4string = sp::CRS("+proj=longlat +datum=WGS84")
  )
  World_df <- data.frame(Country = names(World_sp))
  rownames(World_df) <- names(World_sp)
  World_Final <- sp::SpatialPolygonsDataFrame(World_sp, World_df)
  # ____________________________________________________________________________
  # https://es.wikipedia.org/wiki/Bolivia
  # Iran
  # Problemas NO solucionados con:
  #   https://es.wikipedia.org/wiki/Antigua_y_Barbuda
  #     Madeira Islands
  #   https://es.wikipedia.org/wiki/San_Cristóbal_y_Nieves
  #   https://es.wikipedia.org/wiki/Caribe_Neerlandés
  #   https://es.wikipedia.org/wiki/Santa_Elena,_Ascensión_y_Tristán_de_Acuña
  #   Canary Islands
  # Namibia NO tiene ISO2
  # Siachen Glacier NO Existe en ISO

  # Adicionando las abreviaciones de la ISO3166 --------------------------------
  ISO_3166 <- maps::iso3166
  ISO_3166 <- ISO_3166 |>
    mutate(
      ISOname = recode(
        ISOname,
        'Saint Helena, Ascension and Tristan da Cunha' = 'Ascension Island',
        'Bolivia, Plurinational State of' = 'Bolivia',
        'Bonaire, Sint Eustatius and Saba' = 'Bonaire',
        'Brunei Darussalam' = 'Brunei',
        'British Indian Ocean Territory' = 'Chagos Archipelago',
        'Cocos (Keeling) Islands' = 'Cocos Islands',
        'Falkland Islands (Malvinas)' = 'Falkland Islands',
        'Saint Vincent and the Grenadines' = 'Grenadines',
        'Heard Island and McDonald Islands' = 'Heard Island',
        'Iran, Islamic Republic of' = 'Iran',
        "Cote d'Ivoire" = 'Ivory Coast',
        "Lao People's Democratic Republic" = 'Laos',
        'Federated States of Micronesia' = 'Micronesia',
        'Moldova, Republic of' = 'Moldova',
        "Democratic People's Republic of Korea" = 'North Korea',
        'Republic of North Macedonia' = 'North Macedonia',
        'Palestine, State of' = 'Palestine',
        'Russian Federation' = 'Russia',
        'Saint Martin (French part)' = 'Saint Martin',
        'Sint Maarten (Dutch part)' = 'Sint Maarten',
        'Republic of Korea' = 'South Korea',
        'Syrian Arab Republic' = 'Syria',
        'Tanzania, United Republic of' = 'Tanzania',
        'United Kingdom of Great Britain and Northern Ireland' = 'UK',
        'United States' = 'USA',
        'Holy See (Vatican City State)' = 'Vatican',
        'Venezuela, Bolivarian Republic of' = 'Venezuela',
        'Virgin Islands, U.S.' = 'Virgin Islands, US'
      )
    )

  ISO_3166 <- ISO_3166 |> select(a2:ISOname) |> distinct()
  colnames(ISO_3166) <- c("Alfa2", "Alfa3", "Country")
  World_Final@data <- World_Final@data |>
    left_join(ISO_3166, by = join_by(Country))

  World_Final@data[World_Final@data$Country == 'Antigua', ] <- c(
    'Antigua',
    'AG',
    'ATG'
  )
  World_Final@data[World_Final@data$Country == 'Azores', ] <- c(
    'Azores',
    'PT',
    'PRT'
  )
  World_Final@data[World_Final@data$Country == 'Barbuda', ] <- c(
    'Barbuda',
    'AG',
    'ATG'
  )
  World_Final@data[World_Final@data$Country == 'Canary Islands', ] <- c(
    'Canary Islands',
    'ES',
    'ESP'
  )
  World_Final@data[World_Final@data$Country == 'Madeira Islands', ] <- c(
    'Madeira Islands',
    'PT',
    'PRT'
  )
  World_Final@data[World_Final@data$Country == 'Namibia', ] <- c(
    'Namibia',
    NA,
    'NAM'
  )
  World_Final@data[World_Final@data$Country == 'Nevis', ] <- c(
    'Nevis',
    'KN',
    'KNA'
  )
  World_Final@data[World_Final@data$Country == 'Saba', ] <- c(
    'Saba',
    'BQ',
    'BES'
  )
  World_Final@data[World_Final@data$Country == 'Saint Helena', ] <- c(
    'Saint Helena',
    'SH',
    'SHN'
  )
  World_Final@data[World_Final@data$Country == 'Saint Kitts', ] <- c(
    'Saint Kitts',
    'KN',
    'KNA'
  )
  World_Final@data[World_Final@data$Country == 'Saint Vincent', ] <- c(
    'Saint Vincent',
    'VC',
    'VCT'
  )
  World_Final@data[World_Final@data$Country == 'Sint Eustatius', ] <- c(
    'Sint Eustatius',
    'BQ',
    'BES'
  )
  World_Final@data[World_Final@data$Country == 'South Georgia', ] <- c(
    'South Georgia',
    'GS',
    'SGS'
  )
  World_Final@data[World_Final@data$Country == 'South Sandwich Islands', ] <- c(
    'South Sandwich Islands',
    'GS',
    'SGS'
  )
  World_Final@data[World_Final@data$Country == 'Tobago', ] <- c(
    'Tobago',
    'TT',
    'TTO'
  )
  World_Final@data[World_Final@data$Country == 'Trinidad', ] <- c(
    'Trinidad',
    'TT',
    'TTO'
  )

  # Adicionando el continente de acuerdo con el Alpha3 -----------------------
  Codes <- CountryCode |> select(ISO3 = ISO3_CODE, Continent = continent)
  World_Final@data <- World_Final@data |>
    left_join(Codes, by = join_by(Alfa3 == ISO3))

  World_Final@data[World_Final@data$Country == 'Antarctica', ] <- c(
    'Antarctica',
    'AQ',
    'ATA',
    'Antarctica'
  )
  World_Final@data[World_Final@data$Country == 'Chagos Archipelago', ] <- c(
    'Chagos Archipelago',
    'IO',
    'IOT',
    'Africa'
  )
  World_Final@data[World_Final@data$Country == 'Cocos Islands', ] <- c(
    'Cocos Islands',
    'CC',
    'CCK',
    'Asia'
  )
  World_Final@data[
    World_Final@data$Country == 'French Southern and Antarctic Lands',
  ] <- c('French Southern and Antarctic Lands', 'TF', 'ATF', 'Antarctica')
  World_Final@data[World_Final@data$Country == 'Heard Island', ] <- c(
    'Heard Island',
    'HM',
    'HMD',
    'Oceania'
  )
  World_Final@data[World_Final@data$Country == 'Kosovo', ] <- c(
    'Kosovo',
    'XK',
    '???',
    'Europe'
  )
  World_Final@data[World_Final@data$Country == 'Siachen Glacier', ] <- c(
    'Siachen Glacier',
    NA,
    NA,
    'Asia'
  )
  World_Final@data[World_Final@data$Country == 'South Georgia', ] <- c(
    'South Georgia',
    'GS',
    'SGS',
    'Antarctica'
  )
  World_Final@data[World_Final@data$Country == 'South Sandwich Islands', ] <- c(
    'South Sandwich Islands',
    'GS',
    'SGS',
    'Antarctica'
  )

  # Adicionando la información que se pasa como input --------------------------
  if (all(str_length(datos |> select({{ paises }}) |> pull()) == 3)) {
    varJoin <- "Alfa3"
  } else if (all(str_length(datos |> select({{ paises }}) |> pull()) == 2)) {
    varJoin <- "Alfa2"
  } else {
    varJoin <- "Country"
  }

  if (!estatico) {
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
    if (
      !(is.numeric(zoomMapa) && is.numeric(opacidad) && is.numeric(textSize))
    ) {
      stop(
        "\u00a1Los argumentos 'zoomMapa', 'opacidad' y 'textSize' deben ser un valor num\u00e9rico!",
        call. = FALSE
      )
    }
    opacidadClic <- ifelse(opacidad != 1, opacidad + 0.2, opacidad)
    textPaises <- paste0(textSize + 2, "px")
    # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

    datos <- datos |>
      select(Country := {{ paises }}, VarNumeric := {{ variable }})
    World_Final@data <- World_Final@data |>
      left_join(datos, by = join_by(!!sym(varJoin) == Country))

    Msj <- 'Map created by <a href="https://github.com/JeisonAlarcon">Jeison Alarc\u00f3n</a> &mdash; Data source: &copy; <a href="http://estadisticas.unal.edu.co/home/">DNPE</a>'
    if (naTo0) {
      World_Final@data <- World_Final@data |>
        dplyr::mutate(VarNumeric = replace_na(VarNumeric, 0))
      Comodin <- "%3g"
    } else {
      World_Final@data <- World_Final@data #|> dplyr::mutate(X = replace_na(X, "Sin Informaci\u00f3n"))
      Comodin <- "%s"
    }
    World_Final@data <- rename(World_Final@data, Statistic = VarNumeric)
    # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

    # Ubicación de los centroides correspondientes a cada departamento (extraídos del objeto 'Polygons_Depto' y no de 'Capitales')
    Coordenadas <- sp::coordinates(World_Final)
    Centroides_World <- as.data.frame(Coordenadas)
    Centroides_World$Pais <- rownames(Centroides_World)
    colnames(Centroides_World) <- c("Lon", "Lat", "Country")
    rownames(Centroides_World) <- NULL

    listCountrys <- Centroides_World$Country
    if (!missingArg(centroideMapa)) {
      centroideMapa <- toupper(centroideMapa)
      if (centroideMapa %NotIN% toupper(listCountrys)) {
        op <- options()
        options(warning.length = 8000L)
        stop(
          paste(
            c(
              '\u00a1Por favor introduzca el nombre de un pa\u00eds correcto! Las opciones son:',
              toupper(listCountrys)
            ),
            collapse = "\n\t \u25a0 "
          ),
          call. = FALSE
        )
        options(op)
      }
    } else {
      centroideMapa <- "MEXICO"
    }
    CentroWorld <- Centroides_World |> filter(toupper(Country) == centroideMapa)
    # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

    # Extracción de la segregación y el periodo en cuestión del título ingresado
    TitleSplit <- strsplit(titulo, "\\s+")
    Segregacion <- TitleSplit[[1]][1]
    Periodo <- TitleSplit[[1]][2]
    TitlePais <- paste0(
      "<center>",
      Segregacion,
      " por<br/>departamento<br/>",
      Periodo,
      "</center>"
    )

    # Labels
    Labels_Paises <- sprintf(
      paste0("<strong> %s </strong> <br>", Comodin, " ", tolower(Segregacion)),
      World_Final@data$Country,
      World_Final@data$Statistic
    ) |>
      lapply(htmltools::HTML)
    # -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
    if (tipo == "pais") {
      # MAPA POR PAÍSES
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

      Mapa <- leaflet(data = World_Final) |> addTiles(attribution = Msj)
      for (i in seq_len(length(Baldosas))) {
        Mapa <- Mapa |>
          addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
      }

      Mapa <- Mapa |>
        # Definiendo el centro del mapa.
        setView(
          lat = CentroWorld$Lat,
          lng = CentroWorld$Lon,
          zoom = zoomMapa
        ) |>
        # Adición de grupos de control de capas.
        addLayersControl(
          baseGroups = Baldosas.names,
          options = layersControlOptions(collapsed = compacto)
        ) |>
        # Adición de los polígonos, su relleno y etiquetas con el nombre y estadístico.
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
          label = Labels_Paises,
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
        # Adición de los textos indicando el nombre del país.
        addLabelOnlyMarkers(
          lng = Centroides_World$Lon,
          lat = Centroides_World$Lat,
          label = paste0(sapply(Centroides_World$Country, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            textsize = textPaises
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
        )
    } else if (tipo == "sinopais") {
      # MAPA POR PAÍS (SI/NO)
      if (missingArg(colBorde)) {
        colBorde <- "#16ABAB"
      }
      if (!missingArg(cortes)) {
        warning(
          paste0(
            "\u00a1Usted ha ingresado alg\u00fan valor en el argumento 'cortes' y ha seleccionado el tipo de mapa 'SiNoMpios'!",
            "\n\t Por lo cual se omitir\u00e1 el valor ingresado, pues este tipo de mapa no permite un cambio en los cortes, pues es un intervalo binario."
          ),
          call. = FALSE
        )
      }
      if (!missingArg(colores)) {
        if (length(colores) != 2L) {
          stop(
            "\u00a1La longitud del argumento 'colores' es diferente a 2! Este mapa es binario por lo cual necesita especificar solamente 2 colores.",
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

      Mapa <- leaflet(data = World_Final) |> addTiles(attribution = Msj)
      for (i in seq_len(length(Baldosas))) {
        Mapa <- Mapa |>
          addProviderTiles(provider = Baldosas[i], group = Baldosas.names[i])
      }

      Mapa <- Mapa |>
        setView(
          lat = CentroWorld$Lat,
          lng = CentroWorld$Lon,
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
          label = Labels_Paises,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "12px",
            direction = "auto"
          )
        ) |>
        addPolylines(
          data = World_Final,
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
            paste0("0 ", Segregacion),
            paste0("1 o m\u00e1s ", Segregacion)
          ),
          title = titulo
        ) |>
        addLabelOnlyMarkers(
          lng = Centroides_World$Lon,
          lat = Centroides_World$Lat,
          label = paste0(sapply(Centroides_World$Country, str_to_title)),
          group = "lupa",
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "top",
            textOnly = TRUE,
            textsize = textPaises
          )
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
        )
    }

    if (!limpio) {
      TextJS <- paste0(
        "function(btn, map){ map.setView(L.latLng(",
        CentroWorld$Lat,
        ", ",
        CentroWorld$Lon,
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
    if (missingArg(estilo) || is.null(estilo$continente)) {
      InContinent <- "Todos"
    } else {
      continente <- str_to_title(estilo$continente)
      if (
        any(
          continente %NotIN%
            c(
              "Todos",
              "Africa",
              "Americas",
              "Asia",
              "Europe",
              "Oceania",
              "Antarctica"
            )
        )
      ) {
        stop(
          '\u00a1Por favor introduzca un continente correcto! Las opciones son: "Todos", "Africa", "Americas", "Asia", "Europe", "Oceania" y "Antarctica"',
          call. = FALSE
        )
      }
      InContinent <- continente
    }

    opacidad <- ifelse(missingArg(opacidad), 0.8, opacidad)
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

    World_Final@data <- World_Final@data |>
      left_join(datos, by = join_by(!!sym(varJoin) == {{ paises }}))

    # ----------------------------------------------------------------------------
    World_Final_SF <- sf::st_as_sf(World_Final)
    World_Final_SF$mid <- sf::st_centroid(World_Final_SF$geometry)
    # st_crs(World_Final_SF)

    if (!missingArg(grupo)) {
      World_Final_SF <- World_Final_SF |> filter(!is.na({{ grupo }}))
    }
    if (all(InContinent %NotIN% "Todos")) {
      World_Final_SF <- World_Final_SF |> filter(Continent %in% InContinent)
    }
    if (naTo0) {
      World_Final_SF <- World_Final_SF |>
        dplyr::mutate({{ variable }} := replace_na({{ variable }}, 0))
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

    if (missingArg(estilo) || is.null(estilo$Legend)) {
      ParmsLegend <- list(
        legend.position = "right",
        legend.direction = "vertical"
      )
    } else {
      ParmsLegend <- estilo$Legend
    }
    if (missingArg(estilo) || is.null(estilo$Labs)) {
      ParmsLabs <- list(subtitle = NULL, caption = NULL, tag = NULL)
    } else {
      ParmsLabs <- estilo$Labs
    }

    dots_geom_sf <- list(
      mapping = aes(fill = {{ variable }}),
      linewidth = LineWidth,
      color = LineColor
    )

    if (is.na(estilo$Style)) {
      cortesCute <- waiver()
      if (missingArg(colores)) {
        nLevels <- World_Final_SF |>
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

        cortesCute <- prettyNum(cortes[-1], big.mark = ",")
        NewVar <- cut(
          World_Final_SF |>
            select({{ variable }}) |>
            as.data.frame() |>
            select({{ variable }}) |>
            pull(),
          breaks = cortes,
          dig.lab = 5
        )
        World_Final_SF <- World_Final_SF |> mutate({{ variable }} := NewVar)
      } else if (tolower(estilo$Style) == "sino") {
        if (!missingArg(cortes)) {
          warning(
            paste0(
              "\u00a1Usted ha ingresado alg\u00fan valor en el argumento 'cortes' y ha seleccionado el tipo de mapa 'SiNo'!",
              "\n\t Por lo cual se omitir\u00e1 el valor ingresado, pues este tipo de mapa no permite un cambio en los cortes, pues es un intervalo binario."
            ),
            call. = FALSE
          )
        }
        if (!missingArg(colores)) {
          if (length(colores) != 2L) {
            stop(
              '\u00a1La longitud del argumento "colores" es diferente a 2! Este mapa es binario por lo cual necesita especificar solamente 2 colores.',
              call. = FALSE
            )
          }
        } else {
          colores <- c("#FDAE61", "#A6D96A")
        }

        cortesCute <- waiver()
        NewVar <- if_else(
          World_Final_SF |>
            select({{ variable }}) |>
            as.data.frame() |>
            select({{ variable }}) |>
            pull() >
            0,
          "1 o m\u00e1s .",
          "0 ."
        )
        World_Final_SF <- World_Final_SF |> mutate({{ variable }} := NewVar)
      }
    }

    Mapa <- ggplot(data = World_Final_SF) +
      do.call(geom_sf, dots_geom_sf) +
      labs(
        title = titulo,
        subtitle = ParmsLabs$subtitle,
        x = labelX,
        y = labelY,
        caption = ParmsLabs$caption,
        tag = ParmsLabs$tag
      ) +
      coord_sf(
        crs = "+proj=longlat +datum=WGS84",
        expand = FALSE,
        xlim = Xlim,
        ylim = Ylim
      ) +
      ThemeGG +
      do.call(theme, ParmsLegend)

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
          option = 'C',
          trans = "sqrt",
          alpha = opacidad,
          na.value = colNA
        )
    }

    if (!missingArg(grupo)) {
      Mapa <- Mapa + facet_wrap(facets = vars({{ grupo }}))
    }
    if (missingArg(estilo) || is.null(estilo$Labs)) {
      ParmsLabs <- list(subtitle = NULL, caption = NULL, tag = NULL)
    } else {
      ParmsLabs <- estilo$Labs
    }
    if (missingArg(estilo) || is.null(estilo$showISO)) {
      if (length(estilo$showISO) == 0) {
        showISO <- list(color = "#CF2226", size = 2)
      } else {
        showISO <- estilo$showISO
      }
      dots_geom_text <- append(
        list(aes(label = Alfa2), check_overlap = TRUE),
        showISO
      )
      Mapa <- Mapa + do.call(geom_sf_text, dots_geom_text)
    }
    if (!(missingArg(estilo) || is.null(estilo$scaleX))) {
      Mapa <- Mapa + scale_x_continuous(breaks = estilo$scaleX)
    }
    if (!(missingArg(estilo) || is.null(estilo$scaleY))) {
      Mapa <- Mapa + scale_y_continuous(breaks = estilo$scaleY)
    }

    if (!limpio) {
      Mapa <- Mapa +
        ggspatial::annotation_north_arrow(
          location = "bl",
          which_north = "true",
          pad_x = unit(0.4, "cm"),
          pad_y = unit(0.4, "cm"),
          style = ggspatial::north_arrow_fancy_orienteering
        ) +
        ggspatial::annotation_scale(location = "br", width_hint = 0.1)
    }
  }

  return(Mapa)
}
