#' Open Shiny App
#'
#' Launches the Poly4AT Shiny application for querying geographic data based on coordinates.
#'
#' @return A Shiny application.
#' @export
#'
#'
#' @import leaflet
#' @import sf
#' @import httr
#' @import shinydashboard
#' @importFrom DT dataTableOutput renderDataTable
#' @import leaflet.extras
#' @import utils
#'
#' @importFrom shiny shinyApp fluidRow column textInput selectInput actionButton
#' @importFrom shiny conditionalPanel reactiveVal reactiveValues renderUI
#' @importFrom shiny observe observeEvent modalDialog showModal updateSelectInput
#' @importFrom shiny downloadHandler
#'
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#'   poly4AT_processor()
#' }
#'
#'


poly4AT_processor <- function() {


  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Poly4AT"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Einzelkoordinate abfragen", tabName = "Singlerequest", icon = shiny::icon("location-dot")),
        shinydashboard::menuItem("Koordinatenliste abfragen", tabName = "Mulitrequest", icon = shiny::icon("location-dot")),
        shinydashboard::menuItem("\u00DCber Poly4AT", tabName = "About", icon = shiny::icon("circle-info"))
      )
    ),
    skin = "black",
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "Singlerequest",
                                shiny::fluidRow(
                                  shiny::column(width = 4,
                                                shiny::textInput("latitude", "Latitude:", value = ""),
                                                shiny::textInput("longitude", "Longitude:", value = ""),
                                                shiny::selectInput("year", "Jahr ausw\u00E4hlen:", choices = NULL),
                                                shiny::actionButton("updateMap", "Polygone anzeigen")),
                                  shiny::column(width = 8,
                                                shiny::conditionalPanel(condition = "!output.loading",
                                                                        leaflet::leafletOutput("map")))  # Korrektur hier
                                )),
        shinydashboard::tabItem(tabName = "Mulitrequest",
                                shiny::fluidRow(
                                  shiny::column(width = 4,
                                                shinydashboard::box(title = "Daten-Upload", status = "primary", solidHeader = TRUE, width = 12,
                                                                    shiny::fileInput('file1', 'Excel-Datei mit Koordinaten hochladen (Spalte 1: Name, Spalte 2: latitude, Spalte 3: longitude)',
                                                                                     accept = c(".xlsx")),
                                                                    shiny::actionButton("showTable", "Tabelle mit Koordinaten anzeigen", icon = shiny::icon("table"), style = "margin-bottom: 10px; width: 100%;"),
                                                                    shiny::br(),
                                                                    DT::dataTableOutput("file2")
                                                )
                                  ),

                                  shiny::column(width = 8,
                                                shinydashboard::box(maximizable = TRUE,
                                                                    title = "Jahresauswahl und Polygone", status = "primary", solidHeader = TRUE, width = 12,
                                                                    shiny::fluidRow(
                                                                      shiny::column(width = 6,
                                                                                    shiny::selectInput("yearmulti", "Jahr ausw\u00E4hlen:", choices = NULL, width = '100%')),
                                                                      shiny::column(width = 6,
                                                                                    shiny::actionButton("showmapmulti", "Polygone anzeigen", icon = shiny::icon("map"), style = "width: 100%;"))
                                                                    ),
                                                                    shiny::br(),
                                                                    shiny::conditionalPanel(condition = "!output.loading",
                                                                                            leaflet::leafletOutput("mapmulti", height = "500px")),  # Korrektur hier
                                                                    shiny::br()),

                                                shinydashboard::box(
                                                  width = 12,
                                                  shiny::actionButton("showshape", "Informationen anzeigen", icon = shiny::icon("info-circle"), style = "margin-bottom: 10px; width: 100%;"),
                                                  shiny::br(),
                                                  DT::dataTableOutput("shape"),
                                                  style = "overflow-y: auto; overflow-x: auto;"),

                                                shinydashboard::box(
                                                  shiny::downloadButton(outputId = "downloadShape", label = "Download Shape-Datei", style = "width: 100%;")
                                                )
                                  )
                                )
        ),

        shinydashboard::tabItem(
          tabName = "About",
          shiny::fluidRow(
            shiny::column(
              width = 8, offset = 2,
              style = "text-align: justify;",
              shiny::h2(shiny::strong("\u00DCber Poly4AT")),
              shiny::div(style = "margin-bottom: 20px;",
                         shiny::p("Diese Website wurde entwickelt, um die API-Schnittstelle zu INVEKOS Feldst\u00FCck-Polygone auch ohne Programmierkenntnisse nutzen zu k\u00F6nnen. Die App erm\u00F6glicht es, eine einzelne Koordinate abzufragen sowie eine Koordinatenliste hochzuladen, um Informationen \u00FCber die Schl\u00E4ge zu erhalten.")
                         ),
              shiny::h3(shiny::strong("Datenweiterverarbeitung")),
              shiny::div(style = "margin-bottom: 20px;",
                         shiny::p("Die auf dieser Website angezeigten Daten stammen von der INVEKOS API-Schnittstelle. Die Weiterverwendung der Daten ist nur mit entsprechender Zitierung von AMA INVEKOS erlaubt."),

                         shiny::p("F\u00FCr die Daten haben wir folgenden Zitiervorschlag:"),
                         shiny::tags$blockquote(
                           "AMA. (Jahr der Abfrage). OGC Features API.
                           Abgerufen am dd.mm.yyyy, von https://gis.lfrz.gv.at/api/geodata/i009501/ogc/features/v1",
                           style = "font-style: italic; color: #555;"
                         ),

                         shiny::p("F\u00FCr Poly4AT verwenden Sie die Citation in R:"),
                         shiny::tags$blockquote(
                           "Wieser S (2024). Poly4AT: Access INVEKOS API for Field Polygons. R package version 1.0.",
                           style = "font-style: italic; color: #555;"
                         ),
                         shiny::p("Hier ist der BibTeX-Eintrag f\u00FCr LaTeX-Benutzer:"),
                         shiny::tags$blockquote(
                           "@Manual{Poly4AT,\n",
                           "  title = {Poly4AT: Access INVEKOS API for Field Polygons},\n",
                           "  author = {Sebastian Wieser},\n",
                           "  year = {2024},\n",
                           "  note = {R package version 1.0}\n",
                           "}",
                           style = "font-family: monospace; font-size: 14px; color: #555;"
                         )
              ),

              shiny::h3(shiny::strong("Datenschutz")),
              shiny::div(style = "margin-bottom: 20px;",
                         shiny::p("Bitte beachten Sie, dass wir keine pers\u00F6nlichen Daten ohne Zustimmung sammeln oder weitergeben."),
                         shiny::tags$ul(
                           shiny::tags$li("Ihre Inputdaten werden nicht gespeichert und sind nach der Sitzung gel\u00F6scht.")
                         )
              ),

              shiny::h3(shiny::strong("Lizenz")),
              shiny::div(style = "margin-bottom: 20px;",
                         shiny::p("Der Inhalt und der Quellcode dieser Website unterliegen der MIT Licence. Dies bedeutet, dass Sie den Quellcode verwenden, modifizieren und weiterverbreiten k\u00F6nnen, sofern die urspr\u00FCngliche Urheberschaft anerkannt wird.")
              ),

              shiny::h3(shiny::strong("Kontakt")),
              shiny::p("Wenn Sie Fragen oder Anmerkungen bez\u00FCglich Poly4AT haben, kontaktieren Sie bitte ",
                       shiny::a("poly4at@gmail.com", href = "mailto:poly4at@gmail.com"))
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    all_filtered_sf <- shiny::reactiveVal(NULL)
    coordinatesinput <- shiny::reactiveValues(coordinates = NULL)

    output$loading <- shiny::renderUI({
      shiny::actionButton("updateMap", "Update Map")
    })

    utils::data("austria_boundary", package = "Poly4AT")


    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
        leaflet.extras::addFullscreenControl() %>%
        leaflet::setView(lng = 14.5501, lat = 47.5162, zoom = 7) %>%
        leaflet::addLayersControl(
          baseGroups = c("Satellite", "Street"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    shiny::observe({
      res <- httr::GET("https://gis.lfrz.gv.at/api/geodata/i009501/ogc/features/v1/openapi?f=application%2Fvnd.oai.openapi%2Bjson%3Bversion%3D3.0")
      data <- jsonlite::fromJSON(httr::content(res, "text"))
      InfoDaten <- as.data.frame(data$components$parameters$collectionId$schema$enum)

      colnames(InfoDaten)[1] <- "Year"
      InfoDaten <- InfoDaten[grep("invekos_schlaege", InfoDaten$Year, ignore.case = TRUE), , drop = FALSE]
      InfoDaten <- InfoDaten[grep("polygon", InfoDaten$Year, ignore.case = TRUE), , drop = FALSE]

      InfoDaten$Year <- sub("i009501:", "", InfoDaten$Year)
      shiny::updateSelectInput(session, "year", choices = InfoDaten$Year)
    })

    shiny::observeEvent(input$updateMap, {
      latitude <- as.numeric(input$latitude)
      longitude <- as.numeric(input$longitude)
      selected_year <- input$year

      point <- sf::st_point(c(longitude, latitude))
      point_sf <- sf::st_sfc(point, crs = sf::st_crs(austria_boundary))


      if (sf::st_contains(austria_boundary, point_sf, sparse = FALSE)) {
        bbox <- paste(longitude - 0.001, latitude - 0.001, longitude + 0.001, latitude + 0.001, sep = ",")

        base_url <- "https://gis.lfrz.gv.at/api/geodata/i009501/ogc/features/v1/collections/"

        get_thredds_url <- function(collection, bbox) {
          part1 <- base_url
          part2 <- collection
          part3 <- "/items?limit=100&bbox="
          part4 <- bbox
          part5 <- "&filter-lang=cql-text&additionalProp1="
          return(paste0(part1, part2, part3, part4, part5))
        }

        url <- get_thredds_url(selected_year, bbox)

        AnfrageDaten2 <- tryCatch({
          geojsonsf::geojson_sf(url)
        }, error = function(e) {
          NULL
        })

        if (!is.null(AnfrageDaten2)) {
          contains <- sf::st_contains(AnfrageDaten2, point)
          contains_ids <- which(lengths(contains) > 0)

          leaflet::leafletProxy("map") %>%
            leaflet::clearShapes() %>%
            leaflet::addPolygons(data = AnfrageDaten2,
                                 popup =  ~paste("Fl\u00E4che ha: ", sprintf("%.1f", sl_flaeche_brutto_ha),
                                                "<br>",
                                                "Schlagnutzung: ", snar_bezeichnung)) %>%
            leaflet::addMarkers(lng = longitude, lat = latitude) %>%
            leaflet::setView(lng = longitude, lat = latitude, zoom = 15)
        } else {
          showModal(modalDialog(
            title = "Error",
            "Failed to retrieve data from the server. Please try again later."
          ))
        }
      } else {
        shiny::showModal(shiny::modalDialog(
          title = "Fehler",
          "Die eingegebenen Koordinaten liegen au\u00DFerhalb von \u00D6sterreich. Bitte versuchen Sie es erneut."
        ))
      }
    })

    shiny::observeEvent(input$showTable, {
      output$file2 <- DT::renderDataTable({
        inFile <- utils::head(input$file1)
        if (is.null(inFile))
          return(NULL)
        readxl::read_excel(inFile$datapath)
      })
    })

    shiny::observe({
      res <- httr::GET("https://gis.lfrz.gv.at/api/geodata/i009501/ogc/features/v1/openapi?f=application%2Fvnd.oai.openapi%2Bjson%3Bversion%3D3.0")
      data <- jsonlite::fromJSON(httr::content(res, "text"))
      InfoDaten <- as.data.frame(data$components$parameters$collectionId$schema$enum)

      colnames(InfoDaten)[1] <- "Year"
      InfoDaten <- InfoDaten[grep("invekos_schlaege", InfoDaten$Year, ignore.case = TRUE), , drop = FALSE]
      InfoDaten <- InfoDaten[grep("polygon", InfoDaten$Year, ignore.case = TRUE), , drop = FALSE]

      InfoDaten$Year <- sub("i009501:", "", InfoDaten$Year)
      shiny::updateSelectInput(session, "yearmulti", choices = InfoDaten$Year)
    })

    output$mapmulti <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
        leaflet.extras::addFullscreenControl() %>%
        leaflet::setView(lng = 14.5501, lat = 47.5162, zoom = 7) %>%
        leaflet::addLayersControl(
          baseGroups = c("Satellite", "Street"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })

    shiny::observeEvent(input$showmapmulti, {
      selected_year <- input$yearmulti
      inFile <- readxl::read_excel(input$file1$datapath)

      coordinates <- inFile[, c("Name", "latitude", "longitude")]


      valid_coordinates <- list()
      invalid_coordinates <- list()

      for (i in 1:nrow(coordinates)) {
        point <- sf::st_point(c(coordinates$longitude[i], coordinates$latitude[i]))
        point_sf <- sf::st_sfc(point, crs = sf::st_crs(austria_boundary))

        if (sf::st_contains(austria_boundary, point_sf, sparse = FALSE)) {
          valid_coordinates[[length(valid_coordinates) + 1]] <- coordinates[i, ]
        } else {
          invalid_coordinates[[length(invalid_coordinates) + 1]] <- coordinates[i, ]
        }
      }

      if (length(invalid_coordinates) > 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Fehler",
          paste("Die folgenden Koordinaten liegen au\u00DFerhalb von \u00D6sterreich:",
                paste(sapply(invalid_coordinates, function(x) x[1]), collapse = ", "))
        ))
      }

      coordinates <- do.call(rbind, valid_coordinates)

      coordinates$bbox <- mapply(function(lon, lat) {
        paste(lon - 0.001, lat - 0.001, lon + 0.001, lat + 0.001, sep = ",")
      }, coordinates$longitude, coordinates$latitude)

      base_url <- "https://gis.lfrz.gv.at/api/geodata/i009501/ogc/features/v1/collections/"

      get_thredds_url <- function(collection, bbox) {
        part1 <- base_url
        part2 <- collection
        part3 <- "/items?limit=100&bbox="
        part4 <- bbox
        part5 <- "&filter-lang=cql-text&additionalProp1="
        return(paste0(part1, part2, part3, part4, part5))
      }

      ergebnisse_liste <- list()

      for (i in 1:nrow(coordinates)) {
        bbox <- coordinates$bbox[i]
        url <- get_thredds_url(selected_year, bbox)
        ergebnisse_liste[[coordinates$Name[i]]] <- geojsonsf::geojson_sf(url)
      }

      filtered_polygons <- list()

      for (i in seq_along(ergebnisse_liste)) {
        name <- names(ergebnisse_liste)[i]
        current_result <- ergebnisse_liste[[i]]
        current_point <- sf::st_point(c(coordinates$longitude[i], coordinates$latitude[i]))
        current_point <- sf::st_sfc(current_point, crs = 4326)
        contains <- sf::st_contains(current_result, current_point)
        contains_ids <- which(lengths(contains) > 0)
        filtered_polygons[[name]] <- current_result[contains_ids, ]
      }

      all_filtered_sf(all_filtered_sf <- do.call(rbind, filtered_polygons))

      shiny::observe({
        coordinatesinput$coordinates <- coordinates
      })

      leaflet::leafletProxy("mapmulti") %>%
        leaflet::clearShapes() %>%
        leaflet::addProviderTiles('Esri.WorldImagery') %>%
        leaflet::addPolygons(data = all_filtered_sf(),
                             popup =  ~paste("Fl\u00E4che ha: ", sprintf("%.1f", all_filtered_sf()$sl_flaeche_brutto_ha),
                                           "<br>",
                                           "Schlagnutzung: ", all_filtered_sf()$snar_bezeichnung)) %>%
        leaflet::addMarkers(data = coordinates,
                            popup = paste(coordinates$Name)) %>%
        leaflet::setView(lng = mean(coordinates$longitude), lat = mean(coordinates$latitude), zoom = 12)
    })

    shiny::observeEvent(input$showshape, {
      output$shape <- DT::renderDataTable({
        tableshape <- sf::st_drop_geometry(all_filtered_sf())
        tableshape$Name <- rownames(tableshape)
        coordinates <- coordinatesinput$coordinates
        tableshape <- merge(tableshape, coordinates[,-4], by="Name")
        tableshape
      })
    })

    shape_map <- all_filtered_sf

    output$downloadShape <- shiny::downloadHandler(
      filename = function() {
        "Poly4AT.gpkg"
      },
      content = function(file) {
        sf::st_write(all_filtered_sf(), file)
      },
      contentType = "application/octet-stream"
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}


poly4AT_processor()
