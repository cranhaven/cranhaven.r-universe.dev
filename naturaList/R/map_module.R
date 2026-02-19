#' Check the occurrence records in a interactive map module
#'
#' Allows to delete occurrence records and to select occurrence points by
#' classification levels or by drawing spatial polygons.
#'
#' @param occ.cl Data frame with occurrence records information already
#'   classified by \code{\link{classify_occ}} function.
#' @param action a string with `"clean"` or `"flag"` which defines the action
#'   of `map_module` function with the occurrence dataset. Default is `"clean"`.
#'   If the string is `"clean"` the dataset returned only the occurrences records
#'   selected by the user. If the string is `"flag"`, a column named
#'   `map_module_flag` is added in the output dataset, with tags `selected` and
#'   `deleted`, following the choices of the user in the application.
#' @param occurrence.id column name of \code{occ} with link or code for the
#'  occurrence record. See in
#'  \href{https://dwc.tdwg.org/terms/#dwc:occurrenceID}{Darwin Core Format}
#' @param occ.id deprecated, use \code{occurrence.id} instead
#' @param species column name of \code{occ} with the species names.
#' @param scientific.name deprecated, use \code{species} instead.
#' @param identified.by column name of \code{occ} with the name of who
#'  determined the species.
#' @param determined.by deprecated, use \code{identified.by} instead
#' @param decimal.longitude column name of \code{occ} longitude in decimal
#'  degrees.
#' @param longitude deprecated, use \code{decimal.longitude} instead
#' @param decimal.latitude column name of \code{occ} latitude in decimal
#'  degrees.
#' @param latitude deprecated, use \code{decimal.latitude} instead
#' @param basis.of.record column name with the specific nature of the data
#'  record. See details.
#' @param basis.of.rec deprecated, use \code{basis.of.record} instead.
#' @param media.type column name of \code{occ} with the media type of recording.
#'  See details.
#' @param institution.code column name of \code{occ} with the name (or acronym)
#'  in use by the institution having custody of the object(s) or information
#'  referred to in the record.
#' @param institution.source deprecated, use \code{institution.code} instead.
#' @param collection.code column name of \code{occ} with The name, acronym,
#'  code, or initials identifying the collection or data set from which the
#'  record was derived.
#' @param catalog.number column name of \code{occ} with an identifier
#'  (preferably unique) for the record within the data set or collection.
#' @param year Column name of \code{occ} the four-digit year in which the
#'  Event occurred, according to the Common Era Calendar.
#' @param year.event deprecated, use \code{year} instead.
#' @param date.identified Column name of \code{occ} with the date on which the
#'  subject was determined as representing the Taxon.
#'
#' @return Data frame with the same columns of \code{occ.cl}.
#'
#' @seealso \code{\link{classify_occ}}
#'
#' @examples
#' \dontrun{
#' data("A.setosa")
#' data("speciaLists")
#'
#' occ.class <- classify_occ(A.setosa, speciaLists)
#' occ.selected <- map_module(occ.class)
#' occ.selected
#'
#' }
#'
#' @author Arthur V. Rodrigues
#'
#' @export

map_module <- function(
  occ.cl,
  action = "clean", # clean or flag
  institution.code = "institutionCode",
  collection.code = "collectionCode",
  catalog.number = "catalogNumber",
  year = "year",
  date.identified = "dateIdentified",
  species = "species",
  identified.by = "identifiedBy",
  decimal.latitude = "decimalLatitude",
  decimal.longitude = "decimalLongitude",
  basis.of.record = "basisOfRecord",
  media.type = "mediaType",
  occurrence.id = "occurrenceID",
  institution.source, # deprecated
  year.event, # deprecated
  scientific.name, # deprecated
  determined.by, # deprecated
  latitude, # deprecated
  longitude, # deprecated
  basis.of.rec, # deprecated
  occ.id # deprecated
){
  # new arguments  ----------------------------------------------------------

  if (!missing(institution.source)) {
    warning("argument 'institution.source' is deprecated; please use 'institution.code' instead.",
            call. = FALSE)
    institution.code <- institution.source
  }

  if (!missing(year.event)) {
    warning("argument 'year.event' is deprecated; please use 'year' instead.",
            call. = FALSE)
    year <- year.event
  }

  if (!missing(scientific.name)) {
    warning("argument 'scientific.name' is deprecated; please use 'species' instead.",
            call. = FALSE)
    species <- scientific.name
  }

  if (!missing(determined.by)) {
    warning("argument 'determined.by' is deprecated; please use 'identified.by' instead.",
            call. = FALSE)
    identified.by <- determined.by
  }

  if (!missing(latitude)) {
    warning("argument 'latitude' is deprecated; please use 'decimal.latitude' instead.",
            call. = FALSE)
    decimal.latitude <- latitude
  }

  if (!missing(longitude)) {
    warning("argument 'longitude' is deprecated; please use 'decimal.longitude' instead.",
            call. = FALSE)
    decimal.longitude <- longitude
  }

  if (!missing(basis.of.rec)) {
    warning("argument 'basis.of.rec' is deprecated; please use 'basis.of.record' instead.",
            call. = FALSE)
    basis.of.record <- basis.of.rec
  }

  if (!missing(occ.id)) {
    warning("argument 'occ.id' is deprecated; please use 'occurrence.id' instead.",
            call. = FALSE)
    occurrence.id <- occ.id
  }

  if(!any(action == c("clean", "flag"))){
    stop("argument action must be 'clean' or 'flag'")
  }

  requireNamespace("shiny")
  requireNamespace("shinyWidgets")
  requireNamespace("shinydashboard")
  requireNamespace("leaflet")
  requireNamespace("sp")
  requireNamespace("raster")
  requireNamespace("leaflet.extras")
  requireNamespace("shinyLP")


  final.df <- shiny::runApp(list(
    ui = shiny::fluidPage(
      shiny::titlePanel("Map Module - naturaList"),
      shiny::fluidRow(
        shiny::column(
          3,
          shiny::uiOutput("species"),
          shinydashboard::box(
            width = NULL,
            shinyWidgets::checkboxGroupButtons(
              inputId = "grbox",
              label = "What levels should be
            maintained in the output data set?",
              choices = c(
                "Level 1" = "1",
                "Level 2" = "2",
                "Level 3" = "3",
                "Level 4" = "4",
                "Level 5" = "5",
                "Level 6" = "6"
              ),
              justified = T,
              status = "info",
              size = "xs",
              direction = "vertical",
              checkIcon = list(
                yes = shiny::icon("ok", lib = "glyphicon"),
                no = shiny::icon("remove", lib = "glyphicon")
              ),
              selected = c(
                "1",
                "2",
                "3",
                "4",
                "5",
                "6"
              ),
              width = "100%"
            ),
            shinyWidgets::materialSwitch("del_mkr_button",
                                         label = "Delete points with click",
                                         status = "danger"
            )
          ),
          shinydashboard::box(
            width = NULL, title = "Download",
            solidHeader = T, status = "success",
            shiny::textOutput("sel_display"),
            shiny::textOutput("down.class.text"),
            shinyWidgets::actionBttn("done", "Done!",
                                     style = "bordered",
                                     color = "success",
                                     size = "lg"
            )
          )
        ),
        shiny::column(
          9,
          leaflet::leafletOutput("map", height = 500)
        )
      )
    ),

    server = function(input, output, session) {
      values <- shiny::reactiveValues()

      output$species <- shiny::renderUI({
        species <- as.character(unique(occ.cl[, species]))
        if (length(species) == 1) {
          htmltools::h4(
            htmltools::strong(
              htmltools::em(shiny::textOutput("txt.sp"))
            )
          )
        }
      })

      output$txt.sp <- shiny::renderText({
        as.character(unique(occ.cl[, species]))
      })

      ## occurrences dataset
      occ.cl$id <- row.names(occ.cl)
      values$occur <- occ.cl

      ## Occurrences split by classification
      values$Level_1 <- grep("1", occ.cl$naturaList_levels)
      values$Level_2 <- grep("2", occ.cl$naturaList_levels)
      values$Level_3 <- grep("3", occ.cl$naturaList_levels)
      values$Level_4 <- grep("4", occ.cl$naturaList_levels)
      values$Level_5 <- grep("5", occ.cl$naturaList_levels)
      values$Level_6 <- grep("6", occ.cl$naturaList_levels)

      ## Data frame with points to be deleted with click
      values$MK <- data.frame(
        id = character(),
        x = numeric(),
        y = numeric()
      )

      ## Lines to be inserted in values$MK
      values$add_row <- data.frame(
        id = character(),
        x = numeric(),
        y = numeric()
      )

      # data frame to create polygon
      values$pol <- list()


      ## List of polygons to select points
      values$list.pol.df <- list()

      ## clickedMarker
      values$clickedMarker <- NULL




# leaflet interactive map -------------------------------------------------

     output$map <- leaflet::renderLeaflet({
        values$pt.ctr1 <- values$occur[values$Level_1, ]
        values$pt.ctr2 <- values$occur[values$Level_2, ]
        values$pt.ctr3 <- values$occur[values$Level_3, ]
        values$pt.ctr4 <- values$occur[values$Level_4, ]
        values$pt.ctr5 <- values$occur[values$Level_5, ]
        values$pt.ctr6 <- values$occur[values$Level_6, ]

        leaflet::leaflet("map") %>%
          # Add two tiles
          leaflet::addTiles(
            options = leaflet::providerTileOptions(noWrap = TRUE),
            group = "StreetMap"
          ) %>%
          leaflet::addProviderTiles(
            "Esri.WorldImagery",
            group = "Satellite"
          ) %>%
          # Add marker groups
          leaflet::addCircleMarkers(
            data = values$pt.ctr6,
            lng = values$pt.ctr6[, decimal.longitude],
            lat = values$pt.ctr6[, decimal.latitude],
            radius = 6,
            layerId = row.names(values$pt.ctr6),
            popup = paste(
              htmltools::strong("ID:"), values$pt.ctr6[, occurrence.id], "<br>",
              htmltools::strong("Institution Source:"), values$pt.ctr6[, institution.code], "<br>",
              htmltools::strong("Determined by:"), values$pt.ctr6[, identified.by], "<br>",
              htmltools::strong("Year of data colection:"), values$pt.ctr6[, year], "<br>",
              htmltools::strong("Date Identified:"), (values$pt.ctr6[, date.identified]), "<br>"
            ),
            fillColor = "purple", stroke = F, fillOpacity = 0.8, group = "Level 6"
          ) %>%
          leaflet::addCircleMarkers(
            data = values$pt.ctr5,
            lng = values$pt.ctr5[, decimal.longitude],
            lat = values$pt.ctr5[, decimal.latitude],
            radius = 6,
            layerId = row.names(values$pt.ctr5),
            popup = paste(
              htmltools::strong("ID:"), values$pt.ctr5[, occurrence.id], "<br>",
              htmltools::strong("Institution Source:"), values$pt.ctr5[, institution.code], "<br>",
              htmltools::strong("Determined by:"), values$pt.ctr5[, identified.by], "<br>",
              htmltools::strong("Year of data colection:"), values$pt.ctr5[, year], "<br>",
              htmltools::strong("Date Identified:"), (values$pt.ctr5[, date.identified]), "<br>"
            ),
            fillColor = "blue", stroke = F, fillOpacity = 0.8, group = "Level 5"
          ) %>%
          leaflet::addCircleMarkers(
            data = values$pt.ctr4,
            lng = values$pt.ctr4[, decimal.longitude],
            lat = values$pt.ctr4[, decimal.latitude],
            radius = 6,
            layerId = row.names(values$pt.ctr4),
            popup = paste(
              htmltools::strong("ID:"), values$pt.ctr4[, occurrence.id], "<br>",
              htmltools::strong("Institution Source:"), values$pt.ctr4[, institution.code], "<br>",
              htmltools::strong("Determined by:"), values$pt.ctr4[, identified.by], "<br>",
              htmltools::strong("Year of data colection:"), values$pt.ctr4[, year], "<br>",
              htmltools::strong("Date Identified:"), (values$pt.ctr4[, date.identified]), "<br>"
            ),
            fillColor = "darkgreen", stroke = F, fillOpacity = 0.8, group = "Level 4"
          ) %>%
          leaflet::addCircleMarkers(
            data = values$pt.ctr3,
            lng = values$pt.ctr3[, decimal.longitude],
            lat = values$pt.ctr3[, decimal.latitude],
            radius = 6,
            layerId = row.names(values$pt.ctr3),
            popup = paste(
              htmltools::strong("ID:"), values$pt.ctr3[, occurrence.id], "<br>",
              htmltools::strong("Institution Source:"), values$pt.ctr3[, institution.code], "<br>",
              htmltools::strong("Determined by:"), values$pt.ctr3[, identified.by], "<br>",
              htmltools::strong("Year of data colection:"), values$pt.ctr3[, year], "<br>",
              htmltools::strong("Date Identified:"), (values$pt.ctr3[, date.identified]), "<br>"
            ),
            fillColor = "yellow", stroke = F, fillOpacity = 0.8, group = "Level 3"
          ) %>%
          leaflet::addCircleMarkers(
            data = values$pt.ctr2,
            lng = values$pt.ctr2[, decimal.longitude],
            lat = values$pt.ctr2[, decimal.latitude],
            radius = 6,
            layerId = row.names(values$pt.ctr2),
            popup = paste(
              htmltools::strong("ID:"), values$pt.ctr2[, occurrence.id], "<br>",
              htmltools::strong("Institution Source:"), values$pt.ctr2[, institution.code], "<br>",
              htmltools::strong("Determined by:"), values$pt.ctr2[, identified.by], "<br>",
              htmltools::strong("Year of data colection:"), values$pt.ctr2[, year], "<br>",
              htmltools::strong("Date Identified:"), values$pt.ctr2[, date.identified], "<br>"
            ),
            fillColor = "orange", stroke = F, fillOpacity = 0.8, group = "Level 2"
          ) %>%
          leaflet::addCircleMarkers(
            data = values$pt.ctr1,
            lng = values$pt.ctr1[, decimal.longitude],
            lat = values$pt.ctr1[, decimal.latitude],
            radius = 6,
            layerId = row.names(values$pt.ctr1),
            popup = paste(
              htmltools::strong("ID:"), values$pt.ctr1[, occurrence.id], "<br>",
              htmltools::strong("Institution Source:"), values$pt.ctr1[, institution.code], "<br>",
              htmltools::strong("Determined by:"), values$pt.ctr1[, identified.by], "<br>",
              htmltools::strong("Year of data colection:"), values$pt.ctr1[, year], "<br>",
              htmltools::strong("Date Identified:"), (values$pt.ctr1[, date.identified]), "<br>"
            ),
            fillColor = "red", stroke = F, fillOpacity = 0.8, group = "Level 1"
          ) %>%
          # Add the control widget
          leaflet::addLayersControl(
            overlayGroups = c(
              "Level 1",
              "Level 2",
              "Level 3",
              "Level 4",
              "Level 5",
              "Level 6"
            ),
            baseGroups = c("StreetMap", "Satellite"),
            options = leaflet::layersControlOptions(collapsed = TRUE)
          ) %>%
          ## Add tool to design poligons shapes to selection
          leaflet.extras::addDrawToolbar(
            targetGroup = "Markers",
            polylineOptions = F,
            polygonOptions = T,
            circleOptions = F,
            rectangleOptions = F,
            markerOptions = F,
            circleMarkerOptions = F,
            editOptions = leaflet.extras::editToolbarOptions(
              selectedPathOptions = leaflet.extras::selectedPathOptions()
            )
          ) %>%
          leaflet::addLegend("bottomright",
                             colors = c("red", "orange", "yellow", "darkgreen", "blue", "purple"),
                             labels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5", "Level 6"),
                             opacity = 0.8
          )
      })

      ## Clear points clicked selected
      shiny::observeEvent(input$del_mkr_button, {
        if (input$del_mkr_button == FALSE) {
          values$add_row <- data.frame(
            id = character(),
            x = numeric(),
            y = numeric()
          )
        }


        shiny::observeEvent(input$map_marker_click,
                     {values$clickedMarker <- input$map_marker_click})

        shiny::observeEvent(input$del_mkr_button,
                     {values$clickedMarker <- NULL})

        ## Delete points with click
        shiny::observeEvent(input$del_mkr_button, {

          proxy <- leaflet::leafletProxy("map")

          shiny::observeEvent(values$clickedMarker, {
            if (input$del_mkr_button == TRUE &
                !is.null(values$clickedMarker)) {
              values$add_row <- data.frame(
                id = input$map_marker_click$id,
                x = input$map_marker_click$lng,
                y = input$map_marker_click$lat
              )
              values$MK <- rbind(values$MK, values$add_row)
              dp <- duplicated(values$MK$id)

              if (sum(dp) >= 1) {
                del <- values$MK[, 1] %in% values$MK[dp, 1]
              }

              id <- values$add_row$id
              proxy %>%
                leaflet::removeMarker(id)
            }
          })


        })


        ## Data.frame to create polygon
        shiny::observeEvent(input$map_draw_all_features, {
          if (length(input$map_draw_all_features$features) == 0) {
            values$list.pol.df <- list()
          }
          if (length(input$map_draw_all_features$features) > 0) {
            for (i in 1:length(input$map_draw_all_features$features)) {
              values$list.pol.df[[i]] <-
                pol.coords(input$map_draw_all_features$features[[i]])
            }
          }
        })

        ## Select points accordingly to classification levels
        shiny::observeEvent(input$grbox, {
          # occurrence data
          occ.df <- values$occur

          pttn <- paste(input$grbox, collapse = "|")
          values$g.cri <- grep(pttn, occ.df$naturaList_levels)
        })

        shiny::observeEvent(is.null(input$grbox), {
          if (is.null(input$grbox)) {
            values$g.cri <- NULL
          }
        })

        ## Final data frame
        # Selected by levels chose and polygons created
        shiny::observeEvent(input$grbox, {
          output$sel_display <- shiny::renderText({
            if (is.null(values$g.cri) | length(values$g.cri) == 0) {
              values$sel.points <- data.frame()
            } else {
              occ.df <- values$occur
              n.id <- as.character(values$MK$id)
              del <- row.names(occ.df) %in% n.id ## deleted with click
              c.d <- values$g.cri %in% which(!del) ## deleted with click + selected criteria (box)
              values$pol <- list()

              if (length(values$list.pol.df) == 1) {
                values$pol <- make.polygon(values$list.pol.df[[1]])
              }


              if (length(values$list.pol.df) > 1) {
                values$pol <- raster::bind(
                  lapply(values$list.pol.df, make.polygon)
                )
              }

              df.crit <- values$occur[values$g.cri[c.d], ]

              spt.df <- sp::SpatialPointsDataFrame(
                df.crit[, c(decimal.longitude, decimal.latitude)], df.crit
              )


              if (length(values$pol) == 0) {
                n.col <- ncol(spt.df)
                values$sel.points <- as.data.frame(spt.df)[, 1:n.col]
              }

              if (length(values$pol) >= 1) {
                n.col <- ncol(spt.df)
                values$sel.points <-
                  as.data.frame(spt.df[values$pol, ])[, 1:n.col]
              }
            }

            paste(
              "Selected",
              nrow(values$sel.points), "of",
              nrow(values$occur), "occurrence points."
            )
          })
        })

      shiny::observeEvent(input$done, {
        result.id <- values$sel.points$id
        sel.id <- occ.cl$id %in% result.id

        occ.cl <- occ.cl[, -ncol(occ.cl)]


        if(action == "flag"){
          occ.cl$map_module_flags <- ifelse(sel.id, "selected", "deleted")
          result <- occ.cl
        }

        if(action == "clean"){

          result <- occ.cl[sel.id,]
        }

        shiny::stopApp(returnValue = result)
      })

      })
    }
  ))
  return(final.df)
}
