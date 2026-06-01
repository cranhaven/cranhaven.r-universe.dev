## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----quickstart---------------------------------------------------------------
#  # Load the library
#  library(mapboxer)
#  
#  # Create a source
#  motor_vehicle_collisions_nyc %>%
#    dplyr::mutate(color = ifelse(injured > 0, "red", "yellow")) %>%
#    as_mapbox_source(lng = "lng", lat = "lat") %>%
#    # Setup a map with the default source above
#    mapboxer(
#      center = c(-73.9165, 40.7114),
#      zoom = 10
#    ) %>%
#    # Add a navigation control
#    add_navigation_control() %>%
#    # Add a layer styling the data of the default source
#    add_circle_layer(
#      circle_color = c("get", "color"),
#      circle_radius = 3,
#      # Use a mustache template to add popups to the layer
#      popup = "Number of persons injured: {{injured}}"
#    )

## ---- eval = FALSE------------------------------------------------------------
#  motor_vehicle_collisions_nyc %>%
#    dplyr::filter(killed > 0) %>%
#    as_mapbox_source() %>%
#    mapboxer(
#      center = c(-73.9165, 40.7114),
#      zoom = 9
#    ) %>%
#    add_circle_layer(circle_color = "red")

## ---- eval = FALSE------------------------------------------------------------
#  mvc_sf <- motor_vehicle_collisions_nyc %>%
#    sf::st_as_sf(coords = c("lng", "lat"), crs = 4326)
#  
#  mvc_source_from_sf <- mvc_sf %>%
#    as_mapbox_source()
#  
#  mvc_source_from_df <- motor_vehicle_collisions_nyc %>%
#    as_mapbox_source(lng = "lng", lat = "lat")

## ---- eval = FALSE------------------------------------------------------------
#  mapboxer(
#    center = c(-73.9165, 40.7114),
#    zoom = 9
#  ) %>%
#    add_circle_layer(
#      source = as_mapbox_source(motor_vehicle_collisions_nyc),
#      circle_color = "red",
#      circle_radius = 5
#    )

## ---- eval = FALSE------------------------------------------------------------
#  mapboxer() %>%
#    add_navigation_control(
#      pos = "top-left",
#      # Option passed to the 'NavigationControl'
#      showCompass = FALSE
#  ) %>%
#    add_scale_control(
#      pos = "bottom-left",
#      # Option passed to the 'ScaleControl'
#      unit = "nautical"
#  ) %>%
#    add_text_control(
#      pos = "top-right",
#      text = "mapboxer"
#    )

## ---- eval = FALSE------------------------------------------------------------
#  map <- motor_vehicle_collisions_nyc %>%
#    dplyr::mutate(
#      color = ifelse(injured > 0, "red", "yellow")
#    ) %>%
#    as_mapbox_source() %>%
#    mapboxer(
#      center = c(-73.9165, 40.7114),
#      zoom = 9
#    )
#  
#  map %>%
#    add_circle_layer(
#      # Expression to get the color from the data's color property
#      circle_color = c("get", "color")
#    )

## ---- eval = FALSE------------------------------------------------------------
#  map %>%
#    add_circle_layer(
#      circle_color = list(
#        "case",
#        # 'red' if 'injured > 0'
#        list(">", c("get", "injured"), 0), "red",
#        # Defaults to 'yellow'
#        "yellow"
#      )
#    )

## ---- eval = FALSE------------------------------------------------------------
#  map %>%
#    add_circle_layer(
#      circle_color = c("get", "color"),
#      # Expression to display only data where 'injured > 1'
#      filter = list(">", "injured", 1)
#    )

## ---- eval = FALSE------------------------------------------------------------
#  library(shiny)
#  library(mapboxer)
#  
#  view <- fluidPage(
#    h1("mapboxer"),
#    mapboxerOutput("map")
#  )
#  
#  backend <- function(input, output) {
#    output$map <- renderMapboxer({
#      mapboxer(center = c(9.5, 51.3), zoom = 10) %>%
#        add_navigation_control() %>%
#        add_marker(lng = 9.5, lat = 51.3, popup = "mapboxer")
#    })
#  }
#  
#  if (interactive()) shinyApp(view, backend)

## ---- eval = FALSE------------------------------------------------------------
#  LAYER_ID <- "crashes"
#  START_VALUE <- 4
#  
#  view <- basicPage(
#    sliderInput("slider", "Number of persons injured:",
#                min = 0, max = 7, step = 1, value = START_VALUE),
#    mapboxerOutput("map")
#  )
#  
#  backend <- function(input, output) {
#    output$map <- renderMapboxer({
#      mapboxer(
#        center = c(-73.9165, 40.7114),
#        zoom = 9
#      ) %>%
#        add_circle_layer(
#          source = as_mapbox_source(motor_vehicle_collisions_nyc),
#          circle_color = "red",
#          popup = "{{injured}}",
#          filter = list("==", "injured", START_VALUE),
#          id = LAYER_ID
#        )
#    })
#  
#    observeEvent(input$slider, {
#      mapboxer_proxy("map") %>%
#        set_filter(LAYER_ID, list("==", "injured", input$slider)) %>%
#        update_mapboxer()
#    })
#  }
#  
#  if (interactive()) shinyApp(view, backend)

