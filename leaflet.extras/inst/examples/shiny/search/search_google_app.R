library(leaflet.extras)
library(shiny)


ui <- fluidPage(
  # tags$head(tags$script(type="text/javascript", charset="UTF-8",
  #                       paste0("https://maps.googleapis.com/maps/api/js?v=3&key=",Sys.getenv("GOOGLE_MAPS_API")))),
  # tags$head(tags$script(type="text/javascript", charset="UTF-8", src= "google.js")),
  actionButton("remove","remove"),
  leafletOutput("map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      addSearchGoogle(apikey=Sys.getenv("GOOGLE_MAPS_API"),
                      options = searchOptions(
                        autoCollapse = FALSE,
                        minLength = -1,
                        position = "topleft"))
  })

  observeEvent(input$remove, {
    leafletProxy("map") %>%
      leaflet.extras::removeSearchGoogle()
  })
  observeEvent(input$leafmap_search_location_found, {
    print("Location Found")
    print(input$leafmap_search_location_found)
  })

}

shinyApp(ui, server)
