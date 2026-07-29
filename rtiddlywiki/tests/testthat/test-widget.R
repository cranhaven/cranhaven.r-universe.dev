test_that("widget", {
    library(leaflet)
    content <- paste(sep = "<br/>",
                     "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                     "606 5th Ave. S",
                     "Seattle, WA 98138"
    )

    widget <- leaflet(options = leafletOptions(scrollWheelZoom = FALSE)) %>% addTiles() %>%
        addPopups(-122.327298, 47.597131, content,
                      options = popupOptions(closeButton = FALSE)
        )
    new_widget <- tw_widget(widget)
    expect_true(grepl("leaf", new_widget))
})


