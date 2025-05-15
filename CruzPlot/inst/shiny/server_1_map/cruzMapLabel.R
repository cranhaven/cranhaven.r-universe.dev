# Processing for Label tab of Create and Save Map tab

# Return title label, font, and size
cruzMapLabelTitle <- reactive({
  lab <- input$label_title
  fam <- font.family.vals[as.numeric(input$label_title_font)]
  cex <- input$label_title_size

  list(lab = lab, fam = fam, cex = cex)
})

#	Return axes labels (lon and lat), font, and size
cruzMapLabelAxes <- reactive({
  lab.lon <- input$label_axis_lon
  lab.lat <- input$label_axis_lat
  fam <- font.family.vals[as.numeric(input$label_axis_font)]
  cex <- input$label_axis_size

  list(lab.lon = lab.lon, lab.lat = lab.lat, fam = fam, cex = cex)
})
