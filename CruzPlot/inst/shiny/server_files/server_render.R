# CruzPlot outputs

#------------------------------------------------------------------------------
# output$manual_pdf <- renderUI({
#   tags$iframe(style = "height:850px; width:100%", src = "CruzPlot_Manual_app.pdf")
# })


#------------------------------------------------------------------------------
output$bathy_load_text <- renderText(cruzMapBathyLoad())

output$bathy_message_text <- renderText({
  if (isTruthy(cruz.list$bathy.xyz)) {
    "A bathymetry file is loaded"
  } else {
    NULL
  }
})


#------------------------------------------------------------------------------
### Planned transects outputs
output$planned_transects_text <- renderText(planned_transects())

# output$planned_transects_remove_text <- renderText(planned_transects_remove())

output$planned_transects_message <- renderText({
  req(cruz.list$planned.transects)
  "A planned transects file is loaded"
})


#------------------------------------------------------------------------------
### Non-DAS outputs
output$cruzNonDasLoaded <- renderDataTable({
  df <- req(cruz.list$ndas.df)
  row.names(df) <- 1:nrow(df)

  df
}, options = list(dom = 't'), rownames = TRUE)

output$cruzNonDasAdd_text <- renderText({
  cruzNonDasAdd()
})

output$cruzNonDasFile_LonLat_text <- renderText({
  cruzNonDasFile_LonLat()
  ""
})

output$cruzNonDasRemove_text <- renderText({
  cruzNonDasRemove()
})


#------------------------------------------------------------------------------
### DAS

# Loading DAS file(s)
output$das_file_load_text <- renderText(das_file_load())

output$das_loaded_text <- renderText({
  req(cruz.list$das.data)
  data.name <- unique(cruz.list$das.data$file_das)
  if (length(data.name) == 1) {
    paste("The following DAS file is loaded:", data.name)
  } else {
    paste("The following DAS files are loaded:", paste(data.name, collapse = ", "))
  }
})

# Loading SpCodes file
output$spcodes_user_read_text <- renderText(spcodes_user_read())
output$spcodes_default_read_text <- renderText(spcodes_default_read())

output$spcodes_message <- renderText({
  req(cruz.list$sp.codes.name)

  if (cruz.list$sp.codes.name == "default") {
    "The default SpCodes.dat file is loaded"
  } else {
    paste("The following user-provided species code file is loaded:",
          cruz.list$sp.codes.name)
  }
})


# Text in sightings tab about no SpCodes.data file
output$das_sight_spcodes_message <- renderText({
  validate(
    need(cruz.list$sp.codes,
         "You must load a species code file in the 'Data' window to plot sightings")
  )

  ""
})


# Tabular output
output$das_out_effort_table <- renderTable(cruzDasOutEffort_Table())
output$cruzDasOutEffort_Save_text <- renderText(cruzDasOutEffort_Save())

output$das_out_sight_tot_table <- renderTable({
  cruzDasOutSight_TotTable()
}, rownames = TRUE, colnames = FALSE)
output$das_out_sight_table <- renderTable(cruzDasOutSight_Table())
output$cruzDasOutSight_Save_text <- renderText(cruzDasOutSight_Save())


#------------------------------------------------------------------------------
# Plot Map
output$plot1 <- renderPlot({
  plotMap()()
}, height = map.height, units = "px", res = plot.res)
output$plot1b <- renderPlot({
  plotMap()()
}, height = map.height, units = "px", res = plot.res)
output$plot2 <- renderPlot({
  plotMap()()
}, height = map.height, units = "px", res = plot.res)
output$plot3 <- renderPlot({
  plotMap()()
}, height = map.height, units = "px", res = plot.res)
output$plot4 <- renderPlot({
  plotMap()()
}, height = map.height, units = "px", res = plot.res)
output$plot5 <- renderPlot({
  plotMap()()
}, height = map.height, units = "px", res = plot.res)


#------------------------------------------------------------------------------
# Display color/formatting options
# slight bug: display does not occur when window is resized
output$plotDisplay <- renderPlot({
  cruzDisplaySymbolProp()
})


#------------------------------------------------------------------------------
# Display mammals, turtles, and all species codes
output$sp_message <- renderText({
  validate(
    need(cruz.list$sp.codes,
         "Please load a species codes file in the 'Plot DAS Data - Data' section")
  )

})

output$sp1 <- renderDataTable({ #Mammals
  req(cruz.list$sp.codes)
  sp.mammals <- cruzSpeciesMammals()
  names(sp.mammals) <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")
  sp.mammals
})
output$sp2 <- renderDataTable({ #Turtles
  req(cruz.list$sp.codes)
  sp.turtles <- cruzSpeciesTurtles()
  names(sp.turtles) <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")
  sp.turtles
})
output$sp3 <- renderDataTable({ #All
  sp.all <- req(cruz.list$sp.codes)
  names(sp.all) <- c("Species Code", "Abbreviation", "Scientific Name", "Common Name")
  sp.all
})

#------------------------------------------------------------------------------
