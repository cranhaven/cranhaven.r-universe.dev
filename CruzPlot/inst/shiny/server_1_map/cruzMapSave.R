# saveMap for CruzPlot by Sam Woodman

plotDownload <- function() {
  plotMap()()
}

### Render download button, with checks
output$downloadMap_button <- renderUI({
  # Resolution
  v.val <- input$download_res
  v.message <- "Resolution must be a whole number greater than zero"
  validate(need(!is.na(v.val), v.message))
  validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.message))
  validate(need(v.val > 0, v.message))

  # Plot dimensions
  if (input$download_dim == 2) {
    # Plot width
    v.val <- input$download_width
    v.message <- "Plot width must be greater than zero"
    validate(need(!is.na(v.val), v.message))
    # validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.message))
    validate(need(v.val > 0, v.message))

    # Plot height
    v.val <- input$download_height
    v.message <- "Plot height must be greater than zero"
    validate(need(!is.na(v.val), v.message))
    # validate(need(isTRUE(all.equal(v.val %% 1, 0)), v.message)
    validate(need(v.val > 0, v.message))
  }

  # Button
  downloadButton("downloadMap", label = "Download map")
})


### Download map
output$downloadMap <- downloadHandler(
  filename = function() {
    file.ext <- switch(
      input$download_format, "1" = ".jpeg", "2" = ".pdf", "3" = ".png"
    )

    paste0(
      "cruzPlot_",
      cruz.map.range$lon.range[1], "_", cruz.map.range$lon.range[2], "_",
      cruz.map.range$lat.range[1], "_", cruz.map.range$lat.range[2],
      file.ext
    )
  },

  content = function(file) {
    # Get file dimension values
    file.res <- input$download_res

    if (input$download_dim == 1) {
      file.width <- session$clientData$output_plot1b_width / plot.res
      file.height <- session$clientData$output_plot1b_height / plot.res

    } else if (input$download_dim == 2) {
      file.width <- input$download_width
      file.height <- input$download_height
    }

    plot.bg <- if_else(input$background_transparent, "transparent", "white")

    # Save map
    if (input$download_format == 1) {
      jpeg(file, width = file.width, height = file.height, units = "in",
           res = file.res)
      plotDownload()
      dev.off()
    } else if (input$download_format == 2) {
      pdf(file, width = file.width, height = file.height, onefile = FALSE,
          bg = plot.bg)
      plotDownload()
      dev.off()
    } else if (input$download_format == 3) {
      png(file, width = file.width, height = file.height, units = "in",
          res = file.res, bg = plot.bg)
      plotDownload()
      dev.off()
    }
  }
)
