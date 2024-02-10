output$download_cor_mat <- downloadHandler(
  filename = function() {
    paste('correlation_matrix', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    utils::write.csv(mat(), file)
  }
)

output$download_Marg_Spec <- downloadHandler(
  filename = function() {
    paste('Marg_Spec', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    utils::write.csv(marg_spec(), file)
  }
)

output$download_cor_plot <- downloadHandler(
  filename =  function() {
    paste("cor_plot", input$plot_type_cor, sep=".")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    if(input$plot_type_cor == "png"){
      grDevices::png(file) # open the pdf device
      print(Cor_plotInput())
      dev.off()  # turn the device off
    }

    else{
      grDevices::pdf(file) # open the pdf device
      print(Cor_plotInput())
      dev.off()  # turn the device off
    }
  }
)

output$download_enfa_scatter <- downloadHandler(
  filename =  function() {
    #paste0("ENFA", Sys.Date())
    'ENFA.png'
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
      grDevices::png(file)
      print(enfa_plotInput())
      dev.off()  # turn the device off
  }
)

# downloadHandler contains 2 arguments as functions, namely filename, content
output$down <- downloadHandler(
  filename =  function() {
    paste(input$layer, input$plot_type, sep=".")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    if(input$plot_type == "png"){
      grDevices::png(file) # open the png device
      print(plotInput())
      dev.off()}
    else{
      grDevices::pdf(file) # open the pdf device
      print(plotInput())
      dev.off()  # turn the device off
    }
  }
)

output$download_barchart <- downloadHandler(
  filename =  function() {
    'barchart.png'
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    grDevices::png(file)
    print(plotInput_barchart())
    dev.off()  # turn the device off
  }
)

output$download_mapplot <- downloadHandler(
  filename =  function() {
    'mapplot.png'
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    grDevices::png(file)
    print(plotInput_mapplot())
    dev.off()  # turn the device off
  }
)

output$download_variogram <- downloadHandler(
  filename =  function() {
    paste0("variogram", input$vario_var, ".png")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    grDevices::png(file)
    print(plotInput_variogram())
    dev.off()  # turn the device off
  }
)

######################################""
output$download_Bioclim <- downloadHandler(
  filename =  function() {
    paste0("Bioclim_N_Blocking",".png")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    #if(input$plot_type_Bioclim == "png"){
      grDevices::png(file)
      print(plotInput_bioclim())
      dev.off()  # turn the device off
    #}

    # if(input$plot_type_Bioclim == "pdf"){
    #   grDevices::pdf(file)
    #   print(plotInput_bioclim())
    #   dev.off()  # turn the device off
    # }

    # if(input$plot_type_Bioclim == "tif"){
    #   #r <- raster(system.file("external/test.grd", package="raster"))
    #   res <- writeRaster(map_bioclim(), filename=file, format="GTiff", overwrite=TRUE)
    #
    #   # Show the corresponding output filename
    #   print(res@file@name)
    #
    #   # Rename it to the correct filename
    #   file.rename(res@file@name, file)
    # }
  }
)


output$download_model_ev_Bioclim <- downloadHandler(
  filename =  function() {
    paste0("bioclim", input$model_ev_Bioclim,".png")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    if(input$plot_type_model_ev_Bioclim == "png"){
      grDevices::png(file)
      print(ev_bioclim())
      dev.off()  # turn the device off
    }

    if(input$plot_type_model_ev_Bioclim == "pdf"){
      grDevices::pdf(file)
      print(ev_bioclim())
      dev.off()  # turn the device off
    }


  }
)
