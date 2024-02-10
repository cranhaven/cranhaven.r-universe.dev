output$ui_about <- renderUI({
  out <- fluidRow(
    column(width = 8, offset = 2, h2(("sdmApp"))),
    #column(width = 8, offset = 2, tags$img(src=system.file("docs","Logo_sdmApp.png",package = "sdmApp"),height=50,width=50)),
    column(width = 8, offset = 2, p("sdmApp is a R package containing a 'Shiny'
    application that allows non-expert R users to easily model
    species distribution. It offers a reproducible work flow for
    species distribution modeling into a single and user friendly environment.
    sdmApp takes raster data (in format supported by the raster package)
    and species occurrence data (several format supported) as input argument.
    This package provides an interactive graphical user interface (GUI).")
    ))

  out <- list(out, fluidRow(
    column(width = 8, offset = 2, h4(("Contact and Feedback"))),
    column(width = 8, offset = 2, p("In case you have any suggestions or bug reports, please file an issue at the",
                                    tags$a("issue tracker", href="https://github.com/Abson-dev", target="_blank"),"in our",
                                    tags$a("GitHub repo", href="https://github.com/Abson-dev", target="_blank"),"."))
    ))
  out
})
