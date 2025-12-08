ERT_BBOB_box <- function(width = 12, height = '600px', collapsible = T,
                        collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">BBOB Performance Analysis</p>'),
    width = width, collapsible = collapsible, solidHeader = T,
    status = "primary", collapsed = collapsed,
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput('ERTPlot.BBOB.Alg', label = 'Select which IDs to include:',
                    multiple = F, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "ID selection", content = alg_select_info,
                          placement = "auto"
                        )
                    ),

        actionButton('ERTPlot.BBOB.PlotButton', label = 'Refresh the figure'),
        hr(),
        selectInput('ERTPlot.BBOB.Format', label = 'Select the figure format',
                    choices = supported_fig_format, selected = supported_fig_format[[1]]),

        downloadButton('ERTPlot.BBOB.Download', label = 'Download the figure')
      ),

      mainPanel(
        width = 10,
        column(
          width = 12, align = "center",
          HTML_P('The <b><i>ERT</i></b> is shown against the target
                 values for all functions in the selected dimension.'),
          plotlyOutput.IOHanalyzer('ERTPlot.BBOB.Plot', aspect_ratio = 1)
        )
      )
    )
  )
}
