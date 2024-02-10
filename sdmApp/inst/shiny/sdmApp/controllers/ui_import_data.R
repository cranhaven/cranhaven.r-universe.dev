##################################################################################################################################
########################  Data Upload content ##################################################################################
##################################################################################################################################
output$ui_import_data <- renderUI({
  txt_setup <- "First load raster files and load species occurence file"
  out<-NULL
  out <- fluidRow(
    column(width = 12, offset = 0, h3("Uploading environmental variables and occurrence data"), class="wb-header"),
    column(width = 12, offset = 0, p("Load the dataset."), class="wb-header-hint"),
    fluidRow(column(12, h4("Read Me", tipify(icon("info-circle"), title=txt_setup, placement="bottom"), class="wb-block-title"), align="center"))
  )
  txt_rasters_info<-paste0("You have" ,code(raster::nlayers(data$Env)),"layers.The extent is xmin=",code(raster::extent(data$Env)@xmin),",xmax=",code(raster::extent(data$Env)@xmax),",ymin=",code(raster::extent(data$Env)@ymin),",ymax=",code(raster::extent(data$Env)@ymax))
  out<-list(out,
            sidebarPanel(width = 3,
                         p('Load environmental variables'),
                         uiOutput('Envbug'),
                         shinyFilesButton('envfiles', 'Raster selection', 'Please select rasters', FALSE, multiple = TRUE),
                         selectInput("categorical_var","Categorical variable",
                                     c(No = "No categorical",Yes = "Categorical")),
                         conditionalPanel(
                           condition = "input.categorical_var == 'Categorical'",
                           p('Which variable should be considered as a categorical variable?'),
                           uiOutput('factors')
                         ),
                         myActionButton("load",label=("Load data"), "primary")),
            #actionButton('load', 'Load')),
            mainPanel(width = 6, tabsetPanel(type = "tabs",
                                             tabPanel("About",
                                                      p(HTML(txt_rasters_info))
                                             ),
                                             tabPanel("Plot",
                                                      uiOutput('layerchoice'),
                                                      #myActionButton("export_raster_plot",label=("Export"), "primary"),
                                                      radioButtons(inputId = "plot_type", label = "Select the file type", choices = list("png", "pdf"),inline = TRUE),
                                                      downloadButton(outputId = "down", label = "Download"),
                                                      uiOutput('Envbugplot'),
                                                      plotOutput('env')

                                             )
            ),id = "tabsPreview"),
            sidebarPanel(width = 3, h4("Change aesthetics"),
                         tabsetPanel(tabPanel("Text", checkboxInput(inputId = "label_axes",
                                                                    label = strong("Change labels axes"),
                                                                    value = FALSE), conditionalPanel(condition = "input.label_axes == true",
                                                                                                     textInput("lab_x", "X-axis:", value = "label x-axis")),
                                              conditionalPanel(condition = "input.label_axes == true",
                                                               textInput("lab_y", "Y-axis:",
                                                                         value = "label y-axis")), checkboxInput(inputId = "add_title",
                                                                                                                 label = strong("Add title"), value = FALSE),
                                              conditionalPanel(condition = "input.add_title == true",
                                                               textInput("title", "Title:",
                                                                         value = "Title")), checkboxInput(inputId = "adj_fnt_sz",
                                                                                                          label = strong("Change font size"),
                                                                                                          value = FALSE), conditionalPanel(condition = "input.adj_fnt_sz == true",
                                                                                                                                           numericInput("fnt_sz_ttl", "Size axis titles:",
                                                                                                                                                        value = 12), numericInput("fnt_sz_ax",
                                                                                                                                                                                  "Size axis labels:", value = 10)),
                                              checkboxInput(inputId = "rot_txt", label = strong("Rotate text x-axis"),
                                                            value = FALSE), checkboxInput(inputId = "adj_fnt",
                                                                                          label = strong("Change font"), value = FALSE),
                                              conditionalPanel(condition = "input.adj_fnt == true",
                                                               selectInput("font", "Font", choices = c("Courier",
                                                                                                       "Helvetica", "Times"), selected = "Helvetica"))),
                                     tabPanel("Theme",
                                              conditionalPanel(condition = "input.jitter",
                                                               checkboxInput("adj_jitter", strong("Change look jitter"),
                                                                             FALSE), conditionalPanel(condition = "input.adj_jitter",
                                                                                                      textInput("col_jitter", "Colour (name or RGB):",
                                                                                                                value = "black"), numericInput("size_jitter",
                                                                                                                                               "Size:", value = 1), sliderInput("opac_jitter",
                                                                                                                                                                                "Opacity:", min = 0, max = 1,
                                                                                                                                                                                value = 0.5, step = 0.01), sliderInput("width_jitter",
                                                                                                                                                                                                                       "Width jitter:", min = 0, max = 0.5,
                                                                                                                                                                                                                       value = 0.25, step = 0.01))), checkboxInput("adj_grd",
                                                                                                                                                                                                                                                                   strong("Remove gridlines"), FALSE),
                                              conditionalPanel(condition = "input.adj_grd",
                                                               checkboxInput("grd_maj", strong("Remove major gridlines"),
                                                                             FALSE), checkboxInput("grd_min",
                                                                                                   strong("Remove minor gridlines"),
                                                                                                   FALSE)), selectInput("theme", "Theme",
                                                                                                                        choices = c(bw = "theme_bw()", classic = "theme_classic()",
                                                                                                                                    dark = "theme_dark()", grey = "theme_grey()",
                                                                                                                                    light = "theme_light()", line_draw = "theme_linedraw()",
                                                                                                                                    minimal = "theme_minimal()"), selected = "theme_bw()")),
                                     tabPanel("Legend", conditionalPanel(condition = "input.group != '.'",
                                                                         radioButtons(inputId = "adj_leg", label = NULL,
                                                                                      choices = c("Keep legend as it is",
                                                                                                  "Remove legend", "Change legend"),
                                                                                      selected = "Keep legend as it is"),
                                                                         conditionalPanel(condition = "input.adj_leg=='Change legend'",
                                                                                          textInput("leg_ttl", "Title legend:",
                                                                                                    value = "title legend"), selectInput("pos_leg",
                                                                                                                                         "Position legend", choices = c("right",
                                                                                                                                                                        "left", "top", "bottom"))))),
                                     tabPanel("Size", checkboxInput("fig_size",
                                                                    strong("Adjust plot size on screen"),
                                                                    FALSE), conditionalPanel(condition = "input.fig_size",
                                                                                             numericInput("fig_height", "Plot height (# pixels): ",
                                                                                                          value = 400), numericInput("fig_width",
                                                                                                                                     "Plot width (# pixels):", value = 480)),
                                              checkboxInput("fig_size_download",
                                                            strong("Adjust plot size for download"),
                                                            FALSE), conditionalPanel(condition = "input.fig_size_download",
                                                                                     numericInput("fig_height_download",
                                                                                                  "Plot height (in cm):", value = 14),
                                                                                     numericInput("fig_width_download",
                                                                                                  "Plot width (in cm):", value = 14)))))


  )
  out
  #####################################################
  out<-list(out,
            column(12,
                   sidebarPanel(width = 3,
                                p('Load occurence data'),
                                uiOutput('Envbug_occ'),
                                selectInput("file_type","Type of file:", list(`text (csv)` = "text",
                                                                              Excel = "Excel", SPSS = "SPSS",
                                                                              Stata = "Stata", SAS = "SAS"), selected = "text"),
                                shinyFilesButton('Occ', 'Occurrence selection', 'Please select occurrence file', FALSE),
                                conditionalPanel(condition = "input.file_type=='text'",
                                                 radioButtons('sep', 'Separator',
                                                              c(Comma = ',',
                                                                Semicolon = ';',
                                                                Tab = '\t',
                                                                'White space' = ' '),
                                                              ',', inline = TRUE),
                                                 radioButtons('dec', 'Decimal',
                                                              c(Point ='.',
                                                                Comma = ','),
                                                              '.', inline = TRUE)),
                                uiOutput('Xcol'),
                                uiOutput('Ycol'),
                                uiOutput('Pcol'),
                                myActionButton("load2",label=("Load data"), "primary")

                   ),
                   mainPanel(width = 7, tabsetPanel(type = "tabs",
                                                    tabPanel("Preview",
                                                             uiOutput('Occbug'),
                                                             dataTableOutput('occ')))
                             ,
                             id = "tabs"))
  )
}
)
########################################### End Data upload ##############
##########################################################################

