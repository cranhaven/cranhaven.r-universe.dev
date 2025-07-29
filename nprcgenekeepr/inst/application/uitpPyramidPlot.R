uitpPyramidPlot <-
  tabPanel("Pedigree Age Plot",
           div(
             style = "min-width:1000px",
             # Side Panel
             div(
               style = paste(
                 "float: left; width: 500px; height: 80px; padding: 10px;",
                 "border: 1px solid lightgray; background-color: #EDEDED;",
                 "margin-left:3px; margin-top: 3px; margin-bottom: 3px;",
                 "border-radius: 25px; box-shadow: 0 0 5px 2px #888"
               ),
               includeHTML(file.path("..", "extdata", "ui_guidance",
                                     "pyramidPlot.html"))
             )#,
             # # Main Panel
             # div(style = "margin-left:425px;padding:10px;",
             #     actionButton("displayPyramidPlot",
             #                  label = "Display Pyramid Plot"))
           ),
           plotOutput("pyramidPlot"))
