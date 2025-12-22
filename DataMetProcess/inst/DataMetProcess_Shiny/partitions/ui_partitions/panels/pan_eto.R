panel_ui_calc <- function(id){
  tagList(
    wellPanel(
      numericInput(
        NS(id,"Lat"),
        "Latitude (decimal):",
        value = -22.19392
      ),
      numericInput(
        NS(id,"Alt"),
        "Altitude (m):",
        value = 463
      ),
      numericInput(
        NS(id,"Alt_an"),
        "Anemometer height (m):",
        value = 10
      ),
      numericInput(
        NS(id,"DAP"),
        "Days after planting:",
        value = 1
      ),
      helpText("Days after planting with respect to the first date in the database")
    ),#wellpanel
    wellPanel(
      uiOutput(NS(id,"Temp")),
      uiOutput(NS(id,"Humid")),
      uiOutput(NS(id,"Rad")),
      uiOutput(NS(id,"Wind")),
      uiOutput(NS(id,"Patm")),
      uiOutput(NS(id,"G"))
      #uiOutput("Kc")
    ),#wellpanel
    wellPanel(
      uiOutput(NS(id,'Kc')),
      helpText("If the column with Kc is provided, ETc will be calculated.")
    )#wellpanel
  )
}
