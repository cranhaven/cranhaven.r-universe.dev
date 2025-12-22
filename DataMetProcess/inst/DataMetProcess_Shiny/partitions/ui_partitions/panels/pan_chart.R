
painel_selection <-function(id){
  wellPanel(
    selectInput(
      NS(id,"dataset"),
      "Select data:",
      choices = c("Daily","Monthly","Yearly")
    )
  )
}

painel_labels <- function(id){
  wellPanel(
    uiOutput(NS(id,"colx")),
    uiOutput(NS(id,"coly"))
  )
}
painel_config_chart <- function(id){
  wellPanel(
    textInput(
      NS(id,"labelx"),
      "Title x:"
    ),
    textInput(
      NS(id,"labely"),
      "Title y:"
    ),
    radioButtons(
      NS(id,"Tipogra"),
      "Chart type:",
      choices = c(Line = "line" ,
                  Point = "point",
                  `Point and Line` = "linepoint",
                  Bar = "bar")
    ),
    
    
    tags$h4("Line settings"),
    colPick(
      inputId = NS(id,"corline"),
      label = "Line color"
    ),
    sliderInput(
      NS(id,"widthl"),
      "Line width",
      value = 2,
      min = 0,
      max = 5,
      step = 0.1
      ),
    
    
    tags$h4("Point settings"),
    colPick(
      inputId = NS(id,"corpoint"),
      label = "Point color"
    ),
    sliderInput(
      NS(id,"sizep"),
      "Point size",
      value = 8,
      min = 0,
      max = 15,
      step = 0.1
    ),
    colPick(
      inputId = NS(id,"corlp"),
      label = "Border color",
      selected = "#000000"
    ),
    sliderInput(
      NS(id,"widthlp"),
      "Border width",
      value = 1,
      min = 0,
      max = 5,
      step = 0.1
    ),
    
    
    tags$h4("Bar settings"),
    colPick(
      inputId = NS(id,"corbar"),
      label = "Bar color"
    ),
    colPick(
      inputId = NS(id,"corlb"),
      label = "Border color",
      selected = "#000000"
    ),
    sliderInput(
      NS(id,"widthlb"),
      "Border width",
      value = 1,
      min = 0,
      max = 5,
      step = 0.1
    )
  )
}
