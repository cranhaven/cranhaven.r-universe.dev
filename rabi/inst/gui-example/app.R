library(shiny)
library(tidyverse)

server <- function(input, output) {


  output$downloadData <- downloadHandler("rabi-IDs.csv",


    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(ID_df, file, sep = ",", row.names = FALSE)
    }
  )

#############Misc Server Stuff#################
  #makes a vector full of color names for randomizing
  rand.colors <- colors() %>% stringr::str_extract("[a-z]+") %>% unique() %>% unlist()
  rand.positions <- c("left leg", "right leg", "head", "thorax",
                      "abdomen", "tail", "right tibiotarsus", "forewing",
                      "4th toe on hindlimb", "1st digit", "3rd digit", "caudal fin")

  #calculate the number of IDs generated
  output$howmany <- renderText({
    num.positions <- as.integer(input$num.positions)
    num.robust <- as.integer(input$num.robust)
    num.colors <- as.integer(input$num.colors)

    paste0("With these parameters, you can get ", num.colors^(num.positions - num.robust), " unique IDs.")
    })
########################Dynamic Input Boxes##################
  #make the input boxes for the different color names
  output$input_ui_colors <- renderUI({
    num.colors <- as.integer(input$num.colors)

    #if names is selected, give option for names
    if (input$colornamesYN == "names") {
      lapply(1:num.colors, function(i) {
        textInput(paste0("n_input_colors_", i), label = paste0("Marking name #", i, " (e.g. '", sample(rand.colors,1),"'):"), value = (i-1))
      })

    #if numbers (default) is chosen, don't display anything
    } else if (input$colornamesYN == "numbers") {
      return()

    #if neither, what HAPPENED?
    } else {
      textInput("lawdy", label = "lawdylawdylawdy", value = "lawdy me")
    }
  })

  output$input_ui_positions <- renderUI({
    num.positions <- as.integer(input$num.positions)
    lapply(1:num.positions, function(i) {
      textInput(paste0("n_input_position_", i), label = paste0("Position name #", i, " (e.g. '", sample(rand.positions,1),"'):"), value = (i))  })

  })
########################Data Table##################
  #makes the table of all your values
  output$table <- renderTable({

    num.positions <- as.integer(input$num.positions)
    num.robust <- as.integer(input$num.robust)
    num.colors <- as.integer(input$num.colors)

    #if color names are chosen, take the user input
    if (input$colornamesYN == "names") {
      color.names <- lapply(1:num.colors, function(i) {input[[paste0("n_input_colors_", i)]]}) %>% unlist()
    #else, it means numbers, so go from 0 to X-1
    } else {
      color.names <- as.character(0:(num.colors-1))
    }

    ID_lists <- rabi::rs_IDs(num.positions, num.robust, num.colors, color.names)
    ID_lists %>% as.data.frame() %>% t() %>% as.data.frame() -> ID_df
    rownames(ID_df) <- NULL
    colnames(ID_df) <- lapply(1:dim(ID_df)[2], function(i) {input[[paste0("n_input_position_", i)]]}) %>% unlist()

    ##Making it a global variable so other functions can grab it
    ID_df <<- ID_df

    ID_df
  })

##############Dynamically Adjust Selector Range##############
  output$position.selector <- renderUI({
    selectInput("num.positions", "Number of markable positions on animal:", choices = seq(2,input$num.colors,1))
  })

  output$robust.selector <- renderUI({
    selectInput("num.robust", "Number of marked positions that can be erased before the ID loses guaranteed recoverability:", choices = seq(1,input$num.positions,1))
  })

##############################################
}

ui <- fluidPage(


  tags$title("'rabi' GUI"),


  sidebarLayout(
    sidebarPanel(
      h1("Graphical User Interface for rabi's ID scheme generator"),

      helpText(
        "Note: This example uses Reed-Solomon polynomial oversampling (",
        code("rs_IDs()"),
        ") to generate the ID sequences. ",
        "Consequently, there are several stipulations that must be met:  ",
        tags$ol(
          tags$li("The number of unique markings used (e.g. the different colors you have at your disposal) must be a prime number. (Weird, but whatever.)"),
          tags$li("The number of positions to be marked on the animal can't be greater than the number of unique markings (see above). This number does NOT need to be prime though")
        )
      ),


      radioButtons("colornamesYN", "You can choose to use numbers to refer to different markings or specify your own names.", list("Use numbers (default)" = "numbers","Replace with user-defined names (e.g. blue, indigo, striped, clipped toe, etc)" = "names"), selected = NULL),

      uiOutput("position.selector"),
      uiOutput("robust.selector"),

      selectInput("num.colors", "Number of unique markings available (round down to the closest prime):", choices = c(2, 3, 5, 7, 11, 13 ,17, 19), selected = 3),
      p(" "),
      p(" "),
      hr(),hr(),
      uiOutput("input_ui_colors"),
      downloadButton('downloadData', 'Download as a CSV')
    ),
    mainPanel(
      h4("Name the locations that will be marked on the animal."),
      uiOutput("input_ui_positions"),
      hr(),hr(),
      h4(textOutput("howmany")),
      tableOutput("table")
    )
  )
)

shinyApp(ui = ui, server = server)
