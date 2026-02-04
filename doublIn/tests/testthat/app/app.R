# Visualize contact tracing data
# NB: this code is to check the app. The actual function can be found elsewhere.
#
# Run an application that visualizes contact tracing data that can be used to
# estimate incubation and latency time using the doublIn package.

# . . . . . Prepare the network . . . . . . . . . . . . . . . . . . . . . . .

prepare_network <- function(dat){

  dat <- rename(dat, case_id = id)

  # ID has to be the first variable
  dat <- dat[ , c(which( (colnames(dat) == "case_id") == TRUE) ,
                  which( (colnames(dat)!="case_id") == TRUE))]

  # Uses package epicontacts
  l1 <- sapply(dat$infector, length)
  contacts_rep <- rep(dat$case_id, l1)
  case_unl <- unlist(dat$infector)
  contacts <- cbind(contacts_rep, case_unl)
  contacts <- contacts[!is.na(contacts[,2]),]
  colnames(contacts) <- c("case_id", "infector")

  x <- make_epicontacts(linelist = dat,
                        contacts = contacts, id = "case_id", to = "case_id",
                        from = "infector",
                        directed = TRUE)
  return(x)
}

# Function to prepare the network
prepare_network_new <- function(dat, case_id, infector_id){

  dat <- rename(dat, case_id = case_id, infector = infector_id)

  dat <- dat[ , c(which( (colnames(dat) == case_id) == TRUE) ,
                  which( (colnames(dat)!= case_id) == TRUE))]

  l1 <- sapply(dat$infector, length)
  contacts_rep <- rep(dat$case_id, l1)
  case_unl <- unlist(dat$infector)

  contacts <- cbind(contacts_rep, case_unl)
  contacts <- contacts[!is.na(contacts[,2]),]
  colnames(contacts) <- c("case_id", "infector")

  x <- make_epicontacts(linelist = dat,
                        contacts = contacts, id = "case_id", to = "case_id",
                        from = "infector",
                        directed = TRUE)
  return(x)
}

# . . . . . UI . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

# UI consists of a header, a sidebar and a body.

# Header
header <- dashboardHeader( title = "Visualize contacts" )

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Load data", tabName = "loaddata"),
    menuItem("Contact network", tabName = "network")
  )
)

# Body
body <- dashboardBody(

  tabItems(

    # Tab 1: Load a contact tracing data set
    tabItem(tabName = "loaddata",
            h2(""), # Title

            fluidRow(

              tabBox(width = 6, title = "Step 1", tabPanel("Load the data.",

              # Select a data set
              fileInput(width = 400, inputId = "file1",
              label = "Select a RDS file with contact tracing data to import.",
              accept = ".rds")
              )
              ),

              tabBox(width = 6, title = "Getting started in three steps.",
              p("The aim of this application is to visualize contact tracing data, facilitating data cleaning and processing steps. Before you move on to the other tabs, import a data set in .RDS format. To save data as .RDS file, use"),
              code("saveRDS(data)."),
              p("If everything is correct, the data table should show up. Next, select the required variables. All ID variables need to be in text format; dates need to be in the date format. To check if the format is correct, use the following R functions:"),
              code("type <- class(data$variable_name); is.Date(type); is.character(type); is.factor(type)")
              )

            ),

            fluidRow(

              tabBox(width = 6, title = "Step 2",
              tabPanel("Check if contact tracing data is imported correctly.",
                       dataTableOutput("table")),
              ),

              # Choice of variables
              tabBox(width = 6, title = "Step 3", tabPanel("Choose variables.",
              selectizeInput(width = 400, 'case_var',
                                    'IDs of the infectees:', choices = list(),
                                    multiple = FALSE
                                                           ),
                selectizeInput(width = 400,
              'infector_var', 'IDs of infector(s), i.e. possible sources:',
              choices = list(),
              multiple = FALSE),
                selectizeInput(width = 400,
              'start_of_exposure', 'Start of exposure (E0):',
              choices = list(),
              multiple = FALSE),
                selectizeInput(width = 400,
              'end_of_exposure', 'End of exposure (E1)',
              choices = list(),
              multiple = FALSE),
                selectizeInput(width = 400,
              'symptom_onset', 'Symptom onset (S)',
              choices = list(),
              multiple = FALSE),
                selectizeInput(width = 400,
              'last_negative', 'Last negative test (P0)',
              choices = list(),
              multiple = FALSE),
                selectizeInput(width = 400,
              'first_positive', 'First positive test (P1)',
              choices = list(),
              multiple = FALSE)
              ))
            )
    ),

    # Tab 2: Visualize the contact network
    tabItem(tabName = "network",

            fluidRow(

              tabBox(width = 6, tabPanel("Settings",

                                         selectizeInput(
                                           'nodecol', 'Color network nodes by:',
                                           choices = list(),
                                           multiple = FALSE
                                         ),

              checkboxInput('show_symptoms',
              "Incubation time (infection to symptom onset)", value = TRUE,
              width = NULL),
              checkboxInput('show_testwindow',
              "Latency time (here: infection to PCR positivity)", value = FALSE,
              width = NULL) )
              ),

              tabBox(width = 6, title = "Description",
                     p("The network graph displays the possible transmission between infectee and potential sources. Each arrow represents one possible infection, from infector to infectee. To select a cluster of cases to display in the timeline figure on the right and the table below, click on a central case in the network for two seconds."),
                     p("The figure below visualizes the time of the start- end endpoint for selected observations. Start- and endpoint differs per quantity of interest, i.e. incubation or latency time. Time of infection is typically known to fall in a certain window only. The same holds for start of infectiousness, that is assumed to occur in between the last negative and first positive PCR-test. Start and and of the window are represented by a '0' and '1', respectively. When these are not displayed, they are missing in the data."),

              )

            ),


            fluidRow(
              tabBox(tabPanel("Network", visNetworkOutput("network"))),
              tabBox(tabPanel("Figure", plotlyOutput("timeline")))
            ),

            fluidRow(
              tabBox(width = 12, tabPanel("Table",
                                  dataTableOutput("table_selected_nodes")))
            )

    ) # End tabItem
  ) # End tabitems
) # End dashboardbody

# Combine
ui <- dashboardPage(
  header,
  sidebar,
  body
)

# . . . . . Server . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

server <- shinyServer(function(input, output, session) {

  output$image <- renderImage({
    list(src =  "Timeline_diagram.jpeg",
         alt = "This is alternate text",  height = 400, width = 400
    )
  }, deleteFile = TRUE)

  output$text <- renderText({ input$txt })

  output$text2 <- renderText({ input$txt2 })

  # Load the data
  mydat <- reactive({

  if (is.null(input$file1$datapath)) {
   # Hashed for testing
   # readRDS("inst/extdata/df2.RDS")
  } else {
    readRDS(input$file1$datapath)
  }

  })

  selected_data <- reactive({
    as.data.frame(dat_for_tab())
  })

  # Inputs for subset
  observe({ updateSelectizeInput(session, "nodecol",
                         choices = names(selected_data())) })
  observe({ updateSelectizeInput(session, "infector_var",
                         choices = names(selected_data())) })
  observe({ updateSelectizeInput(session, "case_var",
                         choices = names(selected_data())) })
  observe({ updateSelectizeInput(session, "start_of_exposure",
                         choices = names(selected_data())) })
  observe({ updateSelectizeInput(session, "end_of_exposure",
                         choices = names(selected_data())) })
  observe({ updateSelectizeInput(session, "symptom_onset",
                         choices = names(selected_data())) })
  observe({ updateSelectizeInput(session, "last_negative",
                         choices = names(selected_data())) })
  observe({ updateSelectizeInput(session, "first_positive",
                         choices = names(selected_data())) })
  nodecol <- reactive({
    input$nodecol
  })

  # For figure: should symptoms and last neg. -  first pos. test be plotted?
  show_symptoms <- reactive({input$show_symptoms})
  show_testwindow <- reactive({ input$show_testwindow })

  # Subset of data (based on selection options on the left)
  dat_for_tab <- reactive({mydat() %>% mutate(row_nr = row_number()) %>%
      relocate(row_nr) })

  # Create data table
  output$table <- renderDataTable({
    as.data.frame( dat_for_tab() ) %>%
      datatable(
        options = list(scrollX = TRUE)) %>%
      formatStyle(1:ncol(dat_for_tab()), lineHeight='60%')
  })

  dat_network <- reactive({
    prepare_network_new(dat_for_tab(), input$case_var, input$infector_var)
  })

  output$network <- renderVisNetwork({
    vis_epicontacts(dat_network(), node_color = input$nodecol) %>%
      visOptions(highlightNearest=TRUE,
                 nodesIdSelection = TRUE) %>%
      visInteraction(multiselect = TRUE) %>%
      visEvents(select = "function(data) {
                Shiny.onInputChange('current_nodes_selection', data.nodes);
                Shiny.onInputChange('current_edges_selection', data.edges);
                ;}")
  })

  # Node selection to subset data
  selected_nodes <- reactive({
    input$current_nodes_selection
  })

  # Table selected nodes
  output$table_selected_nodes <- renderDataTable({
    as.data.frame( dat_for_tab() ) %>%
      filter( (Case_ID %in% selected_nodes()) |(infector %in% selected_nodes() )  ) %>%

      datatable(
        options = list(scrollX = TRUE)) %>%
      formatStyle(1:ncol(dat_for_tab()), lineHeight='60%')
  })

  # Timeline
  output$timeline <- renderPlotly({
    colors <- c("exposure window" = "blue", "symptom onset" = "orange",
                "last neg. to first pos." = "purple", "reported" = "green",
                "ID" = "black")
    shapes <- c(95, 43)

    mydat <- as.data.frame( dat_for_tab() ) %>%
      filter( (.[[input$case_var]] %in%
                 selected_nodes()) |(.[[input$infector_var]] %in%
                                       selected_nodes() )  ) %>%
      mutate(case_var = .[[input$case_var]],
             start_of_exposure = .[[input$start_of_exposure]],
             end_of_exposure = .[[input$end_of_exposure]],
             symptom_onset = .[[input$symptom_onset]],
             last_negative = .[[input$last_negative]],
             first_positive = .[[input$first_positive]]
      ) %>% mutate(new_row_nr = row_number(), xloc = min(start_of_exposure,
                                                   na.rm = TRUE))

    p <- ggplot(data = mydat) +

      geom_rect(aes(y = new_row_nr, xmin = start_of_exposure,
                       xmax = end_of_exposure, ymin = new_row_nr - 0.5,
                       ymax = new_row_nr + 0.5), fill = "blue", alpha = .25) +
      geom_rect(aes(y = new_row_nr, xmin = last_negative, xmax = first_positive,
                    ymin = new_row_nr - 0.5, ymax = new_row_nr + 0.5),
                fill = "purple", alpha = .25) +
      geom_text(aes(x = start_of_exposure, y = new_row_nr,
                    color = "exposure window"), size = 4, label = "E0") +
      geom_text(aes(x = end_of_exposure, y = new_row_nr,
                    color = "exposure window"), size = 4, label = "E1")

      if(show_symptoms() == TRUE){
      p <- p + geom_point(aes(x = symptom_onset, y = new_row_nr,
                              color = "symptom onset"), size=2)
      }

      if(show_testwindow() == TRUE){
      p <- p +
        geom_text(aes(x = last_negative, y = new_row_nr,
                      color = "last neg. to first pos."), size = 4,
                      label = "P0") +
        geom_text(aes(x = first_positive, y = new_row_nr,
                      color="last neg. to first pos."), size = 4, label = "P1")
    }

    p <- p +

      geom_text(aes(x = xloc, y = new_row_nr + 0.5, label = case_var,
                    color = "ID"), size = 2) +

      scale_x_date("Calender time") +
      scale_y_discrete(paste("Cases (one per row)")) + labs(color = "") +
      theme_bw() +
      scale_color_manual(values = colors) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y= element_blank(),
            axis.text.x=element_text(color = "black"),
            panel.grid.major = element_blank()
      )

    ggplotly(p, height = 400) %>%
      layout(
        showlegend = T, legend = list(x = 0, y = 1.15, orientation = 'h'),
        xaxis = list(color = "transparent"), yaxis = list(color = "transparent")
      )

  })

  # Data selected nodes
  dat_plot_nodes <- reactive({

    mydat()  %>%
      filter( (id %in% selected_nodes())
              |(infector %in% selected_nodes() )  ) %>%
      mutate(row_nr = row_number()) %>% relocate(row_nr)
  })

})

# . . . . . Run the app . . . . . . . . . . . . . . . . . . . . . . .

shinyApp(ui, server)


