#' distreg.vis function
#'
#' Function to call the distreg.vis Shiny App which represents the core of this
#'   package.
#' @import shiny
#' @import rhandsontable
#' @importFrom utils capture.output
#' @importFrom stats family
#' @importFrom formatR tidy_source
#' @examples
#' library("gamlss")
#' library("bamlss")
#' # A gamlss model
#' normal_gamlss <- gamlss(NO ~ binomial1 + ps(norm2),
#'                         sigma.formula = ~ binomial1 + ps(norm2),
#'                         data = model_fam_data(),
#'                         trace = FALSE)
#'
#' # Start the App - only in interactive modes
#' if (interactive()) {
#' distreg.vis::vis()
#' }

#' @export

### --- Shiny App --- ###

vis <- function() {
  ## Make certain UI elements
  # Sidebars
  overviewpanel <-
    tabPanel("Overview",
             br(),
             selectInput("model", label = "Select a model",
                         choices = c("",search_distreg())), # Here bamlss and gamlss objects are searched for
             uiOutput("family_ui"),
             uiOutput("equations_ui"))

  scenariopanel <-
    tabPanel("Scenarios", value = 5,
             uiOutput("scenarios_ui"))

  scenariodatapanel <-
    tabPanel("Scenario Data", value = 6,
             strong("Edit scenario data here"),
             br(), br(),
             uiOutput("scenariodata_ui"))

  # Plot
  plotpanel <- tabPanel("Plot",
                        #verbatimTextOutput("testprint"))
                        fluidRow(
                          column(width = 9,
                                 plotOutput("plot")),
                          column(width = 3, br(),
                                 uiOutput("plotbar"))
                        )
  )

  # Properties
  proppanel <- tabPanel("Properties", uiOutput("exvxdf_ui"))


  ## Assemble UI
  ui <- fluidPage(

    # Use CSS and ShinyJS for Code Highlighting
    includeCSS(system.file("srcjs/solarized-dark.css", package = "distreg.vis")),
    includeScript(system.file("srcjs/highlight.pack.js", package = "distreg.vis")),
    includeScript(system.file("srcjs/iframeResizer.contentWindow.min.js", package = "distreg.vis")),

    # Title
    titlePanel("Visualize your distributional regression predictions"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(type = "pills", id = "pillpanel",
                    overviewpanel,
                    scenariopanel,
                    scenariodatapanel

        )
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    plotpanel,
                    proppanel)
      )
    )
  )

  server <- function(input, output, session) {

    ## --- Model --- ##
    # Reactive model
    m <- reactive(
      if (input$model != "" & !is.null(input$model))
        get(input$model, envir = .GlobalEnv)
      else
        NULL
    )

    # Reactive model data
    m_data <- reactive({
      if (!is.null(m()))
        model_data(m())
    })

    # Reactive model family
    fam <- reactive({
      if (!is.null(m()))
        fam_obtainer(m())
    })

    # Got Model and data?
    gmad <- reactive({
      if (!is.null(m()) & !is.null(pred$data))
        TRUE
      else
        FALSE
    })

    ## --- Overview tab --- ##

    # Equations Output
    output$equations_ui <- renderUI({
      if (!is.null(m())) {
        list(strong("Model Equations"),
             verbatimTextOutput("equations"))
      }
    })

    # Equations Rendering
    output$equations <- renderPrint({
      if (!is.null(m()))
        formula_printer(m())
    })

    # Family Output
    output$family_ui <- renderUI({
      if (!is.null(m())) {
        list(strong("Model Family"),
             verbatimTextOutput("family"))
      }
    })

    # Family Rendering
    output$family <- renderPrint({
      if (!is.null(m()))
        cat(f_disp(m()))
    })

    ## --- Scenarios Tab --- ##

    output$scenarios_ui <- renderUI({
      if (!is.null(m())) {

        # Create slider UI elements
        m_indep <- model_data(m())
        cnames <- colnames(m_indep)
        ui_list <- list()

        # Some Space
        ui_list[[1]] <- br()

        # Action Button
        ui_list[[2]] <- actionButton("scen_act", "Create Scenario!")

        # Delete all Scenarios
        ui_list[[3]] <- actionButton("scen_clear", "Clear Scenarios")

        # More space
        ui_list[[4]] <- br()

        # Create coefficient elements
        for (i in 1:ncol(m_indep)) {
          if (any(is.numeric(m_indep[, i]))) {
            ui_list[[i + 4]] <- sliderInput(inputId = paste0("var", i),
                                            label = cnames[i],
                                            min = round(min(m_indep[, i]), 2),
                                            max = round(max(m_indep[, i]), 2),
                                            value = round(mean(m_indep[, i]), 2),
                                            sep = "")
          } else if (any(is.factor(m_indep[, i])) | any(is.character(m_indep[, i]))) {
            ui_list[[i + 4]] <- selectInput(inputId = paste0("var", i),
                                            label = cnames[i],
                                            choices = unique(m_indep[, i]),
                                            selected = unique(m_indep[, i])[1])
          }
        }

        # Return the list to uis
        ui_list
      }
    })

    ## --- Newdata --- ##

    # This function catches the current expl variables data from the model
    current_data <- reactive({
      if (!is.null(m())) {
        indep <- model_data(m())

        # Create empty dataframe
        dat <- indep[NULL, , drop = FALSE]

        # Get current variable values
        for (i in 1:ncol(indep))
          dat[1, i] <- input[[paste0("var", i)]]

        # Convert categorical variables to factors with right levels
        dat <- fac_equ(indep, dat)

        # Show DF
        dat
      } else {
        NULL
      }
    })

    # This function updates the prediction data each time the button is clicked
    pred <- reactiveValues(data = NULL)

    observeEvent(input$scen_act, {
      if (is.null(pred$data)) {
        pred$data <- data.frame(current_data(), row.names = "P1")
      }
      else if (!is.null(pred$data)) {
        # Current rowname
        cur_rn <- paste0("P", nrow(pred$data) + 1)
        pred$data <- rbind(pred$data,
                           data.frame(current_data(), row.names = cur_rn))
      }
    })

    observeEvent(input$scen_clear, {
      pred$data <- NULL
    })

    # This function clears the current pred$data when a new model is selected
    observeEvent(m(), {
      pred$data <- NULL
    })

    ## --- Scenario data Tab --- ##

    # This function displays the UI of the handsontable
    output$scenariodata_ui <- renderUI({
      rHandsontableOutput(outputId = "predtable")
    })

    # This function renders the handsontable
    output$predtable <- renderRHandsontable({
      if (!is.null(pred$data)) {
        DF <- pred$data
        DF$rownames <- row.names(DF) # this line creates new variable on which the user can specify own rownames
        rhandsontable(DF, rowHeaders = row.names(DF), width = 300)
      } else {
        NULL
      }
    })

    # This function updates the prediction data when hot changes
    # Since 0.4.5 it also checks whether cov combinations are in range
    # Since 0.4.6 the user can specify own rownames
    observe({
      if (!is.null(input$predtable)) {
        # Convert handsontable to df and give it the original rownames
        DF <- hot_to_r(input$predtable)
        row.names(DF) <- DF$rownames # these two lines
        DF$rownames <- NULL          # assign the user-specified rownames to the actual rownames

        # Check whether newdata is in old data's range
        combs <- range_checker(model_data(m()), DF)
        if (!is.null(combs)) { # if not NULL then we have bad combs
          warn_message <- bad_range_warning(combs)
          showNotification(warn_message, type = "warning", duration = 10)
        }

        # Check whether there is factor and if so convert it back from ordered...
        DF <- fac_check(DF)

        # Assign the new DF to pred$data
        pred$data <- DF
      }
    })

    ## --- Current predictions --- ##

    # This function always catches the current predictions
    cur_pred <- reactive({
      if (!is.null(pred$data))
        preds(m(), pred$data)
    })

    ## --- Plot Tab --- ##

    ## Plot is rendered here
    output$plot <- renderPlot({
      if (gmad()) {
        plot_dist(m(), cur_pred(),
                  palette = input$pal_choices,
                  type = input$type_choices,
                  rug = input$rug)
      } else {
        NULL
      }
    })

    ## Color Choices / pdf/cdf choice are rendered here
    output$plotbar <- renderUI({
      if (!is.null(m()) & any(input$pillpanel == 5, input$pillpanel == 6)) {
        ui_list <- list()

        # CDF/PDF Choice
        ui_list[[length(ui_list) + 1]] <-
          selectInput("type_choices", label = "PDF or CDF?",
                      choices = c("pdf", "cdf"))


        # Palette Choices
        ui_list[[length(ui_list) + 1]] <-
          selectInput("pal_choices", label = "Colour Palette",
                      choices = c("default", "viridis", "Accent", "Dark2",
                                  "Pastel1", "Pastel2", "Set1", "Set2",
                                  "Paired", "Set3", "Blues"))

        # Rug Plot
        ui_list[[length(ui_list) + 1]] <-
          checkboxInput("rug", label = "Rug Plot?", value = FALSE)

        # Action Button for console pasting
        ui_list[[length(ui_list) + 1]] <-
          actionButton("pastecode", icon = icon("code"),
                       label = "Obtain Code!", style = "color:white;
                                  background-color:red")

        ui_list
      }
    })

    ## What happens when pastecode button is pressed
    observeEvent(input$pastecode, {
          # First line of code
          c_data <- capture.output(dput(pred$data))
          c_data <- c("covariate_data <- ", c_data)
          c_data <- paste0(c_data, collapse = "")
          c_data <- tidy_c(c_data) # tidying

          # Second line of code
          c_predictions <- call("preds", model = as.name(input$model),
                                newdata = quote(covariate_data))
          c_predictions <- paste0("pred_data <- ", deparse(c_predictions))
          c_predictions <- tidy_c(c_predictions) # tidying

          # Third line of code
          c_plot <- call("plot_dist", model = as.name(input$model),
                         pred_params = quote(pred_data),
                         type = input$type_choices)
          if (!is.null(input$pal_choices)) # Palette if specified
            if (input$pal_choices != "default")
              c_plot[["palette"]] <- input$pal_choices
          if (input$rug)
            c_plot[["rug"]] <- TRUE
          c_plot <- deparse(c_plot, width.cutoff = 200) # Make call into character
          c_plot <- tidy_c(c_plot)

          # Assemble code
          code <- paste(c_data, c_predictions, c_plot, sep = "\n")

          # Show Model
          showModal(modalDialog(
            title = "Obtain your R code",
            tags$pre(tags$code(code)),
            HTML('<script>$("pre code").each(function(i, block) {
                   hljs.highlightBlock(block);
                  });</script>'),
            easyClose = TRUE
          ))
    })

    # output$testprint <- renderPrint({
    #   # if (!is.null(pred$data))
    #   #   pred$data
    #   # else
    #   #   cat("no scenario selected")
    #   if (gmad()) {
    #     print(m())
    #     print(cur_pred())
    #     print(input$pal_choices)
    #     print(input$type_choices)
    #     print(input$rug)
    #   } else {
    #     NULL
    #   }
    # })

    ## --- Properties Tab --- ##

    # UI for EXVX DF and influence plot
    output$exvxdf_ui <- renderUI({
      if (gmad()) {
        # Plot UI - sidebar
        infl_sidebar <- list()

        # Interested Variable
        infl_sidebar[[length(infl_sidebar) + 1]] <-
          selectInput(inputId = "infl_int_var",
                      choices = colnames(model_data(m())),
                      label = "Expl. variable for plotting influence")

        # Colour Palette
        infl_sidebar[[length(infl_sidebar) + 1]] <-
          selectInput("infl_pal_choices", label = "Colour Palette",
                      choices = c("default", "viridis", "Accent", "Dark2",
                                  "Pastel1", "Pastel2", "Set1", "Set2",
                                  "Paired", "Set3"))

        # Rug Plot
        infl_sidebar[[length(infl_sidebar) + 1]] <-
          checkboxInput("infl_rug", label = "Rug Plot?", value = FALSE)

        # If we analyze a bamlss model there is the ability to compute estimate based on samples
        if (is(m(), "bamlss")) {
          # Samples
          infl_sidebar[[length(infl_sidebar) + 1]] <-
            checkboxInput("infl_samples",
                          label = "Compute estimate based on samples?",
                          value = FALSE)

          # Uncertainty Measures
          infl_sidebar[[length(infl_sidebar) + 1]] <-
            checkboxInput("infl_uncertainty", label = "Uncertainty measures", value = FALSE)
        }

        # Get the code
        infl_sidebar[[length(infl_sidebar) + 1]] <-
          actionButton("infl_pastecode", icon = icon("code"),
                       label = "Obtain Code!", style = "color:white;
                       background-color:red")

        # Include your own function
        infl_sidebar[[length(infl_sidebar) + 1]] <-
          selectInput(inputId = "infl_exfun", choices = search_funs(),
                      label = "Include own function")

        # Plot UI - put things together
        plot_ui <- fluidRow(
          column(width = 9,
                 plotOutput("influence_graph")),
          column(width = 3, br(),
                 infl_sidebar)
        )
        plot_ui_panel <- tabPanel(title = "Influence graph", br(),
                                  plot_ui)

        # Table UI
        table_ui <- tabPanel(title = "Table",
                             tableOutput("exvxdf"))

        # Return the panel to display
        exvx_ui <- list()
        exvx_ui[[1]] <- br()
        exvx_ui[[2]] <- tabsetPanel(plot_ui_panel, table_ui, type = "pills")
        exvx_ui
      }
    })

    # Check whether samples and uncertainty measures are clicked both

    observe({
      if (!is.null(fam())) {
        if (is.bamlss(fam())) {
          if (!is.null(input$infl_uncertainty) && !is.null(input$infl_samples)) {
            if (input$infl_uncertainty && !input$infl_samples) {
              showNotification("Uncertainty measures only possible if\n estimates were computed sample-based",
                               type = "warning", duration = 10)
            }
          }
        }
      }
    })

    ## What happens when infl_pastecode button is pressed
    observeEvent(input$infl_pastecode, {
      # First line of code
      infl_c_data <- capture.output(dput(pred$data))
      infl_c_data <- c("covariate_data <- ", infl_c_data)
      infl_c_data <- paste0(infl_c_data, collapse = "")
      infl_c_data <- tidy_c(infl_c_data)

      # Second line of code
      infl_c_plot <- call("plot_moments", model = as.name(input$model),
                          int_var = input$infl_int_var,
                          pred_data = quote(covariate_data))
      if (input$infl_pal_choices != "default") # Palette if specified
        infl_c_plot[["palette"]] <- input$infl_pal_choices
      if (input$infl_exfun != "" | input$infl_exfun == "NO FUNCTION")
        infl_c_plot[["ex_fun"]] <- input$infl_exfun
      if (input$infl_rug)
        infl_c_plot[["rug"]] <- TRUE
      if (!is.null(input$infl_samples))
        if (input$infl_samples)
          infl_c_plot[["samples"]] <- TRUE
      if (!is.null(input$samples) & !is.null(input$uncertainty))
        if (input$infl_samples && input$infl_uncertainty) # only when both are true we should have samples
          infl_c_plot[["uncertainty"]] <- TRUE
      infl_c_plot <- deparse(infl_c_plot, width.cutoff = 100) # Make call into character
      infl_c_plot <- tidy_c(infl_c_plot)

      infl_code <- paste(infl_c_data, infl_c_plot, sep = "\n")
      showModal(modalDialog(
        title = "Obtain your R code",
        tags$pre(tags$code(infl_code)),
        HTML('<script>$("pre code").each(function(i, block) {
                   hljs.highlightBlock(block);
                  });</script>'),
        easyClose = TRUE
      ))
    })

    # Server-Rendering of DF
    output$exvxdf <- renderTable({
      if (!is.null(m())) {
        moments <- moments(cur_pred(), fam_obtainer(m()))
        moments
      }
    }, rownames = TRUE)

    # Server-Rendering of Influence graph
    output$influence_graph <- renderPlot({
      if (gmad())
        plot_moments(m(),
                     input$infl_int_var,
                     pred$data,
                     palette = input$infl_pal_choices,
                     ex_fun = input$infl_exfun,
                     rug = input$infl_rug,
                     samples = input$infl_samples,
                     uncertainty = input$infl_uncertainty)
    })

  }
  shinyApp(ui, server, options = list(launch.browser = TRUE,
                                      shiny.reactlog = TRUE))
}
