#' @export

#' @importFrom shiny tagList HTML NS br conditionalPanel downloadButton downloadHandler fluidPage h3 hr icon is.reactive mainPanel moduleServer need observe plotOutput reactive renderPlot renderPrint renderText renderUI req selectInput showNotification sidebarLayout sidebarPanel sliderInput tabPanel tabsetPanel tags textOutput uiOutput updateSelectInput validate verbatimTextOutput
#' @importFrom DT DTOutput renderDT
#' @importFrom shinyWidgets materialSwitch
#' @importFrom rclipboard rclipButton rclipboardSetup
#' @importFrom phosphoricons ph


# module UI ----

ctreeUI <- function(id) {
  
  ns = NS(id)
  
  sidebarLayout(
    
    sidebarPanel(
      width = 2,
      
      selectInput(ns("VarY"),
                  label = "Explained variable",
                  choices = NULL,
                  multiple = FALSE,
                  selectize = TRUE),
      
      selectInput(ns("VarX"),
                  label = "Explanatory variables", 
                  choices = NULL,
                  multiple = TRUE),
      
      materialSwitch(
        inputId = ns("CheckWeights"),
        label = "Weights"),
      
      conditionalPanel(
        ns = ns,
        condition = "input.CheckWeights",
        selectInput(ns("Weights"),
                    label = NULL, 
                    choices = NULL,
                    multiple = FALSE,
                    selectize = TRUE)
      ),
      
      sliderInput(ns("Alpha"),
                  "max p-value for split selection",
                  min = 0,
                  max = 1,
                  value = 0.05),
      
      sliderInput(ns("Maxdepth"),
                  "maximal depth",
                  min = 1,
                  max = 20,
                  value = 4,
                  step = 1,
                  ticks = FALSE),
      
      sliderInput(ns("Minsplit"),
                  "min # observations at inner nodes",
                  min = 0,
                  max = 500,
                  value = 20,
                  step = 1,
                  ticks = FALSE),
      
      sliderInput(ns("Minbucket"),
                  "min # observations at terminal nodes",
                  min = 0,
                  max = 500,
                  value = 50,
                  step = 1,
                  ticks = FALSE),
      
      # p("------------------------"),
      
      conditionalPanel(
        ns = ns,
        condition = "input.tabs==1",
        hr(),
        materialSwitch(ns("checkInnerPlots"),
                       label = "Inner node plots",
                       value = FALSE,
                       right = TRUE)
      ),
      
      conditionalPanel(
        ns = ns,
        condition = "input.tabs==2",
        hr(),
        sliderInput(ns("Nsim"),
                    "# simulations for variable importance",
                    min = 1,
                    max = 100,
                    value = 1,
                    step = 1,
                    ticks = FALSE),
      )
      
    ),
    
    mainPanel(
      
      tags$head(
        tags$style(HTML(".shiny-output-error-validation {color: orange;}"))
      ),
      
      tags$style(HTML(".tabbable > .nav > li > a {width: 15vw;}")),
      
      tabsetPanel(id = ns("tabs"),
                  tabPanel(tagList(phosphoricons::ph("tree", title = "Plot"),
                                   "Plot"),
                           value = 1,
                           fluidPage(br(),
                                     conditionalPanel(
                                       ns = ns,
                                       condition = "input.VarX != '' & input.VarY != ''",
                                       downloadButton(ns('downloadPlot'), 'Save')
                                     ),
                                     # br(),
                                     plotOutput(ns("treePlot"), height="600px", width="100%")
                           )
                  ),
                  
                  tabPanel(tagList(phosphoricons::ph("gauge", title = "Statistics"),
                                   "Statistics"),
                           value = 2,
                           fluidPage(h3("Performance of the model"),
                                     verbatimTextOutput(ns("perfPrint")),
                                     br(),
                                     h3("Importance of the variables"),
                                     DTOutput(ns("importancePrint")),
                                     br(),
                                     textOutput(ns("noteText")),
                                     br()
                           )),
                  
                  tabPanel(tagList(phosphoricons::ph("anchor", title = "Stability"),
                                   "Stability"),
                           value = 3,
                           fluidPage(h3("Tree as text"),
                                     verbatimTextOutput(ns("texttreePrint")),
                                     br(),
                                     h3("Variable selection at each split"),
                                     verbatimTextOutput(ns("nodestatsPrint")),
                                     br()
                           )),
                  
                  tabPanel(tagList(phosphoricons::ph("code", title = "R code"),
                                   "R code"),
                           value = 4,
                           br(),
                           fluidPage(rclipboardSetup(),
                                     verbatimTextOutput(ns("codePrint")),
                                     # UI ouputs for the copy-to-clipboard buttons
                                     uiOutput(ns("clip"))),
                           br()
                  )  
      )
      
    )
  )
  
}






# module SERVER ----

#' @export

ctreeServer <- function(id, data, name) {
  
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(name))
  
  moduleServer(id, function(input, output, session) {
    
    observe({
      vnames <- names(data())
      if(sum(sapply(data(), function(x) is.numeric(x) | is.factor(x)))<length(vnames)) {
        showNotification("Some variables in the data are neither factors no numeric vectors and were removed.", type = "warning")        
      }
      updateSelectInput(
        inputId = "VarX",
        choices = c("Choose a variable" = "", as.list(vnames[sapply(data(), function(x) is.numeric(x) | is.factor(x))]))
      )
      updateSelectInput(
        inputId = "VarY",
        choices = c("Choose variables" = "", as.list(vnames[sapply(data(), function(x) is.numeric(x) | is.factor(x))]))
      )
      updateSelectInput(
        inputId = "Weights",
        choices = c("Choose a variable" = "", as.list(vnames[sapply(data(), is.numeric)]))
      )  
    })
    
    wInput <- reactive({
      if(input$CheckWeights & input$Weights!="") {
        w <- data()[[input$Weights]]
      } else {
        w <- rep(1, times = nrow(data()))
      }
      validate(
        need(sum(is.na(w))==0, message = "\nWeights have missing values. Please filter your data.")
      )
      return(w)
    })
    
    formuleInput <- reactive({
      req(input$VarY, input$VarX)
      formule <- as.formula(paste(input$VarY, paste(input$VarX, collapse = "+"), sep = " ~ "))
      validate(
        need(sum(is.na(data()[[input$VarY]]))==0, message = "\nThe explained variable has missing values. Please filter your data.")
      )
      return(formule)
    })
    
    treeInput <- reactive({
      req(formuleInput())
      partykit::ctree(formuleInput(),
                      data = data(),
                      control = partykit::ctree_control(alpha = input$Alpha,
                                                        minsplit = input$Minsplit,
                                                        minbucket = input$Minbucket,
                                                        maxdepth = input$Maxdepth),
                      weights = wInput())
    })
    
    output$perfPrint <- renderPrint({
      req(treeInput())
      pred <- predict(treeInput())
      truth <- data()[,input$VarY]
      if(is.factor(truth)) {
        if(nlevels(truth)==2) {
          pred_prob <- predict(treeInput(), type = "prob")[,levels(truth)[2]]
          auc <- measures::AUC(pred_prob, truth, levels(truth)[1], levels(truth)[2])
          cat(paste("AUC (Area Under Curve) =", round(auc,3)), "\n")
        }
        acc <- measures::ACC(truth, pred)
        cat(paste("Accuracy (% true predictions) =", 100*round(acc,3), "%\n"))
        bac <- measures::BAC(truth, pred, levels(truth)[1], levels(truth)[2])
        cat(paste("Balanced accuracy (average of% true predictions by class) =", 100*round(bac,3), "%\n\n"))
        cat("Confusion matrix :\n\n")
        t <- table(observed = truth, predicted = pred)
        print(t)
        cat("\n\n")
        for(i in 1:nlevels(truth)){
          cat("true ",levels(truth)[i],": ",round(100*t[i,i]/rowSums(t)[i],1)," %\n",sep="")
        }
      }
      if(is.numeric(truth)) {
        rsq <- measures::RSQ(truth, pred)
        cat(paste("Coefficient of determination (R-squared) =", round(rsq,3)))
        tau <- measures::KendallTau(truth, pred)
        cat(paste("\nRank correlation (Kendall's tau) =", round(tau,3)))
      }
    })
    
    output$importancePrint <- renderDT({
      req(treeInput())
      imp_tot <- EasyTreeVarImp(treeInput(), nsim = input$Nsim)
      DT::datatable(imp_tot,
                    rownames = FALSE,
                    # filter = "none",
                    extensions = "Buttons",
                    options = list(dom = "Bt",
                                   buttons = list(list(extend = "copy", text = "copy"))))#,
      #autoWidth = TRUE,
      #columns.width = rep("50px", ncol(imp_tot))))
    })
    
    output$noteText <- renderText({
      req(treeInput())
      truth <- data()[,input$VarY]
      if(is.factor(truth)) {
        note <- "Note : permutation importance, based on several performance measures (AUC, accuracy, balanced accuracy and % true predictions for the various categories of the explained variable)."
      }
      if(is.numeric(truth)) {
        note <- "Note : permutation importance, based on several performance measures (R-squared, Kendall's tau)."
      }
      note
    })
    
    output$texttreePrint <- renderPrint({
      req(treeInput())
      print(treeInput())
    })
    
    output$nodestatsPrint <- renderPrint({
      req(treeInput())
      moreparty::GetSplitStats(treeInput())
    })
    
    output$treePlot <- renderPlot({
      req(treeInput())
      NiceTreePlot(treeInput(), input$checkInnerPlots)
    })
    
    output$downloadPlot <- downloadHandler(
      filename = "mytree.png",
      content = function(file) {
        grDevices::png(file, width = 1440, height = 1440, res= 120)
        NiceTreePlot(treeInput(), input$checkInnerPlots)
        grDevices::dev.off()
      }
    )
    
    rscriptInput <- reactive({
      req(treeInput())
      formule <- paste(input$VarY, paste(input$VarX, collapse = " + "), sep = " ~ ")
      script <- "## Estimation of the model\n\n"
      if(input$CheckWeights & input$Weights!="") { 
        w <- sprintf(", weights = %s$%s)", name(), input$Weights) 
      } else {
        w <- ")"
      }
      script <- paste0(script,
                       sprintf("mytree <- partykit::ctree(%s,
                                              data = %s,
                                              control = partykit::ctree_control(alpha = %s,
                                                                                minsplit = %s,
                                                                                minbucket = %s,
                                                                                maxdepth = %s)%s\n\n\n",
                               formule,name(),input$Alpha,input$Minsplit,input$Minbucket,input$Maxdepth,w))
      script <- paste0(script, "## Tree plot\n\n")
      script <- paste0(script, sprintf("moreparty::NiceTreePlot(mytree, node_plots = %s)\n\n\n", input$checkInnerPlots))
      script <- paste0(script, "## Performance of the model\n\n")
      script <- paste0(script, "pred <- predict(mytree)\n")
      script <- paste0(script, sprintf('truth <- %s[,"%s"]\n',name(),input$VarY))
      truth <- data()[,input$VarY]
      if(is.factor(truth)) {
        if(nlevels(truth)==2) {
          script <- paste0(script, 'pred_prob <- predict(mytree, type = "prob")[,levels(truth)[2]]\n\n')
          script <- paste0(script, "auc <- measures::AUC(pred_prob, truth, levels(truth)[1], levels(truth)[2])\nround(auc,3)\n\n")
        }
        script <- paste0(script, "accuracy <- measures::ACC(truth, pred)\n100*round(accuracy,3)\n\n")
        script <- paste0(script, "balanced_accuracy <- measures::BAC(truth, pred, levels(truth)[1], levels(truth)[2])\n100*round(balanced_accuracy,3)\n\n")
        script <- paste0(script, "confusion_matrix <- table(observed = truth, predicted = pred)\nconfusion_matrix\n\n")
        script <- paste0(script, "for(i in 1:nlevels(truth)){\n")
        script <- paste0(script, '  cat("true ",levels(truth)[i],": ",round(100*confusion_matrix[i,i]/rowSums(confusion_matrix)[i],1)," %\n",sep="")\n}\n\n\n')
      }
      if(is.numeric(truth)) {
        script <- paste0(script, "\nr_squared <- measures::RSQ(truth, pred)\nround(r_squared,3)\n\n")
        script <- paste0(script, "tau <- measures::KendallTau(truth, pred)\nround(tau,3)\n\n\n")
      }
      script <- paste0(script, "## Variable importance\n\n")
      script <- paste0(script, sprintf("imp <- moreparty::EasyTreeVarImp(mytree, nsim = %s)\nimp\n\n\n", input$Nsim))
      script <- paste0(script, "## Stability of the tree\n\n")
      script <- paste0(script, "print(mytree)\n")
      script <- paste0(script, "moreparty::GetSplitStats(mytree)")
      return(script)      
    })
    
    output$codePrint <- renderPrint({
      req(rscriptInput())
      cat(rscriptInput())
    })
    
    # Add clipboard button
    output$clip <- renderUI({
      rclipButton(
        inputId = "clipbtn",
        label = "Copy",
        clipText = as.character(rscriptInput()), 
        icon = icon("clipboard")
      )
    })
    
  })
}
