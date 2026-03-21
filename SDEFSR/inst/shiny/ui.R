
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage( theme = NULL,
  
  titlePanel(h1("Subgroup Discovery with Evolutionary Fuzzy Systems ", id = "textoPrincipal", align = "center"),windowTitle = "Execute Subgroup Discovery Algorithms with R"), 
  br(),
  sidebarLayout(
    sidebarPanel( 
      #------------------------- DATASET INFORMATION -------------------------------
      #--------------------- This is the left panel of the GUI---------------------
       p(h4(HTML("1.- Select a <a title = 'Learn more about KEEL', href = 'http://www.keel.es'>KEEL</a>, <a title = 'Learn more about ARFF', href = 'http://www.cs.waikato.ac.nz/ml/weka/arff.html'>ARFF</a> or <a title = 'Learn more about CSV', href = 'https://en.wikipedia.org/wiki/Comma-separated_values'>CSV</a> dataset: "))),
       fileInput("traFile", "Select Training File: ", accept = c(".dat", ".arff")),
       fileInput("tstFile", "Select Test File: ", accept = c(".dat", ".arff")),
       selectInput("targetClassSelect", "Select the target variable:", choices = NA),
       selectInput("targetValueSelect", "Select the target value:", choices = NA),
       radioButtons("visualization", "Visualize dataset info as a: ", c("Pie Chart", "Histogram", "Box Plot", "Variable vs Variable"), selected = "Pie Chart"),
       br(), br()
      ),

    #-------- MAIN PANEL (Where there are the tabs)
    mainPanel( 
      tags$head(tags$script(src="load.js")),
      tabsetPanel(
        #------------------- EXPLORATORY ANALYSIS TAB -----------------------------
          tabPanel( "Exploratory Analysis",
                    # This condition is to show a 'loading' image when shiny is executing some stuff 
                    conditionalPanel(
                      condition="($('html').hasClass('shiny-busy'))",
                      br(),
                      img(src="busy.gif"), 
                      p("Executing. Please, wait..."), align = "center"
                    ),
                    
                   
            fluidRow(
              #Plot and table, the table is shown when 'variable vs variable' is not selected.
              column(10, plotOutput("datasetInfo")),
              column(2, conditionalPanel(condition = "input.visualization != 'Variable vs Variable'", tableOutput("statistics")), align = "right")
            ),
            
            #For histogram and box plot, visualize the slider for range filter
           conditionalPanel(condition = "(input.visualization == 'Histogram' || input.visualization == 'Box Plot')",
                            fluidRow(
                              column(1),
                              column(6,sliderInput("numericRangeVisualization", label = "Show range", min = 0, max = 0, value = c(0,0)))
                            )
                            ),
           
           #Visualize for "variable vs variable" the visualization
           conditionalPanel(condition = "input.visualization == 'Variable vs Variable'", 
                            fluidRow(
                            column(6,selectInput("Variables1", "Variable X", choices = NA),
                                   sliderInput("numericRange1", "Show range", min = 0, max = 0, value = c(0,0))),
                            column(6,selectInput("Variables2", "Variable Y", choices = NA),
                                   sliderInput("numericRange2", "Show range", min = 0, max = 0, value = c(0,0)))
                            )),
           
           
           fluidRow(
             column(3, radioButtons("traTstRadio", label = "Visualize file:", choices = c("Training File", "Test File"), selected = "Training File"), align = "left"),
             column(6, conditionalPanel(condition = "input.visualization != 'Variable vs Variable'", 
                                        checkboxGroupInput("classNames", "Select values", choices = NULL, inline = T))
                    )
             ) ,
           
           actionButton("filterData", HTML("<strong title = 'Remove instances in the dataset that are not selected or within the ranges'>Keep this data</strong>"), style = "background-color:#303F9F;color:#FFFFFF"), #Button to filter the data
          
           
           fluidRow(
             column(12, textOutput("statusText"))
           ),
           
           align = "center"         
          ),
          
          # ------------------------- ALGORITHM SELECTION TAB -------------------------------------
          tabPanel("Algorithm Selection", 
                   conditionalPanel(
                     condition="($('html').hasClass('shiny-busy'))",
                     br(),
                     img(src="busy.gif"), 
                     p("Executing. Please, wait..."), align = "center"
                   ),
                   fluidRow(
                     column(6,
                            # ---- Algorithm Selection -----
                            # Put common elemnts of algorithms
                            selectInput("algorithm", label = "Algorithm", choices = c("SDIGA", "NMEEF-SD", "MESDIF", "FuGePSD" ) ),
                            helpText(h6("Note: This algorithms may take hours executing on large datasets. ")),
                            
                            sliderInput("nLabels", "Number of fuzzy labels: ", value = 3, min = 1, max = 10, step = 1),
                            selectInput("rulesRep", "Type of rules: ", choices = c("Canonical", "DNF (Disjunctive Normal Form)")),
                            numericInput("nEval", "Number of Evaluations:", value = 10000, min = 0, max = Inf, step = 1),
                            sliderInput("popSize", "Number of individuals in population: ", value = 100, min = 2, max = 500, step = 1),
                            sliderInput("mutProb", label = "Mutation Probability: ", min = 0, max = 1, value = 0.01, step = 0.01), 
                            numericInput("seed", "Specify a seed:", value = 0, min = 0, max = Inf, step = 1),
                            conditionalPanel("input.algorithm == 'FuGePSD'",
                                             sliderInput("gfw1", label = "Global Fitness Weight 1: ", value = 0.7, min = 0, max = 1, step = 0.01),
                                             sliderInput("gfw2", label = "Global Fitness Weight 2: ", value = 0.1, min = 0, max = 1, step = 0.01)
                            )
                     ), 
                     # Here puts conditional elements with parameters of algorithms
                     column(6,
                            conditionalPanel("input.algorithm == 'SDIGA'",
                                             sliderInput("minConf", "Minimum Confidence", value = 0.6, min = 0, max = 1, step = 0.01),
                                             selectInput("Obj1","Objective 1", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Crisp Support"),
                                             sliderInput("w1", "Weight 1: ", value = 0.7, min = 0, max = 1, step = 0.01 ),
                                             selectInput("Obj2","Objective 2", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Crisp Confidence"),
                                             sliderInput("w2", "Weight 2: ", value = 0.3, min = 0, max = 1, step = 0.01),
                                             selectInput("Obj3","Objective 3", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness")),
                                             sliderInput("w3", "Weight 3: ", value = 0, min = 0, max = 1, step = 0.01),
                                             checkboxInput("lSearch", "Perfom Local Search", value = TRUE)
                            ), 
                            conditionalPanel("input.algorithm == 'NMEEF-SD'",
                                             sliderInput("crossProb", label = "Crossover Probability: ", min = 0, max = 1, value = 0.6, step = 0.01),
                                             sliderInput("minConf", "Minimum Confidence", value = 0.6, min = 0, max = 1, step = 0.01),
                                             checkboxInput("reInitPob", "Use re-initialize operator", value = TRUE),
                                             conditionalPanel("input.reInitPob",
                                                              sliderInput("porcCob", "Maximum percentage of variables to use when re-initialize:", min = 0, max = 1, value = 0.5, step = 0.01)
                                             ),
                                             selectInput("Obj1N","Objective 1", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Unusualness"),
                                             selectInput("Obj2N","Objective 2", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Significance"),
                                             selectInput("Obj3N","Objective 3", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness")),
                                             checkboxInput("strictDominance", "Comparison using strict dominance", value = TRUE)
                            ),
                            conditionalPanel("input.algorithm == 'MESDIF'", 
                                             sliderInput("crossProbM", label = "Crossover Probability: ", min = 0, max = 1, value = 0.6, step = 0.01),
                                             sliderInput("elitePop", "Size of elite population: ", min = 1, max = 30, value = 3, step = 1),
                                             selectInput("Obj1M","Objective 1", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Crisp Support"),
                                             selectInput("Obj2M","Objective 2", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"), selected = "Crisp Confidence"),
                                             selectInput("Obj3M","Objective 3", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness")),
                                             selectInput("Obj4M","Objective 4", c("null", "Crisp Support", "Crisp Confidence", "Fuzzy Support", "Fuzzy Confidence", "Coverage", "Significance", "Unusualness"))
                                             
                            ),
                            conditionalPanel("input.algorithm == 'FuGePSD'",
                                             #numericInput("nGens", "Number of Generation:", value = 300, min = 0, max = Inf, step = 1),
                                             sliderInput("crossProbF", label = "Crossover Probability: ", min = 0, max = 1, value = 0.6, step = 0.01),
                                             sliderInput("insProb", label = "Insertion Probability: ", min = 0, max = 1, value = 0.15, step = 0.01) ,
                                             sliderInput("dropProb", label = "Dropping Condition Probability: ", min = 0, max = 1, value = 0.15, step = 0.01),
                                             selectInput("tnorm", label = "T-norm for the Computation of the Compatibility Degree: ", choices = c("Minimum/Maximum", "Product"), selected = "Product"),
                                             selectInput("ruleWeight", label = "Rule Weight Method: ", choices = c("Penalized Certainty Factor", "Average_Penalized_Certainty_Factor", "No_Weights", "Certainty_Factor")),
                                             selectInput("frm", label = "Fuzzy Reasoning Method: ", choices = c("Winning_Rule", "Normalized_Sum", "Arithmetic_Mean")),
                                             numericInput("tournamentSize", label = "Tournament Selection Size: ", value = 2, min = 2, max = 30, step = 1),
                                             sliderInput("gfw3", label = "Global Fitness Weight 3: ", value = 0.05, min = 0, max = 1, step = 0.01),
                                             sliderInput("gfw4", label = "Global Fitness Weight 4: ", value = 0.2, min = 0, max = 1, step = 0.01),
                                             checkboxInput("allClass", label = "ALL_CLASS", value = TRUE)
                            )
                     )),
                   
                   br(),
                   div( actionButton("execute", label = HTML("Execute!"), onClick = "goOnTop()", width = '25%', style = "background-color:#303F9F;color:#FFFFFF"),
                        conditionalPanel(
                          condition="($('html').hasClass('shiny-busy'))",
                          br(),
                          img(src="busy.gif", width = 25, height = 25), align = "center"
                        ), align = "center" ),
                   br(), br(), br()
          ),
          
          #--------------------------- RULES GENERATED TAB --------------------------------
          tabPanel("Rules Generated",
                   conditionalPanel(
                     condition="($('html').hasClass('shiny-busy'))",
                     br(),
                     img(src="busy.gif"), 
                     p("Executing. Please, wait..."), align = "center"
                   ),
                   br(),
                   br(),
                   p(h2("Generated Rules"), align = "center"),
                   #uiOutput("results")
                   dataTableOutput("results")
                   
          ),  
          
          #----------------------- TEST QUALITY MEASURES TAB --------------------------------
          
          tabPanel("Rules Quality Measures",
                       conditionalPanel(
                         condition="($('html').hasClass('shiny-busy'))",
                         br(),
                         img(src="busy.gif"), 
                         p("Executing. Please, wait..."), align = "center"
                       ),
                       br(),
                       br(),
                       p(h2("Test Quality Measures for Generated Rules"), align = "center"),
                       div(actionButton("displayQMGraph", label = "Show/Hide Graph", style = "background-color:#303F9F;color:#FFFFFF"), align = "center"),
                       uiOutput("plotResultUI"),
                       dataTableOutput("measures")
                   
          ),
          
          
          
          # ------------------------- EXECUTION INFO TAB -----------------------------------
          tabPanel("Execution Info",
                   conditionalPanel(
                     condition="($('html').hasClass('shiny-busy'))",
                     br(),
                     img(src="busy.gif"), 
                     p("Executing. Please, wait..."), align = "center"
                   ),
                   br(),
                   br(),
                   p(h2("Execution Info"), align = "center"),
                   uiOutput("execInfo")
                   
          )
          ,type = "pills", id = "tabSet"
      )

    )
    ) 
  
))


