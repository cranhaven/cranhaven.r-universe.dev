######################################################
##                      papci                       ##
##             Interactive User Interface           ##
##                     UI File                      ##
##                                                  ##
##                                                  ##
######################################################

suppressMessages(library(shiny))
suppressMessages(library(shinythemes))


shinyUI(
    navbarPage(
        id="steps_list",
        title="papci",
        theme=shinytheme("yeti"),

        ##info page
        tabPanel(
            value = 'info',
            "Information",
            h2(strong(
                'papci UI: an interactive user interface for calculating prevalence adjusted PPV'
            ),align="center"),
            hr(),
            h3(strong("Overview")),
            p("Positive predictive value (PPV) defined as the conditional probability of clinical trial assay (CTA) being positive given Companion diagnostic device (CDx)
            being positive is a key performance parameter for evaluating the clinical validity utility of a companion diagnostic test in clinical bridging studies.
            When bridging study patients are enrolled based on CTA assay results, Binomial-based confidence intervals (CI) may are not appropriate for PPV CI estimation.
              Bootstrap CIs which are not restricted by the Binomial assumption may be used for PPV CI estimation only when PPV is not 100%.
              Bootstrap CI is not valid when PPV is 100% and becomes a single value of [1, 1]. We proposed a risk ratio-based method for constructing CI for PPV.
              By simulation we illustrated that the coverage probability of the proposed CI is close to the nominal value even when PPV is high and negative percent agreement (NPA) is close to 100%.
              There is a lack of R package for PPV CI calculation. We developed a publicly available R package along with this shiny app to implement the proposed approach and some other existing methods."),
            br(),
            h3(strong("Reference")),
            p("1. Gart John J and Nam Jun-mo (1988). Approximate interval estimation of the ratio of binomial parameters: a review and corrections for skewness, Biometrics, 323-338."),
            p("2. Katz DJSM, Baptista J, Azen SP and Pike MC (1978). Obtaining confidence intervals for the risk ratio in cohort studies, Biometrics, 469-474."),
            p("3. Koopman PAR (1984). Confidence intervals for the ratio of two binomial proportions, Biometrics, 513-517."),
            p("4. Noether Gottfried E (1957). Two confidence intervals for the ratio of two probabilities and some measures of effectiveness, Journal of the American Statistical Association, 52, 36-45."),
            br(),
            h3(strong("Contact")),
            p("Shiny App Author: Kate(Yueyi) Li, Lei Yang, Cui Guo, Chang Xu"),
            p("Maintainer: Kate(Yueyi) Li: kali@foundationmedicine.com")
        ),

        ##start analysis
        navbarMenu(
            "Steps",
            ###first step: upload data
            tabPanel(
                value ='upload',
                strong("Upload"),
                fluidRow(
                    id = 'upload_help_tray',
                    h2(strong(
                        'Upload'
                    ),align="center"),
                    p(HTML(paste(HTML('&nbsp;'),'  To start the analysis, please upload your input.
                      Two options are available:'))),
                    p(HTML(paste(HTML('&nbsp;'),HTML('&nbsp;'),'  1. x : number positive calls by both the reference and the new assay;
 y: number of observations called negative at reference but positive by new assay
 m: number of positive calls at reference
 n: number of negative calls at reference'))),
                    p(HTML(paste(HTML('&nbsp;'),HTML('&nbsp;'),'2. a dataframe in xlsx format whose first column is the reference testing results and the second column is the new assay testing results'
                      ))),
                    p(HTML(paste(HTML('&nbsp;'),HTML('&nbsp;'),'Click Read in data')))
                ),
                hr(),
                sidebarPanel(
                    numericInput(
                        "x",
                        "x: Reference (+) & New Assay (+)",
                        0,
                        min = 0,
                        max = NA,
                        step = NA,
                        width = NULL
                    ),
                    numericInput(
                        "y",
                        "y: Reference (-) & Neew Assay (+)",
                        0,
                        min = 0,
                        max = NA,
                        step = NA,
                        width = NULL
                    ),
                    numericInput(
                        "m",
                        "m: Reference (+)",
                        0,
                        min = 0,
                        max = NA,
                        step = NA,
                        width = NULL
                    ),
                    numericInput(
                        "n",
                        "n: Reference (-)",
                        0,
                        min = 0,
                        max = NA,
                        step = NA,
                        width = NULL
                    ),
                    p(actionButton("Inputreadin","Read in")),
                    br(),
                    p(fileInput('InputFile',
                                tags$strong(HTML('<p style="font-size: 12pt"> Dataframe</p>')),
                                multiple=T,
                                accept='.xlsx')),
                    p(actionButton("Inputreadindf","Read in")),
                    br(),
                    p(actionButton('Inputexample','Example data')),
                    width = 3
                ),
                mainPanel(
                    tags$strong(p(HTML('<p style="font-size: 12pt">2 x 2 Table</p>'))),
                    DT::dataTableOutput("Input_twobytwo"),
                    br(),
                    tags$strong(p(HTML('<p style="font-size: 12pt">Snapshot of Dataframe</p>'))),
                    DT::dataTableOutput("Input_Snapshot"),
                    br(),
                    fluidRow(
                        column(6,align = "center",actionButton("Inputreset","Reset")),
                        column(6,align = "center",actionButton("Inputnextstepbutton","Next"))

                        ),
                    br())
            ),

            ###second step:concordance
            tabPanel(
                value ='concordance',
                strong("Calculate Concordance"),
                fluidRow(
                    id = 'concordance_help_tray',
                    h2(strong(
                        'Calculate Concordance'
                    ),align="center"),
                    p('Please specify the parameters for concordance calculations and choose output'
                      ,align="center")
                ),
                hr(),
                sidebarPanel(
                    numericInput(
                        "conf.level",
                        "confidence level",
                        0.95,
                        min = 0,
                        max = 1,
                        step = NA,
                        width = NULL
                    ),
                    numericInput(
                        "prevalence",
                        "prevalence",
                        0.15,
                        min = 0,
                        max = 1,
                        step = NA,
                        width = NULL
                    ),
                    selectInput(
                        "method_pa",
                        "method for PPA/NPA",c("exact", "ac", "asymptotic", "wilson", "prop.test", "bayes", "logit", "cloglog", "probit","all"),
                        multiple = FALSE,
                        width = NULL
                    ),
                    selectInput(
                        "method_pv",
                        "method for PPV/NPV",
                        c("Koopman","Katz","Gottfried","Gart_Nam","Bootstrap","Plug-In"),
                        multiple = FALSE,
                        width = NULL
                    ),
                    conditionalPanel(
                        condition = "input.method_pv == 'Bootstrap'",
                        numericInput("bootstrap", "Simulation n",2000,min = 0,max = 20000,step = NA,width = NULL)
                    ),
                    selectInput(
                        "alternative",
                        "alternative",
                        c("two.sided", "less", "greater"),
                        multiple = FALSE,
                        width = NULL
                    ),
                    actionButton("calculate","Calculate")
                ),
                mainPanel(
                    selectInput(
                        "concordance",
                        "concordance",
                        c("PPA", "NPA", "PPV","NPV","all"),
                        "PPA",
                        multiple = FALSE,
                        width = NULL
                    ),
                    DT::dataTableOutput("concordance_tb"),
                    br(),
                    downloadButton("download", "Download"))
            )


        )
        )
    )
