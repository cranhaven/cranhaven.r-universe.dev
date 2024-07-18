#' @importFrom stats TukeyHSD aggregate bartlett.test density kruskal.test lm na.omit pairwise.wilcox.test qnorm qqnorm quantile sd shapiro.test
#' @importFrom utils read.csv2 str
#' @import tibble
#' @importFrom graphics hist
#' @importFrom DT renderDataTable
#' @importFrom DT datatable
#' @importFrom DT formatSignif
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard menuSubItem
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard tabItems
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard box
#' @importFrom shinydashboardPlus dashboardPage
#' @import BayesFactor PMCMRplus
#' @importFrom car durbinWatsonTest
#' @importFrom reshape melt
#' @importFrom shiny column radioButtons textOutput checkboxInput fileInput fluidRow htmlOutput icon numericInput reactive renderPrint renderTable renderText renderUI runApp selectInput shinyApp sliderInput stopApp tableOutput tabPanel uiOutput withMathJax verbatimTextOutput
#' @import shinycssloaders BayesFactor broom dplyr highcharter moments nortest stringr waiter
#' @importFrom car some
#' @import htmltools methods
#' @importFrom purrr map
#' @importFrom ggstatsplot ggbetweenstats theme_ggstatsplot
#' @importFrom ggplot2 element_text
#' @importFrom stats TukeyHSD bartlett.test density kruskal.test lm na.omit qnorm qqnorm quantile shapiro.test


globalVariables(c("%>%","A","AB","B","TukeyHSD","Y","y","aes","anovaBF","augment","bartlett.test","box","bptest","dashboardBody","dashboardFooter","dashboardHeader","dashboardSidebar","datatable","density","drop_na","dunnTest","dup_axis","durbinWatsonTest","element_text","filter","ggbetweenstats","ggplot","group_by","hc_add_series","hc_add_series_list","hc_chart","hc_exporting","hc_title","hc_xAxis","hc_yAxis","hc_yAxis_multiples","hcaes","highchart","highchartOutput","htmlOutput","icon","kruskal.test","labs","lillie.test","list_parse2","lm","mainPanel","map","melt","menuItem","na.omit","numericInput","plotOutput","posterior","qnorm","qqnorm","quantile","reactive","read.csv2","renderHighchart","renderPlot","renderTable","renderText","renderUI","runApp","scale_y_continuous","selectInput","shapiro.test","sidebarMenu","skewness","sliderInput","stat_summary","summarise","tabItem","tabItems","tableOutput","tags","theme","theme_bw","uiOutput","withSpinner","aov","fluidRow","column","a","img","dashboardPage","tagList","spin_three_bounce","textOutput","h3","Trat","upr","Trat","upr","lwr","hist","Names","Mean","se_mean","n_eff","names_from_WB","Iteration","mu","sig2","value","HTML","h2","radioButtons","checkboxInput","fileInput","variable"))


#' Interactive panel Two-way ANOVA classic, non parametric and bayesian
#'
#' Interactive panel to visualize and develop two-way analysis of variance models, from the classical, non-parametric and Bayesian approach.
#' @param dataset Data set
#' @return A shiny panel with the classical, non-parametric and Bayesian analyzes of variance, based on the specification of the dependent and independent variable of the data set provided in \code{dataset}, also provides a decision diagram that suggests which method is appropriate, based on the assumptions of the models.
#' @examples
#' \donttest{
#' data(ToothGrowth)
#' ToothGrowth$dose = factor(ToothGrowth$dose)
#' levels(ToothGrowth$dose) = c("Low", "Medium", "High")
#' Multiaovbayes(ToothGrowth)
#' }
#' @export
Multiaovbayes <- function(dataset=FALSE) {

 # require(shiny)
 # require(highcharter)
 # require(shinydashboard)
 # require(shinydashboardPlus)
 # require(BayesFactor)
 # require(dplyr)
 # require(waiter)
 # require(broom)
 # require(nortest)
 # require(moments)
 # require(car)
 # require(DT)
 # require(shinycssloaders)
 # require(reshape)
 # require(purrr)
 # require(stringr)
 # require(tidyverse)
 # require(FSA)
 # require(ggstatsplot)
 # require(lmtest)

  getDF2fact <- function(dataAB){
    dataAB <- dataAB[with(dataAB, order(dataAB[,1], dataAB[,2])), ]
    names(dataAB) <- c("A","B","y")
    listA <- unique(dataAB[,1])
    listB <- unique(dataAB[,2])
    i = 0
    j = 0
    dfAB <- data.frame()
    while(i<length(listA)){
      while(j<length(listB)){
        df <- filter(dataAB, A== listA[i+1] & B== listB[j+1])
        df <- data.frame(Y = as.double(df[,3]),
                         AB = as.factor(paste(listA[i+1], "-",listB[j+1]))
        )
        dfAB <- rbind(dfAB, df)
        j= j+1
      }
      j=0
      i= i+1
    }

    dfAB <- as.data.frame(dfAB)
    return(dfAB)
  }

  runApp(list(
    ui = dashboardPage(
      preloader = list(html = tagList(spin_three_bounce(), h3("Please wait a moment ...")), color = "#1E3A4E"),

      title =  'Two-way analysis of variance' ,
      dashboardHeader(title = "Two-way analysis of variance",
                      titleWidth = 450),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Database", tabName = "BD", startExpanded = TRUE,icon = icon("database")),
          menuItem("Assumptions", tabName = "Assumptions", startExpanded = TRUE,icon = icon("tasks")),
          menuItem("Classic ANOVA", tabName = "ANOVAcl", startExpanded = TRUE,icon = icon("adn")),
          menuItem("Kruskal Wallis", tabName = "KW", startExpanded = TRUE,icon = icon("kickstarter-k")),
          menuItem("Bayesian ANOVA", tabName = "ANOVAby", startExpanded = TRUE,icon = icon("bold"))



        )),

      dashboardBody( tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #DADADA;
                                color: #2B1F57
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #A1A1A1;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #6B94BF;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #546A90;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #A8A8A8;

                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #8B8989;
                                color: #151515;
                                style:"font-family:verdana";
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #6F6F6F;
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #DDDDDD;
                                }

                             /* body */
                                 .skin-blue .main-body .content-wrapper, .right-side {
                                background-color: #F3F3F3;
                                 }

                                .box.box-solid.box-primary>.box-header{
  background: rgb(0, 129, 201);
  color: #57A184;
    font-size: 18px;
  font-weight; bold;
}

.box.box-solid.box-primary{
  font-family: OpenSans;
  font-size: 16px;
  text-align: left;
  color: #AA3B3B;
}

                                '))),
                     tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),

                     tabItems(
                       tabItem(tabName= "BD",
                               box(width=12,title="Upload base in csv",
                                   fluidRow(
                                     column(12,fileInput("file1", " ",
                                                         accept = c(
                                                           "text/csv",
                                                           "comma-separated-values,text/plain",
                                                           ".csv")
                                     ),
                                     checkboxInput("header", "Press if the first row contains the column names", TRUE),
                                     radioButtons(inputId="separador",label="Separador",
                                                  choices = c(Comma=',', Semicolon=";", Tab="\t", Space=''),
                                                  selected = ','))
                                   ),uiOutput('var')),
                               fluidRow(width=12,
                                        box(title="Viewer",
                                            width=12,
                                            DT::dataTableOutput("DTable")))

                       ),
                       tabItem(tabName = "Assumptions",
                               sliderInput(inputId = 'alpha',
                                           label='Enter Alpha (Type I Error)',
                                           value=0.05,
                                           min=0,
                                           max=1),
                               column(width = 12,selectInput("model.entry","Model Selection",
                                                             c("Model with interactions",
                                                               "No interaction model"))),
                               box(title = 'Normality of the residuals',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('normality',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2(textOutput('pruebaNorm')),
                                          tableOutput('normalityTest'),
                                          h3(textOutput('normalityConclu')),
                                          h2(htmlOutput('CumpleNorm')))),
                               box(title = 'Homoscedasticity of the residuals',collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          withSpinner(highchartOutput('homoscedasticity',  height = "350px"), type = 7, color='#C7D5EB')
                                   ),
                                   column(6,
                                          h2('Homoscedasticity test'),
                                          tableOutput('homoscedasticityBart'),
                                          h3(textOutput('homoscedasticityConclu')),
                                          h2(htmlOutput('CumpleHomoc'))
                                   )),
                               box(title = 'Independence of residuals',collapsible = TRUE,
                                   width = 12,

                                   column(12,
                                          h2('Independence by Durbin Watson Test'),
                                          tableOutput('independenceDurbin'),
                                          h3(textOutput('independenceConclu')),
                                          h2(htmlOutput('Cumpleindependence')))),
                               box(title = 'Kruskal-Wallis conditions',
                                   width = 12,collapsible = TRUE,
                                   column(6,
                                          plotOutput('kwconditions',  height = "350px")),
                                   column(6,
                                          h2('Kruskal-Wallis: Skewness & Homoscedasticity'),
                                          tableOutput('symmetryCoef'),
                                          h3(textOutput('symmetryConclu')),
                                          h2(htmlOutput('CumpleSimet')))),
                               box(width = 12,collapsible = TRUE,
                                   withSpinner(highchartOutput('diagram',  height = "650px"), type = 7, color='#C7D5EB'),
                                   h2('Technique available'),
                                   withSpinner(highchartOutput('technique'), type = 7, color='#C7D5EB'))),

                       tabItem(tabName = "ANOVAcl",
                               mainPanel(),
                               sliderInput(inputId = 'alpha2',
                                           label='Enter Alpha (Type I Error)',
                                           value=0.05,
                                           min=0,
                                           max=1),
                               box(width=12,
                                   title = "Classic ANOVA Table",collapsible = TRUE,
                                   column(width = 12,selectInput("anova.entry","Model Selection",
                                                                 c("Model with interactions",
                                                                   "No interaction model"))),
                                   column(width=12,align="center",tableOutput('Aov')),
                                   h2("Conclusion"),
                                   h3(textOutput('conclutionAovA')),
                                   h3(textOutput('conclutionAovB')),
                                   h3(textOutput('conclutionAovAB')),
                                   plotOutput("interactionplot")),
                               box(title = "Violin Plots",collapsible = TRUE,
                                   width = 12,height=20,
                                   column(12,
                                          column(width = 12,selectInput("factor.entry","Select a Factor",
                                                                        c("Factor A",
                                                                          "Factor B",
                                                                          "Interactions"))),
                                          plotOutput('Box',width = "100%"),
                                          style="overflow-x: scroll")),
                               box(title = "Post-Hoc",collapsible = TRUE,
                                   width=12,
                                   column(6,
                                          h3('TukeyHSD'),
                                          column(width = 12,selectInput("resume.entry","Select Post-Hoc results",
                                                                        c("Factor A",
                                                                          "Factor B",
                                                                          "Interactions"))),
                                          tableOutput('AovPostHoc')),
                                   column(6,
                                          plotOutput('AovPostHocGraph')))
                       ),
                       tabItem(tabName = "KW",
                               sliderInput(inputId = 'alphakw',
                                           label='Enter Alpha (Type I Error)',
                                           value=0.05,
                                           min=0,
                                           max=1),

                               box(width= 12,
                                   column(width = 12,selectInput("modelkw.entry","Select a Model",
                                                                 c("No interaction model",
                                                                   "Model with interactions"))),
                                   title = "Kruskal Wallis Table",collapsible = TRUE,
                                   tableOutput('kw'),
                                   h2("Conclusion"),
                                   h3(textOutput('conclutionkwA')),
                                   h3(textOutput('conclutionkwB')),
                                   h3(textOutput('conclutionkwAB'))),
                               box(title = "Violin Plots",collapsible = TRUE,
                                   width = 12,
                                   column(12,
                                          column(width = 12,selectInput("factorK.entry","Select a Factor",
                                                                        c("Factor A",
                                                                          "Factor B",
                                                                          "Interactions"))),
                                          plotOutput('BoxK',width = "100%"),
                                          style="overflow-x: scroll")),
                               box(width=12,
                                   title = 'Post Hoc: Pairwise comparisons',collapsible = TRUE,
                                   column(width = 12,selectInput("dunn.entry","Select Post-Hoc results",
                                                                 c("Factor A",
                                                                   "Factor B",
                                                                   "Interactions"))),
                                   h3('p-values adjusted'),
                                   DT::dataTableOutput('KWpost')
                               )

                       ),
                       tabItem(tabName = "ANOVAby",

                               box(title = "Bayesian ANOVA Table",collapsible = TRUE,
                                   tableOutput('AovBY'),
                                   h2("Conclusion"),
                                   h3(textOutput('conclutionaovby'))),
                               box(title = 'Control center',collapsible = TRUE,
                                   sliderInput(inputId = 'prior',
                                               label='Enter prior probability',
                                               value=0.5,
                                               min=0,
                                               max=1),
                                   numericInput(inputId = 'numberiterations',
                                                label='Enter the number of iterations',
                                                value=1000,
                                                min=500,
                                                max=3000),
                                   sliderInput(inputId = 'chainsnumber',
                                               label='Enter number of chains:',
                                               value=1,
                                               min=1,
                                               max=4)),
                               box(title = "Posterior", width=12,collapsible = TRUE,
                                   column(6,selectInput("usuario.entrada","Model selection",
                                                        c("Model 1",
                                                          "Model 2",
                                                          "Model 3",
                                                          "Model 4"))),
                                   column(12, align="center",DT::dataTableOutput('AovBYpost'))),
                               box(title = "MCMC",collapsible = TRUE,
                                   width = 12,
                                   column(6,
                                          selectInput("mcmcCHAIN","MCMC selection",
                                                      c("Mean and Variance",
                                                        "Treatments")),
                                          withSpinner(highchartOutput('AovBYposmcmc',  height = "450px"), type = 7, color='#C7D5EB')),
                                   column(6,withSpinner(highchartOutput('AovBYposcurves',  height = "450px"), type = 7, color='#C7D5EB'))

                               )

                       )
                     ))),
    dashboardFooter(
      left = NULL,
      right = NULL),

    server = function(input, output) {
      data <- reactive({


        if (is.data.frame(dataset)){
          data = dataset

        } else {

          inFile <- input$file1
          if (is.null(inFile))
            return(NULL)
          data=read.csv2(inFile$datapath, sep=input$separador,header = input$header)
        }
        data
      })

      output$DTable <- DT::renderDataTable({
        Data <- data()

        datatable(Data, extensions = 'FixedColumns',
                  options = list(
                    dom = 't',
                    scrollX = TRUE,
                    fixedColumns = TRUE
                  ))
      })

      output$var <- renderUI({

        if(is.null(data())){return()}

        else list (

          selectInput("y", "Dependent variable", choices =    names(data())),
          selectInput("x1", "Factor A", choices = names(data())),
          selectInput("x2", "Factor B", choices = names(data()))


        )
      })

      output$Aov <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))
        dataAOV <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataAOV) <- c('A','B','y')
        if (input$anova.entry == "Model with interactions"){
          SA <- summary(aov(y~A*B,data = dataAOV))
          S <- as.data.frame(SA[[1]])
          S <- signif(S,4)
          S[is.na(S)] <- ' '
          names= rownames(S)
          S=cbind(names,S)
          colnames(S) <- c('','df','SS','MS','F','p-value')
          S
        }
        else if(input$anova.entry == "No interaction model"){
          SA <- summary(aov(y~A+B,data = dataAOV))
          S <- as.data.frame(SA[[1]])
          S <- signif(S,4)
          S[is.na(S)] <- ' '
          names= rownames(S)
          S=cbind(names,S)
          colnames(S) <- c('','df','SS','MS','F','p-value')
          S
        }
      })

      output$conclutionAovA <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <- summary(aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))+as.factor(as.matrix(Data[,Ind2]))))
        if (SA[[1]][['Pr(>F)']][1] < input$alpha2){
          response <- paste0('1. There are significant differences between the groups of ',Ind1)
        } else if  (SA[[1]][['Pr(>F)']][1] > input$alpha2){
          response<- paste0('1. There are no significant differences between the groups of ',Ind1)}
      })

      output$conclutionAovB <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <- summary(aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))+as.factor(as.matrix(Data[,Ind2]))))
        if (SA[[1]][['Pr(>F)']][2] < input$alpha2){
          response <- paste0('2. There are significant differences between the groups of ',Ind2)
        } else if  (SA[[1]][['Pr(>F)']][2] > input$alpha2){
          response<- paste0('2. There are no significant differences between the groups of ',Ind2)}
      })

      output$conclutionAovAB <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        if (input$anova.entry == "Model with interactions"){
          SA <- summary(aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))*as.factor(as.matrix(Data[,Ind2]))))
          if (SA[[1]][['Pr(>F)']][3] < input$alpha2){
            response <- paste0('3. There are significant differences between the interactions')
          } else if  (SA[[1]][['Pr(>F)']][3] > input$alpha2){
            response<- paste0('3. There are no significant differences between the interactions')}
        }
        else if(input$anova.entry == "No interaction model"){
          response<- paste0(' ')
        }

      })

      output$interactionplot= renderPlot({

        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))
        dataAOV <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataAOV) <- c('A','B','y')

        if (input$anova.entry == "Model with interactions"){
          ggplot(data=dataAOV,aes(x=A,y=y,colour=B,group=B))+
            stat_summary(fun=mean,geom="point")+
            stat_summary(fun=mean,geom = "line")+
            labs(y='mean(Y)')+
            theme_bw()
        }
        else{

        }
      })
      output$normality <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        if (input$model.entry == "Model with interactions"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))*as.factor(as.matrix(Data[,Ind2]))))
        }
        else if(input$model.entry == "No interaction model"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))+as.factor(as.matrix(Data[,Ind2]))))
        }

        Graph <- qqnorm(SA$residuals, pch = 1, frame = FALSE)
        DataLine <- data.frame(xd=Graph[[1]],yd=Graph['y'])
        colnames(DataLine) <- c('xd','yd')
        LIN <- augment(lm(yd~xd, data=DataLine))


        yRES=SA$residuals
        distribution = qnorm
        probs = c(0.25, 0.75)
        qtype = 7

        y1 <- quantile(yRES, probs, names = FALSE, type = qtype, na.rm = TRUE)
        x1 <- distribution(probs)

        slope <- diff(y1)/diff(x1)
        int <- y1[1L] - slope * x1[1L]

        Int=int
        Slp=slope


        x=Graph[[1]]
        Recta <- Int+Slp*x
        lineQQ <- data.frame(x2=Graph[[1]], y2=Recta)
        highchart() %>%
          hc_add_series(lineQQ, "line", hcaes(x = 'x2', y = 'y2'), name='QQ line', color='#A9DEDE',
                        marker= list(symbol='url(graphic.png)'))%>%
          hc_add_series(LIN, "scatter", hcaes(x='xd', y='yd'), name='Points', color='#2B275A') %>%
          hc_yAxis(
            title = list(text = "Standardized Residuals"),
            max=max(lineQQ$y2),
            min=min(lineQQ$y2))%>%
          hc_xAxis(
            title = list(text = "Theoretical Quantiles"))%>%
          hc_title(text='QQ plot')
      })

      output$AovPostHoc <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))
        dataAOV <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataAOV) <- c('A','B','y')

        if (input$anova.entry == "Model with interactions"){
          SA <- aov(y~A*B,data = dataAOV)}
        else if (input$anova.entry == "No interaction model"){
          SA <- aov(y~A+B,data = dataAOV)}

        intervals = TukeyHSD(SA)

        if (input$resume.entry == "Factor A"){
          S <- as.data.frame(intervals[[1]])
          S <- signif(S,4)
          S <- cbind(rownames(S),S)

          names(S)[1] <- ' '

          S}
        else if (input$resume.entry == "Factor B"){
          S <- as.data.frame(intervals[[2]])
          S <- signif(S,4)
          S <- cbind(rownames(S),S)

          names(S)[1] <- ' '

          S}
        else if (input$resume.entry == "Interactions"){
          S <- as.data.frame(intervals[[3]])
          S <- signif(S,4)
          S <- cbind(rownames(S),S)

          names(S)[1] <- ' '

          S}
      })

      output$AovPostHocGraph <- renderPlot({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))
        dataAOV <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataAOV) <- c('A','B','y')

        if (input$resume.entry == "Factor A"){
          tukeyA= TukeyHSD(aov(y~A,data=dataAOV))
          plot(tukeyA)}
        else if (input$resume.entry == "Factor B"){
          tukeyB= TukeyHSD(aov(y~B,data=dataAOV))
          plot(tukeyB)}
        else if (input$resume.entry == "Interactions"){
          tukey= TukeyHSD(aov(y~A*B,data=dataAOV))
          plot(tukey)}
      })

      output$normalityTest <- renderTable({
        Data <- data()

        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        if (input$model.entry == "Model with interactions"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))*as.factor(as.matrix(Data[,Ind2]))))
        }
        else if(input$model.entry == "No interaction model"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))+as.factor(as.matrix(Data[,Ind2]))))
        }

        if (length(SA$residuals)>30){

          Test <- lillie.test(SA$residuals)
          Tabla <- data.frame(Statistic=signif(Test$statistic,4),
                              ValP=signif(Test$p.value,4))
          colnames(Tabla) <- c('KS Statistic','p-value')
          Tabla
        } else {
          Test <- shapiro.test(SA$residuals)
          Tabla <- data.frame(Statistic=Test$statistic,
                              ValP=Test$p.value)
          colnames(Tabla) <- c('Shapiro-Wilk statistic','p-value')
          Tabla
        }
      })


      output$normalityConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        if (input$model.entry == "Model with interactions"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))*as.factor(as.matrix(Data[,Ind2]))))
        }
        else if(input$model.entry == "No interaction model"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))+as.factor(as.matrix(Data[,Ind2]))))
        }


        if (length(SA$residuals)>30){
          Test <- lillie.test(SA$residuals)
          if (Test$p.value >= input$alpha){
            response=paste0('According to the Kolmogorov-Smirnov test, the residuals are normal')
          } else {
            response=paste0('According to the Kolmogorov-Smirnov test, the residuals are not normal')
          }
          response
        } else {
          Test <- shapiro.test(SA$residuals)
          if (Test$p.value >= input$alpha){
            response=paste0('According to the Shapiro-Wilk test, the residuals are normal')
          } else {
            response=paste0('According to the Shapiro-Wilk test, the residuals are not normal')
          }
          response
        }
      })

      output$pruebaNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        if (input$model.entry == "Model with interactions"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))*as.factor(as.matrix(Data[,Ind2]))))
        }
        else if(input$model.entry == "No interaction model"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))+as.factor(as.matrix(Data[,Ind2]))))
        }

        if (length(SA$residuals)>30){

          response=paste0('Normality by Kolmogorov-Smirnov test')

          response
        } else {

          response=paste0('Normality by Shapiro-Wilk test')

          response
        }
      })

      output$CumpleNorm <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        if (input$model.entry == "Model with interactions"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))*as.factor(as.matrix(Data[,Ind2]))))
        }
        else if(input$model.entry == "No interaction model"){
          SA <- (aov(as.numeric(as.matrix(Data[,Dep]))~as.factor(as.matrix(Data[,Ind1]))+as.factor(as.matrix(Data[,Ind2]))))
        }


        if (length(SA$residuals)>30){
          Test <- lillie.test(SA$residuals)

          if(Test$p.value >=  input$alpha ){
            return(paste("Assumption of Normality: ","<span style=\"color:green;\"> Is met. </span>"))

          }else{
            return(paste("Assumption of Normality: ","<span style=\"color:red;\"> Is not met.</span>"))
          }} else {

            Test <- shapiro.test(SA$residuals)

            if(Test$p.value >=  input$alpha ){
              return(paste("Assumption of Normality: ","<span style=\"color:green;\"> Is met.</span>"))

            }else{
              return(paste("Assumption of Normality: ","<span style=\"color:red;\"> Is not met.</span>"))
            }
          }
      })



      #_________________________________________________________________

      output$homoscedasticity <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        if (input$model.entry == "Model with interactions"){
          SA <- (aov(Dep2 ~ Ind21*Ind22, data=dataBY))
        }
        else if(input$model.entry == "No interaction model"){
          SA <- (aov(Dep2 ~ Ind21+Ind22, data=dataBY))
        }

        xs=SA$fitted.values
        ys=SA$residuals
        lineAR <- data.frame(x2=xs, y2=ys)
        highchart() %>%

          hc_yAxis(
            title = list(text = "Residuals"),
            plotLines = list(list(
              value = 0,
              color = '#A9DEDE',
              width = 3,
              zIndex = 4,
              label = list(text = "",
                           style = list( color = '#1D4B5E', fontWeight = 'bold' )))),
            max=max(lineAR$y2),
            min=min(lineAR$y2))%>%
          hc_add_series(lineAR, "scatter", hcaes(x = 'x2', y = 'y2'), name='Residual vs Adjusted', color='#2B275A'
          )%>%
          hc_xAxis(
            title = list(text = "Adjusted values"))%>%
          hc_title(text='Residual vs Adjusted')
      })


      output$homoscedasticityBart <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        if (input$model.entry == "Model with interactions"){
          Bart <- bartlett.test(Dep2 ~ interaction(Ind21, Ind22), data=dataBY)
          Tabla <- data.frame(Statistic=signif(Bart$statistic,4),
                              ValP=signif(Bart$p.value,4))
          colnames(Tabla) <- c('Bartlett`s K-square statistic','p-value')
          Tabla

        }
        else if(input$model.entry == "No interaction model"){
          Bart <- bptest(lm(Dep2 ~ Ind21+Ind22, data=dataBY))
          Tabla <- data.frame(Statistic=signif(Bart$statistic,4),
                              ValP=signif(Bart$p.value,4))
          colnames(Tabla) <- c('Breusch-Pagan`s K-square statistic','p-value')
          Tabla
        }

      })


      output$homoscedasticityConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        prio <- input$prior

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        if (input$model.entry == "Model with interactions"){
          Bart <- bartlett.test(Dep2 ~ interaction(Ind21, Ind22), data=dataBY)
          if (Bart$p.value >= input$alpha){
            response=paste0('According to the Bartlett test, the samples show equal variances')
          } else {
            response=paste0('According to the Bartlett test, the samples show unequal variances')
          }
          response
        }
        else if(input$model.entry == "No interaction model"){
          Bart <- bptest(lm(Dep2 ~ Ind21+Ind22, data=dataBY))
          if (Bart$p.value >= input$alpha){
            response=paste0('According to the Breusch-Pagan test, the samples show equal variances')
          } else {
            response=paste0('According to the Breusch-Pagan test, the samples show unequal variances')
          }
          response
        }

      })


      output$CumpleHomoc <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        prio <- input$prior

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        if (input$model.entry == "Model with interactions"){
          Bart <- bartlett.test(Dep2 ~ interaction(Ind21, Ind22), data=dataBY)
        }
        else if(input$model.entry == "No interaction model"){
          Bart <- bptest(lm(Dep2 ~ Ind21+Ind22, data=dataBY))
        }


        if(Bart$p.value >=  input$alpha ){
          return(paste("Homoscedasticity assumption: ","<span style=\"color:green;\"> Is met.</span>"))

        }else{
          return(paste("Homoscedasticity assumption: ","<span style=\"color:red;\"> Is not met.</span>"))
        }
      })
      #________________________________________________________________




      output$independenceDurbin <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2


        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        if (input$model.entry == "Model with interactions"){
          Aov <- aov(Dep2 ~ Ind21*Ind22, data=dataBY)
        }
        else if(input$model.entry == "No interaction model"){
          Aov <- aov(Dep2 ~ Ind21+Ind22, data=dataBY)
        }

        DW <- durbinWatsonTest(Aov)
        Tabla <- data.frame(Autocor=DW[1],
                            Dw=signif(as.numeric(DW[2]),4),
                            ValP=signif(as.numeric(DW[3]),4))
        colnames(Tabla) <- c('Autocorrelation','D-W Statistic',
                             'p-value')
        Tabla
      })


      output$independenceConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        prio <- input$prior

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))


        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        if (input$model.entry == "Model with interactions"){
          Aov <- aov(Dep2 ~ Ind21*Ind22, data=dataBY)
        }
        else if(input$model.entry == "No interaction model"){
          Aov <- aov(Dep2 ~ Ind21+Ind22, data=dataBY)
        }


        DW <- durbinWatsonTest(Aov)

        if (DW[3] >= input$alpha){
          response=paste0('According to the Durbin Watson test, there is no presence of autocorrelation in the residuals.')
        } else {
          response=paste0('According to the Durbin Watson test, there is the presence of autocorrelation in the residuals.')
        }
        response
      })


      output$Cumpleindependence <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        prio <- input$prior

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        if (input$model.entry == "Model with interactions"){
          Aov <- aov(Dep2 ~ Ind21*Ind22, data=dataBY)
        }
        else if(input$model.entry == "No interaction model"){
          Aov <- aov(Dep2 ~ Ind21+Ind22, data=dataBY)
        }

        DW <- durbinWatsonTest(Aov)

        if (DW[3] >= input$alpha){
          return(paste("Independence assumption: ","<span style=\"color:green;\"> Is met.</span>"))

        }else{
          return(paste("Independence assumption: ","<span style=\"color:red;\"> Is not met.</span>"))
        }
      })

      #_________________________________________________________

      output$kwconditions <- renderPlot({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        c=input$alpha2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        plotViolinAB <- function(df, fac= "A",c){
          df <- drop_na(df)
          c =1-as.double(c)
          dfsortA <- df[with(df, order(df[,1])), ]
          dfsortB <- df[with(df, order(df[,2])), ]
          dataA <- data.frame(dfsortA[,3], dfsortA[,1])
          dataB <- data.frame(dfsortB[,3], dfsortB[,2])
          dataAB <- getDF2fact(df)
          names(dataA) <- c("Y", "A")
          names(dataB) <- c("Y", "B")


          violinA <- ggbetweenstats(data = dataA,
                                    x = A,
                                    y = Y,
                                    type = "p",
                                    #centrality.point.args = list(size = 5, color = "darkred"),
                                    centrality.label.args = list(size = 3,
                                                                 nudge_x = 0.4,
                                                                 segment.linetype = 4,
                                                                 min.segment.length = 0),
                                    pairwise.comparisons = TRUE,
                                    pairwise.display = "ns",
                                    conf.level = c,
                                    outlier.label.args = list(size = 5, color= "red"),
                                    ggsignif.args = list(textsize = 5, tip_length = 0.01),
                                    ggtheme = ggstatsplot::theme_ggstatsplot(),
                                    #centrality.type = "p"
                                    ggplot.component = list(
                                      theme(
                                        text = element_text(
                                          family = NULL,
                                          face = NULL,
                                          colour = NULL,
                                          size = 16,
                                          hjust = NULL,
                                          vjust = NULL,
                                          angle = NULL,
                                          lineheight = NULL,
                                          color = NULL,
                                          margin = NULL,
                                          debug = NULL,
                                          inherit.blank = FALSE
                                        )
                                      )
                                    )

          )+ scale_y_continuous(sec.axis = dup_axis())

          violinB <- ggbetweenstats(data = dataB,
                                    x = B,
                                    y = Y,
                                    type = "p",
                                    centrality.point.args = list(size = 5, color = "darkred"),
                                    centrality.label.args = list(size = 3,
                                                                 nudge_x = 0.4,
                                                                 segment.linetype = 4,
                                                                 min.segment.length = 0),
                                    pairwise.comparisons = TRUE,
                                    pairwise.display = "ns",
                                    conf.level = c,
                                    outlier.label.args = list(size = 5, color= "red"),
                                    ggsignif.args = list(textsize = 5, tip_length = 0.01),
                                    ggtheme = ggstatsplot::theme_ggstatsplot(),
                                    #centrality.type = "p"
                                    #ggplot.component = list(theme(text = element_text(size = 16)))
                                    ggplot.component = list(
                                      theme(
                                        text = element_text(
                                          family = NULL,
                                          face = NULL,
                                          colour = NULL,
                                          size = 16,
                                          hjust = NULL,
                                          vjust = NULL,
                                          angle = NULL,
                                          lineheight = NULL,
                                          color = NULL,
                                          margin = NULL,
                                          debug = NULL,
                                          inherit.blank = FALSE
                                        )
                                      )
                                    )
          )+ scale_y_continuous(sec.axis = dup_axis())

          violinAB <- ggbetweenstats(data = dataAB,results.subtitle=FALSE,centrality.plotting=FALSE,
                                     x = AB,
                                     y = Y,
                                     type = "p",
                                     centrality.point.args = list(size = 5, color = "darkred"),
                                     centrality.label.args = list(size = 4,
                                                                  nudge_x = 0.4,
                                                                  segment.linetype = 4,
                                                                  min.segment.length = 0),
                                     pairwise.comparisons = FALSE,
                                     pairwise.display = "ns",
                                     conf.level = c,
                                     outlier.label.args = list(size = 5, color= "red"),
                                     ggsignif.args = list(textsize = 5, tip_length = 0.01),
                                     ggtheme = ggstatsplot::theme_ggstatsplot(),
                                     #centrality.type = "p"
                                     #ggplot.component = list(theme(text = element_text(size = 16)))
                                     ggplot.component = list(
                                       theme(
                                         text = element_text(
                                           family = NULL,
                                           face = NULL,
                                           colour = NULL,
                                           size = 16,
                                           hjust = NULL,
                                           vjust = NULL,
                                           angle = NULL,
                                           lineheight = NULL,
                                           color = NULL,
                                           margin = NULL,
                                           debug = NULL,
                                           inherit.blank = FALSE
                                         )
                                       )
                                     )
          )+ scale_y_continuous(sec.axis = dup_axis()) + ggplot2::labs(subtitle = NULL)

          if (fac=="A"){
            return(violinA)
          }else if (fac=="B"){
            return(violinB)
          }else if(fac=="AB"){
            return(violinAB)
          }else{

          }
        }
        plotViolinAB(dataBY,"AB",c)

      })


      output$symmetryCoef <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        c=input$alpha2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('A','B','y')

        trats <- getDF2fact(dataBY)
        df_trat = as.data.frame(trats)

        test_bar <- bartlett.test(df_trat$Y,df_trat$AB)

        Tabla <- data.frame(Statistic=test_bar$statistic,p.value=test_bar$p.value)
        colnames(Tabla) <- c("Bartlett's K-squared","P value")
        Tabla
      })


      output$symmetryConclu <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('A','B','y')

        listA <- unique(dataBY[,1])
        listB <- unique(dataBY[,2])
        num_trat= length(listA)*length(listB)

        getDF2fact <- function(dataAB){
          dataAB <- dataAB[with(dataAB, order(dataAB[,1], dataAB[,2])), ]
          names(dataAB) <- c("A","B","y")
          listA <- unique(dataAB[,1])
          listB <- unique(dataAB[,2])
          i = 0
          j = 0
          dfAB <- data.frame()
          dfn <- data.frame()
          while(i<length(listA)){
            while(j<length(listB)){
              df <- filter(dataAB, A== listA[i+1] & B== listB[j+1])
              df <- data.frame(Y = as.double(df[,3]),
                               AB = as.factor(paste(listA[i+1], "-",listB[j+1]))
              )
              df2 <- data.frame(tratamiento = unique(df[,2]), n = dim(df)[1],
                                sesgo = skewness(df[,1]))
              dfn <- rbind(dfn, df2)
              dfAB <- rbind(dfAB, df)
              j= j+1
            }
            j=0
            i= i+1
          }

          dfAB <- as.data.frame(dfAB)
          dfn <- as.data.frame(dfn)
          dataf <- list(dfAB, dfn)
          return(dataf)
        }

        trats <- getDF2fact(dataBY)
        df_n = as.data.frame(trats[2])
        df_trat = as.data.frame(trats[1])

        test_bar <- bartlett.test(df_trat$Y, df_trat$AB)
        homocedasticidad <- (test_bar$p.value>0.05)

        # Case1
        s1 <- which(df_n$sesgo>0)
        # Case2
        s2 <- which(df_n$sesgo==0)
        # Case3
        s3 <- which(df_n$sesgo<0)

        if (length(s1) == length(df_n$sesgo)){
          tsesgo = "Positively Skewed"
          msesgo = TRUE
        }else if(length(s2) == length(df_n$sesgo)){
          tsesgo = "Unskewed"
          msesgo = TRUE
        }else if(length(s3) == length(df_n$sesgo)){
          tsesgo = "Negatively Skewed"
          msesgo = TRUE
        }else{
          msesgo = FALSE
          tsesgo = "at least one of the treatments have different skewness"
        }


        if (homocedasticidad == TRUE  & msesgo==TRUE){
          response=paste0('According to the p value, the distributions of the response variable for all treatments have the same variance and are ',tsesgo)
          response
        } else if (homocedasticidad == FALSE  & msesgo==TRUE){
          response=paste0('According to the p value, the distributions of the response variable for all treatments have different variance and are ',tsesgo)
          response
        } else if (homocedasticidad == TRUE  & msesgo==FALSE){
          response=paste0('According to the p value, the distributions of the response variable for all treatments have the same variance and ',tsesgo)
          response
        } else if (homocedasticidad == FALSE  & msesgo==FALSE){
          response=paste0('According to the p value, the distributions of the response variable for all treatments have different variance and ',tsesgo)
          response
        }

      })


      output$CumpleSimet <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('A','B','y')

        listA <- unique(dataBY[,1])
        listB <- unique(dataBY[,2])
        num_trat= length(listA)*length(listB)

        getDF2fact <- function(dataAB){
          dataAB <- dataAB[with(dataAB, order(dataAB[,1], dataAB[,2])), ]
          names(dataAB) <- c("A","B","y")
          listA <- unique(dataAB[,1])
          listB <- unique(dataAB[,2])
          i = 0
          j = 0
          dfAB <- data.frame()
          dfn <- data.frame()
          while(i<length(listA)){
            while(j<length(listB)){
              df <- filter(dataAB, A== listA[i+1] & B== listB[j+1])
              df <- data.frame(Y = as.double(df[,3]),
                               AB = as.factor(paste(listA[i+1], "-",listB[j+1]))
              )
              df2 <- data.frame(tratamiento = unique(df[,2]), n = dim(df)[1],
                                sesgo = skewness(df[,1]))
              dfn <- rbind(dfn, df2)
              dfAB <- rbind(dfAB, df)
              j= j+1
            }
            j=0
            i= i+1
          }

          dfAB <- as.data.frame(dfAB)
          dfn <- as.data.frame(dfn)
          dataf <- list(dfAB, dfn)
          return(dataf)
        }

        trats <- getDF2fact(dataBY)
        df_n = as.data.frame(trats[2])
        df_trat = as.data.frame(trats[1])

        if (df_n[1,2]>5){

          test_bar <- bartlett.test(df_trat$Y, df_trat$AB)
          test_bar

          homocedasticidad <- (test_bar$p.value>0.05)

          # Case1
          s1 <- which(df_n$sesgo>0)
          # Case2
          s2 <- which(df_n$sesgo==0)
          # Case3
          s3 <- which(df_n$sesgo<0)

          if (length(s1) == length(df_n$sesgo)){
            tsesgo = "Positively Skewed"
            msesgo = TRUE
          }else if(length(s2) == length(df_n$sesgo)){
            tsesgo = "Unskewed"
            msesgo = TRUE
          }else if(length(s3) == length(df_n$sesgo)){
            tsesgo = "Negatively Skewed"
            msesgo = TRUE
          }else{
            msesgo = FALSE
            tsesgo = "At least one of the treatments has different skewness"
          }

          if (homocedasticidad == TRUE  & msesgo==TRUE) {
            kw = TRUE
          } else{
            kw= FALSE
          }


        }else{
          kw= FALSE
        }
        if(kw == TRUE ){
          return(paste("Symmetry Assumption: ","<span style=\"color:green;\"> Is met.</span>"))

        }else{
          return(paste("Symmetry Assumption: ","<span style=\"color:red;\"> Is not met.</span>"))
        }
      })

      #__________________________________________________




      output$Box <- renderPlot({

        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        c=input$alpha2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        getDF2fact <- function(dataAB){
          dataAB <- dataAB[with(dataAB, order(dataAB[,1], dataAB[,2])), ]
          names(dataAB) <- c("A","B","y")
          listA <- unique(dataAB[,1])
          listB <- unique(dataAB[,2])
          i = 0
          j = 0
          dfAB <- data.frame()
          while(i<length(listA)){
            while(j<length(listB)){
              df <- filter(dataAB, A== listA[i+1] & B== listB[j+1])
              df <- data.frame(Y = as.double(df[,3]),
                               AB = as.factor(paste(listA[i+1], "-",listB[j+1]))
              )
              dfAB <- rbind(dfAB, df)
              j= j+1
            }
            j=0
            i= i+1
          }

          dfAB <- as.data.frame(dfAB)
          return(dfAB)
        }

        plotViolinAB <- function(df, fac= "A",c){
          df <- drop_na(df)
          c =1-as.double(c)
          dfsortA <- df[with(df, order(df[,1])), ]
          dfsortB <- df[with(df, order(df[,2])), ]
          dataA <- data.frame(dfsortA[,3], dfsortA[,1])
          dataB <- data.frame(dfsortB[,3], dfsortB[,2])
          dataAB <- getDF2fact(df)
          names(dataA) <- c("Y", "A")
          names(dataB) <- c("Y", "B")


          violinA <- ggbetweenstats(data = dataA,
                                    x = A,
                                    y = Y,
                                    type = "p",
                                    #centrality.point.args = list(size = 5, color = "darkred"),
                                    centrality.label.args = list(size = 3,
                                                                 nudge_x = 0.4,
                                                                 segment.linetype = 4,
                                                                 min.segment.length = 0),
                                    pairwise.comparisons = TRUE,
                                    pairwise.display = "ns",
                                    conf.level = c,
                                    outlier.label.args = list(size = 5, color= "red"),
                                    ggsignif.args = list(textsize = 5, tip_length = 0.01),
                                    ggtheme = ggstatsplot::theme_ggstatsplot(),
                                    #centrality.type = "p"
                                    ggplot.component = list(
                                      theme(
                                        text = element_text(
                                          family = NULL,
                                          face = NULL,
                                          colour = NULL,
                                          size = 16,
                                          hjust = NULL,
                                          vjust = NULL,
                                          angle = NULL,
                                          lineheight = NULL,
                                          color = NULL,
                                          margin = NULL,
                                          debug = NULL,
                                          inherit.blank = FALSE
                                        )
                                      )
                                    )

          )+ scale_y_continuous(sec.axis = dup_axis())

          violinB <- ggbetweenstats(data = dataB,
                                    x = B,
                                    y = Y,
                                    type = "p",
                                    centrality.point.args = list(size = 5, color = "darkred"),
                                    centrality.label.args = list(size = 3,
                                                                 nudge_x = 0.4,
                                                                 segment.linetype = 4,
                                                                 min.segment.length = 0),
                                    pairwise.comparisons = TRUE,
                                    pairwise.display = "ns",
                                    conf.level = c,
                                    outlier.label.args = list(size = 5, color= "red"),
                                    ggsignif.args = list(textsize = 5, tip_length = 0.01),
                                    ggtheme = ggstatsplot::theme_ggstatsplot(),
                                    #centrality.type = "p"
                                    #ggplot.component = list(theme(text = element_text(size = 16)))
                                    ggplot.component = list(
                                      theme(
                                        text = element_text(
                                          family = NULL,
                                          face = NULL,
                                          colour = NULL,
                                          size = 16,
                                          hjust = NULL,
                                          vjust = NULL,
                                          angle = NULL,
                                          lineheight = NULL,
                                          color = NULL,
                                          margin = NULL,
                                          debug = NULL,
                                          inherit.blank = FALSE
                                        )
                                      )
                                    )
          )+ scale_y_continuous(sec.axis = dup_axis())

          violinAB <- ggbetweenstats(data = dataAB,
                                     x = AB,
                                     y = Y,
                                     type = "p",
                                     centrality.point.args = list(size = 5, color = "darkred"),
                                     centrality.label.args = list(size = 4,
                                                                  nudge_x = 0.4,
                                                                  segment.linetype = 4,
                                                                  min.segment.length = 0),
                                     pairwise.comparisons = FALSE,
                                     pairwise.display = "ns",
                                     conf.level = c,
                                     outlier.label.args = list(size = 5, color= "red"),
                                     ggsignif.args = list(textsize = 5, tip_length = 0.01),
                                     ggtheme = ggstatsplot::theme_ggstatsplot(),
                                     #centrality.type = "p"
                                     #ggplot.component = list(theme(text = element_text(size = 16)))
                                     ggplot.component = list(
                                       theme(
                                         text = element_text(
                                           family = NULL,
                                           face = NULL,
                                           colour = NULL,
                                           size = 16,
                                           hjust = NULL,
                                           vjust = NULL,
                                           angle = NULL,
                                           lineheight = NULL,
                                           color = NULL,
                                           margin = NULL,
                                           debug = NULL,
                                           inherit.blank = FALSE
                                         )
                                       )
                                     )
          )+ scale_y_continuous(sec.axis = dup_axis())

          if (fac=="A"){
            return(violinA)
          }else if (fac=="B"){
            return(violinB)
          }else if(fac=="AB"){
            return(violinAB)
          }else{

          }
        }

        if(input$factor.entry == "Factor A"){
          plotViolinAB(dataBY,"A",c)
        }else if (input$factor.entry == "Factor B"){
          plotViolinAB(dataBY,"B",c)
        }else if (input$factor.entry == "Interactions"){
          plotViolinAB(dataBY,"AB",c)
        }
      })

      output$BoxK <- renderPlot({

        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        c=input$alphakw

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        getDF2fact <- function(dataAB){
          dataAB <- dataAB[with(dataAB, order(dataAB[,1], dataAB[,2])), ]
          names(dataAB) <- c("A","B","y")
          listA <- unique(dataAB[,1])
          listB <- unique(dataAB[,2])
          i = 0
          j = 0
          dfAB <- data.frame()
          while(i<length(listA)){
            while(j<length(listB)){
              df <- filter(dataAB, A== listA[i+1] & B== listB[j+1])
              df <- data.frame(Y = as.double(df[,3]),
                               AB = as.factor(paste(listA[i+1], "-",listB[j+1]))
              )
              dfAB <- rbind(dfAB, df)
              j= j+1
            }
            j=0
            i= i+1
          }

          dfAB <- as.data.frame(dfAB)
          return(dfAB)
        }

        plotViolinAB <- function(df, fac= "A",c){
          df <- drop_na(df)
          c =1-as.double(c)
          dfsortA <- df[with(df, order(df[,1])), ]
          dfsortB <- df[with(df, order(df[,2])), ]
          dataA <- data.frame(dfsortA[,3], dfsortA[,1])
          dataB <- data.frame(dfsortB[,3], dfsortB[,2])
          dataAB <- getDF2fact(df)
          names(dataA) <- c("Y", "A")
          names(dataB) <- c("Y", "B")


          violinA <- ggbetweenstats(data = dataA,
                                    x = A,
                                    y = Y,
                                    type = "np",
                                    #centrality.point.args = list(size = 5, color = "darkred"),
                                    centrality.label.args = list(size = 3,
                                                                 nudge_x = 0.4,
                                                                 segment.linetype = 4,
                                                                 min.segment.length = 0),
                                    pairwise.comparisons = TRUE,
                                    pairwise.display = "ns",
                                    conf.level = c,
                                    outlier.label.args = list(size = 5, color= "red"),
                                    ggsignif.args = list(textsize = 5, tip_length = 0.01),
                                    ggtheme = ggstatsplot::theme_ggstatsplot(),
                                    #centrality.type = "p"
                                    ggplot.component = list(
                                      theme(
                                        text = element_text(
                                          family = NULL,
                                          face = NULL,
                                          colour = NULL,
                                          size = 16,
                                          hjust = NULL,
                                          vjust = NULL,
                                          angle = NULL,
                                          lineheight = NULL,
                                          color = NULL,
                                          margin = NULL,
                                          debug = NULL,
                                          inherit.blank = FALSE
                                        )
                                      )
                                    )

          )+ scale_y_continuous(sec.axis = dup_axis())

          violinB <- ggbetweenstats(data = dataB,
                                    x = B,
                                    y = Y,
                                    type = "np",
                                    centrality.point.args = list(size = 5, color = "darkred"),
                                    centrality.label.args = list(size = 3,
                                                                 nudge_x = 0.4,
                                                                 segment.linetype = 4,
                                                                 min.segment.length = 0),
                                    pairwise.comparisons = TRUE,
                                    pairwise.display = "ns",
                                    conf.level = c,
                                    outlier.label.args = list(size = 5, color= "red"),
                                    ggsignif.args = list(textsize = 5, tip_length = 0.01),
                                    ggtheme = ggstatsplot::theme_ggstatsplot(),
                                    #centrality.type = "p"
                                    #ggplot.component = list(theme(text = element_text(size = 16)))
                                    ggplot.component = list(
                                      theme(
                                        text = element_text(
                                          family = NULL,
                                          face = NULL,
                                          colour = NULL,
                                          size = 16,
                                          hjust = NULL,
                                          vjust = NULL,
                                          angle = NULL,
                                          lineheight = NULL,
                                          color = NULL,
                                          margin = NULL,
                                          debug = NULL,
                                          inherit.blank = FALSE
                                        )
                                      )
                                    )
          )+ scale_y_continuous(sec.axis = dup_axis())

          violinAB <- ggbetweenstats(data = dataAB,
                                     x = AB,
                                     y = Y,
                                     type = "np",
                                     centrality.point.args = list(size = 5, color = "darkred"),
                                     centrality.label.args = list(size = 4,
                                                                  nudge_x = 0.4,
                                                                  segment.linetype = 4,
                                                                  min.segment.length = 0),
                                     pairwise.comparisons = FALSE,
                                     pairwise.display = "ns",
                                     conf.level = c,
                                     outlier.label.args = list(size = 5, color= "red"),
                                     ggsignif.args = list(textsize = 5, tip_length = 0.01),
                                     ggtheme = ggstatsplot::theme_ggstatsplot(),
                                     #centrality.type = "p"
                                     #ggplot.component = list(theme(text = element_text(size = 16)))
                                     ggplot.component = list(
                                       theme(
                                         text = element_text(
                                           family = NULL,
                                           face = NULL,
                                           colour = NULL,
                                           size = 16,
                                           hjust = NULL,
                                           vjust = NULL,
                                           angle = NULL,
                                           lineheight = NULL,
                                           color = NULL,
                                           margin = NULL,
                                           debug = NULL,
                                           inherit.blank = FALSE
                                         )
                                       )
                                     )
          )+ scale_y_continuous(sec.axis = dup_axis())

          if (fac=="A"){
            return(violinA)
          }else if (fac=="B"){
            return(violinB)
          }else if(fac=="AB"){
            return(violinAB)
          }else{

          }
        }

        if(input$factorK.entry == "Factor A"){
          plotViolinAB(dataBY,"A",c)
        }else if (input$factorK.entry == "Factor B"){
          plotViolinAB(dataBY,"B",c)
        }else if (input$factorK.entry == "Interactions"){
          plotViolinAB(dataBY,"AB",c)
        }
      })

      output$AovBY <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        prio <- input$prior

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('A','B','y')

        #str(dataBY)
        Anovabyy <- (anovaBF(y ~ A*B, data=dataBY,
                             rscaleFixed = prio,iterations = input$numberiterations))
        Anovabyy= Anovabyy/max(Anovabyy)


        S <- data.frame(Priori=(prio), BF=Anovabyy)
        TabBY <- S[,1:3]
        TabBY$BF.bf <- round(TabBY$BF.bf,3)
        TabBY$BF.error <- signif(TabBY$BF.error,3 )
        colnames(TabBY) <- c('Priori','BF10','Error')
        modelos <- c('Modelo 1','Modelo 2','Modelo 3','Modelo 4')
        Models= rownames(TabBY)
        TabBY <- cbind(modelos,Models,TabBY)

        names(TabBY)[1] <- ''
        TabBY
      })

      output$AovBYpost <- DT::renderDataTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        prio <- input$prior

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        #str(dataBY)
        Anovabyy <- (anovaBF(Dep2 ~ Ind21*Ind22, data=dataBY,
                             rscaleFixed = prio,iterations = input$numberiterations))

        if (input$usuario.entrada == "Model 1"){
          post <- (posterior(Anovabyy[1],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-ncol(post),]
          BFtable
        }
        else if (input$usuario.entrada == "Model 2"){
          post <- (posterior(Anovabyy[2],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-ncol(post),]
          BFtable
        }
        else if (input$usuario.entrada == "Model 3"){
          post <- (posterior(Anovabyy[3],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-c(ncol(post),(ncol(post)-1))])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-c(ncol(post),(ncol(post)-1)),]
          BFtable
        }
        else if (input$usuario.entrada == "Model 4"){
          post <- (posterior(Anovabyy[4],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-c(ncol(post),(ncol(post)-1),(ncol(post)-2))])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-c(ncol(post),(ncol(post)-1),(ncol(post)-2)),]
          BFtable
        }
      })


      output$AovBYposmcmc <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        prio <- input$prior

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('A','B','y')

        #str(dataBY)
        Anovabyy <- (anovaBF(y ~ A*B, data=dataBY,
                             rscaleFixed = prio,iterations = input$numberiterations))

        if (input$usuario.entrada == "Model 1"){
          post <- (posterior(Anovabyy[1],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-ncol(post),]
        }
        else if (input$usuario.entrada == "Model 2"){
          post <- (posterior(Anovabyy[2],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-ncol(post),]
        }
        else if (input$usuario.entrada == "Model 3"){
          post <- (posterior(Anovabyy[3],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-c(ncol(post),(ncol(post)-1))])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-c(ncol(post),(ncol(post)-1)),]
        }
        else if (input$usuario.entrada == "Model 4"){
          post <- (posterior(Anovabyy[4],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-c(ncol(post),(ncol(post)-1),(ncol(post)-2))])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-c(ncol(post),(ncol(post)-1),(ncol(post)-2)),]
        }


        if (input$mcmcCHAIN=="Mean and Variance"){

          highchart()%>%
            hc_yAxis_multiples( list(top = "0%", height = "50%", title = list(text = "Mean"),opposite=FALSE),
                                list(top = "50%", height = "50%", title = list(text = "Sigma2") ,opposite=TRUE))%>%
            hc_add_series(MCMC, type='line', hcaes(x=Iteration,y=mu),yAxis=0, name='Mean',color='#24509C')%>%
            hc_add_series(MCMC, type='line', hcaes(x=Iteration,y=sig2),yAxis=1, name='Sigma2',color='#31999C')
        } else {

          MCMCMer <- melt(MCMC1, id.vars="Iteration")

          Summary3=MCMCMer%>%
            group_by(variable)%>%
            summarise(Min=min(value), Mean=mean(value))

          highchart()%>%
            hc_add_series(MCMCMer, type='line', hcaes(x=Iteration, y=value, group=variable))%>%
            hc_title(text='MCMC chains')%>%
            hc_exporting(enabled = TRUE,
                         filename = paste0('Markov chains'))

        }
      })

      output$AovBYposcurves <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        prio <- input$prior

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('A','B','y')

        #str(dataBY)
        Anovabyy <- (anovaBF(y ~ A*B, data=dataBY,
                             rscaleFixed = prio,iterations = input$numberiterations))

        if (input$usuario.entrada == "Model 1"){
          post <- (posterior(Anovabyy[1],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-ncol(post),]
        }
        else if (input$usuario.entrada == "Model 2"){
          post <- (posterior(Anovabyy[2],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-ncol(post)])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-ncol(post),]
        }
        else if (input$usuario.entrada == "Model 3"){
          post <- (posterior(Anovabyy[3],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-c(ncol(post),(ncol(post)-1))])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-c(ncol(post),(ncol(post)-1))])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-c(ncol(post),(ncol(post)-1)),]
        }
        else if (input$usuario.entrada == "Model 4"){
          post <- (posterior(Anovabyy[4],iterations = input$numberiterations))
          MCMC <- data.frame(Iteration=1:input$numberiterations,post[,-c(ncol(post),(ncol(post)-1),(ncol(post)-2))])
          MCMC1 <- data.frame(Iteration=1:input$numberiterations,post[,-c(ncol(post),(ncol(post)-1),(ncol(post)-2))])[,-c(2, ncol(MCMC))]
          BFtable = as.data.frame(summary(post)$statistics)[-c(ncol(post),(ncol(post)-1),(ncol(post)-2)),]
        }


        MCMCMer <- melt(MCMC, id.vars="Iteration")


        ds <- map(levels(MCMCMer$variable), function(x){
          MCMCMer <- density(MCMCMer$value[MCMCMer$variable == x])[1:2]
          MCMCMer <- list_parse2(as.data.frame(MCMCMer))
          list(data = MCMCMer, name = x)
        })

        highchart() %>%
          hc_add_series_list(ds)%>%
          hc_yAxis(title=list(text='Density'))%>%
          hc_exporting(enabled = TRUE,
                       filename = paste0('Density curves - Posterior marginal distributions.'))%>%
          hc_chart(
            zoomType = "x"
          )
      })



      output$conclutionaovby <- renderText({

        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2
        prio <- input$prior

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        Anovabyy <- (anovaBF(Dep2 ~ Ind21*Ind22, data=dataBY,
                             rscaleFixed = prio,iterations = input$numberiterations))


        S <- data.frame(Priori=(prio), BF=Anovabyy)
        TabBY <- S[,1:3]
        TabBY$BF.bf <- round(TabBY$BF.bf,3)
        TabBY$BF.error <- signif(TabBY$BF.error,3 )
        colnames(TabBY) <- c('Priori','BF10','Error')
        rownames(TabBY) <- c('Model 1', 'Model 2','Model 3','Model 4')
        TabBY <- cbind(rownames(TabBY),TabBY)
        rownames(TabBY[which.max(TabBY$BF10),])

        response= paste0("The model that explains the data its best is: ",rownames(TabBY[which.max(TabBY$BF10),]))
      })


      output$diagram <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        alph <- input$alpha
        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        if (input$model.entry == "Model with interactions"){
          SA <- (aov(Dep2 ~ Ind21*Ind22, data=dataBY))
        }
        else if(input$model.entry == "No interaction model"){
          SA <- (aov(Dep2 ~ Ind21+Ind22, data=dataBY))
        }

        Test <- lillie.test(SA$residuals)

        if(Test$p.value >=  alph ){
          col_normality= "#77DA85"
          col_normality_yes= "#77DA85"
          col_normality_no= "#D5D5D5"

        }else{
          col_normality= "#D5D5D5"
          col_normality_yes= "#D5D5D5"
          col_normality_no= "#77DA85"
        }
        if (input$model.entry == "Model with interactions"){
          Bart <- bartlett.test(Dep2 ~ interaction(Ind21, Ind22), data=dataBY)
        }
        else if(input$model.entry == "No interaction model"){
          Bart <- bptest(lm(Dep2 ~ Ind21+Ind22, data=dataBY))
        }

        if(Bart$p.value >=  alph ){
          col_homoscedasticity= "#77DA85"
          col_homoscedasticity_yes= "#77DA85"
          col_homoscedasticity_no= "#D5D5D5"

        }else{
          col_homoscedasticity= "#D5D5D5"
          col_homoscedasticity_yes= "#D5D5D5"
          col_homoscedasticity_no= "#77DA85"
        }

        if(skewness(SA$residuals) ==  0 ){
          col_symmetry= "#77DA85"
          col_symmetry_yes= "#77DA85"
          col_symmetry_no= "#D5D5D5"

        }else{
          col_symmetry= "#D5D5D5"
          col_symmetry_yes= "#D5D5D5"
          col_symmetry_no= "#77DA85"
        }

        if(durbinWatsonTest(SA)[3] >=  alph){
          col_independence= "#77DA85"
          col_independence_yes= "#77DA85"
          col_independence_no= "#D5D5D5"

        }else{
          col_independence= "#D5D5D5"
          col_independence_yes= "#D5D5D5"
          col_independence_no= "#77DA85"
        }



        if (col_symmetry_yes == "#77DA85"){
          col_kw="#77DA85"
        } else {col_kw="#D5D5D5" }

        if (col_homoscedasticity_yes == "#77DA85"){
          col_independence="#77DA85"
        } else {col_independence="#D5D5D5" }

        #  if (col_independence_no == "#77DA85" | col_independence_yes == "#77DA85"){
        #    col_independence="#77DA85"
        #  } else {col_independence="#D5D5D5" }

        if (col_normality_yes== "#77DA85" & col_homoscedasticity_yes== "#77DA85" ){
          col_anova="#77DA85"
        }else {col_anova="#D5D5D5" }


        if (col_symmetry_yes=="#77DA85" | col_symmetry_no=="#77DA85"){
          col_symmetry= "#77DA85"
        }
        if (col_homoscedasticity_yes=="#77DA85" | col_homoscedasticity_no=="#77DA85"){
          col_homoscedasticity= "#77DA85"
        }


        highchart() %>%
          hc_chart(type = 'organization', inverted = TRUE) %>%
          hc_add_series(name='Diagram of techniques according to compliance with assumptions',
                        data = list(
                          list(from = 'Comparison of means by group', to = 'Does it comply with the normality assumption?'),
                          list(from = 'Does it comply with the normality assumption?', to = 'Yes, it fulfills normality'),
                          list(from = 'Yes, it fulfills normality', to = 'Does it meet the homoscedasticity assumption?'),
                          list(from = 'Does it comply with the normality assumption?', to = 'It does not meet normality'),
                          list(from = 'Does it meet the homoscedasticity assumption?', to = 'Yes, it fulfills homoscedasticity'),
                          list(from = 'Yes, it fulfills homoscedasticity', to = 'Does it comply with the independence assumption?'),
                          list(from = 'Does it comply with the independence assumption?', to = 'Yes, it fulfills independence'),
                          list(from = 'Does it comply with the independence assumption?', to = 'It does not meet independence'),

                          list(from = 'Does it meet the homoscedasticity assumption?', to = 'It does not meet homoscedasticity'),
                          list(from = 'Does it meet the simmetry assumption?', to = 'Yes, it fulfills simmetry'),
                          list(from = 'Does it meet the simmetry assumption?', to = 'It does not meet simmetry'),

                          list(from = 'It does not meet homoscedasticity', to = 'Does it meet the simmetry assumption?')







                        ),
                        nodes=  list(
                          list(id = 'Comparison of means by group', color="#77D0DA"),
                          list(id = 'Does it comply with the normality assumption?', color=col_normality),
                          list(id = 'Yes, it fulfills normality', color=col_normality_yes),
                          list(id = 'It does not meet normality', color=col_normality_no),
                          list(id = 'Does it meet the homoscedasticity assumption?', color=col_homoscedasticity),
                          list(id = 'Yes, it fulfills homoscedasticity', color=col_homoscedasticity_yes),
                          list(id = 'It does not meet homoscedasticity', color=col_homoscedasticity_no),
                          list(id = 'Does it meet the simmetry assumption?', color=col_symmetry),
                          list(id = 'Yes, it fulfills simmetry', color=col_symmetry_yes),
                          list(id = 'It does not meet simmetry', color=col_symmetry_no),
                          list(id = 'Does it comply with the independence assumption?', color=col_independence),
                          list(id = 'Yes, it fulfills independence', color=col_independence_yes),
                          list(id = 'It does not meet independence', color=col_independence_no)

                        ))

      })



      output$technique <- renderHighchart({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        dataBY <- data.frame(Ind21=Factor1, Ind22=Factor2, Dep2=Depend)
        colnames(dataBY) <- c('Ind21','Ind22','Dep2')

        if (input$model.entry == "Model with interactions"){
          SA <- (aov(Dep2 ~ Ind21*Ind22, data=dataBY))
        }
        else if(input$model.entry == "No interaction model"){
          SA <- (aov(Dep2 ~ Ind21+Ind22, data=dataBY))
        }


        Test <- lillie.test(SA$residuals)

        if (input$model.entry == "Model with interactions"){
          Bart <- bartlett.test(Dep2 ~ interaction(Ind21, Ind22), data=dataBY)
        }
        else if(input$model.entry == "No interaction model"){
          Bart <- bptest(lm(Dep2 ~ Ind21+Ind22, data=dataBY))
        }



        if(Test$p.value >=  input$alpha & Bart$p.value >=  input$alpha){
          col_anova="#77DA85"
        } else {col_anova="#DC7676"}

        if(skewness(SA$residuals) ==  0){
          col_kw="#77DA85"
        } else {col_kw="#DC7676"}

        highchart() %>%
          hc_chart(type = 'organization', inverted=TRUE) %>%
          hc_add_series(name='Diagram of techniques according to compliance with assumptions',
                        data = list(
                          list(from = 'Kruskal Wallis', to = 'Kruskal Wallis'),
                          list(from = 'Classic ANOVA', to = 'Classic ANOVA'),
                          list(from = 'Bayesian ANOVA', to = 'Bayesian ANOVA')
                        ),
                        nodes=  list(
                          list(id = 'Classic ANOVA', color=col_anova),
                          list(id = 'Kruskal Wallis', color=col_kw),
                          list(id = 'Bayesian ANOVA', color='#77DA85')
                        ))

      })

      output$kw <- renderTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))
        if (input$modelkw.entry == "Model with interactions"){
          SA <- kruskal.test(Depend ~ Factor1)
          S1 <- data.frame("Factor A",SA$statistic,SA$parameter,signif(SA$p.value,4))
          colnames(S1) <- c("Factor",'Kruskal-Wallis chi-squared','Gl','Val-p')

          SB <- kruskal.test(Depend ~ Factor2)
          S2 <- data.frame("Factor B",SB$statistic,SB$parameter,signif(SB$p.value,4))
          colnames(S2) <- c("Factor",'Kruskal-Wallis chi-squared','Gl','Val-p')

          SAB <- kruskal.test(Depend ~ interaction(Factor1,Factor2))
          S3 <- data.frame("AB Interaction",SAB$statistic,SAB$parameter,signif(SAB$p.value,4))
          colnames(S3) <- c("Factor",'Kruskal-Wallis chi-squared','Gl','Val-p')

          S= rbind(S1,S2,S3)
          S
        }
        else if (input$modelkw.entry == "No interaction model"){
          SA <- kruskal.test(Depend ~ Factor1)
          S1 <- data.frame("Factor A",SA$statistic,SA$parameter,signif(SA$p.value,4))
          colnames(S1) <- c("Factor",'Kruskal-Wallis chi-squared','Gl','Val-p')

          SB <- kruskal.test(Depend ~ Factor2)
          S2 <- data.frame("Factor B",SB$statistic,SB$parameter,signif(SB$p.value,4))
          colnames(S2) <- c("Factor",'Kruskal-Wallis chi-squared','Gl','Val-p')

          S= rbind(S1,S2)
          S
        }
      })
      output$conclutionkwA <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <-kruskal.test(Depend ~ Factor1)
        if (SA$p.value < input$alphakw){
          response <- paste0('1. There are significant differences between the medians of Factor A levels')
        } else if  (SA$p.value > input$alphakw){
          response <- paste0('1. There are no significant differences between the medians of Factor A levels')}
        else if  (SA$p.value == 0){
          response <- paste0('1. Kruskal-Wallis test conditions are not met.')
        }
        response
      })

      output$conclutionkwB <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <-kruskal.test(Depend ~ Factor2)
        if (SA$p.value < input$alphakw){
          response <- paste0('2. There are significant differences between the medians of Factor B levels')
        } else if  (SA$p.value > input$alphakw){
          response <- paste0('2. There are no significant differences between the medians of Factor B levels')}
        else if  (SA$p.value == 0){
          response <- paste0('2. Kruskal-Wallis test conditions are not met.')
        }
        response
      })

      output$conclutionkwAB <- renderText({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        SA <-kruskal.test(Depend ~ interaction(Factor1,Factor2))

        if (input$modelkw.entry == "Model with interactions"){

          if (SA$p.value < input$alphakw){
            response <- paste0('3. There are significant differences between the interactions')
          } else if  (SA$p.value > input$alphakw){
            response <- paste0('3. There are no significant differences between the interactions')}
          else if  (SA$p.value == 0){
            response <- paste0('3. Kruskal-Wallis test conditions are not met.')
          }

          response
        }else if (input$modelkw.entry == "No interaction model"){
          response<- paste0(' ')
          response
        }
      })

      output$KWpost <- DT::renderDataTable({
        Data <- data()
        Data <- na.omit(Data)
        Dep <- input$y
        Ind1 <- input$x1
        Ind2 <- input$x2

        Factor1 <- as.factor(as.matrix(Data[,Ind1]))
        Factor2 <- as.factor(as.matrix(Data[,Ind2]))
        Depend <- as.numeric(as.matrix(Data[,Dep]))

        if (input$dunn.entry == "Factor A"){
          dunn.test= dunnTest(Depend~Factor1,data=Data,method="none")
          DT::datatable(dunn.test$res, extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE,
                          pageLength = nrow(dunn.test$res)
                        ))}
        else if (input$dunn.entry == "Factor B"){
          dunn.test= dunnTest(Depend~Factor2,data=Data,method="none")
          DT::datatable(dunn.test$res, extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE,
                          pageLength = nrow(dunn.test$res)
                        ))}
        else if (input$dunn.entry == "Interactions"){
          dunn.test= dunnTest(Depend~interaction(Factor1,Factor2),data=Data,method="none")
          DT::datatable(dunn.test$res, extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE,
                          pageLength = nrow(dunn.test$res)
                        ))}
      })

    }))}

