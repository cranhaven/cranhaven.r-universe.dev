
library(shiny)
library(shinyMatrix)
library(epitools)
library(rpivotTable)

ui <- fluidPage(


   titlePanel("Contingency Table  Analysis"),


   sidebarLayout(
      sidebarPanel(

        numericInput("nrows","Enter number of rows",value = 2),
        numericInput("ncols","Enter number of cols", value = 3),
        sliderInput("los","Select level of significance",min = 0.01,max = 0.1,value = 0.05,step = 0.01),
        textInput("vname1","Enter the name of the first variable","Variable1"),
        textInput("vname2","Enter the name of the second variable","Variable2"),
        downloadButton("downloaddata", "Download dataset"),
        downloadButton("downloadPlot", "Download Association Plot")

              ),


      mainPanel(
        h6("Edit the observed frequencies and Enter the dimensions(on the margins) to get the Association Plot and dataset"),
        uiOutput("mat"),
        tabsetPanel(type = "tab",
                    tabPanel("ChisquareTest",  verbatimTextOutput("results")),
                    tabPanel("Association Plot",  plotOutput("Visual")),
                    tabPanel("Interactive Plots", rpivotTableOutput("Dashboard"))
                   ),


        h6("", tags$img(src ="K.JPG", height= 400, width=400))



      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$mat <- renderUI({
     matrixInput(
       "myMatrix",
       value = matrix(c(35,91,42,104,63,65),input$nrows,input$ncols),

       rows = list(names= TRUE,editableNames = TRUE),
       cols = list( names = TRUE,editableNames = TRUE),
       copy = TRUE,
       paste = TRUE,
       class = 'numeric'
     )
   })

   output$results <- renderPrint({
     CT =  matrix(input$myMatrix,nrow = input$nrows, ncol = input$ncols)
     row.names(CT) = row.names(input$myMatrix)
     colnames(CT) = colnames(input$myMatrix)
     dataset = expand.table(CT)
     colnames(dataset)= c(input$vname1,input$vname2)
     options(scipen =999)

     cat(sprintf("\n Ho(NULL) :  %s is independent of %s",input$vname1,input$vname2))
     cat(sprintf("\n Ha(ALTERNATE) :  %s is not independent of %s",input$vname1,input$vname2))

     cat(sprintf("\n\nThe expected frequencies for the given data \n"))
     print(expected(CT))
     cat(sprintf("\nThe results of the Chisquare Test of Independence are\n"))
     print(chisq.test(CT))
     df = chisq.test(CT)$parameter
     Chisquaretable = qchisq(1-input$los,df)
     cat(sprintf("\nThe Table value of the Chisquare is %f\n",Chisquaretable))
      cat(sprintf("\nStatistical Inference of the test : "))
      los = round(input$los,2)
    if(chisq.test(CT)$p.value < input$los)
    {
      cat(sprintf("There is enough evidence to reject Ho at %g significance level",los))}
     else
      { cat(sprintf("There is not enough evidence to reject Ho at %g significance level",los))}
     cat(sprintf("\n\nConclusion : "))
    if(chisq.test(CT)$p.value < input$los)
    { cat(sprintf("%s  is  not independent of %s",input$vname1,input$vname2))}
     else
    {cat(sprintf("%s  is  independent of %s",input$vname1,input$vname2))}
     cat(sprintf("\n\nSnapshot of the expanded table is given below \n"))
     print(head(dataset))
   })
   output$Visual <- renderPlot({
     CT =  matrix(input$myMatrix,nrow = input$nrows, ncol = input$ncols)
     row.names(CT) = row.names(input$myMatrix)
     colnames(CT) = colnames(input$myMatrix)

     dataset = expand.table(CT)
     colnames(dataset)= c(input$vname1,input$vname2)
     assocplot(table(dataset),col = c("green","red"))
   })
   datasetInput1 <- reactive({
     CT =  matrix(input$myMatrix,nrow = input$nrows, ncol = input$ncols)
     row.names(CT) = row.names(input$myMatrix)
     colnames(CT) = colnames(input$myMatrix)
     dataset2 = expand.table(CT)
     colnames(dataset2)= c(input$vname1,input$vname2)
     dataset =  dataset2


   })
   output$downloaddata <- downloadHandler(
     filename = function() {
       filetitle = paste("dataset")
       paste(filetitle, ".csv", sep = "")
     },
     content = function(file) {

       write.csv(datasetInput1(), file, row.names = FALSE)
     }
   )
   output$downloadPlot<- downloadHandler(
     filename = function() {
       paste("AssociationPlot", ".png", sep = "")
     },
     content = function(file) {
       png(file)
       CT =  matrix(input$myMatrix,nrow = input$nrows, ncol = input$ncols)
       row.names(CT) = row.names(input$myMatrix)
       colnames(CT) = colnames(input$myMatrix)

       dataset = expand.table(CT)
       colnames(dataset)= c(input$vname1,input$vname2)
       assocplot(table(dataset),col = c("green","red"))
       dev.off()
     })
   output$Dashboard <- renderRpivotTable({
     CT =  matrix(input$myMatrix,nrow = input$nrows, ncol = input$ncols)
     row.names(CT) = row.names(input$myMatrix)
     colnames(CT) = colnames(input$myMatrix)

     dataset = expand.table(CT)
     colnames(dataset)= c(input$vname1,input$vname2)
     rpivotTable(dataset)
   })
}

# Run the application
shinyApp(ui = ui, server = server)

