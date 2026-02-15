# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("AutoPipe"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      checkboxInput("show_sil", "Show Silhouette width", FALSE),
      checkboxInput("show_clin", "Show Clinical Data", FALSE),
      checkboxInput("print_genes", "Print Genes", FALSE),
      checkboxInput("GSE", "Show Gene Set Enrichment Analysis", FALSE),
      checkboxInput("plot_mean_sil", "Show Mean Silhouette width", FALSE),
      sliderInput("TOP",
                  "Number of genes to Cluster:",
                  min = 1000,
                  max = 10000,
                  step = 1000,
                  value = 1000),
      sliderInput("max_clust",
                  "Maximum number of clusters to check:",
                  min = 2,
                  max = 20,
                  step = 1,
                  value = 8),
      sliderInput("TOP_Cluster",
                  "Number of Genes to diplay in each Cluster:",
                  min = 25,
                  max = 1000,
                  value = 150),
      sliderInput("genes_to_print",
                  "The top genes to print:",
                  min = 5,
                  max = 9,
                  value = 5),
      sliderInput("topPaths",
                  "The top pathsways to print:",
                  min = 5,
                  max = 9,
                  value = 5),
      sliderInput("threshold",
                  "Threshold for selecting genes:",
                  min = 0,
                  max = 5,
                  step = 0.1,
                  value = 2),
      selectInput("db", "Database for GSE:",
                  c("C1" = "c1",
                    "C2" = "c2",
                    "C3" = "c3",
                    "C4" = "c4",
                    "C5" = "c5",
                    "C6" = "c6",
                    "C7" = "c7")),
      tags$hr(),
      fileInput("file1", "Choose CSV File for expression data",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      
      radioButtons("sep1", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      checkboxInput("read_expr_file", "Format expression file", FALSE),
      checkboxInput("Trans", "Rows are Samples", FALSE),
      tags$hr(),
      fileInput("file2", "Choose CSV File for sample traits",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      radioButtons("sep2", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot",width = "100%",height = "800px")
    )
  )
)


                                     
                                     
                                     
##samples_data=NULL,plot_mean_sil=FALSE,threshold=2

                                     