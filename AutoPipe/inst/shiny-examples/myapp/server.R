library(org.Hs.eg.db)
options(shiny.maxRequestSize=1000*1024^2)
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    if(input$show_clin) req(input$file2)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        if(input$read_expr_file == "FALSE"){
          df <-read.csv(input$file1$datapath,
                        sep = input$sep1,row.names = 1)
        }else{
          df <-read_expression_file(file= input$file1$datapath, format = "csv", sep=input$sep1,gene_name="SYMBOL", Trans=input$Trans)
        }
        samples_data <- read.csv(input$file2$datapath,
                       sep = input$sep2,row.names = 1)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$read_expr_file == "FALSE") {
      me_x=df
      ## calculate best number of clusters and
      res<-AutoPipe::TopPAM(me_x,max_clusters = input$max_clust, TOP=input$TOP)
      me_TOP=res[[1]]
      number_of_k=res[[3]]
      File_genes=AutoPipe::Groups_Sup(me_TOP, me=me_x, number_of_k,TRw=-1)
      groups_men=File_genes[[2]]
      me_x=File_genes[[1]]
     if(input$show_clin){
       print((samples_data))
       o_g<-AutoPipe::Supervised_Cluster_Heatmap(groups_men = groups_men, gene_matrix=me_x,
                                                 method="PAMR",show_sil=input$show_sil,print_genes=input$print_genes,genes_to_print = input$genes_to_print,TOP_Cluster = input$TOP_Cluster,
                                                 topPaths = input$topPaths,threshold = input$threshold,samples_data = samples_data,
                                                 TOP = input$TOP,GSE=input$GSE,plot_mean_sil=input$plot_mean_sil,sil_mean=res[[2]],db = input$db)
     }else{
       o_g<-AutoPipe::Supervised_Cluster_Heatmap(groups_men = groups_men, gene_matrix=me_x,
                                                 method="PAMR",show_sil=input$show_sil,print_genes=input$print_genes,genes_to_print = input$genes_to_print,TOP_Cluster = input$TOP_Cluster,
                                                 topPaths = input$topPaths,threshold = input$threshold,
                                                 TOP = input$TOP,GSE=input$GSE,plot_mean_sil=input$plot_mean_sil,sil_mean=res[[2]],db = input$db)
     }
    }
    else {
      return(df)
    }
    
  })
  
}