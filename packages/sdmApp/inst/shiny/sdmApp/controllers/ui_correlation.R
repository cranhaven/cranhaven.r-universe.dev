###########################################"

####ui correlation

#glc <- GLcenfa(x = ENFA_var)



#load.occ$spec_select<-input$Pcol
#coor$pa_dataF <- sf::st_as_sf(coor$Specdata, coords = c("lon","lat"), crs = crs(data$Env))
#coor$Cor <- raster::extract(data$Env, coor$pa_dataF, df = TRUE)
#coor$Cor<-coor$Cor[,-1]
# Specdata<-reactive({
#   dsf<-load.occ$select
#   dsf<-dsf %>% dplyr::rename(lon=load.occ$lon,lat=load.occ$lat)
#   dsf[,1]<-as.numeric(unlist(dsf[,1]))
#   dsf[,2]<-as.numeric(unlist(dsf[,2]))
#   dsf[,3]<-as.numeric(unlist(dsf[,3]))
#   dsf
# })
# correlation matrix
# Z<-reactive({
#   CENFA::parScale(data$Env)
# })
#
#
# # Efficient calculation of covariance matrices for Raster* objects
# mat<-reactive({
#   CENFA::parCov(Z())
# })
#
# pa_data<-reactive({
#   sf::st_as_sf(Specdata(), coords = c("lon","lat"), crs = crs(data$Env))
#
# })
# Cor<-reactive({
#   Corr<-raster::extract(data$Env, pa_data(), df = TRUE)
#   Corr<-Corr[,-1]
#   Corr
# })
#
# p.mat <-reactive({
#   p_mat<-ggcorrplot::cor_pmat(Cor())
#   p_mat
# })
output$ui_correlation <- renderUI({


  output$coor_mat <- DT::renderDataTable({
    datatable(mat(),
              rownames = TRUE,
              selection="none",
              options = list(scrollX=TRUE, scrollY=250, lengthMenu=list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), pageLength=20)

    )})
  output$coor_plot <- renderPlot({
    a = try(withProgress(message = 'Correlation plot...',
    ggcorrplot::ggcorrplot(mat(),ggtheme = ggplot2::theme_gray,
                           hc.order = TRUE,
                           type = "lower",
                           p.mat = p.mat(),
                           colors = c("#6D9EC1", "white", "#E46726"))))
    if(inherits(a, 'try-error')){
      output$Envbug_coor_plot <- renderUI(p('Can not plot correlation plot!Please verify your inputs and try again!'))
    }
    else{
      output$Envbug_coor_plot <- renderUI(p())
      a
      }
  })
  fluidRow(column(12, h4("Correlation between environmental variables"), align="center"),
           mainPanel(width = 8, tabsetPanel(type = "tabs",
                                            tabPanel("Correlation matrix",
                                                     downloadButton('download_cor_mat', 'Download'),
                                                     DT::dataTableOutput("coor_mat")
                                            ),
                                            tabPanel("Correlation Plot",
                                                     radioButtons(inputId = "plot_type_cor", label = "Select the file type", choices = list("png", "pdf"),inline = TRUE),
                                                     downloadButton('download_cor_plot', 'Download'),
                                                     plotOutput("coor_plot")
                                            )


           ),
           id = "tabs")
  )
})
############ end ui correlation
