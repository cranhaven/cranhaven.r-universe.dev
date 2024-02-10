##### Maxent content
output$ui_MaxEnt<-renderUI({

  # Specdata<-reactive({
  #   dsf<-load.occ$select
  #   dsf<-dsf %>% dplyr::rename(lon=load.occ$lon,lat=load.occ$lat)
  #   dsf
  # })
  #
  # Specdata_Presence<-reactive({
  #   dsf<-Specdata()
  #   dsf<-dsf[dsf[,ncol(dsf)] == 1,]
  #   sp::coordinates(dsf) <-~lon+lat
  #   sp::proj4string(dsf) <-raster::crs(data$Env)
  #   dsf
  # })
  #
  # glc<-reactive({
  #   GLcenfa(x = data$Env)
  # })
  #
  # mod.enfa<-reactive({
  #   pr<-Specdata_Presence()
  #   pr@data$load.occ$spec_select<-as.numeric(pr@data$load.occ$spec_select)
  #   CENFA::enfa(x = data$Env, s.dat = pr, field = load.occ$spec_select)
  # })
  # enfa_plot<-reactive({
  #   glc <- glc()
  #
  #   mod.enfa <- mod.enfa()
  #   CENFA::scatter(x = mod.enfa, y = glc,n=nlayers(data$Env),p=1)
  # })
  output$enfa_var_Maxent<-renderPlot({
    glc <- glc()
    mod.enfa <- mod.enfa()
    CENFA::scatter(x = mod.enfa,y = glc,n=nlayers(data$Env),p=1)
  })

  observeEvent(input$MaxEnt,{
    validate(
      need(length(input$var_expl) > 0, 'Choose specie predictors first !')
    )

    data$enfa<-raster::subset(data$Env,input$var_expl)
    Specdata<-Specdata()
    set.seed(1994)
    fold<-dismo::kfold(Specdata,input$number_no_block_fold)
    #fold<-kfold()
    model<-list()
    evaluate_model<-list()
    for (i in 1:input$number_no_block_fold) {
      p<-Specdata[Specdata[fold != i,ncol(Specdata)] == 1, 1:(ncol(Specdata)-1)]
      a<-Specdata[Specdata[fold != i,ncol(Specdata)] == 0, 1:(ncol(Specdata)-1)]
      #test<-Specdata[fold == i, ]

      occtest<-Specdata[Specdata[fold == i,ncol(Specdata)] == 1, 1:(ncol(Specdata)-1)]

      bgtest<-Specdata[Specdata[fold == i,ncol(Specdata)] == 0, 1:(ncol(Specdata)-1)]
      model[[i]] <- dismo::maxent(data$enfa, p, a) #, factors='Sol'
      evaluate_model[[i]] <- dismo::evaluate(occtest, bgtest, model[[i]], data$enfa)

    }
    model_pred<-list()
    auc <- sapply(evaluate_model, function(x){x@auc})
    model_pred[["espece"]]<-predict(data$enfa, model[[which.max(auc)]])
    model_pred[["AUC"]]<-auc[which.max(auc)]
    model_pred[["threshold"]]<- threshold(evaluate_model[[which.max(auc)]], 'spec_sens')
    model_pred[["PresenceAbsence"]]<-model_pred[["espece"]]>model_pred[["threshold"]]
    model_pred[["ProbaPresence"]]<-sdmApp::sdmApp_TimesRasters(model_pred[["espece"]],model_pred[["PresenceAbsence"]])
    observeEvent(input$probaplot,{
      if(input$probaplot=='Occurence map'){
        title_probaplot<-'Occurence map'
        map<-model_pred[["espece"]]}
      if(input$probaplot=='Occurence map (Presence/Absence)'){
        title_probaplot<-'Occurence map (Presence/Absence)'
        map<-model_pred[["PresenceAbsence"]]

      }
      if(input$probaplot=='Occurence map (Presence)'){
        title_probaplot<-'Occurence map (Presence)'
        map<-model_pred[["ProbaPresence"]]
      }
      output$proba_occ<-renderPlot({
        if(title_probaplot=='Occurence map (Presence/Absence)'){sdmApp::sdmApp_PA(map)}
        else{
          sdmApp::sdmApp_RasterPlot(map)
        }

      })
    })
    observeEvent(input$model_ev,{
      if(input$model_ev == 'ROC') {ev<-'ROC'}
      if(input$model_ev == 'density') {ev<-'density'}
      if(input$model_ev == 'boxplot') {ev<-'boxplot'}
      if(input$model_ev == 'kappa') {ev<-'kappa'}
      if(input$model_ev == 'FPR') {ev<-'FPR'}
      if(input$model_ev == 'prevalence') {ev<-'prevalence'}
      output$eval<-renderPlot({
        if(ev=='density'){density(evaluate_model[[which.max(auc)]])}
        else{
          if(ev=='boxplot'){boxplot(evaluate_model[[which.max(auc)]], col=c('red', 'green'),xlab=load.occ$spec_select)}
          else{
            plot(evaluate_model[[which.max(auc)]],ev)
          }
        }


      })
    })
    observeEvent(input$response_var,{
      output$response_eco<-renderPlot({
        dismo::response(model[[which.max(auc)]],var=input$response_var,main=load.occ$spec_select)
      })
    })
    output$var_importance<-renderPlot({
      plot(model[[which.max(auc)]], main=load.occ$spec_select,xlab="Purcentage(%)")
    })
  })




  out <- NULL
  #txt_setup<-'The Maxent software is based on the maximum-entropy approach for modeling species niches and distributions. From a set of environmental (e.g., climatic) grids and georeferenced occurrence localities (e.g. mediated by GBIF), the model expresses a probability distribution where each grid cell has a predicted suitability of conditions for the species. Maxent is a stand-alone Java application and can be used on any computer running Java version 1.5 or later.'
  out <- fluidRow(
    column(width = 12, offset = 0, h3("MaxEnt"), class="wb-header"),
    column(width = 12, offset = 0, p("After choosing your spatial blocking approach  please choose the species predictors according to ENFA or own selection, and then the MaxEnt model."), class="wb-header-hint")
    #fluidRow(column(12, h4("Read Me", tipify(icon("info-circle"), title=txt_setup, placement="bottom"), class="wb-block-title"), align="center"))
  )

  out<-list(out,
            sidebarPanel(
              selectInput("choice_block", "Please Choose your model technic (without spatial blocking or with spatial blocking)",
                          c(without="Modelling without spatial blocking",with="Modelling with spatial blocking")
              ),
              conditionalPanel(
                condition = "input.choice_block == 'Modelling without spatial blocking'",
                sliderInput("number_no_block_fold", "Please set the number of fold", min = 1, max = 100, value = 5)
              )),
            conditionalPanel(
              condition = "input.choice_block == 'Modelling without spatial blocking'",
              mainPanel(width = 6, tabsetPanel(type = "tabs",
                                               tabPanel("Specie predictors",
                                                        selectInput('var_expl', 'Please select the specie predictors', names(data$Env), multiple = TRUE, selectize = TRUE),
                                                        myActionButton("MaxEnt",label=("Apply MaxEnt"), "primary"),
                                                        plotOutput("enfa_var_Maxent")
                                               ),
                                               tabPanel("Map",
                                                        selectInput('probaplot', '', c("Occurence map","Occurence map (Presence/Absence)","Occurence map (Presence)"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_MaxEnt", label = "Select the file type to export", choices = list("png", "pdf","tif"),inline = TRUE),
                                                        downloadButton('download_MaxEnt', 'Download'),
                                                        plotOutput("proba_occ")

                                               ),
                                               tabPanel("Model Evaluation",
                                                        selectInput('model_ev', 'Please select the metric to evaluate the model', c("ROC","density","boxplot","kappa","FPR","prevalence"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_model_ev_MaxEnt", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_model_ev_MaxEnt', 'Download'),
                                                        plotOutput("eval")
                                               ),
                                               tabPanel("Variable response",
                                                        selectInput('response_var', 'Please select the variable to get its ecological response', names(data$enfa), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_response_var_MaxEnt", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_response_var_MaxEnt', 'Download'),
                                                        plotOutput("response_eco")
                                               ),
                                               tabPanel("Variable Importance",
                                                        radioButtons(inputId = "plot_type_var_importance_MaxEnt", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_var_importance_MaxEnt', 'Download'),
                                                        plotOutput("var_importance")
                                               )


              ),
              id = "tabs"))
  )
  out

})
###########"end Maxent ######
