### RandomForest contents
output$ui_RF<-renderUI({

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
  output$enfa_var_RF<-renderPlot({
    glc <- glc()
    mod.enfa <- mod.enfa()
    CENFA::scatter(x = mod.enfa,y = glc,n=nlayers(data$Env),p=1)
  })

  observeEvent(input$RandomForest,{
    validate(
      need(length(input$var_expl_RF) > 0, 'Choose specie predictors first !')
    )

    data$enfa<-raster::subset(data$Env,input$var_expl_RF)
    pa_data<-reactive({
      pa_data<-sf::st_as_sf(Specdata(), coords = c("lon","lat"), crs = crs(data$enfa))
      pa_data
    })
    # extract the raster values for the species points as a dataframe
    mydataF<-reactive({
      mydataF <- raster::extract(data$enfa, pa_data(), df = TRUE)
      mydataF <- mydataF[,-1]
      Specdata<-Specdata()
      spec<-Specdata[,load.occ$spec_select]
      mydataF<-cbind(mydataF,spec)
      mydataF<-mydataF %>% dplyr::rename(Species=spec)
      mydataF
    })
    mydataF<-mydataF()
    set.seed(1994)
    fold<-dismo::kfold(Specdata(),input$number_no_block_fold_RF)
    #fold<-kfold()
    model<-list()
    evaluate_model<-list()
    for (i in 1:input$number_no_block_fold_RF) {

      testpres <- mydataF[mydataF[fold == i,ncol(mydataF)] == 1, 1:(ncol(mydataF)-1)]
      testbackg <- mydataF[mydataF[fold == i,ncol(mydataF)] == 0, 1:(ncol(mydataF)-1)]

      model[[i]] <- randomForest(as.numeric(Species)~., mydataF[fold != i, ], na.action=na.omit,importance=TRUE)
      evaluate_model[[i]] <- dismo::evaluate(testpres, testbackg, model[[i]])

    }
    model_pred<-list()
    auc <- sapply(evaluate_model, function(x){x@auc})
    model_pred[["espece"]]<-predict(data$enfa, model[[which.max(auc)]])
    model_pred[["AUC"]]<-auc[which.max(auc)]
    model_pred[["threshold"]]<- threshold(evaluate_model[[which.max(auc)]], 'spec_sens')
    model_pred[["PresenceAbsence"]]<-model_pred[["espece"]]>model_pred[["threshold"]]
    model_pred[["ProbaPresence"]]<-sdmApp::sdmApp_TimesRasters(model_pred[["espece"]],model_pred[["PresenceAbsence"]])
    observeEvent(input$probaplot_RF,{
      if(input$probaplot_RF=='Occurence map'){
        title_probaplot_RF<-'Occurence map'
        map<-model_pred[["espece"]]}
      if(input$probaplot_RF=='Occurence map (Presence/Absence)'){
        title_probaplot_RF<-'Occurence map (Presence/Absence)'
        map<-model_pred[["PresenceAbsence"]]

      }
      if(input$probaplot_RF=='Occurence map (Presence)'){
        title_probaplot_RF<-'Occurence map (Presence)'
        map<-model_pred[["ProbaPresence"]]
      }
      output$proba_occ_RF<-renderPlot({
        if(title_probaplot_RF=='Occurence map (Presence/Absence)'){sdmApp::sdmApp_PA(map)}
        else{
          sdmApp_RasterPlot(map)
        }

      })
    })
    observeEvent(input$model_ev_RF,{
      if(input$model_ev_RF == 'ROC') {ev_RF<-'ROC'}
      if(input$model_ev_RF == 'density') {ev_RF<-'density'}
      if(input$model_ev_RF == 'boxplot') {ev_RF<-'boxplot'}
      if(input$model_ev_RF == 'kappa') {ev_RF<-'kappa'}
      if(input$model_ev_RF == 'FPR') {ev_RF<-'FPR'}
      if(input$model_ev_RF == 'prevalence') {ev_RF<-'prevalence'}
      output$eval_RF<-renderPlot({
        if(ev_RF=='density'){density(evaluate_model[[which.max(auc)]])}
        else{
          if(ev_RF=='boxplot'){boxplot(evaluate_model[[which.max(auc)]], col=c('red', 'green'),xlab=load.occ$spec_select)}
          else{
            plot(evaluate_model[[which.max(auc)]],ev_RF)
          }
        }


      })
    })
    observeEvent(input$response_var_RF,{
      output$response_eco_RF<-renderPlot({
        dismo::response(model[[which.max(auc)]],var=input$response_var_RF,main=load.occ$spec_select)
      })
    })
    output$var_importance_RF<-renderPlot({
      #plot(model[[which.max(auc)]], main=load.occ$spec_select,xlab="Purcentage(%)")
      randomForest::varImpPlot(model[[which.max(auc)]],main=load.occ$spec_select)
    })
  })




  out <- NULL
  #txt_setup<-'The Maxent software is based on the maximum-entropy approach for modeling species niches and distributions. From a set of environmental (e.g., climatic) grids and georeferenced occurrence localities (e.g. mediated by GBIF), the model expresses a probability distribution where each grid cell has a predicted suitability of conditions for the species. Maxent is a stand-alone Java application and can be used on any computer running Java version 1.5 or later.'
  out <- fluidRow(
    column(width = 12, offset = 0, h3("Random Forest"), class="wb-header"),
    column(width = 12, offset = 0, p("After choosing your spatial blocking approach  please choose the species predictors according to ENFA or own selection, and then the Random Forest model."), class="wb-header-hint")
    #fluidRow(column(12, h4("Read Me", tipify(icon("info-circle"), title=txt_setup, placement="bottom"), class="wb-block-title"), align="center"))
  )
  out<-list(out,
            sidebarPanel(
              selectInput("choice_block_RF", "Please Choose your model technic (without spatial blocking or with spatial blocking)",
                          c(without="Modelling without spatial blocking",with="Modelling with spatial blocking")
              ),
              conditionalPanel(
                condition = "input.choice_block_RF == 'Modelling without spatial blocking'",
                sliderInput("number_no_block_fold_RF", "Please set the number of fold", min = 1, max = 100, value = 5)
              )),
            conditionalPanel(
              condition = "input.choice_block_RF == 'Modelling without spatial blocking'",
              mainPanel(width = 6, tabsetPanel(type = "tabs",
                                               tabPanel("Specie predictors",
                                                        selectInput('var_expl_RF', 'Please select the specie predictors', names(data$Env), multiple = TRUE, selectize = TRUE),
                                                        myActionButton("RandomForest",label=("Apply Random Forest"), "primary"),
                                                        plotOutput("enfa_var_RF")
                                               ),
                                               tabPanel("Map",
                                                        selectInput('probaplot_RF', '', c("Occurence map","Occurence map (Presence/Absence)","Occurence map (Presence)"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_RF", label = "Select the file type to export", choices = list("png", "pdf","tif"),inline = TRUE),
                                                        downloadButton('download_RF', 'Download'),
                                                        plotOutput("proba_occ_RF")

                                               ),
                                               tabPanel("Model Evaluation",
                                                        selectInput('model_ev_RF', 'Please select the metric to evaluate the model', c("ROC","density","boxplot","kappa","FPR","prevalence"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_model_ev_RF", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_model_ev_RF', 'Download'),
                                                        plotOutput("eval_RF")
                                               ),
                                               # tabPanel("Variable response",
                                               #          selectInput('response_var_RF', 'Please select the variable to get its ecological response', names(data$enfa), multiple = FALSE, selectize = TRUE),
                                               #          radioButtons(inputId = "plot_type_response_var_RF", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                               #          downloadButton('download_response_var_RF', 'Download'),
                                               #          plotOutput("response_eco_RF")
                                               # ),
                                               tabPanel("Variable Importance",
                                                        radioButtons(inputId = "plot_type_var_importance_RF", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_var_importance_RF', 'Download'),
                                                        plotOutput("var_importance_RF")
                                               )


              ),
              id = "tabs"))
  )
  out

})
### end RandomForest
