### GLM contents
output$ui_GLM<-renderUI({

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
  output$enfa_var_GLM<-renderPlot({
    glc <- glc()
    mod.enfa <- mod.enfa()
    CENFA::scatter(x = mod.enfa,y = glc,n=nlayers(data$Env),p=1)
  })

  observeEvent(input$GLM,{
    validate(
      need(length(input$var_expl_GLM) > 0, 'Choose specie predictors first !')
    )

    data$enfa<-raster::subset(data$Env,input$var_expl_GLM)
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
    fold<-dismo::kfold(Specdata(),input$number_no_block_fold_GLM)
    #fold<-kfold()
    model<-list()
    evaluate_model<-list()
    for (i in 1:input$number_no_block_fold_GLM) {

      testpres <- mydataF[mydataF[fold == i,ncol(mydataF)] == 1, 1:(ncol(mydataF)-1)]
      testbackg <- mydataF[mydataF[fold == i,ncol(mydataF)] == 0, 1:(ncol(mydataF)-1)]
      #dsf<-dsf[dsf[,ncol(dsf)] == 1,]


      model[[i]] <- glm(as.factor(Species)~., mydataF[fold != i, ], na.action=na.omit,
                        family = binomial(link = "logit"))
      evaluate_model[[i]] <- dismo::evaluate(testpres, testbackg, model[[i]])

    }
    model_pred<-list()
    auc <- sapply(evaluate_model, function(x){x@auc})
    model_pred[["espece"]]<-predict(data$enfa, model[[which.max(auc)]])
    model_pred[["AUC"]]<-auc[which.max(auc)]
    model_pred[["threshold"]]<- threshold(evaluate_model[[which.max(auc)]], 'spec_sens')
    model_pred[["PresenceAbsence"]]<-model_pred[["espece"]]>model_pred[["threshold"]]
    model_pred[["ProbaPresence"]]<-sdmApp::sdmApp_TimesRasters(model_pred[["espece"]],model_pred[["PresenceAbsence"]])
    observeEvent(input$probaplot_GLM,{
      if(input$probaplot_GLM=='Occurence map'){
        title_probaplot_GLM<-'Occurence map'
        map<-model_pred[["espece"]]}
      if(input$probaplot_GLM=='Occurence map (Presence/Absence)'){
        title_probaplot_GLM<-'Occurence map (Presence/Absence)'
        map<-model_pred[["PresenceAbsence"]]

      }
      if(input$probaplot_GLM=='Occurence map (Presence)'){
        title_probaplot_GLM<-'Occurence map (Presence)'
        map<-model_pred[["ProbaPresence"]]
      }
      output$proba_occ_GLM<-renderPlot({
        if(title_probaplot_GLM=='Occurence map (Presence/Absence)'){sdmApp::sdmApp_PA(map)}
        else{
          sdmApp_RasterPlot(map)
        }

      })

    })
    observeEvent(input$model_ev_GLM,{
      if(input$model_ev_GLM == 'ROC') {ev_GLM<-'ROC'}
      if(input$model_ev_GLM == 'density') {ev_GLM<-'density'}
      if(input$model_ev_GLM == 'boxplot') {ev_GLM<-'boxplot'}
      if(input$model_ev_GLM == 'kappa') {ev_GLM<-'kappa'}
      if(input$model_ev_GLM == 'FPR') {ev_GLM<-'FPR'}
      if(input$model_ev_GLM == 'prevalence') {ev_GLM<-'prevalence'}
      output$eval_GLM<-renderPlot({
        if(ev_GLM=='density'){density(evaluate_model[[which.max(auc)]])}
        else{
          if(ev_GLM=='boxplot'){boxplot(evaluate_model[[which.max(auc)]], col=c('red', 'green'),xlab=load.occ$spec_select)}
          else{
            plot(evaluate_model[[which.max(auc)]],ev_GLM)
          }
        }


      })
    })
    observeEvent(input$response_var_GLM,{
      output$response_eco_GLM<-renderPlot({
        dismo::response(model[[which.max(auc)]])#,var=input$response_var,main=load.occ$spec_select)
      })
    })
    # output$var_importance_GLM<-renderPlot({
    #   plot(model[[which.max(auc)]])#, main=load.occ$spec_select,xlab="Purcentage(%)")
    # })
  })




  out <- NULL
  #txt_setup<-'The Maxent software is based on the maximum-entropy approach for modeling species niches and distributions. From a set of environmental (e.g., climatic) grids and georeferenced occurrence localities (e.g. mediated by GBIF), the model expresses a probability distribution where each grid cell has a predicted suitability of conditions for the species. Maxent is a stand-alone Java application and can be used on any computer running Java version 1.5 or later.'
  out <- fluidRow(
    column(width = 12, offset = 0, h3("GLM"), class="wb-header"),
    column(width = 12, offset = 0, p("After choosing your spatial blocking approach  please choose the species predictors according to ENFA or own selection, and then the GLM model."), class="wb-header-hint")
    #fluidRow(column(12, h4("Read Me", tipify(icon("info-circle"), title=txt_setup, placement="bottom"), class="wb-block-title"), align="center"))
  )

  out<-list(out,
            sidebarPanel(
              selectInput("choice_block_GLM", "Please Choose your model technic (without spatial blocking or with spatial blocking)",
                          c(without="Modelling without spatial blocking",with="Modelling with spatial blocking")
              ),
              conditionalPanel(
                condition = "input.choice_block_GLM == 'Modelling without spatial blocking'",
                sliderInput("number_no_block_fold_GLM", "Please set the number of fold", min = 1, max = 100, value = 5)
              )),
            conditionalPanel(
              condition = "input.choice_block_GLM == 'Modelling without spatial blocking'",
              mainPanel(width = 6, tabsetPanel(type = "tabs",
                                               tabPanel("Specie predictors",
                                                        selectInput('var_expl_GLM', 'Please select the specie predictors', names(data$Env), multiple = TRUE, selectize = TRUE),
                                                        myActionButton("GLM",label=("Apply GLM"), "primary"),
                                                        plotOutput("enfa_var_GLM")
                                               ),
                                               tabPanel("Map",
                                                        selectInput('probaplot_GLM', '', c("Occurence map","Occurence map (Presence/Absence)","Occurence map (Presence)"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_GLM", label = "Select the file type to export", choices = list("png", "pdf","tif"),inline = TRUE),
                                                        downloadButton('download_GLM', 'Download'),
                                                        plotOutput("proba_occ_GLM")

                                               ),
                                               tabPanel("Model Evaluation",
                                                        selectInput('model_ev_GLM', 'Please select the metric to evaluate the model', c("ROC","density","boxplot","kappa","FPR","prevalence"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_model_ev_GLM", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_model_ev_GLM', 'Download'),
                                                        plotOutput("eval_GLM")
                                               ),
                                               tabPanel("Variable response",
                                                        #selectInput('response_var_GLM', 'Please select the variable to get its ecological response', names(data$enfa), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_response_var_GLM", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_response_var_GLM', 'Download'),
                                                        plotOutput("response_eco_GLM")
                                               )
                                               # ,
                                               # tabPanel("Variable Importance",
                                               #          plotOutput("var_importance_GLM")
                                               # )


              ),
              id = "tabs"))
  )
  out

})
### end GLM
