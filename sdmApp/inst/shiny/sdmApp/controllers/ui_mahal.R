output$ui_mahal<-renderUI({

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
  output$enfa_var_mahal<-renderPlot({
    glc <- glc()
    mod.enfa <- mod.enfa()
    CENFA::scatter(x = mod.enfa,y = glc,n=nlayers(data$Env),p=1)
  })

  observeEvent(input$Mahal,{
    validate(
      need(length(input$var_expl_Mahal) > 0, 'Choose specie predictors first !')
    )

    data$enfa<-raster::subset(data$Env,input$var_expl_Mahal)
    Specdata<-Specdata()
    set.seed(1994)
    fold<-dismo::kfold(Specdata,input$number_no_block_fold_Mahal)
    #fold<-kfold()
    model<-list()
    evaluate_model<-list()
    for (i in 1:input$number_no_block_fold_Mahal) {

      # testpres <- mydataF[mydataF[fold == i,ncol(mydataF)] == 1, 1:(ncol(mydataF)-1)]
      # testbackg <- mydataF[mydataF[fold == i,ncol(mydataF)] == 0, 1:(ncol(mydataF)-1)]
      #dsf<-dsf[dsf[,ncol(dsf)] == 1,]

      p<-Specdata[Specdata[fold != i,ncol(Specdata)] == 1, 1:(ncol(Specdata)-1)]
      a<-Specdata[Specdata[fold != i,ncol(Specdata)] == 0, 1:(ncol(Specdata)-1)]
      #test<-Specdata[fold == i, ]

      occtest<-Specdata[Specdata[fold == i,ncol(Specdata)] == 1, 1:(ncol(Specdata)-1)]

      bgtest<-Specdata[Specdata[fold == i,ncol(Specdata)] == 0, 1:(ncol(Specdata)-1)]
      model[[i]] <- dismo::mahal(data$enfa, p) #, factors='Sol'
      evaluate_model[[i]] <- dismo::evaluate(occtest, bgtest, model[[i]], data$enfa)

    }
    model_pred<-list()
    auc <- sapply(evaluate_model, function(x){x@auc})
    model_pred[["espece"]]<-predict(data$enfa, model[[which.max(auc)]])
    model_pred[["AUC"]]<-auc[which.max(auc)]
    model_pred[["threshold"]]<- threshold(evaluate_model[[which.max(auc)]], 'spec_sens')
    model_pred[["PresenceAbsence"]]<-model_pred[["espece"]]>model_pred[["threshold"]]
    model_pred[["ProbaPresence"]]<-sdmApp::sdmApp_TimesRasters(model_pred[["espece"]],model_pred[["PresenceAbsence"]])
    observeEvent(input$probaplot_Mahal,{
      if(input$probaplot_Mahal=='Occurence map'){
        title_probaplot_Mahal<-'Occurence map'
        map<-model_pred[["espece"]]}
      if(input$probaplot_Mahal=='Occurence map (Presence/Absence)'){
        title_probaplot_Mahal<-'Occurence map (Presence/Absence)'
        map<-model_pred[["PresenceAbsence"]]

      }
      if(input$probaplot_Mahal=='Occurence map (Presence)'){
        title_probaplot_Mahal<-'Occurence map (Presence)'
        map<-model_pred[["ProbaPresence"]]
      }
      output$proba_occ_Mahal<-renderPlot({
        if(title_probaplot_Mahal=='Occurence map (Presence/Absence)'){sdmApp_PA(map)}
        else{
          sdmApp_RasterPlot(map)
        }

      })
    })
    observeEvent(input$model_ev_Mahal,{
      if(input$model_ev_Mahal == 'ROC') {ev<-'ROC'}
      if(input$model_ev_Mahal == 'density') {ev<-'density'}
      if(input$model_ev_Mahal == 'boxplot') {ev<-'boxplot'}
      if(input$model_ev_Mahal == 'kappa') {ev<-'kappa'}
      if(input$model_ev_Mahal == 'FPR') {ev<-'FPR'}
      if(input$model_ev_Mahal == 'prevalence') {ev<-'prevalence'}
      output$eval_Mahal<-renderPlot({
        if(ev=='density'){density(evaluate_model[[which.max(auc)]])}
        else{
          if(ev=='boxplot'){boxplot(evaluate_model[[which.max(auc)]], col=c('red', 'green'),xlab=load.occ$spec_select)}
          else{
            plot(evaluate_model[[which.max(auc)]],ev)
          }
        }


      })
    })
    observeEvent(input$response_var_Mahal,{
      output$response_eco_Mahal<-renderPlot({
        dismo::response(model[[which.max(auc)]],var=input$response_var_Mahal,main=load.occ$spec_select)
      })
    })
    # output$var_importance_Mahal<-renderPlot({
    #   plot(model[[which.max(auc)]], main=load.occ$spec_select,xlab="Purcentage(%)")
    # })
  })




  out <- NULL
  #txt_setup<-'The Mahal software is based on the maximum-entropy approach for modeling species niches and distributions. From a set of environmental (e.g., climatic) grids and georeferenced occurrence localities (e.g. mediated by GBIF), the model expresses a probability distribution where each grid cell has a predicted suitability of conditions for the species. Mahal is a stand-alone Java application and can be used on any computer running Java version 1.5 or later.'
  out <- fluidRow(
    column(width = 12, offset = 0, h3("Mahal"), class="wb-header"),
    column(width = 12, offset = 0, p("After choosing your spatial blocking approach  please choose the species predictors according to ENFA or own selection, and then the mahal model."), class="wb-header-hint")
    #fluidRow(column(12, h4("Read Me", tipify(icon("info-circle"), title=txt_setup, placement="bottom"), class="wb-block-title"), align="center"))
  )
  out<-list(out,
            sidebarPanel(
              selectInput("choice_block_Mahal", "Please Choose your model technic (without spatial blocking or with spatial blocking)",
                          c(without="Modelling without spatial blocking",with="Modelling with spatial blocking")
              ),
              conditionalPanel(
                condition = "input.choice_block_Mahal == 'Modelling without spatial blocking'",
                sliderInput("number_no_block_fold_Mahal", "Please set the number of fold", min = 1, max = 100, value = 5)
              )),
            conditionalPanel(
              condition = "input.choice_block_Mahal == 'Modelling without spatial blocking'",
              mainPanel(width = 6, tabsetPanel(type = "tabs",
                                               tabPanel("Specie predictors",
                                                        selectInput('var_expl_Mahal', 'Please select the specie predictors', names(data$Env), multiple = TRUE, selectize = TRUE),
                                                        myActionButton("Mahal",label=("Apply Mahal"), "primary"),
                                                        plotOutput("enfa_var_mahal")
                                               ),
                                               tabPanel("Map",
                                                        selectInput('probaplot_Mahal', '', c("Occurence map","Occurence map (Presence/Absence)","Occurence map (Presence)"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_Mahal", label = "Select the file type to export", choices = list("png", "pdf","tif"),inline = TRUE),
                                                        downloadButton('download_Mahal', 'Download'),
                                                        plotOutput("proba_occ_Mahal")

                                               ),
                                               tabPanel("Model Evaluation",
                                                        selectInput('model_ev_Mahal', 'Please select the metric to evaluate the model', c("ROC","density","boxplot","kappa","FPR","prevalence"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_model_ev_Mahal", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_model_ev_Mahal', 'Download'),
                                                        plotOutput("eval_Mahal")
                                               ),
                                               tabPanel("Variable response",
                                                        selectInput('response_var_Mahal', 'Please select the variable to get its ecological response', names(data$enfa), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_response_var_Mahal", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_response_var_Mahal', 'Download'),
                                                        plotOutput("response_eco_Mahal")
                                               )
                                               # ,
                                               # tabPanel("Variable Importance",
                                               #          plotOutput("var_importance_Mahal")
                                               # )


              ),
              id = "tabs"))
  )
  out
  # out <- list(out,
  #             column(12,offset=0,sidebarPanel(
  #               selectInput("choice_block", "Please Choose your model technic (without spatial blocking or with spatial blocking)",
  #                           c(without="Modelling without spatial blocking",with="Modelling with spatial blocking")
  #               ),
  #               conditionalPanel(
  #                 condition = "input.choice_block == 'Modelling without spatial blocking'",
  #                 sliderInput("number_no_block_fold", "Please set the number of fold", min = 1, max = 100, value = 5)
  #               )),align="center"))
  # out <- list(out,
  #             fluidRow(
  #               box(title = "Specie predictors",
  #                   selectInput('var_expl', 'Please select the specie predictors', names(data$Env), multiple = TRUE, selectize = TRUE)),
  #               box(title = "Ecological Niche Factor Analysis",
  #                   plotOutput("enfa_var"))))
  #
  # out <- list(out,
  #             fluidRow(actionButton('Mahal', 'Apply')))
  # out <- list(out,
  #             fluidRow(
  #               box(title='Probability of occurence',
  #                   selectInput('probaplot_Mahal', '', c("Probability of occurence(absence/presence)","Presence/Absence","Probability of occurence(presence)"), multiple = FALSE, selectize = TRUE),
  #                   plotOutput("proba_occ_Mahal")),
  #               box(title = "Model Evaluation",
  #                   selectInput('model_ev_Mahal', 'Please select the metric to evaluate the model', c("ROC","density","boxplot","kappa","FPR","prevalence"), multiple = FALSE, selectize = TRUE),
  #                   plotOutput("eval")),
  #               box(title = 'Variable response',
  #                   selectInput('response_var_Mahal', 'Please select the variable to get its ecological response', names(data$enfa), multiple = FALSE, selectize = TRUE),
  #                   plotOutput("response_eco_Mahal")),
  #               box(title = 'Variable importance',
  #                   plotOutput("var_importance_Mahal"))))
  # out

})
