#### bioclim contents

output$ui_bioclim<-renderUI({
  output$enfa_var_bioclim<-renderPlot({
    glc <- glc()
    mod.enfa <- mod.enfa()
    CENFA::scatter(x = mod.enfa,y = glc,n=nlayers(data$Env),p=1)
  })

  observeEvent(input$Bioclim,{
    validate(
      need(length(input$var_expl_Bioclim) > 0, 'Choose specie predictors first !')
    )

    data$enfa<-raster::subset(data$Env,input$var_expl_Bioclim)
    Specdata<-Specdata()
    set.seed(1994)
    fold<-dismo::kfold(Specdata,input$number_no_block_fold_bioclim)
    #fold<-kfold()
    model<-list()
    evaluate_model<-list()
    for (i in 1:input$number_no_block_fold_bioclim) {
      p<-Specdata[Specdata[fold != i,ncol(Specdata)] == 1, 1:(ncol(Specdata)-1)]
      a<-Specdata[Specdata[fold != i,ncol(Specdata)] == 0, 1:(ncol(Specdata)-1)]
      #test<-Specdata[fold == i, ]

      occtest<-Specdata[Specdata[fold == i,ncol(Specdata)] == 1, 1:(ncol(Specdata)-1)]

      bgtest<-Specdata[Specdata[fold == i,ncol(Specdata)] == 0, 1:(ncol(Specdata)-1)]
      model[[i]] <- dismo::bioclim(data$enfa, p) #, factors='Sol'
      evaluate_model[[i]] <- dismo::evaluate(occtest, bgtest, model[[i]], data$enfa)

    }
    model_pred<-list()
    auc <- sapply(evaluate_model, function(x){x@auc})
    model_pred[["espece"]]<-predict(data$enfa, model[[which.max(auc)]])
    model_pred[["AUC"]]<-auc[which.max(auc)]
    model_pred[["threshold"]]<- threshold(evaluate_model[[which.max(auc)]], 'spec_sens')
    model_pred[["PresenceAbsence"]]<-model_pred[["espece"]]>model_pred[["threshold"]]
    model_pred[["ProbaPresence"]]<-sdmApp::sdmApp_TimesRasters(model_pred[["espece"]],model_pred[["PresenceAbsence"]])
    observeEvent(input$probaplot_Bioclim,{
      if(input$probaplot_Bioclim=='Occurence map'){
        title_probaplot_Bioclim<-'Occurence map'
        map<-model_pred[["espece"]]
        load.occ$Bioclim <- map }
      if(input$probaplot_Bioclim=='Occurence map (Presence/Absence)'){
        title_probaplot_Bioclim<-'Occurence map (Presence/Absence)'
        map<-model_pred[["PresenceAbsence"]]
        load.occ$Bioclim <- map
      }
      if(input$probaplot_Bioclim=='Occurence map (Presence)'){
        title_probaplot_Bioclim<-'Occurence map (Presence)'
        map<-model_pred[["ProbaPresence"]]
        load.occ$Bioclim <- map
      }
      output$proba_occ_Bioclim<-renderPlot({
        if(title_probaplot_Bioclim=='Occurence map (Presence/Absence)'){
          model$bioclim <- sdmApp::sdmApp_PA(map)
          sdmApp::sdmApp_PA(map)
          }
        else{
          #if(title_probaplot_Bioclim=='Occurence map (Presence)'){sdmApp::sdmApp_RasterPlot(map)}
          model$bioclim <- sdmApp::sdmApp_RasterPlot(map)
          sdmApp::sdmApp_RasterPlot(map)
          }

      })
    })
    observeEvent(input$model_ev_Bioclim,{
      if(input$model_ev_Bioclim == 'ROC') {ev<-'ROC'}
      if(input$model_ev_Bioclim == 'density') {ev<-'density'}
      if(input$model_ev_Bioclim == 'boxplot') {ev<-'boxplot'}
      if(input$model_ev_Bioclim == 'kappa') {ev<-'kappa'}
      if(input$model_ev_Bioclim == 'FPR') {ev<-'FPR'}
      if(input$model_ev_Bioclim == 'prevalence') {ev<-'prevalence'}
      output$eval_Bioclim<-renderPlot({
        if(ev=='density'){
          data$ev<-density(evaluate_model[[which.max(auc)]])
          data$ev}
        else{
          if(ev=='boxplot'){
            data$ev<-boxplot(evaluate_model[[which.max(auc)]], col=c('red', 'green'),xlab=load.occ$spec_select)
            data$ev}
          else{
            data$ev<-plot(evaluate_model[[which.max(auc)]],ev)
            data$ev
          }
        }


      })
    })
    observeEvent(input$response_var_Bioclim,{
      output$response_eco<-renderPlot({
        dismo::response(model[[which.max(auc)]],var=input$response_var_Bioclim,main=load.occ$spec_select)
      })
    })
    # output$var_importance_Bioclim<-renderPlot({
    #   plot(model[[which.max(auc)]], main=load.occ$spec_select,xlab="Purcentage(%)")
    # })
  })

  out <- NULL
  #txt_setup<-'The Bioclim software is based on the maximum-entropy approach for modeling species niches and distributions. From a set of environmental (e.g., climatic) grids and georeferenced occurrence localities (e.g. mediated by GBIF), the model expresses a probability distribution where each grid cell has a predicted suitability of conditions for the species. Bioclim is a stand-alone Java application and can be used on any computer running Java version 1.5 or later.'
  out <- fluidRow(
    column(width = 12, offset = 0, h3("Bioclim"), class="wb-header"),
    column(width = 12, offset = 0, p("After choosing your spatial blocking approach  please choose the species predictors according to ENFA or own selection, and then the bioclim model."), class="wb-header-hint")
    #fluidRow(column(12, h4("Read Me", tipify(icon("info-circle"), title=txt_setup, placement="bottom"), class="wb-block-title"), align="center"))
  )
  out<-list(out,
            sidebarPanel(
              selectInput("choice_block_bioclim", "Please Choose your model technic (without spatial blocking or with spatial blocking)",
                          c(without="Modelling without spatial blocking",with="Modelling with spatial blocking")
              ),
              conditionalPanel(
                condition = "input.choice_block_bioclim == 'Modelling without spatial blocking'",
                sliderInput("number_no_block_fold_bioclim", "Please set the number of fold", min = 1, max = 100, value = 5)
              )),
            conditionalPanel(
              condition = "input.choice_block_bioclim == 'Modelling without spatial blocking'",
              mainPanel(
                width = 6, tabsetPanel(type = "tabs",
                                       tabPanel("Specie predictors",
                                                selectInput('var_expl_Bioclim', 'Please select the specie predictors', names(data$Env), multiple = TRUE, selectize = TRUE),
                                                myActionButton("Bioclim",label=("Apply Bioclim"), "primary"),
                                                plotOutput("enfa_var_bioclim")
                                       ),
                                       tabPanel("Map",
                                                selectInput('probaplot_Bioclim', '', c("Occurence map","Occurence map (Presence/Absence)","Occurence map (Presence)"), multiple = FALSE, selectize = TRUE),
                                                radioButtons(inputId = "plot_type_Bioclim", label = "Select the file type to export", choices = list("png", "pdf","tif"),inline = TRUE),
                                                downloadButton('download_Bioclim', 'Download'),
                                                plotOutput("proba_occ_Bioclim")),
                                       tabPanel("Model Evaluation",
                                                selectInput('model_ev_Bioclim', 'Please select the metric to evaluate the model', c("ROC","density","boxplot","kappa","FPR","prevalence"), multiple = FALSE, selectize = TRUE),
                                                radioButtons(inputId = "plot_type_model_ev_Bioclim", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                downloadButton('download_model_ev_Bioclim', 'Download'),
                                                plotOutput("eval_Bioclim")
                                       ),
                                       tabPanel("Variable response",
                                                selectInput('response_var_Bioclim', 'Please select the variable to get its ecological response', names(data$enfa), multiple = FALSE, selectize = TRUE),
                                                radioButtons(inputId = "plot_type_response_var_Bioclim", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                downloadButton('download_response_var_Bioclim', 'Download'),
                                                plotOutput("response_eco")
                                       )
                ),
                id = "tabs")

            )

  )
  out


})
##### end bioclim
