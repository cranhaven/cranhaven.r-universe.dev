#### domain contents ###

output$ui_domain<-renderUI({

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
  output$enfa_var_domain<-renderPlot({
    glc <- glc()
    mod.enfa <- mod.enfa()
    CENFA::scatter(x = mod.enfa,y = glc,n=nlayers(data$Env),p=1)
  })

  observeEvent(input$Domain,{
    validate(
      need(length(input$var_expl_Domain) > 0, 'Choose specie predictors first !')
    )

    data$enfa<-raster::subset(data$Env,input$var_expl_Domain)
    Specdata<-Specdata()
    set.seed(1994)
    fold<-dismo::kfold(Specdata,input$number_no_block_fold_Domain)
    #fold<-kfold()
    model<-list()
    evaluate_model<-list()
    for (i in 1:input$number_no_block_fold_Domain) {
      p<-Specdata[Specdata[fold != i,ncol(Specdata)] == 1, 1:(ncol(Specdata)-1)]
      a<-Specdata[Specdata[fold != i,ncol(Specdata)] == 0, 1:(ncol(Specdata)-1)]
      occtest<-Specdata[Specdata[fold == i,ncol(Specdata)] == 1, 1:(ncol(Specdata)-1)]

      bgtest<-Specdata[Specdata[fold == i,ncol(Specdata)] == 0, 1:(ncol(Specdata)-1)]
      model[[i]] <- dismo::domain(data$enfa, p) #, factors='Sol'
      evaluate_model[[i]] <- dismo::evaluate(occtest, bgtest, model[[i]], data$enfa)

    }
    model_pred<-list()
    auc <- sapply(evaluate_model, function(x){x@auc})
    model_pred[["espece"]]<-predict(data$enfa, model[[which.max(auc)]])
    model_pred[["AUC"]]<-auc[which.max(auc)]
    model_pred[["threshold"]]<- threshold(evaluate_model[[which.max(auc)]], 'spec_sens')
    model_pred[["PresenceAbsence"]]<-model_pred[["espece"]]>model_pred[["threshold"]]
    model_pred[["ProbaPresence"]]<-sdmApp::sdmApp_TimesRasters(model_pred[["espece"]],model_pred[["PresenceAbsence"]])

    observeEvent(input$probaplot_Domain,{
      if(input$probaplot_Domain=='Occurence map'){
        title_probaplot_Domain<-'Occurence map'
        map<-model_pred[["espece"]]
        load.occ$Domain <- map}
      if(input$probaplot_Domain=='Occurence map (Presence/Absence)'){
        title_probaplot_Domain<-'Occurence map (Presence/Absence)'
        map<-model_pred[["PresenceAbsence"]]
        load.occ$Domain <- map
      }
      if(input$probaplot_Domain=='Occurence map (Presence)'){
        title_probaplot_Domain<-'Occurence map (Presence)'
        map<-model_pred[["ProbaPresence"]]
        load.occ$Domain <- map
      }
      output$proba_occ_Domain<-renderPlot({
        if(title_probaplot_Domain=='Occurence map (Presence/Absence)'){sdmApp_PA(map)}
        else{
          sdmApp_RasterPlot(map)
        }

      })
    })
    observeEvent(input$model_ev_Domain,{
      if(input$model_ev_Domain == 'ROC') {ev<-'ROC'}
      if(input$model_ev_Domain == 'density') {ev<-'density'}
      if(input$model_ev_Domain == 'boxplot') {ev<-'boxplot'}
      if(input$model_ev_Domain == 'kappa') {ev<-'kappa'}
      if(input$model_ev_Domain == 'FPR') {ev<-'FPR'}
      if(input$model_ev_Domain == 'prevalence') {ev<-'prevalence'}
      output$eval_Domain<-renderPlot({
        if(ev=='density'){density(evaluate_model[[which.max(auc)]])}
        else{
          if(ev=='boxplot'){boxplot(evaluate_model[[which.max(auc)]], col=c('red', 'green'),xlab=load.occ$spec_select)}
          else{
            plot(evaluate_model[[which.max(auc)]],ev)
          }
        }


      })
    })
    observeEvent(input$response_var_Domain,{
      output$response_eco_Domain<-renderPlot({
        dismo::response(model[[which.max(auc)]],var=input$response_var_Domain,main=load.occ$spec_select)
      })
    })
    # output$var_importance_Domain<-renderPlot({
    #   plot(model[[which.max(auc)]], main=load.occ$spec_select,xlab="Purcentage(%)")
    # })
  })


  out <- NULL
  #txt_setup<-'The Domain algorithm computes the Gower distance between environmental variables at any location and those at any of the known locations of occurrence (training sites).'
  out <- fluidRow(
    column(width = 12, offset = 0, h3("Domain"), class="wb-header"),
    column(width = 12, offset = 0, p("After choosing your spatial blocking approach  please choose the species predictors according to ENFA or own selection, and then the domain model."), class="wb-header-hint")
    #fluidRow(column(12, h4("Read Me", tipify(icon("info-circle"), title=txt_setup, placement="bottom"), class="wb-block-title"), align="center"))
  )
  out<-list(out,
            sidebarPanel(
              selectInput("choice_block_Domain", "Please Choose your model technic (without spatial blocking or with spatial blocking)",
                          c(without="Modelling without spatial blocking",with="Modelling with spatial blocking")
              ),
              conditionalPanel(
                condition = "input.choice_block_Domain == 'Modelling without spatial blocking'",
                sliderInput("number_no_block_fold_Domain", "Please set the number of fold", min = 1, max = 100, value = 5)
              )),
            conditionalPanel(
              condition = "input.choice_block_Domain == 'Modelling without spatial blocking'",
              mainPanel(width = 6, tabsetPanel(type = "tabs",
                                               tabPanel("Specie predictors",
                                                        selectInput('var_expl_Domain', 'Please select the specie predictors', names(data$Env), multiple = TRUE, selectize = TRUE),
                                                        myActionButton("Domain",label=("Apply Domain"), "primary"),
                                                        plotOutput("enfa_var_domain")
                                               ),
                                               tabPanel("Map",
                                                        selectInput('probaplot_Domain', '', c("Occurence map","Occurence map (Presence/Absence)","Occurence map (Presence)"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_Domain", label = "Select the file type to export", choices = list("png", "pdf","tif"),inline = TRUE),
                                                        downloadButton('download_Domain', 'Download'),
                                                        plotOutput("proba_occ_Domain")

                                               ),
                                               tabPanel("Model Evaluation",
                                                        selectInput('model_ev_Domain', 'Please select the metric to evaluate the model', c("ROC","density","boxplot","kappa","FPR","prevalence"), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_model_ev_Domain", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_model_ev_Domain', 'Download'),
                                                        plotOutput("eval_Domain")
                                               ),
                                               tabPanel("Variable response",
                                                        selectInput('response_var_Domain', 'Please select the variable to get its ecological response', names(data$enfa), multiple = FALSE, selectize = TRUE),
                                                        radioButtons(inputId = "plot_type_response_var_Domain", label = "Select the file type to export", choices = list("png", "pdf"),inline = TRUE),
                                                        downloadButton('download_response_var_Domain', 'Download'),
                                                        plotOutput("response_eco_Domain")
                                               )
                                               # ,
                                               # tabPanel("Variable Importance",
                                               #          plotOutput("var_importance_Domain")
                                               # )


              ),
              id = "tabs"))
  )
  out
})
#### end domain ###
