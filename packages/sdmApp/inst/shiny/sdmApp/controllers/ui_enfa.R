
####ui enfa

output$ui_enfa<-renderUI({
  glc <- glc()

  mod.enfa <- mod.enfa()
  if(brStick(s.factor(mod.enfa()))==1){

    output$enfa_scatter<-renderPlot({
      CENFA::scatter(x = mod.enfa,y = glc,n=nlayers(data$Env),p=1)
    })
  }
  else{
    observeEvent(input$number_spec,{

      output$enfa_scatter<-renderPlot({
        CENFA::scatter(x = mod.enfa,yax=as.numeric(input$number_spec),y = glc,n=nlayers(data$Env),p=1)

      })
    })
  }



  # marg_spec<-reactive({
  #   mod.enfa <- mod.enfa()
  #   data.frame(mod.enfa@co)
  # })

  output$marg<- DT::renderDataTable({
    datatable(marg_spec(),
              rownames = TRUE,
              selection="none",
              options = list(scrollX=TRUE, scrollY=250, lengthMenu=list(c(20, 50, 100, -1), c('20', '50', '100', 'All')), pageLength=20)

    )})
  txt_enfa_info<-paste0('The number of significant factors is',code(brStick(s.factor(mod.enfa()))))
  fluidRow(column(12, h4("Ecological Niche Factor Analysis"),p("Ecological-niche factor analysis (ENFA, Hirze; et al., 2002), relies on the concept of the ecological niche. Based on the computation of two parameters, the marginality and the specialization, ENFA provides a measure of the realised niche within the available space."), align="center"),
           mainPanel(width = 8, tabsetPanel(type = "tabs",
                                            tabPanel("ENFA",
                                                     conditionalPanel(
                                                       condition = brStick(s.factor(mod.enfa()))>1,
                                                       p(HTML(txt_enfa_info)),
                                                       selectInput('number_spec', 'Please select a number between 2 and the number of significant factors.', 2:brStick(s.factor(mod.enfa())), multiple = FALSE, selectize = TRUE)
                                                     ),
                                                     downloadButton('download_enfa_scatter','Download'),
                                                     plotOutput("enfa_scatter"))
                                            ,
                                            tabPanel("Marginality and specialization",
                                                     downloadButton('download_Marg_Spec','Download'),
                                                     DT::dataTableOutput("marg")
                                            )),
                     id = "tabs")

  )
})
