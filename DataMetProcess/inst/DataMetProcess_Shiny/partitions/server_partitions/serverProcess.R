

serverProcess <- function(id, Data){
  moduleServer(id,function(input, output, session){
    # Desabilitar abas inicialmente
    js$disableTab('Monthly');js$disableTab("Yearly")

    # UI DINÂMICA ----------------------------------

    # Reactive para gerar opções de seleção baseadas nas colunas do Data
    escolhas2 <- reactive({
      return(colnames(Data())[-1])
    })

    # UI para seleção de sum, mean, max e min diários
    output$diariosum <- renderUI({
      selectInput(NS(id, "diariosum"), "Sum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[c(1, 5)])
    })

    output$diariomean <- renderUI({
      selectInput(NS(id, "diariomean"), "Mean:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[c(2,6,7,14,15,17)])
    })

    output$diariomax <- renderUI({
      selectInput(NS(id, "diariomax"), "Maximum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[c(3,8,10,12,16)])
    })

    output$diariomin <- renderUI({
      selectInput(NS(id, "diariomin"), "Minimum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[c(4,9,11,13)])
    })

    output$mensalmax <- renderUI({
      selectInput(NS(id, "mensalmax"), "Maximum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = NULL)
    })

    output$mensalmin <- renderUI({
      selectInput(NS(id, "mensalmin"), "Minimum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected =NULL)
    })


    # UI para seleção de sum, mean, max e min mensais
    output$mensalsum <- renderUI({
      selectInput(NS(id, "mensalsum"), "Sum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[c(1)])
    })

    output$mensalmean <- renderUI({
      selectInput(NS(id, "mensalmean"), "Mean:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[-c(1)])
    })

    # UI para seleção de sum, mean, max e min anuais
    output$anualsum <- renderUI({
      selectInput(NS(id, "anualsum"), "Sum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[c(1)])
    })

    output$anualmean <- renderUI({
      selectInput(NS(id, "anualmean"), "Mean:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = escolhas2()[-c(1)])
    })

    output$anualmax <- renderUI({
      selectInput(NS(id, "anualmax"), "Maximum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected = NULL)
    })

    output$anualmin <- renderUI({
      selectInput(NS(id, "anualmin"), "Minimum:",
                  choices = escolhas2(),
                  multiple = TRUE,
                  selected =NULL)
    })

    # Processamento ----------------------------------

    # Tabela Diária ----------------------------------

    # Função para manter seleção e remover opções já selecionadas entre os seletores
    pre_select_d <- reactive({
      pre_select_fun(a = input$diariosum, b = input$diariomean,
                     c = input$diariomax, d = input$diariomin)
    })

    # Atualiza opções do seletor de média diária com base nos demais
    updatemean <-reactive({
      list(input$diariosum,input$diariomax,input$diariomin)
    })
    observeEvent(updatemean(), {
      updateSelectInput(session, "diariomean",
                        choices = setdiff(escolhas2(),
                                          c(input$diariosum,
                                            input$diariomax,
                                            input$diariomin)),
                        selected = pre_select_d()$b
      )
    })

    # Atualiza opções do seletor de sum diário com base nos demais
    updatesum <-reactive({
      list(input$diariomean,input$diariomax,input$diariomin)
    })
    observeEvent(updatesum(), {
      updateSelectInput(session, "diariosum",
                        choices = setdiff(escolhas2(),
                                          c(input$diariomean,
                                            input$diariomax,
                                            input$diariomin)),
                        selected = pre_select_d()$a
      )
    })


    # Atualiza opções do seletor de max diário com base nos demais
    updatemax <-reactive({
      list(input$diariomean,input$diariosum,input$diariomin)
    })
    observeEvent(updatemax(), {
      updateSelectInput(session, "diariomax",
                        choices = setdiff(escolhas2(),
                                          c(input$diariomean,
                                            input$diariosum,
                                            input$diariomin)),
                        selected = pre_select_d()$c
      )
    })

    # Atualiza opções do seletor de min diário com base nos demais
    updatemin <-reactive({
      list(input$diariomean,input$diariosum,input$diariomax)
    })
    observeEvent(updatemin(), {
      updateSelectInput(session, "diariomin",
                        choices = setdiff(escolhas2(),
                                          c(input$diariomean,
                                            input$diariosum,
                                            input$diariomax)),
                        selected = pre_select_d()$d
      )
    })

    # Gera a tabela diária quando o botão é pressionado
    TableDiario <- eventReactive(input$diariobtn,{
      show_modal_spinner(text = "Processing...",
                         spin = "half-circle",
                         color = "black")

      tryCatch(
        {

          df <-
            dplyr::mutate(Data(),!!rlang::sym(colnames(Data())[1]) := as.Date(!!rlang::sym(colnames(Data())[1])))

          df <-
            calculateDMY(
              data = df,
              col_date = colnames(df)[1],
              col_sum = input$diariosum,
              col_mean = input$diariomean,
              col_max = input$diariomax,
              col_min = input$diariomin
            )

          js$enableTab('Monthly')

          remove_modal_spinner()
          return(df)
        },
        error = function(e) {
          remove_modal_spinner()
          stop(safeError(e))
        },
        warning =function(e){
          remove_modal_spinner()
          stop(safeError(e))
        },
        finally = function(e){
          remove_modal_spinner()
          stop(safeError(e))
        }
      )


    })

    output$table_diario <- renderReactable({
      pers_reactable(mutate(TableDiario(),
                            Date = as.character(Date)))
    })

    # Tabela Mensal ----------------------------------

    # Função para manter seleção e remover opções já selecionadas entre os seletores
    pre_select_m <- reactive({
      pre_select_fun(a=input$mensalsum,b=input$mensalmean,
                     c=input$mensalmax,d=input$mensalmin)
    })

    # Atualiza opções do seletor de média mensal com base nos demais
    updatemean_mensal <-reactive({
      list(input$mensalsum,input$mensalmax,input$mensalmin)
    })
    observeEvent(updatemean_mensal(), {
      updateSelectInput(session, "mensalmean",
                        choices = setdiff(escolhas2(),
                                          c(input$mensalsum,
                                            input$mensalmax,
                                            input$mensalmin)),
                        selected =  pre_select_m()$b
      )
    })

    # Atualiza opções do seletor de sum mensal com base nos demais
    updatesum_mensal <-reactive({
      list(input$mensalmean,input$mensalmax,input$mensalmin)
    })
    observeEvent(updatesum_mensal(), {
      updateSelectInput(session, "mensalsum",
                        choices = setdiff(escolhas2(),
                                          c(input$mensalmean,
                                            input$mensalmax,
                                            input$mensalmin)),
                        selected =  pre_select_m()$a
      )
    })


    # Atualiza opções do seletor de max mensal com base nos demais
    updatemax_mensal <-reactive({
      list(input$mensalmean,input$mensalsum,input$mensalmin)
    })
    observeEvent(updatemax_mensal(), {
      updateSelectInput(session, "mensalmax",
                        choices = setdiff(escolhas2(),
                                          c(input$mensalmean,
                                            input$mensalsum,
                                            input$mensalmin)),
                        selected =  pre_select_m()$c
      )
    })

    # Atualiza opções do seletor de min mensal com base nos demais
    updatemin_mensal <-reactive({
      list(input$mensalmean,input$mensalsum,input$mensalmax)
    })
    observeEvent(updatemin_mensal(), {
      updateSelectInput(session, "mensalmin",
                        choices = setdiff(escolhas2(),
                                          c(input$mensalmean,
                                            input$mensalsum,
                                            input$mensalmax)),
                        selected =  pre_select_m()$d
      )
    })

    # Gera a tabela mensal quando o botão é pressionado

    TableMensal <- eventReactive(input$mensalbtn,{
      show_modal_spinner(text = "Processing...",
                         spin = "half-circle",
                         color = "black")
      tryCatch(
        {

          df <-
            calculateDMY(
              data = TableDiario(),
              col_date = colnames(TableDiario())[1],
              col_sum = input$mensalsum,
              col_mean = input$mensalmean,
              col_max = input$mensalmax,
              col_min = input$mensalmin,
              type = "Monthly"
            )

          js$enableTab('Yearly')
          remove_modal_spinner()
          return(df)
        },
        error = function(e) {
          remove_modal_spinner()
          stop(safeError(e))
        },
        warning =function(e){
          remove_modal_spinner()
          stop(safeError(e))
        },
        finally = function(e){
          remove_modal_spinner()
          stop(safeError(e))
        }
      )


    })

    output$table_mensal <- renderReactable({
      pers_reactable(TableMensal())
    })

    # Tabela Anual ----------------------------------

    # Função para manter seleção e remover opções já selecionadas entre os seletores
    pre_select_a <- reactive({
      pre_select_fun(a=input$anualsum,b=input$anualmean,
                     c=input$anualmax,d=input$anualmin)
    })

    # Atualiza opções do seletor de média anual com base nos demais
    updatemean_anual <-reactive({
      list(input$anualsum,input$anualmax,input$anualmin)
    })
    observeEvent(updatemean_anual(), {
      updateSelectInput(session, "anualmean",
                        choices = setdiff(escolhas2(),
                                          c(input$anualsum,
                                            input$anualmax,
                                            input$anualmin)),
                        selected =  pre_select_a()$b
      )
    })

    # Atualiza opções do seletor de sum anual com base nos demais
    updatesum_anual <-reactive({
      list(input$anualmean,input$anualmax,input$anualmin)
    })
    observeEvent(updatesum_anual(), {
      updateSelectInput(session, "anualsum",
                        choices = setdiff(escolhas2(),
                                          c(input$anualmean,
                                            input$anualmax,
                                            input$anualmin)),
                        selected =  pre_select_a()$a
      )
    })


    # Atualiza opções do seletor de max anual com base nos demais
    updatemax_anual <-reactive({
      list(input$anualmean,input$anualsum,input$anualmin)
    })
    observeEvent(updatemax_anual(), {
      updateSelectInput(session, "anualmax",
                        choices = setdiff(escolhas2(),
                                          c(input$anualmean,
                                            input$anualsum,
                                            input$anualmin)),
                        selected =  pre_select_a()$c
      )
    })

    # Atualiza opções do seletor de min anual com base nos demais
    updatemin_anual <-reactive({
      list(input$anualmean,input$anualsum,input$anualmax)
    })
    observeEvent(updatemin_anual(), {
      updateSelectInput(session, "anualmin",
                        choices = setdiff(escolhas2(),
                                          c(input$anualmean,
                                            input$anualsum,
                                            input$anualmax)),
                        selected =  pre_select_a()$d
      )
    })

    TableAnual <- eventReactive(input$anualbtn,{
      show_modal_spinner(text = "Processing...",
                         spin = "half-circle",
                         color = "black")
      tryCatch(
        {
          df <-
            calculateDMY(
              data = TableMensal(),
              col_date = colnames(TableMensal())[1],
              col_sum = input$anualsum,
              col_mean = input$anualmean,
              col_max = input$anualmax,
              col_min = input$anualmin,
              type = "Yearly"
            )
          remove_modal_spinner()
          return(df)
        },
        error = function(e) {
          remove_modal_spinner()
          stop(safeError(e))
        },
        warning =function(e){
          remove_modal_spinner()
          stop(safeError(e))
        },
        finally = function(e){
          remove_modal_spinner()
          stop(safeError(e))
        }
      )

    })

    output$table_anual <- renderReactable({
      pers_reactable(TableAnual())
    })

    #Download-------
    observeEvent(input$DownBD, {
      shinyalert(html = TRUE, text = tagList(
        radioButtons(
          NS(id,"sepDownBD"), "Separador",
          choices = c(Comma = ",",
                      Semicolon = ";",
                      Tab = "\t"),
          selected = ";"),
        radioButtons(NS(id,"decDownBD"), "Decimal points",
                     choices = c(Comma = ",",
                                 Point = "."),
                     selected = "."),
        actionButton(NS(id,"DownBD_alert"),"OK")
      ),
      showConfirmButton = FALSE,
      size = "xs")
    })

    observe({
      req(input$sepDownBD)
      if(input$decDownBD == "," & input$sepDownBD==","){
        updateRadioButtons(
          session,
          inputId = "decDownBD",
          choices = c(Comma = ",",
                      Point = "."),
          selected = "."
        )
      }
    })

    observeEvent(input$DownBD_alert,{
      runjs("
        var separador = $('input[name=\"processtabs-sepDownBD\"]:checked').val();
        var decimal = $('input[name=\"processtabs-decDownBD\"]:checked').val();
        Reactable.downloadDataCSV('processtabs-table_diario',
        'file.csv',
        { sep: separador,dec: decimal });
        ")
    })


    observeEvent(input$DownBM, {
      shinyalert(html = TRUE, text = tagList(
        radioButtons(
          NS(id,"sepDownBM"), "Separador",
          choices = c(Comma = ",",
                      Semicolon = ";",
                      Tab = "\t"),
          selected = ";"),
        radioButtons(NS(id,"decDownBM"), "Decimal points",
                     choices = c(Comma = ",",
                                 Point = "."),
                     selected = "."),
        actionButton(NS(id,"DownBM_alert"),"OK")
      ),
      showConfirmButton = FALSE,
      size = "xs")
    })

    observe({
      req(input$sepDownBM)
      if(input$decDownBM == "," & input$sepDownBM==","){
        updateRadioButtons(
          session,
          inputId = "decDownBM",
          choices = c(Comma = ",",
                      Point = "."),
          selected = "."
        )
      }
    })

    observeEvent(input$DownBM_alert,{
      runjs("
        var separador = $('input[name=\"processtabs-sepDownBM\"]:checked').val();
        var decimal = $('input[name=\"processtabs-decDownBM\"]:checked').val();
        Reactable.downloadDataCSV('processtabs-table_mensal',
        'file.csv',
        { sep: separador,dec: decimal });
        ")
    })

    observeEvent(input$DownBA, {
      shinyalert(html = TRUE, text = tagList(
        radioButtons(
          NS(id,"sepDownBA"), "Separador",
          choices = c(Comma = ",",
                      Semicolon = ";",
                      Tab = "\t"),
          selected = ";"),
        radioButtons(NS(id,"decDownBA"), "Decimal points",
                     choices = c(Comma = ",",
                                 Point = "."),
                     selected = "."),
        actionButton(NS(id,"DownBA_alert"),"OK")
      ),
      showConfirmButton = FALSE,
      size = "xs")
    })

    observe({
      req(input$sepDownBA)
      if(input$decDownBA == "," & input$sepDownBA==","){
        updateRadioButtons(
          session,
          inputId = "decDownBA",
          choices = c(Comma = ",",
                      Point = "."),
          selected = "."
        )
      }
    })


    observeEvent(input$DownBA_alert,{
      runjs("
        var separador = $('input[name=\"processtabs-sepDownBA\"]:checked').val();
        var decimal = $('input[name=\"processtabs-decDownBA\"]:checked').val();
        Reactable.downloadDataCSV('processtabs-table_anual',
        'file.csv',
        { sep: separador,dec: decimal });
        ")
    })
    return(list(TableDiario,TableMensal,TableAnual))
  })
}
