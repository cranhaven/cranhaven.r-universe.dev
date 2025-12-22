serverEvapo <-
  function(id, Data) {
    moduleServer(id,function(input, output, session) {

      #render outputs-------
      choiceseto <- reactive({
        return(colnames(Data()))
      })

      output$G <- renderUI({
        selectInput(
          NS(id,"G"),
          "Soil heat flux (MJ/m²/day):",
          choices = c("",choiceseto()),
          selected = choiceseto()[2]
        )
      })
      output$Temp <- renderUI({
        selectInput(
          NS(id,"Temp"),
          "Temperature (°C):",
          choices = choiceseto(),
          selected = choiceseto()[3]
        )
      })
      output$Humid<- renderUI({
        selectInput(
          NS(id,"Humid"),
          "Relative humidity (%):",
          choices = choiceseto(),
          selected = choiceseto()[4]
        )
      })
      output$Rad <- renderUI({
        selectInput(
          NS(id,"Rad"),
          "Global solar radiation (MJ/m²):",
          choices = choiceseto(),
          selected = choiceseto()[5]
        )
      })
      output$Wind <- renderUI({
        selectInput(
          NS(id,"Wind"),
          "Wind speed (m/s):",
          choices = choiceseto(),
          selected = choiceseto()[6]
        )
      })
      output$Patm <- renderUI({
        selectInput(
          NS(id,"Patm"),
          "Pressão atmosferica (atm):",
          choices = choiceseto(),
          selected = choiceseto()[7]
        )
      })
      output$Kc <- renderUI({
        selectInput(
          NS(id,"Kc"),
          "Kc:",
          choices = c("",choiceseto()),
          selected = ""
        )
      })

      #Calculando Eto, Etc e LLI-------------
      TableET <- reactive({

        # print(input$Tmed)
        df <- Data()

        df <-
          df %>%
          mutate(across(where(is.character),
                        ~ ifelse(
                          grepl("^\\d+\\.?\\d*$", .),
                          as.double(.), .
                        )))

        df <-
          calculateETrefPM(data = df,
                         lat = input$Lat,
                         alt = input$Alt,
                         za = input$Alt_an,
                         DAP = input$DAP,
                         date = colnames(df)[1],
                         Ta = input$Temp,
                         G = input$G,
                         RH = input$Humid,
                         Rg = input$Rad,
                         AP = input$Patm,
                         WS = input$Wind,
                         Kc = input$Kc
          )

        return(df)
      })

      output$table_calc_eto <- renderReactable({
        reactable(
          mutate(TableET(),across(where(is.numeric), \(x) round(x, digits = 2)))
          )
      })

      observeEvent(input$DownETO, {
        shinyalert(html = TRUE, text = tagList(
          radioButtons(
            NS(id,"sepDownETO"), "Separador",
            choices = c(Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"),
            selected = ";"),
          radioButtons(NS(id,"decDownETO"), "Decimal points",
                       choices = c(Comma = ",",
                                   Point = "."),
                       selected = "."),
          actionButton(NS(id,"DownETO_alert"),"OK")
        ),
        showConfirmButton = FALSE,
        size = "xs")
      })

      observe({
        req(input$sepDownETO)
        if(input$decDownETO == "," & input$sepDownETO==","){
          updateRadioButtons(
            session,
            inputId = "decDownETO",
            choices = c(Comma = ",",
                        Point = "."),
            selected = "."
          )
        }
      })

      observeEvent(input$DownETO_alert,{
        runjs("
        var separador = $('input[name=\"etocalc-sepDownETO\"]:checked').val();
        var decimal = $('input[name=\"etocalc-decDownETO\"]:checked').val();
        Reactable.downloadDataCSV('etocalc-table_calc_eto',
        'file.csv',
        { sep: separador,dec: decimal });
        ")
      })


    })
  }

