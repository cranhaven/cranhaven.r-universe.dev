
mod_kappa_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_kappa_server <- function(id, tipo = "tamanho_amostral", txt_ajuda,
                             translation_pss, linguagem, .rodape, validate_n, try_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                             warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas, erro_painel_principal,
                             lista_de_funcoes_server){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns



      eval(parse(text = warning_inteiro("avaliadores")))
      eval(parse(text = warning_prop("kappa", entre0e1 = TRUE)))
      eval(parse(text = warning_prop("kappa_h0", entre0e1 = TRUE)))
      eval(parse(text = warning_prop("probabilidade2")))

      eval(parse(text = warning_prop("amplitude", entre0e1 = TRUE)))

      eval(parse(text = warning_prop("poder")))
      eval(parse(text = warning_prop("alpha")))
      eval(parse(text = warning_prop("confianca")))
      eval(parse(text = warning_perdas("perc_perdas")))





      # Aba  ----

      output$aba <- renderUI({


        tagList(

          sidebarLayout(
            sidebarPanel(

              if (tipo != "estimar") {
                wellPanel(
                  HTML(
                    paste0(
                      "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                    )
                  ),
                  uiOutput(ns("th_h0")),
                  uiOutput(ns("th_h1"))
                )
              },

              radioButtons( ns("numero_categorias"),
                            translation_pss("Número de categorias do desfecho", linguagem()),
                            choices = 2:5,
                            selected = 2,
                            inline = TRUE
              ),
              actionLink(ns("mudar_nomes"), translation_pss("Mudar rótulos", linguagem())),
              br(), br(),
              uiOutput(ns("probabilidadesUi")),

              numericInput( ns("kappa"),
                            translation_pss("Kappa esperado", linguagem()),
                            value = 0.85,
                            min = 0,
                            max = 1,
                            step = .1
              ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Valor de kappa que se espera encontrar.", txt_ajuda()$txt_definido_pesquisador_OU_literatura)),

              if (tipo != "estimar") {
                numericInput( ns("kappa_h0"),
                              translation_pss("Valor de referência sob a hipótese nula", linguagem()),
                              value = 0.5,
                              min = 0,
                              max = 1,
                              step = .1
                ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Kappa para testar em H0", txt_ajuda()$txt_definido_pesquisador))
              },

              numericInput( ns("avaliadores"),
                            translation_pss("Número de avaliadores", linguagem()),
                            value = 2,
                            min = 2,
                            max = 6,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = paste0("The number of raters that are available", txt_ajuda()$txt_definido_pesquisador)),



              if (tipo == "tamanho_amostral") {
                tagList(
                  numericInput( ns("poder"),
                                translation_pss("Poder (%)", linguagem()),
                                value = 80,
                                min = 0,
                                max = 100,
                                step = 1
                  ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem())),

                  numericInput( ns("alpha"),
                                translation_pss("Nível de significância (%)", linguagem()),
                                value = 5,
                                min = 0,
                                max = 100,
                                step = 1
                  ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem()))
                )


              } else {

                tagList(
                  numericInput( ns("amplitude"),
                                translation_pss("Amplitude do intervalo", linguagem()),
                                value = 0.2,
                                min = 0,
                                max = 1,
                                step = 0.1
                  ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_amplitude, title = translation_pss("Amplitude do intervalo", linguagem())),

                  numericInput( ns("confianca"),
                                translation_pss("Nível de confiança (%)", linguagem()),
                                value = 95,
                                min = 0,
                                max = 100,
                                step = 1
                  ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem()))
                )
              },


              if (tipo != "poder") {
                numericInput( ns("perc_perdas"),
                              translation_pss("Perdas/ Recusas (%)", linguagem()),
                              value = 10,
                              min = 0,
                              max = 100,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
              }
            ),

            mainPanel(
              htmlOutput(ns("texto_principal")) %>%
                shinycssloaders::withSpinner(type = 5),

              uiOutput(ns("cenarios"))
            )
          )

        )

      })


      # Mudar nomes -----


      output$nomes_categoriasUi <- renderUI({
        len <- as.integer(input$numero_categorias)

        lapply(1:len, function(i) {
          div(
            textInput(
              inputId = ns(paste0("categoria_", i)),
              label = paste0("Rótulo da categoria ", i),
              value = LETTERS[i]
            )
          )
        })
      })



      observeEvent(input$mudar_nomes, {
        showModal(
          modalDialog(
            title = translation_pss("Ajustes", linguagem()),
            fluidPage(

              HTML(translation_pss("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>.", linguagem())),
              br(), br(),
              uiOutput(ns("nomes_categoriasUi"))

            ),
            easyClose = TRUE,
            footer    = NULL
          )
        )
      })





      # Hipoteses a serem testadas ----


      output$th_h0 <- renderUI({
        req(!is.null(input$kappa_h0))
        req(!is.na(input$kappa_h0))

        withMathJax(
          paste0(
            "$$H_0: \\kappa = ", input$kappa_h0, "$$"
          )
        )
      })

      output$th_h1 <- renderUI({
        req(!is.null(input$kappa_h0))
        req(!is.na(input$kappa_h0))

        withMathJax(
          paste0(
            "$$H_1: \\kappa \\neq ", input$kappa_h0, "$$"
          )
        )
      })





      # Opcoes de acordo com o numero de categorias -----

      output$prob_categoriasUi <- renderUI({
        len <- as.integer(input$numero_categorias)

        if (len == 2) {
          prob_start <- 60
        } else if (len == 3) {
          prob_start <- c(20.5, 50)
        } else if (len == 4) {
          prob_start <- c(20.5, 30.5, 30)
        } else if (len == 5) {
          prob_start <- c(20.5, 30.5, 30, 10)
        }


        eh_nulo <- lapply(1:len, function(i) {
          is.null(input[[paste0("categoria_", i)]])
        }) %>% unlist()

        rotulos <- LETTERS[1:len]

        rotulos_input <- lapply(1:len, function(i) {
          input[[paste0("categoria_", i)]]
        }) %>% unlist()


        for (i in 1:len) rotulos[i] <- ifelse(eh_nulo[i], rotulos[i], rotulos_input[i])


        lapply(1:(len - 1), function(i) {
          div(
            numericInput(
              inputId = ns(paste0("prob_", i)),
              label = rotulos[i],
              value = prob_start[i],
              width = "50%"
            )
          )
        })
      })


      output$probabilidadesUi <- renderUI({
        req(!is.null(input$numero_categorias))

        wellPanel(
          HTML(paste0("<b>", translation_pss("Percentual (%) de ocorrência da categoria", linguagem()), "</b><br><br>")),
          uiOutput(ns("prob_categoriasUi"))
        )

      })



      probabilidades <- reactive({
        req(!is.null(input$prob_1))

        len <- as.integer(input$numero_categorias)

        probs <- lapply(1:(len - 1), function(i) {
          input[[paste0("prob_", i)]]
        }) %>% unlist()

        probs
      })


      observe({
        req(!is.null(input$numero_categorias))

        lapply(
          1:as.integer(input$numero_categorias),
          function(i) {
            eval(parse(text = warning_prop(paste0("prob_", i))))
          }
        )
      })


      observe({
        req(!is.null(input$numero_categorias))
        req(!is.null(probabilidades()))
        req(!is.null(input$prob_1))

        len <- as.integer(input$numero_categorias)
        probs <- c(probabilidades(), 100 - sum(probabilidades()))


        lapply(
          1:(len - 1),
          function(i) {
            shinyFeedback::hideFeedback(paste0("prob_", i))

            if (any(is.na(probs))) {
              shinyFeedback::showFeedbackWarning(
                inputId = paste0("prob_", i),
                text = "",
                color = "red")
            } else if(any(probs <= 0)){
              shinyFeedback::showFeedbackWarning(
                inputId = paste0("prob_", i),
                text = "",
                color = "red")
            }
          }
        )
      })




      ## Texto ----


      output$texto_principal <- renderText({

        req(!is.null(input$numero_categorias))


        len <- as.integer(input$numero_categorias)
        probs <- c(probabilidades(), 100 - sum(probabilidades()))

        req(all(!is.na(probs)))

        # Tem que cuidar por causa da modal
        eh_nulo <- lapply(1:len, function(i) {
          is.null(input[[paste0("categoria_", i)]])
        }) %>% unlist()

        rotulos <- LETTERS[1:len]

        rotulos_input <- lapply(1:len, function(i) {
          input[[paste0("categoria_", i)]]
        }) %>% unlist()

        for (i in 1:len) rotulos[i] <- ifelse(eh_nulo[i], rotulos[i], rotulos_input[i])

        categorias <- sub(",([^,]*)$", " e\\1", paste(rotulos, collapse = ", "))



        probs_categorias <- sub(",([^,]*)$", " e\\1", paste(probs, collapse = ", "))

        metodos <- sub(",([^,]*)$", " e\\1", paste(letters[1:input$avaliadores], collapse = ", "))







        if (any(probs <= 0)) {
          paste0(
            erro_painel_principal(),
            "<br><br><br>Percentual inválido! ",
            "Com os dados inseridos temos que o percentual de ocorrência da categoria <b>",
            rotulos[len], " é de ", probs[len], "%</b>."
          )



        } else if (tipo %in% c("tamanho_amostral")) {


          foo <- dplyr::case_when(
            input$numero_categorias == "2" ~ "kappaSize::PowerBinary",
            input$numero_categorias == "3" ~ "kappaSize::Power3Cats",
            input$numero_categorias == "4" ~ "kappaSize::Power4Cats",
            input$numero_categorias == "5" ~ "kappaSize::Power5Cats"
          )

          code <- paste0(
            foo, "(kappa0 = ", input$kappa_h0, ", ",
            "kappa1 = ", input$kappa, ", ",
            "props  = c(", paste(probs, collapse = ", "),  ")/100, ",
            "raters = ", input$avaliadores, ", ",
            "alpha  = ", input$alpha, "/100, ",
            "power  = ", input$poder, "/100)"
          )
          # print_r_code(code)
          n <- try_n(code)
          eval(parse(text = validate_n("n")))

          n <- ceiling(n$N)
          eval(parse(text = validate_n_inf("n")))



          paste0(
            "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n, "</font></b></br></br>",

            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>",
              translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

              "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para testar se o coeficiente de concordância de Kappa, que avaliará a ",
              "concordância entre os <b>métodos/ avaliadores ", metodos, "</b>, é diferente de <b>", input$kappa_h0, "</b> ",
              "(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",

              "O cálculo considerou poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>, ",
              "Kappa esperado de <b>", input$kappa, "</b> ",
              "e percentuais de ocorrência de <b>", probs_categorias, "%</b> para as categorias <b>", categorias, "</b>, respectivamente, ",
              "conforme referido em Fulano (1900) OU escolha do pesquisador. ",
              .txt_citacao_pss
            ),
            .txt_referencia_tap,
            print_r_code(code)
          )




          ## Estimar ----
        } else {

          code <- paste0(
            "presize::prec_kappa(",
            "kappa = ", input$kappa, ", ",
            "conf.width = ", input$amplitude, ", ",
            "raters = ", input$avaliadores, ", ",
            "n_category = ", input$numero_categorias, ", ",
            "props  = c(", paste(probs, collapse = ", "),  ")/100, ",
            "conf.level  = ", input$confianca, "/100)"
          )

          # print_r_code(code)
          n <- try_n(code)
          eval(parse(text = validate_n("n")))

          n <- ceiling(n$n)
          eval(parse(text = validate_n_inf("n")))


          paste0(
            "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n, "</font></b></br></br>",

            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>",
              translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

              "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para estimar o coeficiente de concordância de Kappa, que avaliará a ",
              "concordância entre os <b>métodos/ avaliadores ", metodos, "</b>, com amplitude desejada para o intervalo de confiança de de <b>", input$amplitude, "</b> ",
              "(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",

              "O cálculo considerou nível de confiança de <b>", input$confianca, "%</b>, ",
              "Kappa esperado de <b>", input$kappa, "</b> ",
              "e percentuais de ocorrência de <b>", probs_categorias, "%</b> para as categorias <b>", categorias, "</b>, respectivamente, ",
              "conforme referido em Fulano (1900) OU escolha do pesquisador. ",
              .txt_citacao_pss
            ),
            .txt_referencia_tap,
            print_r_code(code)
          )


      }


      })








      } # Nao mexer!!!
)

}
