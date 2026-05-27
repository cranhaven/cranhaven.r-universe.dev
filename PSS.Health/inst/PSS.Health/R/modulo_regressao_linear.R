
mod_regressao_linear_Ui <- function(id) {

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) |>
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_regressao_linear_server <- function(id, tipo = "tamanho_amostral", txt_ajuda,
                                        translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                        warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas, warning_numero,
                                        lista_de_funcoes_server) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      eval(parse(text = warning_prop("r2", entre0e1 = TRUE)))
      eval(parse(text = warning_numero("beta")))
      eval(parse(text = warning_numero_positivo("sigmaX")))
      eval(parse(text = warning_numero_positivo("sigmaY")))

      eval(parse(text = warning_numero_positivo("amplitude")))
      eval(parse(text = warning_prop("poder")))
      eval(parse(text = warning_prop("alpha")))
      eval(parse(text = warning_prop("confianca")))
      eval(parse(text = warning_perdas("perc_perdas")))

      eval(parse(text = warning_inteiro("n")))


      output$aba <- renderUI({
        sidebarLayout(
          sidebarPanel(

            if (tipo %in% c("tamanho_amostral", "poder")) {
              tagList(
                wellPanel(
                  HTML(
                    paste0(
                      "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                    )
                  ),
                  withMathJax(paste0("$$H_0: \\beta_\\text{", nome_preditora(), "} = 0$$")),
                  withMathJax(paste0("$$H_1: \\beta_\\text{", nome_preditora(), "} \\neq 0$$"))
                )

                # checkboxInput(
                #   inputId = ns("usar_r2"),
                #   label   = translation_pss("Calcular utilizando o R²", linguagem()),
                #   value = FALSE
                # )
              )
            },

            actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),
            br(), br(),

            if (tipo == "estimar") {
              numericInput( ns("amplitude"),
                            translation_pss("Amplitude do intervalo", linguagem()),
                            value = 0.1,
                            min = 0,
                            step = 0.1
              ) |> .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_amplitude, title = translation_pss("Amplitude do intervalo", linguagem()))
            },

            selectInput(
              ns('estatistica'),
              "Estatística de entrada",
              choices = opcoes_medidas(),
              selected = 'beta'
            ),

            uiOutput(ns("usar_r2Ui")),

            if (tipo %in% c("tamanho_amostral")) {
              numericInput( ns("poder"),
                            translation_pss("Poder (%)", linguagem()),
                            value = 80,
                            min = 0,
                            max = 100,
                            step = 1
              ) |> .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
            } else if (tipo == "poder") {
              numericInput( ns("n"),
                            translation_pss("Tamanho amostral", linguagem()),
                            value = 200,
                            min = 4,
                            step = 1
              )
            },

            if (tipo %in% c("tamanho_amostral", "poder")) {
              numericInput( ns("alpha"),
                            translation_pss("Nível de significância (%)", linguagem()),
                            value = 5,
                            min = 0,
                            max = 100,
                            step = 1
              ) |> .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem()))
            } else {
              tagList(
                numericInput( ns("confianca"),
                              translation_pss("Nível de confiança (%)", linguagem()),
                              value = 5,
                              min = 0,
                              max = 100,
                              step = 1
                ) |> .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem()))
              )
            },

            if (tipo %in% c("tamanho_amostral", "estimar")) {
              numericInput( ns("perc_perdas"),
                            translation_pss("Perdas/ Recusas (%)", linguagem()),
                            value = 10,
                            min = 0,
                            max = 100,
                            step = 1
              ) |> .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
            }
          ),

          mainPanel(
            htmlOutput(ns("texto_principal")) |>
              shinycssloaders::withSpinner(type = 5)

            # uiOutput(ns("cenarios"))

          )
        )
      })




      # Mudar nomes -----

      observeEvent(input$mudar_nomes, {
        showModal(
          modalDialog(
            title = translation_pss("Ajustes", linguagem()),
            fluidPage(

              HTML(translation_pss("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>.", linguagem())),
              br(), br(),
              textInput(inputId = ns("nome_desfecho"),
                        label   = translation_pss("Descreva o nome do desfecho", linguagem()),
                        value   = ifelse(input$mudar_nomes == 0, "Y", nome_desfecho())),
              HTML("<i>", gsub("<br><br>", "", txt_ajuda()$txt_desfecho), "</i>"),
              br(), br(),

              textInput(inputId = ns("nome_preditora"),
                        label   = translation_pss("Descreva o nome da variável preditora", linguagem()),
                        value   = ifelse(input$mudar_nomes == 0, "X", nome_preditora()))

            ),
            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      nome_desfecho <- reactive({
        ifelse(is.null(input$nome_desfecho), "Y", input$nome_desfecho)
      })

      nome_preditora <- reactive({
        ifelse(is.null(input$nome_preditora), "X", input$nome_preditora)
      })





      # Correlacao ou R²? -----

      opcoes_medidas <- reactive({

        nomes_opcoes_medida <- c(
          translation_pss("Coeficiente de regressão", linguagem()),
          translation_pss("Coeficiente de correlação", linguagem()),
          translation_pss("Coeficiente de determinação", linguagem())
        )
        opcoes_medida <- c('beta', 'r', "r2")
        names(opcoes_medida) <- nomes_opcoes_medida

        opcoes_medida
      })




      output$usar_r2Ui <- renderUI({

        if (input$estatistica == "r2") {
          numericInput( ns("r2"),
                        translation_pss("Coeficiente de determinação", linguagem()),
                        value = 0.2,
                        min = 0,
                        max = Inf,
                        step = 1
          ) |> .help_buttom(linguagem = linguagem(),
            body = txt_ajuda()$txt_coef_determinacao,
            title = translation_pss("Coeficiente de determinação", linguagem())
          )

        } else {

          if (input$estatistica == "r") {
            numericInput( ns("r"),
                          translation_pss("Coeficiente de correlação", linguagem()),
                          value = .7,
                          min = 0,
                          max = 1,
                          step = .01
            ) |> .help_buttom(
              linguagem = linguagem(),
              body = txt_ajuda()$txt_correlacao,
              title = translation_pss("Coeficiente de correlação", linguagem())
            )

          } else {

            tagList(
              numericInput(ns("beta"),
                           translation_pss("Coeficiente de regressão", linguagem()),
                           value = .8,
                           step = 1
              ) |> .help_buttom(
                linguagem = linguagem(),
                body = translation_pss("Coeficiente de inclinação da reta para um modelo de regressão linear simples", linguagem()),
                title = translation_pss("Coeficiente de inclinação da reta para um modelo de regressão linear simples", linguagem())
              ),
              numericInput( ns("sigmaY"),
                            paste0(
                              translation_pss("Desvio padrão esperado de", linguagem()),
                              " ",
                              nome_desfecho()
                            ),
                            value = 0.5,
                            min = 0,
                            max = Inf,
                            step = 1
              ) |> .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão esperado", linguagem())),
              numericInput(ns("sigmaX"),
                           paste0(
                             translation_pss("Desvio padrão esperado de", linguagem()),
                             " ",
                             nome_preditora()
                           ),
                           value = 0.2,
                           min = 0,
                           max = Inf,
                           step = 1
              ) |> .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão esperado", linguagem()))
            )
          }
        }
      })





      # Texto ----

      output$texto_principal <- renderText({


        if (tipo == "tamanho_amostral") {

          code <- ifelse(
            input$estatistica == "beta",
            paste0(
              "powerMediation::ss.SLR(",
              "power = ", input$poder, "/100, ",
              "lambda.a = ", input$beta, ", ",
              "sigma.x = ", input$sigmaX, ", ",
              "sigma.y = ", input$sigmaY, ", ",
              "alpha = ", input$alpha, "/100, ",
              "verbose = FALSE)"
            ),
            ifelse(
              input$estatistica == "r",
              paste0(
                "powerMediation::ss.SLR(",
                "power = ", input$poder, "/100, ",
                "lambda.a = ", input$r, ", ",
                "sigma.x = 1, ",
                "sigma.y = 1, ",
                "alpha = ", input$alpha, "/100, ",
                "verbose = FALSE)"
              ),
              paste0(
                "powerMediation::ss.SLR.rho(",
                "power = ", input$poder, "/100, ",
                "alpha =",  input$alpha, "/100, ",
                "rho2 = ",  input$r2, ", ",
                "verbose = FALSE)"
              )
            )
          )


          n <- try_n(code)
          eval(parse(text = validate_n("n")))
          n <- ceiling(n$n)
          eval(parse(text = validate_n_inf("n")))


          paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
                 "</font></b></br></br>",

                 lista_de_funcoes_server()$sugestao_texto_portugues(
                   "<i>",
                   translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


                   "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",
                   "para testar se o coeficiente de regressão de <i>", nome_preditora(), "</i> ao regredir <i>", nome_desfecho(),
                   "</i> é diferente de 0 ",
                   "(com o acréscimo de ", input$perc_perdas, "% para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",
                   "O cálculo considerou um poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>",

                   if (input$estatistica == "r2" | input$estatistica == "r") {
                     paste0(
                       " e coeficiente de ",
                       ifelse(input$estatistica == "r2",
                              paste0("determinação esperado de <b>", input$r2, "</b>, "),
                              paste0("correlação esperado de <b>", input$r, "</b>, ")
                       ),
                       "como é referido em Fulano (1900) OU escolha do pesquisador. "
                     )
                   } else {

                     paste0(
                       ", coeficiente de regressão esperado de <b>", input$beta, "</b>, ",
                       " desvios padrões esperados de <b>", input$sigmaX, "</b> e <b>", input$sigmaY, "</b> ",
                       translation_pss("u.m.", linguagem()),
                       "para <i>", nome_preditora(), "</i> e <i>", nome_desfecho(), "</i>",
                       ", respectivamente, ",
                       "como são referidos em Fulano (1900). "
                     )
                   },
                   .txt_citacao_pss
                 ),
                 .txt_referencia_tap,
                 print_r_code(code)
          )



          # Se for variavel categorica!
        } else if (tipo == "poder") {

          req(!is.null(input$beta))

          code <- ifelse(
            input$estatistica == "beta",
            paste0(
              "powerMediation::power.SLR(",
              "n = ", input$n, ", ",
              "lambda.a = ", input$beta, ", ",
              "sigma.x = ", input$sigmaX, ", ",
              "sigma.y = ", input$sigmaY, ", ",
              "alpha = ", input$alpha, "/100, ",
              "verbose = FALSE)"
            ),
            ifelse(
              input$estatistica == "r",
              paste0(
                "powerMediation::power.SLR(",
                "n = ", input$n, ", ",
                "lambda.a = ", input$r, "*", input$sigmaY, "/", input$sigmaX, ", ",
                "sigma.x = ", input$sigmaX, ", ",
                "sigma.y = ", input$sigmaY, ", ",
                "alpha = ", input$alpha, "/100, ",
                "verbose = FALSE)"
              ),
              paste0(
                "powerMediation::power.SLR.rho(",
                "n = ", input$n, ", ",
                "alpha =",  input$alpha, "/100, ",
                "rho2 = ",  input$r2, ", ",
                "verbose = FALSE)"
              )
            )
          )


          poder <- eval(parse(text = code))
          poder <- poder$power

          paste0(
            "<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", round(poder*100, digits = 1),
            "%</font></b></br></br>",

            lista_de_funcoes_server()$sugestao_texto_portugues(
              # "<i>",
              "O poder para testar se o coeficiente de regressão de <i>", nome_preditora(), "</i> ao regredir <i>", nome_desfecho(),
              "</i> é diferente de 0 é <b>",
              round(poder*100, digits = 1), "%</b>. ",

              "Este valor foi obtido considerando nível de significância de <b>", input$alpha, "%</b>, ",
              "tamanho amostral igual a <b>", input$n, "</b> sujeitos",

              if (input$estatistica == "r2") {
                paste0(
                  " e coeficiente de determinação esperado de <b>", input$r2, "</b> ",
                  "como é referido em Fulano (1900) OU escolha do pesquisador. "
                )
              } else {

                paste0(
                  if (input$estatistica == "r") {
                    paste0(", coeficiente de correlação esperado de <b>", input$r, "</b>, ")
                  } else {
                    paste0(", coeficiente de regressão esperado de <b>", input$beta, "</b>, ")
                  },
                  " desvios padrões esperados de <b>", input$sigmaX, "</b> e <b>", input$sigmaY, "</b> ",
                  translation_pss("u.m.", linguagem()),
                  "para <i>", nome_preditora(), "</i> e <i>", nome_desfecho(), "</i>",
                  ", respectivamente, ",
                  "como são referidos em Fulano (1900). "
                )
              },
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




