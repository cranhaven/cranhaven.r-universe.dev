
mod_regressao_logistica_Ui <- function(id) {

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba_regressao_logistica")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_regressao_logistica_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, txt_balanceamento_f,
                                           translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                           warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas,
                                           lista_de_funcoes_server) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      eval(parse(text = warning_prop("perc_tratamento")))
      eval(parse(text = warning_prop("perc_controle")))

      eval(parse(text = warning_numero_positivo("rr")))
      eval(parse(text = warning_numero_positivo("rc")))
      eval(parse(text = warning_numero_positivo("rc_continua")))
      eval(parse(text = warning_numero_positivo("logistic_rate_mean")))

      eval(parse(text = warning_numero_positivo("amplitude")))
      eval(parse(text = warning_numero_positivo("balanceamento")))
      eval(parse(text = warning_prop("poder")))
      eval(parse(text = warning_prop("alpha")))
      eval(parse(text = warning_prop("confianca")))
      eval(parse(text = warning_prop("precisao")))
      eval(parse(text = warning_perdas("perc_perdas")))

      eval(parse(text = warning_inteiro("n_controle")))
      eval(parse(text = warning_inteiro("n_tratamento")))
      eval(parse(text = warning_inteiro("n_total")))




      opcoes_tipo_variavel <- reactive({
        tipo <- 0:1

        if (linguagem() == "pt") {
          names(tipo) <- c("Binária", "Contínua")
        } else {
          names(tipo) <- c("Binary", "Continuous")
        }

        tipo
      })






      opcoes_metodos_estimacao <- reactive({
        choices <- c(
          "Logit suavizado" = "indip_smooth",
          "Gart" = "gart",
          "Woolf" = "woolf"
        )

        if (linguagem() == "en") {
          names(choices) <- c("Independence-smoothed logit", "Gart", "Woolf")
        }

        choices
      })



      output$aba_regressao_logistica <- renderUI({
        req(!is.null(opcoes_tipo_variavel()))

        sidebarLayout(
          sidebarPanel(

            wellPanel(HTML(
              '<b><a href="https://youtu.be/wPuUns9pRTA" target="_blank">',
              translation_pss("Vídeo: PSS Health para regressão logística com preditor quantitativo", linguagem()),
              '</a></b><br>'
            )),

            if (tipo %in% c("tamanho_amostral", "poder")) {
              tagList(
                wellPanel(
                  HTML(
                    paste0(
                      "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                    )
                  ),
                  withMathJax("$$H_0: RC = 1 \\text{  vs  } H_1: RC \\neq 1$$")
                ),

                radioButtons(inputId = ns("tipo_variavel"),
                             label   = translation_pss("Qual tipo de variável independente?", linguagem()),
                             choices =  opcoes_tipo_variavel(),
                             selected = 0,
                             inline   = TRUE
                )
              )
            },

            actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),
            br(), br(),

            if (tipo == "estimar") {
              numericInput( ns("amplitude"),
                            translation_pss("Amplitude do intervalo", linguagem()),
                            value = 1.5,
                            min = 0,
                            step = 0.1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_amplitude, title = translation_pss("Amplitude do intervalo", linguagem()))
            },

            # uiOutput(ns("render_input_medida_controle")),
            uiOutput(ns("estatistica_tratamentoUi")),
            uiOutput(ns("balanceamentoUi")),


            if (tipo %in% c("tamanho_amostral")) {
              numericInput( ns("poder"),
                            translation_pss("Poder (%)", linguagem()),
                            value = 80,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
            },

            if (tipo %in% c("tamanho_amostral", "poder")) {
              numericInput( ns("alpha"),
                            translation_pss("Nível de significância (%)", linguagem()),
                            value = 5,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem()))
            } else{
              tagList(
                numericInput( ns("confianca"),
                              translation_pss("Nível de confiança (%)", linguagem()),
                              value = 95,
                              min = 0,
                              max = 100,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem())),

                selectInput(ns("metodo_estimacao"),
                            translation_pss("Método utilizado para calcular o intervalo de confiança", linguagem()),
                            choices = opcoes_metodos_estimacao(),
                            selected = "indip_smooth"
                ) %>%
                  .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_per_method_presize)
              )
            },

            if (tipo %in% c("tamanho_amostral", "estimar")) {
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

            htmlOutput(ns("texto_principal_poder")) %>%
              shinycssloaders::withSpinner(type = 5),

            htmlOutput(ns("texto_principal_estimar")) %>%
              shinycssloaders::withSpinner(type = 5),

            uiOutput(ns("cenarios"))

          )
        )
      })


      estimar_ou_var_binaria <- reactive({
        estimar_ou_var_binaria <- tipo == "estimar"
        if (! estimar_ou_var_binaria) estimar_ou_var_binaria <- input$tipo_variavel == 0
        estimar_ou_var_binaria
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


              if (estimar_ou_var_binaria()) {
                fluidPage(fluidRow(
                  textInput(inputId = ns("nome_grupo_tratamento"),
                            label   = translation_pss("Descreva um nome para o grupo Tratamento", linguagem()),
                            value   = ifelse(input$mudar_nomes == 0, translation_pss("Tratamento", linguagem()), nome_grupo_tratamento())),

                  HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamado de grupo Intervenção ou grupo Exposto.</i><br><br>"),

                  textInput(inputId = ns("nome_grupo_controle"),
                            label   = translation_pss("Descreva um nome para o grupo Controle", linguagem()),
                            value   = ifelse(input$mudar_nomes == 0, translation_pss("Controle", linguagem()), nome_grupo_controle())),

                  HTML("<i>Em alguns estudos o grupo Controle também pode ser chamado de grupo Placebo/ Sham ou grupo Não exposto.</i>")
                ))
              } else {

                textInput(inputId = ns("nome_preditora"),
                          label   = translation_pss("Descreva o nome da variável preditora", linguagem()),
                          value   = ifelse(input$mudar_nomes == 0, "X", nome_preditora()))
              }

            ),
            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      nome_grupo_controle <- reactive({
        ifelse(is.null(input$nome_grupo_controle), translation_pss("Controle", linguagem()), input$nome_grupo_controle)
      })

      nome_grupo_tratamento <- reactive({
        ifelse(is.null(input$nome_grupo_tratamento), translation_pss("Tratamento", linguagem()), input$nome_grupo_tratamento)
      })

      nome_desfecho <- reactive({
        ifelse(is.null(input$nome_desfecho), "Y", input$nome_desfecho)
      })

      nome_preditora <- reactive({
        ifelse(is.null(input$nome_preditora), "X", input$nome_preditora)
      })





      # % ou RC ou RR -----

      output$estatistica_tratamentoUi <- renderUI({

        if (estimar_ou_var_binaria()) {
          tagList(
            numericInput( ns("perc_controle"),
                          paste0("% ", translation_pss("de", linguagem()), " ",
                                 nome_desfecho(), " ",
                                 translation_pss("no", linguagem()), " ",
                                 nome_grupo_controle()
                          ),
                          value = 15,
                          min = 0,
                          max = 100,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perc_esperado),

            wellPanel(
              selectInput(
                ns('estatistica_tratamento'),
                if (linguagem() == "pt") {
                  paste0('Medida do grupo ', nome_grupo_tratamento())
                } else {
                  paste0(nome_grupo_tratamento(), " group measurement")
                },
                choices = opcoes_medidas(),
                selected = 'odds'
              ),

              uiOutput(ns("render_input_medida_controle"))
            )
          )

        } else {

          tagList(
            numericInput( ns("rc_continua"),
                          translation_pss("Razão de chance", linguagem()),
                          value = 2,
                          min = 0,
                          max = Inf,
                          step = 0.1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_razao_chance, title = translation_pss("Razão de chance", linguagem())),

            numericInput( ns("logistic_rate_mean"),
                          ifelse(linguagem() == "pt",
                                 paste0(
                                   "% de ",
                                   nome_desfecho(),
                                   " na média de ",
                                   nome_preditora()
                                 ),
                                 paste0(
                                   "% of ",
                                   nome_desfecho(),
                                   " at the mean of ",
                                   nome_preditora()
                                 )
                          ),
                          value = 50,
                          min  = 0,
                          max  = 100,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = paste0("O percentual de eventos esperados na média da variável preditora contínua.", txt_ajuda()$txt_definido_pesquisador_OU_literatura))
          )
        }
      })


      opcoes_medidas <- reactive({

        nomes_opcoes_medida <- c(
          paste0(
            translation_pss("Razão de chance", linguagem()),
            " (", nome_grupo_tratamento(), "/", nome_grupo_controle(), ")"
          ),
          paste0(
            translation_pss("Risco relativo", linguagem()),
            " (", nome_grupo_tratamento(), "/", nome_grupo_controle(), ")"
          ),
          paste0(
            translation_pss("% esperado no grupo", linguagem()),
            " ", nome_grupo_tratamento()
          )
        )
        opcoes_medida <- c('odds', 'ratio', "percent")
        names(opcoes_medida) <- nomes_opcoes_medida

        opcoes_medida
      })





      output$render_input_medida_controle <- renderUI({

        req(!is.null(input$estatistica_tratamento))

        tagList(
          if (input$estatistica_tratamento == "percent") {
            numericInput( ns("perc_tratamento"),
                          paste0(
                            translation_pss("% esperado no grupo", linguagem()),
                            " ", nome_grupo_tratamento()
                          ),
                          value = 45,
                          min = 0,
                          max = 100,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perc_esperado)

          } else if (input$estatistica_tratamento == "ratio") {
            numericInput( ns("rr"),
                          translation_pss("Risco relativo", linguagem()),
                          value = 2,
                          min = 0,
                          max = Inf,
                          step = 0.1
            )  %>% .help_buttom(linguagem = linguagem(),
              body = txt_ajuda()$txt_risco_relativo,
              title = translation_pss("Risco relativo", linguagem())
            )
          } else {
            numericInput( ns("rc"),
                          translation_pss("Razão de chance", linguagem()),
                          value = 2,
                          min = 0,
                          max = Inf,
                          step = 0.1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_razao_chance, title = translation_pss("Razão de chance", linguagem()))
          }

        )
      })



      # Balancemento ----

      output$balanceamentoUi <- renderUI({

        if (estimar_ou_var_binaria() & tipo != "poder") {

          numericInput( ns("balanceamento"),
                        paste0(
                          translation_pss("Balanceamento", linguagem()),
                          " (", nome_grupo_controle(), ":", nome_grupo_tratamento(), ")"
                        ),
                        value = 1,
                        min   = 0,
                        max   = Inf,
                        step  = .5
          ) %>% .help_buttom(linguagem = linguagem(), txt_balanceamento_f(nome_grupo_controle(), nome_grupo_tratamento()),
                             title = translation_pss("Balanceamento", linguagem()))
        } else if (input$tipo_variavel == 0 & tipo == "poder") {

          tagList(
            HTML("<b><font size = '2.95'>",
                 translation_pss("Tamanho amostral", linguagem()),
                 "</font></b><br>"
            ),
            div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                numericInput( ns("n_controle"),
                              nome_grupo_controle(),
                              value = 35,
                              min = 0,
                              step = 1
                )
            ),
            div(style = "display: inline-block;vertical-align:top; width: 49%;",
                numericInput( ns("n_tratamento"),
                              nome_grupo_tratamento(),
                              value = 60,
                              min = 0,
                              step = 1
                )
            ),
          )
        } else if (input$tipo_variavel == 1 & tipo == "poder") {
          numericInput( ns("n_total"),
                        translation_pss("Tamanho amostral", linguagem()),
                        value = 60,
                        min = 0,
                        step = 1
          )
        }
      })




      # Testar -----

      output$texto_principal <- renderText({

        req(tipo == "tamanho_amostral")
        req(!is.null(input$tipo_variavel))

        if (input$tipo_variavel == 1) {
          req(!is.null(input$logistic_rate_mean))

          code <- paste0(
            "powerMediation::SSizeLogisticCon(",
            "p1 = ", input$logistic_rate_mean, "/100, ",
            "OR = ", input$rc_continua, ", ",
            "alpha = ", input$alpha, "/100, ",
            "power = ", input$poder, "/100)"
          )
          # print_r_code(code)

          n <- try_n(code)
          eval(parse(text = validate_n("n")))
          n <- ceiling(n)
          eval(parse(text = validate_n_inf("n")))


          paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
                 "</font></b></br></br>",

                 lista_de_funcoes_server()$sugestao_texto_portugues(
                   "<i>",
                   translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


                   "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",
                   "para testar se a razão de chances para desenvolver <i>", nome_desfecho(), "</i> a cada acréscimo de uma unidade em <i>", nome_preditora(), "</i> é diferente de 1 ",
                   "(com o acréscimo de ", input$perc_perdas, "% para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",
                   "O cálculo considerou um poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>, ",
                   "percentual de <i>", nome_desfecho(), "</i> na média de <i>", nome_preditora(), "</i> de <b>", input$logistic_rate_mean, "%</b> ",
                   "e razão de chances esperada de <b>", input$rc_continua, "</b> como é referida em Fulano (1900) OU escolha do pesquisador. ",
                   .txt_citacao_pss
                 ),
                   .txt_referencia_tap,
                 print_r_code(code)
          )



          # Se for variavel categorica!
        } else {

          req(!is.null(input$estatistica_tratamento))
          req(!is.null(input$rc))

          if (input$estatistica_tratamento == "percent") {

            p2 <- paste0(input$perc_tratamento, "/100")
            text_just <- paste0("percentual de <i>", nome_desfecho(), "</i> no <i>", nome_grupo_tratamento(), "</i> de <b>", input$perc_tratamento, "% </b>",
                                "e no <i>", nome_grupo_controle(), "</i> de <b>", input$perc_controle, "%</b> como é referida em Fulano (1900). ")

          } else if (input$estatistica_tratamento == "ratio") {

            p2 <- paste0("(", input$perc_controle, "/100)*", input$rr)
            text_just <- paste0("percentual de <i>", nome_desfecho(), "</i> no <i>", nome_grupo_controle(), "</i> de <b>", input$perc_controle, "% </b>",
                                "e risco relativo de <b>", input$rr, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. ")

          } else {

            prob_control <- paste0(input$perc_controle, "/100")
            p2 <- paste0("(", input$rc, "*", prob_control, ") / (1 + ", input$rc, "*", prob_control, " - ", prob_control, ")")
            text_just <- paste0("percentual de <i>", nome_desfecho(), "</i> no <i>", nome_grupo_controle(), "</i> de <b>", input$perc_controle, "% </b>",
                                "e razão de chance de <b>", input$rc, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. ")
          }


          probs <- input$balanceamento/(1 + input$balanceamento)

          code <- paste0("powerMediation::SSizeLogisticBin(",
                         # pr(diseased | X = 0)
                         "p1 = ", input$perc_controle, "/100, ",
                         # pr(diseased | X = 1)
                         "p2 = ", p2, ", ",
                         # pr(X = 1)
                         "B  = ", input$balanceamento, "/(1 + ", input$balanceamento, "), ",
                         "alpha = ", input$alpha, "/100, ",
                         "power = ", input$poder, "/100)")
          # print_r_code(code)
          n <- try_n(code)
          eval(parse(text = validate_n("n")))


          n1 <- ceiling(n*(1 - probs))
          n2 <- ceiling(n*probs)
          n <- n1 + n2
          eval(parse(text = validate_n_inf("n")))

          nperdas1 <- n_perdas(n1, input$perc_perdas)
          nperdas2 <- n_perdas(n2, input$perc_perdas)

         paste0(
            "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
            if (n1 != n2) {
              paste0(
                " (<i>", n2, " ", nome_grupo_controle(), translation_pss(" e ", linguagem()), n1, " ", nome_grupo_tratamento(), "</i>)"
              )
            } else {
              paste0(
                " (<i>", n1, " ", translation_pss("para cada grupo", linguagem()), "</i>)"
              )
            },
            "</font></b></br></br>",

            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>",
              translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


              "Foi calculado um tamanho de amostra de <b>", n , "</b> sujeitos, sendo <b>",
              if (probs == 0.5) {
                paste0(
                  n1, "</b> em cada grupo, "
                )
              } else {
                paste0(
                  n2, "</b> no <i>", nome_grupo_controle(), "</i> e <b>", n1, "</b> no <i>", nome_grupo_tratamento(), "</i>, "
                )
              },

              "para testar se a razão de chances para desenvolver <i>",
              nome_desfecho(), "</i> entre os grupos <i>",
              nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(),
              "</i> é diferente de 1 ",

              if (probs == 0.5) {
                paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ")
              } else {
                paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser ", nperdas1, " <i>", nome_grupo_tratamento(), "</i> e ", nperdas2, " <i>", nome_grupo_controle(), "</i>). ")
              },

              "O cálculo considerou um poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>, ",
              text_just,

              .txt_citacao_pss
            ),
            .txt_referencia_tap,
            print_r_code(code)
          )



        }
      })



      # Poder ----

      output$texto_principal_poder <- renderText({

        req(tipo == "poder")

        if (input$tipo_variavel == 1) {

          code <- paste0(
            "powerMediation::powerLogisticCon(",
            "p1 = ", input$logistic_rate_mean, "/100, ",
            "OR = ", input$rc_continua, ", ",
            "alpha = ", input$alpha, "/100, ",
            "n = ", input$n_total, ")"
          )

          poder <- eval(parse(text = code))

          paste0(
            "<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", round(poder*100, digits = 1),
            "%</font></b></br></br>",

            lista_de_funcoes_server()$sugestao_texto_portugues(
              # "<i>",
              "O poder para testar se a razão de chances para desenvolver <i>", nome_desfecho(), "</i> a cada acréscimo de uma unidade em <i>", nome_preditora(), "</i> é diferente de 1 é <b>",
              round(poder*100, digits = 1), "%</b>. ",

              "Este valor foi obtido considerando nível de significância de <b>", input$alpha, "%</b>, ",
              "tamanho amostral igual a <b>", input$n_total, "</b> sujeitos, ",
              "percentual de <i>", nome_desfecho(), "</i> na média de <i>", nome_preditora(), "</i> de <b>", input$logistic_rate_mean, "%</b> ",
              "e razão de chances esperada de <b>", input$rc_continua, "</b> como é referida em Fulano (1900) OU escolha do pesquisador. ",
              .txt_citacao_pss
            ),
            .txt_referencia_tap,
            print_r_code(code)
          )




          # Categorico
        } else {


          if (input$estatistica_tratamento == "percent") {

            p2 <- paste0(input$perc_tratamento, "/100")
            text_just <- ifelse(
              input$perc_controle != input$perc_tratamento,
              paste0(" percentual de <i>", nome_desfecho(), "</i> no <i>", nome_grupo_tratamento(), "</i> de <b>", input$perc_tratamento, "% </b>",
                     " no <i>", nome_grupo_controle(), "</i> de <b>", input$perc_controle, "%</b> como é referida em Fulano (1900). "
              ),
              paste0(" percentual de <i>", nome_desfecho(), "</i> de <b>", input$perc_tratamento, "% </b>",
                     " em ambos grupos como é referida em Fulano (1900). "
              )
            )

          } else if (input$estatistica_tratamento == "ratio") {

            p2 <- paste0("(", input$perc_controle, "/100)*", input$rr)
            text_just <- paste0(" percentual de <i>", nome_desfecho(), "</i> no <i>", nome_grupo_controle(), "</i> de <b>", input$perc_controle, "% </b>",
                                "e risco relativo de <b>", input$rr, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. ")

          } else {

            prob_control <- paste0(input$perc_controle, "/100")
            p2 <- paste0("(", input$rc, "*", prob_control, ") / (1 + ", input$rc, "*", prob_control, " - ", prob_control, ")")
            text_just <- paste0(" percentual de <i>", nome_desfecho(), "</i> no <i>", nome_grupo_controle(), "</i> de <b>", input$perc_controle, "% </b>",
                                "e razão de chance de <b>", input$rc, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. ")
          }


          code <- paste0(
            "powerMediation::powerLogisticBin(",
            "p1 = ", input$perc_controle, "/100, ",
            "p2 = ", p2, ", ",
            "B = ", input$n_controle, "/(", input$n_controle, " + ", input$n_tratamento, "),",
            "alpha = ", input$alpha, "/100, ",
            "n = ", input$n_controle, " + ", input$n_tratamento,")"
          )
          # print_r_code(code)
          poder <- eval(parse(text = code))

          paste0(
            "<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", round(poder*100, digits = 1),
            "%</font></b></br></br>",

            lista_de_funcoes_server()$sugestao_texto_portugues(
              # "<i>",
              "O poder para testar se a razão de chances para desenvolver <i>",
              nome_desfecho(), "</i> entre os grupos <i>",
              nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(),
              "</i> é diferente de 1 é <b>", round(poder*100, digits = 1), "%</b>. ",

              "Este valor foi obtido considerando nível de significância de <b>", input$alpha, "%</b>, ",
              if (input$n_tratamento == input$n_controle) {
                paste0("tamanho amostral igual a <b>", input$n_controle, "</b> sujeitos em cada grupo, ")
              } else {
                paste0(
                  "tamanho amostral igual a <b>", input$n_tratamento, "</b> sujeitos para o grupo <i>",
                  nome_grupo_tratamento(), "</i>, <b>", input$n_controle, "</b> sujeitos para o grupo <i>",
                  nome_grupo_controle(), "</i>, "
                )
              },
              text_just,

              .txt_citacao_pss
            ),
            .txt_referencia_tap,
            print_r_code(code)
          )

        }

      })



      # Estimar ----

      output$texto_principal_estimar <- renderText({

        req(tipo == "estimar")

        req(!is.null(input$estatistica_tratamento))

        if (input$estatistica_tratamento == "percent") {

          p2 <- paste0(input$perc_tratamento, "/100")
          text_just <- ifelse(
            input$perc_controle != input$perc_tratamento,
            paste0(" percentual de <i>", nome_desfecho(), "</i> no <i>", nome_grupo_tratamento(), "</i> de <b>", input$perc_tratamento, "% </b>",
                   " no <i>", nome_grupo_controle(), "</i> de <b>", input$perc_controle, "%</b> como é referida em Fulano (1900). "
            ),
            paste0(" percentual de <i>", nome_desfecho(), "</i> de <b>", input$perc_tratamento, "% </b>",
                   " em ambos grupos como é referida em Fulano (1900). "
            )
          )

        } else if (input$estatistica_tratamento == "ratio") {

          p2 <- paste0("(", input$perc_controle, "/100)*", input$rr)
          text_just <- paste0(" percentual de <i>", nome_desfecho(), "</i> no <i>", nome_grupo_controle(), "</i> de <b>", input$perc_controle, "%</b> ",
                              "e risco relativo de <b>", input$rr, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. ")

        } else {
          req(!is.null(input$rc))

          prob_control <- paste0(input$perc_controle, "/100")
          p2 <- paste0("(", input$rc, "*", prob_control, ") / (1 + ", input$rc, "*", prob_control, " - ", prob_control, ")")
          text_just <- paste0(" percentual de <i>", nome_desfecho(), "</i> no <i>", nome_grupo_controle(), "</i> de <b>", input$perc_controle, "%</b> ",
                              "e razão de chance de <b>", input$rc, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. ")
        }

        req(!is.null(input$amplitude))

        code <- paste0("presize::prec_or(",
                       "p1 = ", p2, ", ",
                       "p2 = ", input$perc_controle, "/100, ",
                       "conf.width = ", input$amplitude, ", ",
                       "r  = ", input$balanceamento, ", ",
                       "conf.level = ", input$confianca, "/100, ",
                       "method = '", input$metodo_estimacao, "')"
        )

        # print_r_code(code)
        n <- try_n(code)
        eval(parse(text = validate_n_inf("n")))

        n1 <- ceiling(n$n1)
        n2 <- ceiling(n$n2)
        n <- n1 + n2

        eval(parse(text = validate_n("n")))
        eval(parse(text = validate_n_inf("n")))


        nperdas1 <- n_perdas(n1, input$perc_perdas)
        nperdas2 <- n_perdas(n2, input$perc_perdas)

        metodo <- case_when(
          input$metodo_estimacao == "indip_smooth" ~ "Logit suavizado",
          input$metodo_estimacao == "gart" ~ "de Gart",
          input$metodo_estimacao == "woolf" ~ "de Woolf"
        )

        paste0(
          "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
          if (n1 != n2) {
            paste0(
              " (<i>", n1, " ", nome_grupo_tratamento(), translation_pss(" e ", linguagem()), n2, " ", nome_grupo_controle(), "</i>)"
            )
          } else {
            paste0(
              " (<i>", n1, " ", translation_pss("para cada grupo", linguagem()), "</i>)"
            )
          },
          "</font></b></br></br>",

          lista_de_funcoes_server()$sugestao_texto_portugues(
            "<i>",
            translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


            "Foi calculado um tamanho de amostra de <b>", n , "</b> sujeitos, sendo <b>",
            if (input$balanceamento == 1) {
              paste0(
                n1, "</b> em cada grupo, "
              )
            } else {
              paste0(
                n1, "</b> no <i>", nome_grupo_tratamento(), "</i> e <b>", n2, "</b> no <i>", nome_grupo_controle(), "</i>, "
              )
            },

            "para estimar a razão de chances de desenvolver <i>",
            nome_desfecho(), "</i> entre os grupos <i>",
            nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(),
            "</i>, ",
            "com amplitude desejada para o intervalo de confiança de <b>", input$amplitude, "</b> ",

            if (input$balanceamento == 1) {
              paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ")
            } else {
              paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser ", nperdas1, " <i>", nome_grupo_tratamento(), "</i> e ", nperdas2, " <i>", nome_grupo_controle(), "</i>). ")
            },

            "O cálculo (utilizando o método ", metodo, ") ",
            "considerou nível de confiança de <b>", input$confianca, "%</b>, ",
            text_just,
            .txt_citacao_pss
          ),

          .txt_referencia_tap,
          print_r_code(code)
        )

      })





      # Cenarios ----


      output$cenarios <- renderUI({

        req(tipo == "tamanho_amostral")
        # req(input$tipo_variavel == 0)

        req(!is.null(input$estatistica_tratamento))

        if (input$tipo_variavel == 1) {
          req(!is.null(input$rc_continua))
          razao_usada <- input$rc_continua

          if (razao_usada > 1) {
            ratio_start <- razao_usada
            ratio_end  <- razao_usada + 1
            ratio_by   <- 0.1
            ratio_max  <- Inf
          } else{
            ratio_start <- max(0, razao_usada - 0.3)
            ratio_end  <- razao_usada
            ratio_by   <- 0.05
            ratio_max  <- Inf
          }

        } else if (input$estatistica_tratamento == 'percent') {
          req(!is.null(input$perc_tratamento))
          ratio_start <- min(c(5, input$perc_tratamento))
          ratio_end  <- max(c(95, input$perc_tratamento))
          ratio_by   <- 10
          ratio_max  <- 100
        } else {
          razao_usada <- ifelse(input$estatistica_tratamento == 'ratio', input$rr, input$rc)

          if (razao_usada > 1) {
            ratio_start <- razao_usada
            ratio_end  <- razao_usada + 1
            ratio_by   <- 0.1
            ratio_max  <- Inf
          } else{
            ratio_start <- max(0, razao_usada - 0.3)
            ratio_end  <- razao_usada
            ratio_by   <- 0.05
            ratio_max  <- Inf
          }
        }


        fluidPage(fluidRow(
          br(),
          HTML('<hr style="color: black;">'),
          br(),br(),
          titlePanel(translation_pss("Construção de cenários", linguagem())),
          br(),
          wellPanel(translation_pss(
            "Utilize os argumentos abaixo para construir diferentes cenários. Demais informações serão recuperadas do painel lateral.",
            linguagem())
          ),

          if (input$tipo_variavel == 1) {
            HTML(
              paste0("<b>", translation_pss("Defina a sequência da razão de chances", linguagem()), "</b>")
            )
          } else if (input$estatistica_tratamento == 'percent') {
            HTML(
              paste0("<b>", translation_pss("Defina a sequência de valores (%) para o grupo", linguagem()) , nome_grupo_tratamento(), "</b>")
            )
          } else if (input$estatistica_tratamento == 'ratio') {
            HTML(
              paste0("<b>", translation_pss("Defina a sequência do risco relativo", linguagem()),"</b>")
            )
          } else {
            HTML(
              paste0("<b>", translation_pss("Defina a sequência da razão de chances", linguagem()), "</b>")
            )
          },


          br(),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput(ns("from"), translation_pss("Mínimo", linguagem()), value = ratio_start, step = .1, min = 0, max = ratio_max)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("to"), translation_pss("Máximo", linguagem()), value = ratio_end, step = .1, min = 0, max = ratio_max)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("by"), translation_pss("Intervalo", linguagem()), value = ratio_by, min = 0, step = .1) %>%
                .help_buttom(linguagem = linguagem(), body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()),
                             title = "Sequência")
          ),


          fluidRow(
            column(6,
                   textInput(inputId = ns("poder_cenarios"),
                             label   = translation_pss("Digite valores de poder (%) para fazer o gráfico", linguagem()),
                             value   = "80, 90, 95",
                             width   = "400px") %>%
                     .help_buttom(linguagem = linguagem(), body = ajuda_cenarios_multiplos_valores())
            )
          ),

          plotly::plotlyOutput(ns("grafico_cenarios"), width = "80%") %>%
            shinycssloaders::withSpinner(type = 5),

          br(), br(),
          downloadButton(ns("download_tabela_cenarios"), translation_pss("Download tabela", linguagem())),
          DT::dataTableOutput(ns("tabela_cenarios"), width = "100%") %>%
            shinycssloaders::withSpinner(type = 5)

        ))

      })



      eval(parse(text = check_text_input_to_vector("poder_cenarios")))



      tab_p2_TH_cenarios <- reactive({

        poder <- text_input_to_vector(input$poder_cenarios)
        req(length(poder) > 0)

        if (input$tipo_variavel == 1) {

          rrrr <- NA
          odssss <- seq(from = input$from, to = input$to, by = input$by)
          prob_control <- NA
          prop_tratamento <- odssss

        } else if (input$estatistica_tratamento == "percent") {
          rrrr <- NA
          odssss <- NA
          prop_tratamento <- seq(from = input$from, to = input$to, by = input$by)

        } else if (input$estatistica_tratamento == "ratio") {

          rrrr <- seq(from = input$from, to = input$to, by = input$by)
          odssss <- NA
          prop_tratamento <- input$perc_controle*rrrr

        } else {

          rrrr <- NA
          odssss <- seq(from = input$from, to = input$to, by = input$by)
          prob_control <- input$perc_controle/100
          prop_tratamento <- (odssss*prob_control)/ (1 + odssss*prob_control - prob_control) * 100
        }


        df_inputs_prop <- tibble::tibble(
          `Risco relativo` = rrrr,
          `Razão de chance` = odssss,
          prop_tratamento
        )


        simul_n <- expand.grid(`% Controle`   = ifelse(input$tipo_variavel == 0,
                                                       input$perc_controle,
                                                       input$logistic_rate_mean),
                               `% Tratamento` = prop_tratamento,
                               `Nível de significância (%)` = input$alpha,
                               `Poder (%)`   = poder,
                               Balanceamento = input$balanceamento,
                               stringsAsFactors = FALSE) %>%
          dplyr::filter(`% Tratamento` != `% Controle`)

        if (input$tipo_variavel == 0) {

          simul_n <- simul_n %>%
            mutate(
              probs = Balanceamento/(1 + Balanceamento),

              ntemp = powerMediation::SSizeLogisticBin(
                p1 = `% Controle`/100,
                p2 = `% Tratamento`/100,
                B = probs,
                alpha = `Nível de significância (%)`/100,
                power = `Poder (%)`/100
              ),

              `n Tratamento` = ceiling(ntemp*(1 - probs)),
              `n Controle`   = ceiling(ntemp*probs),
              `n total` = `n Tratamento` + `n Controle`
            ) %>%

            dplyr::filter(!is.na(`n Tratamento`) & !is.na(`n Controle`))

        } else {

          simul_n <- simul_n %>%
            mutate(
              `n total` = powerMediation::SSizeLogisticCon(
                p1 = input$logistic_rate_mean/100,
                OR = `% Tratamento`,
                alpha = `Nível de significância (%)`/100,
                power = `Poder (%)`/100
              ),

              `n Tratamento` = NA,
              `n Controle` = NA
            )

        }

        simul_n %>%
          left_join(df_inputs_prop, by = c("% Tratamento" = "prop_tratamento"))


      })


      output$grafico_cenarios <- plotly::renderPlotly({

        metrica <- case_when(
          input$tipo_variavel == 1 ~ "Razão de chance",
          input$estatistica_tratamento == 'percent' ~ "% Tratamento",
          input$estatistica_tratamento == 'ratio'   ~ "Risco relativo",
          TRUE ~ "Razão de chance"
        )

        xlab <- case_when(
          input$tipo_variavel == 1 ~ translation_pss("Razão de chance", linguagem()),
          input$estatistica_tratamento == 'percent' ~ paste0("% ", nome_grupo_tratamento()),
          input$estatistica_tratamento == 'ratio'   ~ translation_pss("Risco relativo", linguagem()),
          TRUE ~ translation_pss("Razão de chance", linguagem())
        )

        g1 <- tab_p2_TH_cenarios() %>%
          mutate(
            `Poder (%)` = factor(`Poder (%)`)
          ) %>%
          ggplot(
            aes(x = !! sym(metrica),
                y = `n total`,
                color = `Poder (%)`,
                Tratamento = `n Tratamento`,
                Controle   = `n Controle`
            )
          ) +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = seq(from = input$from, to = input$to, by = input$by)) +
          xlab(xlab) +
          ylab(translation_pss("Tamanho da amostra*", linguagem())) +
          theme_bw() +
          theme(axis.text = element_text(colour = "black")) +
          scale_color_brewer(name = translation_pss("Poder (%)", linguagem()), palette = "Set1")


        plotly::ggplotly(g1,
                         tooltip = c("x", "colour", "y", translation_pss("Tratamento", linguagem()), translation_pss("Controle", linguagem()))) %>%
          plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                            showarrow = F, xref='paper', yref='paper',
                                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                            font=list(size=10)))
      })








      return_table_tabela_cenarios <- reactive({

        metrica <- case_when(
          input$tipo_variavel == 1 ~ "Razão de chance",
          input$estatistica_tratamento == 'percent' ~ "% Tratamento",
          input$estatistica_tratamento == 'ratio'   ~ "Risco relativo",
          TRUE ~ "Razão de chance"
        )

        xlabb <- case_when(
          input$tipo_variavel == 1 ~ translation_pss("Razão de chance", linguagem()),
          input$estatistica_tratamento == 'percent' ~ paste0("% ", nome_grupo_tratamento()),
          input$estatistica_tratamento == 'ratio'   ~ translation_pss("Risco relativo", linguagem()),
          TRUE ~ translation_pss("Razão de chance", linguagem())
        )

        df_ <- tab_p2_TH_cenarios() %>%
          dplyr::select(
            c("% Controle",
              all_of(metrica),
              "n total",
              "n Tratamento",
              "n Controle",
              "Nível de significância (%)",
              "Poder (%)",
              "Balanceamento"
            )
          )

        colnames(df_) <- c(
          ifelse (input$tipo_variavel == 1,
                  ifelse(linguagem() == "pt",
                         paste0(
                           "% de ",
                           nome_desfecho(),
                           " na média de ",
                           nome_preditora()
                         ),
                         paste0(
                           "% of ",
                           nome_desfecho(),
                           " at the mean of ",
                           nome_preditora()
                         )
                  ),
                  paste0("% ", nome_grupo_controle())
          ),
          xlabb,
          translation_pss("Tamanho amostral", linguagem()),
          paste0("n ", nome_grupo_tratamento()),
          paste0("n ", nome_grupo_controle()),
          translation_pss("Nível de significância (%)", linguagem()),
          translation_pss("Poder (%)", linguagem()),
          translation_pss("Balanceamento", linguagem())
        )

        if (input$tipo_variavel == 1) {
          df_ %>%
            select(
              -c(
                paste0("n ", nome_grupo_tratamento()),
                paste0("n ", nome_grupo_controle()),
                translation_pss("Balanceamento", linguagem())
              )
            )
        } else {
          df_
        }

      })



      output$tabela_cenarios <- DT::renderDataTable({

        return_table_tabela_cenarios() %>%
          DT::datatable(extensions = c('FixedColumns'),
                        rownames   = FALSE,
                        filter     = "none",
                        options    = list(pageLength = 10,
                                          scrollX = TRUE,
                                          scrollY = TRUE,
                                          searching = FALSE,
                                          fixedColumns = list(leftColumns = 1),
                                          dom = 'B<"dwnld">frtip'
                        )
          )
      })


      output$download_tabela_cenarios <- downloadHandler(
        filename = function() { "Cenarios_tamanho_amostra_regressao_logistica.xlsx"},
        content = function(file) {writexl::write_xlsx(return_table_tabela_cenarios(),
                                                      path = file)}
      )







    } # Nao mexer!!!
  )
}




