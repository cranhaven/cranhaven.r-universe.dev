
mod_nao_inferioridade_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_nao_inferioridade_server <- function(id, tipo = "tamanho_amostral", tipo_variavel = "c",
                                         txt_ajuda, txt_balanceamento_f,
                                         translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                         warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas, warning_numero,
                                         lista_de_funcoes_server){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      eval(parse(text = warning_numero("margem")))
      eval(parse(text = warning_numero("diferenca_para_detectar")))
      eval(parse(text = warning_numero_positivo("sigma")))
      eval(parse(text = warning_prop("perc_controle")))
      eval(parse(text = warning_prop("perc_tratamento")))
      eval(parse(text = warning_numero_positivo("rr")))
      eval(parse(text = warning_numero_positivo("rc")))

      eval(parse(text = warning_numero_positivo("balanceamento")))
      eval(parse(text = warning_prop("poder")))
      eval(parse(text = warning_prop("alpha")))
      eval(parse(text = warning_prop("confianca")))
      eval(parse(text = warning_perdas("perc_perdas")))
      eval(parse(text = warning_inteiro("n_controle")))
      eval(parse(text = warning_inteiro("n_tratamento")))



      # Aba ----


      opcoes_testes_equivalencia <- reactive({
        teste <- c("inf", "equi", "sup")

        if (linguagem() == "pt") {
          names(teste) <- c(
            paste0("Não inferioridade de ", nome_grupo_tratamento()),
            paste0("Equivalência de ",  nome_grupo_tratamento()),
            paste0("Superioridade de ", nome_grupo_tratamento())
          )
        } else {
          names(teste) <- c(
            paste0("Non-inferiority of ", nome_grupo_tratamento()),
            paste0("Equivalence of ", nome_grupo_tratamento()),
            paste0("Superiority of ", nome_grupo_tratamento())
          )
        }
        teste
      })






      output$aba <- renderUI({


        sidebarLayout(
          sidebarPanel(

            selectInput(
              ns("opcoes_teste"),
              translation_pss('Tipo de teste', linguagem()),
              choices = opcoes_testes_equivalencia(),
              selected = 'inf'
            ),
              # .help_buttom(linguagem = linguagem(),
              #   body = txt_ajuda()$txt_confianca,
              #   title = translation_pss("Nível de confiança (%)", linguagem())
              # ),
            uiOutput(ns("input_Ui"))
          ),

          mainPanel(
            htmlOutput(ns("texto_principal")) %>%
              shinycssloaders::withSpinner(type = 5)
          )
        )
      })


      output$input_Ui <- renderUI({

        valor_margem_padrao <- case_when(
          input$opcoes_teste == "inf"  & tipo_variavel == "c" ~ -1,
          input$opcoes_teste == "equi" & tipo_variavel == "c" ~ 1,
          input$opcoes_teste == "sup"  & tipo_variavel == "c" ~ 1,
          input$opcoes_teste == "inf"  & tipo_variavel == "b" ~ -10,
          input$opcoes_teste == "equi" & tipo_variavel == "b" ~ 15,
          input$opcoes_teste == "sup"  & tipo_variavel == "b" ~ 10
        )

        valor_margem_max <- case_when(
          input$opcoes_teste == "inf"  & tipo_variavel == "c" ~ 0,
          input$opcoes_teste == "equi" & tipo_variavel == "c" ~ Inf,
          input$opcoes_teste == "sup"  & tipo_variavel == "c" ~ Inf,
          input$opcoes_teste == "inf"  & tipo_variavel == "b" ~ 0,
          input$opcoes_teste == "equi" & tipo_variavel == "b" ~ 100,
          input$opcoes_teste == "sup"  & tipo_variavel == "b" ~ 100
        )

        valor_margem_min <- case_when(
          input$opcoes_teste == "inf"  & tipo_variavel == "c" ~ -Inf,
          input$opcoes_teste == "equi" & tipo_variavel == "c" ~ -Inf,
          input$opcoes_teste == "sup"  & tipo_variavel == "c" ~ 0,
          input$opcoes_teste == "inf"  & tipo_variavel == "b" ~ -100,
          input$opcoes_teste == "equi" & tipo_variavel == "b" ~ 0,
          input$opcoes_teste == "sup"  & tipo_variavel == "b" ~ -100
        )


        diferenca_esperada <- case_when(
          input$opcoes_teste == "inf"  & tipo_variavel == "c" ~ -0.5,
          input$opcoes_teste == "equi" & tipo_variavel == "c" ~ 0.5,
          input$opcoes_teste == "sup"  & tipo_variavel == "c" ~ 0.5
        )




        tagList(
          wellPanel(
            HTML(
              paste0(
                "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
              )
            ),
            uiOutput(ns("th_h0")),
            uiOutput(ns("th_h1"))
          ),

          actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),
          br(), br(),

          numericInput( ns("margem"),
                        paste0(
                          translation_pss("Margem de", linguagem()),
                          " ", tolower(teste_escolhido()), " (",
                          translation_pss("em", linguagem()), " ",
                          ifelse(tipo_variavel == "c", unidade_medida(), "%"),
                          ")"
                        ),
                        value = valor_margem_padrao,
                        min   = valor_margem_min,
                        max   = valor_margem_max,
                        step  = 1) %>%
            .help_buttom(linguagem = linguagem(),
              body = case_when(input$opcoes_teste == "inf"  ~ txt_ajuda()$txt_margem_nao_inferior,
                               input$opcoes_teste == "sup"  ~ txt_ajuda()$txt_margem_superior,
                               TRUE ~ txt_ajuda()$txt_margem_equivalencia),
              title = paste0("Margem de ", teste_escolhido())
            ),



          if ( tipo_variavel == "c") {
            tagList(
              numericInput( ns("diferenca_para_detectar"),
                            paste0(
                              translation_pss("Diferença esperada", linguagem()), " (",
                              nome_grupo_tratamento(), " - ", nome_grupo_controle(), ")"
                            ),
                            value = diferenca_esperada,
                            min = -Inf,
                            max = Inf,
                            step = .5
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_diferenca_clinica, title = translation_pss("Diferença mínima a ser detectada", linguagem())),

              numericInput( ns("sigma"),
                            translation_pss("Desvio padrão", linguagem()),
                            value = 1.2,
                            min = 0,
                            max = Inf,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão", linguagem())),

              actionLink(ns("show_desvio_combinado"), translation_pss("Calcular o desvio padrão combinado", linguagem())),
              br(), br()
            )
          } else {
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
                  selected = 'percent'
                ),

                uiOutput(ns("render_input_medida_controle"))
              )
            )
          },


          if (tipo == "tamanho_amostral") {
            numericInput( ns("balanceamento"),
                          paste0(
                            translation_pss("Balanceamento", linguagem()),
                            " (", nome_grupo_tratamento(), ":", nome_grupo_controle(), ")"
                          ),
                          value = 1,
                          min   = 0,
                          max   = Inf,
                          step  = .5
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_balanceamento_f(nome_grupo_tratamento(), nome_grupo_controle()),
                               title = translation_pss("Balanceamento", linguagem()))
          },


          if (tipo == "poder") {
            tagList(
              HTML(
                "<b><font size = '2.95'>",
                translation_pss("Tamanho amostral", linguagem()),
                "</font></b><br>"
              ),
              div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                  numericInput( ns("n_tratamento"),
                                nome_grupo_tratamento(),
                                value = 60,
                                min = 0,
                                step = 1
                  )
              ),
              div(style = "display: inline-block;vertical-align:top; width: 49%;",
                  numericInput( ns("n_controle"),
                                nome_grupo_controle(),
                                value = 35,
                                min = 0,
                                step = 1
                  )
              )
            )
          } else {
            numericInput( ns("poder"),
                          translation_pss("Poder (%)", linguagem()),
                          value = 80,
                          min = 0,
                          max = 100,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
          },


          numericInput( ns("alpha"),
                        translation_pss("Nível de significância (%)", linguagem()),
                        value = 5,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem())),


          if (tipo %in% c("tamanho_amostral")) {
            numericInput( ns("perc_perdas"),
                          translation_pss("Perdas/ Recusas (%)", linguagem()),
                          value = 10,
                          min = 0,
                          max = 100,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
          }
        )
      })




      # Para var binaria ----

      # % ou RC ou RR

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

          } else if (input$estatistica_tratamento == "ratio"){
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


      # Opcoes testes -----

      teste_escolhido <- reactive({
        names(opcoes_testes_equivalencia()[opcoes_testes_equivalencia() == input$opcoes_teste])
      })






      # Desvio combinado ----


      observeEvent(input$show_desvio_combinado, {
        validate(need(input$show_desvio_combinado > 0, ''))

        showModal(
          modalDialog(
            fluidPage(

              HTML("<b><font size = '3'>", translation_pss("Desvio padrão", linguagem()), " do</font></b><br>"),

              div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                  numericInput( ns("sigma_A"),
                                nome_grupo_tratamento(),
                                value = 2.0,
                                min = 0,
                                max = Inf,
                                step = .5)),
              div(style="display: inline-block;vertical-align:top; width: 49%;",
                  numericInput( ns("sigma_B"),
                                nome_grupo_controle(),
                                value = 1.5,
                                min = 0,
                                max = Inf,
                                step = .5)),


              HTML(paste0("<b><font size = '3'>", translation_pss("Tamanho amostral", linguagem()), " do</font></b><br>")),
              div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                  numericInput( ns("n_A"),
                                nome_grupo_tratamento(),
                                value = 20,
                                min = 3,
                                max = Inf,
                                step = 1)),
              div(style="display: inline-block;vertical-align:top; width: 49%;",
                  numericInput( ns("n_B"),
                                nome_grupo_controle(),
                                value = 30,
                                min = 3,
                                max = Inf,
                                step = 1)),

              htmlOutput(ns("resultado")),

              p(translation_pss("Foi utilizado a fórmula", linguagem())),
              withMathJax(
                paste0(
                  "$$s_{", translation_pss("combinado", linguagem()), "} = \\sqrt{ \\dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} }$$"
                )
              )
            ),
            title = translation_pss("Obter o desvio padrão combinado", linguagem()),
            easyClose = TRUE,
            footer    = NULL,
            size      = "m"
          )
        )
      })


      output$resultado <- renderText({

        s2a <- input$sigma_A^2
        s2b <- input$sigma_B^2

        n1 <- input$n_A
        n2 <- input$n_B

        numerador   <- (n1 - 1)*s2a + (n2 - 1)*s2b
        denominador <- n1 + n2 - 2

        s_pooled <- sqrt(numerador/denominador)

        paste0("<br><br><b><font size = '5'>",
               "<i>", translation_pss("Desvio padrão", linguagem()), "<sub>", translation_pss("combinado", linguagem()), "</sub></i> = ", round(s_pooled, 4),
               "<br><br><br>")
      })





      # Hipoteses a serem testadas ----



      output$th_h0 <- renderUI({

        paramentro <- ifelse(tipo_variavel == "c", "\\mu_\\text{", "\\pi_\\text{")

        if (input$opcoes_teste %in% c("inf", "sup")) {
          withMathJax(
            paste0(
              "$$H_0: ", paramentro, nome_grupo_tratamento(), "} - ", paramentro, nome_grupo_controle(), "} \\leq", input$margem,
              if (tipo_variavel == "b") "\\text{%}",
              "$$"
            )
          )
        } else {
          withMathJax(
            paste0(
              "$$H_0: |", paramentro, nome_grupo_tratamento(), "} - ", paramentro, nome_grupo_controle(), "}| \\geq", input$margem,
              if (tipo_variavel == "b") "\\text{%}",
              "$$"
            )
          )
        }
      })

      output$th_h1 <- renderUI({
        paramentro <- ifelse(tipo_variavel == "c", "\\mu_\\text{", "\\pi_\\text{")

        if (input$opcoes_teste %in% c("inf", "sup")) {
          withMathJax(
            paste0(
              "$$H_1: ", paramentro, nome_grupo_tratamento(), "} - ", paramentro, nome_grupo_controle(), "} >", input$margem,
              if (tipo_variavel == "b") "\\text{%}",
              "$$"
            )
          )
        } else {
          withMathJax(
            paste0(
              "$$H_1: |", paramentro, nome_grupo_tratamento(), "} - ", paramentro, nome_grupo_controle(), "}| <", input$margem,
              if (tipo_variavel == "b") "\\text{%}",
              "$$"
            )
          )
        }
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


              if (tipo_variavel == "c") {
                tagList(
                  textInput(inputId = ns("unidade_medida"),
                            label   = paste0(translation_pss("Descreva a unidade de medida do desfecho", linguagem())),
                            value   = ifelse(input$mudar_nomes == 0, translation_pss("u.m.", linguagem()), unidade_medida())),
                  HTML("<i>", gsub("<br><br>", "", txt_ajuda()$txt_um), "</i>"),
                  br(), br()
                )
              },


              textInput(inputId = ns("nome_grupo_tratamento"),
                        label   = translation_pss("Descreva um nome para o grupo Tratamento", linguagem()),
                        value   = ifelse(input$mudar_nomes == 0, translation_pss("Tratamento", linguagem()), nome_grupo_tratamento())),

              HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamado de grupo Intervenção ou grupo Exposto.</i><br><br>"),

              textInput(inputId = ns("nome_grupo_controle"),
                        label   = translation_pss("Descreva um nome para o grupo Controle", linguagem()),
                        value   = ifelse(input$mudar_nomes == 0, translation_pss("Controle", linguagem()), nome_grupo_controle())),

              HTML("<i>Em alguns estudos o grupo Controle também pode ser chamado de grupo Placebo/ Sham ou grupo Não exposto.</i>"),

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

      unidade_medida <- reactive({
        ifelse(is.null(input$unidade_medida),  translation_pss("u.m.", linguagem()), input$unidade_medida)
      })



      # Checks! -----

      avisos_margem <- reactive({

        req(!is.null(input$opcoes_teste))
        req(!is.null(input$margem))
        req(!is.null(input$diferenca_para_detectar))
        req(!is.na(input$margem))
        req(!is.na(input$diferenca_para_detectar))


        mensagem_print <- NA

        if(input$opcoes_teste == "inf"){
          if(input$margem > 0){
            mensagem_print <- "A margem de não inferioridade deve ser um valor negativo."
          } else if(input$margem >= input$diferenca_para_detectar){
            mensagem_print <- "A margem de não inferioridade deve ser menor do que a diferença esperada."
          }
        } else if(input$opcoes_teste == "sup"){
          if(input$margem < 0){
            mensagem_print <- "A margem de superioridade deve ser um valor positivo."
          } else if(input$margem >= input$diferenca_para_detectar){
            mensagem_print <- "A margem de superioridade deve ser menor do que a diferença esperada."
          }
        } else if(input$opcoes_teste == "equi"){
          if(abs(input$margem) <= abs(input$diferenca_para_detectar)){
            mensagem_print <- "O módulo da margem de equivalência deve ser maior do que o módulo da diferença esperada."
          }
        }

        mensagem_print
      })


      observeEvent(c(input$opcoes_teste, input$margem, input$diferenca_para_detectar), {

        req(!is.null(avisos_margem()))

        if(!is.na(avisos_margem())){
          shinyFeedback::showFeedbackWarning(
            inputId = "margem",
            text = avisos_margem(),
            color = "red"
          )
        } else {
          shinyFeedback::hideFeedback("margem")
          shinyFeedback::hideFeedback("diferenca_para_detectar")
        }
      })




      # Texto -----


      output$texto_principal <- renderText({


        # browser()

        # prepara as informacoes das variaveis qualitaitvas
        if (tipo_variavel == "b") {
          req(!is.null(input$perc_tratamento))
          if (input$estatistica_tratamento == "percent") {

            p2 <- paste0(input$perc_tratamento, "/100")
            text_just <- paste0(
              " percentual de <b>", input$perc_tratamento, "%</b> para o grupo <i>", nome_grupo_tratamento(),
              "</i> e <b>", input$perc_controle, "%</b> para o grupo <i>", nome_grupo_controle(),
              "</i> (referido por Fulano (1900)). "
            )

          } else if (input$estatistica_tratamento == "ratio") {

            p2 <- paste0("(", input$perc_controle, "/100)*", input$rr)
            text_just <- paste0(
              " percentual de <b>", input$perc_tratamento, "%</b> para o grupo <i>", nome_grupo_tratamento(),
              "</i> e risco relativo de <b>", input$rr,
              "</b> para o grupo <i>", nome_grupo_controle(),
              "</i> (referido por Fulano (1900) <b>OU</b> escolha do pesquisador. "
            )

          } else {

            prob_control <- paste0(input$perc_controle, "/100")
            p2 <- paste0("(", input$rc, "*", prob_control, ") / (1 + ", input$rc, "*", prob_control, " - ", prob_control, ")")
            text_just <- paste0(
              " percentual de <b>", input$perc_tratamento, "%</b> para o grupo <i>", nome_grupo_tratamento(),
              "</i> e razão de chance de <b>", input$rc,
              "</b> para o grupo <i>", nome_grupo_controle(),
              "</i> (referido por Fulano (1900) <b>OU</b> escolha do pesquisador. "
            )
          }


          code <- paste0(
            if (input$opcoes_teste == "equi") {
              "epiR::epi.ssequb("
            } else if(input$opcoes_teste == "inf") {
              "epiR::epi.ssninfb("
            } else {
              "epiR::epi.sssupb("
            },
            "treat = ", p2, ", ",
            "control = ", input$perc_controle, "/100, ",
            "delta = abs(",  input$margem, "/100), ",
            if (tipo == "tamanho_amostral") {
              paste0(
                "n = NA, ",
                "r = ",  input$balanceamento, ", ",
                "power  = ",  input$poder, "/100, "
              )
            } else {
              paste0(
                "n = ", input$n_tratamento, " + ", input$n_controle, ", ",
                "r = ",  input$n_tratamento, " / ", input$n_controle, ", ",
                "power  = NA, "
              )
            },
            "alpha = ", input$alpha, "/100)"
          )

        } else {

          req(!is.null(input$sigma))

          code <- paste0(
            if (input$opcoes_teste == "equi") {
              "epiR::epi.ssequc("
            } else if (input$opcoes_teste == "inf") {
              "epiR::epi.ssninfc("
            } else {
              "epiR::epi.sssupc("
            },
            "control = 0, ",
            "treat = ", input$diferenca_para_detectar, ", ",
            "sigma = ",  input$sigma, ", ",
            "delta = abs(",  input$margem, "), ",
            if (tipo == "tamanho_amostral") {
              paste0(
                "n = NA, ",
                "r = ",  input$balanceamento, ", ",
                "power  = ",  input$poder, "/100, "
              )
            } else {
              paste0(
                "n = ", input$n_tratamento, " + ", input$n_controle, ", ",
                "r = ",  input$n_tratamento, " / ", input$n_controle, ", ",
                "power  = NA, "
              )
            },
            "alpha = ", input$alpha, "/100)"
          )
        }

        # print_r_code(code)


        n <- try_n(code)
        # browser()
        eval(parse(text = validate_n("n")))

        if (tipo == "tamanho_amostral") {
          n1 <- n$n.treat
          n2 <- n$n.control
          n <- n1 + n2

          nperdas1 <- n_perdas(n1, input$perc_perdas)
          nperdas2 <- n_perdas(n2, input$perc_perdas)
          eval(parse(text = validate_n_inf("n")))

        } else {

          poder <- round(n$power*100, 1)
          eval(parse(text = validate_n_inf("poder")))
        }

        teste_escolhido <- case_when(
          input$opcoes_teste == "inf"  ~ "não inferioridade",
          input$opcoes_teste == "equi" ~ "equivalência",
          input$opcoes_teste == "sup"  ~ "superioridade"
        )

        paste0(

          # Cabecalho
          if (tipo %in% c("tamanho_amostral", "estimar")) {
            paste0(
              "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
              if (n1 != n2) {
                paste0(
                  " (<i>", n1, " ", nome_grupo_tratamento(),
                  translation_pss(" e ", linguagem()),
                  n2, " ", nome_grupo_controle(), "</i>)"
                )
              } else {
                paste0(
                  " (<i>", n1, " ", translation_pss("para cada grupo", linguagem()), "</i>)"
                )
              }
            )
          } else if (tipo == "poder") {
            paste0("<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", poder, "%")
          },
          "</font></b></br></br>",


          lista_de_funcoes_server()$sugestao_texto_portugues(
            "<i>",
            translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


            # Inicio do texto

            if (tipo == "tamanho_amostral") {
              paste0(
                "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",
                if (n1 != n2) {
                  paste0(
                    "(", n1, " no grupo ", nome_grupo_tratamento(), " e ", n2, " no grupo ", nome_grupo_controle(), ")"
                  )
                } else {
                  paste0(
                    "(", n1, " para cada grupo)"
                  )
                }
              )
            } else if (tipo == "poder") {
              "O poder"
            },

            " para testar a ", teste_escolhido,


            if (tipo_variavel == "c") ", em termos de médias de <i>" else  ", em termos de proporção de <i>",
            nome_desfecho(), "</i>, do grupo <i>", nome_grupo_tratamento(),
            "</i> em relação ao grupo <i>", nome_grupo_controle(), "</i> ",


            if (tipo == "tamanho_amostral") {
              if (input$balanceamento == 1) {
                paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ")
              } else {
                paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser ", nperdas1, " ", nome_grupo_tratamento(), " e ", nperdas2, " ", nome_grupo_controle(), "). ")
              }
            } else {
              paste0("é de <b>", poder, "%</b>. ")
            },


            if (tipo == "tamanho_amostral") {
              paste0(
                "O cálculo considerou como margem de ", teste_escolhido, " <b>", input$margem,

                if (tipo_variavel == "c") paste0(" ", unidade_medida(), "</b>, ") else paste0("%</b>, "),
                "poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>, "
              )
            } else if (tipo == "poder") {
              paste0(
                "Este valor foi obtido considerando margem de ", teste_escolhido, " </b>", input$margem,
                if (tipo_variavel == "c") paste0(" ", unidade_medida(), "</b>, ") else paste0(" %</b>, "),
                "nível de significância de <b>", input$alpha, "%</b>, ",
                if (input$n_tratamento == input$n_controle) {
                  paste0("tamanho amostral igual a <b>", input$n_controle, "</b> sujeitos em cada grupo, ")
                } else {
                  paste0(
                    "tamanho amostral igual a <b>", input$n_tratamento, "</b> sujeitos para o grupo <i>",
                    nome_grupo_tratamento(), "</i> e <b>", input$n_controle, "</b> sujeitos para o grupo <i>",
                    nome_grupo_controle(), "</i> "
                  )
                }
              )
            },

            if (tipo_variavel == "c") {
              paste0(
                "diferença de <b>", input$diferenca_para_detectar, " ", unidade_medida(), "</b> entre as médias e ",
                "desvio padrão igual a <b>", input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)). "
              )
            } else {
              text_just
            },
            .txt_citacao_pss
          ),
          .txt_referencia_tap,
          print_r_code(code)
        )



      })






    } # Nao mexer!!!
  )

}
