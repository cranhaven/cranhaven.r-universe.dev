
mod_cox_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("abaa_cox")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_cox_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, txt_balanceamento_f,
                           translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                           warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas,
                           hazard_ratio,
                           lista_de_funcoes_server){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns




      # Comeco ----

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

      output$abaa_cox <- renderUI({
        req(!is.null(opcoes_tipo_variavel()))
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
                  withMathJax("$$H_0: HR = 1 \\text{  vs  } H_1: HR \\neq 1$$")
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

            numericInput(
              ns("hr"),
              "Hazard ratio",
              value = 2,
              min = 0,
              max = Inf,
              step = 1
            ) %>%
              .help_buttom(linguagem = linguagem(), 
                body = txt_ajuda()$txt_hazard_ratio,
                title = "Hazard ratio"
              ),

            uiOutput(ns("prob_sobrevivenciaUi")),


            if (tipo %in% c("tamanho_amostral")) {
              numericInput( ns("poder"),
                            translation_pss("Poder (%)", linguagem()),
                            value = 80,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
            # } else {
              # numericInput( ns("n"),
              #               translation_pss("Tamanho amostral", linguagem()),
              #               value = 200,
              #               min = 4,
              #               step = 1
              # )
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
                              value = 5,
                              min = 0,
                              max = 100,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem()))
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

            uiOutput(ns("cenarios"))

          )
        )
      })



      estimar_ou_var_binaria <- reactive({
        # TRUE
        estimar_ou_var_binaria <- tipo == "estimar"
        if (!estimar_ou_var_binaria) estimar_ou_var_binaria <- input$tipo_variavel == 0
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
                          value   = ifelse(input$mudar_nomes == 0, "X", nome_desfecho()))
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




      # Prob sobrevivencia Ui ----

      output$prob_sobrevivenciaUi <- renderUI({

        if (estimar_ou_var_binaria()) {

          tagList(
            HTML(
              "<b><font size = '2.99'>",
              translation_pss("Probabilidade",linguagem()),
              " (%) ",
              translation_pss("de",linguagem()),
              " ",
              nome_desfecho(),
              " ",
              translation_pss("até o final do seguimento", linguagem()),
              "</font></b><br>"
            ),

            div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                numericInput( ns("cox_failure_trat"),
                              nome_grupo_tratamento(),
                              value = 80,
                              min  = 0,
                              max  = 100,
                              step = 1)
            ),
            div(style = "display: inline-block;vertical-align:top; width: 49%;",
                numericInput( ns("cox_failure_control"),
                              nome_grupo_controle(),
                              value = 80,
                              min  = 0,
                              max  = 100,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), 
                  body = paste0(
                    translation_pss("Probabilidade",linguagem()),
                    " (%) ",
                    translation_pss("de",linguagem()),
                    " ",
                    nome_desfecho(),
                    " ",
                    translation_pss("até o final do seguimento", linguagem()),
                    txt_ajuda()$txt_definido_literatura
                  )
                )
            ),

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
            } else {
              tagList(
                HTML(
                  "<b><font size = '2.95'>",
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
                )
              )
            }

          )

        } else {

          tagList(


            if (tipo == "poder") {
              numericInput( ns("n"),
                            translation_pss("Tamanho amostral", linguagem()),
                            value = 200,
                            min = 4,
                            step = 1
              )
            },

            numericInput(inputId =  ns("cox_failure_continua"),
                         label = paste0(
                           translation_pss("Probabilidade",linguagem()),
                           " (%) ",
                           translation_pss("de",linguagem()),
                           " ",
                           nome_desfecho(),
                           " ",
                           translation_pss("até o final do seguimento", linguagem())
                         ),
                         value = 80,
                         min  = 0,
                         max  = 100,
                         step = 1
            ) %>% .help_buttom(linguagem = linguagem(), 
              body = paste0(
                translation_pss("Probabilidade",linguagem()),
                " (%) ",
                translation_pss("de",linguagem()),
                " ",
                nome_desfecho(),
                " ",
                translation_pss("até o final do seguimento", linguagem()),
                txt_ajuda()$txt_definido_literatura
              )
            ),

            numericInput( ns("cox_desvio_padrao"),
                          paste0(translation_pss("Desvio padrão esperado de", linguagem()), " ", nome_preditora()),
                          value = 0.3,
                          min  = 0,
                          max  = Inf,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_dp, title = "Desvio padrão esperado"),

            numericInput( ns("cox_r2"),
                          translation_pss("Coeficiente de correlação múltipla", linguagem()),
                          value = 0,
                          min  = -1,
                          max  = 1,
                          step = .2
            ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Coeficiente de correlação múltipla entre a covariável de interesse e outras covariáveis.
                                                     Defina zero (default) se não haverá outras variáveis independentes.", txt_ajuda()$txt_definido_pesquisador_OU_literatura))
          )
        }


      })









      # Texto  -----

      output$texto_principal <- renderText({

        if (estimar_ou_var_binaria()) {

          code <- ifelse(tipo == "tamanho_amostral",
                         paste0("powerSurvEpi::ssizeCT.default(",
                                "k  = ", input$balanceamento, ", ",
                                "pE = ", input$cox_failure_trat, "/100, ",
                                "pC = ", input$cox_failure_control, "/100, ",
                                "RR = ", input$hr,  ", ",
                                "alpha = ", input$alpha, "/100, ",
                                "power = ", input$poder, "/100)"
                         ),
                         paste0("powerSurvEpi::powerCT.default(",
                                "nE = ", input$n_tratamento, ", ",
                                "nC = ", input$n_controle, ", ",
                                "pE = ", input$cox_failure_trat, "/100, ",
                                "pC = ", input$cox_failure_control, "/100, ",
                                "RR = ", input$hr,  ", ",
                                "alpha = ", input$alpha, "/100)"
                         )
          )





          if (tipo == "poder") {

            poder <- try_n(code)

            paste0("<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", round(poder*100, digits = 1),
                   "%</font></b></br></br>",


                   lista_de_funcoes_server()$sugestao_texto_portugues(
                     "<i>",
                     translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
                     "O poder para testar se o hazard ratio para <i>", nome_desfecho(), "</i> ",
                     "entre os grupos <b>",nome_grupo_controle(), "</b> e <b>", nome_grupo_tratamento(),
                     "</b> é diferente de 1 é <b>", round(poder*100, digits = 1), "%</b>. ",

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

                     "probabilidade de ",
                     nome_desfecho(),
                     " até o final do seguimento de <b>", input$cox_failure_trat, "%</b> ",
                     if (input$cox_failure_control == input$cox_failure_trat) {
                       "em ambos grupos "
                     } else {
                       paste0(" e <b>", input$cox_failure_control, "%</b>, respectivamente, ")
                     },
                     "e hazard ratio esperado de <b>", input$hr, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. ",

                     .txt_citacao_pss
                   ),
                   .txt_referencia_tap,
                   print_r_code(code)
            )
          } else {

            n <- try_n(code)
            eval(parse(text = validate_n("n")))

            n1 <- n[1]
            n2 <- n[2]

            n <- n1 + n2
            nperdas1 <- n_perdas(n1, input$perc_perdas)
            nperdas2 <- n_perdas(n2, input$perc_perdas)
            eval(parse(text = validate_n_inf("n")))



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
                "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",
                if (n1 != n2) {
                  paste0(
                    "(", n1, " no grupo ", nome_grupo_tratamento(), " e ", n2, " no grupo ", nome_grupo_controle(), ")"
                  )
                } else {
                  paste0(
                    "(", n1, " para cada grupo)"
                  )
                },

                " para testar se o hazard ratio para <i>", nome_desfecho(), "</i> ",
                "entre os grupos <b>",nome_grupo_controle(), "</b> e <b>", nome_grupo_tratamento(),
                "</b> é diferente de 1 ",

                if (input$balanceamento == 1) {
                  paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ")
                } else {
                  paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser ", nperdas1, " ", nome_grupo_tratamento(), " e ", nperdas2, " ", nome_grupo_controle(), "). ")
                },

                "O cálculo considerou poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>, ",
                "probabilidade de ",
                nome_desfecho(),
                " até o final do seguimento de <b>", input$cox_failure_trat, "%</b> ",
                if (input$cox_failure_control == input$cox_failure_trat) {
                  "em ambos grupos "
                } else {
                  paste0(" e <b>", input$cox_failure_control, "%</b>, respectivamente, ")
                },
                "e hazard ratio esperado de <b>", input$hr, "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. ",
                .txt_citacao_pss
              ),

              .txt_referencia_tap,
              print_r_code(code)
            )


          }


          # Se usar variavel quantitativa
        } else{
          code <- ifelse(tipo == "tamanho_amostral",
                         paste0("powerSurvEpi::ssizeEpiCont.default(",
                                "power = ", input$poder, "/100, ",
                                "theta = ", input$hr,  ", ",
                                "sigma2 = ",  input$cox_desvio_padrao, "^2, ",
                                "psi = ", input$cox_failure_continua, "/100, ",
                                "rho2 = (", input$cox_r2, ")^2, ",
                                "alpha = ", input$alpha, "/100)"
                         ),
                         paste0("powerSurvEpi::powerEpiCont.default(",
                                "n = ", input$n, ", ",
                                "theta = ", input$hr,  ", ",
                                "sigma2 = ",  input$cox_desvio_padrao, "^2, ",
                                "psi = ", input$cox_failure_continua/100, ", ",
                                "rho2 = (", input$cox_r2, ")^2, ",
                                "alpha = ", input$alpha, "/100)"
                         )
          )


          if (tipo == "poder") {

            poder <- try_n(code)

            paste0("<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", round(poder*100, digits = 1),
                   "%</font></b></br></br>",

                   lista_de_funcoes_server()$sugestao_texto_portugues(
                     # "</i>",
                     "O poder para testar se o hazard ratio para <i>", nome_desfecho(), "</i> a cada acréscimo de uma unidade em <i>", nome_preditora(),
                     "</i> é diferente de 1 é <b>", round(poder*100, digits = 1), "%</b>. ",

                     "Este valor foi obtido considerando nível de significância de <b>", input$alpha, "%</b>, ",
                     "tamanho amostral igual a <b>", input$n, "</b> sujeitos, ",
                     if (input$cox_r2 != 0) {
                       paste0("coeficiente de correlação múltipla com as demais covariáveis de <b>", input$cox_r2, "</b> ")
                     },
                     "hazard ratio esperado de <b>", input$hr, "</b>, ",
                     "probabilidade de ",
                     nome_desfecho(),
                     " até o final do seguimento de <b>", input$cox_failure_continua, "%</b> ",
                     " e desvio padrão da <i>", nome_preditora(), "</i> de <b>", input$cox_desvio_padrao, " u.m.</b>, ",
                     "como é referida em Fulano (1900). ",
                     .txt_citacao_pss
                   ),

                   .txt_referencia_tap, print_r_code(code)
            )
          } else {


          n <- try_n(code)
          eval(parse(text = validate_n("n")))
          eval(parse(text = validate_n_inf("n")))

          paste0(
            "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
            "</font></b></br></br>",

            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>",
              translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

              "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",
              "para testar se o hazard ratio para <i>", nome_desfecho(), "</i> a cada acréscimo de uma unidade em <i>", nome_preditora(), "</i> é diferente de 1 ",
              "(com o acréscimo de ", input$perc_perdas, "% para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",
              "O cálculo considerou um poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>, ",
              if (input$cox_r2 != 0) {
                paste0("coeficiente de correlação múltipla com as demais covariáveis de <b>", input$cox_r2, "</b> ")
              },
              "hazard ratio esperado de <b>", input$hr, "</b>, ",
              "probabilidade de ",
              nome_desfecho(),
              " até o final do seguimento de <b>", input$cox_failure_continua, "%</b> ",
              " e desvio padrão da <i>", nome_preditora(), "</i> de <b>", input$cox_desvio_padrao, " u.m.</b>, ",
              "como é referida em Fulano (1900). ",
              .txt_citacao_pss
            ),

            .txt_referencia_tap, print_r_code(code)
          )


          }
        }

      })


      # Cenarios ----

      output$cenarios <- renderUI({

        req(tipo == "tamanho_amostral")

        if (input$hr > 1) {
          ratio_start <- input$hr
          ratio_end  <- input$hr + 1
          ratio_by   <- 0.1
        } else {
          ratio_start <- max(0, input$hr - 0.3)
          ratio_end  <- input$hr
          ratio_by   <- 0.05
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

          HTML(
            "<b>", translation_pss("Defina a sequência do hazard ratio", linguagem()), "</b>"
          ),


          br(),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput(ns("from"), translation_pss("Mínimo", linguagem()), value = ratio_start, step = .1, min = 0, max = Inf)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("to"), translation_pss("Máximo", linguagem()), value = ratio_end, step = .1, min = 0, max = Inf)
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



      tab_cenarios <- reactive({

        poder <- text_input_to_vector(input$poder_cenarios)
        req(length(poder) > 0)

        if (estimar_ou_var_binaria()) {

          df_grid <- expand.grid(alpha = input$alpha,
                                 poder = poder,
                                 hr = seq(input$from, input$to, input$by),
                                 k = input$balanceamento,
                                 pE        = input$cox_failure_trat,
                                 pC        = input$cox_failure_control,
                                 stringsAsFactors = FALSE)
          # df_grid
          df_n <- df_grid %>%
            mutate(
              n_trat = mapply(
                function(k, poder, pE, pC, hr, alpha){

                  n <- tryCatch({
                    powerSurvEpi::ssizeCT.default(
                      k = k,
                      pE = pE/100,
                      pC = pC/100,
                      RR = hr,
                      alpha = alpha/100,
                      power = poder/100)
                  },
                  warning = function(warning_condition) { NA },
                  error = function(error_condition) { NA })

                  n[1]
                }, k, poder, pE, pC, hr, alpha
              ),

              n_control = mapply(
                function(k, poder, pE, pC, hr, alpha){

                  n <- tryCatch({
                    powerSurvEpi::ssizeCT.default(
                      k = k,
                      pE = pE/100,
                      pC = pC/100,
                      RR = hr,
                      alpha = alpha/100,
                      power = poder/100)
                  },
                  warning = function(warning_condition) { NA },
                  error = function(error_condition) { NA })

                  n[2]
                }, k, poder, pE, pC, hr, alpha
              ),

              n = n_trat + n_control
            )

          df_n


        } else {

          df_grid <- expand.grid(alpha = input$alpha,
                                 poder = poder,
                                 hr = seq(input$from, input$to, input$by),
                                 sigma = input$cox_desvio_padrao,
                                 psi = input$cox_failure_continua,
                                 rho = input$cox_r2,
                                 stringsAsFactors = FALSE) %>%
            mutate(
              n = mapply(
                function(hr, alpha, poder, sigma, psi, rho){

                  tryCatch({
                    powerSurvEpi::ssizeEpiCont.default(
                      sigma2 = sigma^2,
                      psi = psi/100,
                      rho = rho^2,
                      theta = hr,
                      alpha = alpha/100,
                      power = poder/100)
                  },
                  warning = function(warning_condition) { NA },
                  error = function(error_condition) { NA })

                }, hr, alpha, poder, sigma, psi, rho
              )
            )



        }
      })


      output$grafico_cenarios <- plotly::renderPlotly({


        g1 <- tab_cenarios() %>%
          mutate(
            `Poder (%)` = factor(poder),
            `Hazard ratio` = hr
          ) %>%
          ggplot(
            aes(x = `Hazard ratio`,
                y = n,
                color = `Poder (%)`
            )
          ) +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = seq(from = input$from, to = input$to, by = input$by)) +
          ylab(translation_pss("Tamanho da amostra*", linguagem())) +
          theme_bw() +
          theme(axis.text = element_text(colour = "black")) +
          scale_color_brewer(name = translation_pss("Poder (%)", linguagem()), palette = "Set1")


        plotly::ggplotly(g1,
                         tooltip = c("x", "colour", "y")) %>%
          plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                            showarrow = F, xref='paper', yref='paper',
                                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                            font=list(size=10)))
      })








      return_table_tabela_cenarios <- reactive({

        df_ <- tab_cenarios()


        if (input$tipo_variavel == 1) {
          colnames(df_) <- c(
            translation_pss("Nível de significância (%)", linguagem()),
            translation_pss("Poder (%)", linguagem()),
            "Hazard ratio",
            paste0(translation_pss("Desvio padrão", linguagem()), " ", nome_preditora()),
            paste0(
              translation_pss("Probabilidade",linguagem()),
              " (%) ",
              translation_pss("de",linguagem()),
              " ",
              nome_desfecho()
            ),
            translation_pss("Coeficiente de correlação múltipla", linguagem()),
            translation_pss("Tamanho da amostra", linguagem())
          )
        } else {
          colnames(df_) <- c(
            translation_pss("Nível de significância (%)", linguagem()),
            translation_pss("Poder (%)", linguagem()),
            "Hazard ratio",
            translation_pss("Balanceamento", linguagem()),
            paste0(
              translation_pss("Probabilidade",linguagem()),
              " (%) ",
              translation_pss("de",linguagem()),
              " ",
              nome_desfecho(),
              ": ",
              nome_grupo_controle()
            ),
            paste0(
              translation_pss("Probabilidade",linguagem()),
              " (%) ",
              translation_pss("de",linguagem()),
              " ",
              nome_desfecho(),
              ": ",
              nome_grupo_tratamento()
            ),
            paste0("n ", nome_grupo_controle()),
            paste0("n ", nome_grupo_tratamento()),
            translation_pss("Tamanho da amostra", linguagem())
          )
        }

        df_

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
        filename = function() { "Cenarios_tamanho_amostra_cox.xlsx"},
        content = function(file) {writexl::write_xlsx(return_table_tabela_cenarios(),
                                                      path = file)}
      )













    } # Nao mexer!!!
  )
}




