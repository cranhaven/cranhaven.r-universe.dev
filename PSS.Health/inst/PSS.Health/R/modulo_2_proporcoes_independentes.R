
mod_2_proporcoes_independentes_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_2_proporcoes_independentes_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, txt_balanceamento_f, h1,
                                                  translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                                  warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas,
                                                  lista_de_funcoes_server){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns


      n_th2_prop <- function(prop_controle,
                             prop_tratamento,
                             significancia,
                             poder,
                             alternative,
                             ratio_controle_caso,
                             correct){


        n <- tryCatch({
          suppressWarnings(
            EnvStats::propTestN(p.or.p1     = prop_tratamento,
                                p0.or.p2    = prop_controle,
                                alpha       = significancia,
                                power       = poder,
                                sample.type = "two.sample",
                                alternative = alternative,
                                ratio       = if (ratio_controle_caso >= 1) ratio_controle_caso else 1/ratio_controle_caso,
                                correct     = correct)
          )
        }, warning = function(warning_condition) {
          NA
          # 1: In EnvStats::propTestN(p.or.p1 = prop_controle, p0.or.p2 = prop_tratamento,  :
          #                             The computed sample sizes 'n1' and 'n2' are too small, relative to the given values of 'p1' and 'p2', for the normal approximation to work well for the following element indices:
          #                             1
        }, error = function(error_condition) {
          NA
        }
        )


        if (sum(is.na(n)) > 0 ) {
          NA
        } else if (ratio_controle_caso > 1) {
          n$n1
        } else if (ratio_controle_caso == 1) {
          n
        } else {
          n$n2
        }

      }

      # n_th2_prop(35/100, 45/100, 5/100, 80/100, "two.sided", 1, TRUE)
      # n_th2_prop(35/100, 45/100, 5/100, 80/100, "two.sided", 2, TRUE)
      # n_th2_prop(35/100, 45/100, 5/100, 80/100, "two.sided", 0.5, TRUE)
      # n_th2_prop(35/100, 45/100, 5/100, 80/100, "two.sided", 2, TRUE, "n2")




      eval(parse(text = warning_numero_positivo("rr")))
      eval(parse(text = warning_numero_positivo("rc")))

      eval(parse(text = warning_prop("perc_tratamento")))
      eval(parse(text = warning_prop("perc_controle")))

      eval(parse(text = warning_numero_positivo("balanceamento")))
      eval(parse(text = warning_prop("poder")))
      eval(parse(text = warning_prop("alpha")))
      eval(parse(text = warning_prop("confianca")))
      eval(parse(text = warning_prop("precisao")))
      eval(parse(text = warning_perdas("perc_perdas")))

      eval(parse(text = warning_inteiro("n_controle")))
      eval(parse(text = warning_inteiro("n_tratamento")))


      # Aba ----

      output$aba <- renderUI({


        sidebarLayout(
          sidebarPanel(


            wellPanel(HTML(
              '<b><a href="https://youtu.be/oH6e1t4-RXw" target="_blank">',
              translation_pss("Vídeo: PSS Health para comparação de duas proporções independentes", linguagem()),
              '</a></b><br>'
            )),

            if (tipo %in% c("tamanho_amostral", "poder")) {
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

            actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),
            br(), br(),

            # uiOutput("perc_controle_testar"),
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
            ),


            if (tipo %in% c("tamanho_amostral", "estimar")) {

              fluidPage(fluidRow(

                if (tipo %in% c("estimar")) {
                  numericInput( ns("precisao"),
                                paste0(
                                  translation_pss("Margem de erro/ semi-amplitude", linguagem()),
                                  " (%)"
                                ),
                                value = 15,
                                min = 0,
                                max = 100,
                                step = 0.1
                  ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_precisao, title = translation_pss("Margem de erro/ semi-amplitude", linguagem()))
                },

                numericInput( ns("balanceamento"),
                              paste0(
                                translation_pss("Balanceamento", linguagem()),
                                " (", nome_grupo_controle(), ":", nome_grupo_tratamento(), ")"
                              ),
                              value = 1,
                              min   = 0,
                              max   = Inf,
                              step  = .5
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_balanceamento_f(nome_grupo_controle(), nome_grupo_tratamento()),
                                   title = translation_pss("Balanceamento", linguagem())),

                if (tipo == "tamanho_amostral") {
                  numericInput( ns("poder"),
                                translation_pss("Poder (%)", linguagem()),
                                value = 80,
                                min = 0,
                                max = 100,
                                step = 1
                  ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
                }
              ))
            } else if (tipo == "poder") {
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
            },


            if (tipo %in% c("tamanho_amostral", "poder")) {
              fluidPage(fluidRow(
                numericInput( ns("alpha"),
                              translation_pss("Nível de significância (%)", linguagem()),
                              value = 5,
                              min = 0,
                              max = 100,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem())),

                selectInput(ns('th_alternativa'),
                            translation_pss('Tipo de teste de acordo com hipótese alternativa', linguagem()),
                            choices = h1(),
                            selected = 'Bilateral'
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_h1)
              ))
            } else {
              fluidPage(fluidRow(
                numericInput( ns("confianca"),
                              translation_pss("Nível de confiança (%)", linguagem()),
                              value = 95,
                              min = 0,
                              max = 100,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem())),

                selectInput(ns('metodo_ic'),
                            translation_pss("Método utilizado para calcular o intervalo de confiança", linguagem()),
                            choices = c("Score" = "score", "Adjusted Wald" = "adjusted Wald"),
                            selected = 'score'
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_per_method_EnvStats),

              ))
            },



            if (tipo %in% c("tamanho_amostral", "estimar")) {
              numericInput( ns("perc_perdas"),
                            translation_pss("Perdas/ Recusas (%)", linguagem()),
                            value = 10,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
            },

            if (tipo != "estimar") {
              checkboxInput(
                ns("prop_correction"),
                translation_pss("Aplicar correção de continuidade", linguagem()),
                value = TRUE
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_correcao_continuidade)
            }
          ),

          mainPanel(
            htmlOutput(ns("texto_principal")) %>%
              shinycssloaders::withSpinner(type = 5),

              uiOutput(ns("cenarios"))
          )
        )
      })


      # Hipoteses a serem testadas ----

      alternative_TH2_prop2 <- reactive({

        if(input$th_alternativa == "Bilateral"){
          "two.sided"
        } else if(input$estatistica_tratamento == "percent"){
          ifelse(input$perc_tratamento > input$perc_controle, "greater", "less")
        } else if(input$estatistica_tratamento == "ratio"){
          ifelse(input$rr > 1, "greater", "less")
        } else if(input$estatistica_tratamento == "odds"){
          ifelse(input$rc > 1, "greater", "less")
        }

      })


      output$th_h0 <- renderUI({
        req(!is.null(alternative_TH2_prop2()))

        sinal_h0 <- case_when(alternative_TH2_prop2() == 'two.sided' ~ "=",
                              alternative_TH2_prop2() == 'greater'   ~ "\\leq",
                              alternative_TH2_prop2() == 'less'      ~ "\\geq")

        withMathJax(
          paste0("$$H_0: \\pi_\\text{", nome_grupo_tratamento(), "} ", sinal_h0, " \\pi_\\text{", nome_grupo_controle(), "}$$"))
      })

      output$th_h1 <- renderUI({
        req(!is.null(alternative_TH2_prop2()))

        sinal_h1 <- case_when(alternative_TH2_prop2() == 'two.sided' ~ "\\neq",
                              alternative_TH2_prop2() == 'greater'   ~ ">",
                              alternative_TH2_prop2() == 'less'      ~ "<")

        withMathJax(
          paste0("$$H_1: \\pi_\\text{", nome_grupo_tratamento(), "}", sinal_h1, " \\pi_\\text{", nome_grupo_controle(), "}$$"))
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





      # % ou RC ou RR -----

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





      # Texto -----

      output$texto_principal <- renderText({

        req(!(is.null(input$perc_controle) | is.null(input$perc_controle) | is.null(input$perc_tratamento)))

        if (input$estatistica_tratamento == "percent") {

          p2 <- paste0(input$perc_tratamento, "/100")
          text_just <- paste0(
            " percentuais de <b>", input$perc_tratamento, "%</b> e <b>", input$perc_controle, "%</b>, respectivamente ",
            "(referido por Fulano (1900)). "
          )

        } else if (input$estatistica_tratamento == "ratio") {

          p2 <- paste0("(", input$perc_controle, "/100)*", input$rr)
          text_just <- paste0(
            " percentual de <b>", input$perc_controle, "%</b> e risco relativo de <b>", input$rr,
            "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. "
          )

        } else {

          prob_control <- paste0(input$perc_controle, "/100")
          p2 <- paste0("(", input$rc, "*", prob_control, ") / (1 + ", input$rc, "*", prob_control, " - ", prob_control, ")")
          text_just <- paste0(
            " percentual de <b>", input$perc_controle, "%</b> e razão de chance de <b>", input$rc,
            "</b> como é referida em Fulano (1900) <b>OU</b> escolha do pesquisador. "
          )
        }



        ratio_controle_caso <- input$balanceamento


        ## Tamanho amostral ou estimar --------
        if (tipo %in% c("tamanho_amostral", "estimar")) {

          if (ratio_controle_caso >= 1) {
            p1a <- paste0(input$perc_controle, "/100")
            p2a <- p2
          } else{
            p2a <- paste0(input$perc_controle, "/100")
            p1a <- p2
          }


          code <- ifelse(tipo == "tamanho_amostral",
                         paste0(
                           "EnvStats::propTestN(p.or.p1     = ", p2a, ", ",
                           "p0.or.p2    = ", p1a, ", ",
                           "alpha       = ", input$alpha,  "/100, ",
                           "power       = ", input$poder, "/100, ",
                           "sample.type = 'two.sample', ",
                           "alternative = '", alternative_TH2_prop2(), "', ",
                           "ratio       = ", ifelse(ratio_controle_caso >= 1, ratio_controle_caso, 1/ratio_controle_caso), ", ",
                           "correct     = ", input$prop_correction, ", ",
                           "warn = FALSE)"
                         ),
                         paste0(
                           "EnvStats::ciBinomN(half.width = ", input$precisao, "/100, ",
                           "p.hat.or.p1.hat = ", p1a, ", ",
                           "p2.hat          = ", p2a, ", ",
                           "sample.type     = 'two.sample', ",
                           "conf.level      = ", input$confianca, "/100, ",
                           "ratio           = ", ifelse(ratio_controle_caso >= 1, ratio_controle_caso, 1/ratio_controle_caso), ", ",
                           "correct         = ", input$prop_correction, ", ",
                           "ci.method       = '", input$metodo_ic, "', ",
                           "n.or.n1.max	    = 1E8)"
                         )
          )

          # print_r_code(code)
          # n <- try_n(code)
          n <- eval(parse(text = code))


          if (tipo == "tamanho_amostral") {
            if (ratio_controle_caso > 1) {
              n1 <- n$n1
              n2 <- n$n2
            } else if (ratio_controle_caso == 1) {
              n1 <- n
              n2 <- n
            } else {
              n1 <- n$n2
              n2 <- n$n1
            }
          } else {
            if (ratio_controle_caso >= 1) {
              n1 <- n$n1
              n2 <- n$n2
            } else {
              n1 <- n$n2
              n2 <- n$n1
            }
          }



          n <- n1 + n2
          nperdas1 <- n_perdas(n1, input$perc_perdas)
          nperdas2 <- n_perdas(n2, input$perc_perdas)
          eval(parse(text = validate_n_inf("n")))




          if (tipo == "estimar") {
            texto_comparacao <- paste0(
              " para estimar a diferença entre os percentuais de <i>",
              nome_desfecho(), "</i> nos grupos <i>", nome_grupo_tratamento(), "</i> e <i>", nome_grupo_controle(), "</i>, ",
              "com margem de erro igual a <b>", input$precisao, "%</b> "
            )
          } else if (alternative_TH2_prop2() == "two.sided") {
            texto_comparacao <- paste0(
              " para testar se existe diferença entre os percentuais de <i>",
              nome_desfecho(), "</i> nos grupos <i>", nome_grupo_tratamento(), "</i> e <i>", nome_grupo_controle(), "</i> "
            )
          } else {
            texto_comparacao <- paste0(
              " para testar se o percentual de <i>",
              nome_desfecho(), "</i> no <i>", nome_grupo_tratamento(), "</i> é ",
              if (alternative_TH2_prop2() == "less") "menor" else "maior",
              " do que no <i>", nome_grupo_controle(), "</i> "
            )
          }


          cabecalho <- paste0(
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
              "<i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
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
          )




          paste0(
            cabecalho,

            lista_de_funcoes_server()$sugestao_texto_portugues(
              texto_comparacao,

              if (input$balanceamento == 1) {
                paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ")
              } else {
                paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser ", nperdas1, " ", nome_grupo_tratamento(), " e ", nperdas2, " ", nome_grupo_controle(), "). ")
              },

              if (tipo == "tamanho_amostral") {
                paste0("O cálculo considerou poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>, ")
              } else {
                paste0("O cálculo considerou nível de confiança de <b>", input$confianca, "%</b>, ")
              },
              text_just,
              .txt_citacao_pss
            ),
            .txt_referencia_tap,
            print_r_code(code)
          )






          ## pODER ----
        } else {


          code <- paste0(
            "EnvStats::propTestPower(",
            "n.or.n1     = ", input$n_tratamento, ", ",
            "p.or.p1 = ", p2, ", ",
            "n2 = ", input$n_controle, ", ",
            "p0.or.p2	= ", input$perc_controle, "/100, ",
            "alpha       = ", input$alpha, "/100, ",
            "sample.type = 'two.sample', ",
            "alternative = '", alternative_TH2_prop2(), "', ",
            "correct     = ", input$prop_correction, ", warn = FALSE)"
          )

          poder <- eval(parse(text = code))

          # print_r_code(code)
          paste0("<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", round(poder*100, digits = 1),
                 "%</font></b></br></br>",

                 lista_de_funcoes_server()$sugestao_texto_portugues(

                   "O poder ",
                   if (alternative_TH2_prop2() == "two.sided") {
                     texto_comparacao <- paste0(
                       " para testar se existe diferença entre os percentuais de <i>",
                       nome_desfecho(), "</i> nos grupos <i>", nome_grupo_tratamento(), "</i> e <i>", nome_grupo_controle(), "</i> "
                     )
                   } else {
                     texto_comparacao <- paste0(
                       " para testar se o percentual de <i>",
                       nome_desfecho(), "</i> no <i>", nome_grupo_tratamento(), "</i> é ",
                       if (alternative_TH2_prop2() == "less") "menor" else "maior",
                       " do que no <i>", nome_grupo_controle(), "</i> "
                     )
                   },
                   " é <b>", round(poder*100, digits = 1), "%</b>. ",
                   "Este valor",
                   if (input$prop_correction) ", aplicando correção de continuidade,",
                   " foi obtido considerando nível de significância de <b>", input$alpha, "</b>%, ",
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





      # Cenarios ----


      output$cenarios <- renderUI({

        req(tipo == "tamanho_amostral")

        req(!is.null(input$estatistica_tratamento))

        if (input$estatistica_tratamento == 'percent') {
          ratio_start <- min(c(5, input$perc_tratamento))
          ratio_end  <- max(c(95, input$perc_tratamento))
          ratio_by   <- 10
          ratio_max  <- 100
        } else {
          razao_usada <- ifelse(input$estatistica_tratamento == 'ratio', input$rr, input$rc)

          if(razao_usada > 1){
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

          if (input$estatistica_tratamento == 'percent') {
            HTML(
              paste0("<b>", translation_pss("Defina a sequência de valores (%) para o grupo", linguagem()), " ", nome_grupo_tratamento(), "</b>")
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


        if(input$estatistica_tratamento == "percent"){
          rrrr <- NA
          odssss <- NA
          prop_tratamento <- seq(from = input$from, to = input$to, by = input$by)/100

        } else if (input$estatistica_tratamento == "ratio") {

          rrrr <- seq(from = input$from, to = input$to, by = input$by)
          odssss <- NA
          prop_tratamento <- (input$perc_controle/100)*rrrr

        } else {

          rrrr <- NA
          odssss <- seq(from = input$from, to = input$to, by = input$by)
          prob_control <- input$perc_controle/100
          prop_tratamento <- (odssss*prob_control)/ (1 + odssss*prob_control - prob_control)
        }


        df_inputs_prop <- tibble::tibble(
          `Risco relativo` = rrrr,
          `Razão de chance` = odssss,
          prop_tratamento
        )


        simul_n <- expand.grid(prop_controle   = input$perc_controle/100,
                               prop_tratamento = prop_tratamento,
                               significancia   = input$alpha/100,
                               poder           = poder/100,
                               alternative     = alternative_TH2_prop2(),
                               ratio_controle_caso = input$balanceamento,
                               correct        = input$prop_correction,
                               stringsAsFactors = FALSE) %>%
          dplyr::filter(prop_tratamento != prop_controle) %>%
          mutate(`n Tratamento` = mapply(n_th2_prop, prop_controle, prop_tratamento, significancia, poder,
                                         alternative, ratio_controle_caso, correct),
                 `n Controle` = ceiling(`n Tratamento`*input$balanceamento)
          ) %>%
          dplyr::filter(!is.na(`n Tratamento`) & !is.na(`n Controle`)) %>%
          mutate(`n total` = `n Tratamento` + `n Controle`,
                 `Nível de significância (%)` = input$alpha,
                 `Perdas/ Recusas (%)`   = input$perc_perdas,
                 `% no Tratamento` = prop_tratamento*100,
                 `Poder (%)` = poder*100,
                 `% Controle` = prop_controle*100,
                 `% Tratamento` = prop_tratamento*100,
                 `Hipótese alternativa` = alternative,
                 `Balanço da amostra (Controle/ Tratamento)` = ratio_controle_caso,
                 `Correção de continuidade` = correct
          )

        simul_n %>%
          left_join(df_inputs_prop, by = "prop_tratamento")


      })


      output$grafico_cenarios <- plotly::renderPlotly({

        metrica <- case_when(
          input$estatistica_tratamento == 'percent' ~ "% no Tratamento",
          input$estatistica_tratamento == 'ratio'   ~ "Risco relativo",
          TRUE ~ "Razão de chance"
        )

        xlab <- case_when(
          input$estatistica_tratamento == 'percent' ~ paste0("% ", translation_pss("no", linguagem()), " ", nome_grupo_tratamento()),
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
          input$estatistica_tratamento == 'percent' ~ "% no Tratamento",
          input$estatistica_tratamento == 'ratio'   ~ "Risco relativo",
          TRUE ~ "Razão de chance"
        )

        xlabb <- case_when(
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
                "Hipótese alternativa",
                "Balanço da amostra (Controle/ Tratamento)",
                "Correção de continuidade"
              )
            )

          colnames(df_) <- c(
            paste0("% ", nome_grupo_controle()),
            xlabb,
            translation_pss("Tamanho amostral", linguagem()),
            paste0("n ", nome_grupo_tratamento()),
            paste0("n ", nome_grupo_controle()),
            translation_pss("Nível de significância (%)", linguagem()),
            translation_pss("Poder (%)", linguagem()),
            translation_pss("Hipótese alternativa", linguagem()),
            translation_pss("Balanceamento", linguagem()),
            translation_pss("Aplicar correção de continuidade", linguagem())
          )

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
        filename = function() { "Cenarios_tamanho_amostra_duas_prop_independentes.xlsx"},
        content = function(file) {writexl::write_xlsx(return_table_tabela_cenarios(),
                                                      path = file)}
      )





    } # Nao mexer!!!
  )

}
