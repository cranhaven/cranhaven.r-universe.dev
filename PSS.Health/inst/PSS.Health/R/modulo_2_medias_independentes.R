
mod_2_medias_independentes_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_2_medias_independentes_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, txt_balanceamento_f, h1, cohen_d,
                                              translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                              warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas, warning_numero,
                                              lista_de_funcoes_server){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      eval(parse(text = warning_numero("d")))
      eval(parse(text = warning_numero("diferenca_para_detectar")))
      eval(parse(text = warning_numero_positivo("sigma")))
      eval(parse(text = warning_numero_positivo("deff")))

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


            # wellPanel(HTML('<b><a href="https://youtu.be/LpwvwqjPkJk" target="_blank">Vídeo: PSS Health para comparar dua médias</a></b><br>')),
            wellPanel(HTML(
              '<b><a href="https://youtu.be/LpwvwqjPkJk" target="_blank">',
              translation_pss("Vídeo: PSS Health para comparar dua médias", linguagem()),
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

            if (tipo %in% c("tamanho_amostral", "poder")) {
              checkboxInput(
                ns("calcular_utilizando_d_cohen"),
                translation_pss("Calcular usando o d de Cohen", linguagem()),
                value = FALSE
              )
            },

            if (tipo == "poder") {
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


            uiOutput(ns("render_input_cohen_medias")),


            if (tipo == "estimar") {
              numericInput( ns("precisao"),
                            translation_pss("Margem de erro/ semi-amplitude", linguagem()),
                            value = 15,
                            min = 0,
                            max = 100,
                            step = 0.1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_precisao, title = translation_pss("Margem de erro/ semi-amplitude", linguagem()))

            } else if (tipo == "tamanho_amostral") {
              numericInput( ns("poder"),
                            translation_pss("Poder (%)", linguagem()),
                            value = 80,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
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
              numericInput( ns("confianca"),
                            translation_pss("Nível de confiança (%)", linguagem()),
                            value = 95,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem()))
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
            # actionLink(ns("mudar_nomess"), translation_pss("Mudar nomes", linguagem())),
            # mod_desvio_padrao_agrupado_Ui(
            #   # ns("teste")
            #   "teste"
            # ),

            htmlOutput(ns("texto_principal")) %>%
              shinycssloaders::withSpinner(type = 5),

            uiOutput(ns("cenarios"))
          )
        )
      })



      # observeEvent(input$mudar_nomess, {
      #   showModal(
      #     modalDialog(
      #       title = translation_pss("Ajustes", linguagem()),
      #       fluidPage(
      #         mod_desvio_padrao_agrupado_Ui(ns("teste"))
      #       ),
      #       easyClose = TRUE,
      #       footer    = NULL
      #     )
      #   )
      # })


      # mod_desvio_padrao_agrupado_server(
      #   # ns("teste"),
      #   "teste",
      #   txt_ajuda = txt_ajuda,
      #   translation_pss = translation_pss,
      #   linguagem = linguagem,
      #   grupo_A = nome_grupo_tratamento,
      #   grupo_B = nome_grupo_controle,
      #   warning_numero_positivo = warning_numero_positivo,
      #   warning_inteiro = warning_inteiro
      # )


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

              br(),
              br(),


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

      th_alternativa <- reactive({

        if (input$th_alternativa == "Bilateral" ){
          "two.sided"
        } else if (input$calcular_utilizando_d_cohen) {
          if (input$th_alternativa != "two.sided" & input$d < 0) "less" else "greater"
        } else {
          if (input$diferenca_para_detectar < 0) "less" else "greater"
        }
      })




      output$th_h0 <- renderUI({
        req(!is.null(th_alternativa()))

        sinal_h0 <- case_when(th_alternativa() == 'two.sided' ~ "=",
                              th_alternativa() == 'greater'   ~ "\\leq",
                              th_alternativa() == 'less'      ~ "\\geq")

        withMathJax(
          paste0(
            "$$",
            "H_0: \\mu_\\text{", nome_grupo_tratamento(), "}", sinal_h0,  "\\mu_\\text{", nome_grupo_controle(), "} ",
            "$$"
          )
        )
      })

      output$th_h1 <- renderUI({
        req(!is.null(th_alternativa()))

        sinal_h1 <- case_when(th_alternativa() == 'two.sided' ~ "\\neq",
                              th_alternativa() == 'greater'   ~ ">",
                              th_alternativa() == 'less'      ~ "<")

        withMathJax(
          paste0(
            "$$",
            "H_1: \\mu_\\text{", nome_grupo_tratamento(), "}", sinal_h1,  "\\mu_\\text{", nome_grupo_controle(), "} ",
            "$$"
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
              textInput(inputId = ns("unidade_medida"),
                        label   = paste0(translation_pss("Descreva a unidade de medida do desfecho", linguagem())),
                        value   = ifelse(input$mudar_nomes == 0, translation_pss("u.m.", linguagem()), unidade_medida())),
              HTML("<i>", gsub("<br><br>", "", txt_ajuda()$txt_um), "</i>"),
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

      unidade_medida <- reactive({
        ifelse(is.null(input$unidade_medida),  translation_pss("u.m.", linguagem()), input$unidade_medida)
      })





      # d cohen ou medias? -----


      output$render_input_cohen_medias <- renderUI({

        if (tipo == "estimar") {
          numericInput( ns("sigma_estimar"),
                        translation_pss("Desvio padrão", linguagem()),
                        value = 10,
                        min = 0,
                        max = Inf,
                        step = 1
          ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão", linguagem()))

        } else if (input$calcular_utilizando_d_cohen) {
          numericInput( ns("d"),
                        translation_pss("d de Cohen", linguagem()),
                        value = 0.4,
                        min = 0,
                        max = Inf,
                        step = 0.1) %>%
            shinyhelper::helper(type = "markdown",
                                title = "Tamanho de efeito d",
                                content = "Effect_size_d_Cohen",
                                buttonLabel = "Fechar",
                                fade = TRUE,
                                colour = "#006338",
                                size = "l")

        } else {

          tagList(

            numericInput( ns("diferenca_para_detectar"),
                          paste0(
                            translation_pss("Diferença mínima a ser detectada", linguagem()), " (",
                            translation_pss("em", linguagem()), " ",
                            unidade_medida(), ")"
                          ),
                          value = 1,
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
            br(), br(),

            if (tipo == "tamanho_amostral") {
              tagList(
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
                                   title = translation_pss("Balanceamento", linguagem())),

                numericInput( ns("deff"),
                              translation_pss("Efeito do plano amostral", linguagem()),
                              value = 1,
                              min = 0,
                              max = Inf,
                              step =.2
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_deff,
                                   title = translation_pss("Efeito do plano amostral", linguagem()))
              )
            }

          ) # Fecha tagList

        }
      })





      # Texto -----


      output$texto_principal <- renderText({


        if (tipo == "tamanho_amostral") {
          req(!is.null(input$calcular_utilizando_d_cohen))

          code <- ifelse(input$calcular_utilizando_d_cohen,
                         paste0(
                           "pwr::pwr.t.test(d = ", input$d, ", ",
                           "sig.level = ", input$alpha, "/100, ",
                           "power = ", input$poder, "/100, ",
                           "alternative = '", th_alternativa(), "', ",
                           "type = 'two.sample')"
                         ),
                         paste0(
                           "epiR::epi.sscompc(",
                           "control = 0, ",
                           "treat = ", abs(input$diferenca_para_detectar), ", ",
                           "sigma = ", input$sigma, ", ",
                           "n = NA, ",
                           "power = ", input$poder, "/100,  ",
                           "conf.level = 1 - ", input$alpha, "/100, ",
                           "r = ", input$balanceamento, ", ",
                           "design = ", input$deff, ", ",
                           "sided.test = ", (th_alternativa() == "two.sided") + 1, ")"
                         )
          )

          n <- try_n(code)
          eval(parse(text = validate_n("n")))

          if (input$calcular_utilizando_d_cohen) {
            n1 <- ceiling(n$n)
            n2 <- ceiling(n$n)
          } else {
            n1 <- ceiling(n$n.treat)
            n2 <- ceiling(n$n.control)
          }

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

              if (th_alternativa() == "two.sided") {
                paste0(
                  " para testar se existe uma diferença mínima ",
                  if (!input$calcular_utilizando_d_cohen) paste0("de <b>", input$diferenca_para_detectar, " ", unidade_medida(), "</b> "),
                  "nas médias de <i>",
                  nome_desfecho(), "</i> entre os grupos <i>", nome_grupo_tratamento(), "</i> e <i>", nome_grupo_controle(), "</i> "
                )
              } else {
                paste0(
                  "para testar se  a média de <i>", nome_desfecho(), "</i> do grupo <i>", nome_grupo_tratamento(), "</i> é ",
                  if (th_alternativa() == "less") "menor" else "maior",
                  " do que a do grupo <i>", nome_grupo_controle(), "</i> ",
                  if (!input$calcular_utilizando_d_cohen) paste0("em <b>", input$diferenca_para_detectar, " ", unidade_medida(), "</b> ")
                )
              },

              if (input$balanceamento == 1) {
                paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ")
              } else {
                paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser ", nperdas1, " ", nome_grupo_tratamento(), " e ", nperdas2, " ", nome_grupo_controle(), "). ")
              },

              "O cálculo considerou poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>",

              if (input$calcular_utilizando_d_cohen) {
                paste0(" e d de Cohen de <b>", input$d, "</b>, (referido por Fulano (1900) OU escolha do pesquisador). ")
              } else {
                paste0(
                  if (input$deff != 1) paste0(", tamanho de efeito do delineamento de <b>", input$deff, "</b> "),
                  " e desvio padrão igual a <b>", input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)). "
                )
              },
              .txt_citacao_pss
            ),
            .txt_referencia_tap,
            print_r_code(code)
          )






        } else if (tipo == "poder") {

          req(!is.null(input$calcular_utilizando_d_cohen))

          d <- ifelse(input$calcular_utilizando_d_cohen,
                      input$d,
                      cohen_d(mean_diff = input$diferenca_para_detectar,
                              n_1 = input$n_tratamento,
                              n_2 = input$n_controle,
                              sd_1 = input$sigma,
                              sd_2 = input$sigma
                      )
          )

          code <- paste0(
            "pwr::pwr.t2n.test(",
            "n1 = ", input$n_tratamento, ", ",
            "n2 = ", input$n_controle, ", ",
            "d = ", round(d, 4), ", ",
            "sig.level = ", input$alpha, "/100, ",
            "power = NULL, ",
            "alternative = '", th_alternativa(), "')"
          )


          poder <- try_n(code)
          poder <- round(poder$power*100, 1)
          eval(parse(text = validate_n("poder")))

          paste0(
            "<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", poder, "%",
            "</font></b></br></br>",


            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>",
              translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
              "O poder",
              if (th_alternativa() == "two.sided") {
                paste0(
                  " para testar se existe uma diferença mínima ",
                  if (!input$calcular_utilizando_d_cohen) paste0("de <b>", input$diferenca_para_detectar, " ", unidade_medida(), "</b> "),
                  "nas médias de <i>",
                  nome_desfecho(), "</i> entre os grupos <i>", nome_grupo_tratamento(), "</i> e <i>", nome_grupo_controle(), "</i> "
                )
              } else {
                paste0(
                  "para testar se  a média de <i>", nome_desfecho(), "</i> do grupo <i>", nome_grupo_tratamento(), "</i> é ",
                  if (th_alternativa() == "less") "menor" else "maior",
                  " do que a do grupo <i>", nome_grupo_controle(), "</i> ",
                  if (!input$calcular_utilizando_d_cohen) paste0("em <b>", input$diferenca_para_detectar, " ", unidade_medida(), "</b> ")
                )
              },
              "é de <b>", poder, "%</b>. ",
              "Este valor foi obtido considerando nível de significância de <b>", input$alpha, "%</b>, ",
              if (input$n_tratamento == input$n_controle) {
                paste0("tamanho amostral igual a <b>", input$n_controle, "</b> sujeitos em cada grupo ")
              } else {
                paste0(
                  "tamanho amostral igual a <b>", input$n_tratamento, "</b> sujeitos para o grupo <i>",
                  nome_grupo_tratamento(), "</i>, <b>", input$n_controle, "</b> sujeitos para o grupo <i>",
                  nome_grupo_controle(), "</i> "
                )
              },
              if (input$calcular_utilizando_d_cohen) {
                paste0("e d de Cohen de <b>", input$d, "</b>, (referido por Fulano (1900) OU escolha do pesquisador). ")
              } else {
                paste0(
                  "e desvio padrão igual a <b>", input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)). "
                )
              },
              .txt_citacao_pss
            ),
            .txt_referencia_tap,
            print_r_code(code)
          )


        } else {

          code <- paste0(
            "EnvStats::ciNormN(",
            "half.width = ", input$precisao, ", ",
            "sigma.hat = ", input$sigma_estimar,  ", ",
            "conf.level = ", input$confianca,  "/100, ",
            "sample.type = 'two.sample')"
          )

          n <- try_n(code)
          n1 <- n
          n2 <- n
          n <- n1 + n2
          nperdas1 <- n_perdas(n1, input$perc_perdas)
          nperdas2 <- n_perdas(n2, input$perc_perdas)
          eval(parse(text = validate_n_inf("n")))

          # print_r_code(code)

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

              if (n1 != n2) {
                paste0(
                  " (<i>", n1, " ", nome_grupo_tratamento(), translation_pss(" e ", linguagem()), n2, " ", nome_grupo_controle(), "</i>)"
                )
              } else {
                paste0(
                  " (<i>", n1, " ", translation_pss("para cada grupo", linguagem()), "</i>)"
                )
              },


              " para estimar a diferença entre as médias de <i>",
              nome_desfecho(), "</i> nos grupos <i>", nome_grupo_tratamento(), "</i> e <i>", nome_grupo_controle(), "</i>, ",

              "com margem de erro igual a <b>", input$precisao, " ", unidade_medida(), "</b> ",
              "(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ",
              "O cálculo considerou nível de confiança de <b>", input$confianca, "</b>% e desvio padrão esperado igual a <b>", input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)). ",
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

        if (input$calcular_utilizando_d_cohen) {
          req(!is.null(input$d))
          val_min <- ifelse(input$d < 0, input$d - 2, input$d)
          val_max <- ifelse(input$d < 0, input$d, input$d + 2)

        } else {
          req(!is.null(input$diferenca_para_detectar))
          val_min <- ifelse(input$diferenca_para_detectar < 0, input$diferenca_para_detectar - 1, input$diferenca_para_detectar)
          val_max <- ifelse(input$diferenca_para_detectar < 0, input$diferenca_para_detectar, input$diferenca_para_detectar + 1)
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


          if (input$calcular_utilizando_d_cohen) {
            HTML("<b>",
                 translation_pss("Defina a sequência de valores para a magnitude do efeito", linguagem()),
                 "</b>"
            )
          } else {
            HTML(
              "<b>",
              translation_pss("Defina a sequência de valores para a diferença a ser detectada", linguagem()),
              "</b>"
            )
          },


          br(),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput(ns("from"), translation_pss("Mínimo", linguagem()), value = val_min, step = .5)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("to"), translation_pss("Máximo", linguagem()), value = val_max, step = .5)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("by"), translation_pss("Intervalo", linguagem()), value = 0.5, min = 0, step = .1) %>%
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



      tab_TH_cenarios <- reactive({

        poder <- text_input_to_vector(input$poder_cenarios)
        req(length(poder) > 0)


        if (input$calcular_utilizando_d_cohen) {

          expand.grid(d = seq(from = input$from, to = input$to, by = input$by),
                      poder = poder,
                      alpha =  input$alpha,
                      alternativa_temp = th_alternativa() == "two.sided",
                      stringsAsFactors = FALSE) %>%

            mutate(
              h1 = case_when(alternativa_temp ~ "two.sided",
                             d > 0 ~ "greater",
                             d < 0 ~ "less"),

              n = mapply(
                function(d, sig.level, power, alternative){
                  tryCatch({
                    pwr::pwr.t.test(d = d,
                                    sig.level = sig.level/100,
                                    power = power/100,
                                    alternative = alternative,
                                    type = 'two.sample')$n * 2},
                    warning = function(warning_condition) { NA },
                    error   = function(error_condition) { NA })
                }, d, alpha, poder, h1),

              n = 2 * ceiling(n/2)
            )

        } else{

          req(!is.null(input$sigma))


          expand.grid(
            diferenca = seq(from = input$from, to = input$to, by = input$by),
            poder = poder,
            alpha =  input$alpha,
            sigma = input$sigma,
            balanceamento = input$balanceamento,
            h1 = th_alternativa(),
            deff = input$deff,
            stringsAsFactors = FALSE
          )%>%
            mutate(
              n1 = mapply(
                function(diferenca, sigma, balanceamento, poder, alpha, h1, deff) {
                  tryCatch({
                    epiR::epi.sscompc(
                      control = 0,
                      treat = diferenca,
                      n = NA,
                      sigma = sigma,
                      power = poder/100,
                      r = balanceamento,
                      conf.level = 1 - alpha/100,
                      sided.test = (h1 == "two.sided") + 1,
                      design = deff,
                      nfractional = FALSE
                    )$n.treat
                  },
                  warning = function(warning_condition) { NA },
                  error   = function(error_condition) { NA })
                }, diferenca,
                sigma,
                balanceamento,
                poder,
                alpha,
                h1,
                deff
              ),
              n2 = ceiling(n1/balanceamento),
              n = n1 + n2
            )
        }


      })


      output$grafico_cenarios <- plotly::renderPlotly({

        metrica <- case_when(
          input$calcular_utilizando_d_cohen ~ "d",
          TRUE ~ "diferenca"
        )

        xlab <- case_when(
          input$calcular_utilizando_d_cohen ~ translation_pss("d de Cohen", linguagem()),
          TRUE ~ translation_pss("Diferença mínima a ser detectada", linguagem())
        )

        g1 <- tab_TH_cenarios() %>%
          mutate(
            `Poder (%)` = factor(poder)
          ) %>%
          ggplot(
            aes(x = !! sym(metrica),
                y = n,
                color = `Poder (%)`
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

        df_ <- tab_TH_cenarios()

        if (input$calcular_utilizando_d_cohen) {

          df_ <- df_ %>%
            dplyr::select(-alternativa_temp) %>%
            mutate(
              n1 = n/2,
              n2 = n/2
            )

          colnames(df_) <- c(
            translation_pss("d de Cohen", linguagem()),
            translation_pss("Poder (%)", linguagem()),
            translation_pss("Nível de significância (%)", linguagem()),
            translation_pss("Hipótese alternativa", linguagem()),
            translation_pss("Tamanho amostral", linguagem()),
            paste0("n ", nome_grupo_tratamento()),
            paste0("n ", nome_grupo_controle())
          )


        } else {
          colnames(df_) <- c(
            translation_pss("Diferença mínima a ser detectada", linguagem()),
            translation_pss("Poder (%)", linguagem()),
            translation_pss("Nível de significância (%)", linguagem()),
            translation_pss("Desvio padrão", linguagem()),
            translation_pss("Balanceamento", linguagem()),
            translation_pss("Hipótese alternativa", linguagem()),
            translation_pss("Efeito do plano amostral", linguagem()),
            paste0("n ", nome_grupo_tratamento()),
            paste0("n ", nome_grupo_controle()),
            translation_pss("Tamanho amostral", linguagem())
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
        filename = function() { "Cenarios_tamanho_amostra_duas_medias_independentes.xlsx"},
        content = function(file) {writexl::write_xlsx(return_table_tabela_cenarios(),
                                                      path = file)}
      )





    } # Nao mexer!!!
  )

}
