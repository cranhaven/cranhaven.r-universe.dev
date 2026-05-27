
mod_2_medias_assimetricas_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_2_medias_assimetricas_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, txt_balanceamento_f, h1, cohen_d,
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


            # numericInput( ns("diferenca_para_detectar"),
            #               paste0(
            #                 translation_pss("Diferença mínima a ser detectada", linguagem()), " (",
            #                 translation_pss("em", linguagem()), " ",
            #                 unidade_medida(), ")"
            #               ),
            #               value = 1,
            #               min = -Inf,
            #               max = Inf,
            #               step = .5
            # ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_diferenca_clinica, title = translation_pss("Diferença mínima a ser detectada", linguagem())),

            if (linguagem() == "pt") {
              HTML("<b><font size = '2.95'>Média esperada do grupo</font></b><br>")
            } else {
              HTML("<b><font size = '2.95'>Mean of group</font></b><br>")
            },
            div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                numericInput( ns("media_controle"),
                              nome_grupo_controle(),
                              value = 3,
                              min = 0,
                              max = Inf,
                              step = 1
                )
            ),
            div(style="display: inline-block;vertical-align:top; width: 49%;",
                numericInput( ns("media_tratamento"),
                              nome_grupo_tratamento(),
                              value = 6,
                              min = 0,
                              max = Inf,
                              step = 1
                )
            ),

            HTML("<b><font size = '2.95'>Parâmetro de forma do grupo</font></b><br>"),

            div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                numericInput( ns("forma_controle"),
                              nome_grupo_controle(),
                              value = 1,
                              min = 0,
                              max = Inf,
                              step = .5
                )
            ),
            div(style="display: inline-block;vertical-align:top; width: 49%;",
                numericInput( ns("forma_tratamento"),
                              nome_grupo_tratamento(),
                              value = 1.5,
                              min = 0,
                              max = Inf,
                              step = .5
                )
            ),

            actionLink(
              ns("show_forma"),
              HTML("<b>", translation_pss("Como definir o parâmetro de forma?", linguagem()), "</b>"),
            ),
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
                                   title = translation_pss("Balanceamento", linguagem()))
              )
            },


            if (tipo == "tamanho_amostral") {
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
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem()))

                # selectInput(ns('th_alternativa'),
                #             translation_pss('Tipo de teste de acordo com hipótese alternativa', linguagem()),
                #             choices = h1(),
                #             selected = 'Bilateral'
                # ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_h1)
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

            htmlOutput(ns("texto_principal")) %>%
              shinycssloaders::withSpinner(type = 5),

            htmlOutput(ns("code")),

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






      # Mudar nomes -----

      observeEvent(input$mudar_nomes, {
        showModal(
          modalDialog(
            title = translation_pss("Ajustes", linguagem()),
            fluidPage(fluidRow(

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

            )),
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





      # Forma ----


      observeEvent(input$show_forma, {
        validate(need(input$show_forma > 0, ''))

        showModal(
          modalDialog(
            fluidRow(

              fluidPage(textInput(
                inputId = ns("forma_cenarios"),
                label   = translation_pss("Valores de forma", linguagem()),
                value   = "1, 1.5, 5"
              )),

              column(
                width = 6,
                fluidPage(
                  plotOutput(ns("plot_forma_contr"), height = "300px"),
                  br(),
                  tableOutput(ns("table_forma_contr")),
                )
              ),
              column(
                width = 6,
                fluidPage(

                  # textInput(
                  #   inputId = ns("forma_cenarios_trat"),
                  #   label   = "Valores de forma",
                  #   value   = "1, 1.5, 5"
                  # ),

                  plotOutput(ns("plot_forma_trat"), height = "300px"),
                  br(),
                  tableOutput(ns("table_forma_trat")),
                )
              ),

            ),
            title = translation_pss("Como definir o parâmetro de forma?", linguagem()),
            easyClose = TRUE,
            footer    = NULL,
            size      = "l"
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









      output$th_h0 <- renderUI({
        # req(!is.null(input$th_alternativa))

        # sinal_h0 <- case_when(input$th_alternativa == 'two.sided' ~ "=",
        #                       input$th_alternativa == 'greater'   ~ "\\leq",
        #                       input$th_alternativa == 'less'      ~ "\\geq")

        sinal_h0 = "="
        withMathJax(
          paste0(
            "$$",
            "H_0: \\mu_\\text{", nome_grupo_tratamento(), "}",  "\\text{/} \\mu_\\text{", nome_grupo_controle(), "} ", sinal_h0, "1",
            "$$"
          )
        )
      })

      output$th_h1 <- renderUI({
        # req(!is.null(input$th_alternativa))

        # sinal_h1 <- case_when(input$th_alternativa == 'two.sided' ~ "\\neq",
        #                       input$th_alternativa == 'greater'   ~ ">",
        #                       input$th_alternativa == 'less'      ~ "<")

        sinal_h1 = "\\neq"
        withMathJax(
          paste0(
            "$$",
            "H_1: \\mu_\\text{", nome_grupo_tratamento(), "}", "\\text{/} \\mu_\\text{", nome_grupo_controle(), "} ", sinal_h1, "1",
            "$$"
          )
        )
      })







      # Grafico forma ----

      # Funcao auxiliar
      simula_descritiva <- function(mu, shape = c(1, 2, 3, 5)) {

        expand.grid(
          Shape = shape
        ) %>%
          dplyr::as_tibble() %>%
          mutate(
            Scale = mu/Shape,
            Mean = Shape*Scale,
            `Standard deviation` = sqrt(Shape*Scale^2),
            Median = qgamma(p = 0.5, shape = Shape, scale = Scale),
            P25 = qgamma(p = 0.25, shape = Shape, scale = Scale),
            P75 = qgamma(p = 0.725, shape = Shape, scale = Scale)
          )
      }




      ## Tratamento ----

      forma_cenarios <- reactive({
        formas <- text_input_to_vector(input$forma_cenarios)
        req(length(formas) > 0)
        formas
      })


      descritiva_distribuicao_trat <- reactive({
        simula_descritiva(mu = input$media_tratamento, shape = forma_cenarios())
      })
      output$table_forma_trat <- renderTable({

        tabela_print <- descritiva_distribuicao_trat() %>%
          dplyr::select(-Mean, - Scale)

        if (linguagem() == "pt") {
          tabela_print %>%
            dplyr::rename(
              `Desvio padrão` = `Standard deviation`,
              Forma = Shape,
              Mediana = Median,
              `Quartil 1` = P25,
              `Quartil 3` = P75
            )
        } else {
          tabela_print
        }
      })




      output$plot_forma_trat <- renderPlot({

        x <- seq(0, 200, 0.1)
        comb <- descritiva_distribuicao_trat()

        densidades <- dgamma(x, shape = comb$Shape[1], scale = comb$Scale[1])

        if (nrow(comb) > 1) {
          for(i in 2:nrow(comb)) {
            densidades <- c(densidades, dgamma(x, shape = comb$Shape[i], scale = comb$Scale[i]))
          }
        }


        data.frame(
          x = x,
          y = densidades,
          Forma = factor(rep(comb$Shape, each = length(x)))
        ) %>%
          subset(y > 1e-3) %>%
          ggplot(aes(x = x, y = y, colour = Forma)) +
          geom_line(linejoin = "bevel") +
          scale_color_brewer(
            name = translation_pss("Forma", linguagem()),
            palette = "Set1"
          ) +
          labs(title = paste0(
            translation_pss("Distribuições com médias iguais a", linguagem()),
            " ", input$media_tratamento, " ", input$unidade_medida)
          ) +
          ylab(translation_pss("Densidade", linguagem())) +
          xlab(nome_desfecho()) +
          theme_bw() +
          theme(
            axis.text  = element_text(colour = "black"),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank()
          )

      })




      ## Controle -----


      descritiva_distribuicao_contr <- reactive({
        simula_descritiva(mu = input$media_controle, shape = forma_cenarios())
      })
      output$table_forma_contr <- renderTable({
        tabela_print <- descritiva_distribuicao_contr() %>%
          dplyr::select(-Mean, -Scale)

        if (linguagem() == "pt") {
          tabela_print %>%
            dplyr::rename(
              `Desvio padrão` = `Standard deviation`,
              Forma = Shape,
              Mediana = Median,
              `Quartil 1` = P25,
              `Quartil 3` = P75
            )

        } else {
          tabela_print
        }

      })



      output$plot_forma_contr <- renderPlot({

        x <- seq(0, 200, 0.1)
        comb <- descritiva_distribuicao_contr()

        densidades <- dgamma(x, shape = comb$Shape[1], scale = comb$Scale[1])

        if (nrow(comb) > 1) {
          for(i in 2:nrow(comb)) {
            densidades <- c(densidades, dgamma(x, shape = comb$Shape[i], scale = comb$Scale[i]))
          }
        }


        data.frame(
          x = x,
          y = densidades,
          Forma = factor(rep(comb$Shape, each = length(x)))
        ) %>%
          subset(y > 1e-3) %>%
          ggplot(aes(x = x, y = y, colour = Forma)) +
          geom_line(linejoin = "bevel") +
          scale_color_brewer(
            name = translation_pss("Forma", linguagem()),
            palette = "Set1"
          ) +
          labs(title = paste0(
            translation_pss("Distribuições com médias iguais a", linguagem()),
            " ",input$media_controle, " ", input$unidade_medida)
          ) +
          ylab(translation_pss("Densidade", linguagem())) +
          xlab(nome_desfecho()) +
          theme_bw() +
          theme(
            axis.text  = element_text(colour = "black"),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank()
          )

      })








      # Texto -----

      output$code <- renderText({

        paste0(

          if (linguagem() == "pt") {
            "</br></br><i>Comando R utilizado:</i><br>"
          } else {
            "</br></br><i>R code:</i><br>"
          },
          "<p style=\"font-family:'Courier New';font-size:100% \">",

          code(
            "#Cundill, B., Alexander, N.D. Sample size calculations for skewed distributions. <i>BMC Med Res Methodol</i> <b>15</b>, 28 (2015). https://doi.org/10.1186/s12874-015-0023-0"
          ),

          "<br>",
          code(paste0("alpha = ", input$alpha, "/100")), "<br>",
          code(paste0("power =  ", input$poder, "/100")), "<br>",
          code(paste0("muA =  ", input$media_controle)), "<br>",
          code(paste0("muB = ", input$media_tratamento)), "<br>",
          code(paste0("kA = ", input$forma_controle)), "<br>",
          code(paste0("kB = ", input$forma_tratamento)), "<br>",
          code(paste0("pB = ", input$balanceamento, "/(1 + ", input$balanceamento, ")")), "<br>",
          code(paste0("pA = 1 - pB")), "<br>",

          code("za <- qnorm(1-(alpha/2))"), "<br>",
          code("zb  <- qnorm(power)"), "<br>",
          code("num <- (za + zb)*sqrt(1/(pA*kA) + 1/(pB*kB))"), "<br>",
          code("den <- log(muA) - log(muB)"), "<br>",
          code("n <- (num/den)^2"), "<br>",

          code("ceiling(n*pA)"), "<br>",
          code("ceiling(n*pB)"),


          if (linguagem() == "pt") {
            "</p><br><br><b><i>* Sempre procure um profissional de estatística para orientações no planejamento do estudo.</b></i>"
          } else {
            "</p><br><br><b><i>* Always consult a statistician for guidance in study design.</b></i>"
          }

        )


      })


      output$texto_principal <- renderText({


        alpha = input$alpha/100
        power = input$poder/100
        muA = input$media_controle
        muB = input$media_tratamento
        kA = input$forma_controle
        kB = input$forma_tratamento
        pB = input$balanceamento/(1 + input$balanceamento)
        pA = 1 - pB


        za <- qnorm(1-(alpha/2))
        zb  <- qnorm(power)
        num <- (za + zb)*sqrt(1/(pA*kA) + 1/(pB*kB))
        den <- log(muA) - log(muB)

        n <- (num/den)^2

        eval(parse(text = validate_n("n")))

        n1 <- ceiling(n*pA)
        n2 <- ceiling(n*pB)

        n <- n1 + n2
        nperdas1 <- n_perdas(n1, input$perc_perdas)
        nperdas2 <- n_perdas(n2, input$perc_perdas)
        eval(parse(text = validate_n_inf("n")))



        paste0(
          "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
          if (n1 != n2) {
            paste0(
              " (<i>", n1, " ", nome_grupo_controle(), translation_pss(" e ", linguagem()), n2, " ", nome_grupo_tratamento(), "</i>)"
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
                "(", n1, " no grupo ", nome_grupo_controle(), " e ", n2, " no grupo ", nome_grupo_tratamento(), ")"
              )
            } else {
              paste0(
                "(", n1, " para cada grupo)"
              )
            },

            " para testar se razão de médias de <i>",
            nome_desfecho(), "</i> entre os grupos <i>", nome_grupo_tratamento(), "</i> e <i>", nome_grupo_controle(), "</i> ",
            " é diferente de 1 ",

            if (input$balanceamento == 1) {
              paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ")
            } else {
              paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser ", nperdas1, " ", nome_grupo_controle(), " e ", nperdas2, " ", nome_grupo_tratamento(), "). ")
            },

            "O cálculo considerou poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b>, ",

            if (input$forma_controle != input$forma_tratamento) {
              paste0(
                " médias esperadas de <b>", input$media_controle, " ", unidade_medida(), "</b> e <b>", input$media_tratamento, " ", unidade_medida(), "</b> e ",
                "parâmetro de forma igual a ", input$forma_controle, " e ", input$forma_tratamento,
                " para os grupos <i>", nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(), "</i>, respectivamente"
              )
            } else {
              paste0(
                " médias esperadas de <b>", input$media_controle, " ", unidade_medida(), "</b> e <b>", input$media_tratamento, " ", unidade_medida(), "</b> ",
                "para os grupos <i>", nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(), "</i>, respectivamente",
                "e parâmetro de forma igual a ", input$forma_controle
              )
            },
            " (referido por Fulano (1900)). ",
            .txt_citacao_pss
          ),
          .txt_referencia_tap
          # print_r_code(code)

        )
    })




    } # Nao mexer!!!
  )

}




