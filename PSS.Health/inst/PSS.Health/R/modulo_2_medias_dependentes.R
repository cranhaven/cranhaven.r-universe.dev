
mod_2_medias_dependentes_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_2_medias_dependentes_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, h1,
                                            translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                            warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas, warning_numero,
                                            lista_de_funcoes_server){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      eval(parse(text = warning_numero("diferenca_para_detectar")))
      eval(parse(text = warning_numero_positivo("sigma")))

      eval(parse(text = warning_prop("poder")))
      eval(parse(text = warning_prop("alpha")))
      eval(parse(text = warning_prop("confianca")))
      eval(parse(text = warning_prop("precisao")))
      eval(parse(text = warning_perdas("perc_perdas")))
      eval(parse(text = warning_inteiro("n")))



      # Aba ----

      output$aba <- renderUI({


        sidebarLayout(
          sidebarPanel(


            wellPanel(HTML(
              '<b><a href="https://youtu.be/mp8qbyUSqV0" target="_blank">',
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
                uiOutput(ns("th_h"))
              )
            },

            actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),
            br(), br(),

            if (tipo != "estimar") {
              numericInput( ns("diferenca_para_detectar"),
                            paste0(
                              translation_pss("Média das diferenças a ser detectada", linguagem()), " (",
                              nome_grupo_controle(), " - ",
                              nome_grupo_tratamento(), ")"
                            ),
                            value = 5,
                            min = -Inf,
                            max = Inf,
                            step = .5
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_diferenca_clinica, title = translation_pss("Diferença mínima a ser detectada", linguagem()))
            },

            numericInput( ns("sigma"),
                          translation_pss("Desvio padrão da diferença", linguagem()),
                          value = 12,
                          min = 0,
                          max = Inf,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão", linguagem())),

            actionLink(ns("show_desvio_diferenca"), translation_pss("Calcular o desvio padrão da diferença", linguagem())),
            br(), br(),


            if (tipo == "poder") {
              numericInput( ns("n"),
                            translation_pss("Tamanho amostral", linguagem()),
                            value = 35,
                            min = 0,
                            step = 1
              )
            },


            if (tipo == "estimar") {
              numericInput( ns("precisao"),
                            translation_pss("Margem de erro/ semi-amplitude", linguagem()),
                            value = 2,
                            min = 0,
                            max = Inf,
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
            htmlOutput(ns("texto_principal")) %>%
              shinycssloaders::withSpinner(type = 5),

              uiOutput(ns("cenarios"))
          )
        )
      })





      # Desvio padrão da diferenca ----


      observeEvent(input$show_desvio_diferenca, {
        validate(need(input$show_desvio_diferenca > 0, ''))

        showModal(
          modalDialog(
            fluidPage(

              HTML("<b><font size = '3'>", translation_pss("Desvio padrão", linguagem()), " ",
                   translation_pss("no", linguagem()),
                   "</font></b><br>"
              ),

              div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                  numericInput(ns("sigma_A"),
                               nome_grupo_tratamento(),
                               value = 4,
                               min = 0,
                               max = Inf,
                               step = 1
                  )
              ),

              div(style="display: inline-block;vertical-align:top; width: 49%;",
                  numericInput(ns("sigma_B"),
                               nome_grupo_controle(),
                               value = 4,
                               min = 0,
                               max = Inf,
                               step = 1
                  )
              ),


              numericInput( ns("correlacao"),
                            paste0(
                              translation_pss("Coeficiente de correlação entre", linguagem()), " ",
                              nome_grupo_tratamento(), " e ",
                              nome_grupo_controle()
                            ),
                            value = 0.5,
                            min = -1,
                            max = 1,
                            step = .1
              ),

              htmlOutput(ns("resultado"))
            ),
            title = translation_pss("Obter o desvio padrão da diferença", linguagem()),
            easyClose = TRUE,
            footer    = NULL,
            size      = "m"
          )
        )
      })


      output$resultado <- renderText({

        s1 <- input$sigma_A
        s2 <- input$sigma_B

        temp <- s1^2 + s2^2 - (2*s1*s2*input$correlacao)

        paste0("<b><font size = '5'><br><br>",
               "<i>",
               translation_pss("Desvio padrão da diferença", linguagem()),
               "</i> = ", round(sqrt(temp), 4)
        )
      })





      # Hipoteses a serem testadas ----

      th_alternativa <- reactive({
        if (input$th_alternativa == "Bilateral" ) {
          "two.sided"
        } else if (input$diferenca_para_detectar < 0) "less" else "greater"
      })




      output$th_h <- renderUI({
        req(!is.null(th_alternativa()))

        sinal_h0 <- case_when(th_alternativa() == 'two.sided' ~ "=",
                              th_alternativa() == 'greater'   ~ "\\leq",
                              th_alternativa() == 'less'      ~ "\\geq")

        sinal_h1 <- case_when(th_alternativa() == 'two.sided' ~ "\\neq",
                              th_alternativa() == 'greater'   ~ ">",
                              th_alternativa() == 'less'      ~ "<")

        withMathJax(paste0("$$H_0: \\mu_D ", sinal_h0, " 0",
                           " \\text{  vs  } H_1: \\mu_D", sinal_h1, " 0 $$"))
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
                        label   = translation_pss("Descreva um nome para o grupo A", linguagem()),
                        value   = ifelse(input$mudar_nomes == 0, translation_pss("A", linguagem()), nome_grupo_tratamento())),

              HTML("<i>Em estudos longitudinais o Grupo 1 pode ser entendido como o Momento 1.</i><br><br>"),
              # HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamado de grupo Intervenção ou grupo Exposto.</i><br><br>"),

              textInput(inputId = ns("nome_grupo_controle"),
                        label   = translation_pss("Descreva um nome para o grupo B", linguagem()),
                        value   = ifelse(input$mudar_nomes == 0, translation_pss("B", linguagem()), nome_grupo_controle())),

              HTML("<i>Em estudos longitudinais o Grupo 2 pode ser entendido como o Momento 2.</i><br><br>"),
              # HTML("<i>Em alguns estudos o grupo Controle também pode ser chamado de grupo Placebo/ Sham ou grupo Não exposto.</i>"),

            ),
            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      nome_grupo_controle <- reactive({
        ifelse(is.null(input$nome_grupo_controle), translation_pss("B", linguagem()), input$nome_grupo_controle)
      })

      nome_grupo_tratamento <- reactive({
        ifelse(is.null(input$nome_grupo_tratamento), translation_pss("A", linguagem()), input$nome_grupo_tratamento)
      })

      nome_desfecho <- reactive({
        ifelse(is.null(input$nome_desfecho), "Y", input$nome_desfecho)
      })

      unidade_medida <- reactive({
        ifelse(is.null(input$unidade_medida),  translation_pss("u.m.", linguagem()), input$unidade_medida)
      })




      # Texto -----


      output$texto_principal <- renderText({


        if (tipo %in% c("tamanho_amostral", "poder")) {
          code <- paste0(
            "stats::power.t.test(",
            if (tipo == "tamanho_amostral") {
              paste0(
                "n = NULL, ",
                "power = ", input$poder, "/100, "
              )
            } else if (tipo == "poder") {
              paste0(
                "n = ", input$n, ", ",
                "power = NULL, "
              )
            },
            "sig.level = ", input$alpha, "/100, ",
            "delta = abs(", input$diferenca_para_detectar, "), ",
            "sd = ", input$sigma, ", ",
            "type = 'paired', ",
            "alternative = '",
            ifelse(th_alternativa() == "two.sided", "two.sided", "one.sided"),
            "')"
          )
        } else {
          code <- paste0(
            "presize::prec_mean(",
            "mean = 0, ",
            "sd = ", input$sigma, ", ",
            "conf.width = ", input$precisao, "*2, ",
            "conf.level = ", input$confianca, "/100)"
          )
        }

          n <- try_n(code)
          eval(parse(text = validate_n("n")))

          n <- ifelse(tipo %in% c("tamanho_amostral", "estimar"), ceiling(n$n), round(n$power*100, 1))

          eval(parse(text = validate_n_inf("n")))


          if (tipo == "tamanho_amostral") {
            paste0(
              "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
              "</font></b></br></br>",


              lista_de_funcoes_server()$sugestao_texto_portugues(
                "<i>",
                translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
                "Foi calculado um tamanho de amostra de <b>", n, "</b> pares ",

                if (th_alternativa() == "two.sided") {
                  paste0(
                    " para testar se existe uma diferença mínima de <b>", input$diferenca_para_detectar, " ", unidade_medida(), "</b> ",
                    "na média das diferenças de <i>",
                    nome_desfecho(), "</i> entre os grupos <i>", nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(), "</i> "
                  )
                } else {
                  paste0(
                    "para testar se  a média das diferenças de <i>", nome_desfecho(), "</i> entre os grupos <i>", nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(), "</i> é ",
                    ifelse(th_alternativa() == "less", "menor", "maior"),
                    " do que zero "
                  )
                },

                "(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",
                "O cálculo considerou poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b> ",
                " e desvio padrão das diferenças igual a <b>", input$sigma, " ", unidade_medida(), "</b> (Fulano (1900)). ",
                .txt_citacao_pss
              ),
              .txt_referencia_tap,
              print_r_code(code)
            )



          } else if (tipo == "poder") {
            paste0(
              "<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", n, "%",
              "</font></b></br></br><i>",


              lista_de_funcoes_server()$sugestao_texto_portugues(
                translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
                "O poder",
                if (th_alternativa() == "two.sided") {
                  paste0(
                    " para testar se existe uma diferença mínima de <b>", input$diferenca_para_detectar, " ", unidade_medida(), "</b> ",
                    "na média das diferenças de <i>",
                    nome_desfecho(), "</i> entre os grupos <i>", nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(), "</i> "
                  )
                } else {
                  paste0(
                    " para testar se  a média das diferenças de <i>", nome_desfecho(), "</i> entre os grupos <i>", nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(), "</i> é ",
                    ifelse(th_alternativa() == "less", "menor", "maior"),
                    " do que zero "
                  )
                },
                "é de <b>", n, "%</b>. ",
                "Este valor foi obtido considerando nível de significância de <b>", input$alpha, "%</b>, ",
                "tamanho amostral igual a <b>", input$n, "</b> pares, ",
                " e desvio padrão das diferenças igual a <b>", input$sigma, " ", unidade_medida(), "</b> (Fulano (1900)). ",
                .txt_citacao_pss
                ),
              .txt_referencia_tap,
              print_r_code(code)

            )
          } else {


            paste0(
              "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
              "</font></b></br></br><i>",

              lista_de_funcoes_server()$sugestao_texto_portugues(
                translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

                "Foi calculado um tamanho de amostra de <b>", n, "</b> pares  para estimar a média das diferenças de <i>",
                nome_desfecho(), "</i> entre os grupos <i>", nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(), "</i>, ",
                "com margem de erro  igual a <b>", input$precisao, " ", unidade_medida(), "</b> ",
                "(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",

                "O cálculo considerou nível de confiança de <b>", input$confianca, "</b>%  e ",
                "desvio padrão das diferenças iguais a <b>", input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)). ",
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


        if (input$diferenca_para_detectar > 0) {
          val_min <- input$diferenca_para_detectar
          val_max  <- input$diferenca_para_detectar + 1
        } else {
          val_min <- input$diferenca_para_detectar - 1
          val_max  <- input$diferenca_para_detectar
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
            "<b>",
            translation_pss("Defina a sequência de valores para a diferença a ser detectada", linguagem()),
            "</b>"
          ),


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
                   textInput(inputId = ns("desvio_cenarios"),
                             label   = translation_pss("Digite valores de desvio padrão para fazer o gráfico", linguagem()),
                             value   = paste0(c(input$sigma, input$sigma + 0.2, input$sigma + 0.5), collapse = ", "),
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



      eval(parse(text = check_text_input_to_vector("desvio_cenarios")))



      tab_TH_cenarios <- reactive({

        desvio <- text_input_to_vector(input$desvio_cenarios)
        req(length(desvio) > 0)

        expand.grid(diferenca = seq(from = input$from, to = input$to, by = input$by),
                    desvio = desvio,
                    poder = input$poder,
                    alpha =  input$alpha,
                    alternativa = ifelse(th_alternativa() == "two.sided", "two.sided", "one.sided"),
                    stringsAsFactors = FALSE) %>%


          mutate(n = mapply(
            function(delta, sd, sig.level, power, alternative){
              tryCatch({
                ceiling(
                  stats::power.t.test(n = NULL,
                                      delta = abs(delta),
                                      sd = sd,
                                      type = 'paired',
                                      sig.level = sig.level/100,
                                      power = power/100,
                                      alternative = alternative)$n)

              },
              warning = function(warning_condition) { NA },
              error   = function(error_condition) { NA })
            }, diferenca, desvio, alpha, poder, alternativa)
          )

      })


      output$grafico_cenarios <- plotly::renderPlotly({

        desvio <- "DP"
        names(desvio) <- translation_pss("DP", linguagem())

        g1 <- tab_TH_cenarios() %>%
          mutate(
            DP = factor(desvio)
          ) %>%
          dplyr::rename(all_of(desvio)) %>%
          ggplot(
            aes(x = diferenca,
                y = n,
                color = !!sym(translation_pss("DP", linguagem()))
            )
          ) +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = seq(from = input$from, to = input$to, by = input$by)) +
          xlab(translation_pss("Diferença mínima a ser detectada", linguagem())) +
          ylab(translation_pss("Tamanho da amostra*", linguagem())) +
          theme_bw() +
          theme(axis.text = element_text(colour = "black")) +
          scale_color_brewer(
            name = translation_pss("Desvio padrão", linguagem()),
            palette = "Set1"
          )


        plotly::ggplotly(g1,
                         tooltip = c("x", "colour", "y")) %>%
          plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                            showarrow = F, xref='paper', yref='paper',
                                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                            font=list(size=10)))
      })





      return_table_tabela_cenarios <- reactive({

        df_ <- tab_TH_cenarios()

        colnames(df_) <- c(
          translation_pss("Diferença mínima a ser detectada", linguagem()),
          translation_pss("Desvio padrão", linguagem()),
          translation_pss("Poder (%)", linguagem()),
          translation_pss("Nível de significância (%)", linguagem()),
          translation_pss("Hipótese alternativa", linguagem()),
          translation_pss("Tamanho amostral", linguagem())
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
        filename = function() { "Cenarios_tamanho_amostra_duas_medias_dependentes.xlsx"},
        content = function(file) {writexl::write_xlsx(return_table_tabela_cenarios(),
                                                      path = file)}
      )





    } # Nao mexer!!!
  )

}
