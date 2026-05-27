
mod_correlacao_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_correlacao_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, txt_balanceamento_f,
                                  translation_pss, linguagem, .rodape, validate_n, try_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                  warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas, warning_correlacao, wp.correlation,
                                  lista_de_funcoes_server){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns



      eval(parse(text = warning_prop("auc", entre0e1 = TRUE)))

      eval(parse(text = warning_numero_positivo("amplitude")))
      eval(parse(text = warning_numero_positivo("balanceamento")))
      eval(parse(text = warning_prop("poder")))
      eval(parse(text = warning_prop("alpha")))
      eval(parse(text = warning_prop("confianca")))
      eval(parse(text = warning_perdas("perc_perdas")))

      eval(parse(text = warning_inteiro("n_controle")))
      eval(parse(text = warning_inteiro("n_tratamento")))




      # Aba  ----

      output$aba <- renderUI({


        tagList(

          sidebarLayout(
            sidebarPanel(

              if (tipo != "estimar") {
                wellPanel(
                  HTML(paste0(
                    "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                  )),
                  uiOutput(ns("th"))
                )
              } else {

                tagList(
                  wellPanel(HTML(
                    '<b><a href="https://youtu.be/4pP4PwlgVnU" target="_blank">',
                    translation_pss("Vídeo: PSS Health para estimar a correlação", linguagem()),
                    '</a></b><br>'
                  )),
                  radioButtons(
                    inputId = ns("tipo_coeficiente"),
                    label   = translation_pss("Coeficiente de correlação", linguagem()),
                    choices = c("Pearson"  = "pearson",
                                "Spearman" = "spearman",
                                "Kendall"  = "kendall"),
                    selected = "pearson",
                    inline   = TRUE)
                )
              },

              textInput(inputId = ns("x1x2"),
                        label   = translation_pss("Descreva o nome das variáveis que deseja correlacionar", linguagem()),
                        value   = translation_pss("X1 e X2", linguagem())
              ),

              numericInput( ns("r"),
                            translation_pss("Coeficiente de correlação esperado", linguagem()),
                            value = .7,
                            min = 0,
                            max = 1,
                            step = .01
              ) %>% .help_buttom(linguagem = linguagem(), 
                body = txt_ajuda()$txt_correlacao,
                title = translation_pss("Coeficiente de correlação esperado", linguagem())
              ),





              if (tipo == "estimar") {
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




              } else {
                tagList(
                  numericInput( ns("r_h0"),
                                translation_pss("Valor de referência sob a hipótese nula", linguagem()),
                                value = 0.5,
                                min = -1,
                                max = 1,
                                step = .1
                  ) %>% .help_buttom(linguagem = linguagem(), body = paste0(
                    "Coeficiente de correlação linear de Pearson sob a hipótese nula. ",
                    "Mais informações em ",
                    '<a href="https://doi.org/10.4322/2357-9730.93649" target="_blank">Hirakata et al. 2019</a>.',
                    txt_ajuda()$txt_definido_pesquisador
                  )),

                  numericInput( ns("p_parcial"),
                                translation_pss("Número de variáveis para correlação parcial", linguagem()),
                                value = 0,
                                min = 0,
                                max = Inf,
                                step = 1
                  ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Número de variáveis para correlação parcial.", txt_ajuda()$txt_definido_pesquisador)),


                  if (tipo == "tamanho_amostral") {
                    numericInput( ns("poder"),
                                  translation_pss("Poder (%)", linguagem()),
                                  value = 80,
                                  min = 0,
                                  max = 100,
                                  step = 1
                    ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
                  } else {
                    numericInput( ns("n"),
                                  translation_pss("Tamanho amostral", linguagem()),
                                  value = 200,
                                  min = 4,
                                  step = 1
                    )
                  },

                  numericInput( ns("alpha"),
                                translation_pss("Nível de significância (%)", linguagem()),
                                value = 5,
                                min = 0,
                                max = 100,
                                step = 1
                  ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem())),



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





      # Hipoteses a serem testadas ----

      output$th <- renderUI({
        req(tipo != "estimar")

        withMathJax(paste0(
          "$$H_0: \\rho = ", input$r_h0,
          " \\text{  vs  } H_1: \\rho \\neq", input$r_h0, "$$"
        ))
      })







      # Texto ----


      output$texto_principal <- renderText({


        if (tipo %in% c("tamanho_amostral", "poder")) {

          code <- paste0(
            "wp.correlation(",
            "r = ", input$r, ", ",
            "p = ", input$p_parcial, ", ",
            "rho0 = ", input$r_h0, ", ",
            "alpha = ", input$alpha,  "/100, ",

            if (tipo == "tamanho_amostral") {
              paste0(
                "power = ", input$poder, "/100, ",
                " n = NULL"
              )
            } else {
              paste0(
                "n = ", input$n, ", ",
                "power = NULL"
              )
            },
            ")"
          )
          # print_r_code(code)


          if (tipo %in% c("tamanho_amostral")) {

            n <- try_n(code)
            n <- ceiling(n$n)
            eval(parse(text = validate_n("n")))
            eval(parse(text = validate_n_inf("n")))


            paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
                   "</font></b></br></br>",


                   lista_de_funcoes_server()$sugestao_texto_portugues(
                     "<i>",
                     translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

                     "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para testar se o coeficiente de correlação linear de Pearson, ",
                     "entre <i>", input$x1x2, "</i>, é diferente de <b>", input$r_h0, "</b>",
                     if (input$p_parcial > 0) {
                       paste0(", quando controlado pela(s) variável(eis) ",
                              sub(",([^,]*)$", " e\\1", paste(LETTERS[1:input$p_parcial], collapse = ", "))
                       )
                     },
                     " (com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",
                     "O cálculo considerou poder de <b>", input$poder, "%</b>, ",
                     "nível de significância de <b>", input$alpha, "%</b> ",
                     "e correlação esperada de <b>", input$r, "</b> conforme referência de Fulano (1900) <b>OU</b> escolha do pesquisador. ",
                     .txt_citacao_pss
                   ),
                   .txt_referencia_tap,
                   print_r_code(
                     gsub("wp.correlation(", "WebPower::wp.correlation(", code, fixed = TRUE)
                   )
            )

            # Poder ----
          } else {

            n <- try_n(code)
            poder <- round(n$power*100, 1)
            eval(parse(text = validate_n("poder")))
            eval(parse(text = validate_n_inf("poder")))

            paste0("<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", poder, "%</font></b></br></br>",

                   lista_de_funcoes_server()$sugestao_texto_portugues(
                     # "<i>",
                     "O poder para testar se o coeficiente de correlação linear de Pearson, ",
                     "entre <i>", input$x1x2, "</i>, é diferente de <b>", input$r_h0, "</b>",
                     if (input$p_parcial > 0) {
                       paste0(", quando controlado pela(s) variável(eis) ",
                              sub(",([^,]*)$", " e\\1", paste(LETTERS[1:input$p_parcial], collapse = ", "))
                       )
                     },
                     " é <b>", poder, "%</b>. ",
                     "Este valor foi obtido considerando nível de significância de <b>", input$alpha, "%</b>, ",
                     "tamanho amostral igual a <b>", input$n, "</b> ",
                     "e correlação esperada de <b>", input$r, "</b> conforme referência de Fulano (1900) <b>OU</b> escolha do pesquisador. ",
                     .txt_citacao_pss
                   ),
                   .txt_referencia_tap,
                   print_r_code(
                     gsub("wp.correlation(", "WebPower::wp.correlation(", code, fixed = TRUE)
                   )
            )

          }

          # Estimar ----
        } else {

          code <- paste0(
            "presize::prec_cor(",
            "r = abs(", input$r, "), ",
            "conf.width = ", input$amplitude, ", ",
            "conf.level = ", input$confianca, "/100, ",
            "method = '", input$tipo_coeficiente, "')"
          )

          n <- eval(parse(text = code))
          n <- n$n

          nperdas <- n_perdas(n, input$perc_perdas)


          paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
                 "</font></b></br></br>",


                 lista_de_funcoes_server()$sugestao_texto_portugues(
                   "<i>",
                   translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

                   "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para estimar o coeficiente de correlação ",
                   case_when(input$tipo_coeficiente == "pearson" ~ "linear de Pearson, ",
                             input$tipo_coeficiente == "spearman" ~ "de postos de Spearman, ",
                             TRUE ~ "de Kendal, "),
                   "entre <i>", input$x1x2, "</i>, ",
                   "com amplitude desejada para o intervalo de confiança de <b>", input$amplitude,
                   "</b> (com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas, "</b>). ",
                   "O cálculo considerou nível de confiança de <b>", input$confianca, "%</b> ",
                   "e correlação esperada de <b>", input$r, "</b> conforme referência de Fulano (1900) <b>OU</b> escolha do pesquisador. ",
                   .txt_citacao_pss
                 ),
                 .txt_referencia_tap,
                 print_r_code(code)
          )


        }


      })



      # Cenarios ----


      output$cenarios <- renderUI({

        # req(3 > 5)

        req(tipo != "poder")


        if (tipo == "estimar") {
          valores_caixa <- paste0(c(input$amplitude, input$amplitude + .1, input$amplitude + .2), collapse = ", ")
          label_caixa <- translation_pss("Digite valores de amplitude para fazer o gráfico", linguagem())
        } else {
          # a <- c(5, 10, 15)
          # a[a <= 10]
          poder_caixa <- c(input$poder, input$poder + 5, input$poder + 10)
          poder_caixa <- poder_caixa[poder_caixa < 100]

          valores_caixa <- paste0(poder_caixa, collapse = ", ")
          label_caixa <- translation_pss("Digite valores de poder (%) para fazer o gráfico", linguagem())
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

          HTML(paste0("<b>", translation_pss("Defina a sequência da correlação", linguagem()), "</b>")),


          br(),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput(ns("from"), translation_pss("Mínimo", linguagem()), value = max(abs(input$r), 0.05), step = .1, min = 0, max = 0)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("to"), translation_pss("Máximo", linguagem()), value = min(abs(input$r) + 0.2, 0.95), step = .1, min = 0, max = 1)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("by"), translation_pss("Intervalo", linguagem()), value = 0.1, min = 0, step = .1) %>%
                .help_buttom(linguagem = linguagem(), body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()),
                             title = "Sequência")
          ),



          fluidRow(
            column(6,
                   textInput(inputId = ns("caixa_cenarios"),
                             label   = label_caixa,
                             value   = valores_caixa,
                             width   = "400px") %>%
                     .help_buttom(linguagem = linguagem(), body = ajuda_cenarios_multiplos_valores())
            )
          ),

          # fluidRow(
          #   column(6,
          #          sliderInput(ns("range_cor_cenarios"),
          #                      translation_pss("Correlação esperada (em módulo)", linguagem()),
          #                      min = 0,
          #                      max = 1,
          #                      value = c(0.1, 0.9),
          #                      step  = 0.05,
          #                      width = "500px") %>%
          #            .help_buttom(linguagem = linguagem(), body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico.", linguagem()))
          #   )
          # ),


          plotly::plotlyOutput(ns("grafico_cenarios"), width = "80%") %>%
            shinycssloaders::withSpinner(type = 5),

          br(), br(),
          downloadButton(ns("download_tabela_cenarios"), translation_pss("Download tabela", linguagem())),
          DT::dataTableOutput(ns("tabela_cenarios"), width = "100%") %>%
            shinycssloaders::withSpinner(type = 5)

        ))

      })



      eval(parse(text = check_text_input_to_vector("caixa_cenarios")))



      tab_TH_cenarios <- reactive({

        caixa_cenarios <- text_input_to_vector(input$caixa_cenarios)
        req(length(caixa_cenarios) > 0)


        if (tipo == "estimar") {

          df_grid <- expand.grid(
            amplitude = caixa_cenarios,
            # correlacao = seq(input$range_cor_cenarios[1], input$range_cor_cenarios[2], 0.05),
            correlacao = seq(from = input$from, to = input$to, by = input$by),
            confianca  = input$confianca,
            coeficiente = input$tipo_coeficiente,
            stringsAsFactors = FALSE
          ) %>%
            mutate(
              n = mapply(
                FUN = function(conf, cor, w, method) {
                  n <- presize::prec_cor(
                    r = cor,
                    conf.width = w,
                    conf.level = conf/100,
                    method = method
                  )
                  return(n$n)
                },
                confianca,
                correlacao,
                amplitude,
                coeficiente),

              n = ceiling(n)
            )

        } else {


          expand.grid(
            poder      = caixa_cenarios,
            # correlacao = seq(input$range_cor_cenarios[1], input$range_cor_cenarios[2], 0.05),
            correlacao = seq(from = input$from, to = input$to, by = input$by),
            alpha = input$alpha,
            correlacao_h0 = input$r_h0,
            p_parcial     = input$p_parcial,
            stringsAsFactors = FALSE) %>%
            mutate(
              n = mapply(
                function(r, rho0, sig.level, power, p) {
                  tryCatch({
                    wp.correlation(n = NULL,
                                   r = r,
                                   power = power/100,
                                   p = p,  # Number of variables to partial out.
                                   rho0 = rho0,
                                   alpha = sig.level/100)$n },
                    warning = function(warning_condition) { NA },
                    error = function(error_condition) { NA })
                },
                correlacao,
                correlacao_h0,
                alpha,
                poder,
                p_parcial),
              n = ceiling(n)
            )

        }


      })


      output$grafico_cenarios <- plotly::renderPlotly({

        metrica <- case_when(
          tipo == "estimar" ~ "amplitude",
          TRUE ~ "poder"
        )

        label_legenda <- case_when(
          tipo == "estimar" ~ translation_pss("Amplitude do intervalo", linguagem()),
          TRUE ~ translation_pss("Poder (%)", linguagem())
        )


        g1 <- tab_TH_cenarios() %>%
          mutate(
            across(all_of(metrica), factor)
          ) %>%
          ggplot(
            aes(x = correlacao,
                y = n,
                color = .data[[metrica]]
            )
          ) +
          geom_line() +
          geom_point() +
          xlab(translation_pss("Correlação esperada (em módulo)", linguagem())) +
          ylab(translation_pss("Tamanho da amostra*", linguagem())) +
          theme_bw() +
          theme(axis.text = element_text(colour = "black")) +
          scale_color_brewer(name = label_legenda, palette = "Set1")


        plotly::ggplotly(g1,
                         tooltip = c("x", "colour", "y")) %>%
          plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                            showarrow = F, xref='paper', yref='paper',
                                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                            font=list(size=10)))
      })








      return_table_tabela_cenarios <- reactive({

        if (tipo == "estimar") {
          df_ <- tab_TH_cenarios() %>%
            dplyr::select(
              amplitude,
              correlacao,
              confianca,
              coeficiente,
              n
            )

          colnames(df_) <- c(
            translation_pss("Amplitude do intervalo", linguagem()),
            translation_pss("Correlação esperada (em módulo)", linguagem()),
            translation_pss("Nível de confiança (%)", linguagem()),
            translation_pss("Coeficiente de correlação", linguagem()),
            translation_pss("Tamanho amostral", linguagem())
          )

        } else {

          df_ <- tab_TH_cenarios() %>%
            dplyr::select(
              poder,
              correlacao,
              alpha,
              correlacao_h0,
              p_parcial,
              n
            )

          colnames(df_) <- c(
            translation_pss("Poder (%)", linguagem()),
            translation_pss("Correlação esperada (em módulo)", linguagem()),
            translation_pss("Nível de significância (%)", linguagem()),
            translation_pss("Valor de referência sob a hipótese nula", linguagem()),
            translation_pss("Número de variáveis para correlação parcial", linguagem()),
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
        filename = function() { "Cenarios_tamanho_amostra_correlacao.xlsx"},
        content = function(file) {writexl::write_xlsx(return_table_tabela_cenarios(),
                                                      path = file)}
      )





    } # Nao mexer!!!
  )

}
