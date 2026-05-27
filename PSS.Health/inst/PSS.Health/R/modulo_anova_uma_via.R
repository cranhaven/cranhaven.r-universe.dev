
mod_anova1_Ui <- function(id) {

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_anova1_server <- function(id, tipo = "tamanho_amostral", txt_ajuda,
                              ajuda_cenarios_multiplos_valores2,
                              translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                              warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas, warning_numero,
                              lista_de_funcoes_server) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

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

            wellPanel(
              HTML(
                paste0(
                  "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                )
              ),
              uiOutput(ns("th_h"))
            ),

            actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),

            checkboxInput(
              ns("calcular_utilizando_f"),
              translation_pss("Usar magnitude de efeito f", linguagem()),
              value = FALSE
            ),

            uiOutput(ns("render_input_f_medias")),



            if (tipo == "tamanho_amostral") {
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





      # Hipoteses a serem testadas ----



      output$th_h <- renderUI({

        if (input$calcular_utilizando_f) {
          req(!is.null(input$k))
          k <- input$k
        } else{
          req(!is.null(input$medias))
          k <- text_input_to_vector(input$medias) %>%
            length()
        }

        withMathJax(
          paste0("$$H_0: ", paste0("\\mu_", LETTERS[1:k], collapse = " = "), "$$")
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
              br(), br()
            ),
            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      nome_desfecho <- reactive({
        ifelse(is.null(input$nome_desfecho), "Y", input$nome_desfecho)
      })

      unidade_medida <- reactive({
        ifelse(is.null(input$unidade_medida),  translation_pss("u.m.", linguagem()), input$unidade_medida)
      })







      # f ou medias? -----


      output$render_input_f_medias <- renderUI({

        if (input$calcular_utilizando_f) {
          tagList(
            numericInput( ns("f"),
                          translation_pss("Magnitude do efeito (f)", linguagem()),
                          value = 0.3,
                          min = 0,
                          max = 1,
                          step = 0.1) %>%
              shinyhelper::helper(type = "markdown",
                                  title = translation_pss("Magnitude do efeito (f)", linguagem()),
                                  content = "Effect_size_f",
                                  buttonLabel = "Fechar",
                                  fade = TRUE,
                                  colour = "#006338",
                                  size = "l"),

            numericInput( ns("k"),
                          translation_pss("Número de grupos", linguagem()),
                          value = 3,
                          min = 2,
                          max = Inf,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = paste0(translation_pss("Número de grupos", linguagem()), txt_ajuda()$txt_definido_pesquisador)),

            if (tipo == "poder") {
              numericInput( ns("n_por_k"),
                            translation_pss("Número de observações (por grupo)", linguagem()),
                            value = 15,
                            min = 1,
                            max = Inf,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = paste0(translation_pss("Número de observações (por grupo)", linguagem()), txt_ajuda()$txt_definido_pesquisador))
            }
          )

        } else {

          tagList(

            textInput( ns("medias"),
                       translation_pss("Médias dos grupos", linguagem()),
                       value = "12.6, 14.9, 16"
            ) %>%
              .help_buttom(linguagem = linguagem(), title = translation_pss("Médias dos grupos", linguagem()),
                           body = paste0(
                             ajuda_cenarios_multiplos_valores2(),
                             txt_ajuda()$txt_definido_pesquisador_OU_literatura
                           )
            ),

            numericInput( ns("sigma"),
                          translation_pss("Desvio padrão", linguagem()),
                          value = 4,
                          min = 0,
                          max = Inf,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão", linguagem())),

            if (tipo == "poder") {
              textInput( ns("n_por_k_texto"),
                         translation_pss("Número de observações (por grupo)", linguagem()),
                         value = "15, 16, 12"
              ) %>% .help_buttom(linguagem = linguagem(), title = translation_pss("Número de observações (por grupo)", linguagem()),
                                 body = ajuda_cenarios_multiplos_valores2()
              )
            }

          ) # Fecha tagList

        }
      })





      # Texto -----


      output$texto_principal <- renderText({

        if (tipo == "tamanho_amostral") {

          if (input$calcular_utilizando_f) {
            req(!is.null(input$k))
            k <- input$k
          } else {
            req(!is.null(input$medias))
            k <- length(text_input_to_vector(input$medias))
          }

          code <- ifelse(input$calcular_utilizando_f,
                         paste0(
                           "pwr::pwr.anova.test(n = NULL, ",
                           "k = ", k, ", ",
                           "f = ", input$f, ", ",
                           "sig.level = ", input$alpha, "/100, ",
                           "power = ", input$poder, "/100)"
                         ),
                         paste0(
                           "EnvStats::aovN(mu.vec = c(",
                           paste(text_input_to_vector(input$medias), collapse = ", "),
                           "), ",
                           "sigma  = ", input$sigma, ", ",
                           "alpha  = ", input$alpha, "/100, ",
                           "power  = ", input$poder, "/100, ",
                           "n.max  = 1E5)"
                         )
          )

          # print_r_code(code)

          n <- try_n(code)
          eval(parse(text = validate_n("n")))

          if (input$calcular_utilizando_f) {
            n <- ceiling(n$n)
          } else {
            n <- n
          }

          nperdas <- n_perdas(n, input$perc_perdas)
          eval(parse(text = validate_n_inf("n")))

          paste0(
            "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n*k,  " (<i>", n, " ", translation_pss("para cada grupo", linguagem()), "</i>)",
            "</font></b></br></br>",


            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


              "Foi calculado um tamanho de amostra de <b>", n*k, "</b> sujeitos (", n, " em cada grupo) para detectar ",
              if (input$calcular_utilizando_f) {
                paste0(
                  "o tamanho de efeito (f) <b>", input$f, "</b> referida em Fulano (1900) <b>OU</b> escolha do pesquisador para "
                )
              } else {
                "diferenças significativas entre as médias de "
              },

              "<i>", nome_desfecho(), "</i> entre os grupos ",
              sub(",([^,]*)$", " e\\1", paste(LETTERS[1:k], collapse = ", ")),

              if (!input$calcular_utilizando_f) {
                paste0(
                  " sendo <b>", sub(",([^,]*)$", " e\\1", input$medias), " ", unidade_medida(),
                  "</b> as médias consideradas para cada grupo, respectivamente"
                )
              },
              " (com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas*k, "</b>). ",
              "O cálculo considerou poder de <b>", input$poder, "%</b>",
              if (input$calcular_utilizando_f) {
                paste0(
                  " e nível de significância de <b>", input$alpha, "%</b>. "
                )
              } else {
                paste0(
                  ", nível de significância de <b>", input$alpha, "%</b>",
                  " e desvio padrão igual a <b>", input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)). "
                )
              },
              .txt_citacao_pss
            ),
            .txt_referencia_tap,
            print_r_code(code)
          )


         # Poder ----
        } else {

          if (input$calcular_utilizando_f) {
            req(!is.null(input$k))
            k <- input$k
          } else {
            req(!is.null(input$medias))
            k <- length(text_input_to_vector(input$medias))
          }

          code <- ifelse(input$calcular_utilizando_f,
                         paste0(
                           "pwr2::pwr.1way(",
                           "n = ", input$n_por_k, ", ",
                           "k = ", k, ", ",
                           "f = ", input$f, ", ",
                           "alpha = ", input$alpha, "/100)"
                         ),
                         paste0(
                           "EnvStats::aovPower(",
                           "n.vec = c(",
                           paste(text_input_to_vector(input$n_por_k_texto), collapse = ", "),
                           "), ",
                           "mu.vec = c(",
                           paste(text_input_to_vector(input$medias), collapse = ", "),
                           "), ",
                           "alpha = ", input$alpha, "/100, ",
                           "sigma = ", input$sigma, ")"
                         )
          )

          # print_r_code(code)

          poder <- try_n(code)
          eval(parse(text = validate_n("poder")))

          if (input$calcular_utilizando_f) {
            poder <- round(poder$power*100, 1)
          } else {
            poder <- round(poder*100, 1)
          }

          eval(parse(text = validate_n_inf("poder")))


          paste0("<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", poder,
                 "%</font></b></br></br>",

                 lista_de_funcoes_server()$sugestao_texto_portugues(

                   "O poder para detectar ",
                   if (input$calcular_utilizando_f) {
                     paste0(
                       "o tamanho de efeito (f) <b>", input$f, "</b> referida em Fulano (1900) <b>OU</b> escolha do pesquisador para "
                     )
                   } else {
                     "diferenças significativas entre as médias de "
                   },

                   "<i>", nome_desfecho(), "</i> entre os grupos ",
                   sub(",([^,]*)$", " e\\1", paste(LETTERS[1:k], collapse = ", ")),

                   if (!input$calcular_utilizando_f) {
                     paste0(
                       " sendo <b>", sub(",([^,]*)$", " e\\1", input$medias), " ", unidade_medida(),
                       "</b> as médias consideradas para cada grupo, respectivamente"
                     )
                   },
                   " é <b>", poder, "%</b>. ",

                   "Este valor foi obtido considerando nível de significância de <b>", input$alpha, "%</b>, ",

                   if (input$calcular_utilizando_f) {
                     paste0(
                       " e tamanho de amostra igua a <b>", input$n_por_k, "</b> sujeitos para cada grupo. "
                     )
                   } else {
                     paste0(
                       " tamanho de amostra igua a <b>",
                       sub(
                         ",([^,]*)$",
                         " e\\1",
                         paste(text_input_to_vector(input$n_por_k_texto), collapse = ", ")
                       ),
                       "</b> sujeitos para cada grupo, respectivamente, ",
                       " e desvio padrão igual a <b>", input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)). "
                     )
                   },
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

        if (input$calcular_utilizando_f) {
          req(!is.null(input$f))
          val_min <- input$f
          val_max <- input$f + 0.3
          val_by <- 0.05

        } else {
          req(!is.null(input$sigma))
          val_min <- input$sigma
          val_max <- input$sigma + 2
          val_by <- 0.5
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


          if (input$calcular_utilizando_f) {
            HTML("<b>",
                 translation_pss("Defina a sequência de valores para a magnitude do efeito", linguagem()),
                 "</b>"
            )
          } else {
            HTML("<b>", translation_pss("Defina a sequência de valores para o desvio padrão", linguagem()), "</b>")
          },


          br(),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput(ns("from"), translation_pss("Mínimo", linguagem()), value = val_min, step = .5)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("to"), translation_pss("Máximo", linguagem()), value = val_max, step = .5)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("by"), translation_pss("Intervalo", linguagem()), value = val_by, min = 0, step = .1) %>%
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


        if (input$calcular_utilizando_f) {

          expand.grid(f = seq(from = input$from, to = input$to, by = input$by),
                      poder = poder,
                      alpha =  input$alpha,
                      k = input$k) %>%
            mutate(n =
                     mapply(function(k, f, sig.level, power) {
                       tryCatch({
                         pwr::pwr.anova.test(n = NULL, k = k, f = f, sig.level = sig.level/100, power = power/100)$n %>% ceiling()},
                         warning = function(warning_condition) { NA },
                         error = function(error_condition) { NA })},
                       k, f, alpha, poder),
                   n = n*k
            )

        } else{

          req(!is.null(input$sigma))


          expand.grid(desvio = seq(from = input$from, to = input$to, by = input$by),
                      media = paste(text_input_to_vector(input$medias), collapse = ", "),
                      k = length(text_input_to_vector(input$medias)),
                      poder = poder,
                      alpha =  input$alpha) %>%

            mutate(n = mapply(function(medias, desvio, sig.level, power) {

              tryCatch({
                medias_anova <- medias #%>% strsplit(",") %>% unlist() %>% as.numeric() %>% na.omit()

                code <- paste0(
                  "EnvStats::aovN(mu.vec = c(",medias_anova, "), ",
                  "sigma  = ", desvio, ", ",
                  "alpha  = ", sig.level/100, ", ",
                  "power  = ", power/100, ", ",
                  "n.max  = 1E5)")

                n <- eval(parse(text = code))

                n
              }, warning = function(warning_condition) { NA },
              error = function(error_condition) { NA })},
              media, desvio, alpha, poder),

              n = n*k
            )
        }


      })


      output$grafico_cenarios <- plotly::renderPlotly({

        metrica <- case_when(
          input$calcular_utilizando_f ~ "f",
          TRUE ~ "desvio"
        )

        xlab <- case_when(
          input$calcular_utilizando_f ~ "f",
          TRUE ~ translation_pss("Desvio padrão", linguagem())
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

        if (input$calcular_utilizando_f) {

          colnames(df_) <- c(
            "f",
            translation_pss("Poder (%)", linguagem()),
            translation_pss("Nível de significância (%)", linguagem()),
            translation_pss("Número de grupos", linguagem()),
            translation_pss("Tamanho amostral", linguagem())
          )


        } else {

          df_ <- df_ %>%
            dplyr::select(-k)

          colnames(df_) <- c(
            translation_pss("Desvio padrão", linguagem()),
            translation_pss("Médias dos grupos", linguagem()),
            translation_pss("Poder (%)", linguagem()),
            translation_pss("Nível de significância (%)", linguagem()),
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
