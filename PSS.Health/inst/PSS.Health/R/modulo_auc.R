
mod_auc_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_auc_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, txt_balanceamento_f,
                           translation_pss, linguagem, .rodape, validate_n, try_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                           warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas,
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
                  HTML("<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"),
                  withMathJax("$$H_0: AUC =0,5 \\text{  vs  } H_1: AUC \\neq 0,5$$"),
                )
              },

              actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),
              br(), br(),

              numericInput( ns("auc"),
                            translation_pss("Área sob a curva ROC", linguagem()),
                            value = 0.7,
                            min = 0.5,
                            max = 1,
                            step = .1
              ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Área sob a curva que se espera encontrar.", txt_ajuda()$txt_definido_pesquisador_OU_literatura)),


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
              } else {
                tagList(
                  # numericInput( ns("balanceamento"),
                  #               paste0(
                  #                 translation_pss("Balanceamento", linguagem()),
                  #                 " (", nome_grupo_controle(), ":", nome_grupo_tratamento(), ")"
                  #               ),
                  #               value = 1,
                  #               min   = 0,
                  #               max   = Inf,
                  #               step  = .5
                  # ) %>% .help_buttom(linguagem = linguagem(), body = txt_balanceamento_f(nome_grupo_controle(), nome_grupo_tratamento()),
                  #                    title = translation_pss("Balanceamento", linguagem())),

                  numericInput( ns("percentual_desfecho"),
                                paste0(
                                  translation_pss("Percentual esperado (%)", linguagem()),
                                  " ",
                                  translation_pss("de", linguagem()),
                                  " ",
                                  nome_desfecho()
                                ),
                                value = 50,
                                min   = 0,
                                max   = 100,
                                step  = 5
                  ) %>%  .help_buttom(linguagem = linguagem(), 
                    body = paste0(txt_ajuda()$txt_perc_esperado, txt_ajuda()$txt_definido_literatura),
                    title = translation_pss("Percentual esperado (%)", linguagem())
                  ),


                  if (tipo == "tamanho_amostral") {
                    numericInput( ns("poder"),
                                  translation_pss("Poder (%)", linguagem()),
                                  value = 80,
                                  min = 0,
                                  max = 100,
                                  step = 1
                    ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
                  } else {

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
                  }
                )
              },

              if (tipo != "estimar") {
                numericInput( ns("alpha"),
                              translation_pss("Nível de significância (%)", linguagem()),
                              value = 5,
                              min = 0,
                              max = 100,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem()))
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
              # textInput(inputId = ns("nome_grupo_tratamento"),
              #           label   = translation_pss("Descreva um nome para o grupo Tratamento", linguagem()),
              #           value   = ifelse(input$mudar_nomes == 0, translation_pss("Tratamento", linguagem()), nome_grupo_tratamento())),
              #
              # HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamado de grupo Intervenção ou grupo Exposto.</i><br><br>"),
              #
              # textInput(inputId = ns("nome_grupo_controle"),
              #           label   = translation_pss("Descreva um nome para o grupo Controle", linguagem()),
              #           value   = ifelse(input$mudar_nomes == 0, translation_pss("Controle", linguagem()), nome_grupo_controle())),
              #
              # HTML("<i>Em alguns estudos o grupo Controle também pode ser chamado de grupo Placebo/ Sham ou grupo Não exposto.</i>"),
              # br(), br(),


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

      nome_grupo_controle <- reactive({
        ifelse(is.null(input$nome_grupo_controle), translation_pss("Controle", linguagem()), input$nome_grupo_controle)
      })

      nome_grupo_tratamento <- reactive({
        ifelse(is.null(input$nome_grupo_tratamento), translation_pss("Tratamento", linguagem()), input$nome_grupo_tratamento)
      })

      nome_preditora <- reactive({
        ifelse(is.null(input$nome_preditora), "X", input$nome_preditora)
      })






      ## Texto ----


      output$texto_principal <- renderText({


        if (tipo %in% c("tamanho_amostral", "poder")) {

          code <- paste0(
            "pROC::power.roc.test(",
            "auc = ", input$auc, ", ",
            "sig.level = ", input$alpha,  "/100, ",
            if (tipo == "tamanho_amostral") {
              paste0(
                "power = ", input$poder, "/100, ",
                # "kappa = ", input$balanceamento
                "kappa = ", input$percentual_desfecho, " / (100 - ", input$percentual_desfecho, ")"
              )
            } else {
              paste0(
                "ncases = ", input$n_tratamento, ", ",
                "ncontrols = ", input$n_controle
              )
            },
            ")"
          )

          print_r_code(code)

          if (tipo %in% c("tamanho_amostral")) {

            n <- try_n(code)
            eval(parse(text = validate_n("n")))

            n_casos <- ceiling(n$ncases)
            n_control <- ceiling(n$ncontrols)
            n <- n_casos + n_control
            eval(parse(text = validate_n_inf("n")))


            nperdas_casos <- n_perdas(n_casos, input$perc_perdas)
            nperdas_controle <- n_perdas(n_control, input$perc_perdas)
            nperdas <- nperdas_casos + nperdas_controle

            paste0(
              "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
              "</font></b></br></br>",


              lista_de_funcoes_server()$sugestao_texto_portugues(
                "<i>",
                translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
                "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",
                "para testar se, utilizar <i>", nome_preditora(), "</i> como variável preditora de <i>", nome_desfecho(), "</i>, fornece uma área sob a curva diferente de 0.5 ",

                paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas, "</b>). "),
                "O cálculo considerou poder de <b>", input$poder, "%</b>, nível de significância de <b>", input$alpha, "%</b> ",
                "e uma área sob a curva esperada de <b>", input$auc, "</b> (referido por Fulano (1900) OU escolha do pesquisador). ",
                .txt_citacao_pss
              ),
              .txt_referencia_tap,
              print_r_code(code)
            )

            ## Poder ----
          } else {

            poder <- try_n(code)
            eval(parse(text = validate_n("poder")))

            poder <- round(poder$power*100, 1)
            eval(parse(text = validate_n_inf("poder")))

            paste0(
              "<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", poder, "%",
              "</font></b></br></br>",

              lista_de_funcoes_server()$sugestao_texto_portugues(
                "<i>",
                translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
                "O poder para testar se, utilizar <i>", nome_preditora(), "</i> como variável preditora de <i>", nome_desfecho(), "</i>, fornece uma área sob a curva diferente de 0.5 é <b>",
                poder, "%</b>. ",

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
                "e uma área sob a curva esperada de <b>", input$auc, "</b> (referido por Fulano (1900) OU escolha do pesquisador). ",
                .txt_citacao_pss
              ),
              .txt_referencia_tap,
              print_r_code(code)
            )

          }

          # Estimar ----
        } else {


          # balanceamento <- paste0(
          #   input$balanceamento, "/(1 + ", input$balanceamento, ")"
          # )
          code <- paste0(
            "presize::prec_auc(",
            "auc = ", input$auc, ", ",
            "prev = ", input$percentual_desfecho, "/100, ",
            "conf.level = ", input$confianca,  "/100, ",
            "conf.width = ", input$amplitude, ")"
          )

          n <- try_n(code)
          eval(parse(text = validate_n("n")))

          n_casos <- ceiling(n$n1)
          n_control <- ceiling(n$n2)
          n <- n_casos + n_control
          eval(parse(text = validate_n_inf("n")))

          nperdas_casos <- n_perdas(n_casos, input$perc_perdas)
          nperdas_controle <- n_perdas(n_control, input$perc_perdas)
          nperdas <- nperdas_casos + nperdas_controle

          # print_r_code(code)

          paste0(
            "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
            "</font></b></br></br>",

            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>",
              translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
              "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",

              " para estimar a área sob a curva ao utilizar <i>", nome_preditora(), "</i> como variável preditora de <i>", nome_desfecho(), "</i> ",

              paste0("(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas, "</b>). "),

              "O cálculo considerou nível de confiança de <b>", input$confianca, "%</b>, ",
              "amplitude desejada para o intervalo de confiança de <b>", input$amplitude, "</b> ",
              "e uma área sob a curva esperada de <b>", input$auc, "</b> (referido por Fulano (1900) OU escolha do pesquisador). ",
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
        req(!is.null(input$auc))

        if (input$auc < 0.85) {
          val_min <- input$auc
          val_max <- input$auc + 0.1

        } else {
          val_min <- input$auc - 0.1
          val_max <- input$auc
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
            translation_pss("Defina a sequência para a área sob a curva", linguagem()),
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
              numericInput(ns("by"), translation_pss("Intervalo", linguagem()), value = 0.02, min = 0, step = .1) %>%
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


        df_grid <- expand.grid(poder = poder,
                               auc = seq(input$from, input$to, input$by),
                               alpha = input$alpha,
                               prevalencia = input$percentual_desfecho,
                               balanceamento = input$percentual_desfecho/(100-input$percentual_desfecho),
                               stringsAsFactors = FALSE) %>%
          mutate(
            n_casos = mapply(
              function(auc, poder, alpha, balanceamento){
                tryCatch({
                  n_p <- pROC::power.roc.test(
                    auc = auc, sig.level = alpha/100, power = poder/100, kappa = balanceamento
                  )
                  n_p$ncases
                },
                warning = function(warning_condition) { NA },
                error   = function(error_condition) { NA })
              }, auc, poder, alpha, balanceamento),

            n_control = n_casos*balanceamento,

            across(
              c(n_control, n_casos), ceiling
            ),
            n = n_casos + n_control
          )

      })


      output$grafico_cenarios <- plotly::renderPlotly({


        g1 <- tab_TH_cenarios() %>%
          mutate(
            `Poder (%)` = factor(poder)
          ) %>%
          ggplot(
            aes(x = auc,
                y = n,
                color = `Poder (%)`
            )
          ) +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = seq(from = input$from, to = input$to, by = input$by)) +
          xlab("AUC") +
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

        df_ <- tab_TH_cenarios() %>%
          dplyr::select(
            poder,
            auc,
            alpha,
            prevalencia,
            n

          )

        colnames(df_) <- c(
          translation_pss("Poder (%)", linguagem()),
          "AUC",
          translation_pss("Nível de significância (%)", linguagem()),
          # translation_pss("Balanceamento", linguagem()),
          translation_pss("Percentual (%)", linguagem()),
          # paste0("n ", nome_grupo_tratamento()),
          # paste0("n ", nome_grupo_controle()),
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
        filename = function() { "Cenarios_tamanho_amostra_AUC.xlsx"},
        content = function(file) {writexl::write_xlsx(return_table_tabela_cenarios(),
                                                      path = file)}
      )





    } # Nao mexer!!!
  )

}
