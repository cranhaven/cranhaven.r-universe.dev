
mod_mc_nemar_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba_mc_nemar")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_mc_nemar_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, txt_balanceamento_f,
                                translation_pss, linguagem, .rodape, try_n, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas,
                                lista_de_funcoes_server){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns


      mecnemar <- function(n = NULL, paid = NULL, psi = NULL, sig.level = 0.05,
                           power = NULL, alternative = c("two.sided", "one.sided"),
                           method = c("normal", "exact", "cond.exact")) {
        if (sum(sapply(list(n, paid, psi, power, sig.level), is.null)) !=
            1) {
          stop("exactly one of 'n', 'paid', 'psi', 'power', and 'sig.level' must be NULL")
        }
        if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 >
                                                                 sig.level | sig.level > 1))
          stop("'sig.level' must be numeric in [0, 1]")
        if (!is.null(paid)) {
          if (any(paid <= 0) || any(paid >= 0.5)) {
            stop("paid is the smallest discordant probability and must be 0<paid<0.5")
          }
        }
        if (!is.null(psi)) {
          if (any(psi <= 1)) {
            stop("psi must be 1 or greater since it is the ratio of the larger discordant probability to the smaller discordant probability")
          }
          if (any((psi + 1) * paid > 1)) {
            stop("psi cannot be so big that the sum of the discordant probabilities exceed 1: ie., (1+paid)*psi>1")
          }
        }
        alternative <- match.arg(alternative)
        method <- match.arg(method)
        tside <- switch(alternative, one.sided = 1, two.sided = 2)
        f <- function(n, paid, psi, sig.level, power) {
          bc <- ceiling(paid * n * (1 + psi))
          pbinom(qbinom(sig.level/tside, size = bc, prob = 0.5) -
                   1, size = bc, prob = 1/(1 + psi)) + 1 - pbinom(qbinom(1 -
                                                                           sig.level/tside, size = bc, prob = 0.5), size = bc,
                                                                  prob = 1/(1 + psi))
        }
        fexact <- function(n, paid, psi, sig.level, power, alt = alternative) {
          sum(dbinom(seq(0, n), size = n, prob = paid * (1 + psi)) *
                power_binom_test(seq(0, n), p0 = 0.5, pa = 1/(1 +
                                                                psi), power = NULL, sig.level = sig.level, alternative = ifelse(alt ==
                                                                                                                                  "two.sided", "two.sided", "less"))$power)
        }
        if (method == "normal") {
          p.body <- quote(pnorm((sqrt(n * paid) * (psi - 1) -
                                   qnorm(sig.level/tside, lower.tail = FALSE) * sqrt(psi +
                                                                                       1))/sqrt((psi + 1) - paid * (psi - 1)^2)))
        }
        else if (method == "exact") {
          p.body <- quote(fexact(n, paid, psi, sig.level, power))
        }
        else {
          p.body <- quote(f(n, paid, psi, sig.level, power))
        }
        if (is.null(power)) {
          power <- eval(p.body)
        }
        else if (is.null(n)) {
          n <- uniroot(function(n) eval(p.body) - power, c(ceiling(log(sig.level)/log(0.5)),
                                                           1e+07))$root
        }
        else if (is.null(paid))
          paid <- uniroot(function(paid) eval(p.body) - power,
                          c(1e-10, 1/(1 + psi) - 1e-10))$root
        else if (is.null(psi))
          psi <- uniroot(function(psi) eval(p.body) - power, c(1 +
                                                                 1e-10, 1/paid - 1 - 1e-10))$root
        else if (is.null(sig.level))
          sig.level <- uniroot(function(sig.level) eval(p.body) -
                                 power, c(1e-10, 1 - 1e-10))$root
        else stop("internal error", domain = NA)
        NOTE <- "n is number of pairs"
        METHOD <- paste("McNemar paired comparison of proportions",
                        ifelse(method == "normal", "approximate", ifelse(method ==
                                                                           "exact", "exact unconditional", "exact conditional")),
                        "power calculation")
        structure(list(n = n, paid = paid, psi = psi, sig.level = sig.level,
                       power = power, alternative = alternative, note = NOTE,
                       method = METHOD), class = "power.htest")
      }


      # Comeco ----

      eval(parse(text = warning_prop("discordante1")))
      eval(parse(text = warning_prop("discordante2")))

      eval(parse(text = warning_prop("poder")))
      eval(parse(text = warning_prop("alpha")))
      eval(parse(text = warning_perdas("perc_perdas")))

      eval(parse(text = warning_inteiro("n")))


      pares <- reactive({
        c(input$discordante1, input$discordante2)
      })

      observeEvent(pares(), {
        req(!is.na(input$discordante1))
        req(!is.na(input$discordante2))

        if (input$discordante1 + input$discordante2 > 100) {
          shinyFeedback::showFeedbackWarning(
            inputId = ("discordante1"),
            text = ifelse(linguagem() == "pt", "A soma dos % deve ser menor do que 100%.", "The sum of the % must be less than 100%."),
            color = "red"
          )

          shinyFeedback::showFeedbackWarning(
            inputId = ("discordante2"),
            text = ifelse(linguagem() == "pt", "A soma dos % deve ser menor do que 100%.", "The sum of the % must be less than 100%."),
            color = "red"
          )
        }  else {
          shinyFeedback::hideFeedback(ns("discordante1"))
          shinyFeedback::hideFeedback(ns("discordante2"))
        }
      })




      metodo <- reactive({

        testes <- c("normal", "cond.exact")

        if (linguagem() == "pt"){
          names(testes) <- c("Assintótico", "Exato")
        } else {
          names(testes) <- c("Asymptotic", "Exact")
        }

        testes
      })




      output$aba_mc_nemar <- renderUI({
        sidebarLayout(
          sidebarPanel(


            wellPanel(HTML(
              '<b><a href="https://youtu.be/c5Rvl_LPnOE" target="_blank">',
              translation_pss("Vídeo: PSS Health para comparação de duas proporções dependentes", linguagem()),
              '</a></b><br>'
            )),

            wellPanel(
              HTML(
                paste0(
                  "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                )
              ),
              withMathJax(
                paste0(
                  "$$H_0: \\text{% ", nome_grupo_controle(),"} =  \\text{% ", nome_grupo_tratamento(),"}$$"
                )
              ),
              withMathJax(
                paste0(
                  "$$H_1: \\text{% ", nome_grupo_controle(),"} \\neq \\text{% ", nome_grupo_tratamento(),"}$$"
                )
              )
            ),

            actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),
            br(), br(),


            # HTML(paste0("<b><font size = '2.99'>% de discordância no grupo</font></b><br>")),
            # div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                numericInput( ns("discordante1"),
                              paste0("% ", nome_grupo_controle(), " (pb)"),
                              value = 50,
                              min = 0,
                              max = 100,
                              step = 5
                ),
            # ),
            # div(style="display: inline-block;vertical-align:top; width: 49%;",
                numericInput( ns("discordante2"),
                              paste0("% ", nome_grupo_tratamento(), " (pc)"),
                              value = 30,
                              min = 0,
                              max = 100,
                              step = 5
                ) %>% .help_buttom(linguagem = linguagem(), body = paste0(
                  "Percentual do total de pares discordantes (aqueles que obtiveram respostas diferentes) em cada categoria - ver explicação no cabeçalho da aba.",
                  txt_ajuda()$txt_definido_pesquisador_OU_literatura
                )
                ),
            # ),


            if (tipo %in% c("tamanho_amostral")) {
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

            selectInput(ns("metodo_estimacao"),
                        translation_pss("Método utilizado para calcular o teste", linguagem()),
                        choices = metodo(),
                        selected = "normal"
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_per_method_MESS)
              ,

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



      # Mudar nomes -----

      observeEvent(input$mudar_nomes, {
        showModal(
          modalDialog(
            title = translation_pss("Ajustes", linguagem()),
            fluidPage(

              HTML(translation_pss("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>.", linguagem())),
              br(), br(),
              textInput(inputId = ns("nome_grupo_controle"),
                        label   = HTML("Descreva a célula p_b"),
                        value   = ifelse(input$mudar_nomes == 0,
                                         translation_pss("Sim para Não", linguagem()),
                                         nome_grupo_controle())),

              br(),

              textInput(inputId = ns("nome_grupo_tratamento"),
                        label   = HTML("Descreva a célula p_c"),
                        value   = ifelse(input$mudar_nomes == 0,
                                         translation_pss("Não para Sim", linguagem()),
                                         nome_grupo_tratamento()))

            ),
            easyClose = TRUE,
            footer    = NULL
          )
        )
      })


      nome_grupo_controle <- reactive({
        ifelse(is.null(input$nome_grupo_controle), translation_pss("Sim para Não", linguagem()), input$nome_grupo_controle)
      })

      nome_grupo_tratamento <- reactive({
        ifelse(is.null(input$nome_grupo_tratamento), translation_pss("Não para Sim", linguagem()), input$nome_grupo_tratamento)
      })











      # Texto  -----

      output$texto_principal <- renderText({

        numerador   <- max(input$discordante1, input$discordante2)
        denominador <- min(input$discordante1, input$discordante2)

        if (tipo == "tamanho_amostral") {

          code <- paste0(
            "MESS::power_mcnemar_test(n = NULL, ",
            "psi = ", numerador, "/", denominador, ", ",
            "paid = ", denominador, "/100, ",
            "sig.level = ", input$alpha, "/100, ",
            "power = ", input$poder, "/100, ",
            'alternative = "two.sided", ',
            'method = "', input$metodo_estimacao, '")'
          )

          # print_r_code(code)
          codeRun <- gsub("MESS::power_mcnemar_test", "mecnemar", code)

          # n <- try_n(codeRun)
          n <- tryCatch({
            eval(parse(text = codeRun))
          }, warning = function(warning_condition) {
            NA
          }, error = function(error_condition) {
            NA
          }
          )

          eval(parse(text = validate_n("n")))
          n <- ceiling(n$n)
          eval(parse(text = validate_n_inf("n")))



          paste0(
            "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
            "</font></b></br></br>",


            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>",
              translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

              "Foi calculado um tamanho de amostra de <b>", n, "</b> pares para testar se as proporções de <i>",
              nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(), "</i> são diferentes ",
              "(com o acréscimo de ", input$perc_perdas, "% para possíveis perdas e recusas este número deve ser ",
              n_perdas(n, input$perc_perdas), "). ",
              "O cálculo, baseado no teste ",
              tolower(names(metodo()[metodo() == input$metodo_estimacao])),
              ", considerou um poder de <b>",
              input$poder, "%</b>, nível de significância de <b>", input$alpha, "</b>%, percentuais de <b>",
              input$discordante1, "%</b> e <b>", input$discordante2, "%</b> de <i>",
              nome_grupo_controle(), "</i> e <i> ", nome_grupo_tratamento(), "</i>, respectivamente (dados de Fulano (1900)). ",
              .txt_citacao_pss
            ),
            .txt_referencia_tap, print_r_code(code)
          )




          # Poder
        } else {

          code <- paste0(
            "MESS::power_mcnemar_test(",
            "n = ", input$n, ", ",
            "psi = ", numerador, "/", denominador, ", ",
            "paid = ", denominador, "/100, ",
            "sig.level = ", input$alpha, "/100, ",
            "power = NULL,",
            'alternative = "two.sided", ',
            'method = "', input$metodo_estimacao, '")'
          )

          # print_r_code(code)
          codeRun <- gsub("MESS::power_mcnemar_test", "mecnemar", code)

          # poder <- try_n(codeRun)
          poder <- tryCatch({
            eval(parse(text = codeRun))
          }, warning = function(warning_condition) {
            NA
          }, error = function(error_condition) {
            NA
          }
          )

          eval(parse(text = validate_n("poder")))


          paste0("<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", round(poder$power*100, digits = 1),
                  "%</font></b></br></br>",

                 lista_de_funcoes_server()$sugestao_texto_portugues(
                   # "<i>",
                   "O poder para testar se as proporções de <i>",
                   nome_grupo_controle(), "</i> e <i>", nome_grupo_tratamento(), "</i> são diferentes é <b>",
                   round(poder$power*100, digits = 1), "</b>. ",
                   " Este valor foi obtido considerando nível de significância de <b>", input$alpha, "</b>%, ",
                   "tamanho amostral de <b>", input$n, "</b> pares, ",
                   "percentuais de <b>",
                   input$discordante1, "%</b> e <b>", input$discordante2, "%</b> de <i>",
                   nome_grupo_controle(), "</i> e <i> ", nome_grupo_tratamento(), "</i>, respectivamente (dados de Fulano (1900)). ",
                   .txt_citacao_pss
                 ),
                 .txt_referencia_tap, print_r_code(code)
          )

        }

      })


      # Cenarios ----

      output$cenarios <- renderUI({

        req(tipo == "tamanho_amostral")

        req(!is.null(input$discordante1))
        req(!is.null(input$discordante2))

        req(!is.na(input$discordante1))
        req(!is.na(input$discordante2))


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


          HTML(paste0(
            "<b>", translation_pss("Defina a sequência de valores", linguagem()),"(%) <i>", nome_grupo_controle(), "</i></b>")),
          br(),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput(ns("from"), translation_pss("Mínimo", linguagem()), value = input$discordante1, step = 1)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("to"), translation_pss("Máximo", linguagem()), value = input$discordante1 + 10, step = 1)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("by"), translation_pss("Intervalo", linguagem()), value = 2, min = 0, step = 1) %>%
                .help_buttom(linguagem = linguagem(), body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()))
          ),


          fluidRow(
            column(6,
                   textInput(inputId = ns("seq_prop2"),
                             label   = paste0(
                               translation_pss("Digite valores de", linguagem()),
                               " (%) ", nome_grupo_tratamento()
                             ),
                             value   = paste0(c(input$discordante2, input$discordante2 + 1, input$discordante2 + 3), collapse = ", "),
                             width   = "400px") %>%
                     .help_buttom(linguagem = linguagem(), body = ajuda_cenarios_multiplos_valores())
            )
          ),



          br(),

          plotly::plotlyOutput(ns("plot"), width = "80%"),
          br(), br(),
          downloadButton(ns("down"),translation_pss("Download tabela", linguagem())),
          DT::dataTableOutput(ns("tab"), width = "100%")


        ))
      })


      eval(parse(text = check_text_input_to_vector("seq_prop2")))



      cenarios <- reactive({

        prop2 <- text_input_to_vector(input$seq_prop2)
        req(length(prop2) > 0)

        combinacoes <- expand.grid(
          perc1 = seq(from = input$from, to = input$to, by = input$by),
          perc2 = prop2,
          poder = input$poder,
          alpha =  input$alpha,
          metodo = input$metodo_estimacao,
          stringsAsFactors = FALSE
        )  %>%
          mutate(
            numerador = pmax(perc1, perc2),
            denominador = pmin(perc1, perc2)
          )

        combinacoes$`Tamanho da amostra` <- NA
        for(i in 1:nrow(combinacoes)){
          # print(i)
          combinacoes$`Tamanho da amostra`[i] <- tryCatch({
            ceiling(
              mecnemar(n = NULL,
                       psi = combinacoes$numerador[i]/combinacoes$denominador[i],
                       paid = combinacoes$denominador[i]/100,
                       sig.level = combinacoes$alpha[i]/100,
                       power = combinacoes$poder[i]/100,
                       alternative = "two.sided",
                       method = combinacoes$metodo[i])$n)

          },
          warning = function(warning_condition) { NA },
          error   = function(error_condition) { NA })
        }

        combinacoes

      })





      output$plot <- plotly::renderPlotly({


        g1 <- cenarios() %>%
          mutate(`Disc. 2` = factor(perc2)) %>%
          ggplot(aes(x = perc1, #`% de pares discordantes no grupo 1`,
                     y = `Tamanho da amostra`,
                     color = `Disc. 2`))+
          geom_point() +
          geom_line() +
          xlab(paste0("% ", nome_grupo_controle())) +
          ylab(translation_pss("Tamanho total da amostra*", linguagem())) +
          scale_x_continuous(breaks = seq(from = input$from, to = input$to, by = input$by)) +
          theme_bw() +
          theme(axis.text = element_text(colour = "black")) +
          scale_color_brewer(
            palette = "Set1",
            name = paste0("% ", nome_grupo_tratamento())
          )


        plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
          plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                            showarrow = F, xref='paper', yref='paper',
                                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                            font=list(size=10)))
      })


      cenarios_print <- reactive({
        dataset <- cenarios() %>%
          select(
            c(
              perc1, perc2, poder, alpha, metodo, `Tamanho da amostra`
            )
          )

        colnames(dataset) <- c(paste0("% ", nome_grupo_controle()),
                               paste0("% ", nome_grupo_tratamento()),
                               translation_pss("Poder (%)", linguagem()),
                               translation_pss("Nível de significância (%)", linguagem()),
                               translation_pss("Método utilizado para calcular o teste", linguagem()),
                               translation_pss("Tamanho da amostra", linguagem())
        )

        dataset
      })


      output$tab <- DT::renderDataTable({

        cenarios_print() %>%
        # cenarios() %>%
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


      output$down <- downloadHandler(
        filename = function() { "Cenarios_tamanho_amostra_duas_prop_dep.xlsx"},
        content = function(file) {writexl::write_xlsx(cenarios_print(), path = file)}
      )













    } # Nao mexer!!!
  )
}




