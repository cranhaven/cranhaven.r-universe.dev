
mod_1_media_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_1_media_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, amostragem_aleatoria_simples = FALSE,
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
      eval(parse(text = warning_numero_positivo("precisao")))
      eval(parse(text = warning_perdas("perc_perdas")))
      eval(parse(text = warning_inteiro("n_controle")))
      eval(parse(text = warning_inteiro("n_tratamento")))



      # Aba ----

      amostragem_aleatoria_simples_selecionada <- reactive({

        if (tipo != "estimar") {
          TRUE
        } else if (amostragem_aleatoria_simples) {
          amostragem_aleatoria_simples
        } else {
          # input$delineamento == "aas"
          # REMOVI AS OUTRAS OPCOES DE PLANOS, POIS NAO ESTAVAM CLAROS OS METODOS
          #   REVER DEPOIS!!!
          TRUE
        }

      })




      output$aba <- renderUI({

        sidebarLayout(
          sidebarPanel(

            if (tipo == "estimar") {
              wellPanel(
                HTML(
                  '<b><a href="https://youtu.be/qqXF6-CAlok" target="_blank">',
                  translation_pss('Vídeo: PSS Health para estimar uma média', linguagem()),
                  '</a></b><br>'
                )
              )
            },


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

            if (tipo == "estimar") {
              # selectInput(inputId  = ns("delineamento"),
              #             label    = translation_pss("Processo de amostragem", linguagem()),
              #             choices  = opcoes_amostragem(),
              #             selected = "aas"
              # )
            },

            actionLink(ns("mudar_nomes"), translation_pss("Mudar nomes", linguagem())),
            br(), br(),


            uiOutput(ns("render_input_delineamentos")),


            if (tipo == "poder") {

              numericInput( ns("n"),
                            translation_pss("Tamanho amostral", linguagem()),
                            value = 285,
                            min = 2,
                            step = 1
              )
            } else if (tipo %in% c("estimar")) {
              numericInput( ns("precisao"),
                            translation_pss("Margem de erro/ semi-amplitude", linguagem()),
                            value = 3,
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


      # Opcoes de amostragem ----
      opcoes_amostragem <- reactive({

        # amostragem <- c("aas", "ac1", "aae")
        amostragem <- c("aas", "aae")

        names(amostragem) <- c(
          translation_pss("Aleatória simples", linguagem()),
          # translation_pss("Conglomerados em um único estágio", linguagem()),
          translation_pss("Estratificada proporcional ao tamanho", linguagem())
        )

        amostragem
      })




      # Hipoteses a serem testadas ----


      h1 <- reactive({
        teste <- c("bi", "sup", "inf")

        if (linguagem() == "pt") {
          names(teste) <- c("Bilateral", "Unilateral superior", "Unilateral inferior")
        } else {
          names(teste) <- c("Two-sided", "Lower-tailed", "Upper-tailed")
        }

        teste
      })


      output$th_h0 <- renderUI({

        sinal_h0 <- case_when(input$th_alternativa == 'bi'  ~ "=",
                              input$th_alternativa == 'sup' ~ "\\leq",
                              input$th_alternativa == 'inf' ~ "\\geq")

        withMathJax(
          paste0(
            "$$H_0: \\mu_\\text{", nome_desfecho(), "} ", sinal_h0, input$media_sob_h0, "$$"
          )
        )
      })

      output$th_h1 <- renderUI({

        sinal_h1 <- case_when(input$th_alternativa == 'bi'  ~ "\\neq",
                              input$th_alternativa == 'sup' ~ ">",
                              input$th_alternativa == 'inf' ~ "<")

        withMathJax(
          paste0(
            "$$H_1: \\mu_\\text{", nome_desfecho(), "}", sinal_h1, input$media_sob_h0, "$$"
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




      # Particularidades delineamentos -----

      delineamento_selecionado <- reactive({
        input$delineamento

      })

      output$render_input_delineamentos <- renderUI({

        if (amostragem_aleatoria_simples_selecionada()) {

          tagList(

            if (tipo != "estimar") {
              tagList(
                numericInput( ns("media_esperada"),
                              translation_pss("Média esperada", linguagem()),
                              value = 2,
                              min = -Inf,
                              max = Inf,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Valor da média que se espera observar na amostra.", txt_ajuda()$txt_definido_literatura)),
                numericInput( ns("media_sob_h0"),
                              translation_pss("Valor de referência sob a hipótese nula", linguagem()),
                              value = 0,
                              min = -Inf,
                              max = Inf,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), body = paste0(
                  "Valor da média sob a hipótese nula ($\\mu_0$) ",
                  "Maiores informações em ",
                  '<a href="https://seer.ufrgs.br/hcpa/article/view/93649/pdf" target="_blank">Hirakata et al. 2019</a>.',
                  txt_ajuda()$txt_definido_pesquisador
                ))
              )
            },

            numericInput( ns("sigma"),
                          translation_pss("Desvio padrão", linguagem()),
                          value = 12,
                          min = 0,
                          max = Inf,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão", linguagem()))


          )
        } else {


          req(!is.null(input$delineamento))

          if (input$delineamento == "aae") {
            tagList(
              numericInput(inputId = ns("numero_estratos"),
                           label   = translation_pss("Número de estratos", linguagem()),
                           value   = 3,
                           min     = 1,
                           max     = Inf,
                           step    = 1),

              textInput( ns("N_estratos"),
                         translation_pss("Tamanho populacional de cada estrato", linguagem()),
                         value = "6000, 5000, 4000"),

              textInput( ns("xbar_estratos"),
                         paste0(
                           translation_pss("Médias esperadas de ", linguagem()),
                           nome_desfecho(), " (",
                           translation_pss("em", linguagem()), " ",
                           unidade_medida(), ") ",
                           translation_pss("em cada estrato", linguagem())
                         ),
                         value = "0.5, 0.2, 0.5"),

              textInput( ns("sigma_estratos"),
                         paste0(
                           translation_pss("Desvio padrão esperado de", linguagem()), " ",
                           nome_desfecho(), " (",
                           translation_pss("em", linguagem()), " ",
                           unidade_medida(), ") ",
                           translation_pss("em cada estrato", linguagem())
                         ),
                         value = "40, 40, 60")
            )


          } else {
            tagList(
              numericInput( ns("n_conglomerados"),
                            "Número total de conglomerados",
                            value = 5,
                            min   = 1,
                            max   = Inf,
                            step  = 1) %>%
                .help_buttom(linguagem = linguagem(), body = paste0("Número total de conglomerados", txt_ajuda()$txt_definido_pesquisador),
                             title = "Número total de conglomerados"),

              numericInput( ns("xbar_conglomerados"),
                            paste0("Média esperada de ", nome_desfecho(), " (em ", unidade_medida(), ")"),
                            value = 34,
                            min   = -Inf,
                            max   = Inf,
                            step  = 1),

              numericInput( ns("desvio_conglomerados"),
                            paste0("Desvio padrão esperado de ", nome_desfecho(), " (em ", unidade_medida(), ")"),
                            value = 25,
                            min   = 0,
                            max   = Inf,
                            step  = 1) %>%
                .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão esperado", linguagem())),


              numericInput( ns("N_medio_conglomerados"),
                            "Média do número de indivíduos em cada conglomerado",
                            value = 25,
                            min   = 1,
                            max   = Inf,
                            step  = 1) %>%
                .help_buttom(linguagem = linguagem(), body = paste0("Média do número de indivíduos em cada conglomerado", txt_ajuda()$txt_definido_pesquisador_OU_literatura),
                             title = "Média do número de indivíduos em cada conglomerado"),

              numericInput( ns("desvio_medio_conglemerados"),
                            "Desvio padrão do número de indivíduos em cada conglomerado",
                            value = 0,
                            min   = 0,
                            max   = Inf,
                            step  = 1) %>%
                .help_buttom(linguagem = linguagem(), body = paste0("Desvio padrão do tamanho de cada conglomerado", txt_ajuda()$txt_definido_literatura),
                             title = "Desvio padrão do tamanho de cada conglomerado"),

              numericInput( ns("rho"),
                            "Coeficiente de correlação intra conglomerados",
                            value = 0.1,
                            min   = 0,
                            max   = 1,
                            step  = .1) %>%
                .help_buttom(linguagem = linguagem(), body = paste0("Coeficiente de correlação intra conglomerados", txt_ajuda()$txt_definido_literatura),
                             title = "Coeficiente de correlação intra conglomerados")
            )
          }
        }
      })


      # Avisos ----

      aae_inputs <- reactive({
        req(input$delineamento == "aae")

        req(!is.null(input$N_estratos))
        req(!is.null(input$xbar_estratos))
        req(!is.null(input$sigma_estratos))

        N <- text_input_to_vector(input$N_estratos)

        medias <- text_input_to_vector(input$xbar_estratos)

        desvio <- text_input_to_vector(input$sigma_estratos)

        list(N = N, medias = medias, desvio = desvio)
      })




      observeEvent(c(input$numero_estratos, input$N_estratos, input$xbar_estratos, input$sigma_estratos), {

        if(is.na(input$numero_estratos)){
          shinyFeedback::showFeedbackWarning(
            inputId = "numero_estratos",
            text = "Deve ser fornecido valores",
            color = "red")

        } else {
          shinyFeedback::hideFeedback("numero_estratos")

          if(all(is.na(aae_inputs()$N))){
            shinyFeedback::showFeedbackWarning(
              inputId = "N_estratos",
              text = "Deve ser fornecido valores",
              color = "red")

          } else if (length(aae_inputs()$N)  != input$numero_estratos) {
            shinyFeedback::showFeedbackWarning(
              inputId = "N_estratos",
              text = paste0("Deve ser fornecido ", input$numero_estratos, " valores válidos."),
              color = "red")

          } else if (any(aae_inputs()$N <= 0) | any(aae_inputs()$N%%1 != 0) | any(aae_inputs()$N < 2) ) {
            shinyFeedback::showFeedbackWarning(
              inputId = "N_estratos",
              text = "Deve ser fornecido apenas valores positivos inteiros maiores do que 1.",
              color = "red")

          } else {
            shinyFeedback::hideFeedback("N_estratos")
          }





          if (all(is.na(aae_inputs()$medias))) {
            shinyFeedback::showFeedbackWarning(
              inputId = "xbar_estratos",
              text = "Deve ser fornecido valores.",
              color = "red")

          } else if (length(aae_inputs()$medias) != input$numero_estratos) {
            shinyFeedback::showFeedbackWarning(
              inputId = "xbar_estratos",
              text = paste0("Deve ser fornecido ", input$numero_estratos, " valores válidos."),
              color = "red")

          } else {
            shinyFeedback::hideFeedback("xbar_estratos")
          }





          if (all(is.na(aae_inputs()$desvio))) {
            shinyFeedback::showFeedbackWarning(
              inputId = "sigma_estratos",
              text = "Deve ser fornecido valores.",
              color = "red")

          } else if (length(aae_inputs()$desvio) != input$numero_estratos) {
            shinyFeedback::showFeedbackWarning(
              inputId = "sigma_estratos",
              text = paste0("Deve ser menor fornecido ", input$numero_estratos, " valores válidos."),
              color = "red")

          } else if (any(aae_inputs()$desvio <= 0)) {
            shinyFeedback::showFeedbackWarning(
              inputId = "sigma_estratos",
              text = "Deve ser menor fornecido apenas valores positivos.",
              color = "red")

          } else {
            shinyFeedback::hideFeedback("sigma_estratos")
          }
        }

      })





      # Texto -----


      output$texto_principal <- renderText({

        if (tipo != "estimar") {

          code <- paste0("stats::power.t.test(",
                         if (tipo == "tamanho_amostral") {
                           paste0(
                             "n = NULL, ",
                             "power = ", input$poder, "/100, "
                           )
                         } else {
                           paste0(
                             "n = ", input$n, ", ",
                             "power = NULL, "
                           )
                         },
                         "delta = ", input$media_esperada, " - ", input$media_sob_h0, ", ",
                         "sd = ", input$sigma, ", ",
                         "type = 'one.sample', ",
                         "sig.level = ", input$alpha,  "/100, ",
                         "alternative = '", case_when(input$th_alternativa == 'bi'  ~ "two.sided",
                                                      input$th_alternativa == 'sup' ~ "greater",
                                                      input$th_alternativa == 'inf' ~ "less"), "')"
          )

          # print_r_code(code)

          n <- try_n(code)
          eval(parse(text = validate_n("n")))
          n <- ifelse(tipo == "tamanho_amostral", ceiling(n$n), round(n$power*100, 1))
          eval(parse(text = validate_n_inf("n")))


          if (tipo == "tamanho_amostral") {
            paste0(
              "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n, "</font></b></br></br>",

              lista_de_funcoes_server()$sugestao_texto_portugues(
                  "<i>",
                  translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
                  "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para testar se a média de <i>", nome_desfecho(),
                  "</i> é ",
                  case_when(input$th_alternativa == 'bi'  ~ "diferente de ",
                            input$th_alternativa == 'sup' ~ "maior do que ",
                            input$th_alternativa == 'inf' ~ "menor do que "),
                  "<b>", input$media_sob_h0, " ", unidade_medida(), "</b> ",
                  "(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",
                  "O cálculo considerou poder de <b>", input$poder, "%</b>, ",
                  "nível de significância de <b>", input$alpha, "%</b>, ",
                  "média esperada de <b>", input$media_esperada, " ", unidade_medida(),"</b> ",
                  "e desvio padrão esperado para <i>", nome_desfecho(), "</i> igual a <b>",
                  input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)).  ",
                  .txt_citacao_pss
              ),

              .txt_referencia_tap,
              print_r_code(code)
            )
          } else {
            paste0(
              "<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", n, "%</font></b></br></br>",

              lista_de_funcoes_server()$sugestao_texto_portugues(
                "<i>",
                  translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
                  "O poder para testar se a média de <i>", nome_desfecho(),
                  "</i> é ",
                  case_when(input$th_alternativa == 'bi'  ~ "diferente de ",
                            input$th_alternativa == 'sup' ~ "maior do que ",
                            input$th_alternativa == 'inf' ~ "menor do que "),
                  "<b>", input$input$media_sob_h0, " ", unidade_medida(), "</b> é <b>", n, "%</b>. ",
                  "Este valor foi obtido considerando nível de significância de de <b>", input$alpha, "%</b>, ",
                  "tamanho de amostra igual a <b>", input$n, "</b>, ",
                  "média esperada de <b>", input$media_esperada, " ", unidade_medida(),"</b> ",
                  "e desvio padrão esperado para <i>", nome_desfecho(), "</i> igual a <b>",
                  input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)).  ",
                  .txt_citacao_pss
                ),
              .txt_referencia_tap,
              print_r_code(code)
            )
          }


        } else {

          if (amostragem_aleatoria_simples_selecionada()) {

            code <- paste0(
              "presize::prec_mean(",
              "mean = 0, ",
              "sd = ", input$sigma, ", ",
              "conf.width = ", input$precisao, "*2, ",
              "conf.level = ", input$confianca, "/100)"
            )
            # print_r_code(code)

            n <- try_n(code)
            eval(parse(text = validate_n("n")))
            n <- ceiling(n$n)
            eval(parse(text = validate_n_inf("n")))

            paste0(
              "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n, "</font></b></br></br>",

              lista_de_funcoes_server()$sugestao_texto_portugues(
                "<i>",
                  translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
                  "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para estimar a média de <i>", nome_desfecho(),  "</i> ",
                  "com margem de erro de <b>", input$precisao," ", unidade_medida(),"</b> ",
                  "(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",
                  "O cálculo considerou nível de confiança de <b>", input$confianca, "%</b> e desvio padrão esperado para <i>", nome_desfecho(),  "</i> igual a <b>",
                  input$sigma, " ", unidade_medida(), "</b> (referido por Fulano (1900)).  ",
                  .txt_citacao_pss
                ),
              .txt_referencia_tap,
              print_r_code(code)
            )


            # Amostragem estratificada
          } else if (input$delineamento == "aae") {

            req(!is.null(aae_inputs()))

            validate(need(all(sapply(aae_inputs(), length) == input$numero_estratos),
                          'Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para psshealth@hcpa.edu.br.'))


            code <- paste0(
              "epiR::epi.ssstrataestc(",
              "strata.n = c(", paste(aae_inputs()$N, collapse = ", "), "), ",
              "strata.xbar = c(", paste(aae_inputs()$medias, collapse = ", "), "), ",
              "strata.sigma = c(", paste(aae_inputs()$desvio, collapse = ", "), "), ",
              "epsilon = ", input$precisao, "*2, ",
              "error = 'absolute', ",
              "conf.level = ", input$confianca, "/100)"
            )

            # print_r_code(code)

            ne <- try_n(code)
            eval(parse(text = validate_n("ne")))
            n <- ne$total.sample
            eval(parse(text = validate_n_inf("n")))

            n_texto <- paste0(ne$strata.sample, " no estrato ", LETTERS[1:input$numero_estratos], collapse = ", ")
            n_texto <- sub(",([^,]*)$", " e\\1", n_texto)

            paste0(
              "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n, "</font></b></br></br>",

              lista_de_funcoes_server()$sugestao_texto_portugues(
                "<i>",
                translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

                  "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos (",
                  n_texto,
                  ") para estimar a média de <i>", nome_desfecho(),  "</i> ",
                  "(com o acréscimo de <b>", input$perc_perdas, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$perc_perdas), "</b>). ",

                  "O cálculo considerou margem de erro de <b>", input$precisao," ", unidade_medida(),"</b>, ",
                  "nível de confiança de <b>", input$confianca, "%</b>, ",

                  "processo de amostragem estratificada simples nos estratos ",
                  sub(",([^,]*)$", " e\\1", paste(LETTERS[1:input$numero_estratos], collapse = ", ")),
                  ", com tamanhos populacionais de <b>",
                  sub(",([^,]*)$", " e\\1", paste(aae_inputs()$N, collapse = ", ")),
                  "</b> indivíduos, médias esperadas de <b>",
                  sub(",([^,]*)$", " e\\1", paste(aae_inputs()$medias, collapse = ", ")),
                  " ", unidade_medida(), "</b> e desvios padrões esperados de <b>",
                  sub(",([^,]*)$", " e\\1", paste(aae_inputs()$desvio, collapse = ", ")),
                  " ", unidade_medida(), "</b>, respectivamente (referido por Fulano (1900)). ",
                  .txt_citacao_pss
                ),

              .txt_referencia_tap,
              print_r_code(code)
            )





            # Amostragem por conglomerados em um estagio
          } else if(input$delineamento == "ac1") {


            code <- paste0(
              "epiR::epi.ssclus1estc(",
              "b = c(", input$N_medio_conglomerados, ", ", input$desvio_medio_conglemerados, "), ",
              "N = ", input$n_conglomerados, "*", input$N_medio_conglomerados,", ",
              "xbar = ", input$xbar_conglomerados, ", ",
              "xsigma = ", input$desvio_conglomerados, ", ",
              "epsilon = ", input$precisao, "*2, ",
              "error = 'absolute', ",
              "rho = ", input$rho, ", ",
              "conf.level = ", input$confianca, "/100)"
            )

            # print_r_code(code)

            nc <- try_n(code)
            eval(parse(text = validate_n("nc")))
            n <- nc$n.psu
            eval(parse(text = validate_n_inf("n")))


            paste0(
              "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": <i>", n, " conglomerados</i></font></b></br></br>",

              lista_de_funcoes_server()$sugestao_texto_portugues(
                "<i>",
                  translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

                  "Foi calculado um tamanho de amostra de <b>", n, "</b> conglomerados para estimar a média de <i>", nome_desfecho(),  "</i>. ",
                  "O cálculo considerou como margem de erro de <b>", input$precisao," ", unidade_medida(),"</b>, ",
                  "nível de confiança de <b>", input$confianca, "%</b>, ",

                  "processo de amostragem por conglomerados em um único estágio ",
                  "de uma população dividida em <b>", input$n_conglomerados, "</b> conglomerados, ",
                  "na qual o número de indivíduos em cada conglomerado é em média de <b>", input$N_medio_conglomerados, "</b> ",
                  if (input$desvio_medio_conglemerados != 0) {
                    paste0("com desvio padrão de <b>", input$desvio_medio_conglemerados, "</b> ")
                  },
                  "indivíduos, ",

                  "média de <i>", nome_desfecho(),  "</i> esperada de <b>", input$xbar_conglomerados, " ", unidade_medida(), "</b>, ",
                  "desvio padrão esperado de <b>", input$desvio_conglomerados, " ", unidade_medida(), "</b> e ",

                  "coeficiente de correlação intra conglomerados de <b>", input$rho, "</b> (referido em Fulano (1900)). ",
                  .txt_citacao_pss
                ),
              .txt_referencia_tap,
              print_r_code(code)
            )

          }

        }



      })





      # Cenarios ----


      output$cenarios <- renderUI({

        req(tipo == "estimar")
        req(amostragem_aleatoria_simples_selecionada())

        req(!is.null(input$precisao))
        val_min <- input$precisao
        val_max <- input$precisao + 3


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


          HTML("<b>",
               translation_pss("Defina a sequência de valores para a margem de erro", linguagem()),
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
              numericInput(ns("by"), translation_pss("Intervalo", linguagem()), value = 0.5, min = 0, step = .5) %>%
                .help_buttom(linguagem = linguagem(), body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()),
                             title = "Sequência")
          ),


          fluidRow(
            column(6,
                   textInput(inputId = ns("sd_plot"),
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



      eval(parse(text = check_text_input_to_vector("sd_plot")))



      tab_TH_cenarios <- reactive({

        desvios_plot <- text_input_to_vector(input$sd_plot)
        req(length(desvios_plot) > 0)

        expand.grid(margem = seq(from = input$from, to = input$to, by = input$by),
                    desvio = desvios_plot,
                    confianca = input$confianca) %>%
          mutate(
            n = mapply(
              function(sd, conf.width, conf.level){
                presize::prec_mean(mean = 0, sd = sd, conf.width = conf.width*2, conf.level = conf.level/100)$n
              },
              desvio, margem, confianca
            ),

            n = ceiling(n)
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
            aes(x = margem,
                y = n,
                color = !!sym(translation_pss("DP", linguagem()))
            )
          ) +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = seq(from = input$from, to = input$to, by = input$by)) +
          xlab(translation_pss("Margem de erro", linguagem()))  +
          ylab(translation_pss("Tamanho da amostra*", linguagem())) +
          theme_bw() +
          theme(axis.text = element_text(colour = "black")) +
          scale_color_brewer(
            name = translation_pss("Desvio padrão", linguagem()),
            palette = "Set1"
          )


        plotly::ggplotly(g1,
                         tooltip = c("x", "colour", "y", translation_pss("Tratamento", linguagem()), translation_pss("Controle", linguagem()))) %>%
          plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                            showarrow = F, xref='paper', yref='paper',
                                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                            font=list(size=10)))
      })








      return_table_tabela_cenarios <- reactive({

        df_ <- tab_TH_cenarios()

        colnames(df_) <- c(
          translation_pss("Margem de erro", linguagem()),
          translation_pss("Desvio padrão", linguagem()),
          translation_pss("Nível de confiança (%)", linguagem()),
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
        filename = function() { "Cenarios_tamanho_amostra_1_media.xlsx"},
        content = function(file) {writexl::write_xlsx(return_table_tabela_cenarios(),
                                                      path = file)}
      )





    } # Nao mexer!!!
  )

}
