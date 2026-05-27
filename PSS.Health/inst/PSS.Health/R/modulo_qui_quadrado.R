
mod_qui_quadrado_Ui <- function(id){

  ns <- NS(id)

  tagList(

    uiOutput(ns("aba_associacao")) %>%
      shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_qui_quadrado_server <- function(id, tipo = "tamanho_amostral", txt_ajuda, txt_outros_desfechos,
                                    translation_pss, linguagem, .rodape, validate_n, ajuda_cenarios_multiplos_valores, validate_n_inf, n_perdas, print_r_code, text_input_to_vector, check_text_input_to_vector,
                                    warning_prop, warning_numero_positivo, warning_inteiro, warning_perdas,
                                    lista_de_funcoes_server){
  shiny::moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      eval(parse(text = warning_prop("sig_chisq_n")))
      eval(parse(text = warning_prop("power_chisq_n")))
      eval(parse(text = warning_numero_positivo("w_chisq_n")))
      eval(parse(text = warning_inteiro("df_chisq_n")))
      eval(parse(text = warning_inteiro("chisq_ncol")))
      eval(parse(text = warning_inteiro("chisq_nrow")))
      eval(parse(text = warning_perdas("chisq_perdas_recusa")))


      opcoes_entradaW <- reactive({
        nomes <- c(translation_pss("Tamanho de efeito w de Cohen", linguagem()),
                   translation_pss("Tabela de contingência com proporções", linguagem()),
                   translation_pss("Tabela de contingência com valores absolutos", linguagem())
        )
        opcoes_numerico = 1:3
        names(opcoes_numerico) <- nomes
        opcoes_numerico
      })


      output$aba_associacao <- renderUI({

        req(!is.null(opcoes_entradaW()))

        tagList(

          sidebarLayout(
            sidebarPanel(

              wellPanel(HTML(
                '<b><a href="https://youtu.be/ldNW1ctO3uE" target="_blank">',
                translation_pss("Vídeo: PSS Health para comparação de três proporções independentes", linguagem()),
                '</a></b><br>'
              )),

              textInput(inputId = ns("chisq_desfecho"),
                        label   = translation_pss("Descreva o nome das variáveis que deseja associar", linguagem()),
                        value   = translation_pss("X1 e X2", linguagem())
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_outros_desfechos()),


              selectInput(inputId = ns("chisq_input"),
                          label = translation_pss("Qual é informação de entrada?", linguagem()),
                          choices = opcoes_entradaW(),
                          selected = 1
              ),


              uiOutput(ns("chis_entrada_valoresUi")),
              uiOutput(ns("poder_oun")),

              numericInput( ns("sig_chisq_n"),
                            translation_pss("Nível de significância (%)", linguagem()),
                            value = 5,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem())),

              if (tipo == "tamanho_amostral") {
                numericInput( ns("chisq_perdas_recusa"),
                              translation_pss("Perdas/ Recusas (%)", linguagem()),
                              value = 10,
                              min = 0,
                              max = 100,
                              step = 1
                ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
              }
            ),

            mainPanel(
              uiOutput(ns("chisq_tab_contUi")) %>%
                shinycssloaders::withSpinner(type = 5),
              shinycssloaders::withSpinner(htmlOutput(ns("chisq_n")), type = 5),

              uiOutput(ns("cenarios_chisq_Ui"))
            )
          )

        )

      })



      output$poder_oun <- renderUI({

        if (tipo == "tamanho_amostral") {
          numericInput( ns("power_chisq_n"),
                        translation_pss("Poder (%)", linguagem()),
                        value = 80,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem()))
        } else if (input$chisq_input != 3) {
          numericInput( ns("chisq_n"),
                        translation_pss("Tamanho amostral", linguagem()),
                        value = 80,
                        min = 4,
                        step = 1
          )
        }
      })



      output$chis_entrada_valoresUi <- renderUI({


        if(input$chisq_input == 1){

          fluidPage(fluidRow(
            numericInput( ns("w_chisq_n"),
                          translation_pss("Tamanho de efeito w de Cohen", linguagem()),
                          value = .3,
                          min = 0,
                          max = 1,
                          step = .01
            ) %>% .help_buttom(linguagem = linguagem(), body = paste0(translation_pss("Tamanho de efeito w de Cohen", linguagem()), txt_ajuda()$txt_definido_pesquisador_OU_literatura)),


            numericInput( ns("df_chisq_n"),
                          translation_pss("Graus de liberdade", linguagem()),
                          value = 3,
                          min = 0,
                          max = Inf,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Número de graus de liberdade da estatística de teste. Pode ser obtido realizando o cálculo (L-1)x(C-1), onde L é o número de categorias da primeira variável e C da segunda variável.", txt_ajuda()$txt_definido_pesquisador_OU_literatura))
          ))
        } else {

          fluidPage(fluidRow(wellPanel(

            numericInput(inputId = ns("chisq_nrow"),
                         label   = "Número de linhas da tabela de contingência",
                         value   = 2,
                         min     = 2,
                         step    = 1

            ),
            numericInput(inputId = ns("chisq_ncol"),
                         label   = "Número de colunas da tabela de contingência",
                         value   = 2,
                         min     = 2,
                         step    = 1
            ),

            HTML("<i><b>ATENÇÃO!</b> Edite a tabela de contingência no painel principal ao lado. As tabelas são editáveis, basta clicar duas vezes sobre a célula --> </i><br><br>")

          )))
        }
      })




      #  Tabela de contingência

      tabela_chisq <- reactiveValues(tab = data.frame(a = 1))

      observeEvent(c(input$chisq_input, input$chisq_nrow, input$chisq_ncol), {

        req(!is.na(input$chisq_nrow) & !is.na(input$chisq_ncol))
        req(input$chisq_ncol%%1 == 0 & input$chisq_ncol >= 2)
        req(input$chisq_nrow%%1 == 0 & input$chisq_nrow >= 2)

        # Cria uma tabela inicial
        set.seed(2022)
        tab_vetor <- round(runif(input$chisq_nrow*input$chisq_ncol, 10, 50), 0)

        if(input$chisq_input == 2){
          tab_vetor <- round(tab_vetor/sum(tab_vetor)*100, 1)
          tab_vetor[1] <- 100 - (sum(tab_vetor) - tab_vetor[1])
          tab_vetor <- round(tab_vetor, 1)
        }


        tab <- matrix(tab_vetor,
                      nrow = input$chisq_nrow,
                      ncol = input$chisq_ncol) %>% as.data.frame()

        colnames(tab) <- paste0("Cat Y", 1:input$chisq_ncol)
        rownames(tab) <- paste0("Cat X", 1:input$chisq_nrow)

        tabela_chisq$tab <- tab
      })



      output$chisq_tab_cont <- DT::renderDT({
        # req(input$chisq_input != 1)


        DT::datatable(tabela_chisq$tab,
                      editable = list(target = "cell", disable = list(columns = c(0))),
                      extensions = c('FixedColumns'),
                      rownames   = TRUE,
                      filter     = "none",
                      options    = list(autoWidth = FALSE,
                                        searching = FALSE,
                                        ordering  = FALSE,
                                        pageLength = 15,
                                        dom = 't',
                                        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      })





      observeEvent(input$chisq_tab_cont_cell_edit, {
        cell <- input$chisq_tab_cont_cell_edit
        newdf <- tabela_chisq$tab
        new_value <- gsub(",", ".", as.character(cell$value)) %>%
          gsub("[^0-9.-]", "", .) %>%
          as.numeric()

        newdf[cell$row, cell$col] <- new_value

        tabela_chisq$tab <- newdf
      })




      chisq_problema_tab_cont <- reactive({
        problemas <- ""

        if(any(is.na(tabela_chisq$tab))){
          problemas <- paste0('<font color = "red"><b>Todas as células devem ser preenchidas.</b></font><br/>')
        } else if(input$chisq_input == 3 & any(tabela_chisq$tab%%1 != 0)){
          problemas <- paste0('<font color = "red"><b>Todos os valores devem ser inteiros não negativos.</b></font><br/>')
        } else if(input$chisq_input == 2 & sum(tabela_chisq$tab) != 100){
          soma <- round(sum(tabela_chisq$tab), 2)

          if(soma > 100){
            problemas <- paste0('<font color = "red"><b>A soma das proporções deve fechar 100%. Atualmente está somando ', soma, '%, precisa remover ', round(soma - 100, 2), '%.</b></font><br/>')
          } else{
            problemas <- paste0('<font color = "red"><b>A soma das proporções deve fechar 100%. Atualmente está somando ', soma, '%, precisa adicionar ', round(100 - soma, 2), '%.</b></font><br/>')
          }

        }
        problemas
      })




      output$chisq_tab_contUi <- renderUI({

        req(input$chisq_input != 1)

        fluidPage(fluidRow(wellPanel(

          if(chisq_problema_tab_cont() != ""){
            HTML(paste0('<font size = "+0.1"><font color = "red">', "<b>Tabela de contingência editável (",
                        ifelse(input$chisq_input == 2, "% do total", "valores absolutos"),
                        "):</b>", '</font></font><br/>'))
          } else{
            HTML(paste0('<font size = "+0.1">', "<b>Tabela de contingência editável (",
                        ifelse(input$chisq_input == 2, "% do total", "valores absolutos"),
                        "):</b>", '</font><br/>'))
          },

          DT::dataTableOutput(ns("chisq_tab_cont")),

          if(chisq_problema_tab_cont() != ""){
            HTML(chisq_problema_tab_cont())
          },
          br(), br()
        )))
      })






      chisq_w <- reactive({

        if(input$chisq_input == 1){
          list(w = input$w_chisq_n, gl = input$df_chisq_n)
        } else {

          validate(need(chisq_problema_tab_cont() == "", "Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para psshealth@hcpa.edu.br."))

          matriz_prop <- as.matrix(tabela_chisq$tab)

          if(input$chisq_input == 2){
            w <- pwr::ES.w2(matriz_prop/100)
          } else{
            w <- pwr::ES.w2(prop.table(as.matrix(tabela_chisq$tab)))
          }
          list(w = round(w, 3),
               gl = (nrow(matriz_prop) - 1) * (ncol(matriz_prop) - 1),
               tabela = matriz_prop
          )



        }
      })






      ## Render output ----


      output$chisq_n <- renderText({

        req(!is.null(chisq_w()))

        if (tipo == "tamanho_amostral") {
          req(!is.null(input$power_chisq_n))

          code <- paste0(
            "pwr::pwr.chisq.test(",
            "N = NULL, ",
            "w = ", chisq_w()$w, ", ",
            "df = ", chisq_w()$gl, ", ",
            "sig.level = ", input$sig_chisq_n, "/100, ",
            "power = ", input$power_chisq_n, "/100)"
          )

          n <- eval(parse(text = code))
          n <- ceiling(n$N)
          eval(parse(text = validate_n("n")))
          eval(parse(text = validate_n_inf("n")))



          paste0(
            "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
            "</font></b></br></br>",


            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>",
              translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

              "Foi calculado um tamanho de amostra de <b>", n,
              "</b> sujeitos para testar se existe associação entre <b>", input$chisq_desfecho,
              "</b> (com o acréscimo de <b>", input$chisq_perdas_recusa,
              "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$chisq_perdas_recusa), "</b>). ",
              "O cálculo considerou um poder de <b>", input$power_chisq_n,
              "%</b>, nível de significância de <b>", input$sig_chisq_n,
              "%</b> e tamanho de efeito w de Cohen igual a <b>", chisq_w()$w, "</b> e <b>", chisq_w()$gl,
              "</b> graus de liberdade conforme obtido em Fulano (1900) <b>OU</b> escolha do pesquisador. ",
              .txt_citacao_pss
            ),
            .txt_referencia_tap, print_r_code(code)
          )


          ## Poder ----
        } else {


          code <- paste0(
            "pwr::pwr.chisq.test(",
            "N = ", ifelse(input$chisq_input != 3, input$chisq_n, sum(as.matrix(tabela_chisq$tab))), ", ",
            "w = ", chisq_w()$w, ", ",
            "df = ", chisq_w()$gl, ", ",
            "sig.level = ", input$sig_chisq_n, "/100, ",
            "power = NULL)"
          )

          n <- eval(parse(text = code))
          n <- round(n$power*100, digits = 1)
          eval(parse(text = validate_n("n")))
          eval(parse(text = validate_n_inf("n")))



          paste0(
            "<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", n,
            "%</font></b></br></br>",

            lista_de_funcoes_server()$sugestao_texto_portugues(
              "<i>",
              translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

              "O poder para testar se existe associação entre <b>", input$chisq_desfecho,
              "</b> é <b>", n,
              "%</b>. O cálculo considerou nível de significância de <b>", input$sig_chisq_n,
              "%</b>, tamanho amostral de <b>", ifelse(input$chisq_input != 3, input$chisq_n, sum(as.matrix(tabela_chisq$tab))),
              "</b> sujeitos, tamanho de efeito w de Cohen igual a <b>", chisq_w()$w, "</b> e <b>", chisq_w()$gl,
              "</b> graus de liberdade conforme obtido em Fulano (1900) <b>OU</b> escolha do pesquisador. ",
              .txt_citacao_pss
            ),
            .txt_referencia_tap, print_r_code(code)
          )


        }



      })



      ## Cenarios ----


      output$cenarios_chisq_Ui <- renderUI({
        req(tipo == "tamanho_amostral")
        req(!is.null(chisq_w()$w))

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
               translation_pss("Defina a sequência de valores para a magnitude do efeito", linguagem()),
               ":</b>"
          ),
          br(),
          div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput(ns("chisq_from"), translation_pss("Mínimo", linguagem()), value = chisq_w()$w, step = 0.5)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("chisq_to"), translation_pss("Máximo", linguagem()), value = chisq_w()$w + 0.8, step = 0.5)
          ),
          div(style="display: inline-block;vertical-align:top; width: 80px;",
              numericInput(ns("chisq_by"), translation_pss("Intervalo", linguagem()), value = 0.1, min = 0, step = 0.1) %>%
                .help_buttom(linguagem = linguagem(), body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()),
                             title = "Sequência")
          ),
          br(),

          fluidRow(
            column(6,
                   textInput(inputId = ns("chisq_power_plot"),
                             label   = translation_pss("Digite valores de poder (%) para fazer o gráfico", linguagem()),
                             value   = "80, 90, 95",
                             width   = "400px") %>%
                     .help_buttom(linguagem = linguagem(), body = ajuda_cenarios_multiplos_valores())
            )
          ),

          br(),

          plotly::plotlyOutput(ns("chisq_plot"), width = "80%"),
          br(), br(),
          downloadButton(ns("download_chisq_tab"),translation_pss("Download tabela", linguagem())),
          DT::dataTableOutput(ns("chisq_tab"), width = "100%")

        ))
      })


      eval(parse(text = check_text_input_to_vector("chisq_power_plot")))

      tab_chisq_cenarios <- reactive({

        power <- text_input_to_vector(input$chisq_power_plot)
        req(length(power) > 0)


        expand.grid(`Magnitude do efeito w` = seq(from = input$chisq_from, to = input$chisq_to, by = input$chisq_by),
                    `Poder (%)` = power,
                    `Nível de significância (%)` =  input$sig_chisq_n,
                    gl = chisq_w()$gl) %>%
          mutate(`Tamanho da amostra` =
                   mapply(function(w, df, sig.level, power){
                     tryCatch({
                       n <- pwr::pwr.chisq.test(N = NULL, w = w, df = df, sig.level = sig.level/100, power = power/100)
                       ceiling(n$N)
                     },
                     warning = function(warning_condition) { NA },
                     error = function(error_condition) { NA })},
                     `Magnitude do efeito w`, gl, `Nível de significância (%)`, `Poder (%)`),
                 `n + perdas/ recusas` = n_perdas(`Tamanho da amostra`, input$chisq_perdas_recusa),
                 `Perdas/ Recusas (%)` = input$chisq_perdas_recusa)
      })



      output$chisq_plot <- plotly::renderPlotly({
        req(tipo == "tamanho_amostral")

        g1 <- tab_chisq_cenarios() %>%
          mutate(`Poder (%)` = factor(`Poder (%)`)) %>%
          ggplot(aes(x = `Magnitude do efeito w`, y = `Tamanho da amostra`, color = `Poder (%)`))+
          geom_point() +
          geom_line() +
          scale_x_continuous(breaks = seq(from = input$chisq_from, to = input$chisq_to, by = input$chisq_by)) +
          xlab(translation_pss("Tamanho de efeito w de Cohen", linguagem())) +
          ylab(translation_pss("Tamanho da amostra*", linguagem())) +
          theme_bw() +
          theme(axis.text = element_text(colour = "black")) +
          scale_color_brewer(
            palette = "Set1",
            name = translation_pss("Poder (%)", linguagem())
          )

        plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
          plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                            showarrow = F, xref='paper', yref='paper',
                                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                            font=list(size=10)))
      })


      # Passa os nomes para outra lingua
      tab_chisq_cenarios_down <- reactive({
        temp <- tab_chisq_cenarios()
        colnames(temp) <- c(
          translation_pss("Tamanho de efeito w de Cohen", linguagem()),
          translation_pss("Poder (%)", linguagem()),
          translation_pss("Nível de significância (%)", linguagem()),
          translation_pss("Graus de liberdade", linguagem()),
          translation_pss("Tamanho da amostra", linguagem()),
          translation_pss("n + perdas/ recusas", linguagem()),
          translation_pss("Perdas/ Recusas (%)", linguagem())
        )

        temp

      })

      output$chisq_tab <- DT::renderDataTable({
        req(tipo == "tamanho_amostral")

        tab_chisq_cenarios_down() %>%
          DT::datatable(extensions = c('FixedColumns'),
                        rownames   = FALSE,
                        filter     = "none",
                        options    = list(pageLength = 10,
                                          scrollX = TRUE,
                                          scrollY = TRUE,
                                          searching = FALSE,
                                          fixedColumns = list(leftColumns = 1),
                                          dom = 'B<"dwnld">frtip'))
      })


      output$download_chisq_tab <- downloadHandler(
        filename = function() { "Cenarios_tamanho_amostra_associacao.xlsx"},
        content = function(file) {writexl::write_xlsx(tab_chisq_cenarios_down(), path = file)}
      )


    }
  )

}
