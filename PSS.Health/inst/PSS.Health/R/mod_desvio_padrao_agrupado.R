
mod_desvio_padrao_agrupado_Ui <- function(id){

  nss <- NS(id)

  tagList(

    actionLink(nss("mudar_nomess"), "MUDAR"),
    p(nss("uuu"))

    # uiOutput(nss("abaa")) %>%
    # shinycssloaders::withSpinner(type = 5)

  )# Fecha tagList
}




mod_desvio_padrao_agrupado_server <- function(id,
                                              txt_ajuda, translation_pss, linguagem,
                                              grupo_A, grupo_B,
                                              warning_numero_positivo, warning_inteiro){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      nss <- session$ns


      # eval(parse(text = warning_numero_positivo("sigma_A")))
      # eval(parse(text = warning_numero_positivo("sigma_B")))
      # eval(parse(text = warning_inteiro("n_A")))
      # eval(parse(text = warning_inteiro("n_B")))


      # Aba ----


      output$resultadoooo <- renderText({

        s2a <- input$sigma_A^2
        s2b <- input$sigma_B^2

        n1 <- input$n_A
        n2 <- input$n_A

        numerador   <- (n1 - 1)*s2a + (n2 - 1)*s2b
        denominador <- n1 + n2 - 2

        s_pooled <- sqrt(numerador/denominador)

        paste0("<br><br><b><font size = '5'>",
               "<i>", transslation_pss("Desvio padrão", linguagem()), "<sub>", transslation_pss("combinado", linguagem()), "</sub></i> = ", round(s_pooled, 4),
               "<br><br><br>")
      })



      observeEvent(input$mudar_nomess, {
        validate(need(input$mudar_nomess > 0, ''))

        showModal(
          function() {
            nss <- session$ns
            modalDialog(
              title = transslation_pss("Ajustes", linguagem()),
              fluidPage(
                # uiOutput(nss("abaa")) %>%
                #   shinycssloaders::withSpinner(type = 5)
                fluidPage(

                  HTML("<b><font size = '3'>", transslation_pss("Desvio padrão", linguagem()), " do</font></b><br>"),

                  div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                      numericInput( nss("sigma_A"),
                                    grupo_A,
                                    value = 2.0,
                                    min = 0,
                                    max = Inf,
                                    step = .5)),
                  div(style="display: inline-block;vertical-align:top; width: 49%;",
                      numericInput( nss("sigma_B"),
                                    grupo_B,
                                    value = 1.5,
                                    min = 0,
                                    max = Inf,
                                    step = .5)),


                  HTML(paste0("<b><font size = '3'>", transslation_pss("Tamanho amostral", linguagem()), " do</font></b><br>")),
                  div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                      numericInput( nss("n_A"),
                                    grupo_A,
                                    value = 20,
                                    min = 3,
                                    max = Inf,
                                    step = 1)),
                  div(style="display: inline-block;vertical-align:top; width: 49%;",
                      numericInput( nss("n_B"),
                                    grupo_B,
                                    value = 30,
                                    min = 3,
                                    max = Inf,
                                    step = 1)),

                  htmlOutput(nss("resultadoooo")),

                  p(transslation_pss("Foi utilizado a fórmula", linguagem())),
                  withMathJax(
                    paste0(
                      "$$s_{", transslation_pss("combinado", linguagem()), "} = \\sqrt{ \\dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} }$$"
                    )
                  )
                )
              ),
              easyClose = TRUE,
              footer    = NULL
            )
          },
          session = session
        )
      })



    } # Nao mexer!!!
  )

}
