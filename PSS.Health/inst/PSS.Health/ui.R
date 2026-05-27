



tagList(
  navbarPage( windowTitle = "PSS Health",
              theme = "theme_pss.css",

              tabPanel(""),

              # Página de entrada #
              tabPanel(textOutput("navbarMenu_bemvindo", inline = TRUE),
                       tags$head(tags$link(rel = "icon", href = "PSS.png")),
                       # Muda a cor dos sliderInput
                       tags$head(tags$style(HTML(as.character(paste0(
                         ".js-irs-1 .irs-single, .js-irs-1 .irs-from, .js-irs-1 .irs-to, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar{background: #006338 ;  }",
                         ".js-irs-2 .irs-single, .js-irs-2 .irs-from, .js-irs-2 .irs-to, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar{background: #006338 ;  }",
                         ".js-irs-3 .irs-single, .js-irs-3 .irs-from, .js-irs-3 .irs-to, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar{background: #006338 ;  }",
                         ".js-irs-4 .irs-single, .js-irs-4 .irs-from, .js-irs-4 .irs-to, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar{background: #006338 ;  }"
                       ))))),


                       fluidPage(fluidRow(
                         column(11,
                                fluidRow(
                                  h2("PSS Health"),
                                 uiOutput("pssVersaoInicio")

                                  , radioButtons(
                                    inputId = "idioma",
                                    label = "Idioma/ Language",
                                    choices = c("Português" = "pt", "English" = "en"),
                                    selected = "pt",
                                    inline = TRUE
                                  )

                                )
                         ),
                         column(1, img(src = 'PSS.png', align = "right", width = "150%"))
                       )),

                       br(),br(),
                       uiOutput("texto_pagina_inicial_pt") %>%
                         shinycssloaders::withSpinner(type = 5),
                       uiOutput("texto_pagina_inicial_en") %>%
                         shinycssloaders::withSpinner(type = 5)

              ),



              # Medias ----
              navbarMenu(title = textOutput("navbarMenu_medias", inline = TRUE),
                         tabPanel(
                           textOutput("navbarMenu_umaamostra", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_1_media")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_2gruposindependentes", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_2_medias_independentes")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_2gruposindependentes_infsupeq", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_nao_inferioridade")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_2gruposdependentes", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_2_medias_dependentes")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_delta2grupos", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_TH_duas_amostra_media_2tempos")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_medidasrepetidas", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_TH_medidas_repetidas")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_ANOVA1via", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_ANOVA1")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_ANOVA2via", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_anova_two_way")
                         )
              ),

              # Proporcao ----
              navbarMenu(textOutput("navbarMenu_proporcoes", inline = TRUE),
                         tabPanel(
                           textOutput("navbarMenu_umaamostrap", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_estimacao_uma_prop")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_2gruposindependentesp", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_2_proporcoes_independentes")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_2gruposindependentes_infsupeqp", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_nao_inferioridade_binaria")
                         ),
                         tabPanel(
                           textOutput("navbarMenu_2gruposdependentesp", inline = TRUE),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_mc_nemar")
                         )
              ),

              # Qui, correlacao ----
              tabPanel(
                textOutput("navbarMenu_chi2", inline = TRUE),
                shinyFeedback::useShinyFeedback(),
                uiOutput("aba_qui_quadrado")
              ),

              tabPanel(
                uiOutput("navbarMenu_correlacao"),
                shinyFeedback::useShinyFeedback(),
                uiOutput("aba_correlacao")
              ),

              # Regressao ----
              navbarMenu(uiOutput("navbarMenu_regressao"),
                         tabPanel(
                           "Linear",
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_regressao_linear")
                         ),
                         tabPanel(
                           "Gamma",
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_2_medias_assimetrica")
                         ),
                         tabPanel(
                           uiOutput("navbarMenu_logistica"),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_regressao_logistica")
                         ),
                         tabPanel(
                           "Cox",
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_cox")
                         )
              ),

              # Classificacao ----
              navbarMenu(uiOutput("navbarMenu_classificacao"),
                         tabPanel(
                           "AUC",
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_auc")
                         ),
                         tabPanel(
                           uiOutput("navbarMenu_sens_esp"),
                           # "Sensibilidade/ Especificidade",
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_sensibilidade")
                         )

              ),

              # Concordancia ----
              navbarMenu(uiOutput("navbarMenu_concordancia"),
                         tabPanel(
                           uiOutput("navbarMenu_kappa"),
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_kappa")
                         ),
                         tabPanel(
                           "ICC",
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_icc")
                         ),
                         tabPanel(
                           "Bland-Altman",
                           shinyFeedback::useShinyFeedback(),
                           uiOutput("aba_estimacao_bland")
                         )
              ),
              #
              #
              # #### Cronbach  ####.
              tabPanel(
                uiOutput("navbarMenu_Cronbach"),
                shinyFeedback::useShinyFeedback(),
                uiOutput("aba_estimacao_Cronbach")
              ),


              navbarMenu(uiOutput("navbarMenu_outras_ferramentas"),
                         tabPanel(
                           uiOutput("navbarMenu_obterDP"),
                           uiOutput("aba_obter_dp")
                         ),
                         tabPanel(
                           uiOutput("navbarMenu_obter_correlacao"),
                           uiOutput("aba_obter_correlacao")
                         ),
                         tabPanel(
                           uiOutput("navbarMenu_dp_combinado"),
                           uiOutput("aba_pooled_var")
                         ),
                         tabPanel(
                           uiOutput("navbarMenu_dCohen"),
                           uiOutput("aba_cohen")
                         ),
                         tabPanel(
                           uiOutput("navbarMenu_perc_para_chance"),
                           uiOutput("aba_obter_razao")
                         )
              )


  )
)

