


server <- function(input, output, session) {



  shinyhelper::observe_helpers(withMathJax = TRUE, help_dir = "www")



    showModal(modalDialog(
      title = "Atenção!",
      HTML(paste0(
        "<b>Este serviço de software gratuito é fornecido SEM NENHUMA GARANTIA.</b><br><br>",
        "O PSS Health foi criado para te auxiliar de forma gratuita, por favor, retribua citando o seu uso:",
        .txt_referencia_tap,
        "<br><br> Você também encontrará estas sugestões de citação nas abas de cálculo."
      )),
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Ok"),

    ))




  linguagem <- reactive({
    input$idioma
    # "pt"
    # "en"
  })



  # Funcao para imprimir as sugestoes de textp apenas no idioma portugues
  sugestao_texto_portugues <- function(...) {
    if (linguagem() == "pt") paste0(...) else ""
  }




  .rodape <- reactive({
    fluidPage(
      HTML("<br><br><hr>"),
      if (linguagem() == "pt") {
        HTML('<b>Sugestão, críticas ou bugs?</b> Mande um e-mail para <a href="mailto:psshealth@hcpa.edu.br">psshealth@hcpa.edu.br</a>.<br><br><br><br>' )
      } else {
        HTML('<b>Suggestion, criticism or bugs? </b> Send an email to <a href="mailto:psshealth@hcpa.edu.br">psshealth@hcpa.edu.br</a>.<br><br><br><br>' )
      },
      fluidRow(
        column(3, p("")    ),
        column(1,
               tags$a(
                 href="https://doi.org/10.22491/2357-9730.109542",
                 tags$img(
                   src = "PSS.png",
                   title = "PSS Health",
                   width = "100%"
                 ),
                 target = "_blank"
               )),
        column(1, p("")    ),
        column(1,
               tags$a(
                 href="https://www.hcpa.edu.br/",
                 tags$img(
                   src = "Logomarca_hcpa.png",
                   title = "HCPA",
                   width = "100%"
                 ),
                 target = "_blank"
               )
        ),
        column(1, p("")    ),
        column(1,
               tags$a(
                 href="http://www.ufrgs.br/",
                 tags$img(
                   src = "Logo_ufrgs.jpg",
                   title = "UFRGS",
                   width = "125%"
                 ),
                 target = "_blank"
               )
        ),
        column(4, p("")    )
      ),
      br(), br(), br(), br()
    )
  })



  #______________--------------
  #      Botoes de ajuda    ----
  #----------------------------.

  # .help_buttom <- function(local, body, title = "Ajuda") {
  #
  #   shinyhelper::helper(shiny_tag = local,
  #                       type = "inline",
  #                       title = title,
  #                       content = body,
  #                       buttonLabel = "Fechar",
  #                       fade = TRUE,
  #                       colour = "#006338",
  #                       size = "m")
  #
  # }

  txt_definido_pesquisador_OU_literatura <- reactive({
    case_when(
      linguagem() == "en" ~ "<br><br><b>It can be a value from the literature or a value that the researcher wants to find that has clinical relevance.</b>",
      TRUE ~"<br><br><b>Pode ser um valor da literatura ou um valor que o pesquisador deseja encontrar e que tenha relevância clínica.</b>"
    )
  })

  txt_definido_pesquisador <- reactive({
    case_when(
      linguagem() == "en" ~ "<br><br><b>Must be defined by the researcher.</b>",
      TRUE ~ "<br><br><b>Deve ser definido pelo pesquisador.</b>"
    )
  })

  txt_definido_literatura <- reactive({
    case_when(
      linguagem() == "en" ~ "<br><br><b>Must be obtained from previous studies (preferably with the same target population) where this variable was measured or from pilot samples.</b>",
      TRUE ~ "<br><br><b>Deve ser obtido de estudos anteriores (de preferência com a mesma população alvo) onde esta variável foi mensurada ou de amostras piloto.</b>"
    )
  })


  txt_ajuda <- reactive({
    list(

      txt_definido_pesquisador_OU_literatura = txt_definido_pesquisador_OU_literatura(),
      txt_definido_pesquisador = txt_definido_pesquisador(),
      txt_definido_literatura  = txt_definido_literatura(),

      txt_um = case_when(
        linguagem() == "en" ~ paste0(
          "Descreva a unidade de medida em que seu desfecho será mensurado para que sirva de guia no preenchimento dos demais valores. ",
          "<br><br>Por exemplo, se seu desfecho de interesse é o colesterol, a unidade de medida pode ser <b>mg/dl</b> ou <b>mmol/l</b>; ",
          "se o interesse é a altura, a unidade de medida pode ser <b>metros</b> ou <b>centímetros</b>."
        ),
        TRUE ~ paste0(
          "Descreva a unidade de medida em que seu desfecho será mensurado para que sirva de guia no preenchimento dos demais valores. ",
          "<br><br>Por exemplo, se seu desfecho de interesse é o colesterol, a unidade de medida pode ser <b>mg/dl</b> ou <b>mmol/l</b>; ",
          "se o interesse é a altura, a unidade de medida pode ser <b>metros</b> ou <b>centímetros</b>."
        )
      ),

      txt_desfecho = case_when(
        linguagem() == "en" ~ paste0(
          "Descreva o nome do desfecho para que sirva de guia no preenchimento dos demais valores. ",
          "Essa informação completará o texto do sugerido do tamanho amostral calculado para relatar nos projetos de pesquisa ou nos trabalhos científicos.",
          "<br><br>O desfecho é a variável mais relevante do estudo, que servirá para testar a hipótese em questão ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">(Castro et al. 2019)</a>.'
        ),
        TRUE ~ paste0(
          "Descreva o nome do desfecho para que sirva de guia no preenchimento dos demais valores. ",
          "Essa informação completará o texto do sugerido do tamanho amostral calculado para relatar nos projetos de pesquisa ou nos trabalhos científicos.",
          "<br><br>O desfecho é a variável mais relevante do estudo, que servirá para testar a hipótese em questão ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">(Castro et al. 2019)</a>.'
        )
      ),


      txt_diferenca_clinica = case_when(
        linguagem() == "en" ~ paste0(
          "It is the smallest difference considered clinically relevant (that has any clinical value). ",
          "<br><br>More details in the section \"<i>QUAL A DIFERENÇA ENTRE SIGNIFICÂNCIA ESTATÍSTICA E RELEVÂNCIA CLÍNICA?</i>\" from ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "É a menor diferença considerada clinicamente relevante (que tenha algum valor clínico). ",
          "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE SIGNIFICÂNCIA ESTATÍSTICA E RELEVÂNCIA CLÍNICA?</i>\" de ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador()
        )
      ),

      txt_amplitude = case_when(
        linguagem() == "en" ~ paste0(
          "Is the total width of the confidence interval (upper bound minus lower bound). ",
          "<br><br>The smaller its value, the greater the accuracy of the estimate, but the required sample size will also be greater.",
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "É a largura total do intervalo de confiança (limite superior menos limite inferior). ",
          "<br><br>Quanto menor seu valor maior será a precisão da estimativa, porém o tamanho de amostra necessário também será maior.",
          txt_definido_pesquisador()
        )
      ),

      txt_precisao = case_when(
        linguagem() == "en" ~ paste0(
          "The margin of error or half width represents half of the total width of the confidence interval. ",
          "<br><br>The smaller its value, the greater the accuracy of the estimate, but the required sample size will also be greater.",
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "A margem de erro ou semi-amplitude representa a metade da largura total do intervalo de confiança. ",
          "<br><br>Quanto menor seu valor maior será a precisão da estimativa, porém o tamanho de amostra necessário também será maior.",
          txt_definido_pesquisador()
        )
      ),

      txt_perc_esperado = case_when(
        linguagem() == "en" ~ paste0(
          "The percentage (%) expected is a measure of the frequency of occurrence of a certain event (for example, occurrence of death, occurrence of an illness, etc.). ",
          "<br><br>Depending on the study design, this percentage is known as prevalence or incidence. ",
          "More details in the section \"<i>QUAL A DIFERENÇA ENTRE PREVALÊNCIA E INCIDÊNCIA?</i>\" from ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.'
        ),
        TRUE ~ paste0(
          "O percentual (%) esperado é uma medida de frequência de ocorrência de um determinado evento (por exemplo, ocorrência de óbito, ocorrência de uma doença, etc.). ",
          "<br><br>Dependendo do delineamento do estudo, esse percentual é conhecido como prevalência ou incidência. ",
          "Mais detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE PREVALÊNCIA E INCIDÊNCIA?</i>\" de ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.'
        )
      ),

      txt_per_method_presize = case_when(
        linguagem() == "en" ~ paste0(
          "Mais detalhes na ",
          '<a href="https://cran.r-project.org/package=presize" target="_blank">documentação original do pacote <i>presize</i>.</a>',

          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "Mais detalhes na ",
          '<a href="https://cran.r-project.org/package=presize" target="_blank">documentação original do pacote.</a>',

          txt_definido_pesquisador()
        )
      ),

      txt_per_method_EnvStats = case_when(
        linguagem() == "en" ~ paste0(
          "Mais detalhes na ",
          '<a href="https://cran.r-project.org/package=EnvStats" target="_blank">documentação original do pacote <i>EnvStats</i>.</a>',

          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "Segundo ",
          "Mais detalhes na ",
          '<a href="https://cran.r-project.org/package=EnvStats" target="_blank">documentação original do pacote.</a>',

          txt_definido_pesquisador()
        )
      ),

      txt_per_method_MESS = case_when(
        linguagem() == "en" ~ paste0(
          "Mais detalhes na ",
          '<a href="https://cran.r-project.org/package=MESS" target="_blank">documentação original do pacote <i>MESS</i>.</a>',

          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "Segundo ",
          "Mais detalhes na ",
          '<a href="https://cran.r-project.org/package=MESS" target="_blank">documentação original do pacote.</a>',

          txt_definido_pesquisador()
        )
      ),


      txt_correcao_continuidade = case_when(
        linguagem() == "en" ~ paste0(
          "Clique aqui para calcular um tamanho de amostra para um teste com correção de continuidade. ",
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "Clique aqui para calcular um tamanho de amostra para um teste com correção de continuidade. ",
          txt_definido_pesquisador()
        )
      ),

      txt_perdas_recusas = case_when(
        linguagem() == "en" ~ paste0(
          "Percentage of expected losses over the course of the survey. The sample size calculation is adjusted to compensate for them. ",
          "<br><br>Considering <b><i>R</i></b> as the percentage of expected losses/refusals and <b><i>n</b></i> as the required sample size without considering losses/refusals, ",
          "this <b><i>n</b></i> is multiplied by <b>1 / [1 - (<i>R</i> / 100)]</b>.",
          txt_definido_pesquisador_OU_literatura()
        ),
        TRUE ~ paste0(
          "Percentual de perdas previstas ao longo da pesquisa. O cálculo do tamanho da amostra é ajustado para compensá-las.",
          "<br><br>Considerando <b><i>R</i></b> como o percentual de perdas/ recusas previstas e <b><i>n</b></i> como o tamanho de amostra necessário sem considerar as perdas/ recusas, ",
          "esse <b><i>n</b></i> é multiplicado por <b>1 / [1 - (<i>R</i> / 100)]</b>.",
          txt_definido_pesquisador_OU_literatura()
        )
      ),


      txt_dp = case_when(
        linguagem() == "en" ~ paste0(
          "Standard deviation is a measure of variability.",
          "<br><br>You can also get the standard deviation of other statistics, see tab ",
          "<i>Other Tools ---> Get the standard deviation of other statistics</i>",
          txt_definido_literatura()
        ),
        TRUE ~ paste0(
          "O desvio padrão é uma medida de variabilidade.",
          "<br><br>Você também pode obter o desvio padrão de outras estatísticas, veja a aba ",
          "<i>Outras ferramentas ---> Obter o desvio padrão de outras estatísticas</i>",
          txt_definido_literatura()
        )
      ),


      txt_correlacao = case_when(
        linguagem() == "en" ~ paste0(
          "Os coeficientes de correlação avaliam a direção e o grau de alinhamento entre duas variáveis. Assumem valores que variam de -1 (correlação negativa perfeita) a 1 (correlação positiva perfeita). ",
          " Mais informações em ",
          '<a href="https://doi.org/10.22491/2357-9730.98944" target="_blank">Modelagem estatística: Perguntas que você sempre quis fazer, mas nunca teve coragem.</a>',
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "Os coeficientes de correlação avaliam a direção e o grau de alinhamento entre duas variáveis. Assumem valores que variam de -1 (correlação negativa perfeita) a 1 (correlação positiva perfeita). ",
          " Mais informações em ",
          '<a href="https://doi.org/10.22491/2357-9730.98944" target="_blank">Modelagem estatística: Perguntas que você sempre quis fazer, mas nunca teve coragem.</a>',
          txt_definido_pesquisador()
        )
      ),


      txt_coef_determinacao = case_when(
        linguagem() == "en" ~ paste0(
          "O coeficiente de determinação é uma medida de quão bem o modelo de regressão descreve os dados observados. ",
          "É o % da variação total de Y que é explicada pela variação de X. ",
          "Por exemplo, suponhamos um modelo com um R² = 0,49, então 49% da variação de Y pode ser explicada pela variação de X",
          txt_definido_pesquisador_OU_literatura()
        ),
        TRUE ~ paste0(
          "O coeficiente de determinação é uma medida de quão bem o modelo de regressão descreve os dados observados. ",
          "É o % da variação total de Y que é explicada pela variação de X. ",
          "Por exemplo, suponhamos um modelo com um R² = 0,49, então 49% da variação de Y pode ser explicada pela variação de X",
          txt_definido_pesquisador_OU_literatura()
        )
      ),


      txt_confianca = case_when(
        linguagem() == "en" ~ paste0(
          "The confidence level represents the percentage of intervals that would include the population parameter if you were to pool samples of the same size from the same population over and over again. ",
          "<br><br>For example, when you have 95% confidence it means that of the numerous confidence intervals constructed from samples of the same size, 95% of them will contain the value of the population parameter. ",
          "<br><br>The higher the confidence level, the larger the required sample size.",
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "O nível de confiança representa a porcentagem de intervalos que iriam incluir o parâmetro populacional se você reunisse amostras do mesmo tamanho, da mesma população, repetidas vezes. ",
          "<br><br>Por exemplo, quando se tem 95% de confiança significa que dos inúmeros intervalos de confiança construídos a partir das amostras de mesmo tamanho, 95% deles conterão o valor do parâmetro populacional. ",
          "<br><br>Quanto maior o nível de confiança, maior o tamanho de amostra necessário.",
          txt_definido_pesquisador()
        )
      ),


      txt_significancia = case_when(
        linguagem() == "en" ~ paste0(
          "The significance level is used as a cut-off point for the probability of making an error when making the statistical decision to reject the null hypothesis (type I error) ",
          "More information in ",
          '<a href="https://doi.org/10.4322/2357-9730.93649" target="_blank">Hirakata et al. 2019</a>.',
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "O nível de significância é utilizado como um ponto de corte na probabilidade de se cometer um erro ao tomarmos a decisão estatística de rejeitar a hipótese nula (erro tipo I). ",
          "Mais informações em ",
          '<a href="https://doi.org/10.4322/2357-9730.93649" target="_blank">Hirakata et al. 2019</a>.',
          txt_definido_pesquisador()
        )
      ),


      txt_power = case_when(
        linguagem() == "en" ~ paste0(
          "The power of a statistical test is the probability of making a correct decision, rejecting the null hypothesis if it is actually false. ",
          "More information in ",
          '<a href="https://doi.org/10.4322/2357-9730.93649" target="_blank">Hirakata et al. 2019</a>.',
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "O poder de um teste estatístico é a probabilidade de se tomar uma decisão correta, rejeitar a hipótese nula se ela realmente for falsa. ",
          "Mais informações em ",
          '<a href="https://doi.org/10.4322/2357-9730.93649" target="_blank">Hirakata et al. 2019</a>.',
          txt_definido_pesquisador()
        )
      ),


      txt_h1 = case_when(
        linguagem() == "en" ~ paste0(
          "Type of test according to the alternative hypothesis: the test can be one or two sided. ",
          "More information in ",
          '<a href="https://doi.org/10.4322/2357-9730.93649" target="_blank">Hirakata et al. 2019</a>.',
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "Tipo de teste de acordo com a hipótese alternativa: o teste pode ser unilateral ou bilateral. ",
          "Mais informações em ",
          '<a href="https://doi.org/10.4322/2357-9730.93649" target="_blank">Hirakata et al. 2019</a>.',
          txt_definido_pesquisador()
        )
      ),


      txt_razao_chance = case_when(
        linguagem() == "en" ~ paste0(
          "The odds ratio is mainly calculated in case-control studies, although it can also be calculated in cross-sectional or longitudinal studies, when the outcome is rare. ",
          "It is obtained by dividing the chance of developing the event in one group by the chance of another group. ",
          "More information in ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador_OU_literatura()
        ),
        TRUE ~ paste0(
          "A razão de chances ou <i>odds ratio</i>, é calculada, principalmente, em estudos do tipo caso-controle, embora também possa ser calculado em estudos transversais ou longitudinais, quando o desfecho for raro. ",
          "É obtida dividindo a chance de desenvolver o evento em um grupo pela chance de outro grupo. ",
          "Mais informações em ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador_OU_literatura()
        )
      ),



      txt_risco_relativo = case_when(
        linguagem() == "en" ~ paste0(
          "Relative risk is a broad term for rate ratios or risk ratios used in longitudinal studies. It is defined as the ratio between the risk of developing the outcome in exposed individuals and the risk of developing the outcome in unexposed individuals based on incidence rates. ",
          "<br><br>",
          "The prevalence ratio, as the name implies, is obtained by the ratio between the prevalence of the disease in exposed individuals and the prevalence of the disease in non-exposed individuals. ",
          "<br><br>More information in ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador_OU_literatura()
        ),
        TRUE ~ paste0(
          "O risco relativo é um termo amplo para designar razões de taxas (<i>rate ratio</i>) ou razões de risco (<i>risk ratio</i>), utilizado em estudos ",
          "longitudinais. É definido como a razão entre o risco de desenvolver o desfecho nos expostos e o risco de desenvolver o desfecho nos não expostos a partir das taxas de incidência. ",
          "<br><br>",
          "A razão de prevalências, como o próprio nome diz, é obtida pela razão entre a prevalência da doença nos expostos e a prevalência da doença nos não expostos. ",
          "<br><br>Mais informações em ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador_OU_literatura()
        )
      ),

      txt_hazard_ratio = case_when(
        linguagem() == "en" ~ paste0(
          "O hazard ratio (HR), ou razão de riscos proporcionais, é calculado quando o desfecho de interesse é o tempo até o evento ocorrer. ",
          "Desta forma, a razão de riscos proporcionais é obtida dividindo o risco (hazard) de desenvolver o evento em um grupo pelo risco de outro grupo. ",
          "Mais informações em ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador_OU_literatura()
        ),
        TRUE ~ paste0(
          "O hazard ratio (HR), ou razão de riscos proporcionais, é calculado quando o desfecho de interesse é o tempo até o evento ocorrer. ",
          "Desta forma, a razão de riscos proporcionais é obtida dividindo o risco (hazard) de desenvolver o evento em um grupo pelo risco de outro grupo. ",
          "Mais informações em ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador_OU_literatura()
        )
      ),

      # txt_balanceamento = case_when(
      #   linguagem() == "en" ~ paste0(
      #     "O balanceamento é uma razão entre tamanhos de dois grupos. Para exemplificar, vamos considerar que faremos o balanceamento para os grupos A e B, isto é <b>A:B</b>:<br><br>",
      #     "<ul>", # inicio da lista
      #     "<li><b>Balanceamento igual a 1:</b> será calculando o tamanho de amostra tal que para cada indivíduo do Grupo A teremos outro indivíduo do grupo B (<b>1:1</b>);</li><br>",
      #     "<li><b>Balanceamento igual a 2:</b> será calculando o tamanho de amostra tal que teremos dois indivíduos do Grupo A para cada indivíduo do grupo B (<b>2:1</b>);</li><br>",
      #     "<li><b>Balanceamento igual a 0.5:</b> será calculando o tamanho de amostra tal que para cada indivíduo do Grupo A teremos dois indivíduos do grupo B (<b>1:2</b>).</li>",
      #     "</ul>", # fim da
      #     txt_definido_pesquisador()
      #   ),
      #   TRUE ~ paste0(
      #     "O balanceamento é uma razão entre tamanhos de dois grupos. Para exemplificar, vamos considerar que faremos o balanceamento para os grupos A e B, isto é <b>A:B</b>:<br><br>",
      #     "<ul>", # inicio da lista
      #     "<li><b>Balanceamento igual a 1:</b> será calculando o tamanho de amostra tal que para cada indivíduo do Grupo A teremos outro indivíduo do grupo B (<b>1:1</b>);</li><br>",
      #     "<li><b>Balanceamento igual a 2:</b> será calculando o tamanho de amostra tal que teremos dois indivíduos do Grupo A para cada indivíduo do grupo B (<b>2:1</b>);</li><br>",
      #     "<li><b>Balanceamento igual a 0.5:</b> será calculando o tamanho de amostra tal que para cada indivíduo do Grupo A teremos dois indivíduos do grupo B (<b>1:2</b>).</li>",
      #     "</ul>", # fim da
      #     txt_definido_pesquisador()
      #   )
      # ),



      txt_margem_nao_inferior = case_when(
        linguagem() == "en" ~ paste0(
          "A margem de não inferioridade quantifica a diferença máxima clinicamente aceitável para que o grupo Tratamento possa ser considerado não inferior ao Controle. ",
          "Esta margem se aplica quando o novo Tratamento traz uma vantagem prática que vale a pena abdicar de uma pequena parte do benefício obtido com o Controle. ",
          "<br><br>Quanto mais próximo de zero esta margem, maior o tamanho de amostra necessário. ",
          "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE ENSAIO CLÍNICO DE COMPARAÇÃO, ENSAIO CLÍNICO DE SUPERIORIDADE, DE EQUIVALÊNCIA E DE não iNFERIORIDADE?</i>\" de ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador()
        ),
        TRUE  ~ paste0(
          "A margem de não inferioridade quantifica a diferença máxima clinicamente aceitável para que o grupo Tratamento possa ser considerado não inferior ao Controle. ",
          "Esta margem se aplica quando o novo Tratamento traz uma vantagem prática que vale a pena abdicar de uma pequena parte do benefício obtido com o Controle. ",
          "<br><br>Quanto mais próximo de zero esta margem, maior o tamanho de amostra necessário. ",
          "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE ENSAIO CLÍNICO DE COMPARAÇÃO, ENSAIO CLÍNICO DE SUPERIORIDADE, DE EQUIVALÊNCIA E DE não iNFERIORIDADE?</i>\" de ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador()
        )
      ),


      txt_margem_superior = case_when(
        linguagem() == "en" ~ paste0(
          "A margem superioridade quantifica a diferença mínica clinicamente aceitável para que o grupo Tratamento possa ser considerado superior ao Controle. ",
          "<br><br>Quanto mais próximo de zero esta margem, maior o tamanho de amostra necessário. ",
          "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE ENSAIO CLÍNICO DE COMPARAÇÃO, ENSAIO CLÍNICO DE SUPERIORIDADE, DE EQUIVALÊNCIA E DE não iNFERIORIDADE?</i>\" de ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador()
        ),
        TRUE  ~ paste0(
          "A margem superioridade quantifica a diferença mínica clinicamente aceitável para que o grupo Tratamento possa ser considerado superior ao Controle. ",
          "<br><br>Quanto mais próximo de zero esta margem, maior o tamanho de amostra necessário. ",
          "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE ENSAIO CLÍNICO DE COMPARAÇÃO, ENSAIO CLÍNICO DE SUPERIORIDADE, DE EQUIVALÊNCIA E DE não iNFERIORIDADE?</i>\" de ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador()
        )
      ),

      txt_margem_equivalencia = case_when(
        linguagem() == "en" ~ paste0(
          "A margem equivalência quantifica a diferença clinicamente aceitável para que o grupo Tratamento possa ser considerado equivalente ao Controle. ",
          "<br><br>Quanto mais próximo de zero esta margem, maior o tamanho de amostra necessário. ",
          "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE ENSAIO CLÍNICO DE COMPARAÇÃO, ENSAIO CLÍNICO DE SUPERIORIDADE, DE EQUIVALÊNCIA E DE não iNFERIORIDADE?</i>\" de ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador()
        ),
        TRUE ~ paste0(
          "A margem equivalência quantifica a diferença clinicamente aceitável para que o grupo Tratamento possa ser considerado equivalente ao Controle. ",
          "<br><br>Quanto mais próximo de zero esta margem, maior o tamanho de amostra necessário. ",
          "<br><br>Maiores detalhes na seção \"<i>QUAL A DIFERENÇA ENTRE ENSAIO CLÍNICO DE COMPARAÇÃO, ENSAIO CLÍNICO DE SUPERIORIDADE, DE EQUIVALÊNCIA E DE não iNFERIORIDADE?</i>\" de ",
          '<a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Castro et al. 2019</a>.',
          txt_definido_pesquisador()
        )
      ),

      txt_deff = case_when(
        linguagem() == "en" ~ paste0(
          "O efeito do plano amostral (em inglês, <i>design effect</i> ou, abreviadamente, <i>deff</i>) é utilizado para medir o efeito de um plano amostral sobre a variância de um estimador. <br><br>",
          "Ele representa o quanto o plano amostral proposto é mais ou menos eficiente, em termos de variabilidade da estimativa, do que a amostragem aleatória simples. <br><br>",
          "Se seu valor for igual a 1, então o plano amostral proposto é a amostragem aleatória simples ou é considerado tão eficiente quanto esta. Valores maiores do que 1 indicam que o plano amostral proposto é menos eficiente do que a amostragem aleatória simples (geralmente a amostragem por conglomerados tem essa característica) e valores menores do que 1 indicam que o plano amostral proposto é mais eficiente do que a amostragem aleatória simples (geralmente a amostragem estratificada tem essa característica).",
          txt_definido_pesquisador_OU_literatura()
        ),
        TRUE ~ paste0(
          "O efeito do plano amostral (em inglês, <i>design effect</i> ou, abreviadamente, <i>deff</i>) é utilizado para medir o efeito de um plano amostral sobre a variância de um estimador. <br><br>",
          "Ele representa o quanto o plano amostral proposto é mais ou menos eficiente, em termos de variabilidade da estimativa, do que a amostragem aleatória simples. <br><br>",
          "Se seu valor for igual a 1, então o plano amostral proposto é a amostragem aleatória simples ou é considerado tão eficiente quanto esta. Valores maiores do que 1 indicam que o plano amostral proposto é menos eficiente do que a amostragem aleatória simples (geralmente a amostragem por conglomerados tem essa característica) e valores menores do que 1 indicam que o plano amostral proposto é mais eficiente do que a amostragem aleatória simples (geralmente a amostragem estratificada tem essa característica).",
          txt_definido_pesquisador_OU_literatura()
        )
      ),








      ## Well panel -----

      wellPanel_txt_uma_media = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> estimation or hypothesis testing relating to an average (called the true mean or parameter) of a quantitative variable in a population of interest. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: how to calculate a sample size to estimate means, proportions, and correlations</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> estimação ou teste de hipótese relativos a uma média (chamada de verdadeira média ou parâmetro) de uma variável quantitativa em uma população de interesse. ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: como calcular tamanho de amostra para estimar média, proporção e correlação</a></b>.')
        )
      ),


      wellPanel_txt_2_medias_independentes = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> compare the true means of a quantitative variable between two independent groups. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: how to calculate sample sizes for mean comparison tests between two groups</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> comparar as verdadeiras médias de uma variável quantitativa entre dois grupos independentes. ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: como calcular tamanho de amostra para testes de comparação de médias de dois grupos</a></b>.')
        )
      ),


      wellPanel_txt_2_medias_assimetricas = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> compare the true means of a quantitative variable, with asymmetric distribution, between two independent groups. "
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> comparar as verdadeiras médias de uma variável quantitativa, com distribuição assimétrica, entre dois grupos independentes. "
        )
      ),


      wellPanel_txt_equivalencia = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> to test whether a new treatment is better (superiority study) than the standard, set an upper limit; or test whether it is not inferior (non-inferiority study) to the standard, setting a lower limit; or test whether it is as effective (equivalence study) as the standard, setting a lower and an upper limit. ",
          "What is the difference between comparison, superiority, equivalence and non-inferiority tests? Read the article: ",
          '<b><a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Biostatistics and epidemiology: questions you always wanted to ask but never had the courage to</a></b>'
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> testar se um novo tratamento é melhor (estudo de superioridade) do que o padrão, fixado um limite superior; ou testar se ele não é inferior (estudo de não inferioridade) ao padrão, fixado um limite inferior; ou testar se ele é tão eficaz (estudo de equivalência) quanto o padrão, fixados um limite inferior e um superior. ",
          "Qual a diferença entre teste de comparação, de superioridade, de equivalência e de não inferioridade? Leia o artigo: ",
          '<b><a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Bioestatística e epidemiologia: perguntas que você sempre quis fazer, mas nunca teve coragem</a></b>'
        )
      ),


      wellPanel_txt_2_medias_dependentes = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> test (or estimate) the true mean difference of a quantitative variable in two dependent (matched or related) groups. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: how to calculate sample sizes for mean comparison tests between two groups</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> testar (ou estimar) a verdadeira diferença média de uma variável quantitativa em dois grupos dependentes (pareados ou relacionados). ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: como calcular tamanho de amostra para testes de comparação de médias de dois grupos</a></b>.')
        )
      ),


      wellPanel_txt_2_deltas = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> compare the true mean change (change) of a quantitative variable (over a time interval) between two independent groups. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: how to calculate sample sizes for mean comparison tests between two groups</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> comparar a verdadeira variação (mudança) média de uma variável quantitativa (em um intervalo de tempo) entre dois grupos independentes. ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: como calcular tamanho de amostra para testes de comparação de médias de dois grupos</a></b>.')
        )
      ),

      wellPanel_txt_medidas_repetidas = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> compare the true mean of this variable between two independent groups at the last of the measurement times. In this case, the 'time' variable is treated as categorical. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: how to calculate sample sizes for mean comparison tests between two groups</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> comparar a verdadeira média desta variável entre dois grupos independentes no último dos momentos de medição. Neste caso, a variável 'momento' é tratada como categórica. ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: como calcular tamanho de amostra para testes de comparação de médias de dois grupos</a></b>.')
        )
      ),

      wellPanel_txt_anova1 = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> compare the true mean of a given quantitative variable between two or more independent groups."
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> comparar a verdadeira média de uma determinada variável quantitativa entre dois ou mais grupos independentes."
        )
      ),


      wellPanel_txt_anova2 = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> comparar a verdadeira média de uma determinada variável quantitativa entre dois ou mais grupos independentes, os quais se originam do cruzamento dos níveis dos dois fatores utilizados."
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> comparar a verdadeira média de uma determinada variável quantitativa entre dois ou mais grupos independentes, os quais se originam do cruzamento dos níveis dos dois fatores utilizados."
        )
      ),


      wellPanel_txt_1_prop = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> estimation or hypothesis testing concerning the proportion of occurrence of one of the categories (called the true proportion or parameter) of a categorical variable in a population of interest. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: how to calculate a sample size to estimate means, proportions, and correlations</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> estimação ou teste de hipótese relativos a proporção de ocorrência de uma das categorias (chamada de verdadeira proporção ou parâmetro) de uma variável categórica em uma população de interesse. ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: como calcular tamanho de amostra para estimar média, proporção e correlação</a></b>.')
        )
      ),


      wellPanel_txt_2_prop_independentes = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> compare the true proportions of occurrence of one of the categories of a categorical variable between two independent groups. The test and power calculations are performed using the normal distribution approximation, so be careful when using the results for small samples. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.126843" target="_blank">PSS Health: how to calculate the sample size to test variables relationships with a binary outcome</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> comparar as verdadeiras proporções de ocorrência de uma das categorias de uma variável categórica entre dois grupos independentes. Os cálculos do teste e do poder são realizados utilizando a aproximação pela distribuição normal, por isso tenha cautela no uso dos resultados para amostras muito pequenas. ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.126843" target="_blank">PSS Health: como calcular tamanho de amostra para testar relações de variáveis com um desfecho binário</a></b>.')
        )
      ),


      wellPanel_txt_qui_quadrado = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> test the existence of an association between two qualitative variables, usually grouped in contingency tables. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.126843" target="_blank">PSS Health: how to calculate the sample size to test variables relationships with a binary outcome</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> testar a existência de associação entre duas variáveis qualitativas, geralmente agrupadas em tabelas de contingência. ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.126843" target="_blank">PSS Health: como calcular tamanho de amostra para testar relações de variáveis com um desfecho binário</a></b>.')
        )
      ),


      wellPanel_txt_correlacao = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> check if two variables vary together. In these cases, the statistic of interest is the correlation coefficient. Correlation coefficients evaluate the direction and degree of alignment between two variables. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: how to calculate a sample size to estimate means, proportions, and correlations</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> verificar se duas variáveis variam conjuntamente. Nestes casos, a estatística de interesse é o coeficiente de correlação. Os coeficientes de correlação avaliam a direção e o grau de alinhamento entre duas variáveis. ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: como calcular tamanho de amostra para estimar média, proporção e correlação</a></b>.')
        )
      ),


      wellPanel_txt_reg_linear = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> observing the joint variation of two variables, assuming a linear relationship, the researcher may be interested in estimating the slope coefficient of the relationship between them."
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> observar a variação conjunta de duas variáveis, supondo uma relação linear, o pesquisador pode estar interessado em estimar o coeficiente de inclinação da relação entre elas."
        )
      ),


      wellPanel_txt_reg_logistica = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> test or estimate the association between a binary outcome and a predictor variable through the odds ratio (OR) in a simple (univariate) logistic regression model. ",
          'More details on using this tab in ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.126843" target="_blank">PSS Health: how to calculate the sample size to test variables relationships with a binary outcome</a></b>.')
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> testar ou estimar a associação entre um desfecho binário em uma variável preditora através da razão de chances (RC) em um modelo de regressão logística simples (univariável). ",
          'Mais detalhes sobre o uso dessa aba em ',
          HTML('<b><a href="https://doi.org/10.22491/2357-9730.126843" target="_blank">PSS Health: como calcular tamanho de amostra para testar relações de variáveis com um desfecho binário</a></b>.')
        )
      ),



      wellPanel_txt_reg_cox = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> assess risk/protective factors for survival using the Cox proportional hazards model for clinical trials."
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> avaliar fatores de risco/proteção para a sobrevivência usando o modelo de riscos proporcionais de Cox para ensaios clínicos."
        )
      ),


      wellPanel_txt_auc = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> test (or estimate) the classification performance of a test through the area under the ROC curve (AUC). AUC is a measure that ranges from 0 (all ratings are incorrect) to 1 (all ratings are correct)."
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> testar (ou estimar) a performance de classificação de um teste através da área sob a curva ROC (AUC). A AUC é uma medida que varia de 0 (todas as classificações estão incorretas) a 1 (todas as classificações estão corretas)."
        )
      ),


      wellPanel_txt_sensibilidade = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> estimate the sensitivity and specificity of a test. Sensitivity and specificity are measures that assess the classification performance of a test."
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> estimar a sensibilidade e a especificidade de um teste. A sensibilidade e a especificidade são medidas que avaliam a performance de classificação de um teste. "
        )
      ),



      wellPanel_txt_kappa = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> test (or estimate) the degree of agreement between raters (or judges, or evaluation methods, etc.) regarding a qualitative variable, using Cohen's Kappa coefficient. The closer to the value 1 (one) this coefficient is, the greater the degree of agreement and the closer to 0 (zero) the greater the disagreement between the raters. "
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> testar (ou estimar) o grau de concordância entre avaliadores (ou juízes, ou métodos de avaliação, etc.) quanto a uma variável qualitativa, através do coeficiente Kappa de Cohen. Quanto mais próximo do valor 1 (um) estiver este coeficiente, maior o grau de concordância e quanto mais próximo de 0 (zero) maior a discordância entre os avaliadores. "
        )
      ),


      wellPanel_txt_icc = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> test (or estimate) the degree of agreement between raters (or judges, or evaluation methods, etc.) regarding a quantitative variable, through the Intraclass Correlation Coefficient (ICC). The closer to the value 1 (one) this coefficient is, the greater the degree of agreement and the closer to 0 (zero) the greater the disagreement between the raters. "
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> testar (ou estimar) o grau de concordância entre avaliadores (ou juízes, ou métodos de avaliação, etc.) quanto a uma variável quantitativa, através do Coeficiente de Correlação Intraclasse (ICC). Quanto mais próximo do valor 1 (um) estiver este coeficiente, maior o grau de concordância e quanto mais próximo de 0 (zero) maior a discordância entre os avaliadores. "
        )
      ),


      wellPanel_txt_bland = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> estimate the limits of agreement of the Bland-Altman plot. "
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> estimar os limites de concordância do gráfico de Bland-Altman. "
        )
      ),


      wellPanel_txt_cronbach = case_when(
        linguagem() == "en" ~ paste0(
          "<b>Objective of the study:</b> estimate the internal consistency (or degree of reliability) of a measurement instrument (composed of items) through Cronbach's Alpha. Cronbach's Alpha values close to 1 indicate high reliability. "
        ),
        TRUE ~ paste0(
          "<b>Objetivo do estudo:</b> estimar a consistência interna (ou grau de confiabilidade) de um instrumento de medida (composto por itens) através do Alfa de Cronbach. Valores do Alfa de Cronbach próximos de 1 indicam alta confiabilidade. "
        )
      ),


      wellPanel_txt_matriz_correlacao = case_when(
        linguagem() == "en" ~ paste0(
          "Indique a correlação esperada entre as medidas nos diferentes momentos. As opções disponíveis são: <br><br>",
          "<ul>", # inicio da lista
          "<li><b>Componente permutável:</b> a correlação entre as medidas é sempre a mesma em todos os momentos);</li><br>",
          "<li><b>AR(1) - Auto-Regressiva de 1\u00AA Ordem:</b> a correlação entre medidas de momentos adjacentes é mais forte e vai enfraquecendo de acordo com o aumento da distância entre as mesmas;</li><br>",
          "<li><b>Não estruturada:</b> as correlações entre as medidas de diferentes momentos podem assumir qualquer valor.</li>",
          "</ul>", # fim da
          txt_definido_pesquisador_OU_literatura()
        ),
        TRUE ~ paste0(
          "Indique a correlação esperada entre as medidas nos diferentes momentos. As opções disponíveis são: <br><br>",
          "<ul>", # inicio da lista
          "<li><b>Componente permutável:</b> a correlação entre as medidas é sempre a mesma em todos os momentos);</li><br>",
          "<li><b>AR(1) - Auto-Regressiva de 1\u00AA Ordem:</b> a correlação entre medidas de momentos adjacentes é mais forte e vai enfraquecendo de acordo com o aumento da distância entre as mesmas;</li><br>",
          "<li><b>Não estruturada:</b> as correlações entre as medidas de diferentes momentos podem assumir qualquer valor.</li>",
          "</ul>", # fim da
          txt_definido_pesquisador_OU_literatura()
        )
      ),


      wellPanel_obter_dp = case_when(
        linguagem() == "en" ~ paste0(
          "In many sample size calculations, it is necessary to inform the standard deviation of the variable of interest, however only other statistics are found in the literature, such as standard error, confidence interval, t or p value."
        ),
        TRUE ~ paste0(
          "Em muitos cálculos de tamanho amostral é necessário informar o desvio padrão da variável de interesse, no entanto é encontrado na literatura somente outras estatísticas, como o erro padrão, intervalo de confiança, valor de t ou p."
        )
      ),


      wellPanel_dp_combinado = case_when(
        linguagem() == "en" ~ paste0(
          "We use the combined standard deviation when we have variance information from two independent groups and want to aggregate them to have a single standard deviation"
        ),
        TRUE ~ paste0(
          "Usamos o desvio padrão combinado quando temos a informação da variância de dois grupos independentes e queremos agrega-las para ter um único desvio padrão"
        )
      )


    )
  })


  txt_balanceamento_f <- function(a, b) {

    case_when(
      linguagem() == "en" ~ paste0(
        "It is a ratio between sizes of two groups:<br><br>",
        "<ul>", # inicio da lista
        "<li><b>Ratio equal to 1:</b> will be calculating the sample size such that for each <i>", a, "</i> we will have another <i>", b, "</i> (<b>1:1</b>);</li><br>",
        "<li><b>Ratio equal to 2:</b> the sample size will be calculated in such a way that we will have two <i>", a, "</i> for each <i>", b, "</i> (<b>2:1</b>);</li><br>",
        "<li><b>Ratio equal to 0.5:</b> the sample size will be calculated in such a way that for each <i>", a, "</i> we will have two in the <i>", b, "</i> (<b>1:2</b>).</li>",
        "</ul>", # fim da
        txt_definido_pesquisador()
      ),
      TRUE ~ paste0(
        "O balanceamento é uma razão entre tamanhos de dois grupos:<br><br>",
        "<ul>", # inicio da lista
        "<li><b>Balanceamento igual a 1:</b> será calculando o tamanho de amostra tal que para cada indivíduo do <i>", a, "</i> teremos outro indivíduo do <i>", b, "</i> (<b>1:1</b>);</li><br>",
        "<li><b>Balanceamento igual a 2:</b> será calculando o tamanho de amostra tal que teremos dois indivíduos do <i>", a, "</i> para cada indivíduo do <i>", b, "</i> (<b>2:1</b>);</li><br>",
        "<li><b>Balanceamento igual a 0.5:</b> será calculando o tamanho de amostra tal que para cada indivíduo do <i>", a, "</i> teremos dois indivíduos do <i>", b, "</i> (<b>1:2</b>).</li>",
        "</ul>", # fim da
        txt_definido_pesquisador()
      )
    )

  }


  txt_outros_desfechos <- function(frase1 = "") {
    paste0(
      frase1,
      "Preencha este campo de acordo com seu estudo para que sirva de guia no preenchimento dos demais campos."
    )
  }



  # Servira para a montagem das hipoteses
  h1 <- reactive({
    teste <- c("Unilateral", "Bilateral")

    if (linguagem() == "pt") {
      names(teste) <- c("Unilateral", "Bilateral")
    } else {
      names(teste) <- c("One-sided", "Two-sided")
    }

    teste
  })





  #----------------------------.
  #      FUNCOES INTERNAS
  #----------------------------.

  print_r_code <- function(code) {
    if (linguagem() == "pt") {
      paste0("</br></br>",
             "<i>Comando R utilizado:</i><br>",
             "<p style=\"font-family:'Courier New';font-size:100% \">", code(code), "</p>",
             "<br><br><b><i>* Sempre procure um profissional de estatística para orientações no planejamento do estudo.</b></i>"
      )
    } else {
      paste0("</br></br>",
             "<i>R code:</i><br>",
             "<p style=\"font-family:'Courier New';font-size:100% \">", code(code), "</p>",
             "<br><br><b><i>* Always consult a statistician for guidance in study design.</b></i>"
      )
    }

  }


  ajuda_cenarios_multiplos_valores <- reactive({
    if (linguagem() == "pt") {
      "Esses valores serão utilizados para criar diferentes linhas no gráfico. Separe os valores por vírgula ',' e utilize ponto '.' como decimal."
    } else {
      "These values will be used to create different lines on the chart. Separate the values with a comma ',' and use a period '.' as decimal."
    }
  })

  ajuda_cenarios_multiplos_valores2 <- reactive({
    if (linguagem() == "pt") {
      "Separe os valores por vírgula ',' e utilize ponto '.' como decimal."
    } else {
      "Separate the values with a comma ',' and use a period '.' as decimal."
    }
  })


  validate_n <- function(n) {
    if (linguagem() == "pt") {
      paste0(
        "validate(need(!is.na(", n,
        "), 'Não foi possível calcular sua solicitação. Verifique os valores no painel lateral. Se o erro persistir, por favor, envie um e-mail para psshealth@hcpa.edu.br.'))"
      )
    } else {
      paste0(
        "validate(need(!is.na(", n,
        "), 'Your request could not be calculated. Check the values in the side panel. If the error persists, please email us at psshealth@hcpa.edu.br.'))"
      )
    }
  }




  validate_n_inf <- function(n) {

    if (linguagem() == "pt") {
      paste0(
        "validate(need(", n,
        " != Inf, 'Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para psshealth@hcpa.edu.br.'))"
      )
    } else {
      paste0(
        "validate(need(", n,
        " != Inf, 'Your request could not be calculated. Check the values in the side panel. If the error persists, please email us at psshealth@hcpa.edu.br.'))"
      )
    }
  }


  erro_painel_principal <- reactive({
    if (linguagem() == "pt") {
      'Não foi possível calcular sua solicitação. Verifique as entradas no painel lateral. Se o erro persistir, por favor, envie um e-mail para psshealth@hcpa.edu.br.'
    } else {
      'Your request could not be calculated. Check the values in the side panel. If the error persists, please email us at psshealth@hcpa.edu.br.'
    }
  })



  try_n <- function(code) {
    tryCatch({
      eval(parse(text = code))
    }, warning = function(warning_condition) {
      NA
    }, error = function(error_condition) {
      NA
    }
    )
  }





  warning_prop <- function(id, entre0e1 = FALSE) {

    if (!entre0e1) {
      paste0(
        'observeEvent(input$', id,', {
      shinyFeedback::hideFeedback("', id, '")
      if (is.na(input$', id,')) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser fornecido um valor.", linguagem()),
          color = "red"
        )
      } else if (input$', id,' >= 100) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser menor do que 100%.", linguagem()),
          color = "red"
        )
      } else if (input$', id,' <= 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser maior do que 0%.", linguagem()),
          color = "red"
        )
      }
    })
    '
      )
    } else{
      paste0(
        'observeEvent(input$', id,', {
      shinyFeedback::hideFeedback("', id, '")
      if (is.na(input$', id,')) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser fornecido um valor.", linguagem()),
          color = "red"
        )
      } else if (input$', id,' > 1) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser menor do que 1.", linguagem()),
          color = "red"
        )
      } else if (input$', id,' < 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser maior do que 0.", linguagem()),
          color = "red"
        )
      }
    })
    '
      )
    }
  }






  warning_numero_positivo <- function(id) {

    # if (!is.null(input[id]))

    paste0(
      'observeEvent(input$', id,', {
      shinyFeedback::hideFeedback("', id, '")
      if (is.na(input$', id,')) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser fornecido um valor.", linguagem()),
          color = "red"
        )
      } else if (input$', id,' <= 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser maior do que 0.", linguagem()),
          color = "red"
        )
      }
    })
    '
    )
  }


  warning_inteiro <- function(id) {

    paste0(
      'observeEvent(input$', id,', {
    shinyFeedback::hideFeedback("', id,'")

    if (is.na(input$', id,')) {
      shinyFeedback::showFeedbackWarning(
        inputId = "', id,'",
        text = translation_pss("Deve ser fornecido um valor.", linguagem()),
        color = "red"
      )
    } else if (input$', id,'%%1 != 0 | input$', id,' < 1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "', id,'",
        text = translation_pss("Deve ser um número inteiro positivo.", linguagem()),
        color = "red"
      )
    }
  })'
    )
  }



  warning_perdas <- function(id) {
    paste0(
      'observeEvent(input$', id,', {
      shinyFeedback::hideFeedback("', id, '")
      if (is.na(input$', id,')) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Tem certeza que não considerarás perdas?", linguagem())
        )
      } else if (input$', id,' >= 100) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser menor do que 100%.", linguagem())
        )
      } else if (input$', id,' <= 0) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Tem certeza que não considerarás perdas?", linguagem())
        )
      }
    })
    '
    )
  }





  warning_numero <- function(id) {
    paste0(
      'observeEvent(input$', id,', {
      shinyFeedback::hideFeedback("', id, '")
      if (is.na(input$', id,')) {
        shinyFeedback::showFeedbackWarning(
          inputId = "', id, '",
          text = translation_pss("Deve ser fornecido um valor.", linguagem()),
          color = "red"
        )
      } else {
      shinyFeedback::hideFeedback("', id, '")
    }
    })
    '
    )
  }


  warning_correlacao <- function(id) {
    paste0(
      'observeEvent(input$', id,', {
    shinyFeedback::hideFeedback("', id,'")
    if (is.na(input$', id,')) {
      shinyFeedback::showFeedbackWarning(
        inputId = "', id,'",
        text = translation_pss("Deve ser fornecido um valor.", linguagem()),
        color = "red"
      )
    } else if (input$', id,' >= 1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "', id,'",
        text = translation_pss("Deve ser menor do que 1.", linguagem()),
        color = "red"
      )
    } else if (input$', id,' <= -1) {
      shinyFeedback::showFeedbackWarning(
        inputId = "', id,'",
        text = translation_pss("Deve ser maior do que -1.", linguagem()),
        color = "red"
      )
    }
  })
    '
    )
  }






  testar_valor_perdas_valido <- function(x) {
    if (!is.na(x)) {
      x > 0 & x < 100
    } else{
      FALSE
    }
  }









  #---------------------------------------.
  #  Calcula n para perdas e recusas
  #---------------------------------------.

  n_perdas <- function(n, perdas) {
    ceiling(n/(1 - perdas/100))
  }



  #---------------------------------------------------------.
  # Tamanho de efeito para comparar dus medias (d de Cohen)
  #---------------------------------------------------------.

  cohen_d <- function(mean_diff, n_1, n_2, sd_1, sd_2) {
    pooled_sd_n <- ((n_1-1)*(sd_1**2)) + ((n_2-1)*(sd_2**2))
    pooled_sd_d <- ((n_1 + n_2) - 2)
    pooled_sd <- sqrt(pooled_sd_n/pooled_sd_d)
    mean_diff / pooled_sd
  }














  text_input_to_vector <- function(input_text) {

    input_text %>%
      strsplit(",") %>%
      unlist() %>%
      strsplit("\\n") %>%
      unlist() %>%
      as.numeric() %>%
      na.omit()
  }


  check_text_input_to_vector <- function(id) {

    paste0("observeEvent(input$", id, ", {

    vetor_teste <- text_input_to_vector(input$", id, ")

    if (length(vetor_teste) == 0) {
      shinyFeedback::showFeedbackWarning(
        inputId = '", id, "',
        text = translation_pss('Entrada inválida.', linguagem()),
        color = 'red')
    } else {
      shinyFeedback::hideFeedback('", id, "')
    }
  })
  "
    )
  }








  #...........................-----
  #  1  media  (ok) ----




  mod_1_media_server(
    "tamanho_amostral_1_media",
    tipo = "tamanho_amostral",
    txt_ajuda = txt_ajuda,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  mod_1_media_server(
    "poder_1_media",
    tipo = "poder",
    txt_ajuda = txt_ajuda,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  mod_1_media_server(
    "estimar_1_media",
    tipo = "estimar",
    txt_ajuda = txt_ajuda,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  output$aba_1_media <- renderUI({

    tagList(

      titlePanel(translation_pss("Uma média", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_uma_media)),
      tabsetPanel(
        tabPanel(translation_pss("Estimar", linguagem()),
                 mod_1_media_Ui("estimar_1_media"),
                 .rodape()
        ),

        tabPanel(translation_pss("Testar", linguagem()),
                 mod_1_media_Ui("tamanho_amostral_1_media"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_1_media_Ui("poder_1_media"),
                 .rodape()
        )
      )
    )
  })







  #_____________----
  #  1 proporção  ----

  output$aba_estimacao_uma_prop <- renderUI({

    tagList(

      titlePanel("Uma proporção"),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_1_prop)),

      tabsetPanel(
        tabPanel(translation_pss("Estimar", linguagem()),
                 sidebarLayout(
                   sidebarPanel(
                     wellPanel(
                       HTML(
                         '<b><a href="https://youtu.be/FRMtGkW7byY" target="_blank">',
                         translation_pss("Vídeo: PSS Health para estimar uma proporção", linguagem()),
                         '</a></b><br>'
                       )
                     ),

                     textInput(inputId = "prop_nome_desfecho",
                               label   = translation_pss("Descreva o nome do desfecho", linguagem()),
                               value   = "Y") %>% .help_buttom(body = txt_ajuda()$txt_desfecho),

                     numericInput( "e_prop",
                                   translation_pss(translation_pss("Amplitude do intervalo (%)", linguagem()), linguagem()),
                                   value = 10,
                                   min = 0,
                                   max = 100,
                                   step = 5
                     ) %>% .help_buttom(body = txt_ajuda()$txt_amplitude, title = translation_pss(translation_pss("Amplitude do intervalo", linguagem()), linguagem())),
                     numericInput( "p_prop",
                                   translation_pss("Percentual esperado (%)", linguagem()),
                                   value = 50,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(),
                       body = paste0(txt_ajuda()$txt_perc_esperado, txt_ajuda()$txt_definido_literatura),
                       title = translation_pss("Percentual esperado (%)", linguagem())
                     ),
                     selectInput("p1_metodo",
                                 translation_pss("Método utilizado para calcular o intervalo de confiança", linguagem()),
                                 choices = c("Wilson" = "wilson",
                                             "Agresti-Coull" = "agresti-coull",
                                             "Exact" = "exact",
                                             "Wald" = "wald"),
                                 selected = "wald"
                     ) %>% .help_buttom(body = txt_ajuda()$txt_per_method_presize),
                     numericInput( "conf_prop",
                                   translation_pss("Nível de confiança (%)", linguagem()),
                                   value = 95,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem())),
                     numericInput( "prop_perdas_recusa",
                                   translation_pss("Perdas/ Recusas (%)", linguagem()),
                                   value = 10,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem())),
                   ),

                   mainPanel(
                     shinycssloaders::withSpinner(htmlOutput("prop"), type = 5),

                     ###  CENARIOS  ####.
                     uiOutput("prop_precisao_cenarioUi")
                   )
                 )
        ),


        tabPanel(
          translation_pss("Testar", linguagem()),
          sidebarLayout(
            sidebarPanel(
              wellPanel(
                HTML(
                  paste0(
                    "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                  )
                ),
                uiOutput("th_prop_formula1")
              ),

              textInput(inputId = "prop_nome_desfecho_testar",
                        label   = translation_pss("Descreva o nome do desfecho", linguagem()),
                        value   = "Y") %>% .help_buttom(body = txt_ajuda()$txt_desfecho),

              numericInput( "p_TH_observado",
                            translation_pss("Percentual esperado (%)", linguagem()),
                            value = 30,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(linguagem = linguagem(),
                body = txt_ajuda()$txt_perc_esperado, title = "Percentual esperado"),
              numericInput( "p_TH_h0",
                            translation_pss("Valor de referência sob a hipótese nula", linguagem()),
                            value = 10,
                            min = 0,
                            max = 100,
                            step = 1
              ),
              numericInput( "alpha_TH_prop",
                            translation_pss("Nível de significância (%)", linguagem()),
                            value = 5,
                            min = 0,
                            max = 100,
                            step = 1
              ),
              numericInput( "beta_TH_prop",
                            translation_pss("Poder (%)", linguagem()),
                            value = 80,
                            min = 0,
                            max = 100,
                            step = 1
              ),

              numericInput( "prop_1th_perdas_recusa",
                            translation_pss("Perdas/ Recusas (%)", linguagem()),
                            value = 10,
                            min = 0,
                            max = 100,
                            step = 1
              ) %>% .help_buttom(body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem())),


              checkboxInput("prop_1th_approx", translation_pss("Calcular utilizando a aproximação pela normal", linguagem()), value = FALSE
              ) %>% .help_buttom(linguagem = linguagem(),
                body = paste0(translation_pss("Calcular utilizando a aproximação pela normal? Se esta opção estiver desmarcada será utilizado o método exato.", linguagem()),
                              txt_ajuda()$txt_definido_pesquisador)
              ),

              conditionalPanel("input.prop_1th_approx == true",
                               checkboxInput("prop_1th_correction", translation_pss("Aplicar correção de continuidade", linguagem()), value = TRUE
                               ) %>% .help_buttom(body = paste0("Clique aqui para calcular um tamanho de amostra para um teste com correção de continuidade", txt_ajuda()$txt_definido_pesquisador))
              )

            ),
            mainPanel(
              shinycssloaders::withSpinner(htmlOutput("TH1prop"), type = 5)
            )
          )
        ),


        tabPanel(translation_pss("Poder", linguagem()),
                 sidebarLayout(
                   sidebarPanel(

                     wellPanel(
                       HTML(
                         paste0(
                           "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                         )
                       ),
                       uiOutput("p_power_th")),

                     textInput(inputId = "prop_nome_desfecho_poder",
                               label   = translation_pss("Descreva o nome do desfecho", linguagem()),
                               value   = "Y") %>% .help_buttom(body = txt_ajuda()$txt_desfecho),

                     numericInput( "p_TH_observado",
                                   translation_pss("Percentual esperado (%)", linguagem()),
                                   value = 30,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(body = txt_ajuda()$txt_perc_esperado, title = translation_pss("Poder", linguagem())),
                     numericInput( "p_power_h0",
                                   translation_pss("Valor de referência sob a hipótese nula", linguagem()),
                                   value = 20,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ),
                     numericInput( "p_power_n",
                                   translation_pss("Tamanho amostral", linguagem()),
                                   value = 150,
                                   min = 0,
                                   max = Inf,
                                   step = 1
                     ),
                     numericInput( "p_power_alpha",
                                   translation_pss("Nível de significância (%)", linguagem()),
                                   value = 5,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ),

                     checkboxInput(
                       "p_power_approx",
                       translation_pss("Calcular utilizando a aproximação pela normal" , linguagem()),
                       value = FALSE
                     ) %>% .help_buttom(linguagem = linguagem(),
                       body = paste0(
                         translation_pss("Calcular utilizando a aproximação pela normal? Se esta opção estiver desmarcada será utilizado o método exato.", linguagem()),
                         txt_ajuda()$txt_definido_pesquisador
                       )
                     ),

                     conditionalPanel("input.p_power_approx == true",
                                      checkboxInput(
                                        "p_power_correction",
                                        translation_pss("Aplicar correção de continuidade", linguagem()),
                                        value = TRUE
                                      ) %>% .help_buttom(linguagem = linguagem(),
                                        body = paste0(
                                          "Clique aqui para calcular um tamanho de amostra para um teste com correção de continuidade",
                                          txt_ajuda()$txt_definido_pesquisador
                                        )
                                      )
                     )

                   ),
                   mainPanel(
                     shinycssloaders::withSpinner(htmlOutput("p_power_output"), type = 5)
                   )
                 )
        )
      ),


      .rodape()
    )
  })




  # Estimar ----

  eval(parse(text = warning_prop("e_prop")))
  eval(parse(text = warning_prop("p_prop")))
  eval(parse(text = warning_prop("conf_prop")))
  eval(parse(text = warning_numero_positivo("N_pop_prop")))
  eval(parse(text = warning_perdas("prop_perdas_recusa")))



  output$prop <- renderText({

    code <- paste0(
      "presize::prec_prop(",
      "p = ", input$p_prop, "/100, ",
      "conf.width = ", input$e_prop, "/100,",
      "conf.level = ", input$conf_prop, "/100, ",
      "method = '", input$p1_metodo, "')"
    )


    n <- try_n(code)
    eval(parse(text = validate_n("n")))
    n <- ceiling(n$n)
    eval(parse(text = validate_n_inf("n")))

    metodo <- input$p1_metodo
    metodo <- paste(toupper(substr(metodo, 1, 1)), substr(metodo, 2, nchar(metodo)), sep = "")


    paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
           "</font></b></br></br>",

           lista_de_funcoes_server()$sugestao_texto_portugues(
             "<i>",
             translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


             "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para estimar a proporção de ocorrência do desfecho <i>", input$prop_nome_desfecho,
             "</i> com <b>", input$e_prop, "%</b> de amplitude para o intervalo de confiança (com o acréscimo de ", input$prop_perdas_recusa, "% para possíveis perdas e recusas este número deve ser ", n_perdas(n, input$prop_perdas_recusa), "). ",
             "O cálculo (utilizando o ",
             if (input$p1_metodo %in% c("wilson", "agresti-coull", "wald")) {
               paste0(" método de ", metodo)
             } else {
               " método exato "
             },
             ") considerou nível de confiança de <b>", input$conf_prop, "%</b> e <b>",
             input$p_prop, "%</b> de percentual esperado para <i>", input$prop_nome_desfecho, "</i> (referido por Fulano (1900)).  ",
             .txt_citacao_pss
           ),
           .txt_referencia_tap, print_r_code(code)
    )

    # }
  })


  ## Cenarios ----

  output$prop_precisao_cenarioUi <- renderUI({
    fluidPage(fluidRow(
      br(),
      HTML('<hr style="color: black;">'),
      br(),br(),

      titlePanel(translation_pss("Construção de cenários", linguagem())),
      br(),


      if (linguagem() == "pt") {
        tagList(
          wellPanel(
            translation_pss(
              "Utilize os argumentos abaixo para construir diferentes cenários. Demais informações serão recuperadas do painel lateral.",
              linguagem())
          ),
          HTML("<b>Defina a sequência de valores para o percentual esperado (%):</b>")
        )
      } else {
        tagList(
          wellPanel(
            "Use the arguments below to build different scenarios. You can specify a sequence of values for the expected percentage and desired precision values. Further information will be retrieved from the side panel."
          ),
          HTML("<b>Set the sequence of values to the expected percentage (%):</b>")
        )
      },


      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("prop_from", translation_pss("Mínimo", linguagem()), value = 0, step = 5, min = 0, max = 100)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("prop_to", translation_pss("Máximo", linguagem()), value = 100, step = 5, min = 0, max = 100)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("prop_by", translation_pss("Intervalo", linguagem()), value = 5, min = 0, step = 1, max = 99) %>%
            .help_buttom(body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()),
                         title = "Sequência da Percentual esperado")
      ),

      fluidRow(
        column(6,
               textInput(inputId = "prop_precisoes_plot",
                         label   = translation_pss("Digite valores de amplitude (%) para fazer o gráfico", linguagem()),
                         value   = paste0(c(input$e_prop, input$e_prop + 1, input$e_prop + 2), collapse = ", "),
                         width   = "600px") %>%
                 .help_buttom(body = ajuda_cenarios_multiplos_valores())
        )
      ),

      shinycssloaders::withSpinner(plotly::plotlyOutput("prop_plot", width = "80%"), type = 5),
      br(), br(),
      downloadButton("download_prop_tab", translation_pss("Download tabela", linguagem())),
      DT::dataTableOutput("prop_tab", width = "100%")

    ))
  })


  eval(parse(text = check_text_input_to_vector("prop_precisoes_plot")))


  tab_prop_cenarios <- reactive({

    precisoes <- text_input_to_vector(input$prop_precisoes_plot)
    req(length(precisoes) > 0)


    expand.grid(`Amplitude (%)` = precisoes,
                `Percentual esperado (%)` = seq(from = input$prop_from, to = input$prop_to, by = input$prop_by),
                `Nível de confiança (%)` = input$conf_prop,
                `Método` = input$p1_metodo,
                stringsAsFactors = FALSE) %>%
      mutate(n = mapply(
        function(e, P, level, method) { presize::prec_prop(p = P, conf.width = e, conf.level = level, method = method)$n },
        `Amplitude (%)`/100, `Percentual esperado (%)`/100, `Nível de confiança (%)`/100, `Método`),
        `Tamanho da amostra`   = ceiling(n)
      ) %>%
      dplyr::filter(n > 0)
  })



  output$prop_plot <- plotly::renderPlotly({

    g1 <- tab_prop_cenarios() %>%
      mutate(`Amplitude (%)` = factor(`Amplitude (%)`)) %>%
      ggplot(aes(x = `Percentual esperado (%)`, y = `Tamanho da amostra`, color = `Amplitude (%)`)) +
      geom_point() +
      geom_line() +
      xlab(translation_pss("Percentual esperado (%)", linguagem())) +
      ylab(translation_pss("Tamanho da amostra*", linguagem())) +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(palette = "Set1")

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))
  })


  tab_prop_cenarios_print <- reactive({
    df <- tab_prop_cenarios() %>%
      dplyr::select(`Percentual esperado (%)`,
                    `Nível de confiança (%)`,
                    `Amplitude (%)`,
                    `Método`,
                    `Tamanho da amostra`)

    colnames(df) <- c(
      translation_pss("Percentual esperado (%)", linguagem()),
      translation_pss("Nível de confiança (%)", linguagem()),
      translation_pss("Amplitude do intervalo (%)", linguagem()),
      translation_pss("Método utilizado para calcular o intervalo de confiança", linguagem()),
      translation_pss("Tamanho da amostra", linguagem())
    )

    df
  })


  output$prop_tab <- DT::renderDataTable({
    tab_prop_cenarios_print() %>%
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


  output$download_prop_tab <- downloadHandler(
    filename = function() { "PSS_Health_Cenarios_tamanho_amostra_uma_proporcao.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_prop_cenarios_print(), path = file)}
  )










  # Testar  ------

  eval(parse(text = warning_prop("p_TH_h0")))
  eval(parse(text = warning_prop("p_TH_observado")))
  eval(parse(text = warning_prop("alpha_TH_prop")))
  eval(parse(text = warning_prop("beta_TH_prop")))
  eval(parse(text = warning_perdas("prop_1th_perdas_recusa")))


  output$th_prop_formula1 <- renderUI({
    withMathJax(paste0("$$H_0: \\pi", "=", input$p_TH_h0,
                       "\\text{  vs  }",
                       "H_1: \\pi", "\\neq", input$p_TH_h0,
                       "$$"))
  })



  output$TH1prop <- renderText({

    code <- paste0(
      "EnvStats::propTestN(p.or.p1     = ", input$p_TH_h0, "/100, ",
      "p0.or.p2 = ", input$p_TH_observado, "/100, ",
      "alpha       = ", input$alpha_TH_prop,  "/100, ",
      "power       = ", input$beta_TH_prop, "/100, ",
      "sample.type = 'one.sample', ",
      # "alternative = '", input$alternative_TH2_prop_pwr2, "', ",
      "approx     = ", input$prop_1th_approx, ", ",
      if (input$prop_1th_approx) {
        paste0("correct = ",  input$prop_1th_correction, ", ")
      },
      "warn = FALSE)"
    )


    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    if (input$prop_1th_approx) {
      n <- ceiling(n)
    } else{
      n <- ceiling(n$n)
    }

    eval(parse(text = validate_n_inf("n")))


    paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
           "</font></b></br></br>",


           lista_de_funcoes_server()$sugestao_texto_portugues(
             "<i>",
             translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


             "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para testar se a proporção de ocorrência do desfecho <i>", input$prop_nome_desfecho_testar,
             "</i> é diferente de <b>", input$p_TH_h0, "%</b> (com o acréscimo de ", input$prop_1th_perdas_recusa, "% para possíveis perdas e recusas este número deve ser ", n_perdas(n, input$prop_1th_perdas_recusa), "). ",
             "O cálculo (utilizando o ",
             if (input$prop_1th_approx) {
               if (input$prop_1th_correction) {
                 " cálculo baseada na aproximação da normal com correção de continuidade"
               } else{
                 " cálculo baseada na aproximação da normal sem correção de continuidade"
               }
             } else{
               " método exato"
             },

             ") considerou poder de <b>", input$beta_TH_prop, "%</b>, nível de significância de <b>", input$alpha_TH_prop, "%</b> e ",
             "</b> percentual esperado para <i>", input$prop_nome_desfecho_testar, "</i> de <b>", input$p_TH_observado, "</b> (referido por Fulano (1900)). ",
             .txt_citacao_pss
           ),
           .txt_referencia_tap, print_r_code(code)
    )


  })





  # Poder ----

  eval(parse(text = warning_prop("p_power_h0")))
  eval(parse(text = warning_prop("p_TH_observado")))
  eval(parse(text = warning_prop("p_power_alpha")))
  eval(parse(text = warning_inteiro("p_power_n")))


  output$p_power_th <- renderUI({
    withMathJax(paste0("$$H_0: \\pi", "=", input$p_power_h0,
                       "\\text{  vs  }",
                       "H_1: \\pi", "\\neq", input$p_power_h0,
                       "$$"))
  })



  output$p_power_output <- renderText({

    code <- paste0(
      "EnvStats::propTestPower (",
      "n.or.n1 = ", input$p_power_n, ", ",
      "p.or.p1 = ", input$p_power_h0, "/100, ", #
      "p0.or.p2 = ", input$p_TH_observado, "/100, ",
      "alpha = ", input$p_power_alpha,  "/100, ",
      "sample.type = 'one.sample', ",
      "approx = ", input$p_power_approx, ", ",
      if (input$p_power_approx) {
        paste0("correct = ",  input$p_power_correction, ", ")
      },
      "warn = FALSE)"
    )


    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    if (input$p_power_approx) {
      n <- round(n*100, 1)
    } else{
      n <- round(n$power*100, 1)
    }

    eval(parse(text = validate_n_inf("n")))


    paste0("<b><font size = '5'>", translation_pss("Poder calculado", linguagem()), ": ", n,
           "%</font></b></br></br>",


           lista_de_funcoes_server()$sugestao_texto_portugues(
             "<i>",
             translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

             "O poder para testar se a proporção de ocorrência de <i>", input$prop_nome_desfecho_poder,
             "</i> é diferente de <b>", input$p_power_h0, "</b> é <b>", n, "%</b>.  ",
             "Este valor (utilizando o ",
             if (input$p_power_approx) {
               if (input$p_power_correction) {
                 "cálculo baseada na aproximação da normal com correção de continuidade"
               } else{
                 "cálculo baseada na aproximação da normal sem correção de continuidade"
               }
             } else{
               "método exato"
             },

             ") foi obtido considerando o nível de significância de <b>", input$p_power_alpha, "%</b>, tamanho de amostra igual a <b>", input$input$p_power_n,
             "</b> sujeitos e percentual de ocorrência esperada para <i>", input$prop_nome_desfecho_poder,
             "</i> igual a <b>", input$p_TH_observado, "%</b> (referido por Fulano (1900)). ",
             .txt_citacao_pss
           ),
           .txt_referencia_tap, print_r_code(code)
    )

  })





  # output$mean_eq <- renderText({
  #
  #   n <- TrialSize::OneSampleMean.Equivalence(
  #     alpha = input$alpha_eq_mean,
  #     beta = 1 - input$beta_eq_mean,
  #     sigma = input$sigma_eq_mean,
  #     margin = input$margin_eq_mean,
  #     delta = input$delta_eq_mean
  #   )
  #
  #   paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ",ceiling(n),
  #          "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>
  #            Com o objetivo de detectar uma diferença mínima entre proporções igual a ",input$margin_eq_mean,
  #          ", considerando o nível de significância igual a ",input$alpha_eq_mean,
  #          ", o poder igual a ", input$beta_eq_mean,
  #          ", o desvio padrão da variável de interesse igual a ", input$sigma_eq_mean,
  #          " e a margem de não inferioridade ou superioridade igual a ",input$delta_eq_mean,
  #          ", o tamanho de amostra calculado é igual a ",ceiling(n), ".")
  #
  # })
  #
  # output$mean_eq2 <- renderText({
  #
  #   n <- TrialSize::OneSampleMean.NIS(
  #     alpha = input$alpha_eq_mean2,
  #     beta = 1 - input$beta_eq_mean2,
  #     sigma = input$sigma_eq_mean2,
  #     margin = input$margin_eq_mean2,
  #     delta = input$delta_eq_mean2
  #   )
  #
  #   paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ",ceiling(n),
  #          "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>
  #             Com o objetivo de detectar uma diferença mínima entre médias igual a ",input$delta_eq_mean,
  #          ", considerando o nível de significância igual a ",input$alpha_eq_mean,
  #          ", o poder igual a ", input$beta_eq_mean,
  #          ", o desvio padrão da variável de interesse igual a ", input$sigma_eq_mean,
  #          " e a margem de não inferioridade ou superioridade igual a ",input$margin_eq_mean,
  #          ", o tamanho de amostra calculado é igual a ",ceiling(n), ".")
  #
  # })
  #
  # output$prop_eq <- renderText({
  #
  #   n <- TrialSize::OneSampleProportion.Equivalence(
  #     alpha = input$alpha_eq_prop,
  #     beta = 1 - input$beta_eq_prop,
  #     p = input$p_eq_prop,
  #     margin = input$margin_eq_prop,
  #     delta = input$delta_eq_prop
  #   )
  #
  #   paste0(
  #     "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ",ceiling(n),
  #     "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>
  #      Considerando o nível de significância igual a ",input$alpha_eq_prop,
  #     ", o poder igual a ", input$beta_eq_prop,
  #     ", a verdadeira proporção da variável de interesse na população igual a ", input$sigma_eq_prop,
  #     ", a diferença mínima a ser detectada igual a ",input$margin_eq_prop,
  #     " e a margem de não inferioridade ou superioridade igual a ",input$delta_eq_prop,
  #     ", o tamanho de amostra calculado para o teste de equivalência entre proporções é igual a ",ceiling(n), ".")
  #
  # })
  #
  # output$prop_eq2 <- renderText({
  #
  #   n <- TrialSize::OneSampleProportion.NIS(
  #     alpha = input$alpha_eq_prop2,
  #     beta = 1 - input$beta_eq_prop2,
  #     p = input$p_eq_prop2,
  #     margin = input$margin_eq_prop2,
  #     delta = input$delta_eq_prop2
  #   )
  #
  #   paste0(
  #     "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ",ceiling(n),
  #     "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>
  #      Considerando o nível de significância igual a ",input$alpha_eq_prop2,
  #     ", o poder igual a ", input$beta_eq_prop2,
  #     ", a verdadeira proporção da variável de interesse na população igual a ", input$sigma_eq_prop2,
  #     ", a diferença mínima a ser detectada igual a ",input$margin_eq_prop2,
  #     " e a margem de não inferioridade ou superioridade igual a ",input$delta_eq_prop2,
  #     ", o tamanho de amostra calculado é igual a ",ceiling(n), ".")
  #
  # })




  #__________-----
  #  Cronbach  ----

  #--------------------------------------------------------------.
  # Sample size to estimate a Cronbach alpha reliability
  #--------------------------------------------------------------.

  size.ci.cron1 <- function(alpha, k, rel, w) {
    # Computes sample size required to estimate a Cronbach
    # alpha reliability with desired precision
    # Arguments:
    #   alpha: alpha value for 1-alpha confidence
    #   k:     number of measurements
    #   rel:   reliability planning value
    #   w:     desired CI width
    # Returns:
    #   required sample size
    z <- qnorm(1 - alpha/2)
    n0 <- ceiling((8*k/(k - 1))*(1 - rel)^2*(z/w)^2 + 2)
    b <- log(n0/(n0 - 1))
    ll <- 1 - exp(log(1 - rel) - b + z*sqrt(2*k/((k - 1)*(n0 - 2))))
    ul <- 1 - exp(log(1 - rel) - b - z*sqrt(2*k/((k - 1)*(n0 - 2))))
    w0 <- ul - ll
    n <- ceiling((n0 - 2)*(w0/w)^2 + 2)
    return(n)
  }


  output$aba_estimacao_Cronbach  <- renderUI({

    tagList(

      titlePanel(translation_pss("Alfa de Cronbach", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_cronbach)),

      sidebarLayout(
        sidebarPanel(
          numericInput( "k_Cronbach",
                        translation_pss("Número de itens", linguagem()),
                        value = 10,
                        min = 2,
                        max = Inf,
                        step = 1
          ) %>% .help_buttom(body = paste0(translation_pss("Número de itens", linguagem()), txt_ajuda()$txt_definido_pesquisador)),
          numericInput( "Cronbach_espected",
                        translation_pss( "Cronbach esperado", linguagem()),
                        value = 0.7,
                        min = 0,
                        max = 1,
                        step = .1
          ) %>% .help_buttom(body = paste0(translation_pss( "Cronbach esperado", linguagem()), txt_ajuda()$txt_definido_pesquisador_OU_literatura)),
          numericInput( "Cronbach_precisao",
                        translation_pss("Amplitude do intervalo", linguagem()),
                        value = 0.2,
                        min = 0,
                        max = 1,
                        step = .1
          ) %>% .help_buttom(body = txt_ajuda()$txt_amplitude, title = translation_pss("Amplitude do intervalo", linguagem())),
          numericInput( "conf_Cronbach",
                        translation_pss("Nível de confiança (%)", linguagem()),
                        value = 95,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem())),
          numericInput( "Cronbach_perdas_recusa",
                        translation_pss("Perdas/ Recusas (%)", linguagem()),
                        value = 10,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem())),
        ),

        mainPanel(
          shinycssloaders::withSpinner(htmlOutput("Cronbach_est"), type = 5),

          htmlOutput("cronbach_code")

        )
      ),

      .rodape()
    )

  })






  # Estimar ----


  eval(parse(text = warning_prop("conf_Cronbach")))
  eval(parse(text = warning_prop("Cronbach_espected", entre0e1 = TRUE)))
  eval(parse(text = warning_numero_positivo("Cronbach_precisao")))
  eval(parse(text = warning_inteiro("k_Cronbach")))


  output$Cronbach_est <- renderText({

    code <- paste0("size.ci.cron1(",
                   "alpha = 1 - ", input$conf_Cronbach, "/100, ",
                   "k = ", input$k_Cronbach, ", ",
                   "rel = ", input$Cronbach_espected, ", ",
                   "w = ", input$Cronbach_precisao,
                   ") # Douglas G. Bonett. (2020). Sample Size Planning for Behavioral Science Research. Sample Size Planning for Behavioral Science Research. https://people.ucsc.edu/~dgbonett/sample.html"
    )

    n <- eval(parse(text = code))
    eval(parse(text = validate_n("n")))
    eval(parse(text = validate_n_inf("n")))


    paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
           "</font></b></br></br>",

           lista_de_funcoes_server()$sugestao_texto_portugues(
             "<i>",
             translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

             "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para estimar o coeficiente alfa de Cronbach ",
             "com amplitude desejada para o intervalo de confiança de <b>", input$Cronbach_precisao, "</b> ",
             "(com o acréscimo de <b>", input$Cronbach_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$Cronbach_perdas_recusa), "</b>). ",
             "O cálculo considerou nível de confiança de <b>", input$conf_Cronbach, "%</b>, alfa de Cronbach esperado de <b>", input$Cronbach_espected,"</b> ",
             "como referido em Fulano (1900) <b>OU</b> escolha do pesquisador. ",
             .txt_citacao_pss
           ),
           .txt_referencia_tap
    )

  })


  output$cronbach_code <- renderText({


    paste0("</br></br>",
           # "<i>Comando R utilizado/ R code::</i><br>",
           ifelse (linguagem() == "en", "<i>R code:</i><br>", "<i>Comando R utilizado</i><br>"),
           "<p style=\"font-family:'Courier New';font-size:100% \">",
           HTML(
             "# Douglas G. Bonett. (2020). Sample Size Planning for Behavioral Science Research. Sample Size Planning for Behavioral Science Research. https://people.ucsc.edu/~dgbonett/sample.html"
           ),
           br(),br(),

           code("size.ci.cron1 <- function(alpha, k, rel, w) {"), br(),
           code("# Computes sample size required to estimate a Cronbach"), br(),
           code("# alpha reliability with desired precision"), br(),
           code("# Arguments:"), br(),
           code("#   alpha: alpha value for 1-alpha confidence"), br(),
           code("#   k:     number of measurements"), br(),
           code("#   rel:   reliability planning value"), br(),
           code("#   w:     desired CI width"), br(),
           code("# Returns:"), br(),
           code("#   required sample size"), br(),
           code("z <- qnorm(1 - alpha/2)"), br(),
           code("n0 <- ceiling((8*k/(k - 1))*(1 - rel)^2*(z/w)^2 + 2)"), br(),
           code("b <- log(n0/(n0 - 1))"), br(),
           code("ll <- 1 - exp(log(1 - rel) - b + z*sqrt(2*k/((k - 1)*(n0 - 2))))"), br(),
           code("ul <- 1 - exp(log(1 - rel) - b - z*sqrt(2*k/((k - 1)*(n0 - 2))))"), br(),
           code("w0 <- ul - ll"), br(),
           code("n <- ceiling((n0 - 2)*(w0/w)^2 + 2)"), br(),
           code("return (n)"), br(),
           code("}"), br(),
           br(), br(),

           code("size.ci.cron1("),
           code(paste0("alpha = 1 - ", input$conf_Cronbach, "/100, ")),
           code(paste0("k = ", input$k_Cronbach, ", ")),
           code(paste0("rel = ", input$Cronbach_espected, ", ")),
           code(paste0("w = ", input$Cronbach_precisao, ")")),

           "</p>",
           "<br><br><b><i>",
           ifelse (
             linguagem() == "en",
             "* Always consult a statistician for guidance in study design.",
             "* Sempre procure um profissional de estatística para orientações no planejamento do estudo."
           ),
           "</b></i>"
    )

  })







  #________________----
  # 2 media ind (ok)----


  mod_2_medias_independentes_server(
    "tamanho_amostral_2_medias_independentes",
    tipo = "tamanho_amostral",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    h1 = h1,
    cohen_d = cohen_d,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  mod_2_medias_independentes_server(
    "poder_2_medias_independentes",
    tipo = "poder",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    h1 = h1,
    cohen_d = cohen_d,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )

  mod_2_medias_independentes_server(
    "estimar_2_medias_independentes",
    tipo = "estimar",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    h1 = h1,
    cohen_d = cohen_d,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  output$aba_2_medias_independentes <- renderUI({

    tagList(

      titlePanel(translation_pss("Comparação entre duas médias de grupos independentes", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_2_medias_independentes)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_2_medias_independentes_Ui("tamanho_amostral_2_medias_independentes"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_2_medias_independentes_Ui("poder_2_medias_independentes"),
                 .rodape()
        ),

        tabPanel(translation_pss("Estimar", linguagem()),
                 mod_2_medias_independentes_Ui("estimar_2_medias_independentes"),
                 .rodape()
        )
      )
    )
  })





  #---------------.
  # Equivalência (ok) ----
  #---------------.



  mod_nao_inferioridade_server(
    "tamanho_amostral_nao_inferioridade_c",
    tipo = "tamanho_amostral",
    tipo_variavel = "c",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  mod_nao_inferioridade_server(
    "poder_nao_inferioridade_c",
    tipo = "poder",
    tipo_variavel = "c",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  output$aba_nao_inferioridade <- renderUI({

    tagList(

      titlePanel(translation_pss("Dois grupos independentes (Inf/ Equi/ Sup)", linguagem())),
      withMathJax(),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_equivalencia)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_nao_inferioridade_Ui("tamanho_amostral_nao_inferioridade_c"),
                 .rodape()
        ),
        tabPanel(translation_pss("Poder", linguagem()),
                 mod_nao_inferioridade_Ui("poder_nao_inferioridade_c"),
                 .rodape()
        )
      )
    )
  })








  # 2 dependentes (ok)----


  mod_2_medias_dependentes_server(
    "tamanho_amostral_2_medias_dependentes",
    tipo = "tamanho_amostral",
    txt_ajuda = txt_ajuda,
    h1 = h1,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  mod_2_medias_dependentes_server(
    "poder_2_medias_dependentes",
    tipo = "poder",
    txt_ajuda = txt_ajuda,
    h1 = h1,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  mod_2_medias_dependentes_server(
    "estimar_2_medias_dependentes",
    tipo = "estimar",
    txt_ajuda = txt_ajuda,
    h1 = h1,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  output$aba_2_medias_dependentes <- renderUI({

    tagList(

      titlePanel(translation_pss("Dois grupos dependentes", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_2_medias_dependentes)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_2_medias_dependentes_Ui("tamanho_amostral_2_medias_dependentes"),
                 .rodape()
        ),
        tabPanel(translation_pss("Poder", linguagem()),
                 mod_2_medias_dependentes_Ui("poder_2_medias_dependentes"),
                 .rodape()
        ),
        tabPanel(translation_pss("Estimar", linguagem()),
                 mod_2_medias_dependentes_Ui("estimar_2_medias_dependentes"),
                 .rodape()
        )
      )
    )
  })







  # Comparação entre duas médias de grupos pareados

  output$print_desvio_tpareado <- renderText({
    s1 <- input$popup_sd_baseline
    s2 <- input$popup_sd_follow

    temp <- s1^2 + s2^2 - (2*s1*s2*input$popup_sd_correlation)

    paste0("<b><font size = '5'><br><br>",
           "<i>Desvio padrão da diferença</i> = ", round(sqrt(temp), 4))
  })






  # 2 Deltas----

  output$aba_TH_duas_amostra_media_2tempos <- renderUI({

    tagList(

      titlePanel(translation_pss("Delta de dois grupos independentes", linguagem())),

      wellPanel(HTML(txt_ajuda()$wellPanel_txt_2_deltas)),

      # wellPanel(
      # includeMarkdown(file.path("www", "Delta_two_groups_independents.Rmd"))
      # ),

      sidebarLayout(
        sidebarPanel(

          wellPanel(HTML(
            '<b><a href="https://youtu.be/c7zzQxgeaS4" target="_blank">',
            translation_pss("Vídeo: PSS Health para comparar dua médias", linguagem()),
            '</a></b><br>'
          )),

          wellPanel(
            HTML(
              paste0(
                "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
              )
            ),
            uiOutput("delta2_mean_formula1"),
            uiOutput("delta2_mean_formula2")
          ),

          actionLink("show_th_2delta", translation_pss("Mudar nomes", linguagem())),
          br(), br(),



          uiOutput("delta2_painelUi"),

          numericInput( "th2_mean_dep_balanceamento",
                        paste0(
                          translation_pss("Balanceamento", linguagem()),
                          " (", delta2_grupoTratamento(), ":", delta2_grupoControle(), ")"
                        ),
                        value = 1,
                        min   = 0,
                        max   = Inf,
                        step  = .5
          ) %>% .help_buttom(body = txt_balanceamento_f(delta2_grupoTratamento(), delta2_grupoControle()),
                             title = translation_pss("Balanceamento", linguagem())),

          numericInput( "th2_mean_dep_pwr",
                        translation_pss("Poder (%)", linguagem()),
                        value = 80,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem())),
          numericInput( "th2_mean_dep_sig",
                        translation_pss("Nível de significância (%)", linguagem()),
                        value = 5,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem())),
          # selectInput('alternative_TH2_mean_pwr',
          #             'Tipo de teste de acordo com hipótese alternativa',
          #             choices = c('A média do grupo A é DIFERENTE da média do grupo B' = 'two.sided',
          #                         'A média do grupo A é MAIOR do que a média do grupo B' = 'greater',
          #                         'A média do grupo A é MENOR do que a média do grupo B' =  'less'),
          #             selected = 'two.sided'
          # ) %>% .help_buttom(body = txt_ajuda()$txt_h1),
          numericInput( "th2_mean_dep_perdas_recusa",
                        translation_pss("Perdas/ Recusas (%)", linguagem()),
                        value = 10,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
        ),

        mainPanel(
          shinycssloaders::withSpinner(htmlOutput("th2_mean_dep_out"), type = 5)
        )
      ),

      .rodape()
    )


  })




  eval(parse(text = warning_numero("th2_mean_dep_diff")))
  eval(parse(text = warning_numero("th2_mean_dep_delta_tratamento")))
  eval(parse(text = warning_numero("th2_mean_dep_delta_controle")))
  eval(parse(text = warning_numero_positivo("th2_mean_dep_sigma1")))
  eval(parse(text = warning_numero_positivo("th2_mean_dep_sigma2")))
  eval(parse(text = warning_numero_positivo("th2_mean_dep_desvio_diff")))
  eval(parse(text = warning_prop("th2_mean_dep_sig")))
  eval(parse(text = warning_prop("th2_mean_dep_pwr")))
  eval(parse(text = warning_perdas("th2_mean_dep_perdas_recusa")))

  eval(parse(text = warning_correlacao("th2_mean_dep_rho")))



  observeEvent(input$show_th_2delta, {
    showModal(
      modalDialog(
        title = translation_pss("Ajustes", linguagem()),
        fluidPage(

          HTML(
            translation_pss("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>.", linguagem())
          ),
          br(), br(),
          textInput(inputId = "delta2_nome_desfecho",
                    label   = translation_pss("Descreva o nome do desfecho", linguagem()),
                    value   = ifelse(input$show_th_2delta == 0, "Y", delta2_nome_desfecho())),
          HTML("<i>", gsub("<br><br>", "", txt_ajuda()$txt_desfecho), "</i>"),

          br(), br(),
          textInput(inputId = "delta2_unidade_medida",
                    label   = translation_pss("Descreva a unidade de medida do desfecho", linguagem()),
                    value   = ifelse(input$show_th_2delta == 0, translation_pss("u.m.", linguagem()), delta2_unidade_medida())),
          HTML("<i>", gsub("<br><br>", "", txt_ajuda()$txt_um), "</i>"),
          br(), br(),

          textInput(inputId = "delta2_grupoTratamento",
                    label   = translation_pss("Descreva um nome para o grupo Tratamento", linguagem()),
                    value   = ifelse(input$show_th_2delta == 0, translation_pss("Tratamento", linguagem()), delta2_grupoTratamento())),

          HTML("<i>Em alguns estudos o grupo Tratamento também pode ser chamadado de grupo Intervenção ou grupo Exposto.</i><br><br>"),

          textInput(inputId = "delta2_grupoControle",
                    label   = translation_pss("Descreva um nome para o grupo Controle", linguagem()),
                    value   = ifelse(input$show_th_2delta == 0, translation_pss("Controle", linguagem()), delta2_grupoControle())),

          HTML("<i>Em alguns estudos o grupo Controle também pode ser chamadado de grupo Placebo/ Sham ou grupo Não exposto.</i>"),


        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })





  delta2_grupoControle <- reactive({
    ifelse(is.null(input$delta2_grupoControle), translation_pss("Controle", linguagem()), input$delta2_grupoControle)
  })

  delta2_grupoTratamento <- reactive({
    ifelse(is.null(input$delta2_grupoTratamento), translation_pss("Tratamento", linguagem()), input$delta2_grupoTratamento)
  })

  delta2_nome_desfecho <- reactive({
    ifelse(is.null(input$delta2_nome_desfecho), "Y", input$delta2_nome_desfecho)
  })

  delta2_unidade_medida <- reactive({
    ifelse(is.null(input$delta2_unidade_medida), translation_pss("u.m.", linguagem()), input$delta2_unidade_medida)
  })



  output$delta2_mean_formula1 <- renderUI({
    req(!is.null(delta2_grupoTratamento()))

    withMathJax(
      paste0("$$H_0: \\bar{\\Delta}_\\text{", delta2_grupoTratamento(), "} = \\bar{\\Delta}_\\text{", delta2_grupoControle(), "} $$"))
  })

  output$delta2_mean_formula2 <- renderUI({
    req(!is.null(delta2_grupoTratamento()))

    withMathJax(
      paste0("$$H_1: \\bar{\\Delta}_\\text{", delta2_grupoTratamento(), "} \\neq \\bar{\\Delta}_\\text{", delta2_grupoControle(), "} $$"))
  })



  output$delta2_painelUi <- renderUI({

    fluidPage(fluidRow(

      wellPanel(

        checkboxInput(
          "th2_mean_dep_utilizar_medias", translation_pss("Utilizar os valores dos deltas de cada grupo", linguagem()), value = FALSE
        ) ,
        # %>% .help_buttom(body = "Clique aqui para usar os valores dos deltas de cada grupo ao invés da diferença esperada."),


        conditionalPanel(condition = "input.th2_mean_dep_utilizar_medias == false",
                         numericInput( "th2_mean_dep_diff",
                                       paste0(
                                         translation_pss("Diferença mínima a ser detectada entre os deltas", linguagem()),
                                         " (",
                                         translation_pss("em", linguagem()),
                                         " ",
                                         delta2_unidade_medida(),
                                         ")"
                                       ),
                                       value = 0.4,
                                       min = 0,
                                       max = Inf,
                                       step = 0.5
                         ) %>% .help_buttom(body = txt_ajuda()$txt_diferenca_clinica, title = translation_pss("Diferença mínima a ser detectada", linguagem()))
        ),

        conditionalPanel(condition = "input.th2_mean_dep_utilizar_medias == true",

                         if (linguagem() == "pt") {
                           HTML(
                             paste0(
                               "<b><font size = '2.95'>Mudança média (delta) de ", delta2_nome_desfecho(), " ao longo do tempo do grupo</font></b><br>"
                             )
                           )
                         } else {
                           HTML(
                             paste0(
                               "<b><font size = '2.95'>Mean change (delta) of ", delta2_nome_desfecho(), " over time from group</font></b><br>"
                             )
                           )
                         },
                         div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                             numericInput( "th2_mean_dep_delta_tratamento",
                                           delta2_grupoTratamento(),
                                           value = 5,
                                           min = -Inf,
                                           max = Inf,
                                           step = .5
                             )
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 49%;",
                             numericInput( "th2_mean_dep_delta_controle",
                                           delta2_grupoControle(),
                                           value = 4.5,
                                           min = 0,
                                           max = Inf,
                                           step = .5
                             )
                         )
        )

      ),


      wellPanel(

        checkboxInput(
          inputId = "th2_mean_usar_desvio_diff",
          label   = translation_pss("Usar o desvio padrão da diferença", linguagem()),
          value   = FALSE
        ),

        conditionalPanel(
          'input.th2_mean_usar_desvio_diff == true',

          numericInput( "th2_mean_dep_desvio_diff",
                        translation_pss("Desvio padrão da diferença", linguagem()),
                        value = 2.5,
                        min = 0,
                        max = Inf,
                        step = 1
          ) %>% .help_buttom(body = paste0(txt_ajuda()$txt_dp, txt_ajuda()$txt_definido_literatura))
        ),

        conditionalPanel(
          'input.th2_mean_usar_desvio_diff == false',

          tagList(
            if (linguagem() == "pt") {
              HTML(paste0("<b><font size = '2.95'>Desvio padrão esperado de ", delta2_nome_desfecho(), " no</font></b><br>"))
            } else {
              HTML(paste0("<b><font size = '2.95'>Expected standard deviation of ", delta2_nome_desfecho(), " at</font></b><br>"))
            },

            div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                numericInput( "th2_mean_dep_sigma1",
                              translation_pss("Início do estudo", linguagem()),
                              value = 1.4,
                              min = 0,
                              max = Inf,
                              step = .5
                )
            ),
            div(style="display: inline-block;vertical-align:top; width: 49%;",
                numericInput( "th2_mean_dep_sigma2",
                              translation_pss("Final do estudo", linguagem()),
                              value = 1.2,
                              min = 0,
                              max = Inf,
                              step = .5
                ) %>% .help_buttom(body = txt_ajuda()$txt_dp, title = translation_pss("Desvio padrão esperado", linguagem()))
            ),
            numericInput( "th2_mean_dep_rho",
                          if (linguagem() == "pt") {
                            paste0("Correlação das medidas de ", delta2_nome_desfecho(), " (início e fim)")
                          } else {
                            paste0("Correlation of ", delta2_nome_desfecho(), " measures (Start and end)")
                          },
                          value = 0.5,
                          min = -1,
                          max = 1,
                          step = 1
            ) %>% .help_buttom(linguagem = linguagem(),
              body = txt_ajuda()$txt_correlacao,
              title = translation_pss("Coeficiente de correlação esperado", linguagem())
            )
          )
        )
      )
    ))
  })




  ## Texto -----


  output$th2_mean_dep_out <- renderText({

    req(!is.null(input$th2_mean_dep_utilizar_medias))

    if (!input$th2_mean_dep_utilizar_medias) {
      delta <- input$th2_mean_dep_diff
      info_texto <- paste0("uma diferença esperada/ desejada de <b>", input$th2_mean_dep_diff, " ", delta2_unidade_medida(), "</b>, ")
    } else {
      delta <- abs(input$th2_mean_dep_delta_tratamento - input$th2_mean_dep_delta_controle)
      info_texto <- paste0("uma variação média no intervalo de tempo de <b>",
                           input$th2_mean_dep_delta_tratamento, " ", delta2_unidade_medida(), "</b> e <b>",
                           input$th2_mean_dep_delta_controle, " ", delta2_unidade_medida(), "</b>",
                           " para o grupo ", delta2_grupoTratamento(), " e ", delta2_grupoControle(), ", respectivamente, ")
    }



    if (input$th2_mean_usar_desvio_diff) {

      desvio_diff <- input$th2_mean_dep_desvio_diff

    } else{
      # correlacao <- input$th2_mean_dep_rho
      sdb <- input$th2_mean_dep_sigma1
      sdf <- input$th2_mean_dep_sigma2
      corr <- input$th2_mean_dep_rho

      sde2 <- sdb^2 + sdf^2  - (2*corr*sdb*sdf)

      desvio_diff <- sqrt(sde2)
    }


    code <- paste0(
      "epiR::epi.sscompc(",
      "control = 0, ",
      "treat = ", delta, ", ",
      "sigma = ", desvio_diff, ", ",
      "n = NA, power = ", input$th2_mean_dep_pwr, "/100, ",
      "conf.level = 1 - ", input$th2_mean_dep_sig, "/100, ",
      "r = ", input$th2_mean_dep_balanceamento, ", ",
      "design = 1, sided.test = 2)"
    )

    npwr <- try_n(code)
    eval(parse(text = validate_n("npwr")))

    n1 <- ceiling(npwr$n.treat)
    n2 <- ceiling(npwr$n.control)

    n <- n1 + n2
    nperdas1 <- n_perdas(n1, input$th2_mean_dep_perdas_recusa)
    nperdas2 <- n_perdas(n2, input$th2_mean_dep_perdas_recusa)
    eval(parse(text = validate_n("n")))
    eval(parse(text = validate_n_inf("n")))

    # print_r_code(code)
    paste0(
      "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
      if (n1 != n2) {
        paste0(
          " (<i>", n1, " ", delta2_grupoTratamento(), translation_pss(" e ", linguagem()), n2, " ", delta2_grupoControle(), "</i>)"
        )
      } else {
        paste0(
          " (<i>", n1, " ", translation_pss("para cada grupo", linguagem()), "</i>)"
        )
      },
      "</font></b></br></br>",


      lista_de_funcoes_server()$sugestao_texto_portugues(
        "<i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
        "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos ",
        if (n1 != n2) {
          paste0(
            "(", n1, " no grupo ", delta2_grupoTratamento(), " e ", n2, " no grupo ", delta2_grupoControle(), ")"
          )
        } else {
          paste0(
            "(", n1, " para cada grupo)"
          )
        },

        " para testar se existe uma diferença mínima de <b>", delta, " ", delta2_unidade_medida(), "</b> ",
        "na variação média de <i>", delta2_nome_desfecho(), "</i>, em um intervalo de tempo <b>T</b>, ",
        "entre os grupos <i>", delta2_grupoTratamento(), "</i> e <i>", delta2_grupoControle(), "</i>",
        if (n1 == n2) {
          paste0(" (com o acréscimo de <b>", input$th2_mean_dep_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser <b>", nperdas1 + nperdas2, "</b>). ")
        } else {
          paste0(" (com o acréscimo de <b>", input$th2_mean_dep_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser ", nperdas1, " ", delta2_grupoTratamento(), " e ", nperdas2, " ", delta2_grupoControle(), "). ")
        },

        "O cálculo considerou poder de <b>", input$th2_mean_dep_pwr, "%</b>, nível de significância de <b>", input$th2_mean_dep_sig, "%</b>",

        if (input$th2_mean_usar_desvio_diff) {
          paste0(" e desvio padrão da diferença de <b>", input$th2_mean_dep_desvio_diff, " ", delta2_unidade_medida(), "</b> ")
        } else {
          paste0(
            if (input$th2_mean_dep_sigma1 == input$th2_mean_dep_sigma2) {
              paste0(
                ", desvios padrões de <b>", input$th2_mean_dep_sigma1, "</b> no ínicio e no final do estudo"
              )
            } else {
              paste0(
                ", desvios padrões de <b>", input$th2_mean_dep_sigma1, "</b> e <b>", input$th2_mean_dep_sigma2, " ", delta2_unidade_medida(), "</b> ",
                "no ínicio e no final do estudo, respectivamente "
              )
            },

            "e correlação entre as medidas de <b>", input$th2_mean_dep_rho, "</b> "
          )
        },



        "(dados de Fulano (1900)). ",
        .txt_citacao_pss
      ),
      .txt_referencia_tap,
      print_r_code(code)

    )


  })




  #________________----
  # 2 media assimetrica ----


  mod_2_medias_assimetricas_server(
    "tamanho_amostral_2_medias_assimetricas",
    tipo = "tamanho_amostral",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    h1 = h1,
    cohen_d = cohen_d,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  output$aba_2_medias_assimetrica <- renderUI({

    tagList(

      titlePanel(translation_pss("Comparação entre duas médias de grupos independentes (distribuição gamma)", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_2_medias_assimetricas)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_2_medias_assimetricas_Ui("tamanho_amostral_2_medias_assimetricas"),
                 .rodape()
        )

        # tabPanel(translation_pss("Poder", linguagem()),
        #          mod_2_medias_independentes_Ui("poder_2_medias_independentes"),
        #          .rodape()
        # ),
        #
        # tabPanel(translation_pss("Estimar", linguagem()),
        #          mod_2_medias_independentes_Ui("estimar_2_medias_independentes"),
        #          .rodape()
        # )
      )
    )
  })



  #___________----
  # 2 proporcao (ok) ----
  #---------------.


  mod_2_proporcoes_independentes_server(
    "tamanho_amostral_2_proporcoes_independentes",
    tipo = "tamanho_amostral",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    h1 = h1,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )

  mod_2_proporcoes_independentes_server(
    "poder_2_proporcoes_independentes",
    tipo = "poder",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    h1 = h1,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )

  mod_2_proporcoes_independentes_server(
    "estimar_2_proporcoes_independentes",
    tipo = "estimar",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    h1 = h1,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  output$aba_2_proporcoes_independentes <- renderUI({

    tagList(

      titlePanel(
        translation_pss("Comparação entre duas proporções de grupos independentes" , linguagem())
      ),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_2_prop_independentes)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_2_proporcoes_independentes_Ui("tamanho_amostral_2_proporcoes_independentes"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_2_proporcoes_independentes_Ui("poder_2_proporcoes_independentes"),
                 .rodape()
        ),

        tabPanel(translation_pss("Estimar", linguagem()),
                 mod_2_proporcoes_independentes_Ui("estimar_2_proporcoes_independentes"),
                 .rodape()
        )
      )
    )
  })









  # Equivalencia (ok)----



  mod_nao_inferioridade_server(
    "tamanho_amostral_nao_inferioridade_b",
    tipo = "tamanho_amostral",
    tipo_variavel = "b",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  mod_nao_inferioridade_server(
    "poder_nao_inferioridade_b",
    tipo = "poder",
    tipo_variavel = "b",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  output$aba_nao_inferioridade_binaria <- renderUI({

    tagList(

      titlePanel(translation_pss("Dois grupos independentes (Inf/ Equi/ Sup)", linguagem())),
      withMathJax(),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_equivalencia)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_nao_inferioridade_Ui("tamanho_amostral_nao_inferioridade_b"),
                 .rodape()
        ),
        tabPanel(translation_pss("Poder", linguagem()),
                 mod_nao_inferioridade_Ui("poder_nao_inferioridade_b"),
                 .rodape()
        )
      )
    )
  })







  # 2 Dependentes (ok) ----



  mod_mc_nemar_server(
    "tamanho_amostral_mc_nemar",
    tipo = "tamanho_amostral",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  mod_mc_nemar_server(
    "poder_mc_nemar",
    tipo = "poder",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )




  output$aba_mc_nemar <- renderUI({

    tagList(

      titlePanel(translation_pss("Dois grupos dependentes", linguagem())),
      withMathJax(),
      wellPanel(
        if (linguagem() == "pt") {
          includeMarkdown(file.path("www", "Teste_Mcnemar.Rmd"))
        } else {
          includeMarkdown(file.path("www", "Teste_Mcnemar_en.Rmd"))
        }
      ),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_mc_nemar_Ui("tamanho_amostral_mc_nemar"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_mc_nemar_Ui("poder_mc_nemar"),
                 .rodape()
        )

      )
    )
  })























  #____________-----
  # Medidas repetidas (ok) -----


  mod_medidas_repetidas_server(
    "tamanho_amostral",
    tipo = "tamanho_amostral",
    translation_pss = translation_pss,
    linguagem = linguagem,
    txt_ajuda = txt_ajuda,

    .rodape = .rodape,
    try_n = try_n,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    n_perdas = n_perdas,
    print_r_code = print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    txt_balanceamento_f = txt_balanceamento_f,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  mod_medidas_repetidas_server(
    "poder",
    tipo = "poder",
    translation_pss = translation_pss,
    linguagem = linguagem,
    txt_ajuda = txt_ajuda,

    .rodape = .rodape,
    try_n = try_n,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    n_perdas = n_perdas,
    print_r_code = print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    txt_balanceamento_f = txt_balanceamento_f,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )

  output$aba_TH_medidas_repetidas <- renderUI({

    tagList(

      wellPanel(HTML(txt_ajuda()$wellPanel_txt_medidas_repetidas)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_medidas_repetidas_Ui("tamanho_amostral"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_medidas_repetidas_Ui("poder"),
                 .rodape()
        )
      )
    )
  })




  #______ -----
  # ANOVA (ok)-----


  mod_anova1_server(
    "anova1_tamanho_amostral",
    tipo = "tamanho_amostral",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    ajuda_cenarios_multiplos_valores2 = ajuda_cenarios_multiplos_valores2,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    try_n = try_n,
    validate_n_inf = validate_n_inf,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  mod_anova1_server(
    "anova_1poder",
    tipo = "poder",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    ajuda_cenarios_multiplos_valores2 = ajuda_cenarios_multiplos_valores2,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    try_n = try_n,
    validate_n_inf = validate_n_inf,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )





  output$aba_ANOVA1 <- renderUI({

    tagList(

      titlePanel(
        translation_pss("ANOVA de uma via", linguagem())
      ),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_anova1)),


      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_anova1_Ui("anova1_tamanho_amostral"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_anova1_Ui("anova_1poder"),
                 .rodape()
        )
      )
    )
  })





  # Two way -----


  output$aba_anova_two_way <- renderUI({

    tagList(

      titlePanel(translation_pss("ANOVA de duas vias", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_anova2)),
      tabsetPanel(
        tabPanel("Efeitos principais",
                 sidebarLayout(
                   sidebarPanel(
                     HTML("<b>ATENÇÃO!</b><br>
                             Nesta aba é calculado o tamanho de amostra para testar os efeitos principais da ANOVA de duas vias.
                             Caso deseje tamanho de amostra para o efeito de interação, utilize a aba 'Efeito de interação'.
                             <br>
                             <br> "),

                     actionLink("show_th_anova2way", translation_pss("Mudar nomes", linguagem())),
                     br(), br(),


                     uiOutput("k_anova_n_ui"),

                     checkboxInput("two_way_cohen",
                                   translation_pss("Usar magnitude de efeito f", linguagem()),
                                   value = FALSE),

                     conditionalPanel(condition = "input.two_way_cohen == true", uiOutput("f_anova_n_ui")),

                     conditionalPanel(condition = "input.two_way_cohen == false",
                                      uiOutput("delta_anova_n_ui"),
                                      # uiOutput("delta_anova_n_B_ui"),
                                      # uiOutput("sigma_anova_n_A_ui"),
                                      uiOutput("sigma_anova_n_ui")
                     ),
                     # HTML('<hr style="color: black;">'),
                     numericInput( "power_anova_n_two",
                                   translation_pss("Poder (%)", linguagem()),
                                   value = 80,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem())),
                     numericInput( "sig_anova_n_two",
                                   translation_pss("Nível de significância (%)", linguagem()),
                                   value = 5,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem())),
                     numericInput( "two_way_perdas_recusa",
                                   translation_pss("Perdas/ Recusas (%)", linguagem()),
                                   value = 10,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
                     # actionButton("help_anova_n_two", "Ajuda")
                   ),

                   mainPanel(
                     shinycssloaders::withSpinner(htmlOutput("anova_n_two"), type =  5),
                   )
                 )
        ),

        tabPanel("Efeito da interação",
                 sidebarLayout(
                   sidebarPanel(
                     HTML("<b>ATENÇÃO!</b><br>
                             Nesta aba é calculado o tamanho de amostra para testar o efeito de interação da ANOVA de duas vias.
                             Caso deseje tamanho de amostra para os efeitos principais, utilize a aba 'Efeitos principais'.
                             <br>
                             <br> "),

                     textInput(inputId = "two_nome_desfechoA2",
                               label   = "Descreva o nome do fator A",
                               value   = "Fator A"
                     ) %>% .help_buttom(body = txt_outros_desfechos("Descreva o nome do fator A para que sirvam de guia no preenchimento dos valores.")),
                     textInput(inputId = "two_nome_desfechoB2",
                               label   = "Descreva o nome do fator B",
                               value   = "Fator B"
                     ) %>% .help_buttom(body = txt_outros_desfechos("Descreva o nome do fator B para que sirvam de guia no preenchimento dos valores.")),
                     uiOutput("k_anova_n_A_ui2"),
                     uiOutput("k_anova_n_B_ui2"),
                     uiOutput("f_anova_n_A_ui2"),
                     numericInput( "power_anova_n_two2",
                                   translation_pss("Poder (%)", linguagem()),
                                   value = 80,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem())),
                     numericInput( "sig_anova_n_two2",
                                   translation_pss("Nível de significância (%)", linguagem()),
                                   value = 5,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem())),
                     numericInput( "two_way_perdas_recusa2",
                                   translation_pss("Perdas/ Recusas (%)", linguagem()),
                                   value = 10,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
                   ),

                   mainPanel(
                     htmlOutput("anova_n_two2") %>%
                       shinycssloaders::withSpinner(type =  5)
                   )
                 )
        )
      ),

      .rodape()
    )

  })




  observeEvent(input$show_th_anova2way, {
    showModal(
      modalDialog(
        title = translation_pss("Ajustes", linguagem()),
        fluidPage(

          HTML(
            translation_pss(translation_pss("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>.", linguagem()), linguagem())
          ),
          br(), br(),
          textInput(inputId = "two_nome_desfechoA",
                    label   = "Descreva o nome do fator A",
                    value   = ifelse(input$show_th_anova2way == 0, "Fator A", nome_fatorA())),
          textInput(inputId = "two_nome_desfechoB",
                    label   = "Descreva o nome do fator B",
                    value   = ifelse(input$show_th_anova2way == 0, "Fator B", nome_fatorB())),
        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })





  nome_fatorA <- reactive({
    ifelse(is.null(input$two_nome_desfechoA), "Fator A", input$two_nome_desfechoA)
  })

  nome_fatorB <- reactive({
    ifelse(is.null(input$two_nome_desfechoB), "Fator B", input$two_nome_desfechoB)
  })


  output$k_anova_n_ui <- renderUI({

    fluidPage(fluidRow(

      HTML("<b>Número de grupos para comparar do </b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "k_anova_n_A",
                        paste(nome_fatorA()),
                        value = 3,
                        min = 2,
                        max = Inf,
                        step = 1
          )
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "k_anova_n_B",
                        paste(nome_fatorB()),
                        value = 2,
                        min = 2,
                        max = Inf,
                        step = 1
          )
      )
    ))
  })



  output$f_anova_n_ui <- renderUI({

    fluidPage(fluidRow(

      HTML("<b>Magnitude do efeito do</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "f_anova_n_A",
                        paste(nome_fatorA()),
                        value = .4,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "f_anova_n_B",
                        paste(nome_fatorB()),
                        value = .2,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      )
    ))


  })


  output$delta_anova_n_ui <- renderUI({



    fluidPage(fluidRow(

      HTML("<b>A menor diferença entre os níveis de</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "delta_anova_n_A",
                        paste(nome_fatorA()),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "delta_anova_n_B",
                        paste(nome_fatorB()),
                        value = 2.1,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      )
    ))

  })



  output$sigma_anova_n_ui <- renderUI({

    fluidPage(fluidRow(

      HTML("<b>Desvio padrão esperado do</b>"),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "sigma_anova_n_A",
                        paste(nome_fatorA()),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "sigma_anova_n_B",
                        paste(nome_fatorB()),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = .01
          )
      )
    ))

  })




  ## Texto ----

  output$anova_n_two <- renderText({

    req(!(is.null(input$k_anova_n_A) | is.null(input$k_anova_n_B) | is.null(input$delta_anova_n_A) | is.null(input$delta_anova_n_B) |
            is.null(input$sigma_anova_n_A) | is.null(input$sigma_anova_n_B)))

    a <- input$k_anova_n_A
    b <- input$k_anova_n_B

    d_a <- input$delta_anova_n_A
    d_b <- input$delta_anova_n_B

    s_a <- input$sigma_anova_n_A
    s_b <- input$sigma_anova_n_B

    f_a <- input$f_anova_n_A
    f_b <- input$f_anova_n_B

    if (input$two_way_cohen) {

      req(!is.null(f_a))
      req(!is.null(f_b))


      code <- paste0(
        "pwr2::ss.2way(a = ", a, ", ",
        "b = ", b, ", ",
        "alpha = ", input$sig_anova_n_two, "/100, ",
        "beta  = (100 - ", input$power_anova_n_two, ")/100, ",
        "f.A   = ", f_a, ", ",
        "f.B   = ", f_b,  ", ",
        "delta.A = NULL, ",
        "delta.B = NULL, ",
        "sigma.A = NULL, ",
        "sigma.B = NULL, ",
        "B = 1000)"
      )

      n <- try_n(code)
      eval(parse(text = validate_n("n")))
      n <- n$n
      n_perdas <- n_perdas(n, input$two_way_perdas_recusa)
      eval(parse(text = validate_n_inf("n")))


      paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n*a*b,
             " (<b>", n, "</b> por grupo de combinação).",
             "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

             "Foi calculado um tamanho de amostra de <b>", n*a*b, "</b> sujeitos (<b>", n, "</b> para cada grupo) ",
             "para detectar as magnitudes de efeito f de <b>", f_a, "</b> e <b>", f_b, "</b> (referido por Fulano (1900) <b>OU</b> escolha do pesquisador) ",
             "dos efeitos principais do <i>", nome_fatorA(), "</i> (com <b>", a, "</b> níveis) ",
             " e de <b>", nome_fatorB(), "</b> (com <b>", b, "</b> níveis), respectivamente ",
             "(com o acréscimo de <b>", input$two_way_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas, "</b>). ",
             "O cálculo considerou poder de <b>", input$power_anova_n_two, "%</b> e nível de significância de <b>", input$sig_anova_n_two, "%</b>. ",
             .txt_citacao_pss,
             .txt_referencia_tap,
             print_r_code(code)
      )


    } else{

      code <- paste0(
        "pwr2::ss.2way(a = ", a, ", ",
        "b = ", b, ", ",
        "alpha = ", input$sig_anova_n_two, "/100, ",
        "beta  = (100 - ", input$power_anova_n_two, ")/100, ",
        "f.A   = NULL, ",
        "f.B   = NULL, ",
        "delta.A = ", d_a, ", ",
        "delta.B = ", d_b, ", ",
        "sigma.A = ", s_a, ", ",
        "sigma.B = ", s_b, ", ",
        "B = 1000)")

      n <- try_n(code)
      eval(parse(text = validate_n("n")))
      n <- n$n
      n_perdas <- n_perdas(n, input$two_way_perdas_recusa)
      eval(parse(text = validate_n_inf("n")))


      paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n*a*b,
             " (<b>", n, "</b> por grupo de combinação).",
             "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

             "Foi calculado um tamanho de amostra de <b>", n*a*b, "</b> sujeitos (<b>", n, "</b> para cada grupo) ",
             "para detectar as diferenças mínimas de <b>", d_a, "</b> e <b>", d_b,
             " u.m.</b> nos efeitos principais do <i>", nome_fatorA(), "</i> (com <b>", a, "</b> níveis) ",
             " e de <b>", nome_fatorB(), "</b> (com <b>", b, "</b> níveis), respectivamente ",
             "(com o acréscimo de <b>", input$two_way_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas, "</b>). ",
             "O cálculo considerou poder de <b>", input$power_anova_n_two, "%</b>, nível de significância de <b>", input$sig_anova_n_two, "%</b>, ",

             if (s_a == s_b) {
               paste0(
                 "e desvios padrões esperados de <b>", s_a,  " u.m.</b> em ambos fatores"
               )
             } else {
               paste0(
                 "desvio padrão de <b>", s_a, " u.m.</b> para o <i>", nome_fatorA(),
                 "</i> e <b>", s_b, " u.m.</b> para o <i>", nome_fatorB(), "</i> "
               )
             },

             "(referido por Fulano (1900)). ",
             .txt_citacao_pss,
             .txt_referencia_tap,
             print_r_code(code)
      )
    }

  })





  #----------------.
  ## Interacao ----
  #----------------.


  output$k_anova_n_A_ui2 <- renderUI({
    numericInput( "k_anova_n_A2",
                  paste("Número de grupos de", input$two_nome_desfechoA2),
                  value = 3,
                  min = 2,
                  max = Inf,
                  step = 1
    ) %>% .help_buttom(linguagem = linguagem(), body = paste("Nº de grupos de", input$two_nome_desfechoA2, "para comparar"))
  })

  output$k_anova_n_B_ui2 <- renderUI({
    numericInput( "k_anova_n_B2",
                  paste("Número de grupos de", input$two_nome_desfechoB2),
                  value = 2,
                  min = 2,
                  max = Inf,
                  step = 1
    ) %>% .help_buttom(linguagem = linguagem(), body = paste("Nº de grupos de", input$two_nome_desfechoB2, "para comparar"))
  })

  output$f_anova_n_A_ui2 <- renderUI({
    numericInput( "f_anova_interacao_2",
                  paste(
                    "Magnitude do efeito da interação ", input$two_nome_desfechoA2, " * ", input$two_nome_desfechoB2,
                    " (Eta quadrado)"
                  ),
                  value = .5,
                  min = 0,
                  max = Inf,
                  step = .5
    ) %>% .help_buttom(linguagem = linguagem(), body = paste("Magnitude do efeito eta quadrado"))
  })




  # Confere inputs -----.

  eval(parse(text = warning_inteiro("k_anova_n_A2")))
  eval(parse(text = warning_inteiro("k_anova_n_B2")))

  eval(parse(text = warning_numero_positivo("f_anova_interacao_2")))
  eval(parse(text = warning_prop("power_anova_n_two2")))
  eval(parse(text = warning_prop("sig_anova_n_two2")))
  eval(parse(text = warning_perdas("two_way_perdas_recusa2")))



  eval(parse(text = warning_numero_positivo("desvio_anova_n22")))
  eval(parse(text = warning_prop("power_anova_n_two22")))
  eval(parse(text = warning_prop("sig_anova_n_two22")))
  eval(parse(text = warning_perdas("two_way_perdas_recusa22")))


  output$anova_n_two2 <- renderText({


    req(!(is.null(input$k_anova_n_A2) | is.null(input$k_anova_n_B2 | is.null(input$f_anova_interacao_2))))



    a = input$k_anova_n_A2
    b = input$k_anova_n_B2
    efeito_interacao = input$f_anova_interacao_2


    #    A funcao easypower::n.multiway nao permite extrair facilmente as informacoes
    # por isso foi necessario realizar um ajuste tecnico, popularmente conhecido como gambiarra,
    # para extrair a informacao do tamanho amostral

    code <- paste0("easypower::n.multiway(",
                   "iv1 = list(name = 'F1', levels = ", a, ", eta.sq = 0.1), ",
                   "iv2 = list(name = 'F2', levels = ", b, ", eta.sq = 0.1), ",
                   "interaction.eta2 = ", efeito_interacao, ", ",
                   "sig.level = ", input$sig_anova_n_two2/100, ", ",
                   "power = ", input$power_anova_n_two2/100, ")")

    out_n_temp <- capture.output(easypower::n.multiway(iv1 = list(name = "NNNNNN",
                                                                  levels = a, eta.sq = 0.1),
                                                       iv2 = list(name = "NNNNNNNNNNNN", levels = b, eta.sq = 0.1),
                                                       interaction.eta2 = efeito_interacao,
                                                       sig.level = input$sig_anova_n_two2/100,
                                                       power     = input$power_anova_n_two2/100))

    n <- tibble::tibble(texto = out_n_temp) %>%
      dplyr::filter(grepl(pattern = "NNNNNN\\*NNNNNNNNNNNN", texto)) %>%
      dplyr::pull() %>%
      strsplit("  ") %>%
      unlist() %>%
      dplyr::last() %>%
      as.numeric()
    n_perdas <- n_perdas(n, input$two_way_perdas_recusa2)

    paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n*a*b,
           " (<b>", n, "</b> por grupo de combinação).",
           "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

           "Foi calculado um tamanho de amostra de <b>", n*a*b, "</b> sujeitos (<b>", n, "</b> para cada grupo gerado pela combinação dos níveis dos dois fatores) ",
           "para detectar a magnitude de efeito eta quadrado de <b>", efeito_interacao, "</b> (referido por Fulano (1900) <b>OU</b> escolha do pesquisador) ",
           "do efeito da interação entre <i>", input$two_nome_desfechoA2, "</i> (com <b>", a, "</b> níveis) ",
           " e de <i>", input$two_nome_desfechoB2, "</i> (com <b>", b, "</b> níveis), respectivamente ",
           "(com o acréscimo de <b>", input$two_way_perdas_recusa2, "%</b> para possíveis perdas e recusas este número deve ser  <b>", n_perdas*a*b, "</b>). ",
           "O cálculo considerou poder de <b>", input$power_anova_n_two2, "%</b> e nível de significância de <b>", input$sig_anova_n_two2, "%</b>. ",
           .txt_citacao_pss,
           .txt_referencia_tap,
           print_r_code(code)
    )

  })






  #_____________-----
  # Qui-quadrado (ok) ----
  #------------------.


  mod_qui_quadrado_server(
    "tamanho_amostral_qui",
    tipo = "tamanho_amostral",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    txt_outros_desfechos = txt_outros_desfechos,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  mod_qui_quadrado_server(
    "poder_qui",
    tipo = "poder",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    txt_outros_desfechos = txt_outros_desfechos,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )

  output$aba_qui_quadrado <- renderUI({

    tagList(

      titlePanel(
        if (linguagem() == "pt") {
          "Teste de associação para variáveis qualitativas"
        } else {
          "Association test for qualitative variables"
        }
      ),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_qui_quadrado)),


      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_qui_quadrado_Ui("tamanho_amostral_qui"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_qui_quadrado_Ui("poder_qui"),
                 .rodape()
        )
      )
    )
  })




  #____________----
  # Correlacao (ok) ----



  mod_correlacao_server(
    "cor_tamanho_amostral",
    tipo = "tamanho_amostral",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_correlacao = warning_correlacao,
    wp.correlation = wp.correlation,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  mod_correlacao_server(
    "cor_poder",
    tipo = "poder",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_correlacao = warning_correlacao,
    wp.correlation = wp.correlation,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  mod_correlacao_server(
    "cor_estimar",
    tipo = "estimar",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_correlacao = warning_correlacao,
    wp.correlation = wp.correlation,
    lista_de_funcoes_server = lista_de_funcoes_server
  )




  output$aba_correlacao <- renderUI({

    tagList(

      titlePanel(translation_pss("Correlação", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_correlacao)),

      tabsetPanel(

        tabPanel(translation_pss("Estimar", linguagem()),
                 mod_correlacao_Ui("cor_estimar"),
                 .rodape()
        ),

        tabPanel(translation_pss("Testar", linguagem()),
                 mod_correlacao_Ui("cor_tamanho_amostral"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_correlacao_Ui("cor_poder"),
                 .rodape()
        )
      )
    )
  })






  #____________----
  # Linear (ok) ----



  mod_regressao_linear_server(
    "tamanho_amostral_linear",
    tipo = "tamanho_amostral",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  mod_regressao_linear_server(
    "poder_linear",
    tipo = "poder",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    warning_numero = warning_numero,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  output$aba_regressao_linear <- renderUI({

    tagList(

      titlePanel(translation_pss("Coeficiente de inclinação da reta para um modelo de regressão linear simples", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_reg_linear)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_regressao_linear_Ui("tamanho_amostral_linear"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_regressao_linear_Ui("poder_linear"),
                 .rodape()
        )
      )
    )
  })





  #____________----
  # Logistica (ok) ----


  mod_regressao_logistica_server(
    "tamanho_amostral_or",
    tipo = "tamanho_amostral",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  mod_regressao_logistica_server(
    "poder_or",
    tipo = "poder",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )

  mod_regressao_logistica_server(
    "estimar_or",
    tipo = "estimar",
    translation_pss = translation_pss,
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  output$aba_regressao_logistica <- renderUI({

    tagList(

      titlePanel(translation_pss("Regressão logística", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_reg_logistica)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_regressao_logistica_Ui("tamanho_amostral_or"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_regressao_logistica_Ui("poder_or"),
                 .rodape()
        ),

        tabPanel(translation_pss("Estimar", linguagem()),
                 mod_regressao_logistica_Ui("estimar_or"),
                 .rodape()
        )
      )
    )
  })










  #____-----
  # Cox (ok) ----
  #---------.



  mod_cox_server(
    "tamanho_amostral_cox",
    tipo = "tamanho_amostral",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )

  mod_cox_server(
    "poder_cox",
    tipo = "poder",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  output$aba_cox <- renderUI({

    tagList(

      titlePanel("Cox"),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_reg_cox)),

      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_cox_Ui("tamanho_amostral_cox"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_cox_Ui("poder_cox"),
                 .rodape()
        )
      )
    )
  })









  #___________------
  # Ferramentas -----


  # obter desvio padrao ----

  desvio_padrao_calc <- reactive({
    # https://handbook-5-1.cochrane.org/chapter_7/7_7_3_data_extraction_for_continuous_outcomes.htm

    ic    <- input$ferramentas_ic_ic
    xbar  <- input$ferramentas_ic_media
    n     <- input$ferramentas_ic_n
    alpha <- input$ferramentas_ic_conf/100


    if (input$ferramentes_desvio_padrao_statistic == "Intervalo de confiança") {

      t_alpha <- qt(1 - (1 - alpha)/2, n-1)

      sd_ <- (ic - xbar)*sqrt(n)/t_alpha
      sd_ <- abs(sd_)

    } else if (input$ferramentes_desvio_padrao_statistic == "Estatística t") {

      standard_error <-  (xbar - input$ferramentas_ic_t_h0)/input$ferramentas_ic_t
      sd_ <- standard_error*sqrt(n)
      sd_ <- abs(sd_)
    } else if (input$ferramentes_desvio_padrao_statistic == "Valor de p") {

      t_calc <- qt(p = input$ferramentas_ic_p/2,
                   df =  n - 1,
                   lower.tail = FALSE)

      standard_error <-  (xbar - input$ferramentas_ic_t_h0)/t_calc
      sd_ <- standard_error*sqrt(n)
      sd_ <- abs(sd_)
    } else if (input$ferramentes_desvio_padrao_statistic == "Erro padrão") {

      sd_ <- input$ferramentas_ep_erro_padrao*sqrt(n)


      # Imputing a change-from-baseline standard deviation using a correlation coefficient
    } else if (input$ferramentes_desvio_padrao_statistic == "Da diferença entre grupos pareados") {

      s1 <- input$ferramentas_sd_baseline
      s2 <- input$ferramentas_sd_follow

      temp <- s1^2 + s2^2 - (2*s1*s2*input$ferramentas_sd_correlation)

      sd_ <- sqrt(temp)
    }

    return(sd_)
  })





  # Imprimi os valores ------.
  output$ferramentas_desvio_padrao_valor <- renderText({

    paste0("<b><font size = '5'>",
           "<i>", translation_pss("Desvio padrão", linguagem()),"</i> = ",
           round(desvio_padrao_calc(), input$ferramentas_desvio_padrao_decimals),
           "<br><br>",
           "<i>", translation_pss("Variância", linguagem()),"</i> = ",
           round(desvio_padrao_calc()^2, input$ferramentas_desvio_padrao_decimals),
           "</b>")
  })






  # Imprimi as formulas ------.
  output$ferramentas_desvio_padrao_formulas <- renderUI({

    # Intervalo de confianca
    if (input$ferramentes_desvio_padrao_statistic == "Intervalo de confiança") {

      withMathJax(
        paste0("$$",
               "\\text{",
               translation_pss("Desvio padrão", linguagem()),
               "} = \\dfrac{\\text{|} ",
               translation_pss("IC", linguagem()),
               " - \\bar X \\text{|} }",
               "{t_{\\alpha/2, n-1}} \\sqrt n",
               "$$"
        )
      )


      # Estatistica t
    } else if (input$ferramentes_desvio_padrao_statistic == "Estatística t") {

      withMathJax(
        paste0("$$",
               "\\text{",
               translation_pss("Desvio padrão", linguagem()),
               "} = \\dfrac{\\bar X - \\mu_0 }",
               "{t_{\\alpha/2, n-1}} \\sqrt n",
               "$$"
        )
      )


      # Valor de p
    } else if (input$ferramentes_desvio_padrao_statistic == "Valor de p") {

      withMathJax(
        paste0("$$",
               "\\text{",
               translation_pss("Desvio padrão", linguagem()),
               "} = \\dfrac{\\bar X - \\mu_0 }",
               "{t_{\\alpha/2, n-1}} \\sqrt n",
               "$$"
        )
      )


      # Erro padrao
    } else if (input$ferramentes_desvio_padrao_statistic == "Erro padrão") {

      # sd_ <- input$ferramentas_ep_erro_padrao*sqrt(input$ferramentas_ep_n)
      #
      # withMathJax(
      #   paste0("$$\\text{Erro padrão} = \\dfrac{\\text{Desvio padrão}}{\\sqrt{\\text{n}}}",
      #          "\\to \\text{Desvio padrão} = \\text{Erro padrão} * \\sqrt{\\text{n}}",
      #          "= ", input$ferramentas_ep_erro_padrao, "*\\sqrt{", input$ferramentas_ep_n, "}",
      #          "\\cong ", round(sd_, trunc(input$ferramentas_ep_decimals)), "$$"
      #   )
      # )
      withMathJax(paste0("$$\\text{",
               translation_pss("Desvio padrão", linguagem()),
               "} = \\text{", translation_pss("Erro padrão", linguagem()),
               "} * \\sqrt{\\text{n}}$$"
      ))





      # Imputing a change-from-baseline standard deviation using a correlation coefficient
    } else if (input$ferramentes_desvio_padrao_statistic == "Da diferença entre grupos pareados") {

      withMathJax(paste0("$$\\text{",
                         translation_pss("Desvio padrão", linguagem()),
                         "}_{\\text{diferença}} = ",
                         "\\text{",
                         translation_pss("Desvio padrão", linguagem()),
                         "}_{\\text{1}}^2 ",
                         "+ \\text{",
                         translation_pss("Desvio padrão", linguagem()),
                         "}_{\\text{2}}^2",
                         " - (2 *\\rho * \\text{",
                         translation_pss("Desvio padrão", linguagem()),
                         "}_{\\text{1}} * ",
                         "\\text{",
                         translation_pss("Desvio padrão", linguagem()),
                         "}_{\\text{2}})}",
                         "$$"))


    }

  })















  # ferramentas_cohen -----
  # observeEvent(input$link_to_cohen, {
  #   updateTabsetPanel(session, "outras_ferramentas", "panel_d_cohen")
  # })


  output$ferramentas_cohen <- renderText({
    cohen <- cohen_d(mean_diff = input$cohen_mean_dif,
                     n_1 = input$cohen_n1,
                     n_2 = input$cohen_n2,
                     sd_1 = input$cohen_sigma1,
                     sd_2 =input$cohen_sigma2
    )

    paste0("<b><font size = '5'>Cohen'd = ", round(cohen, input$cohen_decimals), "</b>")

  })



  #pooled var -----

  output$pooled_var_sdUi <- renderUI({

    estat_ <- ifelse(
      input$pooled_eh_sd,
      translation_pss("Desvio padrão", linguagem()),
      translation_pss("Variância", linguagem())
    )

    fluidPage(
      HTML(
        paste0("<b><font size = '2.99'>", estat_, " </font></b><br>")
      ),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
          numericInput( "pooled_sigma1",
                        paste0("A"),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = .01)
      ),
      div(style="display: inline-block;vertical-align:top; width: 49%;",
          numericInput( "pooled_sigma2",
                        paste0("B"),
                        value = 1.4,
                        min = 0,
                        max = Inf,
                        step = .01)
      )
    )
  })



  output$ferramentas_pooled <- renderText({

    req(!is.null(input$pooled_sigma1))
    req(!is.null(input$pooled_sigma2))

    s2a <- ifelse(input$pooled_eh_sd, input$pooled_sigma1^2, input$pooled_sigma1)
    s2b <- ifelse(input$pooled_eh_sd, input$pooled_sigma2^2, input$pooled_sigma2)

    n1 <- ifelse(input$pooled_equal_size, 4, input$pooled_n1)
    n2 <- ifelse(input$pooled_equal_size, 4, input$pooled_n2)

    numerador   <- (n1 - 1)*s2a + (n2 - 1)*s2b
    denominador <- n1 + n2 - 2

    s_pooled <- sqrt(numerador/denominador)

    paste0("<b><font size = '5'>",
           "<i>", translation_pss("Desvio padrão", linguagem()), "<sub>", translation_pss("combinado", linguagem()), "</sub></i> = ", round(s_pooled, input$pooled_decimals),
           "<br><br>",
           "<i>", translation_pss("Variância", linguagem()), "<sub>", translation_pss("combinado", linguagem()), "</sub></i> = ", round(s_pooled^2, input$pooled_decimals),
           "</b>")

  })











  #_____ ----
  #AUC (ok)-----------



  mod_auc_server(
    "tamanho_amostral_auc",
    tipo = "tamanho_amostral",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  mod_auc_server(
    "poder_auc",
    tipo = "poder",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )



  mod_auc_server(
    "estimar_auc",
    tipo = "estimar",
    txt_ajuda = txt_ajuda,
    txt_balanceamento_f = txt_balanceamento_f,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  output$aba_auc <- renderUI({

    tagList(

      titlePanel(translation_pss("Área sob a curva ROC", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_auc)),

      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_auc_Ui("tamanho_amostral_auc"),
                 .rodape()
        ),

        tabPanel(translation_pss("Poder", linguagem()),
                 mod_auc_Ui("poder_auc"),
                 .rodape()
        ),

        tabPanel(translation_pss("Estimar", linguagem()),
                 mod_auc_Ui("estimar_auc"),
                 .rodape()
        )
      )
    )
  })






  #_____________________----
  # Sens/ especificidade -----


  output$aba_sensibilidade <- renderUI({

    tagList(
      titlePanel(translation_pss("Sensibilidade/ Especificidade", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_sensibilidade)),

      tabsetPanel(
        tabPanel(translation_pss("Estimar", linguagem()),
                 sidebarLayout(
                   sidebarPanel(

                     if (linguagem() == "en") {
                       p("If you want to estimate sensitivity only, set specificity equal to 0% or blank (vice versa). If both values are specified, the sample size will be based on the largest n.")
                     } else {
                       p("Se desejas estimar somente a sensibilidade, coloque especificidade igual a 0% ou em branco (vice-versa). ",
                         "Caso os dois valores sejam especificados, o tamanho amostral será com base no maior n.")
                     },

                     HTML(paste0(
                       "<b><font size = '2.9'>",
                       translation_pss("Valor esperado de", linguagem()),
                       ":</font></b><br>"
                     )),
                     div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                         numericInput( "sensibil_sensibilidade",
                                       translation_pss("sensibilidade", linguagem()),
                                       value = 75,
                                       min = 0,
                                       max = 100,
                                       step = 1)
                     ),
                     div(style = "display: inline-block;vertical-align:top; width: 49%;",
                         numericInput( "especif_especificidade",
                                       translation_pss("especificidade", linguagem()),
                                       value = 75,
                                       min = 0,
                                       max = 100,
                                       step = 1)
                     ),

                     actionLink("mudar_nomes_sens", translation_pss("Mudar nomes", linguagem())),
                     br(), br(),

                     numericInput( "sensibil_prevalencia",
                                   paste0(
                                     translation_pss("Percentual esperado (%)", linguagem()),
                                     " ",
                                     translation_pss("de", linguagem()),
                                     " ",
                                     nome_desfecho_sens()
                                   ),
                                   value = 60,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(),
                       body = paste0(txt_ajuda()$txt_perc_esperado, txt_ajuda()$txt_definido_literatura),
                       title = translation_pss("Percentual esperado (%)", linguagem())
                     ),


                     numericInput( "sensibil_amplitude",
                                   translation_pss("Amplitude do intervalo (%)", linguagem()),
                                   value = 20,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_amplitude, title = translation_pss("Amplitude do intervalo", linguagem())),
                     numericInput( "sensibil_confianca",
                                   translation_pss("Nível de confiança (%)", linguagem()),
                                   value = 95,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem())),
                     selectInput("sensibil_metodo",
                                 translation_pss("Método utilizado para calcular o intervalo de confiança", linguagem()),
                                 choices = c("wilson", "agresti-coull", "exact", "wald"),
                                 selected = "wald"
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_per_method_presize),
                     numericInput( "sensibil_perdas_recusa",
                                   translation_pss("Perdas/ Recusas (%)", linguagem()),
                                   value = 10,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
                   ),

                   mainPanel(
                     htmlOutput("sensibil_output") %>%
                       shinycssloaders::withSpinner(type = 5),


                     ###  CENARIOS  ####.
                     uiOutput("cenarios_sensi_espUi")
                   )
                 )
        )
      ),

      .rodape()

    )

  })

  ## Mudar nomes -----

  observeEvent(input$mudar_nomes_sens, {
    showModal(
      modalDialog(
        title = translation_pss("Ajustes", linguagem()),
        fluidPage(

          HTML(translation_pss("<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>.", linguagem())),
          br(), br(),
          textInput(inputId = "nome_desfecho_sens",
                    label   = translation_pss("Descreva o nome do desfecho", linguagem()),
                    value   = ifelse(input$mudar_nomes_sens == 0, "Y", nome_desfecho_sens())),
          HTML("<i>", gsub("<br><br>", "", txt_ajuda()$txt_desfecho), "</i>"),
          br(), br(),

          textInput(inputId = "nome_preditora_sens",
                    label   = translation_pss("Descreva o nome da variável preditora", linguagem()),
                    value   = ifelse(input$mudar_nomes_sens == 0, "X", nome_preditora_sens()))
        ),
        easyClose = TRUE,
        footer    = NULL
      )
    )
  })


  nome_desfecho_sens <- reactive({
    ifelse(is.null(input$nome_desfecho_sens), "Y", input$nome_desfecho_sens)
  })

  nome_preditora_sens <- reactive({
    ifelse(is.null(input$nome_preditora_sens), "X", input$nome_preditora_sens)
  })





  ## Texto -----

  # eval(parse(text = warning_prop("especif_especificidade")))
  # eval(parse(text = warning_prop("sensibil_sensibilidade")))
  eval(parse(text = warning_numero_positivo("sensibil_amplitude")))
  eval(parse(text = warning_prop("sensibil_prevalencia")))
  eval(parse(text = warning_prop("sensibil_confianca")))


  observeEvent(c(input$especif_especificidade, input$sensibil_sensibilidade), {
    shinyFeedback::hideFeedback("especif_especificidade")
    shinyFeedback::hideFeedback("sensibil_sensibilidade")

    if (is.na(input$especif_especificidade) & is.na(input$sensibil_sensibilidade)) {
      shinyFeedback::showFeedbackWarning(
        inputId = "especif_especificidade",
        text = translation_pss("Deve ser fornecido um valor.", linguagem()),
        color = "red"
      )
      shinyFeedback::showFeedbackWarning(
        inputId = "sensibil_sensibilidade",
        text = translation_pss("Deve ser fornecido um valor.", linguagem()),
        color = "red"
      )

    } else if (
      (input$especif_especificidade == 0 & input$sensibil_sensibilidade == 0) |
      (is.na(input$especif_especificidade) & input$sensibil_sensibilidade == 0) |
      (input$especif_especificidade == 0 & is.na(input$sensibil_sensibilidade))
    ) {
      shinyFeedback::showFeedbackWarning(
        inputId = "especif_especificidade",
        text = translation_pss("Deve ser fornecido um valor.", linguagem()),
        color = "red"
      )
      shinyFeedback::showFeedbackWarning(
        inputId = "sensibil_sensibilidade",
        text = translation_pss("Deve ser fornecido um valor.", linguagem()),
        color = "red"
      )
    }
  })

  output$sensibil_output <- renderText({

    req(
      !(
        (input$especif_especificidade == 0 & input$sensibil_sensibilidade == 0) |
          (is.na(input$especif_especificidade) & input$sensibil_sensibilidade == 0) |
          (input$especif_especificidade == 0 & is.na(input$sensibil_sensibilidade))
      )
    )

    especificidade     = input$especif_especificidade/100
    sensibilidade      = input$sensibil_sensibilidade/100
    prevalencia_doenca = input$sensibil_prevalencia/100
    amplitude          = input$sensibil_amplitude/100
    alpha              = input$sensibil_confianca/100
    metodo             = input$sensibil_metodo


    n_sens = n_espe = 0

    if (!is.na(sensibilidade)) {
      if (sensibilidade != 0) {
        code_sens <- paste0("presize::prec_sens(",
                            "sens = ", sensibilidade, ", ",
                            # "prev = ", prevalencia_doenca, ", ",
                            "conf.width = ", amplitude, ", ",
                            "conf.level = ", alpha, ", ",
                            "method = '", metodo, "')$n/",
                            prevalencia_doenca)

        n_sens <- eval(parse(text = code_sens))
        n_sens <- ceiling(n_sens)
      }
    }


    if (!is.na(especificidade)) {
      if (especificidade != 0) {
        code_esp <- paste0("presize::prec_spec(",
                           "spec = ", especificidade, ", ",
                           # "prev = ", prevalencia_doenca, ", ",
                           "conf.width = ", amplitude, ", ",
                           "conf.level = ", alpha, ", ",
                           "method = '", metodo, "')$n/(1 - ",
                           prevalencia_doenca, ")")
        n_espe <- eval(parse(text = code_esp))
        n_espe <- ceiling(n_espe)
      }
    }

    n <- ifelse(n_sens > n_espe, n_sens, n_espe)
    eval(parse(text = validate_n("n")))
    eval(parse(text = validate_n_inf("n")))

    metodo <- input$sensibil_metodo
    metodo <- paste(toupper(substr(metodo, 1, 1)), substr(metodo, 2, nchar(metodo)), sep = "")

    paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
           "</font></b></br></br>",

           lista_de_funcoes_server()$sugestao_texto_portugues(
             "<i>",
             translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",


             "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para estimar a ",
             if (n_sens != 0 & n_espe != 0) {
               "sensibilidade e a especificidade "
             } else if (n_sens != 0) {
               "e sensibilidade "
             } else {
               "e especificidade "
             },
             " do <i>", nome_preditora_sens(), "</i> para diagnosticar <i>", nome_desfecho_sens(), "</i> com <b>", input$sensibil_amplitude,
             "%</b> de amplitude para o intervalo de confiança ",
             "(com o acréscimo de <b>", input$sensibil_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$sensibil_perdas_recusa), "</b>). ",
             "O cálculo (utilizando o método de ", metodo, ") considerou nível de confiança de <b>", input$sensibil_confianca, "%</b>, ",
             "prevalência do <i>", nome_desfecho_sens(), "</i> de <b>", input$sensibil_prevalencia,

             # Se calculou para os dois
             if (n_sens != 0 & n_espe != 0) {

               if (input$sensibil_sensibilidade != input$especif_especificidade) {
                 paste0(
                   "%</b>, sensibilidade esperada de <b>", input$sensibil_sensibilidade, "%</b> ",
                   "e especificidade esperada de <b>", input$especif_especificidade, "%</b> "
                 )
               } else {
                 paste0(
                   "%</b>, sensibilidade e especificidade esperadas de <b>", input$sensibil_sensibilidade, "%</b> "
                 )
               }

               # se calculou so para sensibilidade
             } else if (n_sens != 0) {
               paste0(
                 "%</b>, sensibilidade esperada de <b>", input$sensibil_sensibilidade, "%</b> "
               )
             } else {
               # se calculou so para especificidade

               paste0(
                 "%</b>, especificidade esperada de <b>", input$especif_especificidade, "%</b> "
               )
             },

             "conforme referida em Fulano (1900) <b>OU</b> escolha do pesquisador. ",
             .txt_citacao_pss
           ),



           .txt_referencia_tap,

           if (n_sens != 0 & n_espe != 0) {
             lista_de_funcoes_server()$sugestao_texto_portugues("</br><br><i>Obs.:</i> n<sub>sens</sub> = ", n_sens, ", n<sub>esp</sub> = ", n_espe)
           },

           if (n_sens != 0 & n_espe != 0) {
             paste0("</br></br>",
                    if (linguagem() == "pt") {
                      "<i>Comando R utilizado:</i><br>"
                    } else {
                      "<i>R code:</i><br>"
                    },
                    "<p style=\"font-family:'Courier New';font-size:100% \">", code(paste0("n_sens <- ", code_sens)), "</p>",
                    "<p style=\"font-family:'Courier New';font-size:100% \">", code(paste0("n_esp <- ", code_esp)), "</p><br>")
           } else if (n_sens != 0) {
             print_r_code(code_sens)
           } else {
             print_r_code(code_esp)
           }
    )


  })





  ## Cenarios ----

  output$cenarios_sensi_espUi <- renderUI({

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

      fluidRow(
        column(6,
               textInput(
                 inputId = "sensibil_prev_plot",
                 label   = if (linguagem() == "pt") {
                   paste0(
                     "Digite valores para o ",
                     tolower(
                       translation_pss("Percentual esperado (%)", linguagem())
                     ),
                     " ",
                     translation_pss("de", linguagem()),
                     " ",
                     nome_desfecho_sens(),
                     " para fazer o gráfico"
                   )
                 } else {
                   paste0(
                     "Enter values to ",
                     tolower(
                       translation_pss("Percentual esperado (%)", linguagem())
                     ),
                     " ",
                     translation_pss("de", linguagem()),
                     " ",
                     nome_desfecho_sens(),
                     " to plot"
                   )
                 },
                 value   = paste0(c(input$sensibil_prevalencia, input$sensibil_prevalencia + 2.5, input$sensibil_prevalencia + 5), collapse = ", "),
                 width   = "400px") %>%
                 .help_buttom(linguagem = linguagem(), body = ajuda_cenarios_multiplos_valores())
        )
      ),



      HTML(
        "<b>",
        translation_pss("Defina a sequência de valores para a sensibilidade/ especificidade (%)", linguagem()),
        "</b>"
      ),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("sensibil_from", translation_pss("Mínimo", linguagem()), value = 5, step = 5)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("sensibil_to", translation_pss("Máximo", linguagem()), value = 95, step = 5)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("sensibil_by", translation_pss("Intervalo", linguagem()), value = 10, min = 0, step = 1) %>%
            .help_buttom(linguagem = linguagem(), body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()),
                         title = "Sequência")
      ),
      br(),

      plotly::plotlyOutput("sensibil_plot", width = "80%"),
      br(), br(),
      downloadButton("download_sensibil_tab",translation_pss("Download tabela", linguagem())),
      DT::dataTableOutput("sensibil_tab", width = "100%")


    ))
  })


  eval(parse(text = check_text_input_to_vector("sensibil_prev_plot")))


  tab_sensibil_th_cenarios <- reactive({


    n_ <- function(alpha, sens_esp, precisao, prevalencia, method, n_sen = TRUE) {
      if (n_sen) {
        n <- tryCatch({
          presize::prec_sens(sens = sens_esp,
                             # prev = prevalencia,
                             conf.width = precisao,
                             conf.level = alpha,
                             method = method)$n/prevalencia},
          warning = function(warning_condition) { NA },
          error = function(error_condition) { NA })

      } else{
        n <- tryCatch({
          presize::prec_spec(spec = sens_esp,
                             # prev = prevalencia,
                             conf.width = precisao,
                             conf.level = alpha,
                             method = method)$n/(1 - prevalencia)},
          warning = function(warning_condition) { NA },
          error = function(error_condition) { NA })
      }

      if (class(n) == "logical") {
        NA_real_
      } else{
        ceiling(n)
      }
    }



    prevalencias <- text_input_to_vector(input$sensibil_prev_plot)
    req(length(prevalencias) > 0)

    df_grid <- expand.grid(
      `Método` = input$sensibil_metodo,
      `Prevalência (%)`   = prevalencias,
      `Sensibilidade/ especificidade (%)` = seq(input$sensibil_from, input$sensibil_to, input$sensibil_by),
      `Amplitude (%)`      = input$sensibil_amplitude,
      `Nível de confiança (%)` = input$sensibil_confianca,
      stringsAsFactors = FALSE
    )

    df_grid %>%
      mutate(
        n_sens = mapply(
          function(alpha, sens_esp, precisao, prevalencia, method) {
            n_(alpha, sens_esp, precisao, prevalencia, method, TRUE)
          },
          `Nível de confiança (%)`/100, `Sensibilidade/ especificidade (%)`/100, `Amplitude (%)`/100, `Prevalência (%)`/100, `Método`
        ),

        n_espec = mapply(
          function(alpha, sens_esp, precisao, prevalencia, method) {
            n_(alpha, sens_esp, precisao, prevalencia, method, FALSE)
          },
          `Nível de confiança (%)`/100, `Sensibilidade/ especificidade (%)`/100, `Amplitude (%)`/100, `Prevalência (%)`/100, `Método`
        ),

        n_maior = pmax(n_sens, n_espec)
      ) %>%
      dplyr::filter(!is.na(n_maior))
  })


  tab_sensibil_th_cenarios_print <- reactive({
    df <- tab_sensibil_th_cenarios() %>%
      dplyr::select(-c(n_sens, n_espec)) %>%
      dplyr::rename(n = n_maior)

    colnames(df) <- c(
      translation_pss("Método utilizado para calcular o intervalo de confiança", linguagem()),
      paste0(
        translation_pss("Percentual esperado (%)", linguagem()),
        " ",
        translation_pss("de", linguagem()),
        " ",
        nome_desfecho_sens()
      ),
      translation_pss("Sensibilidade/ Especificidade", linguagem()),
      translation_pss("Amplitude do intervalo (%)", linguagem()),
      translation_pss("Nível de confiança (%)", linguagem()),
      translation_pss("Tamanho amostral", linguagem())
    )

    df
  })



  output$sensibil_plot <- plotly::renderPlotly({


    g1 <- tab_sensibil_th_cenarios() %>%
      mutate(`Prevalência (%)` = factor(`Prevalência (%)`)) %>%
      ggplot(aes(y = n_maior,
                 x = `Sensibilidade/ especificidade (%)`,
                 colour = `Prevalência (%)`,
                 n_sens = n_sens,
                 n_espec = n_espec))+
      geom_point() +
      geom_line() +
      xlab(translation_pss("Sensibilidade/ Especificidade", linguagem())) +
      ylab(translation_pss("Tamanho da amostra*", linguagem())) +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(
        name = paste0(
          translation_pss("Percentual esperado (%)", linguagem()),
          " ",
          translation_pss("de", linguagem()),
          " ",
          nome_desfecho_sens()
        ),
        palette = "Set1"
      )



    plotly::ggplotly(g1,
                     tooltip = c("x", "colour", "n_sens", "n_espec")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))

  })




  output$sensibil_tab <- DT::renderDataTable({
    tab_sensibil_th_cenarios_print() %>%
      DT::datatable(extensions = c('FixedColumns'),
                    rownames   = FALSE,
                    filter     = "none",
                    # #callback   = DT::JS("$('div.dwnld').append($('#download_auc_tab'));"),
                    options    = list(pageLength = 10,
                                      scrollX = TRUE,
                                      scrollY = TRUE,
                                      searching = FALSE,
                                      fixedColumns = list(leftColumns = 1),
                                      dom = 'B<"dwnld">frtip'))
  })


  output$download_sensibil_tab <- downloadHandler(
    filename = function() { "Cenarios_Sensibilidade_Especificidade.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_sensibil_th_cenarios_print(),
                                                  path = file)}
  )






  # aba_especificidade ----.



  # output$especif_output <- renderText({
  #   # https://www.ncbi.nlm.nih.gov/pubmed/8870764/
  #
  #   especificidade     = input$especif_especificidade/100
  #   prevalencia_doenca = input$especif_prevalencia/100
  #   amplitude          = input$especif_amplitude/100
  #   alpha              = 1 - input$especif_confianca/100
  #
  #
  #   n <- (qnorm(1 - alpha/2)^2 * (especificidade*(1-especificidade))/amplitude^2)/(1 - prevalencia_doenca)
  #   n <- ceiling(n)
  #
  #
  #   paste0("<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
  #          "</font></b></br></br><i>", translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",
  #
  #
  #          "Foi realizado o cálculo do tamanho de amostra para estimar a especificidade do ",
  #          "<b><i>teste tal</i></b> para diagnosticar <b><i>tal desfecho</b></i>, ",
  #          "utilizando as fómulas descritas por Buderer, N.M.F. (1996). ",
  #          "Considerando nível de confiança de <b>", input$especif_confianca, "%</b>, precisão desejada de <b>", input$especif_amplitude,
  #          "%</b>, prevalência do desfecho de <b>", input$especif_prevalencia,
  #          "%</b> e especificidade esperada de <b>", input$especif_sensibilidade, "%</b>, conforme referida em Fulano (1900),",
  #          " chegou-se ao tamanho de amostra total de <b>", n, "</b> sujeitos. Acrescentando <b>", input$especif_perdas_recusa,
  #          "%</b> para possíveis perdas e recusas o tamanho de amostra deverá ser <b>", n_perdas(n, input$especif_perdas_recusa), "</b>.",
  #
  #
  #
  #          "</br><br><br><br>
  #          Buderer, N.M.F. (1996), Statistical Methodology: I. Incorporating the Prevalence of Disease into the Sample Size Calculation for Sensitivity and Specificity. Academic Emergency Medicine, 3: 895-900. ",
  #          "<a href=' https://doi.org/10.1111/j.1553-2712.1996.tb03538.x' target='_blank'>  https://doi.org/10.1111/j.1553-2712.1996.tb03538.x </a>."
  #
  #   )
  #
  #
  # })






  #_______-----
  # Kappa (ok)   ----
  #---------------------.



  mod_kappa_server(
    "tamanho_amostral_kappa",
    tipo = "tamanho_amostral",
    txt_ajuda = txt_ajuda,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    erro_painel_principal = erro_painel_principal,
    lista_de_funcoes_server = lista_de_funcoes_server
  )




  mod_kappa_server(
    "estimar_kappa",
    tipo = "estimar",
    txt_ajuda = txt_ajuda,
    translation_pss = translation_pss,
    linguagem = linguagem,
    .rodape   = .rodape,
    validate_n = validate_n,
    ajuda_cenarios_multiplos_valores = ajuda_cenarios_multiplos_valores,
    validate_n_inf = validate_n_inf,
    try_n = try_n,
    n_perdas = n_perdas,
    print_r_code =  print_r_code,
    text_input_to_vector = text_input_to_vector,
    check_text_input_to_vector = check_text_input_to_vector,
    warning_prop = warning_prop,
    warning_numero_positivo = warning_numero_positivo,
    warning_inteiro = warning_inteiro,
    warning_perdas = warning_perdas,
    erro_painel_principal = erro_painel_principal,
    lista_de_funcoes_server = lista_de_funcoes_server
  )


  output$aba_kappa <- renderUI({

    tagList(

      titlePanel(translation_pss("Kappa de Cohen", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_kappa)),

      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 mod_kappa_Ui("tamanho_amostral_kappa"),
                 .rodape()
        ),
        tabPanel(translation_pss("Estimar", linguagem()),
                 mod_kappa_Ui("estimar_kappa"),
                 .rodape()
        )
      )
    )
  })





  #______----
  #   ICC      ----
  #----------------------.



  output$aba_icc <- renderUI({

    tagList(
      titlePanel(translation_pss("Coeficiente de correlação intraclasse", linguagem())),
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_icc)),
      tabsetPanel(
        tabPanel(translation_pss("Testar", linguagem()),
                 sidebarLayout(
                   sidebarPanel(
                     wellPanel(
                       HTML(
                         paste0(
                           "<b><font size = '2.8'> ", translation_pss("Hipóteses a serem testadas", linguagem()), " </font></b>"
                         )
                       ),
                       uiOutput("icc_th_formula"),
                     ),
                     numericInput( "icc_icc_esperado",
                                   "ICC",
                                   value = 0.6,
                                   min = 0,
                                   max = 1,
                                   step = .1
                     ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Valor do coeficiente de correlação intraclasse que se espera encontrar.", txt_ajuda()$txt_definido_pesquisador_OU_literatura)),
                     numericInput( "icc_h0",
                                   translation_pss("Valor de referência sob a hipótese nula", linguagem()),
                                   value = 0.5,
                                   min = 0,
                                   max = 1,
                                   step = .1
                     ) %>% .help_buttom(linguagem = linguagem(), body = paste0("ICC para testar em H0", txt_ajuda()$txt_definido_pesquisador)),
                     numericInput( "icc_ratings",
                                   translation_pss("Número de avaliadores", linguagem()),
                                   value = 2,
                                   min = 2,
                                   max = Inf,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Número de avaliadores por unidade amostral.", txt_ajuda()$txt_definido_pesquisador)),
                     numericInput( "icc_power",
                                   translation_pss("Poder (%)", linguagem()),
                                   value = 80,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_power, title = translation_pss("Poder (%)", linguagem())),
                     numericInput( "icc_significancia",
                                   translation_pss("Nível de significância (%)", linguagem()),
                                   value = 5,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_significancia, title = translation_pss("Nível de significância (%)", linguagem())),
                     selectInput(inputId = "icc_sided",
                                 translation_pss('Tipo de teste de acordo com hipótese alternativa:', linguagem()),
                                 choices = h1(),
                                 selected = 'Bilateral'
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_h1),
                     numericInput( "icc_perdas_recusa",
                                   translation_pss("Perdas/ Recusas (%)", linguagem()),
                                   value = 10,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
                   ),

                   mainPanel(
                     htmlOutput("icc_output") %>%
                       shinycssloaders::withSpinner(type = 5),

                     ###  CENARIOS  ####.
                     uiOutput("cenarios_icc_thUi")
                   )
                 )
        ),

        tabPanel(translation_pss("Estimar", linguagem()),
                 sidebarLayout(
                   sidebarPanel(
                     numericInput( "icc_est_icc_esperado",
                                   "ICC esperado",
                                   value = 0.70,
                                   min = 0,
                                   max = 1,
                                   step = .1
                     ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Valor do coeficiente de correlação intraclasse que se espera encontrar.", txt_ajuda()$txt_definido_pesquisador_OU_literatura)),
                     numericInput( "icc_est_amplitude",
                                   translation_pss("Amplitude do intervalo", linguagem()),
                                   value = 0.2,
                                   min = 0,
                                   max = 1,
                                   step = .1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_amplitude, title = translation_pss("Amplitude do intervalo", linguagem())),
                     numericInput( "icc_est_ratings",
                                   translation_pss("Número de avaliadores", linguagem()),
                                   value = 2,
                                   min = 2,
                                   max = Inf,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = paste0("Número de avaliadores por unidade amostral.", txt_ajuda()$txt_definido_pesquisador)),
                     numericInput( "icc_est_confiança",
                                   translation_pss("Nível de confiança (%)", linguagem()),
                                   value = 95,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem())),

                     numericInput( "icc_est_perdas_recusa",
                                   translation_pss("Perdas/ Recusas (%)", linguagem()),
                                   value = 10,
                                   min = 0,
                                   max = 100,
                                   step = 1
                     ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem()))
                   ),

                   mainPanel(
                     htmlOutput("icc_est_output") %>%
                       shinycssloaders::withSpinner(type = 5)
                   )
                 )
        )
      ),

      .rodape()
    )

  })


  # Testar ----


  output$icc_th_formula <- renderUI({

    sinal_h0 <- ifelse(input$icc_sided == "Bilateral", "=", "\\leq")
    sinal_h1 <- ifelse(input$icc_sided == "Bilateral", "\\neq", ">")

    withMathJax(
      paste0("$$H_0: ICC ", sinal_h0, input$icc_h0,
             " \\text{  vs  } H_1: ICC ", sinal_h1, input$icc_h0, "$$"))
  })



  eval(parse(text = warning_inteiro("icc_ratings")))
  eval(parse(text = warning_prop("icc_icc_esperado", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("icc_h0", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("icc_power")))
  eval(parse(text = warning_prop("icc_significancia")))
  eval(parse(text = warning_perdas("icc_perdas_recusa")))



  output$icc_output <- renderText({

    req(!(is.null(input$icc_sided)))


    sided   <- ifelse(input$icc_sided == "Bilateral", 2, 1)
    methods <- sub(",([^,]*)$", " e\\1", paste(LETTERS[1:input$icc_ratings], collapse = ", "))

    code <- paste0(
      "ICC.Sample.Size::calculateIccSampleSize(",
      "p  = ", input$icc_icc_esperado, ", ",
      "p0 = ", input$icc_h0, ", ",
      "k  = ", input$icc_ratings, ", ",
      "alpha = ", input$icc_significancia, "/100, ",
      "power = ", input$icc_power, "/100, ",
      "tails = ", sided, ")"
    )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    n <- n[[1]]$N
    eval(parse(text = validate_n_inf("n")))


    paste0(
      "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n, "</font></b></br></br>",

      lista_de_funcoes_server()$sugestao_texto_portugues(
        "<i>",
        translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

        "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para testar se o coeficiente de correlação intraclasse, que avaliará a ",
        "concordância entre os <b>métodos/ avaliadores ", methods, "</b>, ",
        if (sided == 2) {
          paste0("é diferente de <b>", input$icc_h0, "</b>")
        } else{
          paste0("é maior do que <b>", input$icc_h0, "</b>")
        },
        " (com o acréscimo de <b>", input$icc_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$icc_perdas_recusa), "</b>). ",

        "O cálculo considerou poder de <b>", input$icc_power, "%</b>, nível de significância de <b>", input$icc_significancia, "%</b> ",
        " e coeficiente de correlação intraclasse esperado de <b>", input$icc_icc_esperado, "</b> ",
        "conforme referido em Fulano (1900) OU escolha do pesquisador. ",
        .txt_citacao_pss
      ),
      .txt_referencia_tap,
      print_r_code(code)
    )


  })



  ## Cenarios ----

  output$cenarios_icc_thUi <- renderUI({

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

      fluidRow(
        column(6,
               textInput(inputId = "icc_power_plot",
                         label   = translation_pss("Digite valores de poder (%) para fazer o gráfico", linguagem()),
                         value   = "80, 90, 95",
                         width   = "400px") %>%
                 .help_buttom(linguagem = linguagem(), body = ajuda_cenarios_multiplos_valores())
        )
      ),

      HTML(
        "<b>",
        translation_pss("Defina a sequência de valores para o ICC", linguagem()),
        "</b>"
      ),
      br(),
      div(style="display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
          numericInput("icc_from", translation_pss("Mínimo", linguagem()), value = input$icc_icc_esperado, step = 0.05)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("icc_to", translation_pss("Máximo", linguagem()), value = min(1, input$icc_icc_esperado + 0.2), step = 0.05)
      ),
      div(style="display: inline-block;vertical-align:top; width: 80px;",
          numericInput("icc_by", translation_pss("Intervalo", linguagem()), value = 0.05, min = 0, step = 0.05) %>%
            .help_buttom(linguagem = linguagem(), body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()),
                         title = "Sequência")
      ),
      br(),

      plotly::plotlyOutput("icc_plot", width = "80%"),
      br(), br(),
      downloadButton("download_icc_tab",translation_pss("Download tabela", linguagem())),
      DT::dataTableOutput("icc_tab", width = "100%")


    ))
  })

  eval(parse(text = check_text_input_to_vector("icc_power_plot")))


  tab_icc_th_cenarios <- reactive({
    poder <- text_input_to_vector(input$icc_power_plot)
    req(length(poder) > 0)

    sided   <- ifelse(input$icc_sided == "Bilateral", 2, 1)

    df_grid <- expand.grid(`Nível de significância (%)` = input$icc_significancia,
                           `Poder (%)` = poder,
                           ICC         = seq(input$icc_from, input$icc_to, input$icc_by),
                           raters      = input$icc_ratings,
                           icc_h0      = input$icc_h0,
                           tails       = sided,
                           taisF       = input$icc_sided,
                           stringsAsFactors = FALSE)


    df_grid %>%
      mutate(
        n = mapply(
          function(p, p0, k, alpha, power, tails) {

            n <- tryCatch({
              ICC.Sample.Size::calculateIccSampleSize(
                p = p,
                p0 = p0,
                k = k,
                alpha = alpha/100,
                power = power/100,
                tails = tails)[[1]]$N
            }, warning = function(warning_condition) {
              NA
            }, error = function(error_condition) {
              NA
            })
          }, ICC, icc_h0, raters, `Nível de significância (%)`, `Poder (%)`, tails

        )
      ) |>
      dplyr::select(-tails)
  })



  output$icc_plot <- plotly::renderPlotly({

    req(!is.null(tab_icc_th_cenarios()))
    data <- tab_icc_th_cenarios() %>%
      mutate(`Poder (%)` = factor(`Poder (%)`))

    g1 <- ggplot(data, aes(x = ICC,
                           y = n,
                           colour = `Poder (%)`))+
      geom_point() +
      geom_line() +
      xlab("ICC") +
      ylab(translation_pss("Tamanho da amostra*", linguagem())) +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(
        name = translation_pss("Poder (%)", linguagem()),
        palette = "Set1"
      )



    plotly::ggplotly(g1,
                     tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))

  })


  tab_icc_th_cenarios_download <- reactive({
    df <- tab_icc_th_cenarios()

    colnames(df) <- c(
      translation_pss("Nível de significância (%)", linguagem()),
      translation_pss("Poder (%)", linguagem()),
      "ICC",
      translation_pss( "Número de avaliadores", linguagem()),
      translation_pss("Valor de referência sob a hipótese nula", linguagem()),
      translation_pss("Hipótese alternativa", linguagem()),
      translation_pss("Tamanho amostral", linguagem())
    )
    df
  })


  output$icc_tab <- DT::renderDataTable({
    tab_icc_th_cenarios_download() %>%
      DT::datatable(extensions = c('FixedColumns'),
                    rownames   = FALSE,
                    filter     = "none",
                    # #callback   = DT::JS("$('div.dwnld').append($('#download_auc_tab'));"),
                    options    = list(pageLength = 10,
                                      scrollX = TRUE,
                                      scrollY = TRUE,
                                      searching = FALSE,
                                      fixedColumns = list(leftColumns = 1),
                                      dom = 'B<"dwnld">frtip'))
  })


  output$download_icc_tab <- downloadHandler(
    filename = function() { "Cenarios_testar_ICC.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_icc_th_cenarios_download(), path = file)}
  )


  # Estimar ----




  eval(parse(text = warning_prop("icc_est_icc_esperado", entre0e1 = TRUE)))
  eval(parse(text = warning_prop("icc_est_confiança")))
  eval(parse(text = warning_numero_positivo("icc_est_amplitude")))
  eval(parse(text = warning_perdas("icc_est_perdas_recusa")))


  output$icc_est_output <- renderText({

    code <- paste0("presize::prec_icc(",
                   "rho  = ", input$icc_est_icc_esperado, ", ",
                   "k  = ", input$icc_est_ratings, ", ",
                   "conf.level = ", input$icc_est_confiança, "/100, ",
                   "conf.width = ", input$icc_est_amplitude, ")"
    )

    n <- try_n(code)
    eval(parse(text = validate_n("n")))

    methods <- sub(",([^,]*)$", " e\\1", paste(LETTERS[1:input$icc_est_ratings], collapse = ", "))
    n <- n$n
    eval(parse(text = validate_n_inf("n")))


    paste0(
      "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n, "</font></b></br></br>",

      lista_de_funcoes_server()$sugestao_texto_portugues(
        "<i>",
        translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

        "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para estimar o coeficiente de correlação intraclasse, que avaliará a ",
        "concordância entre os <b>métodos/ avaliadores ", methods,
        "</b>, com amplitude desejada para o intervalo de confiança de de <b>", input$icc_est_amplitude, "</b> ",
        "(com o acréscimo de <b>", input$icc_est_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$icc_est_perdas_recusa), "</b>). ",

        "O cálculo considerou nível de significância de <b>", input$icc_est_confiança, "%</b> ",
        " e coeficiente de correlação intraclasse esperado de <b>", input$icc_est_icc_esperado, "</b> ",
        "conforme referido em Fulano (1900) OU escolha do pesquisador. ",
        .txt_citacao_pss
      ),
      .txt_referencia_tap,
      print_r_code(code)
    )


  })





  #___________----
  # Bland Altman ----



  output$aba_estimacao_bland <- renderUI({

    tagList(
      wellPanel(HTML(txt_ajuda()$wellPanel_txt_bland)),

      sidebarLayout(
        sidebarPanel(
          numericInput( "bland_amplitude",
                        translation_pss("Amplitude do intervalo", linguagem()),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = 0.5
          ) %>%
            shinyhelper::helper(type = "inline",
                                title = translation_pss("Amplitude do intervalo", linguagem()),
                                content = includeMarkdown(file.path("www", "Bland_altman_plot.md")),
                                buttonLabel = "Fechar",
                                fade = TRUE,
                                colour = "#006338",
                                size = "l"),

          # .help_buttom(linguagem = linguagem(), body = "É a amplitude do intervalo de confiança (limite superior menos limite inferior)."),
          numericInput( "bland_confianca",
                        translation_pss("Nível de confiança (%)", linguagem()),
                        value = 95,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem())),
          numericInput( "bland_perdas_recusa",
                        translation_pss("Perdas/ Recusas (%)", linguagem()),
                        value = 10,
                        min = 0,
                        max = 100,
                        step = 1
          ) %>% .help_buttom(linguagem = linguagem(), body = txt_ajuda()$txt_perdas_recusas, title = translation_pss("Perdas/ Recusas (%)", linguagem())),
        ),

        mainPanel(
          shinycssloaders::withSpinner(htmlOutput("bland_est"), type = 5),

          ###  CENARIOS  ####.


          br(),
          HTML('<hr style="color: black;">'),
          br(),br(),

          titlePanel(translation_pss("Construção de cenários", linguagem())),
          br(),

          wellPanel(translation_pss(
            "Utilize os argumentos abaixo para construir diferentes cenários. Demais informações serão recuperadas do painel lateral.",
            linguagem())
          ),

          fluidRow(
            column(6,
                   textInput(inputId = "bland_cenarios_confianca",
                             label   = translation_pss("Digite valores de nível de confiança (%) para fazer o gráfico", linguagem()),
                             value   = "90, 95, 99",
                             width   = "400px") %>%
                     .help_buttom(linguagem = linguagem(), body = ajuda_cenarios_multiplos_valores())
            )
          ),

          HTML(
            "<b>",
            translation_pss("Defina a sequência de valores para a amplitude do intervalo de confiança", linguagem()),
            "</b>"
          ),
          br(),
          div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 80px;",
              numericInput("bland_from", translation_pss("Mínimo", linguagem()), value = 0.3, step = 0.5)
          ),
          div(style = "display: inline-block;vertical-align:top; width: 80px;",
              numericInput("bland_to", translation_pss("Máximo", linguagem()), value = 1.1, step = 0.5)
          ),
          div(style = "display: inline-block;vertical-align:top; width: 80px;",
              numericInput("bland_by", translation_pss("Intervalo", linguagem()), value = 0.1, min = 0, step = 0.1) %>%
                .help_buttom(linguagem = linguagem(), body = translation_pss("Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>.", linguagem()),
                             title = "Sequência")
          ),
          br(),

          plotly::plotlyOutput("bland_cenarios_plot", width = "80%"),
          br(), br(),
          downloadButton("bland_cenarios_download",translation_pss("Download tabela", linguagem())),
          DT::dataTableOutput("bland_cenarios_tab", width = "100%")

        )
      ),

      .rodape()
    )
  })


  eval(parse(text = warning_numero_positivo("bland_amplitude")))
  eval(parse(text = warning_prop("bland_confianca")))
  eval(parse(text = warning_perdas("bland_perdas_recusa")))



  output$bland_est <- renderText({

    code <- paste0("presize::prec_lim_agree(n = NULL, ",
                   "conf.width = ", input$bland_amplitude, ", ",
                   "conf.level = ", input$bland_confianca, "/100)"
    )

    n <- eval(parse(text = code))
    eval(parse(text = validate_n("n")))

    n <- ceiling(n$n)
    eval(parse(text = validate_n_inf("n")))
    eval(parse(text = validate_n_inf("n")))



    paste0(
      "<b><font size = '5'>", translation_pss("Tamanho amostral calculado", linguagem()), ": ", n,
      "</font></b></br></br>",

      lista_de_funcoes_server()$sugestao_texto_portugues(
        "<i>",
        translation_pss("Sugestão de texto", linguagem()), ":</i></br></br>",

        "Foi calculado um tamanho de amostra de <b>", n, "</b> sujeitos para estimar os limites de concordância do gráfico de Bland-Altman, ",
        "entre os métodos X e Y, com uma amplitude do intervalo de confiança desejada de <b>", input$bland_amplitude, "</b> ",
        "(com o acréscimo de <b>", input$bland_perdas_recusa, "%</b> para possíveis perdas e recusas este número deve ser <b>", n_perdas(n, input$bland_perdas_recusa), "</b>). ",
        "O cálculo considerou nível de confiança de <b>", input$bland_confianca, "%</b> e foi realizado por meio da ferramenta PSS Health versão ",
        ifelse(!.versao_online, packageVersion("PSS.Health"), "on-line"),
        " (citação abaixo).",
        .txt_referencia_tap
      ),
      .txt_referencia_tap,
      print_r_code(code)
    )

  })


  ## Cenarios ----


  eval(parse(text = check_text_input_to_vector("bland_cenarios_confianca")))


  tab_bland_cenarios <- reactive({

    cenarios_confianca <- text_input_to_vector(input$bland_cenarios_confianca)
    req(length(cenarios_confianca) > 0)


    expand.grid(Amplitude = seq(from = input$bland_from, to = input$bland_to, by = input$bland_by),
                `Nível de confiança (%)` = cenarios_confianca) %>%
      mutate(`Tamanho da amostra` = mapply(
        function(conf.width, conf.level) { presize::prec_lim_agree(n = NULL, conf.width = conf.width, conf.level = conf.level/100)$n },
        Amplitude, `Nível de confiança (%)`),
        `Tamanho da amostra`   = ceiling(`Tamanho da amostra`)
      )
  })



  output$bland_cenarios_plot <- plotly::renderPlotly({


    g1 <- tab_bland_cenarios() %>%
      mutate(`Nível de confiança (%)` = factor(`Nível de confiança (%)`)) %>%
      ggplot(aes(x = Amplitude, y = `Tamanho da amostra`, color = `Nível de confiança (%)`))+
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(from = input$bland_from, to = input$bland_to, by = input$bland_by)) +
      xlab(translation_pss("Amplitude do intervalo", linguagem())) +
      ylab(translation_pss("Tamanho da amostra*", linguagem())) +
      theme_bw() +
      theme(axis.text = element_text(colour = "black")) +
      scale_color_brewer(
        name = translation_pss("Nível de confiança (%)", linguagem()),
        palette = "Set1"
      )

    plotly::ggplotly(g1, tooltip = c("x", "colour", "y")) %>%
      plotly::layout(annotations = list(x = 1, y = -0.1, text = translation_pss("* sem considerar perdas/ recusas.", linguagem()),
                                        showarrow = F, xref='paper', yref='paper',
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=10)))
  })





  tab_bland_cenarios_print <- reactive({

    df <- tab_bland_cenarios() %>%
      dplyr::select(
        Amplitude, `Nível de confiança (%)`, `Tamanho da amostra`
      )

    colnames(df) <- c(
      translation_pss("Amplitude do intervalo", linguagem()),
      translation_pss("Nível de confiança (%)", linguagem()),
      translation_pss("Tamanho amostral", linguagem())
    )

  })


  output$bland_cenarios_tab <- DT::renderDataTable({


    tab_bland_cenarios_print() %>%
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


  output$bland_cenarios_download <- downloadHandler(
    filename = function() { "Cenarios_tamanho_amostra_bland.xlsx"},
    content = function(file) {writexl::write_xlsx(tab_bland_cenarios_print(), path = file)}
  )




  #+++++++++++-----------
  #+++++++++++-----------
  # UI ---------------
  #+++++++++++-----------
  #+++++++++++-----------

  output$navbarMenu_bemvindo <- renderText({
    translation_pss("Boas vindas!", linguagem())
  })

  output$pssVersaoInicio <- renderUI({
    HTML(paste0(
      "<b>P</b>ower and <b>S</b>ample <b>S</b>ize for Health Researchers (",

      if (linguagem() == "pt") {
        "versão "
      } else{
        "version "
      },


      if (!.versao_online) {
        packageVersion("PSS.Health")
      } else {
        "on-line"
      }, ")<br><br><br>")
    )
  })



  output$texto_pagina_inicial_pt <- renderUI({

    req(linguagem() == "pt")

    tagList(
      HTML("Navegue entre as abas para encontrar o cenário correspondente ao objetivo do estudo. Altere os parâmetros conforme necessidade e utilize o texto como auxílio para entender o cálculo.",
           "<br>",
           "<b>Sempre</b> procure um profissional de estatística para orientações no planejamento do estudo.",
           ""),

      br(),br(), br(),
      if (.versao_online) {
        tagList(
          HTML(paste0("O PSS Health está disponível no ",
                      "<a href='https://cran.r-project.org/web/packages/PSS.Health/index.html' target='_blank'>CRAN</a>",
                      " e pode ser utilizado pelo computador por meio do pacote ", code("PSS.Health"), ".")),
          br(), br(),
          code("install.packages('PSS.Health')"),
          br(),
          code("PSS.Health::PSS_Health()"),

          HTML(
            "<i><font size = '2'><br><br>Pode haver diferenças nas funcionalidades presentes entre a versão on-line e a do pacote. Além disso, a versão on-line pode estar indisponível devido a manutenções ou indisponibilidade do servidor.</font></i>"
            # "<br><br><i>Pode haver diferenças nas funcionalidades presentes entre a versão on-line e a do pacote. Além disso, a versão on-line pode estar indisponível devido a manutenções ou indisponibilidade do servidor.</i>"
          )
        )
      },

      br(),br(),br(),
      h3("Leituras recomendadas"),
      HTML("Frequentemente a Unidade de Bioestatística da Diretoria de Pesquisa do Hospital de Clínicas de Porto Alegre publica artigos na seção de bioestatística da revista ",
           "<a href='https://seer.ufrgs.br/hcpa' target='_blank'>Clinical & Biomedical Research</a>. Nessas publicações são abordados temas que podem te auxiliar na definição do tamanho amostral e do poder do teste.",

           "<br><br>",
           "<ul>", # inicio da lista

           "<li><b>Artigos sobre o PSS Health:</b></li>",
           "<ul>",
           '<li><b><a href="https://doi.org/10.22491/2357-9730.109542" target="_blank">Power and Sample Size for Health Researchers: uma ferramenta para cálculo de tamanho amostral e poder do teste voltado a pesquisadores da área da saúde</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: como calcular tamanho de amostra para estimar média, proporção e correlação</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: como calcular tamanho de amostra para testes de comparação de médias de dois grupos</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.126843" target="_blank">PSS Health: como calcular tamanho de amostra para testar relações de variáveis com um desfecho binário</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.139152" target="_blank">PSS Health: como calcular tamanho de amostra para testar hipóteses de variáveis quantitativas</a></b></li>',
           "</ul>",
           "<br><br>",
           "<li><b>Principais conceitos em Epidemiologia:</b> Têm dúvidas sobre os tipos de delineamento e métodos de amostragem?</li>",
           "<ul>",
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/42338/27237" target="_blank">Os principais delineamentos na Epidemiologia</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/44253/28281" target="_blank">Os principais delineamentos na Epidemiologia: Ensaios Clínicos I</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/44657/28397" target="_blank">Os principais delineamentos na Epidemiologia: Ensaios Clínicos II </a></b></li>',
           "</ul>",

           "<br><br>",
           "<li><b>Série \"Perguntas que você sempre quis fazer, mas nunca teve coragem\":</b> Têm dúvidas sobre conceitos comumente utilizados em estatística e epidemiologia?</li>",
           "<ul>",
           '<li><b><a href="https://doi.org/10.4322/2357-9730.89242" target="_blank">Estatística Descritiva: Perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Bioestatística e epidemiologia: perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
           '<li><b><a href="https://doi.org/10.4322/2357-9730.93649" target="_blank">Teste de hipóteses: perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.98944" target="_blank">Modelagem estatística: Perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.101299" target="_blank">Equívocos Estatísticos: Perguntas que você sempre quis fazer, mas nunca teve coragem</a></b></li>',
           "</ul>",

           "<br><br>",
           "<li><b>Outras publicações:</b> </li>",
           "<ul>",
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/27267/16646" target="_blank">Um alerta sobre o uso de amostras pequenas na regressão logística</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/23574/15837" target="_blank">Cálculo de tamanho de amostra: proporções</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/9737/5819" target="_blank">Estudos transversais e longitudinais com desfechos binários: qual a melhor medida de efeito a ser utilizada? </a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/33160/22836" target="_blank">Calculando o tamanho de efeito no SPSS</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/14766/8828" target="_blank">Beanplot uma nova ferramenta gráfica</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/11727/7021" target="_blank">Análise de concordância entre métodos de Bland-Altman</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/36971/23993" target="_blank">Uso do Modelo de Equações de Estimações Generalizadas na análise de dados longitudinais</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/29874/19186" target="_blank">Normalidade de variáveis: métodos de verificação e comparação de alguns testes não-paramétricos por simulação</a></b></li>',
           "</ul>",

           "</ul>" # fim da lista de artigos
      ),

      br(), br(),
      h3("Sobre"),
      HTML("Esse aplicativo foi concebido no trabalho de conclusão do curso de Bacharelado em estatística do aluno ",
           '<a href="https://lume.ufrgs.br/handle/10183/212679" target="_blank">Guilherme Azambuja</a>',
           ", sob a orientação da professora Stela Castro. Recebeu incrementos, novas funcionalidades e é mantido pela equipe da Unidade de Bioestatística da Diretoria de Pesquisa do Hospital de Clínicas de Porto Alegre:<br><br>",

           "<ul>", # inicio da lista

           "<li> Aline Castello Branco Mancuso ",
           "<a href='https://orcid.org/0000-0001-6033-8335' target='_blank'>(Orcid iD)</a> ",
           "<a href='http://lattes.cnpq.br/3041495053719418' target='_blank'>(Lattes iD)</a></li>",


           "<li> Rogério Boff Borges ",
           "<a href='https://orcid.org/0000-0002-2548-1889' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/4664814523190366' target='_blank'>(Lattes iD)</a></li>",

           "<li> Stela Maris de Jezus Castro ",
           "<a href='https://orcid.org/0000-0001-5862-6709' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/3433964793739774' target='_blank'>(Lattes iD)</a></li>",


           "<li>Suzi Alves Camey ",
           "<a href='https://orcid.org/0000-0002-5564-081X' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/8280035478871760' target='_blank'>(Lattes iD)</a></li>",


           "<li>Vanessa Bielefeldt Leotti ",
           "<a href='https://orcid.org/0000-0003-3860-9367' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/5223855158009832' target='_blank'>(Lattes iD)</a></li>",


           "<li>Vânia Naomi Hirakata ",
           "<a href='https://orcid.org/0000-0003-4645-2080' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/4647357908962910' target='_blank'>(Lattes iD)</a></li>",


           "</ul>" # fim da lista de autores
      ),
      .rodape()



    )
  })






  output$texto_pagina_inicial_en <- renderUI({

    req(linguagem() == "en")

    tagList(
      HTML("Navigate between the tabs to find the scenario corresponding to the objective of the study. Change the parameters as needed and use the text to help you understand the calculation.",
           "<br>",
           "<b>Always</b> consult a statistician for guidance in study design.",
           ""),

      br(),br(), br(),
      if (.versao_online) {
        tagList(
          HTML(paste0("PSS Health is available on ",
                      "<a href='https://cran.r-project.org/web/packages/PSS.Health/index.html' target='_blank'>CRAN</a>",
                      " and can be used by the computer via the ", code("PSS.Health"), " package.")),
          br(), br(),
          code("install.packages('PSS.Health')"),
          br(),
          code("PSS.Health::PSS_Health()"),

          HTML(
            "<i><font size = '2'><br><br>There may be differences in functionality present between the online version and the packaged version. In addition, the online version may be unavailable due to maintenance or server outages.</font></i>"
          )
        )
      },

      br(),br(),br(),
      h3("Recommended reading"),
      HTML("The Biostatistics Unit of the Research Directorate of Hospital de Clínicas de Porto Alegre often publishes articles in the biostatistics section of the journal ",
           "<a href='https://seer.ufrgs.br/hcpa' target='_blank'>Clinical & Biomedical Research</a>. These publications address topics that can help you define the sample size and test power.",

           "<br><br>",
           "<ul>", # inicio da lista

           "<li><b>Articles about PSS Health:</b></li>",
           "<ul>",
           '<li><b><a href="https://doi.org/10.22491/2357-9730.109542" target="_blank">Power and Sample Size for Health Researchers: a tool for calculating sample size and statistical power designed for health researchers</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.112466" target="_blank">PSS Health: how to calculate a sample size to estimate means, proportions, and correlations</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.120997" target="_blank">PSS Health: how to calculate sample sizes for mean comparison tests between two groups</a></b></li>',
           "</ul>",
           "<br><br>",
           "<li><b>Key concepts in Epidemiology:</b> Do you have questions about the types of design and sampling methods?</li>",
           "<ul>",
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/42338/27237" target="_blank">Primary study designs in epidemiology</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/44253/28281" target="_blank">The major designs in epidemiology cliniCal trial (part I)</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/44657/28397" target="_blank">The major designs in epidemiology cliniCal trial (part II) </a></b></li>',
           "</ul>",

           "<br><br>",
           "<li><b>Series \"Questions you always wanted to ask, but never had the courage\":</b> Do you have questions about commonly used concepts in statistics and epidemiology?</li>",
           "<ul>",
           '<li><b><a href="https://doi.org/10.4322/2357-9730.89242" target="_blank">Descriptive statistics: questions you have always wanted to ask, but never had the courage to</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.96394" target="_blank">Biostatistics and epidemiology: questions you always wanted to ask but never had the courage to</a></b></li>',
           '<li><b><a href="https://doi.org/10.4322/2357-9730.93649" target="_blank">Hypothesis testing: questions you have always wanted to ask, but never had the courage to</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.98944" target="_blank">Statistical modeling: questions you have always wanted to ask but never had the courage to</a></b></li>',
           '<li><b><a href="https://doi.org/10.22491/2357-9730.101299" target="_blank">Statistical misconceptions: questions you\'ve always wanted to ask, but never dared</a></b></li>',
           "</ul>",

           "<br><br>",
           "<li><b>Other publications:</b> </li>",
           "<ul>",
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/27267/16646" target="_blank">A warning about the use of small samples in logistic regression</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/23574/15837" target="_blank">Sample size calculation: proportions</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/9737/5819" target="_blank">Cross-sectional and longitudinal studies with binary outcomes: what is the best measure of effect to be used? </a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/33160/22836" target="_blank">Calculating the effect size in SPSS</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/14766/8828" target="_blank">Beanplot a new graphical tool</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/11727/7021" target="_blank">Bland-Altman analysis of agreement between methods</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/36971/23993" target="_blank">Use of the generalized estimating equation model in longitudinal dat aanalysis</a></b></li>',
           '<li><b><a href="https://seer.ufrgs.br/hcpa/article/view/29874/19186" target="_blank">Normality of variables: diagnosis methods and comparison of some nonparametric tests by simulation</a></b></li>',
           "</ul>",

           "</ul>" # fim da lista de artigos
      ),

      br(), br(),
      h3("About"),
      HTML("This application was conceived in ",
           "<a href='https://lume.ufrgs.br/handle/10183/212679' target='_blank'>Guilherme Azambuja's</a>",
           "completion work of the Bachelor's in Statistics course ",
           "under the supervision of Professor Stela Castro. It received increments, new functionalities and is maintained by the team of the Biostatistics Unit of the Research Directorate of Hospital de Clínicas de Porto Alegre:<br><br>",

           "<ul>", # inicio da lista

           "<li> Aline Castello Branco Mancuso ",
           "<a href='https://orcid.org/0000-0001-6033-8335' target='_blank'>(Orcid iD)</a> ",
           "<a href='http://lattes.cnpq.br/3041495053719418' target='_blank'>(Lattes iD)</a></li>",


           "<li> Rogério Boff Borges ",
           "<a href='https://orcid.org/0000-0002-2548-1889' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/4664814523190366' target='_blank'>(Lattes iD)</a></li>",

           "<li> Stela Maris de Jezus Castro ",
           "<a href='https://orcid.org/0000-0001-5862-6709' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/3433964793739774' target='_blank'>(Lattes iD)</a></li>",


           "<li>Suzi Alves Camey ",
           "<a href='https://orcid.org/0000-0002-5564-081X' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/8280035478871760' target='_blank'>(Lattes iD)</a></li>",


           "<li>Vanessa Bielefeldt Leotti ",
           "<a href='https://orcid.org/0000-0003-3860-9367' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/5223855158009832' target='_blank'>(Lattes iD)</a></li>",


           "<li>Vânia Naomi Hirakata ",
           "<a href='https://orcid.org/0000-0003-4645-2080' target='_blank'>(Orcid iD)</a>",
           "<a href='http://lattes.cnpq.br/4647357908962910' target='_blank'>(Lattes iD)</a></li>",


           "</ul>" # fim da lista de autores
      ),
      .rodape()



    )
  })


  output$navbarMenu_medias <- renderText({
    translation_pss("Médias", linguagem())
  })

  output$navbarMenu_umaamostra <- renderText({
    translation_pss("Uma amostra", linguagem())
  })

  output$navbarMenu_2gruposindependentes <- renderText({
    translation_pss("Dois grupos independentes", linguagem())
  })

  output$navbarMenu_2gruposindependentes_infsupeq <- renderText({
    translation_pss("Dois grupos independentes (Inf/ Equi/ Sup)", linguagem())
  })

  output$navbarMenu_2gruposdependentes <- renderText({
    translation_pss("Dois grupos dependentes", linguagem())
  })

  output$navbarMenu_delta2grupos <- renderText({
    translation_pss("Delta de dois grupos independentes", linguagem())
  })

  output$navbarMenu_medidasrepetidas <- renderText({
    translation_pss("Medidas repetidas", linguagem())
  })

  output$navbarMenu_ANOVA1via <- renderText({
    translation_pss("ANOVA de uma via", linguagem())
  })

  output$navbarMenu_ANOVA2via <- renderText({
    translation_pss("ANOVA de duas vias", linguagem())
  })

  output$navbarMenu_proporcoes <- renderText({
    translation_pss("Proporções", linguagem())
  })

  output$navbarMenu_umaamostrap <- renderText({
    translation_pss("Uma amostra", linguagem())
  })

  output$navbarMenu_2gruposindependentesp <- renderText({
    translation_pss("Dois grupos independentes", linguagem())
  })

  output$navbarMenu_2gruposindependentes_infsupeqp <- renderText({
    translation_pss("Dois grupos independentes (Inf/ Equi/ Sup)", linguagem())
  })

  output$navbarMenu_2gruposdependentesp <- renderText({
    translation_pss("Dois grupos dependentes", linguagem())
  })

  output$navbarMenu_chi2 <- renderText({
    translation_pss("Qui-quadrado", linguagem())
  })

  output$navbarMenu_correlacao <- renderText({
    translation_pss("Correlação", linguagem())
  })

  output$navbarMenu_regressao <- renderText({
    translation_pss("Regressão", linguagem())
  })

  output$navbarMenu_logistica <- renderText({
    translation_pss("Logística", linguagem())
  })

  output$navbarMenu_classificacao <- renderText({
    translation_pss("Classificação", linguagem())
  })

  output$navbarMenu_sens_esp <- renderText({
    translation_pss("Sensibilidade/ Especificidade", linguagem())
  })

  output$navbarMenu_concordancia <- renderText({
    translation_pss("Concordância", linguagem())
  })

  output$navbarMenu_kappa <- renderText({
    translation_pss("Kappa de Cohen", linguagem())
  })

  output$navbarMenu_Cronbach <- renderText({
    translation_pss("Cronbach", linguagem())
  })

  output$navbarMenu_outras_ferramentas <- renderText({
    translation_pss("Outras ferramentas", linguagem())
  })

  output$navbarMenu_obterDP <- renderText({
    translation_pss("Obter desvio padrão", linguagem())
  })

  output$navbarMenu_obter_correlacao <- renderText({
    translation_pss("Obter a correlação", linguagem())
  })

  output$navbarMenu_dp_combinado <- renderText({
    translation_pss("Desvio padrão combinado", linguagem())
  })

  output$navbarMenu_dCohen <- renderText({
    translation_pss("d de Cohen", linguagem())
  })

  output$navbarMenu_perc_para_chance <- renderText({
    translation_pss("Converter percentual para chance", linguagem())
  })




















  #__________----
  # aba_obter_dp -----

  escolhas_obter_dp <- reactive({
    opcoes <- c(
      "Erro padrão",
      "Intervalo de confiança",
      "Estatística t",
      "Valor de p",
      "Da diferença entre grupos pareados"
    )

    names(opcoes) <- c(
      translation_pss("Erro padrão", linguagem()),
      translation_pss("Intervalo de confiança", linguagem()),
      translation_pss("Estatística t", linguagem()),
      translation_pss("Valor de p", linguagem()),
      translation_pss("Da diferença entre grupos pareados", linguagem())
    )

    opcoes
  })


  output$aba_obter_dp <- renderUI({

    tagList(
      titlePanel(
        translation_pss("Obter o desvio padrão de outras estatísticas", linguagem())
      ),

      wellPanel(txt_ajuda()$wellPanel_obter_dp),


      sidebarLayout(
        sidebarPanel(

          wellPanel(
            selectInput(
              inputId = "ferramentes_desvio_padrao_statistic",
              label   = translation_pss("Escolha a estatística", linguagem()),
              choices = escolhas_obter_dp(),
              selected = "Erro padrão"
            )
          ),

          # Erro padrao
          conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Erro padrão"',

                           numericInput( "ferramentas_ep_erro_padrao",
                                         translation_pss("Erro padrão", linguagem()),
                                         value = 10,
                                         min = 0,
                                         max = Inf,
                                         step = 1
                           )
          ),


          # Intervalo de confianca
          conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Intervalo de confiança"',
                           numericInput( "ferramentas_ic_ic",
                                         translation_pss("Limite do intervalo de confiança", linguagem()),
                                         value = 4.06,
                                         min = -Inf,
                                         max = Inf,
                                         step = 1
                           ) %>% .help_buttom(linguagem = linguagem(), body = "Um dos valores do intervalo de confiança relatado na literatura, supondo que as condições descritas acima estejam satisfeitas.
                                          Pode ser o limite inferior ou o superior.")
          ),

          # Estatistica t
          conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Estatística t"',
                           numericInput( "ferramentas_ic_t",
                                         translation_pss("Estatística t", linguagem()),
                                         value = 4.06,
                                         min = -Inf,
                                         max = Inf,
                                         step = 1
                           )
          ),

          # Valor de p
          conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Valor de p"',
                           numericInput( "ferramentas_ic_p",
                                         translation_pss("Valor de p", linguagem()),
                                         value = 0.003,
                                         min = 0,
                                         max = 1,
                                         step = .01
                           ),
                           if (linguagem() == "en") {
                             p("ATTENTION! When p is not defined exactly, the calculation will not be correct. Use as many decimal places as possible, especially if p < 0.001")
                           } else {
                             p("ATENÇÃO! Quando o p não for definido de forma exata o cálculo não será correto. ",
                               "Utilizar o maior número de casas decimais possíveis, principalmente se p < 0.001.")
                           },
          ),


          # Ho para estatística t e valor de p
          conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Estatística t" || input.ferramentes_desvio_padrao_statistic == "Valor de p"',

                           numericInput( "ferramentas_ic_t_h0",
                                         translation_pss("Valor de referência sob a hipótese nula", linguagem()),
                                         value = 0,
                                         min = -Inf,
                                         max = Inf,
                                         step = 1
                           )
          ),


          # Nao precisa da media para o erro padrao
          conditionalPanel('input.ferramentes_desvio_padrao_statistic != "Erro padrão" && input.ferramentes_desvio_padrao_statistic != "Da diferença entre grupos pareados"',

                           numericInput( "ferramentas_ic_media",
                                         translation_pss("Média", linguagem()),
                                         value = 5,
                                         min = -Inf,
                                         max = Inf,
                                         step = 1
                           ) %>% .help_buttom(body = "Média do estudo na qual o intervalo de confiança foi extraído.")
          ),


          # Nao usa na Da diferença do baseline
          conditionalPanel('input.ferramentes_desvio_padrao_statistic != "Da diferença entre grupos pareados"',

                           numericInput( "ferramentas_ic_n",
                                         translation_pss("Tamanho da amostra", linguagem()),
                                         value = 20,
                                         min = 2,
                                         max = Inf,
                                         step = 1
                           ) %>% .help_buttom(body = "Tamanho da amostra do estudo na qual o intervalo de confiança foi extraído.")
          ),


          # So para o IC
          conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Intervalo de confiança"',
                           numericInput( "ferramentas_ic_conf",
                                         translation_pss("Nível de confiança (%)", linguagem()),
                                         value = 95,
                                         min = 0,
                                         max = 100,
                                         step = 1
                           ) %>% .help_buttom(body = txt_ajuda()$txt_confianca, title = translation_pss("Nível de confiança (%)", linguagem()))
          ),


          # Imputing a change-from-baseline standard deviation using a correlation coefficient
          conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Da diferença entre grupos pareados"',

                           numericInput( "ferramentas_sd_baseline",
                                         paste0(
                                           translation_pss("Desvio padrão", linguagem()),
                                           " 1"
                                         ),
                                         value = 4,
                                         min = 0,
                                         max = Inf,
                                         step = 1) %>%
                             .help_buttom(body = "Em estudos longitudinais o Grupo 1 pode ser entendido como o Momento 1."),

                           numericInput( "ferramentas_sd_follow",
                                         paste0(
                                           translation_pss("Desvio padrão", linguagem()),
                                           " 2"
                                         ),
                                         value = 4.4,
                                         min = 0,
                                         max = Inf,
                                         step = 1) %>%
                             .help_buttom(body = "Em estudos longitudinais o Grupo 2 pode ser entendido como o Momento 2."),

                           numericInput( "ferramentas_sd_correlation",
                                         paste0(
                                           translation_pss("Coeficiente de correlação entre", linguagem()),
                                           " 1 ",
                                           translation_pss("e", linguagem()),
                                           " 2"
                                         ),
                                         value = 0.8,
                                         min = -1,
                                         max = 1,
                                         step = .1)

          ),




          numericInput( "ferramentas_desvio_padrao_decimals",
                        translation_pss("Número de casas decimais", linguagem()),
                        value = 3,
                        min = 0,
                        max = Inf,
                        step = 1
          )

        ), # Fecha sidebar


        mainPanel(

          conditionalPanel('input.ferramentes_desvio_padrao_statistic == "Da diferença do baseline"',
                           HTML(
                             '<a href="https://handbook-5-1.cochrane.org/chapter_16/16_1_3_2_imputing_standard_deviations_for_changes_from_baseline.htm" target="_blank">Mais informações aqui.</a>'
                           ),
                           br()
          ),


          htmlOutput("ferramentas_desvio_padrao_valor") %>%
            shinycssloaders::withSpinner(type = 5),

          br(), br(), br(),
          p(translation_pss("Foi utilizado a fórmula", linguagem())),

          br(),
          uiOutput("ferramentas_desvio_padrao_formulas") %>%
            shinycssloaders::withSpinner(type = 5)


        ) # Fecha main painel
      ) # Fecha layout de painel

      ,
      .rodape()
    )

  })




  # aba_ic_sd ----.


  # aba_cohen d ----


  output$aba_cohen <- renderUI({

    tagList(
      titlePanel(translation_pss("d de Cohen", linguagem())),
      sidebarLayout(
        sidebarPanel(

          numericInput( "cohen_mean_dif",
                        translation_pss("Diferença esperada", linguagem()),
                        value = 4.06,
                        min = -Inf,
                        max = Inf,
                        step = 1
          ) %>% .help_buttom(body = "Diferença observada entre os dois grupos."),
          numericInput( "cohen_sigma1",
                        paste(
                          translation_pss("Desvio padrão esperado de", linguagem()),
                          "A"
                        ),
                        value = 1.2,
                        min = 0,
                        max = Inf,
                        step = .01
          ),
          numericInput( "cohen_sigma2",
                        paste(
                          translation_pss("Desvio padrão esperado de", linguagem()),
                          "B"
                        ),
                        value = 1.4,
                        min = 0,
                        max = Inf,
                        step = .01
          ),

          numericInput( "cohen_n1",
                        paste0(translation_pss("Tamanho amostral do grupo", linguagem()), " A"),
                        value = 20,
                        min = 3,
                        max = Inf,
                        step = 1
          ),# %>% .help_buttom(body = paste0(translation_pss("Tamanho amostral do grupo", linguagem()), " A")),
          numericInput( "cohen_n2",
                        paste0(translation_pss("Tamanho amostral do grupo", linguagem()), " B"),
                        value = 20,
                        min = 3,
                        max = Inf,
                        step = 1
          ),# %>% .help_buttom(body = paste0(translation_pss("Tamanho amostral do grupo", linguagem()), " B")),

          numericInput( "cohen_decimals",
                        translation_pss("Número de casas decimais", linguagem()),
                        value = 3,
                        min = 0,
                        max = Inf,
                        step = 1
          ),# %>% .help_buttom(body = "Número de casas decimais para exibir o desvio padrão calculado.")
        ),

        mainPanel(
          htmlOutput("ferramentas_cohen"),
          br(), br(),
          p(translation_pss("Foi utilizado a fórmula", linguagem())),
          withMathJax(
            "$$\\text{Cohen'd} = \\dfrac{\\text{",
            translation_pss("Diferença esperada", linguagem()),
            "}}{s_{pooled}}$$"
          ),
          br(),
          p(translation_pss("onde", linguagem())),
          withMathJax(
            "$$s_{pooled} = \\sqrt{ \\dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} } .$$"
          )
        )
      ),

      .rodape()

    )


  })

  # aba_pooled_var -----

  output$aba_pooled_var <- renderUI({

    tagList(
      titlePanel(translation_pss("Desvio padrão combinado", linguagem())),
      wellPanel(txt_ajuda()$wellPanel_dp_combinado),

      sidebarLayout(
        sidebarPanel(


          checkboxInput(inputId = "pooled_eh_sd",
                        label   = translation_pss("Entrar com os valores do desvio padrão", linguagem()),
                        value   = TRUE),

          uiOutput("pooled_var_sdUi"),

          checkboxInput(inputId = "pooled_equal_size",
                        label   = translation_pss("Os grupos possuem o mesmo tamanho amostral", linguagem()),
                        value   = FALSE),

          conditionalPanel('input.pooled_equal_size == false',

                           # HTML(
                           #   paste0("<b><font size = '2.99'>", estat_, " do</font></b><br>")
                           # ),
                           div(style = "display: inline-block;vertical-align:bottom;vertical-align:bottom; width: 49%;",
                               numericInput( "pooled_n1",
                                             paste0(translation_pss("Tamanho amostral do grupo", linguagem()), " A"),
                                             value = 20,
                                             min = 3,
                                             max = Inf,
                                             step = 1)
                           ),
                           div(style = "display: inline-block;vertical-align:top; width: 49%;",
                               numericInput( "pooled_n2",
                                             paste0(translation_pss("Tamanho amostral do grupo", linguagem()), " B"),
                                             value = 30,
                                             min = 3,
                                             max = Inf,
                                             step = 1)
                           )
          ),

          numericInput( "pooled_decimals",
                        translation_pss("Número de casas decimais", linguagem()),
                        value = 3,
                        min = 0,
                        max = Inf,
                        step = 1
          )
        ),

        mainPanel(
          htmlOutput("ferramentas_pooled"),
          br(), br(),
          p(translation_pss("Foi utilizado a fórmula", linguagem())),
          # withMathJax(
          #   "$$\\text{Cohen'd} = \\dfrac{\\text{Diferença das médias}}{s_{pooled}}$$"
          # ),
          # br(),
          # p("onde"),
          withMathJax(
            "$$s_{pooled} = \\sqrt{ \\dfrac{(n_A - 1)s_A^2 + (n_B - 1)s_B^2}{n_A+n_B-2} } .$$"
          )
        )
      ),

      .rodape()

    )

  })


  # aba_obter_correlacao -----

  output$aba_obter_correlacao <- renderUI({

    tagList(
      titlePanel(
        translation_pss("Obter a correlação de Pearson entre duas variáveis utilizando os desvios padrões", linguagem())
      ),

      sidebarLayout(
        sidebarPanel(

          numericInput( "outras_ferr_correlacaoA",
                        paste0(
                          translation_pss("Desvio padrão esperado de", linguagem()),
                          " A"
                        ),
                        value = 6.4,
                        min = 0,
                        max = Inf,
                        step = 1
          ),
          br(),

          numericInput( "outras_ferr_correlacaoB",
                        paste0(
                          translation_pss("Desvio padrão esperado de", linguagem()),
                          " B"
                        ),
                        value = 7.1,
                        min = 0,
                        max = Inf,
                        step = 1
          ),
          br(),

          numericInput( "outras_ferr_correlacaoAeB",
                        translation_pss("Desvio padrão da diferença", linguagem()),
                        value = 4.5,
                        min = 0,
                        max = Inf,
                        step = 1
          )



        ),

        mainPanel(
          br(), br(),
          htmlOutput("correlacao_outras_ferramentas"),

          br(), br(),
          p(translation_pss("Foi utilizado a fórmula", linguagem())),
          br(),
          withMathJax(
            "$$\\rho_{AB} = \\dfrac{SD^2_A + SD^2_B - SD^2_{AB}}{2*SD^2_A*SD^2_B} $$"
          ),
          br(), br(),
          .rodape()

        )
      )
    )

  })


  output$correlacao_outras_ferramentas <- renderText({
    correlacao <- (input$outras_ferr_correlacaoA^2 + input$outras_ferr_correlacaoB^2 - input$outras_ferr_correlacaoAeB^2)/
      (2*input$outras_ferr_correlacaoA*input$outras_ferr_correlacaoB)


    paste0("<b><font size = '5'>",
           "<i>",
           translation_pss("Correlação", linguagem()),
           "<sub>AB</sub></i> = ", round(correlacao, 4),
           "<br><br>")
  })





  # aba_obter_razao -----

  eval(parse(text = warning_prop("prop_para_converter")))

  output$aba_obter_razao <- renderUI({

    tagList(
      titlePanel(translation_pss("Converter percentual para chance", linguagem())),

      # sidebarLayout(
      #   sidebarPanel(

      numericInput( "prop_para_converter",
                    translation_pss("Percentual (%)", linguagem()),
                    value = 30,
                    min = 0,
                    max = 100,
                    step = 1
                    # )
      ),

      # mainPanel(
      br(), br(),
      uiOutput("aba_obter_razao_saida"),

      # br(), br(),
      # p("Foi utilizado a fórmula:"),
      # br(),
      # withMathJax(
      #   "$$Chance = \\dfrac{p}{1-p} $$"
      # ),
      br(), br(),
      .rodape()

      # )
      # )
    )

  })

  output$aba_obter_razao_saida <- renderUI({

    p <- input$prop_para_converter/100
    odds <- round(p/(1-p), 2)

    withMathJax(
      paste0(
        "$$",
        translation_pss("Chance", linguagem()),
        "= \\dfrac{p}{1-p} = ",
        "\\dfrac{", p, "}{1- ", p, "} = ",
        odds,
        "$$"
      )
    )

  })





  ## Webpower ----

  # https://cran.r-project.org/web/packages/WebPower/WebPower.pdf
  # Versao 0.5.2
  #   O WebPower eh um pacote muito pesado e eh usando somente uma unicad funcao dele,
  # sendo assim optou por copiar a funcao e fazer somente a citacao, removendo assim
  # uma dependencia do PSS.

  wp.correlation <- function (n = NULL, r = NULL, power = NULL, p = 0, rho0 = 0,
                              alpha = 0.05, alternative = c("two.sided", "less", "greater"))
  {
    if (sum(sapply(list(n, r, power, alpha), is.null)) != 1)
      stop("exactly one of n, r, power, and alpha must be NULL")
    if (!is.null(alpha) && !is.numeric(alpha) || any(0 > alpha |
                                                     alpha > 1))
      stop(sQuote("alpha"), " must be numeric in [0, 1]")
    if (!is.null(power) && !is.numeric(power) || any(0 > power |
                                                     power > 1))
      stop(sQuote("power"), " must be numeric in [0, 1]")
    if (!is.null(n) && min(n) < 4)
      stop("number of observations must be at least 4")
    alternative <- match.arg(alternative)
    tside <- switch(alternative, less = 1, two.sided = 2, greater = 3)
    if (tside == 2 && !is.null(r))
      r <- abs(r)
    if (tside == 3) {
      p.body <- quote({
        delta <- sqrt(n - 3 - p) * (log((1 + r)/(1 - r))/2 +
                                      r/(n - 1 - p)/2 * (1 + (5 + r^2)/(n - 1 - p)/4 +
                                                           (11 + 2 * r^2 + 3 * r^4)/(n - 1 - p)^2/8) -
                                      log((1 + rho0)/(1 - rho0))/2 - rho0/(n - 1 -
                                                                             p)/2)
        v <- (n - 3 - p)/(n - 1 - p) * (1 + (4 - r^2)/(n -
                                                         1 - p)/2 + (22 - 6 * r^2 - 3 * r^4)/(n - 1 -
                                                                                                p)^2/6)
        zalpha <- qnorm(1 - alpha)
        pnorm((delta - zalpha)/sqrt(v))
      })
    }
    if (tside == 1) {
      p.body <- quote({
        delta <- sqrt(n - 3 - p) * (log((1 + r)/(1 - r))/2 +
                                      r/(n - 1 - p)/2 * (1 + (5 + r^2)/(n - 1 - p)/4 +
                                                           (11 + 2 * r^2 + 3 * r^4)/(n - 1 - p)^2/8) -
                                      log((1 + rho0)/(1 - rho0))/2 - rho0/(n - 1 -
                                                                             p)/2)
        v <- (n - 3 - p)/(n - 1 - p) * (1 + (4 - r^2)/(n -
                                                         1 - p)/2 + (22 - 6 * r^2 - 3 * r^4)/(n - 1 -
                                                                                                p)^2/6)
        zalpha <- qnorm(1 - alpha)
        pnorm((-delta - zalpha)/sqrt(v))
      })
    }
    if (tside == 2) {
      p.body <- quote({
        delta <- sqrt(n - 3 - p) * (log((1 + r)/(1 - r))/2 +
                                      r/(n - 1 - p)/2 * (1 + (5 + r^2)/(n - 1 - p)/4 +
                                                           (11 + 2 * r^2 + 3 * r^4)/(n - 1 - p)^2/8) -
                                      log((1 + rho0)/(1 - rho0))/2 - rho0/(n - 1 -
                                                                             p)/2)
        v <- (n - 3 - p)/(n - 1 - p) * (1 + (4 - r^2)/(n -
                                                         1 - p)/2 + (22 - 6 * r^2 - 3 * r^4)/(n - 1 -
                                                                                                p)^2/6)
        zalpha <- qnorm(1 - alpha/2)
        pnorm((delta - zalpha)/sqrt(v)) + pnorm((-delta -
                                                   zalpha)/sqrt(v))
      })
    }
    if (is.null(power))
      power <- eval(p.body)
    else if (is.null(n))
      n <- uniroot(function(n) eval(p.body) - power, c(4 +
                                                         p + 1e-10, 1e+07))$root
    else if (is.null(r)) {
      if (tside == 2) {
        r <- uniroot(function(r) eval(p.body) - power, c(1e-10,
                                                         1 - 1e-10))$root
      }
      else {
        r <- uniroot(function(r) eval(p.body) - power, c(-1 +
                                                           1e-10, 1 - 1e-10))$root
      }
    }
    else if (is.null(alpha))
      alpha <- uniroot(function(alpha) eval(p.body) - power,
                       c(1e-10, 1 - 1e-10))$root
    else stop("internal error")
    METHOD <- "Power for correlation"
    URL <- "http://psychstat.org/correlation"
    structure(list(n = n, r = r, alpha = alpha, power = power,
                   alternative = alternative, method = METHOD, url = URL),
              class = "webpower")
  }







  # Lista de funcoes ------

  lista_de_funcoes_server <- reactive({
    list(
      sugestao_texto_portugues = sugestao_texto_portugues
    )
  })








}# fecha o server !!!!
