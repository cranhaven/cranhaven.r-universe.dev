
translation_pss  <- function(text, language = "pt"){

  if (language == "pt") return(text)

  list_text <- list(

    # Geral
    "Boas vindas!" = list(
      "en" = "Welcome!"
    ),

    "Descreva a unidade de medida de" = list(
      "en" = "Describe the unit of measure for",
      "es" = "Describa la unidad de medida para"
    ),

    "Descreva a unidade de medida do desfecho" = list(
      "en" = "Describe the unit of measurement of the outcome"
    ),

    "Unidade de medida de" = list(
      "en" = "Unit of measure of"
    ),

    "u.m." = list(
      "en" = "m.u."
    ),

    "Descreva o nome do desfecho" = list(
      "en" = "Describe the outcome name"
    ),

    "Descreva um nome para o grupo Tratamento" = list(
      "en" = "Describe a name for the Treatment group"
    ),

    "Descreva um nome para o grupo Controle" = list(
      "en" = "Describe a name for the Control group"
    ),

    "Tratamento" = list(
      "en" = "Treatment"
    ),

    "Controle" = list(
      "en" = "Control"
    ),

    "Sugestão de texto" = list(
      "en" = "Text suggestion"
    ),

    "Construção de cenários" = list(
      "en" = "Scenario construction"
    ),

    "Download tabela" = list(
      "en" = "Download table"
    ),


    # Estatistica ----


    "Probabilidade" = list(
      "en" = "Probability"
    ),

    "DP" = list(
      "en" = "SD"
    ),

    "Desvio padrão" = list(
      "en" = "Standard deviation"
    ),

    "Variância" = list(
      "en" = "Variance"
    ),

    "Desvio padrão esperado" = list(
      "en" = "Expected standard deviation"
    ),

    "Desvio padrão da diferença" = list(
      "en" = "Standard deviation of difference"
    ),

    "Usar o desvio padrão da diferença" = list(
      "en" = "Use the standard deviation of the difference"
    ),

    "Desvio padrão esperado de" = list(
      "en" = "Expected standard deviation of"
    ),

    "Desvio padrão da variável resposta" = list(
      "en" = "Outcome standard deviation"
    ),

    "Desvio padrão do preditor" = list(
      "en" = "Predictor standard deviation"
    ),

    "Uma média" = list(
      "en" = "One-mean"
    ),

    "Médias" = list(
      "en" = "Mean"
    ),

    "Média" = list(
      "en" = "Mean"
    ),

    "Proporções" = list(
      "en" = "Proportions"
    ),

    "Qui-quadrado" = list(
      "en" = "Chi-square"
    ),

    "Regressão" = list(
      "en" = "Regression"
    ),

    "Coeficiente de inclinação da reta para um modelo de regressão linear simples" = list(
      "en" = "Slope coefficient of the line for a simple linear regression model"
    ),

    "Coeficiente de regressão" = list(
      "en" = "Slope coefficient"
    ),

    "Calcular utilizando o R²" = list(
      "en" = "Calculate based on R²"
    ),

    "Correlação" = list(
      "en" = "Correlation"
    ),

    "Coeficiente de correlação" = list(
      "en" = "Correlation coefficient"
    ),

    "Coeficiente de correlação esperado" = list(
      "en" = "Expected correlation coefficient"
    ),

    "Coeficiente de correlação entre" = list(
      "en" = "Correlation coefficient between"
    ),

    "Correlação esperada (em módulo)" = list(
      "en" = "Expected correlation (in module)"
    ),

    "Selecione o tipo de matriz de correlação" = list(
      "en" = "Select the working correlation matrix"
    ),

    "Matriz de correlação" = list(
      "en" = "Working correlation matrix"
    ),

    "Número de variáveis para correlação parcial" = list(
      "en" = "Number of variables to partial out"
    ),

    "Logística" = list(
      "en" = "Logistic"
    ),

    "Regressão logística" = list(
      "en" = "Logistics regression"
    ),

    "Chance" = list(
      "en" = "Odds"
    ),

    "Razão de chance" = list(
      "en" = "Odds ratio"
    ),

    "Risco relativo" = list(
      "en" = "Relative risk"
    ),

    "Mínimo" = list(
      "en" = "From"
    ),

    "Máximo" = list(
      "en" = "To"
    ),

    "Intervalo" = list(
      "en" = "By"
    ),

    "Valor esperado de" = list(
      "en" = "Expected value of"
    ),

    "Percentual (%)" = list(
      "en" = "Percentage (%)"
    ),

    "Percentual esperado (%)" = list(
      "en" = "Expected percentage (%)"
    ),

    "% esperado no grupo" = list(
      "en" = "% expected in the group"
    ),

    "Percentual sob a hipótese nula (%)" = list(
      "en" = "Percentage under the null hypothesis (%)"
    ),

    "Percentual (%) de ocorrência da categoria" = list(
      "en" = "Percentage (%) of the occurrence of the category"
    ),

    "Valor de referência sob a hipótese nula" = list(
      "en" = "Reference value under the null hypothesis"
    ),

    "Uma amostra" = list(
      "en" = "One-sample"
    ),

    "Tamanho amostral" = list(
      "en" = "Sample size"
    ),

    "Tamanho da amostra" = list(
      "en" = "Sample size"
    ),

    "Tamanho total da amostra*" = list(
      "en" = "Total sample size*"
    ),

    "Tamanho da amostra*" = list(
      "en" = "Sample size*"
    ),

    "Tamanho amostral calculado" = list(
      "en" = "Sample size required"
    ),

    "Tamanho amostral do grupo" = list(
      "en" = "Sample size of group"
    ),

    "Poder calculado" = list(
      "en" = "Power of test"
    ),

    "Hipótese alternativa" = list(
      "en" = "Alternative Hypothesis"
    ),

    "Processo de amostragem" = list(
      "en" = "Sampling method"
    ),

    "Aleatória simples" = list(
      "en" = "Random sampling"
    ),

    "Conglomerados em um único estágio" = list(
      "en" = "Single-stage cluster"
    ),

    "Estratificada proporcional ao tamanho" = list(
      "en" = "Stratified (proportionate allocation)"
    ),

    "Número de estratos" = list(
      "en" = "Number of strata"
    ),

    "Tamanho populacional de cada estrato" = list(
      "en" = "Number of individual in each strata"
    ),

    "em cada estrato" = list(
      "en" = "for each strata"
    ),

    "Aplicar correção de continuidade" = list(
      "en" = "Use the continuity correction "
    ),

    "Calcular utilizando a aproximação pela normal" = list(
      "en" = "Compute based on the normal approximation"
    ),

    "Calcular utilizando a aproximação pela normal? Se esta opção estiver desmarcada será utilizado o método exato." = list(
      "en" = "Calculate using normal approximation? If this option is unchecked, the exact method will be used."
    ),


    "Balanceamento" = list(
      "en" = "Ratio of sample size"
    ),

    'Tipo de teste de acordo com hipótese alternativa' = list(
      "en" = "Type of test according to alternative hypothesis"
    ),

    'Tipo de teste' = list(
      "en" = "Type of test"
    ),

    "ANOVA de uma via" = list(
      "en" = "One-way ANOVA"
    ),

    "ANOVA de duas vias" = list(
      "en" = "Two-way ANOVA"
    ),

    "Dois grupos independentes" = list(
      "en" = "Two independent groups"
    ),

    "Dois grupos independentes (Inf/ Equi/ Sup)" = list(
      "en" = "Two independent groups (Inf/ Equi/ Sup)"
    ),

    "Dois grupos dependentes" = list(
      "en" = "Two dependent groups"
    ),

    "Delta de dois grupos independentes" = list(
      "en" = "Delta of two independent groups"
    ),

    "Medidas repetidas" = list(
      "en" = "Repeated measures"
    ),


    "Diferença mínima a ser detectada" = list(
      "en" = "Minimum detectable difference"
    ),

    "Diferença mínima a ser detectada entre os deltas" = list(
      "en" = "Minimum difference to be detected between deltas"
    ),

    "Diferença esperada" = list(
      "en" = "Expected difference"
    ),

    "Efeito do plano amostral" = list(
      "en" = "Design effect"
    ),

    "Calcular usando o d de Cohen" = list(
      "en" = "Calculate using Cohen's d"
    ),

    "d de Cohen" = list(
      "en" = "Cohen's d"
    ),

    "Tamanho de efeito w de Cohen" = list(
      "en" = "Cohen's w"
    ),

    "Graus de liberdade" = list(
      "en" = "Degrees of freedom"
    ),

    "Tabela de contingência com proporções" = list(
      "en" = "Contingency table with proportions"
    ),

    "Tabela de contingência com valores absolutos" = list(
      "en" = "Contingency table with absolute values"
    ),

    "Qual tipo de variável independente?" = list(
      "en" = "What type of independent variable?"
    ),

    "Descreva o nome das variáveis que deseja associar" = list(
      "en" = "Describe the name of the variables you want to associate"
    ),

    "Descreva o nome das variáveis que deseja correlacionar" = list(
      "en" = "Describe the name of the variables you want to correlate"
    ),

    "Descreva o nome da variável preditora" = list(
      "en" = "Describe the name of the predictor variable"
    ),

    "Qual é informação de entrada?" = list(
      "en" = "What's input information?"
    ),

    "Magnitude do efeito (f)" = list(
      "en" = "Effect size (f)"
    ),

    "Usar magnitude de efeito f" = list(
      "en" = "Use effect size f"
    ),

    "Número de grupos" = list(
      "en" = "Number of groups"
    ),

    "Número de observações (por grupo)" = list(
      "en" = "Number of observations (per group)"
    ),

    "Médias dos grupos" = list(
      "en" = "Group means"
    ),

    "Média esperada" = list(
      "en" = "Expected mean"
    ),

    "Médias esperadas de " = list(
      "en" = "Expected means of "
    ),

    "Média das diferenças a ser detectada" = list(
      "en" = "Mean of differences to be detected"
    ),

    "Coeficiente de correlação múltipla" = list(
      "en" = "Multiple correlation coefficient"
    ),

    "Calcular utilizando o R²" = list(
      "en" = "Calculate based on R²"
    ),

    "Coeficiente de determinação esperado" = list(
      "en" = "Expected coefficient of determination"
    ),

    "Coeficiente de determinação" = list(
      "en" = "Coefficient of determination"
    ),

    "Classificação" = list(
      "en" = "Classification"
    ),

    "Área sob a curva ROC" = list(
      "en" = "Area under the ROC curve"
    ),

    "Sensibilidade/ Especificidade"  = list(
      "en" = "Sensitivity/ Specificity"
    ),

    "sensibilidade"  = list(
      "en" = "sensitivity"
    ),

    "especificidade"  = list(
      "en" = "specificity"
    ),

    "Concordância" = list(
      "en" = "Agreement"
    ),

    "Kappa de Cohen" = list(
      "en" = "Cohen's kappa"
    ),

    "Kappa esperado" = list(
      "en" = "Expected kappa"
    ),

    "Coeficiente de correlação intraclasse" = list(
      "en" = "Intraclass correlation coefficient"
    ),

    "Precisão para os limites de concordância do gráfico de Bland-Altman" = list(
      "en" = "Precision for the limits of agreement on Bland-Altman plots"
    ),


    "Número de avaliadores" = list(
      "en" = "Number of ratings"
    ),

    "Número de categorias do desfecho" = list(
      "en" = "Number of outcome categories"
    ),

    "Início do estudo" = list(
      "en" = "Start of study"
    ),

    "Final do estudo" = list(
      "en" = "End of study"
    ),


    "Parâmetro autorregressivo no grupo" = list(
      "en" = "Autoregressive parameter on group"
    ),

    "Correlação entre os momentos" = list(
      "en" = "Correlation between times"
    ),

    "Resposta média no último momento" = list(
      "en" = "Mean response in the last time"
    ),

    "Número de momentos a ser avaliado" = list(
      "en" = "Number of times to be evaluated"
    ),

    "Tamanho amostral do grupo no último momento" = list(
      "en" = "Sample size of the group at the last time"
    ),

    "Matriz de correlação do grupo" = list(
      "en" = "Correlation matrix for group"
    ),

    "Retenção esperada (%) no grupo" = list(
      "en" = "Expected retention (%) in the group"
    ),

    "Cronbach esperado" = list(
      "en" = "Expected Cronbach"
    ),

    "Número de itens" = list(
      "en" = "Number of items"
    ),

    "Alfa de Cronbach" = list(
      "en" = "Cronbach's alpha"
    ),

    "Escolha a estatística" = list(
      "en" = "Choose statistics"
    ),

    "Estatística de entrada" = list(
      "en" = "Input stat"
    ),

    "Erro padrão" = list(
      "en" = "Standard error"
    ),

    "Intervalo de confiança" = list(
      "en" = "Confidence interval"
    ),

    "IC" = list(
      "en" = "CI"
    ),

    "Limite do intervalo de confiança" = list(
      "en" = "Confidence interval bound"
    ),

    "Estatística t" = list(
      "en" = "t statistic"
    ),

    "Valor de p" = list(
      "en" = "p value"
    ),

    "Da diferença entre grupos pareados" = list(
      "en" = "Of the difference between matched groups"
    ),

    "Número de casas decimais" = list(
      "en" = "Number of decimal places"
    ),

    "Entrar com os valores do desvio padrão" = list(
      "en" = "Enter with standard deviation values"
    ),

    "Os grupos possuem o mesmo tamanho amostral" = list(
      "en" = "The groups have the same sample size"
    ),

    "Forma" = list("en" = "Shape"),

    "Valores de forma" = list(
      "en" = "Shape values"
    ),

    "Como definir o parâmetro de forma?" = list(
      "en" = "How to set shape parameter?"
    ),

    "Densidade" = list("en" = "Density"),

    "Distribuições com médias iguais a" = list(
      "en" = "Distributions with means equal to"
    ),


    # Abas ----
    "Estimar" = list(
      "en" = "Estimate"
    ),

    "Testar" = list(
      "en" = "Test"
    ),

    "Poder" = list(
      "en" = "Power"
    ),

    "Ajustes" = list(
      "en" = "Settings"
    ),

    "Mudar nomes" = list(
      "en" = "Change names"
    ),

    "Mudar rótulos" = list(
      "en" = "Change labels"
    ),

    "Calcular o desvio padrão da diferença" = list(
      "en" = "Calculate the standard deviation of the difference"
    ),

    "Calcular o desvio padrão combinado" = list(
      "en" = "Calculate the pooled standard deviation"
    ),

    "Obter o desvio padrão combinado" = list(
      "en" = "Get the combined standard deviation"
    ),

    "combinado" = list(
      "en" = "pooled"
    ),

    "Foi utilizado a fórmula" = list(
      "en" = "The formula was used"
    ),


    "Comparação entre duas médias de grupos independentes" = list(
      "en" = "Comparison between two means of independent groups"
    ),

    "Comparação entre duas médias de grupos independentes (distribuição gamma)" = list(
      "en" = "Comparison between two means of independent groups (gamma distribution)"
    ),

    "Comparação entre duas proporções de grupos independentes" = list(
      "en" = "Comparison between two proportions of independent groups"
    ),

    "Outras ferramentas" = list(
      "en" = "Other tools"
    ),

    "Obter desvio padrão" = list(
      "en" = "Get standard deviation"
    ),

    "Obter a correlação" = list(
      "en" = "Get the correlation"
    ),

    "Desvio padrão combinado" = list(
      "en" = "Pooled standard deviation"
    ),

    "Converter percentual para chance" = list(
      "en" = "Convert percentage to chance"
    ),

    "Obter o desvio padrão de outras estatísticas" = list(
      "en" = "Get standard deviation from other statistics"
    ),

    "Obter a correlação de Pearson entre duas variáveis utilizando os desvios padrões" = list(
      "en" = "Get Pearson's correlation between two variables using standard deviations"
    ),




    # Pequenos ajustes ----

    " e " = list(
      "en" = " and "
    ),

    "em" = list(
      "en" = "in"
    ),

    "de" = list(
      "en" = "of"
    ),

    "no" = list(
      "en" = "at"
    ),

    "X1 e X2" = list(
      "en" = "X1 and X2"
    ),

    "ATENÇÃO" = list(
      "en" = "PAY ATTENTION"
    ),

    "Momento" = list(
      "en" = "Time"
    ),

    "para cada grupo" = list(
      "en" = "for each group"
    ),

    "Sim para Não" = list(
      "en" = "Yes to No"
    ),

    "Não para Sim" = list(
      "en" = "No to Yes"
    ),

    "onde" = list(
      "en" = "where"
    ),



    # Videos ----
    "Vídeo: PSS Health para estimar uma média" = list(
      "en" = "Video: PSS Health to estimate a mean"
    ),

    "Vídeo: PSS Health para comparar dua médias" = list(
      "en" = "Video: PSS Health for comparing two means"
    ),

    "Vídeo: PSS Health para estimar uma proporção" = list(
      "en" = "Video: PSS Health to estimate a proportion"
    ),

    "Vídeo: PSS Health para estimar a correlação" = list(
      "en" = "Video: PSS Health to estimate correlation"
    ),

    # Inferencia ----

    "Hipóteses a serem testadas" = list(
      "en" = "Hypothesis testing"
    ),

    "Nível de confiança (%)" = list(
      "en" = "Confidence level (%)"
    ),

    "Nível de significância (%)" = list(
      "en" = "Significance level (%)"
    ),

    "Poder (%)"  = list(
      "en" = "Power (%)"
    ),

    "Amplitude do intervalo (%)" = list(
      "en" = "The full width of the confidence interval (%)"
    ),

    "Amplitude do intervalo" = list(
      "en" = "The full width of the confidence interval"
    ),

    "Margem de" = list(
      "en" = "Margin of"
    ),

    "Margem de erro" = list(
      "en" = "Margin of error"
    ),

    "Margem de erro/ semi-amplitude" = list(
      "en" = "Margin of error/ half width"
    ),

    "Perdas/ Recusas (%)" = list(
      "en" = "Losses/ Refusals (%)"
    ),

    "n + perdas/ recusas" = list(
      "en" = "n + losses/ refusals"
    ),

    "* sem considerar perdas/ recusas." = list(
      "en" = "* without considering losses/refusals."
    ),

    "Método utilizado para calcular o intervalo de confiança" = list(
      "en" = "The method to use to calculate precision"
    ),

    "Método utilizado para calcular o teste" = list(
      "en" = "Method used to calculate the test"
    ),


    # Warnings ----

    "Deve ser fornecido um valor." = list(
      "en" = "A value must be provided."
    ),

    "Deve ser menor do que 100%." = list(
      "en" = "Must be less than 100%."
    ),

    "Deve ser maior do que 0%." = list(
      "en" = "Must be greater than 0%."
    ),

    "Deve ser menor do que 1." = list(
      "en" = "Must be less than 1."
    ),

    "Deve ser maior do que 0.5" = list(
      "en" = "Must be greater than 0.5"
    ),

    "Deve ser maior do que 0." = list(
      "en" = "Must be greater than 0."
    ),

    "Deve ser maior do que -1." = list(
      "en" = "Must be greater than -1."
    ),

    "Deve ser um número inteiro positivo." = list(
      "en" = "Must be a positive integer."
    ),

    "Tem certeza que não considerarás perdas?" = list(
      "en" = "Are you sure you won't consider losses?"
    ),

    "Entrada inválida." = list(
      "en" = "Invalid input."
    ),

    "Para um estudo de não inferioridade a margem deve ser negativa." = list(
      "en" = "For a non-inferiority study, the margin must be negative."
    ),

    "Para um estudo de superioridade a margem deve ser positiva." = list(
      "en" = "For a superiority study the margin must be positive."
    ),

    "A soma das probabilidades deve ser 100%." = list(
      "en" = "The sum of the probabilities must be 100%."
    ),

    # Textos longos -----

    "até o final do seguimento" = list(
      "en" = "to the end of follow-up"
    ),

    "Um estudo pode ter como objetivo estimar ou testar o valor médio de uma variável quantitativa referente à população de interesse. Mais detalhes sobre o uso dessa aba em " = list(
      "en" = "A study may aim to estimate or test the mean value of a quantitative variable referring to the population of interest. More details on the use of this tab in "
    ),

    "Utilize os argumentos abaixo para construir diferentes cenários. Demais informações serão recuperadas do painel lateral." = list(
      "en" = "Use the arguments below to build different scenarios. Other information will be retrieved from the side panel."
    ),

    "<b>Preencha os campos abaixo de acordo com seu estudo para que sirvam de guia no preenchimento dos demais campos</b>." = list(
      "en" = "<b>Fill in the fields below according to your study to serve as a guide in filling in the other fields</b>."
    ),

    "Digite valores de" = list(
      "en" = "Enter values of"
    ),

    "Digite valores de amplitude (%) para fazer o gráfico" = list(
      "en" = "Enter amplitude values (%) to plot"
    ),

    "Digite valores de amplitude para fazer o gráfico" = list(
      "en" = "Enter amplitude values to plot"
    ),

    "Digite valores de desvio padrão para fazer o gráfico" = list(
      "en" = "Enter standard deviation values to plot"
    ),

    "Digite valores de poder (%) para fazer o gráfico" = list(
      "en" = "Enter power values (%) to plot"
    ),

    "Digite valores de nível de confiança (%) para fazer o gráfico" = list(
      "en" = "Enter confidence level (%) values to plot"
    ),

    "Defina a sequência de valores" = list(
      "en" = "Set the sequence of values"
    ),

    "Defina a sequência de valores para a diferença a ser detectada" = list(
      "en" = "Set the sequence of values for the difference to be detected"
    ),

    "Defina a sequência de valores para a margem de erro" = list(
      "en" = "Set the sequence of values for the margin of error"
    ),

    "Defina a sequência de valores para o desvio padrão" = list(
      "en" = "Define the sequence of values for the standard deviation"
    ),

    "Defina a sequência de valores (%) para o grupo" = list(
      "en" = "Set the sequence of values (%) for the group"
    ),

    "Defina a sequência do risco relativo" = list(
      "en" = "Set the relative risk sequence"
    ),

    "Defina a sequência da razão de chances" = list(
      "en" = "Set the odds ratio sequence"
    ),

    "Defina a sequência do hazard ratio" = list(
      "en" = "Set the hazard ratio sequence"
    ),

    "Defina a sequência para a área sob a curva" = list(
      "en" = "Set the sequence of values for the area under the curve"
    ),

    "Defina a sequência de valores para a magnitude do efeito" = list(
      "en" = "Set the sequence of values for the effect size"
    ),

    "Defina a sequência de valores para o ICC" = list(
      "en" = "Set the sequence of values for the ICC"
    ),

    "Defina a sequência de valores para a sensibilidade/ especificidade (%)" = list(
      "en" = "Set the sequence of values for sensitivity/ specificity (%)"
    ),

    "Defina a sequência da correlação" = list(
      "en" = "Set the correlation sequence"
    ),

    "Defina a sequência de valores para a amplitude do intervalo de confiança" = list(
      "en" = "Define the sequence of values for the confidence interval width"
    ),

    "Essa sequência será utilizada para compor o eixo x do gráfico." = list(
      "en" = "This sequence will be used to compose the x axis of the graph."
    ),

    "Essa sequência será utilizada para compor o eixo x do gráfico. A sequência irá do valor <b>Mínimo</b> até o valor <b>Máximo</b> em intervalos definidos no <b>Intervalo</b>." = list(
      "en" = "This sequence will be used to compose the x axis of the graph. The sequence starting <b>From</b> value ultil <b>To</b> value at intervals defined in the <b>By</b>."
    ),

    "Edite as matrizes de correlação no painel principal ao lado. A matriz é editável, basta clicar duas vezes sobre a célula" = list(
      "en" = "Edit the correlation matrix in the main panel on the side. The matrix is editable, just double click on the cell"
    ),


    "Utilizar os valores dos deltas de cada grupo" = list(
      "en" = "Use the delta values of each group"
    ),

    # Teste
    "v" = list(
      "en" = ""
    )

  )


  termo <- unname(unlist(list_text[[text]][language]))

  if (is.null(termo)) {
    return(text)
  } else {
    return(termo)
  }

}


# translation_pss(text = "Descreva a unidade de medida de", "pt")
# translation_pss(text = "Descreva a unidade de medida de", "en")




