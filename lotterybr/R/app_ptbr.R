#' @import ggplot2
#' @importFrom plotly plot_ly ggplotly subplot renderPlotly plotlyOutput
#' @import shiny
#'
app_ptbr = function(){
  plotlyOutput = NULL
  renderPlotly = NULL
  Numero = NULL
  Frequencia = NULL
  Freq = NULL
  layout = NULL
  Dia = NULL
  winners = NULL
descriptions <- list(

  maismilionaria = "Para ganhar na MaisMilionaria, e necessario acertar pelo menos quatro dos seis numeros sorteados.",
  megasena = "Na Mega-Sena, ha diversas faixas de premiacao. Para ganhar o premio maximo (Sena), e preciso acertar todos os seis numeros sorteados. Ha tambem premiacoes para quem acerta cinco numeros (Quina) e quatro numeros (Quadra).",
  lotofacil = "Na Lotofacil, existem diferentes faixas de premiacao. Para ganhar o premio maximo, e necessario acertar os quinze numeros sorteados. Ha tambem premiacoes para quem acerta onze, doze, treze ou quatorze numeros.",
  quina = "Na Quina, existem diferentes faixas de premiacao. Para ganhar o premio maximo, e necessario acertar os cinco numeros sorteados. Ha tambem premiacoes para quem acerta dois, tres ou quatro numeros.",
  lotomania = "Na Lotomania, ha diversas faixas de premiacao. Para ganhar o premio maximo, e necessario acertar todos os vinte numeros sorteados. Alem disso, ha premiacoes para quem acerta dezesseis, dezessete, dezoito ou dezenove numeros.",
  duplasena = "Na Dupla Sena, existem diferentes faixas de premiacao. Para ganhar o premio maximo (Sena), e necessario acertar os seis numeros sorteados no primeiro ou no segundo sorteio. Ha tambem premiacoes para quem acerta cinco (Quina) ou quatro (Quadra) numeros em um dos sorteios.",
  diadesorte = "No Dia de Sorte, e preciso acertar sete numeros sorteados mais o mes para ganhar o premio maximo. Ha tambem premiacoes para quem acerta seis, cinco, quatro ou tres numeros, independente do mes."
)

# UI
ui <- fluidPage(
  titlePanel("Aplicativo Loteria"),

  sidebarLayout(
    sidebarPanel(

      selectInput("jogo", "Escolha o tipo de jogo:",
                  choices = c("maismilionaria", "megasena", "lotofacil",  "quina", "lotomania", "duplasena", "diadesorte")),

      selectInput("tipo", "Escolha o tipo de dados:",
                  choices = c("numbers", "winners")),

      selectInput("grafico", "Escolha o tipo de grafico:",
                  choices = c("Grafico de Barras")),

      conditionalPanel(
        condition = "input.tipo == 'winners'",
        checkboxInput("log_scale", "Usar escala logaritmica", value = FALSE)
      ),

      verbatimTextOutput("summary_table")
    ),

    mainPanel(

      plotlyOutput("plot"),
      textOutput("dynamic_text"),
      h3("Tabela dos dados"),
      dataTableOutput("data_table")

    )
  )
)

# server
server <- function(input, output) {
  output$plot <- renderPlotly({
    dados <- get_data(input$jogo, input$tipo)
########################## NUMBERS ##########################

    if(input$tipo == "numbers"){
      if(input$jogo == "maismilionaria"){
        if (input$grafico == "Grafico de Barras"){

          resultado = grep("^(0[0-9]+|[1-9][0-9]+)$", dados$numbers_clovers, value = TRUE)
          df= data.frame(table(resultado))
          colnames(df) = c("Numero","Frequencia")

          fig = plot_ly(
            df,
            y = df$Frequencia,
            x = df$Numero,
            type = "bar"
          )
          fig
        }

      }
      else if(input$jogo == "megasena" || input$jogo == "quina" || input$jogo == "lotofacil"  || input$jogo == "lotomania"){
        if (input$grafico == "Grafico de Barras"){
          df =data.frame(table(dados$numbers))
          colnames(df) = c("Numero","Frequencia")

          p2 = ggplot(df, aes(x = Numero, y = Frequencia)) +
            geom_bar(stat = "identity", fill = "#1f77b4") +
            labs(title = "Grafico de Barras",
                 x = "Numero",
                 y = "Frequencia") +
            theme_minimal()
          ggplotly(p2)
        }

      }
      else if(input$jogo == "duplasena"){
        dft1 <- data.frame(table(dados$numbers1))
        dft1 <- dft1[dft1$Freq != 0,]

        dft2 <- data.frame(table(dados$numbers2))
        dft2 <- dft2[dft2$Freq != 0,]

        mean_freq1 <- mean(dft1$Freq)
        mean_freq2 <- mean(dft2$Freq)
        dft1$Numero = as.numeric(as.character(dft1$Var1))
        dft2$Numero = as.numeric(as.character(dft2$Var1))

        ggp1 <- ggplot(dft1, aes(x = Numero, y = Freq)) +
          geom_bar(stat = "identity", fill = "salmon") +
          geom_hline(yintercept = mean_freq1, color = "blue", linetype = "dashed") +
          labs(title = "Grafico de Barras do primeiro sorteio",
               x = "Numero",
               y = "Frequencia") +
          theme_minimal()

        ggp2 <- ggplot(dft2, aes(x = Numero, y = Freq)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          geom_hline(yintercept = mean_freq2, color = "blue", linetype = "dashed") +
          labs(title = "Grafico de Barras do segundo sorteio",
               x = "Numero",
               y = "Frequencia") +
          theme_minimal()


        p1 <- ggplotly(ggp1)
        p2 <- ggplotly(ggp2)

        subplot(p1, p2, nrows = 2, shareX = TRUE) %>%
          layout()


      }
      else if(input$jogo == "diadesorte"){
        df =data.frame(table(dados$numbers))
        colnames(df) = c("Dia","Frequencia")

        p2 = ggplot(df, aes(x = Dia, y = Frequencia)) +
          geom_bar(stat = "identity", fill = "#1f77b4") +
          labs(title = "Grafico de Barras",
               x = "Numero",
               y = "Frequencia") +
          theme_minimal()
        ggplotly(p2)

      }
    }

##################### WINNERS ##########################

    else if(input$tipo == "winners"){
      if (input$grafico == "Grafico de Barras"){
        if(input$log_scale){
          p = ggplot(dados, aes(x = factor((match)), y = log(winners+1), fill = match)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Combinacao", y = "Frequencia Ganhadores", title = "Ganhadores por combinacao") +
            theme_minimal()
          ggplotly(p,tooltip = c("x","y","text"))
        }
        else{
          p = ggplot(dados, aes(x = factor(match), y = winners, fill = match)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Combinacao", y = "Frequencia Ganhadores", title = "Ganhadores por combinacao") +
            theme_minimal()
          ggplotly(p,tooltip = c("x","y","text"))
        }

      }
    }
  })

  # TEXTO DINAMICO
  dynamic_text <- reactive({
    paste(descriptions[[input$jogo]])
  })

  # RENDER TEXTO
  output$dynamic_text <- renderText({
    dynamic_text()
  })
  # RENDER TAB
  output$data_table <- renderDataTable({
    dados = get_data(input$jogo, input$tipo, language = "ptbr")
  })
  # RENDER SUMMARY TAB
  output$summary_table  <- renderPrint({
    summary(get_data(input$jogo, input$tipo, language = "ptbr"))
  })
}

#exe

shinyApp(ui = ui, server = server)
}
