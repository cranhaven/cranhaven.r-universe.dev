#' @import ggplot2
#' @importFrom plotly plot_ly ggplotly subplot renderPlotly plotlyOutput
#' @import shiny
#'
app_eng = function(){

  Number = NULL
  Frequency = NULL
  Freq = NULL
  layout = NULL
  Day = NULL
  winners = NULL

descriptions <- list(

  maismilionaria = "To win in MaisMilionaria, you need to match at least four of the six drawn numbers.",
  megasena = "In Mega-Sena, there are various prize tiers. To win the top prize (Sena), you must match all six drawn numbers. There are also prizes for matching five numbers (Quina) and four numbers (Quadra).",
  lotofacil = "In Lotofacil, there are different prize tiers. To win the top prize, you need to match all fifteen drawn numbers. There are also prizes for matching eleven, twelve, thirteen, or fourteen numbers.",
  quina = "In Quina, there are different prize tiers. To win the top prize, you need to match all five drawn numbers. There are also prizes for matching two, three, or four numbers.",
  lotomania = "In Lotomania, there are various prize tiers. To win the top prize, you need to match all twenty drawn numbers. Additionally, there are prizes for matching sixteen, seventeen, eighteen, or nineteen numbers.",
  duplasena = "In Dupla Sena, there are different prize tiers. To win the top prize (Sena), you need to match all six drawn numbers in either the first or the second draw. There are also prizes for matching five (Quina) or four (Quadra) numbers in one of the draws.",
  diadesorte = "In Dia de Sorte, you need to match seven drawn numbers plus the month to win the top prize. There are also prizes for matching six, five, four, or three numbers, regardless of the month."
)

# UI
ui <- fluidPage(
  titlePanel("Lotterybr App"),

  sidebarLayout(
    sidebarPanel(

      selectInput("jogo", "Choose Game:",
                  choices = c("maismilionaria", "megasena", "lotofacil",  "quina", "lotomania", "duplasena", "diadesorte")),

      selectInput("tipo", "Choose data type",
                  choices = c("numbers", "winners")),

      selectInput("grafico", "Choose graph type",
                  choices = c("Bar Chart")),

      conditionalPanel(
        condition = "input.tipo == 'winners'",
        checkboxInput("log_scale", "Use log scale", value = FALSE)
      ),

      verbatimTextOutput("summary_table")
    ),

    mainPanel(

      plotlyOutput("plot"),
      h5("Description: "),
      textOutput("dynamic_text"),
      h3("Data Table"),
      dataTableOutput("data_table")

    )
  )
)

# server
server <- function(input, output) {
  output$plot <- renderPlotly({
    dados <- get_data(input$jogo, input$tipo, language = "eng")
    ########################## NUMBERS ##########################

    if(input$tipo == "numbers"){
      if(input$jogo == "maismilionaria"){
        if (input$grafico == "Bar Chart"){

          resultado = grep("^(0[0-9]+|[1-9][0-9]+)$", dados$numbers_clovers, value = TRUE)
          df= data.frame(table(resultado))
          colnames(df) = c("Number","Frequency")

          fig = plot_ly(
            df,
            y = df$Frequency,
            x = df$Number,
            type = "bar"
          )
          fig
        }

      }
      else if(input$jogo == "megasena" || input$jogo == "quina" || input$jogo == "lotofacil"  || input$jogo == "lotomania"){
        if (input$grafico == "Bar Chart"){
          df =data.frame(table(dados$numbers))
          colnames(df) = c("Number","Frequency")

          p2 = ggplot(df, aes(x = Number, y = Frequency)) +
            geom_bar(stat = "identity", fill = "#1f77b4") +
            labs(title = "Bar Chart",
                 x = "Number",
                 y = "Frequency") +
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

        dft1$Number = as.numeric(as.character(dft1$Var1))
        dft2$Number = as.numeric(as.character(dft2$Var1))

        ggp1 <- ggplot(dft1, aes(x = Number, y = Freq)) +
          geom_bar(stat = "identity", fill = "salmon") +
          geom_hline(yintercept = mean_freq1, color = "blue", linetype = "dashed") +
          labs(title = "First Draw",
               x = "Number",
               y = "Frequency") +
          theme_minimal()

        ggp2 <- ggplot(dft2, aes(x = Number, y = Freq)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          geom_hline(yintercept = mean_freq2, color = "blue", linetype = "dashed") +
          labs(title = "Second Draw",
               x = "Number",
               y = "Frequency") +
          theme_minimal()


        p1 <- ggplotly(ggp1)
        p2 <- ggplotly(ggp2)

        subplot(p1, p2, nrows = 2, shareX = TRUE) %>%
          layout()


      }
      else if(input$jogo == "diadesorte"){
        df =data.frame(table(dados$numbers))
        colnames(df) = c("Day","Frequency")

        p2 = ggplot(df, aes(x = Day, y = Frequency)) +
          geom_bar(stat = "identity", fill = "#1f77b4") +
          labs(title = "Bar Chart",
               x = "Number",
               y = "Frequency") +
          theme_minimal()
        ggplotly(p2)

      }
    }

    ##################### WINNERS ##########################

    else if(input$tipo == "winners"){
      if (input$grafico == "Bar Chart"){
        if(input$log_scale){
          p = ggplot(dados, aes(x = factor((match)), y = log(winners+1), fill = match)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Match", y = "Winners frequency", title = "Winners by Match") +
            theme_minimal()
          ggplotly(p,tooltip = c("x","y","text"))
        }
        else{
          p = ggplot(dados, aes(x = factor(match), y = winners, fill = match)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Match", y = "Winners frequency", title = "Winners by Match") +
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
    dados = get_data(input$jogo, input$tipo, language = "eng")
  })
  # RENDER SUMMARY TAB
  output$summary_table  <- renderPrint({
    summary(get_data(input$jogo, input$tipo, language = "eng"))
  })
}

#exe

shinyApp(ui = ui, server = server)
}
