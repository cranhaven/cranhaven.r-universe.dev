#' @include global_var.R Person.R experiments.R viz_daily.R viz_sleep.R 
#'     viz_intraday.R
NULL

library(shiny)

ui <- fluidPage(
  # Application title
  titlePanel("lifelogr"),
  tabsetPanel(
    tabPanel("Sleep",
             sidebarLayout(
               sidebarPanel(
                   radioButtons(inputId = "sleep_measure", "Measures:",
                                c("By Weekday" = "by_weekday",
                                  "By Start and End Time" = "by_start_end_time",
                                  "By Date-Time" = "by_datetime",
                                  "By Proportion of Restless Sleep" = 
                                    "by_restless_prop",
                                  "By Length of Restless Sleep" = "by_restless_min",
                                  "By Quality of Sleep" = "by_quality"),
                                selected = "by_weekday"
                   )
               ),
               
               mainPanel(
                 plotOutput("sleepPlot")
               )
             )
    ),
    
    tabPanel("Daily Totals",
             sidebarLayout(
               sidebarPanel(
                 # user should be able to adjust units, etc.
                 radioButtons(inputId = "daily_measure", "Measures:",
                              c("Steps" = "steps",
                                "Floors" = "floors",
                                "Distance" = "distance",
                                "Calories Burned/Consumed" = "calories",
                                "Minutes 'Very Active'" = "mins_very",
                                "Resting Heart Rate" = "rest_hr"),
                              selected = "steps")
               ),
               mainPanel(
                 plotOutput("dailyPlot")
               )
             )
    ),
    
    tabPanel("Typical Day",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "typical_day_measure", "Measures:",
                              c("Steps" = "steps",
                                "Floors" = "floors",
                                "Distance" = "distance",
                                "Calories Burned" = "caloriesBurned",
                                "Minutes 'Active'" = "activeMin",
                                "Heart Rate" = "bpm",
                                "Weight" = "weight"),
                              selected = "steps"
                 )
               ),
               
               mainPanel(
                 plotOutput("typicalDayPlot")
               )
             )
    ),
    
    tabPanel("Over All Time in the Range",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "over_all_time_measure", "Measures:",
                              c("Steps" = "steps",
                                "Floors" = "floors",
                                "Distance" = "distance",
                                "Calories Burned" = "caloriesBurned",
                                "Minutes 'Active'" = "activeMin",
                                "Heart Rate" = "bpm",
                                "Weight" = "weight"),
                              selected = "steps"
                 )
               ),
               
               mainPanel(
                 plotOutput("overAllTimePlot")
               )
             )
    )
    
    
     )

)


server <- function(input, output) {

  output$sleepPlot <- renderPlot({
    plot_sleep(EX, input$sleep_measure)
  })
  
  output$dailyPlot <- renderPlot({
    plot_daily(EX, input$daily_measure)
  })
  
  output$typicalDayPlot <- renderPlot({
    plot_intraday(EX, input$typical_day_measure, TRUE)
  })
  
  output$overAllTimePlot <- renderPlot({
    plot_intraday(EX, input$over_all_time_measure, FALSE)
  })
  
}

shinyApp(ui, server)