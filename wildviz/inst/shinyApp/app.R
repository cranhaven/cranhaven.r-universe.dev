#
# This is a Shiny web application purpose-built for exploring wildfire, climate, and air quality data. It is part of the wildviz pacakge.
#
# For more info, contact: Brad Rafferty, brjrafferty@gmail.com ; Daniel Chung, dshchung@gmail.com

library(shiny)
library(tidyr)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(plotly)

# Load county map data for California for plotting
county_map <- ggplot2::map_data('county', 'california') %>%
  dplyr::rename(FIPS_NAME = subregion) %>%
  dplyr::transmute(long_map = long,
            lat_map = lat,
            group = group,
            FIPS_NAME = FIPS_NAME)

master <- master %>%
  # Add burn time parameter (duration of fire in days)
  # If fire was contained on same day it was discovered, make that burn time 1 instead of 0 to avoid confusion in plotting
  dplyr::mutate(BURN_TIME = ifelse(as.numeric(cont_date - disc_date) == 0, 1, as.numeric(cont_date - disc_date))) %>%
  # Make "timeline" a variable describing whether non-fire data in an observation (climate, aqi) occur before, during, or after discovery date
  dplyr::mutate(timeline = case_when(clim_date < disc_date ~ "before",
                              clim_date >= disc_date & clim_date <= cont_date ~ "during",
                              clim_date > disc_date ~ "after"),
         timeline = factor(x = timeline, levels = c("before", "during", "after"))) %>% # convert to a factor
  # Change county names to lower case (helps joining with county map data later)
  dplyr::mutate(FIPS_NAME = tolower(FIPS_NAME)) %>%
  # Distinguish names by appending discovery date to avoid duplicate names (not uncommon in dataset)
  dplyr::mutate(FIRE_NAME = sprintf('%s_%s', FIRE_NAME, disc_date))


wildfires <- wildfires %>%
  # Add burn time variable
  dplyr::mutate(BURN_TIME = ifelse(as.numeric(CONT_DATE - DISCOVERY_DATE) == 0, 1, as.numeric(CONT_DATE - DISCOVERY_DATE)))

# Make a dataframe of top 25 fires ranked by burn time
df_plot1_explore_fireTimeline <- master %>%
  # Find unique wildfires
  dplyr::distinct(FIRE_NAME, disc_date, BURN_TIME) %>%
  # Select top 25 rows
  dplyr::top_n(25, BURN_TIME) %>%
  # Order in desc
  dplyr::arrange(BURN_TIME) %>%
  # Retain only the join keys
  dplyr::select(FIRE_NAME, disc_date) %>%
  # Join back to master to pick up all dates
  dplyr::inner_join(master, by = c("FIRE_NAME", "disc_date"))

# Units for plot labels
units_label <- list("aqi" = "", "prcp" = ", mm", "snow" = ", mm", "tmax" = ", C", "tmin" = ", C",
                   "ozone" = ", ppm", "co" = ", ppm", "pm25" = ", mcg/m3", "pm10" = ", mcg/m3",
                   "no2" = ", ppb", "FIRE_SIZE" = ", acres", "BURN_TIME" = ", days")
# Clean variable names for plot labels
var_label <- list("aqi" = "AQI", "prcp" = "Precipitation", "snow" = "Snowfall", "tmax" = "Max Temp", "tmin" = "Min Temp",
                  "ozone" = "Ozone", "co" = "CO", "pm25" = "PM2.5", "pm10" = "PM10",
                  "no2" = "NO2", "FIRE_SIZE" = "Fire Size", "BURN_TIME" = "Burn Time")

ui <- fluidPage(theme = shinythemes::shinytheme(theme = "united"),

  # Application title
  titlePanel("wildviz: a Wildfire and Climate Data Exploration Tool"),
  navbarPage("",
             tabPanel(icon("home"),
                      fluidRow(
                               column(width=8,

                                   br(),
                                   p("This dashboard is intended to: (1) provide an overview and inspiration of interesting trends in and links between wildfire data and climate data;
                                     (2) offer easy-to-modify visualizations to uncover more of the goals in item (1)! The data provided span from 2011 through 2015 and include common wildfire, climate, and air quality data for California only.",
                                     style="font-size:16px;text-align:justify;color:black;background-color:orange;padding:15px;border-radius:10px"),
                                   br(),

                                   p("The data used in this application are publicly available from these institutions:", a(href = "https://www.kaggle.com/rtatman/188-million-us-wildfires", "Kaggle,"), "the",
                                     a(href = "https://aqs.epa.gov/aqsweb/documents/data_api.html", "Environmental Protection Agency,"), "and the",
                                     a(href = "https://www.ncdc.noaa.gov/cdo-web/search", "National Oceanic and Atmospheric Administration."),
                                   "Data used in this dashboard were retrieved March 2020 and focus specifically on California.",
                                    style="font-size:16px;text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px")
                                   ), # column
                               column(width = 4, align = "center",
                                   img(src="ca_fires_2001-2015.png",width="280px",height="280px"),
                               ) # column
                       ), # fluidRow

                      hr(), # formatting
                      hr(),

                      p(em("Dashboard developed by:"),br("Brad Rafferty, brjrafferty@gmail.com"),style="text-align:center; font-family: times"),
                      p(em("Data infrastructure developed by:"),br("Daniel Chung, dshchung@gmail.com"),style="text-align:center; font-family: times")
             ), # tabPanel home
             tabPanel("Examples",
                      fluidRow(
                          h1("Let's take a look at some quick example analyses", style="text-align:center"),
                          hr(),
                          column(width = 3,
                                 p("When broken down by month, we see that wildfires are most frequent around summer months.
                                   We can look to other data to see why might this be (of course, we already have an intuition as to why).",
                                   style="font-size:24px;text-align:justify;color:Red")
                                 ), # column text_examples_1
                          column(width = 1), # spacer
                          column(width = 7,
                                 plotOutput(outputId = "plot_examples_1")
                                 ) # column plot_examples_1
                      ), # fluidRow 1
                      hr(),
                      fluidRow(
                          column(width = 3,
                                 p("The mean daily temperatures each month are highest in Summer. No surprise there.
                                   Higher temperatures lead to drier conditions that make forests more succeptible to wildfires.",
                                   style="font-size:24px;text-align:justify;color:Orange"),
                          ), # column text_examples_2
                          column(width = 1), # spacer
                          column(width = 7,
                                 plotOutput("plot_examples_2")
                          ) # column plot_examples_2
                      ), # fluidRow 2
                      hr(),
                      fluidRow(
                          column(width = 3,
                                 p("Additionally, precipitation drops drastically in summer months.
                                   Drought compounds on high temperatures to create ideal conditions for a fire.",
                                   style="font-size:24px;text-align:justify;color:Blue"),
                          ), # column text_examples_3
                          column(width = 1), # spacer
                          column(width = 7,
                                 plotOutput("plot_examples_3")
                          ) # column plot_examples_3
                      ), # fluidRow 3
                      hr(),
                      fluidRow(
                          column(width = 3, align = "center",
                                 HTML("<p style='color:black;font-size:18px;text-align:justify'> When we take a look at what causes wildfires each month, we find that",
                                      "<span style='color:MediumAquamarine;'>lightning</span>",
                                      "is much more prevalent in causing wildfires in the summer. This is interesting since precipitation, and hence storms, are less common in the summer. Other insights include:",
                                      "<span style='color:Peru;'>children</span>",
                                      "are consistently mischevious year-round,",
                                      "<span style='color:Chocolate;'>camping</span>",
                                      "picks up in spring and summer",
                                      "<span style='color:Olive;'>debris burning</span>",
                                      "(e.g. yard waste) is most common following winter,",
                                      "<span style='color:IndianRed;'>arsonists</span>",
                                      "prefer warmer months, and, surprisingly, the contribution from",
                                      "<span style='color:MediumSeaGreen;'>fireworks</span>",
                                      "in California is practically indistinguishable. Switch the barchart y-axis between number and percentage using the buttons below.</p>"
                                      ),
                                 br(),
                                 radioButtons(inputId = "plot_examples_4_bar_position",
                                              label = "Barchart Type:",
                                              choices = c("Numbers" = "stack",
                                                          "Percent" = "fill"),
                                              )# radioButtons
                            ), # column text_examples_4
                          column(width = 1), # spacer
                          column(width = 8,
                                 plotOutput("plot_examples_4", height = 700)
                          ) # column plot_examples_4
                      ), # fluidRow 4
                      hr(),
                      fluidRow(
                          column(width = 3,
                                 p("We have not yet addressed the severity of wildfires. A simple metric of fire severity is the size of the fire.
                                   It is evident that summer wildfires far out-size those in any other season, with the interesting exception in October!
                                   How does this fact affect air quality, for example?",
                                   style="font-size:24px;text-align:justify;color:red"),
                          ), # column text_examples_5
                          column(width = 1), # spacer
                          column(width = 7,
                                 plotOutput("plot_examples_5")
                          ) # column plot_examples_5
                      ), # fluidRow 5
                      hr(),
                      fluidRow(
                          column(width = 3, align = "center",
                                 p("On average, the air quality index (AQI) is higher in the summer. Unintuitively, higher AQI means worse air quality.
                                   Visualize other mean air quality metrics using the dropdown below.",
                                   style="font-size:24px;text-align:justify;color:darkgreen"),
                                 br(),
                                 selectInput(inputId = "plot_examples_6_quality_metric",
                                              label = "Air Quality Metric:",
                                              choices = c("AQI" = "aqi",
                                                          "CO" = "co",
                                                          "Ozone" = "ozone",
                                                          "NO2" = "no2",
                                                          "PM10" = "pm10",
                                                          "PM2.5" = "pm25"),
                                 ) # selectInput
                          ), # column text_examples_6
                          column(width = 1), # spacer
                          column(width = 7,
                                 plotOutput("plot_examples_6")
                          ) # column plot_examples_6
                      ) # fluidRow 6
                  ), # tabPanel Examples
             tabPanel("Explore",
                      h1("A self-guided deep dive into the data", style="text-align:center"),
                      hr(),
                      tabsetPanel(type = "tabs",
                               tabPanel(title = "Maps",
                                        sidebarLayout(
                                            sidebarPanel(width = 3,
                                                dateRangeInput(inputId = "dates_plot1_explore_maps",
                                                               label = "Choose date range:",
                                                               start = min(wildfires$DISCOVERY_DATE),
                                                               end = max(wildfires$DISCOVERY_DATE),
                                                               min = min(wildfires$DISCOVERY_DATE),
                                                               max = max(wildfires$DISCOVERY_DATE)),
                                                selectInput(inputId = "var_plot1_explore_maps",
                                                            label = "Choose parameter to plot:",
                                                            choices = c("Fire Size" = "FIRE_SIZE",
                                                                        "Burn Time" = "BURN_TIME"))
                                            ), # sidebarPanel
                                            mainPanel(width = 8,
                                                  plotOutput("plot1_explore_maps")
                                             ) # mainPanel

                                        ), # sidebarLayout
                                       sidebarLayout(
                                         sidebarPanel(width = 3,
                                                      dateRangeInput(inputId = "dates_plot2_explore_maps",
                                                                     label = "Choose date range:",
                                                                     start = min(wildfires$DISCOVERY_DATE),
                                                                     end = max(wildfires$DISCOVERY_DATE),
                                                                     min = min(wildfires$DISCOVERY_DATE),
                                                                     max = max(wildfires$DISCOVERY_DATE)),
                                                      selectInput(inputId = "var_plot2_explore_maps",
                                                                  label = "Choose parameter to plot:",
                                                                  choices = c("Precipitation" = "prcp",
                                                                              "Max daily temperature" = "tmax",
                                                                              "Min daily temperature" = "tmin",
                                                                              "Snowfall" = "snow",
                                                                              "AQI" = "aqi",
                                                                              "CO" = "co",
                                                                              "Ozone" = "ozone",
                                                                              "NO2" = "no2",
                                                                              "PM2.5" = "pm25",
                                                                              "PM10" = "pm10")),
                                                      p("Note: certain parameters may be lacking data depending on availability from the given weather stations.",
                                                        style="font-size:10px;text-align:justify;color:gray;background-color:white;padding:15px;border-radius:10px")
                                         ), # sidebarPanel
                                         mainPanel(width = 8,
                                                   plotOutput("plot2_explore_maps")
                                         ) # mainPanel

                                       ), # sidebarLayout
                               ), # tabPanel Maps
                               tabPanel(title = "Distributions",
                                        sidebarLayout(
                                            sidebarPanel(width = 3,
                                                         dateRangeInput(inputId = "dates_plot1_explore_distributions",
                                                                        label = "Choose date range:",
                                                                        start = min(wildfires$DISCOVERY_DATE),
                                                                        end = max(wildfires$DISCOVERY_DATE),
                                                                        min = min(wildfires$DISCOVERY_DATE),
                                                                        max = max(wildfires$DISCOVERY_DATE)),
                                                         selectInput(inputId = "var1_plot1_explore_distributions",
                                                                     label = "Choose parameter to rank top 100 wildfires:",
                                                                     choices = c("Fire Size" = "FIRE_SIZE",
                                                                                 "Burn Time" = "BURN_TIME")),
                                                         selectInput(inputId = "var2_plot1_explore_distributions",
                                                                     label = "Choose parameter to plot:",
                                                                     choices = c("AQI" = "aqi",
                                                                                 "Ozone" = "ozone",
                                                                                 "CO" = "co",
                                                                                 "NO2" = "no2",
                                                                                 "PM2.5" = "pm25",
                                                                                 "PM10" = "pm10",
                                                                                 "Precipitation" = "prcp",
                                                                                 "Max daily temperature" = "tmax",
                                                                                 "Min daily temperature" = "tmin",
                                                                                 "Snowfall" = "snow")),
                                                         p("Note: certain parameters may be lacking data depending on availability from the given weather stations.",
                                                           style="font-size:10px;text-align:justify;color:gray;background-color:white;padding:15px;border-radius:10px")
                                            ), # sidebarPanel
                                            mainPanel(width = 8,
                                                      plotlyOutput("plot1_explore_distributions"),
                                                      plotlyOutput("plot2_explore_distributions")
                                            ) # mainPanel
                                        ) # slidebarLayout
                               ), # tabPanel
                               tabPanel(title = "Fire Timeline",
                                        sidebarLayout(
                                          sidebarPanel(width = 3,
                                                       selectInput(inputId = "fireName_plot1_explore_fireTimeline",
                                                                   label = "Choose from Fires with Top 25 Burn Time:",
                                                                   choices = unique(df_plot1_explore_fireTimeline$FIRE_NAME)),
                                                       selectInput(inputId = "var1_plot1_explore_fireTimeline",
                                                                   label = "Choose parameter for first plot:",
                                                                   choices = c("AQI" = "aqi",
                                                                               "Ozone" = "ozone",
                                                                               "CO" = "co",
                                                                               "NO2" = "no2",
                                                                               "PM2.5" = "pm25",
                                                                               "PM10" = "pm10",
                                                                               "Precipitation" = "prcp",
                                                                               "Max daily temperature" = "tmax",
                                                                               "Min daily temperature" = "tmin",
                                                                               "Snowfall" = "snow")),
                                                       selectInput(inputId = "var1_plot2_explore_fireTimeline",
                                                                   label = "Choose parameter for second plot:",
                                                                   choices = c("AQI" = "aqi",
                                                                               "Ozone" = "ozone",
                                                                               "CO" = "co",
                                                                               "NO2" = "no2",
                                                                               "PM2.5" = "pm25",
                                                                               "PM10" = "pm10",
                                                                               "Precipitation" = "prcp",
                                                                               "Max daily temperature" = "tmax",
                                                                               "Min daily temperature" = "tmin",
                                                                               "Snowfall" = "snow")),
                                                       p("Note: certain parameters may be lacking data depending on availability from the given weather stations.",
                                                         style="font-size:10px;text-align:justify;color:gray;background-color:white;padding:15px;border-radius:10px")
                                            ), # sidebarPanel
                                            mainPanel(width = 8,
                                                      plotlyOutput("plot1_explore_fireTimeline"),
                                                      plotlyOutput("plot2_explore_fireTimeline")
                                            ) # mainPanel
                                        ) # slidebarLayout
                               )# tabPanel Fire Timeline
                      ) # tabsetPanel
              ) # tabpanel Explore
    ) # navbarPage
) # fluidPage

server <- function(input, output) {

    # Common plot formatting options
    format_plot <- reactive({
        ggplot2::theme(text = element_text(size = 20),
              plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
              panel.background = element_rect(fill = "white",
                                              colour = "white"),
              panel.grid.major = element_line(size = 0.5,
                                              linetype = "solid",
                                              color = "light gray"))
    }) # format_plot


                              # # # # # # # # #
# # # # # # # # # # # # # # # # Examples tab  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                              # # # # # # # # #
# # # # # # #
# REACTIVES #
# # # # # # #
    group_by_disc_date <- reactive({
        # group by month of fire discovery date; month() converts from date to numeric month;
        # month.abb[] takes numeric month and converts to abbreviated month character string
        # factor(_, levels = month.abb) ensures that the months will be in chronological order and not alphabetical order when plotted
        wildfires %>%
        dplyr::group_by(month = factor(month.abb[lubridate::month(DISCOVERY_DATE)], levels = month.abb))
    }) # group_by_disc_Date

    group_by_clim_date <- reactive({
        # group by month of climate data
        climate %>%
        dplyr::group_by(month = factor(month.abb[lubridate::month(date)], levels = month.abb))
    }) # group_by_clim_Date

    group_by_disc_date_cause <- reactive({
        # group by month and by cause of the fire
        wildfires %>%
            dplyr::group_by(month = factor(month.abb[lubridate::month(DISCOVERY_DATE)], levels = month.abb), cause = STAT_CAUSE_DESCR)
    }) # group_by_disc_date_cause

    group_by_aqi_date <- reactive({
        # group by aqi month
        aqi %>%
            dplyr::group_by(month = factor(month.abb[lubridate::month(date)], levels = month.abb))
    }) # group_by_disc_date_cause


# # # # #
# PLOTS #
# # # # #
    output$plot_examples_1 <- renderPlot({
        group_by_disc_date() %>%
        dplyr::summarize(num_fires = n()) %>%
        ggplot2::ggplot(aes(x = month, y = num_fires)) +
        ggplot2::geom_bar(stat = 'identity', fill = 'red') +
        ggplot2::geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', size = 0.4, color = 'blue') +
        ggplot2::labs(x = '', y = 'Number of wildfires', title = 'California Wildfires by Month') +
        format_plot()
    }) # plot_examples_1

    output$plot_examples_2 <- renderPlot({
        group_by_clim_date() %>%
        dplyr::summarize(mean_tmax = mean(tmax, na.rm = TRUE)) %>%
        ggplot2::ggplot(aes(x = month, y = mean_tmax)) +
        ggplot2::geom_bar(stat = 'identity', fill = 'orange') +
        ggplot2::labs(x = '', y = 'Temperature, C', title = 'Mean Max Daily Temperature by Month') +
        format_plot()
    }) # plot_examples_2

    output$plot_examples_3 <- renderPlot({
        group_by_clim_date() %>%
        dplyr::summarize(mean_prcp = mean(prcp, na.rm = TRUE)) %>%
        ggplot2::ggplot(aes(x = month, y = mean_prcp)) +
        ggplot2::geom_bar(stat = 'identity', fill = 'blue') +
        ggplot2::labs(x = '', y = 'Precipitation, mm', title = 'Mean Precipitation by Month') +
        format_plot()
    }) # plot_examples_3

    output$plot_examples_4 <- renderPlot({
        # Cause of fire by month, stacked bar chart
        group_by_disc_date_cause() %>%
        dplyr::summarize(count_cause = n()) %>%
        ggplot2::ggplot(aes(fill = cause, x = month, y = count_cause)) +
        ggplot2::geom_bar(position = input$plot_examples_4_bar_position, stat = 'identity') +
        ggplot2::labs(x = '', y = 'Cause Count', title = 'Causes of Wildfires by Month') +
        format_plot()
    }) # plot_examples_4

    output$plot_examples_5 <- renderPlot({
        # Mean size of wildfires for each month
        group_by_disc_date() %>%
        dplyr::summarize(mean_size = mean(FIRE_SIZE)) %>%
        ggplot2::ggplot(aes(x = month, y = mean_size)) +
        ggplot2::geom_bar(stat = 'identity', fill = 'red') +
        ggplot2::labs(x = '', y = 'Fire Size, acres', title = 'Mean Size of Wildfires by Month') +
        format_plot()
    }) # plot_examples_5

    output$plot_examples_6 <- renderPlot({
        value = sym(input$plot_examples_6_quality_metric)
        group_by_aqi_date() %>%
        dplyr::summarize(!! input$plot_examples_6_quality_metric := mean(!! value, na.rm = TRUE)) %>%
        ggplot2::ggplot(aes(x = month, y = !!as.name(input$plot_examples_6_quality_metric))) +
        ggplot2::geom_bar(stat = 'identity', fill = 'dark green') +
        ggplot2::labs(x = '',
             y = sprintf('%s%s', var_label[[input$plot_examples_6_quality_metric]], units_label[[input$plot_examples_6_quality_metric]]),
             title = 'Mean Air Quality Metric by Month') +
        format_plot()
    }) # plot_examples_6


                              # # # # # # # #
# # # # # # # # # # # # # # # # Explore tab # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
                              # # # # # # # #

# # # # # # #
# REACTIVES #
# # # # # # #
    format_map_plot <- reactive({
      ggplot2::scale_size_continuous() +
        ggplot2::scale_color_viridis_c(trans = "log") +
        theme_void() + ggplot2::coord_quickmap()
    }) # format_map_plot

    data_plot1_explore_maps <- reactive({
      wildfires %>%
        dplyr::filter(DISCOVERY_DATE >= input$dates_plot1_explore_maps[[1]] & DISCOVERY_DATE <= input$dates_plot1_explore_maps[[2]])
      }) # data_plot1_explore_maps

    data_plot2_explore_maps <- reactive({
      master %>%
        dplyr::filter(disc_date >= input$dates_plot2_explore_maps[[1]] & disc_date <= input$dates_plot2_explore_maps[[2]]) %>%
        dplyr::group_by(FIPS_NAME = tolower(FIPS_NAME)) %>%
        dplyr::summarize(mean_var = as.numeric(mean(!!as.name(input$var_plot2_explore_maps), na.rm = TRUE))) %>%
        dplyr::right_join(county_map, by = "FIPS_NAME")
    }) # data_plot2_explore_maps

    data_plot1_explore_distributions <- reactive({
      master %>%
        # Find unique wildfires
        dplyr::distinct(FIRE_NAME, disc_date, !!as.name(input$var1_plot1_explore_distributions)) %>%
        # Select top 25 rows
        dplyr::top_n(100, !!as.name(input$var1_plot1_explore_distributions)) %>%
        # Order in desc
        dplyr::arrange(!!as.name(input$var1_plot1_explore_distributions)) %>%
        # Retain only the join keys
        dplyr::select(FIRE_NAME, disc_date) %>%
        # Join back to master to pick up all dates
        dplyr::inner_join(master, by = c("FIRE_NAME", "disc_date")) %>%
        # Filter by user-input date range
        dplyr::filter(disc_date >= input$dates_plot1_explore_distributions[[1]] & disc_date <= input$dates_plot1_explore_distributions[[2]])
    }) # data_plot1_explore_distributions

    data_plot1_explore_fireTimeline <- reactive({
      df_plot1_explore_fireTimeline %>%
        dplyr::filter(FIRE_NAME == input$fireName_plot1_explore_fireTimeline)
    }) # data_plot2_explore_maps

# # # # #
# Plots #
# # # # #

# # Explore tab, maps panel
# # # # # # # # # # # # # #

    # Excellent resource for bubble graphs: https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2.html
    output$plot1_explore_maps <- renderPlot({
        ggplot2::ggplot() +
        ggplot2::geom_polygon(data = county_map, aes(x = long_map, y = lat_map, group = group), fill="gray", alpha=0.5) +
        ggplot2::geom_point(data = data_plot1_explore_maps(),
                   aes(x = LONGITUDE,
                       y = LATITUDE,
                       size = !!as.name(input$var_plot1_explore_maps),
                       color = !!as.name(input$var_plot1_explore_maps)),
                   alpha = 1) +
        ggplot2::scale_size_continuous(name = sprintf('%s%s', var_label[[input$var_plot1_explore_maps]], units_label[[input$var_plot1_explore_maps]])) +
        ggplot2::scale_color_viridis_c(name = sprintf('%s%s', var_label[[input$var_plot1_explore_maps]], units_label[[input$var_plot1_explore_maps]]), trans = "log") +
        ggthemes::theme_map() + ggplot2::coord_quickmap()  # account for projection corrections for more accurate display
    }) # plot1_explore_maps

    # The following visualization is inspired by: https://www.kaggle.com/captcalculator/wildfire-exploratory-analysis
    output$plot2_explore_maps <- renderPlot({
        ggplot2::ggplot(data = data_plot2_explore_maps(),
               aes(x = long_map, y = lat_map, group = group, fill = mean_var)) +
        ggplot2::geom_polygon() +
        ggplot2::geom_path(color = 'white', size = 0.1) +
        ggplot2::scale_fill_continuous(low = "pink",
                              high = "darkred",
                              name = sprintf('%s%s', var_label[[input$var_plot2_explore_maps]], units_label[[input$var_plot2_explore_maps]])) +
        ggthemes::theme_map() + # rids plot of extra labels, etc.
        ggplot2::coord_quickmap() +
        ggplot2::ggtitle("") +
        ggplot2::theme(plot.title = element_text(hjust = 0.5))
    }) # plot2_explore_maps

# # Explore tab, distributions panel
# # # # # # # # # # # # # # # # # # #

    output$plot1_explore_distributions <- renderPlotly({
      p <- ggplot2::ggplot(data = data_plot1_explore_distributions(),
             aes(x = !!as.name(input$var2_plot1_explore_distributions),
                 fill = timeline)) +
        ggplot2::geom_density(alpha = 0.2) +
        ggplot2::scale_fill_manual(values =  c("steelblue3", "firebrick3", "seagreen")) + # set before = blue and after = red to be more intuitive
        ggplot2::labs(x = sprintf('%s%s', var_label[[input$var2_plot1_explore_distributions]], units_label[[input$var2_plot1_explore_distributions]]),
             y = 'Density',
             fill = "Timeline",
             title = sprintf('%s Throughout Fire Timeline', var_label[[input$var2_plot1_explore_distributions]])) +
        format_plot() +
        ggplot2::theme(legend.title = element_blank())
      plotly::ggplotly(p) # Use ggplotly to make zooming and other plot manipulation very user-friendly
    }) # plot1_explore_distributions

    output$plot2_explore_distributions <- renderPlotly({
      p <- ggplot2::ggplot(data = data_plot1_explore_distributions(),
                  aes(x = timeline,
                      y = !!as.name(input$var2_plot1_explore_distributions),
                      color = timeline)) +
      ggplot2::geom_boxplot(outlier.shape = 1) +
      ggplot2::scale_color_manual(values =  c("steelblue3", "firebrick3", "seagreen")) +
      ggplot2::labs(x = '', y = sprintf('%s%s', var_label[[input$var2_plot1_explore_distributions]], units_label[[input$var2_plot1_explore_distributions]])) +
      format_plot() +
      ggplot2::theme(legend.title = element_blank())
      plotly::ggplotly(p)
  }) # plot2_explore_distributions

# # Explore tab, Fire Timeline panel
# # # # # # # # # # # # # # # # # # #

    output$plot1_explore_fireTimeline <- renderPlotly({
      p <- ggplot2::ggplot(data = data_plot1_explore_fireTimeline(),
                  aes(x = clim_date,
                      y = !!as.name(input$var1_plot1_explore_fireTimeline),
                      color = timeline)) +
        ggplot2::geom_line() +
        # Add vertical lines to help visualize the start and end of the fire
        ggplot2::geom_vline(aes(xintercept = as.numeric(disc_date[[1]])), linetype = "dashed", color = "firebrick3") +
        ggplot2::geom_vline(aes(xintercept = as.numeric(disc_date[[1]] + BURN_TIME[[1]])), linetype = "dashed", color = "seagreen") +
        ggplot2::labs(x = '',
             y = sprintf('%s%s', var_label[[input$var1_plot1_explore_fireTimeline]], units_label[[input$var1_plot1_explore_fireTimeline]]),
             title = 'Parameters Throughout Fire Timeline') +
        ggplot2::scale_color_manual(values = c("steelblue3", "firebrick3", "seagreen")) +
        format_plot() +
        ggplot2::theme(legend.title = element_blank())
      plotly::ggplotly(p)
    }) # plot1_explore_fireTimeline

    output$plot2_explore_fireTimeline <- renderPlotly({
      p <- ggplot2::ggplot(data = data_plot1_explore_fireTimeline(),
                  aes(x = clim_date,
                      y = !!as.name(input$var1_plot2_explore_fireTimeline),
                      color = timeline)) +
        ggplot2::geom_line() +
        ggplot2::geom_vline(aes(xintercept = as.numeric(disc_date[[1]])), linetype = "dashed", color = "firebrick3") +
        ggplot2::geom_vline(aes(xintercept = as.numeric(disc_date[[1]] + BURN_TIME[[1]])), linetype = "dashed", color = "seagreen") +
        ggplot2::labs(x = '',
             y = sprintf('%s%s', var_label[[input$var1_plot2_explore_fireTimeline]], units_label[[input$var1_plot2_explore_fireTimeline]])) +
        ggplot2::scale_color_manual(values = c("steelblue3", "firebrick3", "seagreen")) +
        format_plot() +
        ggplot2::theme(legend.title = element_blank())
      plotly::ggplotly(p)
    }) # plot2_explore_fireTimeline

}

# Run the application
shinyApp(ui = ui, server = server)
