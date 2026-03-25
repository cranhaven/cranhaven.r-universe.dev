library(collapse)
library(DT)
library(htmltools)
library(magrittr)
library(rpivotTable)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

ui <- 
  shinydashboardPlus::dashboardPage(
    shinydashboardPlus::dashboardHeader(
      title = tagList(span(class = "logo-lg", "National Accounts App", style = "color: maroon; font-size: inherit;"), icon("leaf")),
      userOutput("user")
    ),
    
    # Sidebar for dataset selection and navigation between multiple pages
    shinydashboardPlus::dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "tabHome", icon = icon("home"), badgeLabel = "Home", badgeColor = "green"),
        menuItem("Introduction", tabName = "tabIntro", icon = icon("computer"), badgeLabel = "Info", badgeColor = "green"),
        menuItem("National Accounts", tabName = "tabNA", icon = icon("rupee-sign"), badgeLabel = "NA", badgeColor = "green")
      ),
      disable   = c(TRUE, FALSE)[2],
      width     = NULL,
      collapsed = c(TRUE, FALSE)[2]
    ),
    
    dashboardBody(
      
      tags$head(
        tags$style(
          "
            div {
              text-align: justify;
              text-justify: inter-word;
            }
            * {
              font-family: Latin Modern Roman; 
              font-size: 16px; 
              font-style: italic;
              font-weight: bold;
            }
            h1 {
              font-family: Latin Modern Roman; 
              font-size: 50px; 
              font-style: italic;
              font-weight: bold;
            }
            h2 {
              font-family: Latin Modern Roman; 
              font-size: 30px; 
              font-style: italic;
              font-weight: bold;
            }
            h3 {
              font-family: Latin Modern Roman; 
              font-size: 25px; 
              font-style: italic;
              font-weight: bold;
            }
            h4 {
              font-family: Latin Modern Roman; 
              font-size: 20px; 
              font-style: italic;
              font-weight: bold;
            }
            h5 {
              font-family: Latin Modern Roman; 
              font-size: 15px; 
              font-style: italic;
              font-weight: bold;
            }
            h6 {
              font-family: Latin Modern Roman; 
              font-size: 10px; 
              font-style: italic;
              font-weight: bold;
            } 
            .custom-shinydashboardPlus-box {
              height: 800px;  /* Increase shinydashboardPlus::box height */
              overflow-y: scroll;  /* Enable vertical scrolling */
            }
            .custom-pivot-table {
              width: 100%;  /* Take full width of the page */
              height: 5000px;  /* Adjust height of the pivot table */
            }
          "
        )
      ),
      
      tabItems(
        
        # Home Page
        tabItem(
          tabName = "tabHome",
          h1(tags$span("National Accounts App", style = "color: green; font-size: inherit;"), align = "center"),
          br()
          , h4(
                "Welcome to", tags$span("Pakistan's Quarterly National Accounts", style = "color: green; font-size: inherit;"), 
                "dashboard!"
              ,  align = "justify"
            )
          , br()
          , h4(
                "Explore our interactive tools to gain insights into Pakistan's economic performance. Visualize"
              , tags$span("quarterly trends", style = "color: green; font-size: inherit;")
              , "to identify patterns and anomalies in key economic indicators. Compare the contribution of sectors such as"
              , tags$span("agriculture, industry, and services", style = "color: green; font-size: inherit;")
              , "to understand their role in driving growth or decline. Customize your data views by filtering and 
                 manipulating the information, allowing you to focus on specific areas of interest. This tool is designed 
                 to support policymakers, researchers, and analysts in making informed decisions based on timely and 
                 detailed economic data. Dive into the data and explore Pakistan's economy like never before!"
              ,  align = "justify"
            )
        , br()
        , h4(
              tags$strong("Author/Maintainer:"),
              tags$a(href = "https://myaseen208.com/", "M. Yaseen", target = "_blank"),
              align = "center"
            )
         , h4(
              tags$strong("Contributor(s):"),
              tags$a(href = "mailto:g.zahid@gmail.com", "Zahid Asghar", target = "_blank"),
              align = "center"
            )
         , h4(
              tags$strong("Email:"),
              tags$a(href = "mailto:myaseen208@gmail.com", "myaseen208@gmail.com", target = "_blank"),
              align = "center"
            )
        , h4(
                tags$strong("Website:"),
                tags$a("https://myaseen208.com/PakNAcc/", href = "https://myaseen208.com/PakNAcc/", target = "_blank"),
                align = "center"
              )
           , br()
            , h5(
                  tags$strong("If you find this app to be useful, please let us know at ")
                , tags$a(href = "mailto:myaseen208@gmail.com", tags$strong("myaseen208@gmail.com."))
                , tags$strong("We can use this information to obtain more resources to make the app better. Thank you for your interest in our app!")
                , align = "justify"
                )
              
            ),
        
        # Introduction Page
        tabItem(
          tabName = "tabIntro"
        , h1(tags$span("Quarterly National Accounts", style = "color: green; font-size: inherit;"))
        
        , h2(tags$span("Introduction", style = "color: green; font-size: inherit;"))
        , p("Quarterly National Accounts (QNA) have emerged as an essential tool for policymakers, 
             researchers, and economic analysts. Introduced by the Pakistan Bureau of Statistics (PBS) 
             with the 2015-16 base year, these accounts provide a more frequent measure of economic 
             performance across different sectors. Unlike annual national accounts, which can only 
             show the long-term trajectory of an economy, QNA allows us to track economic fluctuations 
             in a more timely manner, making them vital for understanding economic dynamics in the short term.")
        
        , h2(tags$span("Why Are QNA Data Essential?", style = "color: green; font-size: inherit;"))
        , h3(tags$span("Timely Decision-Making", style = "color: green; font-size: inherit;"))
        , p("The quarterly data allow policymakers to respond swiftly to 
             emerging economic challenges. For instance, if a specific sector shows signs of contraction, 
             appropriate fiscal or monetary measures can be deployed more rapidly than waiting for annual data.")
        
        , h3(tags$span("Trend Analysis and Early Warning", style = "color: green; font-size: inherit;"))
        , p("QNA provides a comprehensive overview of trends, making it possible to spot early signs of 
              economic overheating or downturn. For a country like Pakistan, which has experienced 
              multiple economic challenges, the ability to see where an issue arises early is a 
              significant advantage.")
        
        , h3(tags$span("Sectoral Insights", style = "color: green; font-size: inherit;"))
        , p("The disaggregation by sector is crucial in understanding which parts of the 
             economy are contributing most to growth or decline. For example, is agriculture 
             underperforming due to specific seasonal factors, or is industry facing a 
             contraction due to external market conditions? Such insights are necessary for 
             formulating targeted interventions.")
        
          , br()
          , h5(
                tags$strong("If you find this app to be useful, please let us know at ")
              , tags$a(href = "mailto:myaseen208@gmail.com", tags$strong("myaseen208@gmail.com."))
              , tags$strong("We can use this information to obtain more resources to make the app better. Thank you for your interest in our app!")
              , align = "justify"
              )
        
        ),
        
        # NA Page
        tabItem(
          tabName = "tabNA",
          shinydashboardPlus::box(
            title       = h3("National Accounts"),
            status      = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed   = FALSE,
            icon        = icon("rupee-sign"),
            width       = 12,
            # h4(HTML(
            #   '<h4>This gives the data on:</h4>
            #   <ul style="text-align: justify;">
            #   <li>NACurrent: National Accounts Current</li>
            #   <li>GNICurrent: GNI Current</li>
            #   <li>NACurrent: National Accounts Constant</li>
            #   <li>GNIConstant: GNI Constant</li>
            #   </ul>'
            # )),
            # br(),
            radioButtons(
              "dataset", 
              h4("Select Data"), 
              choices = list(
                "NACurrent", 
                "GNICurrent", 
                "NAConstant",
                "GNIConstant"
              ),
              inline = TRUE
            ),
            fluidRow(
              shinydashboardPlus::box(
                width = 12, 
                class = "custom-shinydashboardPlus-box",
                rpivotTableOutput("PivotTable")
                # tabBox(
                #   id = "tabset1",
                #   width = 12,
                #   tabPanel(
                #     title = h3("Analysis"),
                #     rpivotTableOutput("PivotTable")
                #   )
                # )
              )
            )
            , br()
            , h5(
                  tags$strong("If you find this app to be useful, please let us know at ")
                , tags$a(href = "mailto:myaseen208@gmail.com", tags$strong("myaseen208@gmail.com."))
                , tags$strong("We can use this information to obtain more resources to make the app better. Thank you for your interest in our app!")
                , align = "justify"
                )
          )
        )
      )
    )
  )
