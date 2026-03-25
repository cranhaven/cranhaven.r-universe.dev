library(DT)
library(magrittr)
library(PakPC2017)
library(PakPC2023)
library(rpivotTable)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

ui <- 
  shinydashboardPlus::dashboardPage(
    shinydashboardPlus::dashboardHeader(
      title = tagList(span(class = "logo-lg", "Pakistan Population Census", style = "color: maroon; font-size: inherit;"), icon("leaf")),
      userOutput("user")
    ),
    
    # Sidebar for dataset selection and navigation between multiple pages
    shinydashboardPlus::dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "Home", icon = icon("home"), badgeLabel = "Home", badgeColor = "green"),
        menuItem("Introduction", tabName = "Intro", icon = icon("computer"), badgeLabel = "Info", badgeColor = "green"),
        menuItem("PakPC 2023", icon = icon("database"), startExpanded = TRUE,
                 menuSubItem("Basic", tabName = "PakPC2023Basic", icon = icon("person-arrow-up-from-line")),
                 menuSubItem("Education", tabName = "PakPC2023Edu", icon = icon("school")),
                 menuSubItem("FL & Disability", tabName = "PakPC2023Dis", icon = icon("person-walking")),
                 menuSubItem("Housing Census", tabName = "PakPC2023HC", icon = icon("house")),
                 menuSubItem("Listing", tabName = "PakPC2023Listing", icon = icon("book-open")),
                 menuSubItem("Miscellaneous", tabName = "PakPC2023Misc", icon = icon("person-circle-exclamation"))
        ),
        menuItem("PakPC 2017", tabName = "pakpc2017", icon = icon("database"), badgeLabel = "2017", badgeColor = "blue")
        
      )
      , disable   = c(TRUE, FALSE)[2]
      , width     = NULL
      , collapsed = c(TRUE, FALSE)[2]
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
      ", 
      HTML("
          .custom-box {
            height: 600px;  /* Increase box height */
            overflow-y: scroll;  /* Enable vertical scrolling */
          }
          .custom-pivot-table {
            width: 100%;  /* Take full width of the page */
            height: 5000px;  /* Adjust height of the pivot table */
          }
      
      ")
      )),
    
      tabItems(
        
        # Home Page
        tabItem(
          tabName = "Home",
          h1(tags$span("Pakistan Population Census Analysis", style = "color: green; font-size: inherit;"), align = "center"),
          br()
          , h4(
                "The", tags$span("Pakistan Population Census Analysis App", style = "color: green; font-size: inherit;"), 
                "offers a comprehensive platform for exploring and analyzing data from Pakistan's Population Censuses using the",
                tags$a(href = "https://cran.r-project.org/web/packages/PakPC2023/index.html", "PakPC2023", target = "_blank"),
                "and",
                tags$a(href = "https://cran.r-project.org/web/packages/PakPC2017/index.html", "PakPC2017", target = "_blank"),
                "R packages."
              ,  align = "justify"
            )
          , br()
          , h3(
            tags$span("Key Features", style = "color: green; font-size: inherit;")
            )
          , tags$ul(
                style = "text-align: justify;"
              , tags$li(tags$span("Data Exploration:", style = "color: green; font-size: inherit;"), "Enables detailed cross-tabulations and insights into census data.")
              , tags$li(tags$span("Numerical and Graphical Analysis:", style = "color: green; font-size: inherit;"), "Offers advanced tools for visualizing trends and distributions.")
              , tags$li(tags$span("Statistical Modeling:", style = "color: green; font-size: inherit;"), "Supports diverse models for data interpretation and predictive analysis.")
              , tags$li(tags$span("User-Friendly Interface:", style = "color: green; font-size: inherit;"), "Designed for researchers, policymakers, and professionals to make data-driven decisions with ease.")
              )
        , br()
        , h3(
            tags$span("Applications", style = "color: green; font-size: inherit;")
            )
          , tags$ul(
                style = "text-align: justify;"
              , tags$li(tags$span("Social Policy:", style = "color: green; font-size: inherit;"), "Analyze population trends for education, health, and social services planning.")
              , tags$li(tags$span("Economic Policy:", style = "color: green; font-size: inherit;"), "Gain insights for economic development and resource allocation.")
              , tags$li(tags$span("Research and Academia:", style = "color: green; font-size: inherit;"), "Enhance scholarly work with robust census data analysis tools.")
              )
        , br()
        , h4(
             tags$span("This app empowers users to extract meaningful insights from census data, 
                 driving effective planning and analysis across various domains.", style = "color: green; font-size: inherit;")
              ,  align = "justify"
            )
        , br()
        , h4(
              tags$strong("Author/Maintainer:"),
              tags$a(href = "https://myaseen208.com/", "Muhammad Yaseen", target = "_blank"),
              align = "center"
            )
         , h4(
              tags$strong("Contributor(s):"),
              tags$a(href = "mailto:pbsfsd041@gmail.com", "Muhammad Arfan Dilber;", target = "_blank"),
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
              tags$a(href = "https://myaseen208.com/PakPC", "https://myaseen208.com/PakPC",  target = "_blank"),
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
          tabName = "Intro",
          h1("Pakistan Population Census"),
          
          h2("Introduction"),
      
          p("The ",
            tags$a(href="https://www.pbs.gov.pk/", "Pakistan Bureau of Statistics (PBS)"),
            " holds the pivotal responsibility of orchestrating, collating, 
            and disseminating the outcomes of the population census within the nation. 
            On the significant date of August 5, 2023, the Council of Common Interests (CCI) 
            accorded its approval for the findings derived from the Census of 2023, 
            a comprehensive endeavor led by the PBS to capture the demographic landscape of 
            Pakistan. To facilitate widespread access to this crucial data, the PBS published 
            the conclusive results of the Census 2023 on its ",
            tags$a(href="https://www.pbs.gov.pk/", "official website"),
            " in a PDF format, albeit one that is not machine-readable."
          ),
        
          p("However, this presentation in PDF form poses a significant challenge to researchers 
            and analysts aiming to delve into the insights embedded within this valuable data. 
            The intricacies of extracting, processing, and interpreting information from a 
            non-machine-readable format like PDF hinder the swift utilization of this data for 
            in-depth analysis. Recognizing this impediment, a strategic solution has been crafted 
            in the form of an R programming language package termed ",
            tags$a(href="https://github.com/myaseen208/PakPC2023", "PakPC2023"), "."
          ),
          
          p("The ",
            tags$a(href="https://github.com/myaseen208/PakPC2023", "PakPC2023"),
            " R package has been meticulously developed to surmount the obstacles presented by 
            the non-machine-readable PDF format. This specialized software acts as a bridge, 
            effectively converting the cumbersome PDF-based Census 2023 data into a format that 
            is amenable for analysis within the R software environment. Researchers, analysts, 
            and data enthusiasts can seamlessly install the ",
            tags$a(href="https://github.com/myaseen208/PakPC2023", "PakPC2023"), 
            " package from the Comprehensive R Archive Network (CRAN), a repository that serves 
            as a hub for R packages, ensuring easy accessibility to this innovative tool."
          ),
          
          p("By implementing the ",
            tags$a(href="https://github.com/myaseen208/PakPC2023", "PakPC2023"),
            " package, the door is opened for researchers to harness the power of the 
            Census 2023 data in an efficient and effective manner. This strategic initiative 
            not only streamlines the process of accessing and utilizing the census data but 
            also empowers researchers to unlock valuable insights and trends that lie within, 
            contributing to informed decision-making and comprehensive socio-economic understanding."
          ),
          
          p("This dashboard allows you to explore datasets from the Pakistan Population Census of 2017 
            and 2023. Choose a dataset from the sidebar to generate tables and graphs."), 
          
          p("The ",
            tags$a(href="https://www.pbs.gov.pk/", "Pakistan Bureau of Statistics (PBS)"),
            "  holds the pivotal responsibility of orchestrating, collating, and disseminating 
            the outcomes of the population census within the nation. On the significant date of 
            August 5, 2023, the Council of Common Interests (CCI) accorded its approval for the 
            findings derived from the Census of 2023, a comprehensive endeavor led by the PBS to 
            capture the demographic landscape of Pakistan. To facilitate widespread access to 
            this crucial data, the PBS published the conclusive results of the Census 2023 on 
            its", 
            tags$a(href="https://www.pbs.gov.pk/", "official website"),
            "in a PDF format, albeit one that is not machine-readable."),
    
          p("However, this presentation in PDF form poses a significant challenge to researchers 
             and analysts aiming to delve into the insights embedded within this valuable data. 
             The intricacies of extracting, processing, and interpreting information from a 
             non-machine-readable format like PDF hinder the swift utilization of this data for 
             in-depth analysis. Recognizing this impediment, a strategic solution has been crafted 
             in the form of an R programming language package termed", 
             tags$a(href="https://github.com/myaseen208/PakPC2023", "PakPC2023"), "."),
    
          p("The",
            tags$a(href="https://github.com/myaseen208/PakPC2023", "PakPC2023"),  
            "R package has been meticulously developed to surmount the obstacles presented 
            by the non-machine-readable PDF format. This specialized software acts as a bridge, 
            effectively converting the cumbersome PDF-based Census 2023 data into a format 
            that is amenable for analysis within the R software environment. Researchers, analysts, 
            and data enthusiasts can seamlessly install the", 
            tags$a(href="https://github.com/myaseen208/PakPC2023", "PakPC2023"),  
            "package from the Comprehensive R Archive Network (CRAN), a repository that serves as a 
            hub for R packages, ensuring easy accessibility to this innovative tool."),
        
        p("By implementing the",
          tags$a(href="https://github.com/myaseen208/PakPC2023", "PakPC2023"),  
          "package, the door is opened for researchers to harness the power of the Census 2023 data 
          in an efficient and effective manner. This strategic initiative not only streamlines the 
          process of accessing and utilizing the census data but also empowers researchers to 
          unlock valuable insights and trends that lie within, contributing to informed 
          decision-making and comprehensive socio-economic understanding."),
        
        h3("Province Level"),
        
        br(),
        DT::DTOutput("OutPakPC2023Pak"),
        br(),
        
        tags$figure(
        tags$div(
          tags$img(src = "Plot1.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 1:"), "Population by Regions"), style = "text-align: center; font-style: italic;")
      ),
    
      p("The visual representation presented in Figure 1 provides a 
      comprehensive overview of the population distribution across 
      various regions of Pakistan. This data has been meticulously 
      collected from the Pakistan Population Census of 2023, serving 
      as an accurate reflection of the demographic landscape of the country."),
      
      p("A closer examination of the graph reveals that the province of 
        Punjab emerges as the most populous region, boasting an impressive 
        population count of approximately 128 million individuals. 
        Following closely, the province of Sindh contributes significantly 
        to the nation's population with approximately 56 million residents."),
      
      p("Turning our attention towards the remaining regions, 
         Khyber Pakhtunkhwa stands as a substantial population center with 
         around 41 million inhabitants, further emphasizing its role in the 
         overall demographic makeup of Pakistan. On the other hand, 
         Balochistan, while having a comparably smaller population, 
         still contributes notably with around 15 million people."),
      
      p("It is noteworthy that the data showcased in Figure 1 not only 
         underscores the population sizes of these regions but also 
         underscores the diversity and complexity of Pakistan's demographic 
         distribution. This information serves as a valuable resource for 
         policymakers, researchers, and anyone interested in gaining insights 
         into the population dynamics of Pakistan."),
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot2.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 2:"), "Population by Regions & Area"), style = "text-align: center; font-style: italic;")
      ),
      
      p("Figure 2 illustrates the distribution of Pakistan's population across 
         various regions and areas, providing insights drawn from the Pakistan 
         Population Census 2023. The data underscores a significant urban-rural 
         divide within the country, shedding light on the demographic landscape."),
      
      p("Notably, the data reveals that a substantial portion of Pakistan's 
         population resides in rural areas, accounting for a staggering figure 
        of more than 147.75 million individuals. In contrast, the urban population 
        stands at slightly over 93.75 million. This distribution underscores the 
        predominance of rural living conditions in Pakistan."),
      
      p("Examining the regional disparities, the province of Punjab emerges as the 
         leader in terms of rural population, boasting an impressive count of over 
         75.71 million inhabitants in rural settings. Sindh, on the other hand, 
         leads the pack in urban population, hosting a significant urban community of 
         over 29.92 million individuals."),
      
      p("The data presented within Figure 2 serves to accentuate the pronounced 
         urban-rural divide prevalent within Pakistan. The statistics reaffirm the 
         prevalence of rural living conditions, with Punjab standing out as the province 
         with the highest rural population, surpassing the 75.71 million mark. 
         The second spot in terms of rural population is claimed by Sindh, hosting more 
         than 25.77 million residents in rural areas. In contrast, Balochistan registers 
         as the province with the lowest rural population, counting slightly over 
         4.61 million individuals."),
      
      p("Shifting the focus to urban populations, Punjab once again takes the lead, 
         this time in the urban category, as it accommodates a substantial urban populace 
         exceeding 52.00 million. Sindh follows closely, contributing to the urban landscape 
         with over 29.90 million inhabitants. In stark contrast, Khyber Pakhtunkhwa and 
         Balochistan exhibit the smallest urban populations, each recording figures slightly 
         surpassing 6.10 million and 4.60 million individuals, respectively."),
      
      p("In conclusion, Figure 2 casts a spotlight on the demographic distribution within Pakistan, 
         drawing attention to the prevailing urban-rural divide. The robust presentation of data 
         underscores the dominance of rural populations, particularly in provinces like Punjab and 
         Sindh, while also providing valuable insights into the urban landscape across various regions 
         of the country."),
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot11.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 3:"), "Average Annual Population Growth Rate by Regions"), style = "text-align: center; font-style: italic;")
      ),
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot12.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 4:"), "Average Annual Population Growth Rate by Regions & Area"), style = "text-align: center; font-style: italic;")
      ),
      
      
      h3("Division Level"),
        
        br(),
        DT::DTOutput("OutPakPC2023PakDiv"),
        br(),
      
        tags$figure(
        tags$div(
          tags$img(src = "Plot3.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 5:"), "Population by Regions & Division"), style = "text-align: center; font-style: italic;")
      ),
        
      
        tags$figure(
        tags$div(
          tags$img(src = "Plot13.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 6:"), "Average Annual Population Growth Rate by Regions & Division"), style = "text-align: center; font-style: italic;")
      ),
      
      h3("District Level"),
        
        br(),
        DT::DTOutput("OutPakPC2023PakDist"),
        br(),
      
        tags$figure(
        tags$div(
          tags$img(src = "Plot4.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 7:"), "Population by Division & District for Punjab"), style = "text-align: center; font-style: italic;")
      ),
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot5.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 8:"), "Population by Division & District for Sindh"), style = "text-align: center; font-style: italic;")
      ),
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot6.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 9:"), "Population by Division & District for KP"), style = "text-align: center; font-style: italic;")
      ),
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot7.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 10:"), "Population by Division & District for Balochistan"), style = "text-align: center; font-style: italic;")
      ),
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot14.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 11:"), "Average Annual Population Growth Rate by Division & District for Punjab"), style = "text-align: center; font-style: italic;")
      ),
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot15.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 12:"), "Average Annual Population Growth Rate by Division & District for Sindh"), style = "text-align: center; font-style: italic;")
      ),
      
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot16.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 13:"), "Average Annual Population Growth Rate by Division & District for KP"), style = "text-align: center; font-style: italic;")
      ),
      
      tags$figure(
        tags$div(
          tags$img(src = "Plot17.webp", width = "95%"),
          style = "text-align: center;"
        ),
        tags$figcaption(p(strong("Figure 14:"), "Average Annual Population Growth Rate by Division & District for Balochistan"), style = "text-align: center; font-style: italic;")
      )
        ),
      
        # PakPC2023Basic Page
        tabItem(tabName = "PakPC2023Basic",
          h3("Pakistan Population Census 2023 (Basic)"),
          
          p("This give the basic data on Area, population by sex, sex ratio, population density, urban proportion, household size and annual growth rate")
          ,
          radioButtons(
            "dataset2023basic", 
            h4("Area, population by sex, sex ratio, population density, urban proportion, household size and annual growth rate"), 
            choices = list(
              "TABLE_01", 
              "TABLE_02", 
              "TABLE_03", 
              "TABLE_04", 
              "TABLE_05", 
              "TABLE_06", 
              "TABLE_07", 
              "TABLE_08", 
              "TABLE_09", 
              "TABLE_10", 
              "TABLE_11"
            ),
            inline = TRUE
          ),
           fluidRow(
              box(
                width = 12, 
                class = "custom-box",
                tabBox(
                  width = 12,
                  tabPanel(
                    title = h3("Data"),
                    DT::DTOutput("Out2023Basic")
                  ),
                  tabPanel(
                    title = h3("Analysis"),
                    rpivotTableOutput("Pivot2023Basic")
                  )
                )
              )
           )
          # fluidRow(
          #   box(
          #     title = "Tables & Graphs", 
          #     width = 12, 
          #     class = "custom-box",  # Custom class for the box
          #     #DT::DTOutput("Out2023Basic")
          #     rpivotTableOutput("Pivot2023Basic")
          #   )
          # )
        ),
      
        tabItem(tabName = "PakPC2023Edu",
          h3("Pakistan Population Census 2023 (Literacy and Education)"),
          radioButtons(
            "dataset2023edu", 
            h4("Literacy rate, enrolments, and out-of-school population by sex and rural/urban"), 
            choices = list(
              "TABLE_12", 
              "TABLE_13A", 
              "TABLE_13B"
            ),
            inline = TRUE
          ),
          fluidRow(
              box(
                width = 12, 
                class = "custom-box",
                tabBox(
                  width = 12,
                  tabPanel(
                    title = h3("Data"),
                    DT::DTOutput("Out2023Edu")
                  ),
                  tabPanel(
                    title = h3("Analysis"),
                    rpivotTableOutput("Pivot2023Edu")
                  )
                )
              )
           )
        ),
          
        tabItem(tabName = "PakPC2023Dis",
          h3("Pakistan Population Census 2023 (Functional Limitations & Disability)"),
          radioButtons(
            "dataset2023dis", 
            h4("Disability and functional limitation by region and gender"), 
            choices = list(
              "TABLE_16", 
              "TABLE_17"
            ),
            inline = TRUE
          ),
          fluidRow(
              box(
                width = 12, 
                class = "custom-box",
                tabBox(
                  width = 12,
                  tabPanel(
                    title = h3("Data"),
                    DT::DTOutput("Out2023Dis")
                  ),
                  tabPanel(
                    title = h3("Analysis"),
                    rpivotTableOutput("Pivot2023Dis")
                  )
                )
              )
           )
        ),
        
        tabItem(tabName = "PakPC2023HC",
          h3("Pakistan Population Census 2023 (Housing Census)"),
          radioButtons(
            "dataset2023hc", 
            h4("Type of housing unit by region"), 
            choices = list(
              "TABLE_20", 
              "TABLE_21",
              "TABLE_22",
              "TABLE_23",
              "TABLE_24",
              "TABLE_25"
            ),
            inline = TRUE
          ),
          fluidRow(
              box(
                width = 12, 
                class = "custom-box",
                tabBox(
                  width = 12,
                  tabPanel(
                    title = h3("Data"),
                    DT::DTOutput("Out2023HC")
                  ),
                  tabPanel(
                    title = h3("Analysis"),
                    rpivotTableOutput("Pivot2023HC")
                  )
                )
              )
           )
        ),
        
        tabItem(tabName = "PakPC2023Listing",
          h3("Pakistan Population Census 2023 (Listing)"),
          radioButtons(
            "dataset2023listing", 
            h4("Total number of structure by types and rural/urban"), 
            choices = list(
              "TABLE_26"
            ),
            inline = TRUE
          ),
          fluidRow(
              box(
                width = 12, 
                class = "custom-box",
                tabBox(
                  width = 12,
                  tabPanel(
                    title = h3("Data"),
                    DT::DTOutput("Out2023Listing")
                  ),
                  tabPanel(
                    title = h3("Analysis"),
                    rpivotTableOutput("Pivot2023Listing")
                  )
                )
              )
           )
        ),
        
        
        
        tabItem(tabName = "PakPC2023Misc",
          h3("Pakistan Population Census 2023 (Miscellaneous)"),
          radioButtons(
            "dataset2023misc", 
            h4("Miscellaneous"), 
            choices = list(
              "PakPC2023Pak",
              "PakPC2023PakDiv",
              "PakPC2023PakDist"
            ),
            inline = TRUE
          ),
          fluidRow(
              box(
                width = 12, 
                class = "custom-box",
                tabBox(
                  width = 12,
                  tabPanel(
                    title = h3("Data"),
                    DT::DTOutput("Out2023Misc")
                  ),
                  tabPanel(
                    title = h3("Analysis"),
                    rpivotTableOutput("Pivot2023Misc")
                  )
                )
              )
           )
        ),
      
        # Page for PakPC 2017 dataset selection
        tabItem(tabName = "pakpc2017",
          h3("Pakistan Population Census 2017"),
          radioButtons(
            "dataset2017", 
            h4("Choose Dataset"), 
            choices = list(
              "PakPC2017Pak",
              "PakPC2017Pakistan", 
              "PakPC2017Punjab", 
              "PakPC2017Sindh", 
              "PakPC2017KPK", 
              "PakPC2017Balochistan", 
              "PakPC2017FATA", 
              "PakPC2017Islamabad", 
              "PakPC2017Tehsil", 
              "PakPC2017City10"
            ),
            inline = TRUE
          ),
          fluidRow(
              box(
                width = 12, 
                class = "custom-box",
                tabBox(
                  width = 12,
                  tabPanel(
                    title = h3("Data"),
                    DT::DTOutput("Out2017")
                  ),
                  tabPanel(
                    title = h3("Analysis"),
                    rpivotTableOutput("Pivot2017")
                  )
                )
              )
           )
        )
      )
    )
  )
