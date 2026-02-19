# Define UI for application ---------------------------
shinyAppUI <- dashboardPage(

  #Skin color of app
  skin = "blue",

  ##Title content
  dashboardHeader(title ="MDMAPR 2.0"),


  ##Sidebar content
  dashboardSidebar(
    sidebarMenu(

      #To allow app to use functions from shinyjs package
      useShinyjs(),

      #ID values for sidebar menu items
      id = "tab_being_displayed",

      #Icons for sidebar were obtained from https://fontawesome.com/icons?from=io
      menuItem("Welcome", tabName = "Welcome", icon = icon("user-circle")),
      menuItem("Mapping Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Overview", tabName = "qPCRDataOverviewPage", icon = icon("chart-bar")),
      menuItem("Data Submission", tabName = "DataSubmission", icon = icon("database")),
      menuItem("Available Assays", tabName = "availableData", icon = icon("archive")),
      menuItem("Get Started", tabName = "getStarted", icon = icon("seedling")),
      menuItem("FAQs", tabName = "faqs", icon = icon("question-circle"))

    )
  ),


  ##Body content
  dashboardBody(

    #Style tag for website name, located in left top corner of page
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: Verdana, Geneva, sans-serif;
        font-size: 24px;
      }
    '))),




    tabItems(

      #Mapping Dashboard ---------------------------
      tabItem(tabName = "dashboard",

              h1(strong("Mapping Dashboard")),

              p("Perform geospatial analysis based on collection locations for run qPCR samples.",
                style = "font-size:16px;"),

              fluidRow(

                #Adding static valueboxes
                valueBoxOutput("sampleBox",  width = 3),
                valueBoxOutput("platformBox", width = 3),
                valueBoxOutput("taxonBox", width = 3),
                valueBoxOutput("assayBox", width = 3),

                fluidRow(

                  column(width = 12,
                         box(width = NULL, solidHeader = TRUE,
                             leafletOutput("mymap", height = 460),

                             #Second Row on page for filter functions
                             fluidRow(

                               div(style='height:300px; overflow-y: scroll',
                                   box( width = 12,
                                        title = "Filter Options",
                                        status = "warning",
                                        solidHeader = TRUE,
                               column(2,

                                      #Radio button to select what type of Cq value you want to based on threshold vaue (User provided or system calculated)
                                      radioButtons("thresholdValueButton",
                                                   "Select Threshold Value to view associated Cq Value:",
                                                   c("User Provided Threshold" = 10,
                                                     "System Calculated Threshold" = 12)),

                                      #Slider for Cq intensity
                                      sliderInput("range", "Select the Cq Intensity",
                                                  min = 0,
                                                  max = 40,
                                                  step = 0.10, value=c(0,40)),

                                      #Slider for date
                                      sliderInput("date_input",
                                                  "Select Event Date Range",
                                                  min =as.Date("2010-01-01", "%Y-%m-%d"),
                                                  max = as.Date(Sys.Date(),"%Y-%m-%d"),
                                                  value = range(c(as.Date("2010-01-01", "%Y-%m-%d"), as.Date(Sys.Date(), "%Y-%m-%d"))),
                                                  timeFormat = "%Y-%m-%d",
                                                  step = 1),

                                      #Dropdown menu for Continent
                                      pickerInput(inputId = "continent_input",
                                                  "Continent",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for countrys
                                      pickerInput(inputId = "country_input", "Country",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for state or province
                                      pickerInput(inputId = "stateProvince_input",
                                                  "State or Province",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for locality
                                      pickerInput(inputId = "locality_input",
                                                  "Locality",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE)


                               ),

                               column(4, offset = 1,


                                      #Dropdown menu for family type
                                      pickerInput(inputId = "family_input",
                                                  "Family",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for genus type
                                      pickerInput(inputId = "genus_input",
                                                  "Genus",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for species type
                                      pickerInput(inputId = "species_input",
                                                  "Species",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for ct intensity
                                      pickerInput(inputId = "CqIntensity_input",
                                                  "Cq Intensity",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for Machine type
                                      pickerInput(inputId = "machine_input",
                                                  "Machine Type",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for Target gene
                                      pickerInput(inputId = "targetGene_input",
                                                  "Target Gene",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for project ID
                                      pickerInput(inputId = "projectID_input", "Project",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),


                                      #Dropdown menu for assay
                                      pickerInput(inputId = "assay_input", "Assay",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE),

                                      #Dropdown menu for establishment means
                                      pickerInput(inputId = "establishmentMeans_input",
                                                  "Establishment Means",
                                                  choices = "None",
                                                  selected = "None",
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE)

                               ),


                               #qPCR Data file upload
                               column(4, offset = 1,

                                      #Upload Data to view on map
                                      h4("Upload Data "),


                                      #Upload qPCR experimental fluorescence file
                                      fileInput("qpcr_file",
                                                "Upload qPCR Experimental Fluorescence File (csv/xlsx/xls)",
                                                multiple = FALSE,
                                                accept = c(".csv", ".xlsx", ".xls")),

                                      #Upload metadata file
                                      fileInput("metadata_file", "Upload Metadata File (xlsx/xls)",
                                                multiple = FALSE,
                                                accept = c(".xlsx", ".xls")),

                                      #Select qPCR run platform
                                      selectInput(inputId = "platform",
                                                  label = "qPCR Platform",
                                                  choices = c("None", "StepOnePlus", "Biomeme two3/Franklin", "MIC"),
                                                  selected = "None",
                                                  multiple = FALSE),

                                      fluidRow(textOutput("error_msg")),

                                      fluidRow(column(4, actionButton("submit",
                                                                      "Submit  Files")),
                                               column(4, actionButton("reset",
                                                                      "Reset Files"))))

                             ))))))),

              #Download Mapped data
              fluidRow(p(strong("Mapped Markers Metadata"),
                         style = "font-size:25px"),
                       downloadLink("downloadFilteredData",
                                    p("Download Mapped Markers Metadata",
                                      style = "font-size:16px;
                                      color:#F4412E;
                                       text-decoration: underline;" )),
                       br()),

              #Data table for mapped data.
              fluidRow(DT::dataTableOutput("mapping_data"))),


      #qPCR Data Overview page ---------------------------
      tabItem(tabName = "qPCRDataOverviewPage",

              h1(strong("qPCR Data Overview")),

              p("Analyze individual well samples for a qPCR run.",
                style = "font-size:16px;"),

              br(),

              #Dropdown menu to select standard curve to view
              fluidRow(

                tabBox(id = "tabset1",
                       width = 3,

                       #Tab1
                       tabPanel("Data",
                                column(12,

                                       br(),


                                       p("To analyze qPCR experimental data and standard curve data associated with a specific project please select Assay, then Machine Type, then Project Name, and then Standard Curve. Then Press the 'Submit' button.",  style = "font-size:16px;"),

                                       #Dropdown menu for assay name
                                       pickerInput(inputId = "SC_assay_input",
                                                   "Assay",
                                                   choices = "None",
                                                   selected = "None",
                                                   multiple = FALSE),


                                       #Dropdown menu for machine type
                                       pickerInput(inputId = "SC_machine_input",
                                                   "Machine Type",
                                                   choices = "None",
                                                   selected = "None",
                                                   multiple = FALSE),


                                       #Dropdown menu for project
                                       pickerInput(inputId = "SC_project_input",
                                                   "Project Name",
                                                   choices = "None",
                                                   selected = "None",
                                                   multiple = FALSE),


                                       #Dropdown menu for standard curve
                                       pickerInput(inputId = "SC_input",
                                                   "Standard Curve",
                                                   choices = "None",
                                                   selected = "None",
                                                   multiple = FALSE),

                                       br(),


                                       #Submit button and reset button
                                       fluidRow(column(4,
                                                       actionButton("DA_submit",
                                                                    "Submit  Files")),
                                                column(4,
                                                       offset = 3,
                                                       actionButton("DA_reset",
                                                                    "Reset Files"))))),

                       tabPanel("Upload Files",

                                column(12,

                                       br(),

                                       p("To analyze external qPCR files, upload a qPCR experimental fluorescence file, a qPCR standard curve fluorescence file, and a filled in metadata file. Press the 'Submit' button and then navigate back to the 'Data' panel to view the updated dropdown menu options.",  style = "font-size:16px;"),

                                       #User uploaded fluorescence and metadata files
                                       fileInput("qPCR_fluorescence_file",
                                                 "Upload qPCR  Experimental Fluorescence File (csv/xlsx/xls)",
                                                 multiple = FALSE,
                                                 accept = c(".xlsx", ".xls", ".csv")),

                                       fileInput("SC_fluorescence_file",
                                                 "Upload Standard Curve Fluorescence File (csv/xlsx/xls)",
                                                 multiple = FALSE,
                                                 accept = c(".xlsx", ".xls", ".csv")),

                                       fileInput("qPCR_metadata_file",
                                                 "Upload Metadata File (xlsx/xls)",
                                                 multiple = FALSE,
                                                 accept = c(".xlsx", ".xls")),


                                       #Select qPCR run platform
                                       selectInput(inputId = "DA_platform",
                                                   label = "qPCR Platform",
                                                   choices = c("None",
                                                               "StepOnePlus",
                                                               "Biomeme two3/Franklin",
                                                               "MIC"),
                                                   multiple = FALSE),

                                       #Uploaded files submit and reset button
                                       fluidRow(column(4,
                                                       actionButton("Uploaded_DA_submit",
                                                                    "Submit  Files")),

                                                column(4, actionButton("Uploaded_DA_reset",
                                                                       "Reset Files")))))),


                #Standard curve plot
                tabBox(

                  title = strong("Data Analysis"),
                  id = "data_analysis_box",
                  height = 1000,
                  width = 9,

                  tabPanel("Presence/Absense Samples",

                           strong("Select a radio button to view associated amplification curve on 'Amplification Plot' tab."), p("Cq Value cells are coloured based on the Cq Cutoff value. Cells coloured in orange refer to positive target sequence detections and cells coloured in blue refer to negative detections."),

                           numericInput(inputId = "cqValueCutoff",
                                        label = h3("Enter Cq Cutoff Value",
                                                   style = "font-size:15px;"),
                                        value = 40,
                                        min = 1,
                                        max = 40),

                           reactableOutput("presence_absence_table")),

                  tabPanel("Amplification Plot",  plotlyOutput("selected")),

                  tabPanel("Standard Curve Data Overview",

                           strong("Data from the 'Standard Curve Data Overview' table is visualized in the 'Standard Curve Plot' tab."),

                           DT::dataTableOutput('SC_overview_table')),

                  tabPanel("Standard Curve Plot",
                           strong("The residual gradient colour scheme depicts how well the standard curve fits the data points. Yellow points are best fit by the curve, dark purple points are least fit, and orange points are in between."), p("LOD = Limit of Detection, LOQ = Limit of Quantification"),

                           plotlyOutput("standardCurve_plot"))))),



      #Data Submission page  ---------------------------
      tabItem(tabName = "DataSubmission",

              h1(strong("Data Submission")),

              p("The tool below can be used to format qPCR fluorescence data and associated metadata into a format that is acceptable for the MDMAPR 2.0 database. If the correct data is uploaded,  13 CSV (i.e: projectTable.csv, geographicRegion.csv, siteTable.csv, stationTable.csv, replicateTable.csv, extractTable.csv, resultsTable.csv, runInformationTable.csv, pcrChemistryTable.csv, standardCurveTable.csv, standardCurveResults.csv, assayTable.csv, and taxonTable.csv) should be generated. Check the preview table outputs to verify that your data is correct. The generated 13 CSVs can be downloaded as a zipped file by pressing the 'Download Data Submission Files' button. NOTE: Before you can upload the generated CSV data files into their respective tables in the MDMAPR 2.0 database, the ID columns (projectID, geographicRegionID, siteID, stationID, replicateID, extractID, assayID, runID, pcrChemistryID, resultID, standardCurveID, and SCresultID ) must be manually changed from alphabetical characters to unique numeric IDs.", style = "font-size:16px;"),

              br(),

              h4("Upload Data"),

              #Data input fields
              fluidRow(

                #qPCR Data (CSV File Input)
                column(3,

                       #Upload qPCR Experimental Fluorescence File
                       fileInput("DS_qpcr_file",
                                 "Upload qPCR Experimental Fluorescence File (csv/xlsx/xls)",
                                 multiple = FALSE,
                                 accept = c(".csv", ".xlsx", ".xls"))),

                column(3,

                       #Upload standardd curve fluorescence file
                       fileInput("DS_standardCurve_fluorescence_file",
                                 "Upload Standard Curve Fluorescence File (csv/xlsx/xls)",
                                 multiple = FALSE,
                                 accept = c(".csv", ".xlsx", ".xls"))),

                column(3,

                       #Upload metadata file
                       fileInput("DS_metadata_file", "Upload Metadata File (xlsx/xls)",
                                 multiple = FALSE,
                                 accept = c(".csv", ".xlsx", ".xls"))),


                column(3,

                       #Select qPCR run platform
                       selectInput(inputId = "DS_platform",
                                   label = "qPCR Platform",
                                   choices = c("None", "StepOnePlus", "Biomeme two3/Franklin", "MIC"),
                                   multiple = FALSE))
              ),

              #Output text if error in uploaded files
              fluidRow(textOutput("DS_error_msg")),

              fluidRow(column(1, actionButton("DS_reset", "Reset Files"))),

              br(),

              #Downloaded Parsed Data files
              fluidRow(downloadLink("downloadData",

                                    p("Download Data Submission Files",

                                      style = "font-size:16px;
                                      color:#F4412E;
                                      text-decoration: underline;"))),

              #Preview of the 13 parsed data files.
              fluidRow(

                tabsetPanel(
                  id = 'dataset',
                  tabPanel("Project Data", DT::dataTableOutput("projectTable")),
                  tabPanel("Geographic Region Data",
                           DT::dataTableOutput("geographicRegionTable")),
                  tabPanel("Site Data", DT::dataTableOutput("siteTable")),
                  tabPanel("Station Data", DT::dataTableOutput("stationTable")),
                  tabPanel("Replicate Data", DT::dataTableOutput("replicateTable")),
                  tabPanel("Extract Data", DT::dataTableOutput("extractTable")),
                  tabPanel("Run Information Data",
                           DT::dataTableOutput("runInformationTable")),
                  tabPanel("Results Data", DT::dataTableOutput("resultsTable")),
                  tabPanel("Assay Data", DT::dataTableOutput("assayTable")),
                  tabPanel("Taxonomic Data", DT::dataTableOutput("taxonomicTable")),
                  tabPanel("PCR Chemistry Data",
                           DT::dataTableOutput("pcrChemistryTable")),
                  tabPanel("Standard Curve Data",
                           DT::dataTableOutput("standardCurveTable")),
                  tabPanel("Standard Curve Results Data",
                           DT::dataTableOutput("standardCurveResultsTable"))

                ))),




      #Welcome page ---------------------------
      tabItem(tabName = "Welcome",

              box(solidHeader = TRUE,
                  width = 12,

                  #Header for page
                  h1(strong("MDMAPR 2.0"), align = "Center",
                     style = "font-size:50px;"),

                  br(),

                  p("DEVELOPED TO SUPPORT THE CENTRALIZATION, STANDARDIZATION, AND ANALYSIS OF QPCR DATA", align = "Center", style = "font-size:18px;"),

                  p("The Molecular Detection Mapping and Analysis Platform for R 2.0 (MDMAPR 2.0), is an extensible open source tool to enable researchers to visualize qPCR data and store it in a central standardized location. It consists of three main dashboards: the Mapping Dashboard, the Data Overview page, and the Data Submission page.",  align = "Center",   style = "font-size:16px; font-family: 'Arial', 'Helvetica', sans-serif;"),

                  br(),

                  fluidRow( column(4, h1(icon("dashboard", "fa-4x"),
                                         align = "Center"),
                                   h3(strong("Mapping Dashboard"),
                                      align = "Center",
                                      style = "font-size:30px;"),

                                   p(em("Perform Geospatial analysis on qPCR samples"),
                                     align = "Center",
                                     style = "font-size:16px;") ),

                            column(4, h1(icon("chart-bar", "fa-4x"), align = "Center"),
                                   h3(strong("Data Overview"),
                                      align = "Center",
                                      style = "font-size:30px;"),

                                   p(em("Analyze individual well samples"),
                                     align = "Center",
                                     style = "font-size:16px;") ),

                            column(4, h1(icon("database", "fa-4x"), align = "Center"),
                                   h3( strong("Data Submission"),
                                       align = "Center",
                                       style = "font-size:30px;"),

                                   p(em("Format data to be added to the MDMAPR 2.0 database"),
                                     align = "Center",
                                     style = "font-size:16px;"))),


                  fluidRow(column(4,
                                  box(
                                    solidHeader = TRUE,
                                    width = 12,
                                    height = 160,
                                    p(
                                      "The Mapping Dashboard is used to perform geospatial analysis on qPCR run samples. An interactive map displays qPCR run sample collection locations, provides the ability to filter data, and create exportable customizable reports.",
                                      align = "justify",
                                      style = "font-size:16px; font-family: 'Arial', 'Helvetica', sans-serif;"))),

                           column(4,
                                  box(
                                    solidHeader = TRUE,
                                    width = 12,
                                    height = 160,
                                    p(
                                      "The Data Overview page is used to analyze individual qPCR tube/well samples and allows for quality control inspection of data. Users can inspect amplification curves and standard curves, and export publication quality images.",
                                      align = "justify",
                                      style = "font-size:16px; font-family: 'Arial', 'Helvetica', sans-serif;"))),

                           column(4,
                                  box(
                                    solidHeader = TRUE,
                                    width = 12,
                                    height = 160,
                                    p("The Data Submission page is used to format qPCR fluorescence data and metadata into a format that can be uploaded into the MDMAPR 2.0 MySQL database for storage.",
                                      align = "justify",
                                      style = "font-size:16px; font-family: 'Arial', 'Helvetica', sans-serif;"))))),


              fluidRow(box(
                title = strong("Overview",
                               style = "font-size:30px; font-family: 'Verdana', 'Geneva', sans-serif;"),
                solidHeader = TRUE,
                width = 12,
                p("The MDMAPR 2.0 is an extension of the MDMAPR 1.0 application but incorporates significant upgrades for the inclusion and analysis of raw qPCR data and associated metadata. The newest version of the system has the ability to implement an open source MySQL relational database for data storage of raw qPCR fluorescence data and metadata, which increases the utility of collected qPCR data beyond single study application. As well, the MDMAPR 2.0 Mapping Dashboard now includes expanded filtering capabilities and the functionality to create exportable customizable reports. Furthermore, a new qPCR Data Overview page enables the analysis of individual tube/well samples from a qPCR plate and can facilitate quality control inspection of data. Lastly, on the newly added Data Submission page, raw fluorescence data and metadata can be formatted to be added to the MDMAPR 2.0 MySQL database.",
                  align = "justify",
                  style = "font-size:16px; font-family: 'Arial', 'Helvetica', sans-serif;"))),


              fluidRow(box(
                title = strong("GitHub",
                               style = "font-size:30px; font-family: 'Verdana', 'Geneva', sans-serif;"),
                solidHeader = TRUE,
                width = 12,
                p("To learn more about the MDMAPR 2.0 application and view the applications code please visit the ", tags$a(href="https://github.com/AlkaBenawra/MDMAPR", "MDMAPR 2.0 GitHub page."),
                  align = "justify",
                  style = "font-size:16px; font-family: 'Arial', 'Helvetica', sans-serif;"))),),

      #Get Started page ---------------------------
      tabItem(tabName = "getStarted",

              h1(strong("Get Started")),

              p("The MDMAPR 2.0 is an analysis platform for the standardization, centralization, and visualization of qPCR data. It consists of three main dashboards: the Mapping Dashboard, the Data Overview page, and the Data Submission page. ", style = "font-size:16px;"),

              br(),

              #Mapping dashboard instructions
              fluidRow(box(
                title = p("Mapping Dashboard", style = "font-size:16px;"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                p("The Mapping Dashboard is used to perform geospatial analysis on qPCR run samples. On this page, an interactive map displays location markers for qPCR run sample collection locations and allows for filtering of markers by Cq intensity, date, location, taxon details, machine type, project, and assay. Hovering above a certain marker will display a pop-up menu that shows information about the specific marker. As well, the page contains a data table which shows detailed information about the markers on the map. The data table will update based on the used filters. A copy of the data table can be downloaded as a CSV by pressing the 'Download Mapped Markers Metadata' button. As well, users have the option to directly upload their own qPCR data to view on the map. The user must upload a raw qPCR experimental fluorescence file and the filled in MDMAPR 2.0 metadata file. The qPCR machine types that are currently supported by the system include MIC, StepOnePlus, and Biomeme two3/Franklin.",   style = "font-size:16px;"))),


              #Data analysis page instructions
              fluidRow(box(
                title = p("Data Overview Page",
                          style = "font-size:16px;"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                p("The Data Overview page is used to analyze individual tube/well samples and facilitates the quality control inspection of data. The page has four tabs, which include the 'Presence/Absence Samples', 'Amplification Plot', 'Standard Curve Data Overview', and 'Standard Curve Plot'. The 'Presence/Absence Samples' tab displays a table which indicates if a target sequence was detected in a tube/well based on its Cq Value. The 'Amplification Plot' shows the amplification curve associated with a specific well sample from the 'Presence/Absence Samples' tab. The 'Standard Curve Data Overview' tab displays a table with information related to the standard curve used for the samples in the 'Presence/Absence Samples' tab. Lastly, the 'Standard Curve Plot' tab shows the standard curve plot of the data from the 'Standard Curve Data Overview' tab. Users also have the option to directly upload their own qPCR data to analyze on the page. The user must upload a raw qPCR experimental fluorescence file, a raw standard curve fluorescence file, and the filled in MDMAPR 2.0 metadata file. The qPCR machine types that are currently supported by the system include MIC, StepOnePlus, and Biomeme two3/Franklin.",  style = "font-size:16px;"))),

              #Data Submission page instructions
              fluidRow(box(
                title = p("Data Submission",
                          style = "font-size:16px;"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                p("The Data Submission page is used to format qPCR fluorescence data and associated metadata into a format that is acceptable to be added to the MDMAPR 2.0 database for storage. On this page, a raw qPCR experimental fluorescence file, a raw standard curve fluorescence file, and the filled in MDMAPR 2.0 metadata file are required. The Data Submission tool will parse the data into 13 CSV files. A preview of the tables is viewable on the page. A zipped file of the CSVs can be downloaded by pressing the 'Download Data Submission Files' button. NOTE: Before you can upload the generated CSV data files into their respective tables in the MDMAPR 2.0 database, the ID columns (projectID, geographicRegionID, siteID, stationID, replicateID, extractID, assayID, runID, pcrChemistryID, resultID, standardCurveID, and SCresultID) must be manually changed from alphabetical characters to unique numeric IDs.", style = "font-size:16px;"))),


              #Metadata Template Description
              fluidRow(box(
                title = p("Metadata Template",
                          style = "font-size:16px;"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                p("To learn how to fill in the MDMAPR 2.0 Metadata Template please visit the ",
                  tags$a(href="https://github.com/AlkaBenawra/MDMAPR", "MDMAPR 2.0 GitHub page."),  "The wiki page contains instructions on how to fill in the Metadata template excel file and has a complete guide with descriptions for each field in the metadata template.",  style = "font-size:16px;" ),

                downloadLink("downloadTemplate",
                             p("Click Here to Download the Metadata Template",
                               style = "font-size:16px;
                                             color:#F4412E;
                                              text-decoration: underline;"))

              )),

              #Github page
              fluidRow(box(
                title = p("MDMAPR GitHub",
                          style = "font-size:16px;"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                p("To learn more about the MDMAPR 2.0 application visit the",
                  tags$a(href="https://github.com/AlkaBenawra/MDMAPR",
                         "MDMAPR 2.0 GitHub page."), style = "font-size:16px;" )))),


      # Available Data page ---------------------------
      tabItem(tabName = "availableData",

              h1(strong("Available Assays")),

              p("The table displays assays which are currently stored in the MDMAPR 2.0 database. When  filling in a metadata template, if an assay you are using  exists in the database, you can copy this data and directly fill it in your metadata template.",
                style = "font-size:16px;"),

              box(reactableOutput("availableDataTable"), width = 12)),



      # FAQs page ---------------------------
      tabItem(tabName = "faqs",

              h1(strong("FAQs")),

              br(),

              fluidRow(box(
                title = p("Is the data I upload directly onto the application stored in the MDMAPR 2.0 database?",
                          style = "font-size:16px;"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                p("No, data uploaded onto the application is not stored in the MDMAPR 2.0 database. If you would like more information on how to store data in the MDMAPR 2.0 database please refer to the",
                  tags$a(href="https://github.com/AlkaBenawra/MDMAPR",
                         "MDMAPR 2.0 GitHub page."), style = "font-size:16px;"))),


              fluidRow(box(
                title = p("How are the System Calculated Threshold values determined?",
                          style = "font-size:16px;"),
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                p("The Second Derivative Method is used to calculate the System Calculated Threshold values.", style = "font-size:16px;"))),


              fluidRow(
                box(title = p("What formula is used to calculate the Cq values?",
                              style = "font-size:16px;"),
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    width = 12,
                    p("The function th.cyc() from the R package chipPCR is used to calculate the Cq values.", style = "font-size:16px;"))))





    )
  )
)



