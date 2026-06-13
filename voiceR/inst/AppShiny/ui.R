ui <- fluidPage( # Application title
  shinyjs::useShinyjs(),
  tags$head(
    shiny::tags$style(shiny::HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap');



      .soundFile {
        background-color: #1B1B1B;
        background-position: center;
        max-width: 900px;
        margin: 30px auto 0 auto;
        text-align: left;
        color: white;
        font-style: italic;
        font-size: 16px;
        border: 2px solid #222;
        border-width: 2px;
        box-shadow: 0 0 5px black;
        padding: 7px 0;
        transition: all 200ms;
        background: #444;
      }
      .shinyDirectories.btn{
        margin-bottom: 5px;
        box-shadow: inset 0px 0px 15px 3px #2e495e;
        background: linear-gradient(to bottom, #4F6276 5%, #607184 100%);
        background-color: #394e66;
        border-radius: 8px;
        border: 1px solid #1f2f47;
        display: inline-block;
        cursor: pointer;
        color: #ffffff;
        font-family: Arial;
        font-size: 15px;
        padding: 6px 13px;
        text-decoration: none;
        text-shadow: 0px 1px 0px #273a75;
      }
      #downloadReport{
        margin-bottom: 15px;
      }
      .audioIcon{
        display: inline-block;
        margin-left: 30px;
        margin-right: 25%;

      }
      .shiny-split-layout img{
        width: 97%;
        height: 97%;
      }

      .shiny-split-layout{
        text-align: center;
        margin-top: 30px;
        margin-bottom: 30px;
        box-shadow: 0 0 5px #d3d3d3;
        border: 2px solid #d3d3d3;
        border-width: 2px;
        padding: 7px 0;
        transition: all 200ms;
        background: #ededed;

      }
      #NumberFiles{
        margin-top: 15px;
      }
      #ResultTable{
        margin-top: 30px;
        margin-bottom: 30px;
      }
      .hero{

        height: 30vh;
        display: flex;
        justify-content: center;
        text-align: center;
        color: white;
        align-items: center;
        background-image: linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url(https://images.unsplash.com/photo-1589903308904-1010c2294adc?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1350&q=80);
        background-size: cover;
        background-position: center center;
        background-repeat: no-repeat;
        background-attachment: fixed;

      }
      .hero-text {
        /* Text styles */
        font-size: 5em;
        margin-top: 0;
        margin-bottom: 0.5em;

      }

      #doAnalysis{
        font-size: 1.4rem;
        font-weight: 500;
        fill: #fff;
        color: #fff;
        background-color: #00a6e7;
        border-style: solid;
        border-width: 2px;
        border-color: #00a6e7;
        border-radius: 2px;
        padding: 16px 30px;
      }

      .well{
        background: linear-gradient(to right, #243B55, #141E30);
        opacity: .98;
        transition: background .3s,border-radius .3s,opacity .3s;
        box-shadow: 0 0 5px black;
        border: 2px solid #222;
        margin-top: 2vh;
        color: white;
        margin-left: 15px;
      }
      .container-fluid{
        background: #ECE9E6;  /* fallback for old browsers */
        background: -webkit-linear-gradient(to right, #FFFFFF, #ECE9E6);  /* Chrome 10-25, Safari 5.1-6 */
        background: linear-gradient(to right, #FFFFFF, #ECE9E6); /* W3C, IE 10+/ Edge, Firefox 16+, Chrome 26+, Opera 12+, Safari 7+ */
        padding-right: 0px;
        padding-left: 0px;
        margin-right: 0px;
        margin-left: 0px;

      }
      .row{
        display: flex;
        flex-wrap: wrap;
        margin-right: 0px;
        margin-left: 0px;
        word-break: normal;
      }
      .span5{
        margin-right: 20px;
      }


      html{
        background: #ECE9E6;  /* fallback for old browsers */
        background: -webkit-linear-gradient(to right, #FFFFFF, #ECE9E6);  /* Chrome 10-25, Safari 5.1-6 */
        background: linear-gradient(to right, #FFFFFF, #ECE9E6); /* W3C, IE 10+/ Edge, Firefox 16+, Chrome 26+, Opera 12+, Safari 7+ */
        font-family: 'Roboto', sans-serif;

      }



    "))
  ),
  shiny::div(class = "hero", shiny::div(class = "hero-text", shiny::h1("VoiceR: voice Analytics for R"), shiny::HTML('<img src="https://www.ibt.unisg.ch/wp-content/uploads/2021/08/IBTLogoSquares_NZ.png" width = "90", height = "90"/>'))),
  shiny::sidebarPanel(
    shinyFiles::shinyDirButton("dir", "Input directory", "Upload"),
    shiny::verbatimTextOutput("dir", placeholder = TRUE),


    withTags(div(class='row',
                 div(class='span5', shiny::checkboxInput("recursive", "Recursive", value = FALSE, width = NULL)),
                 div(class='span5', shiny::checkboxInput("preprocess", "Preprocess", value = FALSE, width = NULL)),
                 div(class='span5', shiny::checkboxInput("includeDimensions", "Include Dimensions", value = FALSE, width = NULL)),
                 div(class='span5', shiny::checkboxInput("normalizeData", "Transform data to normal", value = FALSE, width = NULL))
    )),
    shiny::textInput("sep", "File Name Separator", value = "_"),
    shiny::textInput("fileNamePattern", "File Name Pattern", value = "ID_Condition"),
    shiny::textInput("patternRaw", 'Data Selection Filter (comma separated)', placeholder = "345abd, 456hf, Inside"),
    shiny::selectInput("fileType", "File Type:",
                choices=c("WAV", "MP3")),
    shiny::actionButton("doAnalysis", "Analyze")
  ),
  shiny::mainPanel(
    shiny::div(id = "titleMain", shiny::h1("Instructions")),
    shiny::verbatimTextOutput("NumberFiles", placeholder = FALSE),
    shiny::div(id = "MainList", p("The voiceR shiny app simplifies the process of analyzing and comparing multiple audio files, removing any need to code. "),
        shiny::p("It requires the file names to follow a specific pattern, in which the different components (IDs, and in case of different conditions and/or dimensions,
        conditions and dimensions) are separated by a non alphanumeric character, regardless of their order.
                 Examples of valid patterns would be 876h_Exterior (ID_Condition), Exterior-876h (Condition_ID), 876h_Exterior_q1 (ID_Condition_Dimension)."),
        shiny::strong("All file names to be analysed in a run should follow the same file name pattern."),
        shiny::p("To use it, you must first click the 'Input Directory' button and select the directory where the audio files to be analyzed are located.
                 In addition, if you also want to analyze audio files that are in folders within the selected directory, the recursive checkbox should be checked.
                 Likewise, to automatically pre-process the audio files before analyzing them, the 'pre-process' checkbox should be checked. Also, in case that your
                 data included a Dimensions component and you wanted to include it in the comparison graphs, check the 'Include Dimensions?' box"),
        shiny::p("In addition, the separator between the different components must be specified in the field 'Field Name Separator' and the pattern that follows the
                 name of the files must be specified in 'File Name Pattern'. In case the pattern includes more components than you want to analyze, they can be ignored
                 (as long as they are in the last positions) and will not be taken into account in the analyses. For example, if the pattern is ID_Condition_Dimension
                 but only the ID is to be taken into account, then it should be defined as the ID pattern, and if the Dimension is also to be taken into account, then
                 it should be defined as the ID_Condition pattern."),
        shiny::p("The 'Data Selection Filter (comma separated)' field allows you to select the different audio files that should be read by defining comma separated
                 patterns. For example if you want to select audio files containing ID 08788 and 065678, plus the interior condition you should define in this field: 08788,
                 065678, interior."),
        shiny::p("Finally, the correct File Type should be selected. And the analyze button should be clicked in order to start the analysis."),
        shiny::p("In case you want to try the app with some test data, you can download it ", shiny::a("here.", href = "https://osf.io/zt5h2/?view_only=348d1d172435449391e8d64547716477"))),
        tabsetPanel(
      id = "AnalysisData",
      tabPanel("Analysis Data", DT::dataTableOutput("ResultTable")),
      tabPanel("Errors", DT::dataTableOutput("ErrorTable"))
    ),
    shiny::splitLayout(id = "splitN1", cellWidths = c("49%", "49%"),
                       plotly::plotlyOutput("normalPlot1"),
                       plotly::plotlyOutput("normalPlot2")),
    shiny::splitLayout(id = "splitN2", cellWidths = c("49%", "49%"),
                       plotly::plotlyOutput("normalPlot3"),
                       plotly::plotlyOutput("normalPlot4")),
    shiny::splitLayout(id = "splitN3", cellWidths = c("49%", "49%"),
                       plotly::plotlyOutput("normalPlot5"),
                       plotly::plotlyOutput("normalPlot6")),
    shiny::splitLayout(id = "splitN4", cellWidths = c("49%", "49%"),
                       plotly::plotlyOutput("normalPlot7"),
                       plotly::plotlyOutput("normalPlot8")),
    shiny::splitLayout(id = "split1", cellWidths = c("49%", "49%"),
                plotly::plotlyOutput("plot1"),
                plotly::plotlyOutput("plot2")),
    shiny::splitLayout(id = "split2", cellWidths = c("49%", "49%"),
                plotly::plotlyOutput("plot3"),
                plotly::plotlyOutput("plot4")),
    shiny::splitLayout(id = "split3", cellWidths = c("49%", "49%"),
                plotly::plotlyOutput("plot5"),
                plotly::plotlyOutput("plot6")),
    shiny::splitLayout(id = "split4", cellWidths = c("49%", "49%"),
                plotly::plotlyOutput("plot7"),
                plotly::plotlyOutput("plot8")),
    shiny::downloadButton('downloadReport')

  ))
