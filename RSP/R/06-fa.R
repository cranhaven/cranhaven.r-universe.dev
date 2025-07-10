#' Run exploratory factor analysis for dichotomous and polytomous data
#' @import foreign
#' @import rJava
#' @importFrom stats cor
#' @importFrom hornpa hornpa
#' @importFrom utils read.csv2 write.csv2
#' @importFrom utils globalVariables
#' @importFrom psych cortest.bartlett KMO tetrachoric principal
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{FA()}
#' @export

FA <- function(){
  FA_ENV <- new.env()
  js <- "
// This solution from https://stackoverflow.com/a/59674107
// execute the code after the shiny session has started
$(document).on('shiny:sessioninitialized', function(event) {
  // browser detection from https://stackoverflow.com/a/5918791/8099834
  navigator.sayswho= (function(){
    var ua= navigator.userAgent, tem,
    M= ua.match(/(opera|chrome|safari|firefox|msie|trident(?=\\/))\\/?\\s*(\\d+)/i) || [];
    if(/trident/i.test(M[1])){
        tem=  /\\brv[ :]+(\\d+)/g.exec(ua) || [];
        return 'IE '+(tem[1] || '');
    }
    if(M[1]=== 'Chrome'){
        tem= ua.match(/\\b(OPR|Edge)\\/(\\d+)/);
        if(tem!= null) return tem.slice(1).join(' ').replace('OPR', 'Opera');
    }
    M= M[2]? [M[1], M[2]]: [navigator.appName, navigator.appVersion, '-?'];
    if((tem= ua.match(/version\\/(\\d+)/i))!= null) M.splice(1, 1, tem[1]);
    return M.join(' ');
  })();
  // pass browser info from JS to R
  Shiny.onInputChange('myBrowser', navigator.sayswho);
});
"
  ## USER INTERFACE ##

  ui <- fluidPage(
    useShinyjs(),
    theme = shinytheme("readable"),

    # setBackgroundColor(
    #   color = c("white", "gray"),
    #   gradient = "linear",
    #   direction = c("bottom", "right")
    # ),

    uiOutput("cols"),

    ####################################################################################
    tags$head(tags$style(
      type="text/css",
      "#image0 img {max-width: 100%; width: auto; height: 100%; align: center}


    table,img, .tippy-content{ border-collapse: collapse;

  border-radius: 1em;

  overflow: hidden;}

  th, td {

  padding: 1em;

  background: #ddd;

  border-bottom: 2px solid white;

  border-top: 2px solid white;

  }

   #tepe{
  border-bottom: 3px solid black;
  }
    "
    )),

  tags$head(tags$style(
    type="text/css",
    "#image1 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image2 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image3 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image4 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image5 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image6 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),
  ###################################################################################

  tags$style(HTML("#a{color:black; font-family:Lucida Arial ;font-size: 16px;
             font-style: oblique;text-align:center}")), #tabs#

  tags$style(HTML("#ab{color:black; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#

  tags$style(HTML("#b{color:black; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download #
  ####################################################################################

  ## POP UP ##

  bsTooltip(
    id = "text1",
    title = "Only a small part of the data is presented",
    placement = "bottom",
    trigger = "hover"
  ),
  bsTooltip(
    id = "rotation",
    title = "If the correlation between factors is low,choose varimax method",
    placement = "bottom",
    trigger = "hover"
  ),
  bsTooltip(
    id = "factornumber",
    title = "Determine the number of factors according to the result of the parallel analysis",
    placement = "bottom",
    trigger = "hover"
  ),
  bsTooltip(
    id = "scree_plot",
    title = "You can determine the number of eigen values over the black parallel analysis line as the number of factors",
    placement = "top",
    trigger = "hover"
  ),
  bsTooltip(
    id = "eigen_value",
    title = "Eigen values higher than the pa mean are indicated in red and underlined",
    placement = "top",
    trigger = "hover"
  ),
  bsTooltip(
    id = "fakor",
    title = "When the number of factors is more than 2 in order to see all the results slide the bar below to the right.",
    placement = "bottom",
    trigger = "hover"
  ),
  bsTooltip(
    id = "type",
    title = "Make sure you choose the data type correctly!",
    placement = "top",
    trigger = "hover"
  ),
  bsTooltip(
    id = "tableFactor",
    title = "Items with a lower factor loading than the determined cutting score are indicated in red and underlined",
    placement = "top",
    trigger = "hover"
  ),

  bsTooltip(
    id = "KMo",
    title = "You can examine the change in the KMO value when the items are removed or added.",
    placement = "bottom",
    trigger = "hover"
  ),

  ## TITLE PANEL - SIDE BAR PANEL ##

  div(id ="tepe",
      fluidRow(

        column(6,

               h1(id="title", "EXPLORATORY FACTOR ANALYSIS (EFA) "),
               tags$style(HTML("#title{color: black; font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;;font-size:30px;
            font-style: oblique;text-align:left}"))

        )  ,

        column(6,
               h1(id="title2", "RSP PACKAGE  - CRAN"),
               tags$style(HTML("#title2{color: black; font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;;font-size:15px;
            font-style: oblique;text-align:right}"))

        )

      )),


  br(),

  sidebarPanel(
    ## PANEL 1 - INTRODUCTION ##
    conditionalPanel(
      condition = "input.panel==0",
      shiny::img(src = "img/fa1.png", width = "97%"),
      tags$head(
        tags$script(HTML(js))
      ),
      br(),
      br(),
      br(),
      textOutput("browser"),
      tags$head(
        tags$style(
          "#browser{
                       color: darkblue;
                       font-size: 25px;
                       font-family: cursive;
                       font-style: oblique;
                       text-align:center;
                       letter-spacing:1px;
                       }"
        )
      ),
      ######################################################################################
      #imageOutput("image1",width = "75%", height = "100px", inline = TRUE),
      ######################################################################################

      br(),
      br(),

      shinyWidgets::spectrumInput(   # RENK PALET WİDGET
        inputId = "myColor",
        label = "CHANGE THE COLOR OF THE THEME:",
        choices = list(
          list('gray', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
          as.list(brewer_pal(palette = "Blues")(9)),
          as.list(brewer_pal(palette = "Greens")(9)),
          as.list(brewer_pal(palette = "Spectral")(11)),
          as.list(brewer_pal(palette = "Dark2")(8))
        ),
        options = list(`toggle-palette-more-text` = "Show more")
      ),
    ),

    ## PANEL 2 - DATA UPLOAD ##

    conditionalPanel(
      condition = "input.panel==1",
      shiny::img(src = "img/fa1.png", width = "97%"),

      ###################################################################
      #imageOutput("image2",width = "15%", height = "50px", inline = TRUE),
      ###################################################################

      br(),
      br(),

      shinyWidgets::radioGroupButtons(
        inputId = "type",
        label =  h3(id="ab","Select Data Type"),
        choices = c("Polytomous (Likert etc..)"=1,
                    "1-0"=2),

        justified = TRUE,
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon")),

        # status = "primary"
      ),

      # prettyRadioButtons(
      #   inputId = "type",
      #   label = h3(id="ab","Select Data Type"),
      #   choices = c("Polytomous (Likert etc..)"=1,
      #               "1-0"=2),
      #
      #   shape = "curve",animation = "rotate" , inline = FALSE,
      #   bigger = TRUE, status = "primary", outline = TRUE,
      #   fill = FALSE, width = "500px"
      # ),


      shinyWidgets::pickerInput(
        inputId = "type2",
        label = h3(id="ab","Select File Format"),
        choices = list(
          "CSV - Semicolon  Separated  Excel" = 1,
          "CSV - Comma  Separated  Excel" = 2,
          "SAV - SPSS" = 3,
          "XLSX - Excel"=4
        ),
        selected = 3,
        options =  shinyWidgets::pickerOptions(showTick=TRUE,
        ),#style = "btn-primary"),

      ),

      uiOutput("uiHeader"),

      div(  style="color:red;",

            HTML( "<marquee direction='left' scrollamount = '5'>
                      THE DATASET SHOULD CONTAIN ONLY THE VARIABLES TO BE INCLUDED IN THE ANALYSIS!!!

               </marquee>"  )),

      fileInput(
        "data1",
        h3(id="ab","Uplad Data File",icon("paper-plane"))
      ),

      shinyWidgets::dropMenu(

        #actionButton("acb1", "DOWNLOAD"),


        padding = "20px",

        theme="light-border",

        placement = "right-end",


        shinyWidgets::actionBttn(
          inputId = "acb2",
          label = "CLICK TO SEE KMO  AND BARTLET SPHERICITY TEST RESULTS",
          style = "jelly",
          color = "primary"

        ),

        gt::gt_output("dat3")

      ),

      # gt::gt_output("dat3.1")
    ),

    ## PANEL 3 - NUMBER OF FACTORS ##
    conditionalPanel(
      condition = "input.panel==2",
      shiny::img(src = "img/fa1.png", width = "97%"),

      ###################################################################
      #imageOutput("image3",width = "15%", height = "50px", inline = TRUE),
      ###################################################################
      br(),
      br(),

      shinyWidgets::chooseSliderSkin("Big", color = "#112446"),
      uiOutput("factornumber"),
      br(),
      br(),

      shinyWidgets::dropMenu(

        padding = "20px",

        theme="light-border",

        placement = "right-end",

        shinyWidgets::actionBttn(
          inputId = "korfak",
          label = "CLICK TO SEE CORRELATIONS AMONG THE FACTORS",
          style = "jelly",
          color = "primary"

        ),

        gt::gt_output("fakor"),

      ),

      br(),
      br(),

      shinyWidgets::pickerInput(
        inputId = "rotation",
        label = h3(id="ab","Select Rotation Method"),
        choices = list(
          "Varimax" = "varimax",
          "Direct Oblimin" = "oblimin",
          "No Rotation" = "none"
        ),
        selected = "none",
        options =  shinyWidgets::pickerOptions(showTick=TRUE,
        ),#style = "btn-primary"),
      ),
      br(),
      br(),
      shinyWidgets::pickerInput(
        inputId = "fm",
        label = h3(id="ab","Factor Extraction Method"),
        choices = list(
          "Principal Axis" = "pa",
          "Maximum Likelihood" = "ml",
          "Weighted Least Squares" = "wls",
          "Unweighted Least Squares" = "uls"
        ),
        selected = "pa",
        options =  shinyWidgets::pickerOptions(showTick=TRUE,
        ),#style = "btn-primary"),
      ),
    ),

    ## PANEL 4 - FACTOR LOADINGS ##
    conditionalPanel(
      condition = "input.panel==3",
      shiny::img(src = "img/fa1.png", width = "97%"),

      ###################################################################
      #imageOutput("image4",width = "15%", height = "50px", inline = TRUE),
      ###################################################################

      #  br(),

      gt::gt_output("KMo"),
      br(),

      fluidRow(

        column( 3 ),
        column( 9,

                uiOutput("remove_item")) ),
      # br(),

      uiOutput("select_item"),

      # ##################### NEW KMO 1 #################

      br(),


      sliderInput(
        "cut_off",
        h3(id="ab","Select cut-off value for Factor Loadings"),
        min = 0.25,
        max = 0.60,
        step = 0.05,
        value = 0.30
      ),


      br(),

      fluidRow(

        column(6,

               shinyWidgets::dropMenu(

                 padding = "20px",

                 theme="light-border",

                 placement = "right-end",


                 shinyWidgets::actionBttn(
                   inputId = "coms",
                   label = "CLICK TO SEE COMMUNALITIES",
                   style = "jelly",
                   color = "primary"

                 ),


                 DT::DTOutput("commons"),

               ),


        ), # close column



        column(6,

               shinyWidgets::dropMenu(

                 padding = "20px",

                 theme="light-border",

                 placement = "right-end",

                 shinyWidgets::actionBttn(
                   inputId = "acb1",
                   label = "CLICK TO  SEE DOWNLOADS",
                   style = "jelly",
                   color = "primary"

                 ),


                 shinyWidgets::downloadBttn(
                   "factorDownload",
                   label = h1(id="b", "FACTOR LOADINGS"),
                   style = "unite",
                   color = "primary",
                   size = "sm",
                   block = FALSE,
                   no_outline = TRUE,
                   icon = shiny::icon("download")
                 ),


                 br(),
                 br(),


                 shinyWidgets::downloadBttn(
                   "varianceDownload",
                   label = h1(id="b", " EXPLAINED VARIANCE"),
                   style = "unite",
                   color = "primary",
                   size = "sm",
                   block = FALSE,
                   no_outline = TRUE,
                   icon = shiny::icon("download")
                 ),


               ) # close drop menu


        ) # close column

      ) # close fluidrow



    ), # close conditional panel

  ), # sidebar panel

  ##  MAIN PANEL ##

  mainPanel(
    tabsetPanel(
      id = "panel",

      ## MAIN PANEL  1 ##
      tabPanel(

        # h4("INTRODUCTION"),


        h4(id="a", "INTRODUCTION"),


        value = 0,
        br(),
        br(),
        br(),

        ###################################################################
        #imageOutput("image0",width = "15%", height = "50px", inline = TRUE),
        ###################################################################

        fluidRow(
          column(
            12,
            align = "center",
            shiny::img(src = "img/fa1.png", width = "97%")
          )
        )
      ),

      ##  MAIN PANEL 2 ##
      tabPanel(
        # h4( "DATA UPLOAD"),

        h4(id="a", "DATA UPLOAD"),
        value = 1,
        textOutput("text1"),
        tags$head(
          tags$style(
            "#text1{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
          )
        ),
        br(),
        withLoader(
          DT::dataTableOutput("dat1"),
          type = "html",
          loader = "loader1"
        ),
        br(),
        textOutput("text1_1"),
        tags$head(
          tags$style(
            "#text1_1{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
          )
        ),
        br(),
        gt::gt_output("dat2"),
        br(),
        gt::gt_output("dat4"),
      ),

      ## MAIN PANEL 3 ##
      tabPanel(
        # h4("DETERMINING THE NUMBER OF FACTORS"),

        h4(id="a", "DETERMINING THE NUMBER OF FACTORS"),

        value = 2,
        textOutput("text2"),
        tags$head(
          tags$style(
            "#text2{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
          )
        ),
        br(),
        br(),
        withLoader(plotOutput("scree_plot"), type = "html", loader = "loader1"),

        br(),
        gt::gt_output("eigen_value"),
        br()
      ),

      ## MAIN PANEL 4 ##
      tabPanel(

        # h4( "FACTOR LOADINGS AND EXPLAINED VARIANCE"),

        h4 (id="a", "FACTOR LOADINGS AND EXPLAINED VARIANCE"),
        value = 3,
        textOutput("text2_1"),
        tags$head(
          tags$style(
            "#text2_1{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
          )
        ),
        gt::gt_output("tableFactor"),
        gt::gt_output("buton"),
        br(),
        gt::gt_output("tableEigen"),
        gt::gt_output("buton2"),
        br(),








      ),

    ) # close tabsetpanel
  ) #  close mainpanel
  ) #  close fluidpage


  ## SERVER ##

  server <- function(input, output, session) {


    shinyjs::addCssClass(class = "bttn bttn-unite bttn-default bttn-no-outline",
                selector = ".btn-file")

    observeEvent(input$myColor,{

      output$cols<- renderUI({   # WIDGET RENDER UI COLOR CHANGER

        bbb<-input$myColor

        shinyWidgets::setBackgroundColor(
          color = c("white", bbb),
          gradient = "linear",
          direction = c("bottom", "right")
        )})
    })

    output$browser <- renderText({
      req(input$myBrowser)
      if(input$myBrowser == "Chrome 102"){
        paste0("Please click 'Open in Browser' for a better experience")
      } else {
        NULL
      }
      # contains the value returned by the JS function
    })
    ###########################################################################

    output$image0<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "fa1.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image1<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "fa1.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image2<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "fa1.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image3<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "fa1.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image4<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "fa1.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image5<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "fa1.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image6<- renderImage({
      resim2 <- tempfile(fileext = '.gif')
      list(src = "download.gif", contentType = "image/gif")
    },
    deleteFile = FALSE)

    ###########################################################################

    ##  DATA UPLOAD ##
    output$uiHeader <- renderUI({
      if (input$type2 == 3) {
        NULL
      } else {

        shinyWidgets::materialSwitch(
          inputId = "header",
          label =   h4("The first line is the variable name"),
          value = TRUE,
          status = "primary"
        )
        # switchInput(inputId = "header",
        #             label =  " Is the first line include the variable name?",
        #             onLabel = "YES",
        #             offLabel = "NO",
        #             value = TRUE,
        #             inline = TRUE)
      }
    })

    data <- reactive({
      veri <- input$data1
      if (is.null(veri)) {
        return(paste0("PLEASE UPLOAD DATA"))

      } else if (input$type2 == 1) {
        if (tools::file_ext(veri$datapath) != "csv") {
          data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          utils::read.csv2(veri$datapath,
                           header = isTRUE(input$header),
                           sep = ";"
          )
        }
      } else if (input$type2 == 2) {
        if (tools::file_ext(veri$datapath) != "csv") {
          data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          utils::read.csv2(veri$datapath,
                           header = isTRUE(input$header),
                           sep = ","
          )
        }
      } else if (input$type2 == 3) {
        if (tools::file_ext(veri$datapath) != "sav") {
          data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          read.spss(veri$datapath,
                    to.data.frame = TRUE,
                    use.value.labels = FALSE
          ) }
      } else if(input$type2==4) {
        if (tools::file_ext(veri$datapath) != "xlsx") {
          data.frame(warning = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          xlsx::read.xlsx(veri$datapath, 1, header = isTRUE(input$header)
          )

        } }

    })

    ## WIDGETS FOR UIOUTPUTS ##

    output$factornumber <- renderUI({
      req(input$data1)
      sliderInput(
        "fak2",
        h3(id="ab","Define the Number of Factors"),
        min = 1,
        max = ncol(data()),
        step = 1,
        value = 1
      )

    })

    output$select_item <- renderUI({
      if (!is.null(input$data1)) {
        madism <- (1:ncol(data()))
        madisimGL <- madism
        selectInput("grafmad",
                    h3(id="ab","Select Item Number"),
                    choices = madism,
                    multiple = TRUE
        )
      }
    })

    output$remove_item <- renderUI({


      shinyWidgets::actionBttn(
        inputId = "remove",
        label = "Remove Selected Item/s",
        style = "jelly",
        size = "lg",
        color = "primary", no_outline = TRUE
      )


    })

    ##  TEXTS FOR MAIN PANELS ##
    output$text1 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DATA UPLOAD AND BASIC STATISTICS")
      }
    })

    output$text1_1 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DATA WAS UPLOADED SUCCESSFULLY")
      }
    })

    output$text2 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DEFINING NUMBER OF FACTORS -
                PARALLEL ANALYSIS - EIGEN VALUES")
      }
    })

    output$text2_1 <- renderText({
      if (!is.null(input$data1)) {
        paste0("FACTOR LOADINGS AND EXPLAINED VARIANCE")
      }
    })

    output$dat1 <- DT::renderDataTable({
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        colnames(data) <- paste0("item", 1:ncol(data))
        if (dim(data)[2] == 1) {
          data.frame(WARNING = "PLEASE SELECT THE CORRECT FILE FORMAT")
        } else {
          if (ncol(data) <= 15) {
            data[1:10, 1:ncol(data)]
          } else {
            data[1:10, 1:15]
          }
        }
      }
    })

    ## DESCRIPTIVES ##

    output$dat2 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- data()
        NUMBER_OF_ITEMS <- ncol(data)
        NUMBER_OF_RESPONDENTS <- nrow(data)
        NUMBER_OF_BLANK_ITEMS <- length(which(is.na(data)))

        res <- data.frame(
          NUMBER_OF_ITEMS,
          NUMBER_OF_RESPONDENTS,
          NUMBER_OF_BLANK_ITEMS
        )
        res <- gt::gt(res)
        br()
        br()
        br()
        res <- res %>%
          tab_header(title = md("*Basic Statistics About the Data Set*"))

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.15
            ),
            locations = gt::cells_body()
          )

        res <- res %>%
          gt::cols_width(
            c(NUMBER_OF_ITEMS) ~ gt::px(300),
            everything() ~ gt::px(300)
          )

        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%
          tab_options(heading.title.font.size = gt::px(25))
      }
    })

    ## BARTLET - KMO ##

    output$dat3 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        BR <- psych::cortest.bartlett(data)
        Bartlett_Chi_Square <- BR$chisq
        p_value <- round(BR$p.value, 4)
        df <- BR$df
        if (input$type == 1) {
          cor_matrix <- stats::cor(data)
          kmo <- psych::KMO(cor_matrix)$MSA
        } else {
          tet_matrix <- tetrachoric(data)$rho
          kmo <- KMO(tet_matrix)$MSA
        }

        res <- data.frame(KMO = kmo, Bartlett_Chi_Square, p_value, df)
        res <- gt::gt(res)
        br()
        br()
        br()
        res <- res %>%
          tab_header(
            title =
              md("*KMO Test - Bartlett's Homogeneity of Variance Test*")
          )

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.15
            ),
            locations = gt::cells_body()
          )

        res <- res %>%
          gt::cols_width(
            c(Bartlett_Chi_Square) ~ gt::px(200),
            c(p_value) ~ gt::px(150),
            c(df) ~ gt::px(150),
            everything() ~ gt::px(200)
          )

        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%
          tab_options(heading.title.font.size = gt::px(25))
      }
    })

    ## DETERMINANT ##

    output$dat4 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        if (input$type == 1) {
          cor_matrix1 <- stats::cor(data)
        } else {
          cor_matrix1 <- tetrachoric(data)$rho
        }

        Determinant <- det(cor_matrix1)
        total <- rowSums(data)
        skewness <- function(x) {
          numerator <- sum((x - mean(x))^3)
          denominator <- length(x) * (sd(x)^3)
          result <- numerator / denominator
          return(result)
        }

        kurtosis <- function(x) {
          numerator <- sum((x - mean(x))^4)
          denominator <- length(x) * (sd(x)^4)
          result <- (numerator / denominator) - 3
          return(result)
        }

        Skewness <- skewness(total)
        Kurtosis <- kurtosis(total)
        res <- data.frame(
          DETERMINANT = Determinant,
          SKEWNESS = Skewness,
          KURTOSIS = Kurtosis
        )

        res <- gt::gt(res)
        br()
        br()
        br()

        res <- res %>%
          tab_header(title = md(""))

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.15
            ),
            locations = gt::cells_body()
          )

        res <- res %>%
          gt::cols_width(everything() ~ gt::px(300))

        res <- res %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%
          tab_options(heading.title.font.size = gt::px(25))
      }
    })

    ## NUMBER OF FACTORS ##

    # scree Plot #

    output$scree_plot <- renderPlot({

      if (!is.null(input$data1)) {
        data <- na.omit(data())

        ## For 1-0 Data ##

        if (input$type == 1) {
          cor_matrix <- stats::cor(data)

          model1 <-
            psych::fa(r = cor_matrix,
                      nfactors = ncol(data),
                      n.obs = nrow(data),
                      rotate = "none"
            )
        }

        if (input$type == 2) {
          tet_matrix <- tetrachoric(data)$rho

          model1 <-
            psych::fa(r = tet_matrix,
                      nfactors = ncol(data),
                      n.obs = nrow(data),
                      rotate = "none"
            )
        }

        ## PARALLEL ANALYSIS ##

        set.seed <- 123
        horn <- hornpa(
          k = ncol(data),
          size = nrow(data),
          reps = 200
        )
        PA_MEAN <- horn$Mean

        plot(
          model1$values,
          type = "b",
          col = 2,
          lty = 1,
          lwd = 1.5,
          main = " SCREE PLOT AND PARALLEL ANALYSIS",
          xlab = "Number of Factors",
          ylab = " Eigenvalue"
        )

        lines(
          PA_MEAN,
          type = "b",
          col = 1,
          lty = 2,
          lwd = 1.5
        )

        legend(
          "topright",
          legend = c("PA Mean", "Eigenvalue"),
          col = 1:2,
          lty = 1:2,
          lwd = 1.5
        )
      }
    })

    ## Eigen Value ##

    output$eigen_value <- render_gt({
      if (!is.null(input$data1)) {
        set.seed <- 123
        data <- na.omit(data())

        if (input$type == 1) {
          cor_matrix <- stats::cor(data)
          model1 <-
            psych::fa(r = cor_matrix,
                      nfactors = ncol(data),
                      n.obs = nrow(data),
                      rotate = "none"
            )
        } else {
          tet_matrix <- tetrachoric(data)$rho

          model1 <-
            psych::fa(r = tet_matrix,
                      nfactors = ncol(data),
                      n.obs = nrow(data),
                      rotate = "none"
            )
        }

        eigenvalue <- unname(model1$Vaccounted[1, ]) ## CHECK
        eigenvalue <- round(eigenvalue, 4)

        horn <- hornpa::hornpa(
          k = ncol(data),
          size = nrow(data),
          reps = 200
        )
        PA_MEAN <- horn$Mean
        FACTOR <- 1:ncol(data)
        res <- data.frame(FACTOR, PA_MEAN, EIGENVALUE = eigenvalue)

        res <- gt::gt(res)
        res <- res %>%
          tab_header(title = md("**EIGENVALUES AND PARALLEL ANALYSIS**"))

        res <- res %>%
          tab_options(
            heading.title.font.size = gt::px(25),
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.20
            ),
            locations = gt::cells_body()
          )

        res <- res %>%
          gt::cols_width(everything() ~ gt::px(180))

        res <- res %>%
          gt::tab_style(
            style = gt::cell_text(
              weight = "bolder",
              color = "red",
              decorate = "underline"
              #stretch = "extra-expanded"
            ),
            locations = gt::cells_body(
              columns = c(EIGENVALUE),
              rows = EIGENVALUE > PA_MEAN
            )
          )
        return(res)
      }
    })

    ## Correlation Among Factors ##

    output$fakor <- render_gt({
      if (!is.null(input$data1)) {
        data <- na.omit(data())
        if (input$type == 1) {
          cor_matrix <- stats::cor(data)
          model1 <- psych::fa(r = cor_matrix,
                              nfactors = input$fak2,
                              n.obs = nrow(data),
                              rotate = input$rotation,
                              fm = input$fm
          )
        } else {
          tet_matrix <- tetrachoric(data)$rho
          model1 <- psych::fa(r = tet_matrix,
                              nfactors = input$fak2,
                              n.obs = nrow(data),
                              rotate = input$rotation,
                              fm = input$fm
          )
        }

        fac <-  round( model1$r.scores,3)
        Row <- paste("Factor", 1:input$fak2)

        colnames(fac) <- paste("Factor", 1:input$fak2)

        fac1 <- data.frame(FACTORS = Row, fac)

        fac1 <- gt::gt(fac1)

        fac1 <- fac1 %>% tab_header(title = md("**CORRELATION AMONG FACTORS**"))

        fac1 <- fac1 %>%
          gt::cols_width(everything() ~ gt::px(180))

        fac1 <- fac1 %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1), alpha =
                0.20
            ),
            locations = gt::cells_body()
          )

        fac1 <- fac1 %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        return(fac1)
      }
    })


    ## FACTOR LOADINGS ##

    output$tableFactor <- render_gt(align = "center", {
      if (!is.null(input$data1)) {

        data <- na.omit(data())


        if (input$type == 1) {
          cor_matrix <- stats::cor(data)


          result <- psych::fa(r = cor_matrix,
                              nfactors = input$fak2,
                              n.obs = nrow(data),
                              rotate = input$rotation,
                              fm = input$fm
          )
        } else {
          tet_matrix <- tetrachoric(data)$rho


          result <- psych::fa(r = tet_matrix,
                              nfactors = input$fak2,
                              n.obs = nrow(data),
                              rotate = input$rotation,
                              fm = input$fm
          )
        }

        common<- result$communality

        FA_ENV$common<-common

        eigenvalue <- unname(result$Vaccounted[1, ])

        rnames2 <- character()
        for (i in 1:length(eigenvalue)) {
          rnames2[i] <- paste("item", i)
        }

        nn <- ncol(data)
        nfac <- input$fak2
        total <- nn * nfac
        empty_mat <- matrix(NA, nn, nfac)

        if (nfac == 1) {
          if (input$type == 2) {
            empty_mat <- as.matrix(result$loadings[1:total])
          } else {
            empty_mat <- as.matrix(result$loadings[1:total])
          }
        } else {
          if (input$type == 1) {
            for (i in 1:total) {
              if (input$type == 2) {
                empty_mat[i] <- as.matrix(result$loadings[i:total])
              } else {
                empty_mat[i] <- result$loadings[i:total]
              }
            }
          } else {
            for (i in 1:total) {
              empty_mat[i] <- result$loadings[i:total]
            }
          }
        }

        namefac <- NULL

        for (k in 1:nfac) {
          namefac[k] <- paste0("factor", k)
        }

        item <- 1:ncol(data)

        item <- round(item)

        empty_mat <- round(empty_mat, 2)

        colnames(empty_mat) <- namefac[1:nfac]

        fload <- cbind(item, empty_mat)

        fload <- as.data.frame(fload)

        FA_ENV$factorLoading <- fload

        fload <- gt::gt(fload)



        fload <- fload %>% tab_header(title = md("**FACTOR LOADINGS**"))

        ## ARRANGE COLUMNS ACCORDIG TO NUMBER OF FACTORS ##


        if (input$fak2 > 3) {
          fload <- fload %>%
            gt::cols_width(everything() ~ gt::px(120))
        }

        if (input$fak2 <= 3) {
          fload <- fload %>%
            gt::cols_width(everything() ~ gt::px(180))
        }

        ## HIGHLIGHT LOW FACTOR LOADINGS ##

        if (input$fak2 == 1) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < input$cut_off
              )
            )
        }

        if (input$fak2 == 2) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < input$cut_off
              )
            )
        }

        if (input$fak2 == 3) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )
        }



        if (input$fak2 == 4) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 5) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 6) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor6),
                rows = abs(factor6) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 7) {
          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < input$cut_off
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )


          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor6),
                rows = abs(factor6) < abs(input$cut_off)
              )
            )

          fload <- fload %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor7),
                rows = abs(factor7) < abs(input$cut_off)
              )
            )
        }

        fload <- fload %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1), alpha =
                0.20
            ),
            locations = gt::cells_body()
          )

        fload <- fload %>%
          tab_options(
            column_labels.font.size = gt::px(20),
            column_labels.font.weight = "bolder"
          )

        return(fload)
      }
    })





    #### COMMUNALITIES ###


    output$commons<-DT::renderDT({

      data <- na.omit(data())

      if (  length(input$grafmad)==0) {

        data1<-data }  else


        {       x<-as.numeric(input$grafmad)

        dat<-as.data.frame(data)

        data1<-dat[,-x]  }

      if (input$type == 1) {
        cor_matrix <- stats::cor(data1)

        result <- psych::fa(r = cor_matrix,
                            nfactors = input$fak2,
                            n.obs = nrow(data),
                            rotate = input$rotation,
                            fm = input$fm
        )
      } else {
        tet_matrix <- tetrachoric(data1)$rho


        result <- psych::fa(r = tet_matrix,
                            nfactors = input$fak2,
                            n.obs = nrow(data),
                            rotate = input$rotation,
                            fm = input$fm
        )
      }

      common<- result$communality


      res_comon<- data.frame( Items=paste0("item", 1:ncol(data1)),  Extraction= round(common,3) )

      backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                  "aquamarine", "lightblue", "gray"), 1)
      datatable(res_comon) %>% formatStyle(colnames(res_comon),
                                           backgroundColor = backgroundColor) })

    ## EXPAINED VARIANCE ##

    add <- reactive({
      data <- na.omit(data())
      if (!is.null(input$data1)) {
        if (input$type == 1) {
          cor_matrix <- stats::cor(data)

          result <- psych::fa(r = cor_matrix,
                              nfactors = input$fak2,
                              n.obs = nrow(data),
                              rotate = input$rotation,
                              fm = input$fm
          )
        } else {
          tet_matrix <- tetrachoric(data)$rho

          result <- psych::fa(r = tet_matrix,
                              nfactors = input$fak2,
                              n.obs = nrow(data),
                              rotate = input$rotation,
                              fm = input$fm
          )
        }

        result <- result$Vaccounted

        result <- result[-c(4, 5), ]

        result <- unname(result)

        nfac <- input$fak2

        name <- matrix(NA, 3, nfac)

        Col <- paste("Factor", 1:nfac)

        Row <- c(
          "Eigenvalue",
          "Explained Variance",
          "Cummilative Explained Variance"
        )

        colnames(name) <- Col

        if (nfac == 1) {
          name <- result
          Row <- c("Eigenvalue", "Explained Variance")
        } else {
          for (i in 1:nfac) {
            name[, i] <- result[, i]
          }
        }
      }

      req(input$data1)

      add <- data.frame(Statistic = Row, name)
    })


    output$tableEigen <- render_gt(align = "center", {
      FA_ENV$explainedVar <- add()

      add <- gt::gt(add())

      add <- add %>% tab_header(
        title =
          md("**EIGENVALUE AND EXPLAINED VARIANCE**")
      )

      if (input$fak2 > 3) {
        add <- add %>%
          gt::cols_width(everything() ~ gt::px(120))
      }

      if (input$fak2 <= 3) {
        add <- add %>%
          gt::cols_width(everything() ~ gt::px(180))
      }

      add <- add %>%
        gt::tab_style(
          style = cell_fill(
            color = sample(colors()[3:100], 1), alpha =
              0.20
          ),
          locations = gt::cells_body()
        )

      add <- add %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold",
          row_group.font.weight = "bolder"
        )

      return(add)
    })

    ## reactive 1

    remainData <- reactive({
      omitted <- input$grafmad
      All <- 1:ncol(data()) # madisimGL
      dataGL <- na.omit(data())
      colnames(dataGL) <- All
      kalan <- setdiff(All, omitted)
      remainData <- dataGL[, kalan]
    })

    ## reactive 2

    model_removed <- reactive({
      if (input$type == 1) {
        remainCorMatrix <- stats::cor(remainData())

        modelRemain <-
          psych::fa(r = remainCorMatrix,
                    nfactors = input$fak2,
                    n.obs = nrow(remainData()),
                    rotate = input$rotation,
                    fm = input$fm
          )
      } else {
        remainTetMatrix <- tetrachoric(remainData())$rho

        modelRemain <-
          psych::fa(r = remainTetMatrix,
                    nfactors = input$fak2,
                    n.obs = nrow(remainData()),
                    rotate = input$rotation,
                    fm = input$fm
          )
      }
    })


    ############################### testttttttttt ###################

    output$KMo<-render_gt({

      if (!is.null(input$data1)) {

        if (input$type == 1) {

          korr<-stats::cor(remainData())

          res<-KMO(korr)[[1]]


        } else

        {

          korr<- tetrachoric(remainData())$rho

          res<-KMO(korr)[[1]]

        }

        res <- data.frame(KMO = res)

        res <- gt::gt(res)

        res <- res %>%
          tab_header(
            title =
              md("*Current KMO Test Result*")
          )

        res <- res %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1),
              alpha = 0.15
            ),
            locations = gt::cells_body()
          )

        # res <- res %>%
        #   tab_options(
        #     column_labels.font.size = gt::px(17),
        #     column_labels.font.weight = "bold"
        #   )

        res <- res %>%
          tab_options(heading.title.font.size = gt::px(25))

        return(res)
      }
    })


    ############################### testttttttttt ###################


    ## OBSERVE EVENT - RE-COMPUTATION AFTER OMITTED items ##

    nfacRemain <- reactive({
      nfacRemain <- input$fak2
    })

    observeEvent(input$remove, {
      shinyjs::hide("tableFactor")
      shinyjs::hide("tableEigen")

      output$buton <- render_gt(align = "center", {
        remainEigenvalue <- unname(model_removed()$Vaccounted[1, ])

        rnames3 <- character()
        for (i in 1:length(remainEigenvalue)) {
          rnames3[i] <- paste0("Item", i)
        }
        remainN <- ncol(remainData())

        remainTotal <- remainN * nfacRemain()

        remainEmpty <- matrix(NA, remainN, nfacRemain())

        if (input$type == 2) {
          for (i in 1:remainTotal) {
            remainEmpty[i] <- model_removed()$loadings[i]
          }
        } else {
          for (i in 1:remainTotal) {
            remainEmpty[i] <- model_removed()$loadings[i]
          }
        }

        remainFactorName <- NULL
        for (k in 1:nfacRemain()) {
          remainFactorName[k] <- paste0("factor", k)
        }

        items <- 1:ncol(remainData())
        items <- round((items), 2)

        remainEmpty <- round(remainEmpty, 2)

        #

        omitted <- input$grafmad
        All <- 1:ncol(data())
        dataGL <- na.omit(data())
        colnames(dataGL) <- All
        remain_Items <- setdiff(All, omitted)

        #

        colnames(remainEmpty) <- remainFactorName[1:nfacRemain()]

        floadRemain <- cbind(remain_Items, items, remainEmpty)

        floadRemain <- as.data.frame(floadRemain)

        FA_ENV$factorLoadRemain <- floadRemain
        floadRemain <- gt::gt(floadRemain)



        floadRemain <- floadRemain %>% tab_header(
          title =
            md("**FACTOR LOADINGS**")
        )

        ## Column Arrangement According to Factor Numbers ##

        if (input$fak2 > 3) {
          floadRemain <- floadRemain %>%
            gt::cols_width(everything() ~ gt::px(120))
        }

        if (input$fak2 <= 3) {
          floadRemain <- floadRemain %>%
            gt::cols_width(everything() ~ gt::px(180))
        }


        ## UNDERLINELOW FACTOR LOADINGS ##

        if (input$fak2 == 1) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < input$cut_off
              )
            )
        }


        if (input$fak2 == 2) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < input$cut_off
              )
            )
        }


        if (input$fak2 == 3) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )



          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )
        }



        if (input$fak2 == 4) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )
        }


        if (input$fak2 == 5) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 6) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )



          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor6),
                rows = abs(factor6) < abs(input$cut_off)
              )
            )
        }

        if (input$fak2 == 7) {
          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor1),
                rows = abs(factor1) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor2),
                rows = abs(factor2) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor3),
                rows = abs(factor3) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor4),
                rows = abs(factor4) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor5),
                rows = abs(factor5) < abs(input$cut_off)
              )
            )


          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor6),
                rows = abs(factor6) < abs(input$cut_off)
              )
            )

          floadRemain <- floadRemain %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold",
                color = "red",
                decorate = "underline",
                size = 40
              ),
              locations = gt::cells_body(
                columns = c(factor7),
                rows = abs(factor7) < abs(input$cut_off)
              )
            )
        }

        #

        floadRemain <- floadRemain %>%
          gt::tab_style(
            style = cell_fill(
              color = sample(colors()[3:100], 1), alpha =
                0.20
            ),
            locations = gt::cells_body()
          )

        floadRemain <- floadRemain %>%
          tab_options(
            column_labels.font.size = gt::px(17),
            column_labels.font.weight = "bold"
          )

        return(floadRemain)
      })

      ## REMOVED EXPLAINED VARIANCE ##


      ad.var.2 <- reactive({

        # align sola sabitlendi
        if (input$type == 2) {
          modelRemain.var <- model_removed()$loadings
        }
        modelRemain.var <- model_removed()$Vaccounted
        modelRemain.var <- modelRemain.var[-c(4, 5), ]
        modelRemain.var <- unname(modelRemain.var)
        ad.var <- matrix(NA, 3, nfacRemain())
        sutun <- paste0("Factor", 1:nfacRemain())
        satir <-
          c(
            "Eigenvalue",
            "Explained Variance",
            "Cummilative Explained Variance"
          )
        if (nfacRemain() == 1) {
          satir <- c("Eigenvalue", "Explained Variance")
        } else {
          satir <-
            c(
              "Eigenvalue",
              "Explained Variance",
              "Cummilative Explained Variance"
            )
        }
        colnames(ad.var) <- sutun
        if (nfacRemain() == 1) {
          ad.var <- modelRemain.var
        } else {
          for (i in 1:nfacRemain()) {
            ad.var[, i] <- modelRemain.var[, i]
          }
        }

        ad.var.2 <- data.frame(Statistics = satir, ad.var)
      })

      output$buton2 <-
        render_gt(align = "center", {



          # gt table modifying 2
          FA_ENV$explainedVarRemain <- ad.var.2()
          ad.var.2 <- gt::gt(ad.var.2())

          ad.var.2 <- ad.var.2 %>% tab_header(
            title =
              md("**EIGENVALUE AND EXPLAINED VARIANCE**")
          )

          if (input$fak2 > 3) {
            ad.var.2 <- ad.var.2 %>%
              gt::cols_width(everything() ~ gt::px(120))
          }

          if (input$fak2 <= 3) {
            ad.var.2 <- ad.var.2 %>%
              gt::cols_width(everything() ~ gt::px(180))
          }

          ad.var.2 <- ad.var.2 %>%
            gt::tab_style(
              style = cell_fill(
                color = sample(colors()[3:100], 1),
                alpha = 0.20
              ),
              locations = gt::cells_body()
            )

          ad.var.2 <- ad.var.2 %>%
            tab_options(
              column_labels.font.size = gt::px(17),
              column_labels.font.weight = "bold"
            )

          return(ad.var.2 <- ad.var.2)
        })
    }) ##  close observe event

    ## DOWNLOAD OUTPUTS ##

    output$factorDownload <- downloadHandler(


      filename = function() {
        "factor-loadings.csv"
      },
      content = function(file) {
        if (is.null(input$grafmad)) {
          utils::write.csv2(FA_ENV$factorLoading, file)
        } else {
          utils::write.csv2(FA_ENV$factorLoadRemain, file)
        }
      }
    )

    output$varianceDownload <- downloadHandler(

      filename = function() {
        "variance.csv"
      },
      content = function(file) {
        if (is.null(input$grafmad)) {
          utils::write.csv2(FA_ENV$explainedVar, file)
        } else {
          utils::write.csv2(FA_ENV$explainedVarRemain, file)
        }
      }
    )

    session$onSessionEnded(function() {
      stopApp()
    })

    EIGENVALUE <- factor1 <- factor2 <- factor3 <- factor4 <- factor5 <- factor6 <- factor7 <- NULL
  }




  shinyApp(ui = ui, server = server)
}



