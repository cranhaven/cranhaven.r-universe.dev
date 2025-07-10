#' Testing measurement & structural models for dichotomous and polytomous data
#' @import shiny
#' @import GPArotation igraph rJava xlsx
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyBS bsTooltip
#' @importFrom gt render_gt tab_header tab_style cols_width tab_options cell_text cells_body cell_fill md
#' @importFrom polycor polyserial
#' @importFrom ShinyItemAnalysis plotDistractorAnalysis
#' @importFrom DT formatStyle datatable
#' @importFrom foreign read.spss
#' @importFrom shinycustomloader withLoader
#' @importFrom shinythemes shinytheme
#' @importFrom grDevices colors dev.off pdf
#' @importFrom graphics abline barplot legend lines
#' @importFrom stats C D anova na.omit rbinom residuals rnorm runif sd var
#' @importFrom lavaan cfa fitMeasures
#' @importFrom semPlot semPaths
#' @importFrom MVN mvn
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{CFA()}
#' @export
CFA <- function(){
  CFA_ENV <- new.env()
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
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    theme = shinytheme("readable"),

    useShinyjs(),


    # setBackgroundColor(
    #   color = c("white", "gray"),
    #   gradient = "linear",
    #   direction = c("bottom", "right")
    # ),
    #


    uiOutput("cols"),

    ####################################################################################
    tags$head(tags$style(
      type="text/css",
      "#image0 img {max-width: 100%; width: auto; height: 100%; align: center}


      table,img, .tippy-content, textarea{ border-collapse: collapse;

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
    "#imagex img {max-width: 100%; width: auto; height: 50%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image1 img {max-width: 100%; width: auto; height: 50%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image2 img {max-width: 100%; width: auto; height: 50%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image3 img {max-width: 100%; width: auto; height: 50%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image3.1 img {max-width: 100%; width: auto; height: 100%; align: center}"
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

  #######################################################################


  tags$style(HTML("#a{color:black; font-family:Lucida Arial ;font-size: 16px;
             font-style: oblique;text-align:center}")), #tabs#

  tags$style(HTML("#ab{color:black; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#

  tags$style(HTML("#b{color:black; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download #

  tags$style(HTML("#b{color:black; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download #





  ########################################################################3

  ################ POPUP ################
  shinyBS::bsTooltip(
    id = "type",
    title = "Make sure you choose the data type correctly!",
    placement = "right",
    trigger = "hover"
  ),
  shinyBS::bsTooltip(
    id = "type2",
    title = "Make sure you choose the file format correctly!",
    placement = "right",
    trigger = "hover"
  ),
  shinyBS::bsTooltip(
    id = "fmodel",
    title = "Write syntax according to the example above.",
    placement = "right",
    trigger = "hover"
  ),
  shinyBS::bsTooltip(
    id = "ozet",
    title = "Download model summary in excel format!",
    placement = "right",
    trigger = "hover"
  ),
  shinyBS::bsTooltip(
    id = "modIndis",
    title = "Download modification indexes in excel format!",
    placement = "right",
    trigger = "hover"
  ),
  shinyBS::bsTooltip(
    id = "fit",
    title = "You can download all fit indexes from the output tab!",
    placement = "top",
    trigger = "hover"
  ),

  #titlePanel("CONFIRMATORY FACTOR ANALYSIS (CFA)"),

  div(id = "tepe",
      fluidRow(

        column(6,

               h1(id="title", "CONFIRMATORY FACTOR ANALYSIS (CFA)"),
               tags$style(HTML("#title{color: black; font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;font-size:30px;
            font-style: oblique;text-align:left}"))

        )  ,

        column(6,
               h1(id="title2", "RSP PACKAGE  - CRAN"),
               tags$style(HTML("#title2{color: black; font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;font-size:15px;
            font-style: oblique;text-align:right}"))

            # imageOutput("imagex",width = "15%", height = "30px", inline = TRUE),

        )

      )), # close fluidrow

  sidebarPanel(
    conditionalPanel(

      ## PANEL 1 ##

      condition = "input.panel==0",

      shiny::img(src = "img/rsp2.png", width = "97%"),

      tags$head(
        tags$script(HTML(js))
      ),
      # br(),
      # br(),
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
      ####################################################################
      #imageOutput("image1",width = "15%", height = "30px", inline = TRUE),
      ####################################################################

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

    ## PANEL 2 ##

    conditionalPanel(

      condition = "input.panel==1",
      shiny::img(src = "img/rsp2.png", width = "97%"),

      ######################################################################
      #imageOutput("image2",width = "15%", height = "30px", inline = TRUE),
      #####################################################################


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

      shinyWidgets::radioGroupButtons(
        inputId = "type",
        label =  h3(id="ab","Select Data Type"),
        choices = c("Polytomous (Likert etc..)"=1,
                    "1-0"=2),

        justified = TRUE,
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon"))),


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

      # fileInput("data1",
      #           h3(id="ab","Uplad Data File")),

      # fileInput(
      #   "data1",
      #   h3(id="ab","Uplad Data File",icon("paper-plane"))
      # ),

      fileInput(
        "data1",
        h3(id="ab","Uplad Data File",icon("paper-plane"))
      ),


      gt::gt_output("dat2"),

    ),

    ## PANEL 3 ##

    conditionalPanel(

      condition = "input.panel==2",

      br(),

      uiOutput("imageGif"),


      ###################################################################
      #imageOutput("image3",width = "15%", height = "30px", inline = TRUE),
      ###################################################################



      shinyWidgets::switchInput(
        inputId = "gifvid",
        label = "Watch Tutorial!",
        labelWidth = "100px",
        width = "200px"
      ),


      # withLoader(imageOutput("image3",width = "150%", height = "100px", inline = TRUE),
      #            type = "html", loader = "loader1"),


      br(),

      textAreaInput("fmodel", h3(id="ab","Please Write Your Model"),
                    "",
                    height = "300px"),



      # selectInput(
      #   "type3",
      #   h3(id="ab","Select Method Used for Prediction"),
      #
      #   choices = list("MLO" = "ML", "GLS" = "GLS",
      #                  "WLS" = "WLS","DWLS"= "DWLS" ,
      #                  "ULS"= "ULS", "MLR" = "MLR"),
      #   selected = 1
      # ),



      shinyWidgets::pickerInput     (
        "type3",
        h3(id="ab","Select Method Used for Prediction"),

        choices = list("MLO" = "ML", "GLS" = "GLS",
                       "WLS" = "WLS","DWLS"= "DWLS" ,
                       "ULS"= "ULS", "MLR" = "MLR"),
        selected = 1,

        options =  shinyWidgets::pickerOptions(showTick=TRUE,
        )),

      shinyWidgets::actionBttn(
        inputId = "send",
        label = "APPLY",
        style = "gradient",
        color = "default"

      ),

      # actionButton("send", h3(id="ab","Apply")),


      fluidRow(

        column(

          width = 6,

          selectInput(
            "tree",

            h3(id="ab","Graph Type"),

            choices = list("Tree_1" = "tree",

                           "Tree_2" = "tree2",

                           "Circle_1"="circle",

                           "Circle_2"="circle2",

                           "Spring"="spring"),

            selected = "tree2"
          ) ),

        column(
          width = 6,
          selectInput(
            "colour",
            h3(id="ab","Graph Colour"),

            choices = list("Black" = 1,

                           "Red" = 2,

                           "Green"=3,

                           "Blue"=4

            ),

            selected = 4
          ) )
      ),

      plotOutput("path")













    ),



  ),  # close sidebar panel



  ## MAIN PANEL

  mainPanel(
    tabsetPanel(
      id = "panel",

      tabPanel(

        h4(id="a", "INTRODUCTION"),


        value = 0,
        br(),
        br(),
        br(),
        fluidRow(

          column(12, align="center",
                 shiny::img(src = "img/rsp2.png", width = "97%"),

                 ###################################################################
                 #imageOutput("image0",width = "75%", height = "50px", inline = TRUE),
                 ###################################################################

          ))),
      tabPanel(
        h4(id="a","DATA UPLOAD"),
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

        DT::dataTableOutput("dat1"),


        gt::gt_output("mvn1"),

        br(),

        gt::gt_output("mvn4"),
        br(),

      ),



      ###  MAIN PANEL 3

      tabPanel(
        h4(id="a","STRUCTURAL MODEL"),
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
        uiOutput("cfaResult"),
        DT::dataTableOutput("cfaDT"),




        uiOutput("fitResult"),



        DT::dataTableOutput("fit"),



        br(),



        ### fit index

        uiOutput("modificationIndex"),

        br(),

        uiOutput("foraction"),




        uiOutput("foraction2")



      ),  # close tabpanel


    )
  )
  )

  ### SERVER ###

  server <- function(input, output, session) {


    #
    # observeEvent(input$data1, {
    #   show_alert(
    #     title = "Success !!",
    #     text = "DATA WAS UPLOADED",
    #     type = "success"
    #   )
    # })


    observeEvent(input$myColor,{

      output$cols<- renderUI({   # WIDET RENDER UI RENK DEĞİŞİMİ

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


    #################

    output$imagex <- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rspx.png", contentType = "image/png")
    },
    deleteFile = FALSE)
    #
    ###########################


    output$image0<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp2.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image1<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp2.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image2<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp2.png", contentType = "image/png")
    },
    deleteFile = FALSE)





    # output$image3<- renderImage({
    #
    #   req (input$data1)
    #
    #
    #   req( input$gifvid== TRUE)
    #   resim2 <- tempfile(fileext = '.gif')
    #   list(src = "cfa1.gif", contentType = "image/gif")
    # },
    # deleteFile = FALSE)

    output$imageGif <- renderUI({
      req(input$data1)
      req(input$gifvid == TRUE)
      shiny::img(src="https://shiny.eptlab.com/RSPEN/cfa/img/cfa1.gif", width = "97%")
    })



    output$image4<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp2.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image5<- renderImage({
      resim2 <- tempfile(fileext = '.gif')
      list(src = "download.gif", contentType = "image/gif")
    },
    deleteFile = FALSE)

    ###########################################################################

    output$text1<- renderText({

      if (!is.null(input$data1)) {
        paste0 ( "DATA UPLOAD AND DESCRIPTIVE STATISTICS")
      }

    })

    output$text3<- renderText({

      if (!is.null(input$data1)) {
        paste0 ("DOWNLOAD ANALYSIS OUTPUT")
      }

    })

    output$uiHeader <- renderUI({
      if(input$type2 == 3){
        NULL
      } else {

        shinyWidgets::materialSwitch(
          inputId = "header",
          label =   h4("The first line is the variable name"),
          value = TRUE,
          status = "primary"
        )

      }


    })



    ## DATA UPLOAD ##

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

    data1 <- reactive({
      verix <- data()
      colnames(verix) <- paste0("i", c(1:ncol(verix)))
    })

    # VISUALS #

    output$dfaornek<- renderImage({
      resim2 <- tempfile(fileext = '.gif')
      list(src = "dfaornekgif.gif", contentType = "image/gif")
    },
    deleteFile = FALSE)

    ## USER INTERFACE UPDATE ##

    observeEvent(input$send,{


      output$cfaResult <- renderUI({
        h3(id="ab","MODEL SUMMARY")
      })
      output$fitResult <- renderUI({
        h3(id="ab","FIT INDEXES")
      })
      output$modificationIndex <- renderUI({
        h3(id="ab","DETAILED OUTPUTS")
      })

      model <- reactive({
        input$fmodel
      })

      Cfa <- reactive({

        data <- data()
        colnames(data) <- paste0("i", c(1:ncol(data)))
        lavaan::cfa(model(), data, estimator = input$type3)

      })

      # OUTPUT TABLE #

      output$cfaDT <- DT::renderDataTable({

        CFA_ENV$CFA1 <- Cfa()
        Cfa <- CFA_ENV$CFA1
        # data <- data()
        # colnames(data) <- paste0("i", c(1:ncol(data)))

        cfaSum <- lavaan::summary(Cfa, standardized = TRUE)
        cfaSum2 <- data.frame(first = cfaSum$pe$lhs,
                              op = cfaSum$pe$op,
                              last = cfaSum$pe$rhs,
                              est = round(cfaSum$pe$est, 3),
                              std.all = round(cfaSum$pe$std.all, 3),
                              sh = round(cfaSum$pe$se, 3),
                              z = round(cfaSum$pe$z, 3),
                              p = round(cfaSum$pe$pvalue, 6))
        cfaSumDT <- print(cfaSum2)
        backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                    "orange", "lightblue", "gray"), 1)
        datatable(cfaSumDT) %>% formatStyle(colnames(cfaSumDT),
                                            backgroundColor = backgroundColor) %>%
          formatStyle('z',
                      backgroundColor = DT::styleInterval(c(-1.960,1.960),
                                                          c(backgroundColor, 'red', backgroundColor)))

      }, options = list(pageLength = 2))

      output$path <- renderPlot({

        Cfa<- Cfa()

        if(!is.null(Cfa)){

          if(Cfa@pta$nvar[[1]] < 9){
            print(semPaths(Cfa, whatLabels = "std", layout  = input$tree,
                           style = "lisrel", rotation = 1, sizeMan = 10,
                           sizeLat = 10, sizeLat2 = 10, sizeInt = 10,
                           nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                           inheritColor = FALSE, edge.label.cex = 1, width = 10,
                           height = 20))
          } else if(Cfa@pta$nvar[[1]] < 13) {
            print(semPaths(Cfa, whatLabels = "std", layout = input$tree,
                           style = "lisrel", rotation = 1, sizeMan = 7,
                           sizeLat = 7, sizeLat2 = 7, sizeInt = 7,
                           nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                           inheritColor = FALSE, edge.label.cex = 0.7, width = 10,
                           height = 20))
          } else {
            print(semPaths(Cfa, whatLabels = "std", layout = input$tree,
                           style = "lisrel", rotation = 1, sizeMan = 3,
                           sizeLat = 3, sizeLat2 = 3, sizeInt = 3, nCharNodes = 3,
                           nCharEdges = 3, edge.color = input$colour,
                           inheritColor = FALSE, edge.label.cex = 0.3, width = 10,
                           height = 20))
          }
        } else{
          return(NULL)
        }

      })

      output$fit <- DT::renderDataTable({

        Cfa<- Cfa()

        indexFit <- fitMeasures(Cfa)
        if(input$type3 == "MLR"){
          indexFitx <- cbind(indexFit[["chisq"]], indexFit[["df"]],
                             indexFit[["pvalue"]], indexFit[["rmsea.robust"]],
                             indexFit[["cfi.robust"]], indexFit[["agfi"]],
                             indexFit[["tli.robust"]], indexFit["srmr"])
          colnames(indexFitx) <- c("Chi-square", "df", "p", "RMSEA (Robust)",
                                   "CFI (Robust)", "AGFI", "NNFI (TLI) (Robust)", "SRMR")


          rownames(indexFitx) <- "Value"
          paste0("Fit Indexes")
          fitSumDT <- print(round(indexFitx, 3))
          backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                      "aquamarine", "lightblue", "gray"), 1)
          datatable(fitSumDT) %>% formatStyle(colnames(fitSumDT),
                                              backgroundColor = backgroundColor) %>%
            formatStyle('RMSEA (Robust)',
                        backgroundColor = DT::styleInterval(c(0.05,0.08),
                                                            c("green", "orange", "red"))) %>%
            formatStyle('CFI (Robust)',
                        backgroundColor = DT::styleInterval(c(0.90,0.95),
                                                            c("red", "orange", "green"))) %>%
            formatStyle('AGFI',
                        backgroundColor = DT::styleInterval(c(0.90,0.95),
                                                            c("red", "orange", "green"))) %>%
            formatStyle('NNFI (TLI) (Robust)',
                        backgroundColor = DT::styleInterval(c(0.90, 0.95),
                                                            c("red", "orange", "green"))) %>%
            formatStyle('SRMR',
                        backgroundColor = DT::styleInterval(c(0.05,0.08),

                                                            c("green", "orange", "red")))
        } else {
          indexFitx <- cbind(indexFit[["chisq"]], indexFit[["df"]],
                             indexFit[["pvalue"]], indexFit[["rmsea"]],
                             indexFit[["cfi"]], indexFit[["agfi"]],
                             indexFit[["nnfi"]], indexFit["srmr"])
          colnames(indexFitx) <- c("Chi-square", "df", "p", "RMSEA",
                                   "CFI", "AGFI", "NNFI(TLI)", "SRMR")


          rownames(indexFitx) <- "Value"
          paste0("Fit Indexes")
          fitSumDT <- print(round(indexFitx, 3))
          backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                      "aquamarine", "lightblue", "gray"), 1)
          datatable(fitSumDT) %>% formatStyle(colnames(fitSumDT),
                                              backgroundColor = backgroundColor) %>%
            formatStyle('RMSEA',
                        backgroundColor = DT::styleInterval(c(0.05,0.08),
                                                            c("green", "orange", "red"))) %>%
            formatStyle('CFI',
                        backgroundColor = DT::styleInterval(c(0.90,0.95),
                                                            c("red", "orange", "green"))) %>%
            formatStyle('AGFI',
                        backgroundColor = DT::styleInterval(c(0.90,0.95),
                                                            c("red", "orange", "green"))) %>%
            formatStyle('NNFI(TLI)',
                        backgroundColor = DT::styleInterval(c(0.90, 0.95),
                                                            c("red", "orange", "green"))) %>%
            formatStyle('SRMR',
                        backgroundColor = DT::styleInterval(c(0.05,0.08),

                                                            c("green", "orange", "red")))
        }

      })



      output$allfit1<- renderUI({

        req(input$data1)

        DT::DTOutput("allfit2")  })



      output$allfit2<-DT::renderDT({

        req(input$data1)

        a<-lavaan:: fitMeasures(Cfa())

        Values<-as.data.frame( round(a,3))

        #a2<-  datatable(a1)

        backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                    "aquamarine", "lightblue", "gray"), 1)
        datatable(Values) %>% formatStyle(colnames(Values),
                                          backgroundColor = "skyblue")


        # return(Values)

      })


      output$modification <- DT::renderDataTable({
        Cfa<- Cfa()
        #req( input$start2==1)
        mod <- lavaan::modificationindices(Cfa, sort = TRUE)
        modDT <- data.frame(
          first_Variable = mod$lhs,
          operator = mod$op,
          second_Variable = mod$rhs,
          modification_index = round(mod$mi, 3))
        backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                    "aquamarine", "lightblue", "gray"), 1)
        datatable(modDT) %>% formatStyle(colnames(modDT),
                                         backgroundColor = backgroundColor)

      })


      output$fak2 <- renderUI({

        data <- data()
        colnames(data) <- paste0("i", 1:ncol(data))

        lapply(1:input$fak, function(i) {
          selectInput(paste0('f', i), paste0('Factor', i),
                      choices = names(data), multiple = TRUE)})
      })

      output$text2<- renderText({


        if (!is.null(input$data1)) {
          paste0 ( "CFA RESULTS")
        }
      })
    })

    ## SHOW UPLOADED DATA ##

    output$dat1 <- DT::renderDataTable({


      if (!is.null(input$data1)) {
        data <- data()
        colnames(data) <- paste0("i", c(1:ncol(data)))
        if (ncol(data) <= 15)
        {
          data[1:10, 1:ncol(data)]

        }
        else
        {
          data[1:10, 1:15]

        } }  })

    ## DESCRIPTIVE ##

    output$dat2 <- gt::render_gt(align = "center",{
      if (!is.null(input$data1)) {
        data <- (data())

        N_Item <- ncol(data)
        N <- nrow(data)
        Na<- length(which(is.na(data)))

        res <- data.frame(N_Item, N, Na)

        res<- gt::gt(res)
        br()
        br()
        br()
        res<-res  %>%    tab_header(
          title= md("*Information About Dataset*"))

        res<-res %>%

          tab_style(
            style = cell_fill(color = sample(colors()[3:100],1),alpha=0.15),

            locations = cells_body()
          )

        res<- res %>%

          cols_width(
            everything() ~ gt::px(120))

        res <- res %>%

          tab_options(
            column_labels.font.size=gt::px(14),
            column_labels.font.weight="bold"
          )

        res <- res %>%

          tab_options(
            heading.title.font.size = gt::px(25))

      }
    })


    ## MULTIVARIATE NORMALITY TEST ##

    #HZ#

    output$mvn1<- gt::render_gt({

      if (!is.null(input$data1)) {

        data<-data()

        hz<-mvn(data=data,mvnTest = "hz")

        hz<-hz$multivariateNormality

        colnames(hz)<-c("Test","Hz","P_value", "Result")

        hz<-gt::gt(hz)

        hz<-hz  %>%    tab_header(
          title= md("*Henze Zirkler Multivariate Nomality Test*"))

        hz<-hz %>%

          tab_style(
            style = cell_fill(color = sample(colors()[3:100],1),alpha=0.15),

            locations = cells_body()
          )

        hz <- hz %>%

          cols_width(
            everything() ~ gt::px(180))

        hz <- hz %>%

          tab_options(
            column_labels.font.size=gt::px(17),
            column_labels.font.weight="bold"
          )

        hz <- hz %>%

          tab_options(
            heading.title.font.size = gt::px(25))

        return(hz)

      }
    })


    # MARDIA #

    output$mvn4<-gt::render_gt({

      if (!is.null(input$data1)) {

        data<-data()

        mrd<-mvn(data=data,mvnTest = "mardia")

        mrd<-mrd$multivariateNormality

        colnames(mrd)<-c("Test", "Statistic","p_value", "Reult")

        mrd<-mrd[-3,]

        mrd<-gt::gt(mrd)

        mrd<-mrd  %>%    tab_header(
          title= md("*Mardia Multivariate Nomality Test*"))

        mrd<-mrd %>%

          tab_style(
            style = cell_fill(color = sample(colors()[3:100],1),alpha=0.15),

            locations = cells_body()
          )

        mrd<-mrd %>%

          cols_width(
            everything() ~ gt::px(180))

        mrd <- mrd %>%

          tab_options(
            column_labels.font.size=gt::px(17),
            column_labels.font.weight="bold"
          )

        mrd <- mrd %>%

          tab_options(
            heading.title.font.size = gt::px(25))

        return(mrd)
      }
    })

    ## DOWNLOAD OUTPUT ##

    output$ozet <- downloadHandler(
      filename = function() {
        "summary.csv"
      },
      content = function(file) {
        utils::write.csv2(lavaan::summary(CFA_ENV$CFA1)$pe, file)
      }
    )


    output$modIndis <- downloadHandler(
      filename = function() {
        "modIndeks.csv"
      },
      content = function(file) {

        utils::write.csv2(lavaan::modificationindices(CFA_ENV$CFA1), file)
      }
    )
    output$dlPath <- downloadHandler(


      filename = function() {


        "path.pdf"
      },

      content = function(file) {

        pdf(file)
        if(CFA_ENV$CFA1@pta$nvar[[1]] < 9){
          print(semPlot::semPaths(CFA_ENV$CFA1, whatLabels = "std",
                                  layout = input$tree, style = "lisrel", rotation = 1, sizeMan = 10,
                                  sizeLat = 10, sizeLat2 = 10, sizeInt = 10,
                                  nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                                  inheritColor = FALSE, edge.label.cex = 1, width = 10, height = 20))
        } else if(CFA_ENV$CFA1@pta$nvar[[1]] < 13) {


          print(semPlot::semPaths(CFA_ENV$CFA1, whatLabels = "std",
                                  layout = input$tree, style = "lisrel", rotation = 1, sizeMan = 7,
                                  sizeLat = 7, sizeLat2 = 7, sizeInt = 7,
                                  nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                                  inheritColor = FALSE, edge.label.cex = 0.7, width = 10, height = 20))
        } else {


          print(semPlot::semPaths(CFA_ENV$CFA1, whatLabels = "std",
                                  layout = input$tree, style = "lisrel", rotation = 1, sizeMan = 5,
                                  sizeLat = 5, sizeLat2 = 5, sizeInt = 5,
                                  nCharNodes = 3, nCharEdges = 3, edge.color = input$colour,
                                  inheritColor = FALSE, edge.label.cex = 0.3, width = 10, height = 20))
        }

        dev.off()
      }
    )

    output$dlFit <- downloadHandler(


      filename = function() {
        "fitIndexes.csv"
      },

      content = function(file) {

        utils::write.csv2(lavaan::fitMeasures(CFA_ENV$CFA1), file)
      }
    )



    observeEvent(input$send,{

      output$foraction<-renderUI({

        # req(input$send==TRUE)


        fluidRow(

          column(4,


                 shinyWidgets::dropMenu(



                   padding = "20px",

                   theme="light-border",

                   placement = "top-start",


                   shinyWidgets::actionBttn(
                     inputId = "acb3",
                     label = " Click To See All Fit Index",
                     style = "gradient",
                     color = "default",
                     size = "lg"

                   ),

                   uiOutput("allfit1")

                 )


          ),



          column( 4,

                  shinyWidgets::dropMenu(

                    padding = "20px",

                    theme="light-border",

                    placement = "top-start",


                    shinyWidgets::actionBttn(
                      inputId = "modif",
                      label = "Click To See Modifications",
                      style = "gradient",
                      color = "default",
                      size = "lg"

                    ),

                    DT::dataTableOutput("modification")

                  )

          ), #  close clomun 2

          # close drop menu



          column(4,

                 shinyWidgets::dropMenu(

                   padding = "40px",


                   theme="light-border",

                   placement = "top-start",


                   shinyWidgets::actionBttn(
                     inputId = "downs",
                     label = "Download All Outputs",
                     style = "gradient",
                     color = "default",
                     size = "lg"

                   ),

                   fluidRow(
                     column(3,



                            shinyWidgets::downloadBttn(
                              "ozet",
                              label = h5(id="dwn","Model Sum"),
                              style = "jelly",
                              color = "primary",
                              size = "xs",
                              block = FALSE,
                              no_outline = TRUE,
                              icon = shiny::icon("download")
                            ),
                     ),


                     column(3,





                            shinyWidgets::downloadBttn(
                              "modIndis",
                              label = h5(id="dwn", "Mod Index"),
                              style = "jelly",
                              color = "primary",
                              size = "xs",
                              block = FALSE,
                              no_outline = TRUE,
                              icon = shiny::icon("download")
                            ),
                     ),


                     column(3,




                            shinyWidgets::downloadBttn(
                              "dlPath",
                              label = h5(id="dwn", "Path Diag"),
                              style = "jelly",
                              color = "primary",
                              size = "xs",
                              block = FALSE,
                              no_outline = TRUE,
                              icon = shiny::icon("download")
                            ),

                     ),

                     column(3,

                            shinyWidgets::downloadBttn(
                              "dlFit",
                              label = h5(id="dwn", "Fit Index"),
                              style = "jelly",
                              color = "primary",
                              size = "xs",
                              block = FALSE,
                              no_outline = TRUE,
                              icon = shiny::icon("download")
                            ),
                     ),

                     #
                   ) # close fluidrow for downloads

                 ) # close column before fluidrow in downloads

          )   # close column 2



        ) # close fldr


      })  # close renderUI foraction


    }) # observe event


    session$onSessionEnded(function() {
      stopApp()
    })
  }

  shinyApp(ui = ui, server = server)
}


