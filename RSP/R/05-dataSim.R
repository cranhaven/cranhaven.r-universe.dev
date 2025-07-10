
#' Generate simulated data according to IRT for dichotomous and polytomous data
#' Generate multidimensional data for factor analysis
#' # param options(java.parameters = "-Xmx8000m")
#' @import shiny
#' @importFrom Metrics bias rmse
#' @importFrom catR genPolyMatrix genPattern
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{SIMDATA()}
#' @export



SIMDATA <- function(){
  SIMDATA_ENV <- new.env()

  options(java.parameters = "-Xmx8000m")

  js <- "
// This solution from https://stackoverflow.com/a/59674107
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
    useShinyjs(),

    useShinyjs(),
    theme = shinytheme("readable"),

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
  border-bottom: 2px solid black;
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



###################################################################################

tags$style(HTML("#a{color:black; font-family:Lucida Arial ;font-size: 20px;
              font-style: oblique;text-align:center}")), #tabs#

tags$style(HTML("#ab{color:black; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#

tags$style(HTML("#b{color:blcak; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download
###################################################################################3



## POP UPS ##

bsTooltip(
  id = "down",
  title = "Downloading datasets take some time depending on the number of replications!",

  placement = "top",
  trigger = "hover"
),

bsTooltip(
  id = "downpoly",
  title =  "Downloading datasets take some time depending on the number of replications!"
  ,
  placement = "top",
  trigger = "hover"
),


bsTooltip(
  id = "downfac",
  title =  "Downloading datasets take some time depending on the number of replications!"
  ,
  placement = "top",
  trigger = "hover"
),


#titlePanel("DATA GENERATION PANEL"),

# h1(id="title", "DATA GENERATION PANEL"),
# tags$style(HTML("#title{color: black; font-family: Arial;font-size: 35px;
#         font-style: oblique;text-align:left}")),

div(id = "tepe",
    fluidRow(

      column(6,

             h1(id="title", "DATA GENERATION PANEL"),
             tags$style(HTML("#title{color: black; font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;;font-size:30px;
            font-style: oblique;text-align:left}"))

      )  ,

      column(6,
             h1(id="title2", "RSP PACKAGE  - CRAN"),
             tags$style(HTML("#title2{color: black; font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;;font-size:15px;
            font-style: oblique;text-align:right}"))

            # imageOutput("imagex",width = "15%", height = "30px", inline = TRUE),

      )

    )), # close fluidrow



sidebarPanel(

  width = 4,

  ##  PANEL 1 INTRODUCTION ##

  conditionalPanel(
    condition = "input.panel==0",

    shiny::img(src = "img/rsp3.png", width = "97%"),



    ####################################################################################
    #imageOutput("image1",width = "75%", height = "100px", inline = TRUE),
    ######################################################################################


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
      ),

    ),


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

  ## PANEL 2 DICHOTOMOUS DATA ##

  conditionalPanel(
    condition = "input.panel==1",

    shinyWidgets::chooseSliderSkin("Big", color = "#112446"),

    fluidRow(

      column(12,
             shiny::img(src = "img/rsp3.png", width = "97%"),

      ) ),

    br(),

    ####################################################################################
    #imageOutput("image2",width = "75%", height = "100px", inline = TRUE),
    ####################################################################################

    br(),

    shinyWidgets::pickerInput(
      "type1",
      h3(id="ab", "Select Model"),

      choices = list("1PL" = 1, "2PL" = 2,"3PL"=3 ),
      selected = 3
    ),


    fluidRow(
      column(6,

             # numericInput("num1",
             #
             #                 h3(id="ab","Enter Number of Items"), min=5,max=100,value=10)
             #

             shinyWidgets::numericInputIcon(
               inputId = "num1",
               label = NULL,
               min = 5,
               max=200,
               value = 10,
               icon = "Number of Items",
               width = "600px",
               size = "sm"
             )
      ),

      column(6,


             shinyWidgets::numericInputIcon(
               inputId = "num2",
               label = NULL,
               min = 50,
               max=10000,
               value = 300,
               icon = "N",
               width = "600px",
               size = "sm"
             )

      )
    ),


    fluidRow(
      column(6, uiOutput("mp1")),
      column(6, uiOutput("mp1.1")),
    ),

    fluidRow(
      column(6, uiOutput("mp2")),
      column(6, uiOutput("mp3")),
    ),
    fluidRow(
      column(6,

             shinyWidgets::numericInputIcon(
               inputId = "rep",
               label = NULL,
               min = 1,
               max=1000,
               value = 1,
               icon = "Replication"
             )

      ),

      column(6,
             uiOutput("son")

      )
    ),


    shinyWidgets::dropMenu(

      padding = "20px",

      theme="light-border",

      placement = "right-end",


      shinyWidgets::actionBttn(
        inputId = "coms",
        label = "CLICK TO COMPUTE RMSE-BIAS VALUES",
        style = "jelly",
        color = "primary"

      ),


      shinyWidgets::pickerInput("irt", h3(id="ab","DO YOU WANT TO COMPUTE RMSE-BIAS VALUES"),

                  choices = list("YES" = 1, "NO" = 2
                  ), selected = 2),


      gt::gt_output("fit1"),


      gt::gt_output("fit2"),

      withLoader( gt::gt_output("fit3"), type = "html", loader = "loader1"),

      gt::gt_output("grm"),

    ),


  ), # close conditional panel

  ## PANEL 3 POLYTOMOUS DATA ##

  conditionalPanel(
    condition = "input.panel==2",

    fluidRow(

      column(12,
             shiny::img(src = "img/rsp3.png", width = "97%")

      ) ),

    br(),

    ####################################################################################
    #imageOutput("image3",width = "75%", height = "100px", inline = TRUE),
    ####################################################################################

    br(),

    fluidRow(

      column(8,

             shinyWidgets::radioGroupButtons(
               inputId = "type2",
               label =  h3(id="ab","Select Model"),
               choices = list("PCM" = 1, "RSM" = 2,"GPCM"=3,"GRM"=4),
               justified = TRUE,
               checkIcon = list(
                 yes = icon("ok",
                            lib = "glyphicon")),

               # status = "primary"
             )


      ), # close clmn

      column(4,


             shinyWidgets::numericInputIcon(
               inputId = "sec",
               label =  h3(id="ab","Categories"),
               min = 1,
               max=10,
               value = 3,
               icon = " N"
             )



      ) # close column

    ),

    fluidRow(

      fluidRow(
        column(6,

               shinyWidgets::numericInputIcon(
                 inputId = "nitem",
                 label = NULL,
                 min = 5,
                 max=200,
                 value = 30,
                 icon = "Number of Items",
                 width = "600px",
                 size = "sm"
               )

        ), # close column

        column(6,

               shinyWidgets::numericInputIcon(
                 inputId = "nn",
                 label = NULL,
                 min = 50,
                 max=10000,
                 value = 100,
                 icon = "N",
                 width = "600px",
                 size = "sm"
               )


        ) # close column
      ),

      fluidRow(

        column(6,

               shinyWidgets::numericInputIcon(
                 inputId = "polyrep",
                 label = NULL,
                 min = 1,
                 max=1000,
                 value = 1,
                 icon = "Replication"
               )

        ), # close column



        column(6,


               shinyWidgets::switchInput(
                 inputId = "start2",
                 onLabel = "RESET",
                 offLabel = "START",
                 offStatus = "red",
                 handleWidth = "200px",
                 label = "GENERATE",
                 labelWidth = "400px",
                 size = "normal",
                 inline = TRUE

               )



        )


      ), # close fluid row




    ) ),



  ## PANEL 4 MULTIDIMENSIAL DATA


  conditionalPanel(
    condition = "input.panel==3",


    fluidRow(

      column(12,
             shiny::img(src = "img/rsp3.png", width = "97%")

      ) ),


    br(),

    ####################################################################################
    #imageOutput("image4",width = "75%", height = "100px", inline = TRUE),
    ####################################################################################

    br(),

    fluidRow(

      column(6, shinyWidgets::pickerInput( "ftype1",

                             h3(id="ab","Select Data Type"),

                             choices = list("Dichotomous" = TRUE, "Polythomous" = FALSE,"Continious"= 3),
                             selected = FALSE
      ),
      ),

      column(6, sliderInput( "meanload", h3(id="ab","Average Factor Loading"),

                             min = 0, max = 1, step = 0.05, value = 0.55


      )),

    ),


    fluidRow(

      column(6,

             # numericInput("fnitem", h3(id="ab","Number of Items"), min=5,max=100,value=30)


             shinyWidgets::numericInputIcon(
               inputId = "fnitem",
               label = NULL,
               min = 5,
               max=200,
               value = 30,
               icon = "N of Items"
             )




      ),

      column(6,





             shinyWidgets::numericInputIcon(
               inputId = "fnn",
               label = NULL,
               min = 50,
               max=10000,
               value = 100,
               icon = "N"
             )



      )
    ),

    fluidRow(

      column(5,



             shinyWidgets::numericInputIcon(
               inputId = "nfac",
               label = h3(id="ab","Number of Factors"),
               min = 1,
               max=20,
               value = 2,
               icon = "N of factors"
             )



      ),

      column(7,



             shinyWidgets::numericInputIcon(
               inputId = "nlfl",
               label = h3(id="ab","N of Items with Low Loadings"),
               min = 0,
               max=30,
               value = 0,
               icon = "Low loadings"
             )




      )
    ),


    fluidRow(

      column(6,

             shinyWidgets::numericInputIcon(
               inputId = "frep",
               label = NULL,
               min = 1,
               max=1000,
               value = 1,
               icon = "Replication"
             )

      ), # close column



      column(6,


             shinyWidgets::switchInput(
               inputId = "start3",
               onLabel = "RESET",
               offLabel = "START",
               offStatus = "red",
               handleWidth = "200px",
               label = "GENERATE",
               labelWidth = "400px",
               size = "normal",
               inline = TRUE

             )

      )


    ), # close fluid row

  ) # close conditional panel

),  # close sidebar panel

##  MAIN PANEL ##

mainPanel(
  tabsetPanel(

    ## M.PANEL 1 ##

    id = "panel",

    tabPanel(

      h4(id="a", "INTRO"),

      value = 0,

      br(),
      br(),
      br(),

      fluidRow(

        column(12, align="center",
               shiny::img(src = "img/rsp3.png", width = "97%")

        )),

      ####################################################################################
      #imageOutput("image0",width = "75%", height = "100px", inline = TRUE),
      ####################################################################################

    ),

    ## M. PANEL 2 ##

    tabPanel(
      h4(id="a","DICHO DATA GEN (IRT)"),
      value = 1,

      br(),

      textOutput("text0"),

      tags$head(tags$style("#text0{color: darkblue;
                                 font-size: 25px;
                                 font-family: cursive;
                                 font-style: oblique;
                                 text-align:center;
                                 letter-spacing:1px;

                                 }")),




      DT::dataTableOutput("tab1"),

      DT::dataTableOutput("tab2"),

      DT::dataTableOutput("tab3"),

      DT::dataTableOutput("tab4"),


      span( textOutput("text1"),style="color:red"), # NO NEED

      tags$head(tags$style("#text1{color: darkblue;
                                 font-size: 25px;
                                 font-family: cursive;
                                 font-style: oblique;
                                 text-align:center;
                                 letter-spacing:1px;

                                 }")),

      br(),

      uiOutput("down"),


      br(),


      textOutput("text2"),


      tags$head(tags$style("#text2{color: red;
                                 font-size: 25px;
                                 font-family: cursive;
                                 font-style: oblique;
                                 text-align:center;
                                 letter-spacing:1px;

                                 }")),
      br(),

      # gt::gt_output("fit1"),
      #
      #
      # gt::gt_output("fit2"),


      # withLoader( gt::gt_output("fit3"), type = "html", loader = "loader1"),
      #
      #
      # gt::gt_output("grm"),


    ),


    ##  M. PANEL 3 ##

    tabPanel(
      h4(id="a", "POLY DATA GEN (IRT)"),
      value = 2,

      br(),

      textOutput("text00"),

      tags$head(tags$style("#text00{color: darkblue;
                                 font-size: 25px;
                                 font-family: cursive;
                                 font-style: oblique;
                                 text-align:center;
                                 letter-spacing:1px;

                                 }")),

      br(),

      withLoader( DT::DTOutput("tab_pcm"),  type = "html", loader = "loader1"),

      DT::DTOutput("tab_rsm"),

      DT::DTOutput("tab_gpcm"),

      DT::DTOutput("tab_grm"),

      DT::DTOutput("tab_nrm"),


      textOutput("text3"),


      tags$head(tags$style("#text3{color: darkblue;
                                 font-size: 25px;
                                 font-family: cursive;
                                 font-style: oblique;
                                 text-align:center;
                                 letter-spacing:1px;

                                 }")),
      br(),


      uiOutput("downpoly")
    ),

    ## M.PANEL 4  ##

    tabPanel(

      h4(id="a", "MULTIDIM DATA GEN (CTT)"),
      value = 3,

      br(),

      textOutput("text000"),

      tags$head(tags$style("#text000{color: darkblue;
                                 font-size: 25px;
                                 font-family: cursive;
                                 font-style: oblique;
                                 text-align:center;
                                 letter-spacing:1px;

                                 }")),
      br(),

      # dataTableOutput("tab_fac_Poly"),
      #
      #
      # dataTableOutput("tab_fac_Dicho"),
      #
      #
      # dataTableOutput("tab_fac_Cont"),



      uiOutput("tab_fac_all"),


      textOutput("text4"),

      tags$head(tags$style("#text4{color: darkblue;
                                 font-size: 25px;
                                 font-family: cursive;
                                 font-style: oblique;
                                 text-align:center;
                                 letter-spacing:1px;

                                 }")),


      br(),

      br(),

      uiOutput("downfac")


    )
  )


) # close main panel
  ) # close fluidpage




  ## SERVER ##

  server <- function(input,session, output) {



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

    output$image0<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp3.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image1<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp3.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image2<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp3.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image3<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp3.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image4<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp3.png", contentType = "image/png")
    },
    deleteFile = FALSE)



    ###########################################################################



    ## UI WIDGETS ##

    output$mp1<-renderUI( {

      sliderInput("num3",h3(id="ab","Item Difficulty (b)"),
                  min=-3,max=4, step=0.01,value=c(-1,1))
    })


    output$mp1.1<-renderUI({
      if(!is.null(input$type1)){
        if (input$type1==1)

        { sliderInput("num3.1",h3(id="ab","Item Discrimination (a)"),
                      min=-1,max=3, step=0.01,value=1)}

      }
    })

    output$mp2<-renderUI({
      if(!is.null(input$type1)){
        if ( input$type1!=1) {


          sliderInput("num4",
                      h3(id="ab","Item Discrimination (a)"), min=-1,max=3, step=0.01,value=c(0.70,1.5))}

      }
    })

    output$mp3<-renderUI({
      if(!is.null(input$type1)){
        if ( input$type1==3 | input$type1==4 ) {

          sliderInput("num5",
                      h3(id="ab","Guessing Parameter (c)"), min=-0,max=0.35, step=0.01,value=c(0,0.20))}
      }
    })

    output$mp4 <-renderUI({
      if(!is.null(input$type1)){
        if ( input$type1==4 ) {

          sliderInput("num6",h3(id="ab","Upper Asymptotic (d)"), min=0,
                      max=1, step=0.01,value=c(0.70,0.90)) }
      }
    })

    output$son<- renderUI({

      fluidRow(

        column(6,


               shinyWidgets::switchInput(
                 inputId = "start",
                 onLabel = "RESET",
                 offLabel = "START",
                 offStatus = "red",
                 handleWidth = "200px",
                 label = "GENERATE",
                 labelWidth = "400px",
                 size = "normal",
                 inline = TRUE

               )



        ), # close colum
      )

    })






    output$down<-renderUI( {

      req( input$start==1)

      div(style="display: inline-block;vertical-align:top;
        width: 100px;",HTML("<br>"))

      fluidRow(
        column(5),
        column(4,


               # downloadButton("down1", h1(id="b","Download Generated Data"))

               shinyWidgets::downloadBttn(
                 "down1",
                 label = h1(id="b","Download Generated Data"),
                 style = "unit",
                 color = "primary",
                 size = "sm",
                 block = FALSE,
                 no_outline = TRUE,
                 icon = shiny::icon("download")
               ),


        ),
        column(3)

      )

    })

    output$downpoly<-renderUI( {

      req( input$start2==1)

      div(style="display: inline-block;vertical-align:top; width:
        100px;",HTML("<br>"))

      fluidRow(

        column(5),
        column(4,



               shinyWidgets::downloadBttn(
                 "down2",
                 label = h1(id="b","Download Generated Data"),
                 style = "unit",
                 color = "primary",
                 size = "sm",
                 block = FALSE,
                 no_outline = TRUE,
                 icon = shiny::icon("download")
               ),



        ), # close column

        column(3)

      )

    })


    output$downfac<-renderUI( {

      req( input$start3==1)

      div(style="display: inline-block;vertical-align:top; width:
        100px;",HTML("<br>"))

      fluidRow(

        column(5),


        column(4,

               # downloadButton("down3", h1(id="b","Download Generated Data"))









               shinyWidgets::downloadBttn(
                 "down3",
                 label = h1(id="b","Download Generated Data"),
                 style = "unit",
                 color = "primary",
                 size = "sm",
                 block = FALSE,
                 no_outline = TRUE,
                 icon = shiny::icon("download")
               ),



        ), # close column



        column(3)

      )

    })

    ## DATA GENERATION ##

    ## 1PL ##


    output$tab1<-DT::renderDataTable(rownames = TRUE ,{

      req( input$start==1)

      if(input$type1==1)  {

        nitem<-input$num1

        n<- input$num2

        a<-rep(input$num3.1,nitem)

        minb<- min(input$num3)
        maxb<- max(input$num3)

        theta<-rnorm(n,0,1)

        # Function1 #

        pl1<-function(nitem,n,a,minb,maxb,theta){


          b<- runif( nitem, minb,maxb)

          thetaMat <- matrix( rep( theta, length( b ) ), ncol = length( b) )


          thetaMat1<- t (apply( thetaMat , 1, '-', b) )


          thetaMat2 <- t( apply( thetaMat1, 1,'*',a) )


          proB<- 1 / ( 1 + exp(-thetaMat2) )


          pteta <- matrix( proB, ncol = nitem)


          answerMat <- matrix(sapply ( pteta, rbinom, n = 1, size = 1 ),
                              ncol = length(b) )

          answerMat2<-rbind(b,a,answerMat)

          orDer<-paste0("Respondent_",1:n)

          row.names(answerMat2)<- c("b","a",orDer)

          colnames(answerMat2)<-paste0("item",1:nitem)

          res<-as.data.frame(round(answerMat2,3))

          return(res)

        }

        emptyList<-list()

        withProgress(message =
                       paste0("DATA GENERATION IS IN PROGRESS"),
                     detail = "Daraframe format is being generated",
                     style = "notification",
                     value = 0.1,
                     {
                       Sys.sleep(0.25)

                       for ( i in 1:input$rep)  {


                         incProgress(1/input$rep)

                         emptyList[[i]]<- pl1(nitem,n,a,minb,maxb,theta)
                       } })

        SIMDATA_ENV$simDataDichotom <- emptyList


        return(emptyList[[1]][1:10,])

      }

    } )

    ## 2PL ##

    output$tab2<-DT::renderDataTable(rownames = TRUE ,{

      req( input$start==1)

      if(input$type1==2)  {

        nitem<-input$num1
        n<- input$num2

        mina <- min(input$num4)
        maxa <-max(input$num4)

        minb<- min(input$num3)
        maxb<- max(input$num3)

        theta<-rnorm(n,0,1)

        # Function 2 #

        pl2<-function(nitem, n, mina, maxa, minb, maxb, theta){

          b<- runif(nitem, minb, maxb)

          a<- runif(nitem, mina, maxa)

          thetaMat <- matrix( rep( theta, length( b ) ), ncol = length( b))

          thetaMat1<- t (apply( thetaMat , 1, '-', b) )

          thetaMat2 <- t( apply( thetaMat1, 1,'*',a) )

          proB<- 1 / ( 1 + exp(-thetaMat2) )

          pteta <- matrix( proB, ncol = nitem)

          answerMat <- matrix(sapply ( pteta, rbinom, n = 1, size = 1 ),
                              ncol = length(b) )

          answerMat2<-rbind(b,a,answerMat)

          orDer<-paste0("Respondent_",1:n)

          row.names(answerMat2)<- c("b","a",orDer)

          colnames(answerMat2)<-paste0("item",1:nitem)

          res<-as.data.frame(round(answerMat2,3))

          return(res)

        }

        emptyList<-list()

        withProgress(message =
                       paste0("DATA GENERATION IS IN PROGRESS"),
                     detail = "Daraframe format is being generated",
                     style = "notification",
                     value = 0.1,
                     {
                       Sys.sleep(0.25)

                       for ( i in 1:input$rep)  {


                         incProgress(1/input$rep)
                         emptyList[[i]]<- pl2(nitem, n, mina, maxa, minb, maxb, theta)

                       } })

        SIMDATA_ENV$simDataDichotom <- emptyList

        return(emptyList[[1]][1:10,])

      }

    } )

    ## 3 PL ##

    output$tab3<-DT::renderDataTable(rownames = TRUE ,{

      req( input$start==1)

      if(input$type1==3)  {


        nitem<-input$num1

        n<- input$num2

        mina <- min(input$num4)
        maxa <-max(input$num4)

        minb<- min(input$num3)
        maxb<- max(input$num3)

        minc<-min(input$num5)
        maxc<-max(input$num5)

        theta<-rnorm(n,0,1)

        # Function 3 #

        pl3<-function(nitem, n, mina, maxa, minb, maxb, minc, maxc, theta){

          b<- runif(nitem, minb, maxb)

          a<- runif(nitem, mina, maxa)

          c<-runif( nitem, minc, maxc)

          thetaMat <- matrix( rep( theta, length( b ) ), ncol = length( b) )


          thetaMat1<- t (apply( thetaMat , 1, '-', b) )


          thetaMat2 <- t( apply( thetaMat1, 1,'*',a) )


          proB<- 1 / ( 1 + exp(-thetaMat2) )

          ols1<-t(apply(proB,1,'*',(1-c)))

          ols2<-t(apply(ols1,1,'+',c))


          pteta <- matrix( ols2, ncol = nitem)


          answerMat <- matrix(sapply ( pteta, rbinom, n = 1, size = 1 ),
                              ncol = length(b) )


          answerMat2<-rbind(b,a,c,answerMat)

          orDer<-paste0("Respondent_",1:n)

          row.names(answerMat2)<- c("b","a","c",orDer)

          colnames(answerMat2)<-paste0("item",1:nitem)

          res<-as.data.frame(round(answerMat2, 3))

          return(res)

        }

        emptyList<-list()

        withProgress(message =
                       paste0("DATA GENERATION IS IN PROGRESS"),
                     detail = "Daraframe format is being generated",
                     style = "notification",
                     value = 0.1,
                     {
                       Sys.sleep(0.25)

                       for ( i in 1:input$rep)  {

                         incProgress(1/input$rep)

                         emptyList[[i]]<- pl3(nitem, n, mina, maxa, minb,
                                              maxb, minc, maxc, theta)

                       }  })

        SIMDATA_ENV$simDataDichotom <- emptyList

        return(emptyList[[1]][1:10,])

      }

    } )


    ## TEXTS ##

    output$text000<- renderText({

      req(input$start3==2)

      paste0("AFTER DETERMINING THE REQUIRED VALUES FOR DATA GENERATION,
          SELECT 'START GENERATE DATA OPTION'")
    })


    output$text00<- renderText({

      req(input$start2==2)

      paste0("AFTER DETERMINING THE REQUIRED VALUES FOR DATA GENERATION,
          SELECT 'START GENERATE DATA OPTION'")
    })

    output$text0<- renderText({

      req(input$start==2)

      paste0("AFTER DETERMINING THE REQUIRED VALUES FOR DATA GENERATION,
          SELECT 'YES, START GENERATE DATA' OPTION")
    })

    output$text1<-renderText( {

      req( input$start==1)

      return(
        paste(input$rep, "DATA SETS WERE GENERATED,EACH CONSISTING OF",
              input$num2, "RESPONDENTS  AND", input$num1, " ITEMS")
      )
    })

    output$text2<-renderText( {

      if (isTRUE(input$irt) && input$irt==1)

        return(

          "ITEM PARAMETERS WERE ESTIMATED FOR THE GENERATED DATA SETS.
        RMSE AND BIAS VALUES FOR ACTUAL AND ESTIMATED
        PRAMETERS WERE PRESENTED BELOW"
        )
    })

    ## DOWLOAD DATA SETS ##

    # DICHO

    output$down1 <- downloadHandler(

      filename = function() {
        paste("data-", input$type1,"-","PL",  "-", Sys.time(), ".xlsx", sep="")
      },
      content = function(file) {

        withProgress(message =
                       paste0("DOWNLODING DATASETS IS IN PROGRESS"),
                     detail = "This may take time according to number of replications.",
                     style = "notification",
                     value = 0.1,
                     { Sys.sleep(0.25)

                       for (i in 1:input$rep){

                         incProgress(1/input$rep)

                         write.xlsx(SIMDATA_ENV$simDataDichotom[i], file=file, sheetName=paste(i), append=T)
                       }   } )
      }
    )

    # POLY

    output$down2 <- downloadHandler(
      filename = function() {
        if( input$type2==1) {   tip<- "PCM"}
        if( input$type2==2) {   tip<- "RSM"}
        if( input$type2==3) {   tip<- "GPCM"}
        if( input$type2==4) {   tip<- "GRM"}
        paste("data-", tip,  "-", Sys.time(), ".xlsx", sep="")},
      content = function(file){
        if( input$type2==1) {   liste<- SIMDATA_ENV$simDataPCM}
        if( input$type2==2) {   liste<- SIMDATA_ENV$simDataRSM}
        if( input$type2==3) {   liste<-SIMDATA_ENV$simDataGPCM }
        if( input$type2==4) {   liste<-SIMDATA_ENV$simDataGRM}

        withProgress(message =
                       paste0("DOWNLODING DATASETS IS IN PROGRESS"),
                     detail = "This may take time according to number of replications.",
                     style = "notification",
                     value = 0.1,
                     { Sys.sleep(0.25)

                       for (i in 1:input$polyrep){

                         incProgress(1/input$polyrep)

                         write.xlsx(liste[i], file=file, sheetName=paste(i), append=T)
                       } })
      }

    )

    ########## FACTOR DOWNLOAD #############

    output$down3 <- downloadHandler(


      filename = function() {

        if (input$ftype1==FALSE) {name<- "Polythmous" }

        if(input$ftype1==TRUE)  {name<- "Dichotmous"}

        if(input$ftype1==3) {name<- "Continious"}

        paste("data_", name, "-", Sys.time(), ".xlsx", sep="")
      },

      content = function(file) {

        if (input$ftype1==FALSE) {datA<- SIMDATA_ENV$SimMultiDataPoly1 }

        if(input$ftype1==TRUE)  {datA<- SIMDATA_ENV$SimMultiDataDicho1}

        if(input$ftype1==3) {datA<- SIMDATA_ENV$SimMultiDataCont1}

        withProgress(message =
                       paste0("DOWNLODING DATASETS IS IN PROGRESS"),
                     detail = "This may take time according to number of replications.",
                     style = "notification",
                     value = 0.1,
                     {
                       Sys.sleep(0.25)
                       for (i in 1:input$frep){
                         incProgress(1/input$frep)
                         write.xlsx( datA[i], file=file, sheetName=paste(i), append=T)
                       }})
      }
    )

    #######################


    ## RMSE - BIAS ##

    # FOR 1PL #


    output$fit1<- render_gt( align = "center",{


      req( input$start==1)

      if ( input$irt==1)  {

        if(input$type1==1)  {

          emptyListG <- SIMDATA_ENV$simDataDichotom

          emptyListK<-list()



          for ( i in 1:input$rep) {


            emptyListK[[i]]<- mirt::mirt(data = emptyListG[[i]][3:input$num2,],
                                         model = 1 , SE = FALSE , itemtype = "Rasch")
          }

          emptyListP<-list()

          for ( i in 1:input$rep) {

            emptyListP[[i]]<- mirt::coef( emptyListK[[i]],
                                          IRTpars = TRUE, simplify = TRUE)$items
          }

          SIMDATA_ENV$simParamDichotom <-emptyListP

          #1 PL BIAS RMSEA #


          #### biasb- rmseb ####

          biasb<-NULL


          withProgress(message =
                         paste0("COMPUTING BIAS FOR b VALUE"),
                       detail = "This may take time according to number of replications.",
                       style = "notification",
                       value = 0.1,
                       {

                         for (i in 1:input$rep) {

                           incProgress(1/input$rep)

                           biasb[i]<- Metrics::bias(as.numeric(emptyListG[[i]][1,]),
                                                    as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,2] ))  }

                       })

          biasb1<-biasb

          biasb1<-mean(biasb1)

          rmseb <-NULL

          withProgress(message =
                         paste0("COMPUTING RMSE FOR b VALUE"),
                       detail = "This may take time according to number of replications.",
                       style = "notification",
                       value = 0.1,
                       {

                         for ( i in 1:input$rep) {

                           incProgress(1/input$rep)

                           rmseb[i]<- Metrics::rmse( as.numeric( emptyListG[[i]][1,] ),
                                                     as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,2]) )

                         }  })

          rmseb1<-rmseb

          rmseb1<-mean(rmseb1)

          # biasa, rmsea #


          biasa<-NULL

          withProgress(message =
                         paste0("COMPUTING BIAS FOR a VALUE"),
                       detail = "This may take time according to number of replications.",
                       style = "notification",
                       value = 0.1,
                       {

                         for ( i in 1:input$rep) {

                           incProgress(1/input$rep)


                           biasa[i]<- Metrics::bias(  as.numeric(emptyListG[[i]][2,]),
                                                      as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,1]) )
                         }  })

          biasa1<-biasa

          biasa1<-mean(biasa1)

          rmsea<-NULL

          withProgress(message =
                         paste0("COMPUTING RMSE FOR a VALUE"),
                       detail = "This may take time according to number of replications.",
                       style = "notification",
                       value = 0.1,
                       {

                         for ( i in 1:input$rep ) {

                           incProgress(1/input$rep)
                           rmsea[i]<- Metrics::rmse( as.numeric(emptyListG[[i]][2,]),
                                                     as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,1]  ) )

                         }

                       } )

          rmsea1<-rmsea

          rmsea1<-mean(rmsea1)

          SIMDATA_ENV$rmseaBias <- data.frame( biasb=biasb1, rmseb=rmseb1,
                                               biasa=biasa1,rmsea= rmsea1)

          res<-gt::gt(SIMDATA_ENV$rmseaBias)

          res<-res  %>%

            tab_header(

              title= md("RSME AND BIAS VALUES") )

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
              column_labels.font.size=gt::px(20),
              column_labels.font.weight="bold"
            )

          res <- res %>%

            tab_options(
              heading.title.font.size = gt::px(25))

          return(res)

        }}  # close ifelse

    })

    # FOR 2PL #


    output$fit2<- render_gt( align = "center",{

      req( input$start==1)

      if ( input$irt==1)  {

        if(input$type1==2)  {


          emptyListG <- SIMDATA_ENV$simDataDichotom

          emptyListK<-list()


          for ( i in 1:input$rep) {

            emptyListK[[i]]<- mirt::mirt(data = emptyListG[[i]][3:input$num2,],
                                         model = 1 , SE = FALSE , itemtype = "2PL")
          }


          emptyListP<-list()

          for ( i in 1:input$rep) {

            emptyListP[[i]]<- mirt::coef( emptyListK[[i]], IRTpars = TRUE,
                                          simplify = TRUE)$items

          }

          SIMDATA_ENV$simParamDichotom <-emptyListP

          #2 PL BIAS RMSEA #


          # biasb- rmseb #

          biasb<-NULL


          for ( i in 1:input$rep) {

            biasb[i]<-Metrics:: bias(as.numeric(emptyListG[[i]][1,]),
                                     as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,2] ))  }

          biasb1<-biasb

          biasb1<-mean(biasb1)


          rmseb <-NULL

          for ( i in 1:input$rep) {

            rmseb[i]<-Metrics::rmse( as.numeric( emptyListG[[i]][1,] ),
                                     as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,2]) )

          }

          rmseb1<-rmseb

          rmseb1<-mean(rmseb1)

          # biasa rmsea #

          biasa<-NULL

          for ( i in 1:input$rep) {

            biasa[i]<-  Metrics::bias(  as.numeric(emptyListG[[i]][2,]),
                                        as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,1]) )

          }

          biasa1<-biasa

          biasa1<-mean(biasa1)

          rmsea<-NULL

          for ( i in 1:input$rep ) {


            rmsea[i]<-Metrics::rmse( as.numeric(emptyListG[[i]][2,]),
                                     as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,1]  ) )
          }

          rmsea1<-rmsea

          rmsea1<-mean(rmsea1)

          SIMDATA_ENV$rmseaBias <- data.frame( biasb=biasb1,
                                               rmseb=rmseb1,biasa=biasa1,rmsea= rmsea1)

          res<-gt::gt(SIMDATA_ENV$rmseaBias)

          res<-res  %>%

            tab_header(

              title= md("RSME AND BIAS VALUES") )

          res<-res %>%

            tab_style(
              style = cell_fill(color = sample(colors()[3:100],1),alpha=0.15),

              locations = cells_body()
            )

          res<- res %>%

            cols_width(
              everything() ~ gt::px(180))

          res <- res %>%

            tab_options(
              column_labels.font.size=gt::px(20),
              column_labels.font.weight="bold"
            )

          res <- res %>%

            tab_options(
              heading.title.font.size = gt::px(25))

          return(res)

        }}  # close ifelse

    })

    ## FOR  3PL ##

    output$fit3<- render_gt( align = "center",{

      req( input$start==1)

      if ( input$irt==1)  {


        if(input$type1==3)  {

          emptyListG <- SIMDATA_ENV$simDataDichotom

          emptyListK<-list()

          for ( i in 1:input$rep) {

            emptyListK[[i]]<- mirt::mirt(data = emptyListG[[i]][4:input$num2+3,],
                                         model = 1 , SE = FALSE , itemtype = "3PL")
          }


          emptyListP<-list()

          for ( i in 1:input$rep) {

            emptyListP[[i]]<- mirt::coef( emptyListK[[i]], IRTpars = TRUE,
                                          simplify = TRUE)$items

          }

          SIMDATA_ENV$simParamDichotom <- emptyListP

          # 3 PL RMSE BIAS #


          # biasb- rmseb #

          biasb<-NULL


          for ( i in 1:input$rep) {

            biasb[i]<-Metrics::bias(as.numeric(emptyListG[[i]][1,]),
                                    as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,2] ))  }

          biasb1<-biasb

          biasb1<-mean(biasb1)


          rmseb <-NULL

          for ( i in 1:input$rep) {

            rmseb[i]<- Metrics::rmse( as.numeric( emptyListG[[i]][1,] ),
                                      as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,2]) )

          }

          rmseb1<-rmseb

          rmseb1<-mean(rmseb1)

          # biasa, rmsea #

          biasa<-NULL

          for ( i in 1:input$rep) {

            biasa[i]<-Metrics::bias(  as.numeric(emptyListG[[i]][2,]),
                                      as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,1]) )


          }

          biasa1<-biasa

          biasa1<-mean(biasa1)

          rmsea<-NULL

          for ( i in 1:input$rep ) {

            rmsea[i]<-Metrics::rmse( as.numeric(emptyListG[[i]][2,]),
                                     as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,1]  ) )

          }

          rmsea1<-rmsea

          rmsea1<-mean(rmsea1)

          # biasc, rmsec #

          biasc<-NULL

          for ( i in 1:input$rep) {

            biasc[i]<-  Metrics::bias(as.numeric(emptyListG[[i]][3,]),
                                      as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,3]) )


          }

          biasc1<-biasc

          biasc1<-mean(biasc1)


          rmsec<-NULL

          for ( i in 1:input$rep ) {


            rmsec[i]<- Metrics::rmse( as.numeric(emptyListG[[i]][3,]),
                                      as.numeric(SIMDATA_ENV$simParamDichotom[[i]][,3]  ) )
          }

          rmsec1<-rmsec

          rmsec1<-mean(rmsec1)

          SIMDATA_ENV$rmseaBias <- data.frame( biasb=biasb1, rmseb=rmseb1,
                                               biasa=biasa1,rmsea= rmsea1,biasc=biasc1,rmsec=rmsec1)

          res<-gt::gt(SIMDATA_ENV$rmseaBias)

          res<-res  %>%

            tab_header(

              title= md("RSME AND BIAS VALUES") )

          res<-res %>%

            tab_style(
              style = cell_fill(color = sample(colors()[3:100],1),alpha=0.15),

              locations = cells_body()
            )

          res<- res %>%

            cols_width(
              everything() ~ gt::px(180))

          res <- res %>%

            tab_options(
              column_labels.font.size=gt::px(20),
              column_labels.font.weight="bold"
            )

          res <- res %>%

            tab_options(
              heading.title.font.size = gt::px(25))

          return(res)

        }}  # close ifelse
    })

    ## POLYTOMOUS DATA GENERATION  ##

    ## PCM ##

    output$tab_pcm<- DT::renderDT( {

      req( input$start2==1)


      if (input$type2==1)  {

        PCM_B<-list()

        PCM_DAT1<-list()

        withProgress(message =
                       paste0("DATA GENERATION IS IN PROGRESS"),
                     detail = "Dataframe format is being generated",
                     style = "notification",
                     value = 0.1,
                     {
                       Sys.sleep(0.25)

                       for ( i in 1:input$polyrep)   {

                         incProgress(1/input$polyrep)

                         PCM_B[[i]]<-    genPolyMatrix(items = input$nitem,
                                                       nrCat = as.numeric(input$sec),
                                                       model = "PCM",seed = i, same.nrCat = TRUE, cbControl = NULL)

                         PCM_B1<- as.matrix( PCM_B  )

                         PCM_DAT1[[i]]<-genPattern(rnorm(input$nn), PCM_B1[[i]], model = "PCM")

                         SIMDATA_ENV$simDataPCM <- PCM_DAT1

                         colnames(PCM_DAT1[[i]])<-paste0("item",1:input$nitem)

                       } })


        # PCM_DAT1<-as.data.frame(PCM_DAT1)

        return(PCM_DAT1[[1]][1:10,1:10])
      }

    })


    ## RSM ##

    output$tab_rsm<-DT::renderDT({

      req( input$start2==1)

      if (input$type2==2)  {

        RSM_B<-list()

        RSM_DAT1<-list()


        withProgress(message =
                       paste0("DATA GENERATION IS IN PROGRESS"),
                     detail = "Dataframe format is being generated",
                     style = "notification",
                     value = 0.1,
                     {
                       Sys.sleep(0.25)


                       for ( i in 1:input$polyrep)   {

                         incProgress(1/input$polyrep)

                         RSM_B[[i]]<- genPolyMatrix(items = input$nitem,
                                                    nrCat = as.numeric(input$sec),
                                                    model = "RSM",seed = i, same.nrCat = TRUE, cbControl = NULL)

                         RSM_B1<-  as.matrix (RSM_B)

                         RSM_DAT1[[i]]<-  genPattern(rnorm(input$nn), RSM_B1[[i]], model = "RSM")

                         SIMDATA_ENV$simDataRSM <- RSM_DAT1

                         colnames(RSM_DAT1[[i]])<-paste0("item",1:input$nitem)

                       } })

        return(RSM_DAT1[[1]][1:10,1:10])
      }
    })


    ## GPCM ##

    output$tab_gpcm<-DT::renderDT( {

      req( input$start2==1)

      if (input$type2==3)  {

        GPCM_B<-list()

        GPCM_DAT1<-list()


        withProgress(message =
                       paste0("DATA GENERATION IS IN PROGRESS"),
                     detail = "Dataframe format is being generated",
                     style = "notification",
                     value = 0.1,
                     {
                       Sys.sleep(0.25)

                       for ( i in 1:input$polyrep)   {

                         incProgress(1/input$polyrep)

                         GPCM_B[[i]]<- genPolyMatrix(items = input$nitem,
                                                     nrCat = as.numeric(input$sec),
                                                     model = "GPCM",seed = i, same.nrCat = TRUE,cbControl = NULL)

                         GPCM_B1<-   as.matrix (GPCM_B )

                         GPCM_DAT1[[i]]<-genPattern(rnorm(input$nn),GPCM_B1[[i]], model = "GPCM")

                         SIMDATA_ENV$simDataGPCM <- GPCM_DAT1

                         colnames(GPCM_DAT1[[i]])<-paste0("item",1:input$nitem)

                       } })

        # GPCM_DAT1<-as.data.frame(GPCM_DAT1)


        return(GPCM_DAT1[[1]][1:10,1:10])

      }
    })

    ## GRM ##

    output$tab_grm<- DT::renderDT({

      req( input$start2==1)

      if (input$type2==4)  {

        GRM_B<-list()

        GRM_DAT1<-list()

        withProgress(message =
                       paste0("DATA GENERATION IS IN PROGRESS"),
                     detail = "Dataframe format is being generated",
                     style = "notification",
                     value = 0.1,
                     {
                       Sys.sleep(0.25)

                       for ( i in 1:input$polyrep)   {

                         incProgress(1/input$polyrep)

                         GRM_B[[i]]<- genPolyMatrix(items = input$nitem,
                                                    nrCat = as.numeric(input$sec),
                                                    model = "GRM",seed = i, same.nrCat = TRUE, cbControl = NULL)

                         GRM_B1<- as.matrix (GRM_B )

                         GRM_DAT1[[i]]<-genPattern(rnorm(input$nn), GRM_B1[[i]], model = "GRM")

                         SIMDATA_ENV$simDataGRM <- GRM_DAT1


                       } } )

        # GRM_DAT1<-as.data.frame(GRM_DAT1)

        return(GRM_DAT1[[1]][1:10,1:10])

      }
    })


    output$text3<-renderText( {

      req( input$start2==1)

      if( input$type2==1) {mod<- "BASED ON PARTIAL CREDIT MODEL(PCM)"}

      if( input$type2==2) {mod<- "BASED ON RATING SCALE MODEL (RSM)"}

      if( input$type2==3) {mod<- "BASED ON GENERALIZED
    PARTIAL CREDIT MODEL (GPCM)"}

      if( input$type2==4) {mod<- "BASED ON GRADED RESPONSE MODEL (GRM)"}

      return(

        paste(  input$polyrep, "DATA SETS WERE GENERATED", mod,

                "EACH CONSISTING OF", input$fnitem,
                "ITEMS AND", input$nn, "RESPONDENTS" )   )
    })


    output$text4<-renderText( {

      req( input$start3==1)

      if( input$ftype1==FALSE) {mod<- "BASED ON PARTIAL CREDIT MODEL(PCM)"}

      paste ( input$frep, "DATA SETS WITH", input$nfac, "DIMENSIONS WERE GENERATED.",

              " EACH DATA SET CONSISTS OF", input$fnitem, "ITEMS AND", input$fnn,

              "PARTICIPANTS.","AVERAGE FACTOR LOADING IS", input$meanload )
    })


    ## MULTIDIMENSIONAL DATA GENERATE ##

    ## Polythmous Data ##

    output$tab_fac_Poly<- DT::renderDT({

      req( input$start3==1)


      req( input$ftype1==FALSE)

      factorPoly<-function(nitem, n, nfac, factorloading, lowfactoritem=NULL) {
        dat<-as.matrix(sim.VSS(ncases=n, nvariables=nitem, nfactors=nfac,
                               meanloading=factorloading, dichot=FALSE,cut=0))
        kategorik<-function(data=x,madsay=nitem) {
          catdata<-matrix(NA,n,madsay)
          for ( i in 1:madsay) {
            catdata[,i]<-as.factor(cut(data[,i],
                                       breaks=c(-100,-1.2815516,-04537622,0.4537622,1.2815516,100),
                                       labels=c("1","2","3","4","5"))) }
          catdata1<-as.data.frame(catdata)
          colnames(data)=c(paste("i",1:madsay))
          return(catdata1)
          print(summary(catdata1))
        }
        data1<-kategorik(dat,nitem)
        num<-1:nitem

        SIMDATA_ENV$low <- sample(num,lowfactoritem)

        for ( i in SIMDATA_ENV$low){
          data1[,i]<-data1[,i]<-sample(c(1,2,3,4,5),n, replace = TRUE) }
        data1r<-data1

        items1<-colnames(data1r)

        data1r<-as.data.frame(data1r)

        return(data1r)
      }



      seed<-(1:1000000)

      SIMDATA_ENV$seedPoly <-sample(seed,input$frep)

      listGL1<-list()

      withProgress(message =
                     paste0("DATA GENERATION IS IN PROGRESS"),
                   detail = "Dataframe format is being generated",
                   style = "notification",
                   value = 0.1,
                   {
                     Sys.sleep(0.25)
                     for (j in c(SIMDATA_ENV$seedPoly)) {
                       incProgress(1/length(SIMDATA_ENV$seedPoly))
                       set.seed(j)

                       for ( i in 1:input$frep) {

                         listGL1[[i]]<-factorPoly(nitem = input$fnitem, n=input$fnn, nfac = input$nfac,

                                                  factorloading = input$meanload, lowfactoritem = input$nlfl) }
                     }})



      listGL2<-list()
      withProgress(message =
                     paste0("DATA GENERATION IS IN PROGRESS"),
                   detail = "HTML format is being generated",
                   style = "notification",
                   value = 0.1,
                   {
                     Sys.sleep(0.25)
                     for (j in c(SIMDATA_ENV$seedPoly)) {
                       incProgress(1/length(SIMDATA_ENV$seedPoly))
                       set.seed(j)


                       for ( i in 1:input$frep) {


                         listGL2[[i]]<- datatable(  factorPoly(nitem = input$fnitem, n=input$fnn, nfac = input$nfac,
                                                               factorloading = input$meanload, lowfactoritem = input$nlfl)) %>% formatStyle( SIMDATA_ENV$low, backgroundColor = "pink")
                       }
                     }})

      SIMDATA_ENV$SimMultiDataPoly1 <-listGL1

      return(listGL2[[1]])


    })

    ## Dichothmous Data  ##

    output$tab_fac_Dicho<- DT::renderDT({

      req( input$start3==1)

      req( input$ftype1==TRUE)

      factorDicho<-function( nitem, n, nfac, factorloading, lowfactoritem=NULL) {

        dat<-as.matrix(sim.VSS(ncases=n, nvariables=nitem, nfactors=nfac,
                               meanloading=factorloading, dichot=TRUE,cut=0))

        num<-1:nitem

        SIMDATA_ENV$lowD <- sample(num,lowfactoritem)

        data1<-dat

        for ( i in SIMDATA_ENV$lowD ){
          data1[,i]<-data1[,i]<-sample(c(0,1),n, replace = TRUE) }

        data1r<-as.data.frame(data1)

        return(data1r)
      }


      seed<-(1:1000000)

      SIMDATA_ENV$seedDicho <-sample(seed,input$frep)

      listGL1<-list()

      withProgress(message =
                     paste0("DATA GENERATION IS IN PROGRESS"),
                   detail = "Dataframe format is being generated",
                   style = "notification",
                   value = 0.1,
                   {
                     Sys.sleep(0.25)


                     for (j in c(SIMDATA_ENV$seedDicho)) {

                       incProgress(1/length(SIMDATA_ENV$seedDicho))

                       set.seed(j)

                       for ( i in 1:input$frep) {

                         listGL1[[i]]<- factorDicho(nitem = input$fnitem, n=input$fnn, nfac = input$nfac,

                                                    factorloading = input$meanload, lowfactoritem = input$nlfl) }
                     } })



      listGL2<-list()


      withProgress(message =
                     paste0("DATA GENERATION IS IN PROGRESS"),
                   detail = "HTML format is being generated",
                   style = "notification",
                   value = 0.1,
                   {
                     Sys.sleep(0.25)

                     for (j in c(SIMDATA_ENV$seedDicho)) {

                       set.seed(j)


                       for ( i in 1:input$frep) {

                         incProgress(1/length(SIMDATA_ENV$seedDicho))


                         listGL2[[i]]<- datatable( factorDicho(nitem = input$fnitem, n=input$fnn, nfac = input$nfac,
                                                               factorloading = input$meanload, lowfactoritem = input$nlfl)) %>% formatStyle( SIMDATA_ENV$lowD , backgroundColor = "pink")
                       }
                     } })

      SIMDATA_ENV$SimMultiDataDicho1 <-listGL1

      return(listGL2[[1]])

    })

    ## Continious Data ##

    output$tab_fac_Cont<- DT::renderDT({

      req( input$start3==1)

      req( input$ftype1==3)

      factorDicho<-function( nitem, n, nfac, factorloading, lowfactoritem=NULL) {

        dat<-as.matrix(sim.VSS(ncases=n, nvariables=nitem, nfactors=nfac,
                               meanloading=factorloading, dichot=FALSE,cut=0))

        num<-1:nitem

        SIMDATA_ENV$lowC <- sample(num,lowfactoritem)

        data1<-dat

        for ( i in SIMDATA_ENV$lowC){
          data1[,i]<-data1[,i]<-rnorm(n,0,1)}

        data1r<-as.data.frame(data1)

        return(data1r)
      }

      seed<-(1:1000000)

      SIMDATA_ENV$seedCont <-sample(seed,input$frep)

      listGL1<-list()

      withProgress(message =
                     paste0("DATA GENERATION IS IN PROGRESS"),
                   detail = "Dataframe format is being generated",
                   style = "notification",
                   value = 0.1,
                   {
                     Sys.sleep(0.25)

                     for (j in c(SIMDATA_ENV$seedCont)) {

                       set.seed(j)

                       for ( i in 1:input$frep) {

                         incProgress(1/length(SIMDATA_ENV$seedCont))

                         listGL1[[i]]<- factorDicho(nitem = input$fnitem, n=input$fnn, nfac = input$nfac,

                                                    factorloading = input$meanload, lowfactoritem = input$nlfl) }
                     } })


      listGL2<-list()

      withProgress(message =
                     paste0("DATA GENERATION IS IN PROGRESS"),
                   detail = "HTML format is being generated",
                   style = "notification",
                   value = 0.1,
                   {
                     Sys.sleep(0.25)


                     for (j in c(SIMDATA_ENV$seedCont)) {

                       set.seed(j)


                       for ( i in 1:input$frep) {


                         incProgress(1/length(SIMDATA_ENV$seedCont))

                         listGL2[[i]]<- datatable(round(factorDicho(nitem = input$fnitem, n=input$fnn, nfac = input$nfac,
                                                                    factorloading = input$meanload,
                                                                    lowfactoritem = input$nlfl), 4)) %>% formatStyle( SIMDATA_ENV$lowC, backgroundColor = "pink")
                       }
                     } })

      SIMDATA_ENV$SimMultiDataCont1 <-listGL1
      #SIMDATA_ENV$SimMultiDataCont2 <-listGL2

      return(listGL2[[1]])

    })


    output$tab_fac_all <- renderUI({

      if(input$ftype1 == FALSE){

        withLoader(  DT::DTOutput("tab_fac_Poly"), type = "html", loader = "loader1")

      } else if(input$ftype1 == TRUE){

        withLoader(  DT::DTOutput("tab_fac_Dicho"), type = "html", loader = "loader1")

      } else {
        withLoader(  DT::DTOutput("tab_fac_Cont"),type = "html", loader = "loader1")
      }

    })

    session$onSessionEnded(function() {
      stopApp()
    })
    factorPoly <- x <- NULL
  }  # close server

  shinyApp(ui = ui, server = server)
  }



