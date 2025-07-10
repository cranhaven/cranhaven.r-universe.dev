#' Run exploratory factor analysis for dichotomous and polytomous data
#' @import foreign
#' @import rJava
#' @import scales
#' @importFrom stats cor
#' @importFrom hornpa hornpa
#' @importFrom utils read.csv2 write.csv2
#' @importFrom utils globalVariables
#' @importFrom psych cortest.bartlett KMO tetrachoric principal
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{FA()}
#' @export

INTERNAL <- function(){
  REL<- new.env()

  ### FUNCTIONS ###

  hoyt <- function( data)

  {
    N <-nrow(data)*ncol(data)
    n <-nrow(data)
    i <-ncol(data)
    Total_SS <-sum(data^2) - sum(data)^2/N
    person_SS <-sum(rowSums(data)^2)/i -sum(data)^2/N
    item_SS <- sum(colSums(data)^2)/n -sum(data)^2/N
    remainder_SS <- Total_SS -(person_SS + item_SS)

    person_MS = person_SS/(n-1)
    remainder_MS = remainder_SS/(n-1)/(i-1)
    hoyt= (person_MS - remainder_MS)/ person_MS

    result <- data.frame( Nitem=i,N=n, Hoyt=round(hoyt,3) )
    return(result)
  }

  REL$hoyt<-hoyt

  ####

  iki.yari<-function(data,yontem="tekcift") {

    tekcift<-function(data) {
      uzunluk<-ncol(data)
      colnames(data)<-1:uzunluk
      bir<-data[,seq(1,uzunluk,2)]
      iki<-data[,seq(2,uzunluk,2)]
      list(bir,iki)
    }
    rastgele<-function(data) {
      n<-ncol(data)
      colnames(data)<- paste0("soru",1:n)
      nn<-n/2
      x1<-colnames(data)

      ifelse(n%%2 == 0, s1<-sample(x1,nn),
             s1<-sample(x1,(n+1)/2))

      s2<-which(!x1  %in%  s1)
      bir<- data[, s1]
      iki<- data[, s2]
      list(bir,iki)
    }
    if(yontem=="tekcift"){ yari<-tekcift(data)}
    if(yontem=="seckisiz") { yari<-rastgele(data)}
    sum1<-rowSums(yari[[1]])
    sum2<-rowSums(yari[[2]])
    kor<-cor(sum1,sum2)
    sonuc<- (2*kor) / (1 + kor)
    sonuc<-data.frame(rsb=sonuc)
    return(sonuc) }

  REL$iki.yari<-iki.yari

  ###

  KR20 <- function(data){
    i <- ncol(data)
    n <- nrow(data)
    # get the item difficulties
    colM <- colMeans(data)
    # Get total scores
    total <- rowSums(data)
    # observed score variance
    var.total <- var(total)*(n-1)/n
    #  KR-20
    KR20 <- (i/(i-1)) * (1-sum(colM*(1-colM)) / var.total)
    result <- data.frame( I=i, KR20=round(KR20,3) )
    return(result)
  }



  KR21 <- function(data){
    i <- ncol(data)
    n <- nrow(data)
    # Get the average of item difficulties
    mean.p <- mean(rowSums(data))/i
    # Observed score variance
    var.total <- var(rowSums(data))*(n-1)/n
    # KR-21
    KR21 <- (i/(i-1))*(1-(i*mean.p*(1-mean.p))/var.total)
    result <- data.frame( I=i, KR21=round(KR21,3) )
    return(result)
  }

  REL$KR21<-KR21

  REL$KR20<-KR20

  ####

  omega<- function(data, nfactor){
    suppressWarnings({
      i <- ncol(data)
      omega.g <- suppressMessages ( psych ::omega(data,nfactor,ply=T,rotate="oblimin",fm="minres", digits = 3,sl=T)
      )
    })
    result <- data.frame( I=i, omega=round(omega.g$omega.tot,nfactor) )

    return(result)}


  REL$omega<-omega

  ###

  str.alpha2 <- function( data)
  {
    alpha <- function(data)
    {
      # covariance
      cov <- stats::cov( data, use="pairwise.complete.obs" )
      # mean covariance
      I <- ncol(data)
      mean_cov <- sum(cov[row(cov)!=col(cov)]) / ( I^2 - I )
      # mean and variance
      mean_var <- mean( diag(cov) )
      alpha <- I*mean_cov / ( mean_var + (I-1)*mean_cov )
      var.tot <- stats::var( rowSums(data), na.rm=TRUE )
      result <- data.frame( Nitem=I, Alpha=alpha, Var.Tot=var.tot )
      return(result)
    }
    subtest <- cbind( colnames(data), substring( colnames(data), 1,1 ) )
    res0 <- data.frame( Scale="total", alpha(data) )
    for (s in sort(unique( subtest[,2] ))){
      data_s <- data[, subtest[ subtest[,2]==s, 1] ]
      res1 <- data.frame(Scale=paste0("factor",s),alpha(data=data_s) )
      res0 <- rbind( res0, res1 )
    }
    ######tabakalı alfanın hesaplanması
    res0$Stratified.Alpha <- NA
    res0$Stratified.Alpha[1] <- 1 - sum( (1 - res0[-1, 3] ) * res0[-1,4]) / res0[1,4 ]
    print(res0)
  }


  REL$str.alpha2<-str.alpha2



  ui<- fluidPage(


    useShinyjs(),
    theme = shinytheme("readable"),

    tags$head(tags$style(
      type="text/css",
      "# img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),


    tags$head(tags$style(
      type="text/css",
      "#image2 img {max-width: 100%; width: auto; height: 100%; align: center}"
    )),



    uiOutput("cols"), ### RENK DEĞİŞİMİ İÇİN WİDGET




    ###################################################################################

    tags$style(HTML("#a{color:black; font-family:Lucida Arial ;font-size: 16px;
              font-style: oblique;text-align:center}")), #tabs#

    tags$style(HTML("#ab{color:black; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#

    tags$style(HTML("h3{color:black; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")),

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
      id = "type2",
      title = "Make sure you choose the file format correctly!",
      placement = "bottom",
      trigger = "hover"
    ),


    bsTooltip(
      id = "resint",
      title = "Green : Good, Orange: Average, Red: Insufficent",
      placement = "left",
      trigger = "hover"
    ),



    bsTooltip(
      id = "multint",
      title = "Green : Good, Orange: Average, Red: Insufficent",
      placement = "left",
      trigger = "hover"
    ),

    ####################################

    tags$head(tags$style(
      type="text/css",
      "#imagewarn img {max-width: 100%; width: auto; height: 100%; align: center}




        table,img, .tippy-content, textarea{ border-collapse: collapse;

  border-radius: 1em;

  overflow: hidden;}

  th, td {

  padding: 1em;

  background: #ddd;

  border-bottom: 2px solid white;

  border-top: 2px solid white;
  }

"
    )),

tags$head(tags$style(
  type="text/css",
  "#imagegif img {max-width: 100%; width: auto; height: 100%; align: right}"
)),


tags$head(tags$style(
  type="text/css",
  "#internal1 img {max-width: 100%; width: auto; height: 100%; align: right}"
)),

tags$head(tags$style(
  type="text/css",
  "#internal2 img {max-width: 100%; width: auto; height: 100%; align: right}"
)),


tags$head(tags$style(
  type="text/css",
  "#internal3 img {max-width: 100%; width: auto; height: 100%; align: right}"
)),



tags$head(tags$style(
  type="text/css",
  "#internal4 img {max-width: 100%; width: auto; height: 100%; align: right}"
)),



tags$head(tags$style(
  type="text/css",
  "#internal5 img {max-width: 100%; width: auto; height: 100%; align: right}"
)),


##################################

div(id = "tepe", tags$style(HTML("#tepe{border-bottom: 3px solid black;}")),
    fluidRow(

      column(6,

             h1(id="title", "INTERNAL RELIABILITY"),
             tags$style(HTML("#title{color: black; font-family: cursive;font-size:30px;
            font-style: oblique;text-align:left}"))

      )  ,

      column(6,
             h1(id="title2", "RSP PACKAGE  - CRAN"),
             tags$style(HTML("#title2{color: black; font-family: cursive;font-size:15px;
            font-style: oblique;text-align:right}"))
      )

    ) # close fluidrow
),



br(),

sidebarPanel( width = 4,

              ## PANEL 1 - INTRODUCTION ##
              conditionalPanel(
                condition = "input.panel==0",

                br(),
                shiny::img(src = "img/internal.png", width = "97%"),
                # imageOutput("internal2",width = "75%", height = "100px", inline = TRUE),

                shinyWidgets::spectrumInput(   # RENK PALET WİDGET
                  inputId = "myColor",
                  label = "Pick a color:",
                  choices = list(
                    list('gray', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
                    as.list(brewer_pal(palette = "Blues")(9)),
                    as.list(brewer_pal(palette = "Greens")(9)),
                    as.list(brewer_pal(palette = "Spectral")(11)),
                    as.list(brewer_pal(palette = "Dark2")(8))
                  ),
                  options = list(`toggle-palette-more-text` = "Show more")
                ),

                verbatimTextOutput(outputId = "coltext"), # SEÇİLEN RENGİ TEXTE DÖNÜŞTÜRME


              ),

              ## PANEL 2 - DATA UPLOAD ##
              conditionalPanel(
                condition = "input.panel==1",

                br(),
                shiny::img(src = "img/internal.png", width = "97%"),
                # imageOutput("internal3",width = "75%", height = "100px", inline = TRUE),

                br(),

                # selectInput(
                #   "type2",
                #   h3(id="ff","Select File Format"),
                #   choices = list(
                #     "CSV - Semicolon  Separated  Excel" = 1,
                #     "CSV - Comma  Separated  Excel" = 2,
                #     "SAV - SPSS" = 3,
                #     "XLSX - Excel"=4
                #   ),
                #   selected = 3
                # ),




                shinyWidgets::pickerInput(
                  inputId = "type2",
                  label = "Select File Format",
                  choices =  list(
                    "CSV - Semicolon  Separated  Excel" = 1,
                    "CSV - Comma  Separated  Excel" = 2,
                    "SAV - SPSS" = 3,
                    "XLSX - Excel"=4
                  ), selected = 3
                ),



                br(),

                fileInput(
                  "data1",
                  h3(id="ab","Uplad Data File")),

                br(),

                uiOutput("uiHeader"),

                br(),

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

              ),

            ## PANEL 3 - UNI-DIMENSIONAL DATA ##
            conditionalPanel(
              condition = "input.panel==2",

              br(),
              shiny::img(src = "img/internal.png", width = "97%"),
              # imageOutput("internal4",width = "75%", height = "100px", inline = TRUE),

              br(),

              # selectInput(
              #   "type",
              #   h3(id="dt", "Select Data Type"),
              #   choices = list("Polytomous (Likert etc..)" = 1, "1-0 " = 2),
              #   selected = 1
              # ),


              shinyWidgets::pickerInput(
                inputId = "type",
                label =  h3(id="dt", "Select Data Type"),
                choices = list("Polytomous (Likert etc..)" = 1, "1-0 " = 2),
                selected = 1 ),


              uiOutput("method"),

              uiOutput("Items") ,

              br(),

              fluidRow(

                column(6,
                       uiOutput("rep" )

                ),


                # column(6,  actionButton("act", h3("Compute"), icon = icon("cog",
                #                                                           lib = "glyphicon"), width = "210px",height="150px" )  )

                column( 6,
                        shinyWidgets::actionBttn(
                          inputId = "act",
                          label = "Compute",
                          color = "default",
                          style = "gradient",
                          icon = icon("glyphicon"),
                          size="md",
                          block = TRUE
                        )

                )

              ),

              br(),

              # withLoader( plotOutput("plotint1"),  type = "html", loader = "loader1")

            ),





            ## PANEL 4 - MULTI-DIEMNSIONAL DATA ##

            conditionalPanel(
              condition = "input.panel==3",

              shiny::img(src = "img/warn3.jpeg", width = "97%"),
              # imageOutput("imagewarn",width = "75%", height = "100px", inline = TRUE),

              #  checkboxInput("gifvid", h3("Watch tutorial?"), value = FALSE),

              br(),
              br(),


              shinyWidgets::switchInput(
                inputId = "gifvid",
                label = "Watch Tutorial",
                labelWidth = "100px",
                width = "150px"
              ),


              # withLoader(
              uiOutput("imagegif"),

                # imageOutput("imagegif",width = "150%", height = "100px", inline = TRUE),
                         # type = "html", loader = "loader1"),

              br(),


              fluidRow(

                column(6,

                       uiOutput("method2")),

                column(6,

                       uiOutput("Items2") ) ),


              # actionButton("act2", h3("Compute"), icon = icon("cog",
              #                                                 lib = "glyphicon"), width = "260px",col="blue" )



              shinyWidgets::actionBttn(
                inputId = "act2",
                label = "Compute",
                color = "default",
                style = "gradient",
                icon = icon("glyphicon"),
                size="lg",
                block = TRUE)


              # plotOutput("plotint2")


            ),


            ## PANEL 5 -Summary

            conditionalPanel(
              condition = "input.panel==4",

              shiny::img(src = "img/internal.png", width = "97%"),
              # imageOutput("internal5",width = "75%", height = "100px", inline = TRUE),

              br(),

              selectInput(
                "type3",
                h3(id="dt", "Select Data Type"),
                choices = list("Polytomous (Likert etc..)" = 1, "1-0 " = 2),
                selected = 1
              ),

              #
              # selectInput(
              #   "dim",
              #   h3(id="dt", "Select Factor Structure"),
              #   choices = list("Uni-dimensional)" = 1, "Multi-Dimensional " = 2),
              #   selected = 1
              # ),


              uiOutput("Items3"),
              #
              # actionButton("act3", h3("Compute"), icon = icon("cog",
              #                                                 lib = "glyphicon"), width = "260px",col="blue" )



              shinyWidgets::actionBttn(
                inputId = "act3",
                label = "Compute",
                color = "default",
                style = "gradient",
                icon = icon("glyphicon"),
                size="lg",
                block = TRUE)


            ),


), # close sidebar panel

##  MAIN PANEL ##

mainPanel(
  tabsetPanel(
    id = "panel",

    ## MAIN PANEL  1 ##
    tabPanel(

      h4(id="a", "INTRODUCTION"),
      value = 0,
      br(),
      br(),

      shiny::img(src = "img/internal.png", width = "97%"),
      # imageOutput("internal1",width = "75%", height = "100px", inline = TRUE),


    ),

    ##  MAIN PANEL 2 ##
    tabPanel(


      h4(id="a", "DATA UPLOAD"),
      value = 1,

      br(),

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

      DT::DTOutput("upload"),

      br(),

      gt::gt_output("desc1")

    ),

    ## MAIN PANEL 3 ##
    tabPanel(

      h4(id="a", "UNI-DIMENSIONAL DATA"),
      value = 2,

      br(),
      br(),

      br(),

      textOutput("text3"),
      tags$head(
        tags$style(
          "#text3{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
        )
      ),

      shiny::dataTableOutput("unidata"),

      br(),

      textOutput("text4"),
      tags$head(
        tags$style(
          "#text4{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
        )
      ),

      textOutput("text5"),
      tags$head(
        tags$style(
          "#text5{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
        )
      ),



      textOutput("text7"),
      tags$head(
        tags$style(
          "#text7{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
        )
      ),


      withLoader(DT::DTOutput("resint"),  type = "html", loader = "loader1"),

      br(),
      br(),

      withLoader(  plotOutput("plotint1"),  type = "html", loader = "loader1"),


      textOutput("textint"),
      tags$head(
        tags$style(
          "#textint{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
        )
      ),


    ),

    ## MAIN PANEL 4 ##
    tabPanel(

      h4 (id="a", "MULTI-DIMENSIOANAL DATA"),
      value = 3,


      textOutput("text31"),
      tags$head(
        tags$style(
          "#text31{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
        )
      ),

      shiny::dataTableOutput("multidata"),

      br(),

      textOutput("text6"),
      tags$head(
        tags$style(
          "#text6{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
        )
      ),


      textOutput("text8"),
      tags$head(
        tags$style(
          "#text8{
            color: darkblue;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
        )
      ),

      withLoader(DT::DTOutput("multint"), type = "html", loader = "loader1"),

      br(),
      br(),

      withLoader( plotOutput("plotint2"),     type = "html", loader = "loader1")

    ),

    # MAIN PANEL 5 #

    tabPanel(


      h4 (id="a", "SUMMARY"),

      br(),
      br(),

      textOutput("textsum"),
      tags$head(
        tags$style(
          "#textsum{
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

      withLoader(  gt::gt_output ("sumint"),     type = "html", loader = "loader1"),

      br(),

      withLoader(  gt::gt_output ("sumint2"),     type = "html", loader = "loader1"),

      br(),

      withLoader(  gt::gt_output ("sumint3"),type = "html", loader = "loader1"),

      br(),

      withLoader(  gt::gt_output ("sumint4"),type = "html", loader = "loader1"),

      br(),

      withLoader(   plotOutput("plotint3"), type = "html", loader = "loader1"),

      value = 4)

  ) # close tabsetpanel
) #  close mainpanel
  ) #  close fluidpage


  ## SERVER ##

  server <- function(input, output, session) {

    output$internal1<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$internal2<- renderImage({
      resim3 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$internal3<- renderImage({
      resim3 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$internal4<- renderImage({
      resim3 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$internal5<- renderImage({
      resim3 <- tempfile(fileext = '.png')
      list(src = "internal.png", contentType = "image/png")
    },
    deleteFile = FALSE)





    output$uiHeader <- renderUI({

      if(input$type2 == 3){

        NULL

      } else {

        # checkboxInput("header", h2("The first line is the variable name?")



        shinyWidgets::prettySwitch(
          inputId = "header",
          label = "Click if the first line is the variable name!",
          fill = FALSE
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


    # reactive(
    #
    #   setBackgroundColor(
    #     color = c("white", aaa),
    #     gradient = "linear",
    #     direction = c("bottom", "right")
    #   ),
    # )
    #


    ## MAIN PANEL 2 ##

    output$upload<-DT::renderDT({

      req(input$data1)

      data<-data()[1:10,1:5]

      backgroundColor <- sample(c("tomato1", "turquise", "skyblue",
                                  "orange", "lightblue"), 1)

      datatable(data, options = list(dom = 't')) %>% formatStyle(colnames(data),

                                                                 backgroundColor = "white")


    })


    ##  TEXTS FOR MAIN PANELS ##
    output$text1 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DATA UPLOAD AND BASIC STATISTICS")
      }
    })

    output$text2 <- renderText({
      if (!is.null(input$data1)) {
        paste0("DATA WAS UPLOADED SUCCESSFULLY")
      }
    })
    output$text3<- renderText({
      if (!is.null(input$data1)){
        req( length(input$items)>1)
        paste0("VARIABLES INCLUDED IN THE ANALYSIS")
      }
    })


    output$text31<- renderText({
      if (!is.null(input$data1)){
        req( length(input$items2)>1)
        paste0("VARIABLES INCLUDED IN THE ANALYSIS")
      }
    })

    output$text4<- renderText({
      req(input$mthd==1)
      if (!is.null(input$data1)){
        req( length(input$items)>1)
        paste0("RESULTS FOR CRONBACH'S ALPHA")
      }
    })

    output$text5<- renderText({
      req(input$mthd==2)
      if (!is.null(input$data1)){
        req( length(input$items)>1)
        paste0("RESULTS FOR TWO HALVES RELIABILITY")
      }
    })


    output$text6<- renderText({
      req(input$mthd2==1)
      if (!is.null(input$data1)){
        req( length(input$items2)>1)
        paste0("RESULTS FOR STRATIFIED ALPHA")
      }
    })


    output$text7<- renderText({
      req(input$mthd==3)
      if (!is.null(input$data1)){
        req( length(input$items)>1)
        paste0("RESULTS FOR HOYT'S VARIANCE ANALYSIS")
      }
    })

    output$text8<- renderText({
      req(input$mthd2==2)
      if (!is.null(input$data1)){
        req( length(input$items2)>1 )
        paste0("RESULTS FOR OMEGA RELIABILITY")
      }
    })

    output$textsum<- renderText({
      if (!is.null(input$data1)){
        req( length(input$items3)>1 )
        paste0("SUMMARY OF RELIABILITY ANALYSIS")
      }
    })


    ## MAIN PANEL 2 DECRIPTIVES ##

    output$desc1 <- render_gt(align = "center", {
      if (!is.null(input$data1)) {
        data <- data()
        NUMBER_OF_ITEMS <- ncol(data)
        NUMBER_OF_RESPONDENTS <- nrow(data)
        NUMBER_OF_BLANK_ITEMS <- length(which(is.na(data)))

        res <- data.frame(
          N = NUMBER_OF_ITEMS,
          N_ITEMS = NUMBER_OF_RESPONDENTS,
          NA. =  NUMBER_OF_BLANK_ITEMS
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
            c(N_ITEMS) ~ gt::px(250),
            everything() ~ gt::px(250)
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


    ### UI WIDGETS UNI-DIMENSIONAL ##

    #### 1
    output$Items<- renderUI({

      req (input$data1)


      shinyWidgets::pickerInput(
        "items",
        label=h3(id="dt", "Select Items"),
        choices = colnames(data()),
        selected = 1,

        options = list(
          `actions-box` = TRUE),
        multiple = TRUE )

    })



    ### 2

    output$method<-renderUI({

      req(input$data1)

      if (input$type==1)



      {
        shinyWidgets::pickerInput(
          "mthd",
          h3("Select Method"),
          choices = list(
            "Cronbach Alpha" = 1,
            "Two-Halves"=2,
            "Hoyt's Analysis of Variance"=3
          ),
          selected = 1 ) }

      else

      {
        shinyWidgets::pickerInput(
          "mthd",
          h3("Select Method"),
          choices = list(

            "Cronbach Alpha" = 1,
            "Two-Halves"=2,
            "Hoyt's Analysis of Variance"=3,
            "KR 20-21"=4 )) }

    })

    output$rep<-  renderUI({

      req(input$mthd==2)

      # numericInput("repth", h3("Replication"), min=1, max=100000,
      #              value = 1000)


      shinyWidgets::numericInputIcon(
        inputId = "repth",
        label = NULL,
        min = 1,
        max=10000,
        value = 1000,
        icon = "Replication"
      )


    })



    ### UI WIDGETS MULTİ-DIMENSIONAL ##



    output$Items2<- renderUI({

      req (input$data1)



      shinyWidgets::pickerInput(
        "items2",
        label=h3(id="dt", "Select Items"),
        choices = colnames(data()),
        selected = 1,

        options = list(
          `actions-box` = TRUE),
        multiple = TRUE )

    })








    output$method2<-renderUI({

      req(input$data1)


      shinyWidgets::pickerInput(
        "mthd2",
        h3("Select Method"),
        choices = list(
          "Stratfied Alpha"=1,
          "Omega" = 2
        ),
        selected = 1 )

    })

    output$nfac<- renderUI({

      req(input$mthd2==2)

      numericInput("nfac2" ,h3("Number of Factors"), min=1, max=20, value = 1)


    })



    ##### PANEL 3 OUTPUTS  UNI-DATA

    output$unidata<-renderDataTable({

      req(input$data1)

      remitems<-input$items

      head(data()[remitems],3)

    })

    # CR.ALPHA - TWO HALVES ETC...

    output$resint<-DT::renderDT({

      req(input$data1)

      remitems<-input$items

      intdata<-data()[remitems]

      req(input$act)

      req(length(input$items)>1)


      ### ALPHA

      if ( input$mthd==1)    {

        alfa<-ltm::cronbach.alpha(intdata, CI=TRUE)


        alpha<-alfa[[1]]
        N<-alfa[[2]]
        N_items<-alfa[[3]]
        lower_CI <-  unname(alfa[[6]][1])
        upper_CI <-  unname(alfa[[6]][2])

        res1<-data.frame(N, N_items,Alpha=alpha,lower_CI,upper_CI )

        res1<-round(res1,3)


        backgroundColor <- sample(c("turquise", "skyblue",
                                    "lightblue"), 1)

        res1<-  datatable(res1, width = "50px", options = list(dom = 't')) %>% formatStyle(colnames(res1),

                                                                                           backgroundColor = "gray") %>%

          formatStyle('Alpha',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green")))  %>%

          formatStyle('lower_CI',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green")))


        # return(res1)

        REL$res1<-res1

        return(REL$res1)
      }

      ### TWO HALVES




      if ( input$mthd==2) {

        # REL$iki.yari<-iki.yari

        ##############
        oddeven <- unname(iki.yari(intdata, "tekcift"))

        ikiyari_tekrar <- replicate(2500, iki.yari(intdata,yontem="seckisiz")  )

        rsb<-NULL

        for (i in 1:2500) { rsb[i]<-ikiyari_tekrar[[i]]}

        random<-mean(rsb)

        res2<-data.frame( N= nrow(intdata), Nitem= ncol(intdata), oddeven, random ,
                          replication=input$repth )

        res2<-round(res2,3)


        backgroundColor <- sample(c("turquise", "skyblue",
                                    "lightblue"), 1)

        res2<-   datatable(res2, width = "50px", options = list(dom = 't')) %>% formatStyle(colnames(res2),

                                                                                            backgroundColor = "gray") %>%
          formatStyle('random',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green")))  %>%

          formatStyle('oddeven',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green")))

        # return(res2)


        REL$res2<-res2

        return(REL$res2)


      }


      if ( input$mthd==3) {

        # REL$hoyt<-hoyt

        res3<-hoyt(data()[remitems])


        res3<-round(res3,3)


        backgroundColor <- sample(c("turquise", "skyblue",
                                    "lightblue"), 1)

        res3<-   datatable(res3, width = "50px",  options = list(dom = 't')) %>% formatStyle(colnames(res3),

                                                                                             backgroundColor = "gray") %>%
          formatStyle('Hoyt',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green")))

        #  return(res3)


        REL$res3<-res3

        return(REL$res3)

      }

      ### KR 20 21

      if ( input$mthd==4) {


        res4_1<-KR20(data()[remitems])
        res4_2<-KR21(data()[remitems] )


        res4_3<-data.frame(N= nrow(data()[remitems]),
                           Nitem= ncol(data()[remitems]),
                           KR20=res4_1$KR20, KR21=res4_2$KR21)


        backgroundColor <- sample(c("turquise", "skyblue",
                                    "lightblue"), 1)

        res4_3<-   datatable(res4_3, width = "50px", options = list(dom = 't')) %>% formatStyle(colnames(res4_3),

                                                                                                backgroundColor = "gray") %>%
          formatStyle('KR20',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green"))) %>%

          formatStyle('KR21',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green")))


        # return(res4_3)


        REL$res4_3<-res4_3

        return(REL$res4_3)



      }


    })

    ########################################################################################



    ## PLOT UNI-DATA
    #
    output$plotint1<- renderPlot( {

      req(input$data1)
      req(input$act)

      req( length(input$items)>1)

      remitems<-input$items

      intdata<-data()[remitems]

      if ( input$mthd==1)    {

        alfa<-ltm::cronbach.alpha(intdata, CI=TRUE)
        alpha<-alfa[[1]]
        N<-alfa[[2]]
        N_items<-alfa[[3]]
        lower_CI <-  unname(alfa[[6]][1])
        upper_CI <-  unname(alfa[[6]][2])
        res<-data.frame(N, N_items,Alpha=alpha,lower_CI,upper_CI )

        res<-round(res,3)

        value <- res[,3]

        method<-"Alpha"

        res_cr<- data.frame( method, value)

        g<-ggplot2::ggplot(res_cr, ggplot2::aes(x = method, y = value,fill = factor(method))) +
          ggplot2::geom_bar(stat = "identity", col="blue", width = 0.5, alpha=0.3 ) +

          ggplot2::labs(title = "Graph for Alpha Reliability ") +

          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 25)) +

          ggplot2::ylim(0, 1) +

          ggplot2::geom_text(ggplot2::aes(label =  round(value,3)), vjust = 2, col="black",size=8 ) +
          ggplot2::geom_text(ggplot2::aes(label=method), vjust=5, col="black", size=8)

        return(     g + ggplot2::geom_hline(yintercept=0.80, linetype="dashed",
                                   color = "darkgreen", size=1.7)   )

      }



      if (input$mthd==2)    {


        iki.yari<-REL$iki.yari

        oddeven <-  unname(iki.yari(intdata, "tekcift"))

        ikiyari_tekrar <- replicate(input$repth,iki.yari(intdata,yontem="seckisiz"))

        rsb<-NULL

        for (i in 1:input$repth) {rsb[i]<-ikiyari_tekrar[[i]]}

        random<-mean(rsb)

        res2 <-data.frame( N= nrow(intdata), Nitem= ncol(intdata), oddeven,
                           random ,replication=input$repth )

        rth<- cbind(res2$oddeven, res2$random)


        rth<-as.data.frame(round(t(rth),3))

        colnames(rth)<- "value"

        method<-c("oddeven","random")

        rth<-transform(rth, method=method)

        g<-ggplot2::ggplot(rth, ggplot2::aes(x = method, y = value,fill = factor(method))) +
          ggplot2::geom_bar(stat = "identity", col="blue", width = 0.5 ,alpha=0.3) +

          ggplot2::labs(title = "Graph for Two Half Reliability ") +

          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 25)) +

          ggplot2::ylim(0, 1) +

          ggplot2::geom_text(ggplot2::aes(label =  round(value,3)), vjust = 2, col="black",size=8 ) +
          ggplot2::geom_text(ggplot2::aes(label=method), vjust=5, col="black",size=8 )

        return(   g + ggplot2::geom_hline(yintercept=0.80, linetype="dashed",
                                 color = "darkgreen", size=1.7) )


      }


      if (input$mthd==3) {


        hoyt<- REL$hoyt

        reshp<-hoyt(data()[remitems])

        value <- reshp[,3]

        method<-"Hoyt"

        res_hyt<- data.frame( method, value)


        g<-ggplot2::ggplot(res_hyt, ggplot2::aes(x = method, y = value,fill = factor(method))) +
          ggplot2::geom_bar(stat = "identity", col="blue", width = 0.5,alpha=0.3 ) +

          ggplot2::labs(title = "Graph for Hoyt's Variance Analysis ") +

          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 25)) +

          ggplot2::ylim(0, 1) +

          ggplot2::geom_text(ggplot2::aes(label =  round(value,3)), vjust = 2, col="black",size=8 ) +
          ggplot2::geom_text(ggplot2::aes(label=method), vjust=5, col="black", size=8)

        return(     g + ggplot2::geom_hline(yintercept=0.80, linetype="dashed",
                                   color = "darkgreen", size=1.7)   )


      }



      if( input$mthd==4) {


        KR21<-REL$KR21
        KR20<-REL$KR20

        rkr20<-KR20(data()[remitems])
        rkr21<-KR21(data()[remitems])


        method<- c("KR20","KR21")
        value<- c(rkr20[,2],rkr21[,2])
        res_kr<- data.frame(method, value)

        g<-ggplot2::ggplot(res_kr, ggplot2::aes(x = method, y = value,fill = factor(method))) +
          ggplot2::geom_bar(stat = "identity", col="blue", width = 0.5,alpha=0.3 ) +

          ggplot2::labs(title = "Graph for KR Reliability ") +

          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 25)) +

          ggplot2::ylim(0, 1) +

          ggplot2::geom_text(ggplot2::aes(label =  round(value,3)), vjust = 2, col="black",size=8 ) +
          ggplot2::geom_text(ggplot2::aes(label=method), vjust=5, col="black",size=8 )

        return(   g + ggplot2::geom_hline(yintercept=0.80, linetype="dashed",
                                 color = "darkgreen", size=1.7) ) }





    }) # close render plot





    # PANEL 4 OUTPUTS  MULTI-DATA

    output$multidata<-renderDataTable({

      req(input$data1)

      remitems<-input$items2

      head(data()[remitems],3)

    })


    ### STR ALPHA

    output$multint<- DT::renderDT({

      req(input$data1)

      req(input$act2)

      req( length(input$items2)>1)

      remitems<-input$items2

      rem<- data()[remitems]

      colnames(rem)<-toupper(colnames(rem))



      if(input$mthd2==1) {

        res<- str.alpha2(rem)


        res<-  cbind(res[1:2],round(res[,3:5],3))


        res<-as.data.frame(res)

        Nres1<-length(res$Scale)-1

        REL$Nres1<-Nres1

        backgroundColor <- sample(c("turquise", "skyblue",
                                    "lightblue"), 1)

        res_str<- datatable(res, width = "50px", options = list(dom = 't')) %>% formatStyle(colnames(res),

                                                                                             backgroundColor = "gray")  %>%


          formatStyle('Alpha',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green")))  %>%

          formatStyle('Stratified.Alpha',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green")))

        return(res_str)


      } # close str alpha

      ### OMEGA


      if(input$mthd2==2) {

        Nresx <- REL$Nres1

        res_omega<-omega( rem, Nresx)


        res_omega<-data.frame( N=nrow(data()), Nitem=ncol(rem), Nfac= Nresx,
                               Omega=res_omega$omega  )


        backgroundColor <- sample(c("turquise", "skyblue",
                                    "lightblue"), 1)

        res_omega<- datatable(res_omega, height = "400px", options = list(dom = 't')) %>%

          formatStyle(colnames(res_omega),

                      backgroundColor = "gray")  %>%

          formatStyle('Omega',
                      backgroundColor = DT::styleInterval(c( 0.70,0.79),
                                                          c("red", "orange", "green")))

        return(res_omega)



      }



    })



    ## PLOT MULTI - DATA


    output$plotint2<-renderPlot({

      req(input$data1)

      req(input$act2)

      req( length(input$items2)>1)

      # req(input$mthd2==1)  ## IF ELSE LE DEĞİŞİTİR OMEGA GELİNCE

      remitems<-input$items2

      rem<- data()[remitems]

      colnames(rem)<-toupper(colnames(rem))

      if( input$mthd2==1) {

        # str.alpha2<-REL$str.alpha2

        res<- str.alpha2(rem)

        Nres2<-length(res$Scale)-1

        REL$Nres2<-Nres2

        rstr<-NULL

        for ( i in 1:nrow( res )) {

          rstr[i]<-res$Alpha[i] }

        rstr<-rstr[-1]

        r4<-res$Stratified.Alpha[1]

        r34<-c(rstr,r4)

        # value<-r34[-1]
        value <- r34

        scale<-res[,1]

        scale<-scale[-1]

        scale<-c(scale,"Str.Alpha")


        sa<-data.frame(scale,value)



        g<-ggplot2::ggplot(sa, ggplot2::aes(x = scale, y = value,fill = factor(scale))) +
          ggplot2::geom_bar(stat = "identity", col="blue", width = 0.5, alpha=0.3 ) +

          ggplot2::labs(title = "Graph for Stratified Alpha ") +

          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 25)) +

          ggplot2::ylim(0, 1) +

          ggplot2::geom_text(ggplot2::aes(label =  round(value,3)), vjust = 2, col="black",size=8 ) +
          ggplot2::geom_text(ggplot2::aes(label=scale), vjust=3.8, col="black",size=8 )

        return(   g + ggplot2::geom_hline(yintercept=0.80, linetype="dashed",
                                 color = "darkgreen", size=1.7,alpha=0.4) )

      } # close if




      if(input$mthd2==2) {

        res_omg<- omega(rem,REL$Nres2)

        value <- res_omg[,2]

        method<-"omega"

        res_omg2<- data.frame( method, value)


        g<-ggplot2::ggplot(res_omg2, ggplot2::aes(x = method, y = value,fill = factor(method))) +
          ggplot2::geom_bar(stat = "identity", col="blue", width = 0.5,alpha=0.3 ) +

          ggplot2::labs(title = "Graph for Omega Reliability ") +

          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 25)) +

          ggplot2::ylim(0, 1) +

          ggplot2::geom_text(ggplot2::aes(label =  round(value,3)), vjust = 2, col="black",size=8 ) +
          ggplot2::geom_text(ggplot2::aes(label=method), vjust=5, col="black",size=8 )

        return( g + ggplot2::geom_hline(yintercept=0.80, linetype="dashed",
                               color = "darkgreen", size=1.7) ) }




    }) # close render plot

    ## IMAGE RENDER ##


    ### 1
    # output$imagegif<- renderImage({
    #
    #   req (input$data1)
    #
    #
    #   req( input$gifvid== TRUE)
    #
    #   resim2 <- tempfile(fileext = '.gif')
    #   list(src = "gifrel.gif", contentType = "image/gif")
    # },
    # deleteFile = FALSE)

    output$imagegif <- renderUI({
      req(input$data1)
      req(input$gifvid == TRUE)
      shiny::img(src = "https://shiny.eptlab.com/RSPEN/reliability/img/gifrel.gif", width = "97%")
    })



    ### 2
    #
    # output$imagewarn<- renderImage({
    #
    #   req (input$data1)
    #
    #   shiny::img(src = "img/warn3.png", width = "97%")
    #   resim2 <- tempfile(fileext = '.png')
    #   list(src = "warn3.png", contentType = "image/png")
    # },
    # deleteFile = FALSE)





    ### SUMMARY ###



    output$Items3<- renderUI({

      req (input$data1)
      shinyWidgets::pickerInput(
        "items3",
        h3(id="dt", "Select Items"),
        choices = colnames(data()),
        selected = 1, multiple = TRUE
      )



      shinyWidgets::pickerInput(
        "items3",
        label=h3(id="dt", "Select Items"),
        choices = colnames(data()),
        selected = 1,

        options = list(
          `actions-box` = TRUE),
        multiple = TRUE )

    })


    output$sumint<- gt::render_gt ( align = "center", {

      req(input$data1)

      req(input$act3)

      req(length(input$items3)>1)

      remitems<-input$items3

      rem<- data()[remitems]

      dat<-data()

      colnames(dat)<-toupper(colnames(dat))

      sumres<-data.frame( N=nrow(data()), Nitem= length(input$items3),

                          Number_NA = length(which(is.na(data()))))

      res <- gt::gt(sumres)

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
          c(Nitem) ~ gt::px(300),
          everything() ~ gt::px(300)
        )

      res <- res %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- res %>%
        tab_options(heading.title.font.size = gt::px(25))

    })

    ################################

    output$sumint2<- gt::render_gt ( align = "center", {

      req(input$data1)

      req(input$act3)

      req( length(input$items3)>1)

      remitems<-input$items3

      rem<- data()[remitems]


      colnames(rem)<-toupper(colnames(rem))

      ####

      alfa<-ltm::cronbach.alpha(rem, CI=TRUE)
      Alpha<-alfa[[1]]

      REL$Alphas<-Alpha

      ###

      hoyt<-REL$hoyt

      HoyT<- hoyt(rem)

      Hoyt<-HoyT[,3]

      REL$Hoyts<-Hoyt

      ###

      oddeven <- unname(iki.yari(rem, "tekcift"))

      REL$oddevens<-oddeven

      ikiyari_tekrar <- replicate(2000, iki.yari(rem,yontem="seckisiz")  )

      rsb<-NULL

      for (i in 1:1000) { rsb[i]<-ikiyari_tekrar[[i]]}

      random<-mean(rsb)

      REL$randoms<-random

      sumres<-data.frame( Alpha, TwoHalves_odd=oddeven, TwoHalves_rand=random,
                          Hoyt)

      res <- gt::gt(sumres)


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
          c(TwoHalves_odd) ~ gt::px(250),
          everything() ~ gt::px(250)
        )

      res <- res %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- res %>%
        tab_options(heading.title.font.size = gt::px(25))


    })



    #############



    output$sumint3<- gt::render_gt ( align = "center", {

      req(input$data1)

      req(input$act3)


      remitems<-input$items3

      rem<- data()[remitems]

      colnames(rem)<-toupper(colnames(rem))



      res<- str.alpha2(rem)


      Str.Alpha<-res$Stratified.Alpha[1]

      REL$Str.Alphas<-Str.Alpha

      res<-  cbind(res[1:2],round(res[,3:5],3))



      res<-as.data.frame(res)

      Nres1<-length(res$Scale)-1

      REL$Nresum<-Nres1

      omega_res<-omega(rem,Nres1)

      nn<-Nres1-1

      omg<-omega_res$omega

      REL$omegas<-omg

      res$Omega<- c(omg, rep(NA,Nres1))

      res <- gt::gt(res)

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
          c(Stratified.Alpha) ~ gt::px(180),
          everything() ~ gt::px(180)
        )

      res <- res %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- res %>%
        tab_options(heading.title.font.size = gt::px(25))
    })

    #######################################################################

    output$sumint4<- gt::render_gt ( align = "center", {

      req(input$data1)

      req( length(input$items3)>1)

      remitems<-input$items3

      rem<- data()[remitems]

      colnames(rem)<-toupper(colnames(rem))

      req(input$type3==2)

      req(input$act3)

      rkr20<-KR20(rem)
      rkr21<-KR21(rem)

      res1<-data.frame(KR20=rkr20$KR20, KR21=rkr21$KR21)

      res <- gt::gt(res1)


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
          c(KR21) ~ gt::px(300),
          everything() ~ gt::px(300)
        )

      res <- res %>%
        tab_options(
          column_labels.font.size = gt::px(17),
          column_labels.font.weight = "bold"
        )

      res <- res %>%
        tab_options(heading.title.font.size = gt::px(25))

      return(res)

    })


    ############ SUMMARY PLOT ########### SORUNLU???

    output$plotint3<-renderPlot({

      req(input$act3)

      remitems<-input$items3

      rem<- data()[remitems]

      colnames(rem)<-toupper(colnames(rem))


      req(input$data1)

      # req( length(input$items3)>1)

      req(input$act3)


      alfa<-ltm::cronbach.alpha(rem, CI=TRUE)
      Alpha<-alfa[[1]]

      ##
      iki.yari<-REL$iki.yari

      oddeven <-  unname(iki.yari(rem, "tekcift"))

      oddeven<-as.matrix(oddeven)

      ikiyari_tekrar <- replicate(2500,iki.yari(rem,yontem="seckisiz"))

      rsb<-NULL

      for (i in 1:2500) {rsb[i]<-ikiyari_tekrar[[i]]}

      random<-mean(rsb)

      ##

      HoyT<- hoyt(rem)

      Hoyt<-HoyT[,3]

      ##

      res<- str.alpha2(rem)

      Nres2<-length(res$Scale)-1

      Str.Alpha<-res$Stratified.Alpha[1]


      res_omg<- omega(rem,Nres2)

      omegas <- res_omg[,2]


      rkr20<-KR20(rem)
      rkr21<-KR21(rem)




      if( input$type3==1) {



        Methods<-c("Alpha","Hoyt", "TH_oddeven","TH_random","Str.Alpha","Omega")

        Values<-c(Alpha,Hoyt,oddeven[1],random, Str.Alpha, omegas )  }




      else   {

        Methods<-c("Alpha","Hoyt", "TH_oddeven","TH_random", "KR20", "KR21","Str.Alpha","Omega")

        Values<-c(Alpha,Hoyt,oddeven[1],random,rkr20$KR20,rkr21$KR21, Str.Alpha, omegas )


      }

      splot<-cbind(Methods, Values)

      splot<-data.frame(Methods,Values)


      g<-ggplot2::ggplot(splot, ggplot2::aes(x = Methods, y = Values,fill = factor(Methods))) +
        ggplot2::geom_bar(stat = "identity", col="blue", width = 0.80, alpha=0.3 ) +

        ggplot2::labs(title = "Graph Internal Reliability Results ") +

        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 25)) +

        ggplot2::ylim(0, 1) +

        ggplot2::geom_text(ggplot2::aes(label =  round(Values,3)), vjust = 2, col="black",size=5 ) +
        ggplot2::geom_text(ggplot2::aes(label=Methods), vjust=3.8, col="black",size=5 )

      return(   g + ggplot2::geom_hline(yintercept=0.80, linetype="dashed",
                               color = "darkgreen", size=1.7,alpha=0.4) )

    })







    observeEvent(input$myColor,{
      output$cols<- renderUI({   # WIDET RENDER UI RENK DEĞİŞİMİ

        bbb<-input$myColor

        shinyWidgets::setBackgroundColor(
          color = c("white", bbb),
          gradient = "linear",
          direction = c("bottom", "right")
        )})
    })


    # output$cols<- renderUI({   # WIDET RENDER UI RENK DEĞİŞİMİ
    #
    #   bbb<-aaa
    #
    #   setBackgroundColor(
    #     color = c("white", bbb),
    #     gradient = "linear",
    #     direction = c("bottom", "right")
    #   ) })





    session$onSessionEnded(function() {
      stopApp()
    })

    # calib <- calibData <- dataGL <- kalan<- kalan.data<- km<- m1m2<- mke<- NULL
    # optionPlot<-  q3.1<- q3Table  <- secenek<- theta <- tumu<- NULL


  } # close server


  shinyApp(ui = ui, server = server)
}



