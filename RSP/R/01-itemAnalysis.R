#' Item and test statistics based on classical test theory,
#' @import shiny
#' @importFrom rstudioapi isAvailable jobRunScript jobSetStatus executeCommand
#' @return No return value, opens web browser and loads shiny application
#' @examples \dontrun{ITEMAN()}
#' @export

ITEMAN <- function() {
  ITEMAN_ENV <- new.env()

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

  ################################## FUNCTIONS##############################

  ITEMAN_ENV$itemAnalysis1 <- function(data, key){
    data_01 <- as.data.frame(
      matrix(
        nrow = nrow(data),
        ncol = ncol(data)
      )
    )
    for (i in 1:ncol(data_01)){
      data_01[, i] <- ifelse(data[, i] == key[i], 1, 0)
      if (length(which(is.na(data_01[, i]))) != 0) {
        data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
      }
    }

    bs_dataGL <- data_01
    sum_score <- rowSums(data_01)
    p <- colMeans(data_01)
    pbis <- c()
    bis <- c()
    for (k in 1:ncol(data_01)){
      pbis[k] = cor(data_01[, k],
                    sum_score,
                    use = "pairwise.complete.obs")
      bis[k] = polycor::polyserial(sum_score, data_01[, k])
    }
    itemStat <- matrix(nrow = ncol(data_01), ncol = 3)
    colnames(itemStat) <- c("Item_Difficulty",
                            "Point_Biserial",
                            "Biserial")
    rnames <- character()
    for (i in 1:ncol(data_01)) {
      rnames[i] <- paste0("item", i)
    }
    rnames <- rnames
    rownames(itemStat) <- rnames
    Item_Difficulty <- round(p, 3)
    Point_Biserial <- round(pbis, 3)
    Biserial <- round(bis, 3)

    itemStat <- data.frame(
      Items = rnames,
      Item_Difficulty,
      Point_Biserial,
      Biserial
    )
    res1 <- gt::gt(itemStat)
    res1 <- res1 %>%
      tab_header(
        title = md("**ITEM ANALYSIS RESULTS**")
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_text(
          weight = "bolder",
          # stretch = "extra-expanded"
        ),
        locations = cells_body(columns = c("Items"))
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_text(
          weight = "bolder",
          color = "red",
          decorate = "underline",
          # stretch = "extra-expanded"
        ),
        locations = cells_body(
          columns = c(Item_Difficulty),
          rows = Item_Difficulty >= 0.80 | Item_Difficulty  <= 0.25
        )
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_text(
          weight = "bolder",
          color = "red",
          decorate = "underline",
          # stretch = "extra-expanded"
        ),
        locations = cells_body(
          columns = c(Point_Biserial),
          rows = Point_Biserial <= 0.25
        )
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_text(
          weight = "bolder",
          color = "red",
          decorate = "underline"
        ),
        locations = cells_body(
          columns = c(Biserial),
          rows = Biserial <= 0.25
        )
      )
    res1 <- res1 %>%
      cols_width(
        c(Point_Biserial) ~ gt::px(150),
        everything() ~ gt::px(150)
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_fill(
          color = sample(colors()[3:100],1), alpha=0.20
        ),
        locations = cells_body()
      )
    res1 <- res1 %>%
      tab_options(
        column_labels.font.size = gt::px(17),
        column_labels.font.weight = "bold"
      )
    return(res1)
  }

  ####

  ITEMAN_ENV$itemAnalysis2 <- function(data){
    data_01 <- unname(data)
    for (i in 1:ncol(data_01)){
      if (length(which(is.na(data_01[, i]))) != 0){
        data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
      }
    }
    sum_score <- rowSums(data_01)
    p <- colMeans(data_01)
    pbis <- c()
    bis <- c()
    for (k in 1:ncol(data_01)) {
      pbis[k] = cor(data_01[, k],
                    sum_score,
                    use = "pairwise.complete.obs")
      bis[k] = polycor::polyserial(sum_score,
                                   data_01[, k])
    }

    itemStat <- matrix(nrow = ncol(data_01), ncol = 3)
    colnames(itemStat) <- c("Item_Difficulty",
                            "Point_Biserial",
                            "Biserial")
    rnames <- character()
    for (i in 1:ncol(data_01)) {
      rnames[i] <- paste0("item", i)

    }
    rnames <- rnames
    rownames(itemStat) <- rnames
    Item_Difficulty <- round(p, 3)
    Point_Biserial <- round(pbis, 3)
    Biserial <- round(bis, 3)
    itemStat <- data.frame(
      Items = rnames,
      Item_Difficulty,
      Point_Biserial,
      Biserial
    )
    res1 <- gt::gt(itemStat)
    res1 <- res1 %>%
      tab_header(
        title = md("**ITEM ANALYSIS RESULTS**")
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_text(
          weight = "bolder",
          # stretch = "extra-expanded"
        ),
        locations = cells_body(columns = c("Items"))
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_text(
          weight = "bolder",
          color = "red",
          decorate = "underline",
          # stretch = "extra-expanded"
        ),
        locations = cells_body(
          columns = c(Item_Difficulty),
          rows = Item_Difficulty >= 0.80 | Item_Difficulty  <= 0.25
        )
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_text(
          weight = "bolder",
          color = "red",
          decorate = "underline",
          # stretch = "extra-expanded"
        ),
        locations = cells_body(
          columns = c(Point_Biserial),
          rows = Point_Biserial <= 0.25
        )
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_text(
          weight = "bolder",
          color = "red",
          decorate = "underline"
        ),
        locations = cells_body(
          columns = c(Biserial),
          rows = Biserial <= 0.25
        )
      )
    res1 <- res1 %>%
      cols_width(
        c(Point_Biserial) ~ gt::px(150),
        everything() ~ gt::px(150)
      )
    res1 <- res1 %>%
      tab_style(
        style = cell_fill(
          color = sample(colors()[3:100],1), alpha=0.20
        ),
        locations = cells_body()
      )
    res1 <- res1 %>%
      tab_options(
        column_labels.font.size = gt::px(17),
        column_labels.font.weight = "bold"
      )
    return(res1)
  }


  #####


  ITEMAN_ENV$itemAnalysis1_2 <- function(data, key) {
    bs_data <- as.data.frame(
      matrix(nrow = nrow(data),
             ncol = ncol(data))
    )
    for (i in 1:ncol(bs_data)) {
      bs_data[, i] <- ifelse(data[, i] == key[i], 1, 0)
      if (length(which(is.na(bs_data[, i]))) != 0) {
        bs_data[which(is.na(bs_data[, i]) == TRUE), i] = 0
      }
    }

    top_puan <- rowSums(bs_data)
    p <- colMeans(bs_data)
    pbis <- c()
    bis <- c()
    for (k in 1:ncol(bs_data)) {
      pbis[k] = cor(bs_data[, k],
                    top_puan,
                    use = "pairwise.complete.obs")
      bis[k] = polycor::polyserial(top_puan,
                                   bs_data[, k])
    }

    madde_ist <- matrix(nrow = ncol(bs_data),
                        ncol = 3)
    colnames(madde_ist) <- c("Item_Difficulty",
                             "Point_Biserial",
                             "Biserial")
    rnames <- character()
    for (i in 1:ncol(bs_data)) {
      rnames[i] <- paste0("item", i)
    }
    rnames <- rnames
    rownames(madde_ist) <- rnames
    Item_Difficulty <- round(p, 3)
    Point_Biserial <- round(pbis, 3)
    Biserial <- round(bis, 3)
    madde_ist <- data.frame(
      ItemS = rnames,
      Item_Difficulty,
      Point_Biserial,
      Biserial)
    return(madde_ist)
  }

  ## Observe Event Function 2 ##

  ITEMAN_ENV$itemAnalysis2_2 <- function(data) {
    data_01 <- unname(data)
    for (i in 1:ncol(data_01)){
      if (length(which(is.na(data_01[, i]))) != 0){
        data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
      }
    }
    sum_score <- rowSums(data_01)
    p <- colMeans(data_01)
    pbis <- c()
    bis <- c()
    for (k in 1:ncol(data_01)) {
      pbis[k] = cor(data_01[, k],
                    sum_score,
                    use = "pairwise.complete.obs")
      bis[k] = polycor::polyserial(sum_score,
                                   data_01[, k])
    }

    itemStat <- matrix(nrow = ncol(data_01), ncol = 3)
    colnames(itemStat) <- c("Item_Difficulty",
                            "Point_Biserial",
                            "Biserial")
    rnames <- character()
    for (i in 1:ncol(data_01)) {
      rnames[i] <- paste0("item", i)

    }
    rnames <- rnames
    rownames(itemStat) <- rnames
    Item_Difficulty <- round(p, 3)
    Point_Biserial <- round(pbis, 3)
    Biserial <- round(bis, 3)
    itemStat <- data.frame(
      Items = rnames,
      Item_Difficulty,
      Point_Biserial,
      Biserial
    )

    return(itemStat)

  }


  ###################### FUNCTIONS SONU ##############################


  ui <- fluidPage(

    shinyjs::useShinyjs(),


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

  tags$head(tags$style(
    type="text/css",
    "#image5 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image6 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),

  tags$head(tags$style(
    type="text/css",
    "#image7 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),


  tags$head(tags$style(
    type="text/css",
    "#image8 img {max-width: 100%; width: auto; height: 100%; align: center}"
  )),



  #########################################################################################

  ###################################################################################

  tags$style(HTML("#a{color:black; font-family:Lucida Arial ;font-size: 16px;
              font-style: oblique;text-align:center}")), #tabs#

  tags$style(HTML("#ab{color:black; font-family:Lucida Arial ;font-size: 20px;
             font-style: oblique;text-align:center}")), # widgets#

  tags$style(HTML("#b{color:black; font-family: cursive;font-size: 15px;
            font-style: oblique;text-align:center}")), # download #

  ####################################################################################


  theme = shinytheme("readable"), # to select theme

  ## Create Pop-up Messages ##

  shinyBS::bsTooltip(
    id = "ab",
    title = "Make sure you choose the data type correctly!",
    placement = "right",
    trigger = "hover"
  ),
  shinyBS::bsTooltip(
    id = "type",
    title = "For option matrix add the key to the first row of the dataset.",
    placement = "bottom",
    trigger = "focus"
  ),

  shinyBS::bsTooltip(
    id = "type2",
    title = "Make sure you choose the file format correctly!",
    placement = "right",
    trigger = "hover"
  ),

  shinyBS::bsTooltip(
    id = "rmItem",
    title = "Selecet the item/s you want to remove",
    placement = "bottom",
    trigger = "hover"
  ),

  shinyBS::bsTooltip(
    id = "text1",
    title = "Only a small part of  the data is presented",
    placement = "bottom",
    trigger = "hover"
  ),

  shinyBS::bsTooltip(
    id = "text2",
    title = "Problematic items are highlighted in red and underlined!",
    placement = "bottom",
    trigger = "hover"
  ),

  shinyBS::bsTooltip(
    id = "text3",
    title = "Problematic options are highlighted in red and underlined!",
    placement = "bottom",
    trigger = "hover"
  ),

  shinyBS::bsTooltip(
    id = "text4",
    title = "If you choose the wrong data type, the results might be misleading. Please check!",
    placement = "bottom",
    trigger = "hover"
  ),


  shinyBS::bsTooltip(
    id = "crbh",
    title = "You can examine the change in the Cronbach Alpha coefficient when the items are removed or added.",
    placement = "bottom",
    trigger = "hover"
  ),


  shinyBS::bsTooltip(
    id = "dlDistractor",
    title = "Downloading graphs might take some time according to number of items",
    placement = "bottom",
    trigger = "hover"
  ),

  ## Main Frame of User Interface ##

  # titlePanel("ITEM AND DISTRACTOR ANALYSIS"),

  # h1(id="title", "ITEM AND RELIABILITY ANALYSIS"),
  # tags$style(HTML("#title{color: black; font-family: Arial;font-size: 35px;
  #           font-style: oblique;text-align:left}")),


  div(id = "tepe",
      fluidRow(

        column(6,

               h1(id="title", "ITEM AND RELIABILITY ANALYSIS (CTT)"),
               tags$style(HTML("#title{color: black; font-family: font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;font-size:30px;
            font-style: oblique;text-align:left}"))

        )  ,

        column(6,
               h1(id="title2", "RSP PACKAGE  - CRAN"),
               tags$style(HTML("#title2{color: black; font-family: font-family: 'Helvetica Neue', 'Lucida Grande', Helvetica, Arial, sans-serif;font-size:15px;
            font-style: oblique;text-align:right}"))

            # imageOutput("imagex",width = "15%", height = "30px", inline = TRUE),

        )

      )), # close fluidrow

  sidebarPanel(width = 4,

               ## CONDITIONAL PANEL 1 - Introduction ##

               conditionalPanel(
                 condition = "input.panel==0",
                 shiny::img(src = "img/rsp5.png", width = "97%"),
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
                 imageOutput("image1",width = "75%", height = "100px", inline = TRUE),
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

               ## CONDITIONAL PANEL 2 - Data Upload ##

               conditionalPanel(
                 condition = "input.panel==1",
                 shiny::img(src = "img/rsp5.png", width = "97%"),

                 ######################################################################################
                 imageOutput("image2",width = "75%", height = "100px", inline = TRUE),
                 ######################################################################################

                 br(),

                 shinyWidgets::radioGroupButtons(
                   inputId = "type",
                   label =  h3(id="ab","Select Data Type"),
                   choices = list("Option Matrix" = 1, "1-0 Matrix" = 2),

                   justified = TRUE,
                   checkIcon = list(
                     yes = icon("ok",
                                lib = "glyphicon"))),

                 div(  style="color:red;",

                       HTML( "<marquee direction='left' scrollamount = '5'>  THE FIRST LINE OF THE OPTION MATRIX SHOULD BE THE ANSWER KEY!!!

               </marquee>"  )),

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
               fileInput(
                 "data1",
                 h3(id="ab", "Uplad Data File")
               )


               ),

               ## CONDITIONAL PANEL 3 - Item Analysis ##

               conditionalPanel(
                 condition = "input.panel==2",
                 shiny::img(src = "img/rsp5.png", width = "97%"),

                 ######################################################################################
                 imageOutput("image3",width = "75%", height = "100px", inline = TRUE),
                 ######################################################################################

                 br(),
                 br(),
                 gt::gt_output("crbh"),
                 br(),

                 uiOutput("rmItem"),

                 br(),

                 fluidRow(

                   column(4),

                   column(4,
                          uiOutput("uyg") ),

                   column(4)

                 ), # close fluid


                 ###### DROPDOWN ####


                 br(),

                 shinyWidgets::dropMenu(

                   padding = "20px",

                   theme="light-border",

                   placement = "right-end",


                   shinyWidgets::actionBttn(
                     inputId = "acb2",
                     label = "CLICK TO SEE TEST STATISTICS AND REALIABILITY ANALYSIS",
                     style = "jelly",
                     color = "primary"

                   ),

                   withLoader(
                     gt::gt_output("table3"),
                     type = "html",
                     loader = "loader1"
                   ),

                   br(),
                   br(),

                   gt::gt_output("table3.1"),
                   br(),
                   br(),
                   gt::gt_output("table3.2"),
                   br(),
                   br()



                 ), # close dropmenu



                 #####


                 br(),

                 br(),


                 fluidRow(
                   column(
                     6,



                     shinyWidgets::downloadBttn(
                       outputId = "dlItemAn",
                       h1(id="b", "DOWNLOAD ITEM ANALYSIS RESULTS"),
                       style = "unite",
                       color = "primary",
                       size = "sm",
                       block = FALSE,
                       no_outline = TRUE,
                       icon = shiny::icon("download")
                     ),



                   ),
                   column(
                     6,

                     shinyWidgets::downloadBttn(
                       outputId = "downloadTestIst",
                       h1(id="b","DOWNLOAD TEST STATISTICS"),
                       style = "unite",
                       color = "primary",
                       size = "sm",
                       block = FALSE,
                       no_outline = TRUE,
                       icon = shiny::icon("download")
                     ),

                   )
                 ),




               ),   # close conditional panel



               ## CONDITIONAL PANEL 5 - Graphics ##

               conditionalPanel(
                 condition = "input.panel==4",
                 shiny::img(src = "img/rsp5.png", width = "97%"),


                 ######################################################################################
                 #imageOutput("image5",width = "75%", height = "100px", inline = TRUE),
                 ######################################################################################

                 br(),
                 br(),
                 uiOutput("graphitem"),

                 br(),

                 shinyWidgets::radioGroupButtons(
                   inputId = "distGraph",
                   label =  h3(id="ab","Select Graph Type"),
                   choices = list( "Distractors' Percentage of Selection" = 1,
                                   "Discrimination of Distractors" = 2),
                   justified = TRUE,
                   direction = "vertical",
                   status = "primary",
                   checkIcon = list(
                     yes = icon("ok",
                                lib = "glyphicon"))

                 ),



                 br(),
                 br(),

                 # uiOutput ("dlDistractor"),


               ),  # close conditional panel



               ## CONDITIONAL PANEL 7 - Output ##

               conditionalPanel(
                 condition = "input.panel==6",
                 br(),
                 br(),
                 shiny::img(src = "img/rsp5.png", width = "97%"),
                 ######################################################################################
                 #imageOutput("image7",width = "75%", height = "100px", inline = TRUE),
                 ######################################################################################

                 br(),
                 shiny::img(src = "img/download.gif", width = "97%")

                 ######################################################################################
                 #imageOutput("image8",width = "75%", height = "100px", inline = TRUE)
                 ######################################################################################
               )
  ),


  ## MAIN PANEL ##

  mainPanel(

    ## M.PANEL 1 - Introduction ##

    tabsetPanel(
      id = "panel",
      tabPanel(

        h4(id="a", "INTRODUCTION"),
        value = 0,
        br(),
        br(),
        br(),
        # fluidRow(
        #   column(
        #     12,
        #     align="center",
        #     shiny::img(src = "img/B2K.png", width = "90%")
        #   )
        # )

        shiny::img(src = "img/rsp5.png", width = "97%"),
        ######################################################################################
        imageOutput("image0",width = "75%", height = "100px", inline = TRUE),
        ######################################################################################

      ),


      ## M.PANEL 2 - Data Upload ##

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

        br(),

        withLoader(
          DT::dataTableOutput("dat1"),
          type = "html",
          loader = "loader1"
        ),

        br(),


        textOutput("dat3"),
        tags$head(
          tags$style(
            "#dat3{
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
          gt::gt_output("dat2"),
          type = "html",
          loader = "loader5"
        )
      ),

      ## M.PANEL 3 - ıtem Analysis ##


      tabPanel(
        h4(id="a","ITEM ANALYSIS & TEST STATISTICS"),
        value = 2,
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

        br(),
        br(),

        gt::gt_output("buton"),

        withLoader(
          gt::gt_output("table1"),
          type = "html",
          loader = "loader1"
        ),


        gt::gt_output("print")

      ),


      ## M.PANEL 5 - Graphics ##

      tabPanel(
        h4(id="a","GRAPHICS"),
        value = 4,
        br(),
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
        br(),
        br(),
        br(),
        br(),
        textOutput("warning2"),
        tags$head(
          tags$style(
            "#warning2{
            color: red;
            font-size: 25px;
            font-family: cursive;
            font-style: oblique;
            text-align:center;
            letter-spacing:1px;
            }"
          )
        ),
        withLoader(
          plotOutput("graph"),
          type = "html",
          loader = "loader1"
        ),

        br(),

        br(),

        uiOutput ("dlDistractor"),


      ), # close tab panel



      ## M.PANEL 7 - Output ##

      # tabPanel(
      #   h4(id="a", "OUTPUT"),
      #   value = 6,
      #   br(),
      #   br(),
      #   textOutput("text6"),
      #   tags$head(
      #     tags$style(
      #       "#text6{
      #       color: darkblue;
      #       font-size: 25px;
      #       font-family: cursive;
      #       font-style: oblique;
      #       text-align:center;
      #       letter-spacing:1px;
      #       }"
      #     )
      #   ),
      #   br(),
      #   br(),
      #
      #
      #
      #
      #  gt::gt_output("print"),
      #
      #
      #
      #
      #
      #   br(),
      #   br(),
      #   # withLoader(
      #
      #   # uiOutput ("dlDistractor"),
      #   # type = "html",
      #   #loader = "loader1"
      #   # )
      #
      # ) # closa tab panel


    )
  )
  )

  ################################ SERVER ##############################

  server <- function(input, output, session) {






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
        paste0("")
      }
      # contains the value returned by the JS function
    })

    ###########################################################################

    output$image0<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp5.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image1<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp5.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image2<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp5.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image3<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp5.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image4<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp5.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image5<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp5.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image6<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp5.png", contentType = "image/png")
    },
    deleteFile = FALSE)


    output$image7<- renderImage({
      resim2 <- tempfile(fileext = '.png')
      list(src = "rsp5.png", contentType = "image/png")
    },
    deleteFile = FALSE)

    output$image8<- renderImage({
      resim2 <- tempfile(fileext = '.gif')
      list(src = "download.gif", contentType = "image/gif")
    },
    deleteFile = FALSE)



    ###########################################################################


    # observeEvent(input$data1, {
    #   show_alert(
    #     title = "Success !!",
    #     text = "DATA WAS UPLOADED",
    #     type = "success"
    #   )
    # })
    #

    ########################    DATA UPLAD     ########################

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

    ################################ DATA PRESENTATION TAB-2 #####################

    output$text1<- renderText({
      if (!is.null(input$data1)){
        paste0("DATA UPLOAD AND BASIC STATISTICS")
      }
    })

    output$dat1 <- DT::renderDataTable({
      if(!is.null(input$data1)){
        data <-data()
        colnames(data) <- paste0("item", 1:ncol(data))
        if(dim(data)[2] == 1){
          data.frame(WARNING = "INCORRECT FILE FORMAT")
        } else {
          if (ncol(data) <= 15)
          {
            data[1:10, 1:ncol(data)]
          } else {
            data[1:10, 1:15]
          }
        }
      }
    })


    output$dat2 <- render_gt(
      align = "center", {
        if (!is.null(input$data1)){
          data <- (data())
          NUMBER_OF_ITEMS <- ncol(data)
          NUMBER_OF_RESPONDENTS <- nrow(data)
          NUMBER_OF_BLANK_ITEMS <- length(which(is.na(data)))
          res <- data.frame(NUMBER_OF_ITEMS,   NUMBER_OF_RESPONDENTS,  NUMBER_OF_BLANK_ITEMS)
          res<- gt::gt(res)
          br()
          br()
          br()
          res <- res %>%
            tab_header(
              title = md("*Basic Statistics About the Data Set*")
            )
          res <- res %>%
            tab_style(
              style = cell_fill(
                color = sample(colors()[3:100], 1), alpha=0.15),
              locations = cells_body()
            )

          res <- res %>%
            cols_width(
              c(NUMBER_OF_ITEMS) ~ gt::px(250),
              everything() ~ gt::px(250)
            )

        }
      })

    output$dat3 <- renderText({
      if (!is.null(input$data1))
      {
        paste0("DATA WAS UPLOADED SUCCESSFULLY")
      }

    })

    #################### ITEM ANALYSIS - TAB 3 #########################

    output$text2<- renderText({
      if (!is.null(input$data1)){
        paste0("ITEM ANALYSIS RESULTS")
      }
    })

    ##### REACTIVE ######

    keyA<- reactive({
      data<-data()
      keyA <- as.matrix(data[1,]) })


    datA<-reactive({
      data<-data()

      if ( input$type==1) {

        colnames(data) <- paste0("item", 1:ncol(data))
        datA<-data[-1,] }

      else

      {

        colnames(data) <- paste0("item", 1:ncol(data))
        datA<-data


      }
    })

    names<-reactive (colnames(datA()))

    # # Item Selection #
    #
    output$rmItem<- renderUI({
      if (!is.null(input$data1)) {
        # data <- data()
        # colnames(data) <- paste0("item", 1:ncol(data))
        shinyWidgets::pickerInput("itm",
                    h3(id="ab", "Select Item"),
                    choices = names(),
                    options = list(
                      `actions-box` = TRUE),
                    multiple = TRUE )


      }
    })


    ############################## TABLE ITEM ANALYSIS  ##########

    output$table1 <- render_gt(
      align = "center", {

        req(input$data1)


        # USING FUNCTION #

        itemAnalysis1<-  ITEMAN_ENV$itemAnalysis1

        itemAnalysis2<-  ITEMAN_ENV$itemAnalysis2

        if (input$type == 2) {
          ITEMAN_ENV$itemResult <- itemAnalysis2(data = data())
        } else {

          ITEMAN_ENV$itemResult <-itemAnalysis1(datA(),keyA()) }

      })

    ############ OBSERVE EVENT ###################

    ### for datAKalanGL REACTIVE
    #
    datARem<-reactive({

      omit <- input$itm
      all <- names()
      ITEMAN_ENV$remain_items <- setdiff(all, omit)

      remain_items2<- setdiff(all, omit)

      if(input$type == 1){


        ITEMAN_ENV$datA_Rem <- datA()[, remain_items2]  }
      else
      {
        ITEMAN_ENV$datA_Rem <- datA()[, remain_items2] }
    })
    #

    remainItem<- reactive({

      omit <- input$itm
      all <- names()
      remain_items <- setdiff(all, omit)
      return(remain_items)
    })


    keyARem<-reactive({

      omit <- input$itm
      all <- names()
      ITEMAN_ENV$remain_items <- setdiff(all, omit)
      keyA <- as.matrix(keyA())
      colnames(keyA) <- all
      keyA_Rem <- keyA[,ITEMAN_ENV$remain_items]

    })

    observeEvent(input$omit, {


      output$buton <- render_gt(align="center", {


        ## Observe Event Function 2 ##



        ## Observe event - using the functions ##


        omit <- input$itm
        all <- names()
        remain_items <- setdiff(all, omit)



        itemAnalysis2_2<-   ITEMAN_ENV$itemAnalysis2_2


        itemAnalysis1_2<-   ITEMAN_ENV$itemAnalysis1_2


        if(input$type == 1){


          datA_Rem <- datA()[, remain_items]  }
        else
        {
          datA_Rem <- datA()[, remain_items] }


        if (input$type == 2) {

          res1 <- itemAnalysis2_2(data = datA_Rem)
        } else {
          res1 <- itemAnalysis1_2(datA_Rem, keyARem())
        }

        res1 <- res1[-1]
        res1 <- cbind(remain_items, res1)

        res1<-as.data.frame(res1)



        shinyjs::hide("table1")
        shinyjs::hide("omit")

        res1<-gt::gt(res1)

        res1<- res1 %>%

          tab_header(
            title= md("**ITEM ANALYSIS RESULTS**")

          )

        res1<- res1 %>%

          tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = c(remain_items)

            ))

        res1<- res1 %>%

          tab_style(
            style = cell_text(weight = "bolder",color="red",
                              decorate="underline",
                              # stretch="extra-expanded"
                              ),
            locations = cells_body(
              columns = c(Item_Difficulty),
              rows =Item_Difficulty >= 0.80 | Item_Difficulty <= 0.25

            ))

        res1<- res1 %>%

          tab_style(
            style = cell_text(weight = "bolder",color="red",
                              decorate="underline",
                              # stretch="extra-expanded"
                              ),
            locations = cells_body(
              columns = c(Point_Biserial),
              rows =Point_Biserial <= 0.25

            ))

        res1<- res1 %>%

          tab_style(
            style = cell_text(weight = "bolder",color="red",
                              decorate="underline",
                              # stretch="extra-expanded"
                              ),
            locations = cells_body(
              columns = c(Biserial),
              rows =  Biserial <= 0.25

            ))

        res1<-res1 %>%

          cols_width(
            c(Point_Biserial) ~ gt::px(150),
            everything() ~ gt::px(150)
          )

        res1<- res1 %>%

          tab_options(
            column_labels.font.size=gt::px(17),
            column_labels.font.weight="bold"
          )

        ITEMAN_ENV$RemovedItemResult <- res1 %>%

          tab_style(
            style = cell_fill(color = sample(colors()[3:100],1),alpha=0.20),

            locations = cells_body()
          )


        # ITEMAN_ENV$remdatx<<-ITEMAN_ENV$RemovedItemResult[["_data"]]

        return(ITEMAN_ENV$RemovedItemResult)
      })


    })

    #######################  DISTRACOR ANALYSIS ##########################

    ##  DISTRACOR ANALYSIS ##

    output$text3<- renderText({

      if (!is.null(input$data1)) {
        paste0 ( "DISTRACTOR ANALYSIS RESULTS")
      }
    })
    output$warning1<- renderText({

      if(input$type == 2){
        paste0 ( "Distractor Analysis Cannot Be Performed for Items Scored as 1-0!")
      }
    })


    ############################### GRAPH #################################

    output$text5<- renderText({
      if (!is.null(input$data1)) {
        paste0 ("GRAPHS ABOUT DISTRACTOR ANALYSIS")
      }

    })
    output$warning2<- renderText({

      if(input$type == 2){
        paste0 ( "Graphs of distractor analysis cannot be
              performed for items scored as 1-0!")
      }
    })


    ## distGraph

    output$graph<-renderPlot ({

      if( is.null(input$data1)) { return(NULL)}
      else{
        if (input$type == 2) {

          return(NULL)

        } else {

          if(input$distGraph==2) {

            plotDistractorAnalysis(datARem(), keyARem(), item=input$graphItem)

          }

          else

          {
            lisT <- list()

            for (i in 1:ncol(datARem()))  {

              lisT[[i]] <- prop.table(table(datARem()[, i]))
            }
            colNames<-as.vector(colnames(datARem()))

            barplot(lisT[[which(colNames==input$graphItem)]],ylim=c(0,1),

                    xlab="Options",

                    ylab="Marking Rate", main= paste0(input$graphItem),col=c(2,3,4,5) )

          }

        }
      }



    })



    ######################## TEST STATISTICS #################################


    # DaTa<-reactive({
    #
    #   if (input$type == 1) {
    #
    #
    #
    #     data_01 <- as.data.frame(
    #       matrix(
    #         nrow = nrow(datA()),
    #         ncol = ncol(datA())
    #       ))
    #
    #     for (i in 1:ncol(data_01)){
    #       data_01[, i] <- ifelse(datA()[, i] == keyA()[i], 1, 0)
    #       if (length(which(is.na(data_01[, i]))) != 0) {
    #         data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
    #       }
    #     }
    #
    #   } else {
    #
    #     data_01<-data()
    #
    #
    #     return(data_01)
    #
    #   }



    #  })







    output$text4<- renderText({


      if (!is.null(input$data1)) {
        paste0 ("TEST STATISTICS AND RELIABILITY ANALYSIS")
      }

    })
    output$table3 <- render_gt(align = "left", { height = gt::px(1000)
    width = gt::px(1000)

    ## Two Halves Reliability ##
    if( is.null(input$data1)) { return(NULL)}

    two.halves <- function(x, y = 1) {
      oddEven <- function(x) {
        Length <- ncol(x)
        colnames(x) <- 1:Length
        one <- x[, seq(1, Length, 2)]
        two <- x[, seq(2, Length, 2)]
        list(one, two)
      }

      random <- function(x) {
        n <- ncol(x)
        colnames(x) <- paste0("item", 1:n)
        nn <- n / 2
        x1 <- colnames(x)

        ifelse(n %% 2 == 0, s1 <- sample(x1, nn),
               s1 <- sample(x1, (n + 1) / 2))

        s2 <- which(!x1  %in%  s1)

        one <- x[, s1]

        two <- x[, s2]
        list(one, two)
      }

      ifelse(y == 1, half <- oddEven(x), half <- random(x))

      sum1 <- rowSums(half[[1]])
      sum2 <- rowSums(half[[2]])
      correlation <- cor(sum1, sum2)
      result <- (2 *  correlation) / (1 +  correlation)

      return(result)

    }
    ## Cronbach Alpha Reliability ##

    cronbach <- function(x) {
      n <- ncol(x)

      numerator  <- sum(apply(x, 2, var))

      total <- rowSums(x)

      denominator <- var(total)

      res <- n / (n - 1) * (1 - (numerator / denominator))

      return(res)

    }

    ## KR20 Reliability ##

    kr20 <- function(x) {
      n <- ncol(x)

      pq <- function(x) {
        p <- mean(x)
        q <- 1 - p

        res <- p * q
        return (res)
      }

      numerator  <- sum(apply(x, 2, pq))

      total <- rowSums(x)

      denominator<- var(total)

      result <- n / (n - 1) * (1 - (numerator /  denominator))

      return(result)
    }

    ##  Skewness ##

    skewness <- function(x) {
      numerator <- sum((x - mean(x)) ^ 3)

      denominator <- length(x) * (sd(x) ^ 3)

      result <- numerator / denominator

      return(result)
    }

    ## Kurtosis ##

    kurtosis <- function(x) {
      numerator <- sum((x - mean(x)) ^ 4)

      denominator<- length(x) * (sd(x) ^ 4)

      result <- (numerator / denominator) - 3

      return(result)

    }

    ##### First Row ####

    if (is.null(input$itm)) {

      if (input$type == 1) {

        data_01 <- as.data.frame(
          matrix(
            nrow = nrow(datA()),
            ncol = ncol(datA())
          ))

        for (i in 1:ncol(data_01)){
          data_01[, i] <- ifelse(datA()[, i] == keyA()[i], 1, 0)
          if (length(which(is.na(data_01[, i]))) != 0) {
            data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
          }
        }

      } else {

        data_01<-data()

      } }
    else

    { if (input$type == 1) {
      data_01 <- as.data.frame(
        matrix(
          nrow = nrow(datARem()),
          ncol = ncol(datARem())
        ))
      for (i in 1:ncol(data_01)){
        data_01[, i] <- ifelse(datARem()[, i] == keyARem()[i], 1, 0)
        if (length(which(is.na(data_01[, i]))) != 0) {
          data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
        }
      }

    } else {

      data_01<-datARem()
    }
    }

    two <- two.halves(na.omit(data_01), y = 1)
    cr <- cronbach(na.omit(data_01))
    krtw <- kr20(na.omit(data_01))
    skew<-skewness(rowSums(na.omit(data_01)))
    kurt<-kurtosis(rowSums(na.omit(data_01)))


    ist <- data.frame(
      Two_halves = two,
      Cronbach_Alpha = cr,
      KR20 = krtw,
      Skewness = skew,
      Kurtosis = kurt
    )


    ITEMAN_ENV$saveist.1 <- ist
    ist<-gt::gt(ist)


    ist <- ist %>% tab_header(
      title= md("**TEST STATISTICS**")
    )

    ist <- ist %>%

      tab_options(
        heading.title.font.size = gt::px(25),
        column_labels.font.size=gt::px(17),
        column_labels.font.weight="bold" )

    ist<-ist %>%

      tab_style(
        style = cell_fill(color = sample(colors()[3:100],1),alpha=0.20),

        locations = cells_body()
      )

    ist<-ist %>%

      cols_width(
        everything() ~ gt::px(220))


    return(ist)

    })

    #### Second Row ####

    output$table3.1<-render_gt(align = "left", {height = gt::px(1000)
    width = gt::px(5000)

    if( is.null(input$data1)) { return(NULL)}



    if (is.null(input$itm)) {

      if (input$type == 1) {

        data_01 <- as.data.frame(
          matrix(
            nrow = nrow(datA()),
            ncol = ncol(datA())
          ))

        for (i in 1:ncol(data_01)){
          data_01[, i] <- ifelse(datA()[, i] == keyA()[i], 1, 0)
          if (length(which(is.na(data_01[, i]))) != 0) {
            data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
          }
        }

      } else {

        data_01<-data()

      } }
    else

    { if (input$type == 1) {
      data_01 <- as.data.frame(
        matrix(
          nrow = nrow(datARem()),
          ncol = ncol(datARem())
        ))
      for (i in 1:ncol(data_01)){
        data_01[, i] <- ifelse(datARem()[, i] == keyARem()[i], 1, 0)
        if (length(which(is.na(data_01[, i]))) != 0) {
          data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
        }
      }

    } else {

      data_01<-datARem()
    }
    }

    Number_of_items<- ncol(data_01)
    N<-nrow(na.omit(data_01))
    Mean<- mean(rowSums(na.omit(data_01)))
    SD<- sd(rowSums(na.omit(data_01)))
    Variance <- var(rowSums(na.omit(data_01)))

    ist<-data.frame(N,Number_of_items, Mean ,SD,Variance)


    ITEMAN_ENV$saveist.2 <- ist
    ist<-gt::gt(ist)

    ist<-ist %>%

      tab_style(
        style = cell_fill(color = sample(colors()[3:100],1),alpha=0.20),

        locations = cells_body()
      )

    ist<-ist %>%

      cols_width(
        everything() ~ gt::px(220))


    ist <- ist %>%

      tab_options(
        column_labels.font.size=gt::px(17),
        column_labels.font.weight="bold"
      )

    return(ist)

    })

    ## Third Row ##

    output$table3.2<-render_gt(align = "left", {height = gt::px(1000)
    width = gt::px(5000)

    if( is.null(input$data1)) { return(NULL)}


    if(!is.null(input$itm)){


      Average_Difficulty<- mean(ITEMAN_ENV$RemovedItemResult[["_data"]]$Item_Difficulty )
      Average_Biserial<-mean(ITEMAN_ENV$RemovedItemResult[["_data"]]$Biserial)
      Average_P_Biserial<-mean(ITEMAN_ENV$RemovedItemResult[["_data"]]$Point_Biserial)
    } else {
      Average_Difficulty<- mean(ITEMAN_ENV$itemResult[["_data"]]$Item_Difficulty )
      Average_Biserial<-mean(ITEMAN_ENV$itemResult[["_data"]]$Biserial)
      Average_P_Biserial<-mean(ITEMAN_ENV$itemResult[["_data"]]$Point_Biserial)
    }




    ##

    if (is.null(input$itm)) {

      if (input$type == 1) {

        data_01 <- as.data.frame(
          matrix(
            nrow = nrow(datA()),
            ncol = ncol(datA())
          ))

        for (i in 1:ncol(data_01)){
          data_01[, i] <- ifelse(datA()[, i] == keyA()[i], 1, 0)
          if (length(which(is.na(data_01[, i]))) != 0) {
            data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
          }
        }

      } else {

        data_01<-data()

      } }
    else

    { if (input$type == 1) {
      data_01 <- as.data.frame(
        matrix(
          nrow = nrow(datARem()),
          ncol = ncol(datARem())
        ))
      for (i in 1:ncol(data_01)){
        data_01[, i] <- ifelse(datARem()[, i] == keyARem()[i], 1, 0)
        if (length(which(is.na(data_01[, i]))) != 0) {
          data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
        }
      }

    } else {

      data_01<-datARem()
    }
    }

    ##

    Nitem<- ncol(data_01)
    Minimum<- min(rowSums(na.omit(data_01)))
    Maximum<- max(rowSums(na.omit(data_01)))


    ist<-data.frame(Minimum,Maximum,  Average_Difficulty,
                    Average_Biserial,Average_P_Biserial)

    ITEMAN_ENV$saveist.3 <- ist
    ist<-gt::gt(ist)

    ist<-ist %>%

      tab_style(
        style = cell_fill(color = sample(colors()[3:100],1),alpha=0.20),

        locations = cells_body()
      )


    ist<-ist %>%

      cols_width(
        everything() ~ gt::px(220))

    ist <- ist %>%

      tab_options(
        column_labels.font.size=gt::px(17),
        column_labels.font.weight="bold"
      )


    return(ist)

    })

    ################## PRINT OUT ############################

    output$text6<- renderText({

      if (!is.null(input$data1)) {
        paste0 ("DOWNLOAD ANALYSIS RESULTS")
      }
    })


    ######################## ??????????????????? #############################

    output$print<-render_gt({


      ################################# EK HESAPLAMA ####################


      omit <- input$itm
      all <- names()
      remain_items <- setdiff(all, omit)



      itemAnalysis2_2<-   ITEMAN_ENV$itemAnalysis2_2


      itemAnalysis1_2<-   ITEMAN_ENV$itemAnalysis1_2


      if(input$type == 1){


        datA_Rem <- datA()[, remain_items]  }
      else
      {
        datA_Rem <- datA()[, remain_items] }


      if (input$type == 2) {

        res1 <- itemAnalysis2_2(data = datA_Rem)
      } else {
        res1 <- itemAnalysis1_2(datA_Rem, keyARem())
      }

      res1 <- res1[-1]
      res1 <- cbind(remain_items, res1)

      res1<-as.data.frame(res1)

      rmdat<- res1

      ITEMAN_ENV$rmdat<-res1


      ####################################################################



      output$dlItemAn <- downloadHandler(

        filename = function() {
          "item_analysis.csv"
        },

        if (!is.null(input$itm)) {



          content = function(file) {

            write.csv2(   ITEMAN_ENV$rmdat, file)  }


        } else

        { content = function(file) {
          write.csv2(ITEMAN_ENV$itemResult[["_data"]], file) }

        }


      )


      output$downloadTestIst <- downloadHandler(
        filename = function() {
          "test_statistics.csv"
        },
        content = function(file) {


          r1<- as.data.frame(ITEMAN_ENV$saveist.1 )
          r2<- as.data.frame (ITEMAN_ENV$saveist.2 )
          r3<- as.data.frame (ITEMAN_ENV$saveist.3 )

          stat_dat<-  cbind(r1,r2,r3)

          stat_dat<- t(stat_dat[,-1])

          colnames(stat_dat)<- "Value"

          stat_dat<-as.data.frame(stat_dat)

          write.csv2(stat_dat, file)
        }
      )


      output$downloadGraph <- downloadHandler(
        filename = function() {
          "graphs.pdf"
        },
        content = function(file) {
          pdf(file)
          listx <- NULL
          # GRAFİK GEÇ DOWNLOAD OLUYOR

          if(input$distGraph==2) {

            print(plotDistractorAnalysis(datARem(), keyARem(),
                                         item="all"))

          }

          else

          {

            for (i in 1:ncol(datARem()))  {
              print(barplot(prop.table(table(datARem()[, i])),
                            ylim=c(0,1),
                            xlab="options",
                            ylab="marking rate",
                            main= paste0(colnames(datARem()[i])),
                            col=c(2,3,4,5))) }


          }

          dev.off()
        }
      )
      return(NULL)

    })


    #### Alpha Valvure for Item Anlysis Tab ###

    output$crbh<-render_gt({

      req(input$data1)

      if (is.null(input$itm)) {

        if (input$type == 1) {

          data_01 <- as.data.frame(
            matrix(
              nrow = nrow(datA()),
              ncol = ncol(datA())
            ))

          for (i in 1:ncol(data_01)){
            data_01[, i] <- ifelse(datA()[, i] == keyA()[i], 1, 0)
            if (length(which(is.na(data_01[, i]))) != 0) {
              data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
            }
          }

        } else {

          data_01<-data()

        } }
      else

      { if (input$type == 1) {
        data_01 <- as.data.frame(
          matrix(
            nrow = nrow(datARem()),
            ncol = ncol(datARem())
          ))
        for (i in 1:ncol(data_01)){
          data_01[, i] <- ifelse(datARem()[, i] == keyARem()[i], 1, 0)
          if (length(which(is.na(data_01[, i]))) != 0) {
            data_01[which(is.na(data_01[, i]) == TRUE), i] = 0
          }
        }

      } else {

        data_01<-datARem()
      } }

      cronbach <- function(x) {
        n <- ncol(x)
        numerator  <- sum(apply(x, 2, var))
        total <- rowSums(x)
        denominator <- var(total)
        res <- n / (n - 1) * (1 - (numerator / denominator))
        return(res)}

      cr <- cronbach(na.omit(data_01))

      stat <- data.frame(

        Cronbach_Alpha = cr)

      stat<-gt::gt(stat)

      stat <- stat %>%
        tab_header(
          title =
            md("*Current Cronbach Alpha Coefficient*")
        )

      stat<-stat %>%

        tab_style(
          style = cell_fill(color = sample(colors()[3:100],1),alpha=0.20),

          locations = cells_body()
        )

      return(stat)

    })

    ## RENDER UI ##

    output$graphitem <- renderUI({
      if (!is.null(input$data1)) {
        if(is.null(input$itm)){
          omit <- NULL
          remv <- names()
        } else{

          remv<- ITEMAN_ENV$remain_items
        }

        shinyWidgets::pickerInput("graphItem",
                    h4("Select Item Number"),
                    choices = remv,
                    multiple = FALSE)
      }
    })



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
      }
    })


    ## Download Widgets ##

    output$dlDistractor <- renderUI({
      req(input$type==1)
      fluidRow(
        column(
          4,

        ),
        column(
          8,


          shinyWidgets::downloadBttn(
            outputId = "downloadGraph",
            h1(id="b", "DOWNLOAD DISTRACTOR ANALYSIS GRAPHICS"),
            style = "unite",
            color = "primary",
            size = "sm",
            block = FALSE,
            no_outline = TRUE,
            icon = shiny::icon("download")
          ),


        )
      )
    })

    ## Remove Selected Item Button ##

    # output$uyg <- renderUI({
    #   if (!is.null(input$itm)) {
    #     actionButton("omit", h4("Remove Selected Items"))
    #
    # })


    output$uyg <- renderUI({

      if (!is.null(input$itm)) {
        shinyWidgets::actionBttn(
          inputId = "omit",
          label = h3(id="ab","Remove Selected Item/s"),
          size = "lg",
          color = "default", no_outline = TRUE
        )
      }
    })

    session$onSessionEnded(function() {
      stopApp()
    })

    Biserial <- Item_Difficulty <- Items <- ItemS <- Point_Biserial <- NULL

  }


  shinyApp(ui = ui, server = server)
  }




