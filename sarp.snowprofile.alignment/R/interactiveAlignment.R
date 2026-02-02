#' Run interactive alignment app
#'
#' This app allows to interactively explore the alignment of two snowprofiles, which are either
#' given as input to this function, or are uploaded to the app interactively as caaml files.
#' Example profiles are also provided in the app.
#'
#' @import grid
#' @import sarp.snowprofile
#'
#' @param query an optional query snowprofile
#' @param ref an optional reference snowprofile
#'
#' @return An interactive session will be started
#'
#' @examples
#' if (FALSE){  # this example won't be started in package tests.
#'
#' ## start app and choose profiles from within the app:
#' interactiveAlignment()
#'
#' ## start app with package internal profile data (from `sarp.snowprofile`):
#' interactiveAlignment(query = SPpairs$A_modeled, ref = SPpairs$A_manual)
#'
#' }
#'
#' @author fherla
#'
#' @export
interactiveAlignment <- function(query = NaN, ref = NaN) {


  ## --- handle input profiles ----
  if (!all(is.na(query)) && !all(is.na(ref))) {  # start app with input profiles
    values <- shiny::reactiveValues(providedInput = TRUE, query = query, ref = ref)
  } else {
    values <- shiny::reactiveValues(providedInput = FALSE)
  }
  ## --- UI ----
  ui <- shiny::fluidPage(
    shiny::withMathJax(),
    shiny::tags$head(shiny::tags$style(type="text/css", ".container-fluid {  max-width: 1300px; }"),
                     shiny::tags$head(shiny::tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            '))
              ),
    shiny::headerPanel("DTW snow profile alignment app"),
    shiny::fluidRow(
      shiny::column(4, offset = 0,
             ## Source - Select whether to browse
             shiny::conditionalPanel(condition = "!output.onInput",
                                     shiny::selectInput(inputId = "source", label = "Source",
                                                 choices = c("1) Variable local warping", "2) Pre/post storm", "3) Human vs modeled",
                                                       "Browse for your own CAAML files",
                                                       "Browse for your own PRF file"),
                                                 selected = "1) Variable local warping"),
                              ## Action button
                              shiny::actionButton("do", "Load Profiles", style = "background-color: silver;  color:navy"),
                              shiny::br(),
                              shiny::hr()
                             ),
             shiny::h4("Profile Settings"),
             ## Reverse checkbox
             shiny::checkboxInput("reverse", "Switch query and reference", value = FALSE),
             ## Scale profiles to equal heights before alignment
             shiny::checkboxInput("initialScaling", "Scale profiles to equal height before aligning", value = FALSE),
             ## resampling rate:
             shiny::checkboxInput("resampling", "Resample profiles onto regular grid", value = TRUE),
             shiny::conditionalPanel(condition = "input.resampling",
                                     shiny::sliderInput("resamplingRate", "sampling rate (cm)", min = 0.25, max = 5, value = 0.5, step = 0.25)),
      ),
      shiny::column(8,
             ## Browse CAAML or PRF files:
             shiny::conditionalPanel(condition = "(!output.onInput && input.source == 'Browse for your own CAAML files')",
                                     shiny::div(style = "display: inline-block; vertical-align: top;",
                                                shiny::fileInput("queryFile", shiny::h5("Query"), accept = c(".caaml"))),
                                     shiny::div(style = "display: inline-block; vertical-align: top; height: 1px; width: 25px"),
                                     shiny::div(style = "display: inline-block; vertical-align: top;",
                                                shiny::fileInput("refFile", shiny::h5("Reference"), accept = c(".caaml")))
                              ),
             shiny::conditionalPanel(condition = "(!output.onInput && input.source == 'Browse for your own PRF file')",
                                     shiny::div(style = "display: inline-block; vertical-align: top;",
                                                shiny::fileInput("prfFile", shiny::h5("PRF file"), accept = c(".prf"))),
                                     shiny::helpText("Note, the PRF file needs to contain at least two profiles, the first and second of which will be the query and reference profiles.")
                              )
      ),
      shiny::column(12, shiny::hr())
    ),

    shiny::sidebarPanel(
      # tags$head(tags$style(type="text/css", "select { min-width: 300px; max-width: 300px;}"),
      #           tags$style(type="text/css", ".span4 { min-width: 320px; max-width: 320px;}"),
      #           tags$style(type="text/css", ".well { min-width: 300px; max-width: 300px;}")
      #           ),
      width = 3,
      shiny::h4("Alignment Settings"),
      shiny::checkboxInput("openEnd", "Open End alignment", value = TRUE),
      shiny::conditionalPanel(condition = "input.openEnd",
                              shiny::checkboxInput("checkGlobal", "Check global alignment", value = TRUE)),
      shiny::radioButtons("alignDir", "Direction of alignment",
                   choices = list("Bottom-up (BU)", "Top-down (TD)", "BU/TD"),
                   selected = "BU/TD"),
      ## use weighted grain Similarity matrix:
      shiny::checkboxInput("layerWeighting", "Apply a layer weighting scheme to the grain similarity matrix for preferential layer matching", value = TRUE),
      shiny::checkboxInput("ddate", "Add deposition date info", value = FALSE),
      shiny::conditionalPanel(condition = "input.ddate",
                              shiny::sliderInput("ddateNorm",
                                   label = "Date normalization factor (unit: days)",
                                   min = 1, max = 20, value = 3)
      ),

      ## Set weights
      shiny::h4("Weights"),
      shiny::sliderInput(inputId = "weightSlider",
                  label = " grain type | hardness",
                           # div(style="width: 300px;",
                           #    div(style='float:left;', 'grain type'),
                           #    div(style='margin-left: 65%;', 'hardness')),
                  min = 0, max = 1, value = 0.6),

      shiny::conditionalPanel(
        condition = "input.ddate",
        shiny::sliderInput(inputId = "weightSlider2",
                    ## how to top-align following labels??
                    ## tried vertical-align:top without success
                    label = "{grain type & hardness} | ddate",
                            # div(style='width:300px;',
                            #     div(style='float:left;', 'grain type'),
                            #     div(style='margin-left:40%;', 'hardness'),
                            #     div(style='float:right;', 'ddate')
                            # ),
                    min = 0, max = 1, value = 0.7)
      ),
      shiny::br(),
      ## Set warping window
      shiny::h4("Warping window"),
      shiny::sliderInput(inputId = "wsize", "percentage of layers/height",
                  min = 0, max = 1, value = 0.3),
      # conditionalPanel(condition = "input.ddate",
      #                  sliderInput(inputId = "dwsize", "number of days",
      #                              min = 5, max = 40, value = 40)),

      shiny::br(),
      ## Step pattern
      shiny::h4("Local slope constraint"),
      shiny::helpText("symmetricP1 limits layer stretching and compressing to double/half the original thickness"),
      shiny::radioButtons(inputId = "stepPattern_m", label = NULL,
                  choices = list("unconstrained", "symmetricP1", "symmetricP2"), selected = "symmetricP1",
                  inline = FALSE),

      shiny::br(),
      ## Similarity type
      shiny::h4("Similarity assessment method"),
      shiny::radioButtons(inputId = "simType", label = NULL,
                   choices = list("Herla et al (2021)", "Layerwise", "TSA PWL detection", "RTA scaling"), selected = "Herla et al (2021)",
                   inline = FALSE)
    ),

    shiny::mainPanel(
      shiny::tags$style(shiny::HTML("
    .tabbable > .nav > li > a                  {background-color: silver;  color:navy}
    .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}
    ")),
      shiny::tabsetPanel(
        shiny::tabPanel("Profile Alignment",
                        shiny::br(),
                 # fixedRow(column(width = 5, HTML("Normalized DTW <b>distance</b>: ")),
                          # column(width = 2, strong(textOutput("normDist")))),
                 shiny::fixedRow(shiny::column(width = 5, shiny::HTML("<b>Profile similarity</b>: ")),
                                 shiny::column(width = 2, shiny::strong(shiny::textOutput("simSP")))),
                 shiny::br(),
                 # fixedRow(column(width = 7, checkboxInput("verboseSim", "Print detailed similarity to console", value = FALSE),
                                 # textOutput("verboseSim"))),
                 shiny::conditionalPanel(condition = 'input.ddate',
                                         shiny::br(),
                                         shiny::fixedRow(shiny::column(width = 6, shiny::checkboxInput("labelDdate",
                                                                           "Label deposition date", value = FALSE)))),
                 shiny::plotOutput("alignmentPlot", height = "650px")
        ),
        shiny::tabPanel("Cost Density & Warping Path",
                        shiny::br(),
                 # fixedRow(column(width = 5, HTML("Normalized DTW <b>distance</b>: ")),
                          # column(width = 2, strong(textOutput("normDistII")))),
                 shiny::fixedRow(shiny::column(width = 5, shiny::HTML("<b>Profile similarity</b>: ")),
                                 shiny::column(width = 2, shiny::strong(shiny::textOutput("simSPII")))),
                 shiny::br(),
                 shiny::fixedRow(shiny::column(width = 3, offset = 3, shiny::radioButtons("labelHeight", "Units",
                                                         choices = list("Layer #" = FALSE, "Height (cm)" = TRUE),
                                                         selected = FALSE, inline = TRUE)),
                                 shiny::column(width = 3, shiny::radioButtons("localCost", "Cost",
                                                         choices = list("Global" = FALSE, "Local" = TRUE),
                                                         selected = TRUE, inline = TRUE))),
                 shiny::plotOutput("costDensity",  height = "650px")
        )
      )
    )
  )
  ## --- server function ----
  server <- function(input, output, session) {

   ## ---- initialize reactive profile data ----
    ## dependend on action button or provided input
    shiny::isolate({
      if (values$providedInput) profiles <- shiny::reactiveValues(query = values$query, ref = values$ref)
      else profiles <- shiny::reactiveValues(query = NULL, ref = NULL)
    })

    ## ---- load profiles upon action button ----
    ## subroutine
    get_profiles <- shiny::eventReactive(input$do, {
      SPpairs <- sarp.snowprofile::SPpairs
      if (input$source == "3) Human vs modeled") {
        query <- SPpairs$A_modeled
        ref <- SPpairs$A_manual
      } else if (input$source == "2) Pre/post storm") {
        query <- SPpairs$C_day1
        ref <- SPpairs$C_day2
      } else if (input$source == "1) Variable local warping") {
        query <- SPpairs$D_generalAlignment1
        ref <- SPpairs$D_generalAlignment2
      } else if (input$source == "Browse for your own PRF file") {
        prfRead <- snowprofilePrf(input$prfFile$datapath)
        query <- prfRead[[1]]
        ref <- prfRead[[2]]
      } else {
        shiny::req(input$queryFile, input$refFile)
        query <- snowprofileCaaml(input$queryFile$datapath)
        ref <- snowprofileCaaml(input$refFile$datapath)
      }
      list(ref = ref, query = query)
    })
    ## update profiles with subroutine
    ## i.e. workaround to be able to start off with other values written into profiles!
    shiny::observeEvent(get_profiles(), {
      p <- get_profiles()
      profiles$query = p$query
      profiles$ref = p$ref
    })

    ## ---- store and update dims and weights ----
    properties <- shiny::reactiveValues()
    MINdwsize <- shiny::reactiveVal(0)
    shiny::observeEvent(
      c(input$weightSlider, input$weightSlider2, input$ddate),
      priority = 3,
      {
        if (input$ddate) {
          ## check if ddate info available in profiles:
          if (!"ddate" %in% names(shiny::isolate(profiles$query$layers)) |
              !"ddate" %in% names(shiny::isolate(profiles$ref$layers))) {
            shiny::showModal(shiny::modalDialog(
              title = "Oooops!",
              "At least one of your profiles doesn't have any deposition date information.",
              easyClose = TRUE
            ))
            ## change input$ddate to wrong and continue with subsequent if clause:
            shiny::updateCheckboxInput(session, "ddate", value = FALSE)
          } else {
            ## ddate info is available:
            properties$dims = c("gtype", "hardness", "ddate")
            properties$weights = c(gType = input$weightSlider2 * input$weightSlider,
                                   hardness = input$weightSlider2 * (1 - input$weightSlider),
                                   dDate = 1-input$weightSlider2)
            # ## calculate min dwsize which still yields a warping path:
            # ## i.e. find minimum dwsize for every layer and then take maximum of that vector
            # mv <- sapply(profiles$query$layers$ddate, function(x, y) min(abs(x - y), na.rm = TRUE),
            #              y = profiles$ref$layers$ddate)
            # MINdwsize(max(mv)+1)
          }
        }
        if(!input$ddate) {
          properties$dims = c("gtype", "hardness")
          properties$weights = c(gType = input$weightSlider,
                                 hardness = (1-input$weightSlider))
        }
      }
    )

    ## ---- store and UPDATE alignment ----
    ## update align whenever changes happen:
    align <- shiny::reactiveValues()
    shiny::observeEvent(

      ## UPDATE whenever detect changes in:
      c(profiles$ref, profiles$query, properties$weights, input$reverse, input$ddateNorm, input$wsize, input$dwsize,
        input$layerWeighting, input$initialScaling, input$openEnd, input$checkGlobal, input$stepPattern_m,
        input$resampling, input$resamplingRate, input$alignDir, input$simType), {

        ## calculate profile alignment:
        ## requires profiles to continue
          shiny::req(profiles$query, profiles$ref)
        ## 'reverse' UI tick box
        if (input$reverse) profiles <- list(ref = profiles$query, query = profiles$ref)


        ## resampling rate:
        if (input$resampling) rrate <- input$resamplingRate
        else rrate <- NA

        ## don't allow to go below MINdwsize:
        if (FALSE) {
          ## uncomment if date warping window dwsize is wanted:
          # (input$dwsize < MINdwsize()) {
          #   updateSliderInput(session, "dwsize", value =  MINdwsize())
          #   showModal(modalDialog(
          #     title = "Oha!",
          #     HTML(paste0("No warping path available, if you go below ",  MINdwsize(), " days of date window size!<br>
          #            Check out the cost density to better understand why.")),
          #     easyClose = TRUE
          #   ))
        } else {
          ## dwsize is large enough:

          ## choose grain similarity matrix:
          grainDist <- sim2dist(grainSimilarity_align(FALSE))
          if (input$layerWeighting) layerWeights <- layerWeightingMat(FALSE)
          else layerWeights <- NA

          ## choose step pattern:
          if (input$stepPattern_m == "unconstrained") stepPat <- symmetric2
          else if (input$stepPattern_m == "symmetricP1") stepPat <- symmetricP1
          else if (input$stepPattern_m == "symmetricP2") stepPat <- symmetricP2

          ## choose simType:
          if (input$simType == "Herla et al (2021)") simType <- "HerlaEtAl2021"
          else if (input$simType == "Layerwise") simType <- "layerwise"
          else if (input$simType == "TSA PWL detection") {
            simType <- "tsa_WLdetection"
            newProfiles <- tryCatch({
              list(query = computeTSA(profiles$query),
                   ref = computeTSA(profiles$ref))
            }, error = function(err) err)

            if (inherits(newProfiles, "error")) {
              shiny::showModal(shiny::modalDialog(
                title = "Oha!",
                shiny::HTML(paste0("Can't compute TSA index for your profiles, due to ", newProfiles, ". Reverting to Herla et al (2021) approach.")),
                easyClose = TRUE
              ))
              simType <- "HerlaEtAl2021"
              shiny::updateRadioButtons(session, "simType", selected = "Herla et al (2021)")
            } else {
              profiles <- newProfiles
            }
          } else if (input$simType == "RTA scaling") {
            simType <- "rta_scaling"
            newProfiles <- tryCatch({
              list(query = computeRTA(profiles$query),
                   ref = computeRTA(profiles$ref))
            }, error = function(err) err)

            if (inherits(newProfiles, "error")) {
              shiny::showModal(shiny::modalDialog(
                title = "Oha!",
                shiny::HTML(paste0("Can't compute RTA index for your profiles, due to ", newProfiles, ". Reverting to Herla et al (2021) approach.")),
                easyClose = TRUE
              ))
              simType <- "HerlaEtAl2021"
              shiny::updateRadioButtons(session, "simType", selected = "Herla et al (2021)")
            } else {
              profiles <- newProfiles
            }
          }

          ## alignment direction:
          if (input$alignDir == "Bottom-up (BU)") {
            BU <- TRUE
            TD <- FALSE
          } else if (input$alignDir == "Top-down (TD)") {
            BU <- FALSE
            TD <- TRUE
          } else if (input$alignDir == "BU/TD") {
            BU <- TRUE
            TD <- TRUE
          }

          ## call to dtw:
          align$alignment <- tryCatch({
            dtwSP(query = profiles$query,
                  ref = profiles$ref,
                  grain_type_distMat = grainDist,
                  prefLayerWeights = layerWeights,
                  dims = properties$dims, weights = properties$weights,
                  resamplingRate = rrate,
                  rescale2refHS = input$initialScaling,
                  ddateNorm = input$ddateNorm,
                  windowFunction = warpWindowSP,
                  window.size = input$wsize,
                  step.pattern = stepPat,
                  open.end = input$openEnd, checkGlobalAlignment = input$checkGlobal,
                  keep.internals = TRUE, bottom.up = BU, top.down = TD,
                  simType = simType)
          }, error = function(err) {
            WSIZE <- input$wsize
            alignmentLoop <- function() {
              catch <- tryCatch({
                dtwSP(query = profiles$query,
                      ref = profiles$ref,
                      grain_type_distMat = grainDist,
                      prefLayerWeights = layerWeights,
                      dims = properties$dims, weights = properties$weights,
                      resamplingRate = rrate,
                      rescale2refHS = input$initialScaling,
                      ddateNorm = input$ddateNorm,
                      windowFunction = warpWindowSP,
                      window.size = WSIZE,
                      step.pattern = stepPat,
                      open.end = input$openEnd, checkGlobalAlignment = input$checkGlobal,
                      keep.internals = TRUE, bottom.up = BU, top.down = TD,
                      simType = simType)
              }, error = function(err) {
                return(NA)
              })
            }
            align$alignment <- alignmentLoop()
            while (all(is.na(align$alignment))) {
              if (WSIZE >= 1) break()
              WSIZE = WSIZE + 0.01
              align$alignment <- alignmentLoop()
            }
            if (WSIZE < 1) {
              shiny::updateSliderInput(session, "wsize", value = WSIZE)
              shiny::showModal(shiny::modalDialog(
                title = "Oha!",
                shiny::HTML(paste0("No warping path available, if you go below a window size of ", WSIZE, "!<br>
                     Check out the cost density to better understand why. <br> Switch between the axis units 'Layer #' and 'Height (cm)' if necessary.")),
                easyClose = TRUE
              ))
            } else {
              shiny::showModal(shiny::modalDialog(
                title = "Sorry!",
                shiny::HTML(paste0("Something has gone quite wrong and no alignment could be calculated. <br>
                            Try to reverse what you just did and maybe it'll fix it!")),
                easyClose = TRUE
              ))
              browser()
            }

            return(align$alignment)
          })  # close tryCatch call to assign align$alignment
        }
    })

    ## ---- render alignment figure ----
    output$alignmentPlot <- shiny::renderPlot({
      ## requires alignment to continue:
      shiny::req(align$alignment)
      ## plot:
      label.ddate <- ifelse((input$labelDdate & input$ddate), TRUE, FALSE)
      plotSPalignment(query = NA, ref = NA, dtwAlignment = align$alignment, label.ddate = label.ddate,
                      keep.alignment = TRUE)
      ## write text into figure:
      weightText <- paste(sapply(seq(length(properties$dims)),
                                 function(x) paste(names(properties$weights)[x], "=", properties$weights[x])),
                          collapse = " | ")
      grid::grid.text(paste("weights:", weightText),
                      x = 0.45, y = 0.93,
                      gp = grid::gpar(fontsize=12, col="grey"))
    })

    ## ---- render density figure ----
    output$costDensity <- shiny::renderPlot({
      ## requires alignment to continue:
      shiny::req(align$alignment)
      ## plot:
      align$alignment$localCostMatrix[is.na(align$alignment$costMatrix)] <- NA
      plotCostDensitySP(align$alignment, localCost = as.logical(input$localCost),
                        labelHeight = as.logical(input$labelHeight), marginalPros = TRUE)
    })

    ## ---- render text ----
    ## note: next line's hack is necessary to use that identical output in two different tabs!
    output$normDist <- output$normDistII <-  shiny::renderText({
      ## requires alignment to continue:
      shiny::req(align$alignment)
      ## text:
      paste(formatC(align$alignment$normalizedDistance, format = "f", digits = 3))
    })

    output$simSP <- output$simSPII <- shiny::renderText({
      ## requires alignment to continue:
      shiny::req(align$alignment)
      ## text:
      paste(formatC(align$alignment$sim,
                    format = "f", digits = 3))
    })

    ## disabled verbose sim function upon introducing different simTypes (Jan 2022, fherla)
    # output$verboseSim <- renderText({
    #   ## requires alignment
    #   req(align$alignment)
    #   req(align$alignment$queryWarped)
    #   ## verbose text:
    #   if (input$verboseSim) {
    #     tmp <- simSP(align$alignment$reference, align$alignment$queryWarped,
    #               gtype_distMat = sim2dist(grainSimilarity_evaluate(triag = FALSE)),
    #               verbose = TRUE)
    #     "(printed outside app)"
    #   }
    # })

    ## ---- communication between UI and server ----
    ## needed in order to make action button disappear when input profiles are provided
    output$onInput <- shiny::reactive({
      if (values$providedInput) TRUE
      else FALSE
    })
    shiny::outputOptions(output, 'onInput', suspendWhenHidden = FALSE)
  }

  ## --- run app ----
  shiny::shinyApp(ui = ui, server = server)

}
