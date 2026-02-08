#' advanced_parameters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shinyWidgets
#' @importFrom shiny NS tagList
mod_advanced_parameters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    chooseSliderSkin("Modern", color = "#ED5565"),
    box(
      width = 6,
      title = "Advanced options",
      status = "warning",
      solidHeader = FALSE,
      collapsible = FALSE,
      fluidRow(
        column(
          width = 6,
          sliderInput(
            ns("cut_a"),
            p("Lower bound for full string-distance match"),
            min = 0,
            max = 1,
            value = 0.92
          ),
          sliderInput(
            ns("cut_p"),
            p("Lower bound for partial string-distance match"),
            min = 0,
            max = 1,
            value = 0.88
          )
        ),
        column(
          width = 6,
          numericInput(ns("n_cores"),
                       p("The number of cores to parallelize"),
                       value = 1, min = 1),
          numericInput(
            ns("tol_em"),
            p("Convergence tolerance for the EM algorithm"),
            value = 1e-04,
            min = 1e-04,
            max = 1
          ),
          fluidRow(
            column(
              width = 6,
              materialSwitch(
                inputId = ns("dedupe_matches"),
                label = "dedupe.matches",
                status = "danger",
                value = TRUE
              )
            )
            # column(
            #   width = 6,
            #   materialSwitch(
            #     inputId = ns("linprog_dedupe"),
            #     label = "linprog.dedupe",
            #     status = "danger"
            #   )
            # )
          )
        ),
        style = "margin-left: 0px;",
        style = "margin-right: 0px;"
      ),
      hr(),
      helpText(
        "Click this button to execute matching, and your results will appear after a few seconds."
      ),
      actionButton(ns("match"), "Advanced Match", class = "btn-danger")
    ),
    box(
      width = 6,
      height = '20em',
      title = "Matching Summary",
      status = "warning",
      solidHeader = FALSE,
      collapsible = FALSE,
      plotOutput(ns("plot-venn"), height = "315px")
    ),
    box(
      width = 12,
      title = "Matching Results",
      status = "warning",
      solidHeader = FALSE,
      collapsible = FALSE,
      tags$label(
        "Selected row(s) of Matching Results table, and the results will be reflected in the Matching Details page"
      ),
      column(12, DT::dataTableOutput(ns('matched')))
    )
  )
}

#' advanced_parameters Server Functions
#'
#' @import fastLink dplyr grid
#' @importFrom DT renderDT datatable
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom htmlwidgets JS
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom utils capture.output write.csv
#'
#' @noRd
mod_advanced_parameters_server <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observe({
      # Advanced parameters
      state$cut_a <- input$cut_a
      state$cut_p <- input$cut_p
      # state$w_lambda <- input$w_lambda
      # state$w_pi <- input$w_pi

      # state$estimate_only <- input$estimate_only
      state$dedupe_matches <- input$dedupe_matches
      # state$linprog_dedupe <- input$linprog_dedupe

      state$n_cores <- input$n_cores
      state$tol_em <- input$tol_em
      # state$threshold_match <- input$threshold_match

    })

    ## the callback ####
    registerInputHandler("x.child", function(x, ...) {
      fromJSON(toJSON(x, auto_unbox = TRUE, null = "null"),
               simplifyDataFrame = FALSE)
    }, force = TRUE)
    callback = JS(
      "var expandColumn = table.column(0).data()[0] === 'plus-sign' ? 0 : 1;",
      "table.column(expandColumn).nodes().to$().css({cursor: 'pointer'});",
      "",
      "// send selected columns of the main table to Shiny",
      "var tbl = table.table().node();",
      "var tblId = $(tbl).closest('.datatables').attr('id');",
      "var selector = 'td:not(:nth-child(' + (expandColumn+1) + '))';",
      "table.on('click', selector, function(){",
      "  setTimeout(function(){",
      "    var indexes = table.rows({selected:true}).indexes();",
      "    var indices = Array(indexes.length);",
      "    for(var i = 0; i < indices.length; ++i){",
      "      indices[i] = indexes[i];",
      "    }",
      "    Shiny.setInputValue(tblId + '_rows_selected', indices);",
      "  },0);",
      "});",
      "",
      "// make the table header of the nested table",
      "var format = function(d, childId){",
      "  if(d != null){",
      "    var html = '<table class=\"compact hover\" id=\"' + ",
      "                childId + '\"><thead><tr>';",
      "    for(var key in d[d.length-1][0]){",
      "      html += '<th>' + key + '</th>';",
      "    }",
      "    html += '</tr></thead></table>'",
      "    return html;",
      "  } else {",
      "    return '';",
      "  }",
      "};",
      "",
      "// row callback to style the rows background colors of the child tables",
      "var rowCallback = function(row, dat, displayNum, index){",
      "  if($(row).hasClass('odd')){",
      "    $(row).css('background-color', 'papayawhip');",
      "    $(row).hover(function(){",
      "      $(this).css('background-color', '#E6FF99');",
      "    }, function() {",
      "      $(this).css('background-color', 'papayawhip');",
      "    });",
      "  } else {",
      "    $(row).css('background-color', 'lemonchiffon');",
      "    $(row).hover(function(){",
      "      $(this).css('background-color', '#DDFF75');",
      "    }, function() {",
      "      $(this).css('background-color', 'lemonchiffon');",
      "    });",
      "  }",
      "};",
      "",
      "// header callback to style the header of the child tables",
      "var headerCallback = function(thead, data, start, end, display){",
      "  $('th', thead).css({",
      "    'border-top': '3px solid indigo',",
      "    'color': 'indigo',",
      "    'background-color': '#fadadd'",
      "  });",
      "};",
      "",
      "// make the child table",
      "var format_datatable = function(d, childId){",
      "  var dataset = [];",
      "  var n = d.length - 1;",
      "  for(var i = 0; i < d[n].length; i++){",
      "    var datarow = $.map(d[n][i], function(value, index){",
      "      return [value];",
      "    });",
      "    dataset.push(datarow);",
      "  }",
      "  var id = 'table#' + childId;",
      "  var subtable = $(id).DataTable({",
      "             'data': dataset,",
      "             'autoWidth': true,",
      "             'deferRender': true,",
      "             'info': false,",
      "             'lengthChange': false,",
      "             'ordering': d[n].length > 1,",
      "             'order': [],",
      "             'paging': false,",
      "             'scrollX': false,",
      "             'scrollY': false,",
      "             'searching': false,",
      "             'sortClasses': false,",
      "             'rowCallback': rowCallback,",
      "             'headerCallback': headerCallback,",
      "             'select': {style: 'multi'},",
      "             'columnDefs': [{targets: '_all', className: 'dt-center'}]",
      "           });",
      "};",
      "",
      "// send selected rows of the children tables to shiny server",
      "var nrows = table.rows().count();",
      "var nullinfo = Array(nrows);",
      "for(var i = 0; i < nrows; ++i){",
      "  nullinfo[i] = {row: i, selected: null};",
      "}",
      "Shiny.setInputValue(tblId + '_children:x.child', nullinfo);",
      "var sendToR = function(){",
      "  var info = [];",
      "  setTimeout(function(){",
      "    for(var i = 0; i < nrows; ++i){",
      "      var childId = 'child-' + i;",
      "      var childtbl = $('#'+childId).DataTable();",
      "      var indexes = childtbl.rows({selected:true}).indexes();",
      "      var indices;",
      "      if(indexes.length > 0){",
      "        indices = Array(indexes.length);",
      "        for(var j = 0; j < indices.length; ++j){",
      "          indices[j] = indexes[j];",
      "        }",
      "      } else {",
      "        indices = null;",
      "      }",
      "      info.push({row: i, selected: indices});",
      "    }",
      "    Shiny.setInputValue(tblId + '_children:x.child', info);",
      "  }, 0);",
      "}",
      "$('body').on('click', '[id^=child-] td', sendToR);",
      "",
      "// click event to show/hide the child tables",
      "table.on('click', 'td.details-control', function () {",
      "  var cell = table.cell(this);",
      "      row = table.row($(this).closest('tr'));",
      "  if(row.child.isShown()){",
      "    row.child.hide();",
      "    cell.data('expand');",
      "    sendToR();",
      "  } else {",
      "    var childId = 'child-' + row.index();",
      "    row.child(format(row.data(), childId)).show();",
      "    row.child.show();",
      "    cell.data('collapse-down');",
      "    format_datatable(row.data(), childId);",
      "  }",
      "});"
    )
    # Render function, to display the glyphicons ------------------------------
    render <- c(
      "function(data, type, row, meta){",
      "  if(type === 'display'){",
      "    return '<span style=\\\"color:black; font-size:18px\\\">' + ",
      "       '<i class=\\\"glyphicon glyphicon-' + data + '\\\"></i></span>';",
      "  } else {",
      "    return data;",
      "  }",
      "}"
    )

    # Advanced Matching ---------------------------------------------------------
    matched_values <- eventReactive(input$match, {

      req(state$state_dfA)
      req(state$state_dfB)
      dfA <- state$state_dfA
      dfB <- state$state_dfB

      # Testing only
      # dfA <- readxl::read_excel('inst/app/www/lkselectedrecs_cleaned.xlsx')
      # dfB <- readxl::read_excel('inst/app/www/redcapoutput_cleaned.xlsx')
      # dfA <- readxl::read_excel('inst/app/www/unique_in_sample_dataset.xlsx')
      # dfB <- readxl::read_excel('inst/app/www/unique_in_matching_dataset.xlsx')
      # matches.out <- fastLink::fastLink(
      #   dfA = dfA, dfB = dfB,
      #   varnames = c("firstname", "middlename", "lastname", "race", "sex"),
      #   # stringdist.match = c("firstname", "middlename", "lastname", "race", "sex"),
      #   # numeric.match =
      #   # partial.match = c("firstname", "lastname"),
      #   n.cores = 1
      # )

      matches.out <- fastLink::fastLink(
        dfA = dfA,
        dfB = dfB,
        varnames = state$matching_variables,
        stringdist.match = state$string_matching,
        numeric.match = state$numeric_matching,
        partial.match = state$partial_matching,

        # Advanced parameters
        cut.a = state$cut_a,
        cut.p = state$cut_p,
        # w.lambda = state$w_lambda, # Not applicable
        # w.pi = state$w_pi, # Not applicable

        # estimate.only = state$estimate_only, # Not applicable
        dedupe.matches = state$dedupe_matches,
        # linprog.dedupe = state$linprog_dedupe,

        n.cores = state$n_cores,
        tol.em = state$tol_em
        # threshold.match = state$threshold_match # Not applicable
      )

      if (length(matches.out$matches$inds.a) == 0) {
        matched_results <- list(
          Dat = tibble::tibble(),
          matches.out = NULL,
          matched_summary = NULL,
          dfA.match = NULL,
          dfA.unmatch = NULL,
          dfB.match = NULL,
          dfB.unmatch = NULL,
          matched_union = NULL
        )
        state$advanced_results <- matched_results
        sendSweetAlert(
          session = session,
          title = "",
          text = "No matches found",
          type = "warning"
        )
      }

      if (length(matches.out$matches$inds.a) != 0) {

        dfA.match <- dfA[matches.out$matches$inds.a, ]
        dfA.unmatch <- dfA[-matches.out$matches$inds.a, ]
        dfB.match <- dfB[matches.out$matches$inds.b, ]
        dfB.unmatch <- dfB[-matches.out$matches$inds.b, ]

        matched_dfs <- fastLink::getMatches(
          dfA = dfA,
          dfB = dfB,
          fl.out = matches.out,
          threshold.match = 0.85
        )
        matched_dfs <- matched_dfs %>%
          dplyr::select(-tidyselect::any_of(
            c(
              'gamma.1',
              'gamma.2',
              'gamma.3',
              'gamma.4',
              'gamma.5',
              'gamma.6'
              # 'posterior'
            )
          ))

        subdat <- list()

        varnames <- state$matching_variables

        for (i in 1:nrow(matches.out$matches)) {
          dfA_current <-  dfA %>% dplyr::select(varnames)
          dfA_current <-
            dfA_current[matches.out$matches[i,]$inds.a,]
          dfA_current <- dfA_current %>%
            dplyr::mutate(`Data source` = "Sample Dataset", .before = colnames(dfA_current)[1])

          dfB_current <-  dfB %>% dplyr::select(varnames)
          dfB_current <-
            dfB_current[matches.out$matches[i,]$inds.b,]
          dfB_current <- dfB_current %>%
            dplyr::mutate(`Data source` = "Matching Dataset", .before = colnames(dfA_current)[1])

          subdat[[i]] <-
            dplyr::as_tibble(dplyr::bind_rows(dfA_current, dfB_current))

          if ("birthday" %in% colnames(subdat[[i]])) {
            subdat[[i]]$birthday <- as.character(subdat[[i]]$birthday)
          }


        }

        subdats <- lapply(subdat, purrr::transpose)
        Dat <-
          cbind(" " = "expand", matched_dfs, details = I(subdats))


        matched_summary <- summary(matches.out)

        plot_summary <-
          tidyr::pivot_longer(
            matched_summary[1, 2:ncol(matched_summary)],
            cols = 1:4,
            names_to = "Match Type",
            values_to = "Match Count"
          ) %>% dplyr::mutate(`Match Count` = as.numeric(`Match Count`))

        sendSweetAlert(
          session = session,
          title = "Success!",
          text = "Please review each match",
          type = "success"
        )

        # for manual selection
        matched_results <- list(
          Dat = Dat,
          matches.out = matches.out,
          matched_summary = matched_summary,
          dfA.match = dfA.match,
          dfA.unmatch = dfA.unmatch,
          dfB.match = dfB.match,
          dfB.unmatch = dfB.unmatch,
          matched_union = dplyr::bind_rows(matched_dfs, dfA.unmatch, dfB.unmatch)
        )
        state$advanced_results <- matched_results
        return(matched_results)
      }
    })

    # if (length(state$advanced_results[['matches.out']]$matches$inds.a) != 0)
    # Output Matched ----------------------------------------------------------

    Dat <- reactive({
      dplyr::as_tibble(matched_values()[['Dat']])
    })

    output[["matched"]] <- renderDT({
      datatable(
        Dat(),
        callback = callback,
        escape = -2,
        extensions = c("Buttons", "Select"),
        selection = "none",
        options = list(
          select = list(style = "multi", selector = ".selectable"),
          autoWidth = FALSE,
          scrollX = TRUE,
          lengthMenu = list(c(10, 20, 50,-1), c('default', '20', '50', 'All')),
          pageLength = 10,
          dom = 'Blfrtip',
          buttons = list(),
          columnDefs = list(
            list(className = "selectable dt-center",
                 targets = c(0, 2:ncol(
                   matched_values()[['Dat']]
                 ))),
            list(visible = FALSE, targets = ncol(matched_values()[['Dat']])),
            list(
              orderable = FALSE,
              className = 'details-control',
              width = "10px",
              render = JS(render),
              targets = 1
            ),
            list(className = "dt-center", targets = "_all")
          )
        ),
        class = 'compact hover row-border nowrap stripe'
      )
    }, server = TRUE)

    output[["info-main"]] <- renderText({
      capture.output(input[["matched_rows_selected"]])
    })

    output[["info-children"]] <- renderText({
      paste0(capture.output(input[["matched_children"]]), collapse = "\n")
    })
    output[["matched-summary"]] <- renderPrint({
      matched_values()[['matched_summary']]
    })

    output[["plot-venn"]] <- renderPlot({
      if (length(state$advanced_results[['matches.out']]$matches$inds.a) != 0) {
        if (!requireNamespace("ggvenn", quietly = TRUE)) {
          stop("Package 'ggvenn' is required for plotting. Please install it.")
        }
        n_dfA.unmatch <- nrow(matched_values()[['dfA.unmatch']])
        n_dfB.unmatch <- nrow(matched_values()[['dfB.unmatch']])
        n_match <- nrow(matched_values()[['Dat']])
        # Warning: Error in inner_join: could not find function "inner_join"

        x <- list(Sample = c((1:n_dfA.unmatch) + 3e9, 1:n_match),
                  Matching = c(-(1:n_dfB.unmatch) + 5e7, 1:n_match))
        if (length(names(x)) == 2) {
          names(x) <- c("Sample Data", "Matching Data")
        }
        ggvenn::ggvenn(x, fill_color = c("#3b4992", "#008b45"))
      }
    })

    # Download selected rows --------------------------------------------------

    output$download_selected <- downloadHandler(
      filename = function() {
        paste("selected-data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(matched_values()[['Dat']][input[["matched_rows_selected"]] + 1, ], file)
      }
    )

    # Update matched results based on selection -------------------------------
    observe({
      if (!is.null(input[["matched_rows_selected"]])) {
        matched_rows_selected <- input[["matched_rows_selected"]] + 1

        state$advanced_results[['matched_intersect']] <-
          matched_values()[['Dat']][matched_rows_selected, ] %>%
          dplyr::select(-tidyselect::any_of(c('details')))

        matches.out <- state$advanced_results[['matches.out']]

        # Testing only
        # dfA <- readxl::read_excel('inst/app/www/lkselectedrecs_cleaned.xlsx')
        # dfB <- readxl::read_excel('inst/app/www/redcapoutput_cleaned.xlsx')
        # matched_rows_selected <- c(1, 2, 3, 7)
        #
        # matches.out <- fastLink::fastLink(
        #   dfA = dfA, dfB = dfB,
        #   varnames = c("firstname", "middlename", "lastname", "race", "sex"),
        #   # stringdist.match = c("firstname", "middlename", "lastname", "race", "sex"),
        #   # numeric.match =
        #   # partial.match = c("firstname", "lastname"),
        #   n.cores = 1
        # )

        dfA <- state$state_dfA
        dfB <- state$state_dfB

        matches.out.manual <- matches.out
        matches.out.manual$matches <- matches.out$matches[matched_rows_selected, ]
        matches.out.manual$patterns <- matches.out$patterns[matched_rows_selected, ]
        matches.out.manual$posterior <- matches.out$posterior[matched_rows_selected]

        dfA.match <- dfA[matches.out.manual$matches$inds.a, ]
        dfA.unmatch <- dfA[-matches.out.manual$matches$inds.a, ]
        dfB.match <- dfB[matches.out.manual$matches$inds.b, ]
        dfB.unmatch <- dfB[-matches.out.manual$matches$inds.b, ]

        matched_dfs <- fastLink::getMatches(
          dfA = dfA,
          dfB = dfB,
          fl.out = matches.out.manual,
          threshold.match = 0.85,
          combine.dfs = TRUE
        )
        matched_dfs <- matched_dfs %>%
          dplyr::select(-tidyselect::any_of(
            c(
              'gamma.1',
              'gamma.2',
              'gamma.3',
              'gamma.4',
              'gamma.5',
              'gamma.6',
              'posterior'
            )
          ))

        state$advanced_results[['matched_union']] <- dplyr::bind_rows(matched_dfs, dfA.unmatch, dfB.unmatch)
        state$advanced_results[['dfA.unmatch']] <- dfA.unmatch
        state$advanced_results[['dfB.unmatch']] <- dfB.unmatch

      } else {
        state$advanced_results[['matched_intersect']] <-
          dplyr::as_tibble(matched_values()[['Dat']])
      }
    })
  })
}

## To be copied in the UI
# mod_advanced_parameters_ui("advanced_parameters_1")

## To be copied in the server
# mod_advanced_parameters_server("advanced_parameters_1")
