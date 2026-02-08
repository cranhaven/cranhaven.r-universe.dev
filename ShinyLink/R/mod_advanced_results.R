#' advanced_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_advanced_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(
      6,
      box(
        width = 12,
        title = "Start Advanced Matching",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        helpText("Identify which variables correspond to each piece of information"),
        actionButton(ns("match"), "Advanced Match"),
        hr(),
        br(),
        tags$label("Selected row(s) of Matching Results table:"),
        fluidRow(column(6, verbatimTextOutput(ns("info-main"))),
                 column(
                   6,
                   downloadButton(ns("download_selected"), "Download Selected")
                 )),
        hr(),
        tags$label("Summary of matching results:"),
        verbatimTextOutput(ns("matched-summary"))
      )
    ),
    column(
      6,
      box(
        width = 12,
        title = "Matching Summary",
        status = "success",
        solidHeader = FALSE,
        collapsible = FALSE,
        plotOutput(ns("plot-venn"))
      )
    )),
    box(
      width = 12,
      title = "Matching Results",
      status = "success",
      solidHeader = FALSE,
      collapsible = FALSE,
      column(12, DT::dataTableOutput(ns('matched')))
    )
  )
}

#' advanced_results Server Functions
#' @import fastLink
#' @noRd
mod_advanced_results_server <- function(id, state, session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
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

      # library(magrittr)
      dfA <- state$state_dfA
      dfB <- state$state_dfB

      # matches.out <- fastLink(
      #   dfA = dfA, dfB = dfB,
      #   varnames = c("firstname", "middlename", "lastname", "birthday", "race", "sex"),
      #   stringdist.match = c("firstname", "middlename", "lastname", "birthday", "race", "sex"),
      #   numeric.match =
      #   partial.match = c("firstname", "lastname"),
      #   n.cores = 64
      # )

      matches.out <- fastLink(
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
        linprog.dedupe = state$linprog_dedupe,

        n.cores = state$n_cores,
        tol.em = state$tol_em
        # threshold.match = state$threshold_match # Not applicable
      )

      dfA.match <- dfA[matches.out$matches$inds.a, ]
      dfA.unmatch <- dfA[-matches.out$matches$inds.a, ]
      dfB.match <- dfB[matches.out$matches$inds.b, ]
      dfB.unmatch <- dfB[-matches.out$matches$inds.b, ]

      matched_dfs <- getMatches(
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
            'gamma.6',
            'posterior'
          )
        ))

      subdat <- list()

      varnames <- state$matching_variables

      for (i in 1:nrow(matches.out$matches)) {
        dfA_current <-  dfA %>% dplyr::select(varnames)
        dfA_current <- dfA_current[matches.out$matches[i, ]$inds.a, ]
        dfA_current <- dfA_current %>%
          dplyr::mutate(`Data source` = "Sample Dataset", .before = "firstname")

        dfB_current <-  dfB %>% dplyr::select(varnames)
        dfB_current <- dfB_current[matches.out$matches[i, ]$inds.b, ]
        dfB_current <- dfB_current %>%
          dplyr::mutate(`Data source` = "Matching Dataset", .before = "firstname")

        subdat[[i]] <- dplyr::as_tibble(dplyr::bind_rows(dfA_current, dfB_current))
      }

      subdats <- lapply(subdat, purrr::transpose)
      Dat <- cbind(" " = "expand", matched_dfs, details = I(subdats))


      matched_summary <- summary(matches.out)

      plot_summary <-
        tidyr::pivot_longer(
          matched_summary[1, 2:ncol(matched_summary)],
          cols = 1:4,
          names_to = "Match Type",
          values_to = "Match Count"
        ) %>% dplyr::mutate(`Match Count` = as.numeric(`Match Count`))

      # library(ggplot2)
      p <-
        ggplot2::ggplot(plot_summary,
                        ggplot2::aes(x = `Match Type`, y = `Match Count`, fill = `Match Type`)) +
        ggplot2::geom_bar(stat = "identity") + ggplot2::theme_minimal() + ggplot2::scale_fill_manual(values =
                                                                            c("#3b4992", "#ee2200", "#008b45", "#631779"))

      p


      sendSweetAlert(
        session = session,
        title = "Success!",
        text = "Please review each match",
        type = "success"
      )

      # for manual selection


      matched_results <- list(
        Dat = Dat,
        matched_summary = matched_summary,
        dfA.match = dfA.match,
        dfA.unmatch = dfA.unmatch,
        dfB.match = dfB.match,
        dfB.unmatch = dfB.unmatch
      )
      state$advanced_results <- matched_results
      return(matched_results)
    })


    # Output Matched ----------------------------------------------------------
    output[["matched"]] <- renderDT({
      datatable(
        dplyr::as_tibble(matched_values()[['Dat']]),
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
          buttons =
            list(
              "copy",
              list(
                extend = "collection"
                ,
                buttons = c("csv", "excel", "pdf")
                ,
                text = "Download All"
              )
            ),
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
    output[["plot-summary"]] <- renderPlot({
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required for plotting. Please install it.")
      }
      plot_summary <- matched_values()[['matched_summary']]

      plot_summary <-
        tidyr::pivot_longer(
          plot_summary[1, 2:ncol(plot_summary)],
          cols = 1:4,
          names_to = "Match Type",
          values_to = "Match Count"
        ) %>% dplyr::mutate(`Match Count` = as.numeric(`Match Count`))

      # library(ggplot2)
      p <-
        ggplot2::ggplot(plot_summary,
                        ggplot2::aes(x = `Match Type`, y = `Match Count`, fill = `Match Type`)) +
        ggplot2::geom_bar(stat = "identity") + ggplot2::theme_minimal() + ggplot2::scale_fill_manual(values =
                                                                            c("#3b4992", "#ee2200", "#008b45", "#631779"))

      p
    })

    output[["plot-venn"]] <- renderPlot({
      if (!requireNamespace("ggvenn", quietly = TRUE)) {
        stop("Package 'ggvenn' is required for plotting. Please install it.")
      }
      n_dfA.unmatch <- nrow(matched_values()[['dfA.unmatch']])
      n_dfB.unmatch <- nrow(matched_values()[['dfB.unmatch']])
      n_match <- nrow(matched_values()[['Dat']])
      # library("ggvenn")
      x <- list(
        Sample = c((1:n_dfA.unmatch)+3e9, 1:n_match),
        Matching = c(-(1:n_dfB.unmatch) + 5e7, 1:n_match)
      )
      if (length(names(x)) == 2) {
        names(x) <- c("Sample Data", "Matching Data")
      }
      ggvenn::ggvenn(x)
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
  })
}

## To be copied in the UI
# mod_advanced_results_ui("advanced_results_1")

## To be copied in the server
# mod_advanced_results_server("advanced_results_1")

utils::globalVariables(c("Match Count", "Match Type"))

