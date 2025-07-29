`%then%` <- rlang::`%||%`
# nolint start: undesirable_function_linter
library(nprcgenekeepr) #
library(futile.logger)
library(ggplot2)
library(stringi)
suppressMessages(library(DT))
# nolint end: undesirable_function_linter
shinyServer(function(input, output, session) {
  errorLst <- getEmptyErrorLst()
  nprcgenekeeprLog <- paste0(getSiteInfo()$homeDir, "nprcgenekeepr.log")
  flog.logger("nprcgenekeepr", INFO,
              appender = appender.file(nprcgenekeeprLog))

  #############################################################################
  # Functions for handling initial pedigree upload and QC
  getSelectedBreeders <- reactive({
    input$getData # This button starts it all
    if (input$debugger) {
      flog.threshold(DEBUG, name = "nprcgenekeepr")
    } else {
      flog.threshold(INFO, name = "nprcgenekeepr")
    }
    isolate({
      flog.debug(paste0("1st. input$fileContent: ", input$fileContent,
                        "; input$fileType: ", input$fileType,
                        "; input$separator: ", input$separator),
                 name = "nprcgenekeepr")
      sep <- input$separator
      if (input$fileContent == "pedFile") {
        pedigreeFile <- input$pedigreeFileOne
        flog.debug(paste0("pedigreeFile - pedigreeFile$name: ",
                          pedigreeFile$name,
                          "; pedigreeFile$datapath: ", pedigreeFile$datapath),
                   name = "nprcgenekeepr")
      } else if (input$fileContent == "commonPedGenoFile") {
        pedigreeFile <- input$pedigreeFileTwo
        flog.debug(paste0("pedigreeFileTwo - pedigreeFile$name: ",
                          pedigreeFile$name,
                          "; pedigreeFile$datapath: ", pedigreeFile$datapath),
                   name = "nprcgenekeepr")
      } else if (input$fileContent == "separatePedGenoFile") {
        pedigreeFile <- input$pedigreeFileThree
        genotypeFile <- input$genotypeFile
        flog.debug(paste0("pedigreeFileThree - pedigreeFile$name: ",
                          pedigreeFile$name, "; ",
                          "; pedigreeFile$datapath: ", pedigreeFile$datapath,
                          "; genotypeFile$name: ", genotypeFile$name,
                          "; genotypeFile$datapath: ", genotypeFile$datapath),
                   name = "nprcgenekeepr")
      } else if (input$fileContent == "focalAnimals") {
        pedigreeFile <- input$breederFile
        flog.debug(paste0("breederFile - pedigreeFile$name: ",
                          pedigreeFile$name, "; ",
                          "pedigreeFile$datapath: ", pedigreeFile$datapath),
                   name = "nprcgenekeepr")
      } else {
        stop("Data source was not defined.")
      }
      # The minParentAge -- numeric values to set the minimum age in years for
      # an animal to have an offspring. Defaults to 2 years. The check is not
      # performed for animals with missing birth dates. See qcStudbook().
      flog.debug("sep: %s", sep, name = "nprcgenekeepr")
      minParentAge <- tryCatch(
        as.numeric(input$minParentAge),
        warning = function(cond) {
          return(NULL)
        },
        error = function(cond) {
          return(NULL)
        }
      )
      globalMinParentAge <<- minParentAge
      flog.debug(paste0("minParentAge: ", minParentAge),
                 name = "nprcgenekeepr")

      # pedigreeFile and breederFile will be NULL initially.
      # After the user selects a file, it will be a filepath.
      if (is.null(pedigreeFile)) {
        return(NULL)
      }
      flog.debug(paste0("before Load pedigree table ",
                        input$fileContent),
                 name = "nprcgenekeepr")
      # Load pedigree table
      if (input$fileContent == "focalAnimals") {
        flog.debug(paste0("before getFocalAnimalPed: ", pedigreeFile$name),
                   name = "nprcgenekeepr")
        breederPed <- getFocalAnimalPed(pedigreeFile$datapath, sep = sep)
        if (is.element("nprckeepErr", class(breederPed))) {
          errorLst <- breederPed
          breederPed <- NULL
        } else if (is.null(breederPed)) {
          flog.debug(paste0("after getFocalAnimalPed: ", pedigreeFile$name,
                            "; NULL was returned by getFocalAnimalPed ",
                            "function"),
                     name = "nprcgenekeepr")
        } else {
          flog.debug(paste0("after getFocalAnimalPed: ", pedigreeFile$name,
                            "; contents rows: ", nrow(breederPed),
                            ", columns: ", ncol(breederPed), "; col names: '",
                            paste(names(breederPed), collapse = "', '"), "'"),
                     name = "nprcgenekeepr")
        }
      } else {
        breederPed <- getPedigree(pedigreeFile$datapath, sep = sep)
        flog.debug(paste0("after getPedigree pedigreeFile$name: ",
                          pedigreeFile$name,
                          "; contents rows: ", nrow(breederPed),
                          ", columns: ", ncol(breederPed), "; col names: '",
                          paste(names(breederPed), collapse = "', '"), "'"),
                   name = "nprcgenekeepr")
      }

      if (is.null(input$fileContent)) {
        stop("Did not expect input$fileContent to be NULL")
      } else if (input$fileContent == "separatePedGenoFile") {
        # Load pedigree table
        flog.debug(paste0("before getGenotypes genotypeFile$datapath: ",
                          genotypeFile$datapath,
                          "; contents rows: ", nrow(breederPed),
                          ", columns: ", ncol(breederPed), "; col names: '",
                          paste(names(breederPed), collapse = "', '"), "'"),
                   name = "nprcgenekeepr")
        genotype <- getGenotypes(genotypeFile$datapath, sep = sep)
        flog.debug(paste0("genotype$name: ", genotype$name,
                          "; contents rows: ", nrow(genotype),
                          ", columns: ", ncol(genotype), "; col names: '",
                          paste(names(genotype), collapse = "', '"), "'"),
                   name = "nprcgenekeepr")
        genotype <- tryCatch(
          checkGenotypeFile(genotype),
          warning = function(cond) {
            return(NULL)
          },
          error = function(cond) {
            return(NULL)
          },
          finally = {
            flog.debug(paste0("   tryCatch checkGenotype ", "file. ",
                              geterrmessage()),
                       name = "nprcgenekeepr")
          }
        )
        breederPed <- addGenotype(breederPed, genotype)
        flog.debug(paste0("After addGenotype - genotypeFile$name: ",
                          genotypeFile$name,
                          "; contents rows: ", nrow(breederPed),
                          ", columns: ", ncol(breederPed), "; col names: '",
                          paste(names(breederPed), collapse = "', '"), "'"),
                   name = "nprcgenekeepr")
      } else {
        flog.debug(paste0("Setting genotype to NULL."),
                   name = "nprcgenekeepr")
        genotype <- NULL
      }
      flog.debug(paste0("Data files may have been read.\n",
                        "contents rows: ", nrow(breederPed),
                        ", columns: ", ncol(breederPed), "; col names: '",
                        paste(names(breederPed), collapse = "', '"), "'"),
                 name = "nprcgenekeepr")

      if (!is.null(minParentAge)) {
        flog.debug(paste0("Before qcStudbook.\n",
                          "contents rows: ", nrow(breederPed),
                          ", columns: ", ncol(breederPed), "; col names: '",
                          paste(names(breederPed), collapse = "', '"), "'"),
                   name = "nprcgenekeepr")
        if (!checkErrorLst(errorLst)) {
          errorLst <- tryCatch(
            qcStudbook(
              breederPed,
              minParentAge,
              reportChanges = FALSE,
              reportErrors = TRUE
            ),
            warning = function(cond) return(NULL),
            error = function(cond) return(NULL)
          )
        }
        removeTab(inputId = "tab_pages", target = "Changed Columns")
        removeTab(inputId = "tab_pages", target = "Error List")

        if (checkErrorLst(errorLst)) {
          insertTab(
            inputId = "tab_pages",
            getErrorTab(errorLst, pedigreeFile$name),
            target = "Input",
            position = "before",
            select = TRUE
          )
          breederPed <- NULL
        } else {
          if (checkChangedColsLst(errorLst$changedCols)) {
            insertTab(
              inputId = "tab_pages",
              getChangedColsTab(errorLst, pedigreeFile$name),
              target = "Input",
              position = "before",
              select = FALSE
            )
          }
          breederPed <- tryCatch(
            qcStudbook(breederPed, minParentAge),
            warning = function(cond) return(NULL),
            error = function(cond) return(NULL)
          )
          flog.debug(
            paste0(
              "After qcStudbook.\n",
              "contents rows: ",
              nrow(breederPed),
              ", columns: ",
              ncol(breederPed),
              "; col names: '",
              paste(names(breederPed), collapse = "', '"),
              "'"
            ),
            name = "nprcgenekeepr"
          )
        }
      }
      flog.debug(paste0("before validate()."), name = "nprcgenekeepr")
      validate(need(
        !is.null(minParentAge),
        paste0("   Error uploading data. ", geterrmessage())
      ) %then%
        need(
          !is.null(breederPed),
          paste0("   Error uploading data. ", geterrmessage())
        ))
      if (!is.null(breederPed)) {
        updateTabsetPanel(session, "tab_pages", selected = "Pedigree Browser")
      }
      flog.debug(
        paste0(
          "After validate(); nrow(breederPed) = ",
          nrow(breederPed),
          "; ncol(breederPed): ",
          ncol(breederPed)
        ),
        name = "nprcgenekeepr"
      )
      breederPed
    })
  })

  # Load and QA-QC the pedigree once a file has been specified

  getPed <- reactive({
    flog.debug(paste0("In ped <- reactive()\n"), name = "nprcgenekeepr")
    if (is.null(getSelectedBreeders())) {
      return(NULL)
    }
    flog.debug(
      paste0(
        "In ped <- reactive() and ",
        "!is.null(getSelectedBreeders()) == TRUE\n"
      ),
      name = "nprcgenekeepr"
    )

    ped <- getSelectedBreeders()
    flog.debug(paste0("column names: '", paste(names(ped), collapse = "', '"),
                      "'"), name = "nprcgenekeepr")
    flog.debug(" - after ped <- getSelectedBreeders() before tryCatch with ",
               "setPopulation.",
               name = "nprcgenekeepr")
    ped <- tryCatch({
      ped <- getSelectedBreeders()
      flog.debug(paste0("column names: '", paste(names(ped),
                                                 collapse = "', '"),
                        "'"), name = "nprcgenekeepr")
      flog.debug(" - in tryCatch before setPopulation.", name = "nprcgenekeepr")
      ## setPopulation adds the population column if not already present
      ## setPopulation indicates all id to be in the population if
      ##  specifyFocalAnimals() is NULL
      ## otherwise ids returned by specifyFocalAnimals() are set to TRUE and
      ##  others become FALSE
      ped <- setPopulation(ped, specifyFocalAnimals())
      flog.debug(paste0("column names: '", paste0(names(ped),
                                                  collapse = "', '"), "'"),
                 name = "nprcgenekeepr")
      flog.debug(paste0("setPopulation() called\n"), name = "nprcgenekeepr")

      if (input$trim) {
        probands <- ped$id[ped$population]
        ped <- trimPedigree(probands,
                            ped,
                            removeUninformative = FALSE,
                            addBackParents = FALSE)
        #ped <- trimPedigree(probands, ped, removeUninformative = TRUE,
        #                  addBackParents = TRUE)
        flog.debug(paste0("trimPedigree() called\n"), name = "nprcgenekeepr")
      }

      ped["pedNum"] <- findPedigreeNumber(ped$id, ped$sire, ped$dam)
      ped["gen"] <- findGeneration(ped$id, ped$sire, ped$dam)

      ped
    }, error = function(cond) {
      return(FALSE)
    })

    validate(need(ped, geterrmessage()))

    return(ped)
  })

  # Changing the active tab to the "Pedigree Browser" tab
  observe({
    status <- getSelectedBreeders()
    if (!is.null(status))
      updateTabsetPanel(session, "tab_pages", selected = "Pedigree Browser")
  })

  # Creating the pedigree table to be displayed on the Pedigree Browser tab
  output$pedigree <- DT::renderDataTable(DT::datatable({
    if (is.null(getPed())) {
      return(NULL)
    }

    # convert columns to "character" so xtables displays them properly
    ped <- toCharacter(getPed())

    if (!input$uid) {
      ped <- ped[!grepl("^U", ped$id, ignore.case = TRUE), ]
      ped$sire[grepl("^U", ped$sire, ignore.case = TRUE)] <- NA
      ped$dam[grepl("^U", ped$dam, ignore.case = TRUE)] <- NA
    }

    names(ped) <- headerDisplayNames(names(ped))

    ped
  }))

  specifyFocalAnimals <- eventReactive(input$specifyFocalAnimal, {
    ped <- unlist(strsplit(input$focalAnimalIds, "[ ,;\t\n]"))
    if (!is.null(input$focalAnimalUpdate)) {
      if (!input$clearFocalAnimals) {
        focalAnimalUpdate <- input$focalAnimalUpdate
        flog.debug(
          paste0(
            "focalAnimalUpdate - focalAnimalUpdate$name: ",
            focalAnimalUpdate$name,
            "; ",
            "focalAnimalUpdate$datapath: ",
            focalAnimalUpdate$datapath
          ),
          name = "nprcgenekeepr"
        )
      } else {
        focalAnimalUpdate <-
          list(
            name = "emptyFocalAnimals.csv",
            datapath = system.file("extdata", "emptyFocalAnimals.csv",
                                   package  = "nprcgenekeepr")
          )

        flog.debug(
          paste0(
            "focalAnimalUpdate - focalAnimalUpdate$name: ",
            focalAnimalUpdate$name,
            "; ",
            "focalAnimalUpdate$datapath: ",
            focalAnimalUpdate$datapath
          ),
          name = "nprcgenekeepr"
        )
      }
      focalAnimalUpdateDf <- unlist(
        read.table(
          focalAnimalUpdate$datapath,
          header = TRUE,
          sep = ",",
          stringsAsFactors = FALSE,
          na.strings = c("", "NA"),
          check.names = FALSE
        )
      )
      flog.debug(paste0(
        "focalAnimalUpdate - focalAnimalUpdateDf: ",
        focalAnimalUpdateDf
      ),
      name = "nprcgenekeepr")
      updateTextAreaInput(
        session,
        "focalAnimalIds",
        label = paste0(focalAnimalUpdateDf),
        value = paste0(focalAnimalUpdateDf)
      )
      ped <- focalAnimalUpdateDf
    }
    if (length(ped) == 0L) {
      return(NULL)
    } else {
      return(ped)
    }
  }, ignoreNULL = FALSE)

  # Download handler to download the full or trimmed pedigree
  output$downloadPedigree <- downloadHandler(
    filename = function() {
      paste0("Pedigree", ".csv")
    },
    content = function(file) {
      write.csv(getPed(), file, na = "", row.names = FALSE)
    }
  )
  #############################################################################
  # Functions for handling the genetic value analysis generation

  geneticValue <- eventReactive(input$analysis, {
    if (is.null(getPed())) {
      return(NULL)
    }
    # Ensuring the pedigree has been trimmed
    # (if there are too many animals, the program will crash)
    ped <- getPed()
    probands <- ped$id[ped$population]
    ped <- trimPedigree(probands,
                        ped,
                        removeUninformative = FALSE,
                        addBackParents = FALSE)

    validate(need(length(probands) != 0L, "Error: No population specified"))

    # Setting up the progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())

    updateProgress <- function(n = 1L,
                               detail = NULL,
                               value = 0L,
                               reset = FALSE) {
      if (reset) {
        progress$set(detail = detail, value = value)
      } else {
        progress$inc(amount = 1L / n)
      }
    }
    #
    return(
      reportGV(
        ped,
        guIter = input$iterations,
        guThresh = as.integer(input$threshold),
        byID = TRUE,
        updateProgress = updateProgress
      )
    )
  })
  # Returns the geneticValue() report
  rpt <- reactive({
    if (is.null(geneticValue())) {
      return(NULL)
    }
    return(geneticValue()[["report"]])
  })

  # Functions for displaying the Genetic Value Analysis
  gvaView <- reactive({
    if (is.null(rpt())) {
      return(NULL)
    }
    if (input$view == 0L) {
      return(rpt())
    } else {
      ids <- unlist(strsplit(isolate(input$viewIds), "[ ,;\t\n]"))
      ids <- ids[stri_trim(ids) != ""]
      if (length(ids) == 0L) {
        return(rpt())
      } else {
        return(filterReport(ids, rpt()))
      }
    }
  })

  output$gva <- DT::renderDataTable(DT::datatable({
    if (is.null(rpt())) {
      return(NULL)
    }

    g <- gvaView()
    g$indivMeanKin <- round(g$indivMeanKin, 5L)
    g$zScores <- round(g$zScores, 2L)
    g$gu <- round(g$gu, 5L)
    g <- toCharacter(g)
    names(g) <- headerDisplayNames(names(g))

    return(g)
  }))

  # Download handlers for all or a subset of the Genetic Value Analysis
  output$downloadGVAFull <- downloadHandler(
    filename = function() {
      paste0("GVA_full", ".csv")
    },
    content = function(file) {
      write.csv(rpt(), file, na = "", row.names = FALSE)
    }
  )

  output$downloadGVASubset <- downloadHandler(
    filename = function() {
      paste0("GVA_subset", ".csv")
    },
    content = function(file) {
      write.csv(gvaView(), file, na = "", row.names = FALSE)
    }
  )

  #############################################################################
  # Functions for handling printing summary statistics and outputting the
  # kinship matrix

  kmat <- reactive({
    if (is.null(geneticValue())) {
      return(NULL)
    }
    return(geneticValue()[["kinship"]])
  })

  # Download handler for the kinship matrix
  output$downloadKinship <- downloadHandler(
    filename = function() {
      paste0("Kinship", ".csv")
    },
    content = function(file) {
      write.csv(kmat(), file, na = "")
    }
  )

  output$summaryStats <- renderText({
    if (is.null(geneticValue())) {
      return(NULL)
    }

    f <- geneticValue()[["total"]]
    nmf <- geneticValue()[["nMaleFounders"]]
    nff <- geneticValue()[["nFemaleFounders"]]
    fe <- geneticValue()[["fe"]]
    fg <- geneticValue()[["fg"]]

    mk <- summary(rpt()[, "indivMeanKin"])
    gu <- summary(rpt()[, "gu"])
    fe_title_txt <- JS(
      paste(
        "Founder equivalents estimates the expected number
	of equally contributing founders that would be
	required to produce the observed genetic diversity
	in the current population. $f_e = 1 / \\sigma;(p_{i_{2}})$"
      )
    )
    #  </MATH>Where <MATH>p<sub>i</sub></MATH>is the proportion of the genes of
    # the living,
    #	descendant population contributed by founder <MATH>i</MATH>."))
    founder <- htmltools::withTags(table(class = "display", thead(tr(
      th("Known Founders"),
      th("Known Female Founders"),
      th("Known Male Founders"),
      th(JS("Founder Equivalents")),
      th("Founder Genome Equivalents")
    )), tbody(
      td(as.character(f)),
      td(as.character(nff)),
      td(as.character(nmf)),
      td(as.character(round(fe, digits = 2L))),
      td(as.character(round(fg, digits = 2L)))
    )))

    header <- paste(
      "<tr>",
      "<th></th>",
      "<th>Min</th>",
      "<th>1st Quartile</th>",
      "<th>Mean</th>",
      "<th>Median</th>",
      "<th>3rd Quartile</th>",
      "<th>Max</th>",
      "</tr>"
    )

    k <- paste(
      "<tr>",
      "<td>Mean Kinship</td>",
      "<td>",
      as.character(round(mk["Min."], 4L)),
      "</td>",
      "<td>",
      as.character(round(mk["1st Qu."], 4L)),
      "</td>",
      "<td>",
      as.character(round(mk["Mean"], 4L)),
      "</td>",
      "<td>",
      as.character(round(mk["Median"], 4L)),
      "</td>",
      "<td>",
      as.character(round(mk["3rd Qu."], 4L)),
      "</td>",
      "<td>",
      as.character(round(mk["Max."], 4L)),
      "</td>",
      "</tr>"
    )

    g <- paste(
      "<tr>",
      "<td>Genome Uniqueness</td>",
      "<td>",
      as.character(round(gu["Min."], 4L)),
      "</td>",
      "<td>",
      as.character(round(gu["1st Qu."], 4L)),
      "</td>",
      "<td>",
      as.character(round(gu["Mean"], 4L)),
      "</td>",
      "<td>",
      as.character(round(gu["Median"], 4L)),
      "</td>",
      "<td>",
      as.character(round(gu["3rd Qu."], 4L)),
      "</td>",
      "<td>",
      as.character(round(gu["Max."], 4L)),
      "</td>",
      "</tr>"
    )

    return(paste(founder, "<br>", "<br>", "<table>", header, k, g, "</table>"))
  })
  mkHistogram <- function() {
    mk <- rpt()[, "indivMeanKin"]
    avg <- mean(mk, na.rm = TRUE)
    # nolint start: commented_code_linter.
    # std.dev <- sd(mk, na.rm = TRUE)
    # upper <- avg + (2L * std.dev)
    # lower <- avg - (2L * std.dev)
    # nolint end: commented_code_linter
    brx <- pretty(range(mk), 25L)
    ggplot(data.frame(mk = mk), aes(x = mk, y = ..density..)) +
      geom_histogram(
        bins = 25L,
        color = "darkblue",
        fill = "lightblue",
        breaks = brx
      ) +
      theme_classic() +
      xlab("Kinship") + ylab("Frequency") +
      ggtitle("Distribution of Individual Mean Kinship Coefficients") +
      geom_vline(aes(xintercept = avg, color = "red"),
                 linetype = "dashed",
                 show.legend = FALSE)# +
  }
  output$mkHist <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    mkHistogram()
  })
  zscoreHistogram <- function() {
    z <- rpt()[, "zScores"]
    avg <- mean(z, na.rm = TRUE)
    # nolint start: commented_code_linter.
    # std.dev <- sd(z, na.rm = TRUE)
    # upper <- avg + (2L * std.dev)
    # lower <- avg - (2L * std.dev)
    # nolint end: commented_code_linter

    brx <- pretty(range(z), 25L)
    ggplot(data.frame(z = z), aes(x = z, y = ..density..)) +
      geom_histogram(
        bins = 25L,
        color = "darkblue",
        fill = "lightblue",
        breaks = brx
      ) +
      theme_classic() +
      xlab("Z-Score") + ylab("Frequency") +
      ggtitle("Distribution of Mean Kinship Coefficients Z-scores") +
      geom_vline(aes(xintercept = avg, color = "red"),
                 linetype = "dashed",
                 show.legend = FALSE)# +
  }
  output$zscoreHist <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    zscoreHistogram()
  })

  guHistogram <- function() {
    gu <- rpt()[, "gu"]
    avg <- mean(gu, na.rm = TRUE)
    # nolint start: commented_code_linter.
    # std.dev <- sd(gu, na.rm = TRUE)
    # upper <- avg + (2 * std.dev)
    # lower <- avg - (2 * std.dev)
    # nolint end: commented_code_linter

    brx <- pretty(range(gu), 25L)
    ggplot(data.frame(gu = gu), aes(x = gu, y = ..density..)) +
      geom_histogram(
        color = "darkblue",
        fill = "lightblue",
        breaks = brx
      ) +
      theme_classic() +
      xlab("Genome Uniqueness Score") + ylab("Frequency") +
      ggtitle("Distribution of Genome Uniqueness") +
      geom_vline(aes(xintercept = avg, color = "red"),
                 linetype = "dashed",
                 show.legend = FALSE)
  }
  output$guHist <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    guHistogram()
  })

  meanKinshipBoxPlot <- function() {
    gu <- rpt()[, "indivMeanKin"]
    ggplot(data.frame(gu = gu), aes(x = 0L, y = gu)) +
      geom_boxplot(
        color = "darkblue",
        fill = "lightblue",
        notch = FALSE,
        outlier.color = "red",
        outlier.shape = 1L
      ) +
      theme_classic() + geom_jitter(width = 0.2) + coord_flip() +
      ylab("Kinship")  +
      ggtitle("Boxplot of Individual Mean Kinship Coefficients") +
      xlab("")
  }

  output$mkBox <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    meanKinshipBoxPlot()
  })
  zscoreBoxPlot <- function() {
    gu <- rpt()[, "zScores"]
    ggplot(data.frame(gu = gu), aes(x = 0L, y = gu)) +
      geom_boxplot(
        color = "darkblue",
        fill = "lightblue",
        notch = FALSE,
        outlier.color = "red",
        outlier.shape = 1L
      ) +
      theme_classic() + geom_jitter(width = 0.2) + coord_flip() +
      ylab("Z-Score") +
      ggtitle("Boxplot of Mean Kinship Coefficients Z-scores") +
      xlab("")
  }
  output$zscoreBox <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    zscoreBoxPlot()
  })
  guBoxPlot <- function() {
    gu <- rpt()[, "gu"]
    ggplot(data.frame(gu = gu), aes(x = 0L, y = gu)) +
      geom_boxplot(
        color = "darkblue",
        fill = "lightblue",
        notch = FALSE,
        outlier.color = "red",
        outlier.shape = 1L
      ) +
      theme_classic() + geom_jitter(width = 0.2) + coord_flip() +
      ylab("Genome Uniquness")  + ggtitle("Boxplot of Genome Uniqueness") +
      xlab("")
  }

  output$guBox <- renderPlot({
    if (is.null(rpt())) {
      return(NULL)
    }
    guBoxPlot()
  })
  box_and_whisker_desc <- paste0(
    "The upper whisker extends from the hinge to
                              the largest value no further than 1.5 * IQR
                              from the hinge (where IQR is the
                              inter-quartile range, or distance between
                              the first and third quartiles). The lower
                              whisker extends from the hinge to the
                              smallest value at most 1.5 * IQR of the
                              hinge. Data beyond the end of the whiskers
                              are called \"outlying\" points and are plotted
                              individually."
  )
  addPopover(
    session,
    "mkBox",
    "Mean Kinship Coefficients",
    content = box_and_whisker_desc,
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "zscoreBox",
    "Z-scores",
    content = box_and_whisker_desc,
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )
  addPopover(
    session,
    "guBox",
    "Genetic Uniqueness",
    content = box_and_whisker_desc,
    placement = "bottom",
    trigger = "hover",
    options = NULL
  )



  # nolint start: commented_code_linter.
  # output$relations <- eventReactive(input$displayRelations, {
  #   DT::renderDataTable(DT::datatable({
  #     if (is.null(kmat())) {
  #       return(NULL)
  #     }
  #     j <- nrow(kmat()) * ncol(kmat())
  #     # Setting up the progress bar
  #     progress <- shiny::Progress$new()
  #     on.exit(progress$close())
  #     progress$set(message = "Finding Relationship Designations", value = 0)
  #     updateProgress <- function() {
  #       progress$inc(amount = 1 / j)
  #     }
  #
  #     kin <- convertRelationships(kmat(), getPed(),
  #                                 updateProgress = updateProgress)
  #
  #     progress$set(message = "Preparing Table", value = 1)
  #     r <- makeRelationClassesTable(kin)
  #
  #     toCharacter(r)
  #   })
  # )})
  # nolint end: commented_code_linter

  # Download handler for the male founders
  output$downloadMaleFounders <- downloadHandler(
    filename = function() {
      paste0("maleFounders", ".csv")
    },
    content = function(file) {
      mf <- geneticValue()[["maleFounders"]]
      write.csv(mf, file, na = "")
    }
  )
  # Download handler for the male founders
  output$downloadFemaleFounders <- downloadHandler(
    filename = function() {
      paste0("femaleFounders", ".csv")
    },
    content = function(file) {
      ff <- geneticValue()[["femaleFounders"]]
      write.csv(ff, file, na = "")
    }
  )

  # Download handler for the first-order relationships
  output$downloadFirstOrder <- downloadHandler(
    filename = function() {
      paste0("FirstOrder", ".csv")
    },
    content = function(file) {
      ped <- getPed()
      r <- countFirstOrder(ped, ids = ped$id[ped$population])
      write.csv(r, file, na = "")
    }
  )

  output$downloadRelations <- downloadHandler(
    filename = function() {
      paste0("Relations", ".csv")
    },
    content = function(file) {
      ped <- getPed()
      probands <- ped$id[ped$population]
      r <- convertRelationships(kmat(), getPed(), ids = probands)
      write.csv()
    }
  )
  ## Save plots
  output$downloadMeanKinshipCoefficientHistogram <- downloadHandler(
    filename = "meanKinshipCoefficientHistogram.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = 300L,
          units = "in"
        )
      }
      ggsave(file, plot = mkHistogram(), device = "png")
    }
  )
  output$downloadZScoreHistogram <- downloadHandler(
    filename = "meanKinshipCoefficientsZscoreHistogram.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = 300L,
          units = "in"
        )
      }
      ggsave(file, plot = zscoreHistogram(), device = "png")
    }
  )
  output$downloadGenomeUniquenessHistogram <- downloadHandler(
    filename = "geneticUniquenessHistogram.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = 300L,
          units = "in"
        )
      }
      ggsave(file, plot = guHistogram(), device = "png")
    }
  )
  output$downloadMeanKinshipCoefficientBoxPlot <- downloadHandler(
    filename = "meanKinshipCoefficientsBox.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = 300L,
          units = "in"
        )
      }
      ggsave(file, plot = meanKinshipBoxPlot(), device = "png")
    }
  )
  output$downloadZScoreBoxPlot <- downloadHandler(
    filename = "meanKinshipCoefficientsZscoresBox.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = 300L,
          units = "in"
        )
      }
      ggsave(file, plot = zscoreBoxPlot(), device = "png")
    }
  )
  output$downloadGenomeUniquenessBoxPlot <- downloadHandler(
    filename = "geneticUniquenessBox.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = 300L,
          units = "in"
        )
      }
      ggsave(file, plot = guBoxPlot(), device = "png")
    }
  )


  # nolint start: commented_code_linter.
  # ### Display Founders
  # # Creating the male founder table for display on the Summary Statistics tab
  # output$maleFounders <- DT::renderDataTable(DT::datatable({
  #   if (is.null(geneticValue()[["maleFounders"]])) {
  #     return(NULL)
  #   }
  #   # convert columns to "character" so xtables displays them properly
  #   ped <- toCharacter(geneticValue()[["maleFounders"]])
  #   names(ped) <- headerDisplayNames(names(ped))
  #   ped
  # }))
  # # Creating the male founder table for display on the Summary Statistics tab
  # output$femaleFounders <- DT::renderDataTable(DT::datatable({
  #   if (is.null(geneticValue()[["femaleFounders"]])) {
  #     return(NULL)
  #   }
  #   # convert columns to "character" so xtables displays them properly
  #   ped <- toCharacter(geneticValue()[["femaleFounders"]])
  #   names(ped) <- headerDisplayNames(names(ped))
  #   ped
  # }))
  # nolint end: commented_code_linter.

  #############################################################################
  # Functions for handling the breeding group formation process
  textAreaWidget <- eventReactive(input$seedAnimals, {
    seedAnimalList <- lapply(1L:input$numGp, function(i) {
      inputName <- paste0("curGrp", i)
      textInputRow <- function(inputId, value) {
        textAreaInput(
          inputId = inputName,
          paste0("Seed animals ", i),
          value = "",
          rows = 5L,
          cols = 20L,
          resize = "both"
        )
      }
      column(2L, offset = 0L, textInputRow(inputName, ""))
    })
    do.call(tagList, seedAnimalList)
  }, ignoreInit = FALSE)

  getCurrentGroups <- reactive({
    currentGroups <- vapply(seq_len(input$numGp),
                            function(i) character(0L),
                            character(0L))
    for (i in seq_along(input$numGp)) {
      inputName <- paste0("curGrp", i)
      if (is.null(input[[inputName]]))
        # seed animal option is not selected
        break
      currentGroups[[i]] <-
        stri_remove_empty(unlist(strsplit(input[[inputName]], "[, \t\n]")))
    }
    currentGroups
  })
  output$currentGroups <- renderUI({
    textAreaWidget()
  })
  output$getCurrentGroups <- renderText({
    getCurrentGroups()
  })
  textMinParentAge <- eventReactive(input$group_formation_rb, {
    minParentAgeLine <-
      checkboxInput(
        "useMinParentAge",
        label = paste0(
          "Animals will be grouped with the mother ",
          "below the minimum parent age of ",
          globalMinParentAge,
          "."
        ),
        value = FALSE
      )
    do.call(tagList, list(minParentAgeLine))
  }, ignoreInit = FALSE)

  output$minParentAge <- renderUI({
    textMinParentAge()
  })

  bg <- eventReactive(input$grpSim, {
    if (is.null(rpt())) {
      return(NULL)
    }
    currentGroups <- getCurrentGroups()
    currentGroupIds <- unlist(currentGroups)

    ped <- getPed()
    # Filter out unknown animals added into ped
    ped <- removeUnknownAnimals(ped)
    ids <- character(0L)
    if (input$group_formation_rb == "candidates")
      ids <- unlist(strsplit(input$grpIds, "[, \t\n]"))

    if (length(ids) > 0L) {
      ped <- resetGroup(ped, ids)
      candidates <- ids
    } else {
      candidates <- getGrpIds()
    }

    # Assume an animal that is in the group can't also be a candidate
    if (length(currentGroupIds) > 0L) {
      candidates <- setdiff(candidates, currentGroupIds)
    }

    # Filter out low-value animals if desired
    useLv <- input$group_formation_rb != "high-value"
    if (!useLv) {
      rpt <- rpt()
      lv <- rpt$id[rpt$value == "low value"]
      candidates <- setdiff(candidates, lv)
    }
    candidates <- intersect(candidates, ped$id)

    harem <- input$group_sex_rb == "harems"

    validate(need(length(candidates == 0L), "No candidates defined"),
             need(
               !(length(setdiff(
                 candidates, ped$id
               )) > 0L),
               paste(
                 "Group candidates present that are",
                 "not in the provided pedigree\n",
                 paste(setdiff(candidates, ped$id), sep = "\n")
               )
             ),
             need(
               !(length(setdiff(
                 currentGroupIds, ped$id
               )) > 0L),
               paste(
                 "Current group members present that",
                 "are not in the provided pedigree\n",
                 paste(setdiff(currentGroupIds, ped$id), sep = "\n")
               )
             ))
    ignore <- input$ffRel
    ignore <- if (ignore)
      list(c("F", "F"))
    else
      NULL
    threshold <- input$kinThresh
    if (input$useMinParentAge) {
      minAge <- globalMinParentAge
      output$minParentAge <- renderText({
        paste0(minAge)
      })
    } else {
      minAge <- input$minAge
    }

    withKin <- input$withKin
    iter <- input$gpIter
    numGp <- ({
      input$numGp
    })
    sexRatio <- input$sexRatio
    # Setting up the progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Generating Groups", value = 0L)

    n <- 1L
    updateProgress <- function() {
      progress$inc(amount = 1L / iter,
                   detail = paste("Iteration:", n))
      n <<- n + 1L
    }

    grp <- groupAddAssign(
      currentGroups = currentGroups,
      kmat = kmat(),
      ped = ped,
      candidates = candidates,
      threshold = threshold,
      ignore = ignore,
      minAge = minAge,
      iter = iter,
      numGp = numGp,
      harem = harem,
      sexRatio,
      withKin = withKin,
      updateProgress = updateProgress
    )

    return(grp)
  })

  getGrpIds <- reactive({
    ped <- getPed()
    ped <- ped[is.na(ped$exit) & !is.na(ped$birth), ]
    if ("group" %in% colnames(ped)) {
      return(ped$id[ped$group])
    } else {
      return(ped$id[ped$population])
    }
  })

  # Functions to handle breeding group display
  observe({
    if (!is.null(bg())) {
      x <- length(bg()$group)

      if (x > 1L) {
        gp <- list()
        for (i in 1L:(x - 1L)) {
          gp[paste("Group", as.character(i))] <- i
        }
        gp["Unused"] <- x

        updateSelectInput(
          session,
          "viewGrp",
          label = "Enter the group to view:",
          choices = gp,
          selected = 1L
        )
      } else if (x == 1L) {
        updateSelectInput(
          session,
          "viewGrp",
          label = "Enter the group to view:",
          choices = list(Unused = 1L),
          selected = 1L
        )
      } else {
        updateSelectInput(
          session,
          "viewGrp",
          label = "Enter the group to view:",
          choices = list(" " = 1L),
          selected = 1L
        )
      }
    }
  })

  bgGroupView <- reactive({
    if (is.null(bg())) {
      return(NULL)
    }
    i <- withinIntegerRange(input$viewGrp,
                            minimum = 1L,
                            maximum = input$numGp)[1L]
    gp <- bg()$group[[i]]
    ped <- getPed()
    gp <- addSexAndAgeToGroup(gp, ped)
    gp$age <- round(gp$age, 1L)
    colnames(gp) <- c("Ego ID", "Sex", "Age in Years")

    if (nrow(gp) == 0L) {
      return(NULL)
    } else {
      return(gp[order(gp$`Ego ID`), , drop = FALSE])
    }
  })
  bgGroupKinView <- reactive({
    if (is.null(bg()$groupKin)) {
      return(NULL)
    }
    i <- as.numeric(input$viewGrp)
    kmat <- bg()$groupKin[[i]]
    kmat <- as.data.frame(round(kmat, 6L))

    if (nrow(kmat) == 0L) {
      return(NULL)
    } else {
      return(kmat)
    }
  })

  output$breedingGroups <- DT::renderDataTable(DT::datatable({
    if (is.null(bg())) {
      return(NULL)
    }
    return(bgGroupView())
  }))
  output$breedingGroupKin <- DT::renderDataTable(DT::datatable({
    if (is.null(bg()$groupKin)) {
      return(NULL)
    }
    return(bgGroupKinView())
  }))

  # Download handler for the current group
  output$downloadGroup <- downloadHandler(
    filename = getDatedFilename(paste0("Group-", input$viewGrp, ".csv")),
    content = function(file) {
      write.csv(bgGroupView(), file, na = "", row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$downloadGroupKin <- downloadHandler(
    filename = getDatedFilename(paste0("GroupKin-", input$viewGrp, ".csv")),
    content = function(file) {
      write.csv(bgGroupKinView(), file, na = "", row.names = TRUE)
    },
    contentType = "text/csv"
  )

  #############################################################################
  # Function to handle display of pyramid plot
  flog.debug("before renderPlot(getPyramidPlot(ped)))", name = "nprcgenekeepr")
  output$pyramidPlot <- renderPlot(getPyramidPlot(getPed()))
  flog.debug("after renderPlot(getPyramidPlot(ped)))", name = "nprcgenekeepr")
})
