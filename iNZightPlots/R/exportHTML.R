#' ExportHTML
#'
#' \code{exportHTML} is designed to export the iNZight plot as a dynamic, interactive HTML page.
#' Currently only handles single panel plots. Coloured hex plots are currently not available yet.
#'
#' @param x An iNZight plot object that captures iNZight environment
#' @param file Name of temporary HTML file generated (defaults to `index.html`
#' in a temporary directory, or other as specified using `dir`)
#' @param local Logical for creating local files for offline use (default to false)
#' @param dir A directory to store the file and output
# Additional parameters for scatterplots and dotplots only:
#' @param data dataset/dataframe that you wish to investigate and export more variables from
#' @param extra.vars extra variables specified by the user to be exported
#' @param width the desired width of the SVG plot
#' @param height the desired height of the SVG plot
#' @param mapObj iNZightMap object (from iNZightMaps)
#' @param ... extra arguments
#'
#' @return an inzHTML object consisting of a link to an HTML rendering
#'  of \code{x} with filename \code{file},
#'  which can be loaded in the browser (for example using \code{browseURL},
#'  or calling the \code{print()} method of the returned object.
#'
#' @examples
#' \dontrun{
#' x <- iNZightPlot(Petal.Width, Petal.Length, data = iris, colby = Species)
#' exportHTML(x, "index.html")
#'
#' #to export more variables for scatterplots:
#'  exportHTML(x, "index.html", data = iris, extra.vars = c("Sepal.Length", "Sepal.Width"))
#' }
#'
#' @author Yu Han Soh
#' @export
exportHTML <- function(x,
                       file = file.path(dir, "index.html"),
                       data,
                       local = FALSE,
                       dir = tempdir(),
                       extra.vars,
                       ...)
    UseMethod("exportHTML")

#' @describeIn exportHTML method for an iNZightPlot-generating function
#' @export
exportHTML.function <- function(x,
                                file = file.path(dir, "index.html"),
                                data = NULL,
                                local = FALSE,
                                dir = tempdir(),
                                extra.vars = NULL,
                                width = dev.size()[1], height = dev.size()[2],
                                ...) {
    pdf(NULL, width = width, height = height, onefile = TRUE)
    cdev <- dev.cur()
    on.exit(dev.off(cdev), add = TRUE)

    #do exporting:
    obj <- x()
    url <- exportHTML(obj, file, data = data, local = local, dir = dir, extra.vars = extra.vars)

    ## pass URL from exportHTML.inzplotoutput
    invisible(url)
}

#' @describeIn exportHTML method for iNZightMaps or other supported ggplot graphs
#' @export
exportHTML.ggplot <- function(x,
                              file = file.path(dir, "index.html"),
                              data = NULL,
                              local = FALSE,
                              dir = tempdir(),
                              extra.vars = NULL, mapObj,
                              ...) {
    if (missing(mapObj)) {
        if (!requireNamespace("plotly", quietly = TRUE)) {
            stop(
                paste(
                    "The 'plotly' package is needed for this functionality.",
                    "",
                    "You can run the following to install it:",
                    "  install.packages(\"plotly\")",
                    "",
                    sep = "\n"
                )
            )
        }
        return(plotly::ggplotly(x))
    }

    if (!inherits(mapObj, "iNZightMapPlot")) {
        stop("mapObj is not an 'iNZightMapPlot' object. This is only available for iNZightMaps.
            Are you using the new iNZightMaps module?")
    }

    # package check:
    if( !requireNamespace("gridSVG",  quietly = TRUE) ||
        !requireNamespace("knitr",   quietly = TRUE) ||
        !requireNamespace("jsonlite", quietly = TRUE) ) {
        stop(
            paste("Required packages aren't installed",
                "Use 'install.packages('iNZightPlots', dependencies = TRUE)' to install them.",
                sep = "\n"
            )
        )
    }

    # print(x)
    mapObj <- mapObj
    plot <- x
    dt <- as.data.frame(plot[[1]])
    spark <- NULL
    lineType <- NULL
    timeData <- data.frame(mapObj$region.data)
    multi <- mapObj$multiple.obs
    seqVar <- mapObj$sequence.var
    lab <- plot$labels

    if (mapObj$type == "region") {
        colby <- lab$fill
        if (is.character(dt[[colby]]) || is.factor(dt[[colby]])) {
            # sort by alphabetical order for character/factor variables
            dt <- dt[order(dt[[colby]]), ]
            rownames(dt) <- 1:nrow(dt)
        }
        varNames <- c(mapObj$region.var, colby)
    } else if (mapObj$type == "point") {
        colby <- lab$colour
        sizeby <- lab$size
        varNames <- c(mapObj$region.var, colby, sizeby)
    } else {
        ## for sparklines
        colby <- lab$colour
        region <- lab$group
        timex <- lab$line_x
        timey <- lab$line_y
        varNames <- c(mapObj$region.var, timex, timey)
        ## extract sparkline type
        code <- attr(plot, "code")["sparklines"]
        lineType <- sub(".*, sparkline_type = ([[:alpha:]]+).*", "\\1", code)
    }

    # get palette colours used: continuous scale
    grid::grid.force()
    bar <- grid::grid.get("bar.4-2-4-2")
    pal <- rev(as.vector(bar$raster))

    # drop the last column (geometry)
    dt <- dt[, - which(names(dt) == "geometry")]
    timeData <- timeData[, - which(names(timeData) == "geometry")]
    tab <- if (mapObj$type == "sparklines" || multi) timeData else dt
    # only extract variables that are numeric
    t <- sapply(tab, is.numeric)
    numVar <- colnames(tab)[t]
    # find time interval
    if (is.null(seqVar)) {
        int <- NULL
    } else {
        d <- unique(diff(tab[,seqVar]))
        int <- d[which(d > 0)]
    }

    n_polygons <- mapObj$n_polygons

    chart <- list(
        type = mapObj$type,
        data = dt,
        names = varNames,
        palette = pal,
        numVar = numVar,
        multi = multi,
        seqVar = seqVar,
        int = int,
        timeData = timeData,
        sparkline_type = lineType,
        n_polygons = rep(1:length(n_polygons), n_polygons)
    )
    tbl <- list(tab = tab, includeRow = TRUE, cap = "Data")
    mapsJS <- paste(
        readLines(system.file("maps.js", package = "iNZightPlots")),
        collapse = "\n"
    )
    js <- list(chart = jsonlite::toJSON(chart), jsFile = mapsJS)

    url <- createHTML(tbl, js, file, local, dir = dir)
    invisible(url)

}

#' @describeIn exportHTML method for output from iNZightPlot
#' @export
exportHTML.inzplotoutput <- function(x,
                                     file = file.path(dir, "index.html"),
                                     data = NULL,
                                     local = FALSE,
                                     dir = tempdir(),
                                     extra.vars = NULL, ...) {

    #suggest gridSVG, jsonlite, xtable:
    if( !requireNamespace("gridSVG",  quietly = TRUE) ||
        !requireNamespace("knitr",   quietly = TRUE) ||
        !requireNamespace("jsonlite", quietly = TRUE) ) {
        stop(paste("Required packages aren't installed",
                "Use 'install.packages('iNZightPlots', depends = TRUE)' to install them.",
                sep = "\n"))
    }

    x <- x
    plot <- x$all$all

    if (is.null(plot)) {
        if (length(x$all) == 1) { ## for subsets = 1
            plot <- x$all[[1]]
        } else { ## subsets > 1
            warning("iNZight doesn't handle interactive panel plots yet!")
            return()
        }
    }

    #condition for colored hexplots - currently unavailable:
    if(attributes(x)$plottype == "hex" && !is.null(plot$colby)) {
        warning('iNZight cannot handle interactive colored hex plots yet!')
        return()
    }

    # Get data (refer to getInfo function):
    if (is.null(extra.vars) && is.null(data)) {
        info <- getInfo(plot, x)
    } else {
        info <- getInfo(plot, x, data, extra.vars)
    }

    tbl <- info$tbl
    js <- info$js

    #create html
    url <- createHTML(tbl, js, file, local, dir = dir)
    invisible(url)

}

## create HTML - processes and generates HTML file
## @param tbl - list with table information
## @param js - list with javascript information
## @param file - filename for HTML file created
## @param local - logical if it's to be made in a folder with source files
## @return url object of class inzHTML
createHTML <- function(tbl, js,
                       file = file.path(dir, "index.html"),
                       local = FALSE,
                       dir = tempdir()
                       ) {

    # load templates
    HTMLtemplate <- readLines(system.file("template.html", package = "iNZightPlots"))
    styles <- paste(
        readLines(system.file("style.css", package = "iNZightPlots")),
        collapse = "\n"
    )
    inzJS <- paste(
        readLines(system.file("inzplot.js", package = "iNZightPlots")),
        collapse = "\n"
    )
    # generate svg
    svgOutput <- gridSVG::grid.export(NULL)$svg
    svgCode <- paste(capture.output(svgOutput), collapse = "\n")

    # generate HTML table
    if (is.null(tbl)) {
        HTMLtable <- '<p> No table available. </p>'
    } else {
        # switched from xtable to knitr: table in desired format for dataTables to work
        HTMLtable <- knitr::kable(tbl$tab,
            format = "html",
            row.names = tbl$includeRow,
            align = "c",
            caption = tbl$cap,
            table.attr = "id=\"table\" class=\"table table-condensed table-hover\" cellspacing=\"0\""
        )
    }

    jsCode <- js$jsFile
    chartCode <- paste0("var chart = ", js$chart, ";")

    #finding places where to substitute code:
    svgLine <- grep("SVG", HTMLtemplate)
    cssLine <- grep("styles.css", HTMLtemplate)
    inzplotLine <- grep("inzplot", HTMLtemplate)
    jsLine <- grep("JSfile", HTMLtemplate)
    chartLine <- grep("chart.json", HTMLtemplate)
    tableLineOne <- grep("<!-- insert table -->", HTMLtemplate)

    if (local) {
        assets <- system.file("assets.zip", package = "iNZightPlots")
        ## if local = TRUE, create directories
        # outdir <- file.path(dirname(file), "iNZight_interactive_plot")
        utils::unzip(assets, exdir = dir)
        # if assets is in the same directory as file ...
        assets_dir <- file.path(dir, "assets")
        assets <- if (dir == dirname(file)) "assets" else assets_dir

        vendorCSS <- c("bootstrap.min.css", "dataTables.bootstrap.min.css")
        HTMLtemplate[9:10] <- paste0(
            "<link rel='stylesheet' href= '", assets, "/vendor/",
            vendorCSS,
            "'>"
        )
        vendorJS <- c(
            "jquery.min.js",
            "d3.v4.min.js",
            "bootstrap.min.js",
            "jquery.dataTables.min.js",
            "dataTables.bootstrap.min.js"
        )
        HTMLtemplate[12:16] <- paste0(
            "<script src ='", assets, "/vendor/",
            vendorJS,
            "'></script>"
        )

        # create files
        write(styles, file.path(assets_dir, "main.css"))
        write(chartCode, file.path(assets_dir, "chart.js"))
        write(inzJS, file.path(assets_dir, "inzplot.js"))
        write(jsCode, file.path(assets_dir, "main.js"))
        file.copy(
            system.file("inzight_transp.png", package = "iNZightPlots"),
            file.path(assets_dir, "inzight_transp.png")
        )

        HTMLtemplate[cssLine] <- sprintf("<link rel='stylesheet' href='%s/main.css'>", assets)
        HTMLtemplate[chartLine] <- sprintf("<script src='%s/chart.js'></script>", assets)
        HTMLtemplate[inzplotLine] <- sprintf("<script src='%s/inzplot.js'></script>", assets)
        HTMLtemplate[jsLine] <- sprintf("<script src='%s/main.js'></script>", assets)
        HTMLtemplate <- sprintf(gsub("INZIGHT_LOGO", "%s/inzight_transp.png", HTMLtemplate), assets)
    } else {

        ## insert inline JS, CSS
        HTMLtemplate[cssLine] <-
            paste("<style>", styles, "</style>", collapse = "\n")
        HTMLtemplate[chartLine] <-
            paste("<script type='text/javascript'>", chartCode, collapse = "\n")
        HTMLtemplate[inzplotLine] <- inzJS
        HTMLtemplate[jsLine] <- paste(jsCode, "</script>", collapse = "\n")
        logourl <- "https://www.stat.auckland.ac.nz/~wild/iNZight/img/inzight_transp.png"
        HTMLtemplate <- gsub("INZIGHT_LOGO", logourl, HTMLtemplate)
    }

    # insert table and SVG
    HTMLtemplate[tableLineOne] <- HTMLtable
    HTMLtemplate[svgLine] <- svgCode

    # Writing it out to an HTML file:
    write(HTMLtemplate, file)

    # Store url:
    url <- normalizePath(file)
    class(url) <- "inzHTML"

    return(url)
}


#' Print method for `inzHTML` object
#'
#' The default action is for the URL to be 'printed' (opened) in the browser,
#' unless `viewer` is specified as something else.
#' If `viewer = NULL`, then the URL is printed as a character string.
#' @param x      a URL that will be printed
#' @param viewer the viewing function to use to display the URL
#' @param ... additional arguments
#' @return NULL (it's a print function, after all)
#' @export
print.inzHTML <- function(x, viewer = getOption("viewer", utils::browseURL), ...) {

    if (!is.null(viewer))
        viewer(x)
    else
        print(as.character(x))

    invisible(NULL)
}


## getInfo method for retrieving table and JSON
## @param plot (for single panels)
## @param x the entire plot object
## @param data the data set
## @param extra.vars extra variables to be exported
## @return a list consisting of the table (tbl) and JSON (js)
## internal function
getInfo <- function(plot, x = NULL, data = NULL, extra.vars = NULL)  {
    UseMethod("getInfo")
}

getInfo.inzbar <- function(plot, x, ...) {
    # generation of table of counts:
    # plot <- x$all$all
    prop <- plot$phat
    counts <- plot$tab
    percent <- plot$widths
    n <- attributes(x)$total.obs
    type <- "bar"

    if (attributes(x)$total.missing != 0) {
        n <- attributes(x)$total.obs - attributes(x)$total.missing
    }

    # color matching:
    colorMatch <- plot$p.colby
    prop.df <- as.data.frame(t(prop))
    counts.df <- as.data.frame(counts)
    group <- length(percent)
    pct <- as.data.frame(t(round(prop*100, 2)))

    dt <- cbind(counts, pct)
    colnames(dt) <- c('varx', 'counts', 'pct')
    colCounts <- NULL;
    order <- NULL;

    if (all(percent != 1)) {
        # This condition is used to identify if it's a two way plot...
        # different table for two way bar plots
        tab <- cbind(
            round(prop,4),
            format(round(rowSums(prop),4), nsmall = 4),
            rowSums(counts)
        )
        colnames(tab)[(ncol(prop)+1):ncol(tab)] <- c("Total", "Row N")

        ## for JSON:
        prop.df <- as.data.frame(prop)
        dt <- cbind(prop.df, counts.df$Freq)
        colnames(dt) <- c("var1", "var2", "pct", "counts")
        dt$pct <- round(dt$pct*100, 2)
        colCounts <- c("Col N", round(colSums(counts)/n,4), 1)

    } else if (!is.null(colorMatch) && (all(c(0, 1) %in% colorMatch) == FALSE)) {
        # for stacked bar plots - a special two-way table
        colorMatchRev <- colorMatch[nrow(colorMatch):1, ]
        proportions <- round(rbind(colorMatchRev, colSums(colorMatchRev)), 4)
        tab <- rbind(proportions,counts)
        rownames(tab)[nrow(proportions):nrow(tab)] <- c("Total", "Col N")

    } else {
        # creating table for 1 way plots
        tab <- rbind(counts, round(prop*100,2))
        tab <- cbind(tab, rowSums(tab))
        tab[2,] <- paste0(tab[2,], "%")
        colnames(tab)[ncol(tab)] <- "Total"
        rownames(tab) <- c("Counts", "Percent")

        colnames(dt) <- c("othervar", "varx", "counts", "pct")
    }

    ## JSON:
    if (is.null(colorMatch)) {
        colorMatch <- NA
    } else if (all(c(0, 1) %in% colorMatch) == TRUE) {
        ## test if the bar plot colby is the same
        colorMatch <- as.vector(colorMatch)
    } else {
        ## required for stacked bar plots, due to make up of polygons being reversed when plotted
        order <- matrix(1:length(colorMatch), ncol = ncol(colorMatch), byrow = TRUE)
        order <- as.vector(apply(order, 1, rev))
        dt <- as.data.frame(as.table(colorMatch), stringsAsFactors = FALSE)
        dt$pct <- round(dt$Freq*100, 2)
        #counts are in the counts value -> need to merge on var2
        colnames(counts.df) <- c("Var2", "c1")
        dt <- merge(dt, counts.df)
        dt <- dt[order(dt$Var1),]
        # calculate counts
        dt$counts <- with(dt, c1*Freq)
        dt <- cbind(dt, order)
        dt <- dt[order(dt$order), ]

        # reset colorMatch
        colorMatch <- TRUE
        type <- "bp-stacked"
    }

    # attributes for HTML table
    cap <- 'Table of Counts and Proportions'
    includeRow <- TRUE
    tableInfo <- list(caption = cap, includeRow = includeRow, tab = tab, n = n)
    #returning all data in a list:
    chart <- list(
        type = type,
        data = dt,
        colorMatch = colorMatch,
        colCounts = colCounts,
        group = group,
        order = order
    )
    bpJS <- paste(
        readLines(system.file("barplot.js", package = "iNZightPlots")),
        collapse = "\n"
    )
    JSData <- list(chart = jsonlite::toJSON(chart), jsFile = bpJS)
    return(list(tbl = tableInfo, js = JSData))
}

getInfo.inzhist <- function(plot, x, ...) {
    #plot <- x$all$all or plot <- x$all[[1]]

    toPlot <- plot$toplot$all
    if (is.null(toPlot)) {
        stop("This histogram has levels. Currently not available yet!")
    }
    intervals <- toPlot$breaks
    counts <- toPlot$counts
    n <- attributes(x)$total.obs

    if (attributes(x)$total.missing != 0) {
        n <- attributes(x)$total.obs - attributes(x)$total.missing
    }

    # freq distribution table:
    lower <- round(intervals[-length(intervals)], 2)
    upper <- round(intervals[-1], 2)
    interval <- paste(lower, upper, sep ="-")
    tab <- cbind(interval, counts)
    colnames(tab) <- c("Class Interval", "Frequency")
    tab <- rbind(tab, c("Total", sum(counts)))

    ## Attributes for HTML table
    cap <- "Frequency Distribution Table"
    includeRow <- TRUE
    tableInfo <- list(
        caption = cap,
        includeRow = includeRow,
        tab = data.frame(tab),
        n = n
    )

    # next, store info as JSON
    pct <- round(counts / n * 100, 2)
    tab <- as.data.frame(cbind(lower, upper, counts, pct))

    # To obtain box whisker plot information:
    if (!is.null(plot$boxinfo)) {
        boxInfo <- plot$boxinfo$all
        quantiles <- boxInfo$quantiles
        min <- boxInfo$min
        max <- boxInfo$max
        boxTable <- rbind(as.data.frame(quantiles), min, max)
        rownames(boxTable)[4:5] <- c("min", "max")
    } else {
        boxTable <- list()
    }

    if (!is.null(plot$meaninfo)) {
        meanInfo <- plot$meaninfo$all$mean
    } else {
        meanInfo <- list()
    }

    # Output as a list:
    chart <- list(
        type = "hist",
        data = tab,
        boxData = boxTable,
        meanData = meanInfo,
        n = n,
        inf = NULL
    )
    histJS <- paste(
        readLines(system.file("histogram.js", package = "iNZightPlots")),
        collapse = "\n"
    )
    JSData <- list(chart = jsonlite::toJSON(chart), jsFile = histJS)
    return(list(tbl = tableInfo, js = JSData))
}

getInfo.inzdot <- function(plot, x, data = NULL, extra.vars = NULL) {
    plots <- plot$toplot
    levels <- names(plots)
    varNames <-attributes(x)$varnames

    if (length(levels) > 1)  { # for multi-level dot plots

        levList <- list()
        dd <- list()
        boxList <- list()
        meanList <- list()
        order <- list()
        countsTab <- as.numeric()
        countsTab[1] <- 0


        for (i in 1:length(levels)) {
            #currently only takes variable plotted
            levList[[i]] <- plots[[i]]$x
            order <- attr(plots[[i]], "order")

            ## exported data frame with extra variables:
            if (!is.null(extra.vars) && !is.null(data)) {
                dataByLevel <- data[which(data[,varNames$y] == levels[i]), ]
                dd[[i]] <- varSelect(varNames, plots[[i]],
                    order, extra.vars, dataByLevel, levels = TRUE
                )
            } else {
                dd[[i]] <- cbind(levList[[i]], levels[i])
                colnames(dd[[i]]) <- c(
                    attributes(x)$varnames$x,
                    attributes(x)$varnames$y
                )
            }

            if (!is.null(plot$boxinfo)) {
                #obtain boxplot information
                box <- plot$boxinfo
                quantiles <- box[[i]]$quantiles
                min <- box[[i]]$min
                max <- box[[i]]$max

                # get boxplot summaries for each group
                boxTable <- rbind(as.data.frame(quantiles), min, max)
                rownames(boxTable)[4:5] <- c("min", "max")
                boxList[[i]] <- boxTable
            }

            if (!is.null(plot$meaninfo)) {
                meanList[[i]] <- plot$meaninfo[[i]]$mean
            }

            # get cumulative frequency of counts for each group:
            countsTab[i+1] <- sum(plots[[i]]$counts, countsTab[i])
        }

        #bind all groups together:
        tab <- do.call("rbind", dd)
        chart <- list(
            type = "dot",
            data = data.frame(tab),
            boxData = boxList,
            levList = levList,
            countsTab = countsTab,
            levNames = names(plots),
            varNames = colnames(tab),
            meanData = meanList
        )

    } else { #for single dot plots

        colGroupNo <- nlevels(plot$toplot$all$colby)
        pl <- plot$toplot$all
        # note that the order given is with non-missing values (data has been filtered)
        order <- attr(pl, "order")

        #variable selection
        tab <- varSelect(varNames, pl, order, extra.vars, data)

        if (!is.null(plot$boxinfo)) {
            #To obtain box whisker plot information:
            boxInfo <- plot$boxinfo$all
            quantiles <- boxInfo$quantiles
            min <- boxInfo$min
            max <- boxInfo$max
            boxTable <- rbind(as.data.frame(quantiles), min, max)
            rownames(boxTable)[4:5] <- c("min", "max")
        } else {
            boxTable <- list()
        }

        if (!is.null(plot$meaninfo)) {
            meanInfo <- plot$meaninfo$all$mean
        } else {
            meanInfo <- list()
        }
        ## JS:
        chart <- list(
            type = "dot",
            varNames = names(tab),
            data = tab,
            colGroupNo = colGroupNo,
            boxData = boxTable,
            meanData = meanInfo
        )
    }

    ##Attributes for HTML table
    cap <- "Data"
    includeRow <- TRUE
    tableInfo <- list(
        caption = cap,
        includeRow = includeRow,
        tab = data.frame(tab),
        varNames = varNames
    )
    dpspJS <- paste(
        readLines(system.file("dpsp.js", package = "iNZightPlots")),
        collapse = "\n"
    )
    JSData <- list(chart = jsonlite::toJSON(chart), jsFile = dpspJS)
    return(list(tbl = tableInfo, js = JSData))
}

getInfo.inzscatter <- function(plot, x, data = NULL, extra.vars = NULL) {
    obj <- x
    x <- plot$x
    y <- plot$y
    order <- plot$point.order
    varNames <- attributes(obj)$varnames
    colGroupNo <- nlevels(plot$colby)

    tab <- cbind(as.data.frame(x), as.data.frame(y))
    names(tab) <- c(varNames$x, varNames$y)

    # variable selection
    tab <- varSelect(varNames, plot, order, extra.vars, data)

    # for maps only
    if (obj$gen$opts$plottype == "map") {
        names(tab) <- gsub("expression[(]\\.([[:alpha:]]*)[)]", "\\1", names(tab))
    }

    ## Attributes for HTML table
    cap <- "Data"
    includeRow <- TRUE
    tbl <- list(caption = cap, includeRow = includeRow, tab = tab)

    # simplistic inference information: beta coefficients + R^2
    # For future purposes - if summary(obj) returns a list with these inside it, then we
    # could use summary(obj)$formula / summary(obj)$Rspearman instead....
    # currently does NOT take into account of survey design
    # only works if trends are plotted...
    if (is.null(plot$trend)) {
        trendInfo <- NA
    } else {
        trendInfo <- list(linear = NA, quadratic = NA, cubic = NA,  rank.cor = NA)

        if ("linear" %in% plot$trend) {
            beta <- signif(coef(lm(y ~ x)), 4)
            c <- round(cor(x, y), 2)
            dim(beta) <- NULL
            trendInfo$linear <- c(beta, c)
        }

        if ("quadratic" %in% plot$trend) {
            beta <- signif(coef(lm(y ~ x + I(x^2))), 4)
            dim(beta) <- NULL
            trendInfo$quadratic <- beta
        }

        if ("cubic" %in% plot$trend) {
            beta <- signif(coef(lm(y ~ x + I(x^2) + I(x^3))), 4)
            dim(beta) <- NULL
            trendInfo$cubic <- beta
        }
        trendInfo$rank.cor <- cor(x, y, method = "spearman")
    }

    chart <- list(
        type = "scatter",
        varNames = names(tab),
        data = tab,
        colGroupNo = colGroupNo,
        trendInfo = trendInfo
    )
    dpspJS <- paste(
        readLines(system.file("dpsp.js", package = "iNZightPlots")),
        collapse = "\n"
    )
    JSData <- list(chart = jsonlite::toJSON(chart), jsFile = dpspJS)

    return(list(tbl = tbl, js = JSData))
}

getInfo.inzhex <- function(plot, x = NULL, ...) {
    warning("No table available for hexbin plots.")
    tbl <- NULL

    # hexplots use S4 notation
    counts <- plot$hex@count
    xcm <- round(plot$hex@xcm, 2)
    ycm <- round(plot$hex@ycm, 2)
    n <- plot$hex@n

    tab <- as.data.frame(cbind(counts, xcm, ycm))
    tab$pct <- round(tab$counts/n*100, 2)

    # JS
    chart <- list(type = "hex", data = tab, n = n)
    hexJS <- paste(
        readLines(system.file("hexbin.js", package = "iNZightPlots")),
        collapse = "\n"
    )
    JSData <- list(chart = jsonlite::toJSON(chart), jsFile = hexJS)

    return(list(tbl = tbl, js = JSData))
}

getInfo.default <- function(plot, x, ...) {
    warning("There may not be an interactive version of this plot yet...")
    return()
}

# Variable selection: for dot plots and scatter plots
# For exporting additional variables
# @param varNames variables used in iNZightPlot object
# @param pl the plot object (x$all$all)
# @param order the order of points by how they are plotted (see inzscatter/inzdot function)
# @param extra.vars extra variables to export
# @param data original dataset used
# @param levels logical on whether there are levels to be considered (dot plots only)
# @return returns a data frame to be exported
varSelect <- function(varNames, pl, order, extra.vars, data, levels = FALSE) {

    if (!is.null(extra.vars) && !is.null(data)) {

        # filter missing data
        # This is not required for scatter plots as the order listed takes missing data
        # into account
        if (is.null(varNames$y) || levels == TRUE) {
            data <- data[!is.na(data[, varNames$x]), ]
        }

        # selected variables
        selected <- c(extra.vars, varNames$x, varNames$y, varNames$colby, varNames$sizeby)
        colNum <- which(colnames(data) %in% selected)

        # format in order
        tab <- data[order, colNum, drop = FALSE]

        rownames(tab) <- 1:nrow(tab)

    } else {

        # default
        xVal <- pl$x
        tab <- data.frame(xVal)
        names(tab) <- varNames$x

        # y-var
        if (!is.null(varNames$y)) {
            yVal <- pl$y
            tab <- cbind(tab, as.data.frame(yVal))
            names(tab)[ncol(tab)] <- varNames$y
        }

        # find colby/sizeby:
        if (!is.null(pl$colby)) {
            colby <- pl$colby
            tab <- cbind(tab, as.data.frame(colby))
            names(tab)[ncol(tab)] <- varNames$colby
        }

        if (!is.null(varNames$sizeby)) {
            sizeby <- pl$propsize
            tab <- cbind(tab, as.data.frame(sizeby))
            names(tab)[ncol(tab)] <- varNames$sizeby
        }

    }

    return(tab)

}


#' Identify if a plot can be interactive
#'
#' Several iNZightPlots graphs have been enabled with custom interaction,
#' while others make use of the automatic output of `plotly`.
#' This function returns `TRUE` if the provided plot has interaction
#' (as determined by iNZight), and `FALSE` otherwise.
#'
#' Not that, while most `ggplot2` graphs can be passed to `plotly`,
#' and even though we are using plot.ly directly for some of our ggplot2
#' graphs, we still only return `TRUE` if the graph was created
#' by one of the packages in the iNZight collection.
#'
#' @param x a plot object returned from a plotting function
#' @return Logical to identify if there is an interactive version
#' @export
#' @author Tom Elliott, Yu Han Soh
#' @examples
#' can.interact(iNZightPlot(Sepal.Length, data = iris))
can.interact <- function(x) {
    UseMethod("can.interact")
}

#' @describeIn can.interact Default interaction helper (always returns `FALSE`)
#' @export
can.interact.default <- function(x) FALSE

#' @describeIn can.interact Graphs from `iNZightPlot()`, many of which
#'             have interaction enabled, but some do not (for example, hex plots)
#' @export
can.interact.inzplotoutput <- function(x) {
    pl <- x$all$all
    # if it's not single panelled/or of only 1 group
    if (is.null(pl) && (length(x$all) != 1)) {
        return(FALSE)
    }
    ## singular groups
    if (length(x$all) == 1) {
        pl <- x$all[[1]]
    }
    # coloured hex-plots not available
    if(attributes(x)$plottype == "hex" && !is.null(pl$colby)) {
        return(FALSE)
    }
    TRUE
}

#' @describeIn can.interact Those `iNZight*` plotting functions which return
#'             a `ggplot2` object and have been tested to work with plotly
#'             will be tagged as such; this is just a helper to check for
#'             the necessary attribute.
#' @export
can.interact.ggplot <- function(x) {
    # only for iNZightMaps
    if (inherits(x$data, "sf") && !inherits(x, "gTable")) {
        return(TRUE)
    } else {
        ## some other ggplot functions can be interacted with,
        ## based on the attribute `use.ploty`
        !is.null(attr(x, "use.plotly")) && attr(x, "use.plotly")
    }
}
