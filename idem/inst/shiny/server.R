##    ----------------------------------------------------------------------
##    Copyright (C) 2015  Daniel O. Scharfstein and Chenguang Wang
##    This program is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.

##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.

##    You should have received a copy of the GNU General Public License
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.
##    -----------------------------------------------------------------------

library(idem);
##library(shinysky);
##library(shinyIncubator);
##library(ShinyDash);
##source("composite_tool.R");


shinyServer(function(input, output, session) {

    source("composite_ui.R", local=TRUE);

    userLog          <- reactiveValues();
    userLog$uid      <- 1;
    userLog$model    <- -1;
    userLog$data     <- NULL;

    ##------------------------------------
    ##---------main page------------------
    ##------------------------------------
    output$mainpage <- renderUI({
        if (-1 == userLog$uid) {
            tab.login();
        } else {
            tab.main();
        }
    })

    ##--------------------------------------
    ##---------log in page------------------
    ##--------------------------------------

    ##-----user verification------
    observe({
        if (!is.null(input$btnLogin)) {
            if (0 < input$btnLogin) {
                ##to do verify user
                if (input$inPwd == "COMPMISS")
                    userLog$uid <- 0;
            }
        }
    })

    ##--------------------------------------
    ##---------exit-------------------------
    ##--------------------------------------
    observeEvent(input$close, {stopApp()});


    ##--------------------------------------
    ##---------data upload------------------
    ##--------------------------------------

    ##--display data uploaded-----
    output$uiData <- DT::renderDataTable({
                             if (input$displaydata) {
                                 userLog$data;
                             }
                         }, rownames=NULL, selection="none", options=list(pageLength=50))

    ##--------------------------------------
    ##---------model specification----------
    ##--------------------------------------

    ##--model specification------
    output$uiPanelModel <- renderUI({
        if (is.null(userLog$data)) {
            msg.box("Please upload data first.", "warning");
        } else {
            panel.model();
        }
    })

    ##----specify covarates------
    output$uiModel <- renderUI({
        if (!is.null(userLog$data)) {
            do.call(fluidPage, gen.radiobtn(names(userLog$data)));
        }
    })

    ##validate model
    output$uiValid <- renderText({
        if (0 == input$btnValid)
            return(NULL);

        isolate({
            rst <- get.model.valid();
            if (is.null(rst)) {
                rst <- msg.box("Model specification is valid.", "success");
                userLog$model <- 0;
            } else {
                rst <- msg.box(print(rst, html=TRUE), "error");
                userLog$model <- -1;
            }
            HTML(rst)});
    })


    output$uiLabel <- renderUI({
        if (is.null(userLog$data) | -1 == userLog$model)
            return(list(NULL));

        isolate({
            cur.trts <- get.data()$lst.var$trt.label;
        })

        wellPanel(
            msg.box("Treatment labels for tables and figures may be specified here."),
            h4("Labels of Treatment"),
            textInput("inTxtArm0","Arm 0",value = cur.trts[1], width = "200px"),
            textInput("inTxtArm1","Arm 1",value = cur.trts[2], width = "200px"))
    })



    ##--------------------------------------
    ##-----------data visualization---------
    ##--------------------------------------
    output$uiVisual <- renderUI({
        if (is.null(userLog$data)) {
            msg.box("Please upload data first.", "warning");
        } else if (-1 == userLog$model) {
            msg.box("Please validate model first.", "warning");
        } else {
            panel.visual();
        }
    })

    ##missing pattern
    output$outPlotMissing <- renderPlot({
        data.all <- get.data();
        if (is.null(data.all))
            return(NULL);

        plot(get.data(), opt = "missing", c("blue", "gray"));

    }, width=1000, height=600, bg="transparent");

    ##missng table
    output$outTblMissing <- renderTable({
        data.all <- get.data();
        if (is.null(data.all))
            return(NULL);

	     summary(data.all, opt = "misstable" );
    })

    ##survival
    output$outPlotSurv <- renderPlot({
        data.all <- get.data();
        if (is.null(data.all))
            return(NULL);

        plot(data.all, opt = "KM");

    }, width=600, height=600, bg="transparent");


    ##completers
    output$outPlotComp <- renderPlot({
        data.all <- get.data();

        if (is.null(data.all))
            return(NULL);
        plot(data.all, opt = "survivor", by.sace = FALSE);

    }, width=1000, height=600, bg="transparent");

    ##--------------------------------------
    ##------model fitting-------------------
    ##--------------------------------------

    output$uiFitting <- renderUI({
        if (is.null(userLog$data)) {
            msg.box("Please upload data first.", "warning");
        } else if (-1 == userLog$model) {
            msg.box("Please validate model first.", "warning");
        } else {
            panel.fitting();
        }
    })

    observe({
        if (is.null(get.data()) | -1 == userLog$model)
            return(NULL);

        frst <- get.fit.rst()$rst.mdl;
        for (i in 1:length(frst)) {
            cur.trt <- frst[[i]];
            for (j in 1:length(cur.trt)) {
                local({
                    myi <- i;
                    myj <- j;
                    myt <- cur.trt;
                    txtname <- paste("uifit", myi, myj, sep="");
                    output[[txtname]] <- renderPrint({
                        myt[[myj]]$lm$call <- myt[[myj]]$formula;
                        print(summary(myt[[myj]]$lm));
                    })

                    plotname <- paste("uidiag", myi, myj, sep="");
                    output[[plotname]] <- renderPlot({
                        par(mfrow=c(1,2));
                        plot(myt[[myj]]$lm, which=c(1:2), main="");
                    }, bg="transparent")
                })
            }
        }
    })

    ##--------------------------------------
    ##---------configuration----------------
    ##--------------------------------------
    output$uiConfig <- renderUI({
        if (is.null(userLog$data)) {
            msg.box("Please upload data first.", "warning");
        } else if (-1 == userLog$model) {
            msg.box("Please validate model first.", "warning");
        } else {
            panel.config();
        }
    })

    ##check convergence
    output$uiImpConv <- renderUI({
        if (input$btnConverge == 0)
            return(NULL);

        isolate({
            panel.trace();
        })
    })

    output$tabSub <- renderTable({
        mcmc.rst <- get.imputed.mcmc();
        if (is.null(mcmc.rst))
            return(NULL);

        mcmc.rst$dsub;
    })

    output$mcmcSub <- renderPlot({
        mcmc.rst <- get.imputed.mcmc();
        if (is.null(mcmc.rst))
            return(NULL);

        plot(mcmc.rst);

    }, bg="transparent")

    ##progress bar
    output$uiProgressGauge <- renderUI({
        if (0 == input$btnImpute) return(NULL);
    })

    ##--------------------------------------
    ##---------imputation-------------------
    ##--------------------------------------

    output$uiImpute <- renderUI({
        if (is.null(userLog$data)) {
            msg.box("Please upload data first.", "warning");
        } else if (-1 == userLog$model) {
            msg.box("Please validate model first.", "warning");
        } else {
            panel.impute();
        }
    })

    ##get imputation summary
    output$uiImpRst <- renderUI({
        if (0 == input$btnImpute)
            return(NULL);
        if (is.null(get.imputed.full()))
            return(NULL);
        panel.impute.summary();
    })

    ##imputed dataset
    output$impData <- renderDataTable({
        if (is.null(get.imputed.full()))
            return(NULL);
        get.imputed.full()$complete;
    })

    ##density of imputed individual outcomes
    output$outPImpY <- renderPlot({
        imp.data <- get.imputed.full();
        deltas   <- get.deltas();

        if (any(is.null(c(imp.data, deltas))))
            return(NULL);

        plot(imp.data,
             deltas = sort(unique(c(0, range(deltas)))));

    }, width=800, height=800, bg="transparent")

    ##density of imputed individual endpoint
    output$outPImpE <- renderPlot({
        imp.data <- get.imputed.full();
        deltas   <- get.deltas();

        if (any(is.null(c(imp.data,deltas))))
            return(NULL);

        plot(imp.data,
             deltas = sort(unique(c(0, range(deltas)))),
             endp=TRUE);

    }, width=800, height=400, bg="transparent")


    ## Survivor Average Causal Effect
    output$outSACE <- renderPlot({
        isolate({
            if (0 == input$btnImpute) return(NULL);
            rm.rst <- get.rst.orig();

            if (is.null(rm.rst))  return(NULL);
            sens <- get.SACE()
            T0 <- rm.rst$meansurv[which(rm.rst$meansurv$TRT==0),]
            T1 <- rm.rst$meansurv[which(rm.rst$meansurv$TRT==1),]
            meandat <- T1
            meandat$Diff <- T1$MeanSurv - T0$MeanSurv
            plotSace(sace.data=meandat, saceSense = sens,
                     plotTitle = NULL)
        })},
        width=400, height=400, bg='transparent')


    ##composite quantile
    output$outPlotImpComp <- renderPlot({
        imp.data <- get.imputed.full();
        if (is.null(imp.data))
            return(NULL);

        plot(imp.data, opt = "composite");

    }, width=800, height=400, bg="transparent");

    ##rank text
    output$outTblImpRank <- renderTable({
        if (0 == input$btnImpute)
            return(NULL);
        rm.rst <- get.rst.orig();
        rm.rst$theta},
        include.rownames=TRUE,
    		caption = 'Table: Values of theta < 0
                   favor the Intervention group. Note that Delta0 and Delta1
                   indicate the imputation sensitivity parameters used for the
                   Control and Intervention groups, respectively.',
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL))

    ##rank text
    output$outTblImpMedian <- renderTable({
        if (is.null(input$btnImpute))
            return(NULL);

        if (0 == input$btnImpute)
            return(NULL);

        rm.rst <- get.rst.orig();
        rm.rst$effect.quantiles;
    },
    include.rownames=TRUE,
    caption = 'Table: Quantiles of Compositve Variable.',
		caption.placement = getOption("xtable.caption.placement", "top"))

    ##download imputed data
    output$btnImpDload <- downloadHandler(
        filename=function() {
            paste('imputed_',
                  format(Sys.time(), "%m%d%Y%H%M%S"),
                  '.txt',sep="")
        },

        content=function(file) {
            tfile <- tempfile();
            write.table(get.imputed.full()$complete,
                        tfile,
                        row.names=FALSE);
            bytes <- readBin(tfile, "raw", file.info(tfile)$size);
            writeBin(bytes, file);
        })

    ##download imputation Settings
    output$btnSettingsDload <- downloadHandler(
        filename=function() {
        paste('imputation_Settings_',
              format(Sys.time(), "%m%d%Y%H%M%S"),
              '.txt',sep="")
    },

    content=function(file) {
        tfile <- tempfile();
        impStat <- data.frame('RandomSeed' = input$inNSeed,
                              'NumberDataSets' = input$inNImp,
                              'Iterations' = input$inIter,
                              'ScaleFactor' = input$inScale,
                              'Thinning' = input$inThin)
        write.table(impStat, tfile, row.names=FALSE);
        bytes <- readBin(tfile, "raw", file.info(tfile)$size);
        writeBin(bytes, file);
    })

    ##--------------------------------------
    ##----------bootstrap-------------------
    ##--------------------------------------
    ##bootstrap results
    output$uiBootrst <- renderUI({
        if (0 == input$btnBoot)
            return(NULL);
        panel.boot();
    })

    ## Bootstrapped Rank
    output$outTblBootTheta <- renderTable({
        if (0 == input$btnBoot)
            return(NULL);
        rst <- get.rst.boot();
        rst$theta;
    },
    include.rownames=TRUE,
    caption = 'Table: Rank Statistic. Note that Delta0
             and Delta1 indicate the imputation sensitivity parameters used
             for the control and intervention groups, respectively.',
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL));


    ## Bootstrapped Quantiles
    output$outTblBootQuantiles <- renderTable({
        if (0 == input$btnBoot)
            return(NULL);
        rst <- get.rst.boot();
        rst$effect.quantiles;

    }, include.rownames=TRUE,
    caption = 'Table: Quantiles of Compositve Variable',
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL));

    ## Bootstrapped Contour
    output$outBootContourRank <- renderPlot({
        if (0 == input$btnBoot)
            return(NULL);
        rst <- get.rst.boot();
        plot(rst, main="");
    },
    width=500,
    height=500,
    bg="transparent");

    ## -----------survivor only ----------------------
    output$outTblBootThetaSOnly <- renderTable({
        if (0 == input$btnBoot)
            return(NULL);
        rst <- summary(get.rst.boot());
        rst$rst;
    },
    include.rownames=FALSE,
    caption = 'Table: Survivor only treatment effect. Note that Delta0
               and Delta1 indicate the imputation sensitivity parameters used
               for the control and intervention groups, respectively.',
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL));

    ## Contour plots: survivor only
    output$outBootContourSonly <- renderPlot({
        if (0 == input$btnBoot)
            return(NULL);
        rst <- summary(get.rst.boot());
        plot(rst, main="");
    },
    width=500,
    height=500,
    bg="transparent");


    ## -----------sace ----------------------
    output$outTblBootThetaSACE <- renderTable({
        if (0 == input$btnBoot)
            return(NULL);
        rst <- get.rst.boot.sace();
        rst$rst;
    },
    include.rownames=FALSE,
    caption = 'Table: SACE treatment effect. Note that Delta0
               and Delta1 indicate the imputation sensitivity parameters used
               for the control and intervention groups, respectively.',
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL));


    ## Contour plots: SACE
    output$outBootContourSACE <- renderPlot({
        if (0 == input$btnBoot)
            return(NULL);
        rst <- get.rst.boot.sace();
        plot(rst, plot.title="");
    },
    width=500,
    height=500,
    bg="transparent");

    ##--------------------------------------
    ##----------Report----------------------
    ##--------------------------------------
    output$uiReport <- renderUI({
        if (is.null(userLog$data)) {
            msg.box("Please upload data first.", "warning");
        } else if (-1 == userLog$model) {
            msg.box("Please validate model first.", "warning");
        } else {
          wellPanel(
            msg.box('NOTE: If you want to download the report as a pdf, you will need to have
                     both <a href="https://pandoc.org/installing.html" target="_blank"> pandoc</a>
                     and latex typesetting software such as <a href="https://miktex.org/" target="_blank">MiKTeX</a>
                     installed on your computer.'),
            h4('Download the analysis report'),
            radioButtons('format', '', c('PDF', 'HTML', 'Word')),
            downloadButton('btnDload'))
        }
    })

    output$btnDload <- downloadHandler(
        filename=function() {
            paste('report_',
                  format(Sys.time(), "%m%d%Y%H%M%S"),
                  '.',
                  switch(input$format,
                         PDF = 'pdf',
                         HTML = 'html',
                         Word = 'docx'
                         ),
                  sep="")
        },

        content=function(file) {
        out <- rmarkdown::render('report/report.Rmd',
                                 switch(input$format,
                                        PDF  = rmarkdown::pdf_document(),
                                        HTML = rmarkdown::html_document(),
                                        Word = rmarkdown::word_document()
                                        ));

        bytes <- readBin(out, "raw", file.info(out)$size);
        writeBin(bytes, file);
    })

})
