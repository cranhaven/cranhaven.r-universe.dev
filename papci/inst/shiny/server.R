######################################################
##                      papci                       ##
##             Interactive User Interface           ##
##                     Server File                  ##
##                                                  ##
##                                                  ##
######################################################

suppressMessages(library(shiny))
suppressMessages(library(shinythemes))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))



shinyServer(function(input, output,session) {
    options(shiny.maxRequestSize=300*1024^2)
    Maindata<- reactiveValues()

    ### Input ###

    observeEvent(input$Inputreadin,{

        ##calculate two by two table
        Maindata$x = input$x
        Maindata$y = input$y
        Maindata$m = input$m
        Maindata$n = input$n
        Maindata$twobytwo = as.data.frame(matrix(c(Maindata$x,Maindata$m-Maindata$x,Maindata$m,Maindata$y,Maindata$n-Maindata$y,Maindata$n,Maindata$x+Maindata$y,Maindata$m-Maindata$x+Maindata$n-Maindata$y,Maindata$m+Maindata$n),nrow = 3,ncol = 3))

        colnames(Maindata$twobytwo) = c("Reference (+)", "Reference (-)", "Reference Total")
        rownames(Maindata$twobytwo) = c("New Assay (+)", "New Assay (-)", "New Assay Total")
        output$Input_twobytwo = DT::renderDataTable(Maindata$twobytwo,
                                                    escape = F,
                                                    options = list(
                                                        searching = F,
                                                        lengthChange = F,
                                                        scrollY = '150px',
                                                        scrollCollapse = F,
                                                        paging = F,
                                                        info = F,
                                                        ordering=F))
        if(!is.null((Maindata$df))){
            output$Input_Snapshot = DT::renderDataTable (Maindata$df,
                                                         escape = F,
                                                         options = list(
                                                             searching = F,
                                                             lengthChange = F,
                                                             scrollY = '150px',
                                                             scrollCollapse = F,
                                                             paging = F,
                                                             info = F))
        }
    })

    observeEvent(input$Inputreadindf,{
            if(is.null(input$InputFile)){
                showModal(modalDialog(
                    title = "Warning",
                    "Please upload data",
                    easyClose = TRUE
                ))
            }
            if (!is.null(input$InputFile)) {
                Maindata$file = input$InputFile
                withProgress(message="Reading in",detail="0%",{
                    incProgress(1,detail=paste0(round(100),"%"))
                    data = read_excel(Maindata$file$datapath)
                    Maindata$df = data
                    Maindata$twobytwo = bind_rows(as.data.frame.matrix(t(table(Maindata$df)))%>%
                                                      mutate(`Baseline Total` = rowSums(.)),
                                                  as.data.frame.matrix(t(table(Maindata$df)))%>%
                                                      mutate(`Baseline Total` = rowSums(.))%>%
                                                      summarise_all(sum))
                    colnames(Maindata$twobytwo) = c("Baseline (+)", "Baseline (-)", "Baseline Total")
                    rownames(Maindata$twobytwo) = c("Comparator (+)", "Comparator (-)", "Comparator Total")
                })
                output$Input_twobytwo = DT::renderDataTable(Maindata$twobytwo,
                                                            escape = F,
                                                            options = list(
                                                                searching = F,
                                                                lengthChange = F,
                                                                scrollY = '150px',
                                                                scrollCollapse = F,
                                                                paging = F,
                                                                info = F,
                                                                ordering=F))
                output$Input_Snapshot = DT::renderDataTable (Maindata$df,
                                                             escape = F,
                                                             options = list(
                                                                 searching = F,
                                                                 lengthChange = F,
                                                                 scrollY = '150px',
                                                                 scrollCollapse = F,
                                                                 paging = F,
                                                                 info = F))
            }




    })
    observeEvent(input$Inputexample,{
            withProgress(message="Reading in",detail="0%",{
                incProgress(1,detail=paste0(round(100),"%"))
                #Maindata$df = read_excel("C:/Users/kali/Foundation Medicine Inc/Cui Guo - EarlyStageTeam/Projects/PPV_CI/shiny/example.xlsx")
                Maindata$df = read_excel("example.xlsx")
                Maindata$twobytwo = bind_rows(as.data.frame.matrix(t(table(Maindata$df)))%>%
                                                  mutate(`Comparator Total` = rowSums(.)),
                                              as.data.frame.matrix(t(table(Maindata$df)))%>%
                                                  mutate(`Comparator Total` = rowSums(.))%>%
                                                  summarise_all(sum))
            })

        colnames(Maindata$twobytwo) = c("Baseline (+)", "Baseline (-)", "Baseline Total")
        rownames(Maindata$twobytwo) = c("Comparator (+)", "Comparator (-)", "Comparator Total")
        output$Input_twobytwo = DT::renderDataTable(Maindata$twobytwo,
                                                    escape = F,
                                                    options = list(
                                                        searching = F,
                                                        lengthChange = F,
                                                        scrollY = '150px',
                                                        scrollCollapse = F,
                                                        paging = F,
                                                        info = F,
                                                        ordering=F))

            output$Input_Snapshot = DT::renderDataTable (Maindata$df,
                                                         escape = F,
                                                         options = list(
                                                             searching = F,
                                                             lengthChange = F,
                                                             scrollY = '150px',
                                                             scrollCollapse = F,
                                                             paging = F,
                                                             info = F))
    })

    observeEvent(input$Inputreset, {
        Maindata$file = NULL
        Maindata$df = NULL
        Maindata$twobytwo = NULL

    })
    observe({
        if (input$Inputnextstepbutton) {
            isolate({
                updateTabsetPanel(session,"steps_list","concordance")
            })
        }
    })



    ### Concordance ###
    observeEvent(input$calculate,{
        showModal(modalDialog(
            title = "Processing",
            "Pease wait while calculating ",
            easyClose = TRUE
        ))
        Maindata$agreement = agreement(
            tb = Maindata$twobytwo[1:2,1:2],  #x = Maindata$x
                                       # y = Maindata$y,
                                       # m = Maindata$m,
                                       # n = Maindata$n,
                                       conf.level = input$conf.level,
                                       methods_pa = input$method_pa,
                                       methods_pv = input$method_pv,
                                       alternative = input$alternative,
                                       prev = input$prevalence,
            times = input$bootstrap)
        Maindata$ppa = Maindata$agreement$ppa%>%select(-c(x,n))%>%
            mutate(mean = round(mean,3),
                   lower = round(lower,3),
                   upper = round(upper,3))
        Maindata$npa = Maindata$agreement$npa%>%select(-c(x,n))%>%
            mutate(mean = round(mean,3),
                   lower = round(lower,3),
                   upper = round(upper,3))
        Maindata$ppv = Maindata$agreement$ppv%>%
            mutate(mean = round(mean,3),
                   lower = round(lower,3),
                   upper = round(upper,3))
        Maindata$npv = Maindata$agreement$npv%>%
            mutate(mean = round(mean,3),
                   lower = round(lower,3),
                   upper = round(upper,3))
        Maindata$all = rbind(Maindata$ppa,
                             Maindata$npa,
                             Maindata$ppv,
                             Maindata$npv)%>%
            mutate(Metric = c(rep("PPA",nrow(Maindata$ppa)),
                              rep("NPA",nrow(Maindata$npa)),
                              rep("PPV",nrow(Maindata$ppv)),
                              rep("NPV",nrow(Maindata$npv))))%>%
            select(Metric,method,mean,lower,upper)
        rownames(Maindata$all) = c(1,2,3,4)


    })

    observe({
        if (!is.null(input$calculate)&input$concordance=="PPA") {
            output$concordance_tb = DT::renderDataTable (Maindata$ppa,
                                                         escape = F,
                                                         options = list(
                                                             searching = F,
                                                             lengthChange = F,
                                                             scrollY = '150px',
                                                             scrollCollapse = F,
                                                             paging = F,
                                                             info = F))
        }else if (!is.null(input$calculate)&input$concordance=="NPA") {
            output$concordance_tb = DT::renderDataTable (Maindata$npa,
                                                         escape = F,
                                                         options = list(
                                                             searching = F,
                                                             lengthChange = F,
                                                             scrollY = '150px',
                                                             scrollCollapse = F,
                                                             paging = F,
                                                             info = F))
        }else if (!is.null(input$calculate)&input$concordance=="PPV") {
            output$concordance_tb = DT::renderDataTable (Maindata$ppv,
                                                         escape = F,
                                                         options = list(
                                                             searching = F,
                                                             lengthChange = F,
                                                             scrollY = '150px',
                                                             scrollCollapse = F,
                                                             paging = F,
                                                             info = F))
        }
        else if (!is.null(input$calculate)&input$concordance=="NPV") {
            output$concordance_tb = DT::renderDataTable (Maindata$npv,
                                                         escape = F,
                                                         options = list(
                                                             searching = F,
                                                             lengthChange = F,
                                                             scrollY = '150px',
                                                             scrollCollapse = F,
                                                             paging = F,
                                                             info = F))
        }
        else if (!is.null(input$calculate)&input$concordance=="all") {
            output$concordance_tb = DT::renderDataTable (Maindata$all,
                                                         escape = F,
                                                         options = list(
                                                             searching = F,
                                                             lengthChange = T,
                                                             scrollCollapse = T,
                                                             paging = T,
                                                             pageLength = 5,
                                                             info = F
                                                         )
            )
        }
    })


    output$download <- downloadHandler(
        filename = function() {
            paste("agreement", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(Maindata$all, file, row.names = FALSE)
        }
    )




})
