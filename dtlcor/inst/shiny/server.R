options(shiny.maxRequestSize = 200*1024^2)

shinyServer(function(input, output, session) {
    
    source("design_ui.R", local = TRUE)
    
    userLog <- reactiveValues()
    
    ##--------------------------------------
    ##---------main-------------------------
    ##--------------------------------------
    output$mainpage <- renderUI({
        tab_main()
    })
    
    ##--------------------------------------
    ##---------exit-------------------------
    ##--------------------------------------
    observeEvent(input$close, {
        stopApp()})
    
    # ---------------output-----------------
    output$plot_tier = renderPlot({ 
        plot_tier() 
    })
    output$table_tier = DT::renderDT({ 
        datatable(table_tier(), filter="top") %>%
            formatRound(columns="fwer", digits=4)
    })
    
    output$plot_cor = renderPlot({ 
        plot_cor() 
    })
    output$table_cor = DT::renderDT({ 
        datatable(table_cor(), filter="top") %>%
            formatRound(columns="rho_s", digits=4)
    })

    output$table_alpha_t = DT::renderDT({
        table_alpha()$rst_alpha_t %>%
            dplyr::select(-gamma) %>%
            datatable() %>%
            formatRound(columns=c("rho", "alpha_t"), digits=4)
    })
    output$table_alpha_s = DT::renderDT({ 
        table_alpha()$rst_alpha_s %>%
            dplyr::select(-gamma) %>% 
            datatable(filter="top") %>%
        formatRound(columns=c("rho", "alpha_s"), digits=4)
    })
    
})
