
##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

##define the main tabset for beans
tab_main <- function() {
    tabsetPanel(type = "pills",
                id   = "mainpanel",
                tab_fwer(),
                tab_cor_s(),
                tab_alpha_t()
    )
}

tab_fwer <- function() {
    tabPanel("Family-wise Type I Error Rate",
             sidebarLayout(
                 sidebarPanel(
                     textInput("n", "Sample size per arm at DTL Look (n)", 
                               "50, 150, 300"),
                     textInput("delta", "Least difference to decide superiority of High Dose (Δ)", "0, 0.1"),
                     textInput("q", "Null response rate (q)", 
                               "0.3, 0.5, 0.95"),
                     textInput("rho_seq", "Range of correlation between statistics at DTL look and final analysis (ρ)", 
                               "0, 0.5"),
                     numericInput("rho_seq_space", "Space to make a sequence for rho", 0.1),
                     numericInput("alpha", "Significance level (α)", 0.025),
                 ),
                 mainPanel(
                     tabsetPanel(type = "tabs", id = "id_fwer",
                                 tabPanel("Plot",
                                          h4("Graph of family-wise type I error rate"),
                                          plotOutput('plot_tier', width = "100%", height = "700px"),
                                 ),
                                 tabPanel("Table",
                                          h4("Table of family-wise type I error rate"),
                                          DTOutput('table_tier'),
                                 )
                     )
                 )
             )
    )
}

tab_cor_s <- function() {
    tabPanel("Correlation Coefficient Boundary",
             sidebarLayout(
                 sidebarPanel(
                     textInput("tau", "Ratio of sample sizes at DTL look and final analysis (τ = n/N)",
                               "0.5, 0.8"),
                     textInput("q_seq", "Range of null response rate (q)",
                               "0.1, 0.9"),
                     textInput("gamma_seq", "Range of hazards ratio of responder and non-responder for each arm (γ)",
                               "0.1, 0.9"),
                     numericInput("q_seq_space", "Space to make a sequence for q", 0.1),
                     numericInput("gamma_seq_space", "Space to make a sequence for γ", 0.1),
                     numericInput("fix_q", "Fixed value of q for graph of γ vs ρ* (select a value from your inputs above)",0.3),
                     numericInput("fix_gamma", "Fixed value of γ for graph of q vs ρ* (select a value from your inputs above)", 0.2),
                 ),
                 mainPanel(
                     tabsetPanel(type = "tabs", id = "id_fwer",
                                 tabPanel("Plot",
                                          h4("Graph of Correlation Coefficient Boundary"),
                                          plotOutput('plot_cor', width = "100%", height = "500px"),
                                 ),
                                 tabPanel("Table",
                                          h4("Table of Correlation Coefficient Boundary"),
                                          DTOutput('table_cor'),
                                 )
                     )
                 )
             )
    )
}

tab_alpha_t <- function() {
    tabPanel("Adjusted Significance Level",
             sidebarLayout(
                 sidebarPanel(
                     numericInput("n_2", "Sample size per arm at DTL Look (n)", 80),
                     numericInput("N", "Sample size per arm at final analysis (N)", 162),
                     numericInput("delta_2", "Least difference to decide superiority of High Dose (Δ)", 0.05),
                     textInput("q_seq_2", "Range of q (can be 100(1-2α)% CI)", "0.3, 0.5"),
                     numericInput("gamma_L", "Lower bound of γ (can be the lower bound of 100(1-2α)% CI)", 0.2),
                     numericInput("q_seq_space_2", "Space to make a sequence for q", 0.01),
                     numericInput("alpha_2", "Significance Level (α)", 0.025),
                     numericInput("fix_rho", "Fixed ρ or empty for using ρ*", ""),
                 ),
                 mainPanel(
                     h4("Table of adjusted significance level"),
                     DTOutput('table_alpha_t'),
                     br(),
                     h4("Full table"),
                     DTOutput('table_alpha_s'),
                 )
             )
    )
}




#---------------------FWER-----------------------
table_tier = reactive({
    
    alpha_s  = input$alpha
    n        = as.numeric(str_split(input$n,",", simplify = TRUE))
    q        = as.numeric(str_split(input$q,",", simplify = TRUE))
    
    
    rho_range = as.numeric(str_split(input$rho_seq,",", simplify = TRUE))
    rho_seq   = round(seq(rho_range[1], rho_range[2], input$rho_seq_space), 10)
    delta     = as.numeric(str_split(input$delta,",", simplify = TRUE))
    
    para_all = expand.grid(n, q, rho_seq, delta)
    fwer     = rep(NA, nrow(para_all))
    for (i in 1:nrow(para_all)){
        n     = para_all[i, 1]
        q     = para_all[i, 2]
        rho   = para_all[i, 3]
        delta = para_all[i, 4]
        
        fwer[i] = dtl_tier_the(n, t = 1, rho, q, alpha_s, delta)
    }
    rst = data.frame(alpha_s, para_all, fwer)
    colnames(rst) = c("alpha_s", "n", "q", "rho", "delta", "fwer")
    rst
    
})

plot_tier = reactive({
    
    rst_graph_tier = table_tier()
    
    n_names     = paste("n =", unique(rst_graph_tier$n))
    q_names     = paste("q =", unique(rst_graph_tier$q))
    delta_names = paste("Δ =", unique(rst_graph_tier$delta))
    
    rst_graph_tier$n     = factor(rst_graph_tier$n, labels=n_names)
    rst_graph_tier$q     = factor(rst_graph_tier$q, labels=q_names)
    rst_graph_tier$delta = factor(rst_graph_tier$delta, labels=delta_names)
    
    plot_tier = ggplot(data = rst_graph_tier, aes(x = rho, y = fwer)) +
        geom_line(aes(color=n, linetype=n), linewidth = 1) +
        geom_point(aes(color=n, shape=n), size = 3) +
        geom_hline(yintercept = unique(rst_graph_tier$alpha_s), linetype="dashed", linewidth = 1) +
        facet_grid(delta ~ q) +
        labs(x="Correlation", y="Family-wise Type I Error Rate") +
        theme_bw() + 
        theme(legend.title=element_blank(),
              legend.position = "bottom", 
              text=element_text(size=17),
              plot.title=element_text(hjust=0.5),
              legend.key.height= unit(1, 'cm'),
              legend.key.width= unit(1.5, 'cm'))
    plot_tier
    
})


#---------------------Correlation Boundary-----------------------
table_cor = reactive({
    
    tau   = as.numeric(str_split(input$tau,",", simplify = TRUE))
    
    q_range     = as.numeric(str_split(input$q_seq,",", simplify = TRUE))
    gamma_range = as.numeric(str_split(input$gamma_seq,",", simplify = TRUE))
    q_seq       = round(seq(q_range[1], q_range[2], input$q_seq_space), 10)
    gamma_seq   = round(seq(gamma_range[1], gamma_range[2], input$gamma_seq_space), 10)
    
    para_all = expand.grid(tau, q_seq, gamma_seq)
    rho_s    = rep(NA, nrow(para_all))
    for (i in 1:nrow(para_all)){
        tau   = para_all[i, 1]
        q     = para_all[i, 2]
        gamma = para_all[i, 3]
        
        rho_s[i] = dtl_cor_the_PH_upper_bound(tau, pi_ar = 0.5, q, gamma)
    }
    rst = data.frame(para_all, rho_s)
    colnames(rst) = c("tau", "q", "gamma", "rho_s")
    rst
    
})

plot_cor = reactive({
    
    rst_graph_cor = table_cor()
    
    tau_name = paste0("τ = ", unique(rst_graph_cor$tau))
    
    rst_graph_cor$tau = factor(rst_graph_cor$tau, labels=tau_name)
    
    rst_graph_cor_q     = rst_graph_cor %>% 
        filter(gamma == input$fix_gamma) %>%
        mutate(para = q) %>%
        dplyr::select(-c(q, gamma))
    
    rst_graph_cor_gamma = rst_graph_cor %>% 
        filter(q == input$fix_q) %>%
        mutate(para = gamma) %>%
        dplyr::select(-c(q, gamma))
    
    rst_graph_cor_all   = rbind(data.frame(rst_graph_cor_q, val_type = "q"),
                                data.frame(rst_graph_cor_gamma, val_type = "γ"))
    
    plot_cor = ggplot(data=rst_graph_cor_all, aes(x=para, y=rho_s)) +
        geom_line(aes(color=tau, linetype=tau), linewidth = 1) +
        geom_point(aes(color=tau, shape=tau), size = 3) +
        scale_shape_discrete(na.translate = F) +
        scale_x_continuous(breaks = 0.1*seq(0,10,1)) +
        scale_y_continuous(breaks = 0.1*seq(0,10,1)) +
        facet_grid(cols = vars(val_type), scales = "free_x") +
        labs(x="Parameter", y="Correlation Coefficient Boundary (ρ*)") +
        theme_bw() + 
        theme(legend.title=element_blank(),
              legend.position = "bottom", 
              text=element_text(size=17), 
              plot.title=element_text(hjust=0.5),
              legend.key.height= unit(1, 'cm'),
              legend.key.width= unit(1.5, 'cm'))
    plot_cor
    
})

#---------------------Adjusted alpha: alpha_t-----------------------
table_alpha = reactive({
    
    n         = input$n_2
    N         = input$N
    delta     = input$delta_2
    
    q_range_2   = as.numeric(str_split(input$q_seq_2,",", simplify = TRUE))
    q_seq_2     = round(seq(q_range_2[1], q_range_2[2], input$q_seq_space_2), 10)
    gamma_L     = input$gamma_L
    alpha       = input$alpha_2
    fix_rho     = as.numeric(input$fix_rho)
    if (is.na(fix_rho)){
        fix_rho = NULL
    } 
    
    dtl_app_get_alpha_t(n, N, q_seq_2, gamma_L, alpha, fix_rho, delta) 
    
})





