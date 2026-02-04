## app_helpers

## Formatting of result tables

result_table <- function(result, .filter = NULL, flat = FALSE, lr_listed = ""){
  ## build fixes : start ##
  logP <- NULL
  varlogP <- NULL
  logP_lwr <- NULL
  logP_upr <- NULL
  z_score <- NULL
  p_value <- NULL
  n <- NULL
  meta <- NULL
  pop <- NULL
  . <- NULL
  accept <- NULL
  lat <- NULL
  lon <- NULL
  out_of_place <- NULL
  ## build fixes : end ##
  if (is.null(result)) return(NULL)
  hull_column <- grep(pattern = "^hull", x = names(result))
  if(length(hull_column)>0){ result <- result %>% select_(.dots = -hull_column) }
  result <- result %>% ## mutate(n = n/2) %>% 
    mutate_if(is.numeric, funs(round(.,3))) %>% 
    rename(
      `log10 P(G|pop)` = logP,
      `var[log10 P(G|pop)]` = varlogP,
      `CI[log10 P(G|pop)] lwr` = logP_lwr,
      `CI[log10 P(G|pop)] upr` = logP_upr,
      `z-score` = z_score,
      `p-value` = p_value
    )
  row_colours_hex <- bar_colour(result[,c("log10 P(G|pop)","accept",names(result)[1])], alpha = 0.1)
  row_colours <- rgba2rgb(row_colours_hex)
  ## print(row_colours)
  if(!is.null(.filter)){
    result <- result %>% filter_(.dots = .filter)
  }
  result <- result %>% select(1:2, n:accept, -labs, -lat, -lon, -out_of_place)
  if(flat) return(kable(result))
  result %>% 
    DT::datatable(rownames=FALSE, filter = "bottom", selection = 'none',
                  extensions = 'Buttons', options = list(
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  )
    ) %>% 
    formatStyle(columns = 1,
                target = "row",
                backgroundColor = styleEqual(result[[1]], row_colours[result[[1]]])) %>% 
    formatStyle(columns = 1,
                target = "row",
                fontWeight = styleEqual(lr_listed, rep('bold', length(lr_listed))))
}

## Formatting of LR output

LR_list <- function(result = NULL, LR_tab = NULL, lr_pops = NULL, CI, accepted, flat = FALSE){
  ## build fixes : start ##
  numerator <- NULL
  Numerator <- NULL
  denominator <- NULL
  Denominator <- NULL
  logLR <- NULL
  `log10 LR` <- NULL
  var_logLR <- NULL
  CI_lwr <- NULL
  CI_upr <- NULL
  `CI(log10 LR)` <- NULL
  null_in_CI <- NULL
  `Null in CI` <- NULL
  z_score <- NULL
  accept <- NULL
  . <- NULL
  ## build fixes : end ##
  if(is.null(LR_tab)){
    if(is.null(result)) return(NULL)
    if(is.null(lr_pops)) return(NULL)
    lr_list <- LR_table(result_df = result, lr_populations = lr_pops, CI = CI, only_accepted = !accepted)
    if(nrow(lr_list)==0) return(NULL)
  }
  else lr_list <- LR_tab
  lr_list <- lr_list %>% mutate_if(is.numeric, funs(round(.,3)))
  ## lr_list <- format(lr_list, digits = 3, nsmall = 3)
  lr_list <- lr_list %>% 
    mutate(`CI(log10 LR)` = paste0("[", CI_lwr, "; ", CI_upr,"]")) %>% 
    rename(
      Numerator = numerator,
      Denominator = denominator, 
      `log10 LR` = logLR,
      `var(log10 LR)` = var_logLR,
      # `CI(log10 LR) lwr` = CI_lwr,
      # `CI(log10 LR) upr` = CI_upr,
      `Null in CI` = null_in_CI
    ) %>% select(Numerator, Denominator, `log10 LR`, `CI(log10 LR)`, `Null in CI`) %>% 
    mutate_if(is.character, factor)
  ## 
  if(flat) return(kable(lr_list))
  min_z_pop <- result %>% filter(accept) %>% top_n(n = 1, wt = desc(z_score)) %>% pull(var = 1)
  if(length(min_z_pop)==0) min_z_pop <- ""
  formatStyle(datatable(lr_list,rownames=FALSE, filter = 'bottom', extensions = 'Buttons',
                        selection = 'none',
                        options = list(
                          dom = 'Blfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                          autoWidth = TRUE,
                          lengthMenu = list(c(10, 25, 50, 100, -1),
                                            c("10", "25", "50", "100", "All")),
                          columnDefs = list(list(width = '30%', targets = c(3)))
                          )), 
              columns = c('Numerator', 'Denominator'),
              fontWeight = styleEqual(min_z_pop, 'bold'),
              color = styleEqual(result[[1]], bar_colour(result[,c("logP","accept",names(result)[1])]))
  )
}

## Tool tip function

tool_tip <- function(hover, point){ ## , bg = "#FFFFFF"
  if (nrow(point) == 0) return(NULL)
  
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  ## bg_color <- as.numeric(col2rgb(bg))
  bg_color <- if(point$accept) c(255, 131, 131) else c(85, 153, 255)
  style <- paste0("position:absolute; z-index:100; background-color: rgba(", paste(bg_color, collapse=","), ", 0.85); ",
                  "left:", left_px + 2, "px; top:", top_px + 2, "px; ",
                  "padding: 5px; border: 1px solid black; ")
  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    shiny::p(HTML(paste0(
      "<b>Population: </b>", point[[2]], " (",point[[1]], ")<br/>",
      "<b>Sample size: </b>", point$n/2, "<br/>",
      "<b>p-value: </b>", round(point$p_value, 3), "<br/>",
      "<b>log P: </b>", round(point$logP, 3), "<br/>",
      "<b>var(log P): </b>", round(point$varlogP, 3)
    )))
  )
}

## write variable names in italic font with , and 'and' separation
and_text <- function(x, anchor = "i", pre = "", post = ""){
  if(length(x) == 0) return("")
  if(length(x) == 1) return(paste0(pre,"<",anchor,">",x,"</",anchor,">",post))
  else paste0(pre,"<",anchor,">",paste0(x[-length(x)], collapse = paste0("</",anchor,">, <",anchor,">")), "</",anchor,"> and <",anchor,">",x[length(x)],"</",anchor,">",post)
}

## Same as subset but returns NULL in case of empty
nullset <- function(x, ...){
  x <- subset(x, ...)
  if(length(x) == 0) return(NULL)
  x
}
