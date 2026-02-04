#' LR plot
#' 
#' Plots the LR of population likelihoods
#' @param result Result from \code{genogeo}. At least one of \code{result} and \code{LR_list} is needed.
#' @param LR_list Result from \code{LR_table}. At least one of \code{result} and \code{LR_list} is needed.
#' @param rows Which rows from LR list (or the computed) are used
#' @param theme_ The ggplot2 theme
#' @param ... Additional arguments passed to \code{LR_table}
#' @return A plot
#' @export
LR_plot <- function(result = NULL, LR_list = NULL, rows = NULL, theme_ = theme_bw(),...){
  ##
  . <- NULL
  numerator <- NULL
  denominator <- NULL
  ##
  if(!is.null(result) & is.null(LR_list)) LR_list <- LR_table(result_df = result, ...)
  else if(!is.null(LR_list)){
    if(is.null(result)) {
      colour_den <- LR_list %>% select_(pop = .[1]) %>% mutate(colour_den = "#000000")
      colour_num <- LR_list %>% select_(pop = .[1]) %>% mutate(colour_num = "#FFFFFF")
    }
    else{
      colour_den <- bar_colour(result[,c("logP", "accept", names(result)[1])]) %>% enframe(name = "pop", value = "colour_den")
      colour_num <- bar_colour(result[,c("logP", "accept", names(result)[1])], alpha = 0.1) %>% enframe(name = "pop", value = "colour_num")
    }
    LR_list <- LR_list %>% 
      inner_join(colour_num, by = c("numerator" = "pop")) %>% 
      inner_join(colour_den, by = c("denominator" = "pop"))
  }
  else stop("Need either result or LR_list")
  LR_list_n <- LR_list %>% mutate(num_den = paste(numerator, denominator, sep = " : ")) %>% 
    mutate(Numerator = fct_inorder(numerator),
           Denominator = fct_inorder(denominator) %>% fct_rev())
  if(is.null(rows)) rows <- 1:nrow(LR_list_n)
  ## Select the rows
  LR_list_n <- LR_list_n %>% slice(rows)
  ## Common xlim
  LR_list_xlim <- range(c(-1,LR_list_n$CI_lwr,LR_list_n$CI_upr))
  ## Split per numerator population
  LR_list_ns <- LR_list_n %>% split(.$Numerator, drop = TRUE)
  LR_list_ns_len <- length(LR_list_ns)
  ## if just a single numerator
  if(LR_list_ns_len <= 1){
    LR_plots_flat <- lr_plot(LR_list_ns[[1]], x_lim = LR_list_xlim, type = 4L, theme_ = theme_)
  }
  else{
    plot_type <- rep(1L, LR_list_ns_len)
    plot_type[LR_list_ns_len] <- 3L ## last
    plot_type[round(LR_list_ns_len/2)] <- 2L ## mid
    LR_plots <- LR_list_ns %>% purrr::map2(.y = plot_type, .f = lr_plot, x_lim = LR_list_xlim, theme_ = theme_)
    LR_plots_flat <- wrap_plots(LR_plots, heights = map_int(LR_list_ns, nrow), ncol = 1)
  }
  LR_plots_flat
}

lr_plot <- function(dat, x_lim = NULL, type = NULL, theme_ = theme_bw()){
  ##
  logLR <- NULL
  Denominator <- NULL
  CI_lwr <- NULL
  CI_upr <- NULL
  colour_den <- NULL
  null_in_CI <- NULL
  ##
  if(is.null(dat) | nrow(dat) == 0) return(NULL)
  LRplot <- dat %>% 
    ggplot(aes(x = logLR, y = Denominator, xmin = CI_lwr, xmax = CI_upr, colour = I(colour_den))) + 
    geom_vline(xintercept = 0, linetype = 2) + 
    geom_errorbarh() + geom_point() + 
    coord_cartesian(xlim = c(x_lim[1], x_lim[2])) + theme_ + 
    facet_grid(.~Numerator, switch = "y", scales = "free", labeller = label_both, space = "free_y") + 
    theme(strip.background=element_rect(fill= dat$colour_num[1]))
  if(type == 3L) LRplot <- LRplot + labs(x = expression(log[10]~LR), y = "")
  else if(type == 2L) LRplot <- LRplot + labs(y = "Denominator", x = "")
  else if(type == 4L) LRplot <- LRplot + labs(x = expression(log[10]~LR), y = "Denominator")
  else LRplot <- LRplot + labs(x = "", y = "")
  if(type < 3L) LRplot <- LRplot + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),  axis.ticks.x=element_blank())
  LRplot + theme(plot.margin = ggplot2::margin(t=0,r=2,b=-1,l=0))
}


