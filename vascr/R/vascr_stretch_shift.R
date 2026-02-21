# calc_stretch = function(t1, t2, stretch){
#   
#  t2b1 = t2 %>% mutate(Time = .data$Time*stretch) %>% 
#    dplyr::mutate(Time = .data$Time - min(.data$Time)) %>%
#    vascr_subset(time = c(min(t1$Time), max(t1$Time))) 
#  
#  
#  # t2b1 %>% vascr_resample_time(npoints = 10)
#  
#  # t2b1 %>% vascr_resample_time(npoints = 3)
#  
#  t2b = t2b1 %>%
#    vascr_resample_time(npoints = vascr_find_count_timepoints(t1), start = min(t1$Time), end = max(t1$Time))
#  
#  # print(t2b$Value)
#  # rbind(t1, t2, t2s) %>% vascr_plot_line
#  
#  # cc = ccf(t1$Value, t2$Value, lag.max = 0, plot = FALSE)$acf[1]
#  cc_full = ccf(t1$Value, t2b$Value, plot = FALSE)
#  cc.df = data.frame(lag = cc_full$"lag", cc = cc_full$"acf")
#  stretch_cc = (cc.df %>% dplyr::filter(.data$lag == 0))$"cc"
#  stretch_shift_cc  = (cc.df %>% dplyr::filter(.data$cc == max(.data$cc)))$"cc"
#  stretch_shift_shift  = (cc.df %>% dplyr::filter(.data$cc == max(.data$cc)))$"lag"
#  
#  # print(cc)
#  
#  # t2b %>% mutate(cc = cc, stretch = stretch)
#  
#  return(tribble(~"stretch_cc", ~"stretch_factor", ~"stretch_shift_cc", ~"stretch_shift_shift", stretch_cc, stretch, stretch_shift_cc, stretch_shift_shift))
#}
#
#
#
#
#
##Test if stretching a trace will allow it to better fit it's target
##'
##@param t1 Reference trace
##@param t2 Trace to be stretched
##'
##@returns A table of best fit stretch and shift values
##'
##@noRd
##'
##@importFrom dplyr mutate filter
##'
##@examples
## t1 = growth.df %>% vascr_subset(unit = "R", frequency = 4000, experiment = 1, sample = "10,000_cells + HCMEC D3_line") %>% vascr_summarise(level = "experiments")
## t2 = growth.df %>% vascr_subset(unit = "R", frequency = 4000, experiment = 1, sample = "30,000_cells + HCMEC D3_line") %>% vascr_summarise(level = "experiments")
##'
## stretch_cc(t1, t2)
#stretch_cc = function(t1, t2)
#{
#
#  # print(t1)
#  # print(t2)
#
#  # stretch = 1.5
#
#  stretch_series = (c(1:50)/10)
#
#  stretching = foreach(stretch = stretch_series, .combine = rbind) %do%{
#
#
#    t2b1 = t2 %>% mutate(Time = .data$Time*stretch) %>%
#      dplyr::mutate(Time = .data$Time - min(.data$Time)) %>%
#      vascr_subset(time = c(min(t1$Time), max(t1$Time)))
#
#
#    # t2b1 %>% vascr_resample_time(npoints = 10)
#
#    # t2b1 %>% vascr_resample_time(npoints = 3)
#
#    t2b = t2b1 %>%
#      vascr_resample_time(npoints = vascr_find_count_timepoints(t1), start = min(t1$Time), end = max(t1$Time))
#
#    # print(t2b$Value)
#    # rbind(t1, t2, t2s) %>% vascr_plot_line
#
#    # cc = ccf(t1$Value, t2$Value, lag.max = 0, plot = FALSE)$acf[1]
#    cc_full = ccf(t1$Value, t2b$Value, plot = FALSE)
#    cc.df = data.frame(lag = cc_full$"lag", cc = cc_full$"acf")
#    stretch_cc = (cc.df %>% dplyr::filter(.data$lag == 0))$"cc"
#    stretch_shift_cc  = (cc.df %>% dplyr::filter(.data$cc == max(.data$cc)))$"cc"
#    stretch_shift_shift  = (cc.df %>% dplyr::filter(.data$cc == max(.data$cc)))$"lag"
#
#    # print(cc)
#
#    # t2b %>% mutate(cc = cc, stretch = stretch)
#
#    return(tribble(~"stretch_cc", ~"stretch_factor", ~"stretch_shift_cc", ~"stretch_shift_shift", stretch_cc, stretch, stretch_shift_cc, stretch_shift_shift))
#
#  }

  # ad = rbind(t1 %>% vascr_remove_cols(c("n", "sem", "sd", "min", "max")) %>% mutate(cc = 0), stretching) %>%
  #     group_by(Experiment, Sample) %>%
  #     mutate(Value = (Value - min(Value))/(max(Value) - min(Value))) %>%
  #     ungroup()
  #
  #
  #  (ad %>% ggplot()+
  #   geom_line(aes(x = Time, y = Value, colour = cc, group = Experiment)) +
  #   scale_colour_viridis_c()) %>%
  #   plotly::ggplotly()
  #
  # (ad %>% ggplot()+
  #     geom_line(aes(x = Time, y = Value, colour = as.character(cc), group = Experiment))) %>%
  #   plotly::ggplotly()
  #
  # (stretching %>% select("Experiment","cc") %>%
  #   distinct() %>%
  #   ggplot() +
  #   geom_point(aes(x = Experiment, y = cc))) %>%
  #   plotly::ggplotly()
  #


#   # stretching
# 
#   tr1 = stretching %>% dplyr::filter(stretch_cc == max(.data$stretch_cc)) %>% select("stretch_cc", "stretch_factor")
# 
#   tr2 = stretching %>% dplyr::filter(stretch_shift_cc == max(.data$stretch_shift_cc)) %>%
#     select("stretch_shift_cc", "stretch_factor", "stretch_shift_shift") %>%
#     mutate(stretch_shift_factor = .data$stretch_factor, stretch_factor = NULL)
# 
#   tr3 = stretching %>% dplyr::filter(.data$stretch_factor == 1) %>%
#     dplyr::select("stretch_cc", "stretch_shift_cc", "stretch_shift_shift") %>%
#     dplyr::mutate(cc = .data$stretch_cc, stretch_cc = NULL) %>%
#     dplyr::mutate(shift_cc = .data$stretch_shift_cc, stretch_shift_cc = NULL) %>%
#     dplyr::mutate(shift_shift = .data$stretch_shift_shift, stretch_shift_shift = NULL)
# 
#   tr = cbind(tr3, tr2, tr1)
# 
# 
#   return(tr)
# }





#' Summarise the various stretch and shift parameters when comparing vascr datasets
#'
#' @param data.df The data set to analyse
#' @param reference Which sample to reference against, or "none" to run all pairings
#' 
#' @importFrom dplyr as_tibble
#' @importFrom progressr progressor
#' @importFrom doFuture %dofuture%
#'
#' @returns Data frame with various stretch and shift parameters compared
#' 
#' @noRd
#'
#' @examples
#' fast_growth.df = growth.df %>% vascr_resample_time(20)
#' vascr_summarise_cc_stretch_shift(fast_growth.df)
#' vascr_summarise_cc_stretch_shift(growth.df, reference = 5)
vascr_summarise_cc_stretch_shift = function(data.df = vascr::growth.df, unit = "R", frequency = 4000, reference = "none"){
  
  toprocess = data.df %>% vascr_subset(unit = unit, frequency = frequency) %>%
    vascr_resample_time(50)%>% vascr_summarise(level = "experiments")
  # vascr_plot_line(toprocess)
  s_cc = toprocess %>% vascr_cc(reference, cc_only = FALSE)
  
  # cli_progress_bar(total = nrow(cc_data))
  
  #progressr::handlers("cli")
  #progressr::handlers(global = TRUE)
  
  # p <- progressr::progressor(along = c(1:nrow(cc_data)))
  # 
  # i = 0
  # 
  # .options.future = list(packages = "vascr")
  # 
  # stretch_cc = stretch_cc
  # 
  # s_cc = foreach (i  =  c(1:nrow(cc_data)), .combine = rbind) %do% {
  #   
  #   
  #   cc_row = cc_data[i,]
  #   
  #   t1 = toprocess %>% dplyr::filter(.data$Sample == cc_row$Sample.x & .data$Experiment == cc_row$Experiment.x)
  #   t2 = toprocess %>% dplyr::filter(.data$Sample == cc_row$Sample.y & .data$Experiment == cc_row$Experiment.y)
  #   
  #   str = cc_stretch_shift_fit(t1, t2)
  #   # p()
  #   
  #   
  #   toreturn = cbind(cc_row %>% dplyr::select(-"cc"), str) %>% dplyr::as_tibble()
  #   
  #   return(toreturn)
  # }
  
  
  # s_cc
  
  s_long = s_cc %>%
    rowwise() %>%
    mutate(title = paste(as.character(.data$Sample.x), as.character(.data$Sample.y), sep = " - ")) %>%
    select("title", "Sample.x", "Sample.y", "cc", "shift_cc", "shift_offset", "stretch_cc", "stretch_factor", "stretch_shift_cc", "stretch_shift_offset") %>%
    pivot_longer(cols = c(-"title", -"Sample.x", -"Sample.y"))
  
  return(s_long)
  
}


# stretch_cc = function(t1, t2){
#   
#   x_test = 1
#   y_history<-as.data.frame(matrix(ncol=2,nrow=num_iters))
#   colnames(y_history) = c("x","y")
#   
#   for (iter in 1:50){
#     
#     y<-calc_stretch(t1, t2, x_test)$stretch_shift_cc
#     
#     y_history[iter,] <- c(x_test, y)
#     
#     if(y>=max(y_history$y, na.rm = TRUE))
#     {
#       x_test = x_test + (1-y)
#     }
#     
#     if(x_test - max(y_history$x, na.rm = TRUE) <0.001)
#     {
#       break()
#     }
#     
#     
#   }
#   
#   
#   # y_history
#   # 
#   # plot(y_history$y)
#   r1 = calc_stretch(t1, t2, 1) %>% mutate(stretch_factor = NULL, 
#                                           cc = stretch_cc, stretch_cc = NULL,
#                                           shift_cc = stretch_shift_cc, stretch_shift_cc = NULL,
#                                           shift_shift = stretch_shift_shift, stretch_shift_shift = NULL)
#   
#   r2 = calc_stretch(t1, t2, max(y_history$x, na.rm = TRUE))
#   
#   toreturn = cbind(r1, r2)
#   
#   return(toreturn)
#   
# }


# t1 = growth.df %>% vascr_subset(unit = "R", frequency = 4000, experiment = 1, sample = "10,000_cells + HCMEC D3_line") %>% vascr_summarise(level = "experiments")
# t2 = growth.df %>% vascr_subset(unit = "R", frequency = 4000, experiment = 1, sample = "30,000_cells + HCMEC D3_line") %>% vascr_summarise(level = "experiments")

# tic()
# stretch_cc(t1, t2)
# toc()
# 
# ccf(t1$Value, t2$Value)[0]

# stretch_series(t2$Value, 2, return_model = TRUE)

stretch_series = function(series, stretch, return_model = FALSE) {
  reference_times = c(0:(length(series)-1))
  
  resampled = approx(reference_times * stretch, series, c(0:(length(reference_times)*stretch-1)))
  
  if(isTRUE(return_model)){
    return(resampled %>% as.data.frame())
  } else
  {
    return(resampled$y)
  }
  
}

# stretch_cc_fast(s1, s2, stretch)

#' Calculate the cross correlation of a stretched dataset
#'
#' @param s1 Vector containing a reference sequence
#' @param s2 Vector containing the sequence for testing
#' @param stretch The extent by which the stretch should be calculated
#' 
#' 
#' @importFrom stats na.pass ccf
#'
#' @returns A tibble with the stretch number, cross correlation, maximum shift and the shift at the max ccf respectivley
#' 
#' @noRd
#'
#' @examples
#' 
#' t1 = growth.df %>% vascr_subset(unit = "R", frequency = 4000, experiment = 1, sample = "10,000_cells + HCMEC D3_line") %>% vascr_summarise(level = "experiments")
#  t2 = growth.df %>% vascr_subset(unit = "R", frequency = 4000, experiment = 1, sample = "30,000_cells + HCMEC D3_line") %>% vascr_summarise(level = "experiments")
#' 
#' stretch_cc_fast(t1$Value, t2$Value, 5)
#' 
stretch_cc_fast = function(s1, s2, stretch)
{
  stretched = stretch_series(s2, stretch)
  
  cc = ccf(s1, stretched, na.action = na.pass, plot = FALSE)
  
  tibble(
    stretch = stretch,
    stretch_cc = cc[0]$acf[1,1,1],
    stretch_shift_cc = max(cc$acf),
    stretch_shift_shift = cc$lag[which.max(cc$acf)])
}


#' Stretch a value to fit
#'
#' @param s1 Series 1 to test
#' @param s2 Second vector to test against
#'
#' @returns A table with the optimal alignment characteristics
#' 
#' @noRd
#' 
#' @importFrom utils head
#'
#' @examples
#' cc_stretch_shift_fit((growth.df %>% vascr_subset(unit = "R", frequency = "4000", sampleid = c(1)))$Value, 
#'           (growth.df %>% vascr_subset(unit = "R", frequency = "4000", sampleid = c(1)))$Value)
cc_stretch_shift_fit = function(s1, s2){
  
  # s1 = t1$Value
  # s2 = t2$Value
  # stretch = 1.5
  
  # Set I to keep CRAN check happy
  i = 1
  
  stretch_range = foreach (i = c(5:100)/10, .combine = rbind) %do% {
    stretch_cc_fast(s1, s2, i)
  }
  
  
  stretch_1 = filter(stretch_range, .data$stretch == 1) %>% head(1)
  best_stretch = filter(stretch_range, .data$stretch_cc == max(.data$stretch_cc, na.rm = TRUE))%>% head(1)
  best_stretch_shift = filter(stretch_range, .data$stretch_shift_cc == max(.data$stretch_shift_cc, na.rm = TRUE))%>% head(1)
  
  
  data.frame(
    cc = stretch_1$stretch_cc,
    shift_offset = stretch_1$stretch_shift_shift,
    shift_cc = stretch_1$stretch_shift_cc,
    stretch_factor = best_stretch$stretch,
    stretch_cc = best_stretch$stretch_cc,
    stretch_shift_offset = best_stretch_shift$stretch_shift_shift,
    stretch_shift_factor = best_stretch_shift$stretch,
    stretch_shift_cc = best_stretch_shift$stretch_shift_cc
  )
  
}

# transform_series(t1$Value, 2, 4, TRUE)

transform_series = function(series, stretch, shift, norm){
  ns = stretch_series(series, stretch , return_model = TRUE)
  ns$x = ns$x + shift
  ns$y = (ns$y-min(ns$y, na.rm = TRUE))/(max(ns$y, na.rm = TRUE) - min(ns$y, na.rm = TRUE))
  
  return(ns)
}


# ggplot() +
#   geom_point(aes(x = .data$x, y = y, color = "1) reference"), data = transform_series(t1$Value, 1, 0)) +
#   geom_point(aes(x = .data$x, y = y, color = "2) original"), data = transform_series(t2$Value, 1, 0)) +
#   geom_point(aes(x = .data$x, y = y, color = "3) Stretched"), data = transform_series(t2$Value, 1.8, 0)) +
#   geom_point(aes(x = .data$x, y = y, color = "4) Stretched + Shifted"), data = transform_series(t2$Value, 2, 0))
# 
# 
# ccd = growth.df %>% vascr_subset(unit = "R", frequency = 4000) %>%
#   vascr_cc()




  
#' Calculate the statistical differences after stretching and shifting to an optimal cross correlation
#'
#' @param data.df The dataset to reference
#' @param reference 
#' 
#' @importFrom stats t.test p.adjust symnum
#' @importFrom dplyr filter select rowwise group_split
#' @importFrom ggplot2 ggplot geom_point aes
#' @importFrom foreach foreach %do% 
#'
#' @returns A summary table of the results from the cross corelation
#' 
#' @noRd
#'
#' @examples
#' vascr_summarise_cc_stretch_shift_stats(growth.df)
#' 
vascr_summarise_cc_stretch_shift_stats = function(data.df, unit = "R", frequency = 4000, reference = "none"){
  
  s_long = vascr_summarise_cc_stretch_shift(data.df, unit, frequency, reference) %>% filter(.data$name == "cc")
  
  # s_long %>% filter(str_count(.data$name, "cc")>0) %>%
  #   ggplot() +
  #   geom_point(aes(x = .data$value, y = .data$title, colour = .data$name))
  
  
  
  pairs = s_long %>% ungroup() %>% 
    filter(str_count(.data$name, "cc") > 0) %>%
    select("Sample.x", "Sample.y", "title", "name") %>% distinct() %>%
    rowwise() %>%
    group_split()
  
  reference_sample = vascr_find_sample(data.df, reference)
  
  # Create global binding to keep checks happy
  pair = 0
  p = 1
  
  output = foreach (pair = pairs, .combine = rbind) %do% {
    
    
    s1 = pair$Sample.x
    s2 = pair$Sample.y
    sta = pair$name
    
    t1 = s_long %>% filter(.data$Sample.x == s1, .data$Sample.y ==s2) %>% filter(.data$name == sta)
    
    if(isTRUE(reference == "none"))
    {
      t2 = rbind( s_long %>% filter(.data$Sample.x == s1, .data$Sample.y ==s1) %>% filter(.data$name == sta),
                  s_long %>% filter(.data$Sample.x == s2, .data$Sample.y ==s2) %>% filter(.data$name == sta))
    } else
    {
      t2 = rbind( s_long %>% filter(.data$Sample.x == reference_sample, .data$Sample.y == reference_sample) %>% filter(.data$name == sta))
    }
    
    if(length(unique(c(t1$value, t2$value))) == 1){
      p$p.value = 1
    } else {
      p = t.test(t1$value, t2$value, var.equal = FALSE)
    }
    
    
    return(tribble(~name,     ~title,     ~p,        ~mean, ~sd, ~nsample, ~ncontrol,~Sample.x, ~Sample.y, ~refs,
                   pair$name, pair$title, p$p.value, mean(t1$value), sd(t1$value), length(t1$value), length(t2$value), s1, s2, paste(t2$value, collapse = ",")))
  }
  
  
  output$padj = output$p #p.adjust(output$p, "fdr")
  
  output$stars <- symnum(output$p, corr = FALSE, na = FALSE, 
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                         symbols = c("***", "**", "*", ".", " "))
  
  return(output)
}


#' Plot stretched and shifted cross correlation statistics
#'
#' @param data.df The vascr dataset to run
#' @param reference A reference value to compare against, default "none"
#'
#' @returns A ggplot overlaying the calculated statistics from the cross correlation on the underlying data
#' 
#' @noRd
#'
#' @examples
#' vascr_plot_cc_stretch_shift_stats(growth.df)
#' vascr_plot_cc_stretch_shift_stats(growth.df, reference = "5,000_cells + HCMEC D3_line")
#' 
#' 
vascr_plot_cc_stretch_shift_stats = function(data.df, unit= "R", frequency = 4000, reference = "none"){
  
  unit = vascr_find_unit(data.df, unit)
  
  output = vascr_summarise_cc_stretch_shift_stats(data.df, unit, frequency, reference)
  
  output %>%
    ggplot() +
    geom_point(aes(x = .data$mean, y = .data$title, color = .data$name)) +
    geom_errorbar(aes(xmin = mean-sd, xmax = mean+sd, y = .data$title, color = .data$name)) +
    geom_text_repel(aes(x = .data$mean, y = .data$title, color = .data$name, label = as.character(.data$stars)), direction = "y", seed = 1, nudge_y = 0.2, box.padding = 0, point.padding = 0)

  
  
  
  # s_sum = s_long %>% rowwise() %>%
  #   group_by(title, name) %>%
  #   reframe(mean = mean(value), sd = sd(value))
  # 
  # s_sum %>% filter(str_count(name, "cc")>0) %>%
  # ggplot() +
  #   geom_point(aes(x = mean, y = title, color = name))
  
}


#' Setup a progressr and future environment for high performance calculations
#'
#' @returns Nothing, just prepping the environment for other things
#' 
#' @noRd
#'
#' @examples
#' vascr_setup_paralell()
#' 
vascr_setup_paralell = function(){
  future::plan("multisession")
  progressr::handlers(global = TRUE)
  progressr::handlers("cli")
}
