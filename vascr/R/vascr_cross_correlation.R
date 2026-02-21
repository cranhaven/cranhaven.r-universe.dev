#' Title
#'
#' @param data.df 
#' 
#' @importFrom tidyr separate_longer_delim 
#' @importFrom dplyr filter mutate left_join rowwise group_split tribble select
#' @importFrom ggplot2 ggplot geom_point geom_errorbar labs theme scale_colour_manual
#' @importFrom ggnewscale new_scale
#' @importFrom stats t.test
#'
#' @returns A plot of cross-correlations and associated statistics
#' 
#' @noRd
#'
#' @examples
#' 
#' vascr_plot_cc_stats(growth.df)
#' vascr_plot_cc_stats(growth.df, reference = 1)
#' 
#' vascr_plot_cc_stats(growth.df, pval = TRUE)
#' vascr_plot_cc_stats(growth.df, reference = 1, pval = TRUE)
#' 
#' vascr_plot_cc_stats(growth.df, points = TRUE)
#' vascr_plot_cc_stats(growth.df, reference = "0_cells + HCMEC D3_line", points = TRUE)
#' 
#' 
vascr_plot_cc_stats = function(data.df, unit = "R", frequency = 4000, reference = "none", points = FALSE, stars = TRUE, pval = FALSE){
  
  if(isTRUE(pval)){stars = FALSE}
  
  
  if("cc" %in% colnames(data.df))
  {
    cc_data = data.df
  } else{
    
    subdata = data.df %>% vascr_subset(unit = unit, frequency = frequency) %>%
      vascr_summarise("experiments")
    
    cc_data = subdata %>%
      vascr_cc(reference = reference)
    
    
  }

  
ccf_calc =  cc_data %>%
            mutate(title = paste(.data$Sample.x, .data$Sample.y , sep = "x")) %>%
            mutate(expid = paste(.data$Experiment.x, .data$Experiment.y))

colours = vascr_gg_color_hue(length(unique(c(ccf_calc$`Sample.x`, ccf_calc$`Sample.y`))))

hue = tibble(Sample = unique(c(ccf_calc$`Sample.x`, ccf_calc$`Sample.y`)), colours = colours)

x = NULL
y = NULL

ccf_calc = ccf_calc %>% left_join(hue, join_by(x$`Sample.x` == y$`Sample`)) %>% 
  mutate(hue1 = .data$colours, colours = NULL) %>% 
  left_join(hue, join_by(x$`Sample.y` == y$`Sample`)) %>% 
  mutate(hue2 = colours, colours = NULL) %>%
  mutate(title = glue("<span style = 'color:{hue1};'>{`Sample.x`}</span><br>
                       <span style = 'color:{hue2};'>{`Sample.y`}</span>"))

if(!isTRUE(reference == "none"))
{
  ccf_calc$title = ccf_calc$Sample.x
}


pairs = ccf_calc %>% ungroup() %>% select("Sample.x", "Sample.y", "title") %>% distinct() %>%
          rowwise() %>%
          group_split()

# Local binding for namespace checks
pair = NULL


output = foreach (pair = pairs, .combine = rbind) %do% {
  
  s1 = pair$"Sample.x"
  s2 = pair$"Sample.y"
  
  t1 = ccf_calc %>% filter(.data$Sample.x == s1, .data$Sample.y ==s2)
  
  if(isTRUE(reference == "none"))
  {
  t2 = rbind( ccf_calc %>% filter(.data$Sample.x == s1, .data$Sample.y ==s1),
              ccf_calc %>% filter(.data$Sample.x == s2, .data$Sample.y ==s2))
  } else
  {
  t2 = rbind( ccf_calc %>% filter(.data$Sample.x == reference, .data$Sample.y ==reference))
  }
  
  # print(c(t1$cc,t2$cc))
   # print(sd(c(t1$cc,t2$cc)))
  
  if(sd(c(t1$cc,t2$cc))<0.1){
    p = data.frame(pvalue = 1)
  } else{
    p = t.test(t1$cc, t2$cc, var.equal = FALSE)
  }
  
  
  return(tribble(~title, ~p, ~cc, ~sd, ~nsample, ~ncontrol,~Sample.x, ~Sample.y, ~refs,
                 pair$title, as.numeric(p$p.value), mean(t1$cc), sd(t1$cc), length(t1$cc), length(t2$cc), s1, s2, paste(t2$cc, collapse = ",")))
}


# ggplot() +
#   geom_point(aes(x = t1$cc, y = "test"))+
#   geom_point(aes(x = t2$cc, y = "control"))


output$padj = p.adjust(output$p, "fdr")

output$stars <- symnum(output$padj, corr = FALSE, na = FALSE, 
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                symbols = c("***", "**", "*", ".", " "))

toplot = output %>% filter(.data$Sample.x != .data$Sample.y)

toplot = toplot %>% mutate(Sample.x = factor(.data$Sample.x, hue$Sample)) %>% mutate(Sample.y = factor(.data$Sample.y, hue$Sample))

ccf_calc = ccf_calc %>% mutate(Sample.x = factor(.data$Sample.x, hue$Sample)) %>% mutate(Sample.y = factor(.data$Sample.y, hue$Sample))

ungroup_toplot = ccf_calc %>% filter(.data$Sample.x != .data$Sample.y)
# ungroup_references = ccf_calc %>% filter(Sample.x != Sample.y)

long_ref = output %>% 
                  select("title", "refs", "Sample.x", "Sample.y") %>%
                  separate_longer_delim("refs", delim = ",") %>%
                  mutate(refs = as.numeric(.data$refs))



toreturn = toplot %>%
  ggplot() +
  geom_point(aes(y = .data$title, x = .data$cc, color = .data$Sample.y)) +
  geom_errorbar(aes(y = .data$title, xmin = .data$cc-.data$sd, xmax = .data$cc+.data$sd, colour = .data$Sample.x)) +
  labs(colour = "Significance") +
  theme(axis.text.y = element_markdown()) +
  scale_colour_manual(values = hue$colours)

toreturn

if(isTRUE(stars)){
  toreturn = toreturn + geom_text(aes(x = .data$cc, y = .data$title, label = as.character(.data$stars)), nudge_y = 0.2) 
}

if(isTRUE(pval)){
  text2 = toplot %>% filter(!is.na(.data$padj))
  
  toreturn = toreturn + geom_text(aes(x = .data$cc, y = .data$title, label = round(.data$padj,3)), nudge_y = 0.3, data = text2) 
}

if(isTRUE(points))
{
  
  long_ref_noauto = long_ref %>% filter(.data$Sample.x != .data$Sample.y)
  
toreturn = toreturn +
  ggnewscale::new_scale("color") +
  geom_point(aes(y = .data$title, x = .data$cc, color = "Correlations"), data = ungroup_toplot) +
  geom_point(aes(y = .data$title, x = .data$refs, color = "References"), data = long_ref_noauto) +
  scale_colour_manual(values = c("black", "grey")) +
  labs(colour = NULL)

toreturn
}

toreturn



}

#' Calculate the cross correlation coefficients of a vascr dataset
#'
#' @param data.df a vascr dataset to calculate
#' @param reference  The sample to reference all CC's against. Defaults to all comparisons
#'
#' @return a vascr dataset containing the cross correlation coefficients between curves
#' 
#' @importFrom cli cli_progress_bar cli_progress_update cli_process_done
#' @importFrom dplyr bind_cols group_by arrange summarise rename_with inner_join rowwise
#' @importFrom stats ccf
#' @importFrom tidyr nest unnest
#' @importFrom furrr future_map
#' 
#' @noRd
#'
#' @examples
#' data.df = vascr::growth.df %>% vascr_subset(unit = "R", frequency = 4000, sampleid = c(1,4,7), experiment = "1 : Experiment 1", time = c(5,50))
#' 
#' data.df = vascr::growth.df %>% vascr_subset(unit = "R", frequency = 4000, sampleid = c(1,4,7), time = c(5,50)) %>%
#' vascr_summarise(level = "experiments")
#' 
#' vascr_cc(data.df)
#' vascr_cc(data.df, cc_only = FALSE)
#' 
#' vascr_cc(data.df, reference = 1)
#' 
#' vascr_cc(data.df, reference = "35,000_cells + HCMEC D3_line")
#' vascr_cc(data.df, reference = c("35,000_cells + HCMEC D3_line", "5,000_cells + HCMEC D3_line"))
#' 
vascr_cc = function(data.df, reference = "none", cc_only = TRUE) {
  
curves = data.df %>%
                filter(!is.na(.data$Value)) %>% 
                group_by(.data$Well, .data$SampleID, .data$Sample, .data$Experiment) %>%
                arrange(.data$Time, .data$SampleID) %>%
                mutate(Time = NULL) %>%
                summarise(values = list(.data$Value), .groups = "keep") %>%
                ungroup("Well") %>%
                arrange("SampleID") 

if(vascr_find_level(data.df) == "wells")
  {
  pairedcurves = inner_join(curves, curves, by = c("Experiment"), relationship = "many-to-many") %>%
                  filter(.data$SampleID.x <= .data$SampleID.y)
  } else
  {
    # c1 = curves
    # c2 = curves

    # # pairedcurves = tidyr::crossing(c1, c2)
    
    c1 = curves %>% mutate(expid = as.numeric(.data$Experiment))
    c2 = curves %>% mutate(expid = as.numeric(.data$Experiment))
    
    colnames(c1) = paste(colnames(c1), ".x", sep = "")
    colnames(c2) = paste(colnames(c2), ".y", sep = "")
    
    # Global bindings for devtools
    x = NULL
    y = NULL
    
    pairedcurves = inner_join(c1, c2, by = join_by(x$expid.x < y$expid.y)) %>%
      filter(.data$SampleID.x >= .data$SampleID.y) %>%
      filter(!identical(.data$values.x, .data$values.y))

  }

if(!isTRUE(reference == "none")){
  reference = vascr_find_sample(data.df, reference)
  pairedcurves = pairedcurves %>% filter(.data$Sample.x %in% reference | 
                                           .data$Sample.y %in% reference |
                                           .data$Sample.y == .data$Sample.x)
  pairedcurves = pairedcurves %>% 
    mutate(x = ifelse(.data$Sample.x == reference, as.character(.data$Sample.y), as.character(.data$Sample.x))) %>%
    mutate(y = ifelse(.data$Sample.x == reference, as.character(.data$Sample.x), as.character(.data$Sample.y))) %>%
    mutate(Sample.x = .data$x, Sample.y = .data$y, x = NULL, y = NULL)

}



pairedcurves

# %>%
#   #mutate(dtw = dtw(unlist(.data$values.x), unlist(.data$values.y), open.begin = TRUE, open.end = TRUE, step=asymmetric)[["normalisedDistance"]]) %>%
#   #mutate(ndtw = dtw(scale(unlist(.data$values.x)), scale(unlist(.data$values.y)), open.begin = TRUE, open.end = TRUE, step=asymmetric)[["normalizedDistance"]])
# 
# to_export = pairedcurves %>% as.data.frame () %>% rowwise() %>% 
#   mutate(myf(values.x, values.y))

if(isTRUE(cc_only))
{

to_export = pairedcurves %>% rowwise() %>% 
   mutate(cc = ccf(unlist(.data$values.x), unlist(.data$values.y), plot = FALSE, lag.max = 0)[["acf"]][[1]]) 
}

else {

myf = function(a,b)
{
  
  tr = cc_stretch_shift_fit(unlist(a),unlist(b))
  p()
  return(tr)

}


  p = progressor(steps = nrow(pairedcurves))
  
  to_export = pairedcurves %>% as.data.frame () %>% rowwise() %>% 
    nest(data = c("values.x", "values.y")) %>%
    mutate(data = future_map(.data$data, function(df) {myf(df$values.x, df$values.y)})) %>%
    unnest("data")

}

return(to_export)

}




#' Summarise Cross Correlation Data
#'
#' @param data.df The vascr dataset to summarise
#' @param level Level at which to summarise, options are wells, experiments, summary
#'
#' @return a vascr dataset of cross correlaions
#' 
#' @importFrom dplyr group_by_at summarise ungroup mutate select distinct
#' 
#' @noRd
#'
#' @examples
#' data.df = vascr::growth.df %>% 
#'             vascr_subset(unit = "R", frequency = 4000, sampleid = c(1,4,7)) %>%
#'             vascr_cc()
#'             
#' vascr_summarise_cc(data.df, level = "experiments")
#' vascr_summarise_cc(data.df, level = "summary")
vascr_summarise_cc = function(data.df, level = "summary")
{
  
  ccf_exp = data.df %>% ungroup() %>% 
    group_by_at(vars("Sample.x", "Sample.y", "SampleID.x", "SampleID.y", "Experiment")) %>%
    summarise(cc = mean(.data$cc), n = n())
  
  distinct(ccf_exp %>% select(-"cc"))
  
  if(level == "experiments"){
    return(ccf_exp)
  }
  
  ccf_sum = ccf_exp %>% group_by(.data$`Sample.x`, .data$`Sample.y`) %>%
    summarise(ccsem = sd(.data$cc)/n(), cc = mean(.data$cc), totaln = sum(.data$n)) %>%
    mutate(title = paste(.data$`Sample.x`, .data$`Sample.y`, sep = "\n"))
  
  return(ccf_sum)
  
}


#' Plot out a cross-correlation summarized dataset
#' 
#' @param data.df a cross-correlation summarised vascr dataset
#'
#' @return A ggplot of the dataset presented to be cropped
#' 
#' @importFrom ggplot2 geom_tile geom_text facet_wrap scale_colour_manual geom_point geom_errorbar
#' @importFrom dplyr tibble 
#' @importFrom ggtext element_markdown
#' 
#' @noRd
#'
#' @examples
#' data.df = vascr::growth.df %>% 
#'             vascr_subset(unit = "R", frequency = 4000) %>%
#'             vascr_cc()
#'
#' data.df %>% vascr_plot_cc()
#' data.df %>% vascr_summarise_cc("experiments") %>% vascr_plot_cc()
#' data.df %>% vascr_summarise_cc("summary") %>% vascr_plot_cc()
#' 
vascr_plot_cc = function(data.df){

if(vascr_find_level(data.df) == "wells"){
  toreturn = data.df %>% ggplot() +
    geom_tile(aes(x = paste(.data$`Experiment`, .data$`Well.x`, sep = "\n"), y = paste(.data$`Experiment`, .data$`Well.y`, sep = "\n"), fill = .data$cc)) +
    geom_text(aes(x = paste(.data$`Experiment`, .data$`Well.x`, sep = "\n"), y = paste(.data$`Experiment`, .data$`Well.y`, sep = "\n"), label = round(.data$cc,3))) +
    facet_wrap(vars(.data$`Sample.x`, .data$`Sample.y`), scales = "free")
 
   return(toreturn)
}

if(vascr_find_level(data.df) == "experiments"){
  toreturn = data.df %>% ggplot() +
  geom_tile(aes(x = .data$`Experiment`, y = .data$`Experiment`, fill = .data$cc)) +
  facet_wrap(vars(.data$`Sample.x`, .data$`Sample.y`), scales = "free")
  
  return(toreturn)
}
  

colours = vascr_gg_color_hue(length(unique(c(data.df$`Sample.x`, data.df$`Sample.y`))))

hue = tibble(Sample = unique(c(data.df$`Sample.x`, data.df$`Sample.y`)), colours = colours)

# Setup global bindings for the x and y bindings below
x = NULL
y = NULL

toplot = data.df %>% left_join(hue, join_by(x$`Sample.x` == y$`Sample`)) %>% 
  mutate(hue1 = .data$colours, colours = NULL) %>% 
  left_join(hue, join_by(x$`Sample.y` == y$`Sample`)) %>% 
  mutate(hue2 = colours, colours = NULL) %>%
  mutate(title = glue("<span style = 'color:{hue1};'>{`Sample.x`}</span><br>
                       <span style = 'color:{hue2};'>{`Sample.y`}</span>"))


toplot %>%
  ggplot() +
  geom_point(aes(y = .data$title, x = .data$cc, color = .data$`Sample.x`)) +
  geom_errorbar(aes(y = .data$title, xmin = .data$cc -  .data$ccsem, xmax = .data$cc + .data$ccsem, color = .data$`Sample.y`)) +
  theme(axis.text.y = element_markdown()) +
  scale_colour_manual(values = colours)

}


