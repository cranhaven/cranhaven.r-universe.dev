
# Conducting statistical tests --------------------------------------------


#' Make a data frame of what is statistically significant
#' 
#' @param data.df The data frame to analyse
#' @param time The time to analyse
#' @param unit The unit to analyse
#' @param frequency The frequency to analyse
#' @param confidence The confidence level to analyse - default is 0.95
#' @param format The format to return the data frame in
#' 
#' @importFrom stats aov lm TukeyHSD symnum
#' @importFrom tidyr separate
#' @importFrom stringr str_c str_replace
#' @importFrom rstatix tukey_hsd
#' @importFrom dplyr filter "%>%"
#'
#' @return A table of what is significant
#' 
#' @noRd
#'
#' @examples
#' vascr_make_significance_table(data.df = growth.df, time = 50, unit = "R", frequency = 4000, confidence = 0.95)
#' vascr_make_significance_table(growth.df, 50, "R", 4000, 0.95, format = "Tukey_data")
#' 
vascr_make_significance_table = function(data.df, time, unit, frequency, confidence = 0.95, format = "toplot")
{
  
  if(!(vascr_find_normalised(data.df)==FALSE))
  {
    vascr_notify("warning","Normalised dataset detected, ANOVA results may be invalid")
  }
  
  data.df = vascr_subset(data.df, unit = unit, time = time, frequency = frequency)
  
  data.df$Sample = str_replace(data.df$Sample, "-", "~")
  data.df$Sample = str_replace_all(data.df$Sample, "[\\+]", "x")
  
  data.df = ungroup(data.df)
  
  # What is the effect of the treatment on the value ?
  lm = vascr_lm(data.df, unit, frequency, time)
  
  data.df$Sample = factor(data.df$Sample, unique(data.df$Sample))
  # ANOVA = Anova(lm, type = "III")
  # print(ANOVA)
  
  
  # # Tukey test to study each pair of treatment :
  # tukeyanova = aov(lm)
  # TUKEY <- TukeyHSD(x=tukeyanova)
  # 
  tukey = tukey_hsd(lm)
  
  if(format == "Tukey_data")
  {
    return(tukey)
  }
  
  tukey %>%
    mutate(temp = .data$group1, group1 = .data$group2, group2 = .data$temp, temp = NULL) %>%
    rbind(tukey) %>%
    dplyr:: filter(.data$term == "Sample") %>%
    dplyr:: filter(.data$p.adj.signif != "ns") %>%
    mutate(Sample = .data$group1, group1 = NULL) %>%
    group_by(.data$Sample) %>%
    summarise(Label = paste(.data$group2, .data$p.adj.signif, collapse = "\n"))
  
  # labeltable
  
  # # Extract labels and factor levels from Tukey post-hoc 
  # Tukey.levels <- TUKEY[[2]][] # pull out the tukey significance levels
  # Tukey.labels <- data.frame(Tukey.levels)
  # 
  # Tukey.labels$Samplepair = rownames(Tukey.labels)
  # 
  # Tukey.labels = Tukey.labels %>% separate("Samplepair", c("A", "B"), sep = "-")
  # 
  # Tukey.labels$Tukey.level = Tukey.labels$p.adj
  # 
  # Tukey.labels$Significance <- symnum(Tukey.labels$Tukey.level, corr = FALSE, na = FALSE, 
  #                                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                                     symbols = c("***", "**", "*", ".", " "))
  # 
  # if (format == "Tukey_data")
  # {
  #   
  #   return(Tukey.labels)
  # }
  # else if (format == "toplot")
  # {
  #   
  #   #Generate a list of all the row names
  #   alllabels = c(Tukey.labels$A, Tukey.labels$B)
  #   alllabels = unique(data.df$Sample) %>% as.character()
  #   
  #   # Reformat for graphics
  #   # Tukey.labels = subset(Tukey.labels, Tukey.levels<(1-confidence))
  #   
  #   sources = c()
  #   sinks = c()
  #   
  #   Tukey.labels$Asig = paste(Tukey.labels$A, Tukey.labels$Significance)
  #   Tukey.labels$Bsig = paste(Tukey.labels$B, Tukey.labels$Significance)
  #   
  #   for(label in alllabels)
  #   {
  #     sink1 = filter(Tukey.labels, .data$B == label) %>%
  #                 filter(Significance != "") %>%
  #                 mutate(samp = .data$A, lab = .data$Asig, source = .data$B) %>% 
  #                select("samp", "lab", "source")
  #     sink2 = filter(Tukey.labels, .data$A == label) %>% 
  #                 mutate(samp = .data$B, lab = .data$Bsig, source = .data$A) %>% 
  #                 select("samp", "lab", "source")
  #     sink = rbind(sink1, sink2)
  #     sink = sink %>% mutate(samp = factor(.data$samp, alllabels)) %>% arrange("samp") %>% mutate(samp = as.character(.data$samp))
  #     sinktext = str_c("",sink$lab, collapse = "\n")
  #     sources = append(sources, unique(sink$source))
  #     sinks = append(sinks, sinktext)
  #     
  #   }
  #   
  #   labeltable = data.frame("Sample" = alllabels, "Label" = sinks)
  #   
  #   labeltable
  #   
  #   return(labeltable)
  # }
  # else
  # {
  #   vascr_notify("warning","Unknown output format. Check and try again")
  # }
  # 
}


#' Generate a linear model of vascr data
#'
#' @param data.df the dataset to use
#' @param unit Unit to select
#' @param frequency Frequency to select
#' @param time Time to select
#' @param priority override the default vascr priority list
#'
#' @return A linear model object
#' 
#' @noRd
#' 
#' @importFrom stats lm
#'
#' @examples
#' vascr_lm(data.df = growth.df, unit = "R", frequency = 4000, time = 100)
#' 
vascr_lm = function(data.df, unit, frequency, time)
{
  data.df = vascr_subset(data.df, unit = unit, frequency = frequency, time = time) %>% 
    vascr_summarise(level = "experiments")
  
  
  formula = "Value ~ Experiment + Sample"
# print(formula)
  
  fit <- lm(formula, data = data.df)
  return(fit)
}

#' Generate the residuals from a vascr linear model
#'
#' @param data.df the dataset to model
#' @param unit the unit to generate data from
#' @param frequency Frequency to model
#' @param time Time point to model
#' @param priority Priority to model
#'
#' @return A data frame of residuals
#' 
#' @importFrom stats residuals
#' 
#' @noRd
#'
#' @examples
#' 
#' vascr_residuals(growth.df, "R", "4000", 100)
#' 
vascr_residuals = function(data.df, unit, frequency, time)
{
  model = vascr_lm(data.df, unit, frequency, time)
  aov_residuals <- residuals(object = model)
  return(aov_residuals)
}

#' Plot a shapiro test
#'
#' @param data.df vascr dataset to analyse
#' @param unit Unit to plot
#' @param frequency Frequency to plot
#' @param time Time to plot
#' @param priority Vascr priority list
#'
#' @return A Shapiro test of the selected data
#' @noRd
#'
#' @examples
#' vascr_shapiro(growth.df, "R", 4000, 100)
#' 
vascr_shapiro = function(data.df, unit, frequency, time)
{
  aov_residuals = vascr_residuals(data.df, unit, frequency, time)
  shapirotest = shapiro.test(aov_residuals)
  return(shapirotest)
}

#' Run a Levene's test of normailty on a vascr dataset
#' 
#' Runs a test for homogenicity of variance, to ensure that the conditions of an ANOVA are met. Built into anova plotting functions and ANOVA summary.
#'
#' @param data.df vascr dataset to analyse
#' @param unit unit to plot
#' @param frequency frequency to plot
#' @param time time to plot
#' @param priority vascr priority list
#' 
#' @importFrom rstatix levene_test
#'
#' @return A Levene Test object
#' 
#' @noRd
#'
#' @examples
#' vascr_levene(growth.df, "R", 4000, 100)
#' 
vascr_levene = function(data.df, unit, frequency, time)
{
  data_s.df = vascr_subset(data.df, unit = unit, frequency = frequency, time = time) %>% 
    vascr_summarise(level = "experiments") %>% ungroup() %>%
    mutate(Sample = as.factor(.data$Sample))
  
  levenetest = levene_test(data_s.df, Value ~ Sample)
  levenetest
  
  return(levenetest)
}



# Plotting of statistical tests -------------------------------------------



#' Generate a qq plot and shapiro test from a vascr data frame
#'
#' @param data.df vascr dataset to use
#' @param unit Unit to return
#' @param frequency Frequency to return
#' @param time Timepoint to use
#' @param priority vascr priority list for analysis, if blank default will be used
#' 
#' @importFrom ggpubr ggqqplot
#' @importFrom stats shapiro.test
#' @importFrom ggplot2 labs
#'
#' @return A ggpubr ggqqplot object
#' @noRd
#'
#' @examples
#' # vascr_plot_qq(growth.df, "R", 4000, 100)
#' 
vascr_plot_qq = function(data.df, unit, frequency, time)
{
  aov_residuals = vascr_residuals(data.df, unit, frequency, time)
  
  qqplot = ggqqplot(aov_residuals)
  
  shapirotest = vascr_shapiro(data.df, unit, frequency, time)
  shapirow = round(shapirotest$statistic[[1]],3)
  shapirop = round(shapirotest$p,3)
  
  if(shapirop>0.05)
  {
    passes = "Pass"
  }else
  {
    passes = "Fail"
  }
  
  qqplot = qqplot + labs(title = "C) Normality test", subtitle = paste("Shapiro-Wilk, W=", shapirow, ", P=", shapirop, ",", passes)) +
    labs(x = "Theoretical quantiles", y = "Sample quantiles")
  
  qqplot
  
  return(qqplot)
}




#' Plot rediduals overlaid with a normal curve
#'
#' @param data.df The dataset to plot
#' @param unit  Unit to plot
#' @param frequency Frequency to plot
#' @param time Time to plot
#' @param priority Vascr priority list, will use the default if available
#' 
#' @importFrom ggplot2 stat_function ggplot geom_histogram aes after_stat
#' @importFrom stats dnorm sd
#'
#' @return a ggplot with rediduals overlaid by a normal curve
#' @noRd
#'
#' @examples
#' 
#' vascr_plot_normality(growth.df, "R", 4000, 100)
#' 
vascr_plot_normality = function(data.df, unit, frequency, time)
{
  data = vascr_subset(data.df, unit = unit, frequency = frequency, time = time)
  aov_residuals = vascr_residuals(data, unit, frequency, time)
  
  filtered.df = data %>% vascr_summarise(level = "experiments")
  filtered.df$residuals <- aov_residuals
  filteredplot.df = filtered.df
  filteredplot.df$Value = filtered.df$residuals
  
  normaloverlayplot = ggplot(filteredplot.df, aes(x = .data$Value)) + 
    geom_histogram(aes(y =after_stat(.data$density)),
                   colour = "black",
                   bins = 10,
                   fill = "white") +
    stat_function(fun = dnorm, args = list(mean = mean(filteredplot.df$Value), sd = sd(filteredplot.df$Value))) + labs(title = "D) Normality of residuals check", subtitle = "Normal curve and histogram should align", y="Density")
  
  normaloverlayplot
  return(normaloverlayplot)
}



#' Plot the results of a Levene's test with a fitted variables and residuals plot
#'
#' @param data.df a vascr dataset
#' @param unit Unit to analyse
#' @param frequency Frequency to analyse
#' @param time Time to analyse
#' @param priority A vascr list of priorities. If left blank default will be used.
#' 
#' @importFrom ggplot2 ggplot xlab ylab labs stat_smooth geom_hline geom_point
#'
#' @return A ggplot of a Levene's test and the underlying data analysed
#' @noRd
#'
#' @examples
#' vascr_plot_levene(growth.df, "R", 4000, 100)
#' 
#' 
vascr_plot_levene = function(data.df, unit, frequency, time)
{
  
  levenetest = vascr_levene(data.df, unit, frequency, time)
  
  f = round(levenetest$statistic[1],3)
  p = round(levenetest$p[1],3)
  
  if(p>0.05)
  {
    pass = "Pass"
  }else
  {
    pass = "Fail"
  }
  
  fit = vascr_lm(data.df, unit, frequency, time)
  
  p1<-ggplot(fit, aes(fit$fitted.values, fit$residuals))+geom_point()
  p1<-p1+stat_smooth(method="loess", formula = 'y ~ x')+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1 = p1 + labs(title = "E) Homogeneity of Variances test",
                 subtitle = paste("Levene's Test, F=", f, ", P=", p, ",", pass))
  
  return(p1)
}

#' Plot a line graph with a vertical line on it
#'
#' @param data.df A vascr dataset
#' @param unit Unit to plot
#' @param frequency Frequency to plot
#' @param time Time to plot
#' @param ... Other arguments to be passed on to vascr_plot_line
#'
#' @importFrom ggplot2 geom_vline geom_line
#'
#' @return A ggplot2 object
#' @noRd
#'
#' @examples
#' # vascr_plot_time_vline(growth.df, "R", 4000, 100)
#' 
vascr_plot_time_vline = function(data.df, unit, frequency, time)
{
  
  # Round the number given to the function to the nearest actual measurement
  timetouse = vascr_find_single_time(data.df, time)
  
  
  dataset = data.df %>% vascr_subset(unit = unit, frequency = frequency) %>%
    vascr_summarise("summary")
  timeplot = vascr_plot_line(dataset)
  
  timeplot
  
  timeplot = timeplot + geom_vline(xintercept = timetouse, color = "blue")
  timeplot = timeplot + labs(title = "A) Timepoint selected")
  
  return(timeplot)
}


#' Plot replicate data sets as a box plot
#'
#' @param data.df The dataset to plot
#' @param unit Unit to plot
#' @param frequency Frequency to plot
#' @param time Time to plot
#' 
#' @importFrom ggplot2 ggplot aes geom_boxplot labs aes_string element_text
#' @importFrom stringr str_replace_all
#'
#' @return A ggplot2 box plot of replicate experiments
#' 
#' @noRd
#'
#' @examples
#'vascr_plot_box_replicate(growth.df, "R", 4000, 100)
#' 
vascr_plot_box_replicate = function(data.df, unit, frequency, time)
{
  
  data = vascr_subset(data.df, unit = unit, frequency = frequency, time = vascr_find_time(data.df, time))
  
  data$Sample = str_replace_all(data$Sample, "\\+", "\\\n")
  
  overallplot <- ggplot(data, aes(x=.data$Sample, y=.data$Value, color = .data$Experiment)) + 
    geom_boxplot() + labs(title = "B) Replicate data") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    labs(y = vascr_titles(unique(data$Unit), unique(data$Frequency)))
  
  overallplot
  
  return(overallplot)
}



#' Generate a Tukey analysis of vascr data
#'
#' @param data.df The dataset to analyse
#' @param unit Unit to analyse
#' @param frequency Frequency to analyse
#' @param time Time to analyse
#' @param raw If true, a non-processed form of the Tukey results will be returned
#' 
#' @importFrom dplyr arrange
#'
#' @return A table or Tukey HSD test result
#' 
#' @noRd
#'
#' @examples
#' vascr_tukey(growth.df, "R", 4000, 100)
#' vascr_tukey(growth.df, "R", 4000, 100, raw = TRUE)
#' 
vascr_tukey = function(data.df, unit, frequency, time, raw = FALSE)
{
  if(raw)
  {
    sigtable = vascr_make_significance_table(data.df, time, unit, frequency, 1, format = "Tukey_data")
  }
  else
  {
    sigtable = vascr_make_significance_table(data.df, time, unit, frequency, 1, format = "Tukey_data")
    sigtable = arrange(sigtable, "Tukey.levels")
  }

  return(sigtable)
}


#' Create a grid of ANOVA significance
#' 
#' This function presents the significance of each pair of treatments in an ANOVA
#' dataset as a heatmap
#'
#' @param data.df vascr dataset to summarise
#' @param unit the unit to summarise
#' @param frequency frequency to summarise
#' @param time time at which to run the experiment
#' 
#' @importFrom dplyr mutate 
#' @importFrom ggplot2 ggplot geom_tile labs theme scale_fill_manual
#' @importFrom ggtext element_markdown
#'
#' @return a ggplot heatmap
#' 
#' @noRd
#'
#' @examples
#' vascr_plot_anova_grid(growth.df, "R", 4000, 100)
#' 
vascr_plot_anova_grid = function (data.df, unit =  "R", frequency = 4000, time = 100, rotate = 90, separate = " x ")
{
  
  ggplotColours <- function(n = 6, h = c(0, 360) + 15){
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
  }
  
  pal = ggplotColours(8)
  
  sigdata = vascr_make_significance_table(data.df, unit = unit, frequency = frequency, time = time, format = "Tukey_data")
  
  sigdata  = sigdata %>% mutate(group1 = str_replace(.data$group1, "x", "<br>")) %>% 
                  mutate(group2 = str_replace(.data$group2, "x", "<br>"))
  
  sigdata
  
  sigplot = sigdata %>% filter(.data$term == "Sample") %>%
    ggplot() +
    geom_tile(aes(x = .data$group1, y = .data$group2, fill = .data$p.adj.signif)) +
    labs(fill = "P value",
         x = "Treatment 1", y = "Treatment 2") +
    scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
    theme(axis.text.x = element_markdown(angle = 0, vjust = 0.5, hjust=0.5)) +
    theme(axis.text.y = element_markdown(angle = 90, vjust = 0.5, hjust=0.5)) +
    scale_fill_manual(values = c(pal[[5]], pal[[2]], pal[[3]], pal[[4]], pal[[5]]),
                      labels = c("< 0.01", "<0.05", "< 0.1", "<1"),
                      drop = FALSE)
  
  sigplot
  
  return(sigplot)
}


#' Run ANOVA and Dunnett's comparisons on a vascr dataset
#'
#' @param data.df A vascr dataset
#' @param unit  The unit to plot
#' @param frequency The frequency to plot
#' @param time  The time to plot
#' @param reference Reference sample to compare against. If all comparisons are needed use vascr_anova
#' 
#' @importFrom multcomp mcp glht contrMat adjusted
#' @importFrom rstatix add_significance
#' @importFrom nlme gls varIdent
#' @importFrom dplyr filter
#'
#' @returns A table with the results of the Dunnett's test
#' 
#' @export
#'
#' @examples
#' vascr_dunnett(data.df = growth.df, unit = "R", frequency = 4000, time = 100, reference = 6)
#' vascr_dunnett(growth.df, "R", 4000, time = list(50, 100), 6)
#' 
vascr_dunnett = function(data.df, unit, frequency, time, reference){
  
  # reference = "20,000 hCMVEC cells"
  
  data2.df = vascr_subset(data.df, time = time, unit = unit, frequency = frequency)
  
  rl =  data2.df %>% vascr_summarise(level = "experiments") %>% mutate(Time = as.factor(.data$Time))
 
   all_times = foreach(t = time, .combine = rbind) %do% {

         subset_internal.df = rl %>% mutate(Time = as.numeric(as.character(.data$Time))) %>% vascr_subset(time = t) 
         
         if(!is.factor(subset_internal.df$Sample))
         {
           subset_internal.df = subset_internal.df  %>% mutate(Sample = factor(.data$Sample, levels = unique(.data$Sample)))
         }

          fit <- lm(Value ~ Sample + Experiment, data=subset_internal.df)


          gmod = multcomp::glht(fit , linfct = multcomp::mcp(Sample = "Dunnett"))

          summ = summary(gmod, test = adjusted("none"))
  
       tr1 =  tibble(
     "Time_Sample" = summ$test$sigma %>% names(),
     "P" = summ$test$pvalues %>% as.vector(),
     Time = unique(subset_internal.df$Time))

       return(tr1)
  }
  
 
 all_times
 
 all_times$padj =p.adjust(all_times$P, method = "bonferroni")
 
 all_times
 
 toreturn = all_times %>% mutate(Sample = .data$Time_Sample, Time_Sample = NULL)  %>%
       rstatix::add_significance(p.col = "P", output.col = "Label") %>%
       mutate(P_round = round(.data$padj, 3)) %>%
        mutate(P_round =  ifelse(.data$P>0.05, "", .data$P_round))%>%
        mutate(P_round =  ifelse(.data$P_round == "0", "< 0.001", .data$P_round)) %>%
        separate("Sample", into = c("a","b"), sep = " - ", remove = FALSE) %>%
        mutate(Sample = .data$a, a = NULL, b = NULL) %>%
        mutate(Time = as.numeric(.data$Time)) 

 # rawsum = data2.df %>% vascr_normalise(normtime) %>% vascr_summarise(level = "summary") %>% mutate(Time = round(.data$Time,2) %>% as.numeric())
 
 means = data.df %>% vascr_subset(unit = unit, frequency = frequency) %>% 
   vascr_summarise(level = "summary") %>% 
   mutate(Time = as.factor(.data$Time)) %>% 
   mutate(Time = as.character(.data$Time))
 
 toreturn = toreturn %>% mutate(Time = round(.data$Time,4) %>% as.numeric())
 means_small = means %>% mutate(Time = round(as.numeric(.data$Time),4) %>% as.numeric()) %>% vascr_subset(time = time)
 
 tr = left_join(means_small, toreturn, by = c("Sample", "Time")) %>%
   mutate(Label = ifelse(.data$Sample == reference, "+", .data$Label)) %>%
   mutate(P_round = ifelse(.data$Sample == reference, "+", .data$padj)) %>%
   mutate(P = ifelse(.data$Sample == reference, "+", .data$P))
 
 tr$Label
 
 tr

 return(tr)
 
}

#' Plot a bar plot with Dunnett's test
#'
#' @param data.df a vascr dataframe
#' @param unit the unit to use
#' @param frequency the frequency to use
#' @param time the time to use for the bar plot and ANOVA
#' @param reference SampleID of the sample to use as the reference for statistical analysis
#' @param stars Show stars, or rounded P values
#' 
#' @importFrom ggplot2 geom_col ggplot geom_text geom_errorbar aes
#' 
#' @returns A bar plot of data with Dunnett's comparisons
#' 
#' @noRd
#'
#' @examples
#' vascr_plot_bar_dunnett(growth.df, "R", 4000, 50, "0_cells + hCMEC/d3_line")
#' 
vascr_plot_bar_dunnett = function(data.df, unit, frequency, time, reference, stars = TRUE)
{
  toplot = vascr_dunnett(data.df, unit, frequency, time, reference)
  
  if(isTRUE(stars))
  {
  ggplot(toplot) +
    ggplot2::geom_col(aes(x = .data$Sample, y = .data$Value)) +
    geom_errorbar(aes(x = .data$Sample, ymin = .data$Value - .data$sem, ymax = .data$Value + .data$sem)) +
    geom_text_repel(aes(x = .data$Sample, label = .data$Label, y = .data$Value + .data$sem), direction = "y", nudge_y = 1, seed = 5)
  }
  else
  {
    ggplot(toplot) +
      ggplot2::geom_col(aes(x = .data$Sample, y = .data$Value)) +
      geom_text(aes(x = .data$Sample, label = .data$P_round), y = min(toplot$Value)/2) +
      geom_errorbar(aes(x = .data$Sample, ymin = .data$Value - .data$sem, ymax = .data$Value + .data$sem))
  }
  
  
}



#' Create a line plot with Dunnett's statistics
#'
#' @param data.df A vascr dataset
#' @param unit Unit to calculate
#' @param frequency Frequency to calculate from
#' @param time Time to calculate
#' @param reference Sample to reference testing against
#' @param normtime Time to normalise the line plot to, note this does not affect underlying statistical test
#'
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr filter
#' @importFrom stringr str_replace_na
#'
#' @return A line plot, annotated with the P-values determined by Dunnett's test
#' @export
#'
#' @examples
#' vascr_plot_line_dunnett(small_growth.df, unit = "R", frequency = 4000, time = 25, 
#'     reference = "0_cells + HCMEC D3_Line")
#' vascr_plot_line_dunnett(small_growth.df, unit = "R", frequency = 4000, time = list(25,100), 
#'     reference = "0_cells + HCMEC D3_Line")
#' vascr_plot_line_dunnett(small_growth.df, unit = "R", frequency = 4000, time = 180, 
#'     reference = "20,000_cells + HCMEC D3_Line")
#' 
vascr_plot_line_dunnett = function(data.df, unit = "R", frequency = 4000, time = 100, reference = "0_cells + HCMEC D3_Line", normtime = NULL)
{
  
  dun.df = vascr_dunnett(data.df, unit = unit, frequency = frequency, time = time, reference = reference) %>%
    mutate(Time = as.numeric(.data$Time))
  
  subset.df = data.df %>% 
    vascr_subset(unit = unit, frequency = frequency) %>% 
    vascr_normalise(normtime) %>% 
    vascr_summarise(level = "summary") 
  
  dun_norm = dun.df %>% select(-"Value") %>% 
              left_join(subset.df, by = join_by("Time", "Unit", "Frequency", "Sample", "Instrument", "Well", "Experiment")) %>%
              select("Time", "Sample", "Value", "Label") %>%
              mutate(Label = str_replace_na(.data$Label, "+"))
  
  dun_norm
  
  # dun.df$Label
  
  # plab = dun.df %>%
  #   dplyr::filter(!.data$Label == "NA") %>%
  #   dplyr::filter(!.data$Label == "ns") %>%
  #   mutate(Label = paste(" ", .data$Label)) # %>%
  #   # mutate(Label = str_replace_all(Label, "\\*", "✱")) #%>%
  #   #mutate(Label = str_replace_all(Label, "+", "➕")
  # 
  # plab
  # 
  
  plot1 = subset.df %>% vascr_plot_line(alpha = 0.2)
  
  plot1
  
  plot2 = plot1 + geom_vline(xintercept = unique(dun_norm$Time), alpha=  0.8, linetype = 4) +
              geom_text_repel(aes(x = .data$Time, y = .data$Value, label = .data$Label, 
                        group = 1, hjust = 0, color = .data$Sample), alpha = 1, data = dun_norm, 
                        show.legend = FALSE, direction = "y", box.padding = 0.01, seed = 10)
  
  plot2
  
  return(plot2)

}



#' Plot a bar chart with ANOVA statistics superimposed on it as text
#'
#' @param data A vascr dataset
#' @param confidence The minimum confidence level to display
#' @param time Time point to plot
#' @param unit Unit to plot
#' @param frequency Frequency to plot
#' @param format Statistics format to return
#' @param error The style of eror bars to plot
#' @param rotate_x_angle How far to rotate the x angle
#'
#' @return A vascr bar plot with statistics attached to it
#' 
#' @noRd
#' 
#' @importFrom ggplot2 geom_errorbar aes ggplot geom_text geom_bar geom_label scale_y_discrete
#' @importFrom dplyr arrange
#'
#' @examples
#' vascr_plot_bar(data = growth.df, confidence = 0.95, unit = "R", time = 100, 
#'   frequency = 4000)
#' vascr_plot_bar_anova(data = growth.df, confidence = 0.95, unit = "R", 
#'   time = 100, frequency = 4000)
#'
#'vascr_plot_bar_anova(data = growth.df, confidence = 0.95, unit = "R", 
#'   time = 100, frequency = 4000, rotate_x_angle = 45)
#'   
#'vascr_plot_bar_anova(data = growth.df, confidence = 0.95, unit = "R", 
#'   time = 100, frequency = 4000, rotate_x_angle = 90)
#' 
vascr_plot_bar_anova = function(data.df , confidence = 0.95, time, unit, frequency, format = "toplot", error = Inf, breaks = "x", rotate_x_angle = 45)
{
  
  data.df$Sample = str_replace_all(data.df$Sample, "[\\+]", "x")
  
  # Gather graph data based on the ...
  datum = vascr_subset(data.df, unit = unit, frequency = frequency, time = time)
  
  # if(!length(unique(c(data$Time, data$Unit, data$Frequency, data$Instrument)))==4)
  # {
  #   vascr_notify("error","vascr_plot_bar_anova only supports a single time, unit, frequency and instrument at the moment. Please manually create an ANOVA if you need to ask other  statistical questions.")
  # }
  
  # Add structure checks in here

  
  summary = vascr_subset(data.df, frequency = frequency, time = time, unit = unit) %>%
    vascr_summarise(level = "summary")
  
  datum = arrange(datum, .data$Sample)
  
  labeltable = vascr_make_significance_table(data.df = datum, time, unit, frequency, confidence, format = "toplot")
  
  summary$Sample = as.factor(summary$Sample)
  labeltable$Sample = as.factor(labeltable$Sample)
  
  filtered2.df = left_join(summary, labeltable, by = "Sample") %>%
    mutate(Sample = str_replace(.data$Sample, " x ", " + <br>")) %>%
    mutate(Label = str_replace_all(.data$Label, " x ", "\\\n"))
  
  # hjust_needed = sin(rotate_x_angle*pi/180)
  
  plot = ggplot(filtered2.df, aes(x = .data$Sample, y = .data$Value, label = .data$Label, fill = .data$Sample)) + 
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymax = .data$Value + .data$sem, ymin = .data$Value-.data$sem, x = .data$Sample), width = 0.7) +
    geom_label(aes(label=.data$Label, y = min(.data$Value)/2))  +
    theme(axis.text.x = element_markdown(angle = 0, vjust = 1))
    
  
  plot
  
  
  if(error>0)
  { 
    plot = plot + geom_errorbar(aes(ymax = .data$Value + .data$sem, ymin = .data$Value - .data$sem), width = 0.6)
  }
  
  plot
  
  return(plot)
}





#' Make a display with all the ANOVA analysis pre-conducted
#'
#' @param data.df vascr dataset to plot
#' @param unit unit to plot
#' @param frequency frequency to plot
#' @param time timepoint to plot at
#' @param reference Sample to reference post-hoc analysis to
#' @param separate Value to use when separating comparasons in the output (default x)
#' @param rotate degrees of rotation used for labeling the X axis
#'
#' @return A matrix of different ANOVA tests
#' 
#' @importFrom patchwork free plot_layout
#' @importFrom ggtext element_markdown
#' @importFrom stringr str_replace_all
#' @importFrom ggplot2 scale_color_manual guides labs theme guide_legend
#' @importFrom dplyr filter
#' 
#' @export
#'
#' @examples
#' \donttest{
#' # Run, comparing only to a reference
#' vascr_plot_anova(data.df = small_growth.df, unit = "R", frequency = 4000, time = 100, 
#'           reference = "5,000_cells + HCMEC D3_line")
#' }
vascr_plot_anova = function(data.df, unit, frequency, time, reference = NULL, separate = "x", rotate = 45)
{
  data.df = vascr_force_resampled(data.df)
  
  unit = vascr_find_unit(data.df, unit)
  frequency = vascr_find_frequency(data.df, frequency)
  time = vascr_find_time(data.df, time)
  
  if(isTRUE(reference == "all"))
  {
    reference = NULL
  }
  
  timeplot = vascr_plot_time_vline(data.df, unit, frequency, time) + labs(y = "Resistance  
                                                                                  (ohm, 4000 Hz)")
  
  overallplot = vascr_plot_box_replicate(data.df, unit, frequency, time) + labs(y = "Resistance  
                                                                                  (ohm, 4000 Hz)") + 
  
  scale_color_manual(values=c("orange", "blue", "green", "purple", "red", "brown", "grey", "turquoise", "violet"))
  
  qqplot = vascr_plot_qq(data.df, unit, frequency, time)
  normaloverlayplot = vascr_plot_normality(data.df, unit, frequency, time)
  leveneplot = vascr_plot_levene(data.df, unit, frequency, time)
  
  
  tile = vascr_plot_anova_grid(data.df, unit, frequency, time, separate, rotate)  + labs(title = "F) P values") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    theme(legend.position = "bottom") 
   
  if(is.null(reference) | isTRUE(reference == "none"))
  {
    anova_results = vascr_plot_bar_anova(data.df, unit = unit, time = time, frequency = frequency)
  } else {
    anova_results = vascr_plot_bar_dunnett(data.df, unit = unit, time = time, frequency = frequency, reference = reference)
  }
  
  differences =  anova_results +
    theme(legend.position = "none") + labs(title = "G) ANOVA results", y = "Resistance   
                                                    (ohm, 4000 Hz)") +
    theme(legend.position = "none")
  
  
  design <- "
  111222
  334455
  667777"
  
  grid = free(timeplot) + free(overallplot)+ free(qqplot) + free(normaloverlayplot) + free(leveneplot) + free(tile) + free(differences) + plot_layout(design = design)
  
  
  return(grid)
}



