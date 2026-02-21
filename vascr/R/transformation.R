
#' Summarise a vascr data set down to a particular level
#'
#' @param data.df Data set to summarize
#' @param level Level to summarise to, either "median_deviation", "ANOVA", "summary", "experiment" or "wells"
#'
#' @return The summarized data set
#' 
#' @importFrom stringr str_length
#' @importFrom dplyr reframe summarise filter
#' 
#' @export
#'
#' @examples
#' rbgrowth.df = vascr_subset(growth.df, unit = "Rb")
#' 
#' vascr_summarise(rbgrowth.df, level = "summary")
#' vascr_summarise(rbgrowth.df, level = "experiment")
#' vascr_summarise(rbgrowth.df, level = "wells")
vascr_summarise = function(data.df, level = "wells")
{
  level = vascr_match(level, c("summary", "wells", "experiments", "median_deviation", "ANOVA"))
  
  
    data.df = vascr_force_resampled(data.df)

    if(level == "summary")
    {
      data.df = vascr_summarise_summary(data.df)
    }
    
    if(level == "experiments")
    {
      data.df = vascr_summarise_experiments(data.df)
    }
    
    if(level == "median_deviation")
    {
      data.df = vascr_summarise_deviation(data.df)
    }
    
    if(level == "anova"){
     vascr_make_significance_table(data.df, vascr_find(data.df, "Time")[1], vascr_find(data.df, "Unit")[1], vascr_find(data.df, "Frequency")[1], format = "Tukey_data")
    }
    
  return(data.df)
  
}


#' Summarise a vascr data set to the level of deviation
#'
#' @param data.df The data set to summarise
#'
#' @return The dataset, summarized with deviations
#' 
#' @noRd
#' 
#' @importFrom dplyr group_by
#' @importFrom stats median
#'
#' @examples
#' vascr_summarise_deviation(growth.df %>% vascr_subset(unit = "R", frequency = 4000, time = 5, well =c("A01","A02", "A03", "A04", "A05")))
#' 
vascr_summarise_deviation = function(data.df){
  
processed = data.df %>% 
  group_by(.data$Time, .data$Experiment, .data$SampleID, .data$Excluded) %>% 
  mutate(Median_Deviation = abs(.data$Value/median(.data$Value)-1)) %>%
  mutate(Median_Value = median(.data$Value)) %>%
  mutate(MAD = median(.data$Value - .data$Median_Value))

processed

}



#' Summarise a vascr dataset to the level of experiments
#'
#' @param data.df A dataset, must be at the wells level
#' @return A vascr dataset, summarised to the level of experiments
#' 
#' @importFrom dplyr n
#' 
#' @noRd
#'
#' @examples
#' vascr_summarise_experiments(rbgrowth.df)
vascr_summarise_experiments = function(data.df)
{
  
  summary_level = vascr_find_level(data.df)
  
  if(summary_level == "wells")
  {
    experiment.df = data.df %>%
      group_by(.data$Time, .data$Unit, .data$Frequency, .data$Sample, .data$Experiment, .data$Instrument, .data$SampleID, .data$Excluded) %>%
      reframe(sd = sd(.data$Value), n = n(),min = min(.data$Value), max = max(.data$Value), Well = paste0(unique(.data$Well), collapse = ","),Value = mean(.data$Value), sem = .data$sd/sqrt(.data$n))
    
    
    experiment.df = experiment.df %>% ungroup()
    
  } else if(summary_level == "experiments"){
    
    return(data.df)
    
  } else
  {
    vascr_notify("error","Requested data is less summarised than the data input, try again")
  }
  
  return(experiment.df)
}


#' Summarise a vascr dataset to the level of an overall summary
#'
#' @param data.df A vascr dataset, at either wells or experiments level
#'
#' @return A vascr dataset at overall summary level
#' 
#' @importFrom dplyr group_by reframe
#' 
#' @noRd
#'
#' @examples
#' vascr_summarise_summary(rbgrowth.df)
#' 
vascr_summarise_summary = function(data.df)
{
  
  summary_level = vascr_find_level(data.df)
  
  if(summary_level == "wells")
  {
    data.df = vascr_summarise_experiments(data.df)
    summary_level = vascr_find_level(data.df)
  }
  
  if(summary_level == "experiments")
  {
    summary.df = data.df %>%
      group_by(.data$Time, .data$Unit, .data$Frequency, .data$Sample, .data$Instrument) %>%
      reframe(sd = sd(.data$Value), totaln = sum(.data$n), n = n(), min = min(.data$Value), max = max(.data$Value), Well = paste0(unique(.data$Well), collapse = ","), 
              Value = mean(.data$Value), Experiment = "Summary", sem = .data$sd/sqrt(.data$n))
    return(summary.df)
  }
  
  else if(summary_level == "summary")
  {
    return(data.df)
  }

}



# Normalization function --------------------------------------------------

#' Normalize ECIS data to a single time point
#' 
#' This function normalises each unique experiment/well combination to it's value at the specified time. Contains options to do this either by division or subtraction. Can be run twice on the same dataset if both operations are desired.
#'
#' @param data.df Standard vascr data frame
#' @param normtime Time to normalize the data to
#' @param divide  If set to true, data will be normalized via a division. If set to false (default) data will be normalized by subtraction. Default is subtraction
#'
#' @return A standard ECIS dataset with each value normalized to the selected point.
#' 
#' @export
#' 
#' @importFrom dplyr left_join right_join filter as_tibble
#'
#' @examples
#' 
#' data = vascr_normalise(growth.df, normtime = 100)
#' head(data)
#' 
vascr_normalise = function(data.df, normtime, divide = FALSE) {
  
  if(is.null(normtime))
  {
    return(data.df %>% as_tibble())
  }
  
  data.df = vascr_force_resampled(data.df)
  
  data.df = vascr_remove_metadata(data.df)
  
  data.df = ungroup(data.df)
  
  # Create a table that contains the full data set at the time we are normalizing to
  mininormaltable = data.df %>% dplyr::filter(.data$Time == vascr_find_time(data.df, normtime))
  mininormaltable$NormValue = mininormaltable$Value
  mininormaltable$Value = NULL
  mininormaltable$NormTime = normtime
  mininormaltable$Time = NULL
  
  # Now use left_join to match this time point to every other time point.This creates a table with an additional column that everything needs to be normalised to, allowing for the actual normalization to be done via vector maths. Not the most memory efficient, but is explicit and clean.
  
  fulltable = right_join(data.df, mininormaltable, by = c("Frequency", "Well", "Unit", "Instrument", "Experiment", "Sample", "SampleID", "Excluded"))
  
  
  # Run the actual maths for each row
  
  if (divide == TRUE) {
    fulltable$Value = fulltable$Value/fulltable$NormValue
  } else {
    fulltable$Value = fulltable$Value - fulltable$NormValue
  }
  
  # Clean up temporary rows
  fulltable$NormTime = NULL
  fulltable$NormValue = NULL
  
  
  # Warn if maths errors have occoured
  if (isFALSE(all(is.finite(fulltable$Value)))) {
    vascr_notify("warning","NaN values or infinities generated in normalisation. Proceed with caution")
  }
  
  #Return the whole table
  return(fulltable %>% as_tibble())
  
}

# subsample data ---------------------------------------------------------


#' Subsample data
#' 
#' Returns a subset of the original data set that has only every nth value. Greatly increases computational preformance for a minimal loss in resolution during time course experiments.
#'
#' @param data.df An ECIS dataset
#' @param nth  An integer. Every nth value will be preserved in the subsetting
#'
#' @return Downsampled ECIS data set
#' 
#' @importFrom dplyr left_join as_tibble
#' 
#' @noRd
#'
#' @examples
#' 
#' unique(vascr_subsample(growth.df, 10)$Time)
#' 
vascr_subsample = function(data.df, nth) {
  
  Time = unique(data.df$Time)
  TimeID = c(1:length(Time))
  
  if(is.infinite(nth) || nth == 1 || length(Time)==1)
  {
    return(data.df %>% as_tibble())
  }
  
  
  time.df = data.frame(TimeID, Time)
  
  withid.df = dplyr::left_join(data.df, time.df, by = "Time")
  subset.df = subset(withid.df, (TimeID%%nth) == 1)
  
  subset.df$TimeID = NULL
  
  subset.df = as_tibble(subset.df)
  
  return(subset.df)
  
}




#' Interpolate times between two data points
#'
#' @param data.df Takes a vascr dataframe to interpolate, but may only contain one frequency and unit pair
#' @param npoints Number ofpoints to interpolate, defaults to same as submitted dataset
#' @param from Time to start interpolation at, default minimum in dataset
#' @param to Time to end interpolation at, default maximum in dataset
#' @param force_timepoint Forces a timepoint to be included in the resample
#' @param rate The rate at which to resample
#' @param include_disc Add an additional data point after a discrepancy
#' 
#' @importFrom stats approx
#' @importFrom dplyr reframe rename ungroup mutate group_by across
#'
#' @return A resampled vascr dataset
#' 
#' # Not exposed, as a component of vascr_resample_time
#' @noRd 
#' 
#' @examples
#'  data.df = growth.df %>% vascr_subset(time = c(1,10), unit = "R", frequency = 4000, well = c("D01", "D02", "D03"))
#'  vascr_interpolate_time(data.df)
#'  
#'  vascr_interpolate_time(data.df, from = 0, to = 50, rate = 5)
#'  vascr_interpolate_time(data.df, from = 0, to = 50, rate = 5, force_timepoint = 2.222)
vascr_interpolate_time = function(data.df, npoints = vascr_find_count_timepoints(data.df), from = min(data.df$Time), to = max(data.df$Time), rate = NULL, force_timepoint = NULL, include_disc = TRUE)
{
  
  if(length(unique(data.df$Frequency))>1 || length(unique(data.df$Unit))>1)
  {
    vascr_notify("error","vascr_interpolate_time only supports one unit and frequency at a time")
  }
  
  
  
  # originalsample = unique(data.df$Sample)
  
  if(!all(is.null(rate)))
  {
    i = from
    ivec = c()
    
    while(i<=to)
    {
      ivec = c(ivec, i)
      i = i + rate
    }
    
    xout = ivec
    
  } else
  {
  xout = seq(from = from, to = to, length.out = npoints)
  }
  
  if(isTRUE(include_disc)){
    discs = vascr_find_disc(data.df)
    xout = c(xout, discs$original, discs$lag) %>% sort()
    
    for(i in c(0:nrow(discs))){
      if(i != 0){
      row = discs[i,]
      xout = xout[!(row$original>xout & row$lag<xout)]
      }
    }
    
  }
  
  # Add in the forcing factor
  if(!is.null(force_timepoint)){
  xout = c(xout, force_timepoint) %>% sort()
  }
  # approx(data.df$Time, data.df$Value, method = "linear", n = npoints)
  
  processed = data.df %>% group_by(across(c(-"Value", -"Time"))) %>%
    reframe(New_Value = approx(.data$Time, .data$Value, xout = xout, rule = 2)$y, New_Time = approx(.data$Time, .data$Value, xout = xout, rule = 2)$x) %>%
    rename(Value = "New_Value", Time = "New_Time") %>%
    ungroup()
  
  return(processed)
}


#' Find discontinuities in the time of acquisition
#'
#' @param data.df The dataset to interrogate
#' @param threshold Threshold of variation to deem a discontinuity, default 0.2
#'
#' @returns A table containing the discontinuities
#' 
#' @noRd
#'
#' @examples
#' vascr_find_disc(growth.df)
#' 
#' vascr_find_disc(growth_unresampled.df)
#' 
vascr_find_disc = function(data.df, threshold = 0.2) {
  dat = data.df %>% arrange("Time") %>% 
    filter(.data$Well == data.df$Well[1], .data$Frequency == data.df$Frequency[1], .data$Unit == data.df$Unit[1]) %>% 
    vascr_subset(experiment = 1) %>% 
    ungroup()
  
  timechange = data.frame(original = unique(dat$Time), lag = dplyr::lag(dat$Time)) %>%
    mutate(diff = .data$original-.data$lag) %>%
    mutate(disc = .data$diff - mean(.data$diff, na.rm = TRUE))
  
  # mean(timechange$diff, na.rm = TRUE) + sd(timechange$diff, na.rm = TRUE)
  
  timechange %>% filter(.data$disc > threshold) %>% filter(.data$original<.data$lag)
}

#' Remove columns in the dataset, if they exist
#'
#' @param data.df Dataset to remove cols from
#' @param cols Columns to remove from the dataset
#'
#' @returns Revised dataset
#' 
#' @noRd
#'
#' @examples
#' vascr_remove_cols(growth.df, "Sample")
#' 
vascr_remove_cols = function(data.df, cols){
  
  processed.df = data.df
  
  for(col in cols){
    
    if(col %in% colnames(data.df))
    {
      processed.df[col] = NULL
    }
  }
  
  return(processed.df)
}


#' Resample a vascr dataset
#' 
#' Impedance sensing data is often not collected simultaneously, which creates issues
#' summarising and plotting the data. This function interpolates these data to allow
#' these downstream functions to happen.
#'
#' @param data.df The vascr dataset to resample
#' @param npoints Manually specificity the number of points to resample at, default is the same frequency as the input dataset
#' @param t_start Time to start at
#' @param t_end Time to end at
#' @param rate Time between timepoints
#' @param force_timepoint Force a specific timepoint to be part of the resample
#' @param include_disc Add an additional data point either side of a discrepancy. Defaults TRUE
#' 
#' @importFrom foreach foreach `%do%`
#' @importFrom dplyr group_split group_by
#'
#' @return An interpolated vascr dataset
#' 
#' @export
#'
#' @examples
#' # Automatically re sample, mimicking the input data as closely as possible
#' vascr_resample_time(growth.df)
#' 
#' # Fully controlled resample with advanced options
#' vascr_resample_time(growth.df, t_start = 5, t_end = 20, rate = 5, force = c(1,2,3))
#' 
vascr_resample_time = function(data.df, npoints = vascr_find_count_timepoints(data.df), t_start = min(data.df$Time), t_end = max(data.df$Time), rate = NULL, force_timepoint = NULL, include_disc = TRUE)
{
  datasplit = data.df %>% vascr_remove_cols(c("sd", "n", "min", "max", "sem")) %>% group_by(.data$Frequency, .data$Unit) %>% group_split() 
  
  baseline_times = npoints
  
  i = 1
  
  resampled = foreach(i = datasplit, .combine = rbind) %do%
    {
      ith = vascr_interpolate_time(i, baseline_times, t_start, t_end, rate, include_disc = include_disc, force_timepoint=force_timepoint)
      ith
    }
  
  # hist(resampled$Time)
  
  resampled = as_tibble(resampled)
  
  return(resampled)
  
}


#' Calculate the area under the curve of a trace
#'
#' @param data.df vascr data set containing a single trace
#' 
#' @importFrom dplyr lead group_by_all mutate filter
#'
#' @return The calcaulted area under the curve
#' 
#' @noRd
#'
#' @examples
#' vascr_auc(growth.df %>% vascr_subset(unit = "R", frequency = 4000, well = "A01"))
#' 
vascr_auc = function(data.df) {

      auc = data.df %>% mutate(Time2 = lead(.data$Time), Value2 = lead(.data$Value)) %>% 
                  dplyr::filter(!(is.na(.data$Time2) | is.na(.data$Value2) | is.na(.data$Value))) %>%
                  group_by_all() %>%
                  mutate(auc = abs((.data$Time2 - .data$Time) * mean(.data$Value, .data$Value2)))

      sum(auc$auc)
}




#' Plot showing the sensitivity of resampling
#'
#' @param data.df 
#'
#' @returns A plot showing sensitivity of resamping
#' 
#' @noRd
#' 
#' @importFrom cli cli_progress_cleanup cli_progress_update cli_progress_bar
#' @importFrom foreach foreach `%do%`
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 geom_line ylim geom_hline aes
#'
#' @examples
#' vascr_plot_resample_range(growth.df)
#' 
#' bigdata = vascr_import("ECIS", raw ="raw_data/growth/growth1_raw.abp", experiment = 1)
#' 
#' toprocess = bigdata %>% dplyr::mutate(Experiment = "1", Sample = "Test")
#' 
#' vascr_plot_resample_range(data.df = growth.df)
#' 
vascr_find_resample_frequency = function(data.df, unit = "R", frequency  = 4000, well = "A01", res = 50, plot = TRUE, toll = 0.999){
  
  data.df = data.df %>% vascr_subset(unit = unit, frequency = frequency, well = well)  %>%
    arrange("Time")
  
  checktimes = seq(from = vascr_find_count_timepoints(data.df)^0.5, to = 2, length.out = res)^2 %>% round() %>% unique()
  
  cli_progress_bar(total = length(checktimes))
  
    i = 0
    
    p <- progressr::progressor(along = c(1:length(checktimes)))
  
    boot = foreach (i = checktimes, .combine = rbind) %do% {
            p()
            vascr_plot_resample(data.df, plot = FALSE, newn = i)  
    }

  
  # boot %>% dplyr::filter(.data$r2 > 0.999)
  
  if(isFALSE(plot)){
    return(boot)
  }

  min_acc = boot %>% filter(.data$d_auc > toll & .data$r2 > toll & .data$ccf > toll)
  min_acc
  vascr_notify("info", glue("Recomended resampling rate: {min(min_acc$n)}"))

boot2 = boot %>%
          pivot_longer(-n, names_to = "variable", values_to = "value") %>%
          dplyr::filter(.data$value>0.9, .data$value<1.1)

ggplot(boot2) +
  geom_line(aes(x = .data$n, y = .data$value, colour = .data$variable)) +
  ylim(c(0.9,1)) +
  geom_hline(aes(yintercept = 0.995))


}





#' Plot the data re sampling process
#'
#' @param data.df Dataset to analyse
#' @param unit Unit to use, defaults to R
#' @param frequency Frequency to use, defaults to 4000
#' @param well Well to use, defaults to A01 (or first well in plate)
#' @param newn New number of timepoints to compare to current
#' @param plot Return a ggplot or the underlying data. Defaults to TRUE, returning the plot.
#' @param rug Show rug lot, defaults true
#' @param points Show points, defaults to false
#' 
#' @importFrom ggplot2 geom_rug geom_line ylim aes scale_shape_manual
#' @importFrom stats ccf
#' @importFrom dplyr filter
#'
#' @export
#'
#' @returns A plot showing how well the resampled data conforms to the actual data set
#'
#' @examples
#' vascr_plot_resample(growth.df)
#' vascr_plot_resample(growth.df, plot = FALSE)
#' 
#' 
vascr_plot_resample = function(data.df, unit = "R", frequency = "4000", well = "A01", newn = 20, plot = TRUE, rug = TRUE, points = FALSE)
      {
          base_data = data.df %>% dplyr::filter(!is.na(.data$Value))
          
          to_return = list()
          
          to_return["n"] = newn
  
          # Create resampled set
          original_data = base_data %>% vascr_subset(unit = unit, frequency = frequency, well = well) %>% arrange("Time") %>% as.data.frame()
          oldn = vascr_find_count_timepoints(original_data) 
          new_data = original_data %>% vascr_resample_time(npoints = newn) %>% arrange("Time") %>% as.data.frame()
          reverse_processed = new_data %>% vascr_resample_time(npoints = oldn) %>% arrange("Time") %>% as.data.frame()
          
          # Calculate change in ACF
          old_auc = vascr_auc(original_data)
          new_auc = vascr_auc(new_data)
          d_auc = 1-abs(old_auc-new_auc)/old_auc
          
          to_return["d_auc"] = d_auc
          
          # original_data$Time == reverse_processed$Time
          
          # diff.df = tibble(time = original_data$Time, original = original_data$Value, processed = reverse_processed$Value)
          
          # diff.df$residuals = diff.df$original - diff.df$processed
          
          
          to_return["r2"] = 1 - (mean((original_data$Value - reverse_processed$Value)^2) / (mean((original_data$Value - mean(original_data$Value))^2)))
          
          
          
          to_return["ccf"] = ccf(original_data$Value, reverse_processed$Value, lag.max = 0, plot = FALSE)[[1]] %>% as.numeric()
          
          
          if(isFALSE(plot))
          {
            return(to_return %>% as.data.frame())
          }
          
          original_data$source = "Original"
          new_data$source = "Resampled"
          reverse_processed$source = "reverse_processed"
          
          all = rbind(original_data, new_data)
          
          toplot = ggplot(all) +
            geom_line(aes(x = .data$Time, y = .data$Value, colour = .data$source, linetype = .data$source))
          
          if(isTRUE(points)) 
            {
           toplot = toplot + geom_point(aes(x = .data$Time, y = .data$Value, colour = .data$source, shape = .data$source)) +
            scale_shape_manual(values = c(1,16))
          }
          
          if(isTRUE(rug)){
            toplot = toplot + geom_rug(aes(x = .data$Time, colour = .data$source))
          }

          return(toplot)
}




#' Remove all non-core ECIS data from a data frame
#' 
#' Useful if you want to to further data manipulation, without having to worry about tracking multiple, unknown columns.
#' 
#' @param data.df An ECIS data set
#' @param subset What to strip off. Default is all, more options to come.
#' 
#' @importFrom stringr str_trim
#' @importFrom dplyr any_of select
#'
#' @return A dataset containing only the core ECIS columns
#' 
#' @noRd
#'
#' @examples
#' #growth.df$Instrument = "ECIS"
#' #exploded.df = vascr_explode(growth.df)
#' #cleaned.df = vascr_remove_metadata(exploded.df)
#' #identical(growth.df,cleaned.df)
vascr_remove_metadata = function(data.df, subset = "all")
{
  
  summary_level = vascr_find_level(data.df)
  
  if(summary_level == "summary" || summary_level == "experiments")
  {
    vascr_notify("warning","You are removing some summary statistics. These are not re-generatable using vascr_explode alone, and must be regenerated with vascr_summarise.")
  }
  
  removed.df = data.df %>% select(any_of(vascr_cols())) %>% as_tibble()
  
  return(removed.df)
}



#' Set a particular time point as 0 in a vascr dataset
#' 
#' Allows the user to change the time designated as zero to allow clearer plotting of treatments
#'
#' @param data.df A vascr dataset
#' @param time The time to be set to 0
#'
#' @returns An adjusted vascr dataset
#' 
#' @export
#'
#' @examples
#' vascr_zero_time(growth.df, 50)
#' 
vascr_zero_time = function(data.df, time = 0){
  data.df %>% mutate(Time = .data$Time - time)
}

