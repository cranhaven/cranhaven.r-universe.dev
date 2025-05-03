#'Plots the observed weekly means and climatology of a timeseries data
#' 
#'@description This function plots the observed weekly means and climatology of 
#'a timeseries data using ggplot package. It compares the weekly climatology in 
#'a specified period (reference period) to the observed conditions during the 
#'target period analyzed in the case study.
#' 
#'@param data A multidimensional array with named dimensions with at least sdate 
#'  and time dimensions containing observed daily data. It can also be a 
#'  dataframe with computed percentiles as input for ggplot. If it's a 
#'  dataframe, it must contain the following column names: 'week', 'clim', 
#'  'p10', 'p90', 'p33', 'p66', 'week_mean', 'day' and 'data'.
#'@param first_date The first date of the observed values of timeseries. It can 
#'  be of class 'Date', 'POSIXct' or a character string in the format 
#'  'yyyy-mm-dd'. If parameter 'data_years' is not provided, it must be a date
#'  included in the reference period.
#'@param last_date Optional parameter indicating the last date of the target 
#'  period of the daily timeseries. It can be of class 'Date', 'POSIXct' or a 
#'  character string in the format 'yyyy-mm-dd'. If it is NULL, the last date of 
#'  the daily timeseries will be set as the last date of 'data'. As the data is 
#'  plotted by weeks, only full groups of 7 days will be plotted. If the last 
#'  date of the timeseries is not a multiple of 7 days, the last week will 
#'  not be plotted.
#'@param ref_period A vector of numeric values indicating the years of the 
#'  reference period. If parameter 'data_years' is not specified, it must 
#'  be of the same length of dimension 'sdate_dim' of parameter 'data'.
#'@param data_years A vector of numeric values indicating the years of the 
#'  data. It must be of the same length of dimension 'sdate_dim' of parameter 
#'  'data'. It is optional, if not specified, all the years will be used as the 
#'  target period.
#'@param time_dim A character string indicating the daily time dimension name. 
#'  The default value is 'time'.
#'@param sdate_dim A character string indicating the start year dimension name. 
#'  The default value is 'sdate'.
#'@param ylim A numeric vector of length two providing limits of the scale. 
#'  Use NA to refer to the existing minimum or maximum. For more information, 
#'  see 'ggplot2' documentation of 'scale_y_continuous' parameter.
#'@param title The text for the top title of the plot. It is NULL by default.
#'@param subtitle The text for the subtitle of the plot. It is NULL bu default.
#'@param ytitle Character string to be drawn as y-axis title. It is NULL by 
#'  default.
#'@param legend A logical value indicating whether a legend should be included 
#'  in the plot. If it is TRUE or NA, the legend will be included. If it is 
#'  FALSE, the legend will not be included. It is TRUE by default.
#'@param palette A palette name from the R Color Brewer’s package. The default 
#'  value is 'Blues'.
#'@param fileout A character string indicating the file name where to save the 
#'  plot. If not specified (default) a graphics device will pop up.
#'@param device A character string indicating the device to use. Can either be 
#'  a device function (e.g. png), or one of "eps", "ps", "tex" (pictex), 
#'  "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#'@param width A numeric value of the plot width in units ("in", "cm", "mm", or 
#'  "px"). It is set to 8 by default.
#'@param height A numeric value of the plot height in units ("in", "cm", "mm", 
#'  or "px"). It is set to 6 by default.
#'@param units Units of the size of the device (file or window) to plot in. 
#'  Inches (’in’) by default.
#'@param dpi A numeric value of the plot resolution. It is set to 300 by 
#'  default.
#' 
#'@return A ggplot object containing the plot.
#' 
#'@examples
#'data <- array(rnorm(49*20*3, 274), dim = c(time = 49, sdate = 20, member = 3))
#'PlotWeeklyClim(data = data, first_date = '2002-08-09', 
#'               last_date = '2002-09-15', ref_period = 2010:2019, 
#'               data_years = 2000:2019, time_dim = 'time', sdate_dim = 'sdate',
#'               title = "Observed weekly means and climatology", 
#'               subtitle = "Target years: 2010 to 2019", 
#'               ytitle = paste0('tas', " (", "deg.C", ")"))
#' 
#'@import multiApply
#'@import lubridate
#'@import ggplot2
#'@import RColorBrewer
#'@import scales
#'@importFrom ClimProjDiags Subset
#'@importFrom s2dv MeanDims
#'@export
PlotWeeklyClim <- function(data, first_date, ref_period, last_date = NULL, 
                           data_years = NULL, time_dim = 'time', 
                           sdate_dim = 'sdate', ylim = NULL, 
                           title = NULL, subtitle = NULL, 
                           ytitle = NULL, legend = TRUE,
                           palette = "Blues", fileout = NULL, device = NULL, 
                           width = 8, height = 6, units = 'in', dpi = 300) {
  ## Check input arguments
  # data
  if (is.array(data)) {
    if (is.null(names(dim(data)))) {
      stop("Parameter 'data' must have named dimensions.")
    }
    is_array <- TRUE
  } else if (is.data.frame(data)) {
    col_names <- c("week", "clim", "p10", "p90", "p33", "p66", 
                   "week_mean", "day", "data")
    if (!all(col_names %in% names(data))) {
      stop(paste0("If parameter 'data' is a data frame, it must contain the ",
                  "following column names: 'week', 'clim', 'p10', 'p90', 'p33', ", 
                  "'p66', 'week_mean', 'day' and 'data'."))
    }
    is_array <- FALSE
  } else {
    stop("Parameter 'data' must be an array or a data frame.")
  }
  if (is_array) {
    # time_dim
    if (!is.character(time_dim)) {
      stop("Parameter 'time_dim' must be a character string.")
    }
    if (!all(time_dim %in% names(dim(data)))) {
      stop("Parameter 'time_dim' is not found in 'data' dimension.")
    }
    if (dim(data)[time_dim] < 7) {
      stop(paste0("Parameter 'data' must have the dimension 'time_dim' of ",
                  "length equal or grater than 7 to compute the weekly means."))
    }
    # sdate_dim
    if (!is.character(sdate_dim)) {
      stop("Parameter 'sdate_dim' must be a character string.")
    }
    if (!sdate_dim %in% names(dim(data))) {
      warning(paste0("Parameter 'sdate_dim' is not found in 'data' dimension. ",
                     "A dimension of length 1 has been added."))
      data <- InsertDim(data, 1, lendim = 1, name = sdate_dim)
    }
    # legend
    if (!is.logical(legend)) {
      stop("Parameter 'legend' must be a logical value.")
    }
    if (is.na(legend)) {
      legend <- TRUE
    } else if (legend) {
      legend <- NA
    }
    # ref_period (1)
    if (!is.numeric(ref_period)) {
      stop("Parameter 'ref_period' must be numeric.")
    }
    # first_date
    if ((!inherits(first_date, "POSIXct") & !inherits(first_date, "Date")) &&
        (!is.character(first_date) | nchar(first_date) != 10)) {
      stop(paste0("Parameter 'first_date' must be a character string ", 
                  "indicating the date in the format 'yyyy-mm-dd', 'POSIXct' ", 
                  "or 'Dates' class."))
    }
    first_date <- ymd(first_date)
    target_year <- year(first_date)
    taget_year_outside_reference <- FALSE
    # data_years
    if (!is.null(data_years)) {
      if (!is.numeric(data_years)) {
        stop("Parameter 'data_years' must be numeric.")
      }
      if (length(data_years) != dim(data)[sdate_dim]) {
        stop(paste0("Parameter 'data_years' must have the same length as ", 
                    "the dimension '", sdate_dim, "' of 'data'."))
      }
      if (!all(ref_period %in% data_years)) {
        stop(paste0("The 'ref_period' must be included in the 'data_years' ", 
                    "period."))
      }
      if (!any(target_year %in% data_years)) {
        stop(paste0("Parameter 'first_date' must be a date included ", 
                    "in the 'data_years' period."))
      }
      taget_year_outside_reference <- TRUE
    } else {
    # ref_period (2)
      if (length(ref_period) != dim(data)[sdate_dim]) {
        stop(paste0("Parameter 'ref_period' must have the same length as the ", 
                    "dimension '", sdate_dim ,"' of 'data' if ", 
                    "'data_years' is not provided."))
      }
      if (!any(target_year %in% ref_period)) {
        stop(paste0("If parameter 'data_years' is NULL, parameter 'first_date' ", 
                    "must be a date included in the 'ref_period' period."))
      }
      data_years <- ref_period
    }
    # last_date
    if (!is.null(last_date)) {
      if ((!inherits(last_date, "POSIXct") & !inherits(last_date, "Date")) &&
          (!is.character(last_date) | nchar(last_date) != 10)) {
        stop(paste0("Parameter 'last_date' must be a character string ", 
                    "indicating the date in the format 'yyyy-mm-dd', 'POSIXct' ", 
                    "or 'Dates' class."))
      }
      last_date <- ymd(last_date)
      dates <- seq(first_date, last_date, by = "1 day")
      if (length(dates) > dim(data)[time_dim]) {
        warning(paste0("Parameter 'last_date' is greater than the last date ",
                       "of 'data'. The last date of 'data' will be used."))
        dates <- seq(first_date, first_date + days(dim(data)[time_dim]-1), by = "1 day")
      }
    } else {
      dates <- seq(first_date, first_date + days(dim(data)[time_dim]-1), by = "1 day")
    }
    # ylim
    if (is.character(ylim)) {
      warning("Parameter 'ylim' can't be a character string, it will not be used.")
      ylim <- NULL
    }

    index_first_date <- which(dates == first_date)
    index_last_date <- length(dates) - (length(dates) %% 7)
    last_date <- dates[index_last_date]

    ## Data preparation
    # subset time_dim for weeks
    data_subset <- Subset(data, along = time_dim, 
                          indices = index_first_date:index_last_date)

    # remove other dimensions
    dims_subset <- names(dim(data_subset))[which(!names(dim(data_subset)) %in% c(time_dim, sdate_dim))]
    if (!identical(dims_subset, character(0))) {
      data_subset <- Subset(data_subset, dims_subset, as.list(rep(1, length(dims_subset))), drop = TRUE)
    }
    # observed daily data creation
    daily <- Subset(data_subset, along = sdate_dim, 
                    indices = which(data_years == target_year), 
                    drop = TRUE)
    if (taget_year_outside_reference) {
      indexes_reference_period <- which(data_years %in% ref_period)
      # remove values outside reference period for computing the means
      data_subset <- Subset(data_subset, along = sdate_dim, 
                            indices = indexes_reference_period)
    }

    ## Weekly aggregations for reference period
    weekly_aggre <- SplitDim(data_subset, split_dim = time_dim, 
                             indices = sort(rep(1:(length(index_first_date:index_last_date)/7), 7)),
                             new_dim_name = 'week')
    weekly_means <- MeanDims(weekly_aggre, time_dim)
    weekly_clim <- MeanDims(weekly_means, sdate_dim)

    weekly_p10 <- Apply(weekly_means, target_dims = sdate_dim,
                        fun = function(x) {quantile(x, 0.10)})$output1
    weekly_p90 <- Apply(weekly_means, target_dims = sdate_dim,
                        fun = function(x) {quantile(x, 0.90)})$output1
    weekly_p33 <- Apply(weekly_means, target_dims = sdate_dim,
                        fun = function(x) {quantile(x, 0.33)})$output1
    weekly_p66 <- Apply(weekly_means, target_dims = sdate_dim,
                        fun = function(x) {quantile(x, 0.66)})$output1

    clim <- p10 <- p90 <- p33 <- p66 <- NULL
    weekly_data <- data.frame(clim = as.vector(weekly_clim),
                              p10 = as.vector(weekly_p10),
                              p90 = as.vector(weekly_p90), 
                              p33 = as.vector(weekly_p33),
                              p66 = as.vector(weekly_p66),
                              week = 1:(length(index_first_date:index_last_date)/7))

    ## Prepare observations from target year
    daily_data <- data.frame(day = seq(first_date, last_date, by = "1 day"),
                             data = daily, 
                             week = sort(rep(1:(length(index_first_date:index_last_date)/7), 7)))
    week_mean <- aggregate(data ~ week, data = daily_data, mean)
    weekly_data <- cbind(weekly_data, week_mean$data)
    colnames(weekly_data)[7] <- 'week_mean'
    all <- merge(weekly_data, daily_data, by = 'week')
  } else {
    all <- data
  }

  ## Create a ggplot object
  cols <- colorRampPalette(brewer.pal(9, palette))(6)

  p <- ggplot(all, aes(x = day)) +
       geom_ribbon(aes(ymin = p10, ymax = p90, group = week, fill = "p10-p90"), 
                   alpha = 0.7, show.legend = legend) + # extremes clim
       geom_ribbon(aes(ymin = p33, ymax = p66, group = week, fill = "p33-p66"),
                   alpha = 0.7, show.legend = legend) +  # terciles clim
       geom_line(aes(y = clim, group = week, color = "climatological mean",
                 linetype = "climatological mean"),
                 alpha = 1.0, size = 0.7, show.legend = legend) + # mean clim
       geom_line(aes(y = data, color = "observed daily mean",
                 linetype = "observed daily mean"),
                 alpha = 1, size = 0.2, show.legend = legend) + # daily evolution
       geom_line(aes(y = week_mean, group = week, color = "observed weekly mean",
                 linetype = "observed weekly mean"),
                 alpha = 1, size = 0.7, show.legend = legend) + 		 # weekly evolution
       theme_bw() + ylab(ytitle) + xlab(NULL) + 
       ggtitle(title, subtitle = subtitle) +
       scale_x_date(breaks = seq(min(all$day), max(all$day), by = "7 days"),
                    minor_breaks = NULL, expand = c(0.03, 0.03),
                    labels = date_format("%d %b %Y")) +
       theme(axis.text.x = element_text(angle = 45, hjust = 1),
             panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                             colour = "gray92"),
             panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                             colour = "gray92"),
             legend.spacing = unit(-0.2, "cm")) +
       scale_fill_manual(name = NULL,
                         values = c("p10-p90" = cols[3], "p33-p66" = cols[4])) +
       scale_color_manual(name = NULL, values = c("climatological mean" = cols[5],
                                                  "observed daily mean" = "grey20",
                                                  "observed weekly mean" = "black")) +
       scale_linetype_manual(name = NULL, values = c("climatological mean" = "solid",
                                                     "observed daily mean" = "dashed",
                                                     "observed weekly mean" = "solid"),
       guide = guide_legend(override.aes = list(lwd = c(0.7, 0.2, 0.7)))) +
       guides(fill = guide_legend(order = 1)) +
       scale_y_continuous(limits = ylim)

  # Return the ggplot object
  if (is.null(fileout)) {
    return(p)
  } else {
    ggsave(filename = fileout, plot = p, device = device, height = height, 
           width = width, units = units, dpi = dpi) 
  }
}