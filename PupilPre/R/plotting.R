## SHARED CODE BEGINS HERE ##
.pac_theme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}
## SHARED CODE ENDS HERE ##


#' Plots average Pupil.
#'
#' \code{ppl_plot_avg} calculates the grand or conditional averages with
#' standard error. It then plots the results.
#' N.B.: This function will work for data with a maximum of 2 conditions.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import tidyr
#' @import ggplot2
#' @import VWPre
#'
#' @param data A data table object output after having executed
#' \code{\link{create_time_series}}.
#' @param xlim A vector of two integers specifying the limits of the x-axis.
#' @param Column A character string specifying the desired column.
#' @param Averaging A character string indicating how the averaging should
#' be done. "Event" (default) will produce the overall mean in the data, while
#' "Subject" or "Item" (or, in principle, any other column name) will
#' calculate the grand mean by that factor.
#' @param Condition1 A string containing the column name corresponding to the
#' first condition, if available.
#' @param Condition2 A string containing the column name corresponding to the
#' second condition, if available.
#' @param Cond1Labels A named character vector specifying the desired custom
#' labels of the levels of the first condition.
#' @param Cond2Labels A named character vector specifying the desired custom
#' labels of the levels of the second condition.
#' @param ErrorBar A logical indicating whether error bars should be
#' included in the plot.
#' @param ErrorBand A logical indicating whether error bands should be
#' included in the plot.
#' @param ErrorType A string indicating "SE" (Standard Error) or "CI"
#' (Confidence Interval).
#' @param ConfLev A number indicating the confidence level of the CI.
#' @param CItype A string indicating "simultaneous" or "pointwise". Simultaneous
#' performs a Bonferroni correction for the interval.
#' @param PupilPreTheme A logical indicating whether the theme included with the
#' function should be applied, or ggplot2's base theme (to which any other
#' custom theme could be added)..
#' @examples
#' # Load example data
#' data("Pupilex7")
#'
#' ppl_plot_avg(data = Pupilex7, xlim = c(0, 1900), Column = "Pupil",
#'              Condition1 = NULL, Condition2 = NULL, Cond1Labels = NA,
#'              Cond2Labels = NA, ErrorBar = TRUE, PupilPreTheme = TRUE)
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Plotting", package="PupilPre")
#'
ppl_plot_avg <- function(data, xlim = NA, Column = NULL,
                     Averaging = "Event", Condition1 = NULL,
                     Condition2 = NULL, Cond1Labels = NA,
                     Cond2Labels = NA, ErrorBar = TRUE, PupilPreTheme = TRUE,
                     ConfLev = 95, CItype = "simultaneous",
                     ErrorBand = FALSE, ErrorType = "SE") {


  type <- "elogit"
  ylabel = "Pupil dilation"
  ylim = c(-4,4)

  if(is.null(Column)){
    stop("Please supply the column to be plotted!")
  } else {
    Columns <- Column
    names(Columns) <- "Pupil"
  }

  Theme <- PupilPreTheme

  {## SHARED CODE BEGINS HERE ##

    if(!(Averaging %in% colnames(data))){
      stop(paste(Averaging, " column not present in data!"))
    }

    if(ErrorBar==TRUE && ErrorBand==TRUE){
      stop(paste("Please select either error bars OR error bands.
                 For error bars set ErrorBar=TRUE and ErrorBand=FALSE.
                 For error bands set ErrorBar=FALSE and ErrorBand=TRUE."))
    }

    # Set x-axis
    if (is.na(xlim[1])) {
      xlim <- c(range(data$Time)[1], range(data$Time)[2])
      xaxis <- unique(data$Time)
    } else {
      xlim <- xlim
      data<-data[data$Time>=xlim[1] & data$Time<=xlim[2],]
      xaxis <- unique(data$Time)
    }

    # Check condition columns
    if(!(is.null(Condition1))) {
      if(is.na(Condition1)) {
        stop(paste("Please use NULL when not specifying a condition."))
      } else if (!(Condition1 %in% colnames(data))) {
        stop(paste(Condition1, " column not present in data!"))
      }
    }
    if(!(is.null(Condition2))) {
      if(is.na(Condition2)) {
        stop(paste("Please use NULL when not specifying a condition."))
      } else if (!(Condition2 %in% colnames(data))) {
        stop(paste(Condition2, " column not present in data!"))
      }
    }

    # Inform about Grand Average calculation
    message(paste0("Grand average calculated using ", Averaging, " means."))

    # Set columns for selection
    if(is.null(Condition1) && is.null(Condition2)){
      sel_names = quos(UQ(sym(Averaging)), Time, names(Columns))
    } else if(!is.null(Condition1) && is.null(Condition2)){
      sel_names = quos(UQ(sym(Averaging)), Time, names(Columns), UQ(sym(Condition1)))
    } else if(is.null(Condition1) && !is.null(Condition2)){
      sel_names = quos(UQ(sym(Averaging)), Time, names(Columns), UQ(sym(Condition2)))
    } else if(!is.null(Condition1) && !is.null(Condition2)){
      sel_names = quos(UQ(sym(Averaging)), Time, names(Columns), UQ(sym(Condition1)), UQ(sym(Condition2)))
    }

    # Set columns for gathering
    gath_col = quos(names(Columns))

    # Set columns for grouping
    if(is.null(Condition1) && is.null(Condition2)){
      group_names1 = quos(UQ(sym(Averaging)), IA, Time)
      group_names2 = quos(IA, Time)
    } else if(!is.null(Condition1) && is.null(Condition2)){
      group_names1 = quos(UQ(sym(Averaging)), IA, Time, UQ(sym(Condition1)))
      group_names2 = quos(IA, Time, UQ(sym(Condition1)))
    } else if(is.null(Condition1) && !is.null(Condition2)){
      group_names1 = quos(UQ(sym(Averaging)), IA, Time, UQ(sym(Condition2)))
      group_names2 = quos(IA, Time, UQ(sym(Condition2)))
    } else if(!is.null(Condition1) && !is.null(Condition2)){
      group_names1 = quos(UQ(sym(Averaging)), IA, Time, UQ(sym(Condition1)), UQ(sym(Condition2)))
      group_names2 = quos(IA, Time, UQ(sym(Condition1)), UQ(sym(Condition2)))
    }

    # # Make labeller for multipanel
    # if (length(Columns) > 1) {
    #   if (!is.null(Condition1) || !is.null(Condition2)) {
    #   if(is.na(Cond1Labels) && is.na(Cond2Labels)) {
    #     my_labeller <- "label_value"
    #   } else if (is.na(Cond1Labels) && !is.na(Cond2Labels)) {
    #     my_labeller <- labeller(
    #       CustCond1 = Cond1Labels
    #       )
    #   } else if (!is.na(Cond1Labels) && is.na(Cond2Labels)) {
    #     my_labeller <- labeller(
    #       CustCond2 = Cond2Labels
    #     )
    #   }
    #     else {
    #   my_labeller <- labeller(
    #     CustCond1 = Cond1Labels,
    #     CustCond2 = Cond2Labels
    #   )
    #   }
    #   }
    # }
    my_labeller <- labeller()

    # Prepare for averaging
    Avg <- data %>% select(!!!sel_names) %>%
      tidyr::gather("IA", "VALUE", !!!gath_col, na.rm = FALSE, convert = FALSE) %>%
      group_by(!!!group_names1) %>%
      summarise(VALUE = mean(VALUE, na.rm = TRUE)) %>%
      group_by(!!!group_names2)

    # Execute calculation
    if(type == "elogit") {
      Avg <- Avg %>% summarise(mean = mean(VALUE, na.rm = TRUE), n=n(), se = stats::sd(VALUE, na.rm = TRUE) / sqrt(n()))
      if(CItype=="pointwise") {
        tval <- 1-(((100-ConfLev)/2)/100)
      } else if (CItype=="simultaneous") {
        tval <- 1-(((100-ConfLev)/(2*length(unique(xaxis))))/100)
      }
      Avg <- Avg %>% mutate(ci = stats::qt(tval,df=n-1)*se)
    } else if (type=="proportion") {
      Avg <- Avg %>% summarise(mean = mean(VALUE, na.rm = TRUE), n=n(), se = sqrt((mean(VALUE, na.rm = TRUE)*(1-mean(VALUE, na.rm = TRUE)))/n()))
      if(CItype=="pointwise") {
        zval <- 1-(((100-ConfLev)/2)/100)
      } else if (CItype=="simultaneous") {
        zval <- 1-(((100-ConfLev)/(2*length(unique(xaxis))))/100)
      }
      Avg <- Avg %>% mutate(ci = stats::qnorm(zval)*se)
    }
    Avg <- ungroup(Avg)

    # Prepare Condition columns and faceting or legend title/labels
    if (!is.null(Condition1) && is.null(Condition2)) {
      Avg <- Avg %>%
        rename(CustCond1 = UQ(sym(Condition1)))
      if (any(!is.na(Cond1Labels))) {
        lev1 <- unique(levels(Avg$CustCond1))
        for (x in 1:length(names(Cond1Labels))) {
          for(i in 1:length(lev1)) {
            if (lev1[i] == names(Cond1Labels)[x]) {
              lev1[i] <- Cond1Labels[[x]]
            }
          }
        }
        levels(Avg$CustCond1) <- lev1
      }
      Avg <- Avg %>%
        mutate(Cond = CustCond1)
      if (length(Columns)>1) {
        my_facet <- function() {
          eval(
            facet_grid(CustCond1 ~ ., labeller = my_labeller)
          )
        }
      } else {
        Cond <- Condition1
      }
    } else if (is.null(Condition1) && !is.null(Condition2)) {
      Avg <- Avg %>%
        rename(CustCond2 =  UQ(sym(Condition2)))
      if (any(!is.na(Cond2Labels))) {
        lev2 <- unique(levels(Avg$CustCond2))
        for (x in 1:length(names(Cond2Labels))) {
          for(i in 1:length(lev2)) {
            if (lev2[i] == names(Cond2Labels)[x]) {
              lev2[i] <- Cond2Labels[[x]]
            }
          }
        }
        levels(Avg$CustCond2) <- lev2
      }
      Avg <- Avg %>%
        mutate(Cond = CustCond2)
      if (length(Columns)>1) {
        my_facet <- function() {
          eval(
            facet_grid(. ~ CustCond2, labeller = my_labeller)
          )
        }
      } else {
        Cond <- Condition2
      }
    } else if (!is.null(Condition1) && !is.null(Condition2)) {
      Avg <- Avg %>%
        rename(CustCond1 =  UQ(sym(Condition1)), CustCond2 =  UQ(sym(Condition2)))
      if (any(!is.na(Cond1Labels))) {
        lev1 <- unique(levels(Avg$CustCond1))
        for (x in 1:length(names(Cond1Labels))) {
          for(i in 1:length(lev1)) {
            if (lev1[i] == names(Cond1Labels)[x]) {
              lev1[i] <- Cond1Labels[[x]]
            }
          }
        }
        levels(Avg$CustCond1) <- lev1
      }
      if (any(!is.na(Cond2Labels))) {
        lev2 <- unique(levels(Avg$CustCond2))
        for (x in 1:length(names(Cond2Labels))) {
          for(i in 1:length(lev2)) {
            if (lev2[i] == names(Cond2Labels)[x]) {
              lev2[i] <- Cond2Labels[[x]]
            }
          }
        }
        levels(Avg$CustCond2) <- lev2
      }
      Avg <- Avg %>%
        mutate(Cond = paste(CustCond1, CustCond2, sep = "_"))
      if (length(Columns)>1) {
        my_facet <- function() {
          eval(
            facet_grid(CustCond1 ~ CustCond2, labeller = my_labeller)
          )
        }
      } else {
        Cond <- paste(Condition1, Condition2, sep = "_by_")
      }
    }

    # Setting Error
    if(ErrorType=="SE"){
      Avg$error_lower <- Avg$mean - Avg$se
      Avg$error_upper <- Avg$mean + Avg$se
    } else if(ErrorType=="CI") {
      if(type=="elogit") {
        Avg$error_lower <- Avg$mean - Avg$ci
        Avg$error_upper <- Avg$mean + Avg$ci
      } else if(type=="proportion") {
        Avg$error_lower <- Avg$mean - Avg$ci
        Avg$error_lower <- ifelse(Avg$error_lower < 0, 0, Avg$error_lower)
        Avg$error_upper <- Avg$mean + Avg$ci
        Avg$error_upper <- ifelse(Avg$error_upper > 1, 1, Avg$error_upper)
      }
    }

    # Setting appropriate ylim
    if (type == "elogit") {
      ylim[1] = min(Avg$error_lower)
      ylim[2] = max(Avg$error_upper)
    } else if (type == "proportion") {
      ylim[1] = min(Avg$error_lower)
      ylim[2] = max(Avg$error_upper)
      if (ylim[1] > 0) {
        ylim[1] = 0
      }
      if (ylim[2] < 1) {
        ylim[2] = 1
      }
    }

    # Print message regarding CIs
    if (ErrorType=="CI" && (ErrorBand==TRUE | ErrorBar==TRUE)) {
      if(CItype == "pointwise") {
        message(paste0("Plot created with ", CItype, " confindence intervals (set to ", ConfLev, "%)."))
      }
      if(CItype == "simultaneous") {
        if(type=="elogit") {
          message(paste0("Plot created with ", CItype, " confindence intervals (adjusted to ", round(tval*100, 2), "% using the Bonferroni method)."))
        } else if(type=="proportion") {
          message(paste0("Plot created with ", CItype, " confindence intervals (adjusted to ", round(zval*100, 2), "% using the Bonferroni method)."))
        }
      }
    }

    # Set plot grouping
    if (length(Columns) > 1) {
      Col <- "IA"
    } else {
      if (!is.null(Condition1) || !is.null(Condition2)){
        Col <- "Cond"
      } else {
        Col <- NULL
      }
    }

    # Basic plot
    plt <- ggplot(Avg, aes_string(x = "Time", y = "mean", colour = Col)) +
      ylab(ylabel) +
      scale_x_continuous(limits = c(xlim[1], xlim[2])) +
      scale_y_continuous(limits = c(ylim[1], ylim[2]))

    # Basic themeing
    if (Theme == TRUE) {
      plt <- plt + .pac_theme()
    }

    # Determine/add faceting
    if (!is.null(Condition1) || !is.null(Condition2)) {
      if (length(Columns) > 1) {
        plt <- plt + my_facet()
      }
    }

    # Set errorbands according to theme and plot grouping
    if(ErrorBand==TRUE & Theme==TRUE) {
      plt <- plt + aes_string(shape = Col, colour = NULL) +
        geom_ribbon(aes_string(ymin="error_lower", ymax="error_upper", linetype=NA), fill="grey25", alpha=0.15, show.legend = FALSE)
      if(!is.null(Col)) {
        if(Col=="IA") {
          plt <- plt +
            scale_shape_discrete(name="Interest Area",
                                 breaks=names(Columns),
                                 labels=Columns)
        } else {
          plt <- plt +
            scale_shape_discrete(name=Cond,
                                 breaks=unique(Avg$Cond),
                                 labels=unique(Avg$Cond))
        }
      } else {
        plt <- plt
      }
    }
    if(ErrorBand==TRUE & Theme==FALSE) {
      plt <- plt +
        geom_ribbon(aes_string(ymin="error_lower", ymax="error_upper", linetype=NA, fill=Col), alpha=0.15, show.legend = FALSE)
      if(!is.null(Col)) {
        if(Col=="IA") {
          plt <- plt +
            scale_colour_hue(name="Interest Area",
                             breaks=names(Columns),
                             labels=Columns)
        } else {
          plt <- plt +
            scale_colour_hue(name=Cond,
                             breaks=unique(Avg$Cond),
                             labels=unique(Avg$Cond))
        }
      } else {
        plt <- plt
      }
    }

    # Set errorbars according to theme and plot grouping
    if(ErrorBar==TRUE & Theme==TRUE) {
      plt <- plt +
        geom_errorbar(aes_string(ymin="error_lower", ymax="error_upper"), width = .3)
      if(!is.null(Col)) {
        if(Col=="IA") {
          plt <- plt +
            scale_colour_grey(name="Interest Area",
                              breaks=names(Columns),
                              labels=Columns)
        } else {
          plt <- plt +
            scale_colour_grey(name=Cond,
                              breaks=unique(Avg$Cond),
                              labels=unique(Avg$Cond))
        }
      } else {
        plt <- plt
      }
    }
    if(ErrorBar==TRUE & Theme==FALSE) {
      plt <- plt +
        geom_errorbar(aes_string(ymin="error_lower", ymax="error_upper", color=Col), width = .3)
      if(!is.null(Col)) {
        if(Col=="IA") {
          plt <- plt +
            scale_colour_hue(name="Interest Area",
                             breaks=names(Columns),
                             labels=Columns)
        } else {
          plt <- plt +
            scale_colour_hue(name=Cond,
                             breaks=unique(Avg$Cond),
                             labels=unique(Avg$Cond))
        }
      } else {
        plt <- plt
      }
    }

    # Finish plot
    plt <- plt + geom_point() + geom_line()

    plt

    } ## SHARED CODE ENDS HERE ##

  }


#' Plot each event within a group to a directory
#'
#' \code{plot_events} plots each event in a group as a multi-panel plot and
#' saves it into specified directory.
#'
#' @export
#' @import dplyr
#' @import rlang
#' @import ggplot2
#'
#' @param data A data table object output by
#' \code{\link{ppl_select_recorded_eye}}.
#' @param Column A character string indicating the column to plot.
#' @param Grouping A character string indicating the column to serve as the
#' grouping. For example, "Subject" will use the subject identifier, producing
#' one image per subject containing all the events for that subject.
#' @param Ncol = A number specifying how many columns per page.
#' @param Nrow = A number specifying how many rows per page.
#' @param Device A character string indicating device type passed to
#' \code{ggsave}. By default, this is set to "pdf".
#' @param ... Arguments to be passed to \code{ggsave}.
#' @return Files containing plots.
#' @examples
#' # Load example data
#' data("Pupilex3")
#'
#' # Writing files to temporary folder for the example
#' plot_events(Pupilex3, Column = "Pupil", Device = "pdf",
#'             Grouping = "Subject", path = paste0(tempdir(),"/Figs"),
#'             Nrow = 1, Ncol = 1, width = 11, height = 8.5)
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Plotting", package="PupilPre")
#'
plot_events <- function(data = data, Column=NULL, Grouping = "Subject", Nrow = 1, Ncol = 1, Device = "pdf", ...) {

  # Check for column
  if(is.null(Column)){
    stop("Please supply the column for which the difference will be calculated!")
  } else {
    if(!(Column %in% colnames(data))){
      stop(paste(Column, "column not present in data!"))
    }
  }

  # Check if path is supplied
  args <- list(...)
  if("path" %in% names(args)){
    dr <- dir.create(path = args$path, showWarnings = FALSE)
    # Print message about path
    if(dr) {
      message(paste(args$path, "created."))
    } else {
      message(paste(args$path, "already exists"))
    }
  } else {
    stop("Please provide a directory for the argument path to store the figures.")
  }

  # Make plots
  col <- Column
  gr <- as.character(unique(data[[Grouping]]))
  Group <- sym(Grouping)
  for (i in gr) {

    # Reset grid
    C <- Ncol
    R <- Nrow

    # Make subset
    tmp <- filter(data, !!Group == i) %>% droplevels() %>% group_by(Event) %>%
      mutate(Missing = round(sum(is.na(UQ(sym(col))))/n()*100)) %>%
      mutate(Event2 = paste0(Event[1], "_", Missing[1], "%_NA")) %>% ungroup()

    # Calculate plots per page and number of pages needed
    plprpg <- C*R
    pgs <- ceiling(length(unique(tmp$Event))/plprpg)

    # Set same x and y
    if(eval_tidy(Grouping) != "Event") {
      xlm <- range(tmp$Time, na.rm = TRUE)
      ylm <- range(tmp[[eval_tidy(col)]], na.rm = TRUE)
      message(paste(i, "-", pgs, "page(s)"))
    } else {
      message(paste(i))
    }

    # Loop through pages
    for (j in 1:pgs) {

      evs <- unique(tmp$Event)[(1:plprpg)+(plprpg*(j-1))]
      remder <- length(unique(tmp$Event))%%plprpg

      tmp2 <- filter(tmp, Event %in% evs) %>% droplevels()

      # Estimate grid for last page
      if(length(unique(tmp2$Event))==remder) {
        C <- ceiling(sqrt(remder))
        R <- C
      }

      p <- ggplot(tmp2, aes_string(x="Time", y=eval_tidy(col))) +
        geom_point(na.rm = TRUE)

      if(eval_tidy(Grouping) != "Event") {
        p <- p + facet_wrap("Event2", ncol = C, nrow = R, drop = FALSE) +
          coord_cartesian(xlim = xlm, ylim = ylm)
      } else {
        p <- p + ggtitle(paste(tmp2$Event2))
      }

      p <- p + .pac_theme()

      # Make filename tag
      if (eval_tidy(Grouping) != "Event") {
        tag <- paste0("_", j, "of", pgs)
      } else{
        tag <- ""
      }

      ggsave(filename = paste0(i, "_", eval_tidy(col), tag, ".", Device), plot = p, device = Device, ...)
    }

  }
  message(paste0("Event plots created by ", Group, ". ", length(gr), " plots saved to ",
                 ifelse("path" %in% names(args), paste0(args$path, "."), "the working directory.")))
}



#' Plots average difference between two conditions.
#'
#' \code{ppl_plot_avg_cdiff} calculates the average of differences between
#' two specified conditions along with standard error and then plots the
#' results.
#'
#' @export
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @import tidyr
#' @import VWPre
#'
#' @param data A data table object.
#' @param xlim A vector of two integers specifying the limits of the x-axis.
#' @param Column A character vector specifying the desired column.
#' @param Averaging A character string indicating how the averaging should
#' be done. "Subject" (default) will produce the grand mean in the data, while
#' "Item" (or, in principle, any other column name) will
#' calculate the grand mean by that factor.
#' @param Condition A list containing the column name corresponding to the
#' condition and factor levels to be used for calculating the difference.
#' @param CondLabels A named character vector specifying the desired labels
#' of the levels of the condition.
#' @param ErrorBar A logical indicating whether error bars should be
#' included in the plot.
#' @param ErrorBand A logical indicating whether error bands should be
#' included in the plot.
#' @param ErrorType A string indicating "SE" or "CI".
#' @param ConfLev A number indicating the confidence level of the CI.
#' @param CItype A string indicating "simultaneous" or "pointwise". Simultaneous
#' performs a Bonferroni correction for the interval.
#' @param PupilPreTheme A logical indicating whether the theme included with the
#' function should be applied, or ggplot2's base theme (to which any other
#' custom theme could be added).
#' @examples
#' # Load example data
#' data("Pupilex7")
#'
#' ppl_plot_avg_cdiff(data = Pupilex7, Column = "Pupil",
#'                    Condition = list(talker = c("EN3", "CH1")),
#'                    ErrorBar = TRUE, PupilPreTheme = TRUE)
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Plotting", package="PupilPre")
#'
ppl_plot_avg_cdiff <- function(data, Column = NULL, xlim = NA,
                          Averaging = "Subject",
                          Condition = NULL, CondLabels = NA,
                          ErrorBar = TRUE, PupilPreTheme = TRUE,
                          ConfLev = 95, CItype = "simultaneous",
                          ErrorBand = FALSE, ErrorType = "SE") {

  type <- "elogit"
  Theme <- PupilPreTheme

  {## SHARED CODE BEGINS HERE ##

    if(is.null(Column)){
      stop("Please supply the column for which the difference will be calculated!")
    } else {
      if(!(Column %in% colnames(data))){
        stop(paste(Column, "column not present in data!"))
      }
    }

    # Check condition column
    if(is.null(Condition)){
      stop("Please supply a condition column along with the two levels desired for calculating the difference!")
    }
    if(!(is.null(Condition))) {
      if (!(names(Condition)[1] %in% colnames(data))) {
        stop(paste(names(Condition)[1], " column not present in data!"))
      }
      if (length(Condition[[1]]) != 2) {
        stop(paste("Please ensure that two levels are specified for column", names(Condition)[1]))
      }
      if (Condition[[1]][1] %in% unique(data[[names(Condition)[1]]]) &&
          Condition[[1]][2] %in% unique(data[[names(Condition)[1]]])) {
      } else {
        stop(paste("One (or more) of the specified levels do not exist in column", names(Condition)[1]))
      }
    }

    if(is.null(Averaging)){
    } else if(!(Averaging %in% colnames(data))){
      stop(paste(Averaging, "column not present in data!"))
    }

    if(ErrorBar==TRUE && ErrorBand==TRUE){
      stop(paste("Please select either error bars OR error bands.
                 For error bars set ErrorBar=TRUE and ErrorBand=FALSE.
                 For error bands set ErrorBar=FALSE and ErrorBand=TRUE."))
    }


    # Inform about Grand Average calculation
	if(is.null(Averaging)){
	message(paste0("Average of difference between ", Condition[[1]][1], " and ", Condition[[1]][2], " calculated using sample means."))
    } else {
      message(paste0("Grand average of difference between ", Condition[[1]][1], " and ", Condition[[1]][2], " calculated using ", Averaging, " means."))
    }

    # Prepare data

    if (is.na(xlim[1])) {
      xlim <- c(range(data$Time)[1], range(data$Time)[2])
      xaxis <- unique(data$Time)
    } else {
      xlim <- xlim
      data<-data[data$Time>=xlim[1] & data$Time<=xlim[2],]
      xaxis <- unique(data$Time)
    }

    # Custom labels
    if(any(is.na(CondLabels))){
      main <- paste("Difference between", Condition[[1]][1], "and", Condition[[1]][2], "\n")
    } else {
      main <- paste("Difference between",
                    CondLabels[which(names(CondLabels) == Condition[[1]][1])], "and",
                    CondLabels[which(names(CondLabels) == Condition[[1]][2])], "\n")

    }

    tmpdata <- data %>%
      rename(Cond1 = UQ(sym(names(Condition)[1]))) %>%
      filter(Cond1 %in% Condition[[1]]) %>% droplevels()
    tmpdata <- tmpdata %>% mutate(Cond = case_when(
      Cond1 == Condition[[1]][1] ~ "Lev1",
      Cond1 == Condition[[1]][2] ~ "Lev2"))
    tmpdata$Cond <- as.factor(tmpdata$Cond)


    # set column selection
	if(!is.null(Averaging)) {
    sel_names = quos(UQ(sym(Column)), Time, Cond, UQ(sym(Averaging)))
	} else {
	sel_names = quos(UQ(sym(Column)), Time, Cond)
	}

    # set grouping
	if(!is.null(Averaging)) {
    group_names1 = quos(Time, Cond, UQ(sym(Averaging)))
	}
    group_names2 = quos(Time, Cond)

    # Check balance of data
	if(!is.null(Averaging)) {
    chk <- tmpdata %>% group_by(UQ(sym(Averaging)), Cond) %>% summarise()
    chk <- unique(rowSums(table(chk))/2)
    if(length(chk) > 1){
      stop(paste0("Both levels of ", names(Condition)[1]," are not present for each level of", Averaging, ". Please select a different factor for \"Averaging\" and/or \"Condition\". Note that \"Averaging\" can be set to NULL so that sample means are used."))
    } else {
      if(chk == 1) {
      } else {
        stop(paste0("Both levels of ", names(Condition)[1]," are not present for each level of", Averaging, ". Please select a different factor for \"Averaging\" and/or \"Condition\". Note that \"Averaging\" can be set to NULL so that sample means are used."))
      }
    }
	}

    # Averaging
    tmpdata <- tmpdata %>% select(!!!sel_names)

    if(!is.null(Averaging)) {
      tmpdata <- tmpdata %>%
        group_by(!!!group_names1) %>%
        summarise(Mean = mean(UQ(sym(Column)), na.rm = TRUE)) %>%
        group_by(!!!group_names2) %>%
        tidyr::spread(Cond, Mean) %>% arrange(UQ(sym(Averaging)), Time) %>%
        mutate(Diff = Lev1 - Lev2)
    } else {
      tmpdata <- tmpdata %>%
        group_by(!!!group_names2) %>%
        summarise(Mean = mean(UQ(sym(Column)), na.rm = TRUE)) %>%
        tidyr::spread(Cond, Mean) %>%
        arrange(Time) %>%
        mutate(Diff = Lev1 - Lev2)
    }

    if(type != "proportion") {
      tmpdata <- tmpdata %>%
        summarise(meanDiff = mean(Diff, na.rm = TRUE),
                  DC1m = mean(Lev1, na.rm = TRUE),
                  DC2m = mean(Lev2, na.rm = TRUE),
                  DC1sd = stats::sd(Lev1, na.rm = TRUE),
                  DC2sd = stats::sd(Lev2, na.rm = TRUE),
                  n1 = n(), n2 = n()) %>%
        mutate(se = sqrt( ((DC1sd^2)/n1) + ((DC2sd^2)/n2) )) %>%
        mutate(degfree = (((((DC1sd^2)/n1) + ((DC2sd^2)/n2))^2) / (((((DC1sd^2)/n1)^2) / (n1-1)) + ((((DC2sd^2)/n2)^2) / (n2-1)))) )
      if(CItype=="pointwise") {
        tval <- 1-(((100-ConfLev)/2)/100)
      } else if (CItype=="simultaneous") {
        tval <- 1-(((100-ConfLev)/(2*length(unique(xaxis))))/100)
      }
      tmpdata <- tmpdata %>% mutate(ci = stats::qt(tval,df=degfree)*se)
    } else if (type=="proportion") {
      tmpdata <- tmpdata %>%
        summarise(meanDiff = mean(Diff, na.rm = TRUE),
                  DC1m = mean(Lev1, na.rm = TRUE),
                  DC2m = mean(Lev2, na.rm = TRUE),
                  n1 = n(), n2 = n()) %>%
        mutate(se = sqrt(((DC1m*(1-DC1m))/n1)+((DC2m*(1-DC2m))/n2)))
      if(CItype=="pointwise") {
        zval <- 1-(((100-ConfLev)/2)/100)
      } else if (CItype=="simultaneous") {
        zval <- 1-(((100-ConfLev)/(2*length(unique(xaxis))))/100)
      }
      tmpdata <- tmpdata %>% mutate(ci = stats::qnorm(zval)*se)
    }

    tmpdata <- ungroup(tmpdata)

    # Setting Error
    if(ErrorType=="SE"){
      tmpdata$error_lower <- tmpdata$meanDiff - tmpdata$se
      tmpdata$error_upper <- tmpdata$meanDiff + tmpdata$se
    } else if(ErrorType=="CI") {
      tmpdata$error_lower <- tmpdata$meanDiff - tmpdata$ci
      tmpdata$error_upper <- tmpdata$meanDiff + tmpdata$ci
    }

    # Setting ylim
    ylim <- c(0,0)
    if (type != "proportion") {
      ylim[1] = min(tmpdata$error_lower)
      ylim[2] = max(tmpdata$error_upper)
    } else if (type == "proportion") {
      ylim[1] = min(tmpdata$error_lower)
      ylim[2] = max(tmpdata$error_upper)
    }

    # Print message regarding CIs
    if (ErrorType=="CI" && (ErrorBand==TRUE | ErrorBar==TRUE)) {
      if(CItype == "pointwise") {
        message(paste0("Plot created with ", CItype, " confindence intervals (set to ", ConfLev, "%)."))
      }
      if(CItype == "simultaneous") {
        if(type=="elogit") {
          message(paste0("Plot created with ", CItype, " confindence intervals (adjusted to ", round(tval*100, 2), "% using the Bonferroni method)."))
        } else if(type=="proportion") {
          message(paste0("Plot created with ", CItype, " confindence intervals (adjusted to ", round(zval*100, 2), "% using the Bonferroni method)."))
        }
      }
    }

    plt <- ggplot(tmpdata, aes(x = Time, y = meanDiff)) +
      ylab("Difference") +
      geom_hline(yintercept=0) +
      scale_x_continuous(limits = c(xlim[1], xlim[2])) +
      scale_y_continuous(limits = c(ylim[1], ylim[2]))

    if (Theme == TRUE) {
      plt <- plt + .pac_theme()
    }

    # Set errorbands according to theme
    if(ErrorBand==TRUE & Theme==TRUE) {
      plt <- plt +
        geom_ribbon(aes(ymin=error_lower, ymax=error_upper, linetype=NA), fill="grey25", alpha=0.15, show.legend = FALSE)
      plt <- plt
    }

    if(ErrorBand==TRUE & Theme==FALSE) {
      plt <- plt +
        geom_ribbon(aes(ymin=error_lower, ymax=error_upper, linetype=NA), alpha=0.15, show.legend = FALSE)
      plt <- plt
    }

    # Set errorbars according to theme and plot grouping
    if(ErrorBar==TRUE & Theme==TRUE) {
      plt <- plt +
        geom_errorbar(aes(ymin=error_lower, ymax=error_upper), width = .3)
      plt <- plt
    }

    if(ErrorBar==TRUE & Theme==FALSE) {
      plt <- plt +
        geom_errorbar(aes(ymin=error_lower, ymax=error_upper), width = .3)
      plt <- plt
    }

    plt +
      geom_point() +
      geom_line() +
      ggtitle(main)

  } ## SHARED CODE ENDS HERE ##

}



#' Plots average contour surface of pupil data.
#'
#' \code{ppl_plot_avg_contour} calculates the conditional average of pupil
#' size by Time and a specified continuous variable. It then applies a 3D smooth
#' (derived using \code{\link[mgcv]{gam}}) over the surface and plots the
#' results as a contour plot.
#'
#' @export
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @import VWPre
#'
#' @param data A data table object output by either \code{\link{create_time_series}}.
#' @param Column A string specifying the column to use.
#' @param xlim A vector of two integers specifying the limits of the x-axis.
#' @param Var A string containing the column name corresponding to the continuous
#' variable.
#' @param Averaging A character string indicating how the averaging should
#' be done. "Event" (default) will produce the overall mean in the data, while
#' "Subject" or "Item" (or, in principle, any other column name) will
#' calculate the grand mean by that factor.
#' @param VarLabel A string specifying the axis label to use for \code{Var}.
#' @param PupilPreTheme A logical indicating whether the theme included with the
#' function, or ggplot2's base theme (which any other custom theme could be added).
#' @param Colors A vector of two strings specifying the colrs of the contour
#' shading - The default values represent grayscale.
#' @examples
#' # Load example data
#' data("Pupilex7")
#'
#' ppl_plot_avg_contour(data = Pupilex7, Column = "Pupil", Var = "TRIAL_INDEX",
#'                      VarLabel = "Trial", xlim = c(0,2000),
#'                      PupilPreTheme = TRUE, Colors = c("gray20", "gray90"))
#'
#' # Please see the vignettes for detailed example usage.
#' # vignette("PupilPre_Plotting", package="PupilPre")
#'
ppl_plot_avg_contour <- function(data, Column = NULL, Var = NULL,
                             Averaging = "Event",
                             VarLabel = NULL, xlim=NA, PupilPreTheme=TRUE,
                             Colors=c("gray20", "gray90")) {

    type <- "elogit"
    LegendName <- "Mean\npupil\ndilation\n"
    Theme <- PupilPreTheme

  {## SHARED CODE BEGINS HERE ##

    if(is.null(Column)){
      stop("Please supply the column to be plotted!")
    } else if(!(Column %in% colnames(data))) {
      stop(paste(Column, "column not present in data!"))
    }

    if(is.null(Var)){
      stop("Please supply the variable column name to be used in the contour plot!")
    } else if(!(Var %in% colnames(data))) {
      stop(paste(Var, "column not present in data!"))
    }

    if(is.numeric(eval(parse(text=paste0("data$", Var)))) | is.integer(eval(parse(text=paste0("data$", Var))))){
    } else {
      stop("The contour plot variable must be of class 'numeric' or class 'integer'!")
    }

    if(!(Averaging %in% colnames(data))){
      stop(paste(Averaging, " column not present in data!"))
    } else (
      message(paste0("Surface calculated using ", Averaging, " means."))
    )

    # # Check condition columns
    # if(!(is.null(Condition1))) {
    #   if(is.na(Condition1)) {
    #     stop(paste("Please use NULL when not specifying a condition."))
    #   } else if (!(Condition1 %in% colnames(data))) {
    #     stop(paste(Condition1, " column not present in data!"))
    #   }
    # }
    # if(!(is.null(Condition2))) {
    #   if(is.na(Condition2)) {
    #     stop(paste("Please use NULL when not specifying a condition."))
    #   } else if (!(Condition2 %in% colnames(data))) {
    #     stop(paste(Condition2, " column not present in data!"))
    #   }
    # }

    zlim <- c(-4,4)
    Colors <- Colors

    sel_names <- quos(UQ(sym(Averaging)), UQ(sym(Column)), Time, UQ(sym(Var)))
    group_names1 = quos(UQ(sym(Averaging)), UQ(sym(Column)), UQ(sym(Var)), Time)
    group_names2 = quos(UQ(sym(Column)), UQ(sym(Var)), Time)


    if(is.null(VarLabel)) {
      VarLabel <- Var
    }

    if (is.na(xlim)[1]) {
      xlim <- c(range(data$Time)[1], range(data$Time)[2])
    } else {
      xlim <- xlim
    }

    Avg <- data %>% select(!!!sel_names) %>%
      group_by(!!!group_names1) %>%
      summarise(Columnmean = mean(UQ(sym(Column)), na.rm = TRUE)) %>%
      group_by(!!!group_names2) %>%
      summarise(mean = mean(Columnmean, na.rm = TRUE))

    if (type == "proportion") {
      Avg$meanon <- round(Avg$mean*100)
      Avg$meanoff <- 100 - Avg$meanon
      Avg$meanbinom <- cbind(Avg$meanon, Avg$meanoff)
      var<-list(Var)
      model <- lapply(var, function(x) {
        mgcv::gam(substitute(meanbinom ~ te(Time, i), list(i = as.name(x))), data = Avg, family = binomial)
      })
    } else {
      var<-list(Var)
      model <- lapply(var, function(x) {
        mgcv::gam(substitute(mean ~ te(Time, i), list(i = as.name(x))), data = Avg)
      })
    }

    model<-model[[1]]

    names(Avg)[names(Avg)==Var] <- "Variable"
    Varcol<-Avg$Variable
    Varcol<-unique(Varcol)

    # create a vector for variable A of length np
    data<-data[order(data$Event, data$Time),]
    rate <- 1000 / (data$Time[2] - data$Time[1])
    nptime<-round((xlim[2]-xlim[1])/100*(100/(1000/rate)))
    time<-seq(xlim[1],xlim[2],length.out=nptime)
    # create a vector for variable B of length np
    npvar<-length(unique(Varcol))
    variable<-seq(range(Varcol)[1],range(Varcol)[2],length.out=npvar)
    tmp0<-expand.grid(time,variable)
    colnames(tmp0)<-c("Time", Var)
    newdat<-tmp0

    # compute lpmatrix
    X1<-stats::predict(model,newdat,type="lpmatrix")
    mean<-X1%*%stats::coef(model)
    predsmooth<-cbind(newdat, mean)
    names(predsmooth)[names(predsmooth)==Var] <- "Variable"

    if (type == "proportion") {
      predsmooth$mean <- stats::plogis(predsmooth$mean)
      zlim[1] = 0
      zlim[2] = 1
    } else {
      zlim[1] = min(predsmooth$mean) - 0.25
      zlim[2] = max(predsmooth$mean) + 0.25
    }

    plt <- ggplot(predsmooth) +
      aes(x = Time, y = Variable, z = mean, fill = mean) +
      geom_tile(aes(fill = mean)) +
      stat_contour() +
      geom_contour(color = "white") +
      scale_fill_gradient(name = LegendName, limit=c(zlim[1], zlim[2]), low = Colors[1], high = Colors[2]) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      ylab(VarLabel)
    if (Theme == TRUE) {
      plt <- plt +
        .pac_theme()
    }

  plt

  } ## SHARED CODE ENDS HERE ##

}


