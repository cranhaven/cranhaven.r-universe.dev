utils::globalVariables(c("outputplan", "od", "cd", "dd", "logd", "FigureNumber", "graph.region.h", "graph.region.w", "FigureStatus", "FigureTitle", "i" ))

# Graphics functions for figuRes2 ------

# Standard Graphics Names ------
#' @title Standard graphics names
#' @description This is a dummy function whose purpose is to serve as repository for arguments used by figuRes2 functions.
#' @param add.fignum logical (annotate.page)
#' @param addBars logical to add error bars (line.plot)
#' @param addTime logical for ading time stamp (annotate.page)
#' @param at.risk.palette colors to be assocated with categorical variable in accompanying km.plot generated at.risk table 
#' @param background.palette palette gets passed to scale_fill_manual (forest.plot)
#' @param bar.position passed to geom_bar (bar.plot)
#' @param bar.width used by line.plot
#' @param base_family used in set_theme calls
#' @param base_size used in set_theme calls
#' @param bottom.axis.version ggplot object with bottom x-axis (get.top.xaxis)
#' @param bottom.margin used by build.page and annotate.page 
#' @param category option to add a column populated with a factor (by gcurve)
#' @param category.col data.frame column associated with categorical variable (bar.plot, box.plot, cdf.plot, dot.plot, km.plot)
#' @param category.color data.frame column assocated with aes color mapping (forest.plot, line.plot, nsubj.plot, table.plot)
#' @param category.label passed to x-axis label 
#' @param category.palette colors assoicated with categorical variable 
#' @param category.symbol.col used by line.plot
#' @param cd directory where driver (code) files are stored 
#' @param censor.col name of parent.df column associated with censor variable 
#' @param centime.col name of parent.df column associated with censored time 
#' @param dd directory where data is stored 
#' @param envir used internally by some functions 
#' @param fignum figure number (annotate.page)
#' @param fignum.buffer fine-control of vertical position (annotate.page)
#' @param filename common_root.pdf or common_root.csv
#' @param flip.palette logical; if TRUE it reverse the order of colors used for background (forest.plot)
#' @param fnote vector of 5 footnotes. 5th row is traditionally reserved for filepath, table reference and time stamp. Populate from bottom up. (annotate.page)
#' @param fnote.buffer fine-control of vertical position (annotate.page)
#' @param foot.size default: 10; passed to grid.text via gp (annotate.page)
#' @param fromthetop logical. If TRUE KM curve decends from 1, if FALSE KM curve ascends from 0 Ensure you have an appropriate censor.col passed above! 
#' @param gg.list a list of ggplot objects (sync.ylab.widths)
#' @param head.size default: 10 (anotate.page)
#' @param header.buffer fine-control of vertical position (anotate.page)
#' @param interior a list of nrow*ncol grobs/ggplot objects to be displayed in the grid, ordered by row then col (build.page)
#' @param interior.h a vector summing to 1 to indicate how to partition the heights (build.page)
#' @param interior.w a vector summing to 1 to indicate how to partition the widths (build.page)
#' @param killMissing logical used by bar.plot 
#' @param left.margin used by build.page and annotate.page; presumed to be inches  
#' @param linetype.col name of parent.df column associated with linetype 
#' @param line.size value gets passed to size within geom_line, geom_step 
#' @param linetype.palette values passed to scale_linetype_manual 
#' @param loadplan logical; if TRUE then it loads from the filename
#' @param logd directory where log files are sent 
#' @param log.trans Logical; if TRUE log transformation is applied to x axis (ensure x.limits are positive!) (forest.plot)
#' @param lower.lim column holding lower limit of CI 
#' @param main.theme text string name of theme to be called by theme_set, 
#' @param my.path path to main directory, 
#' @param ncol number of columns for the grid of graphics being built by build.page 
#' @param nrow number of rows for the grid of graphics being built by build.page 
#' @param nsubj.plot.label used in km.plot
#' @param od directory where output files are sent 
#' @param outfile If (toPDF== TRUE & outfile == "") a .pdf file with root name taken from outputplan$outfile[which(outputplan$rcode ==source.code)]. Otherwise a .pdf will be created the value of outfile. The pdf is stored in mypath/od defined in setpaths.r.
#' @param override override 
#' @param page.height used by build.page and annotate.page; presumed to be inches 
#' @param page.width used by build.page and annotate.page; presumed to be inches 
#' @param parent.df data.frame used by ggplot 
#' @param pdval value passed to position_dodge (lineplot)
#' @param Point.Est point estimate 
#' @param pos used internally by some functions
#' @param response.col used by cdf.plottttt
#' @param reportNR If TRUE, a plot with missing figure numbers and titles is produced 
#' @param right.margin used by build.page and annotate.page; presumed to be inches
#' @param shape.label value sets passed to labs 
#' @param shape.palette values passed to scale_shape_manual 
#' @param source.code This is intended to be a darapladib graphics driver file returning a graphic possibly with complete headers and footers. 
#' @param text.buffer used by bar.plot to control text placement
#' @param text.col used by nsubj.plot
#' @param test.dim logical. Assists with figure development. If TRUE it makes a call to grid.show.layout.
#' @param text.col1 name of column holding text for column 1 (table.plot)
#' @param text.col2 name of column holding text for column 2; can be NULL (table.plot)
#' @param text.col3 name of column holding text for column 3; can be NULL (table.plot)
#' @param text.col4 name of column holding text for column 4; can be NULL (table.plot)
#' @param text.size value gets passed to geom_text 
#' @param title vector of title lines (annotate.page)
#' @param title.buffer fine-control of vertical position (annotate.page)
#' @param title.size default: 14; passed to grid.text via gp (annotate.page)
#' @param toBMP Logical. If TRUE a .bmp file will be created. (run.specific)
#' @param toEPS Logical. If TRUE a .eps file will be created. (run.specific)
#' @param toJPEG Logical. If TRUE a .jpeg file will be created. (run.specific)
#' @param top.axis.version ggplot object with intended top x-axis in bottom position (get.top.xaxis)
#' @param top.margin used by build.page and annotate.page; presumed to be inches 
#' @param toPDF Logical. If TRUE a .pdf file will be created. If FALSE graphic is sent to screen. (run.specific)
#' @param toPNG Logical. If TRUE a .png file will be created. (run.specific)
#' @param toWMF Logical. If TRUE a .wmf file will be created. (run.specific)
#' @param ulh vector for upper left headers (annotate.page)
#' @param upper.lim column holding upper limit of CI (forest.plot)
#' @param urh vector for upper right headers (annotate.page)
#' @param UseSubset Corresponds to a column name in outputplan holding flags (all_in_one)
#' @param x.col parent.df column associated with response vairable (line.plot, nsubj.plot)
#' @param x.label value gets passed to labs 
#' @param x.limits value gets passed to scale_x_continuous 
#' @param x.ticks value gets passed to scale_x_continuous 
#' @param x.ticks.labels passed to scale_x_continuous 
#' @param y.col parent.df column associated with response vairable 
#' @param y.digits passed to scale_y_continuous label's, fmt (box.plot, line.plot)
#' @param y.label value gets passed to labs 
#' @param y.label.col column holding labels for forest/dot/table plots 
#' @param y.label.rank.col column holding ranks for labels in forest/dot/table plots 
#' @param y.limits passed to scale_y_continuous 
#' @param y.rank.col column holding ranks for line items in forest/dot/table plots 
#' @param y.ticks  passed to scale_y_continuous 
#' @param ymax.col name of parent.df column associated with ymax (line.plot errorbars)
#' @param ymin.col name of parent.df column associated with ymin (line.plot errorbars)
#' @return This function is just a convenient location to store argument names.
#' @author Greg Cicconetti

graphic.params <- function(
  add.fignum  ,
  addBars,
  addTime,
  at.risk.palette,
  background.palette,
  bar.position,
  bar.width,
  base_family,
  base_size,
  bottom.axis.version,
  bottom.margin,
  category,
  category.color,
  category.col,
  category.label,
  category.symbol.col,
  category.palette,
  cd,
  censor.col,
  centime.col,
  dd,
  envir,
  fignum,
  fignum.buffer,
  filename,
  flip.palette,
  fnote,
  fnote.buffer,
  foot.size,
  fromthetop,
  gg.list,
  head.size,
  header.buffer,
  interior,
  interior.h,
  interior.w,
  killMissing,
  left.margin,
  linetype.col,
  line.size,
  linetype.palette,
  loadplan,
  logd,
  log.trans,
  lower.lim,
  main.theme,
  my.path,
  ncol,
  nrow,
  nsubj.plot.label,
  od,
  outfile,
  override,
  page.height,
  page.width,
  parent.df,
  pdval,
  Point.Est,
  pos,
  reportNR,
  response.col,
  right.margin,
  shape.label,
  shape.palette,
  source.code,
  text.buffer,
  test.dim,
  text.col,
  text.col1,
  text.col2,
  text.col3,
  text.col4,
  text.size,
  title,
  title.buffer,
  title.size,
  toBMP,
  toEPS,
  toJPEG,
  top.axis.version,
  top.margin,
  toPDF,
  toPNG,
  toWMF,
  ulh,
  upper.lim,
  urh,
  UseSubset,
  x.col,
  x.label,
  x.limits,
  x.ticks,
  x.ticks.labels,
  y.col,
  y.digits,
  y.label,
  y.label.col,
  y.label.rank.col,
  y.limits,
  y.rank.col,
  y.ticks,
  ymax.col,
  ymin.col
){
return("Hello, this function is just a convient location to store argument names.")
}

# Plot standards -----------------------------------------
# bar.plot -------
#' @title bar.plot
#' @description A function for creating harmonized ggplot2 bar charts
#' @inheritParams graphic.params
#' @return A ggplot object is returned.
#' @examples 
#' {
#' # Access dummy demography dataset
#' data(demog.data)
#' levels(demog.data$SEX) <- c("Female", "Male")
#' 
#' # A ggplot object is returned
#' p1 <- bar.plot(parent.df = demog.data, y.col = "SEX", 
#' x.label= "Gender", y.label = "Percentage of Subjects", 
#' category.col = "REGION", category.label = "Region", 
#' y.limits = c(0, 0.35), y.ticks = seq(0, 0.5, 0.05), 
#' bar.position= "dodge", 
#' category.palette = RColorBrewer::brewer.pal(n=5, name = "Dark2"),
#' text.size =4, text.buffer=.025, killMissing = TRUE) 
#' print(p1)
#' } 
#' @author Greg Cicconetti
bar.plot <- function(
  parent.df, 
  category.col = "TRTGRP",
  category.label = "Treatment Group", 
  x.label= "",
  y.col = "GWHRT", 
  y.label = "Percentage of Subjects", 
  y.limits = c(0, 0.7), 
  y.ticks = seq(0, 0.3, 0.05), 
  bar.position= "dodge",
  category.palette = c("red", "blue"),
  text.size =3,
  text.buffer=.05,
  killMissing = TRUE) 
  {

Var1 <- Var2 <- Freq <- Prop <- Prop.text <- CATEGORY <- RESPONSE <- LOWER <- UPPER <- MEDIAN <- NULL 
  
  if(is.null(y.limits) || is.null(y.ticks)) {
    y.limits = c(0, 1)
    y.ticks <- seq(0,1,.1)
    message("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
  }
  tab <- data.frame(table(parent.df[, y.col], parent.df[, category.col]))
  if (killMissing == T) {
    tab <- subset(tab, Var1 != "Missing")
    tab <- subset(tab, Var2 != "Missing")
    tab$Var1 <- factor(tab$Var1)
  }
  
  barplot.df <- ddply(tab, .(Var1), mutate, Prop= Freq/sum(Freq))
  barplot.df$Prop.text <- barplot.df$Prop + text.buffer
  p1 <- ggplot(data = barplot.df, aes(x = Var1, y = Prop, fill = Var2)) + 
    geom_bar(stat = "identity", position = bar.position) + 
    coord_flip() + 
    geom_text( position=position_dodge(width=.9), size =text.size, 
         aes(y=Prop.text, label=paste(" n = ", Freq, sep = "")))+
      scale_y_continuous(limits = y.limits, breaks = y.ticks, labels = percent_format()) + 
      scale_fill_manual(values = category.palette) + 
      labs(y = y.label, x = x.label, fill = category.label)+
    theme(legend.position= "bottom")
  return(p1)
  }

# Box.plot -------------
#' @title box.plot
#' @description A function for creating harmonized ggplot2 boxplots
#' @inheritParams graphic.params
#' @return A ggplot object is returned.
#' @examples 
#' {
#' data(demog.data)
#' # pre-processing
#' 
#' levels(demog.data$SEX) <- c("Female", "Male")
#' 
#'  p1 <- box.plot(parent.df = demog.data, 
#'      y.col = "BMI", 
#'      y.label = expression(paste("BMI (m/kg",phantom()^2,")")), 
#'      category.col = "SEX",
#'      category.label = "Gender", 
#'       y.limits = c(0, 70), 
#'       y.ticks = seq(0, 100, 10), 
#'       y.digits = 0,
#'       shape.palette = c(20, 20),
#'       category.palette = rainbow(6),
#'       text.size = 4)
#'  print(p1)
#' }
#' @author Greg Cicconetti
box.plot <- function (parent.df, 
                      y.col = "AGE", 
                      y.label = "AGE",
                      category.col = "TRTGRP", 
                      category.label = "Treatment Group",
                      y.limits = NULL, 
                      y.ticks = NULL, 
                      y.digits = 0,
                      shape.palette = c(21,22), 
                      category.palette = c(2,3),
                      text.size = 4) {
        CATEGORY <- RESPONSE <- LOWER <- UPPER <- MEDIAN <- NULL 
        
        get.whiskers <- function(dframe) {
                bplot <- boxplot(dframe$RESPONSE ~ dframe$CATEGORY, plot = FALSE)
                whiskers <- with(bplot, 
                                 data.frame(CATEGORY = names, 
                                            LOWER = stats[1, ], 
                                            MEDIAN = stats[3, ], 
                                            UPPER = stats[5, ],
                                            N=n))
                return(whiskers)
        }
        
        names(parent.df) <- toupper(names(parent.df))
        
        boxplot.df <- data.frame(
                RESPONSE = parent.df[, y.col],
                CATEGORY= parent.df[, category.col])
        
        # Set resonable default limits and ticks if NULL
        if(is.null(y.limits) || is.null(y.ticks)) {
                y.limits = c(min(boxplot.df$RESPONSE, na.rm= TRUE),
                             max(boxplot.df$RESPONSE, na.rm= TRUE)); 
                y.ticks <- pretty(boxplot.df$RESPONSE)
                message("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
        }
        
        whiskers <- get.whiskers(dframe = boxplot.df)
        boxplot.df$cat.lab <- (boxplot.df$CATEGORY)
        levels(boxplot.df$cat.lab)<- paste(levels(boxplot.df$CATEGORY),"\n n = ", table(boxplot.df$CATEGORY))
        
        p1 <- ggplot(boxplot.df, 
                     aes(x = CATEGORY, y = RESPONSE, shape = factor(CATEGORY), fill = factor(CATEGORY))) + 
                geom_boxplot(outlier.size = 2, outlier.colour = alpha("black", 0.2)) + 
                geom_point(aes(x = CATEGORY, y = mean(RESPONSE), shape = CATEGORY), size = 3, bg = "white") + 
                stat_summary(fun = mean, geom = "point", 
                             shape = shape.palette, 
                             size = 3, bg = "white") + 
                scale_shape_manual(values = shape.palette) + 
                scale_fill_manual(values = category.palette, name = category.label) +
                scale_x_discrete(breaks = levels(boxplot.df$CATEGORY), 
                                 labels = paste(levels(boxplot.df$CATEGORY),"\n n = ", whiskers$N)) + 
                scale_y_continuous(limits = y.limits, breaks = y.ticks, labels = fmt(y.digits)) + 
                geom_segment(data = whiskers, aes(x = as.numeric(CATEGORY) - 0.1, 
                                                  y = LOWER, 
                                                  xend = as.numeric(CATEGORY) + 0.1, 
                                                  yend = LOWER)) + 
                geom_segment(data = whiskers, aes(x = as.numeric(CATEGORY) - 0.1, 
                                                  y = UPPER, xend = as.numeric(CATEGORY) + 0.1, 
                                                  yend = UPPER)) + 
                geom_text(data = whiskers, aes(x = as.numeric(CATEGORY) - 0.5, 
                                               y = MEDIAN, label = format(round(MEDIAN, 2))), 
                          size = text.size) + 
                scale_colour_manual(values = category.palette) + 
                guides(colour = "none") + 
                labs(y = y.label, x = category.label, fill = category.label, shape = category.label)+
                theme(legend.position = "bottom")
        return(p1)
}

# cdf.plot ------
#' @title cdf.plot
#' @description A function for creating harmonized ggplot2 cumulative distribution plots. Statistics computed by stat_ecdf().
#' @inheritParams graphic.params
#' @return A ggplot object is returned.
#' @examples 
#' {
#' data(demog.data)
#' cdf.plot(parent.df= demog.data, 
#'   category.col = "SEX",
#'   category.label   = "Gender",
#'   response.col = "BMI", 
#'   x.label = expression(paste("BMI (m/kg",phantom()^2,")")), 
#'   x.limits=c(0,60),
#'   x.ticks=seq(0,60,5),
#'   y.label = "Percentage of Subjects", 
#'   y.limits= c(0,1),
#'   y.ticks = seq(0,1,.2),
#'   line.size =.75,
#'   category.palette =c("red", "blue")
#'   )
#' }
#' @author Greg Cicconetti
cdf.plot <-
  function (parent.df, 
      category.col,
      category.label,
      response.col, 
      x.label = "", 
      x.limits=NULL,
      x.ticks=NULL,
      y.label = "", 
      y.limits= c(0,1),
      y.ticks = seq(0,1,.2),
      line.size =.75,
      category.palette =c("red", "blue"))
{
    names(parent.df) <- toupper(names(parent.df))
  
    RESPONSE <- CATEGORY <- NULL
    cdf.df <- data.frame(
      RESPONSE= parent.df[, response.col],
      CATEGORY= parent.df[, category.col])
    
    # Set reasonable limits & ticks if NULL
    if(is.null(x.limits) || is.null(x.ticks)) {
      x.limits = c(min(cdf.df$RESPONSE, na.rm= TRUE),
       max(cdf.df$RESPONSE, na.rm= TRUE)) 
      x.ticks <- pretty(cdf.df$RESPONSE)
      message("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
    }
    
    if(is.null(y.limits) || is.null(y.ticks)) {
      y.limits = c(0,1) 
      y.ticks <- seq(0,1,.1)
      message("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
    }
    
    p1 <- ggplot(data = cdf.df, aes(x = RESPONSE, colour = CATEGORY, linetype = CATEGORY)) + 
      stat_ecdf(size = line.size) + 
      scale_y_continuous(labels = percent_format()) + 
      scale_x_continuous(limits=x.limits, breaks=x.ticks) +
      labs(x = x.label, y = y.label, colour = category.label, linetype = category.label) + 
      scale_colour_manual(values = category.palette) +
      theme(legend.position= "bottom")
    return(p1)
  }

# dot.plot ------
#' @title dot.plot
#' @description A function for creating harmonized ggplot2 dot plots with compatiability with table.plot and forest.plot. 
#' @return A ggplot object is returned.
#' @inheritParams graphic.params
#' @author Greg Cicconetti
dot.plot <- function (parent.df = dot.df.melt, category.col = "Treatment", 
          y.rank.col = "rank", y.label.rank.col = "label.rank", y.label.col = "subgroup", 
          Point.Est = "percent", x.limits = c(0, 1),
          x.ticks = seq(0, 1, 0.2),
          y.limits = NULL, shape.palette = c(16, 17), 
          x.label = "Estimate", y.label = "Item",
          category.palette = c("red", "blue")) {
dot.df.melt <- RANK <- POINT.EST <- CATEGORY <- NULL
  
  names(parent.df) <- toupper(names(parent.df))
  if (is.null(y.limits)) {
    y.limits = c(min(parent.df[, y.rank.col], na.rm = T) - 
             0.25, max(parent.df[, y.rank.col], na.rm = T) + 0.25)
    message("y.limits are set to NULL; defaults are used.\n")
  }
  if (is.null(x.limits) || is.null(x.ticks)) {
    x.limits = c(0, 1)
    x.ticks <- seq(0, 1, 0.1)
    message("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
  }
  dotplot.df <- data.frame(CATEGORY = parent.df[, category.col], 
         RANK = parent.df[, y.rank.col], 
         POINT.EST = parent.df[,   Point.Est],
         LABEL.RANKS = parent.df[, y.label.rank.col], 
         LABEL.VALUES = parent.df[, y.label.col])
  for.return <- ggplot(data = dotplot.df, aes(y = RANK, x = POINT.EST, 
                shape = CATEGORY, colour = CATEGORY)) + 
    geom_point(size = 2.75,    alpha = 0.7) + 
    scale_shape_manual(values = shape.palette) + 
    labs(x = x.label, y = y.label) + scale_colour_manual(values = category.palette) + 
    scale_x_continuous(limits = x.limits, breaks = x.ticks, 
           labels = percent_format()) + 
    scale_y_continuous(limits = y.limits, breaks = (dotplot.df$LABEL.RANK), 
           labels = (dotplot.df$LABEL.VALUES))
  return(for.return)
}

# forest.plot ------
#' @title forest.plot
#' @description A function for creating harmonized forest.plots via ggplot2 offering compatiability with table.plot and dot.plot. 
#' @inheritParams graphic.params
#' @return A ggplot object is returned.
#' @author Greg Cicconetti
forest.plot <- 
        function (parent.df, 
                  y.rank.col = "rank",  # this maps the line segments to y-axis
                  Point.Est = "hr",  
                  lower.lim = "low", 
                  upper.lim = "high", 
                  y.label.rank.col = "rank",  #  this identifies the y-axis values for labels
                  y.label.col = "subcategory", # This holds the labels which should sync with y.label.breaks
                  x.label = "Estimate", 
                  y.label = "Item",
                  log.trans = TRUE, 
                  x.limits = c(0.21, 5), 
                  x.ticks = 2^(-2:2), 
                  y.limits=NULL,
                  category.color = "category", # This colors the points and line segments
                  background.palette = c("red", "blue"), 
                  category.palette = c("red", "blue"), 
                  shape.palette = c(16, 16), 
                  flip.palette = FALSE) 
        {
                xmin <- xmax <- ymin <- ymax <- period <- POINT.EST <- RANK <- CATEGORY <- LOWER.LIM <- UPPER.LIM <- NULL
                
                if(is.null(y.limits) ) {
                        y.limits = c(min(parent.df[,y.rank.col], na.rm= TRUE)-.25,
                                     max(parent.df[,y.rank.col], na.rm= TRUE)+.25) 
                        message("y.limits are set to NULL; defaults are used.\n")
                }
                
                # Cap names
                names(parent.df) <- toupper(names(parent.df))
                category.color <- toupper(category.color)
                y.rank.col <- toupper(y.rank.col)
                Point.Est <- toupper(Point.Est)
                lower.lim <- toupper(lower.lim)
                upper.lim <- toupper(upper.lim)
                y.label.rank.col <- toupper(y.label.rank.col)
                y.label.col <- toupper(y.label.col)
                y.label.rank.col <- toupper(y.label.rank.col)
                
                # Break if x.limits or x.ticks are not supplied
                if (is.null(x.limits) || is.null(x.ticks)) {
                        message("Either x.limits or x.ticks are set to NULL; specify.")
                        return()
                }
                
                forest.df <- data.frame(RANK = parent.df[, y.rank.col],
                                        POINT.EST = parent.df[, Point.Est], 
                                        LOWER.LIM = parent.df[, lower.lim], 
                                        UPPER.LIM = parent.df[, upper.lim],
                                        LABEL.RANKS = parent.df[, y.label.rank.col],
                                        LABEL.VALUES = parent.df[, y.label.col],
                                        CATEGORY = parent.df[, category.color])
                
                # Flips the sequence of background colors
                ifelse(flip.palette == FALSE, 
                       rects <- data.frame(xmin = c(x.limits[1], log.trans * 1), 
                                           xmax = c(log.trans * 1, x.limits[2]), 
                                           ymin = c(-Inf, -Inf),
                                           ymax = c(Inf, Inf), 
                                           period = c("A", "B")), 
                       rects <- data.frame(xmin = c(x.limits[1], log.trans * 1), 
                                           xmax = c(log.trans * 1, x.limits[2]), 
                                           ymin = c(-Inf, -Inf), 
                                           ymax = c(Inf, Inf), 
                                           period = c("B", "A")))
                # Add the background rectangles
                # Add the point estimate
                # Add the error bars
                # Manipulate the y-axis
                # Shut the guides off
                # Change the background colors
                # Change the category colors
                # Change the shapes
                # Change the axis titles
                
                for.return <- ggplot() + 
                        geom_rect(data = rects, 
                                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = period), 
                                  alpha = 0.2) + 
                        geom_point(data = forest.df, aes(x = POINT.EST, y = (RANK), color = CATEGORY)) + 
                        geom_errorbarh(data = forest.df, height = 0.2, lwd = .75,
                                       aes(y = RANK, x = POINT.EST, xmin = as.numeric(LOWER.LIM), xmax = as.numeric(UPPER.LIM), 
                                           color = CATEGORY)) + 
                        scale_y_continuous(limits=y.limits,
                                           breaks = (forest.df$LABEL.RANK), 
                                           labels = (forest.df$LABEL.VALUES)) + 
                        guides(color = "none", fill = "none")+  
                        scale_fill_manual(values = background.palette) + 
                        scale_color_manual(values = category.palette) + 
                        scale_shape_manual(values = shape.palette) + 
                        labs(x = x.label, y = y.label) 
                
                # Manipulate the x-axis if necessary
                # Add the reference line
                ifelse(log.trans == TRUE, for.return <- for.return + 
                               scale_x_continuous(trans = log_trans(), 
                                                  limits = x.limits, 
                                                  breaks = x.ticks, expand = c(0, 0.001)) + 
                               geom_vline(xintercept = log.trans * 1), 
                       for.return <- for.return + 
                               scale_x_continuous(limits = x.limits, 
                                                  breaks = x.ticks, expand = c(0, 0.001)) +
                               geom_vline(xintercept = log.trans * 1))
                
                for.return$data <- forest.df
                return(for.return)
        }

# gcurve ------
#' @title gcurve 
#' @description A function to exploit base R's curve function.  This returns a data.frame holding x and y values returned from a call to curve, but suppress the plotting of that function
#' @inheritParams graphic.params
#' @param to the range over which the function will be plotted.
#' @param ylab inherited from curve
#' @param log inherited from curve
#' @param expr inherited from curve
#' @param from inherited from curve
#' @param n inherited from curve
#' @param add inherited from curve
#' @param type inherited from curve
#' @param xname inherited from curve
#' @param xlab inherited from curve
#' @param xlim inherited from curve
#' @param ... inherited from curve
#' @seealso graphics::curve
#' @return A data.frame is returned. Columns include x, y, and optionally category.
#' @examples 
#' {
#' require(ggplot2)
#' curve(dnorm(x, mean=0, sd=1), from=-4, to = 4, n= 1001)
#' ggplot(gcurve(expr = dnorm(x, mean=0, sd=1),from=-4, to = 4, n= 1001,
#' category= "Standard Normal"), aes(x=x, y=y)) + geom_line()
#' }
#' @author Greg Cicconetti
gcurve <- function (expr, from = NULL, to = NULL, n = 101, add = FALSE, 
        type = "l", xname = "x", xlab = xname, ylab = NULL, log = NULL, 
        xlim = NULL,category=NULL,...) 
{
  sexpr <- substitute(expr)
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name(xname))
  }
  else {
    if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% 
            all.vars(sexpr))) 
      stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", 
              xname), domain = NA)
    expr <- sexpr
  }
  if (dev.cur() == 1L && !identical(add, FALSE)) {
    warning("'add' will be ignored as there is no existing plot")
    add <- FALSE
  }
  addF <- identical(add, FALSE)
  if (is.null(ylab)) 
    ylab <- deparse(expr)
  if (is.null(from) || is.null(to)) {
    xl <- if (!is.null(xlim)) 
      xlim
    else if (!addF) {
      pu <- par("usr")[1L:2L]
      if (par("xaxs") == "r") 
        pu <- extendrange(pu, f = -1/27)
      if (par("xlog")) 
        10^pu
      else pu
    }
    else c(0, 1)
    if (is.null(from)) 
      from <- xl[1L]
    if (is.null(to)) 
      to <- xl[2L]
  }
  lg <- if (length(log)) 
    log
  else if (!addF && par("xlog")) 
    "x"
  else ""
  if (length(lg) == 0) 
    lg <- ""
  if (grepl("x", lg, fixed = TRUE)) {
    if (from <= 0 || to <= 0) 
      stop("'from' and 'to' must be > 0 with log=\"x\"")
    x <- exp(seq.int(log(from), log(to), length.out = n))
  }
  else x <- seq.int(from, to, length.out = n)
  ll <- list(x = x)
  names(ll) <- xname
  y <- eval(expr, envir = ll, enclos = parent.frame())
  #   if (length(y) != length(x)) 
  #     stop("'expr' did not evaluate to an object of length 'n'")
  #   if (isTRUE(add)) 
  #     lines(x = x, y = y, type = type, ...)
  #   else plot(x = x, y = y, type = type, xlab = xlab, ylab = ylab, 
  #       xlim = xlim, log = lg, ...)
  for.return <- data.frame(x = x, y = y)
  if(is.null(category)== FALSE) for.return$category <- factor(category)
  return(for.return)
}

# km.plot ------
#' @title km.plot 
#' @description A function for creating harmonized Kaplan-Meier plots and accompanying At Risk table.
#' @inheritParams graphic.params
#' @return A ggplot object is returned.
#' @examples 
#' {
#' require(ggplot2); require(gridExtra)
#' data(km.data)
#' working.df <- km.data
#' head(working.df)
#' km.M <- km.plot(parent.df = subset(working.df, SEX== "M"),
#'       centime.col = "CENTIME.DAY",
#'       category.col = "TRTGRP",
#'       category.palette = c("red", "blue"),
#'       at.risk.palette = c("red","blue"),              
#'       linetype.palette = c("solid","dotted"), 
#'       y.limits=c(0,.01), 
#'       y.ticks=seq(0,.01,.005), 
#'       x.limits=c(-3,48),
#'       x.ticks=seq(0,48,6))
#' print(km.M[[1]])
#' print(km.M[[2]])
#' grid.arrange(km.M[[1]] + theme(legend.position= "bottom"), km.M[[2]], ncol=1)
#' comeback <- sync.ylab.widths(list(km.M[[1]]+ theme(legend.position= "bottom"), km.M[[2]]))
#' grid.arrange(comeback[[1]] , comeback[[2]], ncol=1)
#' build.page(interior.h = c(.8, .2),
#'       interior.w = c(1),
#'        ncol=1, nrow=2,
#'        interior = list(comeback[[1]], 
#'            comeback[[2]]))
#' }
#' @seealso sync.ylab.widths, nsubj.plot
#' @author Greg Cicconetti
km.plot <- 
  function (parent.df, 
      censor.col = "CENSOR", 
      centime.col = "CENTIME.DAY", 
      category.col = "REGION", 
      category.palette = rainbow(5), 
      at.risk.palette = rainbow(5), 
      category.label = "Treatment Group", 
      nsubj.plot.label= "Number at Risk",
      linetype.palette = 1:6, 
      x.label = "Time Since Randomization", 
      y.label = "Percetage of Subjects", 
      x.limits = c(0, 48), 
      x.ticks = seq(0, 48, 3), 
      y.ticks = seq(0, .01, 0.005), 
      y.limits = c(0, .01), 
      line.size = 0.75,
      fromthetop= FALSE,
      text.size =4) 
{
CATEGORY <- AT.RISK <- XVALUES <- YVALUES <- NULL    
    names(parent.df) <- toupper(names(parent.df))
    
    if(is.null(category.col) == FALSE){
      km.df <- data.frame(CENTIME = parent.df[, centime.col], 
              CENSOR = parent.df[, censor.col], 
              CATEGORY = parent.df[, category.col])
      
      kmfit <- survfit(Surv(km.df$CENTIME, km.df$CENSOR) ~ km.df$CATEGORY)
      ifelse(fromthetop, 
             kmfit.out <- data.frame(XVALUES = summary(kmfit)$time, 
                   YVALUES = summary(kmfit)$surv, 
                   AT.RISK = summary(kmfit)$n.risk, 
                   CATEGORY = factor(summary(kmfit)$strata)),
             kmfit.out <- data.frame(XVALUES = summary(kmfit)$time, 
                   YVALUES = 1 - summary(kmfit)$surv, 
                   AT.RISK = summary(kmfit)$n.risk,
                   CATEGORY = factor(summary(kmfit)$strata))
      )
      # This renames the levels
      levels(kmfit.out$CATEGORY) <- unlist(strsplit(x = levels(kmfit.out$CATEGORY), 
                      split = "="))[seq(2, nlevels(kmfit.out$CATEGORY) * 2, 2)]
      add0 <- ddply(kmfit.out, .(CATEGORY), summarize, AT.RISK = max(AT.RISK))
      
      # Note: Adding values to the data set to ensure the step functions appropriately begin at the points
      # (0,1) or (0,0), resp.
      ifelse(fromthetop, 
{
  add0$XVALUES <- rep(0, nlevels(kmfit.out$CATEGORY))
  ad0$YVALUES <- rep(1, nlevels(kmfit.out$CATEGORY))
},
{
  add0$XVALUES <- rep(0, nlevels(kmfit.out$CATEGORY))
  add0$YVALUES <- rep(0, nlevels(kmfit.out$CATEGORY))
}
      )

add0 <- add0[, c("XVALUES", "YVALUES", "AT.RISK", "CATEGORY")]
kmfit.out <- rbind(kmfit.out, add0)

if(is.null(x.limits) || is.null(x.ticks)) {
  x.limits = c(min(kmfit.out$XVALUES, na.rm= TRUE),
         max(kmfit.out$XVALUES, na.rm= TRUE)); 
  x.ticks <- pretty(kmfit.out$XVALUES)
  message("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
}

if(is.null(y.limits) || is.null(y.ticks)) {
  y.limits = c(min(kmfit.out$YVALUES, na.rm= TRUE),
         max(kmfit.out$YVALUES, na.rm= TRUE)); 
  y.ticks <- pretty(kmfit.out$YVALUES)
  message("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
}

p1 <- ggplot(data = kmfit.out, 
       aes(x = XVALUES, y = YVALUES, 
     colour = CATEGORY, linetype = CATEGORY)) + 
  geom_step(size = line.size) + 
  labs(x = x.label, y = y.label, colour = category.label, 
       linetype = category.label) + 
  scale_x_continuous(limits = x.limits, breaks = x.ticks) + 
  expand_limits(y = 0) + 
  scale_y_continuous(breaks = y.ticks, limits = y.limits, 
         labels = percent_format()) + 
  scale_colour_manual(values = category.palette) + 
  scale_linetype_manual(values = linetype.palette) + 
  guides(colour = guide_legend(category.label), 
         linetype = guide_legend(category.label))

at.risk <- data.frame(XVALUES = summary(kmfit, time = x.ticks)$time, 
          N = summary(kmfit, time = x.ticks)$n.risk, 
          RISK = 1 - summary(kmfit, time = x.ticks)$surv, 
          CATEGORY = summary(kmfit, time = x.ticks)$strata, 
          TRTPOST = as.numeric(summary(kmfit, time = x.ticks)$strata)
)

levels(at.risk$CATEGORY) <- levels(kmfit.out$CATEGORY)

p2 <- nsubj.plot(parent.df = at.risk, 
     category.palette = at.risk.palette, 
     x.label = "Number of Subjects", 
     y.label = nsubj.plot.label, 
     text.size =text.size,
     x.limits = x.limits, 
     x.ticks = x.ticks, 
     x.ticks.labels = rep("", length(x.ticks))
)

return(list(p1, p2, at.risk))}
    if(is.null(category.col)==TRUE){
  
  km.df <- data.frame(CENTIME = parent.df[, centime.col], 
          CENSOR = parent.df[, censor.col])
  
  kmfit <- survfit(Surv(km.df$CENTIME, km.df$CENSOR) ~ 1)
  kmfit
  
  ifelse(fromthetop, 
         kmfit.out <- data.frame(XVALUES = summary(kmfit)$time, 
               YVALUES = summary(kmfit)$surv, 
               AT.RISK = summary(kmfit)$n.risk),
         kmfit.out <- data.frame(XVALUES = summary(kmfit)$time, 
               YVALUES = 1 - summary(kmfit)$surv, 
               AT.RISK = summary(kmfit)$n.risk)
  )  
  # This renames the levels
  # Note: Adding values to the data set to ensure the step functions appropriately begin at the points
  # (0,1) or (0,0), resp.
  ifelse(fromthetop, {
    add0 <- data.frame(XVALUES=0, YVALUES=1, AT.RISK = 0)},
{add0 <- data.frame(XVALUES=0, YVALUES=0, AT.RISK = 0)}
  )
add0 <- add0[, c("XVALUES", "YVALUES", "AT.RISK")]
kmfit.out <- rbind(kmfit.out, add0)

if(is.null(x.limits) || is.null(x.ticks)) {
  x.limits = c(min(kmfit.out$XVALUES, na.rm= TRUE),
         max(kmfit.out$XVALUES, na.rm= TRUE))
  x.ticks <- pretty(kmfit.out$XVALUES)
  message("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
}
if(is.null(y.limits) || is.null(y.ticks)) {
  y.limits = c(min(kmfit.out$YVALUES, na.rm= TRUE),
         max(kmfit.out$YVALUES, na.rm= TRUE)); 
  y.ticks <- pretty(kmfit.out$YVALUES)
  message("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
}

p1 <- ggplot(data = kmfit.out, 
       aes(x = XVALUES, y = YVALUES )) + 
  geom_step(size = line.size) + 
  labs(x = x.label, y = y.label) + 
  scale_x_continuous(limits = x.limits, breaks = x.ticks) + 
  expand_limits(y = 0) + 
  scale_y_continuous(breaks = y.ticks, limits = y.limits, 
         labels = percent_format()) 

at.risk <- data.frame(XVALUES = summary(kmfit, time = x.ticks)$time, 
          N = summary(kmfit, time = x.ticks)$n.risk, 
          RISK = 1 - summary(kmfit, time = x.ticks)$surv ,
          CATEGORY = factor(1)
)
p2 <- nsubj.plot(parent.df = at.risk, 
     category.palette = at.risk.palette, 
     x.label = "Number of Subjects", 
     y.label = nsubj.plot.label,      
     x.limits = x.limits, 
     x.ticks = x.ticks, 
     x.ticks.labels = rep("", length(x.ticks)))
return(list(p1, p2, at.risk))}
  }

# line.plot ------
#' @title line.plot 
#' @description A function for creating harmonized line plots with optional errorbars.
#' @return A ggplot object is returned.
#' @inheritParams graphic.params
#' @author Greg Cicconetti/David Wade
line.plot <- function (parent.df,
           category.palette = c("red","blue"),
           linetype.palette = c("dotted", "blank", "solid","blank"),
           line.size = 0.75,
           shape.palette = c(24, 21),
           x.label = "Visit", 
           y.label = "Response",
           category.label = "Treatment Group", 
           x.limits = NULL,
           x.ticks = NULL, 
           x.ticks.labels = NULL,
           addBars = TRUE, 
           bar.width=1,
           pdval = 0.25,
           x.col = "XVALUES",
           y.col = "YVALUES",
           y.limits = NULL, 
           y.ticks = NULL,
           category.color = "CATEGORY.COLOR",
           category.symbol.col= "CATEGORY.SYMBOL",
           y.digits = 0, 
           ymin.col = "YMIN",
           ymax.col = "YMAX",
           linetype.col = "LTYPE") 
{
XVALUES <- YVALUES <- YMIN <- YMAX <- CATEGORY <- LTYPE <- NULL
  names(parent.df) <- toupper(names(parent.df))
  
  lineplot.df <- data.frame(XVALUES = parent.df[, x.col],
          YVALUES = parent.df[, y.col],
          CATEGORY.COLOR = parent.df[, category.color],
          CATEGORY.SYMBOL = parent.df[, category.symbol.col],
          YMIN = parent.df[,ymin.col],
          YMAX = parent.df[, ymax.col],
          LTYPE = parent.df[,linetype.col])
  
  if (is.null(x.limits) || is.null(x.ticks)) {
    x.limits = c(min(lineplot.df$XVALUES, na.rm = TRUE), max(lineplot.df$XVALUES, 
                      na.rm = TRUE))
    x.ticks <- pretty(lineplot.df$XVALUES)
    x.ticks.labels <- message("Either x.limits or x.ticks are set to NULL; defaults are used.\n")
  }
  if (is.null(y.limits) || is.null(y.ticks)) {
    y.limits = c(min(lineplot.df$YVALUES, na.rm = TRUE), max(lineplot.df$YVALUES, 
                      na.rm = TRUE))
    y.ticks <- pretty(lineplot.df$YVALUES)
    message("Either y.limits or y.ticks are set to NULL; defaults are used.\n")
  }
  pd <- position_dodge(pdval)
  
  # this is the basic line plot
  p1 <- ggplot(data = lineplot.df, aes(x = XVALUES, y = YVALUES, 
               ymin = YMIN, ymax = YMAX, colour = CATEGORY, linetype = LTYPE, 
               shape = CATEGORY)) + geom_line(position = pd, size = line.size)
  
  # this adds the vertical lines representing whatever measure of variability is fed in
  if (addBars == TRUE)     
    p1 <- p1 + geom_errorbar(linetype = "solid", width = bar.width, 
           position = pd, size = 0.2)
  
  # this adds the blank circle symbols onto which the character symbols are superimposed
  p1 <- p1 + geom_point(position=pd, size =4, shape =16) 
  
  # this adds the character symbols that get superimposed on the blank cicrle symbols
  # plus some legends and axis controls 
  # the geom_point colour is the character symbol color
  # p1 <- p1 + geom_point(position = pd, size = 2.4, fill = "white", colour= "black") +
  p1 <- p1 + geom_point(position = pd, size = 2.4,colour= "black") +
    scale_colour_manual(values = category.palette) +
    scale_linetype_manual(values = linetype.palette) + 
    scale_shape_manual(values = shape.palette) + 
    labs(x = x.label, y = y.label, colour = category.label, shape = category.label) + 
    scale_x_continuous(limits = x.limits, breaks = x.ticks,labels = x.ticks.labels) +
    scale_y_continuous(labels = fmt(y.digits)) + 
    guides(linetype = "none")
  return(p1)
}

# nsubj.plot ------
#' @title nsubj.plot 
#' @description A function to create tables to accompany KMs and lineplots
#' @return A ggplot object is returned.
#' @inheritParams graphic.params
#' @author Greg Cicconetti/David Wade
nsubj.plot <- 
  function (parent.df ,
      category.palette = c("red","blue"), 
      x.label = "Number of Subjects", 
      y.label = "Treatment\nGroup",
      text.size = 4,
      x.col= "XVALUES",
      text.col= "N",
      category.color= "CATEGORY",
      x.limits = c(0.5, 18), 
      x.ticks = unique(parent.df$XVALUES), 
      x.ticks.labels = unique(parent.df$XVALUES)
  ) 
{
    
XVALUES <- YVALUES <- CATEGORY.COLOR <- TEXT <- NULL
    if (is.null(x.limits) || is.null(x.ticks)) {
      message("Either x.limits or x.ticks are set to NULL; specify.\n")
      
    }
    names(parent.df) <- toupper(names(parent.df))
    
    table.df <- data.frame(XVALUES = parent.df[,x.col],
               YVALUES = parent.df[,category.color],
               CATEGORY.COLOR = parent.df[,category.color],
               TEXT = parent.df[,text.col])
    
    p1 <- ggplot(data = table.df, aes(x = XVALUES, y = YVALUES, 
              colour = CATEGORY.COLOR, label = TEXT)) +
      geom_text(size = text.size) + 
      labs(y = y.label, x = x.label) + scale_color_manual(values = category.palette) + 
      scale_x_continuous(limits = x.limits, breaks = x.ticks, 
             labels = x.ticks.labels) + guides(color = "none") + 
      theme_table_nomargins() + theme(axis.ticks = element_line(color = "white"), 
              axis.text.x = element_text(color = "white"))
    
    return(p1)}        

# table.plot ------
#' @title table.plot
#' @description A function for creating harmonized table plots with 
#' A function for plotting columns of text in a figure offering compatiability with forest.plot and dot.plot. 
#' @inheritParams graphic.params
#' @return A ggplot object is returned.
#' @param xtick.labs xtick labels
#' @author Greg Cicconetti
table.plot <- function(
                parent.df ,
                y.rank.col= "Subcategory",
                category.color= "Treatment",
                text.col1 = "Point_Est",
                text.col2 = NULL,
                text.col3 = NULL,
                text.col4 = NULL,
                text.size = 12,
                xtick.labs = c("", "", ""),
                x.limits=NULL,
                y.limits=NULL,
                x.label= "Text",
                y.label= "Item",
                y.label.rank.col = "rank",  #  this identifies the y-axis values for labels
                y.label.col = "subcategory", 
                category.palette = c("red", "blue")){
        
        CATEGORY <- RANK <- TEXT.COL1 <- TEXT.COL2 <- TEXT.COL3 <- TEXT.COL4 <- NULL
        if(is.null(y.limits) ) {
                y.limits = c(min(parent.df[,y.rank.col], na.rm= TRUE)-.25,
                             max(parent.df[,y.rank.col], na.rm= TRUE)+.25) 
                message("y.limits are set to NULL; defaults are used.\n")
        }
        if(is.null(x.limits) ) {
                x.limits = 1:(4-sum(c(is.null(text.col4),is.null(text.col3),is.null(text.col2))))
                message("x.limits are set to NULL; defaults are used.\n")
        }
        # Cap names
        names(parent.df) <- toupper(names(parent.df))
        category.color <- toupper(category.color)
        y.rank.col <- toupper(y.rank.col)
        text.col1 <- toupper(text.col1)
        if(is.null(text.col2)== FALSE) text.col2 <- toupper(text.col2)
        if(is.null(text.col3)== FALSE)text.col3 <- toupper(text.col3)
        if(is.null(text.col4)== FALSE) text.col4 <- toupper(text.col4)
        y.label.rank.col <- toupper(y.label.rank.col)
        y.label.col <- toupper(y.label.col)
        y.label.rank.col <- toupper(y.label.rank.col)
        
        table.df <- data.frame(
                RANK = parent.df[, y.rank.col],
                CATEGORY = parent.df[, category.color],
                TEXT.COL1 = parent.df[, text.col1],
                TEXT.COL2 = parent.df[, text.col2],
                TEXT.COL3 = parent.df[, text.col3],
                TEXT.COL4 = parent.df[, text.col4],
                LABEL.RANKS = parent.df[, y.label.rank.col],
                LABEL.VALUES = parent.df[, y.label.col])
        
        for.return <-  ggplot()+
                geom_text(data = table.df,  size =text.size,
                          aes(x = 1,
                              colour = CATEGORY,
                              y = RANK, 
                              label = TEXT.COL1, hjust = 0.5))
        
        if(is.null(text.col2)== FALSE)
                for.return <- for.return +
                geom_text(data = table.df,   size =text.size,
                          aes(x = 2,  
                              colour = CATEGORY, 
                              y = RANK, 
                              label = TEXT.COL2, hjust = 0.5))
        
        if(is.null(text.col3)== FALSE)
                for.return <- for.return +
                geom_text(data = table.df,   size =text.size,
                          aes(x = 3,   
                              colour = CATEGORY, 
                              y = RANK, 
                              label = TEXT.COL3, hjust = 0.5))
        
        if(is.null(text.col4)== FALSE)
                for.return <- for.return +
                geom_text(data = table.df,  size =text.size,
                          aes(x = 4,   
                              colour = CATEGORY, 
                              y = RANK, 
                              label = TEXT.COL4, hjust = 0.5))
        
        for.return <- for.return +
                scale_x_continuous(limits = c(0.5, 4.5 -(is.null(text.col2)+
                                                                 is.null(text.col3)+
                                                                 is.null(text.col4))), 
                                   breaks=1:(4 -(is.null(text.col2)+
                                                         is.null(text.col3)+
                                                         is.null(text.col4))), labels=xtick.labs) + 
                scale_y_continuous(limits=y.limits,
                                   breaks=table.df$LABEL.RANKS, 
                                   labels=table.df$LABEL.VALUES)+
                guides(size ="none", color="none")+
                labs(x=x.label, y=y.label)+
                scale_color_manual(values=rev(category.palette))+
                theme(plot.background = element_rect(colour = "white"), 
                      panel.background = element_rect(fill = "white", colour = NA), 
                      axis.text.x = element_text(vjust = 1, colour = "black"), 
                      axis.ticks.x = element_line(colour = "transparent"), 
                      axis.ticks.y = element_line(color= "transparent"),
                      panel.grid.minor = element_line(colour = "white", size = 0.25)
                )
        
        return(for.return)
}

# Global functions for figuRes2 


#  Session Starters -------------------------------------

# default.settings -------
#' @title default.settings 
#' @description Global Defaults
#' @details Global Defaults
#' @return This function assigns character string objects to the global environment.
#' @inheritParams graphic.params
#' @section Value: 
#' \describe{
#' The following are assigned to global environment upon calling:
#'  \itemize{
#'  \item my.path
#'  \item dd
#'  \item cd
#'  \item od
#'  \item blankPanel 
#'  \item page.width 
#'  \item page.height 
#'  \item right.margin 
#'  \item left.margin 
#'  \item top.margin 
#'  \item bottom.margin 
#'  \item graph.region.h
#'  \item graph.region.w
#' }
#' }
#' @author Greg Cicconetti
default.settings <- function(pos = 1,
            envir = as.environment(pos),
            my.path=getwd(),
            main.theme = "theme_bw",
            page.width = 11,
            page.height = 8.5,
            right.margin = .75,
            left.margin = .75,  
            top.margin = 1.4-.5,
            bottom.margin = 1.75-.5)
{
  # Directories
  # create objects in the global environment for path and subdirectories
  assign("my.path", my.path, envir = envir)
  assign("dd", paste0(my.path, "/dddata/"), envir = envir)
  assign("cd", paste0(my.path, "/code/"), envir = envir)
  assign("od", paste0(my.path, "/output/"), envir = envir)
  assign("logd", paste0(my.path, "/log/"), envir = envir)
  
  # create objects in the global environment for page specifications
  assign("page.width", 11 , envir = envir)
  assign("page.height", 8.5, envir = envir)
  assign("right.margin", .75, envir = envir)
  assign("left.margin", .75, envir = envir)
  assign("top.margin", 1.4-.5, envir = envir)
  assign("bottom.margin", 1.75-.5, envir = envir)
  
  # Offer feedback:
  message("my.path is set to:", my.path, "\n")
  message("dd is set to:", dd, "\n")
  message("cd is set to:", cd, "\n")
  message("od is set to:", od, "\n")
  message("logd is set to:", logd, "\n")
  assign("graph.region.h", page.height - (top.margin + bottom.margin), envir = envir)
  assign("graph.region.w", page.width - (right.margin + left.margin), envir = envir)
  assign("blankPanel", grid.rect(gp=gpar(col= "white"), draw=FALSE), envir = envir)
  
  message(paste0("The default page dimension is set to landscape: ", 
      page.width, " inches wide by ", page.height, " inches tall.\n",
      "The default page left and right page margins: ",
      left.margin, " inches and ", right.margin  ," inches, respectively.\n",
      "The default top an bottom margins: ", 
      top.margin, " inches and ", bottom.margin, " inches, respectively.\n",
      "The region available for graphics/tables is ", graph.region.w, " inches wide by ", graph.region.h, " inches tall."))
  
  do.call("theme_set", list(do.call(main.theme, list())))
  message(paste("\nThe default theme:", main.theme, "\n"))
}

# Custom Themes -------

# theme_grey2_nomargins -------
#' @title figuRes2 themes
#' @description Adapts theme_grey() found in ggplot2 
#' @details axis.text colour changed from "grey50" to "black"; legend.position changed from "right" to "bottom"; legend.direction changed to "horizontal"; plot.margin changed from default unit(c(1, 1, 0.5, 0.5), "lines") to unit(c(0, 0, 0, 0), "in")
#' @inheritParams graphic.params
#' @return The returns a function that can be passed to ggplot2::theme_set
#' @examples
#' {
#' ggplot2::theme_set(theme_grey2_nomargins())
#' }
#' @author Greg Cicconetti
theme_grey2_nomargins <-  function (base_size = 12, base_family = ""){
  theme(line = element_line(colour = "black", 
          size = 0.5, 
          linetype = 1, 
          lineend = "butt"), 
        rect = element_rect(fill = "white",
          colour = "black", 
          size = 0.5, 
          linetype = 1), 
        text = element_text(family = base_family,
          face = "plain", 
          colour = "black", 
          size = base_size, 
          hjust = 0.5, 
          vjust = 0.5, 
          angle = 0, 
          lineheight = 0.9), 
        axis.text = element_text(size = rel(0.8),  colour = "black"), 
        strip.text = element_text(size = rel(0.8)), 
        axis.line = element_blank(), 
        axis.text.x = element_text(vjust = 1), 
        axis.text.y = element_text(hjust = 1), 
        axis.ticks = element_line(colour = "grey50"), 
        axis.title.x = element_text(), 
        axis.title.y = element_text(angle = 90, vjust=1), 
        axis.ticks.length = unit(0.15, "cm"), 
        axis.ticks.margin = unit(0.1, "cm"), 
        legend.background = element_rect(colour = NA), 
        legend.margin = unit(0.2, "cm"), 
        legend.key = element_rect(fill = "grey95",   colour = "white"), 
        legend.key.size = unit(1.2, "lines"), 
        legend.key.height = NULL, 
        legend.key.width = NULL, 
        legend.text = element_text(size = rel(0.8)), 
        legend.text.align = NULL, 
        legend.title = element_text(size = rel(0.8), 
            face = "bold", 
            hjust = 0), 
        legend.title.align = NULL, 
        legend.position = "bottom",   
        legend.direction = "horizontal",  
        legend.justification = "center", 
        legend.box = NULL, 
        panel.background = element_rect(fill = "grey90", 
                colour = NA), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "grey95", size = 0.25), 
        panel.margin = unit(0.25, "lines"), 
        strip.background = element_rect(fill = "grey80", 
                colour = NA), 
        strip.text.x = element_text(), 
        strip.text.y = element_text(angle = -90), 
        plot.background = element_rect(colour = "white"), 
        plot.title = element_text(size = rel(1.2)), 
        panel.margin.x = NULL, 
        panel.margin.y = NULL,
        plot.margin = unit(c(0, 0, 0, 0), "in"), 
        complete = TRUE)
}

# theme_grey2_default_margins -------
#' @describeIn theme_grey2_nomargins Same as theme_grey2_nomargins but with margins set to ggplot defaults, unit(c(1, 1, 0.5, 0.5), "lines")
theme_grey2_default_margins <-  function (base_size = 12, base_family = ""){
  theme(line = element_line(colour = "black", 
          size = 0.5, 
          linetype = 1, 
          lineend = "butt"), 
        rect = element_rect(fill = "white",
          colour = "black", 
          size = 0.5, 
          linetype = 1), 
        text = element_text(family = base_family,
          face = "plain", 
          colour = "black", 
          size = base_size, 
          hjust = 0.5, 
          vjust = 0.5, 
          angle = 0, 
          lineheight = 0.9), 
        axis.text = element_text(size = rel(0.8),  colour = "black"), 
        strip.text = element_text(size = rel(0.8)), 
        axis.line = element_blank(), 
        axis.text.x = element_text(vjust = 1), 
        axis.text.y = element_text(hjust = 1), 
        axis.ticks = element_line(colour = "grey50"), 
        axis.title.x = element_text(), 
        axis.title.y = element_text(angle = 90, vjust=1), 
        axis.ticks.length = unit(0.15, "cm"), 
        axis.ticks.margin = unit(0.1, "cm"), 
        legend.background = element_rect(colour = NA), 
        legend.margin = unit(0.2, "cm"), 
        legend.key = element_rect(fill = "grey95",   colour = "white"), 
        legend.key.size = unit(1.2, "lines"), 
        legend.key.height = NULL, 
        legend.key.width = NULL, 
        legend.text = element_text(size = rel(0.8)), 
        legend.text.align = NULL, 
        legend.title = element_text(size = rel(0.8), 
            face = "bold", 
            hjust = 0), 
        legend.title.align = NULL, 
        legend.position = "bottom",   
        legend.direction = "horizontal",  
        legend.justification = "center", 
        legend.box = NULL, 
        panel.background = element_rect(fill = "grey90", 
                colour = NA), 
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "grey95", size = 0.25), 
        panel.margin = unit(0.25, "lines"), 
        strip.background = element_rect(fill = "grey80", 
                colour = NA), 
        strip.text.x = element_text(), 
        strip.text.y = element_text(angle = -90), 
        plot.background = element_rect(colour = "white"), 
        plot.title = element_text(size = rel(1.2)), 
        panel.margin.x = NULL, 
        panel.margin.y = NULL,
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"), 
        complete = TRUE)
}

# theme_grey2_nomargins -------
#' @describeIn theme_grey2_nomargins Similar to theme_grey2
theme_bw2_nomargins <- function (base_size = 12, base_family = "") 
{
  theme_grey2_nomargins(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8)), axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), panel.background = element_rect(fill = "white", 
                            colour = NA), panel.border = element_rect(fill = NA, 
                                        colour = "grey50"), panel.grid.major = element_line(colour = "grey90", 
                                                  size = 0.2), panel.grid.minor = element_line(colour = "grey98", 
                                                                 size = 0.5), strip.background = element_rect(fill = "grey80", 
                                                                          colour = "grey50"))
}

#' @describeIn theme_grey2_nomargins Similar to theme_bw_nomargins but with margins set to ggplot defaults, unit(c(1, 1, 0.5, 0.5), "lines")
theme_bw2_default_margins <- function(base_size = 12, base_family = ""){
  theme_grey2_default_margins(base_size = base_size, base_family =base_family) %+replace%
    theme(axis.text = element_text(size = rel(0.8)), 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey50"), 
          panel.grid.major = element_line(colour = "grey90", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5), 
          strip.background = element_rect(fill = "grey80", colour = "grey50"),
          plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"))
}

#' @describeIn theme_grey2_nomargins alteration to theme_grey
theme_table_nomargins <- function (base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.text = element_text(size = rel(0.8)), 
          axis.ticks = element_line(colour = "black"), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "white"), 
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "white", size = 0.5), 
          strip.background = element_rect(fill = "white", colour = "white", size = 0.2),
          plot.margin = unit(c(0, 0, 0, 0), "lines"))
}

# This file holds functions associated with assembling figures on a page

# Builders  -------

# build.page -------
#' @title build.page
#' @description  Takes page dimensions, figure layout dimensions and an ordered list of grobs/ggplot objects orients them on a page
#' @inheritParams graphic.params
#' @return This writes graphics/grobs to a device.
#' @examples 
#' {
#' # Commenting out calls to pdf and dev.off.
#' # pdf(file = "demonstrating build.page.pdf", width = 11, height = 8.5)
#' build.page(test.dim= TRUE)
#' build.page(interior.w = c(.5, .5), ncol=2, nrow=1, test.dim= TRUE)
#' build.page(interior.h = c(.5, .5), ncol=1, nrow=2, test.dim= TRUE)
#' build.page(interior.h = c(.5, .5), interior.w = c(.5, .5), ncol=2, nrow=2, test.dim= TRUE)
#' build.page(interior.h=c(1/3,1/3,1/3),
#'            interior.w=c(1),
#'            ncol=1, nrow=3,
#'            test.dim=TRUE)
#' build.page(interior.h=c(2, 1, 3)/6,
#'            interior.w=c(.6, .4),
#'            ncol=2, nrow=3,
#'            test.dim=TRUE)
#' build.page(interior.h=c(1/3,1/3,1/3),
#'            interior.w=c(.5, .5),
#'            ncol=2, nrow=3,
#'            test.dim=TRUE,
#'            top.margin=.1,
#'            bottom.margin=.1,
#'            right.margin=.1,
#'            left.margin=.1)
#' 
#' parabola.up <- ggplot2::ggplot(data.frame(x=-10:10, y=(-10:10)^2), ggplot2::aes(x=x,y=y))+
#' ggplot2::geom_line()
#' parabola.down <- ggplot2::ggplot(data.frame(x=-10:10, y=-(-10:10)^2), ggplot2::aes(x=x,y=y))+
#' ggplot2::geom_line()
#' cubic.up <- ggplot2::ggplot(data.frame(x=-10:10, y=(-10:10)^3), ggplot2::aes(x=x,y=y))+
#' ggplot2::geom_line()
#' cubic.down <- ggplot2::ggplot(data.frame(x=-10:10, y=-(-10:10)^3), ggplot2::aes(x=x,y=y))+
#' ggplot2::geom_line()
#' 
#' red.parabola.up <- ggplot2::ggplot(data.frame(x=-10:10, y=(-10:10)^2), ggplot2::aes(x=x,y=y))+
#' ggplot2::geom_line(color="red")
#' red.parabola.down <- ggplot2::ggplot(data.frame(x=-10:10, y=-(-10:10)^2), ggplot2::aes(x=x,y=y))+
#' ggplot2::geom_line(color="red")
#' red.cubic.up <- ggplot2::ggplot(data.frame(x=-10:10, y=(-10:10)^3), ggplot2::aes(x=x,y=y))+
#' ggplot2::geom_line(color="red")
#' red.cubic.down <- ggplot2::ggplot(data.frame(x=-10:10, y=-(-10:10)^3), ggplot2::aes(x=x,y=y))+
#' ggplot2::geom_line(color="red")
#' 
#' 
#' 
#' build.page(interior.h = c(.5, .5), nrow=2, ncol=1,
#'            test.dim= FALSE, interior = list(parabola.up, 
#'                                        parabola.down))
#' 
#' build.page(interior.w = c(.5, .5), nrow=1, ncol=2,
#'            test.dim= FALSE, interior = list(parabola.up, 
#'                                        parabola.down))
#' 
#' build.page(interior.w = c(.5, .5), interior.h = c(.5, .5), nrow=2, ncol=2,
#'            test.dim= FALSE, interior = list(parabola.up, 
#'                                        red.parabola.up,
#'                                        parabola.down, 
#'                                        red.parabola.down
#'                                        ))
#' 
#' build.page(interior.h=c(1/3,1/3,1/3),
#'            interior.w=c(1),
#'            ncol=1, nrow=3,
#'            interior = list(parabola.up, 
#'                            parabola.down,
#'                            cubic.up
#'            ))
#' 
#' build.page(interior.w=c(1/3,1/3,1/3),
#'            interior.h=c(1),
#'            ncol=3, nrow=1,
#'            interior = list(parabola.up, 
#'                            parabola.down,
#'                            cubic.up
#'            ))
#' 
#' build.page(interior.h=c(2, 1, 3)/6,
#'            interior.w=c(.6, .4),
#'            ncol=2, nrow=3,
#'            interior = list(parabola.up, 
#'                 parabola.down,
#'                 cubic.up,
#'                 cubic.down, 
#'                 red.parabola.down,
#'                 red.cubic.down)
#'            )
#' 
#' build.page(interior.h=c(1/3, 1/3, 1/3),
#'            interior.w=c(.5, .5),
#'            ncol=2, nrow=3,
#'            interior = list(parabola.up,
#'                            parabola.down,
#'                            cubic.up,
#'                            cubic.down, 
#'                            red.parabola.down,
#'                            red.cubic.down)
#' )
#' 
#' 
#' build.page(interior.h=c(1/3,1/3,1/3),
#'            interior.w=c(.5, .5),
#'            ncol=2, nrow=3,
#'            top.margin=.1,
#'            bottom.margin=.1,
#'            right.margin=.1,
#'            left.margin=.1,
#'            interior = list(parabola.up,
#'                            parabola.down,
#'                            cubic.up,
#'                            cubic.down, 
#'                            red.parabola.down,
#'                            red.cubic.down))
#' 
#' 
#' # dev.off()
#' 
#' } 
#' @author Greg Cicconetti
build.page <- function (
                interior.h=c(1),
                interior.w=c(1),
                ncol=1, nrow=1,
                interior,
                test.dim=FALSE,
                page.height=8.5,
                page.width=11,
                right.margin=.75,
                left.margin=.75,
                top.margin=1.4-.5,
                bottom.margin=1.75-.5,
                pos = 1,
                envir = as.environment(pos))
{
        if(sum(interior.h) !=1) return("Argument interior.h is not equal to 1.")
        if(sum(interior.w) !=1) return("Argument interior.w is not equal to 1.")
        if(length(interior.h) != nrow || length(interior.w) != ncol) return(message("Check arguments: page.heights/page.widths does not correspond with ncol/nrow."))
        
        page.widths <- unit(c(right.margin, interior.w*(page.width-right.margin-left.margin), left.margin), units= "inches")
        page.heights <- unit(c(top.margin, interior.h*(page.height - top.margin - bottom.margin), bottom.margin), units= "inches")
        
        if (test.dim == TRUE) {
                grid.show.layout(grid.layout(nrow = nrow + 2, ncol = ncol + 2, 
                                             heights = page.heights, widths = page.widths))
                message(paste0(
                        "Your page is a rectangle: ", page.width, " inches wide by ", page.height, " inches tall.\n", 
                        "Your page setup allocates: ", left.margin,  " inches to the left margin.\n",
                        "Your page setup allocates: ", right.margin, " inches to the right margin.\n",
                        "Your page setup allocates: ", top.margin,  " inches to the top margin.\n",
                        "Your page setup allocates: ", bottom.margin, " inches to the bottom margin.\n",
                        "Your page setup allocates: a rectangle, ", page.width-right.margin-left.margin, " inches wide by ", page.height - top.margin - bottom.margin, " inches tall for graphics/tables."
                ))
                
        }
        if(test.dim==FALSE){
                fill.it <- function(int.nrow = nrow, int.ncol = ncol, interior.list = interior, 
                                    heights = page.heights, widths = page.widths) {
                        padded <- list()
                        for (i in 1:((int.ncol + 2) * (int.nrow + 2))) {
                                
                                assign("blankPanel", grid.rect(gp=gpar(col= "white"), draw=FALSE) , envir = envir)
                                
                                padded[[length(padded) + 1]] <- grid.rect(gp=gpar(col= "white"), draw=FALSE)
                        }
                        names(padded) <- rep("blankPanel", (int.nrow + 2) * (int.ncol + 
                                                                                     2))
                        for (i in 1:length(interior.list)) {
                                padded[[((i - 1)%%(int.ncol)) + 2 + ((i - 1)%/%int.ncol + 
                                                                             1) * (int.ncol + 2)]] <- interior.list[[i]]
                                names(padded)[((i - 1)%%(int.ncol)) + 1 + ((i - 1)%/%int.ncol + 
                                                                                   1) * (int.ncol + 2)] <- paste("Plot", i)
                        }
                        padded[[length(padded) + 1]] <- int.ncol + 2
                        names(padded)[length(padded)] <- "ncol"
                        padded[[length(padded) + 1]] <- int.nrow + 2
                        names(padded)[length(padded)] <- "nrow"
                        padded[[length(padded) + 1]] <- heights
                        names(padded)[length(padded)] <- "heights"
                        padded[[length(padded) + 1]] <- widths
                        names(padded)[length(padded)] <- "widths"
                        return(padded)
                }
                args.list <- fill.it(int.nrow = nrow, int.ncol = ncol, interior.list = interior, 
                                     heights = page.heights, widths = page.widths)
                do.call(grid.arrange, args.list)
        }
}



# annotate.page -------
#' @title annotate.page
#' @description Optionally adds up to 4 lines for titles, 3 lines for right and left headers, and 5 lines of footnotes
#' @return Following an application of build.page, this function stamps on meta-data.
#' @inheritParams graphic.params
#' @author Greg Cicconetti
annotate.page <- function (
  page.height = 8.5, 
  page.width = 11, 
  top.margin = 1 - 0.5, 
  bottom.margin = 1 - 0.5, 
  right.margin = 0.75, 
  left.margin = 0.75, 
  foot.size = 10, 
  head.size = 10, 
  title.size = 14, 
  add.fignum = TRUE, 
  fnote.buffer = 0, 
  header.buffer = 0, 
  fignum.buffer = 1, 
  title.buffer = 2, 
  fignum = "1.100", 
  title = list(
    "If ggplot populates title, annotate.page's title argument gets a ", 
    "list of whitespace text strings. If annotate.page is populating titles,", 
    "use whitespaces and newline escape characters in ggplot titles", 
    "to ensure ggplot object is shrunken titles do not stamp over your graphs"), 
  ulh = list(
    "Upper Left Header 1",
    "Upper Left Header 2", 
    "Upper Left Header 3"), 
  urh = list("Upper Right Header 1", 
       "Upper Right Header 2", 
       "Upper Right Header 3"), 
  fnote = list(
    "Footnote1: Up to five lines of footnotes can be annotated.",   
    "Footnote2: Graphic region height can be flexed.",       
    "Footnote3", 
    "Footnote4", 
    "Footnote5: In large-scale production, this may hold file name, time stamp, etc."), 
  override = "", addTime =TRUE) 
{

  if (override == "outputplan") {
    title = list(outputplan$FigureTitle1[i], outputplan$FigureTitle2[i], 
           outputplan$FigureTitle3[i], outputplan$FigureTitle4[i])
    urh = list(outputplan$urh1[i], outputplan$urh2[i], outputplan$urh3[i])
    ulh = list(outputplan$ulh1[i], outputplan$ulh2[i], outputplan$ulh3[i])
    fnote = list(outputplan$fnote1[i], outputplan$fnote2[i], 
           outputplan$fnote3[i], outputplan$fnote4[i], 
           outputplan$fnote5[i])
    fignum = outputplan$FigureNumber[i]
  }
  
  time.stamp <- ifelse(addTime ==TRUE,toupper(format(Sys.time(), tz = "GMT", format = "%d%b%Y %H:%M")),"")
  fnote[[5]] <- paste(fnote[[5]], time.stamp)
  
  if (add.fignum == TRUE & is.null(fignum) == FALSE) {
    grid.text(x = unit((page.width)/2, "inch"), 
        y = unit(page.height - top.margin, "inches") - unit(fignum.buffer, "line"), 
        just = "center", 
        paste("Figure", fignum), 
        gp = gpar(fontsize = title.size))
  }
  grid.text(x = unit(page.width/2, "inch"), 
      y = unit(page.height - top.margin, "inches") - unit(title.buffer, "line"), 
      just = "center", title[[1]], gp = gpar(fontsize = title.size))
  grid.text(x = unit(page.width/2, "inch"), 
      y = unit(page.height - top.margin, "inches") - unit(title.buffer + 1, "line"), 
      just = "center", title[[2]], gp = gpar(fontsize = title.size))
  grid.text(x = unit(page.width/2, "inch"), y = unit(page.height - 
                   top.margin, "inches") - unit(title.buffer + 2, "line"), 
      just = "center", title[[3]], gp = gpar(fontsize = title.size))
  grid.text(x = unit(page.width/2, "inch"), y = unit(page.height - 
                   top.margin, "inches") - unit(title.buffer + 3, "line"), 
      just = "center", title[[4]], gp = gpar(fontsize = title.size))
  grid.text(x = unit(left.margin, "inches"), y = unit(page.height - 
                    top.margin, "inches") - unit(header.buffer, "line"), 
      just = "left", ulh[[1]], gp = gpar(fontsize = head.size))
  grid.text(x = unit(left.margin, "inches"), y = unit(page.height - 
                    top.margin, "inches") - unit(header.buffer + 1, "line"), 
      just = "left", ulh[[2]], gp = gpar(fontsize = head.size))
  grid.text(x = unit(left.margin, "inches"), y = unit(page.height - 
                    top.margin, "inches") - unit(header.buffer + 2, "line"), 
      just = "left", ulh[[3]], gp = gpar(fontsize = head.size))
  grid.text(x = unit(page.width - right.margin, "inches"), 
      y = unit(page.height - top.margin, "inches") - unit(header.buffer, 
                      "line"), just = "right", urh[[1]], gp = gpar(fontsize = head.size))
  grid.text(x = unit(page.width - right.margin, "inches"), 
      y = unit(page.height - top.margin, "inches") - unit(header.buffer + 
                        1, "line"), just = "right", urh[[2]], gp = gpar(fontsize = head.size))
  grid.text(x = unit(page.width - right.margin, "inches"), 
      y = unit(page.height - top.margin, "inches") - unit(header.buffer + 
                        2, "line"), just = "right", urh[[3]], gp = gpar(fontsize = head.size))

# Populates final line of page  
  grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                                                    "inches") + unit(0 + fnote.buffer, "line"), just = "left", 
            fnote[[5]], gp = gpar(fontsize = foot.size))
  if (override == "outputplan") {
          if (outputplan$TableID[i] != "") {
                  grid.text(x = unit(page.width - right.margin, "inch"), 
                            y = unit(bottom.margin, "inches") + unit(0 + 
                                                                             fnote.buffer, "line"), just = "right", outputplan$TableID[i], 
                            gp = gpar(fontsize = foot.size))
          }
  }
  
 # Next to last lines...
  grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                "inches") + unit(1 + fnote.buffer, "line"), just = "left", 
      fnote[[4]], gp = gpar(fontsize = foot.size))
  grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                "inches") + unit(2 + fnote.buffer, "line"), just = "left", 
      fnote[[3]], gp = gpar(fontsize = foot.size))
  grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                "inches") + unit(3 + fnote.buffer, "line"), just = "left", 
      fnote[[2]], gp = gpar(fontsize = foot.size))
  grid.text(x = unit(left.margin, "inch"), y = unit(bottom.margin, 
                "inches") + unit(4 + fnote.buffer, "line"), just = "left", 
      fnote[[1]], gp = gpar(fontsize = foot.size))
}

# ggplot helper functions ----

#' @title check.ggplot.outliers
#' @param plot.object the ggplot object to check
#' @description Reports via cat statements when ggplot windows truncate data
#' @details Used in conjunction with log files created with start_session_log
#' @author David Wade
check.ggplot.outliers <- function(plot.object=NULL) {
        if (class(plot.object)[1]=="gg") {
        message(paste("Checking ggplot object for out of range points in",length(plot.object$layers),"layer(s):\n"))
        # identify the x and y axis limits 
        for (scale.i in seq(1:length(plot.object$scales$scales))){
                if (plot.object$scales$scales[[scale.i]]$aesthetics[1]=="y"){
                        scale.y.min <- plot.object$scales$scales[[scale.i]]$limits[1]
                        scale.y.max <- plot.object$scales$scales[[scale.i]]$limits[2]
                        }
                if (plot.object$scales$scales[[scale.i]]$aesthetics[1]=="x"){
                        scale.x.min <- plot.object$scales$scales[[scale.i]]$limits[1]
                        scale.x.max <- plot.object$scales$scales[[scale.i]]$limits[2]
                        }      
                } # end for scale.i loop
        # identify the range of the x and y data points in each layer of the plot
        for (layer.i in seq(1:length(plot.object$layers))){
                layer.name<-plot.object$layers[[layer.i]]$geom$objname
                geom.found<-FALSE
                if (layer.name %in% c("line","point","errorbar")){
                        geom.found<-TRUE
                        # first confirm that these are simple single variable plotting definitions
                        valid.x.variable.found<-TRUE
                        valid.y.variable.found<-TRUE
                        x.name <- as.character(plot.object$layers[[layer.i]]$mapping$x)
                        if (!(length(x.name)==1)){valid.x.variable.found<-FALSE}
                        if (layer.name %in% c("line","point")){
                                y.name <- as.character(plot.object$layers[[layer.i]]$mapping$y)
                                if (!(length(y.name)==1)){valid.y.variable.found<-FALSE}
                                }
                                
                                if (layer.name %in% c("errorbar")){
                                        
                                        ymin.name <- as.character(plot.object$layers[[layer.i]]$mapping$ymin)
                                        
                                        ymax.name <- as.character(plot.object$layers[[layer.i]]$mapping$ymax)
                                        
                                        if (!(length(y.name)==1)){valid.y.variable.found<-FALSE}
                                        
                                        if (!(length(y.name)==1)){valid.y.variable.found<-FALSE}
                                        
                                }
                                
                                
                                
                                if (valid.x.variable.found==TRUE){
                                        
                                        data.x.min <- min(plot.object$data[,x.name],na.rm=TRUE)
                                        
                                        data.x.max <- max(plot.object$data[,x.name],na.rm=TRUE)
                                        
                                }
                                
                                if (valid.x.variable.found==FALSE){message("   Layer ",layer.i," (",layer.name,") has an equation for x so x is not scanned for outliers.\n",sep="")}
                                
                                
                                
                                if (valid.y.variable.found==TRUE){         
                                        
                                        if (layer.name %in% c("line","point")){
                                                
                                                y.name <- as.character(plot.object$layers[[layer.i]]$mapping$y)
                                                
                                                data.y.min <- min(plot.object$data[,y.name],na.rm=TRUE)
                                                
                                                data.y.max <- max(plot.object$data[,y.name],na.rm=TRUE)
                                                
                                        }
                                        
                                        if (layer.name %in% c("errorbar")){
                                                
                                                ymin.name <- as.character(plot.object$layers[[layer.i]]$mapping$ymin)
                                                
                                                ymax.name <- as.character(plot.object$layers[[layer.i]]$mapping$ymax)
                                                
                                                data.y.min <- min(plot.object$data[,ymin.name],na.rm=TRUE)
                                                
                                                data.y.max <- max(plot.object$data[,ymax.name],na.rm=TRUE)
                                                
                                        }
                                        
                                }
                                
                                if (valid.y.variable.found==FALSE){message("   Layer ",layer.i," (",layer.name,") has an equation for y so y is not scanned for outliers.\n",sep="")}
                                
                                
                                
                                if (valid.x.variable.found==TRUE){
                                        
                                        scale.problem.found <- FALSE
                                        
                                        if ( data.x.min < scale.x.min ){message("   figuRes Warning: x point below x scale in layer ",layer.i," (",layer.name,").\n",sep="")
                                                                        
                                                                        scale.problem.found<-TRUE
                                                                        
                                        }
                                        
                                        if ( data.x.max > scale.x.max ){message("   figuRes Warning: x point above x scale in layer ",layer.i," (",layer.name,").\n",sep="")
                                                                        
                                                                        scale.problem.found<-TRUE
                                                                        
                                        }
                                        
                                        if (scale.problem.found==FALSE){message("   No points found beyond x scale limits in layer ",layer.i," (",layer.name,").\n",sep="")}
                                        
                                }
                                
                                
                                
                                if (valid.y.variable.found==TRUE){
                                        
                                        scale.problem.found <- FALSE
                                        
                                        if ( data.x.min < scale.x.min ){message("   figuRes Warning: x point below x scale in layer ",layer.i," (",layer.name,").\n",sep="")
                                                                        
                                                                        scale.problem.found<-TRUE
                                                                        
                                        }
                                        
                                        if ( data.x.max > scale.x.max ){message("   figuRes Warning: x point above x scale in layer ",layer.i," (",layer.name,").\n",sep="")
                                                                        
                                                                        scale.problem.found<-TRUE
                                                                        
                                        }
                                        
                                        if ( data.y.min < scale.y.min ){message("   figuRes Warning: y point below y scale in layer ",layer.i," (",layer.name,").\n",sep="")
                                                                        
                                                                        scale.problem.found<-TRUE
                                                                        
                                        }
                                        
                                        if ( data.y.max > scale.y.max ){message("   figuRes Warning: y point above y scale in layer ",layer.i," (",layer.name,").\n",sep="")
                                                                        
                                                                        scale.problem.found<-TRUE
                                                                        
                                        }
                                        
                                        if (scale.problem.found==FALSE){message("   No points found beyond y scale limits in layer ",layer.i," (",layer.name,").\n",sep="")}
                                        
                                }
                                
                                
                                
                        }  # end if geom is valid statement
                        
                        
                        
                        if (geom.found==FALSE){message("   Layer ",layer.i," (",layer.name,") y axis is not from a geom type that is scanned for outliers.\n",sep="")}
                        
                        
                        
                } # end for layer.i loop   
                
        }   
        
        else {message("   figuRes Error: The object attempted to check was not a ggplot object.\n")}
        
        
        
        message(paste("Checking ggplot object for out of range points has completed.\n\n"))
        
        
        
}
# sync.ylab.widths -------
#' @title sync.ylab.widths
#' @description Aligns the widths of ggplot objects to ensure common plot regions. The maximum length required for y-axis labels among the list is determined and applied to the other plots. This assists in syncing the widths of ggplot objects for the purpose of align figures on a page.
#' @inheritParams graphic.params
#' @return A ggplot object is returned.
#' @param default.length set to 2
#' @author Greg Cicconetti

sync.ylab.widths <- 
  function(gg.list, default.length=2){    
    gtable.list <- list()
    maxWidthList <- list()    
    for(i in 1:length(gg.list)){
      gtable.list[[length(gtable.list)+1]] <- ggplot_gtable(ggplot_build(gg.list[[i]]))
      maxWidthList[[length(maxWidthList)+1]] <- gtable.list[[i]]$widths[2:3]
    }
    
    for (i in 1:length(gg.list)) gtable.list[[i]]$widths[2:3] <- do.call(unit.pmax, maxWidthList)
    
    return(gtable.list)
  }

# get.top.xaxis -------
#' @title get.top.xaxis
#' @description This takes two ggplot objects, steals the bottom x-axis from 2nd object and returns a gtable object with that bottom x-axis per object 1 and top x-axis per object 2
#' @return This function returns a ggplot object. 
#' @inheritParams graphic.params
#' @author Greg Cicconetti
get.top.xaxis <- function(bottom.axis.version, top.axis.version){
  name <- r <- NULL
  # Extract gtable
  g1 <- ggplot_gtable(ggplot_build(bottom.axis.version))
  g2 <- ggplot_gtable(ggplot_build(top.axis.version))
  
  ## overlap the panel of the 2nd plot on that of the 1st plot
  pp <- c(subset(g1$layout, name == "panel", se =t:r))
  g <- gtable_add_grob(g1, 
           g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  # EDIT to have the grid lines align with the lower axis ticks, replace the above line with: 
  # g <- gtable_add_grob(g1, g1$grobs[[which(g1$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  ## steal axis from second plot and modify
  ia <- which(g2$layout$name == "axis-b")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  
  ## switch position of ticks and labels
  ax$heights <- rev(ax$heights)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[2]]$y <- ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm")
  
  ## modify existing row to be tall enough for axis
  g$heights[[2]] <- g$heights[g2$layout[ia,]$t]
  
  ## add new axis
  g <- gtable_add_grob(g, ax, 2, 4, 2, 4)
  
  ## add new row for upper axis label
  g <- gtable_add_rows(g, g2$heights[1], 1)
  g <- gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
  
  return(g)
}

# Helper functions for working with outputplan -------

# refresh.outputplan -------
#' @title Refresh the Output Plan
#' @description Reloads outputplan_study.csv file and applies canonical formatting changes.
#' @details Ensure all columns are read in as character vectors. Ensure all missing entries are replaced with blank character string. Ensure all escape characters for carrige returns are respected. Grabs the 'modified time' from file attributes associated with .csv files named in the outputplan.
#' @return This function returns a data.frame.
#' @inheritParams graphic.params
#' @author Greg Cicconetti
refresh.outputplan <- 
  function (
    loadplan=TRUE,
    filename = "outputplan.csv", pos=1, envir=as.environment(pos)) 
  {
    if(loadplan==TRUE) temp <- read.csv(filename) else temp <-  outputplan
    
    # step through column of outputplan
    # convert columns to text class, use blank space for missing values when reading numeric columns
    # search for \n in FigureTitle column
    for (i in 1:ncol(temp)) {
      temp[, i] <- as.character(temp[, i], na = "")
      temp[, i][is.na(temp[, i]) == TRUE] <- ""
    }
    
    # Replace 
    temp$FigureTitle <- gsub(pattern = "\\n", x = temp$FigureTitle, 
           replacement = "\n", fixed = TRUE)
    temp$FigureTitle <- gsub(pattern = "COMMA", x = temp$FigureTitle, 
           replacement = ",", fixed = TRUE)
    temp$TableID <- gsub(pattern = "COMMA", x = temp$TableID, 
             replacement = ",", fixed = TRUE)
    temp$fnote1 <- gsub(pattern = "COMMA", x = temp$fnote1, 
            replacement = ",", fixed = TRUE)
    temp$fnote2 <- gsub(pattern = "COMMA", x = temp$fnote2, 
            replacement = ",", fixed = TRUE)
    temp$fnote3 <- gsub(pattern = "COMMA", x = temp$fnote3, 
            replacement = ",", fixed = TRUE)
    temp$fnote4 <- gsub(pattern = "COMMA", x = temp$fnote4, 
            replacement = ",", fixed = TRUE)
    # Prepare to split FigureTitle Column into constituent title lines
    temp$FigureTitle1 <- temp$FigureTitle2 <- temp$FigureTitle3 <- temp$FigureTitle4 <- ""
    temp$nTitleLines <- 1
    
    
    # Run though rows, identify number of lines needed and populate constituent title lines
    for(i in 1:nrow(temp)){
      if(length(str_split(temp$FigureTitle[i], pattern= "\n")[[1]]) == 1){
        temp$FigureTitle1[i] <- temp$FigureTitle[i]
        temp$nFootLines[i] <- sum(c(temp$fnote1[i]!= "",temp$fnote2[i]!= "",temp$fnote3[i]!= "",temp$fnote4[i]!= ""))
      }
      if(  length(str_split(temp$FigureTitle[i], pattern= "\n")[[1]]) == 2){
        temp$FigureTitle1[i] <- str_split(temp$FigureTitle[i],pattern= "\n")[[1]][1]
        temp$FigureTitle2[i] <- str_split(temp$FigureTitle[i],pattern= "\n")[[1]][2]
        temp$nTitleLines[i] <- 2
        temp$nFootLines[i] <- sum(c(temp$fnote1[i]!= "",temp$fnote2[i]!= "",temp$fnote3[i]!= "",temp$fnote4[i]!= ""))
        
      }
      if(  length(str_split(temp$FigureTitle[i], pattern= "\n")[[1]]) == 3){
        temp$FigureTitle1[i] <- str_split(temp$FigureTitle[i],pattern= "\n")[[1]][1]
        temp$FigureTitle2[i] <- str_split(temp$FigureTitle[i],pattern= "\n")[[1]][2]
        temp$FigureTitle3[i] <- str_split(temp$FigureTitle[i],pattern= "\n")[[1]][3]
        temp$nTitleLines[i] <- 3
        temp$nFootLines[i] <- sum(c(temp$fnote1[i]!= "",temp$fnote2[i]!= "",temp$fnote3[i]!= "",temp$fnote4[i]!= ""))
        
      }
      if(  length(str_split(temp$FigureTitle[i], pattern= "\n")[[1]]) == 4){
        temp$FigureTitle1[i] <- str_split(temp$FigureTitle[i],pattern= "\n")[[1]][1]
        temp$FigureTitle2[i] <- str_split(temp$FigureTitle[i],pattern= "\n")[[1]][2]
        temp$FigureTitle3[i] <- str_split(temp$FigureTitle[i],pattern= "\n")[[1]][3]
        temp$FigureTitle4[i] <- str_split(temp$FigureTitle[i],pattern= "\n")[[1]][4]
        temp$nTitleLines[i] <- 4
        temp$nFootLines[i] <- sum(c(temp$fnote1[i]!= "",temp$fnote2[i]!= "",temp$fnote3[i]!= "",temp$fnote4[i]!= ""))
        
      }
    }
    
    temp$nFootLines <- as.numeric( temp$nFootLines)
    temp$nTitleLines <- as.numeric(temp$nTitleLines)
    assign("outputplan", temp, envir = envir)
    if (any(duplicated(outputplan$output))) 
      message(paste("Note: outputplan has duplicated values in the output column.\nThe outputplan should be edited or subseted to ensure no duplicates.\nCulprits are:", 
          outputplan$output[duplicated(outputplan$output)]), 
          "\n")
    if (any(duplicated(outputplan$rcode))) 
      message(paste("Note: outputplan has duplicated values in the rcode column.\nThe outputplan should be edited or subseted to ensure no duplicates.\nCulprits are:", 
          outputplan$rcode[duplicated(outputplan$rcode)]), 
          "\n")
  }

# Output control -------

# run.specific -------
#' @title run.specific
#' @description This function sources a .r driver file and sends its product to a newly opened 8.5in x 11in screen or a pdf file with 8.5in x 11in dimensions.
#' @inheritParams graphic.params
#' @param dpires passed to devices
#' @param use.log logical to write a log file
#' @return This function passes output to a device, be it the computer screen or to file.
#' @author David wade
run.specific <-  function (source.code = "g_AErr2.r", outfile = "", toPDF= FALSE, toWMF= FALSE, toJPEG= FALSE, toPNG= FALSE, toBMP= FALSE, toEPS= FALSE, dpires=600, use.log=FALSE)  { 
  if (use.log==TRUE) {
    start_session_log(paste0(str_split(string = "g_AErr2.r",pattern = "\\.", n=2)[[1]][1], ".PDF")) }
    
    myError <- NULL    
    
    if (!exists("outputplan"))
      stop("outputplan does not exist in memory.")
    if (!(source.code %in% outputplan$rcode))
      stop("source.code not found within outputplan")
    # CHECK IT: what's point of filename?
    filename <- source.code
    if(exists("od2") == FALSE) od2 <- od
    i <- which(outputplan$rcode == filename)
    if (toPDF == TRUE) {
      if (outfile == "")
        pdf(paste(od, outputplan[i, ]$outputfile, sep = ""),      
            height = 8.5, width = 11)    
      if (outfile != "")  
        pdf(paste(od2, outfile, sep = ""), height = 8.5, width = 11)      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {  
        myError <<- e$message  
        # this first message goes into the R history file  
        message("\n\nfiguRes2 Error: A fatal error ocurred causing the function to stop running\n")  
        message("********************************************************************************\n")  
        message(paste("  The error message was: ",myError))  
        closeAllConnections()  
        # this second message goes to the console to alert the user  
        message("figuRes2 Error: A fatal error ocurred causing the function to stop running")  
        message(paste("\n  The error message was: ",myError))  
      })      
      dev.off()      
    }
    if (toWMF == TRUE) {      
      if (outfile == "") 
        do.call("win.metafile", list(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".wmf",sep = ""),         
                   height = 8.5, width = 11))
      if (outfile != "")  
        do.call("win.metafile", list(paste(od2,unlist(strsplit(outfile,"\\."))[1],".wmf", sep = ""), height = 8.5, width = 11))
      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {  
        myError <<- e$message  
        # this first message goes into the R history file  
        message("\n\nfiguRes2 Error: A fatal error ocurred causing the function to stop running\n")  
        message("********************************************************************************\n")  
        message(paste("  The error message was: ",myError))  
        closeAllConnections()  
        # this second message goes to the console to alert the user  
        message("figuRes2 Error: A fatal error ocurred causing the function to stop running")  
        message(paste("\n  The error message was: ",myError))  
      })      
      dev.off()      
    } 
    if (toJPEG == TRUE) {      
      if (outfile == "")  
        jpeg(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".jpeg",sep = ""),       
             height = 8.5, width = 11, units= "in", res=dpires)      
      if (outfile != "")  
        jpeg(paste(od2,unlist(strsplit(outfile,"\\."))[1],".jpeg", sep = ""), 
             height = 8.5, width = 11, units= "in", res=dpires)      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {  
        myError <<- e$message  
        # this first message goes into the R history file  
        message("\n\nfiguRes2 Error: A fatal error ocurred causing the function to stop running\n")  
        message("********************************************************************************\n")  
        message(paste("  The error message was: ",myError))  
        closeAllConnections()  
        # this second message goes to the console to alert the user  
        message("figuRes2 Error: A fatal error ocurred causing the function to stop running")  
        message(paste("\n  The error message was: ",myError))       
      })      
      dev.off()      
    }    
    if (toPNG == TRUE) {      
      if (outfile == "")  
        png(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".png",sep = ""),      
            height = 8.5, width = 11, units= "in", res=dpires)      
      if (outfile != "")  
        png(paste(od2,unlist(strsplit(outfile,"\\."))[1],".png", sep = ""), 
            height = 8.5, width = 11, units= "in", res=dpires)      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {  
        myError <<- e$message  
        # this first message goes into the R history file  
        message("\n\nfiguRes2 Error: A fatal error ocurred causing the function to stop running\n")  
        message("********************************************************************************\n")  
        message(paste("  The error message was: ",myError))  
        closeAllConnections()  
        # this second message goes to the console to alert the user  
        message("figuRes2 Error: A fatal error ocurred causing the function to stop running")  
        message(paste("\n  The error message was: ",myError))  
      })      
      dev.off()      
    }    
    if (toBMP == TRUE) {      
      if (outfile == "")  
        bmp(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".bmp",sep = ""),      
            height = 8.5, width = 11, units= "in", res=dpires)      
      if (outfile != "")  
        bmp(paste(od2,unlist(strsplit(outfile,"\\."))[1],".bmp", sep = ""), height = 8.5, width = 11, units= "in", res=dpires)      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {  
        myError <<- e$message  
        # this first message goes into the R history file  
        message("\n\nfiguRes2 Error: A fatal error ocurred causing the function to stop running\n")  
        message("********************************************************************************\n")  
        message(paste("  The error message was: ",myError))  
        closeAllConnections()  
        # this second message goes to the console to alert the user  
        message("figuRes2 Error: A fatal error ocurred causing the function to stop running")  
        message(paste("\n  The error message was: ",myError))  
      })      
      dev.off()      
    }   
    if (toEPS == TRUE) {      
      if (outfile == "")  
        postscript(paste(od2,unlist(strsplit(outputplan[i, ]$outputfile,"\\."))[1],".eps",sep = ""),       
             height = 8.5, width = 12, pagecentre = TRUE, horizontal= TRUE, paper= "letter")      
      if (outfile != "")  
        postscript(paste(od2,unlist(strsplit(outfile,"\\."))[1],".eps", sep = ""), 
             height = 8.5, width = 12, pagecentre = TRUE, horizontal= TRUE, paper= "letter")      
      tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {  
        myError <<- e$message  
        # this first message goes into the R history file  
        message("\n\nfiguRes2 Error: A fatal error ocurred causing the function to stop running\n")  
        message("********************************************************************************\n")  
        message(paste("  The error message was: ",myError))  
        closeAllConnections()  
        # this second message goes to the console to alert the user  
        message("figuRes2 Error: A fatal error ocurred causing the function to stop running")  
        message(paste("\n  The error message was: ",myError))  
      })      
      dev.off()      
    }    
    if (toPDF == FALSE & toWMF == FALSE & toJPEG == FALSE & toPNG == FALSE & toBMP == FALSE & toEPS == FALSE) {      
      if (names(dev.cur()) == "windows")  
        tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {    
          myError <<- e$message    
          # this first message goes into the R history file    
          message("\n\nfiguRes2 Error: A fatal error ocurred causing the function to stop running\n")    
          message("********************************************************************************\n")    
          message(paste("  The error message was: ",myError))    
          closeAllConnections()    
          # this second message goes to the console to alert the user    
          message("figuRes2 Error: A fatal error ocurred causing the function to stop running")
          message(paste("\n  The error message was: ",myError))
        })
      else {
        graphics.off()
        do.call("windows", list(height = 8.5, width = 11))
        tryCatch({source(paste(cd, source.code, sep = ""))},error=function(e) {
          myError <<- e$message
          # this first message goes into the R history file
          message("\n\nfiguRes2 Error: A fatal error ocurred causing the function to stop running\n")
          message("********************************************************************************\n")
          message(paste("  The error message was: ",myError))
          closeAllConnections()
          # this second message goes to the console to alert the user
          message("figuRes2 Error: A fatal error ocurred causing the function to stop running")
          message(paste("\n  The error message was: ",myError))
        }) 
      } 
    }

if (use.log==TRUE) {   stop_session_log() }
}

# all_in_one -------
#' @title all_in_one
#' @description Produces a single pdf file with based on rows in the outputplan whose UseSubset column is equals 'Y'. A progress bar is displayed.
#' @details Prerequisites: You need to have output, code, data directory paths defined in your workspace. These should take variable names od, cd, dd, respectively. This can be done by running a personalized set of the following commands:
#' 
#' Code directory needs to hold the .r files associated with the subset of figures to be produced.
#' 
#' Suggest running outputplan.report() first. A progress bar also helps to see run is incomplete. A manual check on the total number of pages in the final pdf should be made.
#' @inheritParams graphic.params
#' @return This function creates a pdf file holding all figures produced based on a subset of the outputplan.
#' @section Value: 
#' \describe{
#' A .pdf file called filename.pdf is deposited in the output directory.
#' }
#' @author Greg Cicconetti
all_in_one <- function (UseSubset = "SAC", filename = "SAC.pdf", reportNR=TRUE) 
{
  if (!exists("outputplan")) 
    stop("outputplan does not exist in memory.")
  if (!(UseSubset %in% names(outputplan))) 
    stop("Subset not defined in outputplan.")
  total <- length(which(outputplan[, UseSubset] == "Y"))
  counter <- 0
  Start.time <- Sys.time()
  
  # Disabled due to CRAN error on Debian
  # pb <- winProgressBar(title = "Example progress bar", label = "0% done", 
  #          width = 500, min = 0, max = total, initial = 0)
  pdf(paste(od, filename, sep = ""), height = 8.5, width = 11)
  
  if(reportNR ==TRUE){
      r<-  ggplot(data=outputplan[outputplan[,UseSubset]== "N",], aes(y=FigureNumber, x=0, label=paste(FigureStatus))) + 
      geom_text(size =4, hjust=0)+
      geom_text(data= outputplan[outputplan[,UseSubset]== "N",], aes(hjust=0, y=FigureNumber, x = .5, label=FigureTitle, size =4))+
      xlim(0, 5)+
      theme_classic()+
      guides(size= "none") + 
      theme(axis.line.x=element_blank(),axis.text.x= element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())
    print(r)
  }
  
  for (j in which(outputplan[, UseSubset] == "Y")) {
    counter <- counter + 1
    info <- paste("Sourcing: ", outputplan[j, ]$rcode, ", file ", 
            counter, " of ", total, " (", round((counter/total) * 
                    100), "% done), Elapsed Time (mins): ", round(difftime(Sys.time(), 
                                 Start.time, units = "mins"), 2), sep = "")
 #   setWinProgressBar(pb, counter, title = paste(filename, "Progress Bar"), label = info)
    source(paste(cd, outputplan$rcode[j], sep = ""))
  }
  dev.off()
 # close(pb)
  elasped <- round(difftime(Sys.time(), Start.time, units = "mins"),  2)
  elasped
}

# Utility Functions -------

# fmt  -------
#' @title fmt
#' @description A function to control number of digits used in graphics.
#' @details This function is used within ggplot, e.g. (scale_y_continuous(labels=fmt(digits=3))) to control the number of digits presented. By default, axis labels will truncate zeros so that labels might read: 0, 2.5, 5, 7.5. Using this will result in labels: 0.0, 2.5, 5.0, 7.5.
#' @param digits number of digits displayed
#' @author Greg Cicconetti
fmt <- function(digits=2){
  function(x) format(x, nsmall = digits, scientific = FALSE)
}

#' @title FacetLabelAdjuster
#' @description This function takes a 'facet wrapped' ggplot and adds axis labels when a rxc grid is incomplete  
#' @references <http://stackoverflow.com/questions/13297155/add-floating-axis-labels-in-facet-wrap-plot>
#' @param x a ggplot object
#' @param pos maintain default
#' @param newpage maintain default
#' @param vp maintain default
#' @return This function returns a ggplot object.
facetAdjust <-
  function (x, pos = c("up", "down"), newpage = is.null(vp), vp = NULL) 
  {
    # ggplot2:::set_last_plot(x) # Not sure if this is needed!!!
    if (newpage) 
      grid.newpage()
    pos <- match.arg(pos)
    p <- ggplot_build(x)
    gtable <- ggplot_gtable(p)
    dims <- apply(p$panel$layout[2:3], 2, max)
    nrow <- dims[1]
    ncol <- dims[2]
    panels <- sum(grepl("panel", names(gtable$grobs)))
    space <- ncol * nrow
    n <- space - panels
    if (panels != space) {
      idx <- (space - ncol - n + 1):(space - ncol)
      gtable$grobs[paste0("axis_b", idx)] <- list(gtable$grobs[[paste0("axis_b", 
                       panels)]])
      if (pos == "down") {
        rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], 
                "]"), gtable$layout$name)
        lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
        gtable$layout[rows, c("t", "b")] <- gtable$layout[lastAxis, 
                      c("t")]
      }
    }
    if (is.null(vp)) {
      grid.draw(gtable)
    }
    else {
      if (is.character(vp)) 
        seekViewport(vp)
      else pushViewport(vp)
      grid.draw(gtable)
      upViewport()
    }
    invisible(list(p, gtable))
  }

# Log functions -------

# start_session_log -------
#' @title start_session_log
#' @description A function to start logging the session history for a graphic driver run 
#' @details Note that the stop_session_log function is used to stop the logging and save the log file.
#' @param outputfile passed to name the session history log file
#' @param x used internally
#' @param ... additional params
#' @return This function works in conjunction with stop_session_log to create a log file.
#' @inheritParams graphic.params
#' @section Value: 
#' \describe{
#' No objects are returned by this function.
#' }
#' @author David Wade
start_session_log<-function(x, outputfile = "example.PDF", pos=1, envir=as.environment(pos), ...)
{
  logd <- log.start.time <- NULL
  # Strip the extension off of outputfile and replace with .rhis
  log.file.name <- paste0(logd,strsplit(outputfile,split = "[.]")[[1]][1],".rhis")
  # start directing all console output to a log file
  # commenting next line per CRAN
  # options(warn = 1) # default setting
  sink(file =log.file.name)
  sink(stdout(), type = "message", append = FALSE, split=FALSE)  
  # print log header information
  message("*******************************************************************\n")
  message("*                 *\n")
  message("* figuRes2 R Graphics System              *\n")
  message("*                 *\n")    
  message("* Graphics session history log file             *\n")
  message("*                 *\n")
  message("*******************************************************************\n")
  message(" \n")
  message(paste("***"," R session history log file: ", log.file.name," ***\n",sep= ""))  
  message(" \n")
  assign("log.start.time", Sys.time(), envir=envir) 
  message(paste("Run time logging commenced at: ",Sys.time(),"\n"))
  message(" \n")
  message(" \n")
  message(" \n")
  # log information about the R session in which the graphics driver was run
  message(" \n")
  message(" \n")
  message("* R Session Information             *\n")     
  message("*******************************************************************\n")
  #x<-alien        # these commands gets logged if executed outside of function but not in it.
  #Sys.info()      # these commands gets logged if executed outside of function but not in it.
  sessionInfo()
  message(" \n") 
}

#' @title stop_session_log
#' @description A function to stop logging the session history for a graphic driver run and save the session history file
#' @details Note that the start_session_log function is used to start the logging, and it must be called first.
#' @return This function works in conjunction with start_session_log to create a log file.
#' @section Value: 
#' \describe{
#' No objects are returned by this function.
#' }
#' @author David Wade
stop_session_log<-function()
{
  log.start.time <- NULL
  message(" \n")
  message(" \n")
  message(" \n")
  message(paste("*** log ended at ", Sys.time(), " ***",sep= ""))
  log.stop.time<-Sys.time()
  log.elapsed.time<-round((log.stop.time-log.start.time))
  message(paste("*** elapsed run time was ",log.elapsed.time," seconds ***",sep= ""))
  # stop directing all console output to a log file
  sink(type = "message")
  sink()
}

# DRIVER DATA SET DOCUMENTATION -------

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver1 
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver2
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver3
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver4
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver5
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver6
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver7
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver8
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver9
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name driver10
#' @docType data
#' @author Greg Cicconetti
NULL

#' This holds lines to a driver file created by the large-scale vignette
#'
#' @name boxplot.driver
#' @docType data
#' @author Greg Cicconetti
NULL

# DATA SET DOCUMENTATION -------

#' This is a dataset structured for building figures using forest.plot
#'
#' @name benrisk2.data
#' @docType data
#' @author Greg Cicconetti
NULL

#' This is a dataset structured for building figures using cdf.plot
#'
#' @name cdf.data
#' @docType data
#' @author Greg Cicconetti
NULL

#' This is a dataset structured for building figures using bar.plot, box.plot, and cdf.plot
#'
#' @name demog.data
#' @docType data
#' @author Greg Cicconetti
NULL

#' This is a dataset structured for building figures using forest.plot
#'
#' @name forest.data
#' @docType data
#' @author Greg Cicconetti
NULL

#' This is a dataset structured for building figures using km.plot
#'
#' @name km.data
#' @docType data
#' @author Greg Cicconetti
NULL

#' This is a dataset structured to facilitate mass figure production
#'
#' @name outputplan
#' @docType data
#' @author Greg Cicconetti
NULL

#' This is a dataset that would need some pre-processing ahead of using line.plot
#'
#' @name raw.lineplot.data
#' @docType data
#' @author Greg Cicconetti
NULL

#' This is a dataset that would need some pre-processing ahead of using line.plot
#'
#' @name summary.lineplot.data
#' @docType data
#' @author Greg Cicconetti
NULL


#' This is a dataset that would need some pre-processing ahead of using line.plot
#'
#' @name category_by_visit
#' @docType data
#' @author Greg Cicconetti
NULL
# PACKAGE DOCUMENTATION -------

#' figuRes2: A package for building and annotating mult-panel figures with application to large scale figure production
#'
#'This package takes the view that a figure is a collection of graphs/tables assembled on a page and optionally annotated with metadata (titles, headers and footers). The steps to figure building can then be chunked as follows: \enumerate{
#'   \item Data importation
#'   \item Data pre-processing
#'   \item Graph/table building (with subsequent processing necessary)
#'   \item Assembling graph/tables on a page
#'   \item Optional annotation to complete the figure
#'   }
#'   
#' The figuRes2 package provides a suite of functions for producing harmonized figures using the ggplot2 packages. Additional ggplot themes are included. The package provides functions to assist with assembling multiple graphics on a page and annotating the page with headers and footnotes.  Functions to facilitate data processing and mass figure production are included.  Data sets are included to demonstrate how the functions work and this document contains a section that walks through the workflow for large scale figure production. 
#' 
#' All graphing functions in this package presume a data.frame is supplied with a specific data structure.  In practice these can be either imported (e.g., as a .csv file) or generated with R (e.g., output of simulation or call to a probability distirbution function). 
#' 
#' Data pre-processing of imported files may be required to ensure the data.frames are organized properly, factors are properly organized and labeled appropriatel, etc. To handle this, the user may wish to author functions to assist with this pre-processing. The demog.data data set and related process.bslchar function provide an example. 
#' 
#' The the build.page function is designed to help visualize how graphics are organized on a page, as well as execute the task. The graphics passed to this function can be created with the functions in this package or by the user. With the former, keep in mind that these are merely functions that facilitate the construction of ggplot objects. 
#' 
#' In the simplest case a figure will consist of a single graphic.
#' 
#' Some figures call for augmenting a graphic with a table (e.g., forest plots, Kaplan-Meier curves). In these cases, the tables are built using either table.plot or nsubj.plot (or again, the user coded ggplot text table). In the case of Kaplan-Meier curves, it is standard practice to arrange the KM curve on top of a table reporting the Number at Risk. Other figures call for juxtaposing two figures. In these cases, the task is either to arrange 2 graphics in a 1 (row) x 2 (col) or a 2 x 1 grid. More generally, the task is to arrange a dashboard of graphics/tables on an nrow x ncol grid and place them on page with predefined margins.
#' 
#' Once the individual graphs/tables have been created for a figure, pre-processing may be required. E.g., there may be a need to align the y-axes when stacking graphics: if Graph A has the longest y-axis tick label, Graph B will need to be adjusted so graphics are aligned when arranging them on a 2 x 1 grid.
#' 
#' When the collection of graphs/tables have been pre-processed, they can be passed to the build.page function. This function requires the user to specify how the row widths and column heights should be specified as well as the order in which to populate the cells of the grid of graphics.
#' 
#' The defaults presume figures are being displayed on an 8.5 inch x 11 inch page, with landscape orientation and margins of 1.5 inches at the top and bottom and 1 inch margins at the left and right. These dimensions provide sufficient room for 2 lines of headers, 4 lines of footnotes and a effective central region for graphs and tables of size (8.5 - 3) inch x (11 - 2) inch.  Generalizing from the defaults is straightforward.  Trial and error will be required to fine tune aesthetic aspects. 
#' 
#' The function annotate.page has been coded to optionally populate with blank entries (helpful when building graphics that don't require annotation and where margins are minimized), dummy entries (helpful in developement phases) or entries coming from a data.frame called outputplan (helpful for mass figure production).  
#' 
#' @docType package
#' @author Greg Cicconetti
#' @name figuRes2
NULL


# NOTES:
# Updating vignette process: update the .rmd file in vignettes. Start new markdown pdf file.  Add bells and whistles and build pdf.  Paste that pdf file with same root name and place in the vignette folder.

# ```{r, results='hide', message =FALSE, warning=FALSE, echo=FALSE}
# require(knitr)
# opts_chunk$set(echo=TRUE, cache =FALSE, warning=FALSE, dev='pdf',
#    message =FALSE, fig.width=11, fig.height=8.5, comment=NA)
# ```

# Paste back to the .rd file in vignettes dir
# Update package with build and reload and iterate as needed - when all's finished to check.  

