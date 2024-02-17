#' Generate Ecological Limit Function (ELF)
#' @description Generate ELF models by supplying a dataframe of richness and stream size data (streamflow or drainage area), a quantile for evaluating the ecological limit, and a breakpoint threshold.
#' @param watershed.df A dataframe of sites with ecological and hydrologic data
#' @param quantile A specified value for the quantile of interest - 0.95 equals the 95th percentile
#' @param breakpt A breakpoint - either user-supplied fixed value or derived using elfgen breakpoint functions bkpt_pwit() or bkpt_ymax
#' @param yaxis_thresh Value used for specifying y-axis max limit
#' @param xlabel Used to overwrite default x-axis label
#' @param ylabel Used to overwrite default y-axis label
#' @return Object containing plot image and dataframe of ELF statistics
#' @import ggplot2
#' @import quantreg
#' @import testit
#' @export elfgen
#' @examples
#' \donttest{
#' # We don't run this example by R CMD check, because it takes >10s
#'
#' watershed.df <- elfdata(watershed.code = '0208020104', ichthy.localpath = tempdir())
#' breakpt <- 500
#' elfgen(
#'    "watershed.df" = watershed.df,
#'    "quantile" = 0.80,
#'    "breakpt" = breakpt,
#'    "xlabel" = "Mean Annual Flow (ft3/s)",
#'    "ylabel" = "Fish Species Richness"
#'    )
#' }
elfgen <- function(watershed.df,quantile,breakpt,yaxis_thresh,xlabel = FALSE,ylabel = FALSE) {

   # DEFAULT breakpt IF NONE SUPPLIED
   if(missing(breakpt)) {
      breakpt <- 500
   }

   watershed.df.raw <- watershed.df

   #RENAME COLUMNS TO HAVE GENERIC NAMES
   colnames(watershed.df)[1] <- "x_var"
   colnames(watershed.df)[2] <- "y_var"
   colnames(watershed.df)[3] <- "watershed"

   full_dataset <- watershed.df
   data <- watershed.df[!(watershed.df$x_var > breakpt),]

   #Prevents: Error in rq.fit.br(x, y, tau = tau, ...) : Singular design matrix (and others)
   if(length(data[,1]) <= 2) {
      stop("Dataset contains fewer than 3 datapoints, insufficient data to complete analysis \n  ... Try using a larger breakpt \n  ... If still unsuccessful, a larger dataset may be required")
   }

   # UPPER SUBSET
   if (has_warning(rq(y_var ~ log(x_var),data = data, tau = quantile)) == TRUE) {
      stop("Unable to characterize upper subset using quantile regression")
   } else {
      upper.quant.data <- rq(y_var ~ log(x_var),data = data, tau = quantile)
   }


   newy <- c(log(data$x_var)*coef(upper.quant.data)[2]+coef(upper.quant.data)[1])
   upper.quant <- subset(data, data$y_var > newy)

   #Prevents: Error in ru$coefficients[2, 1] : subscript out of bound (and others)
   if(length(upper.quant[,1]) <= 2) {
      stop("Upper subset contains fewer than 3 datapoints, insufficient data to complete analysis \n  ... Try using a smaller quantile \n  ... If still unsuccessful, a larger dataset may be required")
   }

   regupper <- lm(y_var ~ log(x_var),data = upper.quant)
   ru <- summary(regupper)

   ruint <- round(ru$coefficients[1,1], digits = 3)                         #intercept
   ruslope <- round(ru$coefficients[2,1], digits = 3)                       #slope of regression
   rurs <- round(ru$r.squared, digits = 3)                                  #r squared of upper quantile
   rursadj <- round(ru$adj.r.squared, digits = 3)                           #adjusted r squared of upper quantile
   rup <- round(ru$coefficients[2,4], digits = 3)                           #p-value of upper quantile
   rucount <- length(upper.quant$y_var)
   subset_n <- length(data$y_var)

   stats.df <- data.frame(
      watershed = watershed.df$watershed[1],
      breakpt = breakpt,
      quantile = quantile,
      m = ruslope,
      b = ruint,
      rsquared = rurs,
      rsquared_adj = rursadj,
      p = rup,
      n_total = length(full_dataset$y_var),
      n_subset = subset_n,
      n_subset_upper = rucount
   )

   # default ymax if none provided
   if (missing(yaxis_thresh)) {
      yaxis_thresh <- max(full_dataset$y_var)
   }

   #Plot titles
   plot_title <- paste("Watershed: ",watershed.df$watershed[1],"\n",sep="");
   flow_title <- colnames(watershed.df.raw[1])
   biometric_title <- colnames(watershed.df.raw[2])
   if (xlabel != FALSE) {flow_title <- xlabel}
   if (ylabel != FALSE) {biometric_title <- ylabel}
   xaxis_title <- paste(flow_title,"\n","\n","m: ",ruslope,"    b: ",ruint,"    r^2: ",rurs,"    adj r^2: ",rursadj,"    p: ",rup,"\n","    Upper ",((1 - quantile)*100),"% n: ",rucount,"    Data Subset n: ",subset_n,"    Full Dataset n: ",length(full_dataset$y_var),sep="");
   yaxis_title <- paste(biometric_title);
   data_upper_legend <- paste("Data Subset (Upper ",((1 - quantile)*100),"%)",sep="");
   reg_upper_legend <- paste("Regression (Upper ",((1 - quantile)*100),"%)",sep="");
   quantile_legend <- paste(quantile," Quantile (Data Subset)",sep="");
   data_lower_legend <- paste("Data Subset (Lower ",(100-((1 - quantile)*100)),"%)",sep="");

   x_var <- NULL # Fixes NOTE: no visible binding for global variable
   y_var <- NULL # Fixes NOTE: no visible binding for global variable

   result <- ggplot(full_dataset, aes(x=x_var,y=y_var)) +

      geom_point(data = full_dataset,aes(x=x_var,y=y_var, color="aliceblue"), na.rm=TRUE) +
      geom_point(data = data,aes(x=x_var,y=y_var, colour="blue"), na.rm=TRUE) +
      stat_smooth(formula = y ~ x, method = "lm",fullrange=FALSE,level = .95, data = upper.quant, aes(x=x_var,y=y_var,color = "red"), na.rm=TRUE) +
      geom_point(data = upper.quant, aes(x=x_var,y=y_var,color = "black"), na.rm=TRUE) +
      geom_quantile(data = data, formula = y ~ x, quantiles= quantile,show.legend = TRUE,aes(x=x_var,y=y_var, color="red"), na.rm=TRUE) +
      geom_smooth(data = upper.quant, formula = y ~ x, method = "lm", show.legend = TRUE, aes(x=x_var,y=y_var,color = "green"),se=FALSE, na.rm=TRUE) +

      ggtitle(plot_title) +

      theme(
         plot.title = element_text(size = 12, face = "bold"),
         axis.text = element_text(colour = "blue"),
         panel.grid.minor.x = element_blank()
      ) +

      labs(x = xaxis_title, y = yaxis_title) +

      scale_x_continuous(trans='log10',
                         limits = c(0.001, 15000),
                         breaks = c(0.001, 0.01, 0.1, 1.0, 10, 100, 1000, 10000),
                         labels = c("0.001", "0.01", "0.1", "1.0", "10", "100", "1,000", "10,000")
      ) +
      scale_y_continuous(limits=c(0,yaxis_thresh))+

      annotation_logticks(sides = "b")+
      theme(legend.key = element_rect(fill = 'white')) +

      scale_color_manual(
         "Legend",
         values=c("gray66","forestgreen","blue","orange","black"),
         labels=c("Full Dataset",data_upper_legend,data_lower_legend,reg_upper_legend,quantile_legend)
         ) +

      guides(colour = guide_legend(
         override.aes = list(
            size = c(1, 1, 1, 1, 1),
            linetype = c(0, 0, 0, 1, 1),
            shape = c(16, 16, 16, NA, NA)
         ),
         label.position = "right"
      ))

   objects <- list("plot" = result, "stats" = stats.df)
   return(objects)

}
