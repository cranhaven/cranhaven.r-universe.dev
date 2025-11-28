################################################################################
################################################################################
################################################################################
################################################################################
fitted_devianceIncr <- function (obj,
                        plot = TRUE,  
                        title, 
                    quan.val = 0.99, 
                    annotate = TRUE, 
                    line.col = "steelblue4",
                   point.col = "darkblue", 
                   annot.col = "white",
                    newdata = NULL ) 
{
################################################################################
# local functions 
gamlss_prep_data <- function (obj, quan.val, newdata) 
  {
  if (is.null(newdata))
  {
    DIncr <- devianceIncr(obj)
    value <- quantile(DIncr, quan.val)
      obs <- seq_len(length(DIncr))
      obs <- obs[obj$weights!=0]
    DIncr <- DIncr[obj$weights!=0]
      out <- data.frame(obs = obs, DIncr = DIncr)
out$color <- ifelse((out$DIncr >= value), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = 
                               c("normal", "outlier"))
 out$txt <- ifelse(out$color == "outlier", out$obs, NA)
  return(out)  
  } else
  {
    DIncr <- devianceIncr(obj, newdata=newdata)
    value <- quantile(DIncr, quan.val)
    obs <- seq_len(length(DIncr))
    out <- data.frame(obs = obs, DIncr = DIncr)
    out$color <- ifelse((out$DIncr >= value), 
                        c("outlier"), c("normal"))
    out$fct_color <- ordered(factor(out$color), levels = 
                               c("normal", "outlier"))
    out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)    
  }  
     
  } 
#####################################################################
#####################################################################
# the main function starts here  
  if (missing(obj))  stop("A GAMLSS fitted object should be used")
  if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
          # N <- obj$N
           d <- gamlss_prep_data(obj, quan.val=quan.val, newdata=newdata) 
           value <- quantile(d$DIncr, quan.val)
  txt.title <- if (missing(title))  paste("Deviance increment of model",deparse(substitute(obj)))
               else title
       obs <-  DIncr <- txt <-  NULL
          f <- d[d$color == "outlier", c("obs", "DIncr")]
colnames(f) <- c("observation", "quan_resid")
# try colors() for different colors
#facet_wrap(~ cut_number(rent$A, 6))
      gg <- ggplot2::ggplot(d, 
            ggplot2::aes(x = obs, y = DIncr, label = txt, ymin = 0, ymax = DIncr)) + 
          ggplot2::geom_linerange(colour = line.col ) + 
        ggplot2::geom_point(shape = 1, colour = point.col  ) + 
        ggplot2::xlab("Observation number") + # working  with facet_wrap 
        ggplot2::ylab("deviance increment") + # working  with facet_wrap 
        ggplot2::ggtitle(txt.title) +  # working  with facet_wrap 
        ggplot2::geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, 
                  family = "serif",
            fontface = "italic", colour = "darkred", na.rm = TRUE)  # working  with facet_wrap 
#   if (no.lines)  suppressWarnings(return(gg))
#  facet_wrap(~ cut_number(rent$A, 6))
  p <- gg + 
    if (annotate) ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.5, 
                                    vjust = (2*value),
             family = "serif", fontface = "italic", colour = annot.col,
             label = paste0("Threshold: abs(", sprintf("%.3f",value ), ")"))
  if (plot) {
    suppressWarnings(return(p))
  }
  else {
    return(list(plot = p, high.obs = f, threshold = value))
  }
}
#resid_plot(r1, no.lines=T)+facet_wrap(~ cut_number(rent$A, 6))
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
model_devianceIncr_diff <-  function(model1, model2, 
                                     against = "index", 
                                         tol = 20, 
                                        data, newdata=NULL)
{
     dd <- devianceIncr(model1, newdata=newdata)-devianceIncr(model2, newdata=newdata)   
  color <- ifelse((abs(dd) >= tol), c("outlier"), c("normal"))  
     N1 <- model1$N
     N2 <- model2$N
if (N1!=N2) stop("The two models should have the same no of obsrvations")
# get the data   
if (is.null(newdata))
  {
    index <- 1:N1 
      txt <- ifelse(color == "outlier", index, NA)
  if (any(grepl("data", names(model1$call)))) 
    {
      DaTa <- if (startsWith(as.character(model1$call["data"]), "na.omit")) 
        eval(parse(text = as.character(model1$call["data"])))
              else get(as.character(model1$call["data"]))
    }
    else if (!missing(data)) DaTa <- data
    else stop("The data argument is needed in obj")
  } else 
  {
     DaTa <- newdata
    index <- 1:dim(newdata)[1]
      txt <- ifelse(color == "outlier", index, NA)
  }
  
       da <- data.frame(DaTa, diff=dd,  col=color, index=index, txt=txt )  
  v.names <- names(da)
     term <- as.character(against) 
      pos <- which(v.names==term)
       gg <- ggplot2::ggplot(data=da, 
             ggplot2::aes(x=da[,pos], y=diff, label=txt, ymin = 0, ymax = diff ))+
         ggplot2::geom_linerange(colour = "steelblue4")+
         ggplot2::geom_point(shape = 1, colour = "steelblue4")+
         ggplot2::geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, 
              family = "serif", fontface = "italic", colour = "darkred", na.rm = TRUE)+
         ggplot2::xlab(term)
  gg
}
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
