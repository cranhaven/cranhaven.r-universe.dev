################################################################################
################################################################################
################################################################################
################################################################################
## NOTE
##  hatvalues.gamlss() which is used  in gamlss_prep_data() can not cope 
##  with poly(x,3) so it advised to use x+x^2+x^3 instead
##  also ~ ga(~s(Fl, A))  nn(~Fl + A + H + loc) tr( ~Fl + A + H + loc)
##  is not working
################################################################################
################################################################################
################################################################################
################################################################################
fitted_leverage <- function (obj, 
                      plot = TRUE,  
                      title, 
               #      value = NULL,
                  quan.val = 0.99, 
                  annotate = TRUE, 
                  line.col = "steelblue4",
                 point.col = "steelblue4", 
                 annot.col = "darkred") 
{
########################################################################
# local functions 
gamlss_prep_data <- function (obj, quan.val) 
  {
      hat <- hatvalues(obj)
    value <-  quantile(hat, quan.val)
 # hat_out <- abs(hat) > value
      obs <- seq_len(length(hat))
      obs <- obs[obj$weights!=0]
      hat <- hat[obj$weights!=0]
      out <- data.frame(obs = obs, hat = hat)
out$color <- ifelse((out$hat >= value), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = 
                           c("normal", "outlier"))
  out$txt <- ifelse(out$color == "outlier", out$obs, NA)
  return(out)
  } 
################################################################################
################################################################################
# the main function starts here  
if (missing(obj))  stop("A GAMLSS fitted object should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
           di <- dim(obj$mu.x)
           va <- 2*di[2]/di[1]          
            d <- gamlss_prep_data(obj, quan.val=quan.val) 
        value <- quantile(d$hat, quan.val)
    txt.title <- if (missing(title))  paste("Linear leverage of model",deparse(substitute(obj)))
               else title
         obs <-  hat <- txt <-  NULL
           f <- d[d$color == "outlier", c("obs", "hat")]
 colnames(f) <- c("observation", "quan_resid")
         gg <- ggplot2::ggplot(d, 
               ggplot2::aes(x = obs, y = hat, label = txt, ymin = 0, ymax = hat)) + 
           ggplot2::geom_linerange(colour = line.col ) + 
           ggplot2::geom_point(shape = 1, colour = point.col  ) + 
           ggplot2::xlab("Observation number") + # working  with facet_wrap 
           ggplot2::ylab("linear leverage") + # working  with facet_wrap 
           ggplot2::ggtitle(txt.title) +  # working  with facet_wrap 
           ggplot2::geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family = "serif",
               fontface = "italic", colour = "darkred", na.rm = TRUE)  
         # working  with facet_wrap 
#   if (no.lines)  suppressWarnings(return(gg))
#  facet_wrap(~ cut_number(rent$A, 6))
  p <- gg + #geom_hline(yintercept = 0.1, colour = "gray") + #not working  with facet_wrap 
     #geom_hline(yintercept = c(value), colour = "red") +  #not working  with facet_wrap 
    if (annotate) ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = value,
             family = "serif", fontface = "italic", colour = annot.col,
             label = paste0("Threshold: abs(", value, ")"))
  if (plot) {
    suppressWarnings(return(p))
  }
  else {
    return(list(leverage = d$hat, index=d$obs, threshold = value, rn2=va))
       }
}
#resid_plot(r1, no.lines=T)+facet_wrap(~ cut_number(rent$A, 6))
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
