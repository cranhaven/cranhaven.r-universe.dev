################################################################################
################################################################################
################################################################################
# correlation for a data.frame
# it check whether thery are factor and only show the
# correlation for continuous variables
################################################################################
################################################################################
################################################################################
#require(reshape2)
################################################################################
################################################################################
################################################################################
resid_Qstats <- function(model, 
                         xvar= NULL,
                     digits = 3,
                       plot = TRUE,
                     method = c("circle","square"),
              outline.color = "gray",
                     colors = c("blue", "white", "red"),
               legend.title = "Z-scores",
                      title,
                    ggtheme = ggplot2::theme_minimal(),
                     tl.cex = 12,
                     tl.col = "black", 
                     tl.srt = 45,
                        lab = TRUE, 
                    lab_col = "black", 
                   lab_size = 3,
              ...) 
{
################################################################################
################################################################################
  # local function 
  meltit <- function(mat)
  {
    rna <- rownames(mat)
    lrna <- length(rna)
    value <- as.vector(mat)
    Var1 <- gl(length(rna), 1, length = lrna*lrna, labels=rna)
    Var2 <- gl(length(rna), lrna, length = lrna*lrna, labels=rna)
    daf <-  na.omit(data.frame(Var1, Var2, value=value)) 
    daf
  }
################################################################################
################################################################################
             CC <- Q.stats(model, xvar=xvar, plot=FALSE, ...)[, 1:4]
             tt <- dim(CC)[1]
             mm <- match("TOTAL Q stats", rownames(CC))
             CC <-  CC[-(mm:tt), ]
          xlab  <- if(is.null(xvar)) "index" else deparse(substitute(xvar))
      txt.title <- if (missing(title))  
paste("Z-statistics from model",deparse(substitute(model)))
              else title  
if (plot==FALSE) return(CC)
          method <- match.arg(method)
            corr <- meltit(CC, na.rm = TRUE)
  colnames(corr) <- c("x_range", "z_scores", "value")
   corr$abs_corr <- abs(corr$value) * 10
               p <- ggplot2::ggplot(data = corr, 
                 mapping = ggplot2::aes_string(x = "z_scores", 
                                               y = "x_range", fill = "value"))
if (method == "square") {
               p <- p + ggplot2::geom_tile(color = outline.color)
  }
else if (method == "circle") {
               p <- p + ggplot2::geom_point(color = outline.color, shape = 21, 
                                            ggplot2::aes_string(size = "abs_corr")) +
                 ggplot2::scale_size(range = c(4, 10)) +
                 ggplot2::guides(size = "none")
}
 #          label <- round(x = CC, digits = digits)               
  p <- p + ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3], 
            mid = colors[2], midpoint = 0, limit = c(-4, 4), space = "Lab",
            name = legend.title)+ggtitle(txt.title)
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme
  }
  else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }
  p <- p + ggplot2::theme(axis.text.x = element_text(angle = tl.srt, 
                vjust = 1, size = tl.cex, hjust = 1), 
                axis.text.y = element_text(size = tl.cex)) + 
                coord_fixed()
  label <- round(x = corr[, "value"], digits = digits)  
  if (lab) {
    p <- p + ggplot2::geom_text(mapping = aes_string(x = "z_scores", 
                                            y = "x_range"), 
                  label = label, color = lab_col, size = lab_size)
  }
  p
}
################################################################################
################################################################################
################################################################################
################################################################################