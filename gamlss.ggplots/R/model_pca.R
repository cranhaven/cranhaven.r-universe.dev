################################################################################
################################################################################
################################################################################
################################################################################
# plotting different residuals using their first two principal componets
# TO DO : check how to plot differnt componemts
model_pca <- function(obj,..., scale = TRUE, arrow_size = 1.5)
{# what happend for different PC
################################################################################
  # local function
  gamlss_prep_data <- function (obj, ... ) 
  {
    rqres <- obj$residuals
    #  obs <- seq_len(length(rqres))
    #  obs <- obs[obj$weights!=0]
    rqres <- rqres[obj$weights!=0]
    out <- data.frame(rqres = rqres)
    if (length(list(...)) > 0) 
    {
      for (resp in list(...)) 
      {
        res  <- resp[["residuals"]] 
        res  <- res[obj$weights!=0]
        out <- cbind(out, res)
      }
    }
    names(out) <- names
    return(out)    
  }    
  ##############################################################################
  ##############################################################################  
  label <- PC1 <- PC2 <- NULL
  names <- as.character(match.call()[-1])[1:(length(list(...))+1)]
  #  mcall <- mf[c(1L, m)]  
  #  f1 = function(...){return(as.character(match.call())[-1])}
  #  f1
  #  names <- c(deparse(substitute(obj)), deparse(substitute(...)))
  # names <- paste0("R", names)
  if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
  if (length(names)<=1) stop("you need more than two models")
  dfram <- gamlss_prep_data(obj, ...)
  pca_object <- prcomp(dfram, scale = scale)
  #colours <- rev(auditor:::theme_drwhy_colors(length(names(dfram))))
  loadings <- pca_object$rotation
  std_dev <- pca_object$sdev
  arrows <- apply(loadings, 1, function(x, y) {x * y}, std_dev)
  arrows <- data.frame(t(arrows))
  arrows$label <- rownames(arrows)
  arrows2 <- arrows
  arrows2$PC1 <- arrows2$PC2 <- 0
  arrows2 <- rbind(arrows, arrows2)
  gg <- ggplot2::ggplot(data = data.frame(pca_object$x), 
                        ggplot2::aes(x = PC1, y = PC2)) + 
    ggplot2::geom_point(colour = "grey", alpha = 0.75) + 
    ggplot2::geom_hline(aes(yintercept = 0), size = 0.25) + 
    ggplot2::geom_vline(aes(xintercept = 0), size = 0.25) + 
    ggplot2::geom_line(data = arrows2, aes(PC1, PC2, colour = label)) + 
    ggplot2::geom_segment(data = arrows, 
             ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2, colour = label), 
                 size = arrow_size, 
                 arrow = grid::arrow(length = grid::unit(2, "points")), 
                 show.legend = FALSE) +
    ggplot2::ggtitle("Residuals PCA") #+ 
  # scale_color_manual(values = rev(colours), breaks = arrows$label, 
  # 
  #                    guide = guide_legend(nrow = 1)) + theme_drwhy()
return(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################
