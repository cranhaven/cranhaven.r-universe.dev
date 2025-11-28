################################################################################
################################################################################
################################################################################
################################################################################
nl_cor1 <- function(x,y, plot=TRUE, robust=FALSE)
{
  if (robust)
  {
    m1 <- gamlss(y~pb(x), trace=FALSE, family=NET)
    m2 <- gamlss(x~pb(y), trace=FALSE, family=NET)
    r1 <- cor(fitted(m1), y)
    r2 <- cor(fitted(m2), x)
  }else
  {  
    m1 <- gamlss(y~pb(x), trace=FALSE)
    m2 <- gamlss(x~pb(y), trace=FALSE)
    r1 <- cor(fitted(m1), y)
    r2 <- cor(fitted(m2), x)
  }
  if (plot) { po <- par(mfrow=c(1,2))
  plot(x,y); lines(fitted(m1)[order(x)]~x[order(x)], col="red")
  plot(y,x); lines(fitted(m2)[order(y)]~y[order(y)], col="red")
  on.exit(par(po))
  }
  list(cor=max(r1,r2), d1=r1, d2=r2 )
}
################################################################################
################################################################################
################################################################################
################################################################################
data_nlcor1 <- function(data,   
                     digits = 3,
                       plot = TRUE,
                   diag.off = FALSE,
              lower.tri.off = FALSE,  
                     method = c("circle","square"),
              outline.color = "gray",
                     colors = c("blue", "white", "red"),
               legend.title = "Corr",
                     title,
                    ggtheme = ggplot2::theme_minimal(),
                     tl.cex = 12,
                     tl.col = "black", 
                     tl.srt = 45,
                        lab = FALSE, 
                    lab_col = "black", 
                    lab_size = 3) 
{
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
  if (missing(data) || NROW(data) <= 1) 
    stop("nothing to do for this model")
  
  dimD <- dim(data)
  daTa <- subset(data, select=ifelse(sapply(data,is.factor)|sapply(data,is.character)==TRUE, FALSE, TRUE))
  Dim <- dim(daTa)
  diffDim  <- dimD[2]-Dim[2]
  if (diffDim > 0)
  {
    warning(cat(diffDim, 'factors have been omited from the data', "\n"))
  }
  cnames <-  names(daTa)
  lcnames <- length(cnames)
  CC <- matrix(0, ncol=lcnames, nrow=lcnames)
  for (i in 1:lcnames)
  {
    for (j in 2:lcnames)
    {

      if (i<j) CC[i,j] <- CC[j,i] <- nl_cor(daTa[,i], daTa[,j], plot=FALSE)$cor
    }
  }  
rownames(CC) <- cnames
colnames(CC) <- cnames
    diag(CC) <- 1
if (diag.off) diag(CC) <- NA
if  (lower.tri.off) CC[lower.tri(CC)] <- NA   
if (plot==FALSE) return(CC)
    method <- match.arg(method)
    corr <- meltit(CC)
    colnames(corr) <- c("var_1", "var_2", "value")
    txt.title <- if (missing(title))  
      paste("Non-linear correlations from data:",deparse(substitute(data)))
    else title  
    corr$abs_corr <- abs(corr$value) * 10
    p <- ggplot2::ggplot(data = corr, 
                mapping = ggplot2::aes_string(x = "var_1", y = "var_2", fill = "value"))
    if (method == "square") {
      p <- p + ggplot2::geom_tile(color = outline.color)
    }
    else if (method == "circle") {
      p <- p + ggplot2::geom_point(color = outline.color, shape = 21, 
                                   ggplot2::aes_string(size = "abs_corr")) +
        scale_size(range = c(4, 10)) +
        guides(size = "none")
    }
    label <- round(x = CC, digits = digits)               
    p <- p + ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3], 
                           mid = colors[2],  midpoint = 0.5, limit = c(0, 1), 
                           space = "Lab",
                          name = legend.title)+
      ggplot2::ggtitle(txt.title)
    if (class(ggtheme)[[1]] == "function") {
      p <- p + ggtheme
    }
    else if (class(ggtheme)[[1]] == "theme") {
      p <- p + ggtheme
    }
    p <- p + ggplot2::theme(axis.text.x = element_text(angle = tl.srt, 
                         vjust = 1, size = tl.cex, hjust = 1), 
                   axis.text.y = ggplot2::element_text(size = tl.cex)) + 
      ggplot2::coord_fixed()
    label <- round(x = corr[, "value"], digits = digits)  
    if (lab) {
      p <- p + ggplot2::geom_text(
                   mapping = ggplot2::aes_string(x = "var_1", y = "var_2"), 
                   label = label, color = lab_col, size = lab_size)
    }
    p
    
}
################################################################################
################################################################################
################################################################################
################################################################################
nl_cor <- function(x,y, plot=FALSE)
{
    df <- data.frame(x,y)
    m1 <- fit_PB(x,y, data=df, plot=FALSE)
    m2 <- fit_PB(y,x, data=df, plot=FALSE)
    r1 <- cor(fitted(m1), y)
    r2 <- cor(fitted(m2), x)
  if (plot) { 
  po <- par(mfrow=c(1,2))
  plot(x,y); lines(fitted(m1)[order(x)]~x[order(x)], col="red")
  plot(y,x); lines(fitted(m2)[order(y)]~y[order(y)], col="red")
  on.exit(par(po))
  }
  cor <- max(r1,r2)
  attr(cor, "r1") <- r1
  attr(cor, "r2") <- r2 
  return(cor)
}
################################################################################
################################################################################
################################################################################
################################################################################
data_nlcor <- function(data,   
                    digits = 3,
                      plot = TRUE,
                  diag.off = TRUE,
             lower.tri.off = FALSE,  
                    method = c("square", "circle"),
             outline.color = "gray",
                    colors = c("blue", "white", "red"),
              legend.title = "Corr",
                     title,
                   ggtheme = theme_minimal(),
                    tl.cex = 12,
                    tl.col = "black", 
                    tl.srt = 45,
                       lab = TRUE, 
                   lab_col = "black", 
                  lab_size = 3) 
{
################################################################################
################################################################################
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
  if (missing(data) || NROW(data) <= 1) 
    stop("nothing to do for this model")
      dimD <- dim(data)
      daTa <- subset(data, select=ifelse(sapply(data,is.factor)|sapply(data,is.character)==TRUE, FALSE, TRUE))
       Dim <- dim(daTa)
  diffDim  <- dimD[2]-Dim[2]
if (diffDim > 0)
  {
    warning(cat(diffDim, 'factors have been omited from the data', "\n"))
  }
    cnames <-  names(daTa)
   lcnames <- length(cnames)
        CC <- matrix(0, ncol=lcnames, nrow=lcnames)
for (i in 1:lcnames)
  {
    for (j in 2:lcnames)
    {
      
      if (i<j) CC[i,j] <- CC[j,i] <- nl_cor(daTa[,i], daTa[,j], plot=FALSE)
    }
  }  
rownames(CC) <- cnames
colnames(CC) <- cnames
    diag(CC) <- 1
if (diag.off) diag(CC) <- NA
if  (lower.tri.off) CC[lower.tri(CC)] <- NA   
if (plot==FALSE) return(CC)
     method <- match.arg(method)
       corr <- meltit(CC)
colnames(corr) <- c("var_1", "var_2", "value")
  txt.title <- if (missing(title))  
    paste("Non-linear correlations from data:",deparse(substitute(data)))
    else title  
corr$abs_corr <- abs(corr$value) * 10
      p <- ggplot2::ggplot(data = corr, 
                 mapping = ggplot2::aes_string(x = "var_1", y = "var_2",
                                               fill = "value"))
if (method == "square") 
  {
      p <- p + ggplot2::geom_tile(color = outline.color)
  }
  else if (method == "circle") {
      p <- p + ggplot2::geom_point(color = outline.color, shape = 21, 
                        ggplot2::aes_string(size = "abs_corr")) +
                        ggplot2::scale_size(range = c(4, 10)) +
                        ggplot2::guides(size = "none")
  }
  label <- round(x = CC, digits = digits)               
      p <- p + ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3], 
                                mid = colors[2],  midpoint = 0.5, limit = c(0, 1), 
                                space = "Lab",
                                name = legend.title)+ggtitle(txt.title)
if (class(ggtheme)[[1]] == "function") {3
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
    p <- p + ggplot2::geom_text(
      mapping = ggplot2::aes_string(x = "var_1", y = "var_2"), 
      label = label, color = lab_col, size = lab_size)
  }
  p
}
################################################################################
################################################################################
################################################################################
################################################################################








