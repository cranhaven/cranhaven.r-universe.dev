#################################################################
require(grid)
#################################################################
# for only one plot 
boot_coef_one <- function(x, 
                        par = 1, 
                        rug = TRUE, 
                      alpha = 0.2,
                   hist.col = "black", 
                  hist.fill = "white", 
                   line.col = "gray",
                  dens.fill = "#FF6666", title, ...)
{
  #  if (add) {lines(density(x$boot[,par]), ... )}
  #  else {
       da <- data.frame(x=x$boot[,par])
     name <- colnames(x$boot)[par]
txt.title <- if (missing(title))  name
             else title  
      gg <- ggplot2::ggplot(da, ggplot2::aes(x =x)) + 
        ggplot2::geom_density(alpha = alpha, fill = dens.fill) + xlab(name) + 
        ggplot2::ylab("density") + ggtitle(txt.title)+
        ggplot2::geom_vline( xintercept=x$orig.coef[par])+
        ggplot2::geom_vline( xintercept=quantile(x$boot[,par], 
                    c(0.025, .5, .975)), color=line.col)
  if (rug)  gg=gg+ggplot2::geom_rug()
  return(gg)
  #  }
}  
########################################################################
########################################################################
########################################################################
########################################################################  
# to DO : option terms DONE
boot_coef <- function(x, 
                      terms = NULL,   
                   hist.col = "black", 
                  hist.fill = "white", 
                  dens.fill = "#FF6666", 
                      alpha = 0.2,
                       nrow = NULL,
                       ncol = NULL,
             plots.per.page = 9,
                 one.by.one = FALSE,
                      title, ...)
{
  if (!(is(x, "NonParametric.Boot")||is(x, "Bayesian.boot"))) 
    stop("the function needs NonParametric.Boot or Bayesian.Boot objects")
  which.terms <- terms
  da <- if (!is.null(which.terms)) data.frame(x$boot[, which.terms, drop = FALSE])
        else data.frame(x$boot)
  da0 <- if (!is.null(which.terms)) data.frame(t(x$orig.coef)[, which.terms, drop = FALSE])
  else data.frame(t(x$orig.coef))
  names <- names(da)
  GG <- list()
  n.plots <- length(names)    
  for (i in 1:n.plots)
  {
    txt.title <- if (missing(title))  names[i]
    else title  
    da1 <- data.frame(x=da[,i])
    #da2 <- data.frame(x=da0[,i])
    #    data.frame(t(x$orig.coef))
    gg <- ggplot2::ggplot(da1, aes(x = x)) + 
      ggplot2::geom_density(alpha = alpha, fill = dens.fill) + 
      ggplot2::xlab(names[i]) + 
      ggplot2::ylab("density") + 
      ggplot2::ggtitle(txt.title)+geom_rug()+
      ggplot2::geom_vline(xintercept= quantile(da[,i], c(0.025, .5, .975)),
                          color="gray")+
      ggplot2::geom_vline( xintercept=da0[,i])
    GG[[i]] <- gg
  }
  if (one.by.one)
  {
    oask <- devAskNewPage(one.by.one)
    on.exit(devAskNewPage(oask))
    for (i in 1:n.plots) print(GG[[i]])
  } 
  else
  { # multiple plots
################################################################# 
  define_region <- function(row, col){
      viewport(layout.pos.row=row, layout.pos.col=col) }
#################################################################  
    if (n.plots>plots.per.page)
    {
      pages <- ceiling(n.plots/plots.per.page)  
      page <- n.plots%/%plots.per.page
      ppp <- rep(plots.per.page,page) 
      if (n.plots%%plots.per.page != 0) ppp <- c(ppp, n.plots%%plots.per.page)
      if (plots.per.page==9)
      {
        nc <- 3
        nr <- 3
        IJ <- expand.grid(j=1:nc, i=1:nr) 
      } else
      {
        if (is.null(nrow)||is.null(nrow)) stop("the nrow and ncol need to be defined") 
        if (plots.per.page> ncol*nrow) stop("the nrow or ncol has to increase") 
        nc <- ncol
        nr <- nrow
        IJ <- expand.grid(j=1:nc, i=1:nr) 
        IJ <- IJ[1:plots.per.page,]
      }  
      start <- 1
      finish <- ppp[1]
      for (pa in 1:pages)
      {
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(nrow=nr,ncol=nc)))   
        for (p  in start:finish) 
        {
          print(GG[[p]], vp=define_region(IJ$i[p],IJ$j[p]))
        }
        start <- finish +1
        finish <- finish+ppp[pa+1] 
        IJ <- rbind(IJ, IJ)
        oask <- devAskNewPage(ask=TRUE)
        on.exit(devAskNewPage(oask))
      } 
    } else  
    {
      pages <- 1
      ppp <- n.plots%/%pages
      nc  <- nr <- trunc(sqrt(ppp))
      if (nc < 1)  nr <- nc <- 1
      if (nc * nr < ppp) nc <- nc + 1
      if (nc * nr < ppp) nr <- nr + 1 
      IJ <- expand.grid(j=1:nc, i=1:nr)
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(nrow=nr,ncol=nc)))
      for (p  in 1:n.plots) 
      {
        print(GG[[p]], vp=define_region(IJ$i[p],IJ$j[p]))
      }
    }      
  }         
  invisible(GG) 
}  
########################################################################
########################################################################
########################################################################
######################################################################## 
