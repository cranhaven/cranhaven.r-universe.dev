# #########################################################################
# #########################################################################
# #########################################################################
# #########################################################################
resid_plots <- function(obj, theme = c("original", "ts", "new", "ecdf"), value=3)
{
# this local function was taken from
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# no author was mentioned  
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) 
  {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    numPlots <- length(plots)
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) 
    {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if (numPlots==1) 
    {
      print(plots[[1]])
    } else 
    {
      # Set up the page
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }  
###################################################################
###################################################################
# main function starts here
theme <- match.arg(theme)
if (theme=="original")
{
  gg1 <- resid_mu(obj, value=value, title="(a) residuals vs fitted values", annotate=FALSE)
  gg2 <- resid_index(obj,  value=value, title="(b) residuals vs index")
  gg3 <- resid_density(obj, title="(c) residuals density")
  gg4 <- resid_qqplot(obj, value=value, title="(d) QQ-plot of residuals") 
  multiplot(gg1,gg3,gg2,gg4, cols=2)
}
if (theme=="ts")
{
  resid <-residuals(obj)
  gg1 <- y_acf(resid)
  gg2 <- y_pacf(resid)
  gg3 <- resid_density(obj, title="(c) residuals density")
  gg4 <- resid_qqplot(obj, value=value, title="(d) QQ-plot of residuals") 
  multiplot(gg1,gg3,gg2,gg4, cols=2)
}
if (theme=="new")
{
  gg1 <- resid_index(obj,  value=value, title="(a) residuals vs index")
  gg2 <- resid_density(obj, title="(b) residual density")
  gg3 <- resid_wp(obj, title="(c) worm plot") 
  gg4 <- resid_dtop(obj, value=value, title="(d) detrended Own's plot")
  multiplot(gg1,gg3,gg2,gg4, cols=2) 
}
if (theme=="ecdf")
{
   gg <- resid_ecdf(obj, title="(a) ecdf of the residuals")
  gg1 <- gg + stat_function(fun = pNO, args=list(mu=0, sigma=1))
  gg2 <- resid_dtop(obj, value=value, title="(b) detrended Own's plot")
  gg3 <- resid_qqplot(obj, title="(c) residual qqplot")  
  gg4 <- resid_wp(obj, title="(d) worm plot") 
  multiplot(gg1,gg3,gg2,gg4, cols=2) 
}
}  
