## ---- fig.cap="A figure built using a single graphic with top and bottom margins set default values", message=FALSE----
require(figuRes2)
require(grid)
require(gridExtra)
require(ggplot2)
require(scales)

#pdf("figure1 - testing dimensions.pdf", h=8.5, w=11)
build.page(interior.h=c(1),
           interior.w=c(1),
           ncol=1, nrow=1,
           test.dim=TRUE)
annotate.page(override="" )
#dev.off()

## ---- fig.cap=" A figure built using a two graphics side-by-side."------------
build.page(interior.h=c(1),
           interior.w=c(.5, .5),
           ncol=2, nrow=1,
           test.dim=TRUE)
annotate.page(override="")

## ---- fig.cap="A figure built using 3 graphics stacked with annotation."------
build.page(interior.h=c(1/3,1/3,1/3),
           interior.w=c(1),
           ncol=1, nrow=3,
           test.dim=TRUE)
annotate.page(override="")

## ---- fig.cap="Example 1d: A figure built using 3x2 grid of graphics."--------
build.page(interior.h=c(2, 1, 3)/6,
           interior.w=c(.6, .4),
           ncol=2, nrow=3,
           test.dim=TRUE)
annotate.page(override="" )

## ---- fig.cap="Shrinking the perimeter reserved for margins"------------------
build.page(interior.h=c(1/3,1/3,1/3),
           interior.w=c(.5, .5),
           ncol=2, nrow=3,
           test.dim=TRUE,
           top.margin=.1,
           bottom.margin=.1,
           right.margin=.1,
           left.margin=.1)

## ---- eval=FALSE--------------------------------------------------------------
#  top.margin=1.6,paste0("bottom margin = ", bm)

## -----------------------------------------------------------------------------
theme_set(theme_grey2_nomargins())

## ---- eval=FALSE--------------------------------------------------------------
#  # The vector holds c(top, right, bottom, left) margin measurements.
#  left.graphic <- left.graphic + theme(plot.margin=unit(c(0,.1,0,0), "in"))
#  right.graphic <- right.graphic + theme(plot.margin=unit(c(0,0,0,.1), "in"))
#  grid.arrange(left.graphic, right.graphic, nrow=1)

## ---- results='hide', eval=F--------------------------------------------------
#  remove(list=ls())
#  # require(figuRes2)
#  default.settings(
#          my.path = "C:/Users/eri7441//OneDrive - Takeda/Documents/R packages/figuRes2 - testing/",
#          od = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/output/",
#          dd = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/dddata/",
#          cd = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/code/",
#          logd = "C:/Users/eri7441/OneDrive - Takeda/Documents/R packages/figuRes2 - testing/log/")
#  
#  # This code creates directory at the locations specified.
#  dir.create(file.path(cd), showWarnings = FALSE)
#  dir.create(file.path(dd), showWarnings = FALSE)
#  dir.create(file.path(od), showWarnings = FALSE)
#  dir.create(file.path(logd), showWarnings = FALSE)
#  

## -----------------------------------------------------------------------------
set.seed(8675309) # To ensure reproducibility
working.df <- data.frame(x=rnorm(500, 0, 1))
working.df$group <- factor(sample(x=c("A", "B"), replace=TRUE, 
                                  size=nrow(working.df), prob=c(.4,.6)))
working.df$y <- working.df$x + 
  as.numeric(working.df$group)*working.df$x + 
  rnorm(n = nrow(working.df), 0, 3)

## -----------------------------------------------------------------------------
ex.bar <- ggplot(data=working.df, aes(x=group, fill=group)) + 
  geom_bar() +
  labs(x="Group", y="Frequency", title="", fill="Group") +
  scale_y_continuous(limits=c(0,500), breaks=seq(0,500,25)) +
  coord_flip()

## ---- fig.cap="A Bar Chart"---------------------------------------------------
print(ex.bar)

## ---- fig.cap="An assembled bar chart figure"---------------------------------
build.page(interior.h = c(1), 
           interior.w = c(1),
           ncol=1, 
           nrow=1,
           interior =list(ex.bar + ggtitle("\n\n\n\n")),
           test.dim = F)
annotate.page(override = "")

## ---- eval=FALSE--------------------------------------------------------------
#  pdf(paste0(od, "barchart.pdf"), height=8.5, width=11)
#  # In the build of ex.bar title="" allows for room for a single title line
#  build.page(interior.h = c(1),
#             interior.w = c(1),
#             ncol=1,
#             nrow=1,
#             interior =list(ex.bar))
#  annotate.page(override = "")
#  # manipulating the title of the ggplot object allows for two lines
#  build.page(interior.h = c(1),
#             interior.w = c(1),
#             ncol=1,
#             nrow=1,
#             interior =list(ex.bar+ggtitle("\n")))
#  annotate.page(override = "")
#  
#  # manipulating the title of the ggplot object allows for three lines
#  build.page(interior.h = c(1),
#             interior.w = c(1),
#             ncol=1,
#             nrow=1,
#             interior =list(ex.bar+ggtitle("\n\n")))
#  annotate.page(override = "")
#  # manipulating the title of the ggplot object allows for four lines
#  
#  build.page(interior.h = c(1),
#             interior.w = c(1),
#             ncol=1,
#             nrow=1,
#             interior =list(ex.bar+ggtitle("\n\n\n")))
#  annotate.page(override = "")
#  dev.off() # Shuts the pdf device

## ---- results='hide'----------------------------------------------------------
remove(list=ls())
# require(figuRes2) 
default.settings()

## -----------------------------------------------------------------------------
working.df <- data.frame(x=rnorm(500, 0, 1))
working.df$group <- factor(
  sample(x=c("A", "B"), 
         replace=TRUE, 
         size=nrow(working.df),
         prob=c(.4,.6)))

working.df$y <- working.df$x + 
  as.numeric(working.df$group)*working.df$x + 
  rnorm(n = nrow(working.df), 0, 3)
head(working.df)

## -----------------------------------------------------------------------------
xmin <- min(working.df$x)
xmax <- max(working.df$x)
ymin <- min(working.df$y)
ymax <- max(working.df$y)

## ---- fig.cap="The main scatterplot"------------------------------------------
main.plot <- ggplot(data=working.df, aes(x=x,y=y, color=group, shape=group)) + 
  geom_point(size=3, alpha=.3) +
  geom_smooth(method = "lm", size=.75) +
  labs(x="x values", y="y values", color="Group", shape="Group") +
  scale_shape_manual(values=c(16, 17)) +
  scale_color_manual(values=c("red", "blue"))+
  scale_x_continuous(limits=c(xmin+.5,xmax+.5), breaks=seq(-4,4,1), expand=c(0,0))+
  scale_y_continuous(limits=c(ymin+.5,ymax+.5), expand=c(0,0))+
  theme(legend.position=c(.15, .8))
  
print(main.plot)

## ---- fig.cap="Density plot of x-values"--------------------------------------
density.plot.x <- ggplot(data=working.df, aes(x=x, fill=group, shape=group)) + 
  geom_density(alpha=.4) +
  scale_fill_manual(values=c("red", "blue"))+
  scale_x_continuous(limits=c(xmin+.5,xmax+.5), expand=c(0,0))+
  theme(axis.text=element_text(color="white"),
        axis.ticks=element_line(color="white")) +
  labs(x=NULL, y="", title="\n") +
  guides(fill=FALSE)
print(density.plot.x)

## ---- fig.cap="Density plot of y-values"--------------------------------------
density.plot.y <- ggplot(data=working.df, aes(x=y, fill=group, shape=group)) + 
  geom_density(alpha=.4) +
    scale_fill_manual(values=c("red", "blue"))+
  theme(axis.text=element_text(color="white"), 
        axis.ticks=element_line(color="white")) +
  scale_x_continuous(limits=c(ymin+.5,ymax+.5), expand=c(0,0))+
  labs(x=NULL, y="") +
  guides(fill=FALSE) +
  coord_flip()
print(density.plot.y)

## -----------------------------------------------------------------------------
build.page(interior.h = c(.35, .65), 
           interior.w = c(.75, .25),
           ncol=2, 
           nrow=2,
           test.dim = TRUE)
        

## ---- fig.cap="The final assembled figure"------------------------------------
# blankPanel <- grid.rect(gp=gpar(col="white"), draw=FALSE) # created by default.settings
build.page(interior.h = c(.35, .65), 
           interior.w = c(.75, .25),
           ncol=2, 
           nrow=2,
           interior =list(
             density.plot.x, blankPanel, 
             main.plot, density.plot.y))
annotate.page(override = "", title=list("Title Line 1", "","","",""))

## ---- fig.cap="The final assembled figure with reduced plot.margins"----------
build.page(interior.h = c(.35, .65), 
           interior.w = c(.75, .25),
           ncol=2, 
           nrow=2,
           interior =list(
             density.plot.x+
               theme(plot.margin= unit(c(0, -.1, -.1, 0), unit="in")),
             blankPanel, 
             main.plot+theme(plot.margin=unit(c(-.1, -.1, 0, 0), unit="in")),
             density.plot.y + theme(plot.margin=unit(c(-.1, 0, 0, -.3), 
                                                     unit="in"))))
annotate.page(override = "", title=list("Title Line 1", "","","",""))

## ----eval=F-------------------------------------------------------------------
#  pdf(file = paste0(od, "scatterplot with marginal densities.pdf"), width = 11, height = 8.5)
#  # blankPanel <- grid.rect(gp=gpar(col="white"), draw=FALSE) # created by default.settings
#  build.page(interior.h = c(.35, .65),
#             interior.w = c(.75, .25),
#             ncol=2,
#             nrow=2,
#             interior =list(
#               density.plot.x, blankPanel,
#               main.plot, density.plot.y))
#  annotate.page(override = "", title=list("Title Line 1", "","","",""))
#  
#  build.page(interior.h = c(.35, .65),
#             interior.w = c(.75, .25),
#             ncol=2,
#             nrow=2,
#             interior =list(
#               density.plot.x+
#                 theme(plot.margin= unit(c(0, -.1, -.1, 0), unit="in")),
#               blankPanel,
#               main.plot+theme(plot.margin=unit(c(-.1, -.1, 0, 0), unit="in")),
#               density.plot.y + theme(plot.margin=unit(c(-.1, 0, 0, -.3),
#                                                       unit="in"))))
#  annotate.page(override = "", title=list("Title Line 1", "","","",""))
#  dev.off()

