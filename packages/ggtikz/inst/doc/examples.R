## ----setup--------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(ggtikz)
opts_chunk$set(
    dev = "tikz",
    error = TRUE,
    cache = FALSE,
    external = TRUE,
    fig.path = "example-vignette-figures/",
    fig.width = 3,
    fig.height = 3,
    fig.align = "center"
)

## ----basic-usage--------------------------------------------------------------
p <- ggplot(mtcars, aes(disp, mpg)) + geom_point()
ggtikz(p, "\\fill[red] (0.5,0.5) circle (2mm);", xy="plot")

## ----single-panel-setup-------------------------------------------------------
p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    theme(plot.background=element_rect(color = "black", linewidth = 1))

## ----single-panel-relative-plot-----------------------------------------------
canvas <- ggtikzCanvas(p)
annotation <- ggtikzAnnotation(
    "
    \\draw (0,0) -- (1,1);
    \\draw (0,1) -- (1,0);
    \\fill[red] (0.5,0.5) circle (2mm);
    ",
    xy = "plot"
)
p                    # first draw the plot
canvas + annotation  # then draw the annotations

## ----single-panel-relative-panel----------------------------------------------
canvas <- ggtikzCanvas(p)
annotation <- ggtikzAnnotation(
    "
    \\draw (0,0) -- (1,1);
    \\draw (0,1) -- (1,0);
    \\fill[red] (0.5,0.5) circle (2mm);
    ",
    xy = "panel",
    panelx = 1, panely = 1
)
p
canvas + annotation

## ----single-panel-relative-data-----------------------------------------------
canvas <- ggtikzCanvas(p)
annotation <- ggtikzAnnotation(
    "
    \\draw[thick,red] (100,20) -| (400,15);
    \\draw[<-] (153,24) -- ++(30:1cm) node[at end, anchor=south]
        {Interesting!};
    ",
    xy = "data",
    panelx = 1, panely = 1
)
p
canvas + annotation

## ----single-panel-mixed-------------------------------------------------------
canvas <- ggtikzCanvas(p)
annotation <- ggtikzAnnotation(
   "\\fill[red] (0.5,30) circle (2mm);",
   x = "panel", y = "data",
   panelx = 1, panely = 1
)
p
canvas + annotation

## ----single-panel-clipping----------------------------------------------------
canvas <- ggtikzCanvas(p)
annotation_clip <- ggtikzAnnotation(
   "\\fill[red] (0.1,0) circle (5mm);",
   xy = "panel",
   panelx = 1, panely = 1
)

annotation_unclip <- ggtikzAnnotation(
   "\\fill[blue] (0.9,0) circle (5mm);",
   xy = "panel",
   panelx = 1, panely = 1,
   clip = "off"
)

annotation_unclip2 <- ggtikzAnnotation(
   "\\draw[thick, dashed] (0,0) -- (0.5,-0.2) -- (1,0);",
   xy = "panel",
   panelx = 1, panely = 1,
   clip = "off"
)
p
canvas + annotation_clip + annotation_unclip + annotation_unclip2

## ----single-panel-clipping2---------------------------------------------------
p + theme(plot.margin = margin(t=0.5, b = 1, unit = "cm"))
canvas + annotation_clip + annotation_unclip + annotation_unclip2

## ----set-unclip-hook----------------------------------------------------------
set_ggtikz_unclip_hook()

## ----single-panel-clipping3, external=FALSE, unclip=TRUE----------------------
# chunk options: external=FALSE, unclip=TRUE
p
canvas + annotation_unclip2

## ----unset-unclip-hook--------------------------------------------------------
unset_ggtikz_unclip_hook()

## ----wrap-setup---------------------------------------------------------------
p_wrap <- p + facet_wrap(~cyl, scales="free", ncol=2)

## ----wrap1--------------------------------------------------------------------
canvas <- ggtikzCanvas(p_wrap)

# Relative to data coordinates
annotation1 <- ggtikzAnnotation(
    "
    \\node[pin={90:(110,27)}, circle, fill=red,
        inner sep=0, outer sep=0, minimum size=2pt]
        at (110,27)
        {};
    ",
    xy = "data",
    panelx = 1, panely = 1
)

# Relative to data coordinates
annotation2 <- ggtikzAnnotation(
    "
    \\node[pin={90:(200,19)}, circle, fill=red,
    inner sep=0, outer sep=0, minimum size=2pt]
    at (200,19)
    {};
    ",
    xy = "data",
    panelx = 2, panely = 1
)

# Relative to panel coordinates
annotation3 <- ggtikzAnnotation(
    "
    \\node[draw, anchor=center] at (0.5, 0.5)
        {Center of panel};
    ",
    xy = "panel",
    panelx = 1, panely=2
)

p_wrap
canvas + annotation1 + annotation2 + annotation3

## ----wrap2--------------------------------------------------------------------
canvas <- ggtikzCanvas(p_wrap)
annotation1 <- ggtikzAnnotation(
    "
    \\node[pin={90:(110,27)}, circle, fill=red,
        inner sep=0, outer sep=0, minimum size=2pt]
        at (110,27)
        {};
    ",
    xy = "data",
    panelx = 1, panely = 1
)
annotation2 <- ggtikzAnnotation(
    "
    \\node[pin={90:(200,19)}, circle, fill=red,
    inner sep=0, outer sep=0, minimum size=2pt]
    at (200,19)
    {};
    ",
    xy = "data",
    panelx = 2, panely = 1
)
p_wrap
canvas + annotation1 + annotation2

## ----grid-setup---------------------------------------------------------------
p_grid <- p + facet_grid(gear~cyl, scales="free", as.table=FALSE)

## ----grid1, fig.width=5, fig.height=5-----------------------------------------
canvas <- ggtikzCanvas(p_grid)
annot_grid1 <- ggtikzAnnotation(
    "\\node[fill=white, draw, text width=2cm] at (0.5,0.5)
        {panelx=1, panely=1};",
    xy = "panel",
    panelx = 1, panely = 1
)
annot_grid2 <- ggtikzAnnotation(
    "\\node[fill=white, draw, text width=2cm] at (0.5,0.5)
        {panelx=2, panely=3};",
    xy = "panel",
    panelx = 2, panely = 3
)
annot_grid3 <- ggtikzAnnotation(
    "
    \\draw[<-, blue] (90,15) -- ++(30:5mm)
        node [at end, anchor=south west] {(90,15)};
    ",
    xy = "data",
    panelx = 1, panely = 3
)
p_grid
canvas + annot_grid1 + annot_grid2 + annot_grid3

## ----grid2, fig.width=5, fig.height=5-----------------------------------------
p_grid2 <- p + facet_grid(gear~cyl, scales="free", as.table=TRUE)
canvas2 <- ggtikzCanvas(p_grid2)

p_grid2
canvas2 + annot_grid1 + annot_grid2 + annot_grid3

## ----not-available, error = TRUE----------------------------------------------
p

canvas <- ggtikzCanvas(p)
canvas + annot_grid2

## ----logplot------------------------------------------------------------------
p_log <- ggplot(mtcars, aes(mpg, disp)) +
    geom_point() +
    scale_x_continuous(trans="log10")

## ----logplot-transform--------------------------------------------------------
canvas_log <- ggtikzCanvas(p_log)
# Untransformed coordinates: wrong position
annot_log <- ggtikzAnnotation(
    "\\fill[red] (1,100) circle (2mm);
    \\node[anchor=west, text=red] at (1, 100)
        {The circle is not at (1,100)!};
    ", xy = "data", transform = FALSE, panelx = 1, panely = 1
)

# Transformed coordinates: correct position
# The literal coordinate in the node text was wrapped in an \mbox
# LaTeX command to prevent automatic transformation -- it can't
# distinguish between coordinates which are supposed to be text,
# and actual coorinates.
annot_log2 <- ggtikzAnnotation(
    "\\fill[blue] (20,200) circle (2mm);
    \\node[anchor=south, text=blue] at (20, 200)
        {This circle is at (\\mbox{20,200})!};
    ", xy = "data", transform = TRUE, panelx = 1, panely = 1
)

# Untransformed coordinates, calculated manually by hand:
# correct position
annot_log3 <- ggtikzAnnotation(
    "\\fill[magenta] (1.477,400) circle (2mm);
    \\node[anchor=east, text=magenta] at (1.477, 400)
        {This circle is at (30,400)!};
    ", xy = "data", transform = FALSE, panelx = 1, panely = 1
)
p_log
canvas_log + annot_log + annot_log2 + annot_log3

## ----inf-replace--------------------------------------------------------------
p_log_border <- p_log +
    theme(panel.border = element_rect(fill = NA, linewidth = 2))
canvas_log_border <- ggtikzCanvas(p_log_border)
annot_inf <- ggtikzAnnotation(
    "\\draw[red, thick] (-Inf,200) -| (20,-Inf);
    \\draw[green, thick] (-Inf,200) |- (20,-Inf);
    ", xy = "data", replace_inf = TRUE, panelx = 1, panely = 1
)
p_log_border
canvas_log_border + annot_inf

## ----using-styles-------------------------------------------------------------
p

canvas <- ggtikzCanvas(p)
styled_annot <- ggtikzAnnotation(
    "\\node[loud] at (0.5,0.5) {Look at me!};",
    xy = "plot"
)
p
canvas + styled_annot

