## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi=60
)

## ----cran_install, eval=FALSE, message=FALSE, warning=FALSE-------------------
#  install.packages("ggpie")

## ----github_install, eval=FALSE, message=FALSE, warning=FALSE-----------------
#  # install.package("remotes")   #In case you have not installed it.
#  remotes::install_github("showteeth/ggpie")

## ----citation-----------------------------------------------------------------
citation("ggpie")

## ----prepare, message=FALSE, warning=FALSE------------------------------------
library(ggpie)
library(ggplot2)
data(diamonds)
# check data used
str(diamonds)

## ----pie_basic_no_label-------------------------------------------------------
# with no label
ggpie(data = diamonds, group_key = "cut", count_type = "full",label_type = "none")

## ----pie_circle_out-----------------------------------------------------------
ggpie(data = diamonds, group_key = "cut", count_type = "full",
      label_info = "all", label_type = "circle",
      label_size = 4, label_pos = "out")

## ----pie_circle_in_no_split---------------------------------------------------
ggpie(data = diamonds, group_key = "cut", count_type = "full",
      label_info = "all", label_type = "circle", label_split = NULL,
      label_size = 4, label_pos = "in")

## ----pie_horizon_in_no_split--------------------------------------------------
ggpie(data = diamonds, group_key = "cut", count_type = "full",
      label_info = "all", label_type = "horizon", label_split = NULL,
      label_size = 4, label_pos = "in")

## ----pie_horizon_in_split-----------------------------------------------------
ggpie(data = diamonds, group_key = "cut", count_type = "full",
      label_info = "all", label_type = "horizon",
      label_size = 4, label_pos = "in")

## ----pie_horizon_out_no_split-------------------------------------------------
ggpie(data = diamonds, group_key = "cut", count_type = "full",
      label_info = "all", label_type = "horizon",
      label_size = 4, label_pos = "out" )

## ----pie_threashold_horizon_in------------------------------------------------
ggpie(data = diamonds, group_key = "cut", count_type = "full",
      label_info = "all", label_type = "horizon", label_split = NULL,
      label_size = 4, label_pos = "in", label_threshold = 10)

## ----donut_basic_no_label-----------------------------------------------------
# with no label
ggdonut(data = diamonds, group_key = "cut", count_type = "full",label_type = "none")

## ----donut_circle_out---------------------------------------------------------
ggdonut(data = diamonds, group_key = "cut", count_type = "full",
        label_info = "all", label_type = "circle",
        label_size = 4, label_pos = "out")

## ----donut_circle_in_no_split-------------------------------------------------
ggdonut(data = diamonds, group_key = "cut", count_type = "full",
        label_info = "all", label_type = "circle", label_split = NULL,
        label_size = 4, label_pos = "in")

## ----donut_horizon_in_no_split------------------------------------------------
ggdonut(data = diamonds, group_key = "cut", count_type = "full",
        label_info = "all", label_type = "horizon", label_split = NULL,
        label_size = 4, label_pos = "in")

## ----donut_horizon_in---------------------------------------------------------
ggdonut(data = diamonds, group_key = "cut", count_type = "full",
        label_info = "all", label_type = "horizon",
        label_size = 4, label_pos = "in")

## ----donut_horizon_out_no_split-----------------------------------------------
ggdonut(data = diamonds, group_key = "cut", count_type = "full",
        label_info = "all", label_type = "horizon", label_split = NULL,
        label_size = 4, label_pos = "out")

## ----donut_horizon_out--------------------------------------------------------
ggdonut(data = diamonds, group_key = "cut", count_type = "full",
        label_info = "all", label_type = "horizon",
        label_size = 4, label_pos = "out")

## ----donut_threashold_horizon_in----------------------------------------------
ggdonut(data = diamonds, group_key = "cut", count_type = "full",
        label_info = "all", label_type = "horizon", label_split = NULL,
        label_size = 4, label_pos = "in", label_threshold = 10)

## ----donut_threashold_horizon_in_no_split-------------------------------------
ggdonut(data = diamonds, group_key = "cut", count_type = "full",
        label_info = "all", label_type = "horizon",
        label_size = 4, label_pos = "in", label_threshold = 10)

## ----pie3d--------------------------------------------------------------------
p1= ggpie3D(data = diamonds, group_key = "cut", count_type = "full", tilt_degrees = -10, label_size=2) + 
  ggtitle("tilt_degrees = -10") + 
  theme(plot.title = element_text(hjust = 0.5))

p2= ggpie3D(data = diamonds, group_key = "cut", count_type = "full", tilt_degrees = -40, label_size=2) + 
  ggtitle("tilt_degrees = -40") + 
  theme(plot.title = element_text(hjust = 0.5))

p3= ggpie3D(data = diamonds, group_key = "cut", count_type = "full", tilt_degrees = -10, 
            start_degrees = 60, label_size=2) + 
  ggtitle("start_degrees = 60") + 
  theme(plot.title = element_text(hjust = 0.5))

p4= ggpie3D(data = diamonds, group_key = "cut", count_type = "full", tilt_degrees = -10, 
            start_degrees = 180, label_size=2) + 
  ggtitle("start_degrees = 180") + 
  theme(plot.title = element_text(hjust = 0.5))

cowplot::plot_grid(p1,p2,p3,p4,ncol = 2)

## ----nested_pie_inner_outer_circle_in-----------------------------------------
ggnestedpie(data = diamonds, group_key = c("cut", "color"), count_type = "full",
            inner_label_info = "all", inner_label_split = NULL,inner_label_size = 2,
            outer_label_type = "circle", outer_label_pos = "in", outer_label_info = "all")

## ----nested_pie_inner_outer_circle_in_remove----------------------------------
ggnestedpie(data = diamonds, group_key = c("cut", "color"), count_type = "full",
            inner_label_info = "all", inner_label_split = NULL,
            inner_label_threshold = 5, inner_label_size = 2,
            outer_label_type = "circle", outer_label_pos = "in", outer_label_info = "all")

## ----nested_pie_inner_outer_circle_out----------------------------------------
ggnestedpie(data = diamonds, group_key = c("cut", "color"), count_type = "full",
            inner_label_info = "all", inner_label_split = NULL, inner_label_size = 2,
            outer_label_type = "circle", outer_label_pos = "out", outer_label_info = "all")

## ----nested_pie_inner_outer_horizon_out_remove--------------------------------
ggnestedpie(data = diamonds, group_key = c("cut", "color"), count_type = "full",
            inner_label_info = "all", inner_label_split = NULL,
            inner_label_threshold = 1, inner_label_size = 2,
            outer_label_type = "horizon", outer_label_pos = "out", outer_label_info = "all")

## ----nested_pie_inner_outer_remove_horizon_in_remove--------------------------
ggnestedpie(data = diamonds, group_key = c("cut", "color"), count_type = "full",
            inner_label_info = "all", inner_label_split = NULL,
            inner_label_threshold = 1, inner_label_size = 2,
            outer_label_type = "horizon", outer_label_pos = "in",
            outer_label_info = "all", outer_label_threshold = 10)

## ----nested_pie_blank_inner_outer---------------------------------------------
ggnestedpie(data = diamonds, group_key = c("cut", "color"), count_type = "full", 
            r0 = 0.5, r1 = 1.5, r2 = 2.6,inner_label_info = "all", inner_label_split = NULL,
            inner_label_threshold = 1, inner_label_size = 2,
            outer_label_type = "horizon", outer_label_pos = "in",
            outer_label_info = "all", outer_label_threshold = 10)

## ----rose_pie_no_tick, fig.height=10, fig.width=18----------------------------
# pie plot
p1=ggrosepie(diamonds, group_key = "color", count_type = "full", label_info = "all",
             show_tick=F,donut_frac=NULL)
# donut plot
p2=ggrosepie(diamonds, group_key = "color", count_type = "full", label_info = "all",
             show_tick=F,donut_frac=0.3,donut_label_size=3)
cowplot::plot_grid(p1,p2)

## ----rose_pie_with_tick, fig.height=8, fig.width=18---------------------------
# pie plot
p1=ggrosepie(diamonds, group_key = "color", count_type = "full", label_info = "all",
             donut_frac=NULL)
# donut plot
p2=ggrosepie(diamonds, group_key = "color", count_type = "full", label_info = "all",
             donut_frac=0.3,donut_label_size=3)
cowplot::plot_grid(p1,p2)

## ----rose_pie_with_tick_specific_break, fig.height=8, fig.width=18------------
# pie plot
p1=ggrosepie(diamonds, group_key = "color", count_type = "full", label_info = "all",
             tick_break = c(3000,5000,7000,11000), donut_frac=NULL)
# donut plot
p2=ggrosepie(diamonds, group_key = "color", count_type = "full", label_info = "all",
             tick_break = c(3000,5000,7000,11000), donut_frac=0.3,donut_label_size=3)
cowplot::plot_grid(p1,p2)

## ----rose_pie_two_no_tick, fig.height=8, fig.width=18-------------------------
# pie plot
p1=ggrosepie(diamonds, group_key = c("color","clarity"),
             count_type = "full", label_info = "all",
             show_tick=F,donut_frac=NULL)
# donut plot
p2=ggrosepie(diamonds, group_key = c("color","clarity"),
             count_type = "full", label_info = "all",
             show_tick=F,donut_frac=0.3,donut_label_size=3)
cowplot::plot_grid(p1,p2)

## ----rose_pie_two_with_tick, fig.height=8, fig.width=18-----------------------
# pie plot
p1=ggrosepie(diamonds, group_key = c("color","clarity"),
             count_type = "full", label_info = "all",
             donut_frac=NULL)
# donut plot
p2=ggrosepie(diamonds, group_key = c("color","clarity"),
             count_type = "full", label_info = "all",
             donut_frac=0.3,donut_label_size=3)
cowplot::plot_grid(p1,p2)

## ----rose_pie_two_with_tick_specific_break, fig.height=8, fig.width=18--------
# pie plot
p1=ggrosepie(diamonds, group_key = c("color","clarity"),
             count_type = "full", label_info = "all",
             tick_break = c(3000,5000,7000,11000), donut_frac=NULL)
# donut plot
p2=ggrosepie(diamonds, group_key = c("color","clarity"),
             count_type = "full", label_info = "all",
             tick_break = c(3000,5000,7000,11000), donut_frac=0.3,donut_label_size=3)
cowplot::plot_grid(p1,p2)

