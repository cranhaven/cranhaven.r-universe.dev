## ----setup, echo=FALSE, message=FALSE, results='hide'-------------------------
knitr::opts_chunk$set(fig.width = 15, fig.height = 9, fig.align = "center")

## ---- echo=FALSE, out.width=650-----------------------------------------------

library(ggcorset)
library(ggplot2)

# LOAD "drinkdays" data from the ggcorset package
data("drinkdays")

# Magnitude of change: Subtract T1 from T2
drinkdays$change <- drinkdays$time2-drinkdays$time1

# Direction of Change
drinkdays$direction <- ifelse(drinkdays$change<0,"Decrease",
                              ifelse(drinkdays$change>0,"Increase","No Change"))
drinkdays$direction <- factor(drinkdays$direction, 
                              levels = c("Increase","No Change","Decrease"))

# CORSET PLOT: Basic plot + theme_ggcorset()
plot <- gg_corset(drinkdays, y_var1 = "time1", y_var2 = "time2", 
                  c_var = "direction", group = "id", eyelets = T) + 
        theme_ggcorset()

# ADDITIONAL ggplot2 commands
plot + 
  # Changes legend title, and selects a colour-palette
  scale_colour_manual("Direction of Change",
                    values = MetBrewer::met.brewer("Ingres",3)) +
  # Changes the plot title
  ggtitle("Change in Drinking Days") + 
  # Changes the y-axis title
  ylab("Number of Drinking Days per Week") + 
  # Changes the x-axis title (removes in favour of the 2 time point labels below)
  xlab("") +
  # Changes the labels of the 2 time points (on the x-axis)
  scale_x_discrete(labels = c("Pre","Post")) +
  # Makes the legend lines thicker
  guides(colour = guide_legend(override.aes = list(linewidth = 3)))


## ---- fig.show='hide'---------------------------------------------------------

library(ggcorset)
library(ggplot2)

# LOAD "drinkdays" data from the ggcorset package
data("drinkdays")

# Magnitude of change: Subtract T1 from T2
drinkdays$change <- drinkdays$time2-drinkdays$time1

# Direction of Change
drinkdays$direction <- ifelse(drinkdays$change<0,"Decrease",
                              ifelse(drinkdays$change>0,"Increase","No Change"))
drinkdays$direction <- factor(drinkdays$direction, 
                              levels = c("Increase","No Change","Decrease"))

# CORSET PLOT: Basic plot + theme_ggcorset()
plot <- gg_corset(drinkdays, y_var1 = "time1", y_var2 = "time2", 
                  c_var = "direction", group = "id", eyelets = T) + 
        theme_ggcorset()

# ADDITIONAL ggplot2 commands
plot + 
  # Changes legend title, and selects a colour-palette
  scale_colour_manual("Direction of Change",
                    values = MetBrewer::met.brewer("Ingres",3)) +
  # Changes the plot title
  ggtitle("Change in Drinking Days") + 
  # Changes the y-axis title
  ylab("Number of Drinking Days per Week") + 
  # Changes the x-axis title (removes in favour of the 2 time point labels below)
  xlab("") +
  # Changes the labels of the 2 time points (on the x-axis)
  scale_x_discrete(labels = c("Pre","Post")) +
  # Makes the legend lines thicker
  guides(colour = guide_legend(override.aes = list(linewidth = 3)))


## ----out.width=650------------------------------------------------------------

gg_corset(drinkdays, y_var1 = "time1", y_var2 = "time2",
          c_var = "direction", group = "id", faceted = T, facet_design = "group") + 
  theme_ggcorset() +  
  scale_colour_manual("Direction of Change",
                      values = MetBrewer::met.brewer("Demuth",3, direction = -1))  +
  scale_fill_manual(values = MetBrewer::met.brewer("Demuth",3, direction = -1))  +
  ggtitle("Change in Drinking Days") + 
  ylab("Number of Drinking Days per Week") + xlab("") +
  scale_x_discrete(labels = c("Pre","Post")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3)))


## ----out.width=650------------------------------------------------------------

gg_corset(drinkdays, y_var1 = "time1", y_var2 = "time2",
          c_var = "direction", group = "id", faceted = T, facet_design = "line") + 
  theme_ggcorset() +  
  scale_colour_manual("Direction of Change",
                      values = MetBrewer::met.brewer("Demuth",3))  +
  ggtitle("Change in Drinking Days") + 
  ylab("Number of Drinking Days per Week") + xlab("") +
  scale_x_discrete(labels = c("Pre","Post")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3)))


## ---- out.width=650-----------------------------------------------------------

gg_corset(drinkdays, y_var1 = "time1", y_var2 = "time2", 
          c_var = "change", group = "id", line_size = 0.5) + 
  theme_ggcorset() +
  viridis::scale_colour_viridis(
    option = "cividis", direction = -1,
    breaks = c(-7,0,7),  # can set the legend limits here (min and max)
    name = "") +         # can rename the legend title here or omit
  ggtitle("Change in Drinking Days") +
  ylab("Number of Drinking Days per Week") + xlab("") +
  scale_x_discrete(labels = c("Pre","Post"))


## ---- echo=FALSE, out.width=650-----------------------------------------------

### SUBGROUPS

drinkdays$age <- c(1,2,1,1,1,2,1,1,2,1,1,2,1,1,1,1,1,2,1,2,2,1,1,1,1,1,2,1,1,2,2,1,2,2,2,1,2,2,2,1,1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1,1,2,1,1,1,2,1,1,2,2,1,2,2,2,1,1,1,1,2,1,2,1,1,1,2,1,1,2,1,2,1,1,1,1,1,2,1,1,1,2,1,2,2,2,2,2,2,1,1,1,2,1,1,1,2,2,2,1,1,2,1,1,1,2,1,1,2,1,2,2,1,2,1,1,1,1,1,1,1,1,2,1,2,2,1,1,2,1,2,1,1,1,2,1,1,2,1,1,1,1,1,2,1,2,2,1,2,1,2,1,1,1,2,1,1,2,2,1,1,1,1,1,1,1,2,1,1,1,1,2,1,1,2,1,1,2,1,1,1,2,1,1,1,1,2,2,1,1,1,2,2,1,1,1,1,1,1,1,2,2,1,1,1,1,2,2,1,2,1,1,2,1,1,1,2,1,1,1,2,2,1,2,2,2,2,2,1,2,1,1,2,1,1,1,1,2,1,2,2,2,1,2,1,1,1,1,1,1,2,1,1,1,1,2,2,2,1,1,1,1,1,2,1,1,2,2,1,2,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1)

drinkdays$age <- ifelse(drinkdays$age==1,"<30","30+")


gg_corset(drinkdays, y_var1 = "time1", y_var2 = "time2", c_var = "age",
          group = "id", faceted = T, eyelets = T, e_type = "SD") + theme_ggcorset() +
  xlab("") +  scale_colour_manual("",
                                  values = MetBrewer::met.brewer("Morgenstern",2)) +
ggtitle("Change in Drinking Days by Age Group",
) +
  ylab("Number of Drinking Days per Week") +
  xlab("") +
  scale_x_discrete(labels = c("Pre","Post")) +
  guides(colour = guide_legend(override.aes = list(linewidth = 3)))


## ---- echo=FALSE, out.width=650-----------------------------------------------

### LATENT CLASSES

drinkdays$class <- c(3,3,3,1,3,3,1,3,3,1,3,2,3,3,2,2,2,2,1,3,1,3,3,3,3,2,2,1,3,2,2,3,2,2,2,1,3,3,1,3,3,3,1,2,2,3,3,3,1,3,3,3,2,3,2,3,3,3,3,2,2,2,1,3,2,3,3,3,2,3,3,3,1,3,2,3,2,2,3,3,3,3,1,1,2,3,3,3,2,2,2,3,1,3,3,3,2,2,2,2,2,2,1,2,1,2,1,3,1,2,2,2,3,3,3,3,2,3,2,3,1,2,3,3,3,2,3,3,3,2,2,1,3,1,2,2,3,1,2,3,3,3,3,3,1,2,3,2,1,2,2,3,2,3,1,3,2,3,3,3,1,1,3,3,2,3,3,1,2,3,3,3,2,2,2,3,2,2,1,1,2,1,3,1,3,2,1,3,2,3,3,2,3,2,1,3,3,3,2,3,2,2,2,2,3,1,3,3,1,3,3,1,3,3,2,3,1,3,3,1,2,3,3,2,3,3,3,3,2,3,1,3,1,1,2,2,3,3,2,2,2,2,3,2,3,2,2,1,1,3,2,3,3,2,2,2,3,2,3,1,1,1,2,3,1,3,1,3,1,3,2,3,1,1,3,3,2,2,3,1,2,3,1,2,3,3,3,3,2,3,3,3,1,2,1,3,3,2,2,3)

drinkdays$class <- factor(drinkdays$class,levels = c(1,2,3),
                          labels = c("Class 1 (25.7%)","Class 2 (31.0%)", "Class 3 (43.3%)"))

gg_corset(drinkdays, y_var1 = "time1", y_var2 = "time2", c_var = "class",
          group = "id", faceted = T, facet_design = "line", eyelets = T) + theme_ggcorset() +
  xlab("") +  scale_colour_manual("",
                                  values = MetBrewer::met.brewer("Kandinsky",3, direction = 1)) +
  ggtitle("Change in Drinking Days by Latent Class",
  ) +
  ylab("Number of Drinking Days per Week") +
  xlab("") +
  scale_x_discrete(labels = c("Pre","Post")) +
  guides(colour = "none")


