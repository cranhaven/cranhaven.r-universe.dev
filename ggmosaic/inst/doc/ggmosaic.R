## ----setup, echo=FALSE--------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 4, fig.align='center',
                      dev = "png", message = FALSE)

## ----pkgs, echo=FALSE---------------------------------------------------------
library(ggmosaic)
library(gridExtra)
library(grid)
library(patchwork)
library(dplyr)

## ----data, echo=FALSE---------------------------------------------------------
data(fly)
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442","#0072B2", "#D55E00", "#CC79A7")
mypal <- okabe #source: the okabe color palette
scale_fill_discrete <- function(...) scale_fill_manual(..., values = mypal)

# set theme
theme_set(theme_mosaic()+theme(legend.position = "none"))

# A few modifications to data
flights <- fly %>%
  select(do_you_recline, rude_to_recline, eliminate_reclining, region) %>%
  filter(!is.na(do_you_recline), !is.na(rude_to_recline))

## ----variety, fig.height=10.5, fig.cap = "An assortment of plots made with the ggmosaic package."----
# A few modifications to data
flights <- fly  %>%
  filter(!is.na(do_you_recline), !is.na(rude_to_recline))

bar_examp <- ggplot(data = flights) + 
  geom_mosaic(aes(x=product(do_you_recline), fill = do_you_recline), divider="vbar") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y="Do you recline?", x = "", title = "Bar Chart")

spine_examp <- ggplot(data = flights) + 
  geom_mosaic(aes(x=product(do_you_recline), fill = do_you_recline), divider = "vspine") +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y="Do you recline?", x = "", title = "Spine Plot")

stackbar_examp <- ggplot(data = flights) + 
  geom_mosaic(aes(x=product(do_you_recline, rude_to_recline), fill = do_you_recline),
              divider=c("vspine", "hbar")) +   
  labs(x="Is it rude to recline?", y = "Do you recline?", title = "Stacked Bar Chart")

mosaic_examp <- ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline), fill = do_you_recline)) +   
  labs(y="Do you recline?", x="Is it rude to recline?", title = "Mosaic Plot (2 variables)") 

mosaic2_examp <- ggplot(data = flights) +
  geom_mosaic(aes(x=product(eliminate_reclining, do_you_recline, rude_to_recline), fill = do_you_recline, alpha = eliminate_reclining)) + 
  scale_alpha_manual(values =c(.7,.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
  labs(y="Do you recline?", x="Eliminate reclining?:Is it rude to recline?", title = "Mosaic Plot (3 variables)")


ddecker_examp <- ggplot(data = flights) +
  geom_mosaic(aes(x=product(do_you_recline, eliminate_reclining, rude_to_recline), fill = do_you_recline, alpha = eliminate_reclining), divider = ddecker()) + 
  scale_alpha_manual(values =c(.7,.9)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) + 
  labs(y="Do you recline?", x="Eliminate reclining?: Is it rude to recline?", title = "Double Decker Plot")

spine_examp + bar_examp + mosaic_examp + stackbar_examp + mosaic2_examp + ddecker_examp + plot_layout(ncol = 2)

## ----formula-1b---------------------------------------------------------------
 ggplot(data = flights) +
  geom_mosaic(aes(x = product(rude_to_recline), fill=rude_to_recline)) + 
  labs(title='f(rude_to_recline)')

## ----formula-2b---------------------------------------------------------------
ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline), fill=do_you_recline)) + 
  labs(title='f(do_you_recline | rude_to_recline) f(rude_to_recline)')

## ----formula-4b---------------------------------------------------------------
ggplot(data = flights) +
  geom_mosaic(aes(x=product(do_you_recline), fill = do_you_recline, 
                  conds = product(rude_to_recline))) +
  labs(title='f(do_you_recline | rude_to_recline)')

## ----formula-5b, fig.width = 6, fig.height = 6--------------------------------
ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline), fill=do_you_recline), divider = "vspine") +
  labs(title='f(do_you_recline | rude_to_recline)') + 
  facet_grid(~rude_to_recline) +
  theme(aspect.ratio = 3,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

## ----order-b, fig.width = 7, fig.height = 2.4---------------------------------
order1 <- ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline), fill = do_you_recline)) 

order2 <- ggplot(data = flights) +
  geom_mosaic(aes(x=product(rude_to_recline, do_you_recline), fill = do_you_recline)) 
  
order1 + order2

## ----partitions, fig.height = 2-----------------------------------------------
part1 <- ggplot(data = flights) + 
  geom_mosaic(aes(x=product(do_you_recline), fill = do_you_recline)) +  
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x="", y = "", title = "hspine")

part2 <- ggplot(data = flights) + 
  geom_mosaic(aes(x=product(do_you_recline), fill = do_you_recline),
              divider = "vspine") +  
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x="", y = "", title = "vspine")

part3 <- ggplot(data = flights) + 
  geom_mosaic(aes(x=product(do_you_recline), fill = do_you_recline),
              divider = "hbar") +  
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x="", y = "", title = "hbar")

part4 <- ggplot(data = flights) + 
  geom_mosaic(aes(x=product(do_you_recline), fill = do_you_recline),
              divider = "vbar") +  
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(x="", y = "", title = "vbar")

part1 + part2 + part3 + part4 + plot_layout(nrow = 1)

## ----mosaic-a, fig.height = 2-------------------------------------------------
h_mosaic <- ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline, eliminate_reclining), fill=do_you_recline), divider=mosaic("h")) +
  theme(axis.text=element_blank(), axis.ticks=element_blank()) + 
  labs(x="", y="")

v_mosaic <- ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline, eliminate_reclining), fill=do_you_recline), divider=mosaic("v"))  +
  theme(axis.text=element_blank(), axis.ticks=element_blank()) + 
  labs(x="", y="")

doubledecker <- ggplot(data = flights) +
  geom_mosaic(aes(x = product(rude_to_recline, eliminate_reclining), fill=do_you_recline), divider=ddecker()) +
  theme(axis.text=element_blank(), axis.ticks=element_blank()) + 
  labs(x="", y="")

h_mosaic + v_mosaic + doubledecker + plot_layout(nrow = 1)

## ----mosaic-d, fig.height = 2-------------------------------------------------
mosaic4 <-  ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline, eliminate_reclining), fill=do_you_recline), divider=c("vspine", "vspine", "hbar"))  +
  theme(axis.text=element_blank(), axis.ticks=element_blank()) + 
  labs(x="", y="")

mosaic5 <-  ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline, eliminate_reclining), fill=do_you_recline), divider=c("hbar", "vspine", "hbar"))  +
  theme(axis.text=element_blank(), axis.ticks=element_blank()) + 
  labs(x="", y="")

mosaic6 <-  ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline, eliminate_reclining), fill=do_you_recline), divider=c("hspine", "hspine", "hspine"))  +
  theme(axis.text=element_blank(), axis.ticks=element_blank()) + 
  labs(x="", y="")

mosaic7 <-  ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline, eliminate_reclining), fill=do_you_recline), divider=c("vspine", "vspine", "vspine"))  +
  theme(axis.text=element_blank(), axis.ticks=element_blank()) + 
  labs(x="", y="")

mosaic4 + mosaic5 + mosaic6 + mosaic7 + plot_layout(nrow = 1)

## ----offset-b, fig.width = 10, fig.height = 4---------------------------------
offset1 <- ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, region), fill=do_you_recline)) + 
  labs(y = "", title=" offset = 0.01") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(angle = 90))

offset0 <- ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, region), fill=do_you_recline), offset = 0) + 
  labs(y = "", title=" offset = 0") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(angle = 90))

offset2 <- ggplot(data = flights) +
  geom_mosaic(aes(x = product(do_you_recline, region), fill=do_you_recline), offset = 0.02) + 
  labs(y="",  title=" offset = 0.02") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.position = "right")

offset0 + offset1 + offset2 + plot_layout(ncol = 3)

