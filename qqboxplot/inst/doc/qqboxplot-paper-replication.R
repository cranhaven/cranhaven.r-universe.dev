## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  fig.width = 7, 
  fig.height = 4.8, 
  fig.align = "center"
)

## ----setup, message=FALSE-----------------------------------------------------
library(dplyr)
library(ggplot2)
library(qqboxplot)

## ---- fig.height=2.7----------------------------------------------------------

#set value for text size so consistent across figures (only applies to figure 1)
eltext <- 12

#q-q boxplot
qqbox <- expression_data %>% 
  ggplot(aes(specimen, log_count)) + geom_qqboxplot(varwidth = TRUE, notch = TRUE) +
  ylab('logged normalized expression') + ggtitle("c) q-q boxplot") +
  theme(plot.title=element_text(size=eltext, face="plain", hjust=0.5), axis.title.x = element_text(size=eltext), axis.title.y = element_text(size=eltext),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(colour = "grey70"),
        panel.grid.minor = element_line(colour="grey80"))

# regular boxplot
box <- expression_data %>%
  ggplot(aes(specimen, log_count)) + geom_boxplot(varwidth = TRUE, notch = TRUE) +
  ylab('logged normalized expression') + ggtitle('a) boxplot') +
  theme(plot.title=element_text(size=eltext, face="plain", hjust=0.5), axis.title.x = element_text(size=eltext), axis.title.y = element_text(size=eltext),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(colour = "grey70"),
        panel.grid.minor = element_line(colour="grey80"))

override.shape <- c(16, 17)
#q-q plot
qq <- expression_data %>%
  ggplot(aes(sample=log_count)) + geom_qq(aes(color=specimen, shape=specimen)) +
  xlab('theoretical normal distribution') +
  ylab('logged normalized expression') + ggtitle('b) q-q plot') +
  labs(color="specimen") +
  guides(color = guide_legend(override.aes = list(shape=override.shape)), shape=FALSE) +
  theme(plot.title=element_text(size=eltext, face="plain", hjust=0.5), axis.title.x = element_text(size=eltext), axis.title.y = element_text(size=eltext),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(colour = "grey70"),
        panel.grid.minor = element_line(colour="grey80"),
        legend.position = c(0.8, 0.2))

library(gridExtra)
# combine the plots
gridExtra::grid.arrange(box, qq, qqbox, ncol=3)

## -----------------------------------------------------------------------------
simulated_data %>%
  ggplot(aes(factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")), y=y)) +
  geom_boxplot(notch=TRUE, varwidth = TRUE) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x = element_text(angle = 23, size = 15), axis.title.y = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## -----------------------------------------------------------------------------
override.shape <- c(16, 17, 15, 3, 7)

simulated_data %>% ggplot(aes(sample=y, color=factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")),
                              shape=factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")))) +
  geom_qq() + geom_qq_line() + labs(color="distribution") +
  xlab("Normal Distribution") +
  ylab("Simulated Datasets") +
  guides(color = guide_legend(override.aes = list(shape=override.shape)), shape=FALSE) +
  theme(axis.title.x = element_text(size=15), axis.title.y = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## -----------------------------------------------------------------------------
simulated_data %>%
         ggplot(aes(factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")), y=y)) +
         geom_qqboxplot(notch=TRUE, varwidth = TRUE, reference_dist="norm") +
         xlab("reference: normal distribution") +
         ylab(NULL) +
         guides(color=FALSE) +
         theme(axis.text.x = element_text(angle = 23, size = 15), axis.title.y = element_text(size=15),
               axis.title.x = element_text(size=15),
               panel.border = element_blank(), panel.background = element_rect(fill="white"),
               panel.grid = element_line(colour = "grey70"))


## ---- eval=FALSE--------------------------------------------------------------
#  tibble(y=c(rnorm(1000, mean=2), rt(1000, 16), rt(500, 4),
#                     rt(1000, 8), rt(1000, 32)),
#          group=c(rep("normal, mean=2", 1000),
#                  rep("t distribution, df=16", 1000),
#                  rep("t distribution, df=4", 500),
#                  rep("t distribution, df=8", 1000),
#                  rep("t distribution, df=32", 1000)))

## -----------------------------------------------------------------------------
simulated_data %>%
  ggplot(aes(factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")), y=y)) +
  geom_qqboxplot(notch=TRUE, varwidth = TRUE, compdata=comparison_dataset) +
  xlab("reference: simulated normal dataset") +
  ylab(NULL) +
  theme(axis.text.x = element_text(angle = 23, size = 15), axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## ---- eval=FALSE--------------------------------------------------------------
#  rnorm(1000, 5)

## ---- fig.height=2.7----------------------------------------------------------
comparison_data <- indicators %>% filter(year==2008 & `Series Code`=="SL.TLF.ACTI.1524.MA.NE.ZS")

indicators %>%
  #change the labels in series name to shorter titles
  mutate(`Series Name`= ifelse(
    `Series Name`=="Labor force participation rate for ages 15-24, male (%) (national estimate)", 
                               "Male ages 15-24", 
                               "Female ages 15-24")) %>%
  ggplot(aes(as.factor(year), y=indicator))+
  geom_qqboxplot(notch=TRUE, varwidth = TRUE, compdata=comparison_data$indicator) +
  xlab("Year") +
  ylab("Country level labor force\nparticipation rate (%)") +
  facet_wrap(~factor(`Series Name`, levels = c("Male ages 15-24", "Female ages 15-24"))) +
  theme(strip.text = element_text(size=12), axis.text.x = element_text(size = 15), axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=12),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## ---- fig.height=2.7----------------------------------------------------------
indicators %>%
  #change the labels in series name to shorter titles
  mutate(`Series Name`= ifelse(
    `Series Name`=="Labor force participation rate for ages 15-24, male (%) (national estimate)", 
                               "Male ages 15-24", 
                               "Female ages 15-24")) %>%
  ggplot()+
  geom_violin(aes(x=factor(year),y=indicator),fill='grey',trim=F, draw_quantiles = c(.25, .5, .75))+
  geom_segment(aes(
    x=match(factor(year),levels(factor(year)))-0.1,
    xend=match(factor(year),levels(factor(year)))+0.1,
    y=indicator,yend=indicator),
    col='black'
  ) +
  xlab("Year") +
  ylab("Country level labor force\nparticipation rate (%)") +
  facet_wrap(~factor(`Series Name`, levels = c("Male ages 15-24", "Female ages 15-24"))) +
  theme(strip.text = element_text(size=12), axis.text.x = element_text(size = 15), axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=12),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## -----------------------------------------------------------------------------
spike_data %>% filter(region=="V1") %>%
  ggplot(aes(factor(orientation), nspikes)) +
  geom_qqboxplot(notch=TRUE, varwidth = TRUE, reference_dist="norm") +
  xlab("orientation") +
  ylab("spike count") +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size=14), axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## -----------------------------------------------------------------------------
spike_data %>% filter(region=="V1") %>%
  ggplot()+
  geom_violin(aes(x=factor(orientation),y=nspikes),fill='grey',trim=F, draw_quantiles = c(.25, .5, .75))+
  geom_segment(aes(
    x=match(factor(orientation),levels(factor(orientation)))-0.1,
    xend=match(factor(orientation),levels(factor(orientation)))+0.1,
    y=nspikes,yend=nspikes),
    col='black'
  ) +
  xlab("orientation") +
  ylab("spike count") +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size=14), axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## -----------------------------------------------------------------------------
spike_data %>% filter(region=="V2") %>%
  ggplot(aes(factor(orientation), nspikes)) +
  geom_qqboxplot(notch=TRUE, varwidth = TRUE, reference_dist="norm") +
  xlab("orientation") +
  ylab("spike count") +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size=14), axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## -----------------------------------------------------------------------------
spike_data %>% filter(region=="V2") %>%
  ggplot()+
  geom_violin(aes(x=factor(orientation),y=nspikes),fill='grey',trim=F, draw_quantiles = c(.25, .5, .75))+
  geom_segment(aes(
    x=match(factor(orientation),levels(factor(orientation)))-0.1,
    xend=match(factor(orientation),levels(factor(orientation)))+0.1,
    y=nspikes,yend=nspikes),
    col='black'
  ) +
  xlab("orientation") +
  ylab("spike count") +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size=14), axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))

## -----------------------------------------------------------------------------
population_brain_data %>%
  ggplot(aes(x=ecephys_structure_acronym, y=log(rate))) +
  geom_qqboxplot(notch=TRUE, varwidth = TRUE, reference_dist="norm") +
  xlab("Brain regions") +
  ylab("Log firing rate") +
  facet_wrap(~fr_type) +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=15)
        , axis.title.y = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"),
        strip.text.x = element_text(size = 14))

## -----------------------------------------------------------------------------
population_brain_data %>%
  ggplot(aes(x=ecephys_structure_acronym, y=log(rate))) +
  geom_violin(fill='grey',trim=F, draw_quantiles = c(.25, .5, .75))+
  xlab("Brain regions") +
  ylab("Log firing rate") +
  facet_wrap(~fr_type) +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=15)
        , axis.title.y = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"),
        strip.text.x = element_text(size = 14))

## -----------------------------------------------------------------------------

library(scales)

# set grid
h_grid <- seq(from=-2, to=2, length.out = 101)
g_grid <- seq(from=-2, to=2, length.out = 101)
z_grid <- c(-.67448, .67448)

# transformation function
zfun <- function(h, g, z){
  ifelse(g==0, 
    z*exp(h*z^2/2),
    exp(h*z^2/2)*((exp(g*z)-1))/g)
}

# expand grid, calculate transformation of IQR.  Group the data by g and h values
# and then calculate the IQR for the transformed data and the IQR for the normal
# distribution (the second is the same for all groups, but is calculated here
# to avoid hardcoding).  Finally, compute the IQR ratio.
data <- as_tibble(expand.grid(h=h_grid, g=g_grid, z=z_grid)) %>% 
  mutate(zvals = zfun(h, g, z)) %>% group_by(h, g) %>%
  summarize(iqr_mod = max(zvals) - min(zvals), iqr=max(z)-min(z)) %>%
  ungroup() %>%
  mutate(iqr_ratio = (iqr_mod) / iqr)

data %>%
  ggplot(aes(g, h)) + geom_raster(aes(fill=iqr_ratio)) + scale_fill_gradient2(low=muted("blue"), mid="white", high=muted("red"), midpoint=1) +
  guides(fill=guide_colorbar(title="iqr ratio")) +
  theme(axis.title.x = element_text(size=15), axis.title.y = element_text(size=15),
        panel.border = element_blank(), panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour = "grey70"))


