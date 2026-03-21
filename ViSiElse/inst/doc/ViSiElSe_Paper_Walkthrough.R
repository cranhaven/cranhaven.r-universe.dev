## ---- warning=FALSE, eval=FALSE------------------------------------------
#  install.packages("ViSiElse", dependencies = T) # installation

## ---- warning=FALSE------------------------------------------------------
library(ViSiElse) # load

## ---- warning=FALSE, eval=FALSE------------------------------------------
#  data("typDay") # load typDay dataset
#  head(typDay) # print first rows
#  ?typDay # Information about the data
#  
#  data("intubation") # load intubation dataset
#  head(intubation) # print first rows
#  ?intubation # Information about the data
#  
#  data("shoppingBehavior") # load shoppingBehavior dataset
#  head(shoppingBehavior) # print first rows
#  ?shoppingBehavior # Information about the data

## ---- warning=FALSE------------------------------------------------------
data("typDay") # load typDay dataset
head(typDay) # print first rows

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
v1 <- visielse(typDay, informer = NULL) # informer = NULL removes the summary statitics that are displayed by default (we will talk about it later).

## ---- warning=FALSE------------------------------------------------------
b1 <- ConvertFromViSibook(v1@book) # Extract the visibook in a data.frame
b1 

## ---- warning=FALSE------------------------------------------------------
b1 <- b1[order(as.numeric(b1$showorder)), ] # order the data.frame 
# Change the labels of the punctual actions #
b1$label <- c("Sleep", "Stop sleeping", "Wake up", "Take a shower", "Eat breakfast", "Start working", "Start eating lunch", "End of lunch", "Stop working", "Pick up the kids", "Start cooking", "End of dinner", "Go to sleep", "First coffee")
# Define the long actions
b1[15,] <- c("sleep", "Sleeping", "l", 1, "start_sleep", "stop_sleep")
b1[16,] <- c("work", "Working", "l", 5, "start_work", "stop_work")
b1[17,] <- c("lunch", "Lunch break", "l", 6, "start_lunch", "stop_lunch")
b1[18,] <- c("cook", "Cook and eat dinner", "l", 8, "start_cook", "stop_cook")
# Define which actions should be plotted and in which order
b1$showorder <- c(NA, NA, 2, 3, 4, 5, NA, NA, 7, 9, NA, NA, 11, 12, 1, 6, 8, 10) 
b1 <- b1[order(as.numeric(b1$showorder)), ] # re-order the ViSibook according to the action order

# The new ViSibook
b1

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
v2 <- visielse(typDay, book = b1, informer = NULL, doplot = F, pixel = 30)
plot(v2, vp0w = 0.7, unit.tps = "min", scal.unit.tps = 30, main = "Typical day") 

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
# Small pixel parameter : data are not aggregated enough #
p1 <- 10
v3 <- visielse(typDay, book = b1, informer = NULL, doplot = F, pixel = p1)
plot(v3, vp0w = 0.7, unit.tps = "min", main = "Typical day, pixel = 10min", scal.unit.tps = p1) 

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
# High pixel parameter : data are too aggregated #
p2 <- 120
v4 <- visielse(typDay, book = b1, informer = NULL, doplot = F, pixel = p2)
plot(v4, vp0w = 0.7, unit.tps = "min", main = "Typical day, pixel = 120min", scal.unit.tps = p2)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
# Group definition
group <- rep(1, 100)
group[typDay$pickup_kids > 1019] <- 2

# Groups plotted with "cut" method : each group is one under the other #
v5 <- visielse(typDay, book = b1, informer = NULL, group = group, method = "cut", tests = F, pixel = 30, doplot = F)
plot(v5, vp0w = 0.7, unit.tps = "min", scal.unit.tps = 30, main = "Typical day, 'cut' method") 

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
# Groups plotted with "join" method : group spacially mixed #
v6 <- visielse(typDay, book = b1, informer = NULL, group = group, method = "join", tests = F, pixel = 30, doplot = F)
plot(v6, vp0w = 0.7, unit.tps = "min", scal.unit.tps = 30, main = "Typical day, 'join' method")

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
# Groups plotted with "within" method : all data are plotted together in blue 
# and the group specified in "grwithin" is plotted again in pink #
v7 <- visielse(typDay, book = b1, informer = NULL, group = group, method = "within", grwithin = "2", tests = F, pixel = 30, doplot = F)
plot(v7, vp0w = 0.7, unit.tps = "min", scal.unit.tps = 30, main = "Typical day, 'within' method")

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
b2 <- b1
b2 <- b2[order(as.numeric(b2$showorder)), ]
# Add definition of the green zones #
b2$GZDeb <- c(rep(NA, 8), 960, rep(NA, 9))
b2$GZFin <- c(rep(NA, 8), 1020, rep(NA, 9))
# Definition of the black zones before the green one #
b2$BZBeforeDeb <- c(rep(NA, 4), 600, NA, 0, NA, 0, rep(NA, 9))
b2$BZBeforeFin <- c(rep(NA, 4), Inf, NA, 960, NA, 960, rep(NA, 9))
# Add definition of the black zones after the green one #
b2$BZAfterDeb <- c(rep(NA, 8), 1020, rep(NA, 9))
b2$BZAfterFin <- c(rep(NA, 8), Inf, rep(NA, 9))

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
# Add definition of the time limit for long action #
b2$BZLong <- c(rep(NA, 7), 30, rep(NA, 10))
b2$BZLtype <- c(rep(NA, 7), "span", rep(NA, 10)) # type should either be "span" (for a duration not to exceed) or "time" (for a deadline not to cross)
b2

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
v8 <- visielse(typDay, book = b2, informer = NULL, pixel = 30, doplot = F)
plot(v8, vp0w = 0.7, unit.tps = "min", scal.unit.tps = 30, main = "Typical day") 

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
# Mean + standard deviation #
v9 <- visielse(typDay, book = b1, informer = "mean", tests = F, pixel = 30, doplot = F)
plot(v9, vp0w = 0.7, unit.tps = "min", scal.unit.tps = 30, main = "Typical day, mean + SD") 
# Median + IQR #
v10 <- visielse(typDay, book = b1, informer = "median", tests = F, pixel = 30, doplot = F)
plot(v10, vp0w = 0.7, unit.tps = "min", scal.unit.tps = 30, main = "Typical day, median + IQR") 

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
# Statistical test between groups #
v11 <- visielse(typDay, book = b1, informer = "mean", group = group, method = "cut", pixel = 30, doplot = F, tests = TRUE, threshold.test = 0.05)
plot(v11, vp0w = 0.7, unit.tps = "min", scal.unit.tps = 30, main = "Typical day")

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
data("intubation")
head(intubation)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
#### Figure 5 in ViSiElse paper ####
v16 <- visielse(intubation, doplot = F)
b3 <- ConvertFromViSibook(v16@book)
b3$label <- c("Decision to intubate", "Stop ventilation", "Laryngoscope\nblade in", "Insert endotracheal\ntube", "Laryngoscope\nblade out", "Restart ventilation")
b3[7,] <- c("dur_laryngoscope", "Laryngoscope\nduration use", "l", "8", "blade_in", "blade_out")
b3[8,] <- c("dur_intub", "Intubation duration", "l", "9", "stop_ventil", "restart_ventil")
b3$GZDeb <- c(NA, NA, 120, NA, NA, NA, NA, NA)
b3$GZDeb <- c(NA, NA, 120, NA, NA, NA, NA, NA)
b3$GZFin <- c(NA, NA, 210, NA, NA, NA, NA, NA)
b3$BZBeforeDeb <- c(NA, NA, 0, NA, NA, NA, NA, NA)
b3$BZBeforeFin <- c(NA, NA, 120, NA, NA, NA, NA, NA)
b3$BZAfterDeb <- c(NA, NA, 210, NA, NA, NA, NA, NA)
b3$BZAfterFin <- c(NA, NA, Inf, NA, NA, NA, NA, NA)
b3$BZLong <- c(rep(NA, 7), 30)
b3$BZLtype <- c(rep(NA, 7), "span")
v17 <- visielse(intubation, book = b3, informer = "median", doplot = F)
plot(v17, scal.unit.tps = 20, rcircle = 8, vp0h = 0.65, vp0w = 0.7, Fontsize.label.Action = 9, Fontsize.label.Time = 9, Fontsize.label.color = 9, main = "Intubation process in neonatal resuscitation algorithm")

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
data("shoppingBehavior")
head(shoppingBehavior)
# Define group of participants
group_shop <- c(rep(1, 50), rep(2, 50))

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
#### Figure 6 in ViSiElse paper ####
v18 <- visielse(shoppingBehavior, doplot = F)
b4 <- ConvertFromViSibook(v18@book)
b4$label <- c("Need recognition", "Start information search", "Stop information search", "Start evaluation", "Stop evaluation", "Purchase decision")
b4$showorder <- c(1, NA, NA, NA, NA, 4)
b4[7,] <- c("search", "Information search", "l", "2", "start_search", "stop_search")
b4[8,] <- c("eval", "Evaluation", "l", "3", "start_eval", "stop_eval")
v19 <- visielse(shoppingBehavior, book = b4, informer = "mean", pixel = 5, group = group_shop, method = "cut", doplot = F)
plot(v19, scal.unit.tps = 5, rcircle = 8, vp0h = 0.6, vp0w = 0.75, Fontsize.label.Action = 9, Fontsize.label.Time = 9, Fontsize.label.color = 9, lwd.grid = 1, lwdline = 2, main = "Online shopping behaviour", unit.tps = "min")

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
# load the packages
library(ggplot2) # for the heatmap
library(reshape2) # reshape the dataset to adjust its structure

# Create a frequency table with 30min time intervals
typDay2 <-  sapply(typDay[,4:15], function(x){
  table(cut(x, breaks=seq(0, 1440, 30))) 
}) 

# Reshape the dataset to fit ggplot2 data structure
typDay2 <- data.frame(time = factor(seq(0, 1410, 30)), typDay2)
rownames(typDay2) <- 1:nrow(typDay2)
colnames(typDay2) <- c("time", b1$label[c(12, 11, 18, 17, 9, 7, 16, 15, 5, 4, 3, 2)])
typDay2 <- melt(typDay2, id = "time")[, c(2, 1, 3)]

# Set 0 values to "NA"
typDay2$value[typDay2$value == 0] <- NA 
head(typDay2)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE------------
heatmap <- ggplot(data = typDay2, aes(x = time, y = variable, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "#E2E8FF", high = "#2D39A5", name = "Participants", na.value = 'white', limit = c(0, 53)) +
  xlab("Time (min)") + ylab(element_blank()) +
  scale_x_discrete(expand = c(0, 0), breaks = seq(0, 60, 30)) +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(colour = "black", size = 8)) +
  theme(legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10),
        legend.position ="bottom", 
        legend.margin = margin(0, 0, 0, 0, unit = "mm"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(3, "mm"))
print(heatmap)

## ----fig.show='asis',fig.width=7, fig.height=5, warning=FALSE, eval = FALSE----
#  help("visielse")

