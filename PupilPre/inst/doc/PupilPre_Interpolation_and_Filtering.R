## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4, warning=FALSE)

## ---- echo=FALSE, eval=TRUE, message=FALSE, results='hide'---------------
library(PupilPre)
library(ggplot2)
data(Pupilex4)
dat <- clean_artifact(Pupilex4, MADWindow = 100, MADConstant = 2,
                      MADPadding = c(200, 200), MahaConstant = 2,
                      Method = "Robust", XandY = TRUE, Second = T, 
                      MaxValueRun = 5, NAsAroundRun = c(2,2),
                      LogFile = paste0(tempdir(),"/ArtifactCleanupLog.rds"))
UserCleanupLog <- vector("list", length = length(unique(dat$Event)))
names(UserCleanupLog) <- unique(dat$Event)
UserCleanupLog[1:length(UserCleanupLog)] <- NA
UserCleanupLog[["16892.8"]] <- c(1835:1995)
saveRDS(UserCleanupLog, file = paste0(tempdir(),"/UserCleanupLog.rds"))
dat <- apply_user_cleanup(dat, LogFile = paste0(tempdir(),"/UserCleanupLog.rds"))
# saveRDS(dat, file = "Partial_datclean.rds", compress = "xz")
# This is the same as dat5 within the basic processing

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
datlinear <- interpolate_NAs(dat, Method = "linear", XandY = T, MinData = 2)

## ---- eval= TRUE, echo=FALSE, results='asis'-----------------------------
# Plot example
pac_theme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}
datlinear %>% filter(Event %in% c("16849.39")) %>% 
  select(Event, Pupil, Time) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event) %>% 
  ggplot(aes(x=Time, y=PUPIL)) + 
  geom_point(na.rm = T) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
datspline <- interpolate_NAs(dat, Method = "spline", XandY = T, MinData = 2)

## ---- eval= TRUE, echo=FALSE, results='asis'-----------------------------
# Plot example
pac_theme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}
datspline %>% filter(Event %in% c("16849.39")) %>% 
  select(Event, Pupil, Time) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event) %>% 
  ggplot(aes(x=Time, y=PUPIL)) + 
  geom_point(na.rm = T) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  butter_filter_app(datlinear)

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
datfilter <- apply_butter(datlinear, n = 1, W = 0.1, type = "low", plane = "z")

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
dattrim <- trim_filtered(data = datfilter, RmSkipped = TRUE, RmEdges = c(75, 75))

