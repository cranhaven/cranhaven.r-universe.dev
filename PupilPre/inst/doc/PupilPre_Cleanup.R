## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4, warning=FALSE)

## ---- echo=FALSE, eval=TRUE, message=FALSE, results='hide'---------------
library(PupilPre)
library(ggplot2)
data(Pupilex3)
dat <- recode_off_screen(data = Pupilex3, ScreenSize = c(1920, 1080))
# This is the same as dat3 within the basic processing

## ---- eval= TRUE, echo=FALSE, results='asis'-----------------------------
# Take for example Event 16892.8 has one marked blink and one unmarked blink
pac_theme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}
dat %>% filter(Event %in% c("16892.8")) %>% 
  select(Event, Pupil, Time) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event) %>% 
  ggplot(aes(x=Time, y=PUPIL)) + 
  geom_point(na.rm = T) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
datblink <- clean_blink(dat, BlinkPadding = c(100, 100), Delta = 5,
                    MaxValueRun = 5, NAsAroundRun = c(2,2),
                    LogFile = paste0(tempdir(),"/BlinkCleanupLog.rds"))

## ---- eval= TRUE, echo=FALSE, results='asis'-----------------------------
# The function successfully cleaned the marked blink
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
pac_theme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}
datblink %>% filter(Event %in% c("16892.8")) %>% 
  mutate(Compared = !(compareNA(Pupil_Previous, Pupil))) %>% 
  select(Event, Pupil, Pupil_Previous, Time, Compared) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event, -Compared) %>% 
  mutate(Datapoint = ifelse(Compared==F, "Same", "Different")) %>% 
  ggplot(aes(x=Time, y=PUPIL, colour = Datapoint)) + 
  geom_point(na.rm = T) +
  scale_color_manual(values=c("Different" = "red", "Same" = "black")) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

## ---- eval=FALSE, echo=TRUE, results='hide'------------------------------
#  verify_cleanup_app(datblink, LogFile = paste0(tempdir(),"/BlinkCleanupLog.rds"))

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
datblink <- apply_cleanup_change(datblink, LogFile = paste0(tempdir(),"/BlinkCleanupLog.rds"))

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
datart <- clean_artifact(datblink, MADWindow = 100, MADConstant = 2,
                      MADPadding = c(200, 200), MahaConstant = 2,
                      Method = "Robust", XandY = TRUE, Second = T, 
                      MaxValueRun = 5, NAsAroundRun = c(2,2),
                      LogFile = paste0(tempdir(),"/ArtifactCleanupLog.rds"))

## ---- eval=TRUE, echo=FALSE, results='asis'------------------------------
# The function partially cleaned the unmarked blink using default settings
compareNA <- function(v1,v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
pac_theme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}
datart %>% filter(Event %in% c("16892.8")) %>% 
  mutate(Compared = !(compareNA(Pupil_Previous, Pupil))) %>% 
  select(Event, Pupil, Pupil_Previous, Time, Compared) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event, -Compared) %>% 
  mutate(Datapoint = ifelse(Compared==F, "Same", "Different")) %>% 
  ggplot(aes(x=Time, y=PUPIL, colour = Datapoint)) + 
  geom_point(na.rm = T) +
  scale_color_manual(values=c("Different" = "red", "Same" = "black")) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

## ---- eval=FALSE, echo=TRUE, results='hide'------------------------------
#  verify_cleanup_app(datart, LogFile = paste0(tempdir(),"/ArtifactCleanupLog.rds"))

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
datart <- apply_cleanup_change(datart, LogFile = paste0(tempdir(),"/ArtifactCleanupLog.rds"))

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  plot_compare_app(datart)

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
compare_summary(datart)

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  user_cleanup_app(datart, LogFile = paste0(tempdir(),"/UserCleanupLog.rds"))

## ---- eval=TRUE, echo=FALSE, results='hide'------------------------------
UserCleanupLog <- vector("list", length = length(unique(datart$Event)))
names(UserCleanupLog) <- unique(datart$Event)
UserCleanupLog[1:length(UserCleanupLog)] <- NA
UserCleanupLog[["16892.8"]] <- c(1835:1995)
saveRDS(UserCleanupLog, file = paste0(tempdir(),"/UserCleanupLog.rds"))

## ---- eval = FALSE, echo = FALSE, results='asis'-------------------------
#  datclean <- apply_user_cleanup(datart, LogFile = "UserCleanupLog.rds")
#  saveRDS(datclean, file = "Partial_datclean.rds", compress = "xz")

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
datclean <- apply_user_cleanup(datart, LogFile = paste0(tempdir(),"/UserCleanupLog.rds"))

## ---- eval=TRUE, echo=FALSE, results='asis'------------------------------
# The event after automatic and manual cleaning
#datclean <- readRDS(file = "Partial_datclean.rds")
pac_theme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}
datclean %>% filter(Event %in% c("16892.8")) %>% 
  select(Event, Pupil, Time) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event) %>% 
  ggplot(aes(x=Time, y=PUPIL)) + 
  geom_point(na.rm = T) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

