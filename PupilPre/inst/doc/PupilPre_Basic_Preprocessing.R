## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4, warning=FALSE)

## ---- echo=FALSE, eval=TRUE, message=FALSE-------------------------------
library(PupilPre)
library(ggplot2)

## ---- eval= FALSE, echo=TRUE, results='asis'-----------------------------
#  library(PupilPre)
#  Pupildat <- read.table("1000HzData.txt", header = T, sep = "\t", na.strings = c(".", "NA"))

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
data(Pupildat)

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
dat0 <- ppl_prep_data(data = Pupildat, Subject = "RECORDING_SESSION_LABEL", Item = "item")

## ---- eval= FALSE, echo=TRUE, results='asis'-----------------------------
#  dat0 <- ppl_rm_extra_DVcols(dat0)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
dat1 <- create_time_series(data = dat0, Adjust = 500)

## ---- eval= TRUE, echo=FALSE, results='hide'-----------------------------
rm(Pupildat, dat0)
gc()

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_time_series(data = dat1)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_msg_time(data = dat1, Msg = "PLAY_target")

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_all_msgs(data = dat1)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
ppl_check_eye_recording(data = dat1)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
dat2 <- ppl_select_recorded_eye(data = dat1, Recording = "R", WhenLandR = "Right")

## ---- eval= TRUE, echo=FALSE, results='hide'-----------------------------
rm(dat1)
gc()

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
dat3 <- recode_off_screen(data = dat2, ScreenSize = c(1920, 1080))

## ---- eval= TRUE, echo=FALSE, results='hide'-----------------------------
rm(dat2)
gc()
# saveRDS(dat3, file = "PartialProcess_dat3.rds", compress = "xz")

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
blink_summary(dat3, Summary = "Event")

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
NA_summary(dat3, Summary = "Event", PupilColumn = "Pupil")

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
dat3 %>% filter(Event %in% c("16892.8")) %>% 
  select(Event, Pupil, Time) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event) %>% 
  ggplot(aes(x=Time, y=PUPIL)) + 
  geom_point(na.rm = T) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
dat4 <- clean_blink(dat3, BlinkPadding = c(100, 100), Delta = 5,
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
dat4 %>% filter(Event %in% c("16892.8")) %>% 
  mutate(Compared = !(compareNA(Pupil_Previous, Pupil))) %>% 
  select(Event, Pupil, Pupil_Previous, Time, Compared) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event, -Compared) %>% 
  mutate(Datapoint = ifelse(Compared==F, "Same", "Different")) %>% 
  ggplot(aes(x=Time, y=PUPIL, colour = Datapoint)) + 
  geom_point(na.rm = T) +
  scale_color_manual(values=c("Different" = "red", "Same" = "black")) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
dat4a <- clean_artifact(dat4, MADWindow = 100, MADConstant = 2,
                      MADPadding = c(200, 200), MahaConstant = 2,
                      Method = "Robust", XandY = TRUE, Second = T, 
                      MaxValueRun = 5, NAsAroundRun = c(2,2),
                      LogFile = paste0(tempdir(),"/ArtifactCleanupLog.rds"))

## ---- eval= TRUE, echo=FALSE, results='asis'-----------------------------
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
dat4a %>% filter(Event %in% c("16892.8")) %>% 
  mutate(Compared = !(compareNA(Pupil_Previous, Pupil))) %>% 
  select(Event, Pupil, Pupil_Previous, Time, Compared) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event, -Compared) %>% 
  mutate(Datapoint = ifelse(Compared==F, "Same", "Different")) %>% 
  ggplot(aes(x=Time, y=PUPIL, colour = Datapoint)) + 
  geom_point(na.rm = T) +
  scale_color_manual(values=c("Different" = "red", "Same" = "black")) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  plot_compare_app(dat4a)

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
compare_summary(dat4a)

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  user_cleanup_app(dat4a, LogFile = paste0(tempdir(),"/UserCleanupLog.rds"))

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  dat5 <- apply_user_cleanup(dat4a, LogFile = paste0(tempdir(),"/UserCleanupLog.rds"))

## ---- eval= TRUE, echo=FALSE, results='hide'-----------------------------
UserCleanupLog <- vector("list", length = length(unique(dat4a$Event)))
names(UserCleanupLog) <- unique(dat4a$Event)
UserCleanupLog[1:length(UserCleanupLog)] <- NA
UserCleanupLog[["16892.8"]] <- c(1835:1995)
saveRDS(UserCleanupLog, file = paste0(tempdir(),"/UserCleanupLog.rds"))
dat5 <- apply_user_cleanup(dat4a, LogFile = paste0(tempdir(),"/UserCleanupLog.rds"))
rm(dat4, dat4a)
gc()

## ---- eval= TRUE, echo=FALSE, results='asis'-----------------------------
# The event after automatic and manual cleaning
pac_theme <- function(base_size = 12, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(hjust = 0.5, vjust = 1)
    )
}
dat5 %>% filter(Event %in% c("16892.8")) %>% 
  select(Event, Pupil, Time) %>%
  tidyr::gather(Column, PUPIL, -Time, -Event) %>% 
  ggplot(aes(x=Time, y=PUPIL)) + 
  geom_point(na.rm = T) +
  ylab("Pupil Dilation") +
  facet_wrap(. ~ Event) + pac_theme()

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  plot_events(dat5, Column = "Pupil", Device = "pdf", Grouping = "Subject", path = "Manual", Nrow = 2, Ncol = 3, width = 11, height = 8.5)

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
dat6 <- rm_sparse_events(data = dat5, 
                         BaselineWindow = c(-500, 0), 
                         CriticalWindow = c(200, 2000),
                         BaselineRequired = 50, 
                         CriticalRequired = 50)

## ---- eval= TRUE, echo=FALSE, results='hide'-----------------------------
rm(dat5)
gc()

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
check_baseline(dat6, BaselineWindow = c(-500, 0))

## ---- eval=TRUE, echo=TRUE, results='asis'-------------------------------
dat7 <- baseline(dat6, BaselineWindow = c(-500, 0), BaselineType = "Subtraction")

## ---- eval= TRUE, echo=FALSE, results='hide'-----------------------------
rm(dat6)
gc()

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_samplingrate(dat7)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
ds_options(SamplingRate = 250)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
dat8 <- downsample(dat7, SamplingRate = 250, NewRate = 25)

## ---- eval= TRUE, echo=FALSE, results='hide'-----------------------------
rm(dat7)
gc()

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_samplingrate(dat8)

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  saveRDS(dat8, file = "FinalDat.rds", compress = "xz")

