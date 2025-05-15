## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(dplyr)
library(tibble) #For printing output
library(stringr)
library(swfscAirDAS)

## ----data---------------------------------------------------------------------
y <- system.file("airdas_sample.das", package = "swfscAirDAS")
head(readLines(y))

## ----check, eval=FALSE--------------------------------------------------------
#  # Code not run
#  y.check <- airdas_check(y, file.type = "turtle", skip = 0, print.transect = TRUE)

## ----readproc-----------------------------------------------------------------
# Read 
y.read <- airdas_read(y, file.type = "turtle", skip = 0)
head(y.read)

# Process
y.proc <- airdas_process(y.read, trans.upper = TRUE)
head(y.proc)

## ----readprocother------------------------------------------------------------
# The number of events per Beaufort value
table(y.proc$Bft)

# Filter for T/R and O/E events to extract lat/lon points
y.proc %>% 
  filter(Event %in% c("T", "R", "E", "O")) %>% 
  select(Event, Lat, Lon, Trans)

## ----sight--------------------------------------------------------------------
y.sight <- airdas_sight(y.proc)

y.sight %>% 
  select(Event, SightNo:TurtleTail) %>% 
  head()

## ----eff----------------------------------------------------------------------
# Chop the effort every time a condition changes
y.eff <- airdas_effort(
  y.proc, method = "condition", seg.min.km = 0, 
  dist.method = "greatcircle", conditions = c("Bft", "CCover"), 
  num.cores = 1, angle.min = 12, bft.max = 1
)
y.eff.sight <- airdas_effort_sight(y.eff, sp.codes = c("bm", "dc"))

head(y.eff.sight$segdata)

head(y.eff.sight$sightinfo)

## ----eff2---------------------------------------------------------------------
# 'Chop' the effort by continuous effort section
y.eff.section <- airdas_effort(
  y.proc, method = "section", dist.method = "greatcircle", conditions = NULL, 
  num.cores = 1
)
y.eff.section$sightinfo <- y.eff.section$sightinfo %>% 
  mutate(included = ifelse(.data$SpCode == "mn" & abs(.data$Angle > 60), FALSE, .data$included))

y.eff.section.sight <- airdas_effort_sight(y.eff.section, sp.codes = c("mn"))

y.eff.section.sight[[1]] %>% 
  mutate(transect_id = cumsum(.data$event == "T")) %>% 
  group_by(transect_id) %>% 
  summarise(dist_sum = sum(dist), 
            mn_count = sum(ANI_mn)) %>% 
  as.data.frame() #for printing format

## ----comm---------------------------------------------------------------------
y.comm <- airdas_comments(y.proc)
head(y.comm)

str_subset(y.comm$comment_str, "record") #Could also use grepl() here

## ----comm2--------------------------------------------------------------------
# comment.format for default CARETTA data
comment.format <- list(
  n = 5, sep = ";", 
  type = c("character", "character", "numeric", "numeric", "character")
)

# TURTLE/PHOCOENA comment-data
head(airdas_comments_process(y.proc))

