## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(dplyr)
library(stringr)
library(swfscDAS)

## ----data---------------------------------------------------------------------
y <- system.file("das_sample.das", package = "swfscDAS")
head(readLines(y))

## ----check, eval=FALSE--------------------------------------------------------
#  # Code not run
#  y.check <- das_check(y, skip = 0, print.cruise.nums = TRUE)

## ----readproc-----------------------------------------------------------------
# Read 
y.read <- das_read(y, skip = 0)
glimpse(y.read)

# Process
y.proc <- das_process(y)
glimpse(y.proc)

# Note that das_read can read multiple files at once
y2.read <- das_read(c(y, y))

## ----readprocother------------------------------------------------------------
# The number of each event
table(y.proc$Event)

# The number of events per Beaufort value
table(y.proc$Bft)

# Filter for R and E events to extract lat/lon points
y.proc %>% 
  filter(Event %in% c("R", "E")) %>% 
  select(Event, Lat, Lon, Cruise, Mode, EffType) %>% 
  head()

## ----sight--------------------------------------------------------------------
y.sight <- das_sight(y.proc, return.format = "default")
y.sight %>% 
  select(Event, SightNo:PerpDistKm) %>% 
  glimpse()

y.sight.wide <- das_sight(y.proc, return.format = "wide")
y.sight.wide %>% 
  select(Event, SightNo:PerpDistKm) %>% 
  glimpse()

y.sight.complete <- das_sight(y.proc, return.format = "complete")
y.sight.complete %>% 
  select(Event, SightNo:PerpDistKm) %>% 
  glimpse()

## ----sight2-------------------------------------------------------------------
y.sight.sg <- das_sight(y.proc, return.events = c("S", "G"))

# Note that this is equivalent to:
y.sight.sg2 <- das_sight(y.proc) %>% filter(Event %in% c("S", "G"))

## ----eff----------------------------------------------------------------------
# Chop the effort into 10km segments
y.eff.eq <- das_effort(
  y.proc, method = "equallength", seg.km = 10, dist.method = "greatcircle", 
  num.cores = 1
)

# Chop the effort every time a condition changes
y.eff <- das_effort(
  y.proc, method = "condition", seg.min.km = 0, 
  dist.method = "greatcircle", conditions = c("Bft", "SwellHght", "Vis"), 
  num.cores = 1
)
y.eff.sight <- das_effort_sight(y.eff, sp.codes = c("018", "076"))

glimpse(y.eff.sight$segdata)

glimpse(y.eff.sight$sightinfo)

## ----strata_int---------------------------------------------------------------
stratum.file <- system.file("das_sample_stratum.csv", package = "swfscDAS")
y.eff.sight.strata <- das_intersects_strata(y.eff.sight, list(InPoly = stratum.file))

glimpse(y.eff.sight.strata$segdata)

## ----strata_eff---------------------------------------------------------------
y.eff.strata.section <- das_effort(
  y.proc, method = "section", strata.files = list(stratum.file),
  num.cores = 1
)

y.eff.strata.condition <- das_effort(
  y.proc, method = "condition", seg.min.km = 0, 
  strata.files = list(Poly1 = stratum.file),
  num.cores = 1
)

glimpse(y.eff.strata.section$segdata)

## ----comm---------------------------------------------------------------------
y.comm <- das_comments(y.proc)
glimpse(select(y.comm, Event, line_num, comment_str))

y.comm[str_detect(y.comm$comment_str, "gear"), ] #Could also use grepl() here

