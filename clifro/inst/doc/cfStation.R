## ---- echo = FALSE------------------------------------------------------------
library(clifro)

## ---- eval = FALSE------------------------------------------------------------
#  lake.tekapo.st = cf_station(12709, 35567, 39557, 4630, 24945, 4616, 4602)
#  lake.tekapo.st[, c("name", "agent", "start", "end", "open")]

## ---- eval = FALSE------------------------------------------------------------
#  added.stations.st = lake.tekapo.st +
#    cf_station() +
#    cf_find_station("lighthouse", status = "all")
#  added.stations.st[, c("name", "agent", "start", "end", "open")]

