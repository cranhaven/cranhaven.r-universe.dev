## Preload all Suggests so that examples don't take tons of time
requireNamespace("NLMR", quietly = TRUE)
requireNamespace("raster", quietly = TRUE)
requireNamespace("DEoptim", quietly = TRUE)
requireNamespace("sf", quietly = TRUE)
requireNamespace("sp", quietly = TRUE)
requireNamespace("terra", quietly = TRUE)

library(data.table)
origDTthreads <- setDTthreads(2L)

opts <- options(
  Ncpus = 2L,
  sp_evolution_status = 2
)

if (interactive() && Sys.info()["user"] %in% c("emcintir", "achubaty"))
  if (requireNamespace("quickPlot", quietly = TRUE))
    quickPlot::dev()

withr::defer({
  data.table::setDTthreads(origDTthreads)
  options(opts)
}, teardown_env())
