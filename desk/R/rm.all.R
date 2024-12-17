rm.all = function(keep = NULL) {
  rm(list = ls(pos = .GlobalEnv)[! ls(pos = .GlobalEnv) %in% keep], pos = .GlobalEnv)
}
