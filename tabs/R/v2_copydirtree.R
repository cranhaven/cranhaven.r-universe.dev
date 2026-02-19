# # new directory to store shallow water rasters
# copy.dir.tree <- function (from, to){
#   # to = full path to copy the structure to, it will create the directory as well
#   # from = full path to copy the structure from
#   directory <- to
#   dir.create(directory)
#   p <- from
#   dir_l <- list.dirs(p,recursive=TRUE, full.names=FALSE) # list tiles
# 
#   lapply(dir_l[2:length(dir_l)], function(x) dir.create(paste0(directory,x)))
# }
