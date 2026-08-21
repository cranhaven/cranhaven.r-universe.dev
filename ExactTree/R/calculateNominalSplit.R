# calculateNominalSplit<-function(cat){
#
#   ncat<-length(cat)
#   split<-list()
#   cat1<-cat[1]
#   split[[1]]<-cat1
#   listIndex<-1
#   for (i in 2:(ncat-1)) {
#     index <- which(combn(cat,i)[1,]==cat1)
#     for (j in index) {
#       listIndex<-listIndex + 1
#       split[[listIndex]]<-combn(cat,i)[,j]
#     }
#
#
#   }
#
#   return(split)
#
#
# }
#
#
#
#
# calculateAllNominalSplit<-function(cat){
#
#   ncat<-length(cat)
#   split<-list()
#   cat1<-cat[1]
#   split[[1]]<-cat1
#   split[[2]]<-cat[-1]
#   listIndex<-2
#   for (i in 2:(ncat-1)) {
#     index <- which(combn(cat,i)[1,]==cat1)
#     for (j in index) {
#       listIndex<-listIndex + 1
#       split[[listIndex]]<-combn(cat,i)[,j]
#       listIndex<-listIndex + 1
#       split[[listIndex]]<-cat[! cat %in% combn(cat,i)[,j]]
#     }
#
#
#   }
#
#   return(split)
#
#
# }
#


