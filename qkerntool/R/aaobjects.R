# S4 object definitions and assigment/accessor functions for the slots.


setClass("cndkernmatrix",representation("matrix"),prototype=structure(.Data=matrix()))
setClass("qkernmatrix",representation("matrix"),prototype=structure(.Data=matrix()))

#setClassUnion("output", c("matrix","factor","vector","logical","numeric","list","integer","NULL"))

setClassUnion("input", c("matrix","list"))
setClassUnion("kfunction", c("character","function"))

###############################################################################



setClass("qkprc", representation(cndkernf = "kfunction",
                               qpar = "list",
                               xmatrix = "input",
                               ymatrix = "input",
                               kcall = "ANY",
                               terms = "ANY",
                               n.action = "ANY"),contains="VIRTUAL")
####accessor functions

if(!isGeneric("cndkernf")){
  if (is.function("cndkernf"))
    fun <- cndkernf
  else fun <- function(object) standardGeneric("cndkernf")
  setGeneric("cndkernf", fun)
}
setMethod("cndkernf","qkprc", function(object) object@cndkernf)
setGeneric("cndkernf<-", function(x, value) standardGeneric("cndkernf<-"))
setReplaceMethod("cndkernf","qkprc", function(x, value){
  x@cndkernf <- value
  x
})
####

if(!isGeneric("qpar")){
  if (is.function("qpar"))
    fun <- qpar
  else fun <- function(object) standardGeneric("qpar")
  setGeneric("qpar", fun)
}
setMethod("qpar", "qkprc", function(object) object@qpar)
setGeneric("qpar<-", function(x, value) standardGeneric("qpar<-"))
setReplaceMethod("qpar", "qkprc", function(x, value) {
  x@qpar <- value
  x
})


####
if(!isGeneric("xmatrix")){
  if (is.function("xmatrix"))
    fun <- xmatrix
  else fun <- function(object) standardGeneric("xmatrix")
  setGeneric("xmatrix", fun)
}
setMethod("xmatrix","qkprc", function(object) object@xmatrix)
setGeneric("xmatrix<-", function(x, value) standardGeneric("xmatrix<-"))
setReplaceMethod("xmatrix","qkprc", function(x, value){
  x@xmatrix <- value
  x
})

####

if(!isGeneric("ymatrix")){
  if (is.function("ymatrix"))
    fun <- ymatrix
  else fun <- function(object) standardGeneric("ymatrix")
  setGeneric("ymatrix", fun)
}
setMethod("ymatrix","qkprc", function(object) object@ymatrix)
setGeneric("ymatrix<-", function(x, value) standardGeneric("ymatrix<-"))
setReplaceMethod("ymatrix","qkprc", function(x, value){
  x@ymatrix <- value
  x
})



####
if(!isGeneric("kcall")){
  if (is.function("kcall"))
    fun <- kcall
  else fun <- function(object) standardGeneric("kcall")
  setGeneric("kcall", fun)
}
setMethod("kcall","qkprc", function(object) object@kcall)
setGeneric("kcall<-", function(x, value) standardGeneric("kcall<-"))
setReplaceMethod("kcall","qkprc", function(x, value){
  x@kcall <- value
  x
})

####


setMethod("terms", "qkprc", function(x, ...) x@terms)
setGeneric("terms<-", function(x, value) standardGeneric("terms<-"))
setReplaceMethod("terms", "qkprc", function(x, value) {
  x@terms <- value
  x
})


####

if(!isGeneric("n.action")){
  if (is.function("n.action"))
    fun <- n.action
  else fun <- function(object) standardGeneric("n.action")
  setGeneric("n.action", fun)
}
setMethod("n.action","qkprc", function(object) object@n.action)
setGeneric("n.action<-", function(x, value) standardGeneric("n.action<-"))
setReplaceMethod("n.action","qkprc", function(x, value){
  x@n.action <- value
  x
})

#----------------------------------------------------------------#
##kernel principal components object
setClass("qkpca", representation(pcv = "matrix", eVal = "vector", rotated = "matrix"),contains="qkprc")

if(!isGeneric("pcv")){
  if (is.function("pcv"))
    fun <- pcv
  else fun <- function(object) standardGeneric("pcv")
  setGeneric("pcv", fun)
}
setMethod("pcv", "qkpca", function(object) object@pcv)
setGeneric("pcv<-", function(x, value) standardGeneric("pcv<-"))
setReplaceMethod("pcv", "qkpca", function(x, value) {
  x@pcv <- value
  x
})
####
if(!isGeneric("eVal")){
  if (is.function("eVal"))
    fun <- eVal
  else fun <- function(object) standardGeneric("eVal")
  setGeneric("eVal", fun)
}
setMethod("eVal", "qkpca", function(object) object@eVal)
setGeneric("eVal<-", function(x, value) standardGeneric("eVal<-"))
setReplaceMethod("eVal", "qkpca", function(x, value) {
  x@eVal <- value
  x
})
####
if(!isGeneric("rotated")){
  if (is.function("rotated"))
    fun <- rotated
  else fun <- function(object) standardGeneric("rotated")
  setGeneric("rotated", fun)
}
setMethod("rotated", "qkpca", function(object) object@rotated)
setGeneric("rotated<-", function(x, value) standardGeneric("rotated<-"))
setReplaceMethod("rotated", "qkpca", function(x, value) {
  x@rotated <- value
  x
})


#----------------------------------------------------------------#
## qkGDA perform qkernel generalized discriminant analysis object
setClass("qkgda", representation(prj = "matrix",
                                 eVal = "vector",
                                 eVec = "vector",
                                 label = "vector"),contains="qkprc")

###
if(!isGeneric("prj")){
  if (is.function("prj"))
    fun <- prj
  else fun <- function(object) standardGeneric("prj")
  setGeneric("prj", fun)
}
setMethod("prj", "qkgda", function(object) object@prj)
setGeneric("prj<-", function(x, value) standardGeneric("prj<-"))
setReplaceMethod("prj", "qkgda", function(x, value) {
  x@prj <- value
  x
})
##
#if(!isGeneric("eVal")){
#  if (is.function("eVal"))
#    fun <- eVal
#  else fun <- function(object) standardGeneric("eVal")
#  setGeneric("eVal", fun)
#}
setMethod("eVal", "qkgda", function(object) object@eVal)
setReplaceMethod("eVal", "qkgda", function(x, value) {
  x@eVal <- value
  x
})
##
if(!isGeneric("eVec")){
  if (is.function("eVec"))
    fun <- eVec
  else fun <- function(object) standardGeneric("eVec")
  setGeneric("eVec", fun)
}
setMethod("eVec", "qkgda", function(object) object@eVec)
setGeneric("eVec<-", function(x, value) standardGeneric("eVec<-"))
setReplaceMethod("eVec", "qkgda", function(x, value) {
  x@eVec <- value
  x
})

####
if(!isGeneric("label")){
  if (is.function("label"))
    fun <- label
  else fun <- function(object) standardGeneric("label")
  setGeneric("label", fun)
}
setMethod("label", "qkgda", function(object) object@label)
setGeneric("label<-", function(x, value) standardGeneric("label<-"))
setReplaceMethod("label", "qkgda", function(x, value) {
  x@label <- value
  x
})
#-----------------------------------------------------------------------#
setClass("qkIsomap", representation(prj = "matrix",
                                    dims = "numeric",
                                    connum = "numeric",
                                    Residuals = "vector",
                                    eVal = "vector",
                                    eVec = "vector"),contains="qkprc")



setMethod("prj", "qkIsomap", function(object) object@prj)
setReplaceMethod("prj", "qkIsomap", function(x, value) {
  x@prj <- value
  x
})

if(!isGeneric("dims")){
  if (is.function("dims"))
    fun <- dims
  else fun <- function(object) standardGeneric("dims")
  setGeneric("dims", fun)
}
setMethod("dims","qkIsomap", function(object) object@dims)
setGeneric("dims<-", function(x, value) standardGeneric("dims<-"))
setReplaceMethod("dims","qkIsomap", function(x, value){
  x@dims <- value
  x
})


if(!isGeneric("connum")){
  if (is.function("connum"))
    fun <- connum
  else fun <- function(object) standardGeneric("connum")
  setGeneric("connum", fun)
}
setMethod("connum","qkIsomap", function(object) object@connum)
setGeneric("connum<-", function(x, value) standardGeneric("connum<-"))
setReplaceMethod("connum","qkIsomap", function(x, value){
  x@connum <- value
  x
})

if(!isGeneric("Residuals")){
  if (is.function("Residuals"))
    fun <- Residuals
  else fun <- function(object) standardGeneric("Residuals")
  setGeneric("Residuals", fun)
}
setMethod("Residuals","qkIsomap", function(object) object@Residuals)
setGeneric("Residuals<-", function(x, value) standardGeneric("Residuals<-"))
setReplaceMethod("Residuals","qkIsomap", function(x, value){
  x@Residuals <- value
  x
})

setMethod("eVal", "qkIsomap", function(object) object@eVal)
setReplaceMethod("eVal", "qkIsomap", function(x, value) {
  x@eVal <- value
  x
})

setMethod("eVec", "qkIsomap", function(object) object@eVec)
setReplaceMethod("eVec", "qkIsomap", function(x, value) {
  x@eVec <- value
  x
})

#-----------------------------------------------------------------------#
setClass("qkLLE", representation(prj = "matrix",
                                    dims = "numeric",
                                    eVal = "vector",
                                    eVec = "vector"),contains="qkprc")



setMethod("prj", "qkLLE", function(object) object@prj)
setReplaceMethod("prj", "qkLLE", function(x, value) {
  x@prj <- value
  x
})


setMethod("dims","qkLLE", function(object) object@dims)
setReplaceMethod("dims","qkLLE", function(x, value){
  x@dims <- value
  x
})


setMethod("eVal", "qkLLE", function(object) object@eVal)
setReplaceMethod("eVal", "qkLLE", function(x, value) {
  x@eVal <- value
  x
})

setMethod("eVec", "qkLLE", function(object) object@eVec)
setReplaceMethod("eVec", "qkLLE", function(x, value) {
  x@eVec <- value
  x
})




#---------------------------------------------------------#
setClass("qtSNE", representation(dimRed = "matrix"), contains="qkprc")

if(!isGeneric("dimRed")){
  if (is.function("dimRed"))
    fun <- dimRed
  else fun <- function(object) standardGeneric("dimRed")
  setGeneric("dimRed", fun)
}
setMethod("dimRed","qtSNE", function(object) object@dimRed)
setGeneric("dimRed<-", function(x, value) standardGeneric("dimRed<-"))
setReplaceMethod("dimRed","qtSNE", function(x, value){
  x@dimRed <- value
  x
})
#---------------------------------------------------------#
setClass("qsammon", representation(dimRed = "matrix"), contains="qkprc")


setMethod("dimRed","qsammon", function(object) object@dimRed)

setReplaceMethod("dimRed","qsammon", function(x, value){
  x@dimRed <- value
  x
})

#---------------------------------------------------------#
setClass("qkdbscan", representation(clust = "vector",
                                    eps = "numeric",
                                    MinPts ="numeric",
                                    isseed = "logical"),contains="qkprc")


if(!isGeneric("clust")){
  if (is.function("clust"))
    fun <- clust
  else fun <- function(object) standardGeneric("clust")
  setGeneric("clust", fun)
}
setMethod("clust","qkdbscan", function(object) object@clust)
setGeneric("clust<-", function(x, value) standardGeneric("clust<-"))
setReplaceMethod("clust","qkdbscan", function(x, value){
  x@clust <- value
  x
})


if(!isGeneric("eps")){
  if (is.function("eps"))
    fun <- eps
  else fun <- function(object) standardGeneric("eps")
  setGeneric("eps", fun)
}
setMethod("eps","qkdbscan", function(object) object@eps)
setGeneric("eps<-", function(x, value) standardGeneric("eps<-"))
setReplaceMethod("eps","qkdbscan", function(x, value){
  x@eps <- value
  x
})


if(!isGeneric("MinPts")){
  if (is.function("MinPts"))
    fun <- MinPts
  else fun <- function(object) standardGeneric("MinPts")
  setGeneric("MinPts", fun)
}
setMethod("MinPts","qkdbscan", function(object) object@MinPts)
setGeneric("MinPts<-", function(x, value) standardGeneric("MinPts<-"))
setReplaceMethod("MinPts","qkdbscan", function(x, value){
  x@MinPts <- value
  x
})


if(!isGeneric("isseed")){
  if (is.function("isseed"))
    fun <- isseed
  else fun <- function(object) standardGeneric("isseed")
  setGeneric("isseed", fun)
}
setMethod("isseed","qkdbscan", function(object) object@isseed)
setGeneric("isseed<-", function(x, value) standardGeneric("isseed<-"))
setReplaceMethod("isseed","qkdbscan", function(x, value){
  x@isseed <- value
  x
})

#-----------------------------------------------------------------------#



setClass("qkspecc", representation(clust = "vector",
                                       eVal = "vector",
                                       eVec = "vector",
                                       withinss = "vector"),contains="qkprc")


setMethod("clust","qkspecc", function(object) object@clust)
setReplaceMethod("clust","qkspecc", function(x, value){
  x@clust <- value
  x
})

setMethod("eVal", "qkspecc", function(object) object@eVal)
setReplaceMethod("eVal", "qkspecc", function(x, value) {
  x@eVal <- value
  x
})


setMethod("eVec", "qkspecc", function(object) object@eVec)
setReplaceMethod("eVec", "qkspecc", function(x, value) {
  x@eVec <- value
  x
})

if(!isGeneric("withinss")){
  if (is.function("withinss"))
    fun <- withinss
  else fun <- function(object) standardGeneric("withinss")
  setGeneric("withinss", fun)
}
setMethod("withinss", "qkspecc", function(object) object@withinss)
setGeneric("withinss<-", function(x,value) standardGeneric("withinss<-"))
setReplaceMethod("withinss", "qkspecc", function(x, value) {
  x@withinss <- value
  x
})


setClass("qkMDS", representation(prj = "matrix",
                                    dims = "numeric",
                                    connum = "numeric",
                                    Residuals = "vector",
                                    eVal = "vector",
                                    eVec = "vector"),contains="qkprc")



setMethod("prj", "qkMDS", function(object) object@prj)
setReplaceMethod("prj", "qkMDS", function(x, value) {
  x@prj <- value
  x
})


setMethod("dims","qkMDS", function(object) object@dims)
setReplaceMethod("dims","qkMDS", function(x, value){
  x@dims <- value
  x
})



setMethod("connum","qkMDS", function(object) object@connum)
setReplaceMethod("connum","qkMDS", function(x, value){
  x@connum <- value
  x
})


setMethod("Residuals","qkMDS", function(object) object@Residuals)
setReplaceMethod("Residuals","qkMDS", function(x, value){
  x@Residuals <- value
  x
})

setMethod("eVal", "qkMDS", function(object) object@eVal)
setReplaceMethod("eVal", "qkMDS", function(x, value) {
  x@eVal <- value
  x
})

setMethod("eVec", "qkMDS", function(object) object@eVec)
setReplaceMethod("eVec", "qkMDS", function(x, value) {
  x@eVec <- value
  x
})










