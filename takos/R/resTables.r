#'table.avrami
#' @description examples of functions for presenting the results obtained with different methods


#' Title summaryTable A
#'
#' @param mat.mod output matrix from avrami function
#'
#' @return table with the summary of result of applying the avrami function on the selected thermograms
#' @import broom
#' @export
#'

summaryTableA <- function(mat.mod) {

#require(broom)
allmod.list<-rbindlist(lapply(mat.mod$mod$V1,function(x) tidy(x)))
rep.rate <- length(unique(allmod.list$term))
rate<- rep(mat.mod$mod$rate,each = rep.rate)
res.table <- data.table(data.table(rate),allmod.list)
return(res.table)}



#' Title summaryTableOz
#'
#' @param mat.mod output matrix from ozawa function
#'
#' @return table with the summary of result of applying the ozawwa function on the selected thermograms
#' @export
#' @import broom
#'

summaryTableOz <- function(mat.mod) {
#require(broom)
allmod.list<-rbindlist(lapply(mat.mod$mod$V1,function(x) tidy(x)))
rep.Tdeg <- length(unique(allmod.list$term))
T.deg<- rep(mat.mod$mod$T.deg,each = rep.Tdeg)
res.table <- data.table(data.table(T.deg),allmod.list)
return(res.table)}


#' Title summaryTable Mo
#'
#' @param mat.mod output matrix from Mo function
#'
#' @return table with the summary of result of applying the ozawwa function on the selected thermograms
#' @export
#' @import broom
#'

summaryTableMo <- function(mat.mod) {
#require(broom)
allmod.list<-rbindlist(lapply(mat.mod$mod$V1,function(x) tidy(x)))
rep.rit <- length(unique(allmod.list$term))
rit<- rep(mat.mod$mod$rit,each = rep.rit)
res.table <- data.table(data.table(rit),allmod.list)
return(res.table)}

#' Title summaryTableF
#'
#' @param mat.mod output matrix from friedman function
#'
#' @return table with the summary of result of applying the friedman function on the selected thermograms
#' @export
#' @import broom
#'

summaryTableFri <- function(mat.mod) {
#require(broom)
allmod.list<-rbindlist(lapply(mat.mod$mod$V1,function(x) tidy(x)))
rep.rit <- length(unique(allmod.list$term))
rit<- rep(mat.mod$mod$rit,each = rep.rit)
Ea<-data.table(unlist(mat.mod$Ea))
colnames(Ea)=c("Ea")
empty.lines <- rep("",nrow(Ea))
Ea.dup <- c(rbind(as.numeric(unlist(Ea)), empty.lines))
res.table <- data.table(data.table(rit),allmod.list,data.table(Ea.dup))
return(res.table)}

#' Title summaryTableK
#'
#' @param mat.mod output matrix from Starink function
#'
#' @return table with the summary of result of applying the starink function on the selected thermograms
#' @export
#' @import broom
#'

summaryTableKiss <- function(mat.mod) {
#require(broom)
ris<-data.table(tidy(mat.mod$mod))
Ea <- data.table(mat.mod$Ea)
xy <-data.table(mat.mod$xy)
res.table <-data.table(xy,Ea,ris)
return(res.table)}






