### General utilities that have no direct link to Shiny

`%not in%` <- Negate(`%in%`)

## Shortcut usefull to page 'summarise'
wtd.sum     <- function(x, weights, na.rm=TRUE) sum(weights)*Hmisc::wtd.mean(x,weights,na.rm)

## Shortcut usefull to page 'tabular'
wtd.percent <- function (x, y) 100*sum(x)/sum(y)

## Extract the file extension in lowercase
..pathExt <- function (path) path %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()


## Get the list of tables that are within a RData file
## NB: This is just for demonstration, as the whole process of reading one object
## will read the whole file twice, one for objects list, one for the selected object
## A better approach for such needs will be to use lazy loading
##   # convert .RData -> .rdb/.rdx
##   e <- local({load("New.RData"); environment()})
##   tools:::makeLazyLoadDB(e, "New")
##   lazyLoad("New")
..RDataContents <- function (path) {
  load(path)
  unlist(
    Filter(function(x) is.data.frame(get(x)),ls())
  )
}

## Two ways of protecting unnormalized names
..name  <- function(x) ifelse(x==make.names(x),x,paste0('`',x,'`'))
..nameg <- function(x) ifelse(x==make.names(x),x,paste0('(`',x,'`)')) # for formulas in graphics


..collapse0 <- function(x) paste(x,collapse=', ')
..collapse  <- function(x) paste(..name(x),collapse=', ')
..collapse1 <- function(x) paste(ifelse(is.na(x),"NA",paste0('"',x,'"')),collapse=', ')
..collapse2 <- function(x)
  if (length(x)==0) ""    else if (length(x)==1) paste0('"',x,'"') else paste0("c(",..collapse1(x),")")
..collapse3 <- function(x)
  if (length(x)==0) "c()" else if (length(x)==1) paste0('"',x,'"') else paste0("c(",..collapse1(x),")")

..isCondition <- function(x) "condition" %in% class(x)

..isTRUE     <- function (x) (length(x)>0)&&x
..isFALSE    <- function (x) (length(x)>0)&&!x
..isNotNA    <- function (x) (length(x)>0)&&!is.na(x)
..isNotEmpty <- function (s) (length(s)>0)&&(nchar(s)>0)
..isFile     <- function (x) (length(x)>0)&&str_detect(x,"\\.")
..isEQ     <- function (x,y) (length(x)>0)&&(!is.na(x))&&(x==y)
..isNE     <- function (x,y) (length(x)>0)&&(!is.na(x))&&(x!=y)
..isIn     <- function (x,y) (length(x)>0)&&(x %in% y)
..inOrNULL <- function (x,y) (length(x)==0)||(x %in% y)
