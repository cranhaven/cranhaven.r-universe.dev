## ----schunk, setup, include=FALSE---------------------------------------------
knitr::opts_chunk$set(include = FALSE)
library(NADIA)

## ----ochunk, echo=FALSE, fig.align='center', include=TRUE, out.width='100%'----
knitr::include_graphics('rect1029.png')

## ----wchunk, echo=FALSE, fig.align='center', include=TRUE, out.width='100%'----
knitr::include_graphics('B.svg')

## ----pchunk, echo=FALSE, warning=FALSE, include=TRUE, out.width='70%'---------

benchmark_data <- read.csv("benchmark_2-packages.csv")
benchmark_data <- benchmark_data[1:28,]
benchmark_data <- benchmark_data[,-ncol(benchmark_data)]

task_number <- 25

succes <- as.numeric(as.character(unlist(benchmark_data[27,-1,drop=TRUE])))


methods <- colnames(benchmark_data[,-1])

methods <- substr(methods,start = 7,stop = 999)


Resoults <- as.data.frame(matrix(nrow = 2,ncol = 11))

row.names(Resoults) <- c("Succesful tasks","Percent of succesful tasks")
colnames(Resoults) <- methods

Resoults[1,] <- paste(succes,task_number,sep = "/")

Resoults[2,] <- paste0(round(succes/25*100,2),"%")
res<- cbind(methods,t(Resoults))
row.names(res) <- NULL
colnames(res)[1] <- "Package_method"


`%>%` <- magrittr::`%>%`

knitr::kable(res,escape = FALSE,format = 'html',caption = "NADIA test results") %>% kableExtra::column_spec(1:3,bold=TRUE) %>% kableExtra::row_spec(c(1,9),color = "white",background = "#FF5733") %>% kableExtra::row_spec(c(2,4,5,11),color = "white",background = "#F5A9A9")

## ----Lrn, cache=TRUE, include=FALSE-------------------------------------------


graph <- PipeOpMice$new()%>>% lrn('classif.debug')

Learner <- GraphLearner$new(graph)


Learner$param_set$values$classif.debug.error_train	=1 
Learner$param_set$values$classif.debug.error_predict	=1 

## ----d, echo=TRUE, include=TRUE,cache=TRUE, dependson=-1----------------------

# Encaplustion with Evalute
Learner$encapsulate=c(train="evaluate",predict="evaluate")


# Resampling with errors and presenting errors
resample(tsk("pima"),Learner,rsmp("cv",folds=5))$errors


## ----kchunk,dependson=-1,cache=T ,echo=TRUE,dependson=-1,include=TRUE---------

# encaplustion with callr
Learner$encapsulate=c(train="callr",predict="callr")


# Resampling with errors and presenting errors
resample(tsk("pima"),Learner,rsmp("cv",folds=5))$errors



