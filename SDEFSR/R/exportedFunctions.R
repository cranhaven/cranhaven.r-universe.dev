
#' Launch a web interface for use the algorithms easily.
#' @description Launches a Shiny-based interface for the package in your browser.
#'     
#' @details The package \code{SDEFSR} provide simple, shiny-based web interface for performs the taks 
#'     easily. The interface only work with new datasets loaded directly in the platform.
#'   
#'     The web application is structured as follows:
#' \itemize{
#'     \item{ The first you have to do is load your training and test files. This files must be valids KEEL format files.}
#'     \item{ After chose your datasets, you can view information about the dataset or execute the algorithm}
#'     \item{ You can choose the target variable or the variable to visualize and choose the target value or execute the algorithm for all the values.}
#'     \item{ Choosed the target variable, you can  choose the algorithm to execute and change his parameters with the controls provided.}
#'     \item{ After you can execute the algorithm. The results are exposed in three tabs that are at the top of the page, just at the right of the "Exploratory Analysis" tab.}
#' }
#'     The tables can be sorted for each value and also you can search and filter values.
#'     
#'     
#' @examples
#'\dontrun{
#' library(SDEFSR)
#' SDEFSR_GUI()
#'}
#'     
#' @export
SDEFSR_GUI <- function(){
  packages <- installed.packages()[,1]
  
  if(! "ggplot2" %in% packages){
    if(tolower(.yesno("Package 'ggplot2' is not installed and must be installed to run this GUI. Do you want to install it? (Y/n): ")) == "y"){
      install.packages("ggplot2")
    } else {
      stop("Package 'ggplot2' not available")
    }
  }
  
  
  if(! "shiny" %in% packages){
    if(tolower(.yesno("Package 'shiny' is not installed and must be installed to run this GUI. Do you want to install it? (Y/n): ")) == "y"){
      install.packages("shiny")
      message("Launching interface...", appendLF = T)
      shiny::runApp(appDir = system.file("shiny", package="SDEFSR"), launch.browser = TRUE)
      
      invisible()
    } else {
      stop("Package not installed. Execution aborted.")
    }
  } else {
    shiny::runApp(appDir = system.file("shiny", package="SDEFSR"), launch.browser = TRUE)
    
    invisible()
  }
}







#' S3 function to summary a SDEFSR_Dataset object
#' 
#' Summary relevant data of a \code{SDEFSR_Dataset} dataset.
#' 
#' @param object A \code{SDEFSR_Dataset} class.
#' @param ... Additional arguments to the summary function.
#' 
#' @details This function show important information about the \code{SDEFSR_Dataset} dataset for the user. Note that it does not 
#' show all the information available. The rest is only for the algorithms. The values that appear are accessible by the
#' \code{$} operator, e.g. dataset$relation or dataset$examplesPerClass.
#' 
#' @examples 
#'  
#' summary(carTra) 
#' 
#' @export
summary.SDEFSR_Dataset <- function(object, ...){
  message(paste(paste("Summary of the SDEFSR_Dataset object: '", substitute(object),"'", sep = ""),
      paste("\t- relation:", object$relation),
      paste("\t- nVars:", object$nVars),
      paste("\t- Ns:", object$Ns),
      paste("\t- attributeNames:", paste(object$attributeNames, collapse = ", ")),
      paste("\t- class_names:", paste(object$class_names, collapse = ", ")),
      paste("\t- examplesPerClass:" ,paste(unlist(object$examplesPerClass), collapse = ", "))
      , sep = "\n"))
}







#'  S3 function to print in console the contents of the dataset
#'  
#'  This function shows the matrix of data uncoded.
#'  
#' @param x The \code{SDEFSR_Dataset} object to view
#' @param ... Additional arguments passed to the print function
#'  
#' @details This function show the matix of data. Internally, a \code{SDEFSR_Dataset} object has a list of of examples
#'  and this examples are coded numerically. This function decode these examples and convert the list into a matrix.
#'  
#' @return a matrix with the dataset uncoded.
#'  
#'  @examples
#'  
#'  print(habermanTra)
#'  
#' @export
print.SDEFSR_Dataset <- function(x, ...){
  data <- lapply(x$data,
                 function(x, categoricos)
                   vapply(seq_len(length(x)), function(i, example, cateValues){
                     if(is.na(cateValues[[i]][1])){
                       as.character(example[i])
                     } else{
                       cateValues[[i]][example[i] + 1]
                     }
                   }, character(1), x, categoricos)
                 
                 , x$categoricalValues
                 
  )
  
  print(matrix(data = unlist(data), ncol = x$nVars + 1, byrow = TRUE, dimnames = list(NULL,x$attributeNames)), ...)
}



#'  S3 function to convert into a data.frame the SDEFSR dataset
#'  
#'  This function converts a SDEFSR_Dataset object into a data.frame
#'  
#' @param x The \code{SDEFSR_Dataset} object to view
#' @param ... Additional arguments passed to the as.data.frame function
#'  
#' @details  Internally, a \code{SDEFSR_Dataset} object has a list of of examples
#'  and this examples are coded numerically. This function decode these examples and convert the list into a data.frame
#'  
#' @return a data.frame with the dataset uncoded. Numeric attributes are "numeric" class, while categorical attributes are "factor"
#'  
#'  @examples
#'  
#'  as.data.frame(habermanTra)
#'  
#' @export
as.data.frame.SDEFSR_Dataset <- function(x, ...){
  
  frame <- as.data.frame(matrix(unlist(x$data), ncol = x$nVars + 1, byrow = T, dimnames = list(NULL, x$attributeNames)))
  
  newData <- lapply(seq_len(length(frame)), function(i, dat, categorical){
    if(x$attributeTypes[i] == "c"){
      #Categorical, convert to factor
      factor(categorical[[i]][dat[,i] + 1], levels = categorical[[i]])
    } else {
      #Numeric variable, do nothing
      dat[,i]
    }
  }, frame, x$categoricalValues)
  
  as.data.frame(newData, col.names = x$attributeNames)
}


#' 
#' Plot a rule set generated by a SDEFSR algorithm
#' 
#' This function plots the rule set by means of a bar graph that shows TPR vs FPR quality measure of each rule
#' 
#' @param x an \code{SDEFSR_Rules} object generated by a subgroup discovery algorithm of the SDEFSR package
#' @param ... additional arguments passed to the plot
#' 
#' @details This function works depending on the package ggplot2 that allow to generate such graph. If the package ggplot2 is not 
#' installed, the this function ask the user to install it. After install, load the package and show the graph.
#' 
#'  A TPR vs FPR graph show the precision of a rule. Quality rules has big TPR values and small FPR values.
#'  Big values of both quality measures indicates that the rule is too much general and it is too obvious. 
#'  Small values of both indicates that the rule is too much specific and it would be an invalid rule.
#' 
#' @return A TPR vs FPR graph generated by ggplot2 
#' 
#' @examples 
#' plot(habermanRules)
#'
#' @export
#' 
plot.SDEFSR_Rules <- function(x, ...){
  if(class(x) != "SDEFSR_Rules"){
    stop(paste(substitute(x), "must be of class 'SDEFSR_Rules'"))
  }
  
  # Check for neccesary packages
  packages <- installed.packages()[,1]
  
  if(! "ggplot2" %in% packages){
    if(tolower(.yesno("Package 'ggplot2' is not installed and must be installed to show the rule set plot. Do you want to install it? (Y/n): ")) == "y"){
      install.packages("ggplot2")
    } else {
      stop("Package ggplot2 is not available")
    }
  }
  
  #Create a list with the neccessary data (Tpr, fpr and rule number)
  s <- lapply(1:length(x), 
              function(count, x){ 
                data.frame(Rule = c(paste("Rule", count), paste("Rule", count)),
                           value = c(x[[count]]$qualityMeasures$TPr, -x[[count]]$qualityMeasures$FPr), #Fpr goes in negative to show correctly in the graph
                           qualityMeasure = c("TPR", "FPR") #To show the legend and set colours of the bars
                )
              }, x)
  #Join all elements of the list into a single data.frame
  s <- do.call(rbind,s)
  
  #Create the graph with ggplot
  #We use aes_string instead of aes to skip the NOTE from R CMD check
  ggplot2::ggplot(data = s, mapping = ggplot2::aes_string(x = 'Rule', y = 'value', fill = 'qualityMeasure')) +
    ggplot2::geom_bar(data=subset(s,s$qualityMeasure=="TPR"), stat = "identity", position = "identity") + 
    ggplot2::geom_bar(data=subset(s,s$qualityMeasure=="FPR"), stat = "identity", position = "identity") +
    ggplot2::scale_y_continuous(limits = c(-1,1), labels = abs) +  #Represent labels of the x axis as all positive
    ggplot2::coord_flip()
}









#'  @title Return an ordered rule set by a given quality measure
#'  
#'  @description This function sorts a rule set in descendant order by a given quality measure that are available on the object
#'  
#' @param x The rule set passed as a \code{SDEFSR_Rules} object 
#' @param decreasing A logical indicating if the sort should be increasing or decreasing. By default, decreasing.
#' @param ... Additional parameters as "by", a String with the name of the quality measure to order by. Valid values are: \code{nVars, Coverage, Unusualness, Significance, FuzzySupport, Support, FuzzyConfidence, Confidence, Tpr, Fpr}.
#' 
#' @details  The additional argument in "..." is the 'by' argument, which is a s
#'     string with the name of the quality measure to order by. Valid values are: 
#'     \code{nVars, Coverage, Unusualness, Significance, FuzzySupport, Support, FuzzyConfidence, Confidence, Tpr, Fpr}.
#' @return another \code{SDEFSR_Rules} object with the rules sorted
#'  
#' @examples 
#'  sort(habermanRules)
#'  
#' @export
sort.SDEFSR_Rules <- function(x, decreasing = TRUE, ...){
  
  #Catch dots arguments
  by <- list(...)
  if(length(by) > 1){
    stop("The only additional parameter is 'by' which define the quality measure to sort the rules.")
  } 
  
  #Default sorting method: Confidence
  if(length(by) == 0){
    by <- "Confidence"
  } else {
    by <- by[[1]]
  }
  
  if(class(x) != "SDEFSR_Rules"){
    stop(paste(substitute(x), "is not a 'SDEFSR_Rules' object"))
  }
  
  if(length(x) < 1){
    stop(paste("Length of",substitute(x), "is less than 1."))
  }
  
  #Check if valid 'by' is chosen
  if(! by %in% names(x[[1]]$qualityMeasures)){
    stop(paste("Invalid 'by' value. Valid values are:", paste(names(x[[1]]$qualityMeasures), collapse = ", ")))
  }
  
  #Get the quality measure and get the order index
  orden <- order(sapply(x, function(x, measure) x$qualityMeasures[[measure]], by), decreasing = decreasing)
  
  #Return the rule set ordered
  result <- x[orden]
  
  #Assing the SDEFSR_Rules class
  class(result) <- "SDEFSR_Rules"
  #Return
  result
}



#' @title Filter rules in a \code{SDEFSR_Rules} object returning a new \code{SDEFSR_Rules} object
#' @description Generates a new \code{SDEFSR_Rules} object containing the rules that passed the filter
#' specified
#' @param SDEFSR_RulesObject The \code{SDEFSR_RulesObject} object to filter
#' @param condition Expression to filter the \code{SDEFSR_Rules} object
#' 
#' @details  This functions allows to filter the rule set by a given quality measure. The quality measures 
#'     that are available are: \code{nVars, Coverage, Unusualness, Significance, FuzzySupport, 
#'     Support, FuzzyConfidence, Confidence, TPr and FPr}
#' @examples 
#' library(SDEFSR)
#' #Apply filter by unusualness
#' habermanRules[Unusualness > 0.05]
#' 
#' #Also, you can make the filter as complex as you can
#' #Filter by Unusualness and TPr
#' habermanRules[Unusualness > 0.05 & TPr > 0.9]
#' 
#' 
#' @export
"[.SDEFSR_Rules" <- function(SDEFSR_RulesObject, condition = T){
  filter <- substitute(condition)
  #Check if condition is a function call like 'Unusualness > 0.2' or 'c(1:n)' 
  if(is.call(filter)){
    a <- NULL
    #If condition is something like 'c(1:n)' or other function that can be evaluated in the local 
    #environment of this function, it is evaluated. 
    tryCatch(
      a <- eval(filter), #Evaluates the expression.
      #If it fails, assing the value 'NULL' to tha variable 'a' in the local function environment
      error = function(e) assign("a", NULL, envir = parent.frame())
      )
    
    #If the 'a' is NULL means that local evaluation failed. Thus, we must evaluate the condition in the environment
    #of the each $qualityMeasures field for each rule to return a result.
    if(is.null(a)){
    #Eval the filter supplied on each rule:
    rulesToKeep <- sapply(1:length(SDEFSR_RulesObject), function(x){
      eval(filter, SDEFSR_RulesObject[[x]]$qualityMeasures, parent.frame())
    })
    } else {
      rulesToKeep <- a
    }
      
    #Change the class to a list to easily apply the new filter:
    class(SDEFSR_RulesObject) <- "list"
    #Apply the filter and return
    newRuleSet <- SDEFSR_RulesObject[rulesToKeep]
    class(newRuleSet) <- "SDEFSR_Rules" #Return as a SDEFSR_Rules object
    newRuleSet
  
  } else {
    #If not, the condition could be a logical or a numeric. Therefore, return those rules indicated by
    #numbers of logical values
    #Change to a list to easily return the object filtered:
    class(SDEFSR_RulesObject) <- "list"
    newRuleSet <- SDEFSR_RulesObject[condition]
    class(newRuleSet) <- "SDEFSR_Rules" #Return as a SDEFSR_Rules object
    newRuleSet
  }
}


