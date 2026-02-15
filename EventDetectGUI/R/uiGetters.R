
#' getControlList
#'
#' Load a control list based on user input in the xml generated configurators.
#'
#' @param input shiny server input
#' @param strID group name in xml file
#' @param asText for log generation set 'asText' = TRUE
#'
#' @return controlList
#'
#' @keywords internal
getControlList <- function(input, strID, asText = FALSE){
    rootName <- "xml_"
    xmlRoot <- xmlGetRootElement()
    xmlFilteredForID <- xmlRoot[which(names(xmlRoot)==strID)]
    if(strID == "general"){
        selected <- "general"
    }else{
        selected <- input[[paste0(strID,"Selector")]]
    }
    rootName <- paste0(rootName,selected)
    indexSelected <- NULL
    for(i in 1:length(xmlFilteredForID)){
        element <- xmlFilteredForID[[i]]
        if(element$name == selected){
            indexSelected <- i
        }
    }
    settingsSelectedElement <- xmlFilteredForID[[indexSelected]]$variableList
    resList <- getXMLVarListUI(input,rootName,settingsSelectedElement,asText)
    resList <- c(resList, getExtraParametersList(input,strID,asText))

    return(resList)
}

#' getXMLVarListUI
#'
#' Load values of a variablelist from ui based on its xml name
#'
#' @param input shiny server input
#' @param rootName xml root name group name in xml file
#' @param varList the variable name to get
#' @param asText for log generation set 'asText' = TRUE
#'
#' @return varList
#'
#' @keywords internal
getXMLVarListUI <- function(input, rootName, varList, asText = FALSE){
    rootName <- paste0(rootName, varList$name)
    indVars <- which(names(varList) == "variable")
    indVarLists <- which(names(varList) == "variableList")

    l <- list()
    for(ind in indVarLists){
        l<- c(l,getXMLVarListUI(input, rootName, input))
    }

    for(var in indVars){
        l <- c(l,getXMLVariableUI(input,rootName,varList[[var]],asText))
    }
    l
}

#' getXMLVariableUI
#'
#' Load values of a variablelist from ui based on its xml name
#'
#' @param input shiny server input
#' @param rootName xml root name group name in xml file
#' @param var the variable name to get
#' @param asText for log generation set 'asText' = TRUE
#'
#' @return var
#'
#' @keywords internal
getXMLVariableUI <- function(input, rootName, var, asText = FALSE){
    uiName <- paste0(rootName,var$name)
    l <- list()
    if(var$type == "string" & asText){
        l[[var$name]] <- paste0("\"",input[[uiName]],"\"")
    }else if(var$type == "closure"){
        l[[var$name]] <- getClosureVariableFromUI(input,uiName,asText)
    }else if(var$type == "numeric"){
        num <- as.numeric(input[[uiName]])
        if(is.na(num)){
            if(asText){
                num <- "NULL"
            }else{
                num <- NULL
            }
        }
        l[[var$name]] <- num
    }else{
        l[[var$name]] <- input[[uiName]]
    }
    if(!(length(l) == 0)){
        names(l) <- var$name
    }
    l
}

#' getExtraParametersList
#'
#' Get value of extra field below normal setup in gui
#'
#' @param input shiny server input
#' @param strID group name in xml file
#' @param asText for log generation set 'asText' = TRUE
#'
#' @return extraValue
#'
#' @keywords internal
getExtraParametersList <- function(input, strID, asText = FALSE){
    inputName <- paste0(strID,"ExtraParameters")
    if(input[[inputName]] == ""){
        return(NULL)
    }else{
        return(getClosureVariableFromUI(input, inputName, asText))
    }
}

#' getClosureVariableFromUI
#'
#' Missing Docu
#'
#' @param input shiny server input
#' @param inputName missing docu
#' @param asText for log generation set 'asText' = TRUE
#'
#' @return closureVar
#'
#' @keywords internal
getClosureVariableFromUI <- function(input, inputName, asText = FALSE){
    if((input[[inputName]] == "NULL")){
        if(asText){
            return("NULL")
        }else{
            return(NULL)
        }
    }else if((input[[inputName]] == "NA")){
        if(asText){
            return("NA")
        }else{
            return(NA)
        }
    }else if(is.numeric(input[[inputName]])){
        return(as.numeric(input[[inputName]]))
    }else{
        if(!asText){
            tryCatch(return(get(input[[inputName]])),
                     error=return(eval(parse(text=input[[inputName]]))))
        }else{
            return(input[[inputName]])
        }
    }
}
