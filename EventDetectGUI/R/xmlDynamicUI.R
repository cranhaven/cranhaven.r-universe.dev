

#' xmlGetAllConfiguredControlElements
#'
#' Parses the config.xml file. Reads all entries for preProcess, postProcess and algorithm
#' For each configured entrie, a list with the entries name and all of its configurable parameters is returned.
#'
#' @return List with all configured preProcess, postProcess and algorithms
#' as well as their respective parameters.
#'
 xmlGetRootElement <- function(){
xmlInfo <- xmlParse(system.file("config.xml", package="EventDetectGUI"))
xmlList <- xmlToList(xmlInfo)
xmlList
}

xmlGetAllConfiguredControlElements <- function(){
    xmlList <- xmlGetRootElement()
    if(is.null(xmlList)){
        return(NULL)
    }
    ctrlElements <- NULL
    namesList <- names(xmlList)
    for(i in 1:length(xmlList)){
        if(namesList[i] %in% c("preProcess","postProcess","algorithm")){
            paramsList <- list()
            subList <- xmlList[[i]]$variableList
            for(j in 1:length(subList)){
                if(length(subList[[j]])>1){
                    paramsList <- c(paramsList, subList[[j]]$name)
                }
            }
            ctrlElements[[xmlList[[i]]$name]] <- paramsList
        }
    }
    return(ctrlElements)
}

getUIListFromVariables <- function(element, rootName){
    uiList <- list()
    elementNames <- names(element)
    vars <- which(elementNames == "variable")

    for(var in vars){
        localVariable <- element[[var]]
        uiList[[length(uiList) + 1]] <- getUiElementFromXML(localVariable, rootName)
    }
    uiList
}

getUIListFromVarLists <- function(element, rootName){
    uiList <- list()
    elementNames <- names(element)
    varLists <- which(elementNames == "variableList")
    for(l in varLists){
        localVarList <- element[[l]]

        if(!grepl("xml_",rootName,fixed = TRUE)){
            inputID <- paste0("xml_",rootName,localVarList$name)
        }else{
            inputID <- paste0(rootName,localVarList$name)
        }


        uiList[[length(uiList) + 1]] <- checkboxInput(inputId = inputID, label = localVarList$name)

        localNames <- names(localVarList)
        if("variableList" %in% localNames){
            uiList[[length(uiList) + 1]] <- getUIListFromVarLists(localVarList,paste0(rootName,localVarList$name))
        }

        uiList[[length(uiList) + 1]] <- conditionalPanel(condition = paste0("input.xml_",rootName,localVarList$name, " == true"),
                                                         wellPanel(getUIListFromVariables(localVarList,paste0(rootName,localVarList$name))))
    }
    uiList
}

getSelectedElementList <- function(groupString, selectedInput, input){
    xmlList <- xmlGetRootElement()

    nameList <- names(xmlList)
    ids <- which(nameList == groupString)

    idOfSelectedElement <- NULL

    for(i in ids){
        #Loop through XML elements which have the given groupString
        elementName <- xmlList[[i]]$name
        if(elementName == selectedInput){
            idOfSelectedElement <- i
        }
    }

    return(xmlList[[idOfSelectedElement]])
}

getUiXML <- function(strID , input, selectedInput = NULL, selectedElement = NULL){
    if(is.null(selectedInput)){
        selectedInput <- input[[paste0(strID,"Selector")]]
    }
    if(is.null(selectedElement)){
        selectedElement <- getSelectedElementList(strID, selectedInput, input)
    }
    extraParameterInput <- list(textInput(paste0(strID,"ExtraParameters"),
                                          label = "Additional Parameterlist"))

    uiList <- list()
    uiList <- c(uiList, getUIListFromVarLists(selectedElement,selectedInput),
                getUIListFromVariables(selectedElement,selectedInput),
                extraParameterInput)
    uiList
}

getUiElementFromXML <- function(xmlElement,rootName){
    varType <- xmlElement$type
    switch(varType,
           "integer" = numericInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name, value = xmlElement$default,
                                    min = if(!is.null(xmlElement$min)){xmlElement$min}else{NA},
                                    max = if(!is.null(xmlElement$max)){xmlElement$max}else{NA}, step = 1),
           "numeric" = numericInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name, value = xmlElement$default,
                                    min = if(!is.null(xmlElement$min)){xmlElement$min}else{NA},
                                    max = if(!is.null(xmlElement$max)){xmlElement$max}else{NA}),
           "string" = textInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name,
                                value = if(!is.null(xmlElement$default)){xmlElement$default}else{""}),
           "closure" = textInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name,
                                value = if(!is.null(xmlElement$default)){xmlElement$default}else{""}),
           "boolean" = checkboxInput(paste0("xml_",rootName,xmlElement$name),label = xmlElement$name,
                                     value = if(!is.null(xmlElement$default)){eval(parse(text = xmlElement$default))}else{TRUE})
    )
}

getUiSelectorXML <- function(strID, input, returnUIElement = TRUE){
    xmlList <- xmlGetRootElement()

    nameList <- names(xmlList)
    ids <- which(nameList == strID)

    inputSelectorNames <- list()

    variableInputs <- list()

    for(i in ids){
        #Loop through XML elements which have the given strID
        element <- xmlList[[i]]
        elementNames <- names(element)
        inputSelectorNames[[length(inputSelectorNames) + 1]] <- element$name
    }

    if(returnUIElement){
        selectInput(paste0(strID,"Selector"), "Selection",
                    choices = inputSelectorNames)
    }else{
        inputSelectorNames
    }
}
