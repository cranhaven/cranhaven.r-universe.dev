createFunctions <- function(odeEq) {

  trim <- function(x) sub("^\\s+", "", x)
  trimSpace <- function(x) gsub("\\s", "", x)

  formatEqs <- function(modelEq) {
    return(gsub("(d[x,y]*)\\[([0-9]*)\\]=", "", modelEq))
  }

  createCostate <- function(modelODE) {
    if (.Platform$OS.type != "windows"){
      temp_costate_path <- paste0(tempdir(),'/','costate.R')
    } else {
      temp_costate_path <- paste0(tempdir(),'\\','costate.R')
    }
    file.create(temp_costate_path)
    fileCostate <- file(temp_costate_path)
    writeLines(modelODE, fileCostate)
    close(fileCostate)
  }

  createStateHiddenInput <- function(modelODE) {
    if (.Platform$OS.type != "windows"){
      temp_hidden_input_path <- paste0(tempdir(),'/','stateHiddenInput.R')
    } else {
      temp_hidden_input_path <- paste0(tempdir(),'\\','stateHiddenInput.R')
    }
    file.create(temp_hidden_input_path)
    fileState <- file(temp_hidden_input_path)
    writeLines(modelODE, fileState)
    close(fileState)
  }

  formatFuncString <- function(odeEq, funcType) {
    formatStr <- gsub(pattern = "\\[|[1-9]|\\]", replacement = "", strsplit(odeEq@origEq, "=")[[1]][1])
    if (funcType == "costate") {
      stringOde <- odeEq@costateEq

      var_name <- "var_lambda"
      #generate new function 'header'

      #get additional parameters 
      paraInput <- paste0(gsub("^\\s+|\\s+$|)", "", strsplit(odeEq@modelStr[1], split = ",")[[1]][-(1:2)]), collapse = ",")
      #check if cost function needs additional inputs
      addInputs <- getAddInputs(stringOde)

      #check if additional variables are defined in model-function
      formatStr = sub(pattern = "d", replacement = "", formatStr)

      #addInputs = trimAddInputs(odeEq,addInputs,paraInput, formatStr)

      funcStartStr <- paste0(funcType, paste0(" <-function(t,", var_name, ",parameters,input) {"))
      # funcStartStr <- append(funcStartStr,paste0("\twith (as.list(",paste(paraInput, collapse = ","),") {\n"),after = length(funcStartStr)+1)
      funcStartStr <- append(funcStartStr, paste0("\twith (as.list(parameters), {\n"), after = length(funcStartStr) + 1)

      # get additional operations, like setting parameters
      addiOperUntrimed <- odeEq@modelStr[!grepl("\\{|\\}", odeEq@modelStr)]
      addiOper <- append(addiOperUntrimed[!grepl("function|list|d[x,y]|input", addiOperUntrimed)], "\n")

      # get parameters from the measurement function
      addInputsMeasure <- odeEq@measureStr[!grepl("\\{|\\}", odeEq@measureStr)]
      addInputsMeasure = append(addInputsMeasure[!grepl("function|list|[x,y]", addInputsMeasure)], "\n")


      if (length(addiOper) != 0) {
        funcStartStr = append(funcStartStr, addiOper, after = length(funcStartStr) + 2)
      }

      if (length(addInputsMeasure) != 0) {
        funcStartStr = append(funcStartStr, addInputsMeasure, after = length(funcStartStr))
      }

      funcStartStr = append(funcStartStr, "\t\toptW <- input$optW", after = length(funcStartStr) + 1)

      if (sum(as.numeric(grepl("yhat", addInputs))) > 0) {
        dynNetInterpolate <- c("\t\tx <- sapply(input$interpX, mapply, t)",
                                   "\t\ty <- sapply(input$interpY, mapply, t)",
                                   "\t\tyhat <- sapply(input$interpyHat, mapply, t)",
                                   "\t\tq <- sapply(input$q, mapply, t)",
                                   "\t\tu <- sapply(input$u, mapply, t)\n")

        funcStartStr = append(funcStartStr, dynNetInterpolate, after = length(funcStartStr) + 2)
      }

      #generate new function wrap up
      toList <- paste0(unlist(strsplit(x = stringOde, split = "="))[seq(1, 2 * length(stringOde), by = 2)], collapse = ",")

      funcEndStr <- paste0("\n\t\t\t\tlist(c(", toList, "))\n", "\n  })\n}")
      funcEndStr <- gsub("dp([0-9]+)", paste0(var_name, "\\1"), x = funcEndStr)
      stringOde <- paste0("\t\t\t\t", stringOde)
      topMid <- append(funcStartStr, stringOde, after = length(funcStartStr) + 2)
      res <- append(topMid, funcEndStr, after = length(topMid) + 1)

      res <- gsub(pattern = "p(\\[[0-9]+\\])", replacement = paste0(var_name, "\\1"), x = res)
      res <- gsub(pattern = "\\tdp([0-9]+)", replacement = paste0(var_name, "\\1"), x = res)
    }
    else {
      stringOde <- odeEq@origEq
      
      funcStartStr <- paste(funcType, " <- function(t, x, parameters, input) { \n with (as.list(parameters),{ \n")
      funcStartStr = append(funcStartStr, "\t\toptW <- input$optW", after = length(funcStartStr) + 1)

      addiOperUntrimed <- odeEq@modelStr[!grepl("\\{|\\}", odeEq@modelStr)]
      addiOper <- append(addiOperUntrimed[!grepl("function|list|d[x,y]|input", addiOperUntrimed)], "\n")

      dynNetInterpolate <- c("\t\tw <- sapply(input$w, mapply, t)",
                                "\t\tu <- sapply(input$u, mapply, t)\n")
      funcStartStr = append(funcStartStr, dynNetInterpolate, after = length(funcStartStr) + 1)

      if (length(addiOper) != 0) {
        funcStartStr = append(funcStartStr, addiOper, after = length(funcStartStr) + 2)
      }

      #### log Transformation ####
      logTransf <- c(0, 0, 0, 0)
      if (sum(logTransf) > 0) {
        # create  vector of substitutes for variables
        variableOde <- trimSpace(formatStr)
        variableOde = gsub(pattern = "d", replacement = "", trimSpace(formatStr))
        logVarRegex <- paste0(variableOde, '\\[', which(logTransf > 0), '\\]')
        replaceVar <- paste0(variableOde, '[', which(logTransf > 0), ']')
        subOther <- paste0("exp(", replaceVar, ")")
        variableName <- paste0(variableOde, which(logTransf > 0))

        transfMatrix <- cbind(logVarRegex, subOther, variableName, replaceVar)
        print(transfMatrix)

        eq <- stringOde
        formatSameLine <- function(eq, transId, transfMatrix) {

          for (i in 1:nrow(transfMatrix)) {
            selfId <- which(grepl(pattern = paste0("d", transfMatrix[i, 3], '[^0-9]'), x = eq) > 0)
            eqStrSplit <- unlist(strsplit(eq[selfId], split = '='))
            #eqLogStr <- paste(trimSpace(eqStrSplit[1]),trimSpace(paste0('(',eqStrSplit[2],')/',transfMatrix[i,4])), sep = ' = ')
            if (grepl(pattern = transfMatrix[i, 1], eqStrSplit[2])) {
              eqLogStr <- paste(trimSpace(eqStrSplit[1]), trimSpace(paste0('(', eqStrSplit[2], ')/', transfMatrix[i, 4])), sep = ' = ')
            } else {
              eqLogStr <- eq[selfId]
            }


            eq[selfId] = eqLogStr


            #substitute expression in other lines
            eq[which(1:length(eq) != selfId)] = gsub(paste0('[^a-z]', transfMatrix[i, 1]),
                                                     replacement = transfMatrix[i, 2],
                                                     x = eq[which(1:length(eq) != selfId)])
          }
          return(eq)
        }
        stringOde <- formatSameLine(eq, logTransf, transfMatrix)
        print(stringOde)
      }


      stringOde <- paste0("\t\t", paste0(stringOde, paste0("+ optW[", 1:length(stringOde), "] *w[", 1:length(stringOde), "]")))
      funcMiddleStr = append(funcStartStr, stringOde, after = length(funcStartStr) + 2)

      toList <- paste0(unlist(strsplit(x = stringOde, split = "="))[seq(1, 2 * length(stringOde), by = 2)], collapse = ",")
      funcEndStr <- paste0("\n\t \tlist(c(", gsub("\t", "", toList), "))", "\n  })\n}")
      res <- append(funcMiddleStr, funcEndStr, after = length(funcMiddleStr) + 1)
    }
    
    return(res)
  }

  getAddInputs <- function(stringVec) {
    arrayEle <- gsub('([a-z])\\[[0-9]\\]', stringVec, replacement = "\\1") # find elements with index
    arrayEleTrim <- gsub('[\\*, \\/]', arrayEle, replacement = " ") #remove mathematical symbols
    arrayEleTrim = gsub('\\[|\\]|[+,-]|[=]|\\(|\\)', arrayEleTrim, replacement = " ") # get rid of the brackets
    arrayEleTrim = gsub(pattern = '[0-9]', x = arrayEleTrim, replacement = "")

    arrayUniqueEntries <- unique(unlist(strsplit(arrayEleTrim, split = " "))) #get unique entries
    arrayUniqueEntries <- arrayUniqueEntries[arrayUniqueEntries != ""] #remove empty entries
    arrayUniqueEntries = arrayUniqueEntries[!grepl("dp|p", arrayUniqueEntries)] # subsetting so no mathes for the costate parameter p are in

    return(arrayUniqueEntries)
  }


  wrapperCreateFunc <- function(odeEq) {
    if (identical(odeEq@costateEq, character(0)) == FALSE) {
      createCostate(formatFuncString(odeEq, "costate"))
    }
    if (odeEq@dynamicElasticNet) {
      createStateHiddenInput(formatFuncString(odeEq, "hiddenInputState"))
    }
  }


  wrapperCreateFunc(odeEq)

}
