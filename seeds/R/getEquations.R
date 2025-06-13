getEquations <- function(model) {

  extractModel <- function(model) {
    dpTestModel <- deparse(model, width.cutoff = 500)


    # see if conditional statements are present
    condMatch <- grepl(pattern = "if", dpTestModel)

    if (sum(as.numeric(condMatch)) > 0) {
      matches <- grepl(pattern = "^[dx,dy][1-9]*", gsub(pattern = "\\s", replacement = "", dpTestModel))
      eq <- dpTestModel[matches]

      # get the conditions
      matchCond <- grepl(pattern = 'if', dpTestModel)
      condStatem <- dpTestModel[matchCond]
      closindCondInd <- which(grepl(pattern = "\\}", dpTestModel))
      condList <- list()

      for (i in 1:length(condStatem)) {
        if (grepl(pattern = "\\{", condStatem[i])) {
          begCondInd <- which(dpTestModel %in% condStatem[i])
          endCondInd <- closindCondInd - begCondInd
          endCondInd = begCondInd + min(endCondInd[endCondInd > 0])

          cond <- gsub(pattern = "\\{", "", condStatem[i])
          consq <- dpTestModel[(begCondInd + 1):(endCondInd - 1)]
          conStr <- append(cond, consq, after = 1)
          conStr <- gsub(pattern = "\\s", replacement = "", conStr)

        } else {
          condId <- which(dpTestModel %in% condStatem[i])
          consq <- dpTestModel[condId + 1]
          conStr <- append(condStatem[i], consq, after = 1)
          conStr <- gsub(pattern = "\\s", replacement = "", conStr)

        }
        condList[[i]] <- conStr
      }
    } else {
      matches <- grepl(pattern = "^[dx,dy][1-9]*", gsub(pattern = "\\s", replacement = "", dpTestModel))
      eq <- dpTestModel[matches]
      condList <- list()
    }

    res <- list()
    res$eq <- eq
    res$cond <- condList

    return(res) #return equation without a list statement if present
  }



  formatModelEq <- function(modelEq) {
    trim <- function(x) sub("^\\s+", "", x) # get rid of the delimiter
    trimedModelEq <- tolower(gsub("\\[|\\]|[,]", "", trim(modelEq)))
    trimedModelEq <- gsub("([x,y]+)\\s*([0-9]+)", "\\1\\2", trimedModelEq)

    if (grepl("d+[x,y]", x = trimedModelEq[1])) {
      trimDxy <- gsub("(d+[x,y])", "", trimedModelEq) # trim the dx/dy
    } else {
      trimDxy <- gsub("([y]+)\\s*([0-9]+)", "\\2", trimedModelEq)
    }
    if (length(trimDxy) > 1) {
      order <- as.integer(substr(x = trimDxy, start = 1, stop = 2)) #convert character of all first elements of the string vector into numbers
      newModelEq <- trimedModelEq[order]
    } else {
      newModelEq <- trimedModelEq
    }


    return(newModelEq)
  }

  trimSpace <- function(x) gsub("\\s", "", x)

  getPara <- function(model) {
    dpTestModel <- deparse(model, width.cutoff = 500)
    functionHead <- strsplit(dpTestModel[1], split = ",")[[1]]
    paraInd <- trimSpace(functionHead[grepl("para", functionHead)])
    paraInd = gsub(pattern = "[^a-zA-Z]", replacement = "", x = paraInd)

    paras <- dpTestModel[grepl(paste0(paraInd, "\\[[0-9]*"), dpTestModel)]
    paras = strsplit(x = trimSpace(paras), split = "=|<-")
    paras = unlist(lapply(paras, '[[', 1))

    return(paras)
  }

  paras <- as.character(getPara(model))
  exModel <- extractModel(model)
  modelStr <- formatModelEq(exModel$eq)

  return(list(strM = modelStr, strP = paras, cond = exModel$cond))

}



