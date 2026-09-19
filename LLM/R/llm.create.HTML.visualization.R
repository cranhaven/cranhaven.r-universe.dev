#' Create the HTML code for Logit Leaf Model visualization
#'
#' This function generates HTML code for a visualization of the logit leaf model.
#'
#' @param object An object of class logitleafmodel, as that created by the function llm.
#' @param roundingnumbers An integer stating the number of decimals in the visualization.
#' @param headertext Allows to provide the table with a header.
#' @param footertext Allows to provide the table with a custom footer.
#' @return Generates HTML code for a visualization.
#' @export
#' @import stringr
#' @references Arno De Caigny, Kristof Coussement, Koen W. De Bock, A New Hybrid Classification Algorithm for Customer Churn Prediction Based on Logistic Regression and Decision Trees, European Journal of Operational Research (2018), doi: 10.1016/j.ejor.2018.02.009.
#' @author Arno De Caigny, \email{a.de-caigny@@ieseg.fr}, Kristof Coussement, \email{k.coussement@@ieseg.fr} and Koen W. De Bock, \email{kdebock@@audencia.com}
#' @seealso \code{\link{predict.llm}}, \code{\link{llm}}, \code{\link{llm.cv}}
#' @examples
#' ## Load PimaIndiansDiabetes dataset from mlbench package
#' if (requireNamespace("mlbench", quietly = TRUE)) {
#'   library("mlbench")
#' }
#' data("PimaIndiansDiabetes")
#' ## Split in training and test (2/3 - 1/3)
#' idtrain <- c(sample(1:768,512))
#' PimaTrain <-PimaIndiansDiabetes[idtrain,]
#' Pimatest <-PimaIndiansDiabetes[-idtrain,]
#' ## Create the LLM
#' Pima.llm <- llm(X = PimaTrain[,-c(9)],Y = PimaTrain$diabetes,
#'  threshold_pruning = 0.25,nbr_obs_leaf = 100)
#' ## Save the output of the model to a html file
#' Pima.Viz <- table.llm.html(object = Pima.llm, headertext = "This is an example of the LLM model",
#' footertext = "Enjoy the package!")
#' ## Optionaly write it to your working directory
#' # write(Pima.Viz, "Visualization_LLM_on_PimaIndiansDiabetes.html")
#' @export table.llm.html
#'

table.llm.html <- function(object, headertext= "The Logit Leaf Model", footertext= "A table footer comment", roundingnumbers = 2){
  # Calculate max number of decision rules
  decisionrules <- 0
  for (i in 1:length(object[[1]])) {
    decisionrules <- max(decisionrules, (stringr::str_count(object[[1]][i][[1]], "&")+1))
  }

  # Calculate number of segments
  nbrsegments <- length(object[[1]])

  # Create overview table
  decisionrulesoverview <- as.data.frame(1:(nbrsegments*decisionrules))

  iii <- 1
  for (i in 1:length(object[[1]])) {
    # Loop over segment specific rules
    allsegrules <-object[[1]][i][[1]]
    # Create decision rule overview table
    for (ii in 1:decisionrules) {
      if(is.na(strsplit(strsplit(allsegrules, split = "&")[[1]][ii], "'")[[1]])){
        var <- "."
        sign <- "."
        number <- "."
      }else{
        var <- strsplit(stringr::str_trim(strsplit(allsegrules, split = "&")[[1]][ii]), " ")[[1]][[1]]
        sign <- strsplit(stringr::str_trim(strsplit(allsegrules, split = "&")[[1]][ii]), " ")[[1]][[2]]
        # Find the value
        consideredstring <- strsplit(stringr::str_trim(strsplit(allsegrules, split = "&")[[1]][ii]), " ")[[1]][[3]]
        number <- round(as.numeric(consideredstring), roundingnumbers)
      }

      # Fill in the values
      decisionrulesoverview[iii,1] <- i
      decisionrulesoverview[iii,2] <- ii
      decisionrulesoverview[iii,3] <- var
      decisionrulesoverview[iii,4] <- sign
      decisionrulesoverview[iii,5] <- number

      iii <- iii+1
    }
  }
  names(decisionrulesoverview) <- c("segment","rule", "variable", "sign","value" )

  # Calculate shared variables

  for (i in 1:length(object[[2]])) {
    m <- object[[2]][i][[1]]
    if(i==1){
      allnames <- c(names(m$coefficients))
    }else{
      allnames <- c(allnames,names(m$coefficients))
    }

  }

  sharvars <- names(which(table(allnames)>1))

  sharedvariables <- length(sharvars)

  # Create overview table shared variables
  sharedvariablesoverview <- as.data.frame(1:(nbrsegments*sharedvariables))
  kkk <- 1
  for (i in 1:nbrsegments) {
    for (ii in 1:sharedvariables) {
      consideredobj <- object[[2]][i][[1]]
      considervars <- names(consideredobj$coefficients)

      if(ii%in%which(sharvars%in%considervars)){
        sharedvariablesoverview[kkk,1] <- i
        sharedvariablesoverview[kkk,2] <- ii
        sharedvariablesoverview[kkk,3] <- sharvars[ii]
        sharedvariablesoverview[kkk,4] <- round(consideredobj$coefficients[which(considervars == sharvars[ii])][[1]],roundingnumbers)
      }else{
        sharedvariablesoverview[kkk,1] <- i
        sharedvariablesoverview[kkk,2] <- ii
        sharedvariablesoverview[kkk,3] <- "."
        sharedvariablesoverview[kkk,4] <- "."
      }
      kkk <- kkk+1
    }
  }
  names(sharedvariablesoverview) <- c("segment", "nbrsharedvar", "varname", "value")


  # Calculate max unique variables
  univars <- names(which(table(allnames)==1))
  uniquevariables <- 0
  for (i in 1:length(object[[2]])) {
    m <- object[[2]][i][[1]]
    uniquevariables <- max(uniquevariables,length(which(names(m$coefficients)%in%univars)))
  }

  # Create overview table unique variables
  uniquevariablesoverview <- as.data.frame(1:(nbrsegments*uniquevariables))
  kkk <- 1
  for (i in 1:nbrsegments) {
    for (ii in 1:uniquevariables) {
      consideredobj <- object[[2]][i][[1]]
      considervars <- names(consideredobj$coefficients)
      consideredunivars <- which(univars%in%considervars)

      if(ii%in%0:length(consideredunivars)){
        uniquevariablesoverview[kkk,1] <- i
        uniquevariablesoverview[kkk,2] <- ii
        uniquevariablesoverview[kkk,3] <- univars[consideredunivars[ii]]
        uniquevariablesoverview[kkk,4] <- round(consideredobj$coefficients[which(considervars == univars[consideredunivars[ii]])][[1]],roundingnumbers)
      }else{
        uniquevariablesoverview[kkk,1] <- i
        uniquevariablesoverview[kkk,2] <- ii
        uniquevariablesoverview[kkk,3] <- "."
        uniquevariablesoverview[kkk,4] <- "."
      }
      kkk <- kkk+1
    }
  }
  names(uniquevariablesoverview) <- c("segment", "nbruniquevar", "varname", "value")




  # Calculate number of columns needed
  nbrcols <- decisionrules + sharedvariables + uniquevariables + 1

  # # Calculate number of rows needed
  # nbrrows <- 4

  # Define table
  table <- "<table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >"

  # Create header
  currentrule <- ""
  for (i in 1:decisionrules) {
    addnewrule <- paste0("<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Rule",i,"</th>")
    currentrule <- paste0(currentrule, addnewrule)
  }

  header <- gsub(pattern = "\n",replacement = "", x = paste0("<thead>
                                                             <tr><td colspan='",nbrcols,"' style='text-align: left;'>",
                                                             headertext,"</td></tr>
                                                             <tr>
                                                             <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; border-right: 2px solid grey; text-align: center;'colspan=",(decisionrules+1),"> Decision Rules </th>
                                                             <th style='border-bottom: 2px solid grey; border-top: 2px solid grey; text-align: center;'colspan=",(sharedvariables + uniquevariables),"> Logistic Regression </th>
                                                             </tr>
                                                             <tr>
                                                             <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; border-right: 1px solid grey; text-align: center;'>Segment</th>",
                                                             currentrule,
                                                             "<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; border-left: 2px solid grey; text-align: center;'colspan=",sharedvariables,"> Shared Variables</th>
                                                             <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; border-left: 1px solid grey; text-align: center;'colspan=",uniquevariables,"> Unique Variables</th>
                                                             </tr>
                                                             </thead>"))


  # Body:
  # Number of segmentsis number of blocks that need to be constructed
  # First colum is always number of segment
  # Create number of colums dependent on rules
  # Create number of colums dependent on shared variables
  # Create number of colums

  # decisionrulesoverview[which(decisionrulesoverview$segment==sss & decisionrulesoverview$rule == i), "variable"]
  for(sss in 1:nbrsegments){

    rulesforsegment <- ""
    for (i in 1:decisionrules) {
      addnewrule <- paste0("<td style='border-bottom: 1px solid grey;text-align: center;'rowspan=2>",
                           decisionrulesoverview[which(decisionrulesoverview$segment==sss & decisionrulesoverview$rule == i),"variable"]," <br>",
                           decisionrulesoverview[which(decisionrulesoverview$segment==sss & decisionrulesoverview$rule == i),"sign"]," ",
                           decisionrulesoverview[which(decisionrulesoverview$segment==sss & decisionrulesoverview$rule == i),"value"],
                           " </br> </td>")
      rulesforsegment <- paste0(rulesforsegment, addnewrule)
    }
    # <td style='border-bottom: 1px solid grey;text-align: center;'rowspan=2> VarName <br> <= 0.50 </br> </td>

    sharedvariablesforsegment <- ""
    for (i in 1:sharedvariables) {
      # TODO_ add ifelse if there are variables not present

      if(i ==1){
        sharedvariablesforsegment <- paste0("<td style='border-bottom: 1px solid grey;border-left: 2px solid grey;text-align: center;'rowspan=2>",
                                            # VariableName
                                            sharedvariablesoverview[which(sharedvariablesoverview$segment==sss & sharedvariablesoverview$nbrsharedvar == i),"varname"],
                                            " <br>",
                                            # Value,
                                            sharedvariablesoverview[which(sharedvariablesoverview$segment==sss & sharedvariablesoverview$nbrsharedvar == i),"value"],
                                            " </br> </td>")
      }
      if (i>1) {
        addnewrule <- paste0("<td style='border-bottom: 1px solid grey;text-align: center;'rowspan=2>",
                             # Variable
                             sharedvariablesoverview[which(sharedvariablesoverview$segment==sss & sharedvariablesoverview$nbrsharedvar == i),"varname"],
                             " <br>",
                             # Value
                             sharedvariablesoverview[which(sharedvariablesoverview$segment==sss & sharedvariablesoverview$nbrsharedvar == i),"value"],
                             " </br> </td>")
        sharedvariablesforsegment <- paste0(sharedvariablesforsegment, addnewrule)
      }
    }

    uniquevariablesforsegment<- ""
    for (i in 1:uniquevariables) {
      if(i ==1){
        uniquevariablesforsegment <- paste0("<td style='border-bottom: 1px solid grey;border-left: 1px solid grey;text-align: center;'rowspan=2>",
                                            # VariableName
                                            uniquevariablesoverview[which(uniquevariablesoverview$segment==sss & uniquevariablesoverview$nbruniquevar == i),"varname"],
                                            " <br>",
                                            # Value
                                            uniquevariablesoverview[which(uniquevariablesoverview$segment==sss & uniquevariablesoverview$nbruniquevar == i),"value"],
                                            " </br> </td>")
      }
      if (i>1) {
        addnewrule <- paste0("<td style='border-bottom: 1px solid grey;text-align: center;'rowspan=2>",
                             # VariableName
                             uniquevariablesoverview[which(uniquevariablesoverview$segment==sss & uniquevariablesoverview$nbruniquevar == i),"varname"],
                             " <br>",
                             # Value
                             uniquevariablesoverview[which(uniquevariablesoverview$segment==sss & uniquevariablesoverview$nbruniquevar == i),"value"],
                             " </br> </td>")
        uniquevariablesforsegment <- paste0(uniquevariablesforsegment, addnewrule)
      }
    }

    if(sss == 1){
      body <- gsub(pattern = "\n",replacement = "", x = paste0("<tbody><tr><td style='border-right: 1px solid grey;border-bottom: 1px solid grey;text-align: center;'rowspan=2>1</td>", rulesforsegment,sharedvariablesforsegment,uniquevariablesforsegment,"</tr>"))
    }
    if(sss > 1){
      body <- gsub(pattern = "\n",replacement = "", x = paste0(body,"<tr></tr><tr><td style='border-right: 1px solid grey;border-bottom: 1px solid grey;text-align: center;'rowspan=2>",sss,"</td>", rulesforsegment,sharedvariablesforsegment,uniquevariablesforsegment,"</tr>"))
    }
  }
  body <- gsub(pattern = "\n",replacement = "", x = paste0(body,"</tr></tbody>"))

  # Footer
  footer <- paste0("<tfoot><tr><td colspan='5'>",footertext,"</td></tr></tfoot></table>")


  myresult <- paste0(table, header,body, footer)
  return(myresult)
}
