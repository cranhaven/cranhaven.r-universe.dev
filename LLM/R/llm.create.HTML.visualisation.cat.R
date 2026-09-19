#' Create the HTML code for Logit Leaf Model visualization
#'
#' This function generates HTML code for a visualization of the logit leaf model based on the variable importance per variable category.
#'
#' @param object An object of class logitleafmodel, as that created by the function llm.
#' @param category_var_df dataframe containing a column called "iv" with the independent variables and a column called "cat" with the variable category names that is associated with every iv
#' @param roundingnumbers An integer stating the number of decimals in the visualization.
#' @param headertext Allows to provide the table with a header.
#' @param footertext Allows to provide the table with a custom footer.
#' @param methodvarimp Allows to determine the method to calculate the variable importance. There are 4 options: 1/ Variable coefficent (method = 'Coef) 2/ Standardized beta ('Beta') 3/ Wald statistic ('Wald') 4/ Likelihood Rate Test ('LRT')
#' @return Generates HTML code for a visualization.
#' @export
#' @import stringr partykit stats reghelper scales survey
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
#' PimaTrain <- PimaIndiansDiabetes[idtrain,]
#' Pimatest <- PimaIndiansDiabetes[-idtrain,]
#' ## Create the LLM
#' Pima.llm <- llm(X = PimaTrain[,-c(9)],Y = PimaTrain$diabetes,
#'  threshold_pruning = 0.25,nbr_obs_leaf = 100)
#' ## Define the variable categories (note: the categories are only created for demonstration)
#' var_cat_df <- as.data.frame(cbind(names(PimaTrain[,-c(9)]),
#' c("cat_a","cat_a","cat_a","cat_a","cat_b","cat_b","cat_b","cat_b")), stringsAsFactors = FALSE)
#' names(var_cat_df) <- c("iv", "cat")
#' ## Save the output of the model to a html file
#' Pima.Viz <- table.cat.llm.html(object = Pima.llm,category_var_df= var_cat_df,
#'  headertext = "This is an example of the LLM model",
#' footertext = "Enjoy the package!")
#' ## Optionaly write it to your working directory
#' # write(Pima.Viz, "Visualization_LLM_on_PimaIndiansDiabetes.html")
#' @export table.cat.llm.html
#'

table.cat.llm.html <- function(object,category_var_df, headertext= "The Logit Leaf Model", footertext= "A table footer comment", roundingnumbers = 2, methodvarimp = 'Coef'){

  #===============================================================================
  # Self-defined function
  #===============================================================================

  myRescaleDF <- function(x) {
    return((x - min(x)) / (max(x) - min(x)) * 100)
  }

  #-------------------------------------------------------------------------------
  # Fix the regTermTest() function from survey package
  #-------------------------------------------------------------------------------

  regTermTest_fixedLRT <- function (model, test.terms, null = NULL, df = NULL,
                                    method = c("Wald", "WorkingWald", "LRT"),
                                    lrt.approximation = "saddlepoint")
  {
    "
  This function originated from reghelper package. However, the regTermTest()
  function has a bug when the train data of glm() function is not available.
  Therefore, this function is modified to (1) re-created the train data of
  glm() model and (2) fix the train data name in the model$call.
  "

    method <- match.arg(method)
    canonicalOrder <- function(term) {
      tt <- strsplit(term, ":")
      tt <- lapply(tt, sort)
      sapply(tt, paste, collapse = ":")
    }
    if (inherits(test.terms, "formula"))
      test.terms <- attr(terms(test.terms), "term.labels")
    okbeta <- !is.na(coef(model, na.rm = FALSE))
    tt <- attr(terms(model), "term.labels")
    aa <- attr(model.matrix(model), "assign")[okbeta]
    if ((inherits(model, "coxph") || inherits(model, "svyloglin") ||
         inherits(model, "svyolr")) && attr(terms(model), "intercept"))
      aa <- aa[-1]
    index <- which(aa %in% match(canonicalOrder(test.terms),
                                 canonicalOrder(tt)))
    if (any(is.na(index)))
      stop("Terms didn't match:", canonicalOrder(test.terms),
           canonicalOrder(tt))
    beta <- coef(model)[index]
    if (!is.null(null))
      beta <- beta - null
    V <- stats::vcov(model)[index, index]
    if (is.null(df)) {
      if (inherits(model, "svyglm"))
        df <- model$df.residual
      else if (inherits(model, "svycoxph"))
        df <- model$degf.resid
      else if (inherits(model, "lm"))
        df <- model$df.residual
      else if (inherits(model, "coxph"))
        df <- model$n - length(coef(model))
      else if (inherits(model, "MIresult"))
        df <- min(model$df[index])
      else if (inherits(model, "svyloglin"))
        df <- model$df + 1 - length(index)
      else if (inherits(model, "svyolr"))
        df <- model$df.residual
      else df <- length(resid(model)) - length(coef(model))
    }
    if (method %in% c("LRT", "WorkingWald")) {
      if (inherits(model, "svyglm"))
        V0 <- model$naive.cov
      else if (inherits(model, "svycoxph"))
        V0 <- model$inv.info
      else if (inherits(model, "lm"))
        V0 <- vcov(model)
      else if (inherits(model, "coxph")) {
        if (is.null(model$naive.var))
          V0 <- model$var
        else V0 <- model$naive.var
      }
      else if (inherits(model, "svyolr")) {
        V0 <- solve(model$Hess)
      }
      else stop("method='LRT' not supported for this model")
      V0 <- V0[index, index]
      test.formula <- survey::make.formula(test.terms)[[2]]
      if (!("formula") %in% names(model$call))
        names(model$call)[[2]] <- "formula"
      if (method == "LRT") {

        # Fix the bug when the basetable is not exist
        train_data <- model$data  # Re-create the train data
        train_data[, as.character(model$formula[2])] <- model$y
        model$call <- stats::update(model, data=train_data, evaluate=FALSE)

        model0 <- eval(bquote(stats::update(model, . ~ . - (.(test.formula)))))
        chisq <- stats::deviance(model0) - stats::deviance(model)
      }
      else {
        chisq <- beta %*% solve(V0) %*% beta
      }
      misspec <- eigen(solve(V0) %*% V, only.values = TRUE)$values
      if (df == Inf)
        p <- survey::pchisqsum(chisq, rep(1, length(misspec)), misspec,
                       method = lrt.approximation, lower.tail = FALSE)
      else p <- survey::pFsum(chisq, rep(1, length(misspec)), misspec,
                              ddf = df, method = lrt.approximation, lower.tail = FALSE)
      rval <- list(call = sys.call(), mcall = model$call,
                   chisq = chisq, df = length(index), test.terms = test.terms,
                   p = p, lambda = misspec, ddf = df)
      if (method == "LRT")
        class(rval) <- "regTermTestLRT"
      else class(rval) <- "regTermTestWW"
      return(rval)
    }
    chisq <- beta %*% solve(V) %*% beta
    if (df < Inf) {
      Ftest <- chisq/length(index)
      rval <- list(call = sys.call(), mcall = model$call,
                   Ftest = Ftest, df = length(index), ddf = df, test.terms = test.terms,
                   p = stats::pf(Ftest, length(index), df, lower.tail = FALSE))
    }
    else {
      rval <- list(call = sys.call(), mcall = model$call,
                   chisq = chisq, df = length(index), test.terms = test.terms,
                   p = stats::pchisq(chisq, length(index), lower.tail = FALSE))
    }
    class(rval) <- "regTermTest"
    rval
  }

  #-------------------------------------------------------------------------------
  # Variable importance for Logit Leaf Model (LLM) in general
  #-------------------------------------------------------------------------------

  varImpLR <- function(model_glm, method='Coef', isAbs=FALSE) {
    "
  This function extracts the variable importance from the Logistic Regression
  model trained by glm() function. The options for variable importance are as
  follows:

  1) The LR coefficient (method = 'Coef', with/without absolute value)
  2) The LR standardized coefficient (method = 'Beta', with/without absolute value)
  3) The Wald statistics test (method='Wald')
  3) The Likehood ratio test (method='LRT')
  "

    # Get the IV name list
    IV_list <- names(model_glm$coefficients)
    IV_list <- setdiff(IV_list, c("(Intercept)"))

    # Get the variable importance values
    varImp_list <- c()

    if (method == 'Coef') {  # Use variable coefficient as feature importance
      varImp_temp <- model_glm$coefficients
      varImp_list <- varImp_temp[IV_list]
    }

    if (method == 'Beta') {  # Use standardized beta as feature importance
      varImp_temp <- reghelper::beta(model_glm)$coefficients[, 'Estimate']
      names(varImp_temp) <- gsub('.{2}$', '', names(varImp_temp))  # Fix the .z in beta names
      varImp_list <- varImp_temp[IV_list]
    }

    if (method == 'Wald') {  # Use Wald statistics as variable importance
      for (v in IV_list) {
        W <- survey::regTermTest(model_glm, v, method='Wald')
        varImp_list <- c(varImp_list, W$Ftest)
      }
      names(varImp_list) <- IV_list
    }

    if (method == 'LRT') {  # Use Likelihood Ratio as variable importance
      for (v in IV_list) {
        W <- regTermTest_fixedLRT(model_glm, v, method='LRT')
        varImp_list <- c(varImp_list, W$chisq)
      }
      names(varImp_list) <- IV_list
    }

    # Take the absolute value if requested
    if (isAbs) {
      varImp_list <- abs(varImp_list)
    }

    return(varImp_list)
  }

  varImpLLM <- function(model_llm, IV_list, method='Coef', isAbs=FALSE, scale=TRUE) {
    "
  This function extract the variable importance from a single Logit Leaf Model
  model. The LLM model contains multiple single Logistic Regression model in
  different segment.

  The variable importance of each single LR model can be calcuate by 4 methods:
  - Variable coefficent (method = 'Coef', with/without absolute value)
  - Standardized beta (method = 'Beta', with/without absolute value)
  - Wald statistics (method = 'Wald')
  - Likelihood Rate Test (method = 'LRT')

  This function calculate the weighted average of variable importance of all
  single LR model in different segment of LLM model. Finally, the weighted
  variable importance can be scalled between [0, 100] by the scale parameter.
  "

    # Get feature importance of all LR models in each segment
    lr_model_list <- model_llm$Coefficients
    num_obs <- list()  # List to store number of obs. in each segment
    lr_fi <- list()  # List to store the feature importance in each segment

    for (i in 1:length(lr_model_list)) {

      lr_model <- lr_model_list[[i]]
      num_obs[[i]] <- nobs(lr_model)

      # Prepare the full list of variable
      lr_fi_temp <- rep(0, length(IV_list))  # Create a zero-vector to store varImp
      names(lr_fi_temp) <- IV_list  # Name the vector

      # Get the variable importance
      varImp_temp <- varImpLR(lr_model, method, isAbs)
      lr_fi_temp[names(varImp_temp)] <- varImp_temp
      lr_fi_temp[is.na(lr_fi_temp)] <- 0

      # Store the variable importance to a list
      lr_fi[[i]] <- lr_fi_temp

    }

    # Calculate the average feature importance among different segments
    lr_fi_df <- data.frame(lr_fi)
    total_obs <- 0  # Total obs.

    for (i in 1:length(num_obs)) {
      # Multiply the feature importance value with the number of obs. in each segment
      lr_fi_df[, i] <- lr_fi_df[, i] * num_obs[[i]]
      total_obs <- total_obs + num_obs[[i]]
    }

    # Divide the feature importance value to the total number of obs.
    lr_fi_result <- apply(data.frame(lr_fi_df), 1, sum) / total_obs

    # Scale to [0, 100]
    if (scale == TRUE) {
      lr_fi_result <- scales::rescale(lr_fi_result, to=c(0, 100))
    }

    return(lr_fi_result)
  }

  #-------------------------------------------------------------------------------
  # Variable importance of Logit Leaf Model (LLM) in each segment
  #-------------------------------------------------------------------------------

  varImpLLM_segs <- function(model_llm, IV_list, method='Coef', isAbs=FALSE, scale=TRUE) {
    "
  This function extract the variable importance from a single Logit Leaf Model
  model. The variable importance (of each single LR model) will be reported
  separately for each different segment.

  The variable importance of the LR model can be calcuate by 4 methods:
  - Variable coefficent (method = 'Coef', with/without absolute value)
  - Standardized beta (method = 'Beta', with/without absolute value)
  - Wald statistics (method = 'Wald')
  - Likelihood Rate Test (method = 'LRT')

  The variable importance will be multiply to the size of each segment. Finally,
  they can be scalled between [0, 100] (with all importance values in all segments)
  by setting the scale parameter.
  "

    # Get feature importance of all LR models in each segment
    lr_model_list <- model_llm$Coefficients
    num_obs <- list()  # List to store number of obs. in each segment
    lr_fi <- list()  # List to store the feature importance in each segment

    for (i in 1:length(lr_model_list)) {

      lr_model <- lr_model_list[[i]]
      num_obs[[i]] <- nobs(lr_model)

      # Get the variable importance
      varImpLR_temp <- varImpLR(lr_model, method, isAbs)  # Get the varImpLR values
      lr_fi_temp <- rep(0, length(IV_list))  # Create a zero-vector to store varImpLR
      names(lr_fi_temp) <- IV_list  # Name the vector
      lr_fi_temp[names(varImpLR_temp)] <- varImpLR_temp  # Assign the the result vector
      lr_fi_temp[is.na(lr_fi_temp)] <- 0

      # Multiply the varImpLR values to the size of the segment
      lr_fi_temp <- lr_fi_temp * num_obs[[i]]
      lr_fi[[i]] <- lr_fi_temp

    }

    # Prepare the output
    lr_fi_df <- data.frame(lr_fi)  # Put all varImpLR in 1 data.frame
    col_names <- paste(paste0('seg', c(1:length(num_obs))), unlist(num_obs), sep='_')
    names(lr_fi_df) <- col_names

    # Scale to [0, 100]
    if (scale == TRUE) {
      lr_fi_df <- myRescaleDF(lr_fi_df)
    }

    return(list(llm_fi_segs=lr_fi_df,
                segs_size=num_obs))
  }


  #===============================================================================



  # Calculate the variable category importance
  # Add the parameters to be passed here
  outp <- varImpLLM_segs(model_llm = object, IV_list = category_var_df[,1],method = methodvarimp)

  # Calculate variable importance per category
  categoryimplist <- list()

  for (i in 1:ncol(outp$llm_fi_segs)) {
    # Calculate the variable importance percentage
    percentage <- outp$llm_fi_segs[,i]/sum(outp$llm_fi_segs[,i])
    # Group the importances by category
    catimp <- as.data.frame(cbind(category_var_df[,"cat"], percentage), stringsAsFactors = F)
    names(catimp) <- c("cat", "percentage")
    catimp$percentage <- as.numeric(catimp$percentage)
    catimp <- aggregate(list("Percentage"  = catimp[,"percentage"]), by=list("Category" = catimp$cat), sum)
    # Save the results in the list
    categoryimplist[[i]] <- catimp
  }


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

  # Calculate number of categories
  nbrcategories <- length(unique(category_var_df[,2]))

  # Calculate number of columns needed
  nbrcols <- decisionrules + nbrcategories + 3

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
  categorynames <- ""
  for (i in 1:length(categoryimplist[[1]]$Category)) {
    addnewrule <- paste0("<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>",categoryimplist[[1]]$Category[i],"</th>")
    categorynames <- paste0(categorynames, addnewrule)
  }




  header <- gsub(pattern = "\n",replacement = "", x = paste0("<thead>
                                                             <tr>
                                                             <td colspan='",nbrcols,"' style='text-align: left;'>",
                                                             headertext,"</td></tr>
                                                             <tr>
                                                             <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; border-right: 2px solid grey; text-align: center;'colspan=",(decisionrules+3),"> Segment Definition </th>
                                                             <th style='border-bottom: 2px solid grey; border-top: 2px solid grey; text-align: center;'colspan=",(nbrcategories),"> Variable Category Importance </th>
                                                             </tr>
                                                             <tr>
                                                             <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; border-right: 1px solid grey; text-align: center;'>Segment</th>",
                                                             currentrule,
                                                             "<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; border-left: 1px solid grey; text-align: center;'> # obs.</th>,
                                                             <th style='border-bottom: 1px solid grey; border-top: 2px solid grey; border-left: 1px solid grey; border-right: 2px solid grey; text-align: center;'> Incidence %</th>",
                                                             categorynames,
                                                             "</tr>
                                                             </thead>"))


  # Body:
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

    numberobssegment <- paste0("<td style='border-right: 1px solid grey;border-left: 1px solid grey;border-bottom: 1px solid grey;text-align: center;'rowspan=2>",object$`Observations per segment`[[sss]][1],"</td>")
    incidencesegment <- paste0("<td style='border-right: 1px solid grey;border-bottom: 1px solid grey;text-align: center;'rowspan=2>",paste0(round(object$`Incidence of dependent per segment`[[sss]][1]*100, 2),"%"),"</td>")


    categoriessegment <- ""
    for (i in 1:nbrcategories) {
      # TODO_ add ifelse if there are variables not present

      if(i ==1){
        categoriessegment <- paste0("<td style='border-bottom: 1px solid grey;border-left: 2px solid grey;text-align: center;'rowspan=2>",'<table id="tblgraph" style="background-color:#C0C0C0;" align="center" width="100" cellpadding="0" cellspacing="0" border="1">
                                      <tbody><tr style="width:100%"><td align="right"><td align="left" width="100" height="3" colspan="1">
                                      <div style="background-color:blue; width:',round(categoryimplist[[sss]]$Percentage[i]*100,2), ';"><p style="position:relative; left:0; color:#FFFFFF">
                                      <font face="arial" size="-2">&nbsp;',paste0(round(categoryimplist[[sss]]$Percentage[i]*100,2),"%"),'</font></p>
                                      </div>
                                      </td></tr></tbody></table>',"</td>")
      }
      if (i>1) {
        addnewrule <- paste0("<td style='border-bottom: 1px solid grey;text-align: center;'rowspan=2>",
                             '<table id="tblgraph" style="background-color:#C0C0C0;" align="center" width="100" cellpadding="0" cellspacing="0" border="1">
                                      <tbody><tr style="width:100%"><td align="right"><td align="left" width="100" height="3" colspan="1">
                                      <div style="background-color:blue; width:',round(categoryimplist[[sss]]$Percentage[i]*100,2), ';"><p style="position:relative; left:0; color:#FFFFFF">
                                      <font face="arial" size="-2">&nbsp;',paste0(round(categoryimplist[[sss]]$Percentage[i]*100,2),"%"),'</font></p>
                                      </div>
                                      </td></tr></tbody></table>', "</td>")
        categoriessegment <- paste0(categoriessegment, addnewrule)
      }
    }

    if(sss == 1){
      body <- gsub(pattern = "\n",replacement = "", x = paste0("<tbody><tr><td style='border-right: 1px solid grey;border-bottom: 1px solid grey;text-align: center;'rowspan=2>1</td>", rulesforsegment,numberobssegment,incidencesegment,categoriessegment,"</tr>"))
    }
    if(sss > 1){
      body <- gsub(pattern = "\n",replacement = "", x = paste0(body,"<tr></tr><tr><td style='border-right: 1px solid grey;border-bottom: 1px solid grey;text-align: center;'rowspan=2>",sss,"</td>", rulesforsegment,numberobssegment,incidencesegment,categoriessegment,"</tr>"))
    }
  }
  body <- gsub(pattern = "\n",replacement = "", x = paste0(body,"</tr></tbody>"))

  # Footer
  footer <- paste0("<tfoot><tr><td colspan='5'>",footertext,"</td></tr></tfoot></table>")


  myresult <- paste0(table, header,body, footer)
  return(myresult)
}

