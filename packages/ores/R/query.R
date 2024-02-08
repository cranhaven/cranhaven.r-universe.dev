ores_query <- function(path, ...){
  
  url <- paste0("https://ores.wikimedia.org/v3/", path)
  ua <- httr::user_agent("ORES R Client - https://github.com/Ironholds/ores")
  result <- httr::GET(url, ua, ...)
  httr::stop_for_status(result)
  return(httr::content(result, encoding = "UTF-8"))
}

#'@title List Supported Projects
#'@description \code{\link{list_wikis}} lists Wikimedia
#'projects that support some or all of the ORES models.
#'
#'@inheritParams list_models
#'
#'@export
list_wikis <- function(...){
  
  result <- ores_query("scores/", ...)
  return(names(result))
}

#'@title List Model Information
#'@description \code{\link{list_models}} lists information about
#'the models for a particular wiki, including what models are available,
#'how they have been trained, information about the model's accuracy and
#'ROC, and the model's version.
#'
#'@param project a Wikimedia project. Supported projects can be obtained with
#'\code{\link{list_wikis}}. If NULL (the default), model information will be
#'retrieved for all projects.
#'
#'@param ... further arguments to pass to httr's GET.
#'
#'@return a data.frame of three columns - the \code{project} of the model,
#'the \code{model} name and the model \code{version}.
#'
#'@examples
#'# Get model information for the English-language Wikipedia
#'model_data <- list_models("enwiki")
#'
#'@seealso \code{\link{list_wikis}} for retrieving the list of supported projects,
#'and \code{\link{check_damaging}} and similar for actual checking
#'against models.
#'
#'@export
list_models <- function(project = NULL, ...){
  if(is.null(project)){
    result <- ores_query(paste0("scores/"), ...)
  } else {
    result <- ores_query(paste0("scores/", project, "/", ...))
  }
  
  return(do.call("rbind", mapply(function(x, name){
    
        holding <- unlist(x)
        
        return(data.frame(
          project = name,
          model = gsub(x = names(holding), pattern = "(\\.version|models\\.)",
                       replacement = ""),
          version = unname(holding),
          stringsAsFactors = FALSE
        ))
        
      }, x = result, name = names(result),
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
    )
  )
}

#'@title Check Revert Probabilities (defunct)
#'@description \code{check_reverted} previously identified
#'if an edit was considered likely, by
#'the ORES models, to be reverted. This model has now been
#'deprecated; users should instead rely on \code{\link{check_damaging}}
#'
#'@export
check_reverted <- function(){
  .Defunct(msg = "The reverted model has been removed from the ORES API")
}

#'@title Check Good-Faith Probability
#'@description \code{check_goodfaith} identifies whether
#'or not an edit was made in 'good faith' - whether it was well-intentioned,
#'even if it is not a high-quality contribution.
#'
#'@param edits a revision ID, or vector of revision IDs, of the edits
#'to check.
#'
#'@return A data.frame of five columns; \code{edit}, the
#'edit ID, \code{project}, the project, \code{prediction},
#'whether the model predicts that the edit was made in good faith,
#'\code{false_prob}, the probability that the model's prediction
#'is wrong, and \code{true_prob}, the probability that the model's
#'prediction is correct. In the event of an error (due to the edit
#'not being available) NAs will be returned in that row.
#'
#'@examples
#'# A simple, single-diff example
#'goodfaith_data <- check_goodfaith("enwiki", 34854345)
#'
#'@seealso
#'\code{\link{check_quality}} to see a prediction of the article quality class,
#'and \code{\link{check_damaging}} to check if a set of edits
#'were damaging.
#'
#'@inheritParams list_models
#'@export
check_goodfaith <- function(project, edits, ...){
  
  data <- ores_query(
    path = paste0("scores/", project, "/?models=goodfaith&revids=", paste(edits, collapse = "|"))
  )[[project]]$scores
  
  out <- do.call("rbind", mapply(function(x, name, project){
    x <- x$goodfaith
    if("error" %in% names(x)){
      return(data.frame(edit = name,
                        project = project,
                        prediction = NA,
                        false_prob = NA,
                        true_prob = NA,
                        stringsAsFactors = FALSE))
    }
    x <- x$score
    return(data.frame(edit = name,
                      project = project,
                      prediction = x$prediction,
                      false_prob = x$probability$false,
                      true_prob = x$probability$true,
                      stringsAsFactors = FALSE))
  }, x = data, name = names(data), project = project, SIMPLIFY = FALSE,
  USE.NAMES = FALSE))
  
  return(out)
}

#'@title Check Damaging Probability
#'@description \code{check_damaging} identifies whether
#'or not an edit was damaging - the type that caused actual
#'harm to an article.
#'
#'@return A data.frame of five columns; \code{edit}, the
#'edit ID, \code{project}, the project, \code{prediction},
#'whether the model predicts that the edit was damaging,
#'\code{false_prob}, the probability that the model's prediction
#'is wrong, and \code{true_prob}, the probability that the model's
#'prediction is correct. In the event of an error (due to the edit
#'not being available) NAs will be returned in that row.
#'
#'@examples
#'# A simple, single-diff example
#'damaging_data <- check_damaging("enwiki", 34854345)
#'
#'@seealso
#'\code{\link{check_goodfaith}} to identify if a set of edits were made
#'in good faith, and \code{\link{check_quality}} to see a prediction of
#'the article quality class.
#'
#'@inheritParams check_goodfaith
#'@export
check_damaging <- function(project, edits, ...){
  
  data <- ores_query(
    path = paste0("scores/", project, "/?models=damaging&revids=", paste(edits, collapse = "|"))
  )[[project]]$scores
  
  out <- do.call("rbind", mapply(function(x, name, project){
    x <- x$damaging
    if("error" %in% names(x)){
      return(data.frame(edit = name,
                        project = project,
                        prediction = NA,
                        false_prob = NA,
                        true_prob = NA,
                        stringsAsFactors = FALSE))
    }
    x <- x$score
    return(data.frame(edit = name,
                      project = project,
                      prediction = x$prediction,
                      false_prob = x$probability$false,
                      true_prob = x$probability$true,
                      stringsAsFactors = FALSE))
  }, x = data, name = names(data), project = project, SIMPLIFY = FALSE,
  USE.NAMES = FALSE))
  
  return(out)
}

#'@title Check Article Class
#'@description \code{check_quality} identifies the quality class of the
#'article at the moment a particular edit was made.
#'
#'@return A data.frame of nine columns; \code{edit}, the
#'edit ID, \code{project}, the project, \code{prediction},
#'the class that the model predicts the article has, and then one column
#'each for the probability of the article being in each possible class.
#'In the event of an error (due to the edit
#'not being available) NAs will be returned in that row.
#'
#'@examples
#'# A simple, single-diff example
#'article_class <- check_quality("enwiki", 34854345)
#'
#'@seealso
#'\code{\link{check_goodfaith}} to identify if a set of edits were made
#'in good faith, and \code{\link{check_damaging}} to see if a set of edits
#'caused harm.
#'
#'@inheritParams check_goodfaith
#'@export
check_quality <- function(project, edits, ...){
  data <- ores_query(
    path = paste0("scores/", project, "/?models=wp10&revids=", paste(edits, collapse = "|"))
  )[[project]]$scores
  
  out <- do.call("rbind", mapply(function(x, name, project){
    x <- x$wp10
    if("error" %in% names(x)){
      return(data.frame(edit = name,
                        project = project,
                        prediction = NA,
                        stub_prob = NA,
                        start_prob = NA,
                        c_prob = NA,
                        b_prob = NA,
                        ga_prob = NA,
                        fa_prob = NA,
                        stringsAsFactors = FALSE))
    }
    x <- x$score
    return(data.frame(edit = name,
                      project = project,
                      prediction = x$prediction,
                      stub_prob = x$probability$Stub,
                      start_prob = x$probability$Start,
                      c_prob = x$probability$C,
                      b_prob = x$probability$B,
                      ga_prob = x$probability$GA,
                      fa_prob = x$probability$FA,
                      stringsAsFactors = FALSE))
    
  }, x = data, name = names(data), project = project, SIMPLIFY = FALSE,
  USE.NAMES = FALSE))
  
  return(out)
}