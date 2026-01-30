#' Trim an R Object
#' 
#' Trims an R object whilst presuming the results of a given function call,
#' where the R object is given as an argument. One popular example could be 
#' trimming an R model object whilst presuming the results of the 
#' \code{\link{predict}} function on a sample of data.
#' 
#' @param obj \code{list} R object to be trimmed. _MUST_ inherit from the
#' 'list' class.
#' @param obj_arg_name \code{character} what is the name of the parameter, that
#' 'obj' must be set to, when invoking 'fun'. Defaults to NULL, in which case
#' the function assumes, that the 'obj' matches the first parameter of 'fun'.
#' @param fun \code{function} function that must return the same results, when
#' invoked with 'obj' both before and after trimming.
#' @param size_target \code{numeric} desired maximum size in _MegaBytes_ of 
#' object after trimming has been conducted. When this size is achieved, 
#' the trimming stops. Defaults to 0, in which case trimming continues, until
#' no further trimming can be done without breaking results from 'fun'.
#' @param tolerate_warnings \code{logical} tolerate warnings (=TRUE) Or
#' not (=FALSE) from function call results?
#' @param verbose \code{logical} print messages?
#' @param ... other (named) arguments for 'fun'.
#' @param dont_touch \code{list} list with name indices of elements, that must 
#' not be removed from object by trimming procedure.
#'
#' @import data.table
#' @importFrom pryr object_size
#' @importFrom crayon red blue green yellow
#' @importFrom cli cat_bullet symbol
#' @importFrom stats predict
#' 
#' @export
#' 
#' @examples
#' # get training data for predictive model.
#' trn <- datasets::mtcars
#' 
#' # estimate model.
#' mdl <- lm(mpg ~ ., data = trn)
#' trim(obj = mdl, obj_arg_name = "object", fun = predict, newdata = trn)
#' trim(obj = mdl, obj_arg_name = "object", fun = predict, newdata = trn,
#' dont_touch = list(c("model"), c("qr","tol")))
trim <- function(obj,
                 obj_arg_name = NULL,
                 fun = predict,
                 size_target = 0,
                 tolerate_warnings = FALSE,
                 verbose = TRUE,
                 dont_touch = list(),
                 ...) {

  # convert from MB to B.
  size_target <- size_target * 1e06
  
  # compute size of object.
  size_init <- size <- as.numeric(object_size(obj))
  
  if (verbose) {cat_bullet("Initial object size: ", yellow(pf_obj_size(size_init)),
                           bullet_col = "yellow")}
  if (size_target > 0 && verbose) {cat_bullet("Target object size: <= ", 
                                           blue(pf_obj_size(size_target)), 
                                           sep = "", bullet_col = "blue")}
  
  # check if size is already below target.
  if (size <= size_target) {
    cat("Object size is already below target threshold. Nothing done.")
    return(obj)
  }
  
  # check inputs for function.
  check_inputs(obj = obj,
               obj_arg_name = obj_arg_name,
               fun = fun,
               dont_touch = dont_touch)
  
  # get results for initial object.
  results_init <- get_results_for_object(obj = obj,
                                         obj_arg_name = obj_arg_name,
                                         fun = fun,
                                         tolerate_warnings = tolerate_warnings,
                                         ...) 
  
  # check initial results.
  check_initial_results(results_init, tolerate_warnings)
  
  # prepare data.table with candidates for elimination. Initially one for each 
  # entry in root of list. 'i1' stands for 'index 1' - indices of all list 
  # entries at root layer (layer 1).
  cand <- data.table(i1 = seq_len(length(obj)))
  
  # compute object sizes.
  cand <- get_obj_sizes_dt(cand, obj)
  
  # order candidates after object size.
  cand <- order_after_size(cand)
  
  if (verbose) {
    cat("Begin trimming object.\n")
  }
  
  while (nrow(cand) > 1 && size > size_target) {
    
    # divide candidates into two data.tables - one with top candidate, one with
    # all others.
    cand_top <- cand[1, , drop = FALSE]
    cand <- cand[-1, , drop = FALSE]
    
    # get position index of top candidate.
    cand_top_idx <- get_top_candidate_idx(cand_top, obj, verbose = verbose)
    
    # try to remove top candidate.
    remove_ok <- can_candidate_be_removed(obj = obj, 
                                          idx = cand_top_idx, 
                                          obj_arg_name = obj_arg_name,
                                          results_init = results_init, 
                                          fun = fun, 
                                          dont_touch = dont_touch,
                                          tolerate_warnings = tolerate_warnings,
                                          ...) 
    
    # update candidates depending on results.
    if (remove_ok) {
      if (verbose) {cat_bullet("Element removed.", bullet = "tick", 
                               bullet_col = "green")}
      obj[[cand_top_idx]] <- NULL
      # adjust candidate position indices for this removal.
      cand <- adjust_candidates(cand, cand_top_idx)
      # update object size if relevant.
      if (size_target > 0 || verbose) {
        size <- object_size(obj)
        if (verbose) {cat_bullet("Object size after removal: ", 
                                 green(pf_obj_size(size)), " ",
                                 yellow("[", symbol$arrow_down,
                                        pf_obj_size(-(size - size_init)), "]", 
                                        sep = ""), 
                                 sep = "")}
      }
    } else {
      if (verbose) {cat_bullet("Element could not be removed.", 
                               bullet = "cross", bullet_col = "red")}
      cand_length <- get_length_candidate(obj, cand_top_idx)
      if (!is.null(cand_length)) {
        cand <- add_candidates(obj, cand, cand_top, cand_length)
      }
    }
    
  }
  
  # print service message regarding target size criterion.
  if (size_target > 0 && size > size_target) {
    warning("Target size could not be achieved.")
    } 
  
  if (verbose) {cat("Trimming completed.\n")}
  # return object after trimming.
  obj
  
}
