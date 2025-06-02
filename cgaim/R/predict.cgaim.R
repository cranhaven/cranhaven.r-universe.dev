#' Predictions from a fitted CGAIM object
#'
#' Uses a fitted \code{cgaim} object and computes prediction for the
#'    observed data or new data. Predicts the response, indices or 
#'    ridge functions values at the provided data.
#'
#' @param object A \code{gaim} object.
#' @param newdata A list or data.frame containing the new data to predict.
#'    If missing, fitted values from the model are returned.
#' @param type A character indicating the type of prediction to return.
#'    \code{type = "response"} returns the predicted response. 
#'    \code{type = "terms"}, returns ridge and smooth functions evaluated at
#'    index predicted for \code{newdata}. \code{type = "scterms"} is the same,
#'    except that terms are postmultiplied by their scaling coefficients beta.
#'    \code{type = "indices"} returns predicted indices values.
#' @param select A numeric or character vector indicating terms to return
#'    for all types except \code{"response"}.
#' @param na.action A function indicating how to treat NAs. See
#'    \code{\link[stats]{na.fail}}.
#' @param ... For compatibility with the default \code{predict} method. Unused
#'    at the moment.
#' 
#' @details \code{type = "terms"} returns the scaled ridge functions, i.e. before being multiplied by scaling coefficients beta. 
#'
#' @return When \code{type = "response"} returns a vector of predicted response.
#'    When \code{type = "terms"} or \code{"scterms"}, returns a matrix of evaluated ridge and 
#'    smooth terms. When \code{type = "indices"}, returns
#'    a matrix of evaluated indices.
#'    
#' @seealso \code{\link{cgaim}} for main fitting function
#' 
#' @examples 
#' 
#' ## Simulate some data
#' n <- 200
#' x1 <- rnorm(n)
#' x2 <- rnorm(n)
#' x3 <- rnorm(n)
#' x4 <- rnorm(n)
#' mu <- 4 * exp(8 * x1) / (1 + exp(8 * x1)) + exp(x3)
#' y <- mu + rnorm(n)
#' df1 <- data.frame(y, x1, x2, x3, x4)
#' 
#' ## Fit an unconstrained the model
#' ans <- cgaim(y ~ g(x1, x2, label = "foo") + g(x3, x4, label = "bar"), 
#'   data = df1)
#' 
#' ## Get fitted values
#' yhat <- predict(ans)
#' 
#' ## Predict on new data
#' newdf <- as.data.frame(matrix(rnorm(100), 25, 4))
#' names(newdf) <- sprintf("x%i", 1:4)
#' 
#' # predicted response
#' ypred <- predict(ans, newdf)
#' 
#' # Indices
#' indices <- predict(ans, newdata = newdf, type = "indices")
#' 
#' # Ridge functions
#' funs <- predict(ans, newdata = newdf, type = "terms")
#' 
#' ## Select specific terms
#' ind1 <- predict(ans, newdata = newdf, select = "foo", type = "indices")
#' fun1 <- predict(ans, newdata = newdf, select = "foo", type = "terms")
#' 
#' # Plot
#' plot(ans, select = "foo")
#' points(ind1, fun1)
#' 
#' ## Scaled terms
#' fun2 <- predict(ans, newdata = newdf, select = "foo", type = "scterms")
#' 
#' # Plot
#' plot(ans, select = "foo", yscale = TRUE)
#' points(ind1, fun2)
#'  
#' @export
predict.cgaim <- function(object, newdata, 
  type = c("response", "terms", "scterms", "indices"), select = NULL, 
  na.action = "na.pass", ...)
{
  type <- match.arg(type)
  n <- length(object$fitted)
  p <- length(object$beta) - 1
  # Check select
  if (is.null(select)){
    select <- seq_len(p)
  } else {
    if (is.character(select)){
      selmatch <- match(select, colnames(object$gfit))
      nas <- is.na(selmatch)
      if (any(nas)) warning(paste0("Incorrect select removed: ",
        paste(select[nas], collapse = ", ")))
      select <- selmatch[!nas]
    } else {
      inc <- select > p
      if (any(inc)) warning(paste0("Incorrect select removed: ",
        paste(select[inc], collapse = ", ")))
      select <- select[!inc]
    }
  }
  # Extract terms
  mt <- stats::terms(object)
  if (!missing(newdata)){
    mt <- stats::delete.response(mt)
    gind <- attr(mt, "specials")$g
    mfind <- stats::model.frame(mt[gind], data = newdata, 
      na.action = na.action)
    alphas <- object$alpha
    newindex <- do.call(cbind, Map("%*%", mfind, alphas))
    colnames(newindex) <- names(alphas)
    if (type == "indices"){
      newindex <- newindex[,select, drop = FALSE]
      return(newindex)
    }
    Xterms <- c(data.frame(newindex), newdata[names(object$sm_mod$Xcov)])
    sind <- attr(mt, "specials")$s
    objx <- cbind(object$indexfit, object$sm_mod$Xcov)
    gterms <- matrix(0, nrow(mfind), p)
    for (j in 1:p){
      if (is.numeric(Xterms[[j]])){
        inter_fun <- ifelse(j %in% c(gind, sind), "spline", "approx")
        gterms[,j] <- suppressWarnings(do.call(inter_fun, 
          list(x = objx[,j], y = object$gfit[,j], xout = Xterms[[j]]))$y) 
      } else {
        faccoefs <- unique(object$gfit[,j])
        names(faccoefs) <- unique(objx[,j])
        gterms[,j] <- faccoefs[Xterms[[j]]]
      }
    }
    colnames(gterms) <- colnames(object$gfit)
    if (type == "terms"){
      gterms <- gterms[,select, drop = FALSE]
      return(gterms)
    }
    betas <- object$beta
    if(type == "scterms"){
      scgterms <- gterms * matrix(betas[colnames(gterms)], nrow(gterms),
        ncol(gterms), byrow = TRUE)
      return(scgterms[, select, drop = FALSE])
    }
    yhat <- cbind(1, gterms) %*% betas
    return(yhat)
  } else {
    out <- switch(type,
      response = object$fitted,
      terms = object$gfit[, select, drop = FALSE],
      scterms = (object$gfit * matrix(object$beta[colnames(object$gfit)], n,
        p, byrow = TRUE))[, select, drop = FALSE],
      indices = object$indexfit[, select, drop = FALSE]
    )
    return(out)
  }
}
