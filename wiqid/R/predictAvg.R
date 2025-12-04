

# Function to calculate model averaged predictions from multiple models

predictAvg <- function(modList, newdata, parameter, ci=0.95, type = c("link",
    "response"), IC=AICc) {

  nModels <- length(modList)

  # Get model weights
  ic <- sapply(modList, IC)
  delta <- ic - min(ic)
  modLLH <- exp(-delta/2)
  modWt <- modLLH / sum(modLLH)

  # Get model predictions
  ests <- SEs <- matrix(NA, nrow(newdata), nModels)
  # colnames(ests) <- colnames(SEs) <- names(modList)
  links <- character(nModels)
  for(i in 1:nModels) {
    pred <- predict(modList[[i]], newdata=newdata, parameter=parameter,
        ci=ci, type='link')
    ests[,i] <- pred[,1]
    SEs[,i] <- pred[,2]
    links[i] <- attr(pred, "link")
  }
  link <- links[1]
  stopifnot(all(links == link))
  
  out <- matrix(NA_real_, nrow(newdata), 4)
  rownames(out) <- rownames(newdata)
  colnames(out) <- c("est", "SE", "lowCI", "uppCI")

  # Get MA point estimate
  tmp <- sweep(ests, 2, modWt, "*")
  out[, 'est'] <- rowSums(tmp)
  
  # Get unconditional SE
  correctn <- sweep(ests, 1, out[, 'est'], "-")^2
  var.u <- SEs^2 + correctn             # unconditional variances
  tmp <- sweep(var.u, 2, modWt, "*")
  out[, 'SE'] <- sqrt(rowSums(tmp))

  # Get CI
  crit <- fixCI(ci)
  out[, 3:4] <- sweep(outer(out[, 2], crit), 1, out[, 1], "+")

  # Convert to 'response' scale if requested
  type <- match.arg(type)
  if(type == "response") {
    SE <- out[, 2]
    if(link == "logit") {
      out <- plogis(out)
      out[, 2] <- SE * out[, 1] * (1 - out[, 1])
    } else if(link == "probit"){
      SE <- SE * dnorm(out[, 1])
      out <- pnorm(out)
      out[, 2] <- SE
    } else if(link == "log") {
      out <- exp(out)
      out[, 2] <- SE * out[, 1]
    } else {
      stop("Link type ", link, " not recognised.")
    }
  }
  attr(out, "ci") <- ci
  attr(out, "link") <- link
  return(out)
}
