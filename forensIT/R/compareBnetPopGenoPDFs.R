#' @title Compare population and Bayesian network genotype probability density functions # nolint
#' @description Compare population and Bayesian network genotype probability density functions # nolint
#' @param lprobTable list of probability tables
#' @return list of KL divergences
#' @export
compareBnetPopGenoPDFs <- function(lprobTable){ #nolint
  klDiv <- resCrossH<- dH <- dHnorm <- c() # nolint
  bsigma <- FALSE
  dbg    <- TRUE
  for(i in seq_along(lprobTable)){ # nolint
    resCrossH <- rbind(resCrossH, c(crossH(lprobTable[[i]][, "pop"], #nolint
                                  lprobTable[[i]][, "bnet"]),
                                  crossH(lprobTable[[i]][, "bnet"],
                                  lprobTable[[i]][, "pop"])))
    dH        <- rbind(dH, H(lprobTable[[i]][, "pop"]) - #nolint
                           H(lprobTable[[i]]["bnet"]))
    dHnorm    <- rbind(dHnorm, H(lprobTable[[i]][, "pop"],  # nolint
    normalized = TRUE) - H(lprobTable[[i]]["bnet"], normalized = TRUE))
    if (dbg) {
      aux  <- KLde(lprobTable[[i]][, "pop"], lprobTable[[i]][, "bnet"])
      if (bsigma) {
       names(aux) <- c("KL_pop-bnet", "KL_pop-bnet_sigma",
       "KL_pop-bnet_g", "H_gexclusion", "pexclusion", "epsilon")
      }else {
        names(aux) <- c("KL_pop-bnet", "KL_pop-bnet_g",
        "H_gexclusion", "pexclusion", "epsilon")
      }
          }else {
      aux        <- KLd(lprobTable[[i]][, "pop"], lprobTable[[i]][, "bnet"])
      names(aux) <- c("KL_pop-bnet", "pexclusion", "epsilon")

    }
    klDiv     <- rbind(klDiv, #nolint
                  c("KL_bnet-pop" = KLd(lprobTable[[i]][, "bnet"],
                   lprobTable[[i]][, "pop"]), aux))
  }
  colnames(dH)        <- "deltaH" #nolint
  colnames(dHnorm)    <- "deltaHnorm" #nolint
  colnames(resCrossH) <- c("crossH_pop-bnet", "crossH_bnet-pop") #nolint

  rownames(resCrossH) <- rownames(klDiv) <- names(lprobTable) #nolint
  df <- data.frame(dH, dHnorm, resCrossH, klDiv)
  if (length(grep("/", rownames(df))) > 0) {
   aux <- rownames(df)
   aux <- unlist(lapply(lapply(strsplit(aux, "/"),
   function(x) {
    sort(as.numeric(x))}),
    paste0, collapse = "/"))
   rownames(df) <- aux
  }
  return(df)
}
