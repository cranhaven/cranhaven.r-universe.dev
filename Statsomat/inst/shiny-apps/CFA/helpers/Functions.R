# Continuity cutoff with respect to sample size 
cutoffcont <- function(n){
  
  # Cutoff for continuity f(n)=a*log10(n)+b, f(10)=0.75, , f(50)=0.4, f(100)=0.25
  
  b=125
  a=-50
  
  if (n<=50) {  
    cut <- min(1,round((a*log10(n)+b)/100,2))
  } else {
    
    # 20 unique values for sample sizes greater than 50
    cut <- 20/n
  }
  return(cut)
}

# Variables can be: Pure continuous or continuous with max 3 replications or other discrete distribution which can be approximated by continuous 
continuous <- function(col){
  
  dt <- data.table(col)
  reps <- na.omit(dt[,.N,by=col])
  
  if ( (all(reps[,2]<=3)) || 
       (length(unique(na.omit(col))) / length(na.omit(col)) >= cutoffcont(length(na.omit(col))))   ){
    return(TRUE)
  } else {return(FALSE)
  }
}


# Compute outliers by knn proximity based method, liberal 
knnoutlier <- function(data){
  data <- data[complete.cases(data),]
  outliers_scores <- LOOP(data, k=10, lambda=3)
  outliers <- which(outliers_scores > 0.90, arr.ind = TRUE)
  return(outliers)
} 


# Check normality of one variable 
normality <- function(col){
  
  qq <- qqnorm(col,plot=FALSE)
  qqcor <- with(qq,cor(x,y))
  
  if (qqcor >=0.975){
    return(TRUE)
  } else {
    return(FALSE)
  }
}
  

# p-value format
pformat <- function(p){
  if (is.na(p)){
    return(NA)
  } else if (p<0.001){ 
    return("<0.001")
  } else return (round(p,3))
}


# Cutoff value chi2
cutchi2 <- function(n){
  if (n<300){
    return(0.05)
  } else {
    return(0.01)
  }
}


# Cutoff value srmr
cutsrmr <- function(n){
  if (n<300){
    return(0.1)
  } else if (n<500){
    return(0.08) 
  } else {
    return(0.06)
  }
}

# Standardized cov residuals larger eq than 2.58
stand_residuals <- function(x,sign){
  covstd <- lavResiduals(x,type="raw")$cov.z # Std residuals on cov
  if (sign=="neg"){
    covstd = -covstd
  }
  covstd[upper.tri(covstd, diag=TRUE)] <- NA
  m <- data.frame(which(covstd>=2.58, arr.ind=TRUE))
  dimnames <- x@Model@dimNames[[1]][1][[1]]
  dimnames2 <- data.frame(dimnames[m[,1]],dimnames[m[,2]])
  return(dimnames2)
}

# Corr residuals larger eq than 0.1
corr_residuals <- function(x,sign){
  covstd <- resid(x, "cor")$cov # Corr. Residuals 
  if (sign=="neg"){
    covstd = -covstd
  }
  covstd[upper.tri(covstd, diag=TRUE)] <- NA
  m <- data.frame(which(covstd>=0.1, arr.ind=TRUE))
  dimnames <- x@Model@dimNames[[1]][1][[1]]
  dimnames2 <- data.frame(dimnames[m[,1]],dimnames[m[,2]])
  return(dimnames2)
}

# No Cross-loadings is TRUE
cross_loadings <- function(df){
  if (sum(duplicated(df[df$op=="=~",3])) == 0){
    return(TRUE) 
  } else {
    return(FALSE)}
}

# Error covariances existent? 
error_covariances <- function(df,fit){
  covariances_stand <- df[df$op=="~~",]
  covariances_stand <- covariances_stand[which(covariances_stand$lhs != covariances_stand$rhs),]
  ov <- fit@pta$vnames$ov[[1]]
  covariances_error <- covariances_stand[which(covariances_stand$lhs %in% ov),]
  if (nrow(covariances_error)>0){
    return(TRUE) 
  } else {
    return(FALSE)}
}


# Table Interpretation of Unstandardized Factor Loadings
text <-  function(df, cols){
  df <- df[df$op=="=~",]
  x <- df$rhs
  duplicates <- which(duplicated(x))
  if (cols[4]>=0){
    if (cols[3] %in% df[duplicates,3]){
      paste("A 1-unit increase in",cols[1],"leads to a",cols[4],"-unit increase in the",cols[3],"while the other factor(s) are held constant.")
    } else {
      paste("A 1-unit increase in",cols[1],"leads to a",cols[4],"-unit increase in the",cols[3])
    }
  } else {
    if (cols[3] %in% df[duplicates,3]){
      paste("A 1-unit increase in",cols[1],"leads to a",cols[4],"-unit decrease in the",cols[3],"while the other factor(s) are held constant.")
    } else {
      paste("A 1-unit increase in",cols[1],"leads to a",cols[4],"-unit decrease in the",cols[3])
    }
  }
}


# Cross-loadings rowwise 
cross_loadings_row <- function(df,cols){
  if (cols[2] %in% df[which(duplicated(df$Variable)),2]){
    paste("cross")
  } else {
    paste("x") }
}

# Discriminant Validity
discriminantVal <- function (object, cutoff = 0.9, merge = FALSE, level = 0.95, data) {
  free <- lavInspect(object, "free", add.class = FALSE)
  if (lavInspect(object, "ngroups") > 1L | lavInspect(object, 
                                                      "nlevels") > 1L) 
    stop("Only implemented for single-group, single-level models so far.")
  lvs <- lavNames(object, "lv")
  if (cutoff <= 0 | cutoff > 1) 
    stop("The cutoff must be between (0,1]")
  if (merge & !missing(cutoff) & cutoff != 1) 
    message("Merging factors imply constraining factor correlation to 1. ", 
            "Cutoff will be ignored.")
  if (length(lvs) == 0) 
    stop("The model does not have any exogenous latent variables.")
  if (length(lvs) == 1) 
    stop("The model has only one exogenous latent variable. ", 
         "At least two are required for assessing discriminant validity.")
  if (length(lavNames(object, "lv.y")) > 0) 
    warning("The model has at least one endogenous latent variable (", 
            paste(lavNames(object, "lv.y"), collapse = ", "), 
            "). The correlations of these variables will be estimated after ", 
            "conditioning on their predictors.")
  psi <- free$psi[lvs, lvs]
  pt <- parTable(object)
  varIndices <- which(pt$lhs == pt$rhs & pt$lhs %in% lvs & 
                        pt$op == "~~")
  covIndices <- which(pt$lhs != pt$rhs & pt$lhs %in% lvs & 
                        pt$rhs %in% lvs & pt$op == "~~")
  if (any(diag(psi) != 0)) {
    message("Some of the latent variable variances are estimated instead of ", 
            "fixed to 1. The model is re-estimated by scaling the latent ", 
            "variables by fixing their variances and freeing all factor loadings.")
    i <- intersect(varIndices, which(pt$free != 0))
    pt$free[i] <- 0
    pt$ustart[i] <- 1
    pt$user[i] <- 1
    i <- which(pt$lhs %in% pt$lhs[i] & pt$op == "=~")
    pt$free[i] <- -1
    pt$ustart[i] <- NA
    i <- which(pt$free != 0)
    pt$free[i] <- seq_along(i)
    object <- cfa(model = pt[, 1:12], data=data, missing="fiml", estimator="ML")  # already defined above 
    #object <- lavaan::update(object, model = pt[, 1:12])
    pt <- parTable(object)
  }
  est <- lavInspect(object, "est")$psi[lvs, lvs]
  if (any(diag(est) != 1)) {
    message("Some of the latent variable variances are fixed to values other ", 
            "than 1. The model is re-estimated by scaling the latent variables", 
            " based on the first factor loading.")
    pt$ustart[varIndices] <- 1
    object <- cfa(model = pt[, 1:12], data=data, missing="fiml", estimator="ML")  # already defined above 
    #object <- lavaan::update(object, model = pt[, 1:12])
    pt <- parTable(object)
  }
  ret <- lavaan::parameterEstimates(object, ci = TRUE, level = level)[covIndices, 
                                                                      c("lhs", "op", "rhs", "est", 
                                                                        "ci.lower", "ci.upper")]
  rownames(ret) <- seq_len(nrow(ret))
  constrainedModels <- lapply(covIndices, function(i) {
    thisPt <- pt
    if (merge) {
      lhs <- pt$lhs[i]
      rhs <- pt$rhs[i]
      thisPt$lhs[thisPt$lhs == lhs & thisPt$op == "=~"] <- rhs
      thisPt <- thisPt[!(thisPt$lhs == lhs | thisPt$rhs == 
                           lhs), ]
      thisPt$id <- seq_len(nrow(thisPt))
    }
    else {
      if (abs(pt$est[i]) > cutoff) {
        thisCutoff <- pt$est[i]
      }
      else {
        thisCutoff <- ifelse(pt$est[i] < 0, -cutoff, 
                             cutoff)
      }
      thisPt$free[i] <- 0
      thisPt$ustart[i] <- thisCutoff
    }
    j <- which(thisPt$free != 0)
    thisPt$free[j] <- seq_along(j)
    cfa(model = thisPt[, 1:12], data=data, missing="fiml", estimator="ML")  # already defined above 
    #lavaan::update(object, model = thisPt[, 1:12])
  })
  lrTests <- lapply(constrainedModels, function(constrained) {
    lavaan::lavTestLRT(object, constrained)[2, ]
  })
  ret <- cbind(ret, do.call(rbind, lrTests))
  attr(ret, "baseline") <- object
  attr(ret, "constrained") <- constrainedModels
  ret
}





# Direction of Factor Loadings 
direction_fl <- function(line){
  line <- gsub(" ", "", line, fixed = TRUE)
  line <- unlist(strsplit(line, split="=~", fixed=T))
  ov <- unlist(strsplit(line[2], split="\\+\\-|\\+|\\-"))
  if (ov[1]=="") ov <- ov[2:length((ov))]
  
  neg_df <- NULL
  pos_df <- NULL
  if (grepl("\\-", line[2])==TRUE){ 
    neg <- unlist(gregexpr("\\-", line[2]))
    neg_df <- data.frame(neg,sign=rep(-1,length(neg)))
    colnames(neg_df) <- c("Order","Sign")
  }
  if (grepl("\\+", line[2])==TRUE){ 
    pos <- unlist(gregexpr("\\+", line[2]))
    pos_df <- data.frame(pos,sign=rep(1,length(pos)))
    colnames(pos_df) <- c("Order","Sign")
  }
  signs <- rbind(neg_df,pos_df)
  signs <- signs[order(signs$Order),]
  signs$Variable <- ov
  signs$lhs <- rep(line[1],nrow(signs))
  return(signs)
}



# Model variables
modelv <- function(line){
  line <- gsub(" ", "", line, fixed = TRUE)
  line <- unlist(strsplit(line, split="=~", fixed=T))
  ov <- unlist(strsplit(line[2], split="\\+\\-|\\+|\\-"))
  if (ov[1]=="") ov <- ov[2:length((ov))]
  return(ov)
}
