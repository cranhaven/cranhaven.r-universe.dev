#' @importFrom stats as.formula chisq.test complete.cases cor lm

#' @export


BivariateAssoc <- function(Y,X,xx=TRUE) {

  X <- as.data.frame(X)
  xnames <- names(X)
  xformats <- sapply(X,class)
  yformat <- class(Y)
  
  df <- cbind.data.frame(Y,X)
  formule <- as.formula(paste('Y ~',paste(xnames,collapse='+')))
  ct <- party::ctree(formule, df, controls=party::ctree_control(stump=TRUE))
  p.value <- 1-nodes(ct,1)[[1]]$criterion$criterion
  criterion <- -log(nodes(ct,1)[[1]]$criterion$criterion)

  res <- list()
  for(i in 1:ncol(X)) {
    # print(i)
    if(yformat %in% c('numeric','integer') & xformats[i] %in% c('numeric','integer')) {
      assoc <- cor(Y, X[,i], use='complete.obs', method='kendall')
      mesure='kendall'
    }
    if(yformat %in% c('numeric','integer') & xformats[i]=="factor") {
      assoc <- summary(lm(Y ~ X[,i]))$adj.r.squared
      mesure='eta2'
    }
    if(yformat=="factor" & xformats[i]%in% c('numeric','integer')) {
      assoc <- summary(lm(X[,i] ~ Y))$adj.r.squared
      mesure='eta2'
    }
    if(yformat=="factor" & xformats[i]=="factor") {
      t <- table(Y,X[,i])
      assoc <- sqrt(chisq.test(t)$statistic / (length(Y)*(min(nrow(t),ncol(t))-1)))
      mesure="cramer"
    }
    res[[i]] <- data.frame(mesure,assoc,stringsAsFactors = F)
  }
  res <- do.call('rbind.data.frame',res)
  restot <- data.frame(variable=xnames,measure=res$mesure,assoc=round(res$assoc,3),p.value=round(p.value,5),criterion=criterion)
  restot <- restot[order(restot$criterion, decreasing=F),]
  restot$criterion <- round(restot$criterion,10)
  rownames(restot) <- NULL

  if(xx==TRUE) {
    combi <- utils::combn(xnames,2,simplify=F)
    res <- list()
    for(i in 1:length(combi)) {
      x1 <- X[,combi[[i]][1]]
      x2 <- X[,combi[[i]][2]]

      df <- data.frame(x1,x2,stringsAsFactors=F)
      df <- df[complete.cases(df),]
      ct <- party::ctree(x1~x2, data=df, controls=party::ctree_control(stump=TRUE))
      p.value <- 1-nodes(ct,1)[[1]]$criterion$criterion
      criterion <- -log(nodes(ct,1)[[1]]$criterion$criterion)

      if(class(x1) %in% c('numeric','integer') & class(x2) %in% c('numeric','integer')) {
        assoc <- cor(x1,x2, use='complete.obs', method='kendall')
        mesure='kendall'
      }
      if(class(x1) %in% c('numeric','integer') & is.factor(x2)) {
        assoc <- summary(lm(x1~x2))$adj.r.squared
        mesure='eta2'
      }
      if(is.factor(x1) & class(x2) %in% c('numeric','integer')) {
        assoc <- summary(lm(x2~x1))$adj.r.squared
        mesure='eta2'
      }
      if(is.factor(x1) & is.factor(x2)) {
        t <- table(x1,x2)
        assoc <- sqrt(chisq.test(t)$statistic / (length(Y)*(min(nrow(t),ncol(t))-1)))
        mesure="cramer"
      }
      res[[i]] <- data.frame(mesure,assoc,p.value,criterion,stringsAsFactors = F)
    }
    res <- do.call('rbind.data.frame',res)
    noms <- do.call('rbind.data.frame',combi)
    restot2 <- data.frame(variable1=noms[,1],variable2=noms[,2],measure=res$mesure,assoc=round(res$assoc,3),p.value=round(res$p.value,5),criterion=res$criterion,row.names=NULL)
    restot2 <- restot2[order(restot2$criterion, decreasing=F),]
    restot2$criterion <- round(restot2$criterion,10)
    rownames(restot2) <- NULL
  } else {
    restot2 <- NULL
  }
  
  return(list(YX=restot, XX=restot2))
}
