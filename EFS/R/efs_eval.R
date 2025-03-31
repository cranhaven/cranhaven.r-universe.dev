#'@title Evaluation of Ensemble Features Selection
#'@description Provides several evaluation tests of
#'  the ouput of \code{\link{ensemble_fs}}. There are
#'  performance test, namely the logreg test and permutation 
#'  test as well as tests of stability via the variance 
#'  of feature importances and the Jaccard-index (see Details).
#'@param data an object of class data.frame
#'@param efs_table a table object of class matrix (retrieved
#'  from \code{ensemble_fs})
#'@param file_name a character string, name which is used for the two 
#'  possible PDF files.
#'@param classnumber a number indicating the index of variable for binary classification
#'@param NA_threshold a number in range of [0,1]. Threshold for deletion
#'  of features with a greater proportion of NAs than \code{NA_threshold}.
#'@param logreg a logical value indicating whether to conduct an evaluation 
#'via logistic regression or not
#'@param rf a logical value indicating whether to conduct an evaluation 
#'via random forest or not
#'@param permutation a logical value indicating whether to conduct a permutation 
#'of the class variable or not
#'@param p_num number of permutations
#'@param variances a logical value indicating whether to calculate the variances 
#'of importances retrieved from bootrapping or not
#'@param jaccard a logical value indicating whether to calculate the jaccard-index or not
#'@param bs_num a number of boostrap permutations of the importances
#'@param bs_percentage a number in range of [0,1]. Proportion of randomly selected samples for boostraping
#'@inheritParams ensemble_fs
#'@return An object of class list, with the following components: 
#' \cr "AUC of LR with all parameters",
#' \cr "AUC of LR with EFS parameter" 
#' \cr "P-value of LR-ROC test",
#' #' \cr "AUC of RF with all parameters",
#' \cr "AUC of RF with EFS parameter" 
#' \cr "P-value of RF-ROC test",
#' \cr "P-value of permutation", 
#' \cr "Variances of feature importances",
#' \cr "Jaccard-index".
#'
#'@details 
#'  A logistic regression model with leave-one-out cross-validation (LOOCV) of the
#'  selected features and of all feature is conducted by \code{logreg = TRUE}.
#'  Both AUC-values of the ROC curves are compared with \code{\link{roc.test}}.
#'  The ROC curves are illustrated on the PDF file "file_name" + "LG-ROC.pdf".
#'  \cr 
#'  By \code{rf = TRUE}, random forst model will be constructed and evaluated.
#'  Parallel to Logreg, the AUC-values of the two ROC curves of all features and a subset  
#'  of the best ranked feautres are compared with \code{\link{roc.test}}.
#'  The ROC curves are illustrated on the PDF file "file_name" + "RF-ROC.pdf".
#'  \cr
#'  \cr The permutation test (\code{permutation = TRUE}) compares the AUC outcome of 
#'  an logistic regression with \code{p_num} AUCs from random 
#'  permutations of the class variable by a \code{\link{t.test}}.
#'  \cr 
#'  \cr Variances of the importances after a bootstrapping analysis are 
#'  calculated by \code{variances = TRUE}. Thereby the number and proportion
#'  of the bootstrapping can be set by \code{bs_num} and \code{bs_percentage}.
#'  The function also provides a PDF file "file_name" +"_Variances.pdf".
#'  Additionally, the Jaccard-index of this bootstrapped importances 
#'  can be calculated by setting \code{jaccard=TRUE}.
#'@author Ursula Neumann
#'@examples
#'  ## Loading dataset in environment
#'  data(efsdata)
#'  ## Generate a ranking based on importance (with default
#'  ## NA_threshold = 0.7,cor_threshold = 0.2)
#'  efs<-ensemble_fs(efsdata,5,runs=2)
#'  ## Conduct AUC test and permutation test
#'  eval_example <- efs_eval(data = efsdata, efs_table = efs, file_name = 'eval_test', 
#'                       classnumber = 5, NA_threshold = 0.2,
#'                       logreg = TRUE,
#'                       rf = FALSE,
#'                       permutation = TRUE, p_num = 2, 
#'                       variances = FALSE, jaccard = FALSE)
#' ## Calculating variances and the Jaccard-index can take several minutes computation time 
#'@seealso \link[stats]{glm}, \link[pROC]{roc},\link[ROCR]{prediction}, \link[graphics]{boxplot}, \link[utils]{tail}, \link[stats]{t.test}
#'@importFrom ROCR performance prediction plot
#'@importFrom pROC roc roc.test
#'@importFrom graphics boxplot
#'@importFrom stats t.test
#'@importFrom utils tail
#'@export
#'
efs_eval <- function(data, efs_table, file_name, 
                     classnumber, NA_threshold, 
                     logreg = TRUE,
                     rf = TRUE,
                     permutation = TRUE, p_num =  100, 
                     variances = TRUE, jaccard = TRUE,
                     bs_num =100, bs_percentage = 0.9){
  
  if(missing(efs_table))
    stop("efs_table argument missing")
  if(missing(file_name))
    stop("file_name argument missing")
  if(missing(classnumber))
    stop("classnumber argument missing")
  # if(missing(sep))
  #  stop("seperator argument missing")
  if(missing(NA_threshold)){
    NA_threshold=0.2
    print("default value for NA_threshold = 0.2")}
  if(!is.numeric(NA_threshold) | NA_threshold > 1 |
     NA_threshold < 0)
    stop("invalid argument:
           NA_threshold is required to be in [0,1]")
  if(missing(p_num)){
    p_num=100
    print("default value for p_num = 100")}
  if(missing(bs_num)){
    bs_num = 100
    print("default value for bs_num = 100")}
  if(missing(bs_percentage)){
    bs_percentage=0.9
    print("default value for bs_percentage = 0.9")}
  
  classname = colnames(data)[classnumber]
  
  # Deletion of parametern with too many NA
  NrNA= c()
  for(i in 1:length(data[1,])){
    NrNA[i] = length(which(is.na(data[,i])))
  }
  NmNA = which(NrNA/length(data[,1])>0.2)
  if(length(NmNA) != 0) data=data[,-NmNA]
  
  data = na.omit(data, row.names=F)
  
  # or only zeros or variance zero
  data=data[,which(colSums(data,na.rm=F)!=0 & apply(data, 2, var)!=0)]
  
  clnr= which(colnames(data)==classname)
  
  # LogReg test
  logreg_test <- function(data, efs_table, file_name, classnumber){
    
    # Retrieve EFS features
    e.features = which(colSums(efs_table)>
                         mean(colSums(efs_table)))
  
    klasse = data[,classname]
    clnr= which(colnames(data)==classname)
    data = data[,-clnr]
    # k-fold cross-validation
    k=length(data[,1])
    prob1=c()
    prob2=c()
    prob3=c()
    for(i in 1:k){
      Train = seq(i,to=nrow(data),by=k)
      training <- data[-Train, ]
      training.cl <- klasse[-Train]
      testing <- data[ Train, ]
      testing.cl <- klasse[Train]
      logreg1 = glm(as.factor(training.cl)~.,
                    data = training, family = binomial,control = list(maxit = 50))
      logreg3 = glm(as.factor(training.cl)~.,
                    data = training[,e.features],
                    family = binomial,control = list(maxit = 50))
      prob1= (c(prob1,predict(logreg1, newdata=testing)))
      prob3= (c(prob3,predict(logreg3, newdata=testing)))
    }
    
    # Compute AUC for predicting klasse with the model
    prob1=prob1[order(as.numeric(names(prob1)))]
    roc1=roc(klasse, prob1, ci=T)
    ci1=roc1$ci
    ci1= gsub('95% CI:', '',ci1)
    ci1=round(as.numeric(ci1),1)*100
    pred1 <- prediction(prob1, klasse)
    perf1 <- performance(pred1, measure = "tpr",
                         x.measure = "fpr")
    auc1 <- performance(pred1, measure = "auc")
    auc1 <- auc1@y.values[[1]]
    
    prob3=prob3[order(as.numeric(names(prob3)))]
    roc3=roc(klasse, prob3, ci=T)
    ci3=roc3$ci
    ci3= gsub('95% CI:', '',ci3)
    ci3=round(as.numeric(ci3),3)*100
    pred3 <- prediction(prob3, klasse)
    perf3 <- performance(pred3, measure = "tpr",
                         x.measure = "fpr")
    auc3 <- performance(pred3, measure = "auc")
    auc3 <- auc3@y.values[[1]]
    ## Printing p value on plot
    r=roc.test(roc1,roc3)
    p = as.numeric(r$p.value)
    P = paste("p = ",round(p,3),sep ="")
    if(p<0.001){P = "p < 0.001"}
    
    # ROC Kurve
    d=c(0,1)
    pdf(paste(file_name,"_LG_ROC.pdf", sep=""))
    plot(perf1, avg="vertical", spread.estimate="boxplot",
         main="")
    plot(perf3, avg="vertical", spread.estimate="boxplot",
         main="",add=TRUE, col = 'blue')
    abline(d, lty=2)
    text(0.15, 0.9, cex=1, paste("All: ",
                                 round(mean(auc1),3)*100,
                                 "% (", format(ci1[1], nsmall=1),
                                 '...',format(ci1[3], nsmall=1),
                                 ")", sep=""))
    text(0.15, 1,   cex=1, paste("EFS: ",round(mean(auc3),3)*100,
                                 "% (",format(ci3[1], nsmall=1),
                                 '...',format(ci3[3], nsmall=1),
                                 ")", sep=""), col='blue')
    text(0.8, 0.1, cex=1, paste("ROC-test: ",P,sep=""))
    dev.off()
    return(rbind(round(mean(auc1),3)*100,round(mean(auc3),3)*100,p))
  }
  
  
  rf_test <- function(data, efs_table, file_name, classnumber){
    e.features =  which(colSums(efs_table)>
                             mean(colSums(efs_table)))
    #cp.features =  which(colSums(efs_table)>
    #                       cutpoint(efs_table))
    
    klasse = data[,classname]
    clnr= which(colnames(data)==classname)
    data = data[,-clnr]
    
    # keine LOOCV nötig für RF
    votes1 = c()
    real1 = c()
    votes3 = c()
    real3 = c()
    for(i in 1:1000)
    {
      rf1 = randomForest(as.factor(klasse)~., data= data)
      votes1 = cbind(votes1, rf1$votes[,2])
      real1 = cbind(real1, klasse)
      
      rf3 = randomForest(as.factor(klasse)~., data = data[,e.features])
      votes3 = cbind(votes3, rf3$votes[,2])
      real3 = cbind(real3, klasse)
    }
    
    pred1 = prediction(votes1, real1) 
    perf1 = performance(pred1, "auc")
    roc1=roc(klasse,rf1$votes[,2] , ci=T)
    
    pred3 = prediction(votes3, real3) 
    perf3 = performance(pred3, "auc")
    roc3=roc(klasse, rf3$votes[,2], ci=T)
    
    
    # Compute AUC for predicting klasse with the model
    ci1=roc1$ci
    ci1= gsub('95% CI:', '',ci1)
    ci1=round(as.numeric(ci1),1)*100
    pred1 <- prediction(rf1$votes[,2], klasse)
    perf1 <- performance(pred1, measure = "tpr",
                         x.measure = "fpr")
    auc1 <- performance(pred1, measure = "auc")
    auc1 <- auc1@y.values[[1]]
    
    
    ci3=roc3$ci
    ci3= gsub('95% CI:', '',ci3)
    ci3=round(as.numeric(ci3),3)*100
    pred3 <- prediction(rf3$votes[,2], klasse)
    perf3 <- performance(pred3, measure = "tpr",
                         x.measure = "fpr")
    auc3 <- performance(pred3, measure = "auc")
    auc3 <- auc3@y.values[[1]]
    
    
    
    ## Printing p value on plot
    r=roc.test(roc1,roc3)
    p = as.numeric(r$p.value)
    P = paste("p = ",round(p,3),sep ="")
    if(p<0.001){P = "p < 0.001"}
    
    # ROC Kurve
    d=c(0,1)
    pdf(paste(file_name,"_RF_ROC.pdf", sep=""))
    plot(perf1, avg="vertical", spread.estimate="boxplot",
         main="")
    plot(perf3, avg="vertical", spread.estimate="boxplot",
         main="",add=TRUE, col = 'blue')
    abline(d, lty=2)
    text(0.15, 0.9, cex=1, paste("All: ",
                                 round(mean(auc1),3)*100,
                                 "% (", format(ci1[1], nsmall=1),
                                 '...',format(ci1[3], nsmall=1),
                                 ")", sep=""))
    text(0.15, 1,   cex=1, paste("EFS: ",round(mean(auc3),3)*100,
                                 "% (",format(ci3[1], nsmall=1),
                                 '...',format(ci3[3], nsmall=1),
                                 ")", sep=""), col='blue')
    text(0.8, 0.1, cex=1, paste("ROC-test: ",P,sep=""))
    dev.off()
    return(rbind(round(mean(auc1),3)*100,round(mean(auc3),3)*100,p))
  }
  
  # LogReg for permutation - gives back AUC-value
  perm_logreg_test <- function(data, efs_table, file_name, classnumber,
                               NA_threshold){
    klasse = data[[1]]
    data = data.frame(data[,-1])
    
    # Retrieve EFS features
    e.features = which(colSums(efs_table)>
                         mean(colSums(efs_table)))
    
    # k-fold cross-validation
    k=length(data[,1])
    prob1=c()
    prob2=c()
    prob3=c()
    for(i in 1:k){
      Train = seq(i,to=nrow(data),by=k)
      training <- data[-Train, ]
      training.cl <- klasse[-Train]
      testing <- data[ Train, ]
      testing.cl <- klasse[Train]
      logreg1 = glm(as.factor(training.cl)~.,
                    data = training, family = binomial,control = list(maxit = 50))
      logreg3 = glm(as.factor(training.cl)~.,
                    data = training[,e.features],
                    family = binomial,control = list(maxit = 50))
      prob1= (c(prob1,predict(logreg1, newdata=testing)))
      prob3= (c(prob3,predict(logreg3, newdata=testing)))
    }
    
    # Compute AUC for predicting klasse with the model
    prob1=prob1[order(as.numeric(names(prob1)))]
    roc1=roc(klasse, prob1, ci=T)
    ci1=roc1$ci
    ci1= gsub('95% CI:', '',ci1)
    ci1=round(as.numeric(ci1),1)*100
    pred1 <- prediction(prob1, klasse)
    perf1 <- performance(pred1, measure = "tpr",
                         x.measure = "fpr")
    auc1 <- performance(pred1, measure = "auc")
    auc1 <- auc1@y.values[[1]]
    
    
    prob3=prob3[order(as.numeric(names(prob3)))]
    roc3=roc(klasse, prob3, ci=T)
    ci3=roc3$ci
    ci3= gsub('95% CI:', '',ci3)
    ci3=round(as.numeric(ci3),3)*100
    pred3 <- prediction(prob3, klasse)
    perf3 <- performance(pred3, measure = "tpr",
                         x.measure = "fpr")
    auc3 <- performance(pred3, measure = "auc")
    auc3 <- auc3@y.values[[1]]
    ## Printing p value on plot
    r=roc.test(roc1,roc3)
    p = as.numeric(r$p.value)
    P = paste("p = ",round(p,3),sep ="")
    if(p<0.001){P = "p < 0.001"}
    
    return(auc3)
  }
  
  # Permutation function
  permutation_test <- function(classnumber, NA_threshold, efs_table, p_num){
      klasse = data[,clnr]
      data = data[,-clnr]
      
      dat.all = cbind(klasse,data)
      AUC1 = perm_logreg_test (dat.all, efs_table, file_name, classnumber, NA_threshold)
      
      AUC = c()
      for(i in 1:p_num){
        klasse = sample(klasse,replace=FALSE)
        data.1 = cbind(klasse,data)  
        # perm_logreg_test gibt den AUC aus
        AUC[i] = perm_logreg_test(data.1, efs_table, file_name, classnumber, NA_threshold)
      }
      
      ttest = t.test(AUC,alternative = "less", mu=AUC1)
      p.values = as.vector(ttest[["p.value"]])
      return(p.values)
    }
  
  # Calculation of Importances
  stability_test <- function(data, classnumber, bs_num, bs_percentage){
    
    pos = which(data[,clnr]==1)
    neg = which(data[,clnr]==0)
    
    importances <- NULL
    print("Conducting boostrapping:")
    for(i in 1:bs_num){
      print(i)
      pos_sam = sample(pos, bs_percentage*length(pos), replace = FALSE)
      neg_sam = sample(neg, bs_percentage*length(neg), replace = FALSE)
      df=0
      df = rbind(data[pos_sam,], data[neg_sam,])
      
      # Conduct ensemble_fs with 90% of data
      efs_table = ensemble_fs(df, clnr, NA_threshold=0.2, cor_threshold=0.7, runs=100, selection = c(T,T,T,T,T,T,T,T))
      importances = cbind(importances, colSums(efs_table))
    }
    return(importances)
 }
 
  # Variance of feature importances
  feature_var <- function(importances){
   m=NULL
   for(i in 1: length(importances[,1])){
     m = c(m,mean(importances[i,],na.rm=TRUE))
   }
   m = order(m,decreasing=TRUE)[1:5]
   vars=NULL
   for(i in 1:length(importances[m,1])){
     vars = c(vars,var(importances[m,][i,],na.rm=TRUE))
   }
   pdf(paste(file_name,'_Variances.pdf', sep=""))
   boxplot(t(importances[m,-1]),main = file_name, ylim = c(0,1))
   dev.off()
   return(vars)
 }
 
  # Jaccard-index
  jaccard_index <- function(importances){
   #importances = stability_test(data, classnumber, bs_num, bs_percentage)
   importances = importances[ , colSums(is.na(importances)) == 0]
   
   e.features = which(rowMeans(importances) > mean(rowMeans(importances)))
   l = length(e.features)
   x = apply(importances, 2 , order)
   y = tail(x,l)
   z = list(y) 
   
   schnitt = Reduce(intersect, z)
   vereinigung = Reduce(union, z)
   index =  length(schnitt)/length(vereinigung)
   return(index)
 }
 
 
 if(logreg == TRUE){
   lg.efs = logreg_test(data, efs_table, file_name, classnumber)
 }
 else{lg.efs = rbind("Not conducted", "Not conducted","Not conducted")}
  
  if(rf == TRUE){
    rf.efs = rf_test(data, efs_table, file_name, classnumber)
  }
  else{rf.efs = rbind("Not conducted", "Not conducted","Not conducted")}
 
 if(permutation == TRUE){
   permutation.p.values = permutation_test(classnumber, NA_threshold, efs_table,p_num)
 }
  else{permutation.p.values = "Not conducted"}
 
 if(variances == TRUE| jaccard ==TRUE){
   importances = stability_test(data, classnumber, bs_num, bs_percentage)
 }
      
 if(variances == TRUE){
   vars = feature_var(importances)
 }
 else{vars = "Not conducted"}
 
 if(jaccard == TRUE){
   jaccard_index = jaccard_index(importances)
 }
  else{jaccard_index = "Not conducted"}
 
 results = NULL
 results = list("AUC of LR of all parameters" = as.vector(lg.efs[1,1]), "AUC of LR of EFS parameters" = as.vector(lg.efs[2,1]),  "P-value of LG-ROC test" = as.vector(lg.efs[3,1]),
                "AUC of RF of all parameters" = as.vector(rf.efs[1,1]), "AUC of RF of EFS parameters" = as.vector(rf.efs[2,1]),  "P-value of RF-ROC test" = as.vector(rf.efs[3,1]),
                "P-value of permutation" = as.vector(permutation.p.values), "Variances of feature importances"= vars, 
                "Jaccard-index"=jaccard_index)
return(results)
}
