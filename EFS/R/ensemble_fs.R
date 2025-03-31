#'@title Ensemble Feature Selection
#'@description Uses an ensemble of feature selection methods
#'  to create a normalized quantitative
#'  score of all relevant features. Irrelevant features
#'  (e.g. features with too many missing values or variance = 1) will be deleted. See
#'  Details for a list of tests used in this function.
#'@details Following methods are provided in the \code{ensemble_fs}:
#'   \itemize{
#'  \item Median: p-values from Wilcoxon signed-rank
#'    test (\link{wilcox.test})
#'  \item Spearman: Spearman's rank correlation test
#'    arccording to Yu et al. (2004) (\link{cor})
#'  \item Pearson: Pearson's product moment correlation
#'    test arccording to Yu et al. (2004) (\link{cor})
#'  \item LogReg: beta-Values of logistic regression
#'    (\link{glm})
#'  \item Accuracy//Error-rate randomForest: Error-rate-based
#'    variable importance measure embedded in randomForest
#'    according to Breiman (2001) (\link{randomForest})
#'  \item Gini randomForest: Gini-index-based variable
#'    importance measure embedded in randomForest according
#'    to Breiman (2001) (\link{randomForest})
#'  \item Error-rate cforest: Error-rate-based variable
#'    importance measure embedded in cforest according
#'    Strobl et al. (2009) (\link{cforest})
#'  \item AUC cforest: AUC-based variable importance measure
#'    embedded in cforest according to Janitza et al. (2013)
#'    (\link{cforest})}
#'    By the argument \code{selection} the user decides which feature selection methods are used in \code{ensemble_fs}. 
#'    Default value is \code{selection = c(TRUE, TRUE, TRUE,TRUE, TRUE, TRUE, FALSE, FALSE)}, 
#'    i.e., the function does not use either of the cforest variable importance measures.
#'    The maximum score for features depends on the input of \code{selection}.
#'    The scores are always divided through the amount of selected feature selection, respectively the amount of TRUEs. 
#'
#'@param data an object of class data.frame
#'@param classnumber a number indicating the index of variable for binary classification
#'@param NA_threshold a number in range of [0,1]. Threshold for deletion
#'  of features with a greater proportion of NAs than \code{NA_threshold}.
#'@param cor_threshold a number used only for Spearman and Pearson correlation. Correlation threshold within features.
#'  If the correlation of 2 features is greater than \code{cor_threshold} the dependent feature is deleted.
#'@param runs a number used only for randomForest and cforest. Amount of runs to gain higher robustness.
#'@param selection a vector of length eight with TRUE or FALSE values. Selection of feature selection methods to be conducted.
#'@return table of normalized importance values of class matrix
#'  (used methods as rows and features of the imported file as columns).
#'@author Ursula Neumann
#'@references
#'  \itemize{
#'  \item Yu, L. and Liu H.: Efficient feature selection via
#'    analysis of relevance and redundancy. J. Mach. Learn.
#'    Res. 2004, 5:1205-1224. \cr
#'	\item Breiman, L.: Random Forests, Machine Learning.
#'    2001, 45(1): 5-32. \cr
#'	\item Strobl, C., Malley, J. anpercentaged Tutz, G.: An
#'    Introduction to Recursive Partitioning: Rationale,
#'    Application, and Characteristics of Classification and
#'    Regression Trees, Bagging, and Random forests.
#'    Psychological Methods. 2009, 14(4), 323–348. \cr
#'  \item	Janitza, S., Strobl, C. and Boulesteix AL.: An
#'    AUC-based Permutation Variable Importance Measure for
#'    Random Forests. BMC Bioinformatics.2013, 14, 119. \cr
#'}
#'@examples
#'  ## Loading dataset in environment
#'  data(efsdata)
#'  ## Generate a ranking based on importance (with default NA_threshold = 0.2,
#'  ## cor_threshold = 0.7, selection = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
#'  efs <- ensemble_fs(efsdata, 5, runs=2)
#'@seealso \link{wilcox.test},
#'  \link[randomForest]{randomForest},
#'  \link[party]{cforest},
#'  \link[stats]{cor},
#'  \link[stats]{glm}
#'@importFrom randomForest randomForest
#'@importFrom party cforest_control cforest varimp varimpAUC
#'@export

ensemble_fs <- function(data, classnumber,
                        NA_threshold = 0.2, 
                        cor_threshold = 0.7, runs = 100, 
                        selection = c(TRUE, TRUE, TRUE,TRUE, TRUE, TRUE, FALSE, FALSE))
  {
  start.time <- Sys.time()

  if(missing(data))
    stop("data argument missing")
  if(missing(classnumber))
    stop("classnumber argument missing")
  if(missing(NA_threshold)){
    NA_threshold=0.2
    print("default value for NA_threshold = 0.2")}
  if(missing(cor_threshold)){
    cor_threshold=0.7
    print("default value for cor_threshold = 0.7")}
  if(missing(runs)){
    runs=100
    print("default value for runs = 100")}
  if(missing(selection)){
    selection = c(TRUE, TRUE, TRUE,TRUE, TRUE, TRUE, FALSE, FALSE)
    print("default value for selection is c(TRUE, TRUE, TRUE,TRUE, TRUE, TRUE, FALSE, FALSE)")}
  if(!is.numeric(NA_threshold) | NA_threshold > 1 |
     NA_threshold < 0)
    stop("invalid argument:
         NA_threshold is required to be in [0,1]")
  if(!is.numeric(cor_threshold) | cor_threshold > 1 |
     cor_threshold < 0)
    stop("invalid argument:
         cor_threshold is required to be in [0,1]")

  classname = colnames(data)[classnumber]

  # Löschen von Parametern mit zu vielen NA
  NrNA= c()
  for(i in 1:length(data[1,])){
    NrNA[i] = length(which(is.na(data[,i])))
  }
  NmNA = which(NrNA/length(data[,1])>NA_threshold)
  if(length(NmNA) != 0) data=data[,-NmNA]

  data = na.omit(data, row.names=F)

  # oder nur Nullen oder Varianz gleich Null
  data=data[,which(colSums(data,na.rm=F)!=0 &
                     apply(data, 2, var)!=0)]

  klasse = data[,classname]
  # teste auf binäre Klasse
  is.binary <- function(v) {
    x <- unique(v)
    length(x) == 2L && all(x==1 | x==0)
  }
  if(!is.binary(klasse)){
    stop("Class not binary with classlabels 0 and 1")
  }

  clnr= which(colnames(data)==classname)
  data = data[,-clnr]
  norm=length(data[1,])

  #median filter

  if(selection[1]==TRUE)
  {
  print('Start Median')
  positives = which(klasse==1)
  negatives = which(klasse==0)

  data.pos = data[positives,]
  data.neg = data[negatives,]


  f <- function(x,y){
    test <- wilcox.test(x,y)
    data.frame(pval = test$p.value)
  }
  wtest=sapply(colnames(data), function(x) f(data.pos[,x],
                                             data.neg[,x]))

  pval <- unlist(wtest, use.names=FALSE)
  imp = cbind(1:length(pval),  1-pval+min(pval) )
  imp = imp[order(imp[,2], decreasing = T),]
  rank1 = imp[,1]


  imp1 = imp[,2]

  if(length(imp1)>norm){
    imp1 = imp1[1:norm]
  }
  } else {
    imp1=rep(0,length(data[1,]))
    rank1 = seq(1:length(data[1,]))
  }

  #correlation filter

  cor.filter <- function(data, threshold, method){

    #catch missing or bad input

    if(!is.numeric(threshold) | threshold > 1 |
       threshold < 0)
      stop("invalid argument:
           threshold is required to be in [0,1]")

    #analysis

    data.tmp = data
    features = c()

    klasse = data.tmp[,1]
    data.tmp = data.tmp[,-1]
    colnames(data.tmp) = seq(1,length(data.tmp[1,]))

    vars = apply(data.tmp, 2, var)
    dels = which(vars == 0)
    if(length(dels) > 0){
      data.tmp = data.tmp[,-dels]
    }

    names = colnames(data.tmp)

    for(k in 1:norm){
      #höchste Korrelation mit Class label

      c.matrix = cor(cbind(klasse, data.tmp), method=method)

      # Betrag der ersten Spalte ohne erste Zeile
      #korrelation mit class
      cor.class = abs(as.vector(c.matrix[1,-1]))

      #transponieren und als mit
      #names = seq(1, length(data.temp[1,])) versehen

      imp = t(as.matrix(cor.class))
      best = names[which.max(as.vector(imp[1,]))]

      features = c(features, best)

      #########

      c.matrix = c.matrix[-1,-1]
      tmp = abs(c.matrix[best,])

      w = which(tmp > threshold)


      data.tmp = data.tmp[,-w]

      names = names[-w]

      if(is.null(dim(data.tmp))){
        features = as.numeric(features)
        return(features)
      }
      colnames(data.tmp) = names
    }
    return(features)
  }

  # Pearson correlation
  if(selection[2]==TRUE){
  print('Start Pearson')
  rank2 = cor.filter(as.matrix(cbind(klasse, data)),
                     cor_threshold, "pearson")
  if(length(rank2)>norm){
    rank2 = rank2[1:norm]
  }


  c.matrix = cor(cbind(klasse, data), method="pearson")
  cor.class = abs(as.vector(c.matrix[1,-1]))
  imp = t(as.matrix(cor.class))
  imp=as.vector(imp)
  imp= imp[rank2]
  imp2 = imp/max(imp)

  #dependent variables omitted
  #refill vector to length = norm
  if(length(imp2)>norm){
    imp2 = imp2[1:norm]
  }

  if(length(rank2)<norm){
    rest=setdiff(c(1:norm),rank2)
    k=length(rest)
    rank2=c(rank2,rest)
    imp2=c(imp2,rep(0,k))
  }
  } else {
    imp2=rep(0,length(data[1,]))
    rank2 = seq(1:length(data[1,]))
  }
  
  # Spearman correlation
  if(selection[3]==TRUE){
  print('Start Spearman')
  rank3 = cor.filter(as.matrix(cbind(klasse, data)),
                     cor_threshold, "spearman")
  if(length(rank3)>norm){
    rank3 = rank3[1:norm]
  }

  c.matrix = cor(cbind(klasse, data), method="spearman")
  cor.class = abs(as.vector(c.matrix[1,-1]))
  imp = t(as.matrix(cor.class))
  imp=as.vector(imp)
  imp= imp[rank3]
  imp3 = imp/max(imp)

  #dependent variables omitted
  #refill vector to length = norm
  if(length(imp3)>norm){
    imp3 = imp3[1:norm]
  }

  if(length(rank3)<norm){
    res=setdiff(c(1:norm),rank3)
    l=length(res)
    rank3=c(rank3,res)
    imp3=c(imp3,rep(0,l))
  }
  } else {
    imp3=rep(0,length(data[1,]))
    rank3 = seq(1:length(data[1,]))
  }

  #logistic regression
  if(selection[4]==TRUE){ 
  print('Start LogReg')
  imp_lg <- function(dataf){
    # Z-Transformation:
    dataf=data
    for (i in 1:length(dataf[1,])){
      dataf[,i]=(dataf[,i]-mean(dataf[,i]))/var(dataf[,i])
    }
    klassef = (klasse-mean(klasse))/var(klasse)

    model.imp = glm(as.factor(klassef)~., data = dataf,
                    family = binomial(link = "logit"), maxit=100)
    corri = abs(as.vector(model.imp$coefficients))
    corri=corri[-1]
    corri=na.omit(corri)
    imp=c()
    imp = cbind(1:length(corri),  (corri*1)/max(corri))
    imp = imp[order(imp[,2], decreasing = TRUE),]
    rank4 = imp[,1]
    imp4 = imp[,2]

    #NA omitted - refill vector to length = norm
    if(length(rank4)<norm){
      k=norm-length(rank4)
      rest=setdiff(c(1:norm),rank4)
      rank4=c(rank4,rest)
      imp4=c(imp4,rep(0,k))}

    if(length(imp4)>norm){
      imp4 = imp4[1:norm]
    }
    impra4 = cbind(imp4,rank4)
    return(impra4)
  }
  impra4 = imp_lg(dataf=data)

  } else {
    imp4=rep(0,length(data[1,]))
    rank4 = seq(1:length(data[1,]))
    impra4 = cbind(imp4,rank4) 
  }
  
  
  # RandomForest - Breiman
  if(selection[5]==TRUE | selection[6]==TRUE){
  print('Start RF')
  imp_rf <- function(runs,classnumber){

    data.frame= cbind(data,klasse)

    imp_accuracy = c()
    imp_Gini = c()

    for(i in 1:runs){
      print(i)
      start.run <- Sys.time()
      rf =randomForest(as.factor(klasse)~.,
                       data=data.frame,importance=TRUE,
                       replace=FALSE,ntree=1000)
      imp_accuracy = cbind(imp_accuracy,rf$importance[,3])
      imp_Gini = cbind(imp_Gini,rf$importance[,4])
      end.run <- Sys.time()
      diff <- end.run - start.run
      print(diff)
    }

    # Mitteln der runs
    imp_Gini_mean = c()
    imp_Gini_mean = rowMeans(imp_Gini)
    imp_accuracy_mean = c()
    imp_accuracy_mean = rowMeans(imp_accuracy)

    #Ranking
    imp= rbind(abs(imp_accuracy_mean),abs(imp_Gini_mean))
    rank_RF1=cbind(1:length(imp[1,]),as.vector(imp[1,]))
    rank_RF1=rank_RF1[order(rank_RF1[,2], decreasing = T),]
    imp_RF1 =rank_RF1[,2]
    rank_RF1=rank_RF1[,1]
    rank_RF2=cbind(1:length(imp[1,]),as.vector(imp[2,]))
    rank_RF2=rank_RF2[order(rank_RF2[,2], decreasing = T),]
    imp_RF2 =rank_RF2[,2]
    rank_RF2=rank_RF2[,1]
    #Normieren
    imp_RF1 = imp_RF1/max(imp_RF1)
    imp_RF2 = imp_RF2/max(imp_RF2)
    imp_RF=cbind(imp_RF1,rank_RF1,imp_RF2,rank_RF2)

    return(imp_RF)
  }
  imp_RF=imp_rf(runs, classnumber)
  } else {
    imp_RF = matrix(0, nrow = length(data[1,]), ncol=4)
    imp_RF[,1] = rep(0,length(data[1,]))
    imp_RF[,2] = seq(1:length(data[1,]))
    imp_RF[,3] = rep(0,length(data[1,]))
    imp_RF[,4] = seq(1:length(data[1,]))
  }
  if(selection[5]==FALSE){
    imp_RF[,1] = rep(0,length(data[1,]))
    imp_RF[,2] = seq(1:length(data[1,]))
  }
  if(selection[6]==FALSE){
    imp_RF[,3] = rep(0,length(data[1,]))
    imp_RF[,4] = seq(1:length(data[1,]))
  }
  
  # RandomForest - Conditional
  if(selection[7]==TRUE | selection[8]==TRUE){
  print('Start CF')
  imp_cf <- function(runs,classnumber){
    imp = c()
    AUC_imp = c()
    data.frame= cbind(data,klasse)

    for(i in 1:runs){
      print(i)
      start.run <- Sys.time()
      #mincriterion = 0 means p-value can be of arbitrary size
      cf_controls = cforest_control(mincriterion = 0,
                                    ntree = 1000,
                                    mtry = 3,
                                    replace = TRUE)
      cf=party::cforest(klasse~., data=data.frame,
                        controls=cf_controls)
      # Importance und AUC_Importance
      imp=cbind(imp, varimp(cf))
      AUC_imp=cbind(AUC_imp, varimpAUC(cf))
      end.run <- Sys.time()
      diff <- end.run - start.run
      print(diff)
    }

    # Mittelwert der Spalten als Vektor
    imp_mean = c()
    AUC_imp_mean = c()
    imp_mean= rowMeans(imp)
    AUC_imp_mean = rowMeans(AUC_imp)
    #Ranking
    imp= rbind(abs(AUC_imp_mean),abs(imp_mean))
    rank_CF1=cbind(1:length(imp[1,]),as.vector(imp[1,]))
    rank_CF1=rank_CF1[order(rank_CF1[,2], decreasing = T),]
    imp_CF1 =rank_CF1[,2]
    rank_CF1=rank_CF1[,1]
    rank_CF2=cbind(1:length(imp[1,]),as.vector(imp[2,]))
    rank_CF2=rank_CF2[order(rank_CF2[,2], decreasing = T),]
    imp_CF2 =rank_CF2[,2]
    rank_CF2=rank_CF2[,1]
    #Normieren
    imp_CF1 = abs(imp_CF1)
    imp_CF2 = abs(imp_CF2)
    imp_CF1 = imp_CF1/max(imp_CF1)
    imp_CF2 = imp_CF2/max(imp_CF2)
    imp_CF=cbind(imp_CF1,rank_CF1,imp_CF2,rank_CF2)

    return(imp_CF)
  }
  
  imp_CF=imp_cf(runs, classnumber)
  
  } else {
    imp_CF = matrix(0, nrow = length(data[1,]), ncol=4)
    imp_CF[,1] = rep(0,length(data[1,]))
    imp_CF[,2] = seq(1:length(data[1,]))
    imp_CF[,3] = rep(0,length(data[1,]))
    imp_CF[,4] = seq(1:length(data[1,]))
  }
  if(selection[7]==FALSE){
    imp_CF[,1] = rep(0,length(data[1,]))
    imp_CF[,2] = seq(1:length(data[1,]))
  }
  if(selection[8]==FALSE){
    imp_CF[,3] = rep(0,length(data[1,]))
    imp_CF[,4] = seq(1:length(data[1,]))
  }
  

  
  print("Build return matrix")
  # Output generieren
  ER_RF = imp_RF[,1][order(imp_RF[,2], decreasing = F)]
  Gini_RF = imp_RF[,3][order(imp_RF[,4], decreasing = F)]
  AUC_CF = imp_CF[,1][order(imp_CF[,2], decreasing = F)]
  ER_CF =imp_CF[,3][order(imp_CF[,4], decreasing = F)]
  LogReg = impra4[,1][order(impra4[,2], decreasing = F)]
  P_cor = imp2[order(rank2, decreasing = F)]
  S_cor = imp3[order(rank3, decreasing = F)]
  Median= imp1[order(rank1, decreasing = F)]
  table=rbind(Median,P_cor, S_cor, LogReg, ER_RF, Gini_RF, AUC_CF, ER_CF)
  number = length(grep("TRUE",selection))
  table = table/number
  colnames(table)=colnames(data)

  print("Done")
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  return(table)
}
