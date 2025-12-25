######################## Causes of Outcome Learning #############################
######################## Version 30.10.2020

Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)
Sys.setenv('_R_CHECK_FORCE_SUGGESTS_' = FALSE)


########## Minor functions ############

#' Function used as part of other functions
#'
#' Function used as part of other functions
#'
#' @param r rows in matrix
#' @param c columns in matrix
#' @export

random <- function(r,c) {
  w1 <- matrix(NA,nrow = r, ncol = c)
  w1 <- sapply(w1,function(x){rgamma(1,1,100)})
  w1 <- matrix(w1,nrow = r, ncol = c)
  return(w1)
}

#' Function used as part of other functions
#'
#' Function used as part of other functions
#'
#' @param input input in the relu function
#' @export

relu <- function(input) {
  #  return(ifelse(input<0,0,input))
  return((input>0)*input)
}


########## Wrappers ##############


#' CoOL working example with sex, drug A, and drug B
#'
#' To reproduce the CoOL working example with sex, drug A, and drug B.
#'
#' @param n number of observations for the synthetic data.
#' @return A data frame with the columns Y, sex, drug_a, drug_b and rows equal to n.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' 	while (FALSE) {
#'  library(CoOL)
#'  set.seed(1)
#'  data <- CoOL_0_working_example(n=10000)
#'  outcome_data <- data[,1]
#'  exposure_data <- data[,-1]
#'  exposure_data <- CoOL_0_binary_encode_exposure_data(exposure_data)
#'  model <- CoOL_1_initiate_neural_network(inputs=ncol(exposure_data),
#'  output = outcome_data,hidden=5)
#'  model <- CoOL_2_train_neural_network(lr = 1e-4,X_train=exposure_data,
#'  Y_train=outcome_data,X_test=exposure_data, Y_test=outcome_data,
#'  model=model, epochs=1000,patience = 200, input_parameter_reg = 1e-3
#'  ) # Train the non-negative model (The model can be retrained)
#'  model <- CoOL_2_train_neural_network(lr = 1e-5,X_train=exposure_data,
#'  Y_train=outcome_data,X_test=exposure_data, Y_test=outcome_data, model=model,
#'  epochs=1000,patience = 100, input_parameter_reg = 1e-3)
#'  # Train the non-negative model (The model can be retrained)
#'  model <- CoOL_2_train_neural_network(lr = 1e-6,X_train=exposure_data,
#'  Y_train=outcome_data,X_test=exposure_data, Y_test=outcome_data, model=model,
#'  epochs=1000,patience = 50, input_parameter_reg = 1e-3
#'  ) # Train the non-negative model (The model can be retrained)
#'  plot(model$train_performance,type='l',yaxs='i',ylab="Mean squared error",
#'  xlab="Epochs",main="A) Performance during training\n\n",
#'  ylim=quantile(model$train_performance,c(0,.975))) # Model performance
#'  CoOL_3_plot_neural_network(model,names(exposure_data),5/max(model[[1]]),
#'  title = "B) Model connection weights\nand intercepts") # Model visualization
#'  CoOL_4_AUC(outcome_data,exposure_data,model,
#'  title = "C) Receiver operating\ncharacteristic curve") # AUC
#'  risk_contributions <- CoOL_5_layerwise_relevance_propagation(exposure_data,model
#'  ) # Risk contributions
#'  CoOL_6_number_of_sub_groups(risk_contributions = risk_contributions,
#'  low_number = 1, high_number = 5)
#'  CoOL_6_dendrogram(risk_contributions,number_of_subgroups = 3,
#'  title = "D) Dendrogram with 3 sub-groups") # Dendrogram
#'  sub_groups <- CoOL_6_sub_groups(risk_contributions,number_of_subgroups = 3
#'  ) # Assign sub-groups
#'  CoOL_6_calibration_plot(exposure_data = exposure_data,
#'  outcome_data = outcome_data, model = model, sub_groups = sub_groups)
#'  CoOL_7_prevalence_and_mean_risk_plot(risk_contributions,sub_groups,
#'  title = "E) Prevalence and mean risk of sub-groups") # Prevalence and mean risk plot
#'  results <- CoOL_8_mean_risk_contributions_by_sub_group(risk_contributions,
#'  sub_groups,outcome_data = outcome_data,exposure_data = exposure_data,
#'  model=model,exclude_below = 0.01) #  Mean risk contributions by sub-groups
#' 	CoOL_9_visualised_mean_risk_contributions(results = results,  sub_groups = sub_groups)
#' 	CoOL_9_visualised_mean_risk_contributions_legend(results = results)
#' 	}

CoOL_0_working_example <- function(n) {
  drug_a = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)
  sex = sample(1:0,n,prob=c(0.5,0.5),replace=TRUE)
  drug_b = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)
  Y <-  sample(1:0,n,prob=c(0.05,0.95),replace = TRUE)
  for (i in 1:n) {
    if (sex[i] == 0 & drug_a[i] == 1 & sample(1:0,1,prob=c(.15,0.8)) ) {
      Y[i] <- 1
    }
    if (sex[i] == 1 & drug_b[i] == 1 & sample(1:0,1,prob=c(.15,0.85)) ) {
      Y[i] <- 1
    }
  }
  data <- data.frame(Y,sex,drug_a,drug_b) #,C)
  for (i in 1:ncol(data))   data[,i] <- as.numeric(data[,i])
  return(data)
}



#' Binary encode exposure data
#'
#' This function binary encodes the exposure data set so that each category is coded 0 and 1 (e.g. the variable sex will be two variables men (1/0) and women (0/1)).
#'
#' @param exposure_data The exposure data set.
#' @return Data frame with the expanded exposure data, where all variables are binary encoded.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example
#'

CoOL_0_binary_encode_exposure_data <- function(exposure_data) {
  for (i in 1:ncol(exposure_data)) {exposure_data[,i] <- factor(exposure_data[,i])}
  exposure_data <- mltools::one_hot(data.table::as.data.table(exposure_data))
  exposure_data <- as.data.frame(exposure_data)
  for (i in 1:ncol(exposure_data)) {exposure_data[,i] <- as.numeric(exposure_data[,i])}
  return(exposure_data)
}




#' Initiates a non-negative neural network
#'
#' This function initiates a non-negative neural network. The one-hidden layer non-negative neural network is designed to resemble a DAG with hidden synergistic components. With the model, we intend to learn the various synergistic interactions between the exposures and outcome. The model needs to be non-negative and estimate the risk on an additive scale. Neural networks include hidden activation functions (if the sum of the input exceeds a threshold, information is passed on), which can model minimum threshold values of interactions between exposures. We need to specify the upper limit of the number of possible hidden activation functions and through model fitting, the model may be able to learn both stand-alone and synergistically interacting factors.
#'
#' @param inputs The number of exposures.
#' @param output The outbut variable is used to calcualte the mean of it used to initiate the baseline risk.
#' @param hidden Number of hidden nodes.
#' @return A list with connection weights, bias weights and meta data.
#' @details The non-negative neural network can be denoted as:
#' \deqn{
#' P(Y=1|X^+)=\sum_{j}\Big(w_{j,k}^+ReLU_j\big(\sum_{i}(w_{i,j}^+X_i^+) + b_j^-\big)\Big) + R^{b}
#' }
#'
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example
#'


CoOL_1_initiate_neural_network <- function(inputs,output,hidden=10) {
  # Weight initiation
  w1 <- abs(random(inputs,hidden))
  b1 <- -abs(random(1,hidden))
  w2 <- matrix(1,nrow=hidden)
  b2 <- mean(output)
  c2 <- abs(random(1,1))
  performance <- NA
  best_epoch <- NA
  weight_performance <- NA
  epochs <- NA
  b2 <- as.matrix(mean(output))
  return(list(w1,b1,w2,b2,c2,performance,epochs,best_epoch))
}








#' Training the non-negative neural network
#'
#' This function trains the non-negative neural network. Fitting the model is done in a step-wise procedure one individual at a time, where the model estimates individual's risk of the disease outcome, estimates the prediction's residual error and adjusts the model parameters to reduce this error. By iterating through all individuals for multiple epochs (one complete iterations through all individuals is called an epoch), we end with parameters for the model, where the errors are smallest possible for the full population. The model fit follows the linear expectation that synergism is a combined effect larger than the sum of independent effects. The initial values, derivatives, and learning rates are described in further detail in the Supplementary material. The non-negative model ensures that the predicted value cannot be negative. The model does not prevent estimating probabilities above 1, but this would be unlikely, as risks of disease and mortality even for high risk groups in general are far below 1. The use of a test dataset does not seem to assist deciding on the optimal number of epochs possibly due to the constrains due to the non-negative assumption. We suggest splitting data into a train and test data set, such that findings from the train data set can be confirmed in the test data set before developing hypotheses.
#'
#' @param X_train The exposure data for the training data.
#' @param Y_train The outcome data for the training data.
#' @param C_train One variable to adjust the analysis for such as calendar time (training data).
#' @param X_test The exposure data for the test data (currently the training data is used).
#' @param Y_test The outcome data for the test data (currently the training data is used).
#' @param C_test One variable to adjust the analysis for such as calendar time (currently the training data is used).
#' @param model The fitted non-negative neural network.
#' @param lr Learning rate (several LR can be provided, such that the model training will train for each LR and continue to the next).
#' @param epochs Epochs.
#' @param patience The number of epochs allowed without an improvement in performance.
#' @param ipw a vector of weights per observation to allow for inverse probability of censoring weighting to correct for selection bias
#' @param monitor Whether a monitoring plot will be shown during training.
#' @param plot_and_evaluation_frequency The interval for plotting the performance and checking the patience.
#' @param input_parameter_reg Regularisation decreasing parameter value at each iteration for the input parameters.
#' @param spline_df Degrees of freedom for the spline fit for the performance plots.
#' @param restore_par_options Restore par options.
#' @param drop_out To drop connections if their weights reaches zero.
#' @param fix_baseline_risk To fix the baseline risk at a value.
#' @return An updated list of connection weights, bias weights and meta data.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example


CoOL_2_train_neural_network <- function(X_train, Y_train, X_test, Y_test, C_train=0, C_test=0, model, lr = c(1e-4,1e-5,1e-6),
                            epochs = 2000, patience = 100,monitor = TRUE,
                            plot_and_evaluation_frequency = 50, input_parameter_reg = 1e-3, spline_df=10, restore_par_options = TRUE, drop_out = 0, fix_baseline_risk = -1,
                            ipw = 1) {
if (restore_par_options==TRUE) {
    oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
}
if (mean(as.vector(X_test == X_train))!=1) print("Traning data and test data are not equivalent. It is recommended for CoOL that the model is fully trained on the training data but manual control is conducted on a test data set.")
if (mean(as.vector(Y_test == Y_train))!=1) print("Traning outcomes and test outcomes are not equivalent. It is recommended for CoOL that the model is fully trained on the training data but manual control is conducted on a test data set.")
if (mean(as.vector(C_test == C_train))!=1) print("Confounder data in the training and test arguments are not equivalent. It is recommended for CoOL that the model is fully trained on the training data but manual control is conducted on a test data set.")
if (length(ipw) != nrow(X_train)) {
  ipw = rep(1,nrow(X_train))
  print("Equal weights are applied (assuming no selection bias)")
}
if (length(C_train) != nrow(X_train)) {
  C_train = rep(0,nrow(X_train))
  C_test = rep(0,nrow(X_train))
    print("Not adjusting for calendar time")
  model[[5]] = matrix(0) # To illustrate that there is no adjustment for calendar time
}
for (lr_set in lr) {
  print(paste0("############################## Learning rate: ",lr_set," ##############################"))
  performance = model$train_performance
  performance_test = model$test_performance
  weight_performance = model$weight_performance
  baseline_risk_monitor = model$baseline_risk_monitor
  par(mfrow=c(1,3));par(mar=c(3,5,3,1))
    for(rounds in 1:ceiling(c(epochs/plot_and_evaluation_frequency))) {
      model <- cpp_train_network_relu(x=as.matrix(X_train),y=as.matrix(Y_train),testx=as.matrix(X_test),testy=as.matrix(Y_test),
                                      c=as.matrix(C_train),testc = as.matrix(C_test),
              lr = lr_set, maxepochs  = plot_and_evaluation_frequency, W1_input = model[[1]],B1_input = model[[2]],
              W2_input = model[[3]],B2_input = model[[4]],C2_input = model[[5]],input_parameter_reg=input_parameter_reg,drop_out=drop_out,fix_baseline_risk=fix_baseline_risk,
              ipw=ipw)
      performance <- c(performance,model$train_performance)
      performance_test <- c(performance_test,model$test_performance)
      weight_performance <- c(weight_performance,model$weight_performance)
      baseline_risk_monitor <- c(baseline_risk_monitor,model$baseline_risk_monitor)
      if (monitor == TRUE){
      plot(performance, type='l',yaxs='i', ylab="Mean squared error",
           xlab="Epochs",main="Performance on training data set",ylim=c(min(performance),stats::quantile(performance,c(.9))))
      points(smooth.spline(performance, df = spline_df),col="red",type='l',lwd=2)
#      plot(performance_test, type='l',yaxs='i', ylab="Mean squared error",
#           xlab="Epochs",main="Performance on test data set")
     plot(log(weight_performance), type='l', ylab="log of mean squared weight difference",
          xlab="Epochs",main="Log mean squared weight difference")
     points(smooth.spline(log(weight_performance)[is.infinite(log(weight_performance))==FALSE], df = spline_df),col="red",type='l',lwd=2)
     plot(baseline_risk_monitor,type='l', main="Estimated baseline risk by epoch")
     abline(h=mean(Y_train),lty=2)
     points(smooth.spline(baseline_risk_monitor, df = spline_df),col="red",type='l',lwd=2)
      if(length(performance)-which.min(performance)>patience) break
    }}
  model$train_performance <- c(performance)
  model$test_performance <-  c(performance_test)
  model$weight_performance <-  c(weight_performance)
  model$baseline_risk_monitor <- c(baseline_risk_monitor)
  model$epochs = epochs
}
  par(mfrow=c(1,1))
  return(model)
}




#' Plotting the non-negative neural network
#'
#' This function plots the non-negative neural network
#'
#' @param model The fitted non-negative neural network.
#' @param names Labels of each exposure.
#' @param title Title on the plot.
#' @param arrow_size Define the arrow_size for the model illustration in the reported training progress.
#' @param restore_par_options Restore par options.
#' @return A plot visualizing the connection weights.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_3_plot_neural_network <- function(model,names,arrow_size = NA,
                       title = "Model connection weights and intercepts", restore_par_options = TRUE) {
   if (restore_par_options==TRUE) {
     oldpar <- par(no.readonly = TRUE)
     on.exit(par(oldpar))
   }
  par(mar=c(0,0,3,0))
  if (is.na(arrow_size)) arrow_size = 5/max(model[[1]])
  plot(0,0,type='n',xlim=c(0,4),ylim=c(-max(nrow(model[[1]]),nrow(model[[3]]))-1,0),axes=FALSE,ylab="",xlab="",main=title)
  #abline(h=0)
  #points(rep(1,nrow(model[[1]])),-c(1:nrow(model[[1]])),cex=10)
  #points(rep(2,ncol(model[[1]])),-c(1:ncol(model[[1]])),cex=10)
  #points(3,-(ncol(model[[1]])+1)/2,cex=10)
  # Static edges first in grey
  for (g in 1:nrow(model[[3]])) {
    arrows(x0=2,x1=3,y0=-g,y1= -(ncol(model[[1]])+1)/2,lwd=abs(model[[3]][g,1])*5,col=ifelse(model[[3]][g,1]>=0,"lightgrey","red"),length=0)
    #   text(2,-g,round(model[[3]][g,1],2),pos=3)
  }
  # Trained edges
  for (g in 1:nrow(model[[1]])) {
    for (h in 1:ncol(model[[1]])) {
      arrows(x0=1,x1=2,y0=-g,y1=-h,lwd=abs(model[[1]][g,h])*arrow_size,col=ifelse(model[[1]][g,h]>0,adjustcolor("dodgerblue",1),adjustcolor("dodgerblue",0)),length=0)
      #      text(1,-g,round(model[[1]][g,h],2),pos=3)
    }
  }
  for (i in 1:nrow(model[[1]])) {
    text(rep(1,nrow(model[[1]]))[i],-c(1:nrow(model[[1]]))[i],names[i],pos=2)
  }
  text(rep(2,ncol(model[[1]])),-c(1:ncol(model[[1]])),paste0("a=",round(model[[2]][1,],2)),pos=3)
  text(3,-(ncol(model[[1]])+1)/2,bquote(R^"b+"==.(round(model[[4]][1,1],2))),pos=1)
  par(mar=c(5.1,4.1,4.1,2.1))
  #  points(3,-(ncol(model[[1]])+1)/2+1,cex=10)
  #  arrows(x0=3,x1=3,y0=-(ncol(model[[1]])+1)/2+1,y1= -(ncol(model[[1]])+1)/2,lwd=abs(model[[5]][1,1])*arrow_size,col=ifelse(model[[5]][1,1]>0,"green","white"),length=0)
}


#' Predict the risk of the outcome using the fitted non-negative neural network
#'
#' Predict the risk of the outcome using the fitted non-negative neural network.
#'
#' @param X The exposure data.
#' @param model The fitted the non-negative neural network.
#' @return A vector with the predicted risk of the outcome for each individual.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example


CoOL_4_predict_risks <- function(X,model) {
  H <- relu(t(t(as.matrix(X) %*% as.matrix(model[[1]])) + as.vector(model[[2]])))
  o = relu(as.vector(H %*% model[[3]][,1] + as.vector(model[[4]][1,1])))
  if(max(o)>1) print("Warning: Some predicted risks are above 1")
  return(o)
}


#' Plot the ROC AUC
#'
#' Plot the ROC AUC
#'
#' @param exposure_data The exposure data.
#' @param outcome_data The outcome data.
#' @param model The fitted the non-negative neural network.
#' @param title Title on the plot.
#' @param restore_par_options Restore par options.
#' @return A plot of the ROC and the ROC AUC value.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_4_AUC <- function(outcome_data,exposure_data,model,title="Receiver operating\ncharacteristic curve", restore_par_options = TRUE) {
  if (restore_par_options==TRUE) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }
  par(mar=c(0,0,3,0))
  pred <- CoOL_4_predict_risks(exposure_data,model)
  plot(pROC::roc(outcome_data,pred),print.auc=TRUE,main=title)
}




#' Layer-wise relevance propagation of the fitted non-negative neural network
#'
#' Calculates risk contributions for each exposure and a baseline using layer-wise relevance propagation of the fitted non-negative neural network and data.
#'
#' @param X The exposure data.
#' @param model The fitted the non-negative neural network.
#' @details
#' For each individual:\deqn{
#' P(Y=1|X^+)=R^b+\sum_iR^X_i
#' }
#' The below procedure is conducted for all individuals in a one by one fashion. The baseline risk, $R^b$, is simply parameterised in the model. The decomposition of the risk contributions for exposures, $R^X_i$, takes 3 steps:
#'
#' Step 1 - Subtract the baseline risk, $R^b$:
#' \deqn{
#' R^X_k =  P(Y=1|X^+)-R^b
#' }
#' Step 2 - Decompose to the hidden layer:
#' \deqn{
#' R^{X}_j =  \frac{H_j w_{j,k}}{\sum_j(H_j w_{j,k})} R^X_k
#' }
#' Where $H_j$ is the value taken by each of the $ReLU()_j$ functions for the specific individual.
#'
#' Step 3 - Hidden layer to exposures:
#' \deqn{
#' R^{X}_i = \sum_j \Big(\frac{X_i^+ w_{i,j}}{\sum_i( X_i^+ w_{i,j})}R^X_j\Big)
#' }
#' This creates a dataset with the dimensions equal to the number of individuals times the number of exposures plus a baseline risk value, which can be termed a risk contribution matrix. Instead of exposure values, individuals are given risk contributions, R^X_i.
#'
#' @return A data frame with the risk contribution matrix [number of individuals, risk contributors + the baseline risk].
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example


CoOL_5_layerwise_relevance_propagation <- function(X,model) {
  labels <- colnames(X)
  X = as.matrix(X)

  # Forward
  R_X <- matrix(0,ncol=ncol(X),nrow=nrow(X))
  U_B = NULL

  H_all <- relu(t(t(as.matrix(X) %*% as.matrix(model[[1]])) + as.vector(model[[2]])))
  o_all = relu(as.vector(H_all %*% model[[3]][,1] + as.vector(model[[4]][1,1])))

  for (i in 1:nrow(X)) {
    if (i / 1000 == i %/% 1000) {print(i)}
    H <- H_all[i,]
    o <- as.numeric(o_all[i])

    # Layer-wise relevance propagation (LRP)
    Pos1 = model[[3]][,1]
    Pos1 = ifelse(Pos1>0,Pos1,0)
    Pos1_sum = sum(H*Pos1)
    Pos1_sum <- ifelse(is.na(Pos1_sum)|Pos1_sum==0,1,Pos1_sum)
    Pos1 = (H*Pos1)/Pos1_sum

    if(sum(model[[3]]<0)> 0) print("Some weigths are below 0") # ensure none are below 0

    o <- relu(as.numeric(o - relu(relu(model[[4]]) ))) # Subtracting the first U_B
    R_H = Pos1 * o

    Pos2 = model[[1]]
    Pos2 = ifelse(Pos2>0,Pos2,0)
    if(sum(model[[1]]<0)> 0) print("Some weigths are below 0") # ensure none are below 0

    for (g in 1:length(H)) {
      Pos2_sum = sum(X[i,]*Pos2[,g])
      Pos2_sum <- ifelse(is.na(Pos2_sum)|Pos2_sum==0,1,Pos2_sum)
      R_X[i,] = R_X[i,] + (X[i,]*Pos2[,g])/Pos2_sum * R_H[g]
    }

    U_B[i] <- model[[4]]
    if (sum(R_X[i,])==0 | is.na(sum(R_X[i,]))) R_X[i,] = 0
  }


  Baseline_risk <- U_B
  R_X <- data.frame(cbind(R_X,Baseline_risk))
  colnames(R_X) <- c(labels,"Baseline_risk")

  #Sanity check
  if (max(o_all-rowSums(R_X)) > 1e-6) print("WARNING: Some risk contributions do not sum to the predicted value")
  return(R_X)
}




#' Number of subgroups
#'
#' Calculates the mean distance by several number of subgroups to determine the optimal number of subgroups.
#'
#' @param risk_contributions The risk contributions.
#' @param low_number The lowest number of subgroups.
#' @param high_number The highest number of subgroups.
#' @param ipw a vector of weights per observation to allow for inverse probability of censoring weighting to correct for selection bias
#' @param restore_par_options Restore par options.
#' @return A plot of the mean distance by the number of subgroups. The mean distance converges when the optimal number of subgroups are found.
#' @examples
#' #See the example under CoOL_0_working_example


CoOL_6_number_of_sub_groups <- function(risk_contributions, low_number = 1, high_number = 5, ipw = 1, restore_par_options = TRUE) {
    if (restore_par_options==TRUE) {
      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
    }
  mean_dist = NA
  if (length(ipw) != nrow(risk_contributions)) {
    ipw = rep(1, nrow(risk_contributions))
    print("Equal weights are applied (assuming no selection bias)")
  }
  p <- cbind(risk_contributions)
  p$ipw <- ipw
  p <- plyr::count(p, wt_var = "ipw")
  pfreq <- p$freq
  p <- p[, 1:c(ncol(p) - 3)]
  p_h_c <- hclustgeo(dist(p, method = "manhattan"), wt = pfreq)
  for (k in low_number:high_number) {
    pclus <- cutree(p_h_c, k)
    id <- 1:nrow(risk_contributions)
    temp <- merge(cbind(id, risk_contributions), cbind(p, pclus))
    temp <- temp[duplicated(temp) == FALSE, ]
    clusters <- temp$pclus[order(temp$id)]
    print(paste0(k," groups"))
    temp = NA
    N = NA
    for (g in 1:k) {
      temp[g] <- mean(as.matrix(dist(risk_contributions[clusters==g,], method = "manhattan")))
      N[g] <- sum(clusters==g)
    }
    mean_dist[k] <- sum(temp * N) / sum(N)
  }
  par(mfrow=c(1,1));par(mar=c(5,5,1,0))
  print(plot(mean_dist,pch=16,xlab="Sub-groups",ylab="Mean difference in risk contributions",axes=F,type='b')) # x groups
  axis(2);axis(1,1:20,tick = F)
  print("Plot printed")
}




#' Dendrogram and sub-groups
#'
#' Calculates presents a dendrogram coloured by the pre-defined number of sub-groups and provides the vector with sub-groups.
#'
#' @param risk_contributions The risk contributions.
#' @param number_of_subgroups The number of sub-groups chosen (Visual inspection is necessary).
#' @param title The title of the plot.
#' @param colours Colours indicating each sub-group.
#' @param ipw a vector of weights per observation to allow for inverse probability of censoring weighting to correct for selection bias
#' @return A dendrogram illustrating similarities between individuals based on their risk contributions.
#' @examples
#' #See the example under CoOL_0_working_example



CoOL_6_dendrogram <- function(risk_contributions,number_of_subgroups=3, title = "Dendrogram", colours=NA, ipw = 1) {
  requireNamespace("ggtree")
  if (length(ipw) != nrow(risk_contributions)) {
    ipw = rep(1,nrow(risk_contributions))
    print("Equal weights are applied (assuming no selection bias)")
  }
  p <- cbind(risk_contributions)
  p$ipw <- ipw
  p <- plyr::count(p, wt_var = "ipw")
  pfreq <- p$freq
  p <- p[,1:c(ncol(p)-3)]
  p_h_c <- hclustgeo(dist(p,method = "manhattan"), wt=pfreq)
  pclus <- cutree(p_h_c, number_of_subgroups)
  id <- 1:nrow(risk_contributions)
  temp <- merge(cbind(id,risk_contributions),cbind(p,pclus))
  temp <- temp[duplicated(temp)==FALSE,]
  clus <- temp$pclus[order(temp$id)]
  table(clus)
  if (is.na(colours[1])) colours <- c("grey",wes_palette("Darjeeling1"))
  print(ggtree::ggtree(p_h_c,layout="equal_angle") +
          ggtree::geom_tippoint(size=sqrt(pfreq)/2, alpha=.2, color=colours[pclus])+
          ggtitle(title) +
          ggtree::theme(plot.title = element_text(size = 15, face = "bold")))
}



#' Assign sub-groups
#'
#' Calculates presents a dendrogram coloured by the pre-defined number of sub-groups and provides the vector with sub-groups.
#'
#' @param risk_contributions The risk contributions.
#' @param number_of_subgroups The number of sub-groups chosen (Visual inspection is necessary).
#' @param ipw a vector of weights per observation to allow for inverse probability of censoring weighting to correct for selection bias
#' @return A vector [number of individuals] with an assigned sub-group.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_6_sub_groups <- function(risk_contributions,number_of_subgroups=3,ipw=1) {
  if (length(ipw) != nrow(risk_contributions)) {
    ipw = rep(1,nrow(risk_contributions))
    print("Equal weights are applied (assuming no selection bias)")
  }
  p <- cbind(risk_contributions)
  p$ipw <- ipw
  p <- plyr::count(p, wt_var = "ipw")
  pfreq <- p$freq
  p <- p[,1:c(ncol(p)-3)]
  p_h_c <- hclustgeo(dist(p,method = "manhattan"), wt=pfreq)
  pclus <- cutree(p_h_c, number_of_subgroups)
  id <- 1:nrow(risk_contributions)
  x <- data.frame(cbind(id,risk_contributions))
  y <- data.frame(cbind(p,pclus))
  temp <- merge(x,y)
  temp <- temp[duplicated(temp)==FALSE,]
  clus <- temp$pclus[order(temp$id)]
 # reordering
  clus_pred = NA
  for (i in 1:number_of_subgroups) {
    clus_pred[i] <- sum(colMeans(as.matrix(risk_contributions[clus==i,])))
  }
  clus_order <- order(clus_pred)
  clus_new = clus
  index = 1:max(clus)
  for (i in 1:length(clus)) {
    clus_new[i] = index[which(clus_order==clus[i])]
  }
  return(clus_new)
}



#' Calibration curve
#'
#' Shows the calibration curve e.i. the predicted risk vs the actual risk by subgroups.
#'
#' @param exposure_data The exposure dataset.
#' @param outcome_data The outcome vector.
#' @param model The fitted non-negative neural network.
#' @param sub_groups The vector with the assigned sub_group numbers.
#' @param ipw a vector of weights per observation to allow for inverse probability of censoring weighting to correct for selection bias
#' @param restore_par_options Restore par options.
#' @return A calibration curve.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_6_calibration_plot <- function (exposure_data, outcome_data, model, sub_groups, ipw = 1, restore_par_options = TRUE) {
  if (restore_par_options==TRUE) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }
  if (length(ipw) != nrow(exposure_data)) {
    ipw = rep(1, nrow(exposure_data))
    print("Equal weights are applied (assuming no selection bias)")
  }
  c_risk <- NA
  c_pred <- NA
  preds <-  CoOL_4_predict_risks(exposure_data, model)
  for (i in 1:max(sub_groups)) {
    c_pred[i] <- mean(preds[sub_groups==i])
    c_risk[i] <- sum((outcome_data[sub_groups==i] * ipw[sub_groups==i])) / sum(ipw[sub_groups==i])
  }
  par(mfrow=c(1,1));par(mar=c(5,5,1,1))
  plot(c_pred,c_risk,type='n',xlab="Predicted risk in each sub-group",ylab="Actual risk in each sub-group",xlim=c(0,min(c(max(c_pred)*1.2,1))),ylim=c(0,min(c(max(c_risk)*1.2,1))),xaxs='i',yaxs='i',axes=F)
  axis(1);axis(2)
  text(c_pred,c_risk,1:max(sub_groups),cex=1.5)
  abline(0,1)
}





#' Prevalence and mean risk plot
#'
#' This plot shows the prevalence and mean risk for each sub-group. Its distribution hits at sub-groups with great public health potential.
#'
#' @param risk_contributions The risk contributions.
#' @param sub_groups The vector with the sub-groups.
#' @param title The title of the plot.
#' @param y_max Fix the axis of the risk of the outcome.
#' @param restore_par_options Restore par options.
#' @param colours Colours indicating each sub-group.
#' @param ipw a vector of weights per observation to allow for inverse probability of censoring weighting to correct for selection bias
#' @return A plot with prevalence and mean risks by sub-groups.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_7_prevalence_and_mean_risk_plot <- function(risk_contributions,sub_groups,
  title="Prevalence and mean risk\nof sub-groups",y_max = NA, restore_par_options = TRUE, colours=NA, ipw = 1) {
  if (restore_par_options==TRUE) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }
  if (length(ipw) != nrow(risk_contributions)) {
    ipw = rep(1,nrow(risk_contributions))
    print("Equal weights are applied (assuming no selection bias)")
  }
  par(mar=c(5,3,2,2))
  if (is.na(colours[1])) colours <- c("grey",wes_palette("Darjeeling1"))
risk_max = 0
    for (i in 1:max(sub_groups)) {
    risk <- sum(colMeans(as.matrix(risk_contributions[sub_groups==i,])))
    risk_max = max(risk_max,risk)
  }
  plot(0,0,type='n',xlim=c(0,1),ylim=c(0,ifelse(is.na(y_max)==TRUE,risk_max*1.1,y_max)),xaxs='i',yaxs='i',
       axes=FALSE,ylab="Risk",xlab="Prevalence",frame.plot=FALSE,main=title)
  axis(1,seq(0,1,.2));axis(2,seq(0,1,.05))
  rect(0,0,1,1)
  prev0 = 0; total = 0
  for (i in 1:max(sub_groups)) {
    prev <- sum(ipw[sub_groups==i])/sum(ipw)
    risk <- sum(colMeans(as.matrix(risk_contributions[sub_groups==i,])))
    rect(xleft = prev0,ybottom = 0,xright = prev+prev0,ytop = risk, col=colours[i])
    prev0 = prev + prev0
    total = total + risk * prev
  }
  arrows(x0=0,x1=1,y0=mean(risk_contributions$Baseline_risk),lty=2,length=0)
}


#' Mean risk contributions by sub-groups
#'
#' Table with the mean risk contributions by sub-groups.
#'
#' @param risk_contributions The risk contributions.
#' @param sub_groups The vector with the sub-groups.
#' @param exposure_data The exposure data.
#' @param outcome_data The outcome data.
#' @param model The trained non-negative model.
#' @param exclude_below A lower cut-off for which risk contributions shown.
#' @param restore_par_options Restore par options.
#' @param colours Colours indicating each sub-group.
#' @param ipw a vector of weights per observation to allow for inverse probability of censoring weighting to correct for selection bias
#' @return A plot and a dataset with the mean risk contributions by sub-groups.
#' @export
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_8_mean_risk_contributions_by_sub_group <- function(risk_contributions,sub_groups,exposure_data,outcome_data,
                        model,exclude_below=0.001, restore_par_options = TRUE, colours=NA, ipw = 1) {
  if (restore_par_options==TRUE) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }
  if (length(ipw) != nrow(risk_contributions)) {
    ipw = rep(1,nrow(risk_contributions))
    print("Equal weights are applied (assuming no selection bias)")
  }
  if (is.na(colours[1])) colours <- c("grey",wes_palette("Darjeeling1"))
  prev0 = 0; total = 0
  for (i in 1:max(sub_groups)) {
    prev <- sum(ipw[sub_groups==i])/sum(ipw)
    risk <- sum(colMeans(as.matrix(risk_contributions[sub_groups==i,])))
    prev0 = prev + prev0
    total = total + risk * prev
  }
  st <- 1
  d <- data.frame(matrix(NA, nrow=ncol(risk_contributions)))
  for (g in 1:max(sub_groups)) {
    for (i in 1:nrow(d)) {
      d[i,g] <- mean(risk_contributions[sub_groups==g,i])
    }}
  d <- t(d)
  rownames(d) <- paste("Group",1:max(sub_groups))
  colnames(d) <- names(risk_contributions)
  d <- cbind(d[,rev(1:c(ncol(d)-1))],d[,ncol(d)])
  colnames(d)[ncol(d)] <- "Baseline_risk"
  par(mar=c(0,0,0,0))
  plot(0,0,type='n',xlim=c(-ncol(d)-6,0),ylim=c(-nrow(d)-1,1),axes=F)
  text(c(-ncol(d)):c(-1),0,rev(colnames(d)),srt=25,cex=st)
  text(-ncol(d)-6,0,"Mean risk contributions by sub-group\n(Standard deviation)\n[mean risk contribution if other exposures are set to 0]",pos=4,cex=st)
  for (i in 1:max(sub_groups)) {
    prev <- sum(ipw[sub_groups==i])/sum(ipw)
    risk <- sum(colMeans(as.matrix(risk_contributions[sub_groups==i,]),na.rm=T))
    risk_obs <- sum(ipw[sub_groups==i]*outcome_data[sub_groups==i])/sum(ipw[sub_groups==i])
    text(-ncol(d)-6,-i,paste0("Sub-group ",i,": ","n=",round(sum(ipw[sub_groups==i]),1),", e=",round(sum(ipw[sub_groups==i]*outcome_data[sub_groups==i]),1),",Prev=",format(round(prev*100,1),nsmall=1),"%, risk=",format(round(risk*100,1),nsmall=1),"%,\nexcess=",
                              format(round(prev*(risk-mean(risk_contributions$Baseline_risk))/total*100,1),nsmall=1),
                              "%, Obs risk=",format(round(risk_obs*100,1),nsmall=1),"% (",
                              paste0(format(round(prop.test(sum(ipw[sub_groups==i]*outcome_data[sub_groups==i]),sum(ipw[sub_groups==i]))$conf.int*100,1),nsmall=1),collapse="-"),
                              "%)\n",
                              "Risk based on the sum of individual effects =",
                              format(round(mean(CoOL_6_sum_of_individual_effects(exposure_data,model=model)[sub_groups==i])*100,1),nsmall=1),
                              "%"),pos=4,col=colours[i])
  }
  m <- max(d)
  ind_effect_matrix <- CoOL_6_individual_effects_matrix(exposure_data,model)
  for(g in 1:ncol(d)) { for (i in 1:nrow(d)){
    value <- paste0(format(round(as.numeric(d[i,g])*100,1),nsmall=),"%\n(",
                    format(round(sd(risk_contributions[sub_groups==i,g])*100,1),nsmall=1),"%)\n[",
                    format(round(mean(ind_effect_matrix[sub_groups==i,g]*100),1),nsmall=1),"%]")

#    text(-g,-i,value,col=adjustcolor(colours[i],d[i,g]/m),cex=st*d[i,g]/m)
#    text(-g,-i,value,col=adjustcolor(colours[i],1),cex=st*d[i,g]/m)
    text(-g,-i,value,col=adjustcolor(colours[i],ifelse(d[i,g]<   exclude_below,0,1)),cex=st)
  }}
  return(t(d))
  }





#' Visualisation of the mean risk contributions by sub-groups
#'
#' Visualisation of the mean risk contributions by sub-groups. The function uses the output
#'
#' @param results CoOL_8_mean_risk_contributions_by_sub_group.
#' @param sub_groups The vector with the sub-groups.
#' @param ipw a vector of weights per observation to allow for inverse probability of censoring weighting to correct for selection bias
#' @param restore_par_options Restore par options.
#' @export
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_9_visualised_mean_risk_contributions <- function(results, sub_groups, ipw = 1, restore_par_options = TRUE) {
  if (restore_par_options==TRUE) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }
  if (length(ipw) != length(sub_groups)) {
    ipw = rep(1, length(sub_groups))
    print("Equal weights are applied (assuming no selection bias)")
  }
  labels <- NA
  for (i in 1:ncol(results)) {  labels[i] <- paste0("Group ",i," (", format(round(100*sum(ipw[sub_groups==i])/sum(ipw),1),nsmall=1),"%)")}
  colnames(results) <- labels
  res <- results
  res <- round(res*1000)
  res <- res * 1000 / max(res)
  res <- res[-nrow(res),]
  par(mfrow=c(1,1))
  farver <- colorRampPalette(c("white","orange","orange",rep("red",3),"black"))(1000)
  par(mar=c(10,20,.5,.5))
  plot(0,0,xlim=c(0.5,ncol(res)+.5),ylim=c(0,nrow(res)+1),axes=F,ylab="",xlab="",type='n')
  axis(1,at=1:ncol(res),labels=colnames(res),tick=F,las=3)
  axis(2,at=1:nrow(res),labels=rownames(res),tick=F,las=2)
  for(r in 1:nrow(res)) {
    for(c in 1:ncol(res)) {
      rect(c-.5,r-.5,c+.5,r+.5, col=farver[res[r,c]])
    }
  }
}



#' Legend to the visualisation of the mean risk contributions by sub-groups
#'
#' Legend to the visualisation of the mean risk contributions by sub-groups. The function uses the output
#'
#' @param results CoOL_8_mean_risk_contributions_by_sub_group.
#' @param restore_par_options Restore par options.
#' @export
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_9_visualised_mean_risk_contributions_legend <- function(results, restore_par_options = TRUE) {
  if (restore_par_options==TRUE) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
  }
  par(mar=c(1,5,1,1))
  plot(0,0,type='n',ylim=c(0,100),xlim=c(0,1),axes=F,xlab="",ylab="Average risk contributions"); axis(2,at=seq(0,100,length.out=5),labels=round(seq(0,max(results),length.out=5),2),las=2)
  farver <- colorRampPalette(c("white","orange","orange",rep("red",3),"black"))(100)
  for(i in 1:100) rect(0,i-1,1,i,border=farver[i],col=farver[i])
}



#' Predict the risk based on the sum of individual effects
#'
#' By summing the through the risk as if each individual had been exposed to only one exposure, with the value the individual actually had.
#'
#' @param X The exposure data.
#' @param model The fitted the non-negative neural network.
#' @return A value the sum of indivisual effects, had there been no interactions between exposures.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_6_sum_of_individual_effects <- function(X,model) {
# All individuals has the baseline risk
    sum_of_individial_effects = rep(as.vector(model[[4]][1,1]),nrow(X))
  # Loop through each exposure with the actual values by the individuals
    for (i in 1:ncol(X)) {
      X_temp <- as.data.frame(matrix(0,nrow = nrow(X), ncol=ncol(X)))
      X_temp[,i] <- X[,i]
      sum_of_individial_effects = sum_of_individial_effects +
        rowSums(relu(t(t(as.matrix(X_temp) %*% as.matrix(model[[1]])) + as.vector(model[[2]]))))
}
return(sum_of_individial_effects)
}


#' Risk contribution matrix based on individual effects (had all other exposures been set to zero)
#'
#' Estimating the risk contribution for each exposure if each individual had been exposed to only one exposure, with the value the individual actually had.
#'
#' @param X The exposure data.
#' @param model The fitted the non-negative neural network.
#' @return  A matrix [Number of individuals, exposures] with the estimated individual effects by each exposure had all other values been set to zero.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' #See the example under CoOL_0_working_example

CoOL_6_individual_effects_matrix <- function(X,model) {
  ind_effect_matrix <- as.data.frame(matrix(0,nrow = nrow(X), ncol=ncol(X)+1)) # +1 for the baseline risk
    # Loop through each exposure with the actual values by the individuals
  for (i in 1:ncol(X)) {
    X_temp <- as.data.frame(matrix(0,nrow = nrow(X), ncol=ncol(X)))
    X_temp[,i] <- X[,i]
    ind_effect_matrix[,i] <- rowSums(relu(t(t(as.matrix(X_temp) %*% as.matrix(model[[1]])) + as.vector(model[[2]]))))
  }
  # All individuals has the baseline risk
  ind_effect_matrix[,ncol(X)+1] <- rep(as.vector(model[[4]][1,1]),nrow(X))
  return(ind_effect_matrix)
}


#' The default analysis for computational phase of CoOL
#'
#' The analysis and plots presented in the main paper. We recommend using View(CoOL_default) and View() on the many sub-functions to understand the steps and modify to your own research question. 3 sets of training will run with a learning rate of 1e-4 and a patience of 200 epochs, a learning rate of 1e-5 and a patience of 100 epochs, and a learning rate of 1e-6 and a patience of 50 epochs.
#'
#' @param data A data.frame(cbind(outcome_data,exposure_data)).
#' @param sub_groups Define the number of expected sub-groups.
#' @param exclude_below Risk contributions below this value are not shown in the table.
#' @param input_parameter_reg The regularization of the input parameters.
#' @param hidden The number of synergy-functions.
#' @param monitor Whether monitoring plots will be shown in R.
#' @param epochs The maximum number of epochs.
#' @return A series of plots across the full Causes of Outcome Learning approach.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>
#' @examples
#' # Not run
#' while (FALSE) {
#' #See the example under CoOL_0_working_example for a more detailed tutorial
#' library(CoOL)
#' data <- CoOL_0_working_example(n=10000)
#' CoOL_default(data)
#' }

CoOL_default <- function(data,sub_groups=3,exclude_below=0.01, input_parameter_reg = 1e-3,hidden=10,monitor=TRUE,epochs=10000) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  outcome_data <- data[,1]
  exposure_data <- data[,-1]
  exposure_data <- CoOL_0_binary_encode_exposure_data(exposure_data)
  model <- CoOL_1_initiate_neural_network(inputs=ncol(exposure_data), output = outcome_data,hidden=hidden)
  model <- CoOL_2_train_neural_network(lr = 1e-4,X_train=exposure_data, Y_train=outcome_data,X_test=exposure_data, Y_test=outcome_data, model=model, epochs=epochs,patience = 200, input_parameter_reg = input_parameter_reg,monitor=monitor) # Train the non-negative model (The model can be retrained)
  model <- CoOL_2_train_neural_network(lr = 1e-5,X_train=exposure_data, Y_train=outcome_data,X_test=exposure_data, Y_test=outcome_data, model=model, epochs=epochs,patience = 100, input_parameter_reg = input_parameter_reg,monitor=monitor) # Train the non-negative model (The model can be retrained)
  model <- CoOL_2_train_neural_network(lr = 1e-6,X_train=exposure_data, Y_train=outcome_data,X_test=exposure_data, Y_test=outcome_data, model=model, epochs=epochs,patience = 50, input_parameter_reg = input_parameter_reg,monitor=monitor) # Train the non-negative model (The model can be retrained)
  # Use below to combine all plots (See the note regarding the dendrogram)
  layout(matrix(c(1,1,2,2,3,3,4,4,4,5,5,5,6,6,6,6,6,6), 3, 6, byrow = TRUE));par(mar=c(3,3,3,3));par(oma=c(0,0,3,0))
  plot(model$train_performance,type='l',yaxs='i',ylab="Mean squared error",xlab="Epochs",main="Performance - training data\n") # Model performance
  CoOL_3_plot_neural_network(model,names(exposure_data),5/max(model[[1]]), title = "Model",restore_par_options=FALSE) # Model visualization
  mtext(paste0("CoOL (n=",format(nrow(data),big.mark = ",")," events=",format(sum(outcome_data),big.mark = ","),")"),side=3,line=5)
  CoOL_4_AUC(outcome_data,exposure_data,model,restore_par_options=FALSE) # AUC
  risk_contributions <- CoOL_5_layerwise_relevance_propagation(exposure_data,model) # Risk contributions
  if (requireNamespace("ggtree", quietly = TRUE)){
    requireNamespace("imager")
    dendrogram_dir <- tempfile(pattern = "", fileext = ".png")
    png(dendrogram_dir,units = 'in',res=300,height = 4,width = 4)
    CoOL_6_dendrogram(risk_contributions,number_of_subgroups = sub_groups) # Dendrogram
    dev.off()
    im <- imager::load.image(dendrogram_dir);par(mar=c(0,0,0,0));plot(imager::load.image(dendrogram_dir),axes=F);par(mar=c(5,5,3,2))
  } else {
    print("ggtree is not installed - skipping plotting the dendogram, you can install it via:")
    print("if (!requireNamespace('BiocManager', quietly = TRUE))")
    print("    install.packages('BiocManager')")
    print("BiocManager::install('ggtree')")
    plot(0,0,type="n",axes=F,xlab="",ylab="",main="No dendrogram since\nggtree is not installed")
  }
  sub_groups <- CoOL_6_sub_groups(risk_contributions,number_of_subgroups = sub_groups) # Assign sub-groups
  CoOL_7_prevalence_and_mean_risk_plot(risk_contributions,sub_groups,restore_par_options=FALSE) # Prevalence and mean risk plot
  CoOL_8_mean_risk_contributions_by_sub_group(risk_contributions, sub_groups,exposure_data = exposure_data, outcome_data = outcome_data,model=model,exclude_below = exclude_below,restore_par_options=FALSE) #  Mean risk contributions by sub-groups
}



#' Complex example
#'
#' To reproduce the complex example.
#'
#' @param n number of observations for the synthetic data.
#' @return A data frame with the columns Y, Physically_active, Low_SES, Mutation_X, LDL, Night_shifts, Air_pollution and n rows.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>

CoOL_0_complex_simulation <- function(n) {
  #n = 20000
  Genes = sample(1:0,n,prob=c(0.05,0.95),replace=TRUE)
  Living_area = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)

  Low_SES = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)
  Physically_active = sample(1:0,n,prob=c(0.8,0.2),replace=TRUE)
  for (i in 1:n) {
    if (Low_SES[i] == 1 & sample(1:0,1,prob=c(.2,.8)) ) Physically_active[i] <- 0
  }

  Mutation_X = rep(0,n)
  for (i in 1:n) {
    if (Genes[i] == 1 & sample(1:0,1,prob=c(.95,.05)) ) Mutation_X[i] <- 1
  }
  LDL = sample(1:0,n,prob=c(0.3,0.7),replace=TRUE)
  for (i in 1:n) {
    if (Genes[i] == 1 & sample(1:0,1,prob=c(.15,.85)) ) LDL[i] <- 1
  }
  Night_shifts = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)
  for (i in 1:n) {
    if (Living_area[i] == 1 & sample(1:0,1,prob=c(.1,.9)) ) Night_shifts[i] <- 1
    if (Low_SES[i] == 1 & sample(1:0,1,prob=c(.1,.9)) ) Night_shifts[i] <- 1
  }
  Air_pollution = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)
  for (i in 1:n) {
    if (Living_area[i] == 1 & sample(1:0,1,prob=c(.3,.7)) ) Air_pollution[i] <- 1
  }

  Y <-  sample(1:0,n,prob=c(0.05,0.95),replace = TRUE)
  for (i in 1:n) {
    if (Physically_active[i] == 0 & LDL[i] == 1 & Night_shifts[i] == 1 & sample(1:0,1,prob=c(.15,0.85)) ) {
      Y[i] <- 1
    }
    if (Mutation_X[i] == 1 & Air_pollution[i] == 1 & sample(1:0,1,prob=c(.1,0.9)) ) {
      Y[i] <- 1
    }
  }

  #  C = rep(0,n)

  data <- data.frame(Y,Physically_active,Low_SES,Mutation_X,LDL,Night_shifts,Air_pollution) #,C)
  for (i in 1:ncol(data))   data[,i] <- as.numeric(data[,i])
  return(data)
}




#' Common example
#'
#' To reproduce the common causes example.
#'
#' @param n number of observations for the synthetic data.
#' @return A data frame with the columns Y, A, B, C, D, E, F and n rows.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>

CoOL_0_common_simulation <- function(n) {
  #n = 20000
  A <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  B <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  C <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  D <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  E <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  F <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  U <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  Y <- sample(1:0,n,prob = c(0.01,0.99),replace=TRUE)
  for (i in 1:n) if(U[i]==1 & sample(1:0,1,prob=c(.4,.6))) B[i] <- 1
  for (i in 1:n) if(U[i]==1 & sample(1:0,1,prob=c(.3,.7))) E[i] <- 1
  for (i in 1:n) if(B[i]==1 & sample(1:0,1,prob=c(.04,.96))) Y[i] <- 1
  for (i in 1:n) if(E[i]==1 & sample(1:0,1,prob=c(.06,.94))) Y[i] <- 1
  data <- data.frame(Y,A,B,C,D,E,F)
  for (i in 1:ncol(data))   data[,i] <- as.numeric(data[,i])
  return(data)
}




#' Mediation example
#'
#' To reproduce the mediation example.
#'
#' @param n number of observations for the synthetic data.
#' @return A data frame with the columns Y, A,B ,C, D, E, F and n rows.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>

CoOL_0_mediation_simulation <- function(n) {
  #n = 20000
  A <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  B <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  C <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  D <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  E <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  F <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  Y <- sample(1:0,n,prob = c(0.01,0.99),replace=TRUE)
  for (i in 1:n) if(B[i]==1 & sample(1:0,1,prob=c(.2,.8))) E[i] <- 1
  for (i in 1:n) if(B[i]==1 & sample(1:0,1,prob=c(.04,.96))) Y[i] <- 1
  for (i in 1:n) if(E[i]==1 & sample(1:0,1,prob=c(.06,.94))) Y[i] <- 1
  data <- data.frame(Y,A,B,C,D,E,F)
  for (i in 1:ncol(data))   data[,i] <- as.numeric(data[,i])
  return(data)
}

#' Confounding example
#'
#' To reproduce the confounding example.
#'
#' @param n number of observations for the synthetic data.
#' @return A data frame with the columns Y, A, B, C, D, E, F and n rows.
#' @references Rieckmann, Dworzynski, Arras, Lapuschkin, Samek, Arah, Rod, Ekstrom. 2022. Causes of outcome learning: A causal inference-inspired machine learning approach to disentangling common combinations of potential causes of a health outcome. International Journal of Epidemiology <https://doi.org/10.1093/ije/dyac078>

CoOL_0_confounding_simulation <- function(n) {
  #n = 20000
  A <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  B <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  D <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  E <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  F <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  C <- sample(1:0,n,prob = c(0.3,0.7),replace=TRUE)
  Y <- sample(1:0,n,prob = c(0.01,0.99),replace=TRUE)
  for (i in 1:n) if(C[i]==1 & sample(1:0,1,prob=c(.4,.6))) B[i] <- 1
  for (i in 1:n) if(C[i]==1 & sample(1:0,1,prob=c(.3,.7))) F[i] <- 1
  for (i in 1:n) if(C[i]==1 & sample(1:0,1,prob=c(.15,.85))) Y[i] <- 1
  data <- data.frame(Y,A,B,C,D,E,F)
  for (i in 1:ncol(data))   data[,i] <- as.numeric(data[,i])
  return(data)
}



