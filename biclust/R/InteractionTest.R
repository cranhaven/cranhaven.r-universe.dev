
# Note: Some functions of the additivityTests package were altered so that p-values and bootstrapped F-values
# are returned as well. Additionally, the returned modTukey F value was transformed so it could be used to 
# compute a p-value.

# Note: All sampled p-values are obtained through by dividing through B+1. 


# @importFrom additivityTests mandel.test tukey.test
# @import ggplot2
# @importFrom tidyr gather


#' @title Diagnostics F Statistiics Visualization
#' @description Plots distributions of bootstrap replicates of F-statistics for row, column and multiplicative effects obtained from \code{\link{diagnosticTest}} (when \code{save_F=TRUE}). 
#' Contains an option to highlight the observed statistics.
#' @param diagnosticTest output of \code{\link{diagnosticTest}} with \code{save_F=TRUE} which contains the F-statistics and sampling replicates.
#' @param number Number of which BC to plot. This needs to be one of the Biclusters requested in in \code{\link{diagnosticTest}}.
#' @param StatVal Boolean value to draw the observed statistic on the distribution plots.
#' @param binwidth The width of the bins.
#' @export
#' @author Ewoud De Troyer
#' @examples 
#' 
#' \dontshow{
#' test <- matrix(rnorm(5000),100,50)
#' roweff <- sample(1:5,10,replace=TRUE)
#' coleff <- sample(1:5,10,replace=TRUE)
#' test[11:20,11:20] <- test[11:20,11:20] +
#'   matrix(coleff,nrow=10,ncol=10,byrow=TRUE) +
#'   matrix(roweff,nrow=10,ncol=10) +
#'   roweff %*% t(coleff)
#' 
#' 
#' #Apply Plaid Biclustering
#' res <- biclust(test, method=BCPlaid())
#' 
#' #Apply default diagnosticTest
#' out <- diagnosticTest(BCresult=res, data=test, save_F=TRUE, number=1,
#'                       statistics=c("F"),
#'                       samplingtypes=c("Permutation"))
#' diagnosticPlot2(out,number=1)
#' }
#' 
#' \dontrun{
#' #Random matrix with embedded bicluster (with multiplicative effect)
#' test <- matrix(rnorm(5000),100,50)
#' roweff <- sample(1:5,10,replace=TRUE)
#' coleff <- sample(1:5,10,replace=TRUE)
#' test[11:20,11:20] <- test[11:20,11:20] +
#'   matrix(coleff,nrow=10,ncol=10,byrow=TRUE) +
#'   matrix(roweff,nrow=10,ncol=10) +
#'   roweff %*% t(coleff)
#' 
#' 
#' #Apply Plaid Biclustering
#' res <- biclust(test, method=BCPlaid())
#' 
#' #Apply default diagnosticTest
#' out <- diagnosticTest(BCresult=res, data=test, save_F=TRUE, number=1,
#'                       statistics=c("F","Tukey","ModTukey","Tusell","Mandel","LBI","JandG"),
#'                       samplingtypes=c("Permutation","SemiparPerm","SemiparBoot",
#'                       "PermutationCor","SamplingCor","NormSim"))
#' 
#' #Plot Distributions
#' diagnosticPlot2(out,number=1)
#' }
#' 
#' @return Returns a \code{ggplot} object.
diagnosticPlot2 <- function(diagnosticTest,number=1,StatVal=TRUE,binwidth=NULL){
  if(length(number)>1){stop("'number' should be a single BC")}
  if(is.null(names(diagnosticTest))){names(diagnosticTest) <- paste0("BC",1:length(diagnosticTest))}
  if(!(paste0("BC",number)%in%names(diagnosticTest))){stop(paste0("BC",number," not available in diagnosticTest"))}
  diag_data <- diagnosticTest[[which(names(diagnosticTest)==paste0("BC",number))]]
  if(is.null(diag_data$save_F)){stop("Bootstrapped F-values were not saved")}
  
  gdata <- gather(as.data.frame(diag_data$save_F),type,Fval)
  gdata$type <- factor(gdata$type,levels = unique(gdata$type))
  
  
  out_plot <- ggplot(gdata,aes(x=Fval)) + geom_histogram(stat="bin",aes(y=..density..),binwidth = binwidth) + 
    facet_wrap(~type, scales="free") +
    geom_density(colour="blue") + labs(title=paste0("BC",number))
  
  if(StatVal){
    table_sel <- which(diag_data$table$Type!="Theoretical")
    temp_names <- paste0(diag_data$table$Method,"_",diag_data$table$Type)[table_sel]
    gdata2 <- data.frame(StatVal=diag_data$table$StatVal[table_sel],type=temp_names)
    
    out_plot <- out_plot + geom_vline(data=gdata2,aes(xintercept=StatVal),colour="red")
  }
  
  return(out_plot)
}


# MAKE DEFAULTS
# statistics=c("F","Tukey")
# c("F","Tukey","ModTukey","Tusell","Mandel","LBI","JandG")
# sampling=TRUE
# samplingtypes=c("Permutation","SemiparPerm")
# c("Theoretical","Permutation","SemiparPerm","SemiparBoot","PermutatioCor","SamplingCor","NormSim")
# Add table of combination of statistics & samplingtypes
# Add which sampling is default
# Explanation/reference to diff. sampling types



#' @title Testing Procedure for Bicluster Diagnostics
#' @description Calculate the statistical value of the row, column and multiplicative effect based on discovered biclusters in the data. 
#' Additionally multiple sampling methods are available to compute the statistical significance through p-values.
#' @param BCresult An object of class \code{biclust} containing the result of a biclustering algorithm
#' @param data data matrix, which \code{biclust} function was applied to
#' @param number Vector of bicluster numbers of which the diagnostics should be calculated. (default = all available biclusters)
#' @param verbose Boolean value to print progression of computed statistics.
#' @param statistics Vector select which statistics to compute. (default = \code{c("F","Tukey")})
#' \itemize{
#' \item \code{"F"} (Row and column F statistics of two-way ANOVA with one replicate for cell)
#' \item \code{"Tukey"} (Tukey's test for non-additivity)
#' \item \code{"ModTukey"} (\code{\link[additivityTests]{mtukey.test}})
#' \item \code{"Tusell"} (\code{\link[additivityTests]{tusell.test}})
#' \item \code{"Mandel"} (\code{\link[additivityTests]{mandel.test}})
#' \item \code{"LBI"} (\code{\link[additivityTests]{lbi.test}})
#' \item \code{"JandG"} (\code{\link[additivityTests]{johnson.graybill.test}})
#' }
#' @param sampling Boolean value to apply sampling methods to compute statistical significance (default=\code{TRUE}). 
#' If \code{FALSE} only the \code{"Theoretical"} p-values are computed. 
#' If \code{TRUE}, both the \code{"Theoretical"} and \code{samplingtypes} p-values are computed.
#' @param samplingtypes Vector of sampling methods for \code{sampling=TRUE}. (default=\code{NULL}=\code{c("Permutation","SemiparPerm")})
#' \itemize{
#' \item \code{"Permutation"} 
#' \item \code{"SemiparPerm"} 
#' \item \code{"SemiparBoot"} 
#' \item \code{"PermutationCor"} 
#' \item \code{"SamplingCor"}
#' \item \code{"NormSim"}  
#' }
#' See Details for more info.
#' @param nSim Number of permutations/bootstraps.
#' @param alpha Significance level (default=0.05)
#' @param save_F Option to save the permuted/bootstraped statistics. This is necessary for \code{\link{diagnosticPlot2}}
#' 
#' @details 
#' Due to the uncertainty of discovering the true bicluster(s) in the data, it's often advisable to not rely on the theoretical p-values but instead retrieve the p-values through a sampling procedure.
#' 
#' Available p-values/sampling types for each statistical method:
#' \itemize{
#' \item \code{"F"}: \code{"Theoretical"} and \code{"Permutation"} for both row and column effect.
#' \item \code{"Tukey"}: \code{"Theoretical"}, \code{"SemiparPerm"} and \code{"SemiparBoot"}.
#' \item \code{"ModTukey"}: \code{"Theoretical"}, \code{"SemiparPerm"}, \code{"SemiparBoot"}, \code{"PermutationCor"} and \code{"SamplingCor"}.
#' \item \code{"Tusell"}: \code{"SemiparPerm"}, \code{"SemiparBoot"} and \code{"NormSim"}.
#' \item \code{"Mandel"}: \code{"Theoretical"}, \code{"SemiparPerm"} and \code{"SemiparBoot"}.
#' \item \code{"LBI"}: \code{"SemiparPerm"}, \code{"SemiparBoot"} and \code{"NormSim"}.
#' \item \code{"JandG"}: \code{"SemiparPerm"}, \code{"SemiparBoot"} and \code{"NormSim"}.
#' } 
#' More info on the sampling types can be found in the secion below.
#' If available, the \code{"Theoretical"} will always be computed.
#' By default when \code{sampling=TRUE}, a sampling method without replacement is chosen, namely \code{"Permutation"} and \code{"SemiparPerm"}.
#' 
#' When \code{save_F=TRUE}, the null distributions of the statistics can be visualised with \code{\link{diagnosticPlot2}}.
#' 
#' \emph{Disclaimer:} While their functionality did not change, some functions of the \code{\link{additivityTests}} package were altered in order to be able to return the permuted/bootstrapped statistics and p-values.
#' 
#' @section Sampling Types: 
#' For each sampling type a permuted/bootstrapped BC is created as following:
#' \itemize{
#' \item \code{"Permutation"}: Sample a BC from the entire dataset with replacement.
#' \item \code{"SemiparPerm"}: A semi-parametric permutation procedure. Two-way ANOVA is applied on the original BC and the residual matrix extracted. A new residual matrix is created by sampling \emph{without replacement} from the original residual matrix. The sampled BC is then generated by adding this sampled residual matrix on top the mean, row and column effect of the ANOVA procedure of the original BC.
#' \item \code{"SemiparBoot"}: A semi-parametric bootstrapping procedure. Two-way ANOVA is applied on the original BC and the residual matrix extracted. A new residual matrix is created by sampling \emph{with replacement} from the original residual matrix. The sampled BC is then generated by adding this sampled residual matrix on top the mean, row and column effect of the ANOVA procedure of the original BC.
#' \item \code{"PermutationCor"}: See \code{correction=1} parameter of \code{\link[additivityTests]{mtukey.test}}. More info in Simecek and Simeckova (2012).
#' \item \code{"SamplingCor"}: See \code{correction=2} parameter of \code{\link[additivityTests]{mtukey.test}}. More info in Simecek and Simeckova (2012).
#' \item \code{"NormSim"}: Sample a BC from a standard normal distribution. This sampling procedure is used for some methods in the \code{\link{additivityTests}} package.
#' }
#' 
#' 
#' @references Tukey, J.W.: One Degree of Freedom for Non-additivity, \emph{Biometrics} \strong{5}, pp. 232-242, 1949. 
#' @references Simecek, Petr, and Simeckova, Marie. "Modification of Tukey's additivity test." \emph{Journal of Statistical Planning and Inference}, \strong{2012}.
#' 
#' @export
#' @author Ewoud De Troyer
#' @examples 
#' \dontshow{
#' test <- matrix(rnorm(5000),100,50)
#' roweff <- sample(1:5,10,replace=TRUE)
#' coleff <- sample(1:5,10,replace=TRUE)
#' test[11:20,11:20] <- test[11:20,11:20] +
#'   matrix(coleff,nrow=10,ncol=10,byrow=TRUE) +
#'   matrix(roweff,nrow=10,ncol=10) +
#'   roweff %*% t(coleff)
#' 
#' 
#' #Apply Plaid Biclustering
#' res <- biclust(test, method=BCPlaid())
#' 
#' #Apply default diagnosticTest
#' out <- diagnosticTest(BCresult=res, data=test, save_F=TRUE, number=1,
#'                       statistics=c("F","Tukey"),
#'                       samplingtypes=c("Permutation","SemiparPerm"))
#' }
#' 
#' \dontrun{
#' #Random matrix with embedded bicluster (with multiplicative effect)
#' test <- matrix(rnorm(5000),100,50)
#' roweff <- sample(1:5,10,replace=TRUE)
#' coleff <- sample(1:5,10,replace=TRUE)
#' test[11:20,11:20] <- test[11:20,11:20] +
#'   matrix(coleff,nrow=10,ncol=10,byrow=TRUE) +
#'   matrix(roweff,nrow=10,ncol=10) +
#'   roweff %*% t(coleff)
#' 
#' 
#' #Apply Plaid Biclustering
#' res <- biclust(test, method=BCPlaid())
#' 
#' #Apply default diagnosticTest
#' out <- diagnosticTest(BCresult=res, data=test, save_F=TRUE, number=1,
#'                       statistics=c("F","Tukey","ModTukey","Tusell","Mandel","LBI","JandG"),
#'                       samplingtypes=c("Permutation","SemiparPerm","SemiparBoot",
#'                       "PermutationCor","SamplingCor","NormSim"))
#' 
#' out[[1]]$table
#' }
#' 
#' @return Returns a list with \code{length(number)} elements. 
#' Each element corresponds with the requested biclusters and is a list containing:
#' \itemize{
#' \item \code{table}: a data frame where each row is \code{statistics} and \code{samplingtypes} (including Theoretical) combination. The data frame contains the \code{Method}, \code{Type} (p-value type), \code{StatVal} (statistical value), \code{CritVal} (critical value), \code{pVal} and \code{Sign} (0/1 significance indicator based on \code{alpha}).
#' \item \code{save_F}: if \code{save_F=TRUE}, a (\code{nSim} x number of permuted/bootstrapped p-values) matrix contained the sampled statistics.
#' }
#' 
diagnosticTest <- function(BCresult,data,number=1:BCresult@Number,verbose=TRUE,
                           statistics=c("F","Tukey"),
                           sampling=TRUE,samplingtypes=NULL,
                           nSim=1000, alpha=0.05,
                           save_F=FALSE){
  
  if(class(BCresult)!="Biclust"){stop("BCresult needs to be a biclust object")}
  if(any(!(statistics %in% c("F","Tukey","ModTukey","Tusell","Mandel","LBI","JandG")))){stop("statistics contains invalid option")}
  if(BCresult@Number==0){stop("BCresult does not contain discovered biclusters")}
  if(any(number<=0) | any(number>BCresult@Number)){stop("Incorrect choice of 'number'")}
  
  # Make default samplingtypes + always add theoretical
  if(sampling){
    if(is.null(samplingtypes)){
      samplingtypes <- c("Theoretical","Permutation","SemiparPerm")
    }else if(!("Theoretical"%in%samplingtypes)){
      samplingtypes <- c("Theoretical",samplingtypes)
    }
  }else{
    samplingtypes <- "Theoretical"
  }
  if(any(!(samplingtypes%in%c("Theoretical","Permutation","SemiparPerm","SemiparBoot","PermutationCor","SamplingCor","NormSim")))){stop("Incorrect 'samplingtypes'")}
  
  
  
  data <- as.matrix(data)
  
  result <- vector("list",length(number))
  names(result) <- paste0("BC",number)
  
  
  for(i in number){
    cat("BC",i,"\n")
    
    # Extract BC
    BC_data <- data[BCresult@RowxNumber[,i],BCresult@NumberxCol[i,]]
    
    # Make Data Frame
    # method_names <- c("Frow","Fcol",rep("Tukey",3),rep("Tukey_MSE",3),rep("ModTukey",5),rep("Tusell",3),rep("Mandel",3),rep("LBI",3),rep("JandG",3))
    method_names <- c(rep("Frow",2),rep("Fcol",2),rep("Tukey",3),rep("ModTukey",5),rep("Tusell",3),rep("Mandel",3),rep("LBI",3),rep("JandG",3))
    type_temp1 <- c("Theoretical","SemiparPerm","SemiparBoot")
    type_temp2 <- c("SemiparPerm","SemiparBoot")
    # type_names <- c("Theoretical","Theoretical",type_temp1,type_temp1,type_temp1,"PermutationCor","SamplingCor",type_temp2,"NormSim",type_temp1,type_temp2,"NormSim",type_temp2,"NormSim")
    type_names <- c(rep(c("Theoretical","Permutation"),2),type_temp1,type_temp1,"PermutationCor","SamplingCor",type_temp2,"NormSim",type_temp1,type_temp2,"NormSim",type_temp2,"NormSim")
    
    result_df <- data.frame(Method=method_names,Type=type_names,StatVal=rep(NA,length(method_names)),CritVal=rep(NA,length(method_names)),pVal=rep(NA,length(method_names)))
    
    
    # Only keep selected statistics
    if("F" %in% statistics){statistics <- c(statistics,"Frow","Fcol")}
    result_df <- result_df[result_df$Method %in% statistics,]
    
    
    # Only keep selected samplingtypes
    result_df <- result_df[result_df$Type %in% samplingtypes,]
    
    # Check which statistics got deleted
    deleted_statistics <- setdiff(statistics,c(as.character(result_df$Method),"F"))
    if(length(deleted_statistics)>0){
      warning(paste0(paste0(deleted_statistics,collapse=", ")," were removed from the results, due to ",paste0(samplingtypes,collapse=", ")," not being available."))
    }
    
    
    # Make matrix for bootstrap F Values
    if(save_F){
      bootstrap_names <- paste0(result_df$Method,"_",result_df$Type)[which(result_df$Type!="Theoretical")]
      if(length(bootstrap_names)>0){
        F_matrix <- matrix(0,nrow=nSim,ncol=length(bootstrap_names),dimnames = list(NULL,bootstrap_names))
      }else{
        F_matrix <- NULL
      }
    }else{
      F_matrix <- NULL
    }
    
    
    ##########################################
    #### ANOVA: Row and Col F Statistic   ####
    ##########################################
    if(verbose){cat("- Frow/Fcol\n")}
    
    
    
    roweff <- rep(c(1:sum(BCresult@RowxNumber[,i])), sum(BCresult@NumberxCol[i,]))
    coleff <- sort(rep(c(1:sum(BCresult@NumberxCol[i,])), sum(BCresult@RowxNumber[,i])))
    a <- length(unique(roweff))
    b <- length(unique(coleff))
    # data_temp <- as.vector(BC_data)
    anova_model <- aov(as.vector(BC_data) ~ as.factor(roweff) + as.factor(coleff))
    out_anova <- anova(anova_model)
    
    mu <- anova_model$coefficients[1]
    alpha_mat <- matrix(c(0,anova_model$coefficients[2:(a)]),nrow=a,ncol=b,byrow=FALSE)
    beta_mat <- matrix(c(0,anova_model$coefficients[(a+1):length(anova_model$coefficients)]),nrow=a,ncol=b,byrow=TRUE)
    res_mat <- BC_data - (mu+alpha_mat+beta_mat)
    
    
    fval_row <- out_anova[1, "F value"]
    fval_col <- out_anova[2, "F value"]
    critval_row <- qf(1-alpha,out_anova$Df[1],out_anova$Df[3])
    critval_col <- qf(1-alpha,out_anova$Df[2],out_anova$Df[3])
    pval_row <- out_anova[1, "Pr(>F)"]
    pval_col <- out_anova[2, "Pr(>F)"]
    
    if("F" %in% statistics){
      # Theoretical
      result_df[result_df$Method=="Frow" & result_df$Type=="Theoretical",3:5] <- c(fval_row,critval_row,pval_row)
      result_df[result_df$Method=="Fcol" & result_df$Type=="Theoretical",3:5] <- c(fval_col,critval_col,pval_col)
      
      
      # Resample BC (test)
      # NOTE: MOVE ROWCOLPERMUTE UP HERE TO AVOID HIGHER RAM (as well as abolish as.vector)
      if("Permutation" %in% samplingtypes){
        RowColPerm <- RowColPermuteBC(data=data,nSim=nSim,obs_row=fval_row,obs_col=fval_col,roweff=roweff,coleff=coleff,a=a,b=b,alpha=alpha,save_F=save_F)
        
        result_df[result_df$Method=="Frow" & result_df$Type=="Permutation",3:5] <- c(fval_row,RowColPerm[["crit_row"]],RowColPerm[["pval_row"]])
        result_df[result_df$Method=="Fcol" & result_df$Type=="Permutation",3:5] <- c(fval_col,RowColPerm[["crit_col"]],RowColPerm[["pval_col"]])
        
        if(save_F){
          F_matrix[,"Frow_Permutation"] <- RowColPerm$F_row
          F_matrix[,"Fcol_Permutation"] <- RowColPerm$F_col
        }
      }
      
      
      
      
    }
    
    if("Tukey" %in% statistics){
      
      ############################################################
      #### Tukey_MSE                                          ####
      #### degrees if freedom: 1 and (a-1)(b-1)-1 OR ab-(a+b) ####
      ############################################################
      if(verbose){cat("- Tukey\n")}
      
      
      out_tukeymse <- tukey.test(BC_data,alpha=alpha)
      # Theoretical
      result_df[result_df$Method=="Tukey" & result_df$Type=="Theoretical",3:5] <- c(out_tukeymse$stat,out_tukeymse$critical.value,1-pf(out_tukeymse$stat,1,a*b-a-b))
      
      # SemiparPerm
      if("SemiparPerm" %in% samplingtypes){
        SemiparPerm <- SemiPar(nSim=nSim,type=c("Perm"),StatVal=out_tukeymse$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("Tukey_MSE"),alpha=alpha,save_F=save_F)
        result_df[result_df$Method=="Tukey" & result_df$Type=="SemiparPerm",3:5] <- c(out_tukeymse$stat,SemiparPerm[["CritVal"]],SemiparPerm[["pVal"]])
        if(save_F){
          F_matrix[,"Tukey_SemiparPerm"] <- SemiparPerm$F_Stat
        }
      }
      
      # SemiparBoot
      if("SemiparBoot" %in% samplingtypes){
        SemiparBoot <- SemiPar(nSim=nSim,type=c("Boot"),StatVal=out_tukeymse$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("Tukey_MSE"),alpha=alpha,save_F=save_F)
        result_df[result_df$Method=="Tukey" & result_df$Type=="SemiparBoot",3:5] <- c(out_tukeymse$stat,SemiparBoot[["CritVal"]],SemiparBoot[["pVal"]])
        if(save_F){
          F_matrix[,"Tukey_SemiparBoot"] <- SemiparBoot$F_Stat
        }
      }
      
      
      
    }
    
    if("ModTukey" %in% statistics){
      ####################################################################################
      #### ModTukey                                                                   ####
      #### degrees if freedom: 1 and (a-1)(b-1)-1 OR ab-(a+b)                         ####
      ####                                                                            ####
      #### ORIGINALLY STAT IN PACKAGE: (RSS0/RSS - 1) * (ab-a-b)/qf(1-alpha,1,ab-a-b) ####
      ####  -> So you can theoretically compare this stat with a critval of 1         ####
      ####  -> More difficult to get p-value back!                                    ####
      ####                                                                            ####
      #### IN THIS CODE WE CHANGE THE STAT TO EASIER RETRIEVE p_VALUES!               ####     
      ####  -> new Stat: (RSS0/RSS - 1) * (ab-a-b)                                    ####
      ####  -> Which compare with qf(1-alpha,1,ab-a-b)                                ####
      ####################################################################################
      if(verbose){cat("- ModTukey\n")}
      
      # Theoretical
      out_mtukey <- mtukey.test2(BC_data,correction=0,Nboot=nSim,alpha=alpha)
      result_df[result_df$Method=="ModTukey" & result_df$Type=="Theoretical",3:5] <- c(out_mtukey$stat,out_mtukey$critical.value,out_mtukey$pvalue)
      
      # SemiparPerm
      if("SemiparPerm"%in%samplingtypes){
        SemiparPerm <- SemiPar(nSim=nSim,type=c("Perm"),StatVal=out_mtukey$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("ModTukey"),alpha=alpha,save_F=save_F)
        result_df[result_df$Method=="ModTukey" & result_df$Type=="SemiparPerm",3:5] <- c(out_mtukey$stat,SemiparPerm[["CritVal"]],SemiparPerm[["pVal"]])
        if(save_F){
          F_matrix[,"ModTukey_SemiparPerm"] <- SemiparPerm$F_Stat
        }
      }
      
      # SemiparBoot
      if("SemiparBoot"%in%samplingtypes){
        SemiparBoot <- SemiPar(nSim=nSim,type=c("Boot"),StatVal=out_mtukey$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("ModTukey"),alpha=alpha,save_F=save_F)
        result_df[result_df$Method=="ModTukey" & result_df$Type=="SemiparBoot",3:5] <- c(out_mtukey$stat,SemiparBoot[["CritVal"]],SemiparBoot[["pVal"]])
        if(save_F){
          F_matrix[,"ModTukey_SemiparBoot"] <- SemiparBoot$F_Stat
        }
      }
      
      # PermutationCor
      if("PermutationCor"%in%samplingtypes){
        out_mtukey <- mtukey.test2(BC_data,correction=1,Nboot=nSim,alpha=alpha, save_F=save_F)
        result_df[result_df$Method=="ModTukey" & result_df$Type=="PermutationCor",3:5] <- c(out_mtukey$stat,out_mtukey$critical.value,out_mtukey$pvalue)
        if(save_F){
          F_matrix[,"ModTukey_PermutationCor"] <- out_mtukey$F_Stat
        }
      }
      
      
      
      # SamplingCor
      if("SamplingCor"%in%samplingtypes){
        out_mtukey <- mtukey.test2(BC_data,correction=2,Nboot=nSim,alpha=alpha, save_F=save_F)
        result_df[result_df$Method=="ModTukey" & result_df$Type=="SamplingCor",3:5] <- c(out_mtukey$stat,out_mtukey$critical.value,out_mtukey$pvalue)
        if(save_F){
          F_matrix[,"ModTukey_SamplingCor"] <-out_mtukey$F_Stat
        }
      }
    }
    
    if("Tusell" %in% statistics){
      ##############################################
      #### Tusell Additivity Test               ####
      ####  -> NOTE: Q: Uses left-side test...? ####
      ##############################################
      if(verbose){cat("- Tusell\n")}
      
      out_tusell <- tusell.test2(BC_data, Nsim = 0, alpha=alpha)
      
      # SemiparPerm
      if("SemiparPerm"%in%samplingtypes){
        SemiparPerm <- SemiPar(nSim=nSim,type=c("Perm"),StatVal=out_tusell$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("Tusell"),alpha=alpha, save_F=save_F)
        result_df[result_df$Method=="Tusell" & result_df$Type=="SemiparPerm",3:5] <- c(out_tusell$stat,SemiparPerm[["CritVal"]],SemiparPerm[["pVal"]])
        if(save_F){
          F_matrix[,"Tusell_SemiparPerm"] <- SemiparPerm$F_Stat
        }
      }
      
      # SemiparBoot
      if("SemiparBoot"%in%samplingtypes){
        SemiparBoot <- SemiPar(nSim=nSim,type=c("Boot"),StatVal=out_tusell$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("Tusell"),alpha=alpha, save_F=save_F)
        result_df[result_df$Method=="Tusell" & result_df$Type=="SemiparBoot",3:5] <- c(out_tusell$stat,SemiparBoot[["CritVal"]],SemiparBoot[["pVal"]])
        if(save_F){
          F_matrix[,"Tusell_SemiparBoot"] <- SemiparBoot$F_Stat
        }
      }
      
      # NormSim
      if("NormSim"%in%samplingtypes){
        out_tusell <- tusell.test2(BC_data, Nsim = nSim, alpha=alpha, save_F=save_F)
        result_df[result_df$Method=="Tusell" & result_df$Type=="NormSim",3:5] <- c(out_tusell$stat,out_tusell$critical.value,out_tusell$pvalue)
        if(save_F){
          F_matrix[,"Tusell_NormSim"] <- out_tusell$F_Stat
        }
      }
    }
    
    
    if("Mandel" %in% statistics){
      ################################
      #### Mandel Additivity Test ####
      ################################
      if(verbose){cat("- Mandel\n")}
      
      
      if(a>1 | b>2){
        # Theoretical
        out_mandel <- mandel.test(BC_data, alpha=alpha)
        result_df[result_df$Method=="Mandel" & result_df$Type=="Theoretical",3:5] <- c(out_mandel$stat,out_mandel$critical.value,1-pf(out_mandel$stat,a-1,(a-1)*(b-2)))
        
        # SemiparPerm
        if("SemiparPerm"%in%samplingtypes){
          SemiparPerm <- SemiPar(nSim=nSim,type=c("Perm"),StatVal=out_mandel$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("Mandel"), alpha=alpha, save_F=save_F)
          result_df[result_df$Method=="Mandel" & result_df$Type=="SemiparPerm",3:5] <- c(out_mandel$stat,SemiparPerm[["CritVal"]],SemiparPerm[["pVal"]])
          if(save_F){
            F_matrix[,"Mandel_SemiparPerm"] <- SemiparPerm$F_Stat
          }
        }
        
        # SemiparBoot
        if("SemiparBoot"%in%samplingtypes){
          SemiparBoot <- SemiPar(nSim=nSim,type=c("Boot"),StatVal=out_mandel$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("Mandel"), alpha=alpha, save_F=save_F)
          result_df[result_df$Method=="Mandel" & result_df$Type=="SemiparBoot",3:5] <- c(out_mandel$stat,SemiparBoot[["CritVal"]],SemiparBoot[["pVal"]])
          if(save_F){
            F_matrix[,"Mandel_SemiparBoot"] <- SemiparBoot$F_Stat
          }
        }
        
      }else{
        warning(paste0("BC",i," - BC Dimension for Mandel Additivity Test should be at least 2x3"))
        result_df <- result_df[!result_df$Method=="Mandel",]
      }
    }
    
    
    if("LBI" %in% statistics){ 
      ######################################################
      #### Locally Best Invariant (LBI) Additivity Test ####
      ######################################################
      if(verbose){cat("- LBI\n")}
      
      out_LBI <- lbi.test2(BC_data,Nsim=0, alpha=alpha)
      
      # SemiparPerm
      if("SemiparPerm"%in%samplingtypes){
        SemiparPerm <- SemiPar(nSim=nSim,type=c("Perm"),StatVal=out_LBI$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("LBI"), alpha=alpha, save_F=save_F)
        result_df[result_df$Method=="LBI" & result_df$Type=="SemiparPerm",3:5] <- c(out_LBI$stat,SemiparPerm[["CritVal"]],SemiparPerm[["pVal"]])
        if(save_F){
          F_matrix[,"LBI_SemiparPerm"] <- SemiparPerm$F_Stat
        }
      }
      
      # SemiparBoot
      if("SemiparBoot"%in%samplingtypes){
        SemiparBoot <- SemiPar(nSim=nSim,type=c("Boot"),StatVal=out_LBI$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("LBI"), alpha=alpha, save_F=save_F)
        result_df[result_df$Method=="LBI" & result_df$Type=="SemiparBoot",3:5] <- c(out_LBI$stat,SemiparBoot[["CritVal"]],SemiparBoot[["pVal"]])
        if(save_F){
          F_matrix[,"LBI_SemiparBoot"] <- SemiparBoot$F_Stat
        }
      }
      
      # NormSim
      if("NormSim"%in%samplingtypes){
        out_LBI <- lbi.test2(BC_data, Nsim = nSim, alpha=alpha,save_F=save_F)
        result_df[result_df$Method=="LBI" & result_df$Type=="NormSim",3:5] <- c(out_LBI$stat,out_LBI$critical.value,out_LBI$pvalue)
        if(save_F){
          F_matrix[,"LBI_NormSim"] <- out_LBI$F_Stat
        }
      }
    }
    
    if("JandG" %in% statistics){
      ##############################################
      #### Johnson and Graybill Additivity Test ####
      ##############################################
      if(verbose){cat("- JandG\n")}
      
      out_JandG <- johnson.graybill.test2(BC_data,Nsim=0, alpha=alpha)
      
      # SemiparPerm
      if("SemiparPerm"%in%samplingtypes){
        SemiparPerm <- SemiPar(nSim=nSim,type=c("Perm"),StatVal=out_JandG$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("JandG"), alpha=alpha,save_F=save_F)
        result_df[result_df$Method=="JandG" & result_df$Type=="SemiparPerm",3:5] <- c(out_JandG$stat,SemiparPerm[["CritVal"]],SemiparPerm[["pVal"]])
        if(save_F){
          F_matrix[,"JandG_SemiparPerm"] <- SemiparPerm$F_Stat
        }
      }
      
      # SemiparBoot
      if("SemiparBoot"%in%samplingtypes){
        SemiparBoot <- SemiPar(nSim=nSim,type=c("Boot"),StatVal=out_JandG$stat,mu=mu,res_mat=res_mat,alpha_mat=alpha_mat,beta_mat=beta_mat,method=c("JandG"), alpha=alpha,save_F=save_F)
        result_df[result_df$Method=="JandG" & result_df$Type=="SemiparBoot",3:5] <- c(out_JandG$stat,SemiparBoot[["CritVal"]],SemiparBoot[["pVal"]])
        if(save_F){
          F_matrix[,"JandG_SemiparBoot"] <- SemiparBoot$F_Stat
        }
      }
      
      # NormSim
      if("NormSim"%in%samplingtypes){
        out_JandG <- johnson.graybill.test2(BC_data, Nsim = nSim, alpha=alpha,save_F=save_F)
        result_df[result_df$Method=="JandG" & result_df$Type=="NormSim",3:5] <- c(out_JandG$stat,out_JandG$critical.value,out_JandG$pvalue)
        if(save_F){
          F_matrix[,"JandG_NormSim"] <- out_JandG$F_Stat
        }
      }
    }
    
    #######################
    ### ADD SIGN COLUMN ###
    #######################
    result_df$Sign <- (result_df$pVal<alpha)+0
    
    
    ###############################
    ### SAVE DATA FRAME IN LIST ###
    ###############################
    
    result[[i]] <- list(table=result_df,save_F=F_matrix)
    cat("\n")
  }
  
  
  return(result)
  
}


## Each Permute BC is simply randomly drawn values of the full data matrix
# NOTE: MOVE ROWCOLPERMUTE UP HERE TO AVOID HIGHER RAM (as well as abolish as.vector)
RowColPermuteBC <- function(data,nSim,obs_row,obs_col,roweff,coleff,a,b,
                            save_F=FALSE,alpha){
  
  BC_dim <- a*b
  
  fval_row <- fval_col <- 1:nSim
  
  for(i in 1:nSim){
    
    # Making random selection out of full data
    anova_model <- aov(sample(as.vector(data),BC_dim,replace=FALSE) ~ as.factor(roweff) + as.factor(coleff))
    out_anova <- anova(anova_model)
    
    
    fval_row[i] <- out_anova[1, "F value"]
    fval_col[i] <- out_anova[2, "F value"]
    
  }
  
  
  if(save_F){
    return(list(
      pval_row = (sum(fval_row>obs_row)+1)/(nSim+1),
      crit_row = quantile(fval_row,probs=1-alpha),
      pval_col = (sum(fval_col>obs_col)+1)/(nSim+1),
      crit_col = quantile(fval_col,probs=1-alpha),
      F_row = fval_row,
      F_col = fval_col
    ))
  }else{
    return(list(
      pval_row = (sum(fval_row>obs_row)+1)/(nSim+1),
      crit_row = quantile(fval_row,probs=1-alpha),
      pval_col = (sum(fval_col>obs_col)+1)/(nSim+1),
      crit_col = quantile(fval_col,probs=1-alpha)
    ))
  }
}


mtukey.test2 <- function (Y, alpha = 0.05, correction = 0, Nboot = 1000, save_F=FALSE){
  p.test <- function(data) {
    n.a <- nrow(data)
    n.b <- ncol(data)
    mu <- mean(data)
    r.effect <- apply(data, 1, mean) - mu
    c.effect <- apply(data, 2, mean) - mu
    exp <- matrix(mu + rep(r.effect, n.b) + rep(c.effect, 
                                                each = n.a), n.a, n.b)
    err <- data - exp
    k <- sum(cbind(r.effect) %*% rbind(c.effect) * err)/sum(cbind(r.effect)^2 %*% 
                                                              rbind(c.effect)^2)
    y <- data - mu - rep(c.effect, each = n.a)
    x <- matrix(1 + k * rep(c.effect, each = n.a), n.a, n.b)
    a.new <- apply(x * y, 1, sum)/apply(x^2, 1, sum)
    y <- data - mu - rep(r.effect, n.b)
    x <- matrix(1 + k * rep(r.effect, n.b), n.a, n.b)
    b.new <- apply(x * y, 2, sum)/apply(x^2, 2, sum)
    exp2 <- matrix(mu + rep(a.new, n.b) + rep(b.new, each = n.a), 
                   n.a, n.b)
    err2 <- data - exp2
    k.new <- sum(cbind(a.new) %*% rbind(b.new) * err2)/sum(cbind(a.new)^2 %*% 
                                                             rbind(b.new)^2)
    exp3 <- matrix(mu + rep(a.new, n.b) + rep(b.new, each = n.a), 
                   n.a, n.b) + k.new * cbind(a.new) %*% rbind(b.new)
    err3 <- data - exp3
    rss0 <- sum(err^2)
    rss <- sum(err3^2)
    s <- rss/(n.a * n.b - n.a - n.b)
    loglik.ratio <- rss0/rss
    stat <- (loglik.ratio - 1) * (n.a * n.b - n.a - n.b)
    # qf(1 - alpha, 1, n.a * n.b - n.a - n.b)
    return(c(stat, rss, s, loglik.ratio, abs(k.new)))
  }
  n.a <- nrow(Y)
  n.b <- ncol(Y)
  mu <- mean(Y)
  r.effect <- apply(Y, 1, mean) - mu
  c.effect <- apply(Y, 2, mean) - mu
  exp <- matrix(mu + rep(r.effect, n.b) + rep(c.effect, each = n.a), 
                n.a, n.b)
  err <- Y - exp
  ppp <- p.test(Y)
  
  if(ppp[1]==Inf){
    out <- list(result = ppp[1] > 1, stat = ppp[1], critical.value = NA, pvalue=1,
                alpha = alpha, name = "Modified Tukey test (Correction Ignored)")
  }else{
    
    if (correction == 0) 
      out <- list(result = ppp[1] > 1, stat = ppp[1], critical.value = qf(1 - alpha, 1, n.a * n.b - n.a - n.b), pvalue=1-pf(ppp[1], 1, n.a * n.b - n.a - n.b),
                  alpha = alpha, name = "Modified Tukey test")
    if (correction == 1) {
      boot <- replicate(Nboot, p.test(exp + sample(err, n.a * 
                                                     n.b, replace = FALSE)))
      B <- apply(boot, 1, quantile, probs = 1 - alpha)
      
      if(save_F){
        out <- list(result = ppp[1] > B[1], stat = ppp[1], critical.value = B[1],pvalue=sum(boot[1,]>ppp[1])/(Nboot+1),
                    alpha = alpha, name = "Modified Tukey test (small sample size correction, type 1)",F_Stat=boot[1,])
      }else{
        out <- list(result = ppp[1] > B[1], stat = ppp[1], critical.value = B[1],pvalue=sum(boot[1,]>ppp[1])/(Nboot+1),
                    alpha = alpha, name = "Modified Tukey test (small sample size correction, type 1)")
      }
      
    }
    if (correction == 2) {
      boot <- replicate(Nboot, p.test(exp + rnorm(n.a * n.b, 
                                                  sd = sqrt(ppp[3]))))
      B <- apply(boot, 1, quantile, probs = 1 - alpha)
      
      if(save_F){
        out <- list(result = ppp[1] > B[1], stat = ppp[1], critical.value = B[1], pvalue=sum(boot[1,]>ppp[1])/(Nboot+1),
                    alpha = alpha, name = "Modified Tukey test (small sample size correction, type 2)",F_Stat=boot[1,])
      }else{
        out <- list(result = ppp[1] > B[1], stat = ppp[1], critical.value = B[1], pvalue=sum(boot[1,]>ppp[1])/(Nboot+1),
                    alpha = alpha, name = "Modified Tukey test (small sample size correction, type 2)")
      }
    }
  }
  class(out) <- "aTest"
  return(out)
}



tusell.test2 <- function (Y, alpha = 0.05, Nsim = 1000, save_F=FALSE){
  if (nrow(Y) > ncol(Y)){
    Y <- t(Y)
  }
  if (Nsim>0){
    NormSim <- critical.values2(nrow(Y), ncol(Y), Nsim, alpha)
    critical.value <- NormSim$t3
    boot.value <- NormSim$t3_full
  } 
  
  a <- nrow(Y)
  b <- ncol(Y)
  p <- a - 1
  q <- b - 1
  R <- Y - rep(apply(Y, 1, mean), b) - rep(apply(Y, 2, mean), 
                                           each = a) + rep(mean(Y), a * b)
  S <- R %*% t(R)
  vl.cisla <- eigen(S/sum(diag(S)), only.values = TRUE)$values
  
  if(Nsim>0){
    if(save_F){
      out <- list(result = (prod(vl.cisla[1:p]) < critical.value), stat = prod(vl.cisla[1:p]), critical.value = critical.value,pvalue=(sum(boot.value<(prod(vl.cisla[1:p])))+1)/(Nsim+1), alpha = alpha, name = "Tusell test",F_Stat=boot.value)
    }else{
      out <- list(result = (prod(vl.cisla[1:p]) < critical.value), stat = prod(vl.cisla[1:p]), critical.value = critical.value,pvalue=(sum(boot.value<(prod(vl.cisla[1:p])))+1)/(Nsim+1), alpha = alpha, name = "Tusell test")
    }
  }else{
    out <- list(result = NULL, stat = prod(vl.cisla[1:p]), critical.value = NULL,pvalue=NULL, alpha = alpha, name = "Tusell test")
  }
  
  class(out) <- "aTest"
  
  return(out)
}


lbi.test2 <- function (Y, alpha = 0.05, Nsim = 1000, save_F=FALSE) 
{
  if (nrow(Y) > ncol(Y)){
    Y <- t(Y)
  } 
  
  if(Nsim>0){
    NormSim <- critical.values2(nrow(Y), ncol(Y), Nsim, alpha)
    critical.value <- NormSim$t2
    boot.value <- NormSim$t2_full
  }
  
  
  a <- nrow(Y)
  b <- ncol(Y)
  p <- a - 1
  q <- b - 1
  R <- Y - rep(apply(Y, 1, mean), b) - rep(apply(Y, 2, mean), 
                                           each = a) + rep(mean(Y), a * b)
  S <- R %*% t(R)
  vl.cisla <- eigen(S/sum(diag(S)), only.values = TRUE)$values
  
  if(Nsim>0){
    if(save_F){
      out <- list(result = sum(vl.cisla^2) > critical.value, stat = sum(vl.cisla^2), critical.value = critical.value, pvalue=(sum(boot.value>sum(vl.cisla^2))+1)/(Nsim+1), alpha = alpha, name = "Locally Best Invariant test", F_Stat=boot.value)
    }else{
      out <- list(result = sum(vl.cisla^2) > critical.value, stat = sum(vl.cisla^2), critical.value = critical.value, pvalue=(sum(boot.value>sum(vl.cisla^2))+1)/(Nsim+1), alpha = alpha, name = "Locally Best Invariant test")
    }
  }else{
    out <- list(result = NULL, stat = sum(vl.cisla^2), critical.value = NULL, pvalue=NULL, alpha = alpha, name = "Locally Best Invariant test")
  }
  
  class(out) <- "aTest"
  return(out)
}


johnson.graybill.test2 <- function (Y, alpha = 0.05, Nsim = 1000, save_F=TRUE) 
{
  if (nrow(Y) > ncol(Y)){
    Y <- t(Y)
  } 
  
  if(Nsim>0){
    NormSim <- critical.values2(nrow(Y), ncol(Y), Nsim, alpha)
    critical.value <- NormSim$t1
    boot.value <- NormSim$t1_full
  }
  
  
  a <- nrow(Y)
  b <- ncol(Y)
  p <- a - 1
  q <- b - 1
  R <- Y - rep(apply(Y, 1, mean), b) - rep(apply(Y, 2, mean), 
                                           each = a) + rep(mean(Y), a * b)
  S <- R %*% t(R)
  vl.cisla <- eigen(S/sum(diag(S)), only.values = TRUE)$values
  
  
  if(Nsim>0){
    if(save_F){
      out <- list(result = vl.cisla[1] > critical.value, stat = vl.cisla[1], critical.value = critical.value, pvalue= (sum(boot.value>vl.cisla[1])+1)/(Nsim+1), alpha = alpha, name = "Johnson and Graybill test", F_Stat=boot.value)
    }else{
      out <- list(result = vl.cisla[1] > critical.value, stat = vl.cisla[1], critical.value = critical.value, pvalue= (sum(boot.value>vl.cisla[1])+1)/(Nsim+1), alpha = alpha, name = "Johnson and Graybill test")
    }
  }else{
    out <- list(result = NULL, stat = vl.cisla[1], critical.value =NULL, pvalue= NULL, alpha = alpha, name = "Johnson and Graybill test")
  }
  
  class(out) <- "aTest"
  return(out)
}


critical.values2 <- function (a, b, N = 1e+05, alpha = 0.05){
  t1 <- t2 <- t3 <- NULL
  p <- a - 1
  q <- b - 1
  
  t1 <- t2 <- t3 <- 1:N
  
  for (i in 1:N) {
    Y <- matrix(rnorm(a * b), a, b)
    R <- Y - rep(apply(Y, 1, mean), b) - rep(apply(Y, 2, 
                                                   mean), each = a) + rep(mean(Y), a * b)
    S <- R %*% t(R)
    vl.cisla <- eigen(S/sum(diag(S)), only.values = TRUE)$values
    t1[i] <- vl.cisla[1]
    t2[i] <- sum(vl.cisla^2)
    t3[i] <- prod(vl.cisla[1:p])
  }
  return(list(t1 = quantile(t1, 1 - alpha), 
              t2 = quantile(t2, 1 - alpha), 
              t3 = quantile(t3, alpha), 
              alpha = alpha,
              t1_full = t1,
              t2_full = t2,
              t3_full = t3
  ))
}



SemiPar <- function(nSim=1000,type=c("Perm","Boot"),StatVal,mu,res_mat,alpha_mat,beta_mat,method=c("Tukey","Tukey_MSE","ModTukey","Tusell","Mandel","LBI","JandG"),
                    alpha=0.05,save_F=FALSE){
  
  if(!(method %in%c("Tukey","Tukey_MSE","ModTukey","Tusell","Mandel","LBI","JandG")) ){stop("SemiPar method not available")}
  
  StatBoot <- 1:nSim
  replace <- ifelse(type=="Perm",FALSE,TRUE)
  
  for(i.Sim in 1:nSim){
    
    res_boot <- matrix(sample(as.vector(res_mat),size=(nrow(res_mat)*ncol(res_mat)),replace=replace),nrow=nrow(res_mat),ncol=ncol(res_mat))
    data_boot <- mu + alpha_mat + beta_mat + res_boot
    
    if(method=="Tukey"){
      y.bar.j <- colMeans(data_boot)
      y.bar.i <- rowMeans(data_boot)
      y.bar <- mean(as.vector(data_boot))
      B <- sum((y.bar.i - y.bar)^2)
      C <- sum((y.bar.j - y.bar)^2)
      A <- 0
      for (i in 1:nrow(data_boot)) {
        for (j in 1:ncol(data_boot)) {
          A <- A + data_boot[i, j] * (y.bar.i[i] - y.bar) * (y.bar.j[j] -  y.bar)
        }
      }
      StatBoot[i.Sim] <- (A^2)/(B * C)
    }
    
    if(method=="Tukey_MSE"){
      StatBoot[i.Sim] <- tukey.test(data_boot)$stat
    }
    
    if(method=="ModTukey"){
      StatBoot[i.Sim] <- mtukey.test2(data_boot,correction=0)$stat
    }
    
    if(method=="Tusell"){
      StatBoot[i.Sim] <- tusell.test2(data_boot, Nsim = 0)$stat
    }
    
    if(method=="Mandel"){
      StatBoot[i.Sim] <- mandel.test(data_boot)$stat
    }
    
    if(method=="LBI"){
      StatBoot[i.Sim] <- lbi.test2(data_boot,Nsim=0)$stat
    }
    
    if(method=="JandG"){
      StatBoot[i.Sim] <- johnson.graybill.test2(data_boot,Nsim=0)$stat
    }
    
  }
  
  # Return pval and critval at 5%
  if(method=="Tusell"){
    SemiPar_out <- c(quantile(StatBoot,alpha),(sum(StatBoot<StatVal)+1)/(nSim+1))
  }else{
    SemiPar_out <- c(quantile(StatBoot,1-alpha),(sum(StatBoot>StatVal)+1)/(nSim+1))
  }
  names(SemiPar_out) <- c("CritVal","pVal")
  
  if(save_F){
    return(list(
      CritVal=SemiPar_out[1],
      pVal=SemiPar_out[2],
      F_Stat = StatBoot
    ))
  }else{
    return(list(
      CritVal=SemiPar_out[1],
      pVal=SemiPar_out[2]
    ))
  }
  
  
  return(SemiPar_out)
  
}


if(getRversion() >= "2.15.1"){
  globalVariables(c("type","Fval","..density.."))
}
