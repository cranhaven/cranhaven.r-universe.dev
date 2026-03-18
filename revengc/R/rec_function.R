library(mipfp)
library(stringr)
library(dplyr)
library(truncdist)
source("R/cnbinom.pars_function.R")
source("R/seedmatrix_function.R")
source("R/reweight.contingencytable.R")
source("R/reweight.univariatetable.R")
source("R/format_data_for_functions.R")


 rec<-function (X = NULL, Y = NULL, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound, 
                      seed.matrix = NULL, seed.estimation.method = NULL) {
   
   
   # create row and column ranges   
   rowrange<-Xlowerbound: Xupperbound
   colrange<-Ylowerbound: Yupperbound

    #first have to check if X and Y are tables or averages because this effects how mu is found
      
    #case where 2 averages
    if (length(X)==1 && length(Y)==1) {
      rowmu=X
      colmu=Y
      
      # truncated Poisson 
      rowprob<-dtrunc(rowrange,lambda =rowmu, spec = "pois", a = Xlowerbound - 1, b = Xupperbound)
      colprob<-dtrunc(colrange,lambda =colmu, spec = "pois", a = Ylowerbound - 1, b = Yupperbound)
      # updating users about what is happening 
      print("Truncated Poisson distributions were calculated for both the X and Y variable (i.e. Var(X) = E(X) and Var(Y) = E(Y) has to be assumed when only averages are provided)")
      
    } #end of case where 2 averages

    #case where 2 tables
    if (length(X)>1 && length(Y)>1) {
      roww=cnbinom.pars(X)
      rowmu=roww$Average
      rowr = roww$Dispersion
      
      coll=cnbinom.pars(Y)
      colmu=coll$Average
      colr = coll$Dispersion
      
      #Negative binomial 
      rowprob<-dtrunc(rowrange, size =rowr, mu = rowmu, spec = "nbinom", a = Xlowerbound - 1, b = Xupperbound)
      colprob<-dtrunc(colrange,size = colr, mu = colmu, spec = "nbinom", a = Ylowerbound - 1, b = Yupperbound)
      # updating users about what is happening 
      print("Truncated negative binomial distributions between were calculated for both the X and Y variable.")
      
      # reweighting tables 
      #names
      names(rowprob)<-Xlowerbound:Xupperbound
      names(colprob)<-Ylowerbound:Yupperbound
      
      rowprob<-reweight.univariatetable(observed.table = X, estimated.table = rowprob)
      colprob<-reweight.univariatetable(observed.table = Y, estimated.table = colprob)

    } #end of case where 2 tables

    #case where X=table and Y=average
    if (length(X)>1 && length(Y)==1) {
      #find mu from censored table
      roww=cnbinom.pars(X)
      rowmu=roww$Average
      rowr = roww$Dispersion
      #average represent mu for Y
      colmu=Y
      
      #Negative binomial 
      rowprob<-dtrunc(rowrange, size =rowr, mu = rowmu, spec = "nbinom", a = Xlowerbound - 1, b = Xupperbound)
      #Poisson 
      colprob<-dtrunc(colrange,lambda =colmu, spec = "pois", a = Ylowerbound - 1, b = Yupperbound)
      # updating users about what is happening 
      print("A negative binomial distribution was calculated for the X variable and a Poisson distribution was calculated for the Y variable (i.e. Var(Y) = E(Y) has to be assumed when only an average is provided).")
  
      
      # reweighting tables
      #names
      names(rowprob)<-Xlowerbound:Xupperbound
      names(colprob)<-Ylowerbound:Yupperbound
      
      rowprob<-reweight.univariatetable(observed.table = X, estimated.table = rowprob)

    } #end of case where X=table and Y=average

    #case where X=average and Y=table
    if (length(X)==1 && length(Y)>1) {
      #averages represent mu for X
      rowmu=X
      coll=cnbinom.pars(Y)
      colmu=coll$Average
      colr = coll$Dispersion
      
      #Poisson 
      rowprob<-dtrunc(rowrange,lambda =rowmu, spec = "pois", a = Xlowerbound - 1, b = Xupperbound)
      #Negative binomial 
      colprob<-dtrunc(colrange,size = colr, mu = colmu, spec = "nbinom", a = Ylowerbound - 1, b = Yupperbound)
      # updating users about what is happening 
      print("A truncated Poisson distribution was calculated for the X variable (i.e. Var(X) = E(X) has to be assumed when only an average is provided) and a truncated negative binomial distribution was calculated for Y.")
      
      # reweighting tables 
      #names
      names(rowprob)<-Xlowerbound:Xupperbound
      names(colprob)<-Ylowerbound:Yupperbound
      
      colprob<-reweight.univariatetable(observed.table = Y, estimated.table = colprob)
      
    } #end of case where X=average and Y=table
    
    # case where censored contingency table = Y
    if (is.null(X) && length(Y)>2){
      x<-row.marginal(Y)
      roww=cnbinom.pars(x)
      rowmu=roww$Average
      rowr = roww$Dispersion
      
      y<-column.marginal(Y)
      coll=cnbinom.pars(y)
      colmu=coll$Average
      colr = coll$Dispersion
      
      # error if table is weird
      sumx<-sum(x$`Marginal Frequencies`)
      sumy<-sum(y$`Marginal Frequencies`)
      
      # removing marginals to get to inside of table
      # br = bottom row, tr = top row, rc = right column, and lc = left column
      brgone<-Y[-nrow(Y),]
      # for removing the top row we have to also accounte for if user read in csv with header=TRUE or header=FALSE
      trgone=unname(brgone)
      rcgone<-trgone[, -ncol(trgone)]
      lcgone<-rcgone[,-1]
      Inside<-matrix(as.matrix(lcgone), dim(data.frame(lcgone))*dim(data.frame(lcgone))[2], 1)
      #r emoving commas from inside of table
      inside<-str_replace_all(Inside,",","")
      suminside<-sum(as.numeric(inside))
      
      if(all.equal(sumx,sumy) == FALSE) stop ('Margins are not equal.')
      if(all.equal(sumx,suminside) == FALSE)stop ('Row margins are not equal to inside of table.')
      if(all.equal(sumy,suminside )== FALSE) stop ('Column margins are not equal to inside of table.')
      
      #Negative binomial 
      rowprob<-dtrunc(rowrange, size =rowr, mu = rowmu, spec = "nbinom", a = Xlowerbound - 1, b = Xupperbound)
      colprob<-dtrunc(colrange, size = colr, mu = colmu, spec = "nbinom", a = Ylowerbound - 1, b = Yupperbound)
      # updating users about what is happening 
      print("Truncated negative binomial distributions were calculated for both the row X and column Y.")
    }
    
    # case where censored contingency table = X
    if (is.null(Y) && length(X)>2){
      x<-row.marginal(X)
      roww=cnbinom.pars(x)
      rowmu=roww$Average
      rowr = roww$Dispersion
      
      y<-column.marginal(X)
      coll=cnbinom.pars(y)
      colmu=coll$Average
      colr = coll$Dispersion
      
      # error if table is weird
      sumx<-sum(x$`Marginal Frequencies`)
      sumy<-sum(y$`Marginal Frequencies`)
      
      # removing marginals to get to inside of table
      # br = bottom row, tr = top row, rc = right column, and lc = left column
      brgone<-X[-nrow(X),]
      # for removing the top row we have to also accounte for if user read in csv with header=TRUE or header=FALSE
      trgone=unname(brgone)
      rcgone<-trgone[, -ncol(trgone)]
      lcgone<-rcgone[,-1]
      Inside<-matrix(as.matrix(lcgone), dim(data.frame(lcgone))*dim(data.frame(lcgone))[2], 1)
      #r emoving commas from inside of table
      inside<-str_replace_all(Inside,",","")
      suminside<-sum(as.numeric(inside))
      
      if(all.equal(sumx,sumy) ==FALSE ) stop ('Margins are not equal.')
      if(all.equal(sumx,suminside)==FALSE ) stop ('Row margins are not equal to inside of table.')
      if(all.equal(sumy,suminside)==FALSE ) stop ('Column margins are not equal to inside of table.')
      
      #Negative binomial (different ways to calculate it.. )
      rowprob<-dtrunc(rowrange, mu = rowmu, size =rowr, spec = "nbinom", a = Xlowerbound - 1, b = Xupperbound)
      colprob<-dtrunc(colrange, mu = colmu, size = colr, spec = "nbinom", a = Ylowerbound - 1, b = Yupperbound)
      # updating users about what is happening
      print("Truncated negative binomial distributions were calculated for both row X and column Y.")
    }

   
    # error if averages/bounds are incorrect
    if (rowmu <= Xlowerbound) stop ('Check X (row) inputs. X average <= Xlowerbound.')
    if (colmu <= Ylowerbound) stop ('Check Y (column) inputs. Y average <= Ylowerbound.')

    # error if rowmu/colmu > bounds
    if (rowmu >= Xupperbound) stop ('Check X (row) inputs. X average >= Xupperbound.')
    if (colmu >= Yupperbound) stop ('Check Y (column) inputs. Y average >= Yupperbound.')
      

    # both marginals == 1
    rowc_prob<-rowprob
    colc_prob<-colprob
    # print(sum(rowc_prob))
    # print(sum(colc_prob))

    # generating an intial a table to be updated.. different cases ...
    
    # decoupled case no seed provided
    if (!is.null(X) && !is.null(Y) && is.null(seed.matrix)) {
      seedfinal <- array(1,dim=c(length(rowc_prob),c(length(colc_prob))))
      # make probabilities 
      seedfinal <- seedfinal/sum(seedfinal)
      print("The default seed matrix implies independence between variables 1/(length(Xlowerbound:Xupperbound)*length(Ylowerbound:Yupperbound)) (i.e. independence between variables has to be assumed when there is no external information about the joint distribution)")}
    # decoupled case with seed provided 
    if (!is.null(X) && !is.null(Y) && !is.null(seed.matrix)) {seedfinal=seed.matrix}
    
    # case where censored contingency table = X and no seed is provided
    if (is.null(Y) && length(X)>2 && is.null(seed.matrix)){
      seedfinal<- seedmatrix(X, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound)$Probabilities
      print ("The default seed matrix is the output from seedmatrix()$Probabilities: see seedmatrix() function for more information")}
      
    # case where censored contingency table = Y and no seed is provided
    if (is.null(X) && length(Y)>2 && is.null(seed.matrix)){
      seedfinal<- seedmatrix(Y, Xlowerbound, Xupperbound, Ylowerbound, Yupperbound)$Probabilities
      print ("The default seed matrix is the output from seedmatrix()$Probabilities: see seedmatrix() function for more information")}
    # case where censored contingency table = X and seed is provided
    if (is.null(Y) && length(X)>2 && !is.null(seed.matrix)){seedfinal=seed.matrix} 
    # case where censored contingency table = Y and seed is provided
    if (is.null(X) && length(Y)>2 && !is.null(seed.matrix)){seedfinal=seed.matrix}  
    
    if( all.equal(sum(seedfinal), 1) ==FALSE) {
      seedfinal<-seed.matrix/sum(seed.matrix)
      print("seed.matrix was turned to probabilites: seed.matrix/sum(seed.matrix)")
    }
    
    # store the margins in a list
    tgt.data <- list(rowc_prob, colc_prob)
    # list of dimensions of each marginal constrain
    tgt.list <- list(1,2)
    # calling the estimate function (seed.estimation.method = ipfp, ml, chi2, lsq)
    if (is.null(seed.estimation.method)) {seedestimationmethodfinal <- c("ipfp")} else{
      seedestimationmethodfinal=c(seed.estimation.method)}
    
    # print seed and method using
    print(paste("You are using the", seedestimationmethodfinal, "method to updated the seed."))
    
    #mipfp R package 
    final<- Estimate(seedfinal, tgt.list, tgt.data, method = seedestimationmethodfinal)
    
    # check to see if seed method could converge and give error if not
    if (exists("final")==FALSE) {stop ('Estimate() function in mipfp R package cannot complete convergence.')}

    #cell estimated values
    finalx<-data.frame(final$x.hat)
    row.names(finalx)<-Xlowerbound:Xupperbound
    names(finalx)<-Ylowerbound:Yupperbound
    
    #cell probabilities values 
    # finalp<-data.frame(final$x.hat)
  
   # start reweighting contingency  ###########################
   # Y is contingency table
    if (is.null(X)){  
     finalouput<-reweight.contingencytable(observed.table = Y, estimated.table = finalx)
    }
    
   # X is contingency table
    if (is.null(Y)){  
     finalouput<-reweight.contingencytable(observed.table = X, estimated.table = finalx)
    }
    
    # decouled cases 
    if (!is.null(Y) && !is.null(X)) {
      finalouput<-finalx
    }
    
    # end reweighting     ###########################
    
    # negative binomal dispersion parameter is not always used. 
    if (exists("rowr")==FALSE) {rowr=NA}
    if (exists("colr")==FALSE) {colr=NA}
    
    return(list("Probability.Estimates"=finalouput, "RowX.Average" = rowmu, "ColumnY.Average" = colmu, 
                "RowX.Dispersion" = rowr, "ColumnY.Dispersion" = colr))


    
} #end of rec

