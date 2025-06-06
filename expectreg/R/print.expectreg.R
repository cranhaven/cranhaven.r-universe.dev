print.expectreg <-
function(x,...)
{
   cat("\nCall:\n", deparse(x$formula), "\n", sep = "")
   if(!inherits(x,"boost"))
   {
   cat("\nHead of Data:\n", sep = "")
   if(x$formula[[3]] != "1") {
       dat = data.frame(cbind(x$response,matrix(unlist(x$covariates),nrow=length(x$response)))[1:min(6,length(x$response)),])
       names(dat)[1] = attr(x$response,"name")
       if(length(x$covariates)>1) {names(dat)[2:(length(x$covariates)+1)] = names(x$covariates)}
       }
   if(x$formula[[3]] == "1") {
       dat = data.frame(x$response[1:min(6,length(x$response))])
       names(dat)[1] = attr(x$response,"name")
       }
   print(dat)
   cat("\n")
   }
   cat("\nFitted Expectiles:\n",  sep = "")
   print(x$asymmetries)
   cat("\n")
   if(!inherits(x,"boost"))
   {
     cat("\nSmoothing Parameters:\n",  sep = "")
     print(x$lambda)
     cat("\n")
     cat("\nIntercepts:\n", sep = "")
     print(x$inter)
     cat("\n")
   }
   if(x$formula[[3]] != "1") {
       cat("\nRegression Coefficients:\n",  sep = "")
       y = list()
       for(i in 1:length(x$coefficients))
       {
        if (x$effects[[i]] == "random_interc") 
            y[[i]] = summary(x$coefficients[[i]])
        else y[[i]] = x$coefficients[[i]][1:min(20, nrow(x$coef[[i]])),]
        names(y)[i] = names(x$coefficients)[i]
       }
       print(y)
       cat("\n")
   }
   invisible(x)

}
