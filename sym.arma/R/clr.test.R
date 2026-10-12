clr.test <-
function(model1, model2)
{
   if(model1$family!=model2$family) 
       stop("two models not have the same distribution")
   if(length(model1$Y)!=length(model2$Y)) 
       stop("two models not have the data set")
   if(sum(model1$Y!=model2$Y)!=0) 
       stop("two models not have the data set")

   if(is.null(model1$xreg)==TRUE) p1 <- 0L
   else p1 <- NCOL(model1$xreg)
   if(is.null(model2$xreg)==TRUE) p2 <- 0L
   else p2 <- NCOL(model2$xreg)
   a = model1$np + model1$nq + p1
   b = model2$np + model2$nq + p2
   n.rest = abs(a-b)

   zvalue = 2*(model1$loglik - model2$loglik)
   pvalue <- 1-pchisq(abs(zvalue),df=n.rest)
   zvalue <- round(zvalue,digits=4)
   
   code <- NULL
   code[1:4] = c("")
   if(pvalue>=0 && pvalue<0.001)  code[5]=c("***")
   if(pvalue>=0.001 && pvalue<0.01)  code[5]=c("**")
   if(pvalue>=0.01 && pvalue<0.05)  code[5]=c("*")
   if(pvalue>=0.05 && pvalue<0.1)  code[5]=c(".")
   if(pvalue>=0.1 && pvalue<=1)  code[5]=c(" ")
   
   code=noquote(code)
   pvalue <- sprintf("%.4e",pvalue) 

   print(matrix(nrow=1, ncol=0, byrow=TRUE,dimnames = list(c("Conditional ratio test"))),collapse="") 
   print(noquote(matrix(c(sprintf("%.3f",model1$loglik),sprintf("%.3f",model2$loglik),n.rest,sprintf("%.3f",zvalue),pvalue,code),
         nrow=5, ncol=2, byrow=FALSE, dimnames = list(c("LogLik-1","LogLik-2","Df","Chisq","Pr(>Chisq)"),c("","")))))
   print(matrix(nrow=2, ncol=0, byrow=TRUE,dimnames = list(c("---","Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"))))    
}
