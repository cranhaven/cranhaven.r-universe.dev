hct_method_single_MC<-function(dummy, mu0, p, m, n, hct, alpha0, p1, ss, ntest, sampling.p=0.5)
#function(dummy, mu0, p, m, n, hct, alpha0, p1, ss, ntest, analysis, sampling.p=0.5)
{
        data=gen_data(mu0, p, m, n,p1 , sampling.p )

        d1=subset(data$x, data$y==1)
        d2=subset(data$x, data$y==-1)
        n1=nrow(d1)
        n2=nrow(d2)
        mean1=apply(d1,2,mean)
        mean2=apply(d2,2,mean)
        xsq1=apply(d1^2,2,sum)
        xsq2=apply(d2^2,2,sum)
        s1= xsq1-n1*mean1^2
        s2= xsq2-n2*mean2^2

        pool.var= (s1+s2)/(n-2)
        pool.sd= sqrt( pool.var )

        zscore= (mean1-mean2)/( pool.sd*sqrt(1/n1+1/n2))
        pvalue=2*pt(abs(zscore),df=n-2, lower.tail=F)

     pvaluecopy=pvalue

        pvalue_small=sort(pvaluecopy, method='quick')[1:ceiling(p*alpha0)]

        pvalue_threshold=hct(pvalue_small, p, n)

        weight=get_weight_zp(zscore, pvalue, pvalue_threshold)

        testdata=gen_data(mu0, p, m, ntest, p1, sampling.p=0.5)

        if (all(weight==0))
        {
                pred=rbinom(length(testdata$y), size=1, prob=p1)
        }
        else
        {
analysis=1
           if( analysis==1){
                 #option 1
                 mu0hat= dot( abs(weight), abs((mean1-mean2))/2) / (dot(weight, weight))
                 av.pool.var=mean( pool.var)

                 cl_cutoff= (1/2) * log((1-p1)/p1) * (av.pool.var/mu0hat)
                 pred=as.numeric((testdata$x %*% weight)>cl_cutoff)
           }
           else
           {
                #option 2
                muhat=(mean1-mean2)/2

                cl_cutoff= log((1-p1)/p1)  
                pred=as.numeric(( 2*  testdata$x %*% (abs(weight)*muhat/pool.var )) > cl_cutoff )
           } 

        }
 
        # convert (0,1) to (-1, 1)
        pred=pred*2-1
        
if (ss==F)
        c(mean(pred==testdata$y) )
else
        n1test=ntest/2
        c(mean(pred==testdata$y), mean(pred[1:n1test]==testdata$y[1:n1test]), mean(pred[(n1test+1):ntest]==testdata$y[(n1test+1):ntest]) )  
}
