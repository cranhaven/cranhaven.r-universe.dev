### R code from vignette source 'ReproducingKN2020.Rnw'

###################################################
### code chunk number 1: ReproducingKN2020.Rnw:101-104
###################################################
library(apc)
data <- data.loss.XL()
data$response[,1:8]


###################################################
### code chunk number 2: ReproducingKN2020.Rnw:116-118
###################################################
apc.fit.table(data,"log.normal.response")[,c(1,2,6,7)]
apc.fit.table(data,"log.normal.response","AC")[,c(1,2,6,7)]


###################################################
### code chunk number 3: ReproducingKN2020.Rnw:122-131
###################################################
table.APC <- apc.fit.table(data,"log.normal.response")
table.AC  <- apc.fit.table(data,"log.normal.response","AC")
Table41 <- matrix(NA,nrow=3,ncol=6)
Table41[1:3,1:2]  <- table.APC[c(1,3,5),1:2]
Table41[2:3,3:4]  <- table.APC[c(3,5),6:7]
Table41[3,5:6]    <- table.AC[2,6:7]
rownames(Table41) <- c("apc","ac","ad")
colnames(Table41) <- c("-2logL","df","F_sup,apc","p","F_sup,ac","p")
Table41


###################################################
### code chunk number 4: ReproducingKN2020.Rnw:145-147
###################################################
fit <- apc.fit.model(data,"log.normal.response","AC")
fit$coefficients.canonical


###################################################
### code chunk number 5: ReproducingKN2020.Rnw:155-158
###################################################
apc.identify(fit)$coefficients.dif[,1:2]
fit$s2
fit$RSS


###################################################
### code chunk number 6: ReproducingKN2020.Rnw:174-177
###################################################
forecast    <- apc.forecast.ac(fit,quantiles=0.995)
forecast$response.forecast.coh
forecast$response.forecast.all


###################################################
### code chunk number 7: ReproducingKN2020.Rnw:184-188
###################################################
CL.fit <- apc.fit.model(data,"od.poisson.response","AC")
CL.forecast    <- apc.forecast.ac(CL.fit,quantiles=0.995)
CL.forecast$response.forecast.coh
CL.forecast$response.forecast.all


###################################################
### code chunk number 8: ReproducingKN2020.Rnw:199-200
###################################################
m.cum   <- triangle.cumulative(data)


###################################################
### code chunk number 9: ReproducingKN2020.Rnw:212-215
###################################################
library(ChainLadder)
#BS <- BootChainLadder(m.cum, R = 10^3, process.distr=c("od.pois"))
#summary(BS)


###################################################
### code chunk number 10: ReproducingKN2020.Rnw:220-250
###################################################
Table_4_3   <- matrix(NA,nrow=20,ncol=9)
rownames(Table_4_3) <- c(as.character(2:20),"total")
col.3   <- c("Res","se/Res","99.5%/Res")
colnames(Table_4_3) <- c(col.3,col.3,col.3)
#   Table 4.3, part I, log normal Chain Ladder
forecast.coh  <- forecast$response.forecast.coh
forecast.all  <- forecast$response.forecast.all
Table_4_3[1:19,1]   <- forecast.coh[,1]
Table_4_3[1:19,2:3] <- forecast.coh[,c(2,5)]/forecast.coh[,1]
Table_4_3[20,1]     <- forecast.all[,1]
Table_4_3[20,2:3]   <- forecast.all[,c(2,5)]/forecast.all[,1]
#   Table 4.3, part II, standard Chain Ladder
CL.forecast.coh <- CL.forecast$response.forecast.coh 
CL.forecast.all <- CL.forecast$response.forecast.all
Table_4_3[1:19,4]   <- CL.forecast.coh[,1]
Table_4_3[1:19,5:6] <- CL.forecast.coh[,c(2,6)]/CL.forecast.coh[,1]
Table_4_3[20,4]     <- CL.forecast.all[,1]         
Table_4_3[20,5:6]   <- CL.forecast.all[,c(2,6)]/CL.forecast.all[,1]
#   Table 4.3, part III, bootstrap
#sum.bs.B <- summary(BS)$ByOrigin
#sum.bs.T <- summary(BS)$Totals
#qua.bs.B <- quantile(BS, c(0.995))$ByOrigin
#qua.bs.T <- quantile(BS, c(0.995))$Totals
#Table_4_3[1:19,7]   <- sum.bs.B[2:20,3]
#Table_4_3[1:19,8]   <- sum.bs.B[2:20,4]/sum.bs.B[2:20,3]
#Table_4_3[1:19,9]   <- qua.bs.B[2:20,1]/sum.bs.B[2:20,3]
#Table_4_3[20,7]     <- sum.bs.T[3,]
#Table_4_3[20,8]     <- sum.bs.T[4,]/sum.bs.T[3,]
#Table_4_3[20,9]     <- qua.bs.T[1,1]/sum.bs.T[3,]
#Table_4_3


###################################################
### code chunk number 11: ReproducingKN2020.Rnw:260-262
###################################################
data.1      <- apc.data.list.subset(data,0,0,0,1,0,0)
data.2      <- apc.data.list.subset(data,0,0,0,2,0,0)


###################################################
### code chunk number 12: ReproducingKN2020.Rnw:269-278
###################################################
#   Define panels
Table_4_4a <- matrix(NA,nrow=6,ncol=3)
Table_4_4a[1:5,1] <- 16:20
colnames(Table_4_4a) <- c("i","se/Res","99.5%/Res")
Table_4_4b <- Table_4_4c <- Table_4_4d <- Table_4_4a
Table_4_4b[1:5,1] <- 15:19
Table_4_4c[1:5,1] <- 14:18
Table_4_4e <- Table_4_4b
Table_4_4f <- Table_4_4c


###################################################
### code chunk number 13: ReproducingKN2020.Rnw:283-324
###################################################
#   Panel a. log normal Chain Ladder, no cut
for.coh <- forecast$response.forecast.coh
for.all <- forecast$response.forecast.all
Table_4_4a[1:5,2:3] <- for.coh[15:19,c(2,5)]/for.coh[15:19,1]
Table_4_4a[  6,2:3] <- for.all[     ,c(2,5)]/for.all[     ,1]
#   Panel b. log normal Chain Ladder, cut 1 calendar year
fit.1 <- apc.fit.model(data.1,"log.normal.response","AC")
forecast.1  <-  apc.forecast.ac(fit.1,quantiles=c(0.995))
for.1.coh <- forecast.1$response.forecast.coh
for.1.all <- forecast.1$response.forecast.all
Table_4_4b[1:5,2:3] <- for.1.coh[14:18,c(2,5)]/for.1.coh[14:18,1]
Table_4_4b[  6,2:3] <- for.1.all[     ,c(2,5)]/for.1.all[     ,1]
#   Panel c. log normal Chain Ladder, cut 2 calendar years
fit.2 <- apc.fit.model(data.2,"log.normal.response","AC")
forecast.2  <-  apc.forecast.ac(fit.2,quantiles=c(0.995))
for.2.coh <- forecast.2$response.forecast.coh
for.2.all <- forecast.2$response.forecast.all
Table_4_4c[1:5,2:3] <- for.2.coh[13:17,c(2,5)]/for.2.coh[13:17,1]
Table_4_4c[  6,2:3] <- for.2.all[     ,c(2,5)]/for.2.all[     ,1]
#   Panel d. Standard Chain Ladder, no cut
CL.for.coh <- CL.forecast$response.forecast.coh
CL.for.all <- CL.forecast$response.forecast.all
Table_4_4d[1:5,2:3] <- CL.for.coh[15:19,c(2,6)]/CL.for.coh[15:19,1]
Table_4_4d[  6,2:3] <- CL.for.all[     ,c(2,6)]/CL.for.all[     ,1]
#   Panel e. Standard Chain Ladder, cut 1 calendar year
CL.fit.1        <- apc.fit.model(data.1,"od.poisson.response","AC")
CL.forecast.1   <- apc.forecast.ac(CL.fit.1,quantiles=c(0.995))
CL.for.coh <- CL.forecast.1$response.forecast.coh
CL.for.all <- CL.forecast.1$response.forecast.all
Table_4_4e[1:5,2:3] <- CL.for.coh[14:18,c(2,6)]/CL.for.coh[14:18,1]
Table_4_4e[  6,2:3] <- CL.for.all[     ,c(2,6)]/CL.for.all[     ,1]
#   Panel f. Standard Chain Ladder, cut 2 calendar years
CL.fit.2        <- apc.fit.model(data.2,"od.poisson.response","AC")
CL.forecast.2   <- apc.forecast.ac(CL.fit.2,quantiles=c(0.995))
CL.for.coh <- CL.forecast.2$response.forecast.coh
CL.for.all <- CL.forecast.2$response.forecast.all
Table_4_4f[1:5,2:3] <- CL.for.coh[13:17,c(2,6)]/CL.for.coh[13:17,1]
Table_4_4f[  6,2:3] <- CL.for.all[     ,c(2,6)]/CL.for.all[     ,1]
#   Combine table
rbind(cbind(Table_4_4a,Table_4_4b,Table_4_4c),
      cbind(Table_4_4d,Table_4_4e,Table_4_4f))


###################################################
### code chunk number 14: ReproducingKN2020.Rnw:334-367
###################################################
Bartlett <- function(data,model.family,model.design,s.1,s.2,s.3=NULL)
#   data is an apc.data.list
#   s are subset indices, that is sets of six numbers
{   fit.model <- function(data,model.family,model.design,s)
    {   data.s <- apc.data.list.subset(data,s[1],s[2],s[3],s[4],s[5],s[6],
                        suppress.warning=TRUE)
        fit <- apc.fit.model(data.s,model.family,model.design)
        dev <- fit$deviance
        if(model.family=="log.normal.response")
            dev <- fit$RSS
        return(list(dev=dev, df=fit$df.residual))    
    }
    fit <- fit.model(data,model.family,model.design,s.1)    
    dev <- fit$dev
     df <- fit$df
    fit <- fit.model(data,model.family,model.design,s.2)
    dev <- c(dev,fit$dev)
     df <- c( df,fit$df )
    m <- 2 
    if(!is.null(s.3))
    {   fit <- fit.model(data,model.family,model.design,s.3)
        dev <- c(dev,fit$dev)
         df <- c( df,fit$df )
        m <- 3
    }     
    dev.<- sum(dev)
     df.<- sum(df)
    LR  <- df.*log(dev./df.)-sum(df*log(dev/df))
    C   <- 1+(1/3/(m-1))*(sum(1/df)-1/df.)
    t   <- LR/C
    p   <- pchisq(LR/C,m-1,0,FALSE)
    return(list(t=t,p=p))
}


###################################################
### code chunk number 15: ReproducingKN2020.Rnw:371-415
###################################################
Ftest <- function(data,model.family,model.design,s.1,s.2,s.3=NULL)
#   data is an apc.data.list
#   s are subset indices, that is sets of six numbers
{   append <- function(data,model.design,s,v=NULL,d=NULL)
    {   data.s <- apc.data.list.subset(data,s[1],s[2],s[3],s[4],s[5],s[6],
                        suppress.warning=TRUE)
        index  <- apc.get.index(data.s)
        v1 <- index$response[index$index.data]
        d1 <- apc.get.design(index,model.design)$design
        if(is.null(v))  v <- v1 
        else            v <- c(v,v1) 
        if(is.null(d))  d <- d1 
        else
        {   d0 <- matrix(0,nrow(d),ncol(d1))
            d10 <- matrix(0,nrow(d1),ncol(d))
            d <- rbind(cbind(d,d0),cbind(d10,d1))
        }
        return(list(v=v,d=d))
    }
    a <- append(data,model.design,s.1)
    v <- a$v; d <- a$d
    a <- append(data,model.design,s.2,v,d)
    v <- a$v; d <- a$d
    if(!is.null(s.3))
    {   a <- append(data,model.design,s.3,v,d)
        v <- a$v; d <- a$d  }
    fit.R <- apc.fit.model(data,model.family,model.design)    
    if(model.family=="log.normal.response")
    {   fit.R$deviance <- fit.R$RSS
        fit.U <- glm.fit(d,log(v),family=gaussian(link = "identity"))
    }    
    if(model.family=="od.poisson.response")
        fit.U <- glm.fit(d,v,family=quasipoisson(link = "log"))
    dev.R <- fit.R$deviance
    dev.U <- fit.U$deviance
     df.R <- fit.R$df.residual
     df.U <- fit.U$df.residual        
    F <- (dev.R-dev.U)/(df.R-df.U)/(dev.U/df.U)
    p <- pf(F,df.R-df.U,df.U,lower.tail=FALSE)
#    #   reproducing typo
#    F <- (dev.R/df.R)/(dev.U/df.U)
#    p <- pf(F,df.R,df.U,lower.tail=FALSE)    
    return(list(F=F,p=p))
}


###################################################
### code chunk number 16: ReproducingKN2020.Rnw:420-448
###################################################
dim.names <- list(c("a","b","c"),c("LR/C","p","F","p","LR/C","p","F","p"))
Table_4_5 <- matrix(NA,nrow=3,ncol=8,dimnames=dim.names)
s1 <- c(0,0,0,0,0,14)
s2 <- c(0,0,0,0,6,0)
Bt <- Bartlett(data,"log.normal.response","AC",s1,s2)
Ft <-    Ftest(data,"log.normal.response","AC",s1,s2)
Table_4_5[1,1:4] <- c(Bt$t,Bt$p,Ft$F,Ft$p)
Bt <- Bartlett(data,"od.poisson.response","AC",s1,s2)
Ft <-    Ftest(data,"od.poisson.response","AC",s1,s2)
Table_4_5[1,5:8] <- c(Bt$t,Bt$p,Ft$F,Ft$p)
s1 <- c(0,0,0,10,0,0)
s2 <- c(0,0,10,0,0,10)
s3 <- c(0,0,0,0,10,0)
Bt <- Bartlett(data,"log.normal.response","AC",s1,s2,s3)
Ft <-    Ftest(data,"log.normal.response","AC",s1,s2,s3)
Table_4_5[2,1:4] <- c(Bt$t,Bt$p,Ft$F,Ft$p)
Bt <- Bartlett(data,"od.poisson.response","AC",s1,s2,s3)
Ft <-    Ftest(data,"od.poisson.response","AC",s1,s2,s3)
Table_4_5[2,5:8] <- c(Bt$t,Bt$p,Ft$F,Ft$p)
s1 <- c(0,0,0 ,6,0,0)
s2 <- c(0,0,14,0,0,0)
Bt <- Bartlett(data,"log.normal.response","AC",s1,s2)
Ft <-    Ftest(data,"log.normal.response","AC",s1,s2)
Table_4_5[3,1:4] <- c(Bt$t,Bt$p,Ft$F,Ft$p)           
Bt <- Bartlett(data,"od.poisson.response","AC",s1,s2)
Ft <-    Ftest(data,"od.poisson.response","AC",s1,s2)
Table_4_5[3,5:8] <- c(Bt$t,Bt$p,Ft$F,Ft$p)
Table_4_5


