eic.pred <-
function(eic.rec, known.mz, mass.matched=NA, to.use=10, do.plot=FALSE, match.tol.ppm=5, do.grp.reduce=TRUE, remove.bottom=5, max.fpr=0.3, min.tpr=0.8)
{
    
    if(do.grp.reduce)
    {
        q<-eic.qual(eic.rec, known.mz, match.tol.ppm=match.tol.ppm)
        grp.names<-substr(rownames(q), 1, 14)
        uniq.names<-unique(grp.names)
        sel<-rep(0, nrow(q))
        for(i in 1:length(uniq.names))
        {
            this.grp<-which(grp.names == uniq.names[i])
            this.auc<-q[this.grp, 4]
            sel[this.grp[which(this.auc == max(this.auc))[1]]]<-1
        }
        eic.rec<-eic.rec[,c(1, 2, which(sel==1)+2)]
    }
    
    if(remove.bottom>0)
    {
        q<-eic.qual(eic.rec, known.mz, match.tol.ppm=match.tol.ppm)
        eic.rec<-eic.rec[,c(1,2,which(rank(q[,4])>remove.bottom)+2)]
    }
    
    if(do.plot)
    {
        #oldpar <- par(no.readonly = TRUE)
        #on.exit(par(oldpar))
        #par(mfrow=c(2,3))
    }
    
    if(is.na(mass.matched[1]))
    {
        y<-mass.match(x=eic.rec[,2], known.mz=known.mz, match.tol.ppm=match.tol.ppm)
    }else{
        y<-mass.matched
    }
    
    X<-eic.rec[,3:ncol(eic.rec)]
    
    sel.1<-which(y==1)
    sel.0<-which(y==0)
    sel.0<-sample(sel.0, length(sel.1), replace=FALSE)
    
    X.1<-X[c(sel.1, sel.0),]
    y.1<-y[c(sel.1, sel.0)]
    
    sel.2<-sample(length(y.1), round(length(y.1)/3), replace=FALSE)
    X.2<-X.1[sel.2,]
    y.2<-y.1[sel.2]
    
    X.1<-X.1[-sel.2,]
    y.1<-y.1[-sel.2]
    
    dat.1<-data.frame(y.1, X.1)
    dat.2<-data.frame(y.2, X.2)
    colnames(dat.2)[1]<-"y.1"
    
    d<-new("list")
    
    l<-gbm(y.1~., data=dat.1, distribution="adaboost", n.trees=500)
    d[[1]]<-summary(l,plotit=F)
    b<-randomForest(X.1, as.factor(y.1))
    d[[2]]<-b$importance
    d[[2]]<-data.frame(rownames(d[[2]]), d[[2]])
    d[[2]]<-d[[2]][order(-d[[2]][,2]),]
    all.d<-d
    
    all.to.use<-c(ncol(X.1),1:to.use)
    X.1.0<-X.1
    y.1.0<-y.1
    X.2.0<-X.2
    y.2.0<-y.2
    
    all.rec<-new("list")
    
    for(n in 1:2)
    {
        d<-all.d[[n]]
        rrr<-matrix(0, ncol=8, nrow=to.use+1)
        colnames(rrr)<-c("adaboost fit", "adaboost predict", "logistic fit", "logistic predict", "svm fit", "svm predict", "RF fit", "RF predict")
        rownames(rrr)<-c("all", 1:to.use)
        
        for(k in 1:length(all.to.use))
        {
            to.use<-all.to.use[k]
            
            sel.vars<-rownames(d)[1:to.use]
            #		message(sel.vars)
            
            X.1<-X.1.0[,colnames(X.1.0) %in% sel.vars]
            X.2<-X.2.0[,colnames(X.2.0) %in% sel.vars]
            dat.1<-data.frame(y.1, X.1)
            dat.2<-data.frame(y.2, X.2)
            colnames(dat.2)[1]<-"y.1"
            if(to.use == 1) colnames(dat.2)[2]<-"X.1"
            
            #		message("adaboost")
            l<-gbm(y.1~., data=dat.1, distribution="adaboost", n.trees=500)
            f<-l$fit
            f<-1*(f>median(f))
            #		message(table(y.1, f))
            z<-l$fit
            r<-rocs.x(z[y.1==0], z[y.1==1], FDR.cut=1, n.perm=1, do.plot=FALSE)
            rrr[k,1]<-r$fcauc
            z2<-predict(l, newdata=dat.2, n.trees=500)
            r2<-rocs.x(z2[y.2==0], z2[y.2==1], FDR.cut=1, n.perm=1, do.plot=FALSE)
            rrr[k,2]<-r2$fcauc
            pred<-prediction(z2, y.2)
            perf <- performance( pred, "tpr", "fpr" )
            
            #		message("logtistic regression")
            l<-glm(y.1 ~ ., family=binomial, data=dat.1)
            f<-l$fitted
            f<-1*(f>median(f))
            #		message(table(y.1, f))
            z<-	l$fitted
            r<-rocs.x(z[y.1==0], z[y.1==1], FDR.cut=1, n.perm=1, do.plot=FALSE)
            rrr[k,3]<-r$fcauc
            z2<-predict(l, newdata=dat.2)
            r2<-rocs.x(z2[y.2==0], z2[y.2==1], FDR.cut=1, n.perm=1, do.plot=FALSE)
            rrr[k,4]<-r2$fcauc
            pred<-prediction(z2, y.2)
            perf <- performance( pred, "tpr", "fpr" )
            
            
            #		message("svm")
            l<-svm(y.1~., data=dat.1, kernel="radial")
            f<-as.numeric(l$fitted)
            f<-1*(f>median(f))
            #		message(table(y.1, f))
            z<-l$fitted
            r<-rocs.x(z[y.1==0], z[y.1==1], FDR.cut=1, n.perm=1, do.plot=FALSE)
            rrr[k,5]<-r$fcauc
            z2<-predict(l, newdata=dat.2)
            r2<-rocs.x(z2[y.2==0], z2[y.2==1], FDR.cut=1, n.perm=1, do.plot=FALSE)
            rrr[k,6]<-r2$fcauc
            pred<-prediction(z2, y.2)
            perf <- performance( pred, "tpr", "fpr" )
            
            #		message("Random Forest")
            if(to.use==1)
            {
                X.1<-matrix(X.1, ncol=1)
                colnames(X.1)<-"X.1"
            }
            l<-randomForest(X.1, as.factor(y.1))
            f<-l$predicted
            #		message(table(y.1, f))
            z<-l$votes[,2]
            r<-rocs.x(z[y.1==0], z[y.1==1], FDR.cut=1, n.perm=1, do.plot=FALSE)
            rrr[k,7]<-r$fcauc
            z2<-predict(l, newdata=dat.2, type="prob")[,2]
            r2<-rocs.x(z2[y.2==0], z2[y.2==1], FDR.cut=1, n.perm=1, do.plot=FALSE)
            rrr[k,8]<-r2$fcauc
            pred<-prediction(z2, y.2)
            perf <- performance( pred, "tpr", "fpr" )
            
        }
        
        
        all.rec[[n]]<-rrr
    }
    
    new.rec<-data.frame(all.rec[[1]][,c(2,4,6,8)], all.rec[[2]][,c(2,4,6,8)])
    method.sel<-apply(new.rec[-1,],2,max)
    method.sel<-which(method.sel==max(method.sel))[1]
    num.predictors<-which(new.rec[-1,method.sel]==max(new.rec[-1,method.sel]))[1]
    perf.column<-c(2,4,6,8,2,4,6,8)[method.sel]
    block.sel<-1
    if(method.sel>4) block.sel<-2
    
    X.0<-X
    y.0<-y
    
    sel.vars<-rownames(all.d[[block.sel]])[1:num.predictors]
    message(c("variables ranked by: ", c("adaboost","RF")[block.sel]))
    message(c("final classification method used: " , c("adaboost","logistic reg","SVM","RF","adaboost","logistic reg","SVM","RF")[method.sel]))
    message("using these variables: ")
    message(paste(sel.vars, collapse = " "))
    
    sel.1<-which(y==1)
    sel.0<-which(y==0)
    sel.0<-sample(sel.0, length(sel.1), replace=FALSE)
    
    X.1<-X.0[c(sel.1, sel.0),]
    y.1<-y[c(sel.1, sel.0)]
    X.1<-X.1[,colnames(X.1) %in% sel.vars]
    X<-X.0[,colnames(X.0) %in% sel.vars]  #all data points
    
    if(length(sel.vars)==1)
    {
        X.1<-matrix(X.1, ncol=1)
        colnames(X.1)<-"X.1"
        X<-matrix(X, ncol=1)
        colnames(X)<-"X.1"
    }
    X.2<-X[which(!(1:nrow(X) %in% c(sel.1, sel.0))),]  #data points not selected for the training
    
    dat.1<-data.frame(y.1, X.1)
    dat.2<-data.frame(y, X)  # now dat.2 is the full data
    colnames(dat.2)[1]<-"y.1"
    
    if(perf.column == 2)
    {
        l<-gbm(y.1~., data=dat.1, distribution="adaboost", n.trees=500)	
        z2<-predict(l, newdata=dat.2, n.trees=500)
    }else if(perf.column ==4){
        l<-glm(y.1 ~ ., family=binomial, data=dat.1)
        z2<-predict(l, newdata=dat.2)
    }else if(perf.column ==6){
        l<-svm(y.1~., data=dat.1, kernel="radial")
        z2<-predict(l, newdata=dat.2)
    }else if(perf.column ==8){
        l<-randomForest(X.1, as.factor(y.1))
        z.new<-predict(l, newdata=X.2, type="prob")[,2]
        z2<-rep(0,length(y))
        z2[c(sel.1, sel.0)]<-l$votes[,2]
        z2[which(!(1:nrow(X) %in% c(sel.1, sel.0)))]<-z.new
    }
    
    r2<-rocs.x(z2[y==0], z2[y==1], FDR.cut=1, n.perm=1, do.plot=FALSE)
    zz<-unique(z2)
    zz<-zz[order(zz)]
    if(median(z2[y==0]) >  median(z2[y==1])) zz<- -zz
    
    tps<-fps<-rep(NA, length(z2))
    for(i in 1:length(zz)) 
    {
        this.sel<-which(z2==zz[i])
        tps[this.sel] <- r2$tp[i+1]
        fps[this.sel] <- r2$fp[i+1]
    }
    
    chosen<-y*0
    chosen[y==1 & tps<=min.tpr]<-1
    chosen[y==0 & fps<=max.fpr]<-1
    
    names(all.d)<-names(all.rec)<-c("adaboost selection", "RF selection")
    if(do.plot)
    {   
        #oldpar <- par(no.readonly = TRUE)
        #on.exit(par(oldpar))
        #par(mfrow=c(2,2))
        
        pred<-prediction(z2, y)
        perf <- performance( pred, "tpr", "fpr" )
        plot(perf)
        boxplot(split(z2, y), main="split by hmdb match")
        boxplot(split(z2, chosen), main="split by selection")
        #boxplot(split(z2, list(chosen,y)), main="split by selection; left:non-HMDB, right;HMDB")
    }
    return(list(chosen=chosen, fpr=fps, tpr=tps, matched=y, pred.performance=all.rec, feature.rank.method=c("adaboost","RF")[block.sel], model=c("adaboost","logistic reg","SVM","RF","adaboost","logistic reg","SVM","RF")[method.sel], feature.importance=all.d, used.features=sel.vars, final.auc=r2$fcauc))
}
