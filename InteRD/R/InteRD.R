#estimate mean profile for each marker gene, given cell proportions
estimate.geneprofile<-function(bulk.data,gene.used,celltype.unique,cluster.identifier,prop.est)
{
    X<-t(sapply(1:length(gene.used),function (xx)
    {
        x_xx<-rep(0,length(celltype.unique))
        gene_xx<-gene.used[xx]
        bulk_xx<-bulk.data[gene_xx,]
        cell_type_index<-cluster.identifier[xx]
        prop_xx<-prop.est[,cell_type_index]
        #x_xx[cell_type_index]<-sum((bulk_xx*prop_xx))/sum(prop_xx^2)*1e5
        x_xx[cell_type_index]<-sum(bulk_xx)/sum(prop_xx)*1e5 #this formula is weird, I think the previous one is better
        x_xx
    }
    ))
    X
}

#'@title The InteRD1 estimate from reference ensemble
#'@description This function provides a reference-based deconvolution by resembling all estimated cell-type proportions based on each reference set.
#'@usage InteRD1(bulk.data,list_marker,cell_type_unique,comb_used,
#'lambda_option,tol=1e-06)
#'@param bulk.data The `ExpressionSet` object for a target bulk data.
#'@param list_marker A list of pre-specified marker genes corresponding to each cell type.
#'@param cell_type_unique A list of cell types. It should match the order in list.marker.
#'@param comb_used A list of pre-estimated cell type proportions based on different references.
#'@param lambda_option A sequence of values for the tuning parameter.
#'@param tol A tolerance value for convergence. The default is 1e-06
#'
#'@return A list containing estimated cell type proportions corresponding to each tuning value, named `est`;
#'and a sequence of goodness-of-fit values corresponding to each tuning value, named `metrics`.
#'The smaller the better; and a list of weights corresponding to each tuning value, named `weights_list`.
#'
#'@examples
#'##read data
#'library(InteRD)
#'readRDSFromWeb<-function(ref) {readRDS(gzcon(url(ref)))}
#'urlremote<-"https://github.com/chencxxy28/Data/raw/main/data_InteRD/"
#'pseudo.seger<-readRDSFromWeb(paste0(urlremote,"pseudo.seger.rds"))
#'comb<-readRDSFromWeb(paste0(urlremote,"comb_seger.rds"))
#'list_marker<-readRDSFromWeb(paste0(urlremote,"list_markerbaron20.rds"))
#'lambda_option<-0
#'cell_type_unique<-c("alpha","beta","delta","gamma")
#'InteRD1.output<-InteRD1(bulk.data =pseudo.seger,list_marker,
#'cell_type_unique,comb_used=comb,lambda_option,tol=1e-02)
#'InteRD1<-InteRD.predict.prop(InteRD.output=InteRD1.output)
#'
#'@export
#'
InteRD1<-function (bulk.data,list_marker,cell_type_unique,comb_used,lambda_option,tol=1e-06){
    # bulk.eset<-pseudo.seger
    # bulk_data<-(exprs(bulk.eset))
    # marker_gene_used<-unlist(list_marker)
    # gene_test<-setdiff(rownames(bulk_data),marker_gene_used)
    # bulk_data<-bulk_data[gene_test,]
    # exprs_data<-as.matrix(bulk_data)
    # pdata<-data.frame(sample=colnames(bulk_data))
    # fdata<-data.frame(genes=rownames(bulk_data))
    # rownames(pdata)<-colnames(bulk_data)
    # rownames(fdata)<-rownames(bulk_data)
    # pseudo.seger_test<-ExpressionSet(exprs_data,
    #                                  AnnotatedDataFrame(pdata),
    #                                  AnnotatedDataFrame(fdata))
    bulk.data_test<-testset(eset=bulk.data,list.marker=list_marker)

    #run reference free approach
    ct.sub<-cell_type_unique
    bulk.eset<-bulk.data
    bulk_matrix_raw<-(exprs(bulk.eset))
    find_zero_index<-which(rowSums(bulk_matrix_raw,na.rm=TRUE)*1e5==0)
    if(length(find_zero_index)>0)
    {
        bulk_nozero<-getCPM0(bulk_matrix_raw[-find_zero_index,])
    }else{
        bulk_nozero<-getCPM0(bulk_matrix_raw)
    }
    marker_all<-unlist(list_marker)
    #lambda_option<-c(seq(from=0,to=0.075,length=15),1,10,30,50,1000)
    #lambda_option<-c(1,10,30,50,1000)



    criterion<-NULL
    est_all<-list(NULL)
    weights_all<-list(NULL)

    marker_list_sub<-lapply(1:length(list_marker),function (xx)
    {
        marker_xx<-intersect(list_marker[[xx]],rownames(bulk_nozero)) #create new marker list
    }
    )
    names(marker_list_sub)<-ct.sub
    gene_used<-unlist(marker_list_sub) #vectorize the values
    cluster_identifier<-unlist(lapply(1:length(marker_list_sub),function (xx)
    {
        rep(xx,length(marker_list_sub[[xx]])) #create the identifier to discriminate which cell type are markers coming from
    }
    ))


    ll<-1
    for (lambda in lambda_option)
    {
        num_est<-length(comb_used)
        prop_old<-0
        for (i in 1:num_est)
        {
            prop_old<-prop_old+comb_used[[i]]/num_est
        }
        iter<-1
        weights_old<-rep(1/num_est,num_est)

        repeat{
            X<-t(sapply(1:length(gene_used),function (xx)
            {
                x_xx<-rep(0,length(ct.sub))
                gene_xx<-gene_used[xx]
                bulk_xx<-bulk_nozero[gene_xx,]
                Y_aug<-c(bulk_xx,rep(0,length(ct.sub)))

                cell_type_index<-cluster_identifier[xx]
                #prop_xx<-prop_old[,cell_type_index]
                penalty_matrix<-diag(1,length(ct.sub))
                penalty_matrix[cell_type_index,cell_type_index]<-0
                #colnames(penalty_matrix)<-ct.sub
                X_aug<-rbind(prop_old,lambda*penalty_matrix)
                fit<-nnls(A=(X_aug),B=Y_aug)
                fit$X*1e5
                #x_xx[cell_type_index]<-sum((bulk_xx*prop_xx))/sum(prop_xx^2)*1e5
                #x_xx[cell_type_index]<-sum(bulk_xx)/sum(prop_xx)*1e5 #this formula is weird, I think the previous one is better
            }
            ))

            Y_all<-c(bulk_nozero[gene_used,])*1e5

            X_all<-rep()
            for(i in 1:ncol(bulk_nozero))
            {
                Matrix_i<-matrix(0,nrow=length(gene_used),ncol=num_est)
                for (j in 1:num_est)
                {
                    Matrix_i[,j]<-X%*%(comb_used[[j]][i,])
                }
                X_all<-rbind(X_all,Matrix_i)
            }
            fit<-nnls(A=X_all,B=Y_all)
            weights_new<-fit$X/sum(fit$X)

            prop_new<-0
            for (i in 1:num_est)
            {
                prop_new_i<-weights_new[i]*comb_used[[i]]
                prop_new<-prop_new+prop_new_i
            }

            if(sum(abs(weights_new-weights_old))<tol)
            {break}else{
                prop_old<-prop_new
                weights_old<-weights_new
                iter<-iter+1
            }
        }
        criterion_sub<-criteria_onegroup(bulk_data=bulk.data_test, prop_used=prop_new)[2]
        criterion<-c(criterion,criterion_sub)
        est_all[[ll]]<-prop_new
        weights_all[[ll]]<-weights_new
        #print(ll)
        ll<-ll+1
    }

    prop_est<-est_all[[which.min(criterion)]]
    list(est=est_all,metrics=criterion,weights_list=weights_all)
}


#'@title The InteRD2 estimate
#'@description This function provides a robust deconvolution framework to integrate information from scRNA-seq references,
#'marker genes, and prior biological knowledge.
#'@usage InteRD2(bulk.data,list_marker,cell_type_unique,comb_sampled,ave_est,ave_sd,
#'lambda_option,tol=0.0005)
#'@param bulk.data The `ExpressionSet` object for a target bulk data.
#'@param list_marker A list of pre-specified marker genes corresponding to each cell type.
#'@param cell_type_unique A list of cell types. It should match the order in list.marker.
#'@param comb_sampled A pre-specified cell type proportions for the target bulk data, which could be obtained from reference-based deconvolution approach.
#'@param ave_est A pre-specified population-level cell type proportions, which could be obtained from single-cell RNA-seq and external expression data from different studies, species, or data types
#'@param ave_sd  A pre-specified standard deviation for cell-type proportion estimation. The default is 1 for each cell type.
#'@param lambda_option A sequence of values for the tuning parameter.
#'@param tol A tolerance value for convergence. The default is 0.0005.
#'@return A list containing estimated cell type proportions corresponding to each tuning value, named `est`; and a sequence of goodness-of-fit values corresponding to each tuning value, named `metrics`. The smaller the better.
#'
#'@examples
#'##read data
#'library(InteRD)
#'readRDSFromWeb<-function(ref) {readRDS(gzcon(url(ref)))}
#'urlremote<-"https://github.com/chencxxy28/Data/raw/main/data_InteRD/"
#'pseudo.seger<-readRDSFromWeb(paste0(urlremote,"pseudo.seger.rds"))
#'InteRD1<-readRDSFromWeb(paste0(urlremote,"InteRD1.rds"))
#'ave_est<-readRDSFromWeb(paste0(urlremote,"ave_est.rds"))
#'ave_sd<-readRDSFromWeb(paste0(urlremote,"ave_sd.rds"))
#'list_marker<-readRDSFromWeb(paste0(urlremote,"list_markerbaron20.rds"))
#'lambda_option<-0
#'cell_type_unique<-c("alpha","beta","delta","gamma")
#'lambda_option<-10e+05
#'InteRD2.output<-InteRD2(bulk.data=pseudo.seger,list_marker,cell_type_unique,
#'comb_sampled=InteRD1,ave_est,ave_sd,lambda_option=lambda_option,tol=0.01)
#'InteRD2<-InteRD.predict.prop(InteRD.output=InteRD2.output)
#'
#'@export
#'@import ggplot2
#'
InteRD2<-function (bulk.data,list_marker,cell_type_unique,comb_sampled,ave_est,ave_sd,lambda_option,tol=0.0005){
    # bulk.eset<-pseudo.seger
    # bulk_data<-(exprs(bulk.eset))
    # marker_gene_used<-unlist(list_marker)
    # gene_test<-setdiff(rownames(bulk_data),marker_gene_used)
    # bulk_data<-bulk_data[gene_test,]
    # exprs_data<-as.matrix(bulk_data)
    # pdata<-data.frame(sample=colnames(bulk_data))
    # fdata<-data.frame(genes=rownames(bulk_data))
    # rownames(pdata)<-colnames(bulk_data)
    # rownames(fdata)<-rownames(bulk_data)
    # pseudo.seger_test<-ExpressionSet(exprs_data,
    #                                  AnnotatedDataFrame(pdata),
    #                                  AnnotatedDataFrame(fdata))
    bulk.data_test<-testset(eset=bulk.data,list.marker=list_marker)

    #run reference free approach
    comb_sampled<-comb_sampled[,cell_type_unique]
    ct.sub<-cell_type_unique
    bulk.eset<-bulk.data
    bulk_matrix_raw<-(exprs(bulk.eset))
    find_zero_index<-which(rowSums(bulk_matrix_raw,na.rm=TRUE)*1e5==0)
    if(length(find_zero_index)>0)
    {
        bulk_nozero<-getCPM0(bulk_matrix_raw[-find_zero_index,])
    }else{
        bulk_nozero<-getCPM0(bulk_matrix_raw)
    }
    marker_genes<-unlist(list_marker) #match the marker gene
    #check marker gene availability for each cell types
    marker_list_sub_i<-lapply(1:length(list_marker),function (xx)
    {
        marker_xx<-intersect(list_marker[[xx]],rownames(bulk_nozero)) #create new marker list
    }
    )
    names(marker_list_sub_i)<-ct.sub
    gene_used<-unlist(marker_list_sub_i) #vectorize the values
    cluster_identifier<-unlist(lapply(1:length(marker_list_sub_i),function (xx)
    {
        rep(xx,length(marker_list_sub_i[[xx]])) #create the identifier to discriminate which cell type are markers coming from
    }
    ))
    #setdiff(unlist(list_marker),intersect(rownames(bulk_nozero),unlist(list_marker)))
    marker_all<-unlist(list_marker)
    #lambda_option<-c(seq(from=0,to=0.075,length=15),1,10,30,50,1000)
    #lambda_option<-c(1)
    group<-bulk.data@phenoData@data[["groups"]]
    criterion<-NULL
    est_all<-rep()
    for(lambda1 in lambda_option)
    {
        iter<-0
        prop_old<-matrix(1/length(ct.sub),nrow=ncol(bulk_matrix_raw),ncol=length(ct.sub))
        X<-t(sapply(1:length(gene_used),function (xx)
        {
            x_xx<-rep(0,length(ct.sub))
            gene_xx<-gene_used[xx]
            bulk_xx<-bulk_nozero[gene_xx,]
            cell_type_index<-cluster_identifier[xx]
            prop_xx<-prop_old[,cell_type_index]
            #x_xx[cell_type_index]<-sum((bulk_xx*prop_xx))/sum(prop_xx^2)*1e5
            x_xx[cell_type_index]<-sum(bulk_xx)/sum(prop_xx)*1e5 #this formula is weird, I think the previous one is better
            x_xx
        }
        ))
        rownames(X)<-gene_used
        lambda_adjust<-mean((bulk_nozero[gene_used,]*1e5-X%*%t(prop_old))^2)
        repeat{
            X<-t(sapply(1:length(gene_used),function (xx)
            {
                x_xx<-rep(0,length(ct.sub))
                gene_xx<-gene_used[xx]
                bulk_xx<-bulk_nozero[gene_xx,]
                cell_type_index<-cluster_identifier[xx]
                prop_xx<-prop_old[,cell_type_index]
                #x_xx[cell_type_index]<-sum((bulk_xx*prop_xx))/sum(prop_xx^2)*1e5
                x_xx[cell_type_index]<-sum(bulk_xx)/sum(prop_xx)*1e5 #this formula is weird, I think the previous one is better
                x_xx
            }
            ))
            rownames(X)<-gene_used

            prop_new<-t(sapply(1:ncol(bulk_nozero), function (x)
            {
                #each subject estimation
                Y_initial<-bulk_nozero[,x]
                zero_index<-which(Y_initial==0) #find genes with zero expression in current subject
                if (length(zero_index)>0)
                {
                    Y_initial<-Y_initial[-zero_index] #delate zero expressed genes !!!!!!!!!!!!check this!!!!!!!!!!!
                }
                #Y_initial<-Y_initial[Y_initial<quantile(Y_initial,0.99)] #remove the outliers
                gene_names<-names(Y_initial)
                marker_genes<-intersect(gene_names,marker_all) #match the marker gene
                #check marker gene availability for each cell types
                marker_list_sub_i<-lapply(1:length(list_marker),function (xx)
                {
                    marker_xx<-intersect(list_marker[[xx]],marker_genes) #create new marker list
                }
                )
                names(marker_list_sub_i)<-ct.sub
                gene_used<-unlist(marker_list_sub_i) #vectorize the values
                cluster_identifier<-unlist(lapply(1:length(marker_list_sub_i),function (xx)
                {
                    rep(xx,length(marker_list_sub_i[[xx]])) #create the identifier to discriminate which cell type are markers coming from
                }
                ))
                find_zero_counts<-which(table(cluster_identifier)==0)

                #construct Y and corresponding augmented iterms (major change)
                Y<-Y_initial[gene_used]*1e5
                lambda_adjust1<-lambda_adjust*lambda1

                ave_truep<-as.matrix(comb_sampled)[x,]*sqrt(lambda_adjust1)
                ave_truep2<-ave_est*sqrt(lambda_adjust)/ave_sd

                X<-X[gene_used,]
                X_aug<-sqrt(lambda_adjust1)*diag(1,length(ct.sub))
                X_aug2<-sqrt(lambda_adjust)*diag(1,length(ct.sub))/ave_sd

                Y_aug<-ave_truep
                y_aug2<-ave_truep2
                #Y_aug<-c(sapply(1:length(combo), function (xx) {sqrt(lambda_adjust*weight[xx])*(combo[[xx]][x,])}))
                Y_comb<-c(Y,Y_aug,y_aug2)

                #X_aug<-do.call(rbind,lapply(1:length(combo), function (xx) {sqrt(lambda_adjust*weight[xx])*diag(1,length(ct.sub))}))
                X_comb<-rbind(X,X_aug,X_aug2)
                #heatmap(X)
                ##construct the constrain matrix
                E_used<-rep(1,length(ct.sub))
                F_used<-1
                G_used<-diag(1,length(ct.sub))
                H_used<-rep(0,length(ct.sub))
                #fit<-lsei(A=X,B=Y,E=E_used,F=F_used,G=G_used,H=H_used)
                #fit$X
                fit<-nnls(A=X_comb,B=Y_comb)

                #calcualte GCV values
                #x_x<-t(X)%*%X
                #inverse_term<-solve(x_x+sum(lambda_adjust*weight))
                #P_lambda<-X%*%inverse_term%*%t(X)
                #penalty_1<-(1-(tr(P_lambda)-1)/length(Y))^2
                #penalty_2<-
                #gcv_values<-(Y%*%((diag(1,nrow(P_lambda))-P_lambda)^2)%*%Y)/penalty_1+
                fit$X/sum(fit$X)
                #print(x)
            })
            )
            if(mean(abs(prop_new[,1:length(ct.sub)]-prop_old))<tol | iter>1000)
            {
                break
            }else{
                prop_old<-prop_new[,1:length(ct.sub)]
                lambda_adjust<-mean((bulk_nozero[gene_used,]*1e5-X%*%t(prop_old))^2)
                iter<-iter+1
            }
        }
        est_meta_all<-list(prop_new)
        criterion_sub<-criteria_onegroup(bulk_data=bulk.data_test, prop_used=prop_new)[2]
        criterion<-c(criterion,criterion_sub)
        est_all<-append(est_all,est_meta_all)
        #print(ll)
    }

    prop_est<-est_all[[which.min(criterion)]]
    list(est=est_all,metrics=criterion)
}


#'@title A reference-free deconvolution estimate
#'@description This function provides a reference-free deconvolution estimate, given a list of marker genes
#'@usage Ref_free(bulk.data,list_marker,cell_type_unique,tol=0.001)
#'@param bulk.data The `ExpressionSet` object for a target bulk data.
#'@param list_marker A list of pre-specified marker genes corresponding to each cell type.
#'@param cell_type_unique A list of cell types. It should match the order in `list.marker`.
#'@param tol A tolerance value for convergence. The default is 0.001.
#'@return The estimated cell type proportions, named `est`; and a goodness-of-fit value, named `metrics`. The smaller the better.
#'
#'@examples
#'##read data
#'library(InteRD)
#'readRDSFromWeb<-function(ref) {readRDS(gzcon(url(ref)))}
#'urlremote<-"https://github.com/chencxxy28/Data/raw/main/data_InteRD/"
#'pseudo.seger<-readRDSFromWeb(paste0(urlremote,"pseudo.seger.rds"))
#'list_marker<-readRDSFromWeb(paste0(urlremote,"list_markerbaron20.rds"))
#'cell_type_unique<-c("alpha","beta","delta","gamma")
#'ref_free.output<-Ref_free(bulk.data=pseudo.seger,list_marker=list_marker,
#'cell_type_unique=cell_type_unique,tol=0.01) #make tol=0.001
#'reffree<-InteRD.predict.prop(InteRD.output=ref_free.output)
#'
#'@export
#'

Ref_free<-function (bulk.data,list_marker,cell_type_unique,tol=0.001){
    # bulk.eset<-pseudo.seger
    # bulk_data<-(exprs(bulk.eset))
    # marker_gene_used<-unlist(list_marker)
    # gene_test<-setdiff(rownames(bulk_data),marker_gene_used)
    # bulk_data<-bulk_data[gene_test,]
    # exprs_data<-as.matrix(bulk_data)
    # pdata<-data.frame(sample=colnames(bulk_data))
    # fdata<-data.frame(genes=rownames(bulk_data))
    # rownames(pdata)<-colnames(bulk_data)
    # rownames(fdata)<-rownames(bulk_data)
    # pseudo.seger_test<-ExpressionSet(exprs_data,
    #                                  AnnotatedDataFrame(pdata),
    #                                  AnnotatedDataFrame(fdata))
    bulk.data_test<-testset(eset=bulk.data,list.marker=list_marker)

    #run reference free approach
    ct.sub<-cell_type_unique
    bulk.eset<-bulk.data
    bulk_matrix_raw<-(exprs(bulk.eset))
    find_zero_index<-which(rowSums(bulk_matrix_raw,na.rm=TRUE)*1e5==0)
    if(length(find_zero_index)>0)
    {
        bulk_nozero<-getCPM0(bulk_matrix_raw[-find_zero_index,])
    }else{
        bulk_nozero<-getCPM0(bulk_matrix_raw)
    }
    marker_all<-unlist(list_marker)
    #lambda_option<-c(seq(from=0,to=0.075,length=15),1,10,30,50,1000)
    #lambda_option<-c(1)
    group<-bulk.data@phenoData@data[["groups"]]
    criterion<-NULL
    est_all<-list(NULL)
    lambda_option<-0

    for(ll in 1:length(lambda_option))
    {
        iter<-0
        prop_old<-matrix(1/length(ct.sub),nrow=ncol(bulk_matrix_raw),ncol=length(ct.sub))
        lambda<-lambda_option[ll]
        repeat{
            prop_new<-t(sapply(1:ncol(bulk_nozero), function (x)
            {
                #each subject estimation
                Y_initial<-bulk_nozero[,x]
                zero_index<-which(Y_initial==0) #find genes with zero expression in current subject
                if (length(zero_index)>0)
                {
                    Y_initial<-Y_initial[-zero_index] #delate zero expressed genes !!!!!!!!!!!!check this!!!!!!!!!!!
                }
                #Y_initial<-Y_initial[Y_initial<quantile(Y_initial,0.99)] #remove the outliers
                gene_names<-names(Y_initial)
                marker_genes<-intersect(gene_names,marker_all) #match the marker gene
                #check marker gene availability for each cell types
                marker_list_sub_i<-lapply(1:length(list_marker),function (xx)
                {
                    marker_xx<-intersect(list_marker[[xx]],marker_genes) #create new marker list
                }
                )
                names(marker_list_sub_i)<-ct.sub
                gene_used<-unlist(marker_list_sub_i) #vectorize the values
                cluster_identifier<-unlist(lapply(1:length(marker_list_sub_i),function (xx)
                {
                    rep(xx,length(marker_list_sub_i[[xx]])) #create the identifier to discriminate which cell type are markers coming from
                }
                ))
                find_zero_counts<-which(table(cluster_identifier)==0)

                #construct Y
                Y<-Y_initial[gene_used]*1e5
                lambda_adjust<-sum(max(Y)^2)*lambda
                # if(ll>=length(lambda_option)-4)
                # {
                #   ave_truep<-as.matrix(comb_sampled)[x,]
                # }else{
                #   ave_truep<-ave_est
                # }

                Y_aug<-c(1:length(ct.sub))*sqrt(lambda_adjust)
                #Y_aug<-ave_truep*sqrt(lambda_adjust)/ave_sd
                #Y_aug<-c(sapply(1:length(combo), function (xx) {sqrt(lambda_adjust*weight[xx])*(combo[[xx]][x,])}))
                Y_comb<-c(Y,Y_aug)

                #construct X matrix
                X<-t(sapply(1:length(gene_used),function (xx)
                {
                    x_xx<-rep(0,length(ct.sub))
                    gene_xx<-gene_used[xx]
                    bulk_xx<-bulk_nozero[gene_xx,]
                    cell_type_index<-cluster_identifier[xx]
                    prop_xx<-prop_old[,cell_type_index]
                    #x_xx[cell_type_index]<-sum((bulk_xx*prop_xx))/sum(prop_xx^2)*1e5
                    x_xx[cell_type_index]<-sum(bulk_xx)/sum(prop_xx)*1e5 #this formula is weird, I think the previous one is better
                    x_xx
                }
                ))

                X_aug<-sqrt(lambda_adjust)*diag(1,length(ct.sub))

                #X_aug<-do.call(rbind,lapply(1:length(combo), function (xx) {sqrt(lambda_adjust*weight[xx])*diag(1,length(ct.sub))}))
                X_comb<-rbind(X,X_aug)
                #heatmap(X)
                ##construct the constrain matrix
                E_used<-rep(1,length(ct.sub))
                F_used<-1
                G_used<-diag(1,length(ct.sub))
                H_used<-rep(0,length(ct.sub))
                #fit<-lsei(A=X,B=Y,E=E_used,F=F_used,G=G_used,H=H_used)
                #fit$X
                fit<-nnls(A=X_comb,B=Y_comb)

                #calcualte GCV values
                #x_x<-t(X)%*%X
                #inverse_term<-solve(x_x+sum(lambda_adjust*weight))
                #P_lambda<-X%*%inverse_term%*%t(X)
                #penalty_1<-(1-(tr(P_lambda)-1)/length(Y))^2
                #penalty_2<-
                #gcv_values<-(Y%*%((diag(1,nrow(P_lambda))-P_lambda)^2)%*%Y)/penalty_1+
                fit$X/sum(fit$X)
                #print(x)
            })
            )
            if(mean(abs(prop_new[,1:length(ct.sub)]-prop_old))<tol | iter>1000)
            {
                break
            }else{
                prop_old<-prop_new[,1:length(ct.sub)]
                iter<-iter+1
            }
        }
        est_meta_all<-prop_new
        criterion_sub<-criteria_onegroup(bulk_data=bulk.data_test, prop_used=prop_new)[2]
        criterion<-c(criterion,criterion_sub)
        est_all[[ll]]<-est_meta_all
        print(ll)
    }

    prop_est<-est_all[[which.min(criterion)]]
    list(est=est_all,metrics=criterion)
}



