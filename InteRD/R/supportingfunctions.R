#normalize
getCPM0 <- function(x, verbose = FALSE){
  if (is.null(dim(x))){
    if (verbose){
      message("Normalizing a vector instead of a matrix")
    }
    vec = as.matrix(x/sum(x))
    vec
  } else {
    cpm <- t(t(x)/apply(x,2,sum))
    cpm
  }
}

#create the test data set
testset<-function(eset,list.marker)
{
    bulk.eset<-eset
    #bulk_data<-(exprs(bulk.eset))
    marker_gene_used<-unlist(list.marker)
    gene_test<-setdiff(rownames(bulk.eset),marker_gene_used)
    bulk_data<-bulk.eset[gene_test,]
    bulk_data
}

#A criterion function to assess the performance of deconvolution
criteria_onegroup<-function (bulk_data, prop_used)
{
    message("calculate criteria")
    bulk_matrix_raw<-(exprs(bulk_data))
    find_zero_index<-which(rowSums(bulk_matrix_raw,na.rm=TRUE)*1e5==0)
    if(length(find_zero_index)>1)
    {
        bulk_nozero<-getCPM0(bulk_matrix_raw[-find_zero_index,])
    }else{
        bulk_nozero<-getCPM0(bulk_matrix_raw)
    }
    all_nozero_index<-which(apply(bulk_nozero,1,function (x) sum(x==0))==0)
    bulk_all_nozero<-bulk_nozero[all_nozero_index,]
    check_rowsum<-rowSums(bulk_all_nozero)
    bulk_all_nozero<-bulk_all_nozero[check_rowsum<quantile(check_rowsum,0.95) & check_rowsum>quantile(check_rowsum,0.15),]
    sigma_subject<-apply(bulk_all_nozero,1,sd)
    bulk_all_nozero<-bulk_all_nozero[sigma_subject!=0,]
    gene_validate<-rownames(bulk_all_nozero)


    prop_new<-as.matrix(prop_used)
    ##do training

    bulk_nozero_train<-bulk_all_nozero
    prop_new_train<-prop_new
    X_all<-t(sapply(1:length(gene_validate),function (xx)
    {
        gene_xx<-gene_validate[xx]
        bulk_xx<-bulk_nozero_train[gene_xx,]*1e5
        prop_xx<-prop_new_train
        fit<-nnls(A=prop_xx,B=bulk_xx)
        fit$X
    }
    ))
    rownames(X_all)<-gene_validate

    bulk_nozero_test<-bulk_all_nozero
    prop_new_test<-prop_new
    prop_new_withscaler<-t(sapply(1:ncol(bulk_nozero_test), function (x)
    {
        #each subject estimation
        Y_initial<-bulk_nozero_test[,x]
        zero_index<-which(Y_initial==0) #find genes with zero expression in current subject
        if(length(zero_index)>0)
        {
            Y_initial<-Y_initial[-zero_index] #delate zero expressed genes
        }
        genes_left<-names(Y_initial)

        #construct Y
        gene_used_now<-intersect(genes_left,gene_validate)
        Y<-Y_initial[gene_used_now]*1e5
        #Y<-Y[Y<quantile(Y,0.85) & Y>quantile(Y,0.15)] #remove the outliers
        gene_used_final<-names(Y)

        #construct X
        X<-X_all[gene_used_final,]

        values_final<-sort(abs(Y-X%*%(prop_new_test[x,])),decreasing = TRUE)[1:50]

        c(median(values_final),mean(values_final),sqrt(mean(values_final^2)))
        #print(x)
    })
    )
    apply(prop_new_withscaler,2,mean)
}

#extract estimated cell type proportions via InteRD1 and InteRD2
#'@title Extract the estimated proportions from InteRD
#'@description This function extract estimated cell type proportions via InteRD1 and InteRD2.
#'@usage InteRD.predict.prop(InteRD.output)
#'@param InteRD.output An object from InteRD1 or InteRD2.
#'@return Estimated cell type proportions from InteRD.
#'
#'@examples
#'##read data
#'library(InteRD)
#'readRDSFromWeb<-function(ref) {readRDS(gzcon(url(ref)))}
#'urlremote<-"https://github.com/chencxxy28/Data/raw/main/data_InteRD/"
#'InteRD1.output<-readRDSFromWeb(paste0(urlremote,"InteRD1.output.rds"))
#'lambda_option<-0
#'cell_type_unique<-c("alpha","beta","delta","gamma")
#'InteRD1<-InteRD.predict.prop(InteRD.output=InteRD1.output)
#'
#'@export
InteRD.predict.prop<-function(InteRD.output)
{
    criteria_values<-InteRD.output$metrics
    InteRD.output$est[[which.min(criteria_values)]]
}


#evaluation
#'@title Evaluation for estimated cell type proportions
#'@description Several evaluation metrics are provided, such as mean absolute deviance (`MAD`), Kendall-tau correlation coefficient (`Ken`), Pearson correlation coefficient (`Cor`), given true cell type proportions.
#'@usage evaluate(est.prop,true.prop)
#'@param est.prop The estimated cell type proportions.
#'@param true.prop The True cell type proportions
#'@return Cell-type level evaluations based on MAD, Ken, and Pearson (`cell.type.eva`), and overall evaluations based on averaged MAD, Ken, and Pearson (`all.eva`).
#'
#'@examples
#'##read data
#'library(InteRD)
#'readRDSFromWeb<-function(ref) {readRDS(gzcon(url(ref)))}
#'urlremote<-"https://github.com/chencxxy28/Data/raw/main/data_InteRD/"
#'pseudo.seger<-readRDSFromWeb(paste0(urlremote,"pseudo.seger.rds"))
#'true_p<-readRDSFromWeb(paste0(urlremote,"true_p.rds"))
#'SCDC_ENSEMBLE_MAD<-readRDSFromWeb(paste0(urlremote,"SCDC_ENSEMBLE_MAD_seger.rds"))
#'evaluate(SCDC_ENSEMBLE_MAD,true_p)$all.eva
#'@export

evaluate<-function(est.prop,true.prop)
{
    ct.mad<-apply(est.prop-true.prop,2,function (x) mean(abs(x)))
    all.mad<-mean(ct.mad)
    ct.ken<-sapply(1:ncol(est.prop),function(x)
    {
       cor(est.prop[,x],true.prop[,x],method = c("kendall"))
    }
        )
    all.ken<-mean(ct.ken)
    ct.cor<-unlist(sapply(1:ncol(est.prop),function(x)
    {
        cor(est.prop[,x],true.prop[,x])
    }
    )
    )
    all.cor<-mean(ct.cor)
    cell.type.eva<-data.frame(ct.mad=ct.mad,ct.ken=ct.ken,ct.cor=ct.cor)
    all.eva<-data.frame(all.mad=all.mad, all.ken=all.ken,all.cor=all.cor)
    return(list(cell.type.eva=cell.type.eva,all.eva=all.eva))
}

#to obtain population-level cell type proportions from a given subject-level cell type proportions
pop.ct.prop.subj<-function(subj.prop)
{
    apply(subj.prop,2,mean)
}

#to obtain population-level cell type proportions from single cell RNA-seq data
#'@title Calculate the population-level cell type proportions from a single-cell data.
#'@description Calculate population-level cell type proportions from single-cell data.
#'@usage pop.ct.prop.scRNA(scRNA,cluster="cluster",sample="sample",cell_type_unique)
#'@param scRNA The `ExpressionSet` object for single-cell data.
#'@param cluster The character string specifying the variable name for cell types. The default is "cluster".
#'@param sample The character string specifying the variable name for subject/samples. The default is "sample".
#'@param cell_type_unique A vector of cell types. It should match the order in list.marker.
#'@return The population-level cell type proportions (`pop.ct.prop`) and corresponding standard deviations (`pop.ct.sd`).
#'
#'@examples
#'##read data
#'library(InteRD)
#'readRDSFromWeb<-function(ref) {readRDS(gzcon(url(ref)))}
#'urlremote<-"https://github.com/chencxxy28/Data/raw/main/data_InteRD/"
#'seger<-readRDSFromWeb(paste0(urlremote,"segerstolpe.rds"))
#'cell_type_unique<-c("alpha","beta","delta","gamma")
#'ave_est<-pop.ct.prop.scRNA(scRNA=seger[["sc.eset.qc"]],
#'cell_type_unique=cell_type_unique)$pop.ct.prop
#'ave_est
#'
#'@export
pop.ct.prop.scRNA<-function(scRNA,cluster="cluster",sample="sample",cell_type_unique)
{
    # ct_ad<-unique(scRNA@phenoData@data[[cluster]])
    # sampleid<-unique(scRNA@phenoData@data[[sample]])
    #
    # sc_proportions<-sapply(1:length(unique(scRNA@phenoData@data[[sample]])),function (x)
    # {
    #     ct_x<-scRNA@phenoData@data[[cluster]][sampleid==sampleid[x]]
    #     table(ct_x)/sum(table(ct_x))
    # }
    # )

    sampleid<-scRNA@phenoData@data[[sample]]
    clusterid<-scRNA@phenoData@data[[cluster]]
    sc_proportions<-sapply(1:length(unique(scRNA@phenoData@data[[sample]])),function (x)
    {
        ct_x<-scRNA@phenoData@data[[cluster]][sampleid %in% unique(sampleid)[x] & (clusterid %in% cell_type_unique)]
        table(ct_x)/sum(table(ct_x))
    }
    )

    sc_proportions<-t(sc_proportions[cell_type_unique,])

    output.mean<-apply(sc_proportions,2,mean)
    output.sd<-apply(sc_proportions,2,sd)

    return(list(pop.ct.prop=output.mean,pop.ct.sd=output.sd))
}

