#' @title Converts MAF into mutation matrix.
#' @description The function `get_mut_matrix` converts mutation annotation file (MAF) format data into a mutations matrix.Then use the fisher exact test to select the geneset with higher mutation frequency in alive sample group.Finally return the higher mutation frequency matrix.
#' @param maffile Input mutation annotation file (MAF) format data. It must be an absolute path or the name relatived to the current working directory.
#' @param is.TCGA 	Is input MAF file from TCGA source. If TRUE uses only first 15 characters from Tumor_Sample_Barcode.
#' @param mut_fre A threshold value(zero as the default value). The genes with a given mutation frequency equal or greater than the threshold value are retained for the following analysis.
#' @param nonsynonymous Logical,tell if extract the non-synonymous somatic mutations (nonsense mutation, missense mutation, frame-shif indels, splice site, nonstop mutation, translation start site, inframe indels).
#' @param cut_Cox.pval The significant cut_off pvalue for the univariate Cox regression.
#' @param cut_HR The cut_off HR for the univariate Cox regression, uses to select the genes with survival benefit mutations.
#' @param sur A nx2 data frame of samples' survival data,the first line is samples' survival event and the second line is samples' overall survival.
#' @return The survival-related mutations matrix.
#' @importFrom utils read.delim
#' @importFrom maftools read.maf
#' @importFrom stats fisher.test
#' @export
#' @examples
#' #get the path of the mutation annotation file and samples' survival data
#' \donttest{maf<-system.file("extdata","data_mutations_extended.txt",package = "pathwayTMB")
#' sur_path<-system.file("extdata","sur.csv",package = "pathwayTMB")
#' sur<-read.csv(sur_path,header=TRUE,row.names = 1)
#' #perform the function 'get_mut_matrix'
#' mut_matrix<-get_mut_matrix(maffile=maf,mut_fre = 0.01,is.TCGA=FALSE,sur=sur)}
get_mut_matrix<-function(maffile,is.TCGA=TRUE,mut_fre=0,nonsynonymous = TRUE,cut_Cox.pval=1,cut_HR=1,sur){
  get_univarCox_result<-function(DE_path_sur){
    covariates<-colnames(DE_path_sur)[1:(length(DE_path_sur[1,])-2)]
    univ_formulas <- sapply(covariates,function(x) as.formula(paste('Surv(survival, event) ~', x)))
    univ_models <- lapply( univ_formulas, function(x){coxph(x, data =DE_path_sur)})

    univ_results <- lapply(univ_models,
                           function(x){
                             x <- summary(x)
                             p.value<-signif(x$wald["pvalue"], digits=2)
                             wald.test<-signif(x$wald["test"], digits=2)
                             beta<-signif(x$coef[1], digits=2);#coeficient beta
                             HR <-signif(x$coef[2], digits=2);#exp(beta)
                             HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                             HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)

                             res<-c(beta, HR, wald.test, p.value)
                             names(res)<-c("beta", "HR", "wald.test",
                                           "p.value")
                             return(res)
                             #return(exp(cbind(coef(x),confint(x))))
                           })
    res <- t(as.data.frame(univ_results, check.names = FALSE))
    result<-as.data.frame(res)
    return(result)
  }
  if(nonsynonymous){
    data<-as.data.frame(read.maf(maffile)@data)
  }else{data<-read.delim( maffile, header = TRUE, comment.char = '#', stringsAsFactors = FALSE )}
  if(is.TCGA){
    data_9<-data[,c(1,2,9,10,16)]
    for(i in 1:dim(data_9)[2]){
      data_9[,i]<-as.character(data_9[,i])
    }
    data_9[,5]<-substr(gsub(pattern = "\\-",replacement = "\\.",x =data_9[,5]),1,16)
  }else{
    data_9<-data[,c(1,2,10,11,17)]
  }
  samples<-unique(data_9[,5])
  genes<-unique(data_9[,1])
  freq_matrix<-matrix(0,length(genes),length(samples))
  rownames(freq_matrix)<-genes
  colnames(freq_matrix)<-samples
  for(i in 1:length(samples)){
    s1<-table(data_9[which(data_9[,5]==samples[i]),1])
    freq_matrix[names(s1),i]<-s1
  }
  a<-apply(freq_matrix,1,function(x){(length(which(x!=0))/length(x))})
  freq_matrix<-freq_matrix[which(a >= mut_fre),]
  #fisher.test
  mut_matrix<-freq_matrix
  inter<-intersect(rownames(sur),colnames(mut_matrix))
  mut_matrix<-mut_matrix[,inter]
  sur<-sur[inter,]
  #单因素cox筛选基因
  cox_data<-as.data.frame(cbind(t(mut_matrix),event=sur[,1],survival=sur[,2]))
  DEpathname<-colnames(cox_data)[-c(dim(cox_data)[2],dim(cox_data)[2]-1)]
  colnames(cox_data)<-c(paste0("a",1:length(mut_matrix[,1])),"event","survival")
  name2name<-cbind(pathname=DEpathname,na=paste0("a",1:length(mut_matrix[,1])))
  rownames(name2name)<-name2name[,2]
  res<-get_univarCox_result(cox_data)
  gene<-rownames(res)[which(res$p.value<cut_Cox.pval & res$HR<cut_HR)]
  genes<-name2name[gene,1]
  mut_matrix<-mut_matrix[genes,]
  return(mut_matrix)
}

#' @title Calculate the Pathway-based Tumor Mutational Burden.
#' @description The function `get_PTMB` uses to calculate the Pathway-based Tumor Mutational Burden (PTMB). PTMB is defined as pathway-based tumor mutational burden corrected by genes’ length and number.
#' @param freq_matrix The mutations matrix,generated by `get_mut_matrix`.
#' @param genesmbol The genes' length matrix,generated by `get_gene_length`.
#' @param path_mut_cutoff A threshold value(zero percent as the default value).Pathways with a given mutation frequency equal or greater than the threshold value are retained for the following analysis.
#' @param gene_path User input pathways geneset list.
#' @return Return the Pathway-based Tumor Mutational Burden matrix.
#' @export
#' @examples
#' #get the path of the mutation annotation file and samples' survival data
#' \donttest{maf<-system.file("extdata","data_mutations_extended.txt",package = "pathwayTMB")
#' sur_path<-system.file("extdata","sur.csv",package = "pathwayTMB")
#' sur<-read.csv(sur_path,header=TRUE,row.names = 1)
#' #perform the function 'get_mut_matrix'
#' mut_matrix<-get_mut_matrix(maffile=maf,mut_fre = 0.01,is.TCGA=FALSE,sur=sur)}
#' #perform the function `get_PTMB`
#' PTMB_matrix<-get_PTMB(freq_matrix=mut_matrix,genesmbol=genesmbol,gene_path=gene_path)
get_PTMB<-function(freq_matrix,genesmbol,path_mut_cutoff=0,gene_path){
  inter_genesymbol<-intersect(rownames(freq_matrix),names(genesmbol))
  inter_freq<-freq_matrix[inter_genesymbol,]
  inter_length<-genesmbol[inter_genesymbol]
  leng<-c()
  for(i in 1:length(inter_length)){
    a<-inter_length[[i]]
    leng<-c(leng,a)
  }
  inter_length<-as.data.frame(cbind(symbol=as.character(names(inter_length)),length=as.numeric(as.character((leng)))))
  rownames(inter_length)<-inter_length[,1]
  inter_length[,2]<-as.numeric(as.character(inter_length[,2]))
  path_TMB<-data.frame()
  for(i in 1:length(gene_path)){
    gene<-intersect(gene_path[[i]],rownames(inter_freq))
    if(length(gene)>1){
      b<-c()
      for(j in 1:length(gene)){
        a<-inter_freq[gene[j],]/(as.numeric(inter_length[gene[j],2])/1000000)
        b<-rbind(b,a)
      }
      c<-colSums(b)/length(gene_path[[i]])
    }
    else if(length(gene)==1){
      b<-inter_freq[gene,]/(as.numeric(inter_length[gene,2])/1000000)
      c<-b/length(gene_path[[i]])}
    else{c=rep(0,length(inter_freq[1,]));names(c)<-colnames(inter_freq)}
    path_TMB<-rbind(path_TMB,c)
  }
  colnames(path_TMB)<-colnames(inter_freq)
  rownames(path_TMB)<-names(gene_path)
  a<-apply(path_TMB,1,function(x){length(which(x!=0))})
  if(length(which(a>=(length(path_TMB[1,])*path_mut_cutoff)))>0){
    path_TMB_cutoff<-path_TMB[which(a>=(length(path_TMB[1,])*path_mut_cutoff)),]
  }
  else{path_TMB_cutoff<-path_TMB}
  return(path_TMB_cutoff)
}


#' @title Filter cancer-specific dysfunction pathways.
#' @description The function `get_final_signature` , using to filter cancer-specific dysfunction pathways (a potential marker for cancer prognostic and immunotherapy), is the main function of our analysis.
#' @param PTMB The pathway tumor mutation burden matrix,generated by`get_PTMB`.
#' @param sur A nx2 data frame of samples' survival data,the first line is samples' survival event and the second line is samples' overall survival.
#' @param pval_cutoff A threshold value (0.01 as the default value) to identify the differential PTMB pathway.
#' @importFrom stats wilcox.test
#' @importFrom stats median
#' @importFrom caret rfe
#' @importFrom randomForest randomForest
#' @importFrom caret predictors
#' @importFrom caret rfeControl
#' @importFrom caret rfFuncs
#' @importFrom survival Surv
#' @importFrom glmnet cv.glmnet
#' @importFrom stats coef
#' @importFrom survival survfit
#' @importFrom survminer surv_pvalue
#' @return Return the final PTMB signature,could be a potential marker for prognostic and immunotherapy prediction.
#' @export
#' @examples
#' #get the path of the mutation annotation file and samples' survival data
#' maf<-system.file("extdata","data_mutations_extended.txt",package = "pathwayTMB")
#' \donttest{sur_path<-system.file("extdata","sur.csv",package = "pathwayTMB")
#' sur<-read.csv(sur_path,header=TRUE,row.names = 1)
#' #perform the function 'get_mut_matrix'
#' mut_matrix<-get_mut_matrix(maffile=maf,mut_fre = 0.01,is.TCGA=FALSE,sur=sur)
#' #perform the function `get_PTMB`
#' PTMB_matrix<-get_PTMB(freq_matrix=mut_matrix,genesmbol=genesmbol,gene_path=gene_path)
#' set.seed(1)
#' final_character<-get_final_signature(PTMB=PTMB_matrix,sur=sur)}
get_final_signature<-function(PTMB,sur,pval_cutoff=0.01){
  inter<-intersect(colnames(PTMB),rownames(sur))
  if(length(inter)==0){
    stop("please input the same sample data.")
  }
  path_TMB_inter<-PTMB[,inter]
  sur<-sur[inter,]
  colnames(sur)<-c("event","survival")
  pvalue<-apply(path_TMB_inter,1,function(x){wilcox.test(as.numeric(x[which(sur[,1]==0)]),as.numeric(x[-which(sur[,1]==0)]))$p.value})
  #FC<-apply(path_TMB_inter[,which(sur[,1]==0)],1,mean)/apply(path_TMB_inter[,-which(sur[,1]==0)],1,mean)
  DE_path<-names(pvalue)[which(pvalue<pval_cutoff)]
  #DE_path<-names(pvalue)[which(pvalue<0.05)]
  DE_path_sur<-as.data.frame(t(rbind(path_TMB_inter[DE_path,],event=sur[,1])))
  rfProfile <- rfe(DE_path_sur[,1:(dim(DE_path_sur)[2]-1)],as.factor(DE_path_sur[,(dim(DE_path_sur)[2])]), sizes = c(1:(dim(DE_path_sur)[2]-1)),rfeControl = rfeControl(functions = rfFuncs,method = "cv"))
  final_character<-predictors(rfProfile)
  #lasso回归
  data3<-as.data.frame(DE_path_sur[,c(final_character,"event")])
  x = as.matrix(data3[,-dim(data3)[2]])
  sur[,1]<-as.double(sur[,1])
  sur[,2]<-as.double(sur[,2])
  surs<-Surv(time = sur[,2],event = sur[,1])
  lasso_fit<-cv.glmnet(x,surs,family="cox",type.measure = "deviance")
  coeff<-coef(lasso_fit,s=lasso_fit$lambda.min)
  final_character<-rownames(coeff)[which(as.numeric(coeff)!=0)]
  return(final_character)
}

