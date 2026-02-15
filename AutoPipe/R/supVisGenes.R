#' @import  graphics
supVisGenes=function(groups_men, gene_matrix, method,TOP=1000,p_val=0.05,OR=3
                     ,threshold=2, TOP_Cluster=150){
  me_x=gene_matrix
  number_of_k=max(groups_men$cluster)
  ordert_genes=if(method=="PAMR"){
    mydata <- list(x=as.matrix(me_x),y=factor(groups_men$cluster), geneid=rownames(me_x),genenames=rownames(me_x))
    #training the data
    mytrain <-pamr::pamr.train(mydata)
    #leave 10 out cross validation (LOCV)
    mycv <- pamr::pamr.cv(mytrain,mydata, nfold=10)
    #plot to check different thresholds
    #pamr::pamr.plotcv(mycv)
    #confusion matrix to check the prediction error
    #pamr::pamr.confusion(mycv, threshold=threshold)
    #provides gene list and their scores for each class
    order_file=pamr::pamr.listgenes(mytrain, mydata, threshold=threshold)

    gene_lists=as.array(lapply(1:number_of_k, function(i){
      list_of_genes=as.data.frame(as.numeric(order_file[,i+1]))
      rownames(list_of_genes)=order_file[,1]
      names(list_of_genes)="Sig"
      list_of_genes$Test=1
      list_of_genes=list_of_genes[order(list_of_genes$Sig, decreasing = T), ]

      # vector_gene=c(rownames(list_of_genes[1:150, ]),
      #rownames(list_of_genes[(nrow(list_of_genes)-150):nrow(list_of_genes), ]))
      #
      # if (length(vector_gene)>1){return(me_x[vector_gene, ])}
      # else{return(NA)}

    }))
    ordert_genes<-as.array(lapply(1:number_of_k, function(i){
      list_of_genes<-gene_lists[[i]]
      if(nrow(list_of_genes)<TOP_Cluster){
        stop("Please Choose Another Threshold")
      }
      vector_gene=c(rownames(list_of_genes[1:TOP_Cluster, ]),rownames(list_of_genes[(nrow(list_of_genes)-TOP_Cluster):nrow(list_of_genes), ]))

      if (length(vector_gene)>1){return(me_x[vector_gene, ])}
      else{return(NA)}
    }))
    print("########################## Finish with PAMR ##################################################")
    return(list(ordert_genes,gene_lists))
  }
  ordert_genes=if(method=="SAM"){
    ###################################################################
    #
    # SAM_analysis.R -- Performs SAM analysis
    # This analysis was performed after silhouette based selection of
    # core 387 samples
    #
    ###################################################################
    sam.out<-siggenes::sam(as.matrix(me_x),cl=groups_men$cluster,gene.names=rownames(me_x),rand=123)
    graphics::plot(sam.out)
    sam.out
    ordert_genes=as.array(lapply(1:1, function(i){
      list_of_genes=data.frame(sam.out@p.value)
      list_of_genes$sam.out.p.value=as.numeric(list_of_genes$sam.out.p.value)
      list_of_genes$Test=1
      vector_gene=rownames(list_of_genes[list_of_genes$sam.out.p.value<0.00001, ])

      return(me_x[vector_gene, ])


    }))


  }
  ordert_genes=if(method=="EXReg"){

    mx=me_x

    #Filter genes to 10.000 Tops
    sd=as.data.frame(apply(mx,1, function(x){stats::var(x)}))
    sd=as.data.frame(sd[order(sd[,1], decreasing = T), ,drop = FALSE])
    #mx_TOP=t(mx_TOP)
    dim(mx)
    mx_TOP=as.matrix(mx[rownames(sd)[1:TOP], ])
    dim(mx_TOP)



    ####working

    order_file=as.data.frame(do.call(rbind,lapply(1:nrow(mx_TOP),function(i){

      #seperate gene
      gene=as.data.frame(mx_TOP[i, ], drop=F)
      colnames(gene)=rownames(mx_TOP)[i]
      #look for Cluster
      out=data.frame(do.call(cbind,lapply(1:number_of_k, function(i1){
        if(nrow(groups_men[groups_men$cluster==i1, ])!=0){
          #gene<-as.data.frame(gene)
          gene_x<-(gene)  ##### sometimes need to activate DONT KNOW WHY YET
          group=c(0)
          gene_x<-cbind(gene_x,group)
          gene_x[rownames(groups_men[groups_men$cluster==i1, ]),"group"]=1
          #pred <- prediction(gene[,1], gene$group)
          #auc.perf = performance(pred, measure = "auc")
          #AUC=as.numeric(auc.perf@y.values)

          model=stats::glm(gene_x[,"group"]~gene_x[,1], family = stats::binomial(link = "logit"))
          exp=as.numeric(stats::coef(model))[2]
          p_value=as.numeric(stats::coef(summary(model))[,4])[2]

          out1=as.data.frame(cbind(exp,p_value))
          names(out1)=c(paste("OR_Cluster",i1,sep=""),paste("P_value_Cluster",i1,sep=""))
          rownames(out1)=colnames(gene_x)[1]
          return(out1)
        }else{
          out1=as.data.frame(cbind(NA,NA))
          names(out1)=c(paste("OR_Cluster",i1,sep=""),paste("P_value_Cluster",i1,sep=""))
          rownames(out1)=names(gene)[1]
          return(out1)
        }
      })))
      return(out)
    })))


    stats::na.omit(order_file)
    lists_of_genes<-as.array(lapply(1:number_of_k, function(i){
      c=i
      isna_cluster=is.na(order_file[,((c-1)*2)+2])
      if(!(sum(!isna_cluster)==0)){
        list_genes<-order_file[,c(((c-1)*2)+1,((c-1)*2)+2)]
        list_genes<-list_genes[list_genes[,2]<p_val,]
        #order_file_s=order_file[order_file[,((c-1)*2)+2]<p_val,  ]
        list_genes<-list_genes[(list_genes[,1]>OR)|(list_genes[,1]<(-OR)),]
        #order_file_s2=order_file_s[order_file_s[,((c-1)*2)+1]>OR|order_file_s[,((c-1)*2)+1]<(-OR),  ]
        #order_file_s2=order_file_s2[order(order_file_s2[,((c-1)*2)+1], decreasing = T), ]
        list_genes<-list_genes[order(list_genes[,1],decreasing = T),]
        cluster_genes=rownames(list_genes)
        if(length(cluster_genes)==0)
          return(NA)
        else
          return(list_genes)
      }else{

        return(NA)
      }
    }))


    ordert_genes=as.array(lapply(1:number_of_k, function(i){
      c=i
      isna_cluster=is.na(order_file[,((c-1)*2)+2])
      if(!(sum(!isna_cluster)==0)){
        order_file_s=order_file[order_file[,((c-1)*2)+2]<p_val,  ]
        order_file_s2=order_file_s[order_file_s[,((c-1)*2)+1]>OR|order_file_s[,((c-1)*2)+1]<(-OR),  ]
        order_file_s2=order_file_s2[order(order_file_s2[,((c-1)*2)+1], decreasing = T), ]
        cluster_genes=rownames(order_file_s2)
        if(length(cluster_genes)==0)
          return(NA)
        else
          return(mx[cluster_genes, ])
      }else{

        return(NA)
      }
    }))

    remove_from_file<-do.call(rbind,lapply(1:number_of_k, function(i){
      if(is.na(ordert_genes[[i]])){
        return(F)
      }else{
        return(T)
      }
    }))

    index<-remove_from_file[,1]

    ordert_genes<-ordert_genes[index]

    return(list(ordert_genes,lists_of_genes))
    print("########################## Finish with EXReg ##################################################")
  }


}
