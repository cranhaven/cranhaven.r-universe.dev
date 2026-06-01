#' @title scAnnotate
#'
#' @description Annotate cell type labels of test data using a trained mixture model from training data
#'
#' @param train A data frame of cell type label in the first column and a gene expression matrix where each row is a cell and each column is a gene from training data
#' @param test A data matrix where each row is a cell and each column is a gene from test data
#' @param distribution A character string indicates the distribution assumption on positive gene expression, which should be one of "normal"(default) or "dep". "dep" refers to depth measure, which is a non-parametric distribution estimation approach.
#' @param correction A character string indicates the batch effect removal, which should be one of "auto"(default), "seurat", or "harmony". "auto" will automatically select the batch effect removal to follow our suggestion. That uses Seurat for dataset with at most one rare cell population (at most one cell population less than 100 cells) and Harmony for dataset with at least two rare cell populations (at least two cell populations less than 100 cells).
#' @param screening A character string indicates the gene screening methods, which should be one of "wilcox"(default) or "t.test".
#' @param threshold A numeric number indicates the threshold used for probabilities to classify cells, which should be a number from "0"(default) to "1". If there's no probability higher than the threshold associated with a cell type, the cell will be labeled as "unassigned."
#' @param lognormalized A logical string indicates if both input data are log-normalized or raw matrix. TRUE (default) indicates input data are log-normalized, and FALSE indicates input data are raw data.
#'
#' @return A vector contain annotate cell type labels for test data
#'
#' @export
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom stats predict
#' @importFrom Seurat CreateSeuratObject FindVariableFeatures RunPCA ScaleData SCTransform PercentageFeatureSet LayerData
#' @importFrom SeuratObject LayerData
#' @examples
#' data(pbmc1)
#' data(pbmc2)
#' \dontrun{
#'   predict_label=scAnnotate(train=pbmc1,
#'                            test=pbmc2[,-1],
#'                            distribution="normal",
#'                            correction ="harmony",
#'                            screening ="wilcox",
#'                            threshold=0,
#'                            lognormalized=TRUE)
#'  }

scAnnotate=function(train,
                    test,
                    distribution=c("normal","dep"),
                    correction=c("auto","harmony","seurat"),
                    screening=c("wilcox","t.test"),
                    threshold=0,
                    lognormalized=TRUE){
  #parameter
  distribution=match.arg(distribution)
  screening=match.arg(screening)
  correction=match.arg(correction)

  #arrange the dataset with cell type
  train=train[order(train[,1]),]
  train_cellnames=names(table(train[,1]))

  if(correction=="auto"){
    t.ss=table(train[,1])
    if(sum(t.ss>=100)>(length(train_cellnames)-2)){
      correction="seurat"
    }else{
      correction="harmony"
    }
  }

  if(correction=="seurat"){
    correction="harmony"
    cat("\n !!! \n Switch to Harmony batch effect removal, Seurat batch effect removal
         is tempoarily disabled due to conflict introduced by its version 5 upgrade \n !!! \n")
  }
  if(F){
    #1.  Batch effect by Seurat
    align.matrix=br_seurat(train.xx = train[,-1],
                           test = test,
                           lognormalized=lognormalized)

    train=data.frame(yy=train[,1],
                         t(align.matrix[["train"]]))
    test=t(align.matrix[["test"]])

    #2. gene selection
    # Wilcox
    if(screening=="wilcox"){
      p_matrix=gs_wilcox(dat=train,cellnames = train_cellnames)
    }
    # t-test
    if(screening=="t.test"){
      p_matrix=gs_t(dat=train,cellnames = train_cellnames)
    }

    genes.m=unique(as.vector(p_matrix[1:50,]))

    train2=train[,c(1,which(colnames(train)%in%genes.m))]
    test2=test[,c(which(colnames(test)%in%genes.m))]

    #3. splite the train to two part to train the mixture parameter and weights from elastic net:
    #3.1 straitified sampling
    #80% of data are training(idx==0), 20% of data are test(idx==1).
    sample.eachtype=table(train2[,1])
    idx.matrix=matrix(NA,nrow = nrow(train2),ncol = 10)
    for(ii in 1:ncol(idx.matrix)){
      idx=vector()
      for(jj in 1:length(train_cellnames)){
        temp=vector()
        temp[1:sample.eachtype[jj]]=createFolds(train2[,1][1:sample.eachtype[jj]],k=5,list = F)
        idx=c(idx,temp)
      }# out for jj
      idx.matrix[,ii]=idx
    }

    idx.matrix[which(idx.matrix!=1)]=0


    response.matrix.list=list()

    for(ii in 1:ncol(idx.matrix)){
      idx=idx.matrix[,ii]
      train_1=train2[which(idx==1),]
      train_2_xx=train2[which(idx==0),-1]

      if(distribution=="normal"){
        #3.2 estimated mix model parameter for each celltype
        mix.d=mix_ln_pt(dat = train_1,cellnames=train_cellnames)

        #3.3 estimated the weight from elastic net
        #a)probability for train_2_xx
        px.matrix=probability_ln(test=train_2_xx,mix.d = mix.d)

        px.com.train2=px.matrix[,1,]
        for(jj in 2:length(train_cellnames)){
          temp=px.matrix[,jj,]
          px.com.train2=rbind(px.com.train2,temp)
        }
        colnames(px.com.train2)=rownames(train_2_xx)

        #b) probability for test2
        px.matrix.test=probability_ln(test=test2,mix.d = mix.d)

      }

      if(distribution=="dep"){
        #3.3 estimated the weight from elastic net
        #a)probability for train_2_xx
        px.matrix=qjm(dat=train_1,test=train_2_xx)

        px.com.train2=px.matrix[,1,]
        for(jj in 2:length(train_cellnames)){
          temp=px.matrix[,jj,]
          px.com.train2=rbind(px.com.train2,temp)
        }
        colnames(px.com.train2)=rownames(train_2_xx)

        #b) probability for test2
        px.matrix.test=qjm(dat=train_1,test=test2)
      }

      px.com.test=px.matrix.test[,1,]
      for(jj in 2:length(train_cellnames)){
        temp=px.matrix.test[,jj,]
        px.com.test=rbind(px.com.test,temp)
      }
      colnames(px.com.test)=rownames(test2)

      #c)pca
#     browser()
       obj.pc=CreateSeuratObject(counts = cbind(px.com.train2,px.com.test))
      obj.pc@meta.data$dat=c(rep("train",ncol(px.com.train2)),rep("test",ncol(px.com.test)))
      obj.pc=FindVariableFeatures(obj.pc)
      #VariableFeaturePlot(obj.pc)
      obj.pc=ScaleData(obj.pc)
      obj.pc=RunPCA(obj.pc,npcs = 20)

      pc.matrix= obj.pc@reductions$pca@cell.embeddings
      pc.train=pc.matrix[obj.pc@meta.data$dat=="train",]
      pc.test=pc.matrix[obj.pc@meta.data$dat=="test",]

      #3) fit elastic net with proability as predictor
      yy=as.matrix(train2[which(idx==0),1])
      #xx=t(px.com.train2)
      xx=pc.train
      ##########find the lambda and alpha for binary elastic net for each cell type from training dataset##########
      cv.multi=cv.glmnet(xx,yy,alpha=0.3,nfolds=3,family="multinomial",grouped = TRUE,type.measure = "class")
      lambda.select=cv.multi$lambda.1se

      ##########fit model##########
      model.ela=glmnet(xx,yy,alpha=0.3,family="multinomial",type.multinomial = "grouped",lambda = lambda.select)
      #x.test=t(px.com.test)
      x.test=pc.test
      response.matrix=predict(model.ela,x.test,type="response")
      response.matrix.list[[ii]]=response.matrix

    }


    all_response.matrix=do.call(cbind,response.matrix.list)
    all_response.matrix=array(all_response.matrix, dim=c(dim(response.matrix.list[[1]]),length(response.matrix.list)))
    mean.reponse.matrix=apply(all_response.matrix, c(1,2), mean)
    predict_label=train_cellnames[apply(mean.reponse.matrix,1,which.max)]

    #unassigned label
    max.prob=apply(mean.reponse.matrix,1,max)
    predict_label[which(max.prob<threshold)]="unassigned"
  }

  if(correction=="harmony"){
    align.matrix=br_harmony(train.xx = train[,-1],
                            test = test,
                            lognormalized=lognormalized)
    train=data.frame(yy=train[,1],
                         align.matrix[["train"]])
    test=align.matrix[["test"]]

    if(distribution=="normal"){
      predict_label=mix_ln(train= train,test = test,cellnames = train_cellnames)
    }
    if(distribution=="dep"){
      predict_label=mix_dep(train= train,test = test,cellnames = train_cellnames)
    }

  }

  return(predict_label)

}
