#'@name pvalue
#'@aliases pvalue
#'@title t-Test on Summary Dataframe
#'
#'@description This function calculate the significance (t-test) within groups of 'dataframe'
#'
#'@usage pvalue (dataframe, control, sigval)
#'
#'@param dataframe a summary dataframe of \code{\link{dfsummary}} output
#'@param control control group name
#'@param sigval pvalue cutoff for significance
#'
#' @details The 'dataframe' should be having similar format of \code{\link{dfsummary}} output. 'control' should be an element from the column just before 'label'. 'N', 'Mean', 'SD' and 'CV' columns in the 'dataframe' are used for calculating p value by t-test (one to one t-test with 'control' in that group). significant if pvalue is < 'sigval'. Different groups in 'dataframe' are evaluated separately (columns before label is used for grouping).
#'
#'@return A dataframe. New columns named 'pvalue' (p values of t-test.If the value is less than 0.001, then appear as "< 0.001") and 'significance' (yes if pvalue less than 'sigval') are attached to the left.
#'
#'@author A.A Palakkan
#'
#'@examples
#'## loading data
#'data(metafile384, rawdata384)
#'rawdata<-plate2df(data2plateformat(rawdata384,platetype = 384))
#'data_DF2<- dplyr::inner_join(rawdata,metafile384,by=c("row","col","position"))
#'result3 <- dfsummary(data_DF2,y = "value",
#'                     grp_vector = c("cell","compound","concentration"),
#'                     rm_vector = c("B", "drug2", "huh7"),
#'                     nickname = "",
#'                     rm = "FALSE", param = c(strict = "FALSE", cutoff = 40,n = 12))
#'
#'## eg:1 t-test on result3.
#'pvalue(result3,"C3",sigval=0.05)
#'
#'@keywords htest
#'
#'@importFrom magrittr %>%
#'@importFrom dplyr select
#'@importFrom stats pt
#'@export
#'
#'

pvalue<-function(dataframe,control,sigval){

# Initial setup ##
rowname <- newrowname <- NULL
zzz <- data.frame(stringsAsFactors= FALSE) ## An empty dataframe to dump result
fr<-data.frame(stringsAsFactors= FALSE) ## An empty dataframe to dump result

ctrlcolumn<-names(dataframe)[which(dataframe == control, arr.ind=TRUE)[, "col"]][1]
ctrlcolno<-which( colnames(dataframe)==ctrlcolumn )

if(length(which(dataframe[,ctrlcolno]==control))==1){

  df1<-dataframe
  df1$rowname<-rownames(df1)
  ctrlrows<- as.numeric(which(df1[,ctrlcolumn]==control))
  nonctrlrows<-as.numeric(which(df1[,ctrlcolumn]!=control))
  numb<-nlevels(as.factor(df1[,as.character(ctrlcolumn)]))-1

  df1$newrowname<-"NA"
  df1[ctrlrows,]$newrowname<-as.numeric(df1[ctrlrows,]$rowname)-numb
  df1[nonctrlrows,]$newrowname<-as.numeric(df1[nonctrlrows,]$rowname)+1

  rownames(df1)<-as.numeric(df1$newrowname)
  df1<-dplyr::select(df1,c(-rowname,-newrowname))
  df1 <- df1[ order(row.names(df1)), ]


    for (z in seq_along(rownames(df1))) {

      fr <- df1[z,]
      if(fr[1,ctrlcolno]==control){
        n1<-fr$N
        s1<-fr$SD
        m1<-fr$Mean
        pval<-"control "
        sig<-" "
      }else{

        if(fr[1,ctrlcolno]!=control){
          n2<-fr$N
          s2<-fr$SD
          m2<-fr$Mean
          se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) )
          df <- n1+n2-2
          t <- (m1-m2)/se
          pval<-stats::pt(-abs(t), df)
          if (pval< sigval) {sig<-"Yes"}
          if (pval>=sigval) {sig <-" "
          pval <- round(pval,3)}
          ifelse (pval<0.001,pval <- "< 0.001", pval<-round(pval,4))
          #pval<-round(stats::pt(-abs(t), df),6)
        }
      }

      fr$pvalue <- pval
      fr$significance <-sig

      zzz<-rbind(zzz,fr)
    }
  rownames(zzz)<-c(seq(1,nrow(zzz),1))
return(zzz)

}else{
   sss<-dataframe[,seq(1,ctrlcolno,1)]
  sss<-dplyr::select(sss,-as.symbol(ctrlcolumn))

#########################

expcolumn<-which(colnames(dataframe)=="label")-1
if(ctrlcolno != expcolumn){stop("Error: Control is not the approprite one for the analysis")}
############################
runs<-c(seq(1,ncol(sss),1))
exp<-""
ord<-""


for (j in seq_along(runs)) {
  assign(paste0("F",runs[j]),unique(sss[,j]))
  exp<-paste0(exp,",",colnames(sss)[j],"=","F",j)
  ord<-paste0(ord, "," ,"grid$",colnames(sss)[j])
}

exp<-sub(",","",exp)
ord<-sub(",","",ord)


grid<- eval(parse(text = paste("expand.grid", "(",exp, ")")))
grid<-eval(parse(text= paste("grid","[","order","(",ord,")",",","]")))
grid<-as.data.frame(grid)
rownames(grid)<-c(seq(1,nrow(grid),1))
grid[] <- lapply(grid, as.character)

###############
df1<-data.frame()
vectorfilters<-colnames(grid)

for (gr in seq(1,nrow(grid),1)) {

  df1<-dataframe
  vector2<-grid[gr,]

  for (i in seq_along(vector2)) {
    df1<-df1 %>% dplyr::filter (get(vectorfilters[i])==as.character(vector2[i]))
  }

  df1$rowname<-rownames(df1)

  ctrlrows<- as.numeric(which(df1[,ctrlcolumn]==control))
  nonctrlrows<-as.numeric(which(df1[,ctrlcolumn]!=control))
  numb<-nlevels(as.factor(df1[,as.character(ctrlcolumn)]))-1

  df1$newrowname<-"NA"
  df1[ctrlrows,]$newrowname<-as.numeric(df1[ctrlrows,]$rowname)-numb
  df1[nonctrlrows,]$newrowname<-as.numeric(df1[nonctrlrows,]$rowname)+numb

  rownames(df1)<-as.numeric(df1$newrowname)
  df1<-dplyr::select(df1,c(-rowname,-newrowname))
  df1 <- df1[ order(row.names(df1)), ]


  for (z in seq(1,nrow(df1),1)) {

    fr <- df1[z,]
    if(fr[1,ctrlcolno]==control){
      n1<-fr$N
      s1<-fr$SD
      m1<-fr$Mean
      pval<-"control "
      sig<-" "
    }else{

      if(fr[1,ctrlcolno]!=control){
        n2<-fr$N
      s2<-fr$SD
      m2<-fr$Mean
      se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) )
      df <- n1+n2-2
      t <- (m1-m2)/se
      pval<-stats::pt(-abs(t), df)
      if (pval<sigval) {sig<-"Yes"}
      if (pval>sigval){sig <-" "
      pval <- round(pval,3)}
      ifelse(pval<0.001,pval<-"< 0.001", pval<-round(pval,4))
      #pval<-round(stats::pt(-abs(t), df),6)
      }
  }

   fr$pvalue <- pval
   fr$significance <-sig

  zzz<-rbind(zzz,fr)

}
}
  rownames(zzz)<-c(seq(1,nrow(zzz),1))
  return(zzz)
}
}
