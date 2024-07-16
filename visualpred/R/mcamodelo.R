#' Basic MCA function for clasification
#'
#' This function presents visual graphics by means of Multiple correspondence Analysis projection.
#' Interval variables are categorized to bins.
#' Dependent classification variable is set as supplementary variable. It is used as base for mcacontour function.
#' @usage mcamodelobis(dataf=dataf,listconti,listclass, vardep,bins=8,selec=1,
#' Dime1="Dim.1",Dime2="Dim.2")
#' @param dataf data frame.
#' @param listconti Interval variables to use, in format c("var1","var2",...).
#' @param listclass Class variables to use, in format c("var1","var2",...).
#' @param vardep  Dependent binary classification variable.
#' @param bins  Number of bins for categorize interval variables .
#' @param Dime1,Dime2 MCA Dimensions to consider. Dim.1 and Dim.2 by default.
#' @param selec 1 if stepwise logistic variable selection is required, 0 if not.
#' @keywords MCA
#' @export
#' @importFrom magrittr %>%
#' @examples
#' data(breastwisconsin1)
#' dataf<-breastwisconsin1
#' listconti=c( "clump_thickness","uniformity_of_cell_shape","mitosis")
#' listclass=c("")
#' vardep="classes"
#'result<-mcacontour(dataf=dataf,listconti,listclass,vardep,bins=8,title="",selec=1)
#' @import MASS
#' @importFrom mltools bin_data
#' @importFrom FactoMineR MCA
#' @importFrom dplyr inner_join
#' @importFrom data.table data.table
#' @return A list with the following objects:\describe{
#' \item{df1}{ dataset used for graph1}
#' \item{df2}{ dataset used for graph2}
#' \item{df3}{ dataset used for graph2}
#' \item{listconti}{ interval variables used}
#' \item{listclass}{ class variables used}
#' \item{axisx}{ axis definition in plot}
#' \item{axisy}{axis definition in plot}
#' }
#'
mcamodelobis<-function(dataf=dataf,listconti,listclass, vardep,bins=8,selec=1,
Dime1="Dim.1",Dime2="Dim.2")
{

  count.dups <- function(DF){
    DT <- data.table::data.table(DF)
    DT[,.N, by=list(dimf1=DF$dimf1,dimf2=DF$dimf2)]
  }
.N<-numeric()
minori<-numeric()
dimf1<-numeric()
dimf2<-numeric()
Frecu<-numeric()
Variable=character()
fontface=numeric()

  # class minor
  tabla1<-as.data.frame(table(dataf[,vardep]))
  tabla1<-tabla1[order(tabla1$Freq),]
  minoritaria<-as.character(tabla1[1,c("Var1")])
  tabla1<-tabla1[order(-tabla1$Freq),]
  mayoritaria<-as.character(tabla1[1,c("Var1")])

  if (any(listclass==c(""))==TRUE)
  {
    dataf<-dataf[,c(listconti,vardep)]
  }

  if (any(listclass==c(""))==FALSE)
  {
    if (any(listconti==c(""))==FALSE)
    {dataf<-dataf[,c(listconti,listclass,vardep)]}

    if (any(listconti==c(""))==TRUE)
    {dataf<-dataf[,c(listclass,vardep)]}
  }

  dataf2<-dataf


  #  -INTERVAL:
  #    1) STANDARD TO 0,1
  #  2) BINNING
  #  -CLASS VARIABLES: NOTHING

  #  ALL ARE RECODED TO A FACTOR
  #

  #  0-1
  if (any(listconti==c(""))==FALSE)
  {
    normFunc <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}
    dataf[c(listconti)] <- apply(dataf[c(listconti)], 2, normFunc)
  }
  # CLASS TO  FACTOR
  if (any(listclass==c(""))==FALSE)
  {
    dataf[c(listclass,vardep)]<-
      lapply(dataf[c(listclass,vardep)],factor)
  }

  # SELECTION IF IT IS REQUIRED

  if (selec==1)
  {

    formu1<-paste("factor(",vardep,")~.")
    full.model <-stats::glm(stats::formula(formu1), data = dataf, family = binomial(link="logit"))
    step.model <- full.model %>% stepAIC(trace = FALSE)
    cosa<-attr(stats::terms(step.model), "term.labels")


    # Reduce data frame

    if (any(listclass==c(""))==FALSE)
    {
      listclass <- listclass[listclass %in%cosa]

      if (any(listconti==c(""))==FALSE)
      {
        listconti <- listconti[listconti %in%cosa]
      }

    }
    if (any(listclass==c(""))==TRUE)
    {
      listconti <- listconti[listconti %in%cosa]
    }

    if (any(listclass==c(""))==TRUE)
    {
      dataf<-dataf[,c(listconti,vardep)]
    }
    if (any(listclass==c(""))==FALSE)
    {
      if (any(listconti==c(""))==FALSE)
      {dataf<-dataf[,c(listconti,listclass,vardep)]}
      if (any(listconti==c(""))==TRUE)
      {dataf<-dataf[,c(listclass,vardep)]}
    }

  }

  # Binning
  if (any(listconti==c(""))==FALSE)
  {
    for(col in listconti)
    {
        dataf[[col]] <- as.integer(bin_data(dataf[[col]],bins=bins,binType="quantile")) - 1L
    }
  }

  # All to factor
  if (any(listclass==c(""))==FALSE)
  {
    if (any(listconti==c(""))==FALSE)
    {
      dataf[c(listconti,listclass,vardep)]<-
        lapply(dataf[c(listconti,listclass,vardep)],factor)
    }
    if (any(listconti==c(""))==TRUE)
    {
      dataf[c(listclass,vardep)]<-
        lapply(dataf[c(listclass,vardep)],factor)
    }

  }

  if (any(listclass==c(""))==TRUE)
  {
    dataf[c(listconti,vardep)]<-
      lapply(dataf[c(listconti,vardep)],factor)
  }

  # LA DEPENDIENTE A FACTOR

  dataf[,c(vardep)]<-as.factor(as.character(dataf[,c(vardep)]))


  # DEPENDENT as SUPLEMENTARy
  colu<-which( colnames(dataf)==vardep)

  # MCA FROM FACTOMINER

  mca1 =MCA(dataf,quali.sup=colu,graph=FALSE)

  if (ncol(dataf)>2)

  {
    cats1 = apply(dataf[,-c(colu)], 2, function(x) nlevels(as.factor(x)))
    mca1_vars_df1 = data.frame(mca1$var$coord, Variable = rep(names(cats1), cats1))
  }

  # cats2 =nlevels(dataf[,c(colu)])
  # names(cats2)<-vardep


  if (ncol(dataf)==2)
  {
    a<-  names(dataf)
    a<- a[a!=vardep]
    mca1_vars_df1 = data.frame(mca1$var$coord, Variable =a )
  }

  # mca1_vars_df2= data.frame(mca1$quali.sup$coord, Variable = rep(names(cats2), cats2))
  # mca1_vars_df2$Variable<-paste(".",mca1_vars_df2$Variable,sep="")

  mca1_obs_df = data.frame(mca1$ind$coord)
  mca1_obs_df<-cbind(mca1_obs_df,vardep=dataf[,vardep])

  variss<-data.frame(mca1$quali.sup$coord)
  variss$Variable<-paste(".",vardep,sep="")

  mca1_vars_df1$tama<-4
  # mca1_vars_df2$tama<-5
  # uni<-rbind(mca1_vars_df1,mca1_vars_df2)
  uni<-mca1_vars_df1
  uni$fontface<-ifelse(uni$Variable==vardep,2,1)
  uni$Variable<-as.character(uni$Variable)


  # SUPLEMENTARIA
  variss$dimf1<-variss[,c(Dime1)]
  variss$dimf2<-variss[,c(Dime2)]

  # AXES

  first<-as.numeric(substr(c(Dime1),5,5))
  second<-as.numeric(substr(c(Dime2),5,5) )


  ejex<-paste(Dime1,"(",trunc(mca1$eig[first,2]*10)/10,"%)",sep="")
  ejey<-paste(Dime2,"(",trunc(mca1$eig[second,2]*10)/10,"%)",sep="")

  # FREQ

  mca1_obs_df$minori<-ifelse(mca1_obs_df$vardep==minoritaria,1,0)

  mca1_obs_df$minori<-as.character(mca1_obs_df$minori)

  df<-mca1_obs_df

  df$dimf1<-df[,c(Dime1)]
  df$dimf2<-df[,c(Dime2)]


  uni$dimf1<-uni[,c(Dime1)]
  uni$dimf2<-uni[,c(Dime2)]

  au<-as.data.frame(count.dups(df))
  au$Frecu<-au$N
  au$N<-NULL


  df<-inner_join(df,au, by = c("dimf1","dimf2"))


  cosa<-list(df,uni,dataf2,listconti,listclass,ejex,ejey,variss)
  names(cosa)<-c("df1","df2","df3","listconti","listclass","axisx","axisy","variss")

  return(cosa)
}


