#' @title        displayResponseRatebyLevels
#'
#' @description  This function displays the response percents  of the levels of an attribute.
#'
#' @param dset   The data frame containing the data set
#'
#' @param col    A character respresenting the  name of the attribute . The attribute can either be numeric or categorical
#'
#' @param resp   A character respresenting the  name of the binary outcome variable
#'               The binary outcome variable may be a factor with two levels or an integer (or numeric ) with two unique values
#'
#' @param bins   A number denoting the number of bins.Default value is 10
#'
#' @param adjFactor A number or a decimal denoting what is to be added to the number of responses (binary outcome variable is 1 ) or to the number of non responses (binary outcome variable is 0) if either is zero for any level of the attribute
#'
#' @return       NULL
#'
#' @examples
#'
#' # Load the German_Credit data set supplied with this package
#'
#' data("German_Credit")
#'
#' displayResponseRatebyLevels(German_Credit,col="Credit_History",resp="Good_Bad")
#'
#' @export

displayResponseRatebyLevels<-function(dset,col="job",resp="Good_Bad",bins=10,adjFactor=0.5)
{
  df<-data.frame()
  d<-data.frame()
  df_tot<-data.frame()
  df_one<-data.frame()
  df_zero<-data.frame()

  d<-dset
  naml<-col
  if(class(dset[[resp]])=="factor")
  {

    d[[resp]]<-as.numeric(d[[resp]])
    d[[resp]]<-ifelse(d[[resp]]==max(d[[resp]]),1,0)
  }
  if(class(dset[[resp]])=="numeric" | class(dset[[resp]])=="integer")
  {
    d[[resp]]<-ifelse(d[[resp]]==max(d[[resp]]),1,0)
  }
  if(class(d[[col]])=="numeric" | class(d[[col]])=="integer")
  {
    nr<-length(dset[[col]])
    n<-round(nr/bins)
    n
    GC1<-data.frame()
    GC1<-d[order(d[[col]]),]
    vec<-GC1[[col]]
    br<-numeric(length = bins+1)
    lrec<-length(vec)

    for(k in 1:bins+1)
    {
      if(k==1)
      {
        br[k]<-vec[k]


      }
      else if (k==(bins+1))
      {
        br[k]<-vec[lrec]

      }
      else{
        br[k]<-vec[((k-1)*n)+1]
      }

    }

    br<-unique(br)

    cbr<-cut(vec,breaks=br,right=FALSE,include.lowest = TRUE)

    naml<-gsub(" ","",naml)
    varnum<-paste('categorical',naml,sep="")
    GC1<-cbind(GC1,cbr)
    names(GC1)[ncol(GC1)]<-varnum
    naml<-names(GC1)[ncol(GC1)]
    d<-data.frame()
    d<-GC1

  }

  df_tot <- d %>%  dplyr::group_by_(naml) %>% dplyr::summarise(tot=dplyr::n())

  df_tot<-as.data.frame(df_tot)


  val<-1
  filter_criteria<-lazyeval::interp(~y==x, .values = list(y=as.name(resp),x=val))
  df_one <- d %>%  dplyr::filter(d[[resp]]==1) %>% dplyr::group_by_(naml) %>% dplyr::summarise(bad=dplyr::n())

  df_one<-as.data.frame(df_one)


  one_rate <- (df_one[,2]/sum(df_one[,2]))*100


  val<-0
  filter_criteria<-lazyeval::interp(~y==x, .values = list(y=as.name(resp),x=val))
  df_zero <- d %>%  dplyr::filter(d[[resp]]==0) %>% dplyr::group_by_(naml) %>% dplyr::summarise(good=dplyr::n())

  df_zero<-as.data.frame(df_zero)


  zero_rate<-(df_zero[,2]/sum(df_zero[,2]))*100


  if(nrow(df_tot)>nrow(df_zero))
  {
    zero<-as.numeric()
    zero<-df_tot[,2]-df_one[,2]
    zero_rate<-as.numeric()
    zero_rate<-(zero/sum(zero))*100
    df<-cbind(df_tot,response=df_one[,2],non_response=zero,response_pct=one_rate,non_response_pct=zero_rate)


  }
  else if(nrow(df_tot)>nrow(df_one))
  {

    one<-as.numeric()
    one<-df_tot[,2]-df_zero[,2]
    one_rate<-as.numeric()
    one_rate<-(one/sum(one))*100
    df<-cbind(df_tot,response=one,non_response=df_zero[,2],response_pct=one_rate,non_response_pct=zero_rate)


  }
  else
  {
    df<-cbind(df_tot,response=df_one[,2],non_response=df_zero[,2],response_pct=one_rate,non_response_pct=zero_rate)
  }

  df<-df[order(df$response_pct),]

  b=numeric()

  for(i in 1:nrow(df))
  {
    if(i==1)
    {
      b[i]<-0
    }
    else
    {
      b[i]<-((df$response_pct[i]-df$response_pct[i-1])/df$response_pct[i])*100
    }

  }

  df<-cbind(df,response_pct_change=b)

  ggplot2::ggplot(df,ggplot2::aes(x=df[[1]],y=df[["response_pct"]])) + ggplot2::geom_bar(stat = "identity") + ggplot2::labs(x=col,y="Response rate")
}

