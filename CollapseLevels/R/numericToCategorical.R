#' @title        numericToCategorical
#'
#' @description  This function categorizes a numerical variable by binning
#'
#' @param dset   The data frame containing the data set
#'
#' @param col    A character respresenting the  name of the numeric attribute which we want to categorize
#'
#' @param resp   A character respresenting the  name of the binary outcome variable
#'               The binary outcome variable may be a factor with two levels or an integer (or numeric ) with two unique values
#'
#' @param bins   A number denoting the number of bins.Default value is 10
#'
#' @param adjFactor A number or a decimal denoting what is to be added to the number of responses (binary outcome variable is 1 ) or to the number of non responses (binary outcome variable is 0) if either is zero for any level of the attribute
#'
#' @return       A list containing the categorized attribute,a table of Information Values for the levels of the categorized attribute,the Information Value for  the entire attribute,a table showing the response rates of the levels of the categorized attribute
#'
#' @examples
#'
#' # Load the German_Credit data set supplied with this package
#'
#' data("German_Credit")
#'
#' # Create an empty list
#'
#' l<-list()
#'
#' # Call the function as follows.
#' #This will categorize the numeric variable Duration in the German_Credit dataset.
#'
#' l<-numericToCategorical(German_Credit,col="Duration",resp="Good_Bad")
#'
#'
#' # To view the categorized variable
#'
#'  l$categoricalVariable
#'
#'  # To view the IV table of the levels of the categorized variable
#'
#'  l$IVTable
#'
#'  # To view the total IV value of the  categorized variable
#'
#'  l$IV
#'
#'  # To view the response rates of the levels of the categorized variable
#'
#'  l$collapseLevels
#'
#' @export

numericToCategorical<-function(dset,col="job",resp="y",bins=10,adjFactor=0.5)
{

  nr<-length(dset[[col]])
  n<-round(nr/bins)
  n
  GC<-data.frame()
  GC<-dset[order(dset[[col]]),]
  vec<-GC[[col]]
  br<-numeric(length = bins+1)
  lrec<-length(vec)
  for(i in 1:bins+1)
  {
    if(i==1)
    {
      br[i]<-vec[i]


    }
    else if (i==(bins+1))
    {
      br[i]<-vec[lrec]

    }
    else{
      br[i]<-vec[((i-1)*n)+1]
    }

  }
  br<-unique(br)
  cbr<-cut(vec,breaks=br,right=FALSE,include.lowest = TRUE)
  varnum<-paste('categorical',col,sep="")
  GC<-cbind(GC,cbr)
  names(GC)[ncol(GC)]<-varnum


  df<-data.frame()
  d<-data.frame()
  df_tot<-data.frame()
  df_one<-data.frame()
  df_zero<-data.frame()
  d<-GC
  if(class(dset[[resp]])=="factor")
  {

    d[[resp]]<-as.numeric(d[[resp]])
    d[[resp]]<-ifelse(d[[resp]]==max(d[[resp]]),1,0)
  }
  if(class(dset[[resp]])=="numeric" | class(dset[[resp]])=="integer")
  {
    d[[resp]]<-ifelse(d[[resp]]==max(d[[resp]]),1,0)
  }
  l<-ncol(d)
  naml<-names(d[l])

  df_tot <- d %>%  dplyr::group_by_(naml) %>% dplyr::summarise(tot=dplyr::n())
  df_tot<-as.data.frame(df_tot)

  df_one <- d %>%  dplyr::filter(d[[resp]]==1) %>% dplyr::group_by_(naml) %>% dplyr::summarise(bad=dplyr::n())

  df_one<-as.data.frame(df_one)

  one_rate <- (df_one[,2]/sum(df_one[,2]))*100

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

  dfiv<-data.frame()
  dfiv<-df

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

  dfiv[dfiv$response==0,"response"]<-adjFactor
  dfiv[dfiv$non_response==0,"non_response"]<-adjFactor

  dfiv$response_pct<-(dfiv$response/sum(dfiv$response))
  dfiv$non_response_pct<-(dfiv$non_response/sum(dfiv$non_response))

  woe<-numeric()
  iv<-numeric()

  woe<-log(dfiv$non_response_pct/dfiv$response_pct)

  if(sum(dfiv$response==0)>0)
  {
    woe[dfiv$response==0]<-0
  }
  if(sum(dfiv$non_response==0)>0)
  {
    woe[dfiv$non_response==0]<-0
  }

  iv<-(dfiv$non_response_pct-dfiv$response_pct)*woe

  dfiv<-cbind(dfiv,woe=woe,iv=iv)

  IV<-sum(dfiv$iv)

  dfiv$response<-ifelse(dfiv$response==adjFactor,0,dfiv$response)
  dfiv$non_response<-ifelse(dfiv$non_response==adjFactor,0,dfiv$non_response)

  dfiv$response_pct<-(dfiv$response/sum(dfiv$response))
  dfiv$non_response_pct<-(dfiv$non_response/sum(dfiv$non_response))

  mnlist<-list()
  mnlist<-list(categoricalVariable=cbr,collapseLevels=df,IVTable=dfiv,IV=sum(iv))
  return(mnlist)

}
