#' @title        levelsCollapser
#'
#' @description  This function displays the response rates by the levels of an attribute
#'               Levels with similar response rates may be combined
#'
#' @param dset   The data frame containing the data set
#'
#' @param resp   A character respresenting the  name of the binary outcome variable
#'               The binary outcome variable may be a factor with two levels or an integer (or numeric ) with two unique values
#'
#' @param bins   A number denoting the number of bins.Default value is 10
#'
#' @return       A list containing the tables of response rate by levels for every attribute
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
#' # Call the function as follows
#'
#' l<-levelsCollapser(German_Credit,resp="Good_Bad",bins=10)
#'
#' # response rate by levels of the Account_Balance in the German_Credit data
#'
#' l$Account_Balance
#'
#' # Collapse levels with similar response percentages.
#'
#' @export

levelsCollapser<-function(dset,resp="y",bins=10)
{

  l<-list()
  d<-data.frame()
  d<-dset
  if(class(dset[[resp]])=="factor")
  {

    d[[resp]]<-as.numeric(d[[resp]])
    d[[resp]]<-ifelse(d[[resp]]==max(d[[resp]]),1,0)
  }
  if(class(dset[[resp]])=="numeric" | class(dset[[resp]])=="integer")
  {
    d[[resp]]<-ifelse(d[[resp]]==max(d[[resp]]),1,0)
  }

  for(i in 1:ncol(d))
  {
    if(names(d)[i]!= resp)
    {
      naml<-names(d)[i]
      df<-data.frame()
      df_tot<-data.frame()
      df_one<-data.frame()
      df_zero<-data.frame()

      if(class(d[[i]])=="numeric" | class(d[[i]])=="integer")
      {
        nr<-length(dset[[i]])
        n<-round(nr/bins)
        n
        GC1<-data.frame()
        GC1<-d[order(d[[i]]),]
        vec<-GC1[[i]]
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

        if(length(br)==2)
        {
          cbr<-cut(vec,breaks=br,right=FALSE)
        }
        else
        {
          cbr<-cut(vec,breaks=br,right=FALSE,include.lowest = TRUE)
        }


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

      df<-df[order(df$response_pct),]

      b=numeric()

      for(j in 1:nrow(df))
      {
        if(j==1)
        {
          b[j]<-0
        }
        else
        {
          b[j]<-((df$response_pct[j]-df$response_pct[j-1])/df$response_pct[j])*100
        }

      }

      df<-cbind(df,response_pct_change=b)
      l[[i]]=df
      names(l)[[i]]<-names(d)[i]


    }
  }
  return(l)
}
