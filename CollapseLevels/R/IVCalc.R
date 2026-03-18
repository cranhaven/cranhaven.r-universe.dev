#' @title        IVCalc
#'
#' @description  This function displays the Information Values  by the levels of an attribute
#'               This information is displayed for all attributes in the data set
#'
#' @param dset   The data frame containing the data set
#'
#' @param resp   A character respresenting the  name of the binary outcome variable
#'               The binary outcome variable may be a factor with two levels or an integer (or numeric ) with two unique values
#'
#' @param bins   A number denoting the number of bins.Default value is 10
#'
#' @param adjFactor A number or a decimal denoting what is to be added to the number of responses (binary outcome variable is 1 ) or to the number of non responses (binary outcome variable is 0) if either is zero for any level of the attribute
#'
#'
#' @return       A list containing the tables of Information Values  by levels for every attribute
#'
#' @examples
#'
#' # Load the German_Credit data set supplied with this package
#'
#' data("German_Credit")
#'
#' l<-list()
#'
#' # Call the function as follows
#'
#' l<-IVCalc(German_Credit,resp="Good_Bad",bins=10)
#'
#' # Information Value for  the attribute Account_Balance in the German_Credit data
#'
#' l$Account_Balance
#'
#'
#' @export

IVCalc<-function(dset,resp="y",bins=10,adjFactor=0.5)
{
  mnlist<-list()
  d<-data.frame()
  d<-dset
  if(class(d[[resp]])=="factor")
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


      mnlist[[i]]<-list(IVTable=dfiv,IV=sum(iv))
      names(mnlist)[i]<-names(d)[i]




    }
  }

  return(mnlist)
}
