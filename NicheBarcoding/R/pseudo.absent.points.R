

#' Generation of pseudo absent points for niche model building
#'
#' @description Randomly generate pseudo points outside the 95%CI of the
#' ecological space of the present data when there is no absent data for
#' building a niche model.
#'
#' @param  data Data frame, longitude and latitude of a single species.
#' @param  outputNum Numeric, the expected number of points.
#' @param  en.vir RasterBrick, the globle bioclimate data obtained from
#' "raster::getData" function.
#' @param  map Logical. Should a map be drawn?
#'
#' @return A data frame of simulated pseudo points.
#' @return A data frame of bioclimate variables of each pseudo points.
#'
#' @keywords pseudo.absent.points
#' @export
#'
#' @author Cai-qing YANG (Email: yangcq_ivy(at)163.com) and Ai-bing ZHANG
#' (Email:zhangab2008(at)cnu.edu.cn), Capital Normal University (CNU), Beijing,
#' CHINA.
#'
#'
#' @examples
#' data(en.vir)
#' #envir<-raster::getData("worldclim",download=FALSE,var="bio",res=2.5)
#' #en.vir<-raster::brick(envir)
#'
#' data<-data.frame(species=rep("Acosmeryx anceus",3),
#'                  Lon=c(145.380,145.270,135.461),
#'                  Lat=c(-16.4800,-5.2500,-16.0810))
#'
#' absent.points<-pseudo.absent.points(data,en.vir=en.vir,outputNum=100)
#' head(absent.points$lonlat)
#' head(absent.points$envir)


pseudo.absent.points<-function(data,outputNum=500,en.vir=NULL,map=TRUE){
  if (!is.data.frame(data)|dim(data)[2]!=3){
    stop ("The input data must be a dataframe with three columns (species name,
          lon, lat)!\n")
  }else{
    pseudo.absent<-list()

    if (is.null(en.vir) == T){  #the parameter "en.vir" is not provided
      cat("Environmental layers downloading ... ")
      envir<-raster::getData("worldclim",download=TRUE,var="bio",res=10)
      en.vir<-raster::brick(envir)
      cat("Done!\n")
    }

    back<-dismo::randomPoints(mask=en.vir,n=outputNum*2,ext=NULL,extf=1.1,
                              excludep=TRUE,prob=FALSE,cellnumbers=FALSE,
                              tryf=3,warn=2,lonlatCorrection=TRUE)
    absen<-raster::extract(en.vir,back)
    prese<-raster::extract(en.vir,data[,2:3])

    search.For.Diff.Absen.From.Prese<-function(prese,absen){
      ### 0.0 functions required:
      #euclidean.dist.two.vectors
      eucl.dist.two.vect<-function(v1,v2){
        v1minusv2<-v1-v2

        squared.v1minusv2<-v1minusv2*v1minusv2
        out.sqrt<-sqrt(sum(squared.v1minusv2))

        return(out.sqrt)
      }### end of fucntion

      ### 0.1 conversion of data type, and remove na data points!
      prese<-stats::na.omit(prese)
      absen<-stats::na.omit(absen)
      prese<-as.matrix(prese)
      absen<-as.matrix(absen)

      # 1 find out the centre of prese, by the function mean
      prese<-apply(prese,MARGIN=2,as.numeric) #factor2numeric
      group.mean.prese<-apply(prese, MARGIN=2, mean, na.rm = T)

      # 2.calculate distance of each point of presence data to the center
      dist2center.prese<-apply(prese,1,eucl.dist.two.vect,v2=group.mean.prese)
      #dist2center.prese

      # 3.calculate 95%CI of the distance
      ci95<-stats::quantile(dist2center.prese,prob=c(0.025,0.975),na.rm = T)

      # 4.to calculate the distance of absence data to the center of presence data
      dist2center.absen<-apply(absen,1,eucl.dist.two.vect,v2=group.mean.prese)
      dist2center.absen

      # 5.to check if the distance of absence to the center is within the range
      #of 95% CI
      within.CI95<-function(ci,x){
        if(x>=ci[1]&&x<=ci[2]) return(TRUE)
        else return (FALSE)
      }

      out2<-sapply(dist2center.absen, within.CI95,ci=ci95)
      out2
      return(out2)

    }

    diff.absen.from.prese<-search.For.Diff.Absen.From.Prese(prese,absen)
    diff.point<-back[which(diff.absen.from.prese==FALSE),]
    diff.env<-absen[which(diff.absen.from.prese==FALSE),]

    samp<-sample(dim(diff.point)[1],size=outputNum)
    pseudo.absent$lonlat<-diff.point[samp,]
    pseudo.absent$envir<-diff.env[samp,]

    if (map == T){
      maps::map("world")
      graphics::points(pseudo.absent$lonlat,col="red",pch=1,cex=0.6)
      graphics::points(data[,2:3],col="blue",pch=19,cex=0.6)
      graphics::points(back[which(diff.absen.from.prese==TRUE),],col="blue",
                       pch=1,cex=0.6)
      graphics::legend(x="bottom",inset=-0.12,
             col=c("blue","blue","red"),
             pch=c(19,1,1),
             legend=c("Present","Wrong absent","Absent"),
             horiz=T,
             xpd=T,
             bty="n")
    }

    return(pseudo.absent)
  }
}

# The end of pseudo.absent.points #

