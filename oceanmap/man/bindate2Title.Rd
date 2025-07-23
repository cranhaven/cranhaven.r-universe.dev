\name{bindate2Title}
\alias{bindate2Title}
\alias{bindate2main}
\alias{bindate2ylab}

\title{
returns formatted date string for v-plot titles
}
\description{
returns formatted date string for v-plot titles by provided date information (e.g. filename of \code{'.gz'}-files, name of raster-layers. \code{bindate2Title} is returned by default by \link{v}-calls. \code{bindate2main} and \code{bindate2ylab} are plotted when \link{v} is called with \code{sidelabels=T}.
}
\usage{
bindate2Title(timestep, date1, date2=date1)

bindate2main(timestep, date1, date2=date1)

bindate2ylab(timestep, date1, date2=date1)

}
\arguments{
  \item{timestep}{
character string, indicating  the range of the time unit in numbers and the time unit (e.g. "1d" for daily data; "7d" or "1w" for weekly data; "1m" for monthly data) 
}
  \item{date1, date2}{
character string, indicating the first and last date of the timeframe covered (recognized format is \%Y\%m\%d\%H or \%Y\%m\%d). E.g. 20030301 and 20030331 for monthly data (\code{timestep} = 1m) of March 2003.
}
}
\seealso{ \link{name_split}, \link{v}}


\author{
Robert K. Bauer
}

\examples{
## Example 1: output of different bindate2???-functions
path <- system.file("test_files", package="oceanmap")
gz.files <- Sys.glob(paste0(path,'/*.gz')) # load sample-'.gz'-files
u <- name_split(gz.files)

print(gz.files[1]) # print filename
print(u[1,]) # print splitted filename
bindate2main(u$timestep[1],u$date1[1],u$date2[1]) # main
bindate2Title(u$timestep[1],u$date1[1],u$date2[1]) # Title
bindate2ylab(u$timestep[1],u$date1[1],u$date2[1]) # ylab


## Example 2: Visualize output for multiple '.gz'-files 
u$option <- '... .'

dev.new(width=9.7,height=7.8,xpos=-1)
empty.plot()
box()
for (i in 1:nrow(u)){
  mtext(name_join(u[i,]),side=1,line=i-10)
  main <- bindate2main(u$timestep[i],u$date1[i],u$date2[i]) # main
  Title <- bindate2Title(u$timestep[i],u$date1[i],u$date2[i]) # Title
  ylab <- bindate2ylab(u$timestep[i],u$date1[i],u$date2[i]) # ylab
  mtext(c(Title,ylab,main),side=1:3,line=c(i,nrow(u)+1-i,nrow(u)+1-i))
  mtext(paste("file",i),side=c(1,1:3),line=c(i-10,i,nrow(u)+1-i,nrow(u)+1-i),adj=0)
}
mtext(c("filename",
        "bindate2Title (default)",
         "bindate2ylab (sidelabels=T)",
         "bindate2main  (sidelabels=T)"),
      side=c(1,1:3),line=c(-11,rep(i+2,3)),font=2)

}