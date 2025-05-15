\name{rotation2dAnimation}
\alias{rotation2dAnimation}
\title{Cretaes video by FFmpeg with animation of dataset rotated}
\usage{
rotation2dAnimation(conf2d,
ani.interval=0.2,
ani.nmax=361,
ani.width=500,
ani.height=500,
ani.video.name="mds_rotate.mp4",
angle.start=-pi,
angle.stop=pi,
angle.step=pi/180)
}
\arguments{
\item{conf2d}{two dimensional dataset ot matrix}

\item{ani.video.name}{the file name of the output video (e.g.
\file{animation.mp4} or \file{animation.avi})}
\item{ani.interval}{interval betwwen animation frames}
\item{ani.nmax}{maximal number of frames}
\item{ani.width}{width of movie}
\item{ani.height}{height of movie}
\item{angle.start}{starting angle for animation}
\item{angle.stop}{end angle for animation}
\item{angle.step}{step of animation in radians}
}

\value{
An integer indicating failure (-1) or success (0) of the converting
  (refer to \code{\link{system}}).
}
\description{
This function opens a graphics device to record the images produced in the
code \code{expr}, then uses FFmpeg to convert these images to a video.
}
\details{
This function uses \code{\link{system}} to call FFmpeg to convert the images
to a single video. The command line used in this function is: \command{ffmpeg
-y -r <1/interval> -i <img.name>\%d.<ani.type> other.opts video.name}

where \code{interval} comes from \code{ani.options('interval')}, and
\code{ani.type} is from \code{ani.options('ani.type')}. For more details on
the numerous options of FFmpeg, please see the reference.

Some linux systems may use the alternate software 'avconv' instead of 'ffmpeg'. The package will attempt to determine which command is present and set \code{\link{ani.options}('ffmpeg')} to an appropriate default value. This can be overridden by passing in the \code{ffmpeg} argument.
}
\seealso{
Other utilities: \code{\link{im.convert}},
  \code{\link{saveGIF}}, \code{\link{saveHTML}},
  \code{\link{saveLatex}}, \code{\link{saveSWF}}
}
\author{
Marek Walesiak \email{marek.walesiak@ue.wroc.pl}, Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} 

Department of Econometrics and Computer Science, Wroclaw University of Economics and Business, Poland
}
\references{
Walesiak, M. (2016), Visualization of Linear Ordering Results for Metric Data with the Application of Multidimensional Scaling, Ekonometria, 2(52), 9-21. Available at: \doi{10.15611/ekt.2016.2.01}.

Walesiak, M. (2017), The application of multidimensional scaling to measure and assess changes in the level of social cohesion of the Lower Silesia region in the period 2005-2015, Ekonometria, 3(57), 9-25. Available at: \doi{10.15611/ekt.2017.3.01}.

Walesiak, M., Dudek, A. (2017), \emph{Selecting the Optimal Multidimensional Scaling Procedure for Metric Data with R Environment}, STATISTICS IN TRANSITION new series, September, Vol. 18, No. 3, pp. 521-540.

\url{https://yihui.org/animation/example/savevideo/}

\url{http://ffmpeg.org/documentation.html}
}
\examples{
  \donttest{
    library(mdsOpt)
    library(smacof)
    library(animation)
    library(spdep)
    library(clusterSim)
    data(data_lower_silesian)
    z<-data.Normalization(data_lower_silesian, type="n1")
    d<-dist.GDM(z, method="GDM1")
    res<-smacofSym(delta=d,ndim=2,type="interval")
    konf<-as.matrix(res$conf)
    #Uncomment only if ffmpeg is properly installed for animation package 
    #see:  https://yihui.org/animation/example/savevideo/ 
    #oopts = if (.Platform$OS.type == "windows") \{
    # ani.options(ffmpeg = "D:/Installer/ffmpeg/bin/ffmpeg.exe")
    #\}
    #rotation2dAnimation(conf2d=konf,angle.start=-0,angle.stop=2*pi)
  }
}
\keyword{2D rotation}
