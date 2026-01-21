\name{MakeSnapshots}
\alias{MakeSnapshots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Convert Binary Snapshot Output to PNGs
}
\description{
  With the help of MakeSnapshotInfo, converts binary snapshot output
  files from infraFDTD to PNG format.
}
\usage{
MakeSnapshots(info, outputdir = "./anim", prefix = "snapshot", nums =
1:info$N, dt = 0.1, makesec1 = TRUE, makesec2 = TRUE, makesurf = TRUE,
res = 96, fn_topomap = "topomap.png", width = 480, height = NaN, asp =
1, pointsize = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{info}{
    Output of MakeSnapshotInfo().
}
  \item{outputdir}{
    Directory where PNG files should be written.
}
  \item{prefix}{
    Prefix to use in output file names (e.g., outputdir/prefix_sec1_001.png).
}
  \item{nums}{
    Vector of snapshot numbers to convert (default is all of them).
  }
  \item{dt}{
    Snapshot interval in seconds
  }
  \item{makesec1}{
    Logical: should section 1 snapshots be converted?
}
  \item{makesec2}{
    Logical: should section 2 snapshots be converted?
}
  \item{makesurf}{
    Logical: should surface snapshots be converted?
}
  \item{res}{
    Resolution of output PNG files (see ?png).
}
  \item{fn_topomap}{
    File name for topo map output.
}
  \item{width}{
    Width of output images (pixels)
}
  \item{height}{
    Optional: if blank, MakeSnapshots() will find the correct height
    given the image dimensions and asp. If set, forces the height of
    output images.
}
  \item{asp}{
    Aspect ratio of images (default 1).
}
  \item{pointsize}{
    Pointsize in output images (default 12).
}
}
\details{
  Use a shell command like 'convert' (imagemagick, available in linux)
  to make a gif out of the stills.
}
\value{
  None, file output only.
}
\author{
  Jake Anderson; code is heavily modified from Keehoon Kim's original.
}
\seealso{
  MakeSnapshotInfo
}
\examples{
## Don't Run: (requires snapshot files)
## info = MakeSnapshotInfo('./snapshots', dframe = 20, N = 400,
## xs = 100, ys = 100, zs = 500)
## MakeSnapshots(info, outputdir = './anim', prefix = 'snapshot', dt = 0.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
\keyword{ file }% __ONLY ONE__ keyword per line
