\name{MakeSnapshotInfo}
\alias{MakeSnapshotInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  First Step in Snapshot Conversion
}
\description{
  Helps convert binary snapshot output from infraFDTD into images. This
  function collects information from the snapshots to facilitate the
  conversion. It does NOT perform the conversion itself (see
  MakeSnapshots for that).
}
\usage{
MakeSnapshotInfo(inputdir = "./snapshots", dframe = 20, N = 400, xs, ys,
zs, makesec1 = TRUE, makesec2 = TRUE, makesurf = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputdir}{
    Directory containing snapshot files.
}
  \item{dframe}{
    Snapshot interval in samples (difference between snapshot file numbers).
}

  \item{N}{
    Number of snapshots.
}
  \item{xs}{
    X source coordinates (m); can be a vector if multiple sources.
}
  \item{ys}{
    Y source coordinates (m); can be a vector if multiple sources.
}
  \item{zs}{
    Z source coordinates (m); can be a vector if multiple sources.
}
  \item{makesec1}{
    Logical: should section 1 snapshots be processed?
}
  \item{makesec2}{
    Logical: should section 2 snapshots be processed?
}
  \item{makesurf}{
    Logical: should surface snapshots be processed?
}
}
\details{
  inputdir must include all snapshot files as well as coord_sec1.txt,
  coord_sec2.txt, and coord_sur.txt.
  
  MakeSnapshotInfo is fairly time-consuming, so it's separated from
    MakeSnapshots so that MakeSnapshots can be run repeatedly to test
    different graphical settings without having to re-calculate this
    information every time.
}
\value{
  A list of data to be passed to MakeSnapshots.
}
\author{
  Jake Anderson; code is heavily modified from Keehoon Kim's original
    snapshot code.
}
\seealso{
  MakeSnapshots
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
