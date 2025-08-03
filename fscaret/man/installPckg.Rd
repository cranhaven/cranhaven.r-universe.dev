\name{installPckg}
\alias{installPckg}
\title{
installPckg
}
\description{
Function installs the packages that are listed in data(requiredPackages). The function is called within fscaret function.
If argument "installReqPckg = TRUE" the function installs required packages.
}
\usage{
installPckg(requiredPackages)
}
\arguments{
  \item{requiredPackages}{
Vector of packages to be installed
}
}
\details{
Be advised setting "installReqPckg = TRUE" installs packages in your home directory (.R). To install packages for all users please login as root (admin).
}
\author{
Jakub Szlek and Aleksander Mendyk
}
\keyword{ data }
\keyword{ package }
