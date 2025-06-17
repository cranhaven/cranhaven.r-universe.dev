\name{coalitions}
\alias{coalitions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Coalitions}
\description{
Possible coalitions with n players.}
\usage{
coalitions(n)
}
\arguments{
  \item{n}{
Number of players
}
}

\value{
This function gives the coalitions in a binary mode and usual way.
}

\examples{
coalitions(3)
#$Binary
#     [,1] [,2] [,3]
#[1,]    0    0    0
#[2,]    1    0    0
#[3,]    0    1    0
#[4,]    0    0    1
#[5,]    1    1    0
#[6,]    1    0    1
#[7,]    0    1    1
#[8,]    1    1    1
#
#$Classic
#[1] "0"   "'{ 1 }'" "'{ 2 }'" "'{ 3 }'" "'{ 1,2 }'" "'{ 1,3 }'" "'{ 2,3 }'"  
#[8] "'{ 1,2,3 }'"

}
