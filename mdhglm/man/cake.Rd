\name{cake}
\alias{cake}
\docType{data}
\title{Cake Data}
\description{
In an experiment on the preparation of chocolate cakes, conducted at
Iowa State College, 3 recipes for preparing the batter were compared
(Cochran and Cox, 1957). Recipes I and II differed in that the chocolate
was added at 40C and 60C, respectively, while recipe III contained extra
sugar. In addition, 6 different baking temperatures were tested: these
ranged in 10C steps from 175C to 225C. For each mix, enough batter
was prepared for 6 cakes, each of which was baked at a different temperature.
Thus the recipes are the whole-unit treatments, while the baking
temperatures are the sub-unit treatments. There were 15 replications,
and it will be assumed that these were conducted serially according to a
randomized blocks scheme: that is, one replication was completed before
starting the next, so that differences among replicates represent time
differences. A number of measurements were made on the cakes. The
measurement presented here is the breaking angle. One half of a slab of
cake is held fixed, while the other half is pivoted about the middle until
breakage occurs. The angle through which the moving half has revolved
is read on a circular scale. Since breakage is gradual, the reading tends
to have a subjective element.
}
\usage{data("cake")}
\format{
  A data frame with 270 observations on the following 5 variables.
  \describe{
    \item{\code{Replicate}}{replication number for 1...15 replicates}
    \item{\code{Batch}}{recipes number for 1...3 recipes}
    \item{\code{Recipe}}{recipes number for 1...3 recipes}
    \item{\code{Temperature}}{baking temperature}
    \item{\code{Angle}}{breaking angle}
  }
}

\references{
Cochran, W.G. and Cox, G.M. (1957). Experimental Designs. John
Wiley and Sons, New York, 2nd edition.
}

