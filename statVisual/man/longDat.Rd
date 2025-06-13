\name{longDat}
\alias{longDat}
\docType{data}
\title{
A Simulated Dataset for Longitudinal Data Analysis
}
\description{
A simulated dataset for longitudinal data analysis.
}
\usage{data("longDat")}
\format{
  A data frame with 540 observations on the following 4 variables.
  \describe{
    \item{\code{sid}}{subject id}
    \item{\code{time}}{time points. A factor with levels \code{time1} \code{time2} \code{time3} \code{time4} \code{time5} \code{time6}}
    \item{\code{y}}{numeric. outcome variable}
    \item{\code{grp}}{subject group. A factor with levels \code{grp1} \code{grp2} \code{grp3}}
  }
}
\details{
The dataset is generated from the following mixed effects model for repeated measures:
\deqn{y_{ij}=\beta_{0i}+\beta_1 t_{j} + \beta_2 grp_{2i} +
\beta_3 grp_{3i} + \beta_4 \times\left(t_{j}\times grp_{2i}\right)
+ \beta_5 \times\left(t_{j}\times grp_{3i}\right)
+\epsilon_{ij},
}{
yij=beta0i+beta1*tj + beta2*grp2i +
beta3*grp3i + beta4*(tj*grp2i)
+ beta5*(tj*grp3i)+epsilonij,
} where \eqn{y_{ij}}{yij} is the outcome value for the \eqn{i}-th subject
measured at \eqn{j}-th time point \eqn{t_{j}}{tj}, 
\eqn{grp_{2i}}{grp2i} is a dummy variable indicating if the \eqn{i}-th subject
is from group 2, 
\eqn{grp_{3i}}{grp3i} is a dummy variable indicating if the \eqn{i}-th subject
is from group 3, 
\eqn{\beta_{0i}\sim N\left(\beta_0, \sigma_b^2\right)}{
beta0i ~ N(beta0, sigmab^2)
},
\eqn{\epsilon_{ij}\sim N\left(0, \sigma_e^2\right)}{
epsilonij ~ N(0, sigmae^2)
}, \eqn{i=1,\ldots, n, j=1, \ldots, m}{i=1, ..., n, j=1, ..., m},
\eqn{n} is the number of subjects, and \eqn{m} is the number of time points.

When \eqn{t_j=0}, the expected outcome value is
\deqn{
E\left(y_{ij}\right)=\beta_0+\beta_2 dose_{2i} + \beta_3 dose_{3i}.}{
E(yij)=beta0+beta2*dose2i + beta3*dose3i.
}

Hence, we have at baseline
\deqn{
E\left(y_{ij}\right)=\beta_0,\; \mbox{for dose 1 group}.}{
E(yij)=beta0, for dose 1 group.
}

\deqn{
E\left(y_{ij}\right)=\beta_0 + \beta_2,\; \mbox{for dose 2 group}.}{
E(yij)=beta0+beta2, for dose 2 group.
}

\deqn{
E\left(y_{ij}\right)=\beta_0 + \beta_3,\; \mbox{for dose 3 group}.}{
E(yij)=beta0+beta2, for dose 3 group.
}

For dose 1 group, the expected outcome values across time is
\deqn{
E\left(y_{ij}\right)=\beta_0+\beta_1 t_{j}.}{
E(yij)=beta0+beta1*tj.
}

We also can get the expected difference of outcome values between dose 2 group and dose 1 group, between dose 3 group and dose 1 group, and between
dose 3 group and dose 2 group:
\deqn{
E\left(y_{ij} - y_{i'j}\right) =\beta_2+\beta_4 t_{j},\;\mbox{for subject $i$ in dose 2 group and subject $i'$ in dose 1 group},}{
E(yij - yi'j) =beta2+beta4*tj, for subject i in dose 2 group and subject i' in dose 1 group,
}

\deqn{
E\left(y_{kj} - y_{i'j}\right) =\beta_3+\beta_5 t_{j},\;\mbox{for subject $k$ in dose 3 group and subject $i'$ in dose 1 group},}{
E(ykj - yi'j) =beta3+beta5*tj, for subject k in dose 3 group and subject i' in dose 1 group,
}

\deqn{
E\left(y_{kj} - y_{ij}\right) =\left(\beta_3-\beta_2\right)+\left(\beta_5-\beta_4\right) t_{j},\;\mbox{for subject $i$ in dose 3 group and subject $i$ in dose 2 group}.}{
E(ykj - yij) =(beta3-beta2)+(beta5-beta4)*tj, for subject k in dose 3 group and subject i in dose 2 group.
}

We set \eqn{n=90}, \eqn{m=6}, 
\eqn{\beta_0=5}{beta0=5},
\eqn{\beta_1=0}{beta1=0},
\eqn{\beta_2=0}{beta2=0},
\eqn{\beta_3=0}{beta3=0},
\eqn{\beta_4=2}{beta4=2},
\eqn{\beta_5=-2}{beta5=-2},
\eqn{\sigma_e=1}{sigmae=1},
\eqn{\sigma_b=0.5}{sigmab=0.5},
and
\eqn{t_{ij}=j, j=1, \ldots, m}{tij=j, j=1, ..., m}.


That is, the trajectories for dose 1 group are horizontal with mean intercept at \eqn{5}, the trajectories for dose 2 group are linearly increasing with slope \eqn{2} and mean intercept \eqn{5}, and the trajectories for dose 3 group are linearly decreasing with slope \eqn{-2} and mean intercept \eqn{5}.


}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
%\references{
%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(longDat)

print(dim(longDat))
print(longDat[1:3,])

print(table(longDat$time, useNA = "ifany"))
print(table(longDat$grp, useNA = "ifany"))
print(table(longDat$sid, useNA = "ifany"))

print(table(longDat$time, longDat$grp))
}
\keyword{datasets}
