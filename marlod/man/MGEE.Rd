\name{MGEE}
\alias{MGEE}
\title{Function of a Generalized Estimating Equation (GEE) Model}
\description{The function is used to calculate a empirical MSE minimization criterion (EMMC) value in the "Selected.GEE" function.}
\usage{MGEE(id, y, x, lod, substitue, corstr, typetd, maxiter)}
\arguments{
  \item{id}{A column matrix of subject IDs. The number of rows is the total number of observations. Data must be sorted by IDs.}
  \item{y}{A column matrix of the observed outcome values or responses.}
  \item{x}{A matrix of covariate values, for which the number of columns is the number of covariates.}
  \item{lod}{A numeric value of limit of detection (LOD).}
  \item{substitue}{A character string specifying the substitution approach, including "None", "LOD", "LOD2", "LODS2", "BetaMean", "BetaGM", "MIWithID", "MIWithIDRM", and "QQplot".}
  \item{corstr}{A character string specifying the working correlation structure, given by either "exchangeable" or "AR-1".}
  \item{typetd}{An atomic vector specifying the types of time-dependent covaraites. The length of this vector is the number of regression paramenters, including the intercept. "1" is assigned to any time-indepednet covariates or covariates in a cluster study.}
  \item{maxiter}{The maximum number of iterations.}
}
\value{An object of class "MGEE".}
