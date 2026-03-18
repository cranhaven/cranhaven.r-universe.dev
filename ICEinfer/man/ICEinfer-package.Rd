\name{ICEinfer-package}
\alias{ICEinfer-package}
\docType{package}
\title{ICE Statistical Inference and Economic Preference Variation}
\description{
  Functions in the ICE Statistical Inference package make head-to-head comparisons
  between patients in two treatment cohorts (assumed to be unbiased samples) in two
  distinct dimensions, cost and effectiveness.
  
  Bootstrap resampling methods quantify the endogenous Distribution of ICE Uncertainty
  and define Wedge-Shaped Statistical Confidence Regions equivariant relative to
  exogenous choice for the numerical Shadow Price of Health, lambda.
  
  Preference maps with (linear or nonlinear) indiference curves can be viewed or
  superimposed upon endogenous confidence wedges to illustrate that considerable
  additional, potentially self-contradictory Economic Preference Uncertainty
  results from deliberately varying lambda.
}
\details{
  \tabular{ll}{
  Package: \tab ICEinfer\cr
  Type: \tab Package\cr
  Version: \tab 1.3\cr
  Date: \tab 2020-10-10\cr
  License: \tab GNU GENERAL PUBLIC LICENSE, Version 2, June 1991\cr
  }
Statistical inference using functions from the ICEinfer package usually starts
with (possibly multiple) invocations of ICEscale() to help determine a reasonable
value for the Shadow Price of Health, lambda.  This is invariably followed by a
single call to ICEuncrt to generate the Bootstrap Distribution of ICE Uncertainty
corresponding to the chosen value of lambda.  The print() and plot()
functions for objects of type ICEuncrt have optional arguments, lfact and swu,
to help users quantify and visualize the consequences of changing lambda and
switching between cost and effe units.

A single call to ICEwedge() then yields the equivariant, wedge-shaped region
of specified statistical confidence within [.50, .99] ...by computing ICE Angle
Order Statistics around a circle with center at the ICE Origin: (DeltaEffe, DeltaCost)
= (0, 0).

Researchers wishing to view alternative ICE Acceptability Curves would then envoke
ICEalice().

Finally, multiple calls to ICEcolor for different values of lambda and/or different
forms of (linear or nonlinear) ICE Preference Maps are typically used to illustrate
the considerable additional Economic Preference Uncertainty that can be introduced
in these ways. This Economic Preference uncertainty is superimposed on top of the
inherent Statistical Uncertainty contained within even unbiased, patient level data
on the relative cost and effectiveness of two treatments for the same disease or
health condition.
}
\author{Bob Obenchain <wizbob@att.net>}
\references{
 Black WC.  The CE plane: a graphic representation of cost-effectiveness.  \emph{Med
 Decis Making} 1990; \bold{10}: 212-214.
 
 Hoch JS, Briggs AH, Willan AR. Something old, something new, something borrowed,
 something blue: a framework for the marriage of health econometrics and
 cost-effectiveness analysis. \emph{Health Economics} 2002; \bold{11}: 415-430.

 Laupacis A, Feeny D, Detsky AS, Tugwell PX.  How attractive does a new technology have
 to be to warrant adoption and utilization?  Tentative guidelines for using clinical
 and economic evaluations. \emph{Can Med Assoc J} 1992; \bold{146(4)}: 473-81.
 
 O'Brien B, Gersten K, Willan A, Faulkner L.  Is there a kink in consumers' threshold
 value for cost-effectiveness in health care?  \emph{Health Economics}  2002; \bold{11}:
 175-180.

 Obenchain RL.  ICE Preference Maps:  Nonlinear Generalizations of Net Benefit and
 Acceptability. \emph{Health Serv Outcomes Res Method}  2008; \bold{8}: 31-56. DOI 
 10.1007/s10742-007-0027-2. Open Access.
 
 Obenchain RL. (2020) \emph{ICEinfer_in_R.PDF}  ICEinfer package vignette-like document.
 \url{http://localcontrolstatistics.org}
 
 Stinnett AA.  Adjusting for Bias in C/E Ratio Estimates. \emph{Health Economics} LETTERS
 Secton. 1996; \bold{5}: 470-472.

 Stinnett AA, Mullahy J.  Net health benefits: a new framework for the analysis of
 uncertainty in cost-effectiveness analysis.  \emph{Medical Decision Making}, Special
 Issue on Pharmacoeconomics 1998; \bold{18}: s68-s80.
}
\keyword{package}
\examples{
  demo(fluoxpin)
}