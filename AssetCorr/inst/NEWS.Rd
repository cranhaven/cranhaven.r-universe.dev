\name{NEWS}
\title{News for Package \pkg{AssetCorr}}


\section{Changes in version 1.0.4 (2021-04-16)}{

\subsection{Bug Fixes}{

\itemize{

\item Changed soft Dependencies  \cr


}

}



}

\section{Changes in version 1.0.3 (2018-08-29)}{

\subsection{Bug Fixes}{

\itemize{

\item Minor bug and performance fixes   \cr


}

}



}


\section{Changes in version 1.0.2 (2018-07-08)}{
\subsection{New Features}{
  \itemize{
    
    \item New feature in  \code{<intraAMLE>} I:\cr
  If confidence intervals based on Duellmann and Gehde-Trapp (2004) are selected, the confidence intervals are also constructed  for the unconditional PD.
  
     \item New feature in  \code{<intraAMLE>} II:\cr
In addition to PD and intra correlation, also the asymptotic Value-at-Risk and Expected Shortfall is now calculated. Additionally  confidence intervals for both risk measures are constructed via delta method.
  
   \item New feature in  \code{<interCopula>}:\cr
The asymptotic confidence intervals are now computed analytically instead of numerically via the \code{<VineCopula>} package.
  

}

}
\subsection{Bug Fixes}{

\itemize{

\item Minor bug fixes. \cr


}

}



}


\section{Changes in version 1.0.1 (2018-06-20)}{
\subsection{New Features}{
  \itemize{
    
    \item New function \code{<interALL>}:\cr
    Combines all available inter correlation functions to investigate the dependencies between default time series in detail. Examples can be found in the vignette.
  
   \item New function \code{<analyze_AssetCorr>}: \cr
  Combines all available intra and inter correlation functions to investigate the dependencies withing a portfolio in detail. Examples can be found in the vignette.
  
  \item New function  \code{<intraBeta>}:\cr
  Estimating the intra correlation by matching Value-at-Risks, accoding to Botha and van Vuuren (2010)
  
    \item New function  \code{<intraMode>}: \cr
  Estimating the intra correlation by matching the theoretical and empirical estimated mode, accoding to Botha and van Vuuren (2010)
  
}

}
\subsection{Bug Fixes}{

\itemize{

\item Bug fixes in bootstrap correction applications.\cr


}

}

\subsection{Other}{

\itemize{

\item Reduction of package dependencies.\cr


}

}
}


