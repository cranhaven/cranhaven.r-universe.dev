\name{NEWS}
\title{News for Package \pkg{fscaret}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}


\section{Changes in version 0.9.4.4}{
\itemize{

\item Get rid of vectorized conditions in if() statements, this fix was suggested by Kurt Hornik.
\item A bug in importance calculation summary was fixed. A bug description: showing NA values of SUM when one of the models yielded NA values.


}

}


\section{Changes in version 0.9.4.3}{
\itemize{

\item Windows related bug was fixed, though starting from version 0.9.4.3 parallel implementation is deprecated.


}

}


\section{Changes in version 0.9.4.2}{
\itemize{

\item Updated code to remove "WARN" from CRAN checks in R version >= 3.4.4.


}

}


\section{Changes in version 0.9.4.1}{
\itemize{

\item Added scaling option. Please see manual of impCalc() for additional information.
\item Updated imports.

}

}



\section{Changes in version 0.9.4}{
\itemize{

\item Added "known issues" in vignette
\item Rd files were corrected
\item Added option "saveModel" in the function fscaret()

}

}


\section{Changes in version 0.9.3}{
\itemize{

\item Multicore functionality now available on Windows OS.
\item Function timeout() was rewritten due to multiple errors occurence of killed fork processes
}

}


\section{Changes in version 0.9}{
\itemize{

\item New component is available regarding classification problems.
\item Few minor code blocks were rewrited.

}

}


\section{Changes in version 0.8.6.3}{
\itemize{

\item Critical bug fixed, sorting by labels at reading varImp files was incorrect between v0.8.6.1 and v0.8.6.2 which inflicted importance where variables were sorted in descending order regardless input number.
\item Check on existance of RData and corresponding varImp tables was added.

}

}


\section{Changes in version 0.8.6.2}{
\itemize{

\item Done some tidying R code in regVarImp()

}

}


\section{Changes in version 0.8.6.1}{
\itemize{

\item Bunch of new algorithms added following caret development.
\item Re-written regVarImp() function.
\item Vignette was revised and few explanatory sentences added.
\item Fixed few small bugs.

}

}


\section{Changes in version 0.8.5.6}{
\itemize{

\item Small bug for parameter no.cores was fixed, now one should use numeric format, leave in NULL or simple delete the parameter.

}

}

\section{Changes in version 0.8.5.5}{
\itemize{

\item Information on founding bodies were added to description section and vigniette together with references.

}

}

\section{Changes in version 0.8.5.4}{
\itemize{

\item Multiple threads CRAN check fixed

\item Citation file added

}

}

\section{Changes in version 0.8.5.3}{
\itemize{ 

\item A buggy code creating vignette on some OS's was fixed
\item Probably buggy code reading data frame header was fixed.

}

}

\section{Changes in version 0.8.5.2}{
\itemize{ 

\item A bug found by Aleksander Mandyk fixed. If dependent package caret wasn't
      changed at time when other dependencies were changing the function stopped.
      It was fixed making the predict() fn with try() clause,
      but the resulting generalization error is NA and then in impCalc() it gets
      x100000 less rank than the worst model. Aleksander found another bug related
      models using java (RWeka especially). Also fixed.

\item A vignette was added.

\item A NEWS log was added.

\item A bug related to double output "Model calculation failed! ..." was fixed.

 }  
}

