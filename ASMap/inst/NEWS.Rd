\name{news}
\title{Release information for ASMap}
\section{Changes in version 1.0-x}{
\subsection{README}{
  \itemize{
    \item Version 1.0-x of the \pkg{ASMap} package celebrates the
    Journal of Statistical Software publication, Taylor and Butler
    (2017). The paper succinctly summarizes the important contents of
    the package vignette and provides a useful quick reference for the
    functionality of the package. If you use \pkg{ASMap} in your
    publication please reference it using the bibliographic reference obtainable using
    \code{citation("ASMap")} and given below.
    \item Taylor, J., Butler, D. (2017) R Package ASMap: Efficient Genetic
    Linkage Map Construction and Diagnosis. Journal of Statistical Software,
    \bold{79}(6), 1--29.
  }
}
\subsection{NEW FEATURES}{
    \itemize{
      \item There is now a p-value function to emulate Figure 1.1 in
      the vignette. The function allows users to specify their own set
      of threshold distances and population sizes, making it easier to
      determine the correct choice of \code{p.value} to use in
      \code{mstmap.cross} and \code{mstmap.data.frame}.
      \item The function \code{combineMap} has been updated to include
      merging of linkage maps by \code{"marker"} or
      \code{"genotype"}. Merging by \code{"genotype"} is identical to
      the previous version of the function. Merging by
      \code{"marker"} provides the ability to combine linkage maps with
     different sets of genotypes through common markers.
    }
  }
\subsection{BUG FIXES}{
  \itemize{
    \item \code{fixClones} muddled linkage groups when an inputted map
    had more than one linkage group. This has now been amended.
    \item The \code{fixClones} function crashed when \code{consensus =
      FALSE} was set. This has now been amended.
    \item The \code{cross.scheme} of the object was lost when pushing
    back markers using \code{pushCross}. This has now been amended.
    \item The function \code{quickEst} crashed when there were linkage
    groups with one marker. This has now been amended.
    \item The function \code{alignCross} crashed with a non-descript
    error when there were no matching markers in supplied reference
    maps. This has now been amended to provide a more descript error.
   }
 }
 }
\section{Changes in version 0.4-x}{
  \subsection{README}{
  \itemize{
  \item This is the first documented NEWS release of ASMap. For this
  reason it contains information about features of the package as well
  as recent bug fixes.
 }
}
\subsection{NEW FEATURES}{
    \itemize{
      \item The package now contains a function to assist in identifying
      and orienting linkage groups (see \code{?alignCross}).
      \item The heat map in the package now uses a diverging colour
      palette chosen from the default palettes available in
      \pkg{RColorBrewer}.
      \item The package now includes a PDF vignette. This can be found
      on any operating system using the command

      \code{> vignette("ASMap")}

      The vignette includes extensive descriptions of the package
      functions as well as fully worked example that showcases the
      package functions throuh pre-diagnosis, construction and post
      diagnosis of a linkage map. It also describes how the package can
      be used efficiently for techniques such as fine mapping or combining
      maps.
      \item The package now includes three fully documented marker data
      sets for users to explore. See the vignette for more details.
}
}
\subsection{BUG FIXES}{
  \itemize{
    \item Fixed a nasty bug in \code{combineMap()} that switched
      genotype names for certain linkage maps
     \item Fixed bug that returned inflated distances for \code{"ARIL"}
     populations when using \code{mstmap.data.frame()}.
     \item If the constructed map contained linkage groups with one
      marker \code{profileMark()} graphically displaced the interval
      statistics. This has now been amended.
     \item \code{pushCross()} can now push large volumes of co-located
      markers back into the constructed map very efficiently.
    \item \code{profileMark()} was not outputting \code{crti.val} column
      when \code{crit.val = "bonf"} argument was chosen. This has now
      been amended.
      \item \code{quickEst()} has been improved and now provides an even
      more accurate and efficient distance calculations for linkage maps.
    }
}
}


