%% glpkAPI Version History

\name{NEWS}
\title{glpkAPI News}
\encoding{UTF-8}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.3.4 2022-10-09}{
  \itemize{
    \item Maintainer change.
    \item Fix for CRAN issue concerning function declarations without prototype.
    \item Repository moved to GitHub and will be discontinued and archived on GitLab. 
      New location: \url{https://github.com/SysBioChalmers/sybil-glpkapi}.
  }
}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.3.3 2021-10-28}{
  \itemize{
    \item Fix for CRAN issue concerning deprecated autoconf macros.
  }
}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.3.2 2020-02-13}{
  \itemize{
    \item Quick fix for CRAN issue: 'config' variable 'CPP' is deprecated.
  }
}

% ---------------------------------------------------------------------------- %
\section{Changes in version 1.3.1 2018-09-13}{
  \itemize{
    \item{fixed a bug where an allocating function was called with an unprotected
        allocated R object in a makro funtion defined in \file{glpkAPI.h}.}
    \item{changed package maintainer}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.3.0 2015-01-05}{
  \itemize{
    \item added new vignette that describes the use of glpkAPI and GNU MathProg 
    	language (GMPL). Thanks to Louis Luangkesorn.
    \item{changed package maintainer}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.11 2014-11-16}{
  \itemize{
    \item added bsais factorization control parameters: \code{GLP_BF_LUF}
          and \code{GLP_BF_BTF}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.10 2014-01-08}{
  \itemize{
    \item wring column or row indices to \code{setMatRowGLPK} and
          \code{setMatColGLPK} will not longer stop the running R session
    \item fixed a bug in functions \code{delRowsGLPK} and \code{delColsGLPK}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.9 2013-07-27}{
  \itemize{
    \item fixed a bug in \code{loadMatrixGLPK}: symbol \code{R_orderVector} is
          first available in R version 3.0.0
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.8 2013-07-16}{
  \itemize{
    \item duplicate entries in inputs to \code{loadMatrixGLPK} will not longer
          stop the running R session
    \item added function \code{getRowsTypesGLPK}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.7 2013-05-23}{
  \itemize{
    \item a minor change in file \file{Makevars.win} so that the Windows version
          will be compiled using --enable-chkargs
    \item added functions \code{setColsNamesGLPK()}, \code{setRowsNamesGLPK()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.6 2013-02-12}{
  \itemize{
    \item prints the version number of GLPK on startup
    \item removed argument \code{check} from function \code{loadMatrixGLPK}
    \item if compiled with --enable-chkargs [which is the default]), arguments
          to GLPK API functions containing GLPK constants or row and column
          indices are checked more carefully
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.5 2012-11-14}{
  \itemize{
    \item very basic support for the GLPK callback routine
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.4 2012-07-06}{
  \itemize{
    \item inconsistent inputs to \code{loadMatrixGLPK} will not longer stop
          the running R session
    \item new argument \code{check} to function \code{loadMatrixGLPK}:
          if set to \code{TRUE} (and \pkg{glpkAPI} compiled with
          --enable-chkargs [which is the default]), other arguments to
          \code{loadMatrixGLPK} will be reviewed in terms of vector length and
          valid row and column indices, before changing the problem object
    \item functions \code{setMatRowGLPK} and \code{getMatRowGLPK} can use
          vectors returned by \code{getMatRowGLPK} and \code{getMatColGLPK}
          respectively
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.3 2012-04-20}{
  \itemize{
    \item bugfix in \file{configure.ac} when compiling with \command{R32}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.2 2012-03-27}{
  \itemize{
    \item renamed \code{glpkPtr} method \code{isNULLpointer} to
          \code{isNULLpointerGLPK}

    \item renamed \code{glpkPtr} method \code{pType} to \code{glpkPtrType}

    \item renamed \code{glpkPtr} method \code{ptr} to \code{glpkPointer}

    \item renamed \code{glpkPtr} slot \code{pType} to \code{glpkPtrType}

    \item renamed \code{glpkPtr} slot \code{ptr} to \code{glpkPointer}

    \item updated \file{configure.ac} script

    \item file \file{NEWS} is now in \file{inst/NEWS.Rd}

    \item fixed a bug in \code{getCbindGLPK}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.1 2012-03-15}{
  \itemize{
    \item class \code{glpkPtr} does not extend \code{externalptr} (not allowed
          in R version <= 2.14)

    \item method show for objects of class \code{glpkPtr}

    \item fixed a bug in \file{init.c}: number of arguments to
          \code{mplAllocWksp} was wrong

    \item added methods \code{isGLPKpointer}, \code{isNULLpointer} and
          \code{isTRWKSpointer} for objects of class \code{glpkPtr}

    \item make use of \code{glp_error_hook} in \file{glpkAPI.c} in order to
          prevent GLPK exiting abnormal and so terminating the running R session

    \item check arguments for row and column indices (\code{i} and \code{j}) if
          they fit to the given problem object (again: prevent GLPK from
          terminating the running R session)
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.2.0 2012-02-14}{
  \itemize{
    \item exported API-functions are registered

    \item class \code{glpkPtr} extends \code{externalptr}

    \item file \file{c2r.map} in \file{inst/}: mapping of filenames between
          C API and \pkg{glpkAPI}, run
      \preformatted{
c2r <- system.file(package = "glpkAPI", "c2r.map")
source(c2r)
      }
      in order to use original function names of the GLPK C API.

    \item fixed a bug in \code{copyProbGLPK()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.9 2012-02-13}{
  \itemize{
    \item added finalizers for GLPK problem objects and MathProg
          translator workspace objects

    \item Functions \code{initProbGLPK()} and \code{mplAllocWkspGLPK()} return
          instances of class \code{glpkPtr}. This class has two data slots:
    \tabular{ll}{
      pType \tab a character string giving the pointer type and \cr
      ptr   \tab a pointer to a C structure                     \cr
    }
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.8 2012-02-07}{
  \itemize{
    \item added MathProg support:
          \code{mplAllocWkspGLPK()},
          \code{mplBuildProbGLPK()},
          \code{mplFreeWkspGLPK()},
          \code{mplGenerateGLPK()},
          \code{mplPostsolveGLPK()},
          \code{mplReadDataGLPK()} and
          \code{mplReadModelGLPK()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.7 2012-01-10}{
  \itemize{
    \item some minor changes in \file{configure.ac}
    \item empty \file{configure.win} file and some finetuning in
          \file{Makevars.win}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.6 2012-01-09}{
  \itemize{
    \item some minor changes in \file{configure.ac}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.5 2011-07-28}{
  \itemize{
    \item added \file{NAMESPACE}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.4 2011-07-27}{
  \itemize{
    \item removed file \file{COPYING}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.3 2011-07-27}{
  \itemize{
    \item updated \file{configure.ac} script
    \item updated package vignette
    \item added files \file{INSTALL} and \file{README}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.2 2011-07-12}{
  \itemize{
    \item renamed from sybilGLPK to \pkg{glpkAPI}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.1 2011-06-14}{
  \itemize{
    \item fixed a bug in \code{getColsKind()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.1.0 2011-05-17}{
  \itemize{
    \item updated documentation
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.0.4 2011-05-09}{
  \itemize{
    \item renamed functions \code{getFluxDistGLPK()} and
          \code{getFluxDistIptGLPK()} to \code{getColsPrimGLPK()} and
          \code{getColsPrimIptGLPK()} respectively

    \item added functions \code{getColKindGLPK()},
                          \code{getColsDualGLPK()},
                          \code{getColsDualIptGLPK()},
                          \code{getColsKindGLPK()},
                          \code{getColsStatGLPK()},
                          \code{getMIPParmGLPK()},
                          \code{getNumBinGLPK()},
                          \code{getNumIntGLPK()},
                          \code{getRowsDualGLPK()},
                          \code{getRowsDualIptGLPK()},
                          \code{getRowsPrimGLPK()},
                          \code{getRowsPrimIptGLPK()},
                          \code{getRowsStatGLPK()},
                          \code{mipColValGLPK()},
                          \code{mipColsValGLPK()},
                          \code{mipObjValGLPK()},
                          \code{mipRowValGLPK()},
                          \code{mipRowsValGLPK()},
                          \code{mipStatusGLPK()},
                          \code{printIptGLPK()},
                          \code{printMIPGLPK()},
                          \code{printSolGLPK()},
                          \code{readIptGLPK()},
                          \code{readMIPGLPK()},
                          \code{readSolGLPK()},
                          \code{setColKindGLPK()},
                          \code{setColsKindGLPK()},
                          \code{setDefaultMIPParmGLPK()},
                          \code{setMIPParmGLPK()},
                          \code{solveMIPGLPK()},
                          \code{writeIptGLPK()},
                          \code{writeMIPGLPK()} and
                          \code{writeSolGLPK()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.0.3 2011-03-08}{
  \itemize{
    \item fixed a bug in \code{findColGLPK()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.0.2 2011-02-15}{
  \itemize{
    \item added functions \code{checkDupGLPK()},
                          \code{sortMatrixGLPK()},
                          \code{delRowsGLPK()},
                          \code{delColsGLPK()},
                          \code{setBfcpGLPK()},
                          \code{getBfcpGLPK()},
                          \code{readProbGLPK()},
                          \code{writeProbGLPK()},
                          \code{getBheadGLPK()},
                          \code{getRbindGLPK()} and
                          \code{getCbindGLPK()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.0.1 2010-11-04}{
  \itemize{
    \item added functions \file{setMatRowGLPK()} and \file{setMatColGLPK()}
  }
}


% ---------------------------------------------------------------------------- %
\section{Changes in version 1.0.0 2010-08-19}{
  \itemize{
    \item initial public release
  }
}


