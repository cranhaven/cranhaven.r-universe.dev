\name{NEWS}
\title{News for Package "R2SWF"}

\section{Changes in R2SWF version 0.9-9}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Updated the linking on Windows for upcoming version of Rtools,
            thanks to Tomas Kalibera.
    }
  }
}

\section{Changes in R2SWF version 0.9-8}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed compiler warnings.
    }
  }
}

\section{Changes in R2SWF version 0.9-7}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Supported UCRT on Windows, thanks to Tomas Kalibera and Uwe Ligges.
    }
  }
}

\section{Changes in R2SWF version 0.9-6}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed the configure script, thanks to Prof. Brian Ripley.
      \item Fixed the issue of not exporting the initialization function on Windows.
      \item Fixed various compiler warnings.
    }
  }
}

\section{Changes in R2SWF version 0.9-5}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a string comparison issue in the configure script
      \item Fixed a bug in testing the capability of Cairo-based devices
    }
  }
}

\section{Changes in R2SWF version 0.9-4}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New device functions to support future R graphics engine,
            thanks to Prof. Paul Murrell
    }
  }
}

\section{Changes in R2SWF version 0.9-3}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Updated libming
      \item Fixed a \code{-Wlogical-not-parentheses} warning, thanks to Prof. Ripley
    }
  }
}

\section{Changes in R2SWF version 0.9-2}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Added a cleanup script per CRAN's policy
      \item Registered native routines per CRAN's policy
      \item Fixed PROTECT errors detected by CRAN
    }
  }
}

\section{Changes in R2SWF version 0.9-1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a compilation conflict with FreeType 2.6, thanks to Kurt Hornik
    }
  }
}

\section{Changes in R2SWF version 0.9}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a bug in setting the dimension of the SWF file that occurs in
            \code{image2swf()}
      \item Eliminated R CMD check warnings
    }
  }
}

\section{Changes in R2SWF version 0.8-2}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a misuse of \code{abs()} in the C code, thanks to Prof. Ripley
    }
  }
}

\section{Changes in R2SWF version 0.8-1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a memory bug detected by valgrind, thanks to Prof. Ripley
    }
  }
}

\section{Changes in R2SWF version 0.8}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item SWF graphics device now supports the \code{hadj} parameter
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item Functions \code{list.fonts()} and \code{add.fonts()} were removed
      \item Functions \code{font.paths()}, \code{font.families()}, \code{font.files()}
            and \code{font.add()} were moved to the \pkg{sysfonts} package that the
            current version of \pkg{R2SWF} depends on
    }
  }
}

\section{Changes in R2SWF version 0.7-1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed a bug in detecting the OS type
    }
  }
}

\section{Changes in R2SWF version 0.7}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Added function \code{font.files()} to list available font files
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed \code{swf2html()} to make it work properly with knitr hook
            \code{hook_r2swf()}
      \item Renamed \code{list.fonts()} to \code{font.families()}, and
            \code{add.fonts()} to \code{font.add()}.
            Old function names are deprecated but kept for compatibility
    }
  }
}

\section{Changes in R2SWF version 0.6-1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Removed \code{configure.win} so it should be built on both architectures
            on Windows
      \item Fixed loading problem on R 2.15.3
    }
  }
}

\section{Changes in R2SWF version 0.6}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Clarified the copyrights
      \item Removed the source code of some external libraries
    }
  }
}

\section{Changes in R2SWF version 0.5-0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item A SWF device for R through \code{swf()}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Takes care of absolute and relative paths of files
      \item Remove '\code{outdir}' argument from \code{dev2swf()}
    }
  }
}

\section{Changes in R2SWF version 0.4-1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Updated zlib to 1.2.8
      \item Updated libpng to 1.6.2
      \item Added FreeType 2.5.0
      \item Updated libming to 0.4.5
      \item Fixed several memory leak problems
      \item Uses absolute paths for \code{dev2swf()}, \code{swf2html()} etc.
      \item HTML file created by \code{swf2html()} will make a copy of the SWF file
            in the same directory
    }
  }
}

\section{Changes in R2SWF version 0.4}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item A new function \code{file2swf()} as a wrapper of \code{image2swf()}
            and \code{svg2swf()}
      \item \code{swf2html()} uses a simple \code{<embed>} tag instead of the long ugly old
            \code{<object>} tag to write the Flash file in HTML; it also gained a new
            argument '\code{fragment}' to decide whether to return an HTML fragment or
            a complete HTML document
    }
  }
}

\section{Changes in R2SWF version 0.3-1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Windows version of R supports \code{svg()} since 2.14.0.
            Now \code{svg2swf()} also accepts SVG files created by \code{svg()}
      \item \code{dev2swf()} now supports \code{svg()} device
    }
  }
}

\section{Changes in R2SWF version 0.3-0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Allows users to set the background color of the output SWF file
      \item Re-designed the arguments list of \code{image2swf()} and
            \code{svg2swf()} for ease of use
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item (Internal) Uses a more appropriate way to insert frames in a SWF file
    }
  }
}

\section{Changes in R2SWF version 0.2-2}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed Makefile errors
    }
  }
}

\section{Changes in R2SWF version 0.2-1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed compilation errors in Windows
    }
  }
}

\section{Changes in R2SWF version 0.2-0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{svg2swf()} to convert SVG files to a Flash animation
    }
  }
}

\section{Changes in R2SWF version 0.1-0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{image2swf()} and \code{dev2swf()} to convert png/jpeg files
            into a Flash animation
      \item \code{swf2html()} to embed a Flash file into an HTML page
    }
  }
}
