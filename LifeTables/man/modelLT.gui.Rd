\name{modelLT.gui}
\alias{modelLT.gui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical User Interface for the Model Life Table Functions
}
\description{
The function launches a graphical user interface (GUI) from which various functionality of this package can be accessed.
}
\usage{
modelLT.gui()
}

\details{
In the main window of the GUI, select an indicator of child mortality and enter its value. Optionally, choose if an adult mortality should be added and if it is the case, enter its value. Then select a sex for the life table. Pressing the \sQuote{Generate Life Table} button creates a window containing the corresponding life table. 

Now you can enter the desired life expectancy target (e0). Press the Enter key to update the life table and the alpha value. Alternatively, you can update the alpha value directly. Again, press the Enter key to update the content  of the window.

To export the life table into a csv file, press the \sQuote{Export} button. To plot any of the life table columns as a function of age, select the column in the drop-list at the bottom of the window, choose if the y-axis should be on a log scale and press \sQuote{Plot}. From here, you can save the plot in various formats.

For the GUI to run the user will first need to install GTK+. 

GTK+ can be downloaded here \url{http://www.gtk.org/download/index.php}. 

If the user does not first install GTK and invokes the GUI from the \code{R} console, a dialogue box will appear to ask if you want to install GTK.  

}
\author{
Hana Sevcikova, David Sharrow, Samuel Clark
}

\examples{
\dontrun{
modelLT.gui()}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ programming }
