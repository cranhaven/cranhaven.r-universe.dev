\name{rpart.JGR}
\alias{rpart.JGR}
\title{Dialog box for classification and regression trees}
\description{
  Provides dialog box for fitting a classification and regression tree.
}
\details{
  \strong{User Interface}

  Select \emph{Analysis Tools -> Tree Regression} from the menus.  A dialog box
  will open.  Select the data set of interest from the pull-down menu,
  or browse for a tab-delimited text file.  The Data Subsetting tab can
  be used to select a subset of the data file by choosing a variable
  from the pull down menu and then selecting the levels of that variable
  to include.  You can hold down the <CTRL> key to add several different
  levels.
  
Select a \strong{Dependent} variable (the variable you wish to predict).
Select all \strong{Independent} variables you wish to include in the model (the
variables used to predict the dependent variable).  You may hold down
the <CTRL> key to several variables.  Note: the dependent variable is in
the list of possible independent variables, but it should not be
selected as an independent variable, or a degenerate regression will be
produced.

You can set further options for controlling the tree regression by
setting the three analysis options.

The output is a graph containing the tree structure and its estimated
regression coefficients. 
}


\seealso{
  \code{\link[rpart]{rpart}}
}
