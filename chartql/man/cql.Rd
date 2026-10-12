\name{cql}
\alias{cql}
\title{\code{chartql} Chart Generator}
\usage{
cql(source_frame, cql_string)
}
\description{
Generates a chart/plot using \code{source_frame} as the source dataframe and using \code{cql_string} as the query string. Options and structure of the query string are described below:
}
\arguments{
\item{source_frame}{DataFrame object. Used as the source to build the plot.}
\item{cql_string}{\code{chartql} query string. Used to specify options in terms of how to build the plot. This includes the type of plot, choice of x and y variables, axis titles and more. Full parameter list below.}
}
\value{
\item{response}{List object. Entries are the valid parameters and their values extracted from \code{cql_string}.}
\item{plot_obj}{ggplot object. This is the resulting plot.}
}
\details{
The \code{chartql} query language uses a fairly simple format to let users generate amazing plots but with minimal scripting, allowing for much faster prototyping. The \code{chartql} string uses the following general format:

\code{CHART <chart_type> X <x_var> Y <y_var> (Options) <options>}

The main variables that are required for all types of plots are:


\tabular{rllll}{
{Chart} \code{<chart_type>}: \tab\tab\tab\tab \code{bar|scatter|hist|line}\cr
{X} \code{<x_var>, <(x_facet)>}: \tab\tab\tab\tab  X-axis variable. May include a second categorical\cr
\tab\tab\tab\tab variable (comma separated)\cr
{Y} \code{<y_var>}: \tab\tab\tab\tab Y-axis variable. *Not used for \code{hist}.\cr
}


\bold{Optional Variables}

\tabular{rllll}{
{X_label} \code{'<xlab>'}: \tab\tab\tab\tab  X-axis custom label. Defaults to <x_var>\cr
{Y_label} \code{'<ylab>'}: \tab\tab\tab\tab  Y-axis custom label. Defaults to <y_var>\cr
{Legend} \code{'<legend>'}: \tab\tab\tab\tab  Legend custom label. Defaults to <x_category>\cr
{Colorset} \code{'<clist>'}: \tab\tab\tab\tab  Custom set of colors for <x_category> levels.\cr
\tab\tab\tab\tab Must be comma-separated list.\cr
{AggFunc} \code{'<func>'}: \tab\tab\tab\tab Summary function for \code{bar} type.\cr
\tab\tab\tab\tab Valid types: \code{mean|median|count|sum}.\cr
\tab\tab\tab\tab Defaults to \code{mean}.\cr
{Fit} \code{'<show_se>'}: \tab\tab\tab\tab Valid types: \code{true|false}. Fit quadratic line.\cr
\tab\tab\tab\tab Show/Don't show standard error curves.\cr
{ConfInt} \code{'<interval>'}: \tab\tab\tab\tab  Show error bars. Value is confidence interval value. E.g. '.95'
}
}
\note{
\enumerate{
\item Custom label parameters \code{X_label}, \code{Y_label} and \code{Legend} may contain newlines but should be escaped with an extra backslash as: "\\\\n".
\item The parameter names themselves are not case-sensitive (e.g. both \code{"CHART bar"} and \code{"chart bar"} are valid formatting).
\item You can hide the Legend by setting the value to "<>" (e.g. \code{"Legend '<>'"})
}
}
\examples{

# Test data
dframe <- data.frame(
category = factor(c(rep("Sports",50), rep("Home", 150), rep("Fashion", 100))),
season = factor(c(rep(c("Fall","Winter"),150))),
sales = c(runif(100,min=0,max=100), runif(100,min=50,max=200), runif(100,min=50,max=80))
)
dframe$visitors <- dframe$sales * runif(300, min=0.5, max=1.5)


# Bar chart with product category on x-axis and total sales on y-axis
cql_str <- "CHART bar X category Y sales";
cql(dframe, cql_str);


# Bar chart but facet x-axis with both category and season of the year
cql_str <- "CHART bar X category, season Y sales X_label 'Product\\\\nCategory'";
cql(dframe, cql_str);


# Bar chart with 95\% confidence interval error bars
cql_str <- "CHART bar X category, season Y sales ConfInt '.95'";
cql(dframe, cql_str);


# Bar chart but specify the colors for each season
cql_str <- "CHART bar X category, season Y sales Colorset '#FF9900, #990099'";
cql(dframe, cql_str);


# Scatter plot of number of visitors and sales
cql_str <- "CHART scatter X visitors Y sales";
cql(dframe, cql_str);


# Scatter plot but facet by season
cql_str <- "CHART scatter X visitors, season Y sales";
cql(dframe, cql_str);


# Scatter plot with fitted line (no SE curves)
cql_str <- "CHART scatter X visitors, season Y sales Fit 'false'";
cql(dframe, cql_str);
}
