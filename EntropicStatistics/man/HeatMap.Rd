\name{HeatMap}
\alias{HeatMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{HeatMap for Distribution Visualization
}
\description{
Returns a heatmap to display characteristic information from selected groups.
}
\usage{
HeatMap(
  data_frequency_list,
  orders = seq(0.50, 3, by = 0.01),
  selection = 1:length(data_frequency_list),
  plot_order = selection,
  RowNames = names(data_frequency_list)[plot_order],
  title = "HeatMap",
  x_ticks = round(stats::quantile(orders, c(0,0.25, 0.5, 0.75, 1)), 2),
  plot_margin = ggplot2::margin(0.5,0.2,0.2,1,"cm"),
  text_face = 1,
  fill_colors = c("blue4", "white", "red3"),
  title_text_size = 25,
  label_text_size = 25
)
}

\arguments{
  \item{data_frequency_list}{
  A list contains the frequency of data. Each sublist herein is a frequency counts of a group.
}
\item{orders}{
  Orders of Generalized Shannon's Entropy used in the heatmap.
}
\item{selection}{
  Indexes of sublist in \code{data_frequency_list} that one wishes to include in the heatmap.
}
\item{plot_order}{
  The order of selected groups in the heatmap, from bottom to top.
}
\item{RowNames}{
  The display names of the selected groups in the heatmap.
}
\item{title}{
  The title of the heatmap.
}
\item{x_ticks}{
  The location of x-axis ticks on the heatmap.
}
\item{plot_margin}{
  The plot margins of the final heatmap.
}
\item{text_face}{
  The text style in the heatmap. \code{1} = “plain”,  \code{2} = “italic”,  \code{3} = “bold”, and \code{4} = “bold. italic”.
}
\item{fill_colors}{
  Three colors in the heatmap that represent lower, medium, and upper values.
}
\item{title_text_size}{
  Title text size in the heatmap.
}
\item{label_text_size}{
  Labels text size in the heatmap.
}
}
\details{
This is a preliminary tool to identify distributional information from multiple groups simultaneuously without any parametric assumptions.
}
\value{
A \code{ggplot} object representing the heatmap.
}
\author{
Jialin Zhang (JZ) <jzhang@math.msstate.edu>
}

\examples{
## Creating data
binom_n <- 10
sample_size <- 1000
sample_1 <- table(stats::rbinom(size=binom_n, n=sample_size, 0.1))
sample_2 <- table(stats::rbinom(size=binom_n, n=sample_size, 0.2))
sample_3 <- table(stats::rbinom(size=binom_n, n=sample_size, 0.3))
sample_4 <- table(stats::rbinom(size=binom_n, n=sample_size, 0.4))
sample_5 <- table(stats::rbinom(size=binom_n, n=sample_size, 0.5))
sample_6 <- table(stats::rbinom(size=binom_n, n=sample_size, 0.6))
sample_7 <- table(stats::rbinom(size=binom_n, n=sample_size, 0.7))
sample_8 <- table(stats::rbinom(size=binom_n, n=sample_size, 0.8))
sample_9 <- table(stats::rbinom(size=binom_n, n=sample_size, 0.9))
sample_poisson_1 <- stats::rpois(sample_size, 1)
sample_poisson_2 <- stats::rpois(sample_size, 2)
sample_poisson_3 <- stats::rpois(sample_size, 3)
sample_poisson_4 <- stats::rpois(sample_size, 4)
sample_poisson_5 <- stats::rpois(sample_size, 5)
sample_poisson_6 <- stats::rpois(sample_size, 6)
sample_poisson_7 <- stats::rpois(sample_size, 7)
sample_poisson_8 <- stats::rpois(sample_size, 8)
sample_poisson_9 <- stats::rpois(sample_size, 9)
data_samples <- list(binom_0.1 = sample_1, binom_0.2 = sample_2, binom_0.3 = sample_3,
binom_0.4 = sample_4, binom_0.5 = sample_5, binom_0.6 = sample_6, binom_0.7 = sample_7,
binom_0.8 = sample_8, binom_0.9 = sample_9, Poisson_1 = sample_poisson_1,
Poisson_2 = sample_poisson_2, Poisson_3 = sample_poisson_3, Poisson_4 = sample_poisson_4,
Poisson_5 = sample_poisson_5, Poisson_6 = sample_poisson_6, Poisson_7 = sample_poisson_7,
Poisson_8 = sample_poisson_8, Poisson_9 = sample_poisson_9)

## Obtain the heatmap for all sublists in the data.
HeatMap(data_samples)

## Obtain the heatmap for six random sublists in the data.
HeatMap(data_samples, selection = c(sample(1:length(data_samples), 6)))

## Obtain the heatmap for the binomial sublists in the data.
HeatMap(data_samples, selection = 1:9)

## Obtain the heatmap for the first 4 poisson sublists in the data.
HeatMap(data_samples, selection = 10:13)

## Obtain the heatmap for the last 5 poisson sublists in the data.
HeatMap(data_samples, selection = 14:18)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\concept{heatmap}
\concept{gse}
% For non-standard keywords, use \concept instead of \keyword:
\concept{Entropic Statistics}
