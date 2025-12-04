\name{ranks_read_genres}
\alias{ranks_read_genres}
\docType{data}
\title{Reading Genres Data (partial rankings with covariates)}
\description{The Reading Genres dataset was collected through an on-line survey conducted in Italy to investigate reading preferences in the context of the 2019 project \emph{Patto per la lettura – Conta chi legge}. The questionnaire was administrated by the municipality of Latina (Latium, Italy), in collaboration with Sapienza University of Rome and the School of Government of the University of Tor Vergata. A sample of \eqn{N=507} respondents provided their partial top-5 rankings of \eqn{n=11} reading genres according to their personal preferences. The reading genres are: 1 = Classic, 2 = Novel, 3 = Thriller, 4 = Fantasy, 5 = Biography, 6 = Teenage, 7 = Horror, 8 = Comics, 9 = Poetry, 10 = Essay and 11 = Humor. The dataset also includes several covariates concerning respondents' socio-demographics characteristics and other free time activities.}
\usage{data(ranks_read_genres)}
\format{
A data frame gathering \eqn{N=507} partial top-5 rankings of the reading genres in the first \eqn{n=11} columns (rank 1 = most preferred item) and individual covariates in the remaining columns. Missing positions are coded as \code{NA}. The variables are detailed below:
\describe{
\item{Classic}{Rank assigned to Classic.}
\item{Novel}{Rank assigned to Novel.}
\item{Thriller}{Rank assigned to Thriller.}
\item{Fantasy}{Rank assigned to Fantasy.}
\item{Biography}{Rank assigned to Biography.}
\item{Teenage}{Rank assigned to Teenage.}
\item{Horror}{Rank assigned to Horror.}
\item{Comics}{Rank assigned to Comics.}
\item{Poetry}{Rank assigned to Poetry.}
\item{Essay}{Rank assigned to Essay.}
\item{Humor}{Rank assigned to Humor.}
\item{Gender}{Gender.}
\item{Region}{Italian region of residence.}
\item{Age}{Age (years).}
\item{N_children}{Number of children.}
\item{Education}{Education level.}
\item{Final_mark}{Final grade of the education degree, scaled in the interval [6,10].}
\item{N_books}{Number of books read in the last 12 months.}
}
}
\references{
Crispino M, Mollica C, Astuti V and Tardella L (2023). Efficient and accurate inference for mixtures of Mallows models with Spearman distance. \emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.

Mollica (2019). On-line questionnaire of the Italian 2019 project \emph{Patto per la lettura – Conta chi legge} available at \url{https://form.jotformeu.com/90275118835359}.

}
\examples{
str(ranks_read_genres)
head(ranks_read_genres)
}
\keyword{datasets}
