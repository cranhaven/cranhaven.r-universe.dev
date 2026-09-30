\name{autism}
\alias{autism}
\docType{data}
\title{
Autism dataset
}
\description{
Total RNA obtained from lmyphoblast cell lines derived from 250 individuals, 137 of which suffer from autism and 113 are healthy. The dataset consists of four batches of sizes 101, 96, 45 and 8.
}
\usage{data(autism)}
\format{
1) \code{X} - the covariate matrix: a matrix of dimension 250 x 1000, containing the numerical transcript values 

2) \code{batch} - the batch variable: a factor with levels '1', '2', '3' and '4'

3) \code{y} - the target variable: a factor with levels '1' corresponding to 'healthy' and '2' corresponding to 'autism'
}
\details{
The RNA measurements were obtained by the Illumina HumanRef-8 v3.0 Expression BeadChip featuring 24,526 transcripts. To reduce computational burden of potential analyses performed using this dataset we randomly selected 1,000 of these 24,526 transcripts. Moreover, the original dataset consisted of five batches and contained measurements of 439 individuals. Again to reduce computational burden of potential analyses we excluded the biggest batch featuring 189 individuals resulting in the 250 individuals included in the dataset made available in \code{bapred}.
}
\source{
ArrayExpress, accession number: E-GEOD-37772
}
\references{
Luo, R., Sanders, S. J., Tian, Y., Voineagu, I., Huang, N., Chu, S. H., Klei, L., Cai, C., Ou, J., Lowe, J. K., Hurles, M. E., Devlin, B., State, M. W., Geschwind, D. H. (2012). Genome-wide Transcriptome Profiling Reveals the Functional Impact of Rare De Novo and Recurrent CNVs in Autism Spectrum Disorders. The American Journal of Human Genetics 91:38–55, <\doi{10.1016/j.ajhg.2012.05.011}>.
}
\examples{
data(autism)
}
