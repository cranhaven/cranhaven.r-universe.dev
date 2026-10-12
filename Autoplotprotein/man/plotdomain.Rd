\name{plotdomain}
\alias{plotdomain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ploting domain
}
\description{
Draw the domain of the protein
}
\usage{
plotdomain()
}
%- maybe also 'usage' for other objects documented here.
\details{
The tool ennable visualization of amino acid changes at the protein level,The scale of a protein domain and the position of a functional motif/site will be precisely defined. The features available include domains
}
\value{

The starting position, end position and name of the protein domain
}
\references{
https://cran.r-project.org/doc/manuals/R-exts.html
}
\author{
Xiaoyu Zhang
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
code{\link{help}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    protein = read.table("Protein.txt", sep = "\t", stringsAsFactors = F)
    domain = read.table("Domain.txt", sep = "\t", stringsAsFactors = F)
    length = read.table("Length.txt", sep = "\t", stringsAsFactors = F)
    site = read.table("Site.txt", sep = "\t", stringsAsFactors = F)
    muta = read.table("Mutagenesis.txt", sep = "\t", stringsAsFactors = F)
    option = read.table("Option.txt", sep = "\t", stringsAsFactors = F)
    zoomin = read.table("ZoomIn.txt", sep = "\t", stringsAsFactors = F)
    Domain = function(start, end, name, height = -0.3, color = "orange", 
        face = "stereoscopic", protein_width, x_y) {
        h1 = -2.8
        h2 = -3.1
        dec = 2 * nchar(name) * protein_width/100
        if (face == "stereoscopic") {
            cylindrect(start, h1, end, h2, col = color, gradient = "y")
        }
        else {
            rect(start, h1, end, h2, col = color)
        }
        if (end - start >= dec) {
            par(srt = 0)
            text((end + start)/2, h1 + height/2, name, cex = 0.7)
            isContain = TRUE
        }
        else {
            isContain = FALSE
        }
        isContain
    }
    Domain_w = function(domain_pos, domain_name, protein_width) {
        dec = 1.4 * protein_width/100
        position2 = 1:length(domain_pos)
        position2[1] = domain_pos[1]
        if (length(domain_pos) > 1) {
            for (i in 2:length(domain_pos)) {
                if (domain_pos[i] - domain_pos[i - 1] <= dec) {
                  if (domain_pos[i] != domain_pos[i - 1]) {
                    position2[i] = position2[i - 1] + dec
                  }
                  else {
                    position2[i] = position2[i - 1]
                  }
                }
                else {
                  position2[i] = domain_pos[i]
                }
            }
        }
        return(position2)
    }
    Domain_h = function(position, position2, name, height = -0.3, 
        x_y, up_down) {
        h1 = -0.1
        h2 = -0.2
        h = -0.4
        hh1 = -2.8
        if (up_down == "up") {
            if (position == position2) {
                segments(position, hh1 + height, position, hh1 + 
                  height + h)
            }
            else {
                segments(position, hh1 + height, position, hh1 + 
                  height + h1)
                segments(position2, hh1 + height + h - h2, position2, 
                  hh1 + height + h)
                segments(position, hh1 + height + h1, position2, 
                  hh1 + height + h - h2)
            }
            text(position2, hh1 + height + h - 0.02, name, srt = 90, 
                adj = c(0, 0.5), cex = 0.8)
        }
        else {
            if (position == position2) {
                segments(position, hh1, position, hh1 - h)
            }
            else {
                segments(position, hh1, position, hh1 - h1)
                segments(position2, hh1 - h + h2, position2, 
                  hh1 - h)
                segments(position, hh1 - h1, position2, hh1 - 
                  h + h2)
            }
            text(position2, hh1 - h + 0.02, name, srt = 270, 
                adj = c(0, 0.5), cex = 0.8)
        }
    }
    if (!is.na(domain[1, 1])) {
        domainn = domain
        count = 0
        for (i in 1:nrow(domainn)) {
            isContain = Domain(start = as.numeric(domainn[i, 
                1]), end = as.numeric(domainn[i, 2]), name = as.character(domainn[i, 
                3]), height = as.numeric(protein[4]), color = i + 
                1, face = protein[6], protein_width = as.numeric(length[2]), 
                x_y = flag)
            if (isContain == TRUE) {
                domain = domain[-i + count, ]
                count = count + 1
            }
        }
        domain2 = (domain[, 1] + domain[, 2])/2
        if (length(domain2) != 0) {
            flag = TRUE
            if (flag == TRUE) {
                position3 = Domain_w(domain2, domain[, 3], as.numeric(length[2]))
            }
            for (i in 1:nrow(domain)) {
                position1 = (as.numeric(domain[i, 1]) + as.numeric(domain[i, 
                  2]))/2
                Domain_h(position = position1, position2 = position3[i], 
                  name = as.character(domain[i, 3]), height = as.numeric(protein[4]), 
                  x_y = flag, up_down = "down")
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot domain}
\keyword{ file }
