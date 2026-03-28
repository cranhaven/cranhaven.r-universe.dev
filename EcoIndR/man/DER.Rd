\name{DER}
\alias{DER}
\encoding{latin1}
\title{DIFFERENTIATING SAMPLES USING RARITY, HETEROGENEITY, EVENNESS, TAXONOMIC, PHYLOGENETIC AND FUNCTIONAL DIVERSITY INDICES}
\description{An algorithm for differentiating samples on the basis of the rarity, heterogeneity, evenness, taxonomic/phylogenetic and functional diversity indices that better reflect the differences among assemblages.}
\usage{
DER(data, Samples, Species, Taxon, TaxonFunc=NULL, TaxonPhyl=NULL,
pos=NULL, varSize="Richness", varColor="Rarity.G", Index=NULL, corr="sqrt",
palette= "heat.colors", size=c(1,5),digitsS=1, digitsC=2, ncolor=100,
transparency=1, references=TRUE, a=1.5, q=2.5, ResetPAR=TRUE, PAR=NULL,
dbFD=NULL, LEGENDS=NULL, TEXT=NULL, COLOR=c("#EEC591FF", "black", "grey50"),
file1="Diversity indices.csv", file2="Polar coordinates.csv",
file3="Indices and area of the polygon.csv", na="NA", dec=",", row.names=FALSE, save=TRUE)
}
\arguments{
  \item{data}{Data file with the taxonomy, abundance of the species and functional traits (optional). The format of the file must be: an optional column with the position of labels' samples in the DER plot (blue column) in the same order as the variables with the species' abundance in the samples (red columns), the columns with the taxonomy of the species (as many as needed, green columns), the columns with the abundance of the species in each sample (red columns) and optionally the columns with the functional traits of the species. Each row is an unique species, genus, family, etc.
\tabular{r}{
\if{html}{\figure{EcoIndRF1.jpg}{options: width="50"}}
\if{latex}{\figure{EcoIndRF1.jpg}{options: width=4.5in}}
}
}
  \item{Samples}{Variables with the abundance of the species in each sample: sampling sites, dates, etc.}
  \item{Species}{Variable with the name of the species (without including the genus). It may be other node of the phylogenetic tree, such as the genus, family, etc., for genus level phylogenies, family level phylogenies, etc.}
  \item{Taxon}{Variables with the taxonomy of the species (taxonomic diversity), as many levels as needed but without including the variable with the node of the argument \emph{Species}.}
  \item{TaxonFunc}{Optionally variables with the functional traits (functional diversity).}
  \item{TaxonPhyl}{Optionally the name of the RData file of the class phylo with the phylogeny. The file must be in the working directory.}
  \item{pos}{Optionally it is possible to indicate a column with the position of labels' samples in the DER plot. It must be as many as the number of samples and in the same order than the variables described in the argument \emph{Samples}. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates.}
 \item{varSize}{This variable defines the size of the bubble in the DER plot.}
  \item{varColor}{This variable defines the color gradient of the bubbles in the DER plot.}
  \item{Index}{The four/five indices used in the DER algorithm. If it is NULL the algorithm select one, index of rarity, one of heterogeneity, one of evenness one of taxonomy and one of the functional group (if functional traits are provided in the argument \emph{TaxonFunc}) that achieve a higher dispersion among samples in a polar coordinates system.}
  \item{corr}{Character string specifying the correction method to use, in the function \link[FD]{dbFD}, when the species-by-species distance matrix cannot be represented in a Euclidean space. Options are "sqrt" (default), "cailliez", "lingoes" or "none".}
  \item{palette}{The color gradient of the bubbles may be one of these palettes: "heat.colors", "terrain.colors", "gray.colors", "topo.colors" or "cm.colors", or any other option defined by the user.}
  \item{size}{Range of size of the bubbles. Two values: minimum and maximum size.}
  \item{digitsS}{Number of digits of the bubble size legend.}
  \item{digitsC}{Number of digits of the color legend.}
  \item{ncolor}{Gradient color of the color legend.}
  \item{transparency}{Transparency of the color gradient, from 0 to 1.}
  \item{references}{If it is TRUE the reference points are depicted on the DER plot.}
  \item{a}{Scale of Renyi diversity.}
  \item{q}{Scale of Tsallis diversity.}
  \item{ResetPAR}{If it is FALSE, the default condition of the function PAR of the package StatR is not placed and maintained those defined by the user in previous graphics.}
  \item{PAR}{It accesses the function PAR of the package StatR that allows to modify many different aspects of the graph.}
  \item{dbFD}{It accesses the function \link[FD]{dbFD} which allows to specify the arguments that calculates the functional diversity indices.}
  \item{LEGENDS}{It allows to modify the legend of the bubble size.}
  \item{TEXT}{It allows to modify the text of the labels in the bubbles.}
  \item{COLOR}{A vector with three values: color of the ellipse, color of the points in the legend of the size of the bubbles and color of the references points in the ellipse, respectively.}
  \item{file1}{CSV FILES. Filename with values of total abundance, richness and the rarity, heterogeneity, evenness, taxonomic, phylogenetic and functional diversity indices of each sample.}
  \item{file2}{CSV FILES. Filename with the polar coordinates of all samples considering the four/five selected indices.}
  \item{file3}{CSV FILES. Filename with the area of the convex hull (alpha=6) and Euclidean distance obtained in the polar coordinates system for all combinations of the indices.}
  \item{na}{CSV FILE. Text that is used in the cells without data.}
  \item{dec}{CSV FILE. It defines if the comma "," is used as decimal separator or the dot ".".}
  \item{row.names}{CSV FILE. Logical value that defines if identifiers are put in rows or a vector with a text for each of the rows.}
  \item{save}{If it is TRUE, the CSV files are saved.}
}
\value{It is depicted a plot of polar coordinates estimated with the rarity, heterogeneity, evenness, taxonomic/phylogenetic and functional diversity indices, CSV files are saved with all the indices, the polar coordinates estimated with the indices specified in the argument \emph{Index} or estimated by the algorithm, and the area of the convex hull and mean Euclidean distance obtained in the polar coordinates system for all combinations of the indices.}
\details{
\strong{DER algorithm}

The steps of DER algorithm are described below:

\strong{1.} The function DER calculates the most often used indices (see below): a total of 39 indices that includes 2 of rarity, 14 of heterogeneity, 7 of evenness, 2 of taxonomic diversity, 8 of phylogenetic diversity and 6 of functional diversity. It is important to mention that the indices included in the groups of phylogenetic diversity and functional diversity, each explores a different facet of phylogenetic diversity (Kembel et al., 2010) and functional diversity (Laliberte et al., 2010), respectively.

\emph{Rarity indices}

In the following equations \emph{S} is the number of species (species richness) in the sample, \emph{s} is the number of samples, \eqn{p_{ij}} is the presence or absence (0 or 1) of the species \emph{i} in the sample \emph{j}, \eqn{r_{ij}} is the number of records or abundance of the species \emph{i} in the sample \emph{j}, \eqn{R_{i}} is the total number of records or total abundance of the species \emph{i} considering all samples, \eqn{Q_i} is the occurrence of species \emph{i}, \eqn{Q_{min}} and \eqn{Q_{max}} are respectively the minimum and maximum occurrences in the species pool, \emph{r} is the chosen rarity cutoff point (as a percentage of occurrence), \eqn{w_i} is the weight of the \emph{ith} species in the assemblage, \eqn{w_{min}} and \eqn{w_{max}} the minimum and maximum weights respectively.

\emph{Leroy} (Leroy et al., 2012; 2013)
\deqn{w_i=\frac{1}{e^{(\frac{Q_i-Q_{min}}{rQ_{max}Q_{min}}*0.97+1.05)^2}}}
\deqn{I_{RR}=\frac{\frac{\displaystyle\sum_{i=1}^{S}w_i}{S}-w_{min}}{w_{max}-w_{min}}}
\emph{Geographical rarity} This index is a novel contribution of this package.
\deqn{GR=1-\frac{\displaystyle\sum_{i=1}^{S}\frac{\displaystyle\sum_{j=1}^{s}p_{ij}}{s}}{S}}
\emph{Occurrential rarity} This index is a novel contribution of this package.
\deqn{OR=1-\frac{\displaystyle\sum_{i=1}^{S}\frac{\displaystyle\sum_{j=1}^{s}r_{ij}}{R_{i}}}{S}}

\emph{Heterogeneity indices}

In the following equations \emph{S} is the number of species (species richness) in the sample, \eqn{p_i} is the abundace's proportion of species \emph{i}, \emph{N} is the total number of individuals in the sample, \eqn{n_i} is the number of individuals of the species \emph{i}, \eqn{n_{max}} is the number of individuals of the most abundant species, \emph{a} and \emph{q} are the orders of Renyi and Tsallis indices respectively and, finally, in Fisher's alpha the index is the \eqn{\alpha} parameter.

\emph{log Shannon-Wiener (S.W.LOG2)} and \emph{ln Shannon-Wiener (S.W)} (Wiener, 1939; 1948; 1949; Shannon, 1948; Shannon & Weaver, 1949). See Spellerger & Fedor (2013) for an explanation of the dual use of the terms Shannon-Wiener and Shannon-Weaver to refer to this diversity index.
\deqn{H=-\displaystyle\sum_{i=1}^{S}p_ilog_2p_i}
\deqn{H'=-\displaystyle\sum_{i=1}^{S}p_ilnp_i}

\emph{Fisher's alpha} (Fisher et al., 1943)
\deqn{{\alpha}x,\frac{{\alpha}x^2}{2},\frac{{\alpha}x3}{3},.....,\frac{{\alpha}x^n}{n}}

\emph{Simpson} (Simpson, 1949) 
\deqn{D_1=1-\displaystyle\sum_{i=1}^{S}p_i^2}

\emph{Inverse Simpson (InvSimpson)} Williams (1964)
\deqn{D_2=\frac{1}{\displaystyle\sum_{i=1}^{S}p_i^2}}

\emph{Brillouin} (Brillouin, 1956)
\deqn{H_B=\frac{lnN!-\displaystyle\sum_{i=1}^{S}lnn_i!}{N}}

\emph{Margalef} (Margalef, 1959)
\deqn{D_{Mg}=\frac{(S-1)}{lnN}}

\emph{Renyi entropy} (Renyi, 1961) 
\deqn{H_a=\frac{1}{(1-a)}log\displaystyle\sum_{i=1}^{S}p_i^a} 

\emph{Menhinick} (Menhinick, 1964)
\deqn{D_{Mn}=\frac{S}{\sqrt{N}}}

\emph{McIntosh} (McIntosh, 1967)
\deqn{D_{Mc}=\frac{N-\sqrt{\displaystyle\sum_{i=1}^{S}n_i^2}}{N-\sqrt{N}}}

\emph{Inverse Berger-Parker (InvB.P)} (Berger & Parker, 1970)
\deqn{D_{BP}=\frac{1}{\frac{n_{max}}{N}}}

\emph{Hill numbers} (Hill, 1973)

Hill-Renyi
\deqn{N_a=e^{H_a}}

Hill-Tsallis
\deqn{N_q=(1-(q-1)H_q)^\frac{1}{(1-q)}}

where

a or q = 0 is species richness

a or q = 1 is Shannon's index (H')

a or q = 2 is Inverse Simpson's index (\eqn{D_2})

a or q = Inf is Inverse Berger-Parker index (\eqn{D_{BP}})

\emph{Tsallis entropy} (Patil & Taillie, 1982; Tsallis, 1988)
\deqn{H_q=\frac{1}{(q-1)}(1-\displaystyle\sum_{i=1}^{S}p_i^q)}

\emph{Evenness indices}

Annotations of the equations as mentioned for heterogeneity indices.

\emph{Simpson evenness (SimpsonE)} (Simpson, 1949) 
\deqn{E_{D1}=\frac{D_1}{S}}

\emph{Pielou (PielouE)} (Pielou, 1966)
\deqn{J'=\frac{H'}{lnS}}

\emph{McIntosh evenness (McIntoshE)} (McIntosh, 1967)
\deqn{E_{Mc}=\frac{N-\sqrt{\displaystyle\sum_{i=1}^{S}n_i^2}}{N-\frac{N}{\sqrt{S}}}}

\emph{Hill evenness (HillE)} (Hill, 1973)

It is used Hill-Renyi numbers where in \eqn{N_2} the value of a = 2 and in \eqn{N_2} the value of a = 1
\deqn{E_{2,1}=\frac{N_2}{N_1}}

\emph{Heip evenness (HeipE)} (Heip, 1974)
\deqn{E_{Heip}=\frac{e^{H'}-1}{S-1}}

\emph{Camargo (CamargoE)} (Camargo, 1992)
\deqn{E_{Camargo}=1-\displaystyle\sum_{i=1}^{S}\displaystyle\sum_{j=i'+1}^{S}\frac{|p_i-p_j|}{s}}

\emph{Smith and Wilson's Index (Evar)} (Smith and Wilson, 1996)
\deqn{E_{var}=1-\left(\frac{2}{\pi}\right)\left(arctan\left(\frac{\displaystyle\sum_{i=1}^{S}{\left(lnn_i-\frac{\displaystyle\sum_{j=1}^{S}n_j}{S}\right)}^2}{S}\right)\right)}

\emph{Taxonomic diversity indices}

In the following equations summation goes over species \emph{i} and \emph{j}, \eqn{\omega} are the taxonomic distances among taxa, \emph{x} are species abundances, and \emph{n} is the total abundance for a site.

\emph{Taxonomic diversity (D)} (Clarke, 1995; 1998; 2001):
\deqn{\Delta=\frac{\sum\sum_{i<j}\omega_{ij}x_ix_j}{n(n-1)/2}}

\emph{Taxonomic distinctness (Dstar)} (Clarke and Warwick, 1998):
\deqn{\Delta^*=\frac{\sum\sum_{i<j}\omega_{ij}x_ix_j}{\sum\sum_{i<j}x_ix_j}}

\emph{Phylogenetic diversity indices}

\tabular{llll}{
Faith's phylogenetic diversity \tab  \tab   \tab Faith (1992)\cr
Mean pairwise phylogenetic distance \tab  \tab   \tab Webb et al. (2008)\cr
Mean nearest taxon distance \tab  \tab   \tab Webb et al. (2008)\cr
Phylogenetic species richness \tab  \tab   \tab Helmus et al. (2007)\cr
Phylogenetic species variability \tab  \tab   \tab Helmus et al. (2007)\cr
Phylogenetic species evenness \tab  \tab   \tab Helmus et al. (2007)\cr
Phylogenetic species clustering \tab  \tab   \tab Helmus et al. (2007)\cr
Quadratic entropy \tab  \tab   \tab Rao (1982)
}

\emph{Functional diversity indices}

In the following equations \emph{S} is the number of species, where \eqn{d_{ij}} is the difference between the \emph{i-th} and \emph{j-th} species, \eqn{p_i} is the abundance's proportion of the species \emph{i}, \eqn{p_j} is the abundance's proportion of the species \eqn{j}, \emph{EW} is weighted evenness, dist(\emph{i}, \emph{j}) is the Euclidean distance between species \emph{i} and \emph{j}, the species involved is branch \emph{l}, \eqn{w_i} is the relative abundance of the species \emph{i}, \eqn{PEW_l} is the partial weighted evenness, \eqn{g_k} are the coordinates of the center of gravity of the \emph{V} species forming the vertices of the convex hull, \eqn{x_{ik}} is the coordinate of species \emph{i} on trait \emph{k}, \eqn{dG_i} is the Euclidean distance of the center of gravity, \eqn{\bar{dG}} is the mean distance of the \emph{S} species to the center of gravity, \eqn{\Delta}\emph{d} is the sum of abundance-weighted deviances and \eqn{\Delta}\emph{|d|} absolute abundance-weighted deviances.

\emph{Rao's quadratic entropy} (Rao, 1982; Botta-Dukat, 2005):
\deqn{Q=\displaystyle\sum_{i=1}^{S-1}\displaystyle\sum_{j=i+1}^{S-1}d_{ij}p_ip_j}
\deqn{d_{ij}=\displaystyle\sum_{k=1}^{n}\displaystyle\sum_{l=1}^{n}w_{kl}(X_{ik}-X_{jk})(X_{il}-X_{jl})}

\emph{Functional group richness (FGR)} Petchey and Gaston (2006)

\emph{Functional richness (FRic)} is measured as the amount of functional space (convex hull volume) filled by the community (Villeger et al., 2008).

\emph{Functional evenness (FEve)} (Villeger et al., 2008):
\deqn{FEve=\frac{\displaystyle\sum_{l=1}^{S-1}min(PEW_l,\frac{1}{S-1})-\frac{1}{S-1}}{1-\frac{1}{S-1}}}
\tabular{ll}{
\eqn{PEW_l=\frac{EW_l}{\displaystyle\sum_{l=1}^{S}EW_l}} \tab \eqn{EW_l=\frac{dis(i,j)}{w_i+w_j}}\cr
}
\emph{Functional divergence (FDiv)} (Villeger et al., 2008):
\deqn{FDiv=\frac{\Delta{d}+\bar{dG}}{\Delta{|d|}+\bar{dG}}}
\tabular{ll}{
\eqn{\Delta{|d|}=\displaystyle\sum_{i=1}^{S}w_i|dG_i-\bar{dG}|} \tab \eqn{\Delta{d}=\displaystyle\sum_{i=1}^{S}w_i(dG_i-\bar{dG})}\cr
}
\tabular{lll}{
\eqn{\bar{dG}=\frac{1}{S}\displaystyle\sum_{i=1}^{S}dG_i} \tab \eqn{dG_i=\sqrt{\displaystyle\sum_{k=1}^{T}(x_{ik}-g_k)^2}} \tab \eqn{g_k=\frac{1}{V}\displaystyle\sum_{i=1}^{V}x_{ik}}\cr
}
\emph{Functional dispersion (FDis)} (Laliberte and Legendre, 2010):
\tabular{ll}{
\eqn{FDisp=\frac{\sum{a_jz_j}}{\sum{a_j}}} \tab \eqn{c=\frac{\sum{a_jx_{ij}}}{\sum{a_j}}}\cr
}

\strong{2.} Each index is transformed to a scale range between 0 and 1 for all samples with the following equation:
\deqn{\frac{(index of the sample-min)}{(max-min)}}

where \emph{min} and \emph{max} are the minimum and maximum values of the index considering all samples, respectively.

\strong{3.} With the standardized values of the indices, the algorithm calculates the polar coordinates of all samples with all possible combinations among all groups of indices. Therefore, in each combination an index of each group of rarity, heterogeneity (species richness is included in this group), evenness, taxonomic/ phylogenetic diversity and functional diversity (if it is included functional traits in the analysis) is used for calculating the polar coordinates of all samples. In the group of taxonomic/phylogenetic diversity the user must use either taxonomy or a phylogenetic tree, so either taxonomic diversity or phylogenetic diversity indices are used in the algorithm. The X and Y polar coordinates for each sample are estimated using the following equations:

\tabular{ll}{
\eqn{X=\displaystyle\sum_{i=1}^{4}|z_j|cos(\alpha)} \tab \eqn{Y=\displaystyle\sum_{i=1}^{4}|z_j|sin(\alpha)}
}
where \emph{z} is the standardized value of the index \emph{j} of the four groups considered.

Each index is assigned an angle (\eqn{\alpha}). Degrees to radians angle conversion is carried out assuming that 1 degree = 0.0174532925 radians.

\strong{4.} With the polar coordinates of the samples obtained for each combination, it is calculated the convex hull (alpha = 6) and the mean Euclidean distance, and the values are saved in a file.

\strong{5.} The algorithm selects the combination of indices with the highest value of the mean between convex hull and mean Euclidean distance among samples, therefore priority is given to maximize dispersion among samples (see Fig. 1). The polar coordinates of the selected combination are depicted on a diagram, where it is possible to see the differences in rarity, heterogeneity, evenness and taxonomic/phylogenetic  diversity and/or functional diversity (if it is included) among assemblages.

\strong{6.} Finally, DER function allows the user to select the four/five indices to be used in the diagram, so the algorithm of selecting the combination with the maximum dispersion among samples is not applied.

\strong{FUNCTIONS}

The index Fisher alpha was estimated with the function \link[vegan]{fisher.alpha}, the index Renyi with the function \link[vegan]{renyi}, the index Tsallis with the function \link[vegan]{tsallis}, the taxonomic diversity and taxonomic distinctness with the functions \link[vegan]{taxa2dist} and \link[vegan]{taxondive}, all of them of the package vegan (Oksanen et al., 2016). The ellipse is depicted with the function \link[shape]{plotellipse} of the package shape (Soetaert, 2016). The convex hull (alpha=6) was calculated with the function \link[splancs]{areapl} of the package splancs (Bivand et al., 2016). The color legend of DER plot was depicted with the function \link[plotrix]{color.legend} of the package plotrix (Lemon et al., 2016). The rarity index of Leroy was calculated with the functions \link[Rarity]{rWeights} and \link[Rarity]{Irr}, both of the package Rarity (Leroy et al., 2012; 2103; Leroy, 2016). The functional diversity indices were calculated with the function \link[FD]{dbFD} of the package FD (Laliberte et al., 2015). The phylogenetic indices were calculated with the functions psv, psr, pse, psc, raoD, mntd, mpd and pd of the package picante (Kembel et al. 2010 2016)

\strong{EXAMPLE}

The example without functional diversity is a dataset with the abundance of rotifers species in ponds (see table 1 in Mazuelos et al., 1993). In the argument \emph{Index} were selected Rarity, Menhinick, McIntoshE and Dstar, which are the indices selected by the algorithm when \emph{Index=NULL} (default option). The sample G3.1 had the lowest values of the indices of rarity, heterogeneity, evenness and taxonomic diversity and the pond I3.1 the highest values for all indices.
\tabular{c}{
\if{html}{\figure{EcoIndRF2.jpg}{options: width="50"}}
\if{latex}{\figure{EcoIndRF2.jpg}{options: width=5in}}
}
}
\references{
Berger, W.H., Parker, F.L. (1970) Diversity of planktonic Foramenifera in deep sea sediments. \emph{Science}, 168: 1345-1347.

Bivand, R., Rowlingson, B., Diggle, P., Petris, G., Eglen, S. (2016) Spatial and Space-Time Point Pattern Analysis. R Package Version 2.01-39. \url{https://CRAN.R-project.org/package=splancs}

Botta-Dukat, Z. (2005) Rao's quadratic entropy as a measure of functional diversity based on multiple traits. \emph{Journal of Vegetation Science}, 16: 533-540.

Brillouin, L. (1956) \emph{Science and information theory}. New York: Academic Press.

Camargo, J.A. (1992) New diversity index for assessing structural alterations in aquatic communities. \emph{Bulletin of Environmental Contamination and Toxicology}, 48, 428-434.

Clarke, K.R. & Warwick, R.M. (1998). A taxonomic distinctness index and its statistical properties. \emph{Journal of Applied Ecology}, 35: 523-531.

Faith, D.P. (1992) Conservation evaluation and phylogenetic diversity. \emph{Biological Conservation}, 61: 1-10.

Fisher, R.A., Corbet, A.S. & Williams, C.B. (1943) The relation between the number of species and the number of individuals in a random sample of animal population. \emph{Journal of Animal Ecology}, 12: 42-58.

Heip, C. 1974. A new index measuring evenness. \emph{Journal of the Marine Biological Association of the United Kingdom}, 54: 555-557.

Helmus, M.R., Bland, T.J., Williams, C.J., Ives, A.R. (2007) Phylogenetic measures of biodiversity. \emph{The American Naturalist}, 169: E68-E83.

Hill, M.O. (1973) Diversity and evenness: a unifying notation and its consequences. \emph{Ecology}, 54: 427-432.

Hurlbert, S.H. 1971. The nonconcept of species diversity: a critique and alternative parameters. \emph{Ecology}, 52: 577-586.

Kembel, S.W., Cowan, P.D., Helmus, M.R., Cornwell, W.K., Morlon, H., Ackerly, D.D., Blomberg, S.P.,Webb, C.O. (2010) Picante: R tools for integrating phylogenies and ecology. \emph{Bioinformatics}, 26: 1463-1464.

Kembel, S.W., Ackerly, D.D., Blomberg, S.P., Cornwell, W.K., Cowan, P.D., Helmus, M.R., Morlon, H. & Webb, C.O. (2016) R tools for integrating phylogenies and ecology. R Package Version 1.6-2. \url{https://CRAN.R-project.org/package=picante}

Laliberte, E. & Legendre, P. (2010) A distance-based framework for measuring functional diversity from multiple traits. \emph{Ecology}, 91: 299-305.

Laliberte, E., Legendre, P. & Shipley, B. (2015) Measuring functional diversity (FD) from multiple traits, and other tools for functional ecology. R package version 1.0-12. Available at: \url{https://CRAN.R-project.org/package=FD}.

Lemon, J., Bolker, B., Oom, S., Klein, E., Rowlingson, B.,Wickham, H., Tyagi, A., Eterradossi, O., Grothendieck, G., Toews, M., Kane, J., Turner, R., Witthoft, C., Stander, J., Petzoldt, T., Duursma, R., Biancotto, E., Levy, O., Dutang, C., Solymos, P., Engelmann, R., Hecker, M., Steinbeck, F., Borchers, H., Singmann, H., Toal, T. & Ogle, D. (2016) Various plotting functions. R package version 3.6-3. Available at: \url{https://CRAN.R-project.org/package=plotrix}.

Leroy, B., Petillon, J., Gallon, R., Canard, A. & Ysnel, F. (2012) Improving occurrence-based rarity metrics in conservation studies by including multiple rarity cut-off points. \emph{Insect Conservation and Diversity}, 5, 159-168.

Leroy, B., Canard, A. & Ysnel, F. (2013) Integrating multiple scales in rarity assessments of invertebrate taxa. \emph{Diversity and Distributions}, 19, 794-803.

Leroy, B. (2016) Calculation of Rarity Indices for Species and Assemblages of Species. R package version 1.3-4. Available at: \url{https://CRAN.R-project.org/package=Rarity}.

Oksanen, J., Blanchet, F.G., Kindt, R., Legendre, P., Minchin, P.R., O'Hara, R.B., Simpson, G.L., Solymos, P., Henry, M., Stevens, H. & Wagner, H. (2016) Community Ecology Package. R package version 2.4-0. Available at: \url{https://CRAN.R-project.org/package=vegan}.

Patil, G.P. & Taillie, C. (1982) Diversity as a concept and its measurement. \emph{Journal of the Acoustical Society of America}, 77: 548-561.

Pielou, E.C. (1966) The measurement of diversity in different types of biological collections. \emph{Journal Theoretical Biology}, 13: 131-144.

Petchey, O.L. & Gaston, K.J. (2002) Functional diversity (FD), species richness and community composition. \emph{Ecology Letters}, 5: 402-411.

Margalef, R. (1958) Information theory in ecology. \emph{General Systems}, 3: 36-71.

Mazuelos, N., Toja, J. & Guisande, C. (1993) Rotifers in ephemeral ponds of Doñana National Park. \emph{Hydrobiologia}, 255/256: 429-434.

McIntosh, R.P. (1967) An index of diversity and the relation of certain concepts to diversity. \emph{Ecology}, 48: 392-404.

Menhinick, E.P. (1964) A comparison of some species-individual diversity indices applies to samples of field insects. \emph{Ecology}, 45: 859-861.

Renyi, A. (1961) On measures of information and entropy. \emph{Proceedings of the fourth Berkeley Symposium on Mathematics, Statistics and Probability Vol. 1}: 547-561.

Rao, C.R. (1982) Diversity and dissimilarity coefficients: a unified approach. \emph{Theoretical Population Biology}, 21: 24-43.

Shannon, C.E. (1948) A Mathematical Theory of Communication. \emph{Bell System Technical Journal}, 27 (4): 379-423, 623-656.

Shannon, C.E. & Weaver, W. (1949) \emph{The Mathematical Theory of Communication}. The University of Illinois Press, Illinois. 

Simpson, E.H. (1949) Measurement of diversity. \emph{Nature}, 163: 688.

Smith, B. & J.B. Wilson (1996) A consumer's guide to evenness indices. \emph{Oikos}, 76: 70-82.

Soetaert, K. 2016. Functions for plotting graphical shapes, colors. R package version 1.4.2. Available at: \url{https://CRAN.R-project.org/package=shape}.

Spellerger, I.F. & Fedor P.J. (2013) A tribute to Claude Shannon (1916-2001) and a plea for more rigorous use of species richness, species diversity and the "Shannon-Wiener" Index. \emph{Global Ecology and Biogeography}, 12: 177-179.

Tsallis, C. (1988) Possible generalization of Boltzmann-Gibbs statistics. \emph{Journal of Statistical Physics}, 52: 479-487.

Villeger, S., Mason, N.W.H. & Mouillot, D. (2008) New multidimensional functional diversity indices for a multifaceted framework in functional ecology. \emph{Ecology} 89: 2290-2301.

Webb, C.O., Ackerly, D.D. & Kembel, W. (2008) Phylocom: software for the analysis of phylogenetic community structure and trait evolution. \emph{Bioinformatics}, 24: 2098-2100.

Wiener, N. (1939) The ergodic theorem. \emph{Duke Mathematical Journal}, 5: 1-18.

Wiener, N. (1948) \emph{Cybernetics}. Wiley, New York.

Wiener, N. (1949) \emph{The interpolation, extrapolation, and smoothing of stationary time series}. Wiley, New York.
}
\examples{

#An example without functional diversity

data(Rotifers)

DER(data=Rotifers, Samples=c("J1.1","K4.1","G3.1","F2.1","K2.2","F8.2","F8.1",
"F1.1","F4.1","J2.1","E5.1","H5.1","K3.2","E4.2","I6.1","K2.1","J5.1","I3.1",
"K3.3","G5.1","E6.1","J1.2","J6.1","G7.1","G6.1","G4.1","E3.1","E4.3","E2.1",
"H6.2","F7.1","J6.2"), Species="Species", Taxon=c("Class","Subclass",
"Superorder","Order","Family","Genus"), pos="Pos", Index=c("Rarity.G","Menhinick",
"McIntoshE", "Dstar"), save=FALSE)



}
\keyword{DER}
