\name{keys}
\alias{keys}

\title{Diagnostic keys}

\usage{
keys
}

\format{

The list which contains four data frames representing three different
flavors of biological diagnostic keys: two simple bracket keys, one
branched (indented variant) and one serial key. Last two keys are
real-world keys, first to determine Plantago (ribworts, plantains) from
European Russia (Shipunov, 2000), second -- from North America (Shipunov,
2019).

}

\description{

Diagnostic keys are data structures which help to identify biological
samples, i.e. give them (scientific) names. They are old but still very
popular because they are simple and efficient, sometimes even for not
very experienced user.

The second goal of these keys is the compact representation of biological
diversity. Diagnostic keys are not very far from classification lists
(see 'classifs'), phylogeny trees (like 'phylo' objects in 'ape'
package), from core R 'dendrogram' and 'hclust' objects, and especially
from recursive partitioning objects (e.g., from 'tree' or 'rpart'
packages).

In biology, diagnostic keys exist in many flavors which are possible to
reduce into two main types:

I. Branched keys, where alternatives are separated.

You compare your sample with the first description. Then, if the sample
agrees with first description, you go to second description (these keys
are usually fully dichotomous), then to the third, until you reach the
temninal (name of the organism). If not, you find the alternative
description of the _same level_ (same depth). The main difficulty here is
how to find it.

To help user find descriptions of the same depths, branched keys are
usually presented as _indented_ where each line starts with an indent.
Bigger indent means bigger depth.

Branched or indented keys could be traced at least to 1668, to one of
John Wilkins books:

\if{latex}{\figure{wilkins1668indented.png}{options: width=8cm}}
\if{html}{\figure{wilkins1668indented.png}{options: width="40\%" alt="Figure: wilkins1668indented.png"}}
\if{text}{Figure: wilkins1668indented.png (to see it, check pdf or html)}

(and maybe to much earlier scholastic works.)

Indented keys are widely used, especially in English-language
publications.

Another modification could be traced to 1892 when A.
Semenow-Tjan-Shanskij published his serial key:

\if{latex}{\figure{semenow1892serial.png}{options: width=10cm}}
\if{html}{\figure{semenow1892serial.png}{options: width="50\%" alt="Figure: semenow1892serial.png"}}
\if{text}{Figure: semenow1892serial.png (to see it, check pdf or html)}

Serial keys are similar to all branched keys but numbering style is
different. All steps are numbered sequentially but each has a
back-reference to the alternative so user is not required to find the
description of the same depth, they are already here. Serial keys are
strictly dichotomous. They are probably the most space-saving keys, and
still in use, especially in entomology.

II. Bracket keys, where alternatives are together, and user required to
use 'goto' references to take the next step.

They can be traced to the famous "Flora Francoise" (1778) where J.-B.
Lamarck likely used them the first time:

\if{latex}{\figure{lamarck1778bracket.png}{options: width=10cm}}
\if{html}{\figure{lamarck1778bracket.png}{options: width="50\%" alt="Figure: lamarck1788bracket.png"}}
\if{text}{Figure: lamarck1778bracket.png (to see it, check pdf or html)}

You compare your sample with first description, and if it agrees, go to
where 'goto' reference says. If not, go to second (alternative)
description, and then again use its 'goto'. On the last steps, 'goto' is
just the terminal, the name you want. Sometimes, bracket keys have more
than one alternative (e.g., not fully dichotomous).

Bracket keys pose another difficulty: it is not easy to go back (up) if
you by mistake went into the wrong direction. Williamson (1922) proposed
backreferenced keys where each step supplied with back-reference:

\if{latex}{\figure{williamson1922backreferenced.png}{options: width=10cm}}
\if{html}{\figure{williamson1922backreferenced.png}{options: width="50\%" alt="Figure: williamson1922backreferenced.png"}}
\if{text}{Figure: williamson1922backreferenced.png (to see it, check pdf or html)}

Sometimes, back-references exist only in case where the referenced step
is not immediately before the current.

Bracket keys (backreferenced or not) are probably most popular in
biology, and most international as well.

Here bracket, branched and serial keys are standardized as rectangular
tables (data frames). Each feature (id, backreference, description,
terminal, 'goto') is just one column. In bracket keys, terminal and
'goto' are combined. For example, if you need a bracket key without
backreferences, use three columns: id, description and terminal+'goto'.
Order of columns is important, column name is not. Please see examples to
understand better.

Note that while this format is human-readable, it is not typographic. To
make keys more typographic, user might want to convert them into LaTeX
where several packages allow for typesetting diagnostic keys (for
example, my 'biokey' package.)

}

\source{

Lamarck J.-B., de. 1778. Flore Francoise. Paris.

Semenow-Tjan-Shanskij A.P. 1892. Note sur la subdivision du genre Lethrus
Scop. et description de deux nouvelles. Trudy Russkago Entomologicheskago
Obschestva. 26: 232--244.

Shipunov A. 2000. The genera Plantago L. and Psyllium Mill.
(Plantaginaceae Juss.) in the flora of East Europe. Novosti Systematiki
Vysshikh Rastenij. 32: 139--152. [In Russian]

Shipunov A. 2019. Plantago. In: Freeman, C. and Rabeler R. (eds.) Flora
of North America. 2019. 17: 280--293. Oxford University Press, New York
and Oxford.

Shipunov A. 2019. biokey -- Flexible identification key tables in LaTeX.
Version 3.1. See "https://ctan.org/pkg/biokey".

Sviridov A.V. 1994. Types of the biodiagnostic keys and their uses.
Moscow. [In Russian]

Wilkins J. 1663. An essay towards the real character and philosophical
language. London.

Williamson E. 1922. Keys in systematic work. Science. 55: 703. 

}

\seealso{\code{\link{Biokey}}}

\examples{
attach(keys)

head(bracket1)
head(bracket2)
head(branched)
head(serial)

## convert keys with Biokey()
sii <- Biokey(serial, from="serial", to="indented")
sbb <- Biokey(serial, from="serial", to="bracket")
bbr <- Biokey(branched, from="branched", to="bracket")

## convert keys and visualize them as trees
library(ape) # load 'ape' library to plot Newick trees
plot(read.tree(text=Biokey(bracket1, from="bracket", to="newick")))
plot(read.tree(text=Biokey(bracket2, from="bracket", to="newick")))
plot(read.tree(text=Biokey(branched, from="branched", to="newick")))
plot(read.tree(text=Biokey(serial, from="serial", to="newick")))

detach(keys)

## to make a new bracket key (without backreferences)
## supply three columns: id, description and 'goto'+terminal
bracket3 <- read.table(as.is=TRUE, text="
1 Small Ant
1 Big 2
2 Blue Sky
2 Green Grass
")
bracket3
Biokey(bracket3, from="bracket", to="newick")
cophenetic(ape::read.tree(text=Biokey(bracket3, from="bracket", to="newick")))
}

\keyword{datasets}
