\name{HEMDAG-package}
\alias{HEMDAG-package}
\alias{HEMDAG}
\docType{package}
\title{HEMDAG: Hierarchical Ensemble Methods for Directed Acyclic Graphs}

\description{
The HEMDAG package:
\itemize{
\item provides an implementation of several Hierarchical Ensemble Methods (HEMs) for Directed Acyclic Graphs (DAGs);
\item reconciles flat predictions with the topology of the ontology;
\item can enhance predictions of virtually any flat learning methods by taking into account the hierarchical relationships between ontology classes;
\item provides biologically meaningful predictions that obey the true-path-rule, the biological and logical rule that governs the internal coherence of biomedical ontologies;
\item is specifically designed for exploiting the hierarchical relationships of DAG-structured taxonomies, such as the Human Phenotype Ontology (HPO) or the Gene Ontology (GO), but can be safely applied to tree-structured taxonomies as well (as FunCat), since trees are DAGs;
\item scales nicely both in terms of the complexity of the taxonomy and in the cardinality of the examples;
\item provides several utility functions to process and analyze graphs;
\item provides several performance metrics to evaluate HEMs algorithms;
}

A comprehensive tutorial showing how to apply HEMDAG to real case bio-medical case studies is available at \href{https://hemdag.readthedocs.io}{https://hemdag.readthedocs.io}.

}

\details{
The HEMDAG package implements the following Hierarchical Ensemble Methods for DAGs:
\enumerate{
\item \bold{HTD-DAG}: Hierarchical Top Down (\code{\link{htd}});
\item \bold{GPAV-DAG}: Generalized Pool-Adjacent Violators, \cite{Burdakov et al.} (\code{\link{gpav}});
\item \bold{TPR-DAG}: True-Path Rule (\code{\link{tpr.dag}});
\item \bold{DESCENS}: Descendants Ensemble Classifier (\code{\link{tpr.dag}});
\item \bold{ISO-TPR}: Isotonic-True-Path Rule (\code{\link{tpr.dag}});
\item \bold{Max}, \bold{And}, \bold{Or}: Heuristic Methods, \cite{Obozinski et al.} (\code{\link{obozinski.heuristic.methods}});
}}

\author{
\emph{Marco Notaro}\eqn{^1} (\href{https://orcid.org/0000-0003-4309-2200}{https://orcid.org/0000-0003-4309-2200});\cr
\emph{Alessandro Petrini}\eqn{^1} (\href{https://orcid.org/0000-0002-0587-1484}{https://orcid.org/0000-0002-0587-1484});\cr
\emph{Giorgio Valentini}\eqn{^1} (\href{https://orcid.org/0000-0002-5694-3919}{https://orcid.org/0000-0002-5694-3919});\cr

Maintainer:
\emph{Marco Notaro}
\email{marco.notaro@unimi.it}\cr

\eqn{^1} \href{https://sites.google.com/site/anacletolaboratory/}{AnacletoLab}, Computational Biology and Bioinformatics Laboratory, Computer Science Department, University of Milan, Italy}

\references{
Marco Notaro, Max Schubach, Peter N. Robinson and Giorgio Valentini,
\emph{Prediction of Human Phenotype Ontology terms by means of Hierarchical Ensemble methods},
BMC Bioinformatics 2017, 18(1):449, \doi{10.1186/s12859-017-1854-y}
}

\keyword{package}
