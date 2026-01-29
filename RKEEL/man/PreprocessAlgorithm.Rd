\docType{class}
\name{PreprocessAlgorithm}
\alias{PreprocessAlgorithm}
\title{Preprocess Algorithm}
\description{
Class inheriting of KeelAlgorithm, to common methods for all KEEL Preprocess Algorithms.
The specific preprocessing algorithms must inherit of this class.

The run() method receives three parameters.
The folderPath parameter indicates where to place the folder with the experiments if wanted. If it is not indicated, the folder is placen ind a temporary random directory and then removed. If indicated, the experiment folder is not removed.
The expUniqueName parameter indicates the name of the experiment folder. If not indicated, it is a random name. If indicated, ensure that the name is unique in the previously indicated folder.
The javaOptions parameter indicates, if wanted, extra parameters to the java command line, as for example the maximum memory allowed by java.
}
\keyword{preprocess}

