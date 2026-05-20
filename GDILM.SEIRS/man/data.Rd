\docType{data}
\name{Datasets}
\alias{adjacency_matrix}
\alias{data}
\title{Hypothetical Datasets}
\description{
The primary function, \code{GDILM_SEIRS_Par_Est}, fits the Geographically Dependent Individual Level Model (GDILM) for infectious disease transmission incorporating reinfection dynamics within the SEIRS framework, using real-world data. It can be applied to any dataset with the appropriate structure, requiring two dataframes: \code{data} and \code{adjacency_matrix}, along with the necessary parameters. For illustration purposes, we provide two hypothetical examples of \code{data} and \code{adjacency_matrix} to demonstrate the structure of the inputs. These examples will also be used to illustrate how the function works in practice.
}
\section{\code{data}}{
A data frame with 100 rows and 12 columns.

This hypothetical dataset demonstrates the structure required for the dataframe used in this package. The dataset for use with the package should adhere to the same column format and order but can include any number of rows, with each row representing an infected individual. The example dataset includes individual-level characteristics (e.g., age, infection status) and area-level characteristics (e.g., socioeconomic status, STI rate) for 100 individuals, each associated with a postal code. This dataset will be used as input in the example for the \code{GDILM_SEIRS_Par_Est} function.
\describe{
\item{Ave_Postal_Pop}{Average population of each postal code}
\item{AverageAge}{Average age of individuals within each postal code (individual-level data)}
\item{InfectedTime}{Time of infection for each individual, represented as a numerical value from 1 to the end of the pandemic period}
\item{LAT}{Latitude of the postal code}
\item{LONG}{Longitude of the postal code}
\item{Label_NC_shape}{The region number that the postal code belongs to, here assuming the study area is divided into five subregions}
\item{MaleRate}{Rate of males in the population of the postal code (individual-level data)}
\item{NInfected}{Number of infected individuals in the postal code}
\item{SES}{Socioeconomic status indicator of the region to which the postal code belongs (area-level data)}
\item{STI}{Sexually transmitted infection rate of the region that the postal code belongs to (area-level data)}
\item{SymptomRate}{Rate of disease symptoms in the postal code (individual-level data), indicating whether individuals are symptomatic or asymptomatic}
\item{status}{1 if the postal code is infected for the first time, and 0 if the postal code is reinfected}
}
}



\section{\code{adjacency_matrix}}{
A 5x5 matrix.

This hypothetical adjacency matrix is provided to illustrate the structure required for use with this package. The matrix used with the package should follow a similar format, maintaining the same layout but allowing for any number of regions. The adjacency matrix  defines the neighborhood relationships between subregions in a hypothetical study area. In this example, it represents a spatial structure with five subregions, where each cell indicates the presence or absence of a connection between the corresponding subregions. The example for the \code{GDILM_SEIRS_Par_Est} function will use this matrix as input.
\describe{
\item{V1}{Subregion 1: Represents the first subregion in the region under study}
\item{V2}{Subregion 2: Represents the second subregion in the region under study}
\item{V3}{Subregion 3: Represents the third subregion in the region under study}
\item{V4}{Subregion 4: Represents the fourth subregion in the region under study}
\item{V5}{Subregion 5: Represents the fifth subregion in the region under study}
\item{Value}{Each cell in the matrix (e.g., between subregion 1 and subregion 2) represents the connection (typically 0 or 1) between the two subregions, where 1 indicates they are neighbors and 0 indicates they are not.}
}
}


