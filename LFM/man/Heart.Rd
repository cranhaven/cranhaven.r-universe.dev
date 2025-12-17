\name{Heart}
\alias{Heart}
\title{Heart}
\usage{data(Heart)}
\description{
This dataset contains information about heart disease diagnosis, including various clinical attributes and the presence of heart disease in patients. The dataset is commonly used for classification tasks to predict the presence of heart disease.
}
\format{
A data frame with multiple rows and 14 columns representing different features related to heart disease diagnosis.
\itemize{
\item \code{age}: Age in years (integer).
\item \code{sex}: Sex (1 = male; 0 = female) (categorical).
\item \code{cp}: Chest pain type (categorical).
\item \code{trestbps}: Resting blood pressure (in mm Hg on admission to the hospital) (integer).
\item \code{chol}: Serum cholesterol in mg/dl (integer).
\item \code{fbs}: Fasting blood sugar > 120 mg/dl (1 = true; 0 = false) (categorical).
\item \code{restecg}: Resting electrocardiographic results (categorical).
\item \code{thalach}: Maximum heart rate achieved (integer).
\item \code{exang}: Exercise-induced angina (1 = yes; 0 = no) (categorical).
\item \code{oldpeak}: ST depression induced by exercise relative to rest (integer).
\item \code{slope}: The slope of the peak exercise ST segment (categorical).
\item \code{ca}: Number of major vessels (0-3) colored by fluoroscopy (integer).
\item \code{thal}: Thalassemia (3 = normal; 6 = fixed defect; 7 = reversible defect) (categorical).
\item \code{num}: Diagnosis of heart disease (angiographic disease status) (integer).
}
}
\examples{
# Load the dataset
data(Heart)

# Print the first few rows of the dataset
print(head(Heart))
}
\keyword{datasets}
