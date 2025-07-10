\name{sdac}
\alias{sdac}
\docType{data}
\title{
  Data on Autologous Peripheral Blood Stem Cell Transplants in Alberta Health Service
}
\description{Autologous peripheral blood stem cell (PBSC) transplants have been widely used for rapid
hematologic recovery following myeloablative therapy for various malignant hematological disorders. A 
study enrolled 242 patients who consented to autologous PBSC transplant after myeloablative doses of 
chemotherapy between year 2003 and 2008 at the Edmonton Hematopoietic Stem Cell Lab in Cross Cancer 
Institute - Alberta Health Services. The Data is a data frame containing information about the patients'
age, gender, as well as their clinical characteristics. 
}
\usage{data("sdac")}
\format{
   	A data frame with 239 observations on the following 5 variables. 
   	\describe{
   	   	\item{\code{age}}{patients' ages}
		\item{\code{gender}}{patients' genders}
		\item{\code{rcd}}{recovery rates for viable CD34+ cells}
        \item{\code{chemo}}{dummy variable indicating if a patient receives a chemotherapy on a one-day 
			protocol(0) or on a 3-day protocol(1)}
		\item{\code{ageadj}}{adjusted age variable. \code{age} < 40 is set as the baseline age and other ages
			are adjusted by subtracting by 40}
   	}
}

\references{
Allan, D. and Keeney, M. and Howson-Jan, K. and Popma, J. and Weir, K. and Bhatia, M. and Sutherland, D. and 
Chin_yee, I. (2002) Number of Viable CD34+ Cells Reinfused Predicts Engraftment in Autologous Hematopoietic 
Stem Cell Transplantation. \emph{BONE MARROW TRANSPL,} \bold{ 20:} 967-72

Yang, H. and Acker, J. and Cabuhat, M. and Letcher, B. and Larratt, L. and McGann, L. (2005) Association of 
Post_Thaw viable CD34+ Cells and CFU-GM with Time to Hematopoietic Engraftment. \emph{BONE MARROW TRANSPL,} 
\bold{ 35:} 881-887

Zhang, P. and Qiu, Z. and Shi, C. (2016) {simplexreg}: An {R} Package for Regression
	Analysis of Proportional Data Using the Simplex Distribution. \emph{ Journal of Statistical Software,}
	\bold{ 71:} 1--21
}