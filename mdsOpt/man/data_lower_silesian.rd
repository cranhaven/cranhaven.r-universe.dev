\name{data_lower_silesian}
\alias{data_lower_silesian}
\docType{data}
\title{The evaluation of tourist attractiveness of Lower Silesian counties}
\description{The empirical study uses the statistical data presented in the article (Gryszel, Walesiak, 2014) and referring to the attractiveness level of 31 objects
(29 Lower Silesian counties, pattern and antipattern object)   
The evaluation of tourist attractiveness of Lower Silesian counties was performed using 16 metric variables (measured on a ratio scale):
x1 – beds in hotels per 1 km2 of a county area,
x2 – number of nights spent daily by resident tourists per 1000 inhabitants of a county,
x3 – number of nights spent daily by foreign tourists per 1000 inhabitants of a county,
x4 – gas pollution emission in tons per 1 km2 of a county area,
x5 – number of criminal offences and crimes against life and health per 1000 inhabitants of a county,
x6 – number of property crimes per 1000 inhabitants of a county,
x7 – number of historical buildings per 100 km2 of a county area,
x8 – % of a county forest cover,
x9 – % share of legally protected areas within a county area,
x10 – number of events as well as cultural and tourist ventures in a county,
x11 – number of natural monuments calculated per 1 km2 of a county area,
x12 – number of tourist economy entities per 1000 inhabitants of a county (natural and legal persons),
x13 – expenditure of municipalities and counties on tourism, culture and national heritage protection as well as physical culture per 1 inhabitant of a county in PLN,
x14 – viewers in cinemas per 1000 inhabitants of a county,
x15 – museum visitors per 1000 inhabitants of a county,
x16 – number of construction permits (hotels and accommodation buildings, commercial and service buildings, transport and communication buildings, civil and water engineering constructions) issued in a county in the years 2011-2012 per 1 km2 of a county area.
The statistical data were collected in 2012 and come from the Local Data Bank of the Central Statistical Office of Poland, the data for x7 variable only were obtained from the regional conservation officer.
}
\format{data.frame: 31 objects (29 counties, pattern and antipattern object), 16 variables. 
The coordinates of a pattern object cover the most preferred preference variable (stimulants, destimulants, nominants) values. 
The coordinates of an anti-pattern object cover the least preferred preference variable values.}
\source{
Gryszel, P., Walesiak, M., (2014), Zastosowanie uogólnionej miary odległości GDM w ocenie atrakcyjności turystycznej powiatów Dolnego Śląska [The Application of the General Distance Measure (GDM) in the Evaluation of Lower Silesian Districts’ Attractiveness], Folia Turistica, 31, 127-147. Available at: \url{http://www.folia-turistica.pl/attachments/article/402/FT_31_2014.pdf}.
}
\examples{
\donttest{
  library(mdsOpt)
  metnor<-c("n1","n2","n3","n5","n5a","n8","n9","n9a","n11","n12a")
  metscale<-c("ratio","interval")
  metdist<-c("euclidean","GDM1")
  data(data_lower_silesian)
  res<-optSmacofSym_mMDS(data_lower_silesian,normalizations=metnor,
  distances=metdist,mdsmodels=metscale)
  print(findOptimalSmacofSym(res))
  }
}
\keyword{data set}