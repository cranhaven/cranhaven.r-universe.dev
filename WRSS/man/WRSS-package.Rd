\name{WRSS-package}
\alias{WRSS-package}
\alias{WRSS}
\docType{package}
\title{
  Water Resources System Simulator
}
\description{
  The WRSS is an object-oriented R package, which provides tools for simulation and analysis of large-scale supply and hydropower water resources systems. The package includes functions and methods for building, simulation, and visualization of water resources components.
}
\details{
  \tabular{ll}{
    Package: \tab WRSS\cr
    Type: \tab Package\cr
    Version: \tab 3.0\cr
    Date: \tab 2022-05-17\cr
    License: \tab GPL-3\cr
  }
  the package includes three major types of functions as follows:
    
    1- functions for construction and manipulatation of water resources features:
    \describe{
    \item{a) \code{\link[WRSS]{createArea}}}{constructor for basin/study area objects}
    \item{b) \code{\link[WRSS]{createJunction}}}{constructor for junction objects}
    \item{c) \code{\link[WRSS]{createRiver}}}{constructor for reach, river, and channel objects}
    \item{d) \code{\link[WRSS]{createReservoir}}}{constructor for reservoir objects}
    \item{e) \code{\link[WRSS]{createDiversion}}}{constructor for diversion objects}
    \item{f) \code{\link[WRSS]{createAquifer}}}{constructor for aquifer objects}
    \item{g) \code{\link[WRSS]{createDemandSite}}}{constructor for demand site objects}
    \item{h) \code{\link[WRSS]{set.as}}}{WRSS objects connector}
    \item{i) \code{\link[WRSS]{addObjectToArea}}}{adds objects form mentioned above constructors to a basin inherited from class of \code{createBasin}}
  }                                           
  
  2- functions for analysis and operation of water resources objects using Standard Operating Policy (SOP):
    \describe{
    \item{a) \code{\link[WRSS]{riverRouting}}}{river operation using}
    \item{b) \code{\link[WRSS]{reservoirRouting}}}{reservoir operation}
    \item{c) \code{\link[WRSS]{aquiferRouting}}}{aquifer operation}
    \item{d) \code{\link[WRSS]{diversionRouting}}}{diversion operation}
    \item{e) \code{\link[WRSS]{sim}}}{simulates an objects inherited from class of \code{createArea}}
    \item{f) \code{\link[WRSS]{rippl}}}{computes no-failure storage volume using the sequent peak algorithm(SPA)}
    \item{g) \code{\link[WRSS]{cap_design}}}{computes RRV measures for a range of design parameters}
  }          
  3- functions for performance analysis and visualization.
  \describe{
    \item{a) \code{\link[WRSS]{plot.sim}}}{plots the results of simulations for an object inherited from class of \code{sim}}
    \item{b) \code{\link[WRSS]{plot.createArea}}}{plots an object from class of \code{createArea}}
    \item{c) \code{\link[WRSS]{risk}}}{computes risk-based criateria for an object inherited from class of \code{sim}}
    \item{d) \code{\link[WRSS]{GOF}}}{Goodness of fit function}
  }
}
\author{
  Rezgar Arabzadeh; Parisa Aberi; Kaveh Panaghi; Shahab Araghinejad; Majid Montaseri
  
  Maintainer: Rezgar Arabzadeh <rezgararabzadeh@ut.ac.ir>
}
\examples{
###---------- loading data
data(zarrineh)

###---------- Constructing main features of Zerrineh river basin
Area<-createArea(name='Zerrineh',location='Kurdistan',
                 simulation=list(start='1900-01-01',
                                 end='1909-12-01',
                                 interval='month'))

   ###---------- Bukan dam
Q<-zarrineh$bukan$timeSeries[,1]
E<-zarrineh$bukan$timeSeries[,2]
R<-zarrineh$bukan$timeSeries[,3]
D<-zarrineh$bukan$timeSeries[,4]
A<-zarrineh$bukan$timeSeries[,5]
RC<-zarrineh$bukan$ratingCurve
min<-zarrineh$bukan$capacity[1]$min
max<-zarrineh$bukan$capacity[2]$max
bukan<-createReservoir(name='bukan',netEvaporation=E,
                       initialStorage=max, 
                       geometry=list(deadStorage=min,
                                     capacity=max,
                                     storageAreaTable=RC))
Zerrineh<-createRiver(name='Zerrineh-River',downstream=bukan,discharge=Q)
R<-createDemandSite(name='E1',demandTS=R,suppliers=list(bukan),priority=1)
D<-createDemandSite(name='U1',demandTS=D,suppliers=list(bukan),priority=2)
A<-createDemandSite(name='A1',demandTS=A,suppliers=list(bukan),priority=3)
Area<-addObjectToArea(Area,Zerrineh)
Area<-addObjectToArea(Area,bukan)
Area<-addObjectToArea(Area,R)
Area<-addObjectToArea(Area,D)
Area<-addObjectToArea(Area,A)

   ###---------- a junction located in Bukan dam upstream
J<-createJunction(name='J1', downstream=Zerrineh)
Area<-addObjectToArea(Area,J)

   ###---------- Markhuz dam
Q<-zarrineh$Markhuz$timeSeries[,1]
E<-zarrineh$Markhuz$timeSeries[,2]
A<-zarrineh$Markhuz$timeSeries[,3]
RC<-zarrineh$Markhuz$ratingCurve
min<-zarrineh$Markhuz$capacity[1]$min
max<-zarrineh$Markhuz$capacity[2]$max
Markhuz<-createReservoir(name='Markhuz',netEvaporation=E,
                         downstream=J,initialStorage=max,
                         geometry=list(deadStorage=min,
                         capacity=max,
                         storageAreaTable=RC))
River<-createRiver(name='Markhuz-River',downstream=Markhuz,discharge=Q)
A<-createDemandSite(name='A3',demandTS=A,returnFlowFraction=0.3,
                    suppliers=list(Markhuz),downstream=J,priority=1)
Area<-addObjectToArea(Area, River)
Area<-addObjectToArea(Area, Markhuz)
Area<-addObjectToArea(Area, A)

   ###---------- Cheragh Veys dam
Q<-zarrineh$cheraghVeys$timeSeries[,1]
E<-zarrineh$cheraghVeys$timeSeries[,2]
R<-zarrineh$cheraghVeys$timeSeries[,3]
D<-zarrineh$cheraghVeys$timeSeries[,4]
A<-zarrineh$cheraghVeys$timeSeries[,5]
RC<-zarrineh$cheraghVeys$ratingCurve
min<-zarrineh$cheraghVeys$capacity[1]$min
max<-zarrineh$cheraghVeys$capacity[2]$max
cheraghVeys<-createReservoir(name='cheraghVeys',netEvaporation=E,
                             downstream=J,initialStorage=max,
                             geometry=list(deadStorage=min,
                                           capacity=max,
                                           storageAreaTable=RC))
River<-createRiver(name='Cheragh Veys-River',downstream=cheraghVeys,discharge=Q)
R<-createDemandSite(name='E2',demandTS=R,returnFlowFraction=1.0,
                    suppliers=list(cheraghVeys),downstream=J,priority=1)
D<-createDemandSite(name='U2',demandTS=D,returnFlowFraction=0.7,
                    suppliers=list(cheraghVeys),downstream=J,priority=2)
                    A<-createDemandSite(name='A2',demandTS=A,returnFlowFraction=0.3,
suppliers=list(cheraghVeys),downstream=J,priority=3)
Area<-addObjectToArea(Area, River)
Area<-addObjectToArea(Area, cheraghVeys)
Area<-addObjectToArea(Area, R)
Area<-addObjectToArea(Area, D)
Area<-addObjectToArea(Area, A)

   ###---------- Sonata dam
Q<-zarrineh$Sonata$timeSeries[,1]
E<-zarrineh$Sonata$timeSeries[,2]
R<-zarrineh$Sonata$timeSeries[,3]
A<-zarrineh$Sonata$timeSeries[,4]
RC<-zarrineh$Sonata$ratingCurve
min<-zarrineh$Sonata$capacity[1]$min
max<-zarrineh$Sonata$capacity[2]$max
Sonata<-createReservoir(name='Sonata',netEvaporation=E,downstream=J,
                        initialStorage=max,
                        geometry=list(deadStorage=min,
                                      capacity=max,
                                      storageAreaTable=RC))
River<-createRiver(name='Sonata-River',downstream=Sonata,discharge=Q)
R<-createDemandSite(name='E3',demandTS=R,returnFlowFraction=1.0,
                    suppliers=list(Sonata),downstream=J,priority=1)
A<-createDemandSite(name='A4',demandTS=A,returnFlowFraction=0.3,
                    suppliers=list(Sonata),downstream=J,priority=2)
Area<-addObjectToArea(Area, River)
Area<-addObjectToArea(Area, Sonata)
Area<-addObjectToArea(Area, R)
Area<-addObjectToArea(Area, A)

   ###---------- Sarogh dam
Q<-zarrineh$Sarogh$timeSeries[,1]
E<-zarrineh$Sarogh$timeSeries[,2]
D<-zarrineh$Sarogh$timeSeries[,3]
A<-zarrineh$Sarogh$timeSeries[,4]
RC<-zarrineh$Sarogh$ratingCurve
min<-zarrineh$Sarogh$capacity[1]$min
max<-zarrineh$Sarogh$capacity[2]$max
Sarogh<-createReservoir(name='Sarogh',netEvaporation=E,downstream=J,
                        initialStorage=max,
                        geometry=list(deadStorage=min,
                                      capacity=max,
                                      storageAreaTable=RC))
River<-createRiver(name='Sarogh-River',downstream=Sarogh,discharge=Q)
D<-createDemandSite(name='U3',demandTS=D,returnFlowFraction=0.7,
                    suppliers=list(Sarogh),downstream=J,priority=1)
                    A<-createDemandSite(name='A5',demandTS=A,returnFlowFraction=0.3,
suppliers=list(Sarogh),downstream=J,priority=2)
Area<-addObjectToArea(Area, River)
Area<-addObjectToArea(Area, Sarogh)
Area<-addObjectToArea(Area, D)
Area<-addObjectToArea(Area, A)
\dontrun{
plot(Area)
}
plot(sim(Area))
}
\references{
  Loucks, Daniel P., et al. Water resources systems planning and management: an introduction to methods, models and applications. Paris: Unesco, 2005.
  Arabzadeh, R.; Aberi, P.; Hesarkazzazi, S.; Hajibabaei, M.; Rauch, W.; Nikmehr, S.; Sitzenfrei, R. WRSS: An Object-Oriented R Package for Large-Scale Water Resources Operation. Water 2021, 13, 3037. https://doi.org/10.3390/w13213037
}
\keyword{package}
\seealso{\code{\link[WRSS]{addObjectToArea}}, \code{\link[WRSS]{plot.sim}}}