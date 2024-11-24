## ----eval=FALSE---------------------------------------------------------------
#  #1. Install 'devtools' if you do not already have it installed:
#  install.packages("devtools")
#  
#  #2. Load the 'devtools' package and temporarily install the development version of
#  #'dietr' from GitHub:
#  library(devtools)
#  dev_mode(on=T)
#  install_github("sborstein/dietr")  # install the package from GitHub
#  library(dietr)# load the package
#  
#  #3. Leave developers mode after using the development version of 'dietr' so it will not
#  #remain on your system permanently.
#  dev_mode(on=F)

## ----eval=TRUE, echo= TRUE----------------------------------------------------
library(dietr)

## ----eval=FALSE---------------------------------------------------------------
#  data(FishBasePreyVals)#Load the Fishbase trophic levels of prey items
#  #Standardized trophic levels of prey items for elasmobranchs
#  data(CortesPreyVals)#Load the Cortes (1992)

## ----eval=FALSE---------------------------------------------------------------
#  # Convert Fishbase Diet Data and exclude juvenile and larval records
#  my.diets <- ConvertFishbaseDiet(ExcludeStage=c("recruits/juv.","larvae"))

## ----eval=FALSE---------------------------------------------------------------
#  #Remove Data for Epinephelus itajara
#  my.diets$DietItems <- my.diets$DietItems[my.diets$DietItems$Species ==
#    "Epinephelus itajara",]
#  my.diets$Taxonomy <- my.diets$Taxonomy[my.diets$Taxonomy$Species ==
#    "Epinephelus itajara",]

## ----eval=FALSE---------------------------------------------------------------
#  data(FishBasePreyVals)#load the FishBase prey values that are part of the dietr package
#  #Calculate trophic level with DietTroph function
#  my.TL<-DietTroph(DietItems = my.diets$DietItems,PreyValues = FishBasePreyVals,
#  Taxonomy = my.diets$Taxonomy, PreyClass=c("FoodI","FoodII","FoodIII","Stage"),
#    SumCheck = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  #Get some food item data from rfishbase
#  my.food<-rfishbase::fooditems(c("Lutjanus apodus","Epinephelus itajara"))

## ----eval=FALSE---------------------------------------------------------------
#  #Convert FishBase food item data to a format usable for FoodTroph
#  converted.foods<-ConvertFishbaseFood(my.food)
#  #Calculate trophic level from food items
#  my.TL<-FoodTroph(FoodItems = converted.foods$FoodItems,PreyValues = FishBasePreyVals,
#    Taxonomy = converted.foods$Taxonomy,PreyClass=c("FoodI","FoodII","FoodIII","Stage"),
#    Iter = 100, SE.Type = "TrophLab")

## ----eval=FALSE---------------------------------------------------------------
#  data(Herichthys)

## ----eval=FALSE---------------------------------------------------------------
#  #Subset out individuals with diet data
#  HMdat<-Herichthys[Herichthys$total==100,]

## ----eval=FALSE---------------------------------------------------------------
#  #Make a data frame of the individuals, lake by year, and lake.
#  HMtax<-cbind.data.frame(HMdat$individual,paste(HMdat$lake,HMdat$year),HMdat$lake)
#  #Name the data frame
#  colnames(HMtax)<-c("Individual","Lake x Year","Lake (all years)")
#  #To calculate trophic level for the entire species, add a vector to the data frame of
#  #the species name
#  HMtax$species<-"Herichthys minckleyi"

## ----eval=FALSE---------------------------------------------------------------
#  HMdat<-HMdat[,c("individual","X.Gastrop","X.Insect","X.Fish","X.Zoopl","X.plants",
#              "X.algae", "X.detritus")]

## ----eval=FALSE---------------------------------------------------------------
#  #Repeat the individual name the number of unique prey types (6)
#  Inds<-rep(x = HMdat$individual, times=6)[order(rep(x = HMdat$individual, times=6))]
#  #Repeat the number of food typed the length of the number of individuals
#  FoodTypes<-rep(x = colnames(HMdat[2:7]),times=length(unique(HMtax$Individual)))
#  #Make a data frame, the length of the individuals with three columns
#  HM.mat<-as.data.frame(matrix(nrow = length(Inds),ncol = 3))
#  #Name these columns
#  colnames(HM.mat)<-c("Individual","FoodItem","Percent")
#  #Populate the dataframes first column with the individual and the second column with
#  #the prey type
#  HM.mat$Individual<-Inds
#  HM.mat$FoodItem<-FoodTypes
#  #Run this for loop to find the diet data based on the individual and then match the
#  #diet percentage based on the name of the prey type
#  for(i in 1:nrow(HMdat)){
#    rows2match<-which(HM.mat$Individual==HMdat$individual[i])
#    HM.mat$Percent[rows2match]<-as.vector(as.numeric(HMdat[i,2:7]))
#  }
#  #Remove prey that do not contribute to diets
#  HM.mat<-HM.mat[!HM.mat$Percent==0,]

## ----eval=FALSE---------------------------------------------------------------
#  #Create a empty data frame for prey values
#  PreyMat<-as.data.frame(matrix(ncol = 3,nrow = 6))
#  #Name the columns something useful
#  colnames(PreyMat)<-c("FoodItem","TL","SE")
#  #Add in the prey name to the PreyMat
#  PreyMat[,1]<-unique(FoodTypes)
#  #Add in the trophic levels of the prey
#  PreyMat[,2]<-c(2.37,2.2,3.5,2.1,2,2)
#  #Add in the SE of the prey
#  PreyMat[,3]<-c(.58,.4,.8,.3,0,0)

## ----eval=FALSE---------------------------------------------------------------
#  HM.TL<-DietTroph(DietItems = HM.mat,PreyValues = PreyMat, PreyClass = "FoodItem",
#    Taxonomy = HMtax, SumCheck = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  data(Horn1982)# load data
#  Horn1982$Consumed#See data for prey consumption
#  Horn1982$Available#See data for available prey

## ----eval=TRUE, echo=TRUE-----------------------------------------------------
my.indices <- Electivity(Diet = Horn1982$Consumed, Available = Horn1982$Available, 
  Indices = c("ForageRatio","Ivlev","Strauss","JacobsQ","JacobsD","Chesson",
  "VanderploegScavia"),LogQ = TRUE, CalcAbundance = FALSE, Depleting = FALSE)

## ----eval=TRUE, echo=FALSE, fig.height=6, fig.width=11, echo=TRUE-------------
PlotElectivity(Electivity.Calcs = my.indices, Indices = "VanderploegScavia", 
  BarColor = c("Red","Purple","Black","Grey"))

## ----eval=TRUE, echo=TRUE, fig.height=6, fig.width=11-------------------------
PlotElectivity(Electivity.Calcs = my.indices)

## -----------------------------------------------------------------------------
#Load diet data from Casaux1998, which contains diet for two populations
#and is listed in percent frequency, percent number, and percent mass.
data(Casaux1998)

#Calculate all three diet indices (IOP, IRI, FQ), return the raw data, and all calculations.
CompositeIndices(DietData = Casaux1998, Indices = c("IOP","IRI","FQ"), PercentNumber = 4, 
  PercentOccurrence = 3, PercentVolWeight = 5, ReturnRaw = TRUE, PercentOnly = FALSE)

#Calculate all three diet indices and return only the percent of the index
CompositeIndices(DietData = Casaux1998, Indices = c("IOP","IRI","FQ"), PercentNumber = 4, 
  PercentOccurrence = 3, PercentVolWeight = 5, ReturnRaw = FALSE, PercentOnly = TRUE)

#Calculate Feeding Quotient and return the raw data and the all calculations.
CompositeIndices(DietData = Casaux1998, Indices = "FQ", PercentNumber = 4, 
  PercentVolWeight = 5,ReturnRaw = FALSE, PercentOnly = FALSE)

## -----------------------------------------------------------------------------
data(SebastesStomachs)#load example data
VacuityIndex(StomachData = SebastesStomachs)

## -----------------------------------------------------------------------------
data(SebastesStomachs)#load example data
#Calculate the Gastro-somatic Index and Vacuity Index
GastrosomaticIndex (StomachData = SebastesStomachs, Calc.Vacuity = TRUE)

