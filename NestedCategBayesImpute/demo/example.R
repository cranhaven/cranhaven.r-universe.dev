
###################################################################################
###################################### START ######################################
###################################################################################

library(NestedCategBayesImpute)
library(dplyr)

### Set indicator for whether of not to move the household head
### Also set indicator for the weighting/capping option
options <- list()
options$HHhead_at_group_level <- TRUE #set to TRUE to move household head to the group level
options$weight_option <- FALSE #set to FALSE for weighting/capping option. If TRUE, must supply weights

PrepareData <- function(options) {
  ### Use data included in package; prepare data and specify variable indexes
  if (options$HHhead_at_group_level) {
    orig.file <- system.file("extdata","origdata.txt",package="NestedCategBayesImpute")
    orig.data <- read.table(orig.file,header = TRUE, sep = " ")
    orig.data$relate <- orig.data$relate - 1L #recode relate variable to 11 levels
    household.size <- as.data.frame(table(orig.data$Hhindex))
    household.size[,1] <- as.numeric(household.size[,1]) #conver factor to numeric
    names(household.size) <- c("Hhindex", 'householdsize')
    household <- orig.data %>% inner_join(household.size)

    individual_variable_index = c(3:7)
    household_variable_index = c(8:13) #make sure the last column represents household size

  } else {
    orig.file <- system.file("extdata","origdata_oldFormat.txt",package="NestedCategBayesImpute")
    orig.data <- read.table(orig.file,header = TRUE, sep = " ")
    orig.data$Hhindex
    household.size <- as.data.frame(table(orig.data$Hhindex))
    household.size[,1] <- as.numeric(household.size[,1])
    names(household.size) <- c("Hhindex", 'householdsize')
    household <- orig.data %>% inner_join(household.size)

    individual_variable_index = c(3:7)
    household_variable_index = c(8,9) #make sure the last column represents household size
  }
  return(list(household = household,
              individual_variable_index = individual_variable_index,
              household_variable_index = household_variable_index))
}

ExampleData <- PrepareData(options)

### Initialize and set parameters for missing data
MissData <- initMissing(ExampleData,
                        struc_zero_variables=c("sex","age","relate","headsex","headage"),
                        miss_batch=10)

MissData$prop_batch <- 1.2

### Initialize the input data structure
orig <- initData(MissData)

### Set parameters for faulty data
#ErrorData <- SetErrorPara_EI(orig,var_in_error_house=c("headsex","headage"),var_in_error_indiv=c("sex","age","relate"),
#                             imp_batch=1000,error_batch=10,prop_batch=1.2)

### Supply weights; one for each household size
if(options$weight_option){
  struc_weight <- c(1/2,1/2,1/3) #must be ordered & no household size must be excluded
} else {
  struc_weight <- rep(1,length(orig$n_star_h)) #just a dummy column of ones if struc_weight=FALSE
}

### Set mcmc parameters

mc <- list(nrun = 100, burn = 50, thin = 5)
#mc <- list(nrun = 10000, burn = 5000, thin = 5)
mc$eff.sam <- (mc$nrun-mc$burn)/mc$thin

### Set number of categories for each household level variable
dHH <- rep(0,length(ExampleData$household_variable_index))
for (i in 1:length(dHH)) {
  dHH[i] <- max(na.omit(ExampleData$household[,ExampleData$household_variable_index[i]]))
  if (i == length(dHH) & !options$HHhead_at_group_level) {
    dHH[length(dHH)] <- dHH[length(dHH)] - 1 #Household head within household assumes that the household size starts from 2
  }
}

### Set hyper parameters
#aa & ab are gamma hyperparameters for alpha while ba & bb are gamma hyperparameters for beta
#blocksize is the number of impossible households to sample at once (we use batch sampling to speed up mcmc)
#FF is the max number of group-level latent classes
#SS is the max number of individual-level classes
hyper <- list(FF=20 , SS=15, aa=0.25, ab=0.25, ba=0.25,bb=0.25,dHH = dHH, blocksize = 10000)


### Initialize parameters and output
para <- initParameters(orig,hyper,options$HHhead_at_group_level)
output <- initOutput(orig,hyper,mc)



### Set number of synthetic data and the mcmc indexes for them
mm <- 5
synindex <- NULL
MissData$miss_index <- round(seq((mc$burn +1),mc$nrun,length.out=mm))

### Run model

ModelResults <- RunModel(orig,mc,hyper,para,output,synindex,
                         ExampleData$individual_variable_index,
                         ExampleData$household_variable_index,
                         options$HHhead_at_group_level,options$weight_option,struc_weight,MissData,Parallel = TRUE)


###################################################################################
####################################### END #######################################
###################################################################################







