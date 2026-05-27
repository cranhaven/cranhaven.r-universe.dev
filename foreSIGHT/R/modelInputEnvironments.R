# Model Environment Definitions
# Used to store user inputs and default parameters for the models
#--------------------------------------------------------------------------

# Is passing in ... argument to the write function a good idea? maybe not - spelling mistakes in arguments would not be detected
#     Ideal to decide the fields inside the enviornment apriori

foreSIGHT_modelEnv <- new.env(parent = emptyenv())
foreSIGHT_modelEnv$P_modelEnv <- new.env(parent = emptyenv())
foreSIGHT_modelEnv$Temp_modelEnv <- new.env(parent = emptyenv())
foreSIGHT_modelEnv$PET_modelEnv <- new.env(parent = emptyenv())
foreSIGHT_modelEnv$Radn_modelEnv <- new.env(parent = emptyenv())

foreSIGHT_optimizationDiagnosticsEnv <- new.env(parent = emptyenv())

foreSIGHT_optimizationInputOutputEnv <- new.env(parent = emptyenv())

# foreSIGHT_optimizationSeedTrackerEnv <- new.env(parent = emptyenv())

