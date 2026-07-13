## ----memoryUse, eval=FALSE, echo=TRUE-----------------------------------------
# if (requireNamespace("future", quietly = TRUE) &&
#     requireNamespace("future.callr", quietly = TRUE)) {
#   options(spades.memoryUseInterval = 0.5)
# 
#   # sim <- simInit(...)
#   # sim <- spades(sim)
# 
#   memoryUse(sim, max = TRUE)  # peak memory by event type (summarised across times)
#   memoryUse(sim, max = FALSE) # peak memory for every individual event
# }

## ----codecheck-options, eval=FALSE--------------------------------------------
# options(spades.moduleCodeChecks = TRUE)    # turn checking on/off
# options(spades.codeCheckEngine = "v2")     # "v1" (legacy) or "v2" (new, structured)

## ----codecheck-standalone, eval=FALSE-----------------------------------------
# # returns a data.frame of findings; also prints a grouped report
# findings <- codeCheckModule("path/to/myModule")
# 
# # subset by severity or rule id
# findings[findings$severity == "warning", ]
# findings[findings$id == "param_used_undeclared", ]

