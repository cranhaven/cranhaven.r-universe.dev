# Show for Tensor: update the calls of head()
setMethod(f="show",
          signature="Tensor",
          definition=function(object){
            cat("Numeric Tensor of", object@num_modes, "Modes\n", sep=" ")
            cat("Modes: ", object@modes, "\n", sep=" ")
            cat("Data: \n")
            # print(head(as.vector(object@data), n = 6))
            print(summary(c(object@data)))
          })
