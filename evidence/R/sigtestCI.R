sigtestCI <- function(p) {
    if (runif(1) < (1 - p)) {
        cat("H0 NOT rejected\n")
        cat(100 * (1 - p), "% conf. int.: empty set\n")
    } else {
        cat("Hurray!!! H0 REJECTED!!!\n")
        cat(100 * (1 - p), paste("% conf. int.: -", 
            expression(infinity), "to +", expression(infinity), 
            "\n"))
    }
}
