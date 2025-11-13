print.catpredi <-
function(x, digits = 4, ...) {
	cat("\nCall:\n")
	print(x$call)    
	cat("\n\n*************************************************\n")
	cat("\n*************************************************\n")
	method <- switch(x$method,
	"addfor" = cat("Addfor Search Algorithm"),
	"genetic" = cat("Genetic Search Algorithm"),
	"backaddfor" = cat("Backaddfor Search Algorithm"))
	cat("\n*************************************************\n")
	cat("\n*************************************************\n\n")
  if(x$method == "addfor") {
	if(x$correct.AUC == TRUE){
	cutpoints <- format(x$results$cutpoints, digits = digits, justify = "right")
		AUC  <- format(x$results$AUC, digits = digits, justify = "left")
		AUC.corrected <- format(x$results$AUC.cor, digits = digits, justify = "left")
		p.table <- cbind(cutpoints, AUC,c(rep("NA",length(x$results$cutpoints)-1),AUC.corrected))
		dimnames(p.table) <- list(rep("", l = length(x$results$AUC)), c("Optimal cutpoints", "Optimal AUC", "Corrected AUC"))
		print(p.table, quote = FALSE, justify = "right")    
	} else {
		cutpoints <- format(x$results$cutpoints, digits = digits, justify = "right")
		AUC  <- format(x$results$AUC, digits = digits, justify = "left")
		p.table <- cbind(cutpoints, AUC)
		dimnames(p.table) <- list(rep("", l = length(x$results$AUC)), c("Optimal cutpoints", "Optimal AUC"))
		print(p.table, quote = FALSE, justify = "right")
	} 
	} else { # Genetic/Backaddfor
		
	if(x$correct.AUC == TRUE){
	cutpoints <- cbind(format(sort(x$results$cutpoints), digits = digits, justify = "right"))
		dimnames(cutpoints) <- list(rep("", l = length(x$results$cutpoints)), "Optimal cutpoints")
		print(cutpoints, quote = FALSE, justify = "right")
		cat("\n")
		AUC <- cbind(format(x$results$AUC, digits = digits, justify = "right"))
		dimnames(AUC) <- list(rep("", l = length(x$results$AUC)), "Optimal AUC")
		print(AUC, quote = FALSE, justify = "right")	
	cat("\n")
		AUC.corrected <- cbind(format(x$results$AUC.cor, digits = digits, justify = "right"))
		dimnames(AUC.corrected) <- list(rep("", l = length(x$results$AUC.cor)), "Corrected AUC")
		print(AUC.corrected, quote = FALSE, justify = "right")	
		
	} else {
	cutpoints <- cbind(format(sort(x$results$cutpoints), digits = digits, justify = "right"))
		dimnames(cutpoints) <- list(rep("", l = length(x$results$cutpoints)), "Optimal cutpoints")
		print(cutpoints, quote = FALSE, justify = "right")
		cat("\n")
		AUC <- cbind(format(x$results$AUC, digits = digits, justify = "right"))
		dimnames(AUC) <- list(rep("", l = length(x$results$AUC)), "Optimal AUC")
		print(AUC, quote = FALSE, justify = "right")
		}
	}
	invisible(x)
}
