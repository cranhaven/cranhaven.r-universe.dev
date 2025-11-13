print.catpredi.survival <-
function(x, digits = 4, ...) {
	cat("\nCall:\n")
	print(x$call)    
	cat("\n\n*************************************************\n")
	method <- switch(x$method,
	"addfor" = cat("Addfor Search Algorithm"),
	"genetic" = cat("Genetic Search Algorithm"),
	"backaddfor" = cat("Backaddfor Search Algorithm"))
	conc.index <- switch(x$conc.index,
	"cindex" = cat("Concordance C-index"),
	"cpe" = cat("Concordance Probability Estimator - CPE"))
	cat("\n*************************************************\n\n")
	if(x$method == "addfor" & x$conc.index == "cindex") {
		if(x$correct.index == TRUE){
			cutpoints <- format(x$results$cutpoints, digits = digits, justify = "right")
			Cindex  <- format(x$results$Cindex, digits = digits, justify = "left")
			Cindex.corrected <- format(x$results$Cindex.cor, digits = digits, justify = "left")
			p.table <- cbind(cutpoints, Cindex,c(rep("NA",length(x$results$cutpoints)-1),Cindex.corrected))
			dimnames(p.table) <- list(rep("", l = length(x$results$Cindex)), c("Optimal cutpoints", "Optimal C-index", "Corrected C-index"))
			print(p.table, quote = FALSE, justify = "right")    
		} else {
			cutpoints <- format(x$results$cutpoints, digits = digits, justify = "right")
			Cindex  <- format(x$results$Cindex, digits = digits, justify = "left")
			p.table <- cbind(cutpoints, Cindex)
			dimnames(p.table) <- list(rep("", l = length(x$results$Cindex)), c("Optimal cutpoints", "Optimal C-index"))
			print(p.table, quote = FALSE, justify = "right")
		}
	} else if(x$method == "addfor" & x$conc.index == "cpe") {
		if(x$correct.index == TRUE){
			cutpoints <- format(x$results$cutpoints, digits = digits, justify = "right")
			CPE  <- format(x$results$CPE, digits = digits, justify = "left")
			CPE.corrected <- format(x$results$CPE.cor, digits = digits, justify = "left")
			p.table <- cbind(cutpoints, CPE,c(rep("NA",length(x$results$cutpoints)-1),CPE.corrected))
			dimnames(p.table) <- list(rep("", l = length(x$results$CPE)), c("Optimal cutpoints", "Optimal CPE", "Corrected CPE"))
			print(p.table, quote = FALSE, justify = "right")    
		} else {
			cutpoints <- format(x$results$cutpoints, digits = digits, justify = "right")
			CPE  <- format(x$results$CPE, digits = digits, justify = "left")
			p.table <- cbind(cutpoints, CPE)
			dimnames(p.table) <- list(rep("", l = length(x$results$CPE)), c("Optimal cutpoints", "Optimal CPE"))
			print(p.table, quote = FALSE, justify = "right")
		}
	} else if(x$method == "genetic" & x$conc.index == "cindex") {  
		if(x$correct.index == TRUE){
		cutpoints <- cbind(format(sort(x$results$cutpoints), digits = digits, justify = "right"))
			dimnames(cutpoints) <- list(rep("", l = length(x$results$cutpoints)), "Optimal cutpoints")
			print(cutpoints, quote = FALSE, justify = "right")
			cat("\n")
			Cindex <- cbind(format(x$results$Cindex, digits = digits, justify = "right"))
			dimnames(Cindex) <- list(rep("", l = length(x$results$Cindex)), "Optimal Cindex")
			print(Cindex, quote = FALSE, justify = "right")	
			cat("\n")
			Cindex.corrected <- cbind(format(x$results$Cindex.cor, digits = digits, justify = "right"))
			dimnames(Cindex.corrected) <- list(rep("", l = length(x$results$Cindex.cor)), "Corrected Cindex")
			print(Cindex.corrected, quote = FALSE, justify = "right")			
		} else {
			cutpoints <- cbind(format(sort(x$results$cutpoints), digits = digits, justify = "right"))
			dimnames(cutpoints) <- list(rep("", l = length(x$results$cutpoints)), "Optimal cutpoints")
			print(cutpoints, quote = FALSE, justify = "right")
			cat("\n")
			Cindex <- cbind(format(x$results$Cindex, digits = digits, justify = "right"))
			dimnames(Cindex) <- list(rep("", l = length(x$results$Cindex)), "Optimal Cindex")
			print(Cindex, quote = FALSE, justify = "right")
		}
	} else {
		if(x$correct.index == TRUE) {
			cutpoints <- cbind(format(sort(x$results$cutpoints), digits = digits, justify = "right"))
			dimnames(cutpoints) <- list(rep("", l = length(x$results$cutpoints)), "Optimal cutpoints")
			print(cutpoints, quote = FALSE, justify = "right")
			cat("\n")
			CPE <- cbind(format(x$results$CPE, digits = digits, justify = "right"))
			dimnames(CPE) <- list(rep("", l = length(x$results$CPE)), "Optimal CPE")
			print(CPE, quote = FALSE, justify = "right")	
			cat("\n")
			CPE.corrected <- cbind(format(x$results$CPE.cor, digits = digits, justify = "right"))
			dimnames(CPE.corrected) <- list(rep("", l = length(x$results$CPE.cor)), "Corrected CPE")
			print(CPE.corrected, quote = FALSE, justify = "right")				
		} else {
			cutpoints <- cbind(format(sort(x$results$cutpoints), digits = digits, justify = "right"))
			dimnames(cutpoints) <- list(rep("", l = length(x$results$cutpoints)), "Optimal cutpoints")
			print(cutpoints, quote = FALSE, justify = "right")
			cat("\n")
			CPE <- cbind(format(x$results$CPE, digits = digits, justify = "right"))
			dimnames(CPE) <- list(rep("", l = length(x$results$CPE)), "Optimal CPE")
			print(CPE, quote = FALSE, justify = "right")
		}
	}
	invisible(x)
}
