
summary.vht <- function(object, ...) {
  if (!inherits(object, "vht")) {
    stop("This object is not of class 'vht'.")
  }
  
    cat("\n", "",object$method,paste("(alpha = ",object$alpha,")",sep = ""), "\n", 
        sep = " ")
    header_line <- paste(object$method, paste("(alpha =", object$alpha, ")"))
    line_length <- max(nchar(header_line), 40)
    line_sep <- strrep("-", (line_length+3))
    cat(line_sep, "\n", sep = " ")
    dep_var <- deparse(object$formula[[2]])
    grp_var <- deparse(object$formula[[3]])
    cat("  dependent var :", dep_var, "\n")
    cat("  grouping var  :", grp_var, "\n\n")
    
    if(object$method == "Cochran's C Test" | object$method == "Modified Z Variance Test" 
       | object$method == "Fisher's Test" | object$method == "G Test" 
       | object$method == "Levene's Test" | object$method == "O'Brien Test"   
       | object$method == "Z Variance Test"){
    cat("  F         =", object$statistic, "\n", sep = " ")
    cat("  num df    =", object$parameter[[1]], "\n", sep = " ")
    cat("  denom df  =", object$parameter[[2]], "\n", sep = " ")
    
    } else if (object$method == "Hartley's Maximum F-Ratio Test"){
      cat("  F-max     =", object$statistic, "\n", sep = " ")
      cat("  df        =", object$parameter[[1]], "\n", sep = " ")
      
    } else if(object$method == "Bartlett's Test" | object$method == "Ansari Bradley Test"
               | object$method == "Capon Test" | object$method == "David Barton Test"
               | object$method == "Duran Test" | object$method == "Adjusted Taha Test"
               | object$method == "Fligner-Killeen Test" | object$method == "Klotz Test"
               | object$method == "Mood Test" | object$method == "Siegel Tukey Test"
               | object$method == "Taha Test"| object$method == "Talwar and Gentle Test" ){
      cat("  X-squared =", object$statistic, "\n", sep = " ")
      cat("  df        =", object$parameter, "\n", sep = " ")
    }
    
      
     
    if(object$p.value < 2.2e-16) cat("  p-value   < 2.2e-16\n\n") else cat("  p-value   =", format.pval(object$p.value, ...), "\n\n", sep = " ")
    cat(if (object$p.value > object$alpha) {
      "  Result : Variances are homogeneous."
    }
    else {
      "  Result : Variances are not homogeneous."
    }, "\n")
    cat(line_sep, "\n", sep = " ")
  
}

