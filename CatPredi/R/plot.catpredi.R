plot.catpredi <-
function(x, ...){
  # Activate pause between plots
  old_ask <- par(ask = TRUE)
  
  # Ensure the setting is reset at the end
  on.exit(par(ask = old_ask))
  
  # Fit the model
  formula <- update(x$formula, as.formula(paste("~ . + s(", x$cat.var, ")", sep = "")))
  fit.gam <- gam(formula, family = binomial, data = x$data)
  pos <- length(attr(terms.formula(formula, specials = c("s")),"specials")$s)
  plot(fit.gam, select = pos, shade = TRUE, ylab = paste0("f(",x$cat.var,")"), all.terms = TRUE)
  abline(v = x$results$cutpoints, lty = 2)
  
  # Plot the OR
  formula <- update(x$formula, as.formula(paste0("~ . +", x$cat.var,"_CatPredi")))
  fit.gam.cut <- gam(formula, family = binomial, data = x$data)
  x_CatPredi = unique(x$data[,paste0(x$cat.var,"_CatPredi")])
  sel.cuts <- paste0(x$cat.var,"_CatPredi",x_CatPredi[-1])
  ORs.xcut <- data.frame(x_CatPredi = x_CatPredi,
                         OR = exp(fit.gam.cut$coef)[c("(Intercept)",sel.cuts)], 
                         CI_Lower = exp(confint.default(fit.gam.cut))[c("(Intercept)",sel.cuts),1],
                         CI_Upper = exp(confint.default(fit.gam.cut))[c("(Intercept)",sel.cuts),2])
  
  p <- ggplot(ORs.xcut, aes(x = x_CatPredi, y = ORs.xcut$OR)) +
    geom_point(size = 3, color = "blue") + # Plot the ORs
    geom_errorbar(aes(ymin = ORs.xcut$CI_Lower, ymax = ORs.xcut$CI_Upper), width = 0.2, color = "blue") + # Add error bars
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") + # Reference line at OR = 1
    coord_flip() + # Flip coordinates for better readability
    labs(
      title = "Odds Ratios with 95% Confidence Intervals",
      x = paste0(x$cat.var,"_CatPredi"),
      y = "Odds Ratio (OR)"
    ) +
    theme_minimal()
  print(p)
}
