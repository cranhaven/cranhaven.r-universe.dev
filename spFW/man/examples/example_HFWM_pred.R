library(spFW)

# load and split data
data(spFW_example_data)
idx_pred <- sample(125, 25)

Y0 <- spFW_example_data$yield
VAR0 <- spFW_example_data$geno
ENV0 <- spFW_example_data$loc


Y1 <- Y0[-idx_pred]
Y2 <- Y0[idx_pred]
VAR1 <- VAR0[-idx_pred]
VAR2 <- VAR0[idx_pred]
ENV1 <- ENV0[-idx_pred]
ENV2 <- ENV0[idx_pred]
order_y <- order(Y2)

# run model
pred0 <- HFWM_pred(Y1, VAR1, ENV1, VAR2, ENV2, save_int = TRUE,
                   M_iter = 1000, burn_in = 500, thin = 5)

# visualize prediction results
plot(1:25, pred0$PY[order_y], ylim = c(50, 250), pch = 15, col = "red",
     xlab = "Plant ID for Prediction", ylab = "Yield",
     main = "95% Prediction Intervals with Predicted Mean (Red) Versus True Yield (Blue)")
points(1:25, Y2[order_y], col = "blue")
for (i in 1:25){
  lines(x = c(i,i), y = c(pred0$PY_CI[,order_y][1,i], pred0$PY_CI[,order_y][4,i]))
}



