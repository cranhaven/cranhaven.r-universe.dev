## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load package-------------------------------------------------------------
# Allocate 10GB of memory for Java. Must be called before library(iBART)
options(java.parameters = "-Xmx10g") 
library(iBART)

## ----load data----------------------------------------------------------------
load("../data/catalysis.rda")    # load data
summary(catalysis)

## ----iBART, eval=FALSE--------------------------------------------------------
#  iBART_real_data <- iBART(X = catalysis$X, y = catalysis$y,
#                           head = catalysis$head,  # colnames of X
#                           unit = catalysis$unit,  # units of X
#                           opt = c("binary", "unary", "binary"), # binary operator first
#                           out_sample = FALSE,
#                           Lzero = TRUE,
#                           K = 5, # maximum descriptors in l-zero model
#                           standardize = FALSE,
#                           seed = 888)

## ----load result, echo=FALSE--------------------------------------------------
load("../data/iBART_real_data.rda")             # load full result

## ----size_plot, fig.width=7, fig.height=3.5-----------------------------------
library(ggplot2)
df_dim <- data.frame(dim = c(iBART_real_data$iBART_sel_size, iBART_real_data$iBART_gen_size),
                     iter = rep(0:3, 2),
                     type = rep(c("Selected", "Generated"), each = 4))
ggplot(df_dim, aes(x = iter, y = dim, colour = type, group = type)) +
  theme(text = element_text(size = 15), legend.title = element_blank()) +
  geom_line(size = 1) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_text(data = df_dim, aes(label = dim, y = dim + 40, group = type),
            position = position_dodge(0), size = 5, colour = "blue") +
  labs(x = "Iteration", y = "Number of descriptors") +
  scale_x_continuous(breaks = c(0, 1, 2, 3))

## ----k-descriptor-------------------------------------------------------------
iBART_real_data$Lzero_names[[3]]
summary(iBART_real_data$Lzero_models[[3]])

## ----OIS vs non-OIS, fig.width=7, fig.height=3.5------------------------------
# Train a non-OIS model with 3 predictors
set.seed(123)
model_no_OIS <- k_var_model(X_train = catalysis$X, y_train = catalysis$y, k = 3, parallel = FALSE)

#### Figure 7 ####
library(ggpubr)
model_OIS <- iBART_real_data$Lzero_model[[3]]

# Prepare data for plotting
data_OIS <- data.frame(y = catalysis$y, y_hat = model_OIS$fitted.values)
data_no_OIS <- data.frame(y = catalysis$y, y_hat = model_no_OIS$models$fitted.values)

p1 <- ggplot(data_OIS, aes(x = y_hat, y = catalysis$y)) +
  geom_point() +
  geom_abline() +
  xlim(c(min(data_OIS$y_hat, data_OIS$y) - 0.2, max(data_OIS$y_hat, data_OIS$y) + 0.2)) +
  ylim(c(min(data_OIS$y_hat, data_OIS$y) - 0.2, max(data_OIS$y_hat, data_OIS$y) + 0.2)) +
  xlab("") +
  ylab("") +
  annotate("text", x = -12, y = -3, parse = TRUE,
           label = paste("R^{2} ==", round(summary(model_OIS)$r.squared, 4)))
p2 <- ggplot(data_no_OIS, aes(x = y_hat, y = catalysis$y)) +
  geom_point() +
  geom_abline() +
  xlim(c(min(data_no_OIS$y_hat, data_no_OIS$y) - 0.2, max(data_no_OIS$y_hat, data_no_OIS$y) + 0.2)) +
  ylim(c(min(data_no_OIS$y_hat, data_no_OIS$y) - 0.2, max(data_no_OIS$y_hat, data_no_OIS$y) + 0.2)) +
  xlab("") +
  ylab("") +
  annotate("text", x = -12, y = -3, parse = TRUE,
           label = paste("R^{2} ==", round(summary(model_no_OIS$models)$r.squared, 4)))
fig <- ggarrange(p1, p2,
                 labels = c("OIS", "non-OIS"),
                 ncol = 2, nrow = 1)
annotate_figure(fig,
                bottom = text_grob("Predicted binding energy from descriptors (eV)"),
                left = text_grob("DFT binding energy (eV)", rot = 90))

## ----sessioninfo--------------------------------------------------------------
sessionInfo()

