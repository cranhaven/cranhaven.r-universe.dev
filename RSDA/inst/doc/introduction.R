## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 5
)
library(RSDA)

## ---- eval=F------------------------------------------------------------------
#  install.packages("RSDA", dependencies=TRUE)

## ---- eval=F------------------------------------------------------------------
#  devtools::install_github("PROMiDAT/RSDA")

## -----------------------------------------------------------------------------
ex3 <- read.sym.table(file = 'tsym1.csv', header=TRUE, sep=';',dec='.', row.names=1)
ex3

## ---- eval=F------------------------------------------------------------------
#  write.sym.table(ex3, file = 'tsymtemp.csv', sep = ';',dec = '.',
#                  row.names = TRUE, col.names = TRUE)

## -----------------------------------------------------------------------------
data(example3)
example3

## -----------------------------------------------------------------------------
example3[2,]
example3[,3]
example3[2:3,5]
example3$F1

## -----------------------------------------------------------------------------
data(ex1_db2so)
ex1_db2so

## -----------------------------------------------------------------------------
result <- classic.to.sym(x = ex1_db2so, 
                         concept = c(state, sex),
                         variables = c(county, group, age))
result

## -----------------------------------------------------------------------------
result <- classic.to.sym(x = ex1_db2so, 
                         concept = c("state", "sex"),
                         variables = c(county, group, age),
                         age_hist = sym.histogram(age, breaks = pretty(ex1_db2so$age, 5)))
result

## -----------------------------------------------------------------------------
data(USCrime)
head(USCrime)

## -----------------------------------------------------------------------------
result  <- classic.to.sym(x = USCrime,
                          concept = state, 
                          variables= c(NumInShelters,
                                       NumImmig,
                                       ViolentCrimesPerPop),
                          ViolentCrimesPerPop_hist = sym.histogram(ViolentCrimesPerPop,
                                                                   breaks = pretty(USCrime$ViolentCrimesPerPop,5)))
result

## -----------------------------------------------------------------------------
data("ex_mcfa1") 
head(ex_mcfa1)

## -----------------------------------------------------------------------------
sym.table <- classic.to.sym(x = ex_mcfa1, 
                            concept = suspect, 
                            variables=c(hair,
                                        eyes,
                                        region),
                            default.categorical = sym.set)
sym.table

## -----------------------------------------------------------------------------
sym.table <- classic.to.sym(x = ex_mcfa1, 
                            concept = suspect,
                            default.categorical = sym.set)
sym.table

## -----------------------------------------------------------------------------
hani3101 <- SDS.to.RSDA(file.path = "hani3101.sds")
hani3101

## ---- eval=F------------------------------------------------------------------
#  # We can save the file in CSV to RSDA format as follows:
#  write.sym.table(hani3101,
#                  file='hani3101.csv',
#                  sep=';',
#                  dec='.',
#                  row.names=TRUE,
#                  col.names=TRUE)

## -----------------------------------------------------------------------------
abalone <- SODAS.to.RSDA("abalone.xml")
abalone

## ---- eval=F------------------------------------------------------------------
#  write.sym.table(abalone,
#                  file='abalone.csv',
#                  sep=';',
#                  dec='.',
#                  row.names = TRUE,
#                  col.names = TRUE)

## -----------------------------------------------------------------------------
data(example3)
mean(example3$F1)
mean(example3[,1])

## -----------------------------------------------------------------------------
mean(example3$F2)
mean(example3[,2])

## -----------------------------------------------------------------------------
mean(example3$F2,method = "interval")
mean(example3[,2],method = "interval")

## -----------------------------------------------------------------------------
median(example3$F1)
median(example3[,1])

## -----------------------------------------------------------------------------
median(example3$F2)
median(example3[,2])

## -----------------------------------------------------------------------------
median(example3$F6, method = 'interval')
median(example3[,6], method = 'interval')

## -----------------------------------------------------------------------------
var(example3[,1])
var(example3[,2])
var(example3$F6)
var(example3$F6, method = 'interval')
var(example3$F6, method = 'billard')
sd(example3$F1)
sd(example3$F2)
sd(example3$F6)
sd(example3$F6, method = 'interval')
sd(example3$F6, method = 'billard')

## -----------------------------------------------------------------------------
cor(example3$F1, example3$F4)
cor(example3[,1], example3[,4])
cor(example3$F2, example3$F6, method = 'centers')
cor(example3$F2, example3$F6, method = 'billard')

## -----------------------------------------------------------------------------
library(ggpolypath)

data(oils)
oils <- RSDA:::to.v3(RSDA:::to.v2(oils))
sym.radar.plot(oils[2:3,])
sym.radar.plot(oils[2:5,])

res <- interval.histogram.plot(oils[,2],
                               n.bins = 4,
                               col = c(2,3,4,5))
res

res <- interval.histogram.plot(oils[,3],
                               n.bins = 3,
                               main = "Histogram",
                               col = c(2, 3, 4))
res

## -----------------------------------------------------------------------------
data("oils")
DM <- sym.dist.interval(sym.data = oils[,1:4],
                        method = "Gowda.Diday")
model <- hclust(DM)
plot(model, hang = -1)

## -----------------------------------------------------------------------------
DM <- sym.dist.interval(sym.data= oils[,1:4],
                        method = "Ichino")
model <- hclust(DM)
plot(model, hang = -1)

## -----------------------------------------------------------------------------
DM <- sym.dist.interval(sym.data = oils[,c(1,2,4)],
                        gamma = 0.5,
                        method = "Hausdorff",
                        normalize = FALSE,
                        SpanNormalize = TRUE,
                        euclidea = TRUE,
                        q = 2)
model <- hclust(DM)
plot(model, hang = -1)

## -----------------------------------------------------------------------------
data(int_prost_train)
data(int_prost_test)
res.cm <- sym.lm(formula = lpsa~., sym.data = int_prost_train, method = 'cm')
res.cm

## -----------------------------------------------------------------------------
pred.cm <- sym.predict(model = res.cm, new.sym.data = int_prost_test)

## -----------------------------------------------------------------------------
RMSE.L(int_prost_test$lpsa, pred.cm$Fitted)
RMSE.U(int_prost_test$lpsa, pred.cm$Fitted)
R2.L(int_prost_test$lpsa, pred.cm$Fitted)
R2.U(int_prost_test$lpsa, pred.cm$Fitted)
deter.coefficient(int_prost_test$lpsa, pred.cm$Fitted)

## -----------------------------------------------------------------------------
data(int_prost_train)
data(int_prost_test)

## -----------------------------------------------------------------------------
res.cm.lasso <- sym.glm(sym.data = int_prost_train,
                        response = 9,
                        method = 'cm',
                        alpha = 1,
                        nfolds = 10,
                        grouped = TRUE)

## -----------------------------------------------------------------------------
pred.cm.lasso <- sym.predict(res.cm.lasso,
                             response = 9,
                             int_prost_test,
                             method = 'cm')

## -----------------------------------------------------------------------------
plot(res.cm.lasso)
plot(res.cm.lasso$glmnet.fit, "lambda", label=TRUE)

## -----------------------------------------------------------------------------
RMSE.L(int_prost_test$lpsa,pred.cm.lasso)
RMSE.U(int_prost_test$lpsa,pred.cm.lasso) 
R2.L(int_prost_test$lpsa,pred.cm.lasso) 
R2.U(int_prost_test$lpsa,pred.cm.lasso) 
deter.coefficient(int_prost_test$lpsa, pred.cm.lasso)

## -----------------------------------------------------------------------------
data(int_prost_train)
data(int_prost_test)

res.cm.ridge <- sym.glm(sym.data = int_prost_train,
                        response = 9,
                        method = 'cm',
                        alpha = 0,
                        nfolds = 10,
                        grouped = TRUE)

## -----------------------------------------------------------------------------
pred.cm.ridge <- sym.predict(res.cm.ridge,
                             response = 9,
                             int_prost_test,
                             method = 'cm')

## -----------------------------------------------------------------------------
plot(res.cm.ridge)
plot(res.cm.ridge$glmnet.fit, "lambda", label=TRUE)
RMSE.L(int_prost_test$lpsa, pred.cm.ridge)
RMSE.U(int_prost_test$lpsa, pred.cm.ridge)
R2.L(int_prost_test$lpsa, pred.cm.ridge)
R2.U(int_prost_test$lpsa, pred.cm.ridge)
deter.coefficient(int_prost_test$lpsa, pred.cm.ridge)

## -----------------------------------------------------------------------------
data("oils")
res <- sym.pca(oils,'centers')
plot(res, choix = "ind")
plot(res, choix = "var")

## -----------------------------------------------------------------------------
res <- sym.pca(oils,'tops')
plot(res, choix = "ind")

## -----------------------------------------------------------------------------
res <- sym.pca(oils, 'principal.curves')
plot(res, choix = "ind")

## -----------------------------------------------------------------------------
res <- sym.pca(oils,'optimized.distance')
plot(res, choix = "ind")
plot(res, choix = "var")

## -----------------------------------------------------------------------------
res <- sym.pca(oils,'optimized.variance')
plot(res, choix = "ind")
plot(res, choix = "var")

## -----------------------------------------------------------------------------
data("ex_mcfa1") 
ex_mcfa1

## -----------------------------------------------------------------------------
sym.table <- classic.to.sym(x = ex_mcfa1, 
                            concept = suspect, 
                            default.categorical = sym.set)
sym.table

## -----------------------------------------------------------------------------
res <- sym.mcfa(sym.table, c(2,3))
mcfa.scatterplot(res[,2], res[,3], sym.data = sym.table, pos.var = c(2,3))

## -----------------------------------------------------------------------------
res <- sym.mcfa(sym.table, c(2,3,4))
mcfa.scatterplot(res[,2], res[,3], sym.data = sym.table, pos.var = c(2,3,4))


## -----------------------------------------------------------------------------
datos <- oils
datos

## -----------------------------------------------------------------------------
x <- sym.umap(datos)
x

## -----------------------------------------------------------------------------
plot(x)

## -----------------------------------------------------------------------------
datos <- Cardiological
datos

## -----------------------------------------------------------------------------
x <- sym.umap(datos)
x

## -----------------------------------------------------------------------------
plot(x)

## -----------------------------------------------------------------------------
data(oils)
datos <- oils
interval.length(datos)

## -----------------------------------------------------------------------------
data("hardwoodBrito")
Hardwood.histogram<-hardwoodBrito
Hardwood.cols<-colnames(Hardwood.histogram)
Hardwood.names<-row.names(Hardwood.histogram)
Hardwood.histogram

Hardwood.histogram[[1]][[1]]

## -----------------------------------------------------------------------------
weighted.center<-weighted.center.Hist.RSDA(Hardwood.histogram)

## -----------------------------------------------------------------------------
BIN.Matrix<-matrix(rep(3,length(Hardwood.cols)*length(Hardwood.names)),nrow = length(Hardwood.names))

## -----------------------------------------------------------------------------
pca.hist<-sym.histogram.pca(Hardwood.histogram,BIN.Matrix)
pca.hist$classic.PCA
pca.hist$sym.hist.matrix.PCA

## -----------------------------------------------------------------------------
ACER.p1<-Sym.PCA.Hist.PCA.k.plot(data.sym.df = pca.hist$Bins.df,
                             title.graph = " ",
                             concepts.name = c("ACER"),
                             title.x = "First Principal Component (84.83%)",
                             title.y = "Frequency",
                             pca.axes = 1)

ACER.p1

## -----------------------------------------------------------------------------
ALL.p1<-Sym.PCA.Hist.PCA.k.plot(data.sym.df = pca.hist$Bins.df,
                    title.graph = " ",
                    concepts.name = unique(pca.hist$Bins.df$Object.Name),
                    title.x = "First Principal Component (84.83%)",
                    title.y = "Frequency",
                    pca.axes = 1)

ALL.p1

## -----------------------------------------------------------------------------
Hardwood.quantiles.PCA<-quantiles.RSDA(pca.hist$sym.hist.matrix.PCA,3)

label.name<-"Hard Wood"
Title<-"First Principal Plane"
axes.x.label<- "First Principal Component (84.83%)"
axes.y.label<- "Second Principal Component (9.70%)"
concept.names<-c("ACER")
var.names<-c("PC.1","PC.2")

quantile.ACER.plot<-Percentil.Arrow.plot(Hardwood.quantiles.PCA,
                     concept.names,
                     var.names,
                     Title,
                     axes.x.label,
                     axes.y.label,
                     label.name
                     )

quantile.ACER.plot

## -----------------------------------------------------------------------------
label.name<-"Hard Wood"
Title<-"First Principal Plane"
axes.x.label<- "First Principal Component (84.83%)"
axes.y.label<- "Second Principal Component (9.70%)"
concept.names<-row.names(Hardwood.quantiles.PCA)
var.names<-c("PC.1","PC.2")

quantile.plot<-Percentil.Arrow.plot(Hardwood.quantiles.PCA,
                     concept.names,
                     var.names,
                     Title,
                     axes.x.label,
                     axes.y.label,
                     label.name
                     )

quantile.plot

## -----------------------------------------------------------------------------
label.name<-"Hard Wood"
Title<-"First Principal Plane"
axes.x.label<- "PC 1 (84.83%)"
axes.y.label<- "PC 2 (9.70%)"
concept.names<-c("ACER")
var.names<-c("PC.1","PC.2")

plot.3D.HW<-sym.quantiles.PCA.plot(Hardwood.quantiles.PCA,
                               concept.names,
                               var.names,
                               Title,
                               axes.x.label,
                               axes.y.label,
                               label.name)

plot.3D.HW

## -----------------------------------------------------------------------------
concept.names<-row.names(Hardwood.quantiles.PCA)
sym.all.quantiles.plot(Hardwood.quantiles.PCA,
                               concept.names,
                               var.names,
                               Title,
                               axes.x.label,
                               axes.y.label,
                               label.name)

## -----------------------------------------------------------------------------
sym.all.quantiles.mesh3D.plot(Hardwood.quantiles.PCA,
                               concept.names,
                               var.names,
                               Title,
                               axes.x.label,
                               axes.y.label,
                               label.name)

## -----------------------------------------------------------------------------
Hardwood.quantiles.PCA.2<-quantiles.RSDA.KS(pca.hist$sym.hist.matrix.PCA,100)
h<-Hardwood.quantiles.PCA.2[[1]][[1]]
tmp<-HistRSDAToEcdf(h)

h2<-Hardwood.quantiles.PCA.2[[1]][[2]]
tmp2<-HistRSDAToEcdf(h2)

h3<-Hardwood.quantiles.PCA.2[[1]][[3]]
tmp3<-HistRSDAToEcdf(h3)

h4<-Hardwood.quantiles.PCA.2[[1]][[4]]
tmp4<-HistRSDAToEcdf(h4)

h5<-Hardwood.quantiles.PCA.2[[1]][[5]]
tmp5<-HistRSDAToEcdf(h5)

breaks.unique<-unique(c(h$breaks,h2$breaks,h3$breaks,h4$breaks,h5$breaks))
tmp.unique<-breaks.unique[order(breaks.unique)]

tmp<-tmp(v = tmp.unique)
tmp2<-tmp2(v = tmp.unique)
tmp3<-tmp3(v = tmp.unique)
tmp4<-tmp4(v = tmp.unique)
tmp5<-tmp5(v = tmp.unique)
abs_dif <-  abs(tmp2 - tmp)
# La distancia Kolmogorov–Smirnov es el máximo de las distancias absolutas.
distancia_ks <- max(abs_dif)
distancia_ks

## ----eval=FALSE---------------------------------------------------------------
#  library(tidyr)
#  # Se unen los valores calculados en un dataframe.
#  df.HW <- data.frame(
#    PC.1 = tmp.unique,
#    ACER = tmp,
#    ALNUS = tmp2,
#    FRAXINUS = tmp3,
#    JUGLANS = tmp4,
#    QUERCUS = tmp5
#  ) %>%
#    pivot_longer(
#      cols = c(ACER, ALNUS,FRAXINUS,JUGLANS,QUERCUS),
#      names_to = "HardWood",
#      values_to = "ecdf"
#    )
#  
#  grafico_ecdf <- ggplot(data = df.HW,
#                         aes(x = PC.1, y = ecdf, color = HardWood)) +
#    geom_line(size = 1) +
#    labs(
#      color = "Hardwood",
#      y = "Empirical Cumulative Distribution "
#    ) +
#    theme_bw() +
#    theme(legend.position = "bottom",
#          plot.title = element_text(size = 12))+geom_line()
#  
#  grafico_ecdf

