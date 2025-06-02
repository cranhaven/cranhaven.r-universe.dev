## ----echo = FALSE, message = FALSE--------------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(LMest)
library(knitr)
opts_chunk$set(fig.align = "center",
               fig.width = 6, fig.height = 5,
               dev.args = list(pointsize=10),
               collapse = TRUE, par = TRUE,
               warning = FALSE, message = FALSE,
               highlight = FALSE)
set.seed(1945)

## ----message = FALSE, echo=1--------------------------------------------------
library(LMest)
cat(LMest:::Startup.Message(), sep="")

## -----------------------------------------------------------------------------
data("RLMSlong")
dim(RLMSlong)
str(RLMSlong)

## -----------------------------------------------------------------------------
data("PSIDlong")
dim(PSIDlong)
str(PSIDlong)

## -----------------------------------------------------------------------------
data(data_criminal_sim)
dim(data_criminal_sim)
str(data_criminal_sim)

## -----------------------------------------------------------------------------
data("NLSYlong")
dim(NLSYlong)

## -----------------------------------------------------------------------------
dt <- lmestData(data = NLSYlong, id = "id", time="time",
                responsesFormula= anti+self ~NULL)
summary(dt, dataSummary="responses", varType=rep("c",ncol(dt$Y)))

## -----------------------------------------------------------------------------
data_criminal_sim<-data.frame(data_criminal_sim)
crimf <- data_criminal_sim[data_criminal_sim$sex == 2,]
dt1 <- lmestData(data = crimf, id = "id", time = "time")
summary(dt1, varType = rep("d",ncol(dt1$Y)))  

## -----------------------------------------------------------------------------
fmBasic <- lmestFormula(data = RLMSlong, response = "value")

## -----------------------------------------------------------------------------
fmLatent <- lmestFormula(data = PSIDlong, response = "Y", 
                         LatentInitial = "X", LatentTransition ="X")

## -----------------------------------------------------------------------------
fmLatent2 <- lmestFormula(data = PSIDlong, response = "Y", 
                          LatentInitial = c("X1Race","X2Age","X3Age2","X9Income"), 
                          LatentTransition =c("X1Race","X2Age","X3Age2","X9Income"))

## ----eval=TRUE, add=TRUE, warning=FALSE,  results='hide'----------------------
mod <- lmest(responsesFormula = fmLatent$responsesFormula,
             index = c("id","time"),
             data = PSIDlong, k = 2) 

## ----eval=TRUE, add=TRUE, warning=FALSE, results='hide'-----------------------
mod <- lmest(responsesFormula = fmLatent$responsesFormula,
             index = c("id","time"),
             data = PSIDlong, k = 1:3) 

## -----------------------------------------------------------------------------
print(mod)

## -----------------------------------------------------------------------------
se(mod)

## ----eval=TRUE, add=TRUE, warning=FALSE---------------------------------------
mod2 <- lmest(responsesFormula = fmLatent$responsesFormula,
             latentFormula =  fmLatent$latentFormula,
             index = c("id","time"),
             data = PSIDlong, k = 2,
             paramLatent = "multilogit",
             start = 0, out_se=TRUE) 

## ----echo=TRUE, eval=TRUE, include=TRUE---------------------------------------
summary(mod2)

## ----fig.width = 5, fig.height = 4--------------------------------------------
plot(mod2, what = "CondProb")

## -----------------------------------------------------------------------------
plot(mod2, what="transitions")

## -----------------------------------------------------------------------------
plot(mod2, what="marginal")

## ----results='hide', warning=FALSE--------------------------------------------
dt$data$id = as.numeric(dt$data$id)
dt$data$time = as.numeric(dt$data$time)
modc <- lmestCont(responsesFormula = anti + self  ~ NULL,
                  latentFormula = ~ gender + childage + hispanic + black + pov +
                    momwork + married|   gender + childage + hispanic + black+ pov+
                    momwork + married,
                  index = c("id", "time"), 
                  data = dt$data,
                  k = 1:3, 
                  modBasic=1,  
                  output = TRUE, 
                  tol=10^-1)

## -----------------------------------------------------------------------------
plot(modc,what="modSel")

## -----------------------------------------------------------------------------
plot(modc, what="density")

## -----------------------------------------------------------------------------
plot(modc,what="density",components=c(1,2))

## -----------------------------------------------------------------------------
plot(modc,what="transitions")

## -----------------------------------------------------------------------------
semodc<-se(modc)

## -----------------------------------------------------------------------------
TabBe <-cbind(modc$Be, semodc$seBe, modc$Be/semodc$seBe)
colnames(TabBe) <- c("estBe",  "s.e.Be","t-test") 
round(TabBe,3) 

## -----------------------------------------------------------------------------
TabGa1 <- cbind(modc$Ga,semodc$seGa,modc$Ga/semodc$seGa) 
colnames(TabGa1) <- c("estGa(1)","estGa(2)", "s.e.Ga(1)","s.e.Ga(2)", "t-test(1)","t-test(2)")  
round(TabGa1,3) 

## ----results='hide'-----------------------------------------------------------
responsesFormula <- lmestFormula(data = crimf,response = "y")$responsesFormula

modm <- lmestMixed(responsesFormula =responsesFormula,
                  index = c("id","time"),
                  k1 = 2, k2 = 2,
                  tol = 10^-3,
                  data = crimf)


## -----------------------------------------------------------------------------
summary(modm)
round(modm$Psi[2, , ], 3)

## ----search, include=TRUE, results='hide'-------------------------------------
out <- lmestSearch(responsesFormula =  fmBasic$responsesFormula,
                   index = c("id","time"), 
                   data = RLMSlong,version ="categorical", k = 1:4,
                   modBasic = 1, seed = 123)

## -----------------------------------------------------------------------------
summary(out)

## -----------------------------------------------------------------------------
mod4 <- out$out.single[[4]]
summary(mod4)

## -----------------------------------------------------------------------------
plot(mod4, what="CondProb")

## -----------------------------------------------------------------------------
dec <- lmestDecoding(mod)

head(dec$Ul)

head(dec$Ug)

## ----include=TRUE, results='hide'---------------------------------------------
mboot <- bootstrap(modc, n = 581, B = 2, seed = 172)

## -----------------------------------------------------------------------------
mboot$seMu

## -----------------------------------------------------------------------------
draw3 <- draw(est = mod4, format = "matrices", seed = 4321, n = 100)
head(draw3$Y)

