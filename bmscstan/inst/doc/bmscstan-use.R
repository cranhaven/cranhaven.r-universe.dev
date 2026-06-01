## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  eval = nzchar(Sys.getenv("bmscstan_eval"))
)

## ---- fig.height= 6, fig.width= 6---------------------------------------------
library(ggplot2)
library(bmscstan)

data(BSE)

str(data.pt)

str(data.ctrl)

ggplot(data.pt, aes(y = RT, x = Body.District:Side , fill = Congruency))+
  geom_boxplot()

ggplot(data.ctrl, aes(y = RT, x = Body.District:Side , fill = Congruency))+
  geom_boxplot()+
  facet_wrap( ~ ID , ncol = 4)

## ---- fig.show='hold'---------------------------------------------------------
qqnorm(data.ctrl$RT, main = "Controls")
qqline(data.ctrl$RT)

qqnorm(data.pt$RT, main = "Single Case")
qqline(data.pt$RT)

## ---- fig.show='hold'---------------------------------------------------------
out <- boxplot.stats( data.ctrl$RT )$out
data.ctrl <- droplevels( data.ctrl[ !data.ctrl$RT %in% out , ] )

out <- boxplot.stats( data.pt$RT )$out
data.pt <- droplevels( data.pt[ !data.pt$RT %in% out , ] )

qqnorm(data.ctrl$RT, main = "Controls")
qqline(data.ctrl$RT)

qqnorm(data.pt$RT, main = "Single Case")
qqline(data.pt$RT)

## -----------------------------------------------------------------------------
contrasts( data.ctrl$Side )          <- contr.treatment( n = 2 )
contrasts( data.ctrl$Congruency )    <- contr.treatment( n = 2 )
contrasts( data.ctrl$Body.District ) <- contr.treatment( n = 2 )

contrasts( data.pt$Side )            <- contr.treatment( n = 2 )
contrasts( data.pt$Congruency )      <- contr.treatment( n = 2 )
contrasts( data.pt$Body.District )   <- contr.treatment( n = 2 )

## -----------------------------------------------------------------------------
data.ctrl$BD_ID <- interaction( data.ctrl$Body.District , data.ctrl$ID )

## ---- warning = FALSE, message = FALSE----------------------------------------
mdl <- BMSC(formula = RT ~ Body.District * Congruency * Side +
             (Congruency * Side | BD_ID),
             data_ctrl = data.ctrl,
             data_sc = data.pt,
             chains = 2,
             cores = 1,
             seed = 2020)

## ---- fig.width=6, fig.height=6-----------------------------------------------
pp_check( mdl )

## -----------------------------------------------------------------------------
print( sum_mdl <- summary( mdl ) , digits = 3 )

## -----------------------------------------------------------------------------
tmp <- sum_mdl[[1]][sum_mdl[[1]]$BF10 > 3,c("BF10","mean","2.5%","97.5%")]

colnames(tmp) <- c("$BF_{10}$", "$\\mu$", "low $95\\%~CI$", "up $95\\%~CI$")

knitr::kable(
  tmp,
  digits = 3
)

## ---- fig.height=6, fig.width=6-----------------------------------------------
plot( mdl , who = "control" )

## -----------------------------------------------------------------------------
pp <- pairwise.BMSC(mdl = mdl , contrast = "Body.District2:Congruency2" ,
                    who = "control")

print( pp , digits = 3 )

## ---- fig.height=6, fig.width=6-----------------------------------------------
plot( pp )

## ---- fig.show = "hold"-------------------------------------------------------
p1 <- pairwise.BMSC(mdl , contrast = "Body.District2" ,  who = "control" )

plot( p1 )[[1]] +
  ggtitle("Body District" , subtitle = "Marginal effects") 

plot( p1 )[[2]] +
  ggtitle("Body District" , subtitle = "Contrasts") 

p2 <- pairwise.BMSC(mdl , contrast = "Congruency2" ,  who = "control" )

plot( p2 )[[1]] +
  ggtitle("Congruency" , subtitle = "Marginal effects")

plot( p2 )[[2]] +
  ggtitle("Congruency" , subtitle = "Contrasts")

p3 <- pairwise.BMSC(mdl , contrast = "Side2" ,  who = "control" )

plot( p3 )[[1]] +
  ggtitle("Side" , subtitle = "Marginal effects")

plot( p3 )[[2]] +
  ggtitle("Side" , subtitle = "Contrasts")

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot( mdl ) +
  theme_bw( base_size = 18 )+
  theme( legend.position = "bottom",
         legend.direction = "horizontal")

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot( mdl ,who = "delta" ) +
  theme_bw( base_size = 18 )

## -----------------------------------------------------------------------------
tmp <- sum_mdl[[3]][sum_mdl[[3]]$BF10 > 3,c("BF10","mean","2.5%","97.5%")]

colnames(tmp) <- c("$BF_{10}$", "$\\mu$", "low $95\\%~CI$", "up $95\\%~CI$")

knitr::kable(
  tmp,
  digits = 3
)

## -----------------------------------------------------------------------------
p4 <- pairwise.BMSC(mdl , contrast = "Body.District2:Congruency2" ,
                    who = "delta")

print( p4 , digits = 3 )

## ---- fig.show="hold"---------------------------------------------------------
plot( p4 , type = "interval")

plot( p4 , type = "area")

plot( p4 , type = "hist")

## ---- fig.height=6, fig.width=6-----------------------------------------------
p5 <- pairwise.BMSC(mdl , contrast = "Body.District2:Congruency2" ,
                    who = "singlecase")

plot( p5 , type = "hist")[[1]]

## ---- fig.height=6, fig.width=6-----------------------------------------------
p6 <- pairwise.BMSC(mdl , contrast = "Body.District2:Side2" , who = "delta")

print( p6 , digits = 3 )

plot( p6 , type = "hist")[[1]] +
  theme_bw( base_size = 18)+
  theme( strip.text.y = element_text( angle = 0 ) )

## ---- fig.height=6, fig.width=6-----------------------------------------------
p7 <- pairwise.BMSC(mdl ,
                    contrast = "Body.District2:Congruency2:Side2" ,
                    who = "delta")

print( p7 , digits = 3 )

plot( p7 , type = "hist")[[1]] +
  theme_bw( base_size = 18)+
  theme( strip.text.y = element_text( angle = 0 ) )

## -----------------------------------------------------------------------------
print( loo1 <- BMSC_loo( mdl ) )

plot( loo1 )

## -----------------------------------------------------------------------------
mdl.null <- BMSC(formula = RT ~ 1 +
             (Congruency * Side | BD_ID),
             data_ctrl = data.ctrl,
             data_sc = data.pt,
             cores = 1,
             chains = 2,
             seed = 2021)

print( loo2 <- BMSC_loo( mdl.null ) )

plot( loo2 )

BMSC_loo_compare( list( loo1, loo2 ) )

## -----------------------------------------------------------------------------
######################################
# simulation of controls' group data
######################################

# Number of levels for each condition and trials
NCond  <- 2
Ntrials <- 20
NSubjs  <- 40

betas <- c( 0.5 , 0 )

data.sim <- expand.grid(
  trial      = 1:Ntrials,
  ID         = factor(1:NSubjs),
  Cond      = factor(1:NCond)
)

### d.v. generation
y <- rep( times = nrow(data.sim) , NA )

# cheap simulation of individual random intercepts
set.seed(1)
rsubj <- rnorm(NSubjs , sd = 0.1)

for( i in 1:length( levels( data.sim$ID ) ) ){
  
  sel <- which( data.sim$ID == as.character(i) )
  
  mm  <- model.matrix(~ 1 + Cond , data = data.sim[ sel , ] )
  
  set.seed(1 + i)
  y[sel] <- mm %*% as.matrix(betas + rsubj[i]) +
    rnorm( n = Ntrials * NCond )
  
}

data.sim$y <- y
data.sim$bin <- sapply(
  LaplacesDemon::invlogit(data.sim$y),
  function(x) rbinom( 1, 1, x)
  )

data.sim.bin <- aggregate( bin ~ Cond * ID, data = data.sim, FUN = sum)
data.sim.bin$n <- aggregate( bin ~ Cond * ID,
                             data = data.sim, FUN = length)$bin

######################################
# simulation of patient data
######################################

betas.pt <- c( 0 , 2  )

data.pt <- expand.grid(
  trial      = 1:Ntrials,
  Cond      = factor(1:NCond)
)

### d.v. generation
mm  <- model.matrix(~ 1 + Cond , data = data.pt )

set.seed(5)
data.pt$y <- (mm %*% as.matrix(betas.pt + betas) +
  rnorm( n = Ntrials * NCond ))[,1]
data.pt$bin <- sapply(
  LaplacesDemon::invlogit(data.pt$y),
  function(x) rbinom( 1, 1, x)
  )

data.pt.bin <- aggregate( bin ~ Cond, data = data.pt, FUN = sum)
data.pt.bin$n <- aggregate( bin ~ Cond,
                             data = data.pt, FUN = length)$bin


plot(x = data.sim.bin$Cond, y = data.sim.bin$bin, ylim = c(0,20))
points(x = data.pt.bin$Cond, y = data.pt.bin$bin, col = "red")

## -----------------------------------------------------------------------------
mdlBin <- BMSC(formula = cbind(bin, n) ~ 1 + Cond,
            data_ctrl = data.sim.bin, data_sc = data.pt.bin, seed = 2022,
            chains = 2,
            family = "binomial", cores = 1)

print( summary( mdlBin ) , digits = 3 )

