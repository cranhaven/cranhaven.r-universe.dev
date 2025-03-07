##
## Examples: Completely Randomized Design (CRD)
##

## The parameters can be: formula, aov, lm or lmer.

## Example 1: an small experiment
library(TukeyC)
data(CRD1)

## From: formula - balanced
tk1 <- with(CRD1,
            TukeyC(y ~ x,
                   dfm))
tk1
summary(tk1)

plot(tk1)

plot(tk1,
     di='mm',
     d.lty=3)

plot(tk1,
     di='sd')

plot(tk1,
     di='ci',
     d.col='red')

## From: formula - unbalanced
u_tk1 <- with(CRD1,
              TukeyC(y ~ x,
                     dfm[-1,]))
u_tk1
summary(u_tk1)

## From: aov - balanced
av1 <- with(CRD1,
            aov(y ~ x,
                data=dfm))
summary(av1)

tk2 <- TukeyC(av1)
tk2
summary(tk2)

## From: lm - unbalanced
u_lm1 <- with(CRD1,
              lm(y ~ x,
                 data=dfm[-1,]))
summary(u_lm1)

u_tk2 <- TukeyC(u_lm1)
u_tk2
summary(u_tk2)

## Example 2: a lot of groups
data(CRD2)

## From: data.frame (dfm) - balanced
tk3 <- with(CRD2,
            TukeyC(y ~ x,
                   dfm))
plot(tk3,
     id.las=2,
     yl=FALSE,
     di='sd',
     d.lty=3,
     d.col='red')

## From: data.frame (dfm) - unbalanced
u_tk3 <- with(CRD2,
              TukeyC(y ~ x,
                     dfm[-1,]))
plot(u_tk3,
     id.las=2,
     yl=FALSE,
     di='sd',
     d.lty=3,
     d.col='red')

## From: aov - balanced
av2 <- with(CRD2,
            aov(y ~ x,
                data=dfm))
summary(av2)

tk4 <- TukeyC(av2)
plot(tk4,
     id.las=2,
     yl=FALSE,
     di='sd',
     d.lty=4,
     d.col='darkgreen')

## From: lm - unbalanced
u_lm2 <- with(CRD2,
              lm(y ~ x,
                 data=dfm[-1,]))
summary(u_lm2)

u_tk8 <- TukeyC(u_lm2)

plot(u_tk8,
     id.las=2,
     yl=FALSE)
