##
## Example: Split-plot Experiment (SPE)
##

## The parameters can be: formula, aov, lm or lmer.

## Note: Upper case for factors and lowercase for levels

library(TukeyC)
data(SPE)

## From: formula
## Main factor: SP
tk1 <- with(SPE,
            TukeyC(y ~ blk + P*SP + Error(blk/P),
                   data=dfm,
                   which='SP'))
summary(tk1)

## Nested: p1/SP
tk2 <- with(SPE,
            TukeyC(y ~ blk + P*SP + Error(blk/P),
                   data=dfm,
                   which='P:SP',
                   fl1=1 ))
summary(tk2)
plot(tk2,
     di='sd',
     d.col='red',
     d.lty=3)

## Nested: sp1/P - it is necessary to inform how to combinate the errors
tk3 <- with(SPE,
            TukeyC(y ~ blk + P*SP + Error(blk/P),
                   data=dfm,
                   which='SP:P',
                   error='Within/blk:P',
                   fl1=1))
summary(tk3)

## From: lm
lm1 <- with(SPE,
            lm(y ~ blk*P + P*SP,
               dfm))

tk4 <- TukeyC(lm1,
              which='P:SP',
              fl1=1)

summary(tk4)

tk5 <- TukeyC(lm1,
              which='SP:P',
              error='Within/blk:P',
              fl1=1)
summary(tk5)


## From: aov
av1 <- with(SPE,
            aov(y ~ blk + P*SP + Error(blk/P),
                data=dfm))
summary(av1)

## Main factor: SP
tk6 <- TukeyC(av1,
              which='SP',
              sig.level=0.1)
summary(tk6)

## Main factor: P
## It is necessary to inform the appropriate error for the test
tk7 <- TukeyC(av1,
              which='P',
              error='blk:P')

summary(tk7)

## Nested: p1/SP
## Testing SP inside of level one of P
tk8 <- TukeyC(av1,
              which='P:SP',
              fl1=1)
summary(tk8)

## Nested: p2/SP
tk9 <- TukeyC(av1,
              which='P:SP',
              fl1=2)
summary(tk9)

## Nested: p3/SP
tk10 <- TukeyC(av1,
               which='P:SP',
               fl1=3)
summary(tk10)

## Nested: sp1/P - it is necessary to inform how to combinate the errors
tk11 <- TukeyC(av1,
               which='SP:P',
               error='Within/blk:P',
               fl1=1)
summary(tk11)
