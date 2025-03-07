##
## Example: Split-split-plot Experiment (SSPE)
##

## The parameters can be: formula, aov, lm or lmer.

## Note: Upper case for factors and lowercase for levels

library(TukeyC)
data(SSPE)

## From: formula
## Main factor: P
## It is necessary to inform the appropriate error for the test
tk1 <- with(SSPE,
            TukeyC(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                   dfm,
                   which='P',
                   error='blk:P'))
summary(tk1)

## Nested: p2/SP
## It is necessary to inform the appropriate error for the test
tk2 <- with(SSPE,
            TukeyC(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                   dfm,
                   which='P:SP',
                   error='blk:P:SP',
                   fl1=2))
summary(tk2)

## Nested: p2/SSP
tk3 <- with(SSPE,
            TukeyC(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                   dfm,
                   which='P:SSP',
                   fl1=2))
summary(tk3)
plot(tk3,
     di='sd',
     d.col='red',
     d.lty=3)


## From: lm
lm1 <- with(SSPE,
            lm(y ~ blk*P + blk*P*SP + P*SP*SSP,
               data=dfm))
summary(lm1)

## Main factor: P
## It is necessary to inform the appropriate error for the test
tk4 <- TukeyC(lm1,
              which='P',
              error='blk:P')
summary(tk4)

## Main factor: SP
tk5 <- TukeyC(lm1,
              which='SP',
              error='blk:P:SP')
summary(tk5)

## Main factor: SSP
tk6 <- TukeyC(lm1,
              which='SSP')
summary(tk6)

## Nested: p1/SP
## It is necessary to inform the appropriate error for the test
tk7 <- TukeyC(lm1,
              which='P:SP',
              error='blk:P:SP',
              fl1=1)
summary(tk7)


## From: aov
av1 <- with(SSPE,
            aov(y ~  blk + P*SP*SSP + Error(blk/P/SP),
                data=dfm))
summary(av1)

## Main factor: P 
## It is necessary to inform the appropriate error for the test
tk8 <- TukeyC(av1,
              which='P',
              error='blk:P')
summary(tk8)

## Main factor: SSP
tk9 <- TukeyC(av1,
              which='SSP')
summary(tk9)

## Nested: p1/SP
## It is necessary to inform the appropriate error for the test
tk10 <- TukeyC(av1,
               which='P:SP',
               error='blk:P:SP',
               fl1=1)
summary(tk10)

## Nested: p2/SP
tk11 <- TukeyC(av1,
               which='P:SP',
               error='blk:P:SP',
               fl1=2)
summary(tk11)

## Nested: Pi/SPi/SSP (at various levels of P and SP)
tk12 <- TukeyC(av1,
               which='P:SP:SSP',
               fl1=1,
               fl2=1)
summary(tk12)

tk13 <- TukeyC(av1,
               which='P:SP:SSP',
               fl1=2,
               fl2=1)
summary(tk13)

tk14 <- TukeyC(av1,
               which='P:SP:SSP',
               fl1=3,
               fl2=3)
summary(tk14)

tk15 <- TukeyC(av1,
               which='P:SP:SSP',
               fl1=2,
               fl2=3)
summary(tk15)

## Nested: sp1/P
## It is necessary to inform the appropriate error for the test
tk16 <- TukeyC(av1,
               which='SP:P',
               error='blk:P:SP/blk:P',
               fl1=1)

summary(tk16)

## Nested: ssp1/SP
tk17 <- TukeyC(av1,
               which='SSP:SP',
               error='Within/blk:P:SP',
               fl1=1)
summary(tk17)

## Nested: ssp1/sp1/P
## It is necessary to inform the appropriate error for the test
tk18 <- TukeyC(av1,
               which='SSP:SP:P',
               error='Within/blk:P:SP/blk:P',
               fl1=1,
               fl2=1)
summary(tk18)
