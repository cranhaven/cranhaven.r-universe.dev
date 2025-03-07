##
## Example: Split-plot Experiment in time (SPET)
##

## The parameters can be: formula, aov, lm or lmer.

## Note: The factors are in uppercase and its levels in lowercase!

library(TukeyC)
data(SPET)

## From: formula
## Main factor: year
tk1 <- with(SPET,
            TukeyC(y ~ blk + tra*year + Error(blk/tra),
                   dfm,
                   which='year'))
summary(tk1)

## Nested: crotjuncea/year
tk2 <- with(SPET,
            TukeyC(y ~ blk + tra*year + Error(blk/tra),
                   dfm,
                   which='tra:year',
                   fl1=2))
summary(tk2)

## Nested: year_1/tra
## It is necessary to inform how to combinate the errors
tk3 <- with(SPET,
            TukeyC(y ~ blk + tra*year + Error(blk/tra),
                   dfm,
                   which='year:tra',
                   error='Within/blk:tra',
                   fl1=1))
summary(tk3)


## From: lm
lm1 <- with(SPET,
            lm(y ~ blk*tra + tra*year,
               data=dfm))

## Nested: tra1/year
tk4 <- TukeyC(lm1,
              which='tra:year',
              fl1=1)

summary(tk4)

## Nested: year1/tra
## It is necessary to inform how to combinate the errors
tk5 <- TukeyC(lm1,
              which='year:tra',
              error='Within/blk:tra',
              fl1=1)
summary(tk5,
        complete=FALSE)

## Nested: year2/tra
## It is necessary to inform how to combinate the errors
tk6 <- TukeyC(lm1,
              which='year:tra',
              error='Within/blk:tra',
              fl1=2)
summary(tk6,
        complete=FALSE)

## From: aov
av1 <- with(SPET,
            aov(y ~ blk + tra*year + Error(blk/tra),
                data=dfm))
summary(av1)

## Main factor: year
tk7 <- TukeyC(av1,
              which='year')
summary(tk7)

## Main factor: tra
## It is necessary to inform the appropriate error for the test
tk8 <- TukeyC(av1,
              which='tra',
              error='blk:tra')
summary(tk8,
        complete=FALSE)

## Nested: crotjuncea/year
tk9 <- TukeyC(av1,
              which='tra:year',
              fl1=2)
summary(tk9)

## Nested: guandu/year
tk10 <- TukeyC(av1,
               which='tra:year',
               fl1=4)
summary(tk10)

## Nested: year_1/tra - it is necessary to inform how to combinate the errors
tk11 <- TukeyC(av1,
               which='year:tra',
               error='Within/blk:tra',
               fl1=1)
summary(tk11,
        complete=FALSE)

op <- par(mar=c(6, 3, 3, 2))
plot(tk10,
     id.las=2,
     xlab='',
     di='sd',
     d.col='red',
     d.lty=3)

## Nested: year_2/tra - it is necessary to inform how to combinate the errors
tk12 <- TukeyC(av1,
               which='year:tra',
               error='Within/blk:tra',
               fl1=2)
summary(tk12)
op <- par(mar=c(7, 3, 3, 2))
plot(tk12,
     id.las=2,
     xlab='',
     di='sd',
     d.col='red',
     d.lty=3)
par(op)
