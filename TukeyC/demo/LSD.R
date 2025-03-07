##
## Example: Latin Squares Design (LSD)
##

## The parameters can be: formula, aov, lm or lmer.

library(TukeyC)
data(LSD)

## From: formula
## Testing tra
tk1 <- with(LSD,
            TukeyC(y ~ rows + cols + tra,
                   dfm,
                   which='tra',
                   sig.level=.052))
summary(tk1)

## From: formula
## Testing rows
tk2 <- with(LSD,
            TukeyC(y ~ rows + cols + tra,
                   dfm,
                   which='rows'))
summary(tk2)

## From: aov
av1 <- with(LSD,
            aov(y ~ rows + cols + tra,
                data=dfm))
summary(av1)

## From: aov
## Testing tra
tk3 <- TukeyC(av1,
              which='tra',
              sig.level=.052)
summary(tk3)

## From: aov
## Testing cols
tk4 <- TukeyC(av1,
              which='cols')
summary(tk4)
